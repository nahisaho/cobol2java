/**
 * OpenAI LLM Client
 */

import { type LLMClient, type CompletionOptions } from './client.js';

/**
 * OpenAI client configuration
 */
export interface OpenAIConfig {
  apiKey?: string;
  model?: string;
  baseUrl?: string; // For Azure OpenAI or custom endpoints
  azureDeployment?: string; // Azure OpenAI deployment name
  apiVersion?: string; // Azure API version
  maxRetries?: number;
  retryDelayMs?: number;
}

/**
 * COBOL to Java conversion system prompt
 */
const COBOL2JAVA_SYSTEM_PROMPT = `You are an expert COBOL to Java converter. Your task is to:
1. Analyze COBOL code patterns and convert them to idiomatic Java
2. Preserve business logic exactly
3. Use appropriate Java types (BigDecimal for COMP-3, String for PIC X, etc.)
4. Generate clean, maintainable Java code
5. Add appropriate comments explaining complex conversions
6. Follow Spring Boot conventions when requested

Be precise and only output valid Java code without explanations unless asked.`;

/**
 * OpenAI client implementation with retry logic and Azure support
 */
export class OpenAIClient implements LLMClient {
  readonly provider = 'openai';
  
  private readonly config: OpenAIConfig;
  private readonly defaultModel = 'gpt-4o';
  private readonly maxRetries: number;
  private readonly retryDelayMs: number;

  constructor(apiKeyOrConfig?: string | OpenAIConfig, model?: string) {
    if (typeof apiKeyOrConfig === 'object') {
      this.config = apiKeyOrConfig;
    } else {
      this.config = {
        apiKey: apiKeyOrConfig,
        model: model,
      };
    }
    this.maxRetries = this.config.maxRetries ?? 3;
    this.retryDelayMs = this.config.retryDelayMs ?? 1000;
  }

  private getApiUrl(): string {
    if (this.config.baseUrl && this.config.azureDeployment) {
      // Azure OpenAI endpoint
      const apiVersion = this.config.apiVersion || '2024-02-01';
      return `${this.config.baseUrl}/openai/deployments/${this.config.azureDeployment}/chat/completions?api-version=${apiVersion}`;
    }
    return this.config.baseUrl || 'https://api.openai.com/v1/chat/completions';
  }

  private getHeaders(): Record<string, string> {
    const headers: Record<string, string> = {
      'Content-Type': 'application/json',
    };

    if (this.config.azureDeployment) {
      headers['api-key'] = this.config.apiKey || '';
    } else {
      headers['Authorization'] = `Bearer ${this.config.apiKey}`;
    }

    return headers;
  }

  async complete(prompt: string, options?: CompletionOptions): Promise<string> {
    if (!this.config.apiKey) {
      throw new Error('OpenAI API key is required');
    }

    const systemPrompt = options?.systemPrompt || COBOL2JAVA_SYSTEM_PROMPT;
    let lastError: Error | null = null;

    for (let attempt = 0; attempt < this.maxRetries; attempt++) {
      try {
        const response = await fetch(this.getApiUrl(), {
          method: 'POST',
          headers: this.getHeaders(),
          body: JSON.stringify({
            model: this.config.model || this.defaultModel,
            messages: [
              { role: 'system', content: systemPrompt },
              { role: 'user', content: prompt },
            ],
            max_tokens: options?.maxTokens ?? 4096,
            temperature: options?.temperature ?? 0.2,
            stop: options?.stopSequences,
          }),
        });

        if (response.status === 429) {
          // Rate limited - wait and retry
          const retryAfter = parseInt(response.headers.get('Retry-After') || '5', 10);
          await this.sleep(retryAfter * 1000);
          continue;
        }

        if (!response.ok) {
          const error = await response.text();
          throw new Error(`OpenAI API error (${response.status}): ${error}`);
        }

        const data = await response.json() as {
          choices: Array<{ message: { content: string } }>;
        };

        return data.choices[0]?.message.content ?? '';
      } catch (error) {
        lastError = error instanceof Error ? error : new Error(String(error));
        if (attempt < this.maxRetries - 1) {
          await this.sleep(this.retryDelayMs * (attempt + 1));
        }
      }
    }

    throw lastError || new Error('OpenAI request failed after retries');
  }

  /**
   * Stream completion (for future streaming UI support)
   */
  async *streamComplete(
    prompt: string,
    options?: CompletionOptions
  ): AsyncGenerator<string, void, unknown> {
    if (!this.config.apiKey) {
      throw new Error('OpenAI API key is required');
    }

    const response = await fetch(this.getApiUrl(), {
      method: 'POST',
      headers: this.getHeaders(),
      body: JSON.stringify({
        model: this.config.model || this.defaultModel,
        messages: [
          { role: 'system', content: options?.systemPrompt || COBOL2JAVA_SYSTEM_PROMPT },
          { role: 'user', content: prompt },
        ],
        max_tokens: options?.maxTokens ?? 4096,
        temperature: options?.temperature ?? 0.2,
        stream: true,
      }),
    });

    if (!response.ok) {
      throw new Error(`OpenAI API error: ${response.statusText}`);
    }

    const reader = response.body?.getReader();
    if (!reader) throw new Error('No response body');

    const decoder = new TextDecoder();

    while (true) {
      const { done, value } = await reader.read();
      if (done) break;

      const text = decoder.decode(value);
      const lines = text.split('\n').filter(line => line.trim());

      for (const line of lines) {
        if (line.startsWith('data: ')) {
          const data = line.slice(6);
          if (data === '[DONE]') return;

          try {
            const parsed = JSON.parse(data);
            const content = parsed.choices?.[0]?.delta?.content;
            if (content) yield content;
          } catch {
            // Skip invalid JSON
          }
        }
      }
    }
  }

  async isAvailable(): Promise<boolean> {
    if (!this.config.apiKey) return false;
    
    try {
      const response = await fetch('https://api.openai.com/v1/models', {
        headers: { 'Authorization': `Bearer ${this.config.apiKey}` },
      });
      return response.ok;
    } catch {
      return false;
    }
  }

  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}
