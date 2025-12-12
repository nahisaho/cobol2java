/**
 * Claude (Anthropic) LLM Client
 */

import { type LLMClient, type CompletionOptions } from './client.js';

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
 * Claude client configuration
 */
export interface ClaudeConfig {
  apiKey?: string;
  model?: string;
  maxRetries?: number;
  retryDelayMs?: number;
}

/**
 * Claude client implementation with retry logic
 */
export class ClaudeClient implements LLMClient {
  readonly provider = 'claude';
  
  private readonly config: ClaudeConfig;
  private readonly defaultModel = 'claude-3-5-sonnet-20241022';
  private readonly maxRetries: number;
  private readonly retryDelayMs: number;

  constructor(apiKeyOrConfig?: string | ClaudeConfig, model?: string) {
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

  async complete(prompt: string, options?: CompletionOptions): Promise<string> {
    if (!this.config.apiKey) {
      throw new Error('Claude API key is required');
    }

    const systemPrompt = options?.systemPrompt || COBOL2JAVA_SYSTEM_PROMPT;
    let lastError: Error | null = null;

    for (let attempt = 0; attempt < this.maxRetries; attempt++) {
      try {
        const response = await fetch('https://api.anthropic.com/v1/messages', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'x-api-key': this.config.apiKey,
            'anthropic-version': '2023-06-01',
          },
          body: JSON.stringify({
            model: this.config.model || this.defaultModel,
            max_tokens: options?.maxTokens ?? 4096,
            system: systemPrompt,
            messages: [{ role: 'user', content: prompt }],
            stop_sequences: options?.stopSequences,
          }),
        });

        if (response.status === 429) {
          const retryAfter = parseInt(response.headers.get('Retry-After') || '5', 10);
          await this.sleep(retryAfter * 1000);
          continue;
        }

        if (!response.ok) {
          const error = await response.text();
          throw new Error(`Claude API error (${response.status}): ${error}`);
        }

        const data = await response.json() as {
          content: Array<{ type: string; text: string }>;
        };

        const textContent = data.content.find(c => c.type === 'text');
        return textContent?.text ?? '';
      } catch (error) {
        lastError = error instanceof Error ? error : new Error(String(error));
        if (attempt < this.maxRetries - 1) {
          await this.sleep(this.retryDelayMs * (attempt + 1));
        }
      }
    }

    throw lastError || new Error('Claude request failed after retries');
  }

  /**
   * Stream completion (for streaming UI support)
   */
  async *streamComplete(
    prompt: string,
    options?: CompletionOptions
  ): AsyncGenerator<string, void, unknown> {
    if (!this.config.apiKey) {
      throw new Error('Claude API key is required');
    }

    const systemPrompt = options?.systemPrompt || COBOL2JAVA_SYSTEM_PROMPT;

    const response = await fetch('https://api.anthropic.com/v1/messages', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'x-api-key': this.config.apiKey,
        'anthropic-version': '2023-06-01',
      },
      body: JSON.stringify({
        model: this.config.model || this.defaultModel,
        max_tokens: options?.maxTokens ?? 4096,
        system: systemPrompt,
        messages: [{ role: 'user', content: prompt }],
        stop_sequences: options?.stopSequences,
        stream: true,
      }),
    });

    if (!response.ok) {
      throw new Error(`Claude API error: ${response.statusText}`);
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
            if (parsed.type === 'content_block_delta') {
              const content = parsed.delta?.text;
              if (content) yield content;
            }
          } catch {
            // Skip invalid JSON
          }
        }
      }
    }
  }

  async isAvailable(): Promise<boolean> {
    return !!this.config.apiKey;
  }

  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}
