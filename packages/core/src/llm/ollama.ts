/**
 * Ollama (Local LLM) Client
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
 * Ollama client configuration
 */
export interface OllamaConfig {
  baseUrl?: string;
  model?: string;
  maxRetries?: number;
  retryDelayMs?: number;
}

/**
 * Ollama client implementation with retry logic
 */
export class OllamaClient implements LLMClient {
  readonly provider = 'ollama';
  
  private readonly config: OllamaConfig;
  private readonly defaultBaseUrl = 'http://localhost:11434';
  private readonly defaultModel = 'llama3.2';
  private readonly maxRetries: number;
  private readonly retryDelayMs: number;

  constructor(baseUrlOrConfig?: string | OllamaConfig, model?: string) {
    if (typeof baseUrlOrConfig === 'object') {
      this.config = baseUrlOrConfig;
    } else {
      this.config = {
        baseUrl: baseUrlOrConfig,
        model: model,
      };
    }
    this.maxRetries = this.config.maxRetries ?? 3;
    this.retryDelayMs = this.config.retryDelayMs ?? 1000;
  }

  async complete(prompt: string, options?: CompletionOptions): Promise<string> {
    const baseUrl = this.config.baseUrl || this.defaultBaseUrl;
    const systemPrompt = options?.systemPrompt || COBOL2JAVA_SYSTEM_PROMPT;
    let lastError: Error | null = null;

    for (let attempt = 0; attempt < this.maxRetries; attempt++) {
      try {
        const response = await fetch(`${baseUrl}/api/generate`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            model: this.config.model || this.defaultModel,
            prompt: `${systemPrompt}\n\n${prompt}`,
            stream: false,
            options: {
              num_predict: options?.maxTokens ?? 4096,
              temperature: options?.temperature ?? 0.2,
              stop: options?.stopSequences,
            },
          }),
        });

        if (!response.ok) {
          const error = await response.text();
          throw new Error(`Ollama API error (${response.status}): ${error}`);
        }

        const data = await response.json() as { response: string };
        return data.response ?? '';
      } catch (error) {
        lastError = error instanceof Error ? error : new Error(String(error));
        // Connection errors are common with local Ollama - retry
        if (attempt < this.maxRetries - 1) {
          await this.sleep(this.retryDelayMs * (attempt + 1));
        }
      }
    }

    throw lastError || new Error('Ollama request failed after retries');
  }

  async isAvailable(): Promise<boolean> {
    try {
      const baseUrl = this.config.baseUrl || this.defaultBaseUrl;
      const response = await fetch(`${baseUrl}/api/tags`);
      return response.ok;
    } catch {
      return false;
    }
  }

  /**
   * Stream completion for real-time UI updates
   */
  async *streamComplete(prompt: string, options?: CompletionOptions): AsyncGenerator<string> {
    const baseUrl = this.config.baseUrl || this.defaultBaseUrl;
    const systemPrompt = options?.systemPrompt || COBOL2JAVA_SYSTEM_PROMPT;

    const response = await fetch(`${baseUrl}/api/generate`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        model: this.config.model || this.defaultModel,
        prompt: `${systemPrompt}\n\n${prompt}`,
        stream: true,
        options: {
          num_predict: options?.maxTokens ?? 4096,
          temperature: options?.temperature ?? 0.2,
          stop: options?.stopSequences,
        },
      }),
    });

    if (!response.ok) {
      const error = await response.text();
      throw new Error(`Ollama API error (${response.status}): ${error}`);
    }

    const reader = response.body?.getReader();
    if (!reader) {
      throw new Error('No response body reader');
    }

    const decoder = new TextDecoder();
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;

      const chunk = decoder.decode(value, { stream: true });
      const lines = chunk.split('\n').filter(line => line.trim());
      
      for (const line of lines) {
        try {
          const data = JSON.parse(line) as { response?: string; done?: boolean };
          if (data.response) {
            yield data.response;
          }
          if (data.done) {
            return;
          }
        } catch {
          // Skip invalid JSON lines
        }
      }
    }
  }

  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}
