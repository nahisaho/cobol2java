/**
 * Ollama (Local LLM) Client
 */

import { type LLMClient, type CompletionOptions } from './client.js';

/**
 * Ollama client implementation
 */
export class OllamaClient implements LLMClient {
  readonly provider = 'ollama';
  
  private readonly baseUrl: string;
  private readonly model: string;

  constructor(baseUrl?: string, model?: string) {
    this.baseUrl = baseUrl || 'http://localhost:11434';
    this.model = model || 'llama3.2';
  }

  async complete(prompt: string, options?: CompletionOptions): Promise<string> {
    const response = await fetch(`${this.baseUrl}/api/generate`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        model: this.model,
        prompt: options?.systemPrompt
          ? `${options.systemPrompt}\n\n${prompt}`
          : prompt,
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
      throw new Error(`Ollama API error: ${error}`);
    }

    const data = await response.json() as { response: string };
    return data.response ?? '';
  }

  async isAvailable(): Promise<boolean> {
    try {
      const response = await fetch(`${this.baseUrl}/api/tags`);
      return response.ok;
    } catch {
      return false;
    }
  }
}
