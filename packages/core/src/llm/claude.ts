/**
 * Claude (Anthropic) LLM Client
 */

import { type LLMClient, type CompletionOptions } from './client.js';

/**
 * Claude client implementation
 */
export class ClaudeClient implements LLMClient {
  readonly provider = 'claude';
  
  private readonly apiKey?: string;
  private readonly model: string;

  constructor(apiKey?: string, model?: string) {
    this.apiKey = apiKey;
    this.model = model || 'claude-3-5-sonnet-20241022';
  }

  async complete(prompt: string, options?: CompletionOptions): Promise<string> {
    if (!this.apiKey) {
      throw new Error('Claude API key is required');
    }

    const response = await fetch('https://api.anthropic.com/v1/messages', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'x-api-key': this.apiKey,
        'anthropic-version': '2023-06-01',
      },
      body: JSON.stringify({
        model: this.model,
        max_tokens: options?.maxTokens ?? 4096,
        system: options?.systemPrompt,
        messages: [{ role: 'user', content: prompt }],
        stop_sequences: options?.stopSequences,
      }),
    });

    if (!response.ok) {
      const error = await response.text();
      throw new Error(`Claude API error: ${error}`);
    }

    const data = await response.json() as {
      content: Array<{ type: string; text: string }>;
    };

    const textContent = data.content.find(c => c.type === 'text');
    return textContent?.text ?? '';
  }

  async isAvailable(): Promise<boolean> {
    // Claude doesn't have a simple health check endpoint
    // Return true if API key is set
    return !!this.apiKey;
  }
}
