/**
 * No-op LLM Client
 * 
 * Used when LLM is disabled (llmProvider: 'none')
 */

import { type LLMClient, type CompletionOptions } from './client.js';

/**
 * No-op client implementation
 */
export class NoopClient implements LLMClient {
  readonly provider = 'none';

  async complete(_prompt: string, _options?: CompletionOptions): Promise<string> {
    // Return empty string - rule-based conversion only
    return '';
  }

  async isAvailable(): Promise<boolean> {
    return true; // Always available
  }
}
