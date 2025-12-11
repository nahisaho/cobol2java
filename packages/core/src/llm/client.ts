/**
 * LLM Client Interface
 * 
 * Abstract interface for LLM providers
 */

import { OpenAIClient } from './openai.js';
import { ClaudeClient } from './claude.js';
import { OllamaClient } from './ollama.js';
import { NoopClient } from './noop.js';

/**
 * Completion options
 */
export interface CompletionOptions {
  /** Maximum tokens to generate */
  maxTokens?: number;
  /** Temperature for sampling (0-1) */
  temperature?: number;
  /** Stop sequences */
  stopSequences?: string[];
  /** System prompt */
  systemPrompt?: string;
}

/**
 * LLM Client interface
 */
export interface LLMClient {
  /** Provider name */
  readonly provider: string;
  
  /**
   * Complete a prompt
   * 
   * @param prompt - User prompt
   * @param options - Completion options
   * @returns Generated completion
   */
  complete(prompt: string, options?: CompletionOptions): Promise<string>;
  
  /**
   * Check if the client is available
   * 
   * @returns True if client is available
   */
  isAvailable(): Promise<boolean>;
}

/**
 * LLM Client options
 */
export interface LLMClientOptions {
  /** LLM provider */
  provider: 'openai' | 'claude' | 'ollama' | 'copilot' | 'none';
  /** Model name */
  model?: string;
  /** API key */
  apiKey?: string;
  /** Base URL (for Ollama) */
  baseUrl?: string;
}

/**
 * Create an LLM client
 * 
 * @param options - Client options
 * @returns LLM client instance
 */
export function createLLMClient(options: LLMClientOptions): LLMClient {
  switch (options.provider) {
    case 'openai':
      return new OpenAIClient(options.apiKey, options.model);
    case 'claude':
      return new ClaudeClient(options.apiKey, options.model);
    case 'ollama':
      return new OllamaClient(options.baseUrl, options.model);
    case 'copilot': {
      // Dynamic import to avoid issues when not in VS Code context
      const { CopilotClient } = require('./copilot.js');
      return new CopilotClient({ family: options.model || 'gpt-4o' });
    }
    case 'none':
    default:
      return new NoopClient();
  }
}
