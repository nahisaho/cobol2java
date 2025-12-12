/**
 * Enhanced LLM Client Interface
 *
 * Extended interface with streaming support, token estimation,
 * and structured completions.
 */

import { type LLMClient, type CompletionOptions } from './client.js';
import { type PromptTemplate, renderPrompt, type TemplateVariables } from './prompts.js';

/**
 * Token usage information
 */
export interface TokenUsage {
  /** Prompt tokens used */
  promptTokens: number;
  /** Completion tokens generated */
  completionTokens: number;
  /** Total tokens */
  totalTokens: number;
}

/**
 * Completion result with metadata
 */
export interface CompletionResult {
  /** Generated text */
  text: string;
  /** Token usage (if available) */
  usage?: TokenUsage;
  /** Model used */
  model?: string;
  /** Finish reason */
  finishReason?: 'stop' | 'length' | 'content_filter' | 'error';
  /** Processing time in milliseconds */
  processingTimeMs?: number;
}

/**
 * Streaming chunk
 */
export interface StreamChunk {
  /** Text chunk */
  text: string;
  /** Is this the final chunk? */
  done: boolean;
  /** Usage info (only on final chunk) */
  usage?: TokenUsage;
}

/**
 * Enhanced LLM client with streaming and metadata
 */
export interface EnhancedLLMClient extends LLMClient {
  /**
   * Complete with full result metadata
   */
  completeWithMetadata(
    prompt: string,
    options?: CompletionOptions
  ): Promise<CompletionResult>;

  /**
   * Stream completion chunks
   */
  streamComplete?(
    prompt: string,
    options?: CompletionOptions
  ): AsyncGenerator<StreamChunk, void, unknown>;

  /**
   * Estimate token count for text
   */
  estimateTokens?(text: string): number;

  /**
   * Get model context window size
   */
  getContextWindow?(): number;
}

/**
 * Template-based completion options
 */
export interface TemplateCompletionOptions {
  /** Additional completion options */
  completionOptions?: Omit<CompletionOptions, 'systemPrompt'>;
}

/**
 * LLM Client Wrapper with enhanced functionality
 *
 * Wraps a base LLMClient with additional features:
 * - Template-based prompts
 * - Token estimation
 * - Response caching
 * - Rate limiting
 */
export class EnhancedLLMClientWrapper implements EnhancedLLMClient {
  private readonly client: LLMClient;
  private readonly cache: Map<string, CompletionResult> = new Map();
  private readonly maxCacheSize: number;
  private lastRequestTime: number = 0;
  private readonly minRequestIntervalMs: number;

  constructor(
    client: LLMClient,
    options: {
      enableCache?: boolean;
      maxCacheSize?: number;
      minRequestIntervalMs?: number;
    } = {}
  ) {
    this.client = client;
    this.maxCacheSize = options.maxCacheSize ?? 100;
    this.minRequestIntervalMs = options.minRequestIntervalMs ?? 0;
  }

  get provider(): string {
    return this.client.provider;
  }

  /**
   * Rate limiting helper
   */
  private async enforceRateLimit(): Promise<void> {
    if (this.minRequestIntervalMs > 0) {
      const elapsed = Date.now() - this.lastRequestTime;
      if (elapsed < this.minRequestIntervalMs) {
        await new Promise(resolve =>
          setTimeout(resolve, this.minRequestIntervalMs - elapsed)
        );
      }
    }
    this.lastRequestTime = Date.now();
  }

  /**
   * Cache management
   */
  private getCacheKey(prompt: string, options?: CompletionOptions): string {
    return JSON.stringify({ prompt, options });
  }

  private getCached(key: string): CompletionResult | undefined {
    return this.cache.get(key);
  }

  private setCached(key: string, result: CompletionResult): void {
    // LRU eviction
    if (this.cache.size >= this.maxCacheSize) {
      const firstKey = this.cache.keys().next().value;
      if (firstKey) this.cache.delete(firstKey);
    }
    this.cache.set(key, result);
  }

  /**
   * Estimate token count using simple heuristic
   * Approximately 4 characters per token for English
   * Approximately 2 characters per token for code
   */
  estimateTokens(text: string): number {
    // Code typically has more tokens per character
    const codePatterns = /[{}()[\];=<>+\-*\/]/g;
    const codeMatches = text.match(codePatterns)?.length ?? 0;

    // Estimate based on content type
    const isCode = codeMatches > text.length * 0.02;
    const charsPerToken = isCode ? 3 : 4;

    return Math.ceil(text.length / charsPerToken);
  }

  /**
   * Get context window for common models
   */
  getContextWindow(): number {
    const modelWindows: Record<string, number> = {
      'gpt-4o': 128000,
      'gpt-4-turbo': 128000,
      'gpt-4': 8192,
      'gpt-3.5-turbo': 16385,
      'claude-3-5-sonnet-20241022': 200000,
      'claude-3-opus': 200000,
      'claude-3-haiku': 200000,
      'llama3.2': 8192,
      'llama3.1': 128000,
      'codellama': 16384,
    };

    return modelWindows[this.provider] ?? 8192;
  }

  async complete(prompt: string, options?: CompletionOptions): Promise<string> {
    const result = await this.completeWithMetadata(prompt, options);
    return result.text;
  }

  async completeWithMetadata(
    prompt: string,
    options?: CompletionOptions
  ): Promise<CompletionResult> {
    // Check cache
    const cacheKey = this.getCacheKey(prompt, options);
    const cached = this.getCached(cacheKey);
    if (cached) {
      return { ...cached, finishReason: 'stop' };
    }

    await this.enforceRateLimit();

    const startTime = Date.now();

    try {
      const text = await this.client.complete(prompt, options);
      const processingTimeMs = Date.now() - startTime;

      const result: CompletionResult = {
        text,
        processingTimeMs,
        finishReason: 'stop',
        usage: {
          promptTokens: this.estimateTokens(prompt),
          completionTokens: this.estimateTokens(text),
          totalTokens:
            this.estimateTokens(prompt) + this.estimateTokens(text),
        },
      };

      this.setCached(cacheKey, result);
      return result;
    } catch (error) {
      return {
        text: '',
        processingTimeMs: Date.now() - startTime,
        finishReason: 'error',
      };
    }
  }

  async isAvailable(): Promise<boolean> {
    return this.client.isAvailable();
  }

  /**
   * Complete using a prompt template
   */
  async completeWithTemplate(
    template: PromptTemplate,
    variables: TemplateVariables,
    options?: TemplateCompletionOptions
  ): Promise<CompletionResult> {
    const { systemPrompt, userPrompt } = renderPrompt(template, variables);

    return this.completeWithMetadata(userPrompt, {
      ...options?.completionOptions,
      systemPrompt,
      temperature: options?.completionOptions?.temperature ?? template.temperature,
      maxTokens: options?.completionOptions?.maxTokens ?? template.maxTokens,
    });
  }

  /**
   * Clear the response cache
   */
  clearCache(): void {
    this.cache.clear();
  }

  /**
   * Get cache statistics
   */
  getCacheStats(): { size: number; maxSize: number } {
    return {
      size: this.cache.size,
      maxSize: this.maxCacheSize,
    };
  }
}

/**
 * Create an enhanced LLM client wrapper
 */
export function createEnhancedClient(
  client: LLMClient,
  options?: {
    enableCache?: boolean;
    maxCacheSize?: number;
    minRequestIntervalMs?: number;
  }
): EnhancedLLMClientWrapper {
  return new EnhancedLLMClientWrapper(client, options);
}
