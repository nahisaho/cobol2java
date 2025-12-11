/**
 * LLM Client Module
 * 
 * Provides LLM abstraction layer for multiple providers
 */

export { createLLMClient, type LLMClient, type LLMClientOptions } from './client.js';
export { OpenAIClient } from './openai.js';
export { ClaudeClient } from './claude.js';
export { OllamaClient } from './ollama.js';
export { NoopClient } from './noop.js';
export { CopilotClient, createCopilotClient, isVSCodeContext, type CopilotClientConfig } from './copilot.js';

export type LLMProvider = 'openai' | 'claude' | 'ollama' | 'copilot' | 'none';
