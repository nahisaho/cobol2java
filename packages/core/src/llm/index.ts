/**
 * LLM Client Module
 * 
 * Provides LLM abstraction layer for multiple providers
 */

// Base clients
export { createLLMClient, type LLMClient, type LLMClientOptions, type CompletionOptions } from './client.js';
export { OpenAIClient, type OpenAIConfig } from './openai.js';
export { ClaudeClient, type ClaudeConfig } from './claude.js';
export { OllamaClient, type OllamaConfig } from './ollama.js';
export { NoopClient } from './noop.js';
export { CopilotClient, createCopilotClient, isVSCodeContext, type CopilotClientConfig } from './copilot.js';

// Enhanced client
export {
  createEnhancedClient,
  EnhancedLLMClientWrapper,
  type EnhancedLLMClient,
  type CompletionResult,
  type TokenUsage,
  type StreamChunk,
  type TemplateCompletionOptions,
} from './enhanced.js';

// Prompt templates
export {
  renderPrompt,
  getPromptTemplate,
  listTemplates,
  COBOL_ANALYSIS_TEMPLATE,
  COBOL_PATTERN_TEMPLATE,
  COBOL2JAVA_TEMPLATE,
  COBOL2SPRINGBOOT_TEMPLATE,
  JAVA_OPTIMIZATION_TEMPLATE,
  JAVA_TEST_TEMPLATE,
  DOCUMENTATION_TEMPLATE,
  PROMPT_TEMPLATES,
  type PromptTemplate,
  type TemplateVariables,
} from './prompts.js';

// AI Pipeline
export {
  createPipeline,
  AiConversionPipeline,
  type PipelineStage,
  type PipelineOptions,
  type PipelineResult,
  type StageResult,
  type CobolAnalysis,
  type CobolPattern,
} from './pipeline.js';

export type LLMProvider = 'openai' | 'claude' | 'ollama' | 'copilot' | 'none';

