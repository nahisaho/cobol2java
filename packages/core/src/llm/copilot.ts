/**
 * GitHub Copilot Language Model API Client
 * 
 * This module provides integration with VS Code's Language Model API,
 * which allows extensions to use GitHub Copilot's language models.
 * 
 * IMPORTANT: This client can only be used within a VS Code extension context.
 * It requires the vscode module to be available.
 */

import { type LLMClient, type LLMClientOptions } from './client.js';

// VS Code types (optional dependency)
interface VSCodeLM {
  selectChatModels(selector: { vendor?: string; family?: string }): Promise<any[]>;
  LanguageModelChatMessage: {
    User(content: string): any;
  };
  LanguageModelError: new (...args: any[]) => Error;
}

// Conditional import of vscode module
let vscode: VSCodeLM | undefined;
try {
  // This will only succeed in a VS Code extension context
  // eslint-disable-next-line @typescript-eslint/no-var-requires
  const vsModule = require('vscode') as VSCodeLM;
  vscode = vsModule;
} catch {
  // Not running in VS Code extension context
  vscode = undefined;
}

/**
 * GitHub Copilot LLM Client configuration
 */
export interface CopilotClientConfig extends Partial<LLMClientOptions> {
  /** Model family to use (default: 'gpt-4o') */
  family?: string;
  /** System prompt for the conversation */
  systemPrompt?: string;
}

/**
 * GitHub Copilot Language Model Client
 * 
 * Uses VS Code's Language Model API to interact with GitHub Copilot models.
 */
export class CopilotClient implements LLMClient {
  readonly provider = 'copilot';
  private readonly config: CopilotClientConfig;
  private model: any | undefined;

  constructor(config: CopilotClientConfig = {}) {
    this.config = {
      family: 'gpt-4o',
      ...config,
    };
  }

  /**
   * Check if the Copilot LM API is available
   */
  async isAvailable(): Promise<boolean> {
    if (!vscode) {
      return false;
    }

    try {
      const models = await vscode.selectChatModels({
        vendor: 'copilot',
        family: this.config.family,
      });
      return models.length > 0;
    } catch {
      return false;
    }
  }

  /**
   * Initialize the model for use
   */
  private async initModel(): Promise<void> {
    if (!vscode) {
      throw new Error('VS Code API is not available. This client can only be used within a VS Code extension.');
    }

    if (!this.model) {
      const models = await vscode.selectChatModels({
        vendor: 'copilot',
        family: this.config.family,
      });

      if (models.length === 0) {
        throw new Error(`No Copilot models available for family: ${this.config.family}`);
      }

      this.model = models[0];
    }
  }

  /**
   * Send a completion request to GitHub Copilot
   */
  async complete(prompt: string, _options?: { temperature?: number; maxTokens?: number }): Promise<string> {
    await this.initModel();

    if (!vscode) {
      throw new Error('VS Code API is not available');
    }

    // Build messages
    const messages: any[] = [];

    // Add system message if configured
    if (this.config.systemPrompt) {
      messages.push(vscode.LanguageModelChatMessage.User(this.config.systemPrompt));
    }

    // Add user prompt
    messages.push(vscode.LanguageModelChatMessage.User(prompt));

    try {
      // Send request
      const response = await this.model.sendRequest(messages, {
        justification: 'COBOL to Java code conversion',
      });

      // Collect response text
      let result = '';
      for await (const chunk of response.text) {
        result += chunk;
      }

      return result;
    } catch (err: unknown) {
      if (err instanceof Error) {
        throw new Error(`Copilot API error: ${err.message}`);
      }
      throw err;
    }
  }
}

/**
 * Create a Copilot client instance
 */
export function createCopilotClient(config?: CopilotClientConfig): CopilotClient {
  return new CopilotClient(config);
}

/**
 * Check if running in VS Code extension context
 */
export function isVSCodeContext(): boolean {
  return vscode !== undefined;
}
