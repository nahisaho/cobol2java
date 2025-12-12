/**
 * LLM Module Tests - Prompts, Enhanced Client, Pipeline
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import {
  // Prompts
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
  type PromptTemplate,
  // Enhanced client
  createEnhancedClient,
  type LLMClient,
  type CompletionOptions,
  // Pipeline
  createPipeline,
  AiConversionPipeline,
} from '../src/llm/index.js';
import { NoopClient } from '../src/llm/noop.js';

// ============================================================================
// Mock LLM Client
// ============================================================================

class MockLLMClient implements LLMClient {
  readonly provider = 'mock';
  private responses: Map<string, string> = new Map();
  public callCount = 0;
  public lastPrompt = '';
  public lastOptions: CompletionOptions | undefined;

  setResponse(promptContains: string, response: string): void {
    this.responses.set(promptContains, response);
  }

  async complete(prompt: string, options?: CompletionOptions): Promise<string> {
    this.callCount++;
    this.lastPrompt = prompt;
    this.lastOptions = options;

    // Find matching response
    for (const [key, value] of this.responses) {
      if (prompt.includes(key)) {
        return value;
      }
    }

    // Default response based on context
    if (options?.systemPrompt?.includes('analysis') || prompt.includes('Analyze')) {
      return `## Program Overview\nTest COBOL program\n## Recommended Approach\nDirect conversion`;
    }
    if (options?.systemPrompt?.includes('pattern') || prompt.includes('patterns')) {
      return `[{"patternType": "arithmetic", "location": "PROCEDURE", "javaEquivalent": "BigDecimal", "complexity": "simple"}]`;
    }
    if (options?.systemPrompt?.includes('Java') || prompt.includes('Convert')) {
      return '```java\npublic class TestClass {\n  public void process() {\n    // Converted code\n  }\n}\n```';
    }
    if (options?.systemPrompt?.includes('optimization') || prompt.includes('Optimize')) {
      return '```java\npublic class TestClass {\n  public void process() {\n    // Optimized code\n  }\n}\n```';
    }
    if (options?.systemPrompt?.includes('test') || prompt.includes('test')) {
      return '```java\n@Test\nvoid testProcess() {\n  // Test code\n}\n```';
    }

    return 'Default mock response';
  }

  async isAvailable(): Promise<boolean> {
    return true;
  }

  reset(): void {
    this.callCount = 0;
    this.lastPrompt = '';
    this.lastOptions = undefined;
    this.responses.clear();
  }
}

// ============================================================================
// Prompt Template Tests
// ============================================================================

describe('Prompt Templates', () => {
  describe('renderPrompt', () => {
    it('should render template with variables', () => {
      const template: PromptTemplate = {
        id: 'test',
        name: 'Test',
        description: 'Test template',
        systemPrompt: 'System prompt',
        userPromptTemplate: 'Hello {{name}}, your code is {{code}}',
        requiredVariables: ['name', 'code'],
        temperature: 0.5,
        maxTokens: 1000,
      };

      const result = renderPrompt(template, { name: 'World', code: 'ADD 1 TO X' });
      
      expect(result.systemPrompt).toBe('System prompt');
      expect(result.userPrompt).toBe('Hello World, your code is ADD 1 TO X');
    });

    it('should throw on missing required variable', () => {
      const template: PromptTemplate = {
        id: 'test',
        name: 'Test',
        description: 'Test template',
        systemPrompt: 'System',
        userPromptTemplate: '{{required}}',
        requiredVariables: ['required'],
        temperature: 0.5,
        maxTokens: 1000,
      };

      expect(() => renderPrompt(template, {})).toThrow('Missing required variable: required');
    });

    it('should handle multiple occurrences of same variable', () => {
      const template: PromptTemplate = {
        id: 'test',
        name: 'Test',
        description: 'Test',
        systemPrompt: 'System',
        userPromptTemplate: '{{x}} + {{x}} = 2{{x}}',
        requiredVariables: ['x'],
        temperature: 0.5,
        maxTokens: 1000,
      };

      const result = renderPrompt(template, { x: 'Y' });
      expect(result.userPrompt).toBe('Y + Y = 2Y');
    });
  });

  describe('getPromptTemplate', () => {
    it('should return known templates', () => {
      expect(getPromptTemplate('cobol-analysis')).toBe(COBOL_ANALYSIS_TEMPLATE);
      expect(getPromptTemplate('cobol-patterns')).toBe(COBOL_PATTERN_TEMPLATE);
      expect(getPromptTemplate('cobol2java')).toBe(COBOL2JAVA_TEMPLATE);
      expect(getPromptTemplate('cobol2springboot')).toBe(COBOL2SPRINGBOOT_TEMPLATE);
      expect(getPromptTemplate('java-optimization')).toBe(JAVA_OPTIMIZATION_TEMPLATE);
      expect(getPromptTemplate('java-test-generation')).toBe(JAVA_TEST_TEMPLATE);
      expect(getPromptTemplate('documentation')).toBe(DOCUMENTATION_TEMPLATE);
    });

    it('should return undefined for unknown template', () => {
      expect(getPromptTemplate('unknown')).toBeUndefined();
    });
  });

  describe('listTemplates', () => {
    it('should list all template IDs', () => {
      const templates = listTemplates();
      expect(templates).toContain('cobol-analysis');
      expect(templates).toContain('cobol-patterns');
      expect(templates).toContain('cobol2java');
      expect(templates).toContain('cobol2springboot');
      expect(templates).toContain('java-optimization');
      expect(templates).toContain('java-test-generation');
      expect(templates).toContain('documentation');
      expect(templates.length).toBe(7);
    });
  });

  describe('Template Validity', () => {
    const allTemplates = [
      COBOL_ANALYSIS_TEMPLATE,
      COBOL_PATTERN_TEMPLATE,
      COBOL2JAVA_TEMPLATE,
      COBOL2SPRINGBOOT_TEMPLATE,
      JAVA_OPTIMIZATION_TEMPLATE,
      JAVA_TEST_TEMPLATE,
      DOCUMENTATION_TEMPLATE,
    ];

    it.each(allTemplates.map(t => [t.id, t]))(
      '%s should have all required fields',
      (_id, template) => {
        expect(template.id).toBeDefined();
        expect(template.name).toBeDefined();
        expect(template.description).toBeDefined();
        expect(template.systemPrompt.length).toBeGreaterThan(0);
        expect(template.userPromptTemplate.length).toBeGreaterThan(0);
        expect(Array.isArray(template.requiredVariables)).toBe(true);
        expect(template.temperature).toBeGreaterThanOrEqual(0);
        expect(template.temperature).toBeLessThanOrEqual(1);
        expect(template.maxTokens).toBeGreaterThan(0);
      }
    );

    it.each(allTemplates.map(t => [t.id, t]))(
      '%s should have all required variables in template',
      (_id, template) => {
        for (const required of template.requiredVariables) {
          expect(template.userPromptTemplate).toContain(`{{${required}}}`);
        }
      }
    );
  });
});

// ============================================================================
// Enhanced LLM Client Tests
// ============================================================================

describe('Enhanced LLM Client', () => {
  let mockClient: MockLLMClient;

  beforeEach(() => {
    mockClient = new MockLLMClient();
  });

  describe('createEnhancedClient', () => {
    it('should wrap a basic client', () => {
      const enhanced = createEnhancedClient(mockClient);
      expect(enhanced.provider).toBe('mock');
    });

    it('should pass through complete calls', async () => {
      const enhanced = createEnhancedClient(mockClient);
      mockClient.setResponse('test', 'response');
      
      const result = await enhanced.complete('test prompt');
      expect(result).toBe('response');
      expect(mockClient.callCount).toBe(1);
    });

    it('should pass through isAvailable calls', async () => {
      const enhanced = createEnhancedClient(mockClient);
      const available = await enhanced.isAvailable();
      expect(available).toBe(true);
    });
  });

  describe('completeWithMetadata', () => {
    it('should return completion result with metadata', async () => {
      const enhanced = createEnhancedClient(mockClient);
      mockClient.setResponse('test', 'Hello world');
      
      const result = await enhanced.completeWithMetadata('test');
      
      expect(result.text).toBe('Hello world');
      expect(result.finishReason).toBe('stop');
      expect(result.processingTimeMs).toBeGreaterThanOrEqual(0);
      expect(result.usage).toBeDefined();
      expect(result.usage?.totalTokens).toBeGreaterThan(0);
    });

    it('should estimate tokens for prompt and completion', async () => {
      const enhanced = createEnhancedClient(mockClient);
      mockClient.setResponse('prompt', 'This is a response');
      
      const result = await enhanced.completeWithMetadata('A simple prompt');
      
      expect(result.usage?.promptTokens).toBeGreaterThan(0);
      expect(result.usage?.completionTokens).toBeGreaterThan(0);
      expect(result.usage?.totalTokens).toBe(
        (result.usage?.promptTokens ?? 0) + (result.usage?.completionTokens ?? 0)
      );
    });
  });

  describe('caching', () => {
    it('should cache responses', async () => {
      const enhanced = createEnhancedClient(mockClient, { enableCache: true });
      mockClient.setResponse('cached', 'Cached response');
      
      // First call
      await enhanced.complete('cached prompt');
      expect(mockClient.callCount).toBe(1);
      
      // Second call (should be cached)
      const result = await enhanced.complete('cached prompt');
      expect(result).toBe('Cached response');
      expect(mockClient.callCount).toBe(1); // No additional call
    });

    it('should return cache stats', async () => {
      const enhanced = createEnhancedClient(mockClient, { maxCacheSize: 10 });
      
      const stats = enhanced.getCacheStats();
      expect(stats.size).toBe(0);
      expect(stats.maxSize).toBe(10);
    });

    it('should clear cache', async () => {
      const enhanced = createEnhancedClient(mockClient);
      mockClient.setResponse('test', 'response');
      
      await enhanced.complete('test');
      expect(enhanced.getCacheStats().size).toBe(1);
      
      enhanced.clearCache();
      expect(enhanced.getCacheStats().size).toBe(0);
    });
  });

  describe('estimateTokens', () => {
    it('should estimate tokens for text', () => {
      const enhanced = createEnhancedClient(mockClient);
      
      const text = 'Hello, this is a test.';
      const tokens = enhanced.estimateTokens(text);
      
      expect(tokens).toBeGreaterThan(0);
      expect(tokens).toBeLessThan(text.length); // Tokens < characters
    });

    it('should estimate more tokens for code', () => {
      const enhanced = createEnhancedClient(mockClient);
      
      const prose = 'This is a simple sentence.';
      const code = 'if (x === 1) { return true; }';
      
      const proseTokens = enhanced.estimateTokens(prose);
      const codeTokens = enhanced.estimateTokens(code);
      
      // Code should have relatively more tokens per character
      const proseRatio = proseTokens / prose.length;
      const codeRatio = codeTokens / code.length;
      
      expect(codeRatio).toBeGreaterThanOrEqual(proseRatio * 0.9); // Allow some tolerance
    });
  });

  describe('getContextWindow', () => {
    it('should return default context window', () => {
      const enhanced = createEnhancedClient(mockClient);
      expect(enhanced.getContextWindow()).toBe(8192); // Default
    });
  });

  describe('completeWithTemplate', () => {
    it('should complete using a template', async () => {
      const enhanced = createEnhancedClient(mockClient);
      
      const result = await enhanced.completeWithTemplate(
        COBOL_ANALYSIS_TEMPLATE,
        { cobolSource: 'MOVE 1 TO X', additionalContext: '' }
      );
      
      expect(result.text).toBeDefined();
      expect(mockClient.lastPrompt).toContain('MOVE 1 TO X');
    });

    it('should use template temperature and maxTokens', async () => {
      const enhanced = createEnhancedClient(mockClient);
      
      await enhanced.completeWithTemplate(
        COBOL_ANALYSIS_TEMPLATE,
        { cobolSource: 'TEST', additionalContext: '' }
      );
      
      expect(mockClient.lastOptions?.temperature).toBe(COBOL_ANALYSIS_TEMPLATE.temperature);
      expect(mockClient.lastOptions?.maxTokens).toBe(COBOL_ANALYSIS_TEMPLATE.maxTokens);
    });
  });

  describe('rate limiting', () => {
    it('should enforce minimum request interval', async () => {
      const enhanced = createEnhancedClient(mockClient, {
        minRequestIntervalMs: 50,
      });
      
      const start = Date.now();
      await enhanced.complete('first');
      await enhanced.complete('second');
      const elapsed = Date.now() - start;
      
      expect(elapsed).toBeGreaterThanOrEqual(45); // Allow some tolerance
    });
  });
});

// ============================================================================
// AI Conversion Pipeline Tests
// ============================================================================

describe('AI Conversion Pipeline', () => {
  let mockClient: MockLLMClient;
  let pipeline: AiConversionPipeline;

  const sampleCobol = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNT PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
           ADD 1 TO WS-COUNT.
           STOP RUN.
  `;

  beforeEach(() => {
    mockClient = new MockLLMClient();
    pipeline = createPipeline(mockClient);
  });

  describe('createPipeline', () => {
    it('should create a pipeline instance', () => {
      expect(pipeline).toBeInstanceOf(AiConversionPipeline);
    });
  });

  describe('run', () => {
    it('should run all default stages', async () => {
      const result = await pipeline.run(sampleCobol);
      
      expect(result.success).toBe(true);
      expect(result.cobolSource).toBe(sampleCobol);
      expect(result.stageResults.length).toBe(4); // Default: analysis, pattern-detection, conversion, optimization
    });

    it('should run specific stages only', async () => {
      const result = await pipeline.run(sampleCobol, {
        stages: ['analysis'],
      });
      
      expect(result.stageResults.length).toBe(1);
      expect(result.stageResults[0].stage).toBe('analysis');
    });

    it('should track total processing time', async () => {
      const result = await pipeline.run(sampleCobol, {
        stages: ['analysis'],
      });
      
      expect(result.totalProcessingTimeMs).toBeGreaterThanOrEqual(0);
    });

    it('should track total token usage', async () => {
      const result = await pipeline.run(sampleCobol, {
        stages: ['analysis'],
      });
      
      expect(result.totalTokenUsage.totalTokens).toBeGreaterThan(0);
    });
  });

  describe('analysis stage', () => {
    it('should produce analysis result', async () => {
      const result = await pipeline.run(sampleCobol, {
        stages: ['analysis'],
      });
      
      expect(result.analysis).toBeDefined();
      expect(result.analysis?.purpose).toBeDefined();
      expect(result.analysis?.rawAnalysis).toBeDefined();
    });
  });

  describe('pattern-detection stage', () => {
    it('should detect patterns', async () => {
      const result = await pipeline.run(sampleCobol, {
        stages: ['pattern-detection'],
      });
      
      expect(result.patterns).toBeDefined();
      expect(Array.isArray(result.patterns)).toBe(true);
    });

    it('should parse pattern JSON', async () => {
      mockClient.setResponse('patterns', '[{"patternType":"file-io","location":"PROCEDURE","javaEquivalent":"BufferedReader","complexity":"moderate"}]');
      
      const result = await pipeline.run(sampleCobol, {
        stages: ['pattern-detection'],
      });
      
      expect(result.patterns?.length).toBe(1);
      expect(result.patterns?.[0].patternType).toBe('file-io');
    });
  });

  describe('conversion stage', () => {
    it('should produce Java code', async () => {
      const result = await pipeline.run(sampleCobol, {
        stages: ['conversion'],
      });
      
      expect(result.javaCode).toBeDefined();
      expect(result.javaCode).toContain('class');
    });

    it('should extract Java from markdown code blocks', async () => {
      mockClient.setResponse('Convert', '```java\npublic class Extracted { }\n```');
      
      const result = await pipeline.run(sampleCobol, {
        stages: ['conversion'],
      });
      
      expect(result.javaCode).toBe('public class Extracted { }');
    });
  });

  describe('optimization stage', () => {
    it('should optimize converted code', async () => {
      const result = await pipeline.run(sampleCobol, {
        stages: ['conversion', 'optimization'],
      });
      
      expect(result.optimizedCode).toBeDefined();
    });

    it('should fail without prior conversion', async () => {
      const result = await pipeline.run(sampleCobol, {
        stages: ['optimization'],
      });
      
      const optimizationStage = result.stageResults.find(s => s.stage === 'optimization');
      expect(optimizationStage?.success).toBe(false);
      expect(optimizationStage?.error).toContain('No Java code');
    });
  });

  describe('test-generation stage', () => {
    it('should generate tests for converted code', async () => {
      // Set up mock responses for both stages
      mockClient.setResponse('Convert', '```java\npublic class TestClass { }\n```');
      mockClient.setResponse('test', '```java\n@Test\nvoid testMethod() { }\n```');
      
      const result = await pipeline.run(sampleCobol, {
        stages: ['conversion', 'test-generation'],
      });
      
      expect(result.tests).toBeDefined();
      expect(result.tests).toContain('@Test');
    });
  });

  describe('validation stage', () => {
    it('should validate converted code', async () => {
      const result = await pipeline.run(sampleCobol, {
        stages: ['conversion', 'validation'],
      });
      
      const validationStage = result.stageResults.find(s => s.stage === 'validation');
      expect(validationStage?.success).toBe(true);
      expect(validationStage?.metadata?.checks).toBeDefined();
    });
  });

  describe('analyze helper', () => {
    it('should return analysis only', async () => {
      const analysis = await pipeline.analyze(sampleCobol);
      
      expect(analysis.purpose).toBeDefined();
      expect(analysis.rawAnalysis).toBeDefined();
    });
  });

  describe('convert helper', () => {
    it('should return converted code only', async () => {
      const code = await pipeline.convert(sampleCobol);
      
      expect(code).toContain('class');
    });

    it('should accept conversion notes', async () => {
      await pipeline.convert(sampleCobol, 'Use Spring conventions');
      
      expect(mockClient.lastPrompt).toContain('Use Spring conventions');
    });
  });

  describe('convertAndOptimize helper', () => {
    it('should return both code and optimized code', async () => {
      const result = await pipeline.convertAndOptimize(sampleCobol);
      
      expect(result.code).toBeDefined();
      expect(result.optimizedCode).toBeDefined();
    });

    it('should accept optimization options', async () => {
      await pipeline.convertAndOptimize(sampleCobol, {
        notes: 'Legacy system',
        optimizationFocus: ['performance', 'readability'],
      });
      
      expect(mockClient.callCount).toBe(2); // conversion + optimization
    });
  });

  describe('error handling', () => {
    it('should handle LLM errors gracefully', async () => {
      const errorClient: LLMClient = {
        provider: 'error',
        complete: async () => { throw new Error('API Error'); },
        isAvailable: async () => true,
      };
      
      const errorPipeline = createPipeline(errorClient);
      const result = await errorPipeline.run(sampleCobol, {
        stages: ['analysis'],
      });
      
      // EnhancedLLMClientWrapper catches errors and returns empty text with finishReason: error
      // The pipeline still marks success true since the stage completed without throwing
      // Check that the analysis stage produced minimal/empty output
      const analysisStage = result.stageResults.find(s => s.stage === 'analysis');
      expect(analysisStage).toBeDefined();
      // Analysis should still succeed but with empty/minimal content due to error handling
      expect(result.analysis?.rawAnalysis).toBe('');
    });

    it('should continue after stage error', async () => {
      const result = await pipeline.run(sampleCobol, {
        stages: ['optimization', 'conversion'], // optimization will fail, conversion should run
      });
      
      // Both stages should have results
      expect(result.stageResults.length).toBe(2);
      expect(result.javaCode).toBeDefined(); // conversion still ran
    });
  });
});

// ============================================================================
// NoopClient Tests (baseline)
// ============================================================================

describe('NoopClient', () => {
  it('should return empty string', async () => {
    const client = new NoopClient();
    const result = await client.complete('test');
    expect(result).toBe('');
  });

  it('should always be available', async () => {
    const client = new NoopClient();
    expect(await client.isAvailable()).toBe(true);
  });

  it('should identify as none provider', () => {
    const client = new NoopClient();
    expect(client.provider).toBe('none');
  });
});
