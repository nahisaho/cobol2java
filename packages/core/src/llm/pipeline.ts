/**
 * AI-Assisted Conversion Pipeline
 *
 * Integrates LLM capabilities into the COBOL-to-Java conversion process
 * for enhanced analysis, optimization, and quality assessment.
 */

import { type LLMClient, type CompletionOptions } from './client.js';
import {
  type EnhancedLLMClientWrapper,
  type CompletionResult,
  createEnhancedClient,
} from './enhanced.js';
import {
  COBOL_ANALYSIS_TEMPLATE,
  COBOL_PATTERN_TEMPLATE,
  COBOL2JAVA_TEMPLATE,
  JAVA_OPTIMIZATION_TEMPLATE,
  JAVA_TEST_TEMPLATE,
  type PromptTemplate,
  renderPrompt,
} from './prompts.js';

/**
 * Conversion pipeline stage
 */
export type PipelineStage =
  | 'analysis'
  | 'pattern-detection'
  | 'conversion'
  | 'optimization'
  | 'test-generation'
  | 'validation';

/**
 * Pipeline stage result
 */
export interface StageResult {
  /** Stage identifier */
  stage: PipelineStage;
  /** Stage succeeded */
  success: boolean;
  /** Stage output */
  output: string;
  /** Detailed metadata */
  metadata?: Record<string, unknown>;
  /** Processing time in ms */
  processingTimeMs: number;
  /** Token usage */
  tokenUsage?: {
    promptTokens: number;
    completionTokens: number;
    totalTokens: number;
  };
  /** Error message if failed */
  error?: string;
}

/**
 * COBOL Analysis Result
 */
export interface CobolAnalysis {
  /** Program name */
  programName?: string;
  /** Program purpose */
  purpose: string;
  /** Data structures found */
  dataStructures: Array<{
    name: string;
    type: string;
    description: string;
  }>;
  /** External dependencies */
  dependencies: Array<{
    type: 'file' | 'database' | 'program' | 'copybook';
    name: string;
    description: string;
  }>;
  /** Conversion challenges */
  challenges: Array<{
    type: string;
    description: string;
    severity: 'low' | 'medium' | 'high';
  }>;
  /** Recommended conversion approach */
  recommendedApproach: string;
  /** Raw analysis text */
  rawAnalysis: string;
}

/**
 * COBOL Pattern Detection Result
 */
export interface CobolPattern {
  /** Pattern type */
  patternType: string;
  /** Location in source */
  location: string;
  /** Recommended Java equivalent */
  javaEquivalent: string;
  /** Conversion complexity */
  complexity: 'simple' | 'moderate' | 'complex';
}

/**
 * Pipeline options
 */
export interface PipelineOptions {
  /** Stages to execute (default: all) */
  stages?: PipelineStage[];
  /** Enable caching */
  enableCache?: boolean;
  /** Target Spring Boot (affects conversion) */
  springBoot?: boolean;
  /** Spring Boot version */
  springVersion?: string;
  /** Target package name */
  packageName?: string;
  /** Additional conversion notes */
  conversionNotes?: string;
  /** Optimization focus areas */
  optimizationFocus?: string[];
  /** Test generation focus */
  testFocus?: string[];
}

/**
 * Full pipeline result
 */
export interface PipelineResult {
  /** Pipeline succeeded */
  success: boolean;
  /** Original COBOL source */
  cobolSource: string;
  /** COBOL analysis (if analysis stage ran) */
  analysis?: CobolAnalysis;
  /** Detected patterns (if pattern-detection stage ran) */
  patterns?: CobolPattern[];
  /** Converted Java code (if conversion stage ran) */
  javaCode?: string;
  /** Optimized Java code (if optimization stage ran) */
  optimizedCode?: string;
  /** Generated tests (if test-generation stage ran) */
  tests?: string;
  /** Stage results */
  stageResults: StageResult[];
  /** Total processing time */
  totalProcessingTimeMs: number;
  /** Total token usage */
  totalTokenUsage: {
    promptTokens: number;
    completionTokens: number;
    totalTokens: number;
  };
  /** Errors encountered */
  errors: string[];
}

/**
 * AI-Assisted Conversion Pipeline
 *
 * Orchestrates LLM-powered COBOL analysis and Java conversion.
 */
export class AiConversionPipeline {
  private readonly client: EnhancedLLMClientWrapper;

  constructor(llmClient: LLMClient) {
    this.client = createEnhancedClient(llmClient, {
      enableCache: true,
      maxCacheSize: 50,
    });
  }

  /**
   * Run the full conversion pipeline
   */
  async run(
    cobolSource: string,
    options: PipelineOptions = {}
  ): Promise<PipelineResult> {
    const startTime = Date.now();
    const stages = options.stages ?? [
      'analysis',
      'pattern-detection',
      'conversion',
      'optimization',
    ];

    const result: PipelineResult = {
      success: true,
      cobolSource,
      stageResults: [],
      totalProcessingTimeMs: 0,
      totalTokenUsage: {
        promptTokens: 0,
        completionTokens: 0,
        totalTokens: 0,
      },
      errors: [],
    };

    // Execute each stage
    for (const stage of stages) {
      const stageResult = await this.executeStage(stage, cobolSource, result, options);
      result.stageResults.push(stageResult);

      if (stageResult.tokenUsage) {
        result.totalTokenUsage.promptTokens += stageResult.tokenUsage.promptTokens;
        result.totalTokenUsage.completionTokens += stageResult.tokenUsage.completionTokens;
        result.totalTokenUsage.totalTokens += stageResult.tokenUsage.totalTokens;
      }

      if (!stageResult.success) {
        result.success = false;
        if (stageResult.error) {
          result.errors.push(`[${stage}] ${stageResult.error}`);
        }
        // Continue with other stages despite errors
      }
    }

    result.totalProcessingTimeMs = Date.now() - startTime;
    return result;
  }

  /**
   * Execute a single pipeline stage
   */
  private async executeStage(
    stage: PipelineStage,
    cobolSource: string,
    currentResult: PipelineResult,
    options: PipelineOptions
  ): Promise<StageResult> {
    const startTime = Date.now();

    try {
      switch (stage) {
        case 'analysis':
          return await this.runAnalysisStage(cobolSource, currentResult, startTime);

        case 'pattern-detection':
          return await this.runPatternDetectionStage(cobolSource, currentResult, startTime);

        case 'conversion':
          return await this.runConversionStage(cobolSource, currentResult, options, startTime);

        case 'optimization':
          return await this.runOptimizationStage(currentResult, options, startTime);

        case 'test-generation':
          return await this.runTestGenerationStage(currentResult, options, startTime);

        case 'validation':
          return await this.runValidationStage(currentResult, startTime);

        default:
          return {
            stage,
            success: false,
            output: '',
            processingTimeMs: Date.now() - startTime,
            error: `Unknown stage: ${stage}`,
          };
      }
    } catch (error) {
      return {
        stage,
        success: false,
        output: '',
        processingTimeMs: Date.now() - startTime,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  /**
   * Analysis stage - understand the COBOL program
   */
  private async runAnalysisStage(
    cobolSource: string,
    currentResult: PipelineResult,
    startTime: number
  ): Promise<StageResult> {
    const completion = await this.client.completeWithTemplate(
      COBOL_ANALYSIS_TEMPLATE,
      { cobolSource, additionalContext: '' }
    );

    // Parse analysis result
    const analysis = this.parseAnalysis(completion.text);
    currentResult.analysis = analysis;

    return {
      stage: 'analysis',
      success: true,
      output: completion.text,
      metadata: { analysis },
      processingTimeMs: Date.now() - startTime,
      tokenUsage: completion.usage,
    };
  }

  /**
   * Pattern detection stage - identify common patterns
   */
  private async runPatternDetectionStage(
    cobolSource: string,
    currentResult: PipelineResult,
    startTime: number
  ): Promise<StageResult> {
    const completion = await this.client.completeWithTemplate(
      COBOL_PATTERN_TEMPLATE,
      { cobolSource }
    );

    // Parse patterns JSON
    let patterns: CobolPattern[] = [];
    try {
      // Extract JSON from response
      const jsonMatch = completion.text.match(/\[[\s\S]*\]/);
      if (jsonMatch) {
        patterns = JSON.parse(jsonMatch[0]);
      }
    } catch {
      // Fallback: couldn't parse JSON
    }

    currentResult.patterns = patterns;

    return {
      stage: 'pattern-detection',
      success: true,
      output: completion.text,
      metadata: { patterns, patternCount: patterns.length },
      processingTimeMs: Date.now() - startTime,
      tokenUsage: completion.usage,
    };
  }

  /**
   * Conversion stage - convert COBOL to Java
   */
  private async runConversionStage(
    cobolSource: string,
    currentResult: PipelineResult,
    options: PipelineOptions,
    startTime: number
  ): Promise<StageResult> {
    const completion = await this.client.completeWithTemplate(
      COBOL2JAVA_TEMPLATE,
      {
        cobolSource,
        conversionNotes: options.conversionNotes ?? '',
      }
    );

    // Extract Java code
    const javaCode = this.extractCodeBlock(completion.text, 'java');
    currentResult.javaCode = javaCode || completion.text;

    return {
      stage: 'conversion',
      success: true,
      output: currentResult.javaCode,
      processingTimeMs: Date.now() - startTime,
      tokenUsage: completion.usage,
    };
  }

  /**
   * Optimization stage - improve the generated Java
   */
  private async runOptimizationStage(
    currentResult: PipelineResult,
    options: PipelineOptions,
    startTime: number
  ): Promise<StageResult> {
    if (!currentResult.javaCode) {
      return {
        stage: 'optimization',
        success: false,
        output: '',
        processingTimeMs: Date.now() - startTime,
        error: 'No Java code to optimize (run conversion stage first)',
      };
    }

    const completion = await this.client.completeWithTemplate(
      JAVA_OPTIMIZATION_TEMPLATE,
      {
        javaSource: currentResult.javaCode,
        focusAreas: options.optimizationFocus?.join(', ') ?? 'readability, performance',
      }
    );

    const optimizedCode = this.extractCodeBlock(completion.text, 'java');
    currentResult.optimizedCode = optimizedCode || completion.text;

    return {
      stage: 'optimization',
      success: true,
      output: currentResult.optimizedCode,
      processingTimeMs: Date.now() - startTime,
      tokenUsage: completion.usage,
    };
  }

  /**
   * Test generation stage - create JUnit tests
   */
  private async runTestGenerationStage(
    currentResult: PipelineResult,
    options: PipelineOptions,
    startTime: number
  ): Promise<StageResult> {
    const codeToTest = currentResult.optimizedCode || currentResult.javaCode;
    if (!codeToTest) {
      return {
        stage: 'test-generation',
        success: false,
        output: '',
        processingTimeMs: Date.now() - startTime,
        error: 'No Java code to test (run conversion stage first)',
      };
    }

    const completion = await this.client.completeWithTemplate(
      JAVA_TEST_TEMPLATE,
      {
        javaSource: codeToTest,
        testFocus: options.testFocus?.join(', ') ?? 'core functionality',
      }
    );

    const tests = this.extractCodeBlock(completion.text, 'java');
    currentResult.tests = tests || completion.text;

    return {
      stage: 'test-generation',
      success: true,
      output: currentResult.tests,
      processingTimeMs: Date.now() - startTime,
      tokenUsage: completion.usage,
    };
  }

  /**
   * Validation stage - verify the conversion
   */
  private async runValidationStage(
    currentResult: PipelineResult,
    startTime: number
  ): Promise<StageResult> {
    // Basic validation checks
    const checks: Array<{ name: string; passed: boolean; message: string }> = [];

    // Check Java code exists
    checks.push({
      name: 'java-code-generated',
      passed: !!currentResult.javaCode,
      message: currentResult.javaCode
        ? 'Java code was generated'
        : 'No Java code generated',
    });

    // Check for class declaration
    if (currentResult.javaCode) {
      checks.push({
        name: 'has-class-declaration',
        passed: /\bclass\s+\w+/.test(currentResult.javaCode),
        message: 'Contains class declaration',
      });

      // Check for valid Java structure
      checks.push({
        name: 'valid-structure',
        passed:
          currentResult.javaCode.includes('{') &&
          currentResult.javaCode.includes('}'),
        message: 'Has valid code structure',
      });
    }

    const allPassed = checks.every(c => c.passed);

    return {
      stage: 'validation',
      success: allPassed,
      output: JSON.stringify(checks, null, 2),
      metadata: { checks, allPassed },
      processingTimeMs: Date.now() - startTime,
    };
  }

  /**
   * Parse analysis text into structured format
   */
  private parseAnalysis(text: string): CobolAnalysis {
    return {
      purpose: this.extractSection(text, 'Program Overview') ||
               this.extractSection(text, 'Purpose') ||
               'Analysis pending',
      dataStructures: [],
      dependencies: [],
      challenges: [],
      recommendedApproach: this.extractSection(text, 'Recommended Approach') || '',
      rawAnalysis: text,
    };
  }

  /**
   * Extract a section from markdown-formatted text
   */
  private extractSection(text: string, heading: string): string | null {
    const regex = new RegExp(
      `(?:^|\\n)#+\\s*\\**${heading}\\**[:\\s]*([\\s\\S]*?)(?=\\n#+|$)`,
      'i'
    );
    const match = text.match(regex);
    return match ? match[1].trim() : null;
  }

  /**
   * Extract code block from markdown
   */
  private extractCodeBlock(text: string, language: string): string | null {
    const regex = new RegExp(`\`\`\`${language}\\s*([\\s\\S]*?)\`\`\``, 'i');
    const match = text.match(regex);
    return match ? match[1].trim() : null;
  }

  /**
   * Run analysis only
   */
  async analyze(cobolSource: string): Promise<CobolAnalysis> {
    const result = await this.run(cobolSource, { stages: ['analysis'] });
    return result.analysis ?? {
      purpose: 'Analysis failed',
      dataStructures: [],
      dependencies: [],
      challenges: [],
      recommendedApproach: '',
      rawAnalysis: '',
    };
  }

  /**
   * Run conversion only
   */
  async convert(cobolSource: string, notes?: string): Promise<string> {
    const result = await this.run(cobolSource, {
      stages: ['conversion'],
      conversionNotes: notes,
    });
    return result.javaCode ?? '';
  }

  /**
   * Run full conversion with optimization
   */
  async convertAndOptimize(
    cobolSource: string,
    options?: {
      notes?: string;
      optimizationFocus?: string[];
    }
  ): Promise<{ code: string; optimizedCode: string }> {
    const result = await this.run(cobolSource, {
      stages: ['conversion', 'optimization'],
      conversionNotes: options?.notes,
      optimizationFocus: options?.optimizationFocus,
    });
    return {
      code: result.javaCode ?? '',
      optimizedCode: result.optimizedCode ?? result.javaCode ?? '',
    };
  }
}

/**
 * Create an AI conversion pipeline
 */
export function createPipeline(client: LLMClient): AiConversionPipeline {
  return new AiConversionPipeline(client);
}
