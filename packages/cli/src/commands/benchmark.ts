/**
 * Benchmark command
 * 
 * cobol2java benchmark [options]
 */

import { Command } from 'commander';
import { readFile, readdir, writeFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { execSync } from 'node:child_process';
import { convert, type ConversionOptions } from '@cobol2java/core';

interface BenchmarkResult {
  name: string;
  converted: boolean;
  compiled: boolean;
  executed: boolean;
  errors: string[];
  executionTime: number;
  output?: string;
}

export const benchmarkCommand = new Command('benchmark')
  .description('Run COBOLEval benchmark suite')
  .option('-d, --dataset <path>', 'Path to COBOLEval dataset', './storage/cobol-samples/COBOLEval/data/CobolEval.jsonl')
  .option('-e, --examples <path>', 'Path to examples directory', './examples')
  .option('--llm <provider>', 'LLM provider', 'none')
  .option('--model <model>', 'LLM model name')
  .option('-n, --limit <count>', 'Limit number of problems to run')
  .option('--verbose', 'Verbose output')
  .option('--mode <mode>', 'Benchmark mode: examples or coboleval', 'examples')
  .action(async (options) => {
    try {
      if (options.mode === 'examples') {
        await runExamplesBenchmark(options);
      } else {
        await runCOBOLEvalBenchmark(options);
      }
    } catch (error) {
      console.error(`Error: ${error instanceof Error ? error.message : error}`);
      process.exit(1);
    }
  });

async function runExamplesBenchmark(options: {
  examples: string;
  llm: string;
  model?: string;
  verbose?: boolean;
}) {
  console.log('Running Examples Benchmark...\n');
  console.log(`Examples directory: ${options.examples}`);
  console.log(`LLM: ${options.llm}`);
  console.log('');

  const files = await readdir(options.examples);
  const cobolFiles = files.filter(f => f.endsWith('.cbl') || f.endsWith('.cob'));

  console.log(`Files: ${cobolFiles.length}`);
  console.log('─'.repeat(60));

  const results: BenchmarkResult[] = [];
  const tempDir = './benchmark-output';
  await mkdir(tempDir, { recursive: true });
  await mkdir(join(tempDir, 'com', 'example'), { recursive: true });

  const conversionOptions: ConversionOptions = {
    llmProvider: options.llm as 'none' | 'openai' | 'claude' | 'ollama' | 'copilot',
    llmModel: options.model,
    packageName: 'com.example',
  };

  for (const file of cobolFiles) {
    const startTime = Date.now();
    const result: BenchmarkResult = {
      name: file,
      converted: false,
      compiled: false,
      executed: false,
      errors: [],
      executionTime: 0,
    };

    try {
      // Read COBOL source
      const cobolSource = await readFile(join(options.examples, file), 'utf-8');

      // Convert
      const conversionResult = await convert(cobolSource, conversionOptions);

      if (conversionResult.errors.length > 0) {
        result.errors.push(...conversionResult.errors.map(e => `Convert: ${e.message}`));
      } else {
        result.converted = true;

        // Write Java file
        const javaPath = join(tempDir, 'com', 'example', `${conversionResult.className}.java`);
        await writeFile(javaPath, conversionResult.java, 'utf-8');

        // Try to compile
        try {
          execSync(`javac ${javaPath}`, { stdio: 'pipe' });
          result.compiled = true;

          // Try to execute
          try {
            const className = `com.example.${conversionResult.className}`;
            const output = execSync(`java -cp ${tempDir} ${className}`, { 
              stdio: 'pipe',
              timeout: 10000,
            });
            result.executed = true;
            result.output = output.toString().trim().slice(0, 200);
          } catch (execError: unknown) {
            if (execError instanceof Error) {
              result.errors.push(`Execute: ${execError.message.slice(0, 100)}`);
            }
          }
        } catch (compileError: unknown) {
          if (compileError instanceof Error) {
            result.errors.push(`Compile: ${compileError.message.slice(0, 100)}`);
          }
        }
      }
    } catch (error: unknown) {
      if (error instanceof Error) {
        result.errors.push(`Error: ${error.message}`);
      }
    }

    result.executionTime = Date.now() - startTime;
    results.push(result);

    // Print result
    const status = result.executed ? '✓✓✓' : result.compiled ? '✓✓✗' : result.converted ? '✓✗✗' : '✗✗✗';
    console.log(`${status} ${file.padEnd(25)} (${result.executionTime}ms)`);
    if (options.verbose && result.errors.length > 0) {
      result.errors.forEach(e => console.log(`    ${e}`));
    }
    if (options.verbose && result.output) {
      console.log(`    Output: ${result.output.replace(/\n/g, ' | ')}`);
    }
  }

  // Summary
  const converted = results.filter(r => r.converted).length;
  const compiled = results.filter(r => r.compiled).length;
  const executed = results.filter(r => r.executed).length;

  console.log('─'.repeat(60));
  console.log('\nBenchmark Results:');
  console.log(`  Total:      ${results.length}`);
  console.log(`  Converted:  ${converted} (${((converted / results.length) * 100).toFixed(1)}%)`);
  console.log(`  Compiled:   ${compiled} (${((compiled / results.length) * 100).toFixed(1)}%)`);
  console.log(`  Executed:   ${executed} (${((executed / results.length) * 100).toFixed(1)}%)`);
  console.log(`\n  Pass@1:     ${((executed / results.length) * 100).toFixed(1)}%`);
}

async function runCOBOLEvalBenchmark(options: {
  dataset: string;
  llm: string;
  model?: string;
  limit?: string;
  verbose?: boolean;
}) {
  console.log('Running COBOLEval benchmark...\n');
  console.log(`Dataset: ${options.dataset}`);
  console.log(`LLM: ${options.llm}`);
  console.log('');

  // Read dataset
  const datasetContent = await readFile(options.dataset, 'utf-8');
  const problems = datasetContent
    .split('\n')
    .filter(line => line.trim())
    .map(line => JSON.parse(line) as {
      task_id: string;
      prompt: string;
      canonical_solution?: string;
      test?: string;
    });

  const limit = options.limit ? parseInt(options.limit, 10) : problems.length;
  const testProblems = problems.slice(0, limit);

  console.log(`Problems: ${testProblems.length} / ${problems.length}`);
  console.log('─'.repeat(50));

  let passed = 0;
  let failed = 0;

  const conversionOptions: ConversionOptions = {
    llmProvider: options.llm as 'none' | 'openai' | 'claude' | 'ollama' | 'copilot',
    llmModel: options.model,
  };

  for (const problem of testProblems) {
    try {
      // Extract COBOL code from prompt
      const cobolMatch = problem.prompt.match(/```cobol([\s\S]*?)```/i);
      const cobolSource = cobolMatch?.[1]?.trim() || problem.prompt;

      const result = await convert(cobolSource, conversionOptions);

      if (result.errors.length === 0) {
        passed++;
        if (options.verbose) {
          console.log(`✓ ${problem.task_id}`);
        }
      } else {
        failed++;
        if (options.verbose) {
          console.log(`✗ ${problem.task_id}: ${result.errors[0]?.message}`);
        }
      }
    } catch (error) {
      failed++;
      if (options.verbose) {
        console.log(`✗ ${problem.task_id}: ${error instanceof Error ? error.message : 'Unknown error'}`);
      }
    }
  }

  // Summary
  console.log('─'.repeat(50));
  console.log('\nResults:');
  console.log(`  Total:   ${testProblems.length}`);
  console.log(`  Passed:  ${passed}`);
  console.log(`  Failed:  ${failed}`);
  console.log(`  Pass@1:  ${((passed / testProblems.length) * 100).toFixed(1)}%`);
}
