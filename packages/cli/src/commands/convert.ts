/**
 * Convert command
 * 
 * cobol2java convert <input> [options]
 */

import { Command } from 'commander';
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { convert, type ConversionOptions, formatError } from '@cobol2java/core';

export const convertCommand = new Command('convert')
  .description('Convert COBOL source file to Java')
  .argument('<input>', 'COBOL source file or directory')
  .option('-o, --output <dir>', 'Output directory', './output')
  .option('--llm <provider>', 'LLM provider (openai, claude, ollama, copilot, none)', 'none')
  .option('--model <model>', 'LLM model name')
  .option('--api-key <key>', 'LLM API key (or use env var)')
  .option('-p, --package <name>', 'Java package name', 'com.example')
  .option('--java-version <version>', 'Target Java version', '17')
  .option('--spring-boot', 'Generate Spring Boot compatible code')
  .option('--generate-tests', 'Generate unit tests')
  .option('--strict', 'Fail on warnings')
  .option('--verbose', 'Verbose output')
  .action(async (input: string, options) => {
    try {
      console.log(`Converting ${input}...`);

      // Read COBOL source
      const cobolSource = await readFile(input, 'utf-8');

      // Build conversion options
      const conversionOptions: ConversionOptions = {
        llmProvider: options.llm,
        llmModel: options.model,
        llmApiKey: options.apiKey || process.env.OPENAI_API_KEY || process.env.ANTHROPIC_API_KEY,
        packageName: options.package,
        javaVersion: parseInt(options.javaVersion, 10) as 11 | 17 | 21,
        springBoot: options.springBoot || false,
        generateTests: options.generateTests || false,
        strict: options.strict || false,
      };

      // Convert
      const result = await convert(cobolSource, conversionOptions);

      // Create output directory
      await mkdir(options.output, { recursive: true });

      // Generate output filename
      const outputFilename = `${result.className}.java`;
      const outputPath = join(options.output, outputFilename);

      // Write output
      await writeFile(outputPath, result.java, 'utf-8');

      console.log(`✓ Generated ${outputPath}`);

      // Report errors/warnings
      if (result.errors.length > 0) {
        console.log('\nErrors:');
        result.errors.forEach(err => console.log(`  ${formatError(err)}`));
      }

      if (result.warnings.length > 0) {
        console.log('\nWarnings:');
        result.warnings.forEach(warn => console.log(`  ${formatError(warn)}`));
      }

      // Report metadata
      if (options.verbose) {
        console.log('\nMetadata:');
        console.log(`  Program: ${result.metadata.programName}`);
        console.log(`  Lines: ${result.metadata.linesConverted}`);
        console.log(`  Duration: ${result.metadata.durationMs}ms`);
        console.log(`  LLM: ${result.metadata.llmProvider}`);
      }

      // Exit with error code if there were errors
      if (result.errors.length > 0) {
        process.exit(1);
      }
    } catch (error) {
      console.error(`Error: ${error instanceof Error ? error.message : error}`);
      process.exit(1);
    }
  });
