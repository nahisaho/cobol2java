/**
 * Validate command
 * 
 * cobol2java validate <input>
 */

import { Command } from 'commander';
import { readFile } from 'node:fs/promises';
import { CobolParser, formatError, type ErrorInfo } from '@cobol2java/core';

export const validateCommand = new Command('validate')
  .description('Validate COBOL source file syntax')
  .argument('<input>', 'COBOL source file')
  .option('--strict', 'Treat warnings as errors')
  .action(async (input: string, options) => {
    try {
      console.log(`Validating ${input}...`);

      // Read COBOL source
      const cobolSource = await readFile(input, 'utf-8');

      // Parse
      const parser = new CobolParser();
      const ast = parser.parse(cobolSource);

      // Report results
      if (ast.errors.length === 0) {
        console.log('✓ No errors found');
        process.exit(0);
      }

      const errors = ast.errors.filter((e: ErrorInfo) => e.severity === 'fatal' || e.severity === 'error');
      const warnings = ast.errors.filter((e: ErrorInfo) => e.severity === 'warning');

      if (errors.length > 0) {
        console.log('\nErrors:');
        errors.forEach((err: ErrorInfo) => console.log(`  ${formatError(err)}`));
      }

      if (warnings.length > 0) {
        console.log('\nWarnings:');
        warnings.forEach((warn: ErrorInfo) => console.log(`  ${formatError(warn)}`));
      }

      // Exit code
      if (errors.length > 0 || (options.strict && warnings.length > 0)) {
        process.exit(1);
      }
    } catch (error) {
      console.error(`Error: ${error instanceof Error ? error.message : error}`);
      process.exit(1);
    }
  });
