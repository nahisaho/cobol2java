/**
 * cobol2java CLI
 * 
 * Command-line interface for COBOL to Java conversion
 */

import { Command } from 'commander';
import { convertCommand } from './commands/convert.js';
import { validateCommand } from './commands/validate.js';
import { benchmarkCommand } from './commands/benchmark.js';

const program = new Command();

program
  .name('cobol2java')
  .description('Convert COBOL source code to Java')
  .version('0.1.0');

// Register commands
program.addCommand(convertCommand);
program.addCommand(validateCommand);
program.addCommand(benchmarkCommand);

/**
 * Run the CLI
 */
export function run(): void {
  program.parse();
}

export { program };
