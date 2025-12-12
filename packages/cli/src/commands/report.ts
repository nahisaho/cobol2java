/**
 * Report command
 * 
 * cobol2java report <input> [options]
 * 
 * Generate detailed error/warning reports for COBOL files
 */

import { Command } from 'commander';
import { readFile, readdir, writeFile, mkdir } from 'node:fs/promises';
import { join, basename } from 'node:path';
import { convert, type ConversionOptions, type ErrorInfo } from '@cobol2java/core';

interface ReportEntry {
  file: string;
  success: boolean;
  className: string;
  errors: ErrorInfo[];
  warnings: ErrorInfo[];
  metadata: {
    programName: string;
    linesConverted: number;
    durationMs: number;
  };
}

interface Report {
  timestamp: string;
  summary: {
    totalFiles: number;
    successfulFiles: number;
    failedFiles: number;
    totalErrors: number;
    totalWarnings: number;
    totalDurationMs: number;
  };
  entries: ReportEntry[];
}

export const reportCommand = new Command('report')
  .description('Generate detailed conversion report for COBOL files')
  .argument('<input>', 'COBOL source file or directory')
  .option('-o, --output <file>', 'Output report file')
  .option('--format <format>', 'Report format: text, json, or html', 'text')
  .option('--llm <provider>', 'LLM provider', 'none')
  .option('-p, --package <name>', 'Java package name', 'com.example')
  .option('--spring-boot', 'Generate Spring Boot compatible code')
  .option('--include-source', 'Include source code snippets in report')
  .action(async (input: string, options) => {
    try {
      const files = await getCobolFiles(input);
      
      if (files.length === 0) {
        console.error('No COBOL files found');
        process.exit(1);
      }

      const conversionOptions: ConversionOptions = {
        llmProvider: options.llm,
        packageName: options.package,
        springBoot: options.springBoot || false,
      };

      const entries: ReportEntry[] = [];
      let totalDurationMs = 0;

      for (const file of files) {
        const source = await readFile(file, 'utf-8');
        const result = await convert(source, conversionOptions);
        
        entries.push({
          file: basename(file),
          success: result.errors.length === 0,
          className: result.className,
          errors: result.errors,
          warnings: result.warnings,
          metadata: {
            programName: result.metadata.programName,
            linesConverted: result.metadata.linesConverted,
            durationMs: result.metadata.durationMs,
          },
        });

        totalDurationMs += result.metadata.durationMs;
      }

      const report: Report = {
        timestamp: new Date().toISOString(),
        summary: {
          totalFiles: entries.length,
          successfulFiles: entries.filter(e => e.success).length,
          failedFiles: entries.filter(e => !e.success).length,
          totalErrors: entries.reduce((sum, e) => sum + e.errors.length, 0),
          totalWarnings: entries.reduce((sum, e) => sum + e.warnings.length, 0),
          totalDurationMs,
        },
        entries,
      };

      // Output report
      let output: string;
      switch (options.format) {
        case 'json':
          output = formatJsonReport(report);
          break;
        case 'html':
          output = formatHtmlReport(report);
          break;
        default:
          output = formatTextReport(report);
      }

      if (options.output) {
        const dir = options.output.includes('/') ? options.output.substring(0, options.output.lastIndexOf('/')) : '.';
        await mkdir(dir, { recursive: true });
        await writeFile(options.output, output, 'utf-8');
        console.log(`Report written to ${options.output}`);
      } else {
        console.log(output);
      }

    } catch (error) {
      console.error(`Error: ${error instanceof Error ? error.message : error}`);
      process.exit(1);
    }
  });

async function getCobolFiles(input: string): Promise<string[]> {
  try {
    const stats = await readFile(input, 'utf-8').then(() => 'file').catch(() => 'dir');
    if (stats === 'file') {
      return [input];
    }
  } catch {
    // It's a directory
  }

  try {
    const files = await readdir(input);
    return files
      .filter(f => f.endsWith('.cbl') || f.endsWith('.cob') || f.endsWith('.cobol'))
      .map(f => join(input, f));
  } catch {
    return [input];
  }
}

function formatJsonReport(report: Report): string {
  return JSON.stringify(report, null, 2);
}

function formatTextReport(report: Report): string {
  const lines: string[] = [];
  
  lines.push('═'.repeat(60));
  lines.push('COBOL2Java Conversion Report');
  lines.push('═'.repeat(60));
  lines.push(`Generated: ${report.timestamp}`);
  lines.push('');
  
  // Summary
  lines.push('SUMMARY');
  lines.push('─'.repeat(40));
  lines.push(`Total Files:      ${report.summary.totalFiles}`);
  lines.push(`Successful:       ${report.summary.successfulFiles} (${((report.summary.successfulFiles / report.summary.totalFiles) * 100).toFixed(1)}%)`);
  lines.push(`Failed:           ${report.summary.failedFiles}`);
  lines.push(`Total Errors:     ${report.summary.totalErrors}`);
  lines.push(`Total Warnings:   ${report.summary.totalWarnings}`);
  lines.push(`Total Duration:   ${report.summary.totalDurationMs}ms`);
  lines.push('');

  // Details
  lines.push('FILE DETAILS');
  lines.push('─'.repeat(40));
  
  for (const entry of report.entries) {
    const status = entry.success ? '✓' : '✗';
    lines.push(`${status} ${entry.file} → ${entry.className}.java (${entry.metadata.durationMs}ms)`);
    
    if (entry.errors.length > 0) {
      for (const err of entry.errors) {
        lines.push(`    [ERROR] ${err.code}: ${err.message}`);
        if (err.line) {
          lines.push(`            at line ${err.line}${err.column ? `:${err.column}` : ''}`);
        }
        if (err.suggestion) {
          lines.push(`            Suggestion: ${err.suggestion}`);
        }
      }
    }
    
    if (entry.warnings.length > 0) {
      for (const warn of entry.warnings) {
        lines.push(`    [WARN]  ${warn.code}: ${warn.message}`);
      }
    }
  }

  lines.push('');
  lines.push('═'.repeat(60));
  
  return lines.join('\n');
}

function formatHtmlReport(report: Report): string {
  const successRate = ((report.summary.successfulFiles / report.summary.totalFiles) * 100).toFixed(1);
  
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>COBOL2Java Conversion Report</title>
  <style>
    :root {
      --bg: #1e1e1e;
      --fg: #d4d4d4;
      --accent: #569cd6;
      --success: #4ec9b0;
      --error: #f14c4c;
      --warning: #cca700;
      --border: #3c3c3c;
    }
    body {
      font-family: 'Segoe UI', system-ui, sans-serif;
      background: var(--bg);
      color: var(--fg);
      margin: 0;
      padding: 20px;
      line-height: 1.6;
    }
    .container {
      max-width: 1200px;
      margin: 0 auto;
    }
    h1 {
      color: var(--accent);
      border-bottom: 2px solid var(--accent);
      padding-bottom: 10px;
    }
    .summary {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
      gap: 20px;
      margin: 20px 0;
    }
    .stat {
      background: #2d2d2d;
      border-radius: 8px;
      padding: 20px;
      text-align: center;
    }
    .stat-value {
      font-size: 2em;
      font-weight: bold;
      color: var(--accent);
    }
    .stat-label {
      color: #888;
      font-size: 0.9em;
    }
    .success .stat-value { color: var(--success); }
    .error .stat-value { color: var(--error); }
    .warning .stat-value { color: var(--warning); }
    table {
      width: 100%;
      border-collapse: collapse;
      margin: 20px 0;
    }
    th, td {
      padding: 12px;
      text-align: left;
      border-bottom: 1px solid var(--border);
    }
    th {
      background: #2d2d2d;
      color: var(--accent);
    }
    tr:hover { background: #2a2a2a; }
    .status-success { color: var(--success); }
    .status-failed { color: var(--error); }
    .error-list {
      font-size: 0.9em;
      color: var(--error);
      margin: 5px 0;
    }
    .warning-list {
      font-size: 0.9em;
      color: var(--warning);
    }
    .timestamp {
      color: #888;
      font-size: 0.85em;
    }
  </style>
</head>
<body>
  <div class="container">
    <h1>⚙️ COBOL2Java Conversion Report</h1>
    <p class="timestamp">Generated: ${report.timestamp}</p>
    
    <div class="summary">
      <div class="stat">
        <div class="stat-value">${report.summary.totalFiles}</div>
        <div class="stat-label">Total Files</div>
      </div>
      <div class="stat success">
        <div class="stat-value">${report.summary.successfulFiles}</div>
        <div class="stat-label">Successful</div>
      </div>
      <div class="stat error">
        <div class="stat-value">${report.summary.failedFiles}</div>
        <div class="stat-label">Failed</div>
      </div>
      <div class="stat error">
        <div class="stat-value">${report.summary.totalErrors}</div>
        <div class="stat-label">Errors</div>
      </div>
      <div class="stat warning">
        <div class="stat-value">${report.summary.totalWarnings}</div>
        <div class="stat-label">Warnings</div>
      </div>
      <div class="stat">
        <div class="stat-value">${successRate}%</div>
        <div class="stat-label">Success Rate</div>
      </div>
    </div>

    <h2>File Details</h2>
    <table>
      <thead>
        <tr>
          <th>Status</th>
          <th>File</th>
          <th>Class</th>
          <th>Lines</th>
          <th>Time</th>
          <th>Issues</th>
        </tr>
      </thead>
      <tbody>
        ${report.entries.map(e => `
        <tr>
          <td class="${e.success ? 'status-success' : 'status-failed'}">${e.success ? '✓' : '✗'}</td>
          <td>${e.file}</td>
          <td>${e.className}.java</td>
          <td>${e.metadata.linesConverted}</td>
          <td>${e.metadata.durationMs}ms</td>
          <td>
            ${e.errors.map(err => `<div class="error-list">[${err.code}] ${err.message}</div>`).join('')}
            ${e.warnings.map(w => `<div class="warning-list">[${w.code}] ${w.message}</div>`).join('')}
          </td>
        </tr>
        `).join('')}
      </tbody>
    </table>
  </div>
</body>
</html>`;
}
