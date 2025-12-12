/**
 * Batch Converter Tests
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { promises as fs } from 'fs';
import * as path from 'path';
import * as os from 'os';
import {
  batchConvert,
  writeBatchReport,
  writeBatchReportMarkdown,
  type BatchConversionOptions,
} from '../src/batch/batch-converter.js';

describe('Batch Converter', () => {
  let tempDir: string;
  let inputDir: string;
  let outputDir: string;

  beforeEach(async () => {
    tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'cobol2java-batch-'));
    inputDir = path.join(tempDir, 'input');
    outputDir = path.join(tempDir, 'output');
    await fs.mkdir(inputDir, { recursive: true });
  });

  afterEach(async () => {
    await fs.rm(tempDir, { recursive: true, force: true });
  });

  describe('Basic Batch Conversion', () => {
    it('should throw for non-existent input directory', async () => {
      await expect(
        batchConvert({
          inputDir: '/non/existent/path',
          outputDir,
          packageName: 'com.test',
        })
      ).rejects.toThrow('入力ディレクトリが見つかりません');
    });

    it('should handle empty directory', async () => {
      const result = await batchConvert({
        inputDir,
        outputDir,
        packageName: 'com.test',
      });

      expect(result).toBeDefined();
      expect(result.totalFiles).toBe(0);
      expect(result.successCount).toBe(0);
    });
  });

  describe('Report Generation', () => {
    it('should have writeBatchReport function', () => {
      expect(typeof writeBatchReport).toBe('function');
    });

    it('should have writeBatchReportMarkdown function', () => {
      expect(typeof writeBatchReportMarkdown).toBe('function');
    });
  });

  describe('BatchConversionResult Structure', () => {
    it('should return proper structure for empty directory', async () => {
      const result = await batchConvert({
        inputDir,
        outputDir,
        packageName: 'com.test',
      });

      expect(result).toHaveProperty('totalFiles');
      expect(result).toHaveProperty('successCount');
      expect(result).toHaveProperty('failureCount');
      expect(result).toHaveProperty('totalDuration');
      expect(result).toHaveProperty('results');
      expect(result).toHaveProperty('summary');
    });

    it('should have proper summary structure', async () => {
      const result = await batchConvert({
        inputDir,
        outputDir,
        packageName: 'com.test',
      });

      expect(result.summary).toHaveProperty('totalLinesOfCode');
      expect(result.summary).toHaveProperty('averageConversionTime');
      expect(result.summary).toHaveProperty('conversionRate');
      expect(result.summary).toHaveProperty('failedFiles');
    });
  });
});
