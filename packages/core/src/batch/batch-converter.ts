/**
 * COBOL2Java Batch Converter
 *
 * 複数のCOBOLファイルを一括でJavaに変換するツール
 */

import { promises as fs } from 'fs';
import * as path from 'path';
import { glob } from 'glob';
import { CobolParser } from '../parser.js';
import { JavaGenerator, GeneratorOptions } from '../generator.js';

export interface BatchConversionOptions extends GeneratorOptions {
  /** 入力ディレクトリ */
  inputDir: string;
  /** 出力ディレクトリ */
  outputDir: string;
  /** ファイルパターン (glob) */
  pattern?: string;
  /** 並列処理数 */
  concurrency?: number;
  /** エラー時に続行するか */
  continueOnError?: boolean;
  /** 詳細ログ */
  verbose?: boolean;
  /** 進捗コールバック */
  onProgress?: (progress: BatchProgress) => void;
  /** ファイル変換完了コールバック */
  onFileComplete?: (result: FileConversionResult) => void;
}

export interface BatchProgress {
  total: number;
  completed: number;
  failed: number;
  currentFile: string;
  percentage: number;
}

export interface FileConversionResult {
  inputPath: string;
  outputPath: string;
  className: string;
  success: boolean;
  error?: string;
  duration: number;
  linesOfCode: number;
}

export interface BatchConversionResult {
  totalFiles: number;
  successCount: number;
  failureCount: number;
  totalDuration: number;
  results: FileConversionResult[];
  summary: BatchSummary;
}

export interface BatchSummary {
  totalLinesOfCode: number;
  averageConversionTime: number;
  conversionRate: number;
  failedFiles: string[];
}

/**
 * バッチ変換を実行
 */
export async function batchConvert(
  options: BatchConversionOptions
): Promise<BatchConversionResult> {
  const startTime = Date.now();
  const {
    inputDir,
    outputDir,
    pattern = '**/*.{cob,cbl,cobol,cpy}',
    concurrency = 4,
    continueOnError = true,
    verbose = false,
    onProgress,
    onFileComplete,
    ...generatorOptions
  } = options;

  // 入力ディレクトリの存在確認
  try {
    await fs.access(inputDir);
  } catch {
    throw new Error(`入力ディレクトリが見つかりません: ${inputDir}`);
  }

  // 出力ディレクトリの作成
  await fs.mkdir(outputDir, { recursive: true });

  // 対象ファイルの検索
  const files = await glob(pattern, {
    cwd: inputDir,
    nodir: true,
    absolute: false,
  });

  if (files.length === 0) {
    return {
      totalFiles: 0,
      successCount: 0,
      failureCount: 0,
      totalDuration: 0,
      results: [],
      summary: {
        totalLinesOfCode: 0,
        averageConversionTime: 0,
        conversionRate: 0,
        failedFiles: [],
      },
    };
  }

  if (verbose) {
    console.log(`Found ${files.length} COBOL files to convert`);
  }

  const results: FileConversionResult[] = [];
  let completed = 0;
  let failed = 0;

  // 並列処理用のワーカー
  const processFile = async (relativePath: string): Promise<FileConversionResult> => {
    const inputPath = path.join(inputDir, relativePath);
    const fileStartTime = Date.now();

    try {
      // ファイル読み込み
      const source = await fs.readFile(inputPath, 'utf-8');
      const linesOfCode = source.split('\n').length;

      // パース
      const parser = new CobolParser();
      const ast = parser.parse(source);

      // 変換
      const generator = new JavaGenerator(generatorOptions);
      const result = await generator.generate(ast);

      // 出力パスの構築
      const outputRelativePath = relativePath.replace(
        /\.(cob|cbl|cobol|cpy)$/i,
        '.java'
      );
      const outputPath = path.join(outputDir, outputRelativePath);

      // 出力ディレクトリの作成
      await fs.mkdir(path.dirname(outputPath), { recursive: true });

      // ファイル書き込み
      await fs.writeFile(outputPath, result.code, 'utf-8');

      const duration = Date.now() - fileStartTime;

      return {
        inputPath,
        outputPath,
        className: result.className,
        success: true,
        duration,
        linesOfCode,
      };
    } catch (error) {
      const duration = Date.now() - fileStartTime;
      const errorMessage = error instanceof Error ? error.message : String(error);

      if (!continueOnError) {
        throw error;
      }

      return {
        inputPath,
        outputPath: '',
        className: '',
        success: false,
        error: errorMessage,
        duration,
        linesOfCode: 0,
      };
    }
  };

  // 並列実行 (制限付き)
  const chunks: string[][] = [];
  for (let i = 0; i < files.length; i += concurrency) {
    chunks.push(files.slice(i, i + concurrency));
  }

  for (const chunk of chunks) {
    const chunkResults = await Promise.all(
      chunk.map(async (file) => {
        const result = await processFile(file);

        completed++;
        if (!result.success) {
          failed++;
        }

        // 進捗通知
        if (onProgress) {
          onProgress({
            total: files.length,
            completed,
            failed,
            currentFile: file,
            percentage: Math.round((completed / files.length) * 100),
          });
        }

        // 完了通知
        if (onFileComplete) {
          onFileComplete(result);
        }

        if (verbose) {
          const status = result.success ? '✓' : '✗';
          console.log(`${status} ${file} (${result.duration}ms)`);
        }

        return result;
      })
    );

    results.push(...chunkResults);
  }

  const totalDuration = Date.now() - startTime;

  // サマリー計算
  const successResults = results.filter((r) => r.success);
  const totalLinesOfCode = successResults.reduce((sum, r) => sum + r.linesOfCode, 0);
  const averageConversionTime =
    successResults.length > 0
      ? successResults.reduce((sum, r) => sum + r.duration, 0) / successResults.length
      : 0;

  return {
    totalFiles: files.length,
    successCount: successResults.length,
    failureCount: results.filter((r) => !r.success).length,
    totalDuration,
    results,
    summary: {
      totalLinesOfCode,
      averageConversionTime,
      conversionRate: files.length > 0 ? (successResults.length / files.length) * 100 : 0,
      failedFiles: results.filter((r) => !r.success).map((r) => r.inputPath),
    },
  };
}

/**
 * バッチ変換結果をJSONレポートとして出力
 */
export async function writeBatchReport(
  result: BatchConversionResult,
  reportPath: string
): Promise<void> {
  const report = {
    timestamp: new Date().toISOString(),
    ...result,
    results: result.results.map((r) => ({
      ...r,
      inputPath: path.relative(process.cwd(), r.inputPath),
      outputPath: r.outputPath ? path.relative(process.cwd(), r.outputPath) : '',
    })),
  };

  await fs.writeFile(reportPath, JSON.stringify(report, null, 2), 'utf-8');
}

/**
 * バッチ変換結果をMarkdownレポートとして出力
 */
export async function writeBatchReportMarkdown(
  result: BatchConversionResult,
  reportPath: string
): Promise<void> {
  const lines: string[] = [
    '# COBOL2Java バッチ変換レポート',
    '',
    `**生成日時**: ${new Date().toISOString()}`,
    '',
    '## サマリー',
    '',
    `| 項目 | 値 |`,
    `|------|-----|`,
    `| 総ファイル数 | ${result.totalFiles} |`,
    `| 成功 | ${result.successCount} |`,
    `| 失敗 | ${result.failureCount} |`,
    `| 成功率 | ${result.summary.conversionRate.toFixed(1)}% |`,
    `| 総コード行数 | ${result.summary.totalLinesOfCode.toLocaleString()} |`,
    `| 平均変換時間 | ${result.summary.averageConversionTime.toFixed(2)}ms |`,
    `| 総処理時間 | ${(result.totalDuration / 1000).toFixed(2)}s |`,
    '',
  ];

  if (result.results.length > 0) {
    lines.push('## 変換結果', '');
    lines.push('| ファイル | ステータス | クラス名 | 時間 |');
    lines.push('|----------|----------|----------|------|');

    for (const r of result.results) {
      const status = r.success ? '✓ 成功' : '✗ 失敗';
      const className = r.className || '-';
      const duration = `${r.duration}ms`;
      const inputFile = path.basename(r.inputPath);
      lines.push(`| ${inputFile} | ${status} | ${className} | ${duration} |`);
    }

    lines.push('');
  }

  if (result.summary.failedFiles.length > 0) {
    lines.push('## 失敗したファイル', '');
    for (const file of result.summary.failedFiles) {
      const failedResult = result.results.find((r) => r.inputPath === file);
      lines.push(`- **${path.basename(file)}**: ${failedResult?.error || '不明なエラー'}`);
    }
    lines.push('');
  }

  await fs.writeFile(reportPath, lines.join('\n'), 'utf-8');
}
