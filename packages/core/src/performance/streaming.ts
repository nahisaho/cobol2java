/**
 * Streaming Processing Module
 *
 * Provides streaming and chunked processing for large COBOL files
 */

import { Transform, Readable, Writable } from 'stream';
import { EventEmitter } from 'events';

/**
 * Chunk processing result
 */
export interface ChunkResult {
  chunkIndex: number;
  startLine: number;
  endLine: number;
  content: string;
  success: boolean;
  error?: string;
}

/**
 * Streaming processor options
 */
export interface StreamingOptions {
  /** Lines per chunk (default: 1000) */
  chunkSize: number;
  /** Maximum concurrent chunks (default: 4) */
  concurrency: number;
  /** High water mark for streams (default: 64KB) */
  highWaterMark: number;
  /** Emit progress events (default: true) */
  emitProgress: boolean;
}

/**
 * Default streaming options
 */
const DEFAULT_STREAMING_OPTIONS: StreamingOptions = {
  chunkSize: 1000,
  concurrency: 4,
  highWaterMark: 64 * 1024,
  emitProgress: true,
};

/**
 * Progress event data
 */
export interface ProgressEvent {
  type: 'chunk-start' | 'chunk-complete' | 'complete' | 'error';
  chunkIndex?: number;
  totalChunks?: number;
  percentage?: number;
  bytesProcessed?: number;
  totalBytes?: number;
  error?: Error;
}

/**
 * Chunked file processor
 */
export class ChunkedProcessor extends EventEmitter {
  private options: StreamingOptions;

  constructor(options: Partial<StreamingOptions> = {}) {
    super();
    this.options = { ...DEFAULT_STREAMING_OPTIONS, ...options };
  }

  /**
   * Process content in chunks
   */
  async processChunks<T>(
    content: string,
    processor: (chunk: string, index: number, startLine: number) => Promise<T>
  ): Promise<T[]> {
    const lines = content.split('\n');
    const chunks = this.splitIntoChunks(lines);
    const results: T[] = [];

    let startLine = 1;
    for (let i = 0; i < chunks.length; i++) {
      const chunk = chunks[i];
      if (!chunk) continue;

      if (this.options.emitProgress) {
        this.emit('progress', {
          type: 'chunk-start',
          chunkIndex: i,
          totalChunks: chunks.length,
          percentage: (i / chunks.length) * 100,
        } as ProgressEvent);
      }

      const result = await processor(chunk.join('\n'), i, startLine);
      results.push(result);
      startLine += chunk.length;

      if (this.options.emitProgress) {
        this.emit('progress', {
          type: 'chunk-complete',
          chunkIndex: i,
          totalChunks: chunks.length,
          percentage: ((i + 1) / chunks.length) * 100,
        } as ProgressEvent);
      }
    }

    if (this.options.emitProgress) {
      this.emit('progress', {
        type: 'complete',
        totalChunks: chunks.length,
        percentage: 100,
      } as ProgressEvent);
    }

    return results;
  }

  /**
   * Process chunks concurrently with limited parallelism
   */
  async processChunksConcurrent<T>(
    content: string,
    processor: (chunk: string, index: number, startLine: number) => Promise<T>
  ): Promise<T[]> {
    const lines = content.split('\n');
    const chunks = this.splitIntoChunks(lines);
    const results: (T | null)[] = new Array(chunks.length).fill(null);

    const startLines: number[] = [];
    let currentLine = 1;
    for (const chunk of chunks) {
      startLines.push(currentLine);
      currentLine += chunk?.length ?? 0;
    }

    // Process in batches
    for (let i = 0; i < chunks.length; i += this.options.concurrency) {
      const batch = chunks.slice(i, i + this.options.concurrency);
      const batchStartLines = startLines.slice(i, i + this.options.concurrency);

      const batchPromises = batch.map((chunk, batchIndex) => {
        const globalIndex = i + batchIndex;
        return processor(chunk?.join('\n') ?? '', globalIndex, batchStartLines[batchIndex] ?? 1);
      });

      const batchResults = await Promise.all(batchPromises);
      for (let j = 0; j < batchResults.length; j++) {
        results[i + j] = batchResults[j] ?? null;
      }

      if (this.options.emitProgress) {
        this.emit('progress', {
          type: 'chunk-complete',
          chunkIndex: Math.min(i + this.options.concurrency, chunks.length) - 1,
          totalChunks: chunks.length,
          percentage: (Math.min(i + this.options.concurrency, chunks.length) / chunks.length) * 100,
        } as ProgressEvent);
      }
    }

    return results.filter((r): r is T => r !== null);
  }

  /**
   * Split lines into chunks
   */
  private splitIntoChunks(lines: string[]): string[][] {
    const chunks: string[][] = [];
    for (let i = 0; i < lines.length; i += this.options.chunkSize) {
      chunks.push(lines.slice(i, i + this.options.chunkSize));
    }
    return chunks;
  }
}

/**
 * Line-by-line transform stream
 */
export class LineTransform extends Transform {
  private buffer = '';
  private lineNumber = 0;

  constructor(private lineProcessor: (line: string, lineNumber: number) => string | null) {
    super({ objectMode: true });
  }

  _transform(
    chunk: Buffer,
    _encoding: BufferEncoding,
    callback: (error?: Error | null) => void
  ): void {
    this.buffer += chunk.toString();
    const lines = this.buffer.split('\n');

    // Keep last incomplete line in buffer
    this.buffer = lines.pop() ?? '';

    for (const line of lines) {
      this.lineNumber++;
      const result = this.lineProcessor(line, this.lineNumber);
      if (result !== null) {
        this.push(result + '\n');
      }
    }

    callback();
  }

  _flush(callback: (error?: Error | null) => void): void {
    if (this.buffer) {
      this.lineNumber++;
      const result = this.lineProcessor(this.buffer, this.lineNumber);
      if (result !== null) {
        this.push(result + '\n');
      }
    }
    callback();
  }
}

/**
 * Create a readable stream from string
 */
export function stringToStream(content: string, chunkSize: number = 64 * 1024): Readable {
  let position = 0;

  return new Readable({
    read() {
      if (position >= content.length) {
        this.push(null);
        return;
      }

      const chunk = content.slice(position, position + chunkSize);
      position += chunkSize;
      this.push(chunk);
    },
  });
}

/**
 * Collect stream to string
 */
export async function streamToString(stream: Readable): Promise<string> {
  const chunks: (string | Buffer)[] = [];

  return new Promise((resolve, reject) => {
    stream.on('data', (chunk: string | Buffer) => chunks.push(chunk));
    stream.on('error', reject);
    stream.on('end', () => {
      const result = chunks.map(c => 
        typeof c === 'string' ? c : c.toString('utf8')
      ).join('');
      resolve(result);
    });
  });
}

/**
 * Create a progress tracking writable stream
 */
export function createProgressStream(
  totalBytes: number,
  onProgress: (bytesProcessed: number, percentage: number) => void
): Writable {
  let bytesProcessed = 0;

  return new Writable({
    write(chunk: Buffer, _encoding, callback) {
      bytesProcessed += chunk.length;
      const percentage = (bytesProcessed / totalBytes) * 100;
      onProgress(bytesProcessed, percentage);
      callback();
    },
  });
}

/**
 * Incremental hash calculator for large content
 */
export class IncrementalHasher {
  private hash: import('crypto').Hash;
  private bytesProcessed = 0;

  constructor() {
    // Dynamic import to avoid bundling issues
    const crypto = require('crypto');
    this.hash = crypto.createHash('sha256');
  }

  /**
   * Update hash with chunk
   */
  update(chunk: string | Buffer): void {
    this.hash.update(chunk);
    this.bytesProcessed += Buffer.isBuffer(chunk) ? chunk.length : Buffer.byteLength(chunk);
  }

  /**
   * Get final hash
   */
  digest(): string {
    return this.hash.digest('hex');
  }

  /**
   * Get bytes processed
   */
  getBytesProcessed(): number {
    return this.bytesProcessed;
  }
}

/**
 * Memory-efficient large string builder
 */
export class StringBuilder {
  private chunks: string[] = [];
  private totalLength = 0;

  /**
   * Append string
   */
  append(str: string): this {
    this.chunks.push(str);
    this.totalLength += str.length;
    return this;
  }

  /**
   * Append line with newline
   */
  appendLine(str: string = ''): this {
    return this.append(str + '\n');
  }

  /**
   * Get total length
   */
  get length(): number {
    return this.totalLength;
  }

  /**
   * Build final string
   */
  toString(): string {
    const result = this.chunks.join('');
    // Compact memory
    this.chunks = [result];
    return result;
  }

  /**
   * Clear builder
   */
  clear(): void {
    this.chunks = [];
    this.totalLength = 0;
  }
}
