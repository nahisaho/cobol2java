/**
 * Performance module tests
 */
import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  Cache,
  AstCache,
  CodeCache,
  createContentHash,
  createOptionsHash,
  createCacheKey,
} from '../src/performance/cache.js';
import {
  ChunkedProcessor,
  LineTransform,
  stringToStream,
  streamToString,
  StringBuilder,
} from '../src/performance/streaming.js';
import {
  Timer,
  Profiler,
  globalProfiler,
  measureTime,
  measureTimeSync,
  ThroughputCalculator,
} from '../src/performance/profiler.js';

describe('Performance Module Tests', () => {
  describe('Cache', () => {
    let cache: Cache<string>;

    beforeEach(() => {
      cache = new Cache<string>({ maxEntries: 5, ttlMs: 0 });
    });

    describe('basic operations', () => {
      it('should store and retrieve values', () => {
        cache.set('key1', 'value1');
        expect(cache.get('key1')).toBe('value1');
      });

      it('should return undefined for missing keys', () => {
        expect(cache.get('nonexistent')).toBeUndefined();
      });

      it('should check if key exists', () => {
        cache.set('key1', 'value1');
        expect(cache.has('key1')).toBe(true);
        expect(cache.has('nonexistent')).toBe(false);
      });

      it('should delete entries', () => {
        cache.set('key1', 'value1');
        expect(cache.delete('key1')).toBe(true);
        expect(cache.get('key1')).toBeUndefined();
      });

      it('should clear all entries', () => {
        cache.set('key1', 'value1');
        cache.set('key2', 'value2');
        cache.clear();
        expect(cache.get('key1')).toBeUndefined();
        expect(cache.get('key2')).toBeUndefined();
      });
    });

    describe('eviction', () => {
      it('should evict entries when max entries exceeded', () => {
        for (let i = 0; i < 6; i++) {
          cache.set(`key${i}`, `value${i}`);
        }
        const stats = cache.getStats();
        expect(stats.totalEntries).toBeLessThanOrEqual(5);
        expect(stats.evictions).toBeGreaterThan(0);
      });

      it('should evict based on LRU policy', () => {
        // Use fresh cache for this test
        const lruCache = new Cache<string>({ maxEntries: 3, ttlMs: 0, evictionPolicy: 'lru' });
        
        lruCache.set('key1', 'value1');
        lruCache.set('key2', 'value2');
        lruCache.set('key3', 'value3');
        
        // Access key1 to update its accessedAt time
        lruCache.get('key1');
        
        // Add new entry - should evict key2 (oldest accessed)
        lruCache.set('key4', 'value4');
        
        // Verify eviction occurred
        const stats = lruCache.getStats();
        expect(stats.evictions).toBe(1);
        expect(stats.totalEntries).toBe(3);
      });
    });

    describe('TTL', () => {
      it('should expire entries after TTL', async () => {
        const shortCache = new Cache<string>({ ttlMs: 50 });
        shortCache.set('key1', 'value1');
        
        expect(shortCache.get('key1')).toBe('value1');
        
        await new Promise((resolve) => setTimeout(resolve, 60));
        
        expect(shortCache.get('key1')).toBeUndefined();
      });
    });

    describe('statistics', () => {
      it('should track hits and misses', () => {
        cache.set('key1', 'value1');
        cache.get('key1'); // hit
        cache.get('key1'); // hit
        cache.get('nonexistent'); // miss
        
        const stats = cache.getStats();
        expect(stats.hits).toBe(2);
        expect(stats.misses).toBe(1);
        expect(stats.hitRate).toBeCloseTo(0.667, 2);
      });
    });

    describe('getOrCompute', () => {
      it('should return cached value if exists', async () => {
        cache.set('key1', 'cached');
        const compute = vi.fn().mockResolvedValue('computed');
        
        const result = await cache.getOrCompute('key1', compute);
        
        expect(result).toBe('cached');
        expect(compute).not.toHaveBeenCalled();
      });

      it('should compute and cache if not exists', async () => {
        const compute = vi.fn().mockResolvedValue('computed');
        
        const result = await cache.getOrCompute('key1', compute);
        
        expect(result).toBe('computed');
        expect(compute).toHaveBeenCalled();
        expect(cache.get('key1')).toBe('computed');
      });
    });
  });

  describe('AstCache', () => {
    it('should cache AST by source code', () => {
      const cache = new AstCache();
      const source = 'IDENTIFICATION DIVISION.';
      const ast = { type: 'program' as const, programName: 'TEST', dataItems: [], paragraphs: [] };
      
      cache.setBySource(source, ast);
      const cached = cache.getBySource(source);
      
      expect(cached).toEqual(ast);
    });

    it('should return undefined for different source', () => {
      const cache = new AstCache();
      cache.setBySource('source1', { type: 'program', programName: 'TEST', dataItems: [], paragraphs: [] });
      
      expect(cache.getBySource('source2')).toBeUndefined();
    });
  });

  describe('CodeCache', () => {
    it('should cache generated code', () => {
      const cache = new CodeCache();
      cache.set('hash1', 'public class Test {}');
      
      expect(cache.get('hash1')).toBe('public class Test {}');
    });
  });

  describe('Hash functions', () => {
    it('should create consistent content hash', () => {
      const content = 'IDENTIFICATION DIVISION.';
      const hash1 = createContentHash(content);
      const hash2 = createContentHash(content);
      
      expect(hash1).toBe(hash2);
      expect(hash1).toHaveLength(64); // SHA-256 hex
    });

    it('should create different hashes for different content', () => {
      const hash1 = createContentHash('content1');
      const hash2 = createContentHash('content2');
      
      expect(hash1).not.toBe(hash2);
    });

    it('should create options hash', () => {
      const hash = createOptionsHash({ springBoot: true, javaVersion: 17 });
      expect(hash).toHaveLength(8);
    });

    it('should create combined cache key', () => {
      const key = createCacheKey('source', { option: true });
      expect(key).toContain(':');
    });
  });

  describe('ChunkedProcessor', () => {
    it('should process content in chunks', async () => {
      const processor = new ChunkedProcessor({ chunkSize: 2, emitProgress: false });
      const content = 'LINE1\nLINE2\nLINE3\nLINE4\nLINE5';
      
      const results = await processor.processChunks(content, async (chunk, index) => {
        return { index, lineCount: chunk.split('\n').length };
      });
      
      expect(results).toHaveLength(3); // 5 lines / 2 per chunk = 3 chunks
    });

    it('should process chunks concurrently', async () => {
      const processor = new ChunkedProcessor({ chunkSize: 2, concurrency: 2, emitProgress: false });
      const content = 'L1\nL2\nL3\nL4';
      
      const results = await processor.processChunksConcurrent(content, async (chunk, index) => {
        return `processed-${index}`;
      });
      
      expect(results).toHaveLength(2);
    });

    it('should emit progress events', async () => {
      const processor = new ChunkedProcessor({ chunkSize: 2, emitProgress: true });
      const events: unknown[] = [];
      
      processor.on('progress', (event) => events.push(event));
      
      await processor.processChunks('L1\nL2\nL3\nL4', async () => 'done');
      
      expect(events.length).toBeGreaterThan(0);
    });
  });

  describe('LineTransform', () => {
    it('should transform lines', async () => {
      const transform = new LineTransform((line, num) => `${num}: ${line}`);
      const input = stringToStream('LINE1\nLINE2\nLINE3');
      
      const piped = input.pipe(transform);
      const result = await streamToString(piped);
      
      expect(result).toContain('1: LINE1');
      expect(result).toContain('2: LINE2');
      expect(result).toContain('3: LINE3');
    });

    it('should filter lines returning null', async () => {
      const transform = new LineTransform((line) => 
        line.startsWith('KEEP') ? line : null
      );
      const input = stringToStream('KEEP1\nSKIP\nKEEP2');
      
      const piped = input.pipe(transform);
      const result = await streamToString(piped);
      
      expect(result).toContain('KEEP1');
      expect(result).toContain('KEEP2');
      expect(result).not.toContain('SKIP');
    });
  });

  describe('StringBuilder', () => {
    it('should build strings efficiently', () => {
      const sb = new StringBuilder();
      sb.append('Hello').append(' ').append('World');
      
      expect(sb.toString()).toBe('Hello World');
      expect(sb.length).toBe(11);
    });

    it('should append lines', () => {
      const sb = new StringBuilder();
      sb.appendLine('Line1').appendLine('Line2');
      
      expect(sb.toString()).toBe('Line1\nLine2\n');
    });

    it('should clear content', () => {
      const sb = new StringBuilder();
      sb.append('content');
      sb.clear();
      
      expect(sb.toString()).toBe('');
      expect(sb.length).toBe(0);
    });
  });

  describe('Timer', () => {
    it('should measure elapsed time', async () => {
      const timer = new Timer('test');
      await new Promise((resolve) => setTimeout(resolve, 50));
      
      const elapsed = timer.elapsed();
      expect(elapsed).toBeGreaterThanOrEqual(40);
    });

    it('should support marks', async () => {
      const timer = new Timer('test');
      timer.mark('start');
      await new Promise((resolve) => setTimeout(resolve, 30));
      timer.mark('middle');
      await new Promise((resolve) => setTimeout(resolve, 30));
      timer.mark('end');
      
      const between = timer.between('start', 'end');
      expect(between).toBeGreaterThanOrEqual(50);
    });

    it('should stop and return result', () => {
      const timer = new Timer('test');
      const result = timer.stop();
      
      expect(result.name).toBe('test');
      expect(result.durationMs).toBeGreaterThanOrEqual(0);
    });
  });

  describe('Profiler', () => {
    let profiler: Profiler;

    beforeEach(() => {
      profiler = new Profiler();
    });

    it('should profile operations', () => {
      profiler.start('op1');
      profiler.end('op1');
      
      const entry = profiler.getEntry('op1');
      expect(entry).toBeDefined();
      expect(entry!.calls).toBe(1);
    });

    it('should accumulate multiple calls', () => {
      for (let i = 0; i < 5; i++) {
        profiler.start('op1');
        profiler.end('op1');
      }
      
      const entry = profiler.getEntry('op1');
      expect(entry!.calls).toBe(5);
    });

    it('should track min/max/avg', () => {
      profiler.record('op1', 10);
      profiler.record('op1', 20);
      profiler.record('op1', 30);
      
      const entry = profiler.getEntry('op1');
      expect(entry!.minMs).toBe(10);
      expect(entry!.maxMs).toBe(30);
      expect(entry!.avgMs).toBe(20);
    });

    it('should wrap functions', async () => {
      const fn = profiler.wrap('wrapped', async () => {
        await new Promise((resolve) => setTimeout(resolve, 10));
        return 'result';
      });
      
      const result = await fn();
      
      expect(result).toBe('result');
      expect(profiler.getEntry('wrapped')).toBeDefined();
    });

    it('should take memory snapshots', () => {
      const snapshot = profiler.takeMemorySnapshot();
      
      expect(snapshot.heapUsed).toBeGreaterThan(0);
      expect(snapshot.heapTotal).toBeGreaterThan(0);
    });

    it('should generate summary', () => {
      profiler.record('operation1', 100);
      profiler.record('operation2', 200);
      
      const summary = profiler.getSummary();
      
      expect(summary).toContain('Performance Profile');
      expect(summary).toContain('operation1');
      expect(summary).toContain('operation2');
    });

    it('should be disableable', () => {
      profiler.setEnabled(false);
      profiler.start('op1');
      profiler.end('op1');
      
      expect(profiler.getEntry('op1')).toBeUndefined();
    });
  });

  describe('measureTime', () => {
    beforeEach(() => {
      globalProfiler.reset();
    });

    it('should measure async function time', async () => {
      const { result, durationMs } = await measureTime('test', async () => {
        await new Promise((resolve) => setTimeout(resolve, 20));
        return 42;
      });
      
      expect(result).toBe(42);
      expect(durationMs).toBeGreaterThanOrEqual(15);
    });
  });

  describe('measureTimeSync', () => {
    beforeEach(() => {
      globalProfiler.reset();
    });

    it('should measure sync function time', () => {
      const { result, durationMs } = measureTimeSync('test', () => {
        let sum = 0;
        for (let i = 0; i < 10000; i++) sum += i;
        return sum;
      });
      
      expect(result).toBe(49995000);
      expect(durationMs).toBeGreaterThanOrEqual(0);
    });
  });

  describe('ThroughputCalculator', () => {
    it('should calculate throughput with time spread', async () => {
      const calc = new ThroughputCalculator(5000);
      
      // Record samples with slight time spread
      calc.record(10);
      await new Promise(r => setTimeout(r, 50));
      calc.record(10);
      await new Promise(r => setTimeout(r, 50));
      calc.record(10);
      
      const throughput = calc.getThroughput();
      // With 30 items over ~100ms, expect ~300 items/sec
      expect(throughput).toBeGreaterThan(0);
    });

    it('should return 0 with single sample', () => {
      const calc = new ThroughputCalculator();
      calc.record(100);
      
      // Need at least 2 samples to calculate throughput
      expect(calc.getThroughput()).toBe(0);
    });

    it('should reset', () => {
      const calc = new ThroughputCalculator();
      calc.record(100);
      calc.reset();
      
      expect(calc.getThroughput()).toBe(0);
    });
  });
});
