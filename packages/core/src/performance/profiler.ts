/**
 * Performance Profiler Module
 *
 * Provides profiling and performance monitoring utilities
 */

/**
 * Timer result
 */
export interface TimerResult {
  name: string;
  durationMs: number;
  startTime: number;
  endTime: number;
}

/**
 * Profile entry
 */
export interface ProfileEntry {
  name: string;
  calls: number;
  totalMs: number;
  minMs: number;
  maxMs: number;
  avgMs: number;
  lastMs: number;
}

/**
 * Memory snapshot
 */
export interface MemorySnapshot {
  timestamp: number;
  heapUsed: number;
  heapTotal: number;
  external: number;
  rss: number;
}

/**
 * Simple timer for measuring execution time
 */
export class Timer {
  private startTime: number;
  private endTime?: number;
  private marks = new Map<string, number>();

  constructor(private name: string = 'default') {
    this.startTime = performance.now();
  }

  /**
   * Mark a point in time
   */
  mark(label: string): void {
    this.marks.set(label, performance.now());
  }

  /**
   * Get time since start or since a mark
   */
  elapsed(fromMark?: string): number {
    const now = performance.now();
    if (fromMark && this.marks.has(fromMark)) {
      return now - (this.marks.get(fromMark) ?? this.startTime);
    }
    return now - this.startTime;
  }

  /**
   * Stop the timer
   */
  stop(): TimerResult {
    this.endTime = performance.now();
    return {
      name: this.name,
      durationMs: this.endTime - this.startTime,
      startTime: this.startTime,
      endTime: this.endTime,
    };
  }

  /**
   * Get duration between two marks
   */
  between(startMark: string, endMark: string): number | undefined {
    const start = this.marks.get(startMark);
    const end = this.marks.get(endMark);
    if (start !== undefined && end !== undefined) {
      return end - start;
    }
    return undefined;
  }
}

/**
 * Performance profiler for tracking multiple operations
 */
export class Profiler {
  private entries = new Map<string, ProfileEntry>();
  private activeTimers = new Map<string, number>();
  private memorySnapshots: MemorySnapshot[] = [];
  private enabled = true;

  /**
   * Enable/disable profiling
   */
  setEnabled(enabled: boolean): void {
    this.enabled = enabled;
  }

  /**
   * Start timing an operation
   */
  start(name: string): void {
    if (!this.enabled) return;
    this.activeTimers.set(name, performance.now());
  }

  /**
   * End timing and record
   */
  end(name: string): number {
    if (!this.enabled) return 0;

    const startTime = this.activeTimers.get(name);
    if (startTime === undefined) {
      console.warn(`Profiler: No start time for "${name}"`);
      return 0;
    }

    const duration = performance.now() - startTime;
    this.activeTimers.delete(name);
    this.record(name, duration);
    return duration;
  }

  /**
   * Record a duration directly
   */
  record(name: string, durationMs: number): void {
    if (!this.enabled) return;

    const existing = this.entries.get(name);
    if (existing) {
      existing.calls++;
      existing.totalMs += durationMs;
      existing.minMs = Math.min(existing.minMs, durationMs);
      existing.maxMs = Math.max(existing.maxMs, durationMs);
      existing.avgMs = existing.totalMs / existing.calls;
      existing.lastMs = durationMs;
    } else {
      this.entries.set(name, {
        name,
        calls: 1,
        totalMs: durationMs,
        minMs: durationMs,
        maxMs: durationMs,
        avgMs: durationMs,
        lastMs: durationMs,
      });
    }
  }

  /**
   * Wrap a function with profiling
   */
  wrap<T extends (...args: unknown[]) => unknown>(name: string, fn: T): T {
    const profiler = this;
    return function (this: unknown, ...args: Parameters<T>): ReturnType<T> {
      profiler.start(name);
      try {
        const result = fn.apply(this, args);
        if (result instanceof Promise) {
          return result.finally(() => profiler.end(name)) as ReturnType<T>;
        }
        profiler.end(name);
        return result as ReturnType<T>;
      } catch (error) {
        profiler.end(name);
        throw error;
      }
    } as T;
  }

  /**
   * Take memory snapshot
   */
  takeMemorySnapshot(): MemorySnapshot {
    const mem = process.memoryUsage();
    const snapshot: MemorySnapshot = {
      timestamp: Date.now(),
      heapUsed: mem.heapUsed,
      heapTotal: mem.heapTotal,
      external: mem.external,
      rss: mem.rss,
    };
    this.memorySnapshots.push(snapshot);
    return snapshot;
  }

  /**
   * Get memory growth since first snapshot
   */
  getMemoryGrowth(): number {
    if (this.memorySnapshots.length < 2) return 0;
    const first = this.memorySnapshots[0]!;
    const last = this.memorySnapshots[this.memorySnapshots.length - 1]!;
    return last.heapUsed - first.heapUsed;
  }

  /**
   * Get all profile entries
   */
  getEntries(): ProfileEntry[] {
    return Array.from(this.entries.values());
  }

  /**
   * Get entry by name
   */
  getEntry(name: string): ProfileEntry | undefined {
    return this.entries.get(name);
  }

  /**
   * Get summary report
   */
  getSummary(): string {
    const entries = this.getEntries().sort((a, b) => b.totalMs - a.totalMs);
    const lines = [
      '=== Performance Profile ===',
      '',
      'Operation                        Calls    Total(ms)    Avg(ms)    Min(ms)    Max(ms)',
      '-'.repeat(90),
    ];

    for (const entry of entries) {
      lines.push(
        `${entry.name.padEnd(30)} ${entry.calls.toString().padStart(8)} ` +
          `${entry.totalMs.toFixed(2).padStart(12)} ` +
          `${entry.avgMs.toFixed(2).padStart(10)} ` +
          `${entry.minMs.toFixed(2).padStart(10)} ` +
          `${entry.maxMs.toFixed(2).padStart(10)}`
      );
    }

    // Memory summary
    if (this.memorySnapshots.length > 0) {
      const last = this.memorySnapshots[this.memorySnapshots.length - 1]!;
      const growth = this.getMemoryGrowth();
      lines.push('');
      lines.push('=== Memory ===');
      lines.push(`Heap Used: ${formatBytes(last.heapUsed)}`);
      lines.push(`Heap Total: ${formatBytes(last.heapTotal)}`);
      lines.push(`Growth: ${formatBytes(growth)}`);
    }

    return lines.join('\n');
  }

  /**
   * Reset all data
   */
  reset(): void {
    this.entries.clear();
    this.activeTimers.clear();
    this.memorySnapshots = [];
  }
}

/**
 * Global profiler instance
 */
export const globalProfiler = new Profiler();

/**
 * Decorator for profiling methods
 */
export function profile(name?: string) {
  return function (
    _target: unknown,
    propertyKey: string,
    descriptor: PropertyDescriptor
  ): PropertyDescriptor {
    const originalMethod = descriptor.value;
    const profileName = name ?? propertyKey;

    descriptor.value = function (...args: unknown[]) {
      globalProfiler.start(profileName);
      try {
        const result = originalMethod.apply(this, args);
        if (result instanceof Promise) {
          return result.finally(() => globalProfiler.end(profileName));
        }
        globalProfiler.end(profileName);
        return result;
      } catch (error) {
        globalProfiler.end(profileName);
        throw error;
      }
    };

    return descriptor;
  };
}

/**
 * Measure execution time of a function
 */
export async function measureTime<T>(
  name: string,
  fn: () => T | Promise<T>
): Promise<{ result: T; durationMs: number }> {
  const start = performance.now();
  const result = await fn();
  const durationMs = performance.now() - start;
  globalProfiler.record(name, durationMs);
  return { result, durationMs };
}

/**
 * Synchronous time measurement
 */
export function measureTimeSync<T>(
  name: string,
  fn: () => T
): { result: T; durationMs: number } {
  const start = performance.now();
  const result = fn();
  const durationMs = performance.now() - start;
  globalProfiler.record(name, durationMs);
  return { result, durationMs };
}

/**
 * Format bytes for display
 */
function formatBytes(bytes: number): string {
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(2)} KB`;
  return `${(bytes / (1024 * 1024)).toFixed(2)} MB`;
}

/**
 * Throughput calculator
 */
export class ThroughputCalculator {
  private samples: Array<{ timestamp: number; count: number }> = [];
  private windowMs: number;

  constructor(windowMs: number = 5000) {
    this.windowMs = windowMs;
  }

  /**
   * Record an item processed
   */
  record(count: number = 1): void {
    const now = Date.now();
    this.samples.push({ timestamp: now, count });

    // Remove old samples
    const cutoff = now - this.windowMs;
    this.samples = this.samples.filter((s) => s.timestamp >= cutoff);
  }

  /**
   * Get current throughput (items per second)
   */
  getThroughput(): number {
    if (this.samples.length < 2) return 0;

    const now = Date.now();
    const cutoff = now - this.windowMs;
    const relevantSamples = this.samples.filter((s) => s.timestamp >= cutoff);

    if (relevantSamples.length < 2) return 0;

    const totalCount = relevantSamples.reduce((sum, s) => sum + s.count, 0);
    const first = relevantSamples[0]!;
    const last = relevantSamples[relevantSamples.length - 1]!;
    const durationSec = (last.timestamp - first.timestamp) / 1000;

    return durationSec > 0 ? totalCount / durationSec : 0;
  }

  /**
   * Reset calculator
   */
  reset(): void {
    this.samples = [];
  }
}
