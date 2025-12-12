/**
 * Worker Threads Support
 * 
 * Provides parallel processing for CPU-intensive conversion tasks
 */

import { EventEmitter } from 'events';

/**
 * Worker task definition
 */
export interface WorkerTask<T, R> {
  id: string;
  type: string;
  data: T;
  priority?: number;
}

/**
 * Worker result
 */
export interface WorkerResult<R> {
  taskId: string;
  success: boolean;
  result?: R;
  error?: string;
  duration: number;
}

/**
 * Worker pool configuration
 */
export interface WorkerPoolConfig {
  /** Maximum number of workers (default: CPU count - 1) */
  maxWorkers: number;
  /** Task timeout in ms (default: 30000) */
  taskTimeout: number;
  /** Whether to restart failed workers (default: true) */
  restartOnFailure: boolean;
}

/**
 * Default worker pool configuration
 */
const DEFAULT_CONFIG: WorkerPoolConfig = {
  maxWorkers: Math.max(1, 4 - 1), // Default to 3 workers
  taskTimeout: 30000,
  restartOnFailure: true,
};

/**
 * Task queue with priority support
 */
export class TaskQueue<T> {
  private queue: Array<{ item: T; priority: number }> = [];

  enqueue(item: T, priority: number = 0): void {
    const entry = { item, priority };
    // Insert in priority order (higher priority first)
    let inserted = false;
    for (let i = 0; i < this.queue.length; i++) {
      if ((this.queue[i]?.priority ?? 0) < priority) {
        this.queue.splice(i, 0, entry);
        inserted = true;
        break;
      }
    }
    if (!inserted) {
      this.queue.push(entry);
    }
  }

  dequeue(): T | undefined {
    return this.queue.shift()?.item;
  }

  get length(): number {
    return this.queue.length;
  }

  isEmpty(): boolean {
    return this.queue.length === 0;
  }

  clear(): void {
    this.queue = [];
  }
}

/**
 * Simulated Worker Pool (browser/Node.js compatible)
 * 
 * Uses microtask scheduling to simulate parallel execution
 * For true parallelism in Node.js, use worker_threads module
 */
export class WorkerPool<T, R> extends EventEmitter {
  private config: WorkerPoolConfig;
  private taskQueue: TaskQueue<WorkerTask<T, R>>;
  private activeWorkers: number = 0;
  private pendingResults: Map<string, {
    resolve: (result: WorkerResult<R>) => void;
    reject: (error: Error) => void;
    timeout: ReturnType<typeof setTimeout>;
  }> = new Map();
  private processor: (data: T) => Promise<R>;
  private isShutdown: boolean = false;

  constructor(
    processor: (data: T) => Promise<R>,
    config: Partial<WorkerPoolConfig> = {}
  ) {
    super();
    this.processor = processor;
    this.config = { ...DEFAULT_CONFIG, ...config };
    this.taskQueue = new TaskQueue();
  }

  /**
   * Submit a task for processing
   */
  async submit(task: WorkerTask<T, R>): Promise<WorkerResult<R>> {
    if (this.isShutdown) {
      throw new Error('Worker pool is shut down');
    }

    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        this.pendingResults.delete(task.id);
        reject(new Error(`Task ${task.id} timed out after ${this.config.taskTimeout}ms`));
      }, this.config.taskTimeout);

      this.pendingResults.set(task.id, { resolve, reject, timeout });
      this.taskQueue.enqueue(task, task.priority ?? 0);
      this.processNext();
    });
  }

  /**
   * Submit multiple tasks and wait for all results
   */
  async submitBatch(tasks: WorkerTask<T, R>[]): Promise<WorkerResult<R>[]> {
    return Promise.all(tasks.map(task => this.submit(task)));
  }

  /**
   * Process next task if workers are available
   */
  private async processNext(): Promise<void> {
    if (this.isShutdown) return;
    if (this.activeWorkers >= this.config.maxWorkers) return;
    if (this.taskQueue.isEmpty()) return;

    const task = this.taskQueue.dequeue();
    if (!task) return;

    this.activeWorkers++;
    this.emit('task-start', task.id);

    const startTime = Date.now();

    try {
      // Use setImmediate/setTimeout to allow other tasks to be scheduled
      await new Promise(resolve => setTimeout(resolve, 0));

      const result = await this.processor(task.data);
      const duration = Date.now() - startTime;

      const workerResult: WorkerResult<R> = {
        taskId: task.id,
        success: true,
        result,
        duration,
      };

      this.resolveTask(task.id, workerResult);
    } catch (error) {
      const duration = Date.now() - startTime;
      const workerResult: WorkerResult<R> = {
        taskId: task.id,
        success: false,
        error: error instanceof Error ? error.message : String(error),
        duration,
      };

      this.resolveTask(task.id, workerResult);
    } finally {
      this.activeWorkers--;
      this.emit('task-complete', task.id);
      // Process next task
      this.processNext();
    }
  }

  /**
   * Resolve a pending task
   */
  private resolveTask(taskId: string, result: WorkerResult<R>): void {
    const pending = this.pendingResults.get(taskId);
    if (pending) {
      clearTimeout(pending.timeout);
      this.pendingResults.delete(taskId);
      pending.resolve(result);
    }
  }

  /**
   * Get current pool statistics
   */
  getStats(): {
    activeWorkers: number;
    queuedTasks: number;
    pendingResults: number;
  } {
    return {
      activeWorkers: this.activeWorkers,
      queuedTasks: this.taskQueue.length,
      pendingResults: this.pendingResults.size,
    };
  }

  /**
   * Shutdown the worker pool
   */
  shutdown(): void {
    this.isShutdown = true;
    this.taskQueue.clear();
    
    // Reject all pending tasks
    for (const [taskId, pending] of this.pendingResults.entries()) {
      clearTimeout(pending.timeout);
      pending.reject(new Error('Worker pool shutdown'));
    }
    this.pendingResults.clear();
    
    this.emit('shutdown');
  }

  /**
   * Check if pool is active
   */
  isActive(): boolean {
    return !this.isShutdown;
  }
}

/**
 * Parallel COBOL converter using worker pool
 */
export class ParallelConverter {
  private pool: WorkerPool<string, { code: string; className: string }>;

  constructor(
    converter: (source: string) => Promise<{ code: string; className: string }>,
    config?: Partial<WorkerPoolConfig>
  ) {
    this.pool = new WorkerPool(converter, config);
  }

  /**
   * Convert multiple COBOL sources in parallel
   */
  async convertBatch(
    sources: Array<{ id: string; source: string; priority?: number }>
  ): Promise<Array<{ id: string; success: boolean; result?: { code: string; className: string }; error?: string }>> {
    const tasks = sources.map(s => ({
      id: s.id,
      type: 'convert',
      data: s.source,
      priority: s.priority,
    }));

    const results = await this.pool.submitBatch(tasks);

    return results.map(r => ({
      id: r.taskId,
      success: r.success,
      result: r.result,
      error: r.error,
    }));
  }

  /**
   * Get pool statistics
   */
  getStats() {
    return this.pool.getStats();
  }

  /**
   * Shutdown converter
   */
  shutdown(): void {
    this.pool.shutdown();
  }
}
