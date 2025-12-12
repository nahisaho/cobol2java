/**
 * Object Pooling for Memory Efficiency
 * 
 * Reduces GC pressure for frequently allocated objects
 */

/**
 * Poolable object interface
 */
export interface Poolable {
  /** Reset object state for reuse */
  reset(): void;
}

/**
 * Object pool configuration
 */
export interface PoolConfig {
  /** Initial pool size */
  initialSize: number;
  /** Maximum pool size */
  maxSize: number;
  /** Whether to pre-allocate objects */
  preAllocate: boolean;
}

/**
 * Default pool configuration
 */
const DEFAULT_POOL_CONFIG: PoolConfig = {
  initialSize: 100,
  maxSize: 1000,
  preAllocate: false,
};

/**
 * Generic object pool
 */
export class ObjectPool<T extends Poolable> {
  private pool: T[] = [];
  private factory: () => T;
  private config: PoolConfig;
  private borrowed: number = 0;
  private created: number = 0;

  constructor(factory: () => T, config: Partial<PoolConfig> = {}) {
    this.factory = factory;
    this.config = { ...DEFAULT_POOL_CONFIG, ...config };

    if (this.config.preAllocate) {
      this.preAllocate();
    }
  }

  /**
   * Borrow an object from the pool
   */
  borrow(): T {
    this.borrowed++;
    
    if (this.pool.length > 0) {
      return this.pool.pop()!;
    }

    this.created++;
    return this.factory();
  }

  /**
   * Return an object to the pool
   */
  release(obj: T): void {
    this.borrowed--;
    
    if (this.pool.length < this.config.maxSize) {
      obj.reset();
      this.pool.push(obj);
    }
    // If pool is full, object is discarded for GC
  }

  /**
   * Pre-allocate objects
   */
  private preAllocate(): void {
    for (let i = 0; i < this.config.initialSize; i++) {
      this.pool.push(this.factory());
      this.created++;
    }
  }

  /**
   * Get pool statistics
   */
  getStats(): {
    available: number;
    borrowed: number;
    created: number;
    hitRate: number;
  } {
    const hits = this.borrowed - this.created;
    return {
      available: this.pool.length,
      borrowed: this.borrowed,
      created: this.created,
      hitRate: this.borrowed > 0 ? hits / this.borrowed : 0,
    };
  }

  /**
   * Clear the pool
   */
  clear(): void {
    this.pool = [];
  }
}

/**
 * Poolable StringBuilder for efficient string construction
 */
export class PoolableStringBuilder implements Poolable {
  private parts: string[] = [];
  private _length: number = 0;

  append(str: string): this {
    this.parts.push(str);
    this._length += str.length;
    return this;
  }

  appendLine(str: string = ''): this {
    this.parts.push(str + '\n');
    this._length += str.length + 1;
    return this;
  }

  toString(): string {
    return this.parts.join('');
  }

  get length(): number {
    return this._length;
  }

  reset(): void {
    this.parts = [];
    this._length = 0;
  }
}

/**
 * Poolable AST node
 */
export class PoolableASTNode implements Poolable {
  type: string = '';
  name: string = '';
  value: unknown = null;
  children: PoolableASTNode[] = [];
  parent: PoolableASTNode | null = null;
  line: number = 0;
  column: number = 0;

  reset(): void {
    this.type = '';
    this.name = '';
    this.value = null;
    this.children = [];
    this.parent = null;
    this.line = 0;
    this.column = 0;
  }

  setType(type: string): this {
    this.type = type;
    return this;
  }

  setName(name: string): this {
    this.name = name;
    return this;
  }

  setValue(value: unknown): this {
    this.value = value;
    return this;
  }

  addChild(child: PoolableASTNode): this {
    child.parent = this;
    this.children.push(child);
    return this;
  }
}

/**
 * Poolable token
 */
export class PoolableToken implements Poolable {
  type: string = '';
  value: string = '';
  line: number = 0;
  column: number = 0;

  reset(): void {
    this.type = '';
    this.value = '';
    this.line = 0;
    this.column = 0;
  }

  set(type: string, value: string, line: number, column: number): this {
    this.type = type;
    this.value = value;
    this.line = line;
    this.column = column;
    return this;
  }
}

/**
 * Global pool manager
 */
class PoolManager {
  private stringBuilderPool: ObjectPool<PoolableStringBuilder>;
  private astNodePool: ObjectPool<PoolableASTNode>;
  private tokenPool: ObjectPool<PoolableToken>;

  constructor() {
    this.stringBuilderPool = new ObjectPool(
      () => new PoolableStringBuilder(),
      { initialSize: 50, maxSize: 200, preAllocate: true }
    );
    
    this.astNodePool = new ObjectPool(
      () => new PoolableASTNode(),
      { initialSize: 500, maxSize: 5000, preAllocate: false }
    );
    
    this.tokenPool = new ObjectPool(
      () => new PoolableToken(),
      { initialSize: 1000, maxSize: 10000, preAllocate: false }
    );
  }

  borrowStringBuilder(): PoolableStringBuilder {
    return this.stringBuilderPool.borrow();
  }

  releaseStringBuilder(sb: PoolableStringBuilder): void {
    this.stringBuilderPool.release(sb);
  }

  borrowASTNode(): PoolableASTNode {
    return this.astNodePool.borrow();
  }

  releaseASTNode(node: PoolableASTNode): void {
    this.astNodePool.release(node);
  }

  borrowToken(): PoolableToken {
    return this.tokenPool.borrow();
  }

  releaseToken(token: PoolableToken): void {
    this.tokenPool.release(token);
  }

  /**
   * Get combined pool statistics
   */
  getStats(): Record<string, ReturnType<ObjectPool<Poolable>['getStats']>> {
    return {
      stringBuilder: this.stringBuilderPool.getStats(),
      astNode: this.astNodePool.getStats(),
      token: this.tokenPool.getStats(),
    };
  }

  /**
   * Clear all pools
   */
  clearAll(): void {
    this.stringBuilderPool.clear();
    this.astNodePool.clear();
    this.tokenPool.clear();
  }
}

// Singleton pool manager
let poolManager: PoolManager | null = null;

/**
 * Get the global pool manager
 */
export function getPoolManager(): PoolManager {
  if (!poolManager) {
    poolManager = new PoolManager();
  }
  return poolManager;
}

/**
 * Memory-efficient string builder using pooled instance
 */
export function withStringBuilder<T>(fn: (sb: PoolableStringBuilder) => T): T {
  const manager = getPoolManager();
  const sb = manager.borrowStringBuilder();
  try {
    return fn(sb);
  } finally {
    manager.releaseStringBuilder(sb);
  }
}

/**
 * Batch release AST nodes (recursive)
 */
export function releaseASTTree(node: PoolableASTNode): void {
  const manager = getPoolManager();
  
  function releaseRecursive(n: PoolableASTNode): void {
    for (const child of n.children) {
      releaseRecursive(child);
    }
    manager.releaseASTNode(n);
  }
  
  releaseRecursive(node);
}
