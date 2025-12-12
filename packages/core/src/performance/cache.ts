/**
 * Caching Module
 *
 * Provides caching for parsed ASTs and generated code to improve performance
 */

import { createHash } from 'crypto';
import type { CobolAst } from '../parser.js';

/**
 * Cache entry with metadata
 */
export interface CacheEntry<T> {
  value: T;
  hash: string;
  createdAt: number;
  accessedAt: number;
  accessCount: number;
  sizeBytes: number;
}

/**
 * Cache statistics
 */
export interface CacheStats {
  hits: number;
  misses: number;
  evictions: number;
  totalEntries: number;
  totalSizeBytes: number;
  hitRate: number;
}

/**
 * Cache configuration
 */
export interface CacheConfig {
  /** Maximum number of entries */
  maxEntries: number;
  /** Maximum size in bytes */
  maxSizeBytes: number;
  /** TTL in milliseconds (0 = no expiration) */
  ttlMs: number;
  /** Eviction policy */
  evictionPolicy: 'lru' | 'lfu' | 'fifo';
}

/**
 * Default cache configuration
 */
const DEFAULT_CACHE_CONFIG: CacheConfig = {
  maxEntries: 100,
  maxSizeBytes: 50 * 1024 * 1024, // 50MB
  ttlMs: 30 * 60 * 1000, // 30 minutes
  evictionPolicy: 'lru',
};

/**
 * Generic LRU/LFU Cache implementation
 */
export class Cache<T> {
  private cache = new Map<string, CacheEntry<T>>();
  private config: CacheConfig;
  private stats: CacheStats = {
    hits: 0,
    misses: 0,
    evictions: 0,
    totalEntries: 0,
    totalSizeBytes: 0,
    hitRate: 0,
  };

  constructor(config: Partial<CacheConfig> = {}) {
    this.config = { ...DEFAULT_CACHE_CONFIG, ...config };
  }

  /**
   * Get value from cache
   */
  get(key: string): T | undefined {
    const entry = this.cache.get(key);

    if (!entry) {
      this.stats.misses++;
      this.updateHitRate();
      return undefined;
    }

    // Check TTL
    if (this.config.ttlMs > 0 && Date.now() - entry.createdAt > this.config.ttlMs) {
      this.delete(key);
      this.stats.misses++;
      this.updateHitRate();
      return undefined;
    }

    // Update access metadata
    entry.accessedAt = Date.now();
    entry.accessCount++;
    this.stats.hits++;
    this.updateHitRate();

    return entry.value;
  }

  /**
   * Set value in cache
   */
  set(key: string, value: T, sizeBytes?: number): void {
    const hash = this.computeHash(key);
    const size = sizeBytes ?? this.estimateSize(value);

    // Evict if necessary
    while (
      (this.cache.size >= this.config.maxEntries ||
        this.stats.totalSizeBytes + size > this.config.maxSizeBytes) &&
      this.cache.size > 0
    ) {
      this.evictOne();
    }

    const entry: CacheEntry<T> = {
      value,
      hash,
      createdAt: Date.now(),
      accessedAt: Date.now(),
      accessCount: 1,
      sizeBytes: size,
    };

    this.cache.set(key, entry);
    this.stats.totalEntries = this.cache.size;
    this.stats.totalSizeBytes += size;
  }

  /**
   * Check if key exists
   */
  has(key: string): boolean {
    const entry = this.cache.get(key);
    if (!entry) return false;

    // Check TTL
    if (this.config.ttlMs > 0 && Date.now() - entry.createdAt > this.config.ttlMs) {
      this.delete(key);
      return false;
    }

    return true;
  }

  /**
   * Delete entry
   */
  delete(key: string): boolean {
    const entry = this.cache.get(key);
    if (entry) {
      this.stats.totalSizeBytes -= entry.sizeBytes;
      this.cache.delete(key);
      this.stats.totalEntries = this.cache.size;
      return true;
    }
    return false;
  }

  /**
   * Clear all entries
   */
  clear(): void {
    this.cache.clear();
    this.stats.totalEntries = 0;
    this.stats.totalSizeBytes = 0;
  }

  /**
   * Get cache statistics
   */
  getStats(): CacheStats {
    return { ...this.stats };
  }

  /**
   * Get or compute value
   */
  async getOrCompute(key: string, compute: () => Promise<T>, sizeBytes?: number): Promise<T> {
    const cached = this.get(key);
    if (cached !== undefined) {
      return cached;
    }

    const value = await compute();
    this.set(key, value, sizeBytes);
    return value;
  }

  /**
   * Evict one entry based on policy
   */
  private evictOne(): void {
    let keyToEvict: string | null = null;
    let minScore = Infinity;

    for (const [key, entry] of this.cache.entries()) {
      let score: number;

      switch (this.config.evictionPolicy) {
        case 'lru':
          score = entry.accessedAt;
          break;
        case 'lfu':
          score = entry.accessCount;
          break;
        case 'fifo':
          score = entry.createdAt;
          break;
      }

      if (score < minScore) {
        minScore = score;
        keyToEvict = key;
      }
    }

    if (keyToEvict) {
      this.delete(keyToEvict);
      this.stats.evictions++;
    }
  }

  /**
   * Compute hash for key
   */
  private computeHash(key: string): string {
    return createHash('sha256').update(key).digest('hex').substring(0, 16);
  }

  /**
   * Estimate size of value
   */
  private estimateSize(value: T): number {
    try {
      return JSON.stringify(value).length * 2; // UTF-16 estimate
    } catch {
      return 1024; // Default estimate
    }
  }

  /**
   * Update hit rate
   */
  private updateHitRate(): void {
    const total = this.stats.hits + this.stats.misses;
    this.stats.hitRate = total > 0 ? this.stats.hits / total : 0;
  }
}

/**
 * Specialized cache for COBOL AST
 */
export class AstCache extends Cache<CobolAst> {
  constructor(config: Partial<CacheConfig> = {}) {
    super({
      maxEntries: 50,
      maxSizeBytes: 100 * 1024 * 1024, // 100MB
      ttlMs: 60 * 60 * 1000, // 1 hour
      evictionPolicy: 'lru',
      ...config,
    });
  }

  /**
   * Get cached AST by source code
   */
  getBySource(source: string): CobolAst | undefined {
    const key = this.hashSource(source);
    return this.get(key);
  }

  /**
   * Cache AST by source code
   */
  setBySource(source: string, ast: CobolAst): void {
    const key = this.hashSource(source);
    this.set(key, ast);
  }

  /**
   * Hash source code for cache key
   */
  private hashSource(source: string): string {
    return createHash('sha256').update(source).digest('hex');
  }
}

/**
 * Specialized cache for generated Java code
 */
export class CodeCache extends Cache<string> {
  constructor(config: Partial<CacheConfig> = {}) {
    super({
      maxEntries: 100,
      maxSizeBytes: 50 * 1024 * 1024, // 50MB
      ttlMs: 30 * 60 * 1000, // 30 minutes
      evictionPolicy: 'lru',
      ...config,
    });
  }
}

/**
 * Create content hash for cache key
 */
export function createContentHash(content: string): string {
  return createHash('sha256').update(content).digest('hex');
}

/**
 * Create options hash for cache key
 */
export function createOptionsHash(options: Record<string, unknown>): string {
  const sorted = JSON.stringify(options, Object.keys(options).sort());
  return createHash('sha256').update(sorted).digest('hex').substring(0, 8);
}

/**
 * Create combined cache key
 */
export function createCacheKey(source: string, options: Record<string, unknown>): string {
  const contentHash = createContentHash(source);
  const optionsHash = createOptionsHash(options);
  return `${contentHash}:${optionsHash}`;
}
