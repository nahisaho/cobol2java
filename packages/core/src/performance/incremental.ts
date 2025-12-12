/**
 * Incremental Parsing Support
 * 
 * Provides incremental parsing for large files with minimal re-parsing
 */

import { CobolAst, CobolParser } from '../parser.js';

/**
 * Source change event
 */
export interface SourceChange {
  /** Start position (0-indexed line, column) */
  start: { line: number; column: number };
  /** End position (0-indexed line, column) */
  end: { line: number; column: number };
  /** New text inserted at the range */
  newText: string;
}

/**
 * Parsed region cache entry
 */
export interface RegionCacheEntry {
  /** Content hash */
  hash: string;
  /** Start line (0-indexed) */
  startLine: number;
  /** End line (0-indexed) */
  endLine: number;
  /** Parsed content (partial AST or tokens) */
  parsed: unknown;
  /** Timestamp of last access */
  lastAccess: number;
}

/**
 * Region type for incremental parsing
 */
export type RegionType = 
  | 'IDENTIFICATION'
  | 'ENVIRONMENT'
  | 'DATA'
  | 'PROCEDURE'
  | 'UNKNOWN';

/**
 * Incremental parser state
 */
export interface IncrementalState {
  /** Full source content */
  source: string;
  /** Cached regions */
  regions: Map<RegionType, RegionCacheEntry>;
  /** Full AST (may be stale) */
  ast: CobolAst | null;
  /** Whether full reparse is needed */
  dirty: boolean;
  /** Version number */
  version: number;
}

/**
 * Incremental Parser
 * 
 * Caches parsed regions and only re-parses affected areas
 */
export class IncrementalParser {
  private state: IncrementalState;
  private parser: CobolParser;
  private maxCacheAge: number = 5 * 60 * 1000; // 5 minutes

  constructor() {
    this.parser = new CobolParser();
    this.state = {
      source: '',
      regions: new Map(),
      ast: null,
      dirty: true,
      version: 0,
    };
  }

  /**
   * Initialize with source
   */
  initialize(source: string): void {
    this.state.source = source;
    this.state.dirty = true;
    this.state.version++;
    this.identifyRegions();
  }

  /**
   * Apply incremental change
   */
  applyChange(change: SourceChange): void {
    const lines = this.state.source.split('\n');
    
    // Calculate affected region
    const startOffset = this.getOffset(lines, change.start.line, change.start.column);
    const endOffset = this.getOffset(lines, change.end.line, change.end.column);
    
    // Apply text change
    this.state.source = 
      this.state.source.substring(0, startOffset) +
      change.newText +
      this.state.source.substring(endOffset);
    
    this.state.version++;
    
    // Invalidate affected regions
    this.invalidateAffectedRegions(change.start.line, change.end.line, change.newText);
    
    // Re-identify regions if structure changed
    if (this.isStructuralChange(change.newText)) {
      this.identifyRegions();
    }
  }

  /**
   * Get current AST (parsing if needed)
   */
  getAST(): CobolAst {
    if (this.state.dirty || !this.state.ast) {
      this.state.ast = this.parser.parse(this.state.source);
      this.state.dirty = false;
    }
    return this.state.ast;
  }

  /**
   * Get AST with incremental update (experimental)
   */
  getIncrementalAST(): CobolAst {
    // For now, fall back to full parse
    // Future: implement true incremental parsing
    return this.getAST();
  }

  /**
   * Check if a region is dirty
   */
  isRegionDirty(region: RegionType): boolean {
    const entry = this.state.regions.get(region);
    if (!entry) return true;
    
    const currentHash = this.hashRegion(region);
    return entry.hash !== currentHash;
  }

  /**
   * Get cached region if valid
   */
  getCachedRegion<T>(region: RegionType): T | null {
    const entry = this.state.regions.get(region);
    if (!entry) return null;
    
    // Check if expired
    if (Date.now() - entry.lastAccess > this.maxCacheAge) {
      this.state.regions.delete(region);
      return null;
    }
    
    // Check if hash matches
    const currentHash = this.hashRegion(region);
    if (entry.hash !== currentHash) {
      return null;
    }
    
    entry.lastAccess = Date.now();
    return entry.parsed as T;
  }

  /**
   * Cache parsed region
   */
  cacheRegion(region: RegionType, parsed: unknown): void {
    const bounds = this.getRegionBounds(region);
    if (!bounds) return;

    this.state.regions.set(region, {
      hash: this.hashRegion(region),
      startLine: bounds.start,
      endLine: bounds.end,
      parsed,
      lastAccess: Date.now(),
    });
  }

  /**
   * Clear all caches
   */
  clearCache(): void {
    this.state.regions.clear();
    this.state.dirty = true;
  }

  /**
   * Get current version
   */
  getVersion(): number {
    return this.state.version;
  }

  /**
   * Identify DIVISION regions in source
   */
  private identifyRegions(): void {
    const lines = this.state.source.split('\n');
    let currentRegion: RegionType = 'UNKNOWN';
    let regionStart = 0;

    const divisionPattern = /(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION/i;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i] || '';
      const match = line.match(divisionPattern);
      
      if (match) {
        // Save previous region
        if (currentRegion !== 'UNKNOWN' && i > regionStart) {
          this.updateRegionBounds(currentRegion, regionStart, i - 1);
        }
        
        currentRegion = match[1]!.toUpperCase() as RegionType;
        regionStart = i;
      }
    }

    // Save last region
    if (currentRegion !== 'UNKNOWN') {
      this.updateRegionBounds(currentRegion, regionStart, lines.length - 1);
    }
  }

  /**
   * Update region bounds
   */
  private updateRegionBounds(region: RegionType, start: number, end: number): void {
    const existing = this.state.regions.get(region);
    if (existing) {
      existing.startLine = start;
      existing.endLine = end;
    } else {
      this.state.regions.set(region, {
        hash: '',
        startLine: start,
        endLine: end,
        parsed: null,
        lastAccess: Date.now(),
      });
    }
  }

  /**
   * Get region bounds
   */
  private getRegionBounds(region: RegionType): { start: number; end: number } | null {
    const entry = this.state.regions.get(region);
    if (!entry) return null;
    return { start: entry.startLine, end: entry.endLine };
  }

  /**
   * Invalidate regions affected by change
   */
  private invalidateAffectedRegions(startLine: number, endLine: number, newText: string): void {
    const linesDelta = newText.split('\n').length - (endLine - startLine + 1);

    for (const [region, entry] of this.state.regions.entries()) {
      // Check if change overlaps with region
      if (startLine <= entry.endLine && endLine >= entry.startLine) {
        // Invalidate this region
        entry.hash = '';
        this.state.dirty = true;
      }
      
      // Adjust bounds for regions after the change
      if (entry.startLine > endLine) {
        entry.startLine += linesDelta;
        entry.endLine += linesDelta;
      } else if (entry.endLine > endLine) {
        entry.endLine += linesDelta;
      }
    }
  }

  /**
   * Check if change is structural
   */
  private isStructuralChange(text: string): boolean {
    const upper = text.toUpperCase();
    return upper.includes('DIVISION') || 
           upper.includes('SECTION') ||
           upper.includes('PROGRAM-ID');
  }

  /**
   * Calculate hash for a region
   */
  private hashRegion(region: RegionType): string {
    const bounds = this.getRegionBounds(region);
    if (!bounds) return '';

    const lines = this.state.source.split('\n');
    const regionContent = lines.slice(bounds.start, bounds.end + 1).join('\n');
    
    return this.simpleHash(regionContent);
  }

  /**
   * Simple string hash
   */
  private simpleHash(str: string): string {
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i);
      hash = ((hash << 5) - hash) + char;
      hash = hash & hash; // Convert to 32bit integer
    }
    return hash.toString(16);
  }

  /**
   * Get character offset from line/column
   */
  private getOffset(lines: string[], line: number, column: number): number {
    let offset = 0;
    for (let i = 0; i < line && i < lines.length; i++) {
      offset += (lines[i]?.length ?? 0) + 1; // +1 for newline
    }
    return offset + column;
  }
}

/**
 * Create an incremental parser instance
 */
export function createIncrementalParser(): IncrementalParser {
  return new IncrementalParser();
}
