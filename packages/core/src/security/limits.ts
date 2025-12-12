/**
 * Security Limits
 *
 * Configurable limits to prevent resource exhaustion attacks
 */

/**
 * Security limit configuration
 */
export interface SecurityLimits {
  /** Maximum input size in bytes (default: 10MB) */
  maxInputSize: number;
  /** Maximum number of lines (default: 100,000) */
  maxLines: number;
  /** Maximum line length (default: 500 characters) */
  maxLineLength: number;
  /** Maximum nesting depth for data structures (default: 50) */
  maxNestingDepth: number;
  /** Maximum number of data items (default: 10,000) */
  maxDataItems: number;
  /** Maximum number of paragraphs (default: 5,000) */
  maxParagraphs: number;
  /** Maximum identifier length (default: 255) */
  maxIdentifierLength: number;
  /** Maximum string literal length (default: 10,000) */
  maxStringLiteralLength: number;
  /** Maximum recursion depth for transformations (default: 100) */
  maxRecursionDepth: number;
  /** Timeout for parsing in milliseconds (default: 30,000) */
  parseTimeout: number;
  /** Timeout for generation in milliseconds (default: 60,000) */
  generateTimeout: number;
}

/**
 * Default security limits
 */
export const DEFAULT_LIMITS: SecurityLimits = {
  maxInputSize: 10 * 1024 * 1024, // 10MB
  maxLines: 100_000,
  maxLineLength: 500,
  maxNestingDepth: 50,
  maxDataItems: 10_000,
  maxParagraphs: 5_000,
  maxIdentifierLength: 255,
  maxStringLiteralLength: 10_000,
  maxRecursionDepth: 100,
  parseTimeout: 30_000,
  generateTimeout: 60_000,
};

/**
 * Strict security limits for untrusted input
 */
export const STRICT_LIMITS: SecurityLimits = {
  maxInputSize: 1 * 1024 * 1024, // 1MB
  maxLines: 10_000,
  maxLineLength: 200,
  maxNestingDepth: 20,
  maxDataItems: 1_000,
  maxParagraphs: 500,
  maxIdentifierLength: 100,
  maxStringLiteralLength: 1_000,
  maxRecursionDepth: 50,
  parseTimeout: 10_000,
  generateTimeout: 30_000,
};

/**
 * Check result with details
 */
export interface LimitCheckResult {
  valid: boolean;
  violations: LimitViolation[];
}

/**
 * Limit violation details
 */
export interface LimitViolation {
  limit: keyof SecurityLimits;
  actual: number;
  maximum: number;
  message: string;
}

/**
 * Check input against security limits
 */
export function checkLimits(
  input: string,
  limits: Partial<SecurityLimits> = {}
): LimitCheckResult {
  const effectiveLimits = { ...DEFAULT_LIMITS, ...limits };
  const violations: LimitViolation[] = [];

  // Check input size
  const inputSize = Buffer.byteLength(input, 'utf8');
  if (inputSize > effectiveLimits.maxInputSize) {
    violations.push({
      limit: 'maxInputSize',
      actual: inputSize,
      maximum: effectiveLimits.maxInputSize,
      message: `Input size (${formatBytes(inputSize)}) exceeds maximum (${formatBytes(effectiveLimits.maxInputSize)})`,
    });
  }

  // Check line count
  const lines = input.split('\n');
  if (lines.length > effectiveLimits.maxLines) {
    violations.push({
      limit: 'maxLines',
      actual: lines.length,
      maximum: effectiveLimits.maxLines,
      message: `Line count (${lines.length}) exceeds maximum (${effectiveLimits.maxLines})`,
    });
  }

  // Check line lengths
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (line && line.length > effectiveLimits.maxLineLength) {
      violations.push({
        limit: 'maxLineLength',
        actual: line.length,
        maximum: effectiveLimits.maxLineLength,
        message: `Line ${i + 1} length (${line.length}) exceeds maximum (${effectiveLimits.maxLineLength})`,
      });
      break; // Only report first violation
    }
  }

  return {
    valid: violations.length === 0,
    violations,
  };
}

/**
 * Format bytes for human-readable display
 */
function formatBytes(bytes: number): string {
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
  return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
}

/**
 * Create a timeout promise
 */
export function withTimeout<T>(
  promise: Promise<T>,
  timeoutMs: number,
  operation: string
): Promise<T> {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      reject(new Error(`${operation} timed out after ${timeoutMs}ms`));
    }, timeoutMs);

    promise
      .then((result) => {
        clearTimeout(timer);
        resolve(result);
      })
      .catch((error) => {
        clearTimeout(timer);
        reject(error);
      });
  });
}
