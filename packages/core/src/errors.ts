/**
 * Error types and utilities for COBOL to Java conversion
 * 
 * Error Code System:
 * - CVT001-099: Conversion structure errors
 *   - CVT001: Missing IDENTIFICATION DIVISION
 *   - CVT002: Missing PROCEDURE DIVISION
 *   - CVT003: Invalid program name
 * 
 * - CVT100-199: Data division errors
 *   - CVT101: Invalid PIC clause
 *   - CVT102: Unsupported data type
 *   - CVT103: Invalid OCCURS clause
 * 
 * - CVT200-299: Procedure division errors  
 *   - CVT201: Unknown COBOL verb
 *   - CVT202: Invalid statement syntax
 *   - CVT203: Undefined paragraph reference
 * 
 * - CVT300-399: LLM errors
 *   - CVT301: LLM connection failed
 *   - CVT302: LLM response invalid
 *   - CVT303: LLM rate limited
 * 
 * - CVT400-499: Output errors
 *   - CVT401: Invalid Java syntax generated
 *   - CVT402: File write failed
 */

/**
 * Error severity levels
 */
export enum ErrorSeverity {
  /** Fatal error - conversion cannot proceed */
  FATAL = 'fatal',
  /** Error - partial failure, conversion continues */
  ERROR = 'error',
  /** Warning - non-critical issue */
  WARNING = 'warning',
  /** Informational message */
  INFO = 'info',
}

/**
 * Error information
 */
export interface ErrorInfo {
  /** Error code (e.g., CVT001) */
  code: string;
  /** Error message */
  message: string;
  /** Error severity */
  severity: ErrorSeverity;
  /** Source file (if available) */
  file?: string;
  /** Line number (1-based) */
  line?: number;
  /** Column number (1-based) */
  column?: number;
  /** COBOL construct that caused the error */
  construct?: string;
  /** Suggestion for fixing the error */
  suggestion?: string;
}

/**
 * Base error class for conversion errors
 */
export class ConversionError extends Error {
  public readonly errors: ErrorInfo[];

  constructor(message: string, errors: ErrorInfo[] = []) {
    super(message);
    this.name = 'ConversionError';
    this.errors = errors;
  }
}

/**
 * Parse error - COBOL syntax is invalid
 */
export class ParseError extends ConversionError {
  constructor(message: string, errors: ErrorInfo[] = []) {
    super(message, errors);
    this.name = 'ParseError';
  }
}

/**
 * Transform error - conversion rule failed
 */
export class TransformError extends ConversionError {
  constructor(message: string, errors: ErrorInfo[] = []) {
    super(message, errors);
    this.name = 'TransformError';
  }
}

/**
 * LLM error - LLM communication failed
 */
export class LLMError extends ConversionError {
  constructor(message: string, errors: ErrorInfo[] = []) {
    super(message, errors);
    this.name = 'LLMError';
  }
}

/**
 * Create an error info object
 */
export function createError(
  code: string,
  message: string,
  options?: Partial<ErrorInfo>
): ErrorInfo {
  return {
    code,
    message,
    severity: ErrorSeverity.ERROR,
    ...options,
  };
}

/**
 * Create a warning info object
 */
export function createWarning(
  code: string,
  message: string,
  options?: Partial<ErrorInfo>
): ErrorInfo {
  return {
    code,
    message,
    severity: ErrorSeverity.WARNING,
    ...options,
  };
}

/**
 * Format error info for display
 */
export function formatError(error: ErrorInfo): string {
  let result = `[${error.severity.toUpperCase()}] ${error.code}: ${error.message}`;
  
  if (error.file || error.line) {
    const location = [
      error.file,
      error.line ? `line ${error.line}` : null,
      error.column ? `col ${error.column}` : null,
    ]
      .filter(Boolean)
      .join(':');
    result += ` (${location})`;
  }

  if (error.suggestion) {
    result += `\n  Suggestion: ${error.suggestion}`;
  }

  return result;
}
