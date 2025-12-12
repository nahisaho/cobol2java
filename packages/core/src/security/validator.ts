/**
 * Input Validation
 *
 * Validates COBOL source code for security concerns
 */

import { checkLimits, type SecurityLimits } from './limits.js';

/**
 * Validation result
 */
export interface ValidationResult {
  valid: boolean;
  errors: ValidationError[];
  warnings: ValidationWarning[];
}

/**
 * Validation error
 */
export interface ValidationError {
  code: string;
  message: string;
  line?: number;
  column?: number;
}

/**
 * Validation warning
 */
export interface ValidationWarning {
  code: string;
  message: string;
  line?: number;
}

/**
 * Validation options
 */
export interface ValidationOptions {
  /** Security limits to apply */
  limits?: Partial<SecurityLimits>;
  /** Allow empty input */
  allowEmpty?: boolean;
  /** Strict COBOL validation */
  strictCobol?: boolean;
  /** Check for potentially dangerous patterns */
  checkDangerousPatterns?: boolean;
}

/**
 * Patterns that may indicate malicious input
 */
const DANGEROUS_PATTERNS = [
  // Script injection attempts
  { pattern: /<script\b/i, code: 'SCRIPT_INJECTION', message: 'Potential script injection detected' },
  // SQL injection patterns
  { pattern: /('|"|;|--|\bOR\b|\bAND\b|\bUNION\b|\bSELECT\b|\bDROP\b|\bDELETE\b|\bINSERT\b|\bUPDATE\b)\s*\S+\s*=/i, code: 'SQL_INJECTION', message: 'Potential SQL injection pattern detected' },
  // Command injection
  { pattern: /[`$]\([^)]+\)/, code: 'CMD_INJECTION', message: 'Potential command injection detected' },
  { pattern: /\|\s*\w+/, code: 'PIPE_INJECTION', message: 'Potential pipe command injection detected' },
  // Path traversal
  { pattern: /\.\.[\\/]/, code: 'PATH_TRAVERSAL', message: 'Potential path traversal detected' },
  // Null bytes
  { pattern: /\x00/, code: 'NULL_BYTE', message: 'Null byte detected in input' },
  // Unicode direction override
  { pattern: /[\u202A-\u202E\u2066-\u2069]/, code: 'UNICODE_DIRECTION', message: 'Unicode direction override character detected' },
];

/**
 * Valid COBOL division patterns
 */
const COBOL_DIVISIONS = [
  /IDENTIFICATION\s+DIVISION/i,
  /ENVIRONMENT\s+DIVISION/i,
  /DATA\s+DIVISION/i,
  /PROCEDURE\s+DIVISION/i,
];

/**
 * Validate COBOL input
 */
export function validateInput(
  input: string,
  options: ValidationOptions = {}
): ValidationResult {
  const errors: ValidationError[] = [];
  const warnings: ValidationWarning[] = [];

  // Check for empty input
  if (!input || input.trim().length === 0) {
    if (!options.allowEmpty) {
      errors.push({
        code: 'EMPTY_INPUT',
        message: 'Input is empty or contains only whitespace',
      });
    }
    return { valid: errors.length === 0, errors, warnings };
  }

  // Check security limits
  const limitResult = checkLimits(input, options.limits);
  if (!limitResult.valid) {
    for (const violation of limitResult.violations) {
      errors.push({
        code: `LIMIT_${violation.limit.toUpperCase()}`,
        message: violation.message,
      });
    }
  }

  // Check for dangerous patterns
  if (options.checkDangerousPatterns !== false) {
    for (const { pattern, code, message } of DANGEROUS_PATTERNS) {
      if (pattern.test(input)) {
        warnings.push({ code, message });
      }
    }
  }

  // Strict COBOL validation
  if (options.strictCobol) {
    const hasDivision = COBOL_DIVISIONS.some((pattern) => pattern.test(input));
    if (!hasDivision) {
      warnings.push({
        code: 'NO_DIVISION',
        message: 'No standard COBOL division found in input',
      });
    }

    // Check for PROGRAM-ID
    if (!/PROGRAM-ID\s*\./i.test(input)) {
      warnings.push({
        code: 'NO_PROGRAM_ID',
        message: 'PROGRAM-ID not found',
      });
    }
  }

  // Check for binary content
  if (hasBinaryContent(input)) {
    errors.push({
      code: 'BINARY_CONTENT',
      message: 'Input appears to contain binary content',
    });
  }

  // Check encoding issues
  if (hasEncodingIssues(input)) {
    warnings.push({
      code: 'ENCODING_ISSUE',
      message: 'Input may have encoding issues',
    });
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings,
  };
}

/**
 * Check if input contains binary content
 */
function hasBinaryContent(input: string): boolean {
  // Check for common binary file signatures
  const binarySignatures = [
    '\x89PNG', // PNG
    '\xFF\xD8\xFF', // JPEG
    'PK\x03\x04', // ZIP
    '\x7FELF', // ELF
    'MZ', // DOS/Windows executable
    '\x1F\x8B', // GZIP
  ];

  for (const sig of binarySignatures) {
    if (input.startsWith(sig)) {
      return true;
    }
  }

  // Check for high ratio of control characters
  let controlCount = 0;
  const sampleSize = Math.min(input.length, 1000);
  for (let i = 0; i < sampleSize; i++) {
    const code = input.charCodeAt(i);
    if (code < 32 && code !== 9 && code !== 10 && code !== 13) {
      controlCount++;
    }
  }

  return controlCount / sampleSize > 0.1;
}

/**
 * Check for encoding issues
 */
function hasEncodingIssues(input: string): boolean {
  // Check for replacement character (indicates failed decoding)
  if (input.includes('\uFFFD')) {
    return true;
  }

  // Check for byte order marks in wrong places
  const bomPattern = /[\uFEFF\uFFFE]/;
  const firstOccurrence = input.search(bomPattern);
  if (firstOccurrence > 0) {
    return true;
  }

  return false;
}

/**
 * Validate identifier name
 */
export function validateIdentifier(
  name: string,
  maxLength: number = 255
): ValidationResult {
  const errors: ValidationError[] = [];
  const warnings: ValidationWarning[] = [];

  if (!name || name.length === 0) {
    errors.push({
      code: 'EMPTY_IDENTIFIER',
      message: 'Identifier cannot be empty',
    });
    return { valid: false, errors, warnings };
  }

  if (name.length > maxLength) {
    errors.push({
      code: 'IDENTIFIER_TOO_LONG',
      message: `Identifier length (${name.length}) exceeds maximum (${maxLength})`,
    });
  }

  // COBOL identifier rules: must start with letter, contain letters/digits/hyphens
  if (!/^[A-Za-z][A-Za-z0-9-]*$/i.test(name)) {
    errors.push({
      code: 'INVALID_IDENTIFIER',
      message: 'Identifier must start with a letter and contain only letters, digits, and hyphens',
    });
  }

  // Check for reserved words (sample list)
  const RESERVED_WORDS = [
    'ADD', 'COPY', 'DIVISION', 'MOVE', 'PERFORM', 'SECTION', 'STOP', 'RUN',
  ];
  if (RESERVED_WORDS.includes(name.toUpperCase())) {
    warnings.push({
      code: 'RESERVED_WORD',
      message: `'${name}' is a COBOL reserved word`,
    });
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings,
  };
}

/**
 * Validate file path for security
 */
export function validateFilePath(path: string): ValidationResult {
  const errors: ValidationError[] = [];
  const warnings: ValidationWarning[] = [];

  if (!path || path.length === 0) {
    errors.push({
      code: 'EMPTY_PATH',
      message: 'File path cannot be empty',
    });
    return { valid: false, errors, warnings };
  }

  // Check for path traversal
  if (/\.\.[\\/]/.test(path)) {
    errors.push({
      code: 'PATH_TRAVERSAL',
      message: 'Path traversal sequences are not allowed',
    });
  }

  // Check for absolute paths (security risk in some contexts)
  if (/^[\/\\]|^[A-Za-z]:/.test(path)) {
    warnings.push({
      code: 'ABSOLUTE_PATH',
      message: 'Absolute paths may pose security risks',
    });
  }

  // Check for special characters
  if (/[<>"|?*\x00-\x1F]/.test(path)) {
    errors.push({
      code: 'INVALID_PATH_CHARS',
      message: 'Path contains invalid characters',
    });
  }

  // Check length
  if (path.length > 4096) {
    errors.push({
      code: 'PATH_TOO_LONG',
      message: 'Path exceeds maximum length (4096 characters)',
    });
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings,
  };
}
