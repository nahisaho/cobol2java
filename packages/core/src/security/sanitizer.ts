/**
 * Input Sanitization
 *
 * Sanitizes input to remove potentially dangerous content
 */

/**
 * Sanitization options
 */
export interface SanitizationOptions {
  /** Remove control characters except CR/LF/TAB */
  removeControlChars?: boolean;
  /** Normalize line endings to LF */
  normalizeLineEndings?: boolean;
  /** Remove null bytes */
  removeNullBytes?: boolean;
  /** Remove Unicode direction overrides */
  removeDirectionOverrides?: boolean;
  /** Trim leading/trailing whitespace from lines */
  trimLines?: boolean;
  /** Remove BOM */
  removeBom?: boolean;
  /** Maximum line length (truncate if exceeded) */
  maxLineLength?: number;
  /** Escape HTML entities */
  escapeHtml?: boolean;
}

/**
 * Default sanitization options
 */
const DEFAULT_SANITIZATION_OPTIONS: SanitizationOptions = {
  removeControlChars: true,
  normalizeLineEndings: true,
  removeNullBytes: true,
  removeDirectionOverrides: true,
  trimLines: false,
  removeBom: true,
  maxLineLength: undefined,
  escapeHtml: false,
};

/**
 * Sanitize COBOL source input
 */
export function sanitizeInput(
  input: string,
  options: SanitizationOptions = {}
): string {
  const opts = { ...DEFAULT_SANITIZATION_OPTIONS, ...options };
  let result = input;

  // Remove BOM
  if (opts.removeBom) {
    result = removeBom(result);
  }

  // Remove null bytes
  if (opts.removeNullBytes) {
    result = result.replace(/\x00/g, '');
  }

  // Remove Unicode direction overrides
  if (opts.removeDirectionOverrides) {
    result = result.replace(/[\u202A-\u202E\u2066-\u2069]/g, '');
  }

  // Normalize line endings
  if (opts.normalizeLineEndings) {
    result = result.replace(/\r\n/g, '\n').replace(/\r/g, '\n');
  }

  // Remove control characters
  if (opts.removeControlChars) {
    result = removeControlCharacters(result);
  }

  // Process lines
  if (opts.trimLines || opts.maxLineLength) {
    const lines = result.split('\n');
    for (let i = 0; i < lines.length; i++) {
      let line = lines[i] ?? '';
      if (opts.trimLines) {
        line = line.trimEnd();
      }
      if (opts.maxLineLength && line.length > opts.maxLineLength) {
        line = line.substring(0, opts.maxLineLength);
      }
      lines[i] = line;
    }
    result = lines.join('\n');
  }

  // Escape HTML entities
  if (opts.escapeHtml) {
    result = escapeHtmlEntities(result);
  }

  return result;
}

/**
 * Remove BOM (Byte Order Mark)
 */
function removeBom(input: string): string {
  // UTF-8 BOM
  if (input.charCodeAt(0) === 0xFEFF) {
    return input.slice(1);
  }
  return input;
}

/**
 * Remove control characters except CR, LF, TAB
 */
function removeControlCharacters(input: string): string {
  // Remove ASCII control characters except TAB (9), LF (10), CR (13)
  return input.replace(/[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]/g, '');
}

/**
 * Escape HTML entities
 */
function escapeHtmlEntities(input: string): string {
  const entities: Record<string, string> = {
    '&': '&amp;',
    '<': '&lt;',
    '>': '&gt;',
    '"': '&quot;',
    "'": '&#39;',
  };
  return input.replace(/[&<>"']/g, (char) => entities[char] || char);
}

/**
 * Sanitize for Java string literal
 */
export function sanitizeForJavaString(input: string): string {
  return input
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t')
    .replace(/[\x00-\x1F\x7F]/g, (char) => {
      const code = char.charCodeAt(0);
      return `\\u${code.toString(16).padStart(4, '0')}`;
    });
}

/**
 * Sanitize for Java identifier
 */
export function sanitizeForJavaIdentifier(input: string): string {
  // Replace invalid characters
  let result = input
    .replace(/[^a-zA-Z0-9_$]/g, '_')
    .replace(/-/g, '_')
    .replace(/__+/g, '_');

  // Ensure starts with valid character
  if (!/^[a-zA-Z_$]/.test(result)) {
    result = '_' + result;
  }

  // Handle Java reserved words
  const JAVA_RESERVED = [
    'abstract', 'assert', 'boolean', 'break', 'byte', 'case', 'catch', 'char',
    'class', 'const', 'continue', 'default', 'do', 'double', 'else', 'enum',
    'extends', 'final', 'finally', 'float', 'for', 'goto', 'if', 'implements',
    'import', 'instanceof', 'int', 'interface', 'long', 'native', 'new',
    'package', 'private', 'protected', 'public', 'return', 'short', 'static',
    'strictfp', 'super', 'switch', 'synchronized', 'this', 'throw', 'throws',
    'transient', 'try', 'void', 'volatile', 'while', 'true', 'false', 'null',
  ];

  if (JAVA_RESERVED.includes(result.toLowerCase())) {
    result = result + '_';
  }

  return result;
}

/**
 * Sanitize for Java package name
 */
export function sanitizeForJavaPackage(input: string): string {
  return input
    .toLowerCase()
    .replace(/[^a-z0-9.]/g, '_')
    .replace(/\.{2,}/g, '.')
    .replace(/^\.+|\.+$/g, '')
    .split('.')
    .map((part) => {
      // Ensure each part starts with letter
      if (!/^[a-z]/.test(part)) {
        part = '_' + part;
      }
      return part;
    })
    .join('.');
}

/**
 * Sanitize output for logging (prevent log injection)
 */
export function sanitizeForLog(input: string, maxLength: number = 1000): string {
  let result = input
    // Remove newlines
    .replace(/[\r\n]/g, ' ')
    // Remove control characters
    .replace(/[\x00-\x1F\x7F]/g, '')
    // Escape common log injection patterns
    .replace(/\${/g, '\\${');

  // Truncate if too long
  if (result.length > maxLength) {
    result = result.substring(0, maxLength) + '...[truncated]';
  }

  return result;
}

/**
 * Redact sensitive information
 */
export function redactSensitive(input: string): string {
  return input
    // Redact potential API keys (32+ char alphanumeric)
    .replace(/\b[a-zA-Z0-9]{32,}\b/g, '[REDACTED]')
    // Redact potential passwords in key=value format
    .replace(/(password|passwd|pwd|secret|token|api[_-]?key)\s*[=:]\s*\S+/gi, '$1=[REDACTED]')
    // Redact email addresses
    .replace(/[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}/g, '[EMAIL_REDACTED]');
}
