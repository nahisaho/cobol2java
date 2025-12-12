/**
 * Security module tests
 */
import { describe, it, expect } from 'vitest';
import {
  checkLimits,
  DEFAULT_LIMITS,
  STRICT_LIMITS,
  withTimeout,
} from '../src/security/limits.js';
import {
  validateInput,
  validateIdentifier,
  validateFilePath,
} from '../src/security/validator.js';
import {
  sanitizeInput,
  sanitizeForJavaString,
  sanitizeForJavaIdentifier,
  sanitizeForJavaPackage,
  sanitizeForLog,
  redactSensitive,
} from '../src/security/sanitizer.js';

describe('Security Tests', () => {
  describe('Limits', () => {
    describe('checkLimits', () => {
      it('should accept valid input within limits', () => {
        const input = 'IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.';
        const result = checkLimits(input);
        expect(result.valid).toBe(true);
        expect(result.violations).toHaveLength(0);
      });

      it('should reject input exceeding size limit', () => {
        const input = 'A'.repeat(11 * 1024 * 1024); // 11MB
        const result = checkLimits(input);
        expect(result.valid).toBe(false);
        expect(result.violations.some(v => v.limit === 'maxInputSize')).toBe(true);
      });

      it('should reject input exceeding line count', () => {
        const input = 'LINE\n'.repeat(150_000);
        const result = checkLimits(input);
        expect(result.valid).toBe(false);
        expect(result.violations.some(v => v.limit === 'maxLines')).toBe(true);
      });

      it('should reject input with long lines', () => {
        const input = 'A'.repeat(600);
        const result = checkLimits(input);
        expect(result.valid).toBe(false);
        expect(result.violations.some(v => v.limit === 'maxLineLength')).toBe(true);
      });

      it('should use custom limits', () => {
        const input = 'A'.repeat(100);
        const result = checkLimits(input, { maxLineLength: 50 });
        expect(result.valid).toBe(false);
      });

      it('should have stricter STRICT_LIMITS', () => {
        expect(STRICT_LIMITS.maxInputSize).toBeLessThan(DEFAULT_LIMITS.maxInputSize);
        expect(STRICT_LIMITS.maxLines).toBeLessThan(DEFAULT_LIMITS.maxLines);
      });
    });

    describe('withTimeout', () => {
      it('should resolve before timeout', async () => {
        const result = await withTimeout(
          Promise.resolve('success'),
          1000,
          'test'
        );
        expect(result).toBe('success');
      });

      it('should reject on timeout', async () => {
        const slowPromise = new Promise((resolve) => setTimeout(resolve, 500));
        await expect(
          withTimeout(slowPromise, 50, 'slow operation')
        ).rejects.toThrow('timed out');
      });

      it('should propagate errors', async () => {
        await expect(
          withTimeout(Promise.reject(new Error('fail')), 1000, 'test')
        ).rejects.toThrow('fail');
      });
    });
  });

  describe('Validator', () => {
    describe('validateInput', () => {
      it('should accept valid COBOL input', () => {
        const input = `
          IDENTIFICATION DIVISION.
          PROGRAM-ID. TEST.
        `;
        const result = validateInput(input);
        expect(result.valid).toBe(true);
      });

      it('should reject empty input by default', () => {
        const result = validateInput('');
        expect(result.valid).toBe(false);
        expect(result.errors.some(e => e.code === 'EMPTY_INPUT')).toBe(true);
      });

      it('should allow empty input when configured', () => {
        const result = validateInput('', { allowEmpty: true });
        expect(result.valid).toBe(true);
      });

      it('should warn on potential script injection', () => {
        const input = 'IDENTIFICATION DIVISION. <script>alert(1)</script>';
        const result = validateInput(input);
        expect(result.warnings.some(w => w.code === 'SCRIPT_INJECTION')).toBe(true);
      });

      it('should warn on path traversal', () => {
        const input = 'COPY "../../../etc/passwd".';
        const result = validateInput(input);
        expect(result.warnings.some(w => w.code === 'PATH_TRAVERSAL')).toBe(true);
      });

      it('should detect null bytes', () => {
        const input = 'PROGRAM\x00ID';
        const result = validateInput(input);
        expect(result.warnings.some(w => w.code === 'NULL_BYTE')).toBe(true);
      });

      it('should warn on missing division in strict mode', () => {
        const input = 'JUST SOME TEXT';
        const result = validateInput(input, { strictCobol: true });
        expect(result.warnings.some(w => w.code === 'NO_DIVISION')).toBe(true);
      });

      it('should detect binary content', () => {
        const input = '\x89PNG\r\n\x1a\nBINARY';
        const result = validateInput(input);
        expect(result.valid).toBe(false);
        expect(result.errors.some(e => e.code === 'BINARY_CONTENT')).toBe(true);
      });

      it('should detect encoding issues', () => {
        const input = 'TEXT\uFFFDWITH\uFFFD REPLACEMENT';
        const result = validateInput(input);
        expect(result.warnings.some(w => w.code === 'ENCODING_ISSUE')).toBe(true);
      });
    });

    describe('validateIdentifier', () => {
      it('should accept valid COBOL identifiers', () => {
        expect(validateIdentifier('CUSTOMER-NAME').valid).toBe(true);
        expect(validateIdentifier('WS-COUNTER').valid).toBe(true);
        expect(validateIdentifier('A123').valid).toBe(true);
      });

      it('should reject empty identifiers', () => {
        expect(validateIdentifier('').valid).toBe(false);
      });

      it('should reject identifiers starting with numbers', () => {
        expect(validateIdentifier('123-ABC').valid).toBe(false);
      });

      it('should reject identifiers with invalid characters', () => {
        expect(validateIdentifier('CUSTOMER@NAME').valid).toBe(false);
        expect(validateIdentifier('DATA$ITEM').valid).toBe(false);
      });

      it('should reject identifiers exceeding max length', () => {
        const longId = 'A'.repeat(300);
        expect(validateIdentifier(longId).valid).toBe(false);
      });

      it('should warn on COBOL reserved words', () => {
        const result = validateIdentifier('MOVE');
        expect(result.warnings.some(w => w.code === 'RESERVED_WORD')).toBe(true);
      });
    });

    describe('validateFilePath', () => {
      it('should accept valid relative paths', () => {
        expect(validateFilePath('src/main.cbl').valid).toBe(true);
        expect(validateFilePath('programs/customer.cob').valid).toBe(true);
      });

      it('should reject empty paths', () => {
        expect(validateFilePath('').valid).toBe(false);
      });

      it('should reject path traversal', () => {
        expect(validateFilePath('../../../etc/passwd').valid).toBe(false);
        expect(validateFilePath('src/..\\..\\passwd').valid).toBe(false);
      });

      it('should warn on absolute paths', () => {
        const result = validateFilePath('/etc/cobol/program.cbl');
        expect(result.warnings.some(w => w.code === 'ABSOLUTE_PATH')).toBe(true);
      });

      it('should reject paths with invalid characters', () => {
        expect(validateFilePath('file<name>.cbl').valid).toBe(false);
        expect(validateFilePath('file\x00name.cbl').valid).toBe(false);
      });

      it('should reject very long paths', () => {
        const longPath = 'a/'.repeat(2500);
        expect(validateFilePath(longPath).valid).toBe(false);
      });
    });
  });

  describe('Sanitizer', () => {
    describe('sanitizeInput', () => {
      it('should remove BOM', () => {
        const input = '\uFEFFIDENTIFICATION DIVISION.';
        const result = sanitizeInput(input);
        expect(result.startsWith('IDENTIFICATION')).toBe(true);
      });

      it('should remove null bytes', () => {
        const input = 'PROGRAM\x00ID';
        const result = sanitizeInput(input);
        expect(result).toBe('PROGRAMID');
      });

      it('should remove Unicode direction overrides', () => {
        const input = 'TEXT\u202EWITH\u202CRTL';
        const result = sanitizeInput(input);
        expect(result).toBe('TEXTWITHRTL');
      });

      it('should normalize line endings', () => {
        const input = 'LINE1\r\nLINE2\rLINE3\nLINE4';
        const result = sanitizeInput(input);
        expect(result).toBe('LINE1\nLINE2\nLINE3\nLINE4');
      });

      it('should remove control characters', () => {
        const input = 'TEXT\x01\x02\x03WITH\x1FCONTROLS';
        const result = sanitizeInput(input);
        expect(result).toBe('TEXTWITHCONTROLS');
      });

      it('should preserve tabs and newlines', () => {
        const input = 'LINE1\n\tINDENTED';
        const result = sanitizeInput(input);
        expect(result).toBe('LINE1\n\tINDENTED');
      });

      it('should trim lines when configured', () => {
        const input = 'LINE1   \nLINE2\t\n';
        const result = sanitizeInput(input, { trimLines: true });
        expect(result).toBe('LINE1\nLINE2\n');
      });

      it('should truncate long lines when configured', () => {
        const input = 'A'.repeat(100);
        const result = sanitizeInput(input, { maxLineLength: 50 });
        expect(result.length).toBe(50);
      });

      it('should escape HTML when configured', () => {
        const input = '<script>alert("XSS")</script>';
        const result = sanitizeInput(input, { escapeHtml: true });
        expect(result).toBe('&lt;script&gt;alert(&quot;XSS&quot;)&lt;/script&gt;');
      });
    });

    describe('sanitizeForJavaString', () => {
      it('should escape backslashes', () => {
        expect(sanitizeForJavaString('C:\\path')).toBe('C:\\\\path');
      });

      it('should escape quotes', () => {
        expect(sanitizeForJavaString('Say "Hello"')).toBe('Say \\"Hello\\"');
      });

      it('should escape newlines', () => {
        expect(sanitizeForJavaString('Line1\nLine2')).toBe('Line1\\nLine2');
      });

      it('should escape control characters', () => {
        const result = sanitizeForJavaString('Text\x01Here');
        expect(result).toBe('Text\\u0001Here');
      });
    });

    describe('sanitizeForJavaIdentifier', () => {
      it('should convert COBOL names to valid Java identifiers', () => {
        expect(sanitizeForJavaIdentifier('CUSTOMER-NAME')).toBe('CUSTOMER_NAME');
        expect(sanitizeForJavaIdentifier('WS-DATA-ITEM')).toBe('WS_DATA_ITEM');
      });

      it('should handle identifiers starting with numbers', () => {
        expect(sanitizeForJavaIdentifier('123-ABC')).toBe('_123_ABC');
      });

      it('should handle reserved words', () => {
        expect(sanitizeForJavaIdentifier('class')).toBe('class_');
        expect(sanitizeForJavaIdentifier('public')).toBe('public_');
      });

      it('should remove consecutive underscores', () => {
        expect(sanitizeForJavaIdentifier('A--B--C')).toBe('A_B_C');
      });
    });

    describe('sanitizeForJavaPackage', () => {
      it('should convert to valid package names', () => {
        expect(sanitizeForJavaPackage('com.example.App')).toBe('com.example.app');
      });

      it('should remove invalid characters', () => {
        expect(sanitizeForJavaPackage('com.example-app.Main')).toBe('com.example_app.main');
      });

      it('should handle parts starting with numbers', () => {
        expect(sanitizeForJavaPackage('com.123app')).toBe('com._123app');
      });
    });

    describe('sanitizeForLog', () => {
      it('should remove newlines', () => {
        expect(sanitizeForLog('Line1\nLine2')).toBe('Line1 Line2');
      });

      it('should escape template literals', () => {
        expect(sanitizeForLog('${process.env}')).toBe('\\${process.env}');
      });

      it('should truncate long input', () => {
        const input = 'A'.repeat(2000);
        const result = sanitizeForLog(input, 100);
        expect(result.length).toBeLessThanOrEqual(120);
        expect(result).toContain('[truncated]');
      });
    });

    describe('redactSensitive', () => {
      it('should redact potential API keys', () => {
        // 32+ alphanumeric characters
        const input = 'key=abcdefghijklmnopqrstuvwxyz123456';
        const result = redactSensitive(input);
        expect(result).toContain('[REDACTED]');
      });

      it('should redact password fields', () => {
        const input = 'password=secret123';
        const result = redactSensitive(input);
        expect(result).toContain('[REDACTED]');
        expect(result).not.toContain('secret123');
      });

      it('should redact email addresses', () => {
        const input = 'user@example.com';
        const result = redactSensitive(input);
        expect(result).toBe('[EMAIL_REDACTED]');
      });
    });
  });
});
