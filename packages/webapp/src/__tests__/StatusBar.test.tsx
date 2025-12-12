/**
 * StatusBar component tests
 */
import { describe, it, expect } from 'vitest';
import { render, screen } from '@testing-library/react';
import StatusBar from '../components/StatusBar';
import type { ConversionResult } from 'cobol2java-core';
import { ErrorSeverity } from 'cobol2java-core';

describe('StatusBar', () => {
  const createResult = (overrides: Partial<ConversionResult> = {}): ConversionResult => ({
    java: 'public class Test {}',
    className: 'Test',
    errors: [],
    warnings: [],
    metadata: {
      programName: 'TEST-PROGRAM',
      linesConverted: 50,
      durationMs: 100,
      llmProvider: 'none',
      timestamp: '2024-01-01T00:00:00.000Z',
    },
    ...overrides,
  });

  describe('Ready State', () => {
    it('shows "Ready" when no result and not converting', () => {
      render(<StatusBar result={null} isConverting={false} />);
      
      expect(screen.getByText('Ready')).toBeInTheDocument();
    });

    it('shows version info in ready state', () => {
      render(<StatusBar result={null} isConverting={false} />);
      
      expect(screen.getByText(/COBOL2Java v0\.1\.0/)).toBeInTheDocument();
    });
  });

  describe('Converting State', () => {
    it('shows "Converting..." when isConverting is true', () => {
      render(<StatusBar result={null} isConverting={true} />);
      
      expect(screen.getByText('Converting...')).toBeInTheDocument();
    });

    it('does not show Ready when converting', () => {
      render(<StatusBar result={null} isConverting={true} />);
      
      expect(screen.queryByText('Ready')).not.toBeInTheDocument();
    });
  });

  describe('Success State', () => {
    it('shows success message when no errors', () => {
      render(<StatusBar result={createResult()} isConverting={false} />);
      
      expect(screen.getByText(/Converted successfully/)).toBeInTheDocument();
    });

    it('shows program name', () => {
      render(<StatusBar result={createResult()} isConverting={false} />);
      
      expect(screen.getByText(/Program: TEST-PROGRAM/)).toBeInTheDocument();
    });

    it('shows lines converted', () => {
      render(<StatusBar result={createResult()} isConverting={false} />);
      
      expect(screen.getByText(/Lines: 50/)).toBeInTheDocument();
    });

    it('shows duration', () => {
      render(<StatusBar result={createResult()} isConverting={false} />);
      
      expect(screen.getByText(/Duration: 100ms/)).toBeInTheDocument();
    });
  });

  describe('Error State', () => {
    it('shows error count when errors exist', () => {
      const result = createResult({
        errors: [
          { code: 'CVT001', line: 1, message: 'Error 1', severity: ErrorSeverity.ERROR },
          { code: 'CVT002', line: 2, message: 'Error 2', severity: ErrorSeverity.ERROR },
        ],
      });
      
      render(<StatusBar result={result} isConverting={false} />);
      
      expect(screen.getByText(/2 error\(s\)/)).toBeInTheDocument();
    });

    it('shows error styling', () => {
      const result = createResult({
        errors: [
          { code: 'CVT001', line: 1, message: 'Error', severity: ErrorSeverity.ERROR },
        ],
      });
      
      render(<StatusBar result={result} isConverting={false} />);
      
      const errorElement = screen.getByText(/1 error\(s\)/);
      expect(errorElement).toHaveClass('status-error');
    });
  });

  describe('Warning State', () => {
    it('shows warning count when warnings exist', () => {
      const result = createResult({
        warnings: [
          { code: 'CVT100', line: 1, message: 'Warning 1', severity: ErrorSeverity.WARNING },
          { code: 'CVT101', line: 2, message: 'Warning 2', severity: ErrorSeverity.WARNING },
          { code: 'CVT102', line: 3, message: 'Warning 3', severity: ErrorSeverity.WARNING },
        ],
      });
      
      render(<StatusBar result={result} isConverting={false} />);
      
      expect(screen.getByText(/3 warning\(s\)/)).toBeInTheDocument();
    });

    it('shows warning styling', () => {
      const result = createResult({
        warnings: [
          { code: 'CVT100', line: 1, message: 'Warning', severity: ErrorSeverity.WARNING },
        ],
      });
      
      render(<StatusBar result={result} isConverting={false} />);
      
      const warningElement = screen.getByText(/1 warning\(s\)/);
      expect(warningElement).toHaveClass('status-warning');
    });

    it('shows both errors and warnings', () => {
      const result = createResult({
        errors: [
          { code: 'CVT001', line: 1, message: 'Error', severity: ErrorSeverity.ERROR },
        ],
        warnings: [
          { code: 'CVT100', line: 1, message: 'Warning', severity: ErrorSeverity.WARNING },
        ],
      });
      
      render(<StatusBar result={result} isConverting={false} />);
      
      expect(screen.getByText(/1 error\(s\)/)).toBeInTheDocument();
      expect(screen.getByText(/1 warning\(s\)/)).toBeInTheDocument();
    });
  });

  describe('Metadata Display', () => {
    it('handles missing program name', () => {
      const result = createResult({
        metadata: {
          programName: '',
          linesConverted: 25,
          durationMs: 50,
          llmProvider: 'none',
          timestamp: '2024-01-01T00:00:00.000Z',
        },
      });
      
      render(<StatusBar result={result} isConverting={false} />);
      
      expect(screen.queryByText(/Program:/)).not.toBeInTheDocument();
      expect(screen.getByText(/Lines: 25/)).toBeInTheDocument();
    });

    it('displays metadata with zero values', () => {
      const result = createResult({
        metadata: {
          programName: 'TEST',
          linesConverted: 0,
          durationMs: 0,
          llmProvider: 'none',
          timestamp: '2024-01-01T00:00:00.000Z',
        },
      });
      
      render(<StatusBar result={result} isConverting={false} />);
      
      expect(screen.getByText(/Lines: 0/)).toBeInTheDocument();
      expect(screen.getByText(/Duration: 0ms/)).toBeInTheDocument();
    });
  });
});
