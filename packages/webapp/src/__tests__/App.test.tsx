/**
 * App component tests
 */
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import App from '../App';
import { convert } from '@cobol2java/core';

// Mock the convert function
vi.mock('@cobol2java/core', async () => {
  const actual = await vi.importActual('@cobol2java/core');
  return {
    ...actual,
    convert: vi.fn(),
  };
});

const mockConvertResult = {
  java: `public class HelloWorld {
  public static void main(String[] args) {
    System.out.println("HELLO, WORLD!");
  }
}`,
  className: 'HelloWorld',
  errors: [],
  warnings: [],
  metadata: {
    programName: 'HELLO-WORLD',
    linesConverted: 25,
    durationMs: 42,
    llmProvider: 'none',
    timestamp: '2024-01-01T00:00:00.000Z',
  },
};

describe('App', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.mocked(convert).mockResolvedValue(mockConvertResult);
  });

  describe('Rendering', () => {
    it('renders the app with header and panels', () => {
      render(<App />);
      
      // Check for app title in header (COBOL2Java)
      expect(screen.getByRole('heading', { level: 1 })).toBeInTheDocument();
      expect(screen.getByText('COBOL Source')).toBeInTheDocument();
      expect(screen.getByText('Java Output')).toBeInTheDocument();
    });

    it('renders with sample COBOL code preloaded', () => {
      render(<App />);
      
      const cobolEditor = screen.getByTestId('editor-cobol');
      expect(cobolEditor).toBeInTheDocument();
      
      const textarea = cobolEditor.querySelector('textarea');
      expect(textarea?.value).toContain('HELLO-WORLD');
    });

    it('renders sample selector with options', () => {
      render(<App />);
      
      const selector = screen.getByLabelText(/Sample:/i);
      expect(selector).toBeInTheDocument();
      expect(selector).toHaveValue('Hello World');
    });

    it('renders history button', () => {
      render(<App />);
      
      expect(screen.getByText(/History/i)).toBeInTheDocument();
    });

    it('renders status bar with ready state', () => {
      render(<App />);
      
      expect(screen.getByText('Ready')).toBeInTheDocument();
    });
  });

  describe('Sample Selection', () => {
    it('changes COBOL source when selecting different sample', () => {
      render(<App />);
      
      const selector = screen.getByLabelText(/Sample:/i);
      fireEvent.change(selector, { target: { value: 'Fibonacci' } });
      
      const cobolEditor = screen.getByTestId('editor-cobol');
      const textarea = cobolEditor.querySelector('textarea');
      expect(textarea?.value).toContain('FIBONACCI');
    });

    it('clears Java output when changing sample', async () => {
      render(<App />);
      
      // First convert
      const convertButton = screen.getByRole('button', { name: /Convert/i });
      fireEvent.click(convertButton);
      
      await waitFor(() => {
        expect(screen.getByTestId('editor-java')).toBeInTheDocument();
      });
      
      // Change sample
      const selector = screen.getByLabelText(/Sample:/i);
      fireEvent.change(selector, { target: { value: 'Fibonacci' } });
      
      // Java output should be cleared
      const javaEditor = screen.getByTestId('editor-java');
      const textarea = javaEditor.querySelector('textarea');
      expect(textarea?.value).toBe('');
    });
  });

  describe('Conversion', () => {
    it('calls convert with correct options when clicking Convert', async () => {
      render(<App />);
      
      const convertButton = screen.getByRole('button', { name: /Convert/i });
      fireEvent.click(convertButton);
      
      await waitFor(() => {
        expect(convert).toHaveBeenCalled();
      });
      
      expect(convert).toHaveBeenCalledWith(
        expect.stringContaining('HELLO-WORLD'),
        expect.objectContaining({
          llmProvider: 'none',
          packageName: 'com.example',
          javaVersion: 17,
          springBoot: false,
          springBatch: false,
          generateValidation: false,
        })
      );
    });

    it('displays Java output after successful conversion', async () => {
      render(<App />);
      
      const convertButton = screen.getByRole('button', { name: /Convert/i });
      fireEvent.click(convertButton);
      
      await waitFor(() => {
        const javaEditor = screen.getByTestId('editor-java');
        const textarea = javaEditor.querySelector('textarea');
        expect(textarea?.value).toContain('HelloWorld');
      });
    });

    it('shows loading state during conversion', async () => {
      vi.mocked(convert).mockImplementation(() => 
        new Promise(resolve => setTimeout(() => resolve(mockConvertResult), 100))
      );
      
      render(<App />);
      
      const convertButton = screen.getByRole('button', { name: /Convert/i });
      fireEvent.click(convertButton);
      
      // Multiple "Converting..." texts appear - status bar and spinner
      expect(screen.getAllByText('Converting...').length).toBeGreaterThan(0);
      
      await waitFor(() => {
        expect(screen.queryByText('Converting...')).not.toBeInTheDocument();
      });
    });

    it('handles conversion errors gracefully', async () => {
      vi.mocked(convert).mockRejectedValue(new Error('Conversion failed'));
      
      render(<App />);
      
      const convertButton = screen.getByRole('button', { name: /Convert/i });
      fireEvent.click(convertButton);
      
      await waitFor(() => {
        expect(screen.getByText(/error/i)).toBeInTheDocument();
      });
    });
  });

  describe('Options', () => {
    it('includes Spring Boot option when checked', async () => {
      render(<App />);
      
      const springBootCheckbox = screen.getByLabelText(/Spring Boot/i);
      fireEvent.click(springBootCheckbox);
      
      const convertButton = screen.getByRole('button', { name: /Convert/i });
      fireEvent.click(convertButton);
      
      await waitFor(() => {
        expect(convert).toHaveBeenCalledWith(
          expect.any(String),
          expect.objectContaining({
            springBoot: true,
          })
        );
      });
    });

    it('includes Spring Batch option when checked', async () => {
      render(<App />);
      
      const springBatchCheckbox = screen.getByLabelText(/Spring Batch/i);
      fireEvent.click(springBatchCheckbox);
      
      const convertButton = screen.getByRole('button', { name: /Convert/i });
      fireEvent.click(convertButton);
      
      await waitFor(() => {
        expect(convert).toHaveBeenCalledWith(
          expect.any(String),
          expect.objectContaining({
            springBatch: true,
          })
        );
      });
    });

    it('updates package name option', async () => {
      render(<App />);
      
      const packageInput = screen.getByDisplayValue('com.example');
      fireEvent.change(packageInput, { target: { value: 'org.mycompany' } });
      
      const convertButton = screen.getByRole('button', { name: /Convert/i });
      fireEvent.click(convertButton);
      
      await waitFor(() => {
        expect(convert).toHaveBeenCalledWith(
          expect.any(String),
          expect.objectContaining({
            packageName: 'org.mycompany',
          })
        );
      });
    });
  });

  describe('Actions', () => {
    it('copies Java output to clipboard', async () => {
      render(<App />);
      
      // First convert
      const convertButton = screen.getByRole('button', { name: /Convert/i });
      fireEvent.click(convertButton);
      
      await waitFor(() => {
        expect(screen.getByTestId('editor-java').querySelector('textarea')?.value).toBeTruthy();
      });
      
      // Copy
      const copyButton = screen.getByRole('button', { name: /Copy/i });
      fireEvent.click(copyButton);
      
      expect(navigator.clipboard.writeText).toHaveBeenCalledWith(mockConvertResult.java);
    });

    it('downloads Java file', async () => {
      render(<App />);
      
      // First convert
      const convertButton = screen.getByRole('button', { name: /Convert/i });
      fireEvent.click(convertButton);
      
      await waitFor(() => {
        expect(screen.getByTestId('editor-java').querySelector('textarea')?.value).toBeTruthy();
      });
      
      // Download
      const downloadButton = screen.getByRole('button', { name: /Download/i });
      fireEvent.click(downloadButton);
      
      expect(URL.createObjectURL).toHaveBeenCalled();
    });
  });

  describe('History', () => {
    it('adds conversion to history', async () => {
      render(<App />);
      
      // Convert
      const convertButton = screen.getByRole('button', { name: /Convert/i });
      fireEvent.click(convertButton);
      
      await waitFor(() => {
        expect(screen.getByText(/History \(1\)/i)).toBeInTheDocument();
      });
    });

    it('shows history panel when clicking history button', async () => {
      render(<App />);
      
      // Convert first
      const convertButton = screen.getByRole('button', { name: /Convert/i });
      fireEvent.click(convertButton);
      
      await waitFor(() => {
        expect(screen.getByText(/History \(1\)/i)).toBeInTheDocument();
      });
      
      // Toggle history
      const historyButton = screen.getByText(/History/i);
      fireEvent.click(historyButton);
      
      expect(screen.getByText('HelloWorld')).toBeInTheDocument();
      expect(screen.getByText(/25 lines/)).toBeInTheDocument();
    });
  });
});
