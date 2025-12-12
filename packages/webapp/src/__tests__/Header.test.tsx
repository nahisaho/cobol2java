/**
 * Header component tests
 */
import { describe, it, expect, vi } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import Header from '../components/Header';

describe('Header', () => {
  const defaultProps = {
    onConvert: vi.fn(),
    isConverting: false,
    samples: ['Hello World', 'Fibonacci', 'EXEC SQL'],
    selectedSample: 'Hello World',
    onSampleChange: vi.fn(),
    hasOutput: false,
    onCopy: vi.fn(),
    onDownload: vi.fn(),
    springBoot: false,
    onSpringBootChange: vi.fn(),
    springBatch: false,
    onSpringBatchChange: vi.fn(),
    generateValidation: false,
    onGenerateValidationChange: vi.fn(),
    packageName: 'com.example',
    onPackageNameChange: vi.fn(),
    onFileUpload: vi.fn(),
  };

  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders header with title', () => {
      render(<Header {...defaultProps} />);
      
      expect(screen.getByText(/COBOL/i)).toBeInTheDocument();
      expect(screen.getByText(/Java/i)).toBeInTheDocument();
    });

    it('renders Convert button', () => {
      render(<Header {...defaultProps} />);
      
      expect(screen.getByRole('button', { name: /Convert/i })).toBeInTheDocument();
    });

    it('renders sample selector with all options', () => {
      render(<Header {...defaultProps} />);
      
      const selector = screen.getByLabelText(/Sample:/i);
      expect(selector).toBeInTheDocument();
      
      defaultProps.samples.forEach(sample => {
        expect(screen.getByRole('option', { name: sample })).toBeInTheDocument();
      });
    });

    it('renders file upload button', () => {
      render(<Header {...defaultProps} />);
      
      expect(screen.getByText(/Upload/i)).toBeInTheDocument();
    });

    it('renders option checkboxes', () => {
      render(<Header {...defaultProps} />);
      
      expect(screen.getByLabelText(/Spring Boot/i)).toBeInTheDocument();
      expect(screen.getByLabelText(/Spring Batch/i)).toBeInTheDocument();
      expect(screen.getByLabelText(/Validation/i)).toBeInTheDocument();
    });

    it('renders package name input', () => {
      render(<Header {...defaultProps} />);
      
      expect(screen.getByDisplayValue('com.example')).toBeInTheDocument();
    });
  });

  describe('Convert Button', () => {
    it('calls onConvert when clicked', () => {
      render(<Header {...defaultProps} />);
      
      fireEvent.click(screen.getByRole('button', { name: /Convert/i }));
      
      expect(defaultProps.onConvert).toHaveBeenCalledTimes(1);
    });

    it('shows loading state when converting', () => {
      render(<Header {...defaultProps} isConverting={true} />);
      
      expect(screen.getByRole('button', { name: /Converting/i })).toBeInTheDocument();
    });

    it('disables Convert button when converting', () => {
      render(<Header {...defaultProps} isConverting={true} />);
      
      const button = screen.getByRole('button', { name: /Converting/i });
      expect(button).toBeDisabled();
    });
  });

  describe('Sample Selection', () => {
    it('calls onSampleChange when sample is selected', () => {
      render(<Header {...defaultProps} />);
      
      const selector = screen.getByLabelText(/Sample:/i);
      fireEvent.change(selector, { target: { value: 'Fibonacci' } });
      
      expect(defaultProps.onSampleChange).toHaveBeenCalledWith('Fibonacci');
    });
  });

  describe('Options', () => {
    it('calls onSpringBootChange when Spring Boot is toggled', () => {
      render(<Header {...defaultProps} />);
      
      fireEvent.click(screen.getByLabelText(/Spring Boot/i));
      
      expect(defaultProps.onSpringBootChange).toHaveBeenCalledWith(true);
    });

    it('calls onSpringBatchChange when Spring Batch is toggled', () => {
      render(<Header {...defaultProps} />);
      
      fireEvent.click(screen.getByLabelText(/Spring Batch/i));
      
      expect(defaultProps.onSpringBatchChange).toHaveBeenCalledWith(true);
    });

    it('calls onGenerateValidationChange when Validation is toggled', () => {
      render(<Header {...defaultProps} />);
      
      fireEvent.click(screen.getByLabelText(/Validation/i));
      
      expect(defaultProps.onGenerateValidationChange).toHaveBeenCalledWith(true);
    });

    it('calls onPackageNameChange when package name is changed', () => {
      render(<Header {...defaultProps} />);
      
      const input = screen.getByDisplayValue('com.example');
      fireEvent.change(input, { target: { value: 'org.company' } });
      
      expect(defaultProps.onPackageNameChange).toHaveBeenCalledWith('org.company');
    });

    it('shows checked state correctly for Spring Boot', () => {
      render(<Header {...defaultProps} springBoot={true} />);
      
      const checkbox = screen.getByLabelText(/Spring Boot/i);
      expect(checkbox).toBeChecked();
    });
  });

  describe('Copy and Download', () => {
    it('shows copy button when output exists', () => {
      render(<Header {...defaultProps} hasOutput={true} />);
      
      expect(screen.getByText(/Copy/i)).toBeInTheDocument();
    });

    it('shows download button when output exists', () => {
      render(<Header {...defaultProps} hasOutput={true} />);
      
      expect(screen.getByText(/Download/i)).toBeInTheDocument();
    });

    it('calls onCopy when copy button is clicked', () => {
      render(<Header {...defaultProps} hasOutput={true} />);
      
      fireEvent.click(screen.getByText(/Copy/i));
      
      expect(defaultProps.onCopy).toHaveBeenCalledTimes(1);
    });

    it('calls onDownload when download button is clicked', () => {
      render(<Header {...defaultProps} hasOutput={true} />);
      
      fireEvent.click(screen.getByText(/Download/i));
      
      expect(defaultProps.onDownload).toHaveBeenCalledTimes(1);
    });
  });

  describe('File Upload', () => {
    it('accepts .cbl, .cob, .cobol, .txt files', () => {
      render(<Header {...defaultProps} />);
      
      const input = document.querySelector('input[type="file"]');
      expect(input).toHaveAttribute('accept', '.cbl,.cob,.cobol,.txt');
    });

    it('calls onFileUpload when file is uploaded', async () => {
      render(<Header {...defaultProps} />);
      
      const input = document.querySelector('input[type="file"]') as HTMLInputElement;
      const file = new File(['COBOL CODE'], 'test.cbl', { type: 'text/plain' });
      
      // Mock FileReader
      const mockFileReader = {
        readAsText: vi.fn(),
        onload: null as ((e: ProgressEvent<FileReader>) => void) | null,
        result: 'COBOL CODE',
      };
      vi.spyOn(window, 'FileReader').mockImplementation(() => mockFileReader as unknown as FileReader);
      
      Object.defineProperty(input, 'files', {
        value: [file],
      });
      
      fireEvent.change(input);
      
      // Simulate FileReader onload
      if (mockFileReader.onload) {
        mockFileReader.onload({ target: { result: 'COBOL CODE' } } as ProgressEvent<FileReader>);
      }
      
      expect(defaultProps.onFileUpload).toHaveBeenCalledWith('COBOL CODE');
    });
  });
});
