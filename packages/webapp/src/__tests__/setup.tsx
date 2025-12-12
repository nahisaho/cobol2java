/**
 * Vitest setup file for React testing
 */
import '@testing-library/jest-dom';

// Mock Monaco Editor - it doesn't work in jsdom
vi.mock('@monaco-editor/react', () => ({
  default: ({ value, onChange, language, readOnly, placeholder }: {
    value: string;
    onChange?: (value: string) => void;
    language: string;
    readOnly?: boolean;
    placeholder?: string;
  }) => {
    return (
      <div data-testid={`editor-${language}`}>
        <textarea
          value={value}
          onChange={(e) => onChange?.(e.target.value)}
          readOnly={readOnly}
          placeholder={placeholder}
          data-language={language}
        />
      </div>
    );
  },
}));

// Mock clipboard API
Object.assign(navigator, {
  clipboard: {
    writeText: vi.fn().mockResolvedValue(undefined),
  },
});

// Mock URL.createObjectURL
global.URL.createObjectURL = vi.fn(() => 'blob:mock-url');
global.URL.revokeObjectURL = vi.fn();
