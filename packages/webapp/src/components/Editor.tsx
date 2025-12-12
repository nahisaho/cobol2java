import MonacoEditor from '@monaco-editor/react';
import { useCallback } from 'react';

interface EditorProps {
  value: string;
  onChange?: (value: string) => void;
  language: 'cobol' | 'java';
  readOnly?: boolean;
  placeholder?: string;
}

function Editor({ value, onChange, language, readOnly }: EditorProps) {
  const handleChange = useCallback((newValue: string | undefined) => {
    if (onChange && newValue !== undefined) {
      onChange(newValue);
    }
  }, [onChange]);

  return (
    <div className="editor-container">
      <MonacoEditor
        height="100%"
        language={language === 'cobol' ? 'cobol' : 'java'}
        value={value}
        onChange={handleChange}
        theme="vs-dark"
        options={{
          readOnly,
          minimap: { enabled: false },
          fontSize: 14,
          fontFamily: "'JetBrains Mono', 'Fira Code', 'Consolas', monospace",
          lineNumbers: 'on',
          scrollBeyondLastLine: false,
          automaticLayout: true,
          tabSize: 4,
          wordWrap: 'on',
          renderWhitespace: 'selection',
        }}
        beforeMount={(monaco) => {
          // Register COBOL language if not exists
          if (!monaco.languages.getLanguages().some((lang: { id: string }) => lang.id === 'cobol')) {
            monaco.languages.register({ id: 'cobol' });
            monaco.languages.setMonarchTokensProvider('cobol', {
              ignoreCase: true,
              keywords: [
                'IDENTIFICATION', 'DIVISION', 'PROGRAM-ID', 'AUTHOR', 'DATE-WRITTEN',
                'ENVIRONMENT', 'CONFIGURATION', 'SECTION', 'SOURCE-COMPUTER', 'OBJECT-COMPUTER',
                'INPUT-OUTPUT', 'FILE-CONTROL', 'SELECT', 'ASSIGN',
                'DATA', 'FILE', 'WORKING-STORAGE', 'LOCAL-STORAGE', 'LINKAGE',
                'PROCEDURE', 'PERFORM', 'MOVE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
                'COMPUTE', 'IF', 'ELSE', 'END-IF', 'EVALUATE', 'WHEN', 'END-EVALUATE',
                'DISPLAY', 'ACCEPT', 'STOP', 'RUN', 'GO', 'TO', 'THRU', 'THROUGH',
                'OPEN', 'CLOSE', 'READ', 'WRITE', 'REWRITE', 'DELETE', 'START',
                'CALL', 'USING', 'RETURNING', 'EXIT', 'GOBACK', 'CONTINUE',
                'STRING', 'UNSTRING', 'INSPECT', 'REPLACING', 'TALLYING',
                'INITIALIZE', 'SET', 'SEARCH', 'SORT', 'MERGE', 'RELEASE', 'RETURN',
                'PIC', 'PICTURE', 'VALUE', 'OCCURS', 'TIMES', 'REDEFINES',
                'FILLER', 'COMP', 'COMP-3', 'PACKED-DECIMAL', 'BINARY',
                'NOT', 'AND', 'OR', 'EQUAL', 'GREATER', 'LESS', 'THAN',
                'ZERO', 'ZEROS', 'ZEROES', 'SPACE', 'SPACES', 'LOW-VALUE', 'HIGH-VALUE',
                'TRUE', 'FALSE', 'GIVING', 'ROUNDED', 'ON', 'SIZE', 'ERROR',
                'INTO', 'FROM', 'BY', 'UNTIL', 'VARYING', 'AFTER', 'BEFORE',
                'WITH', 'TEST', 'AT', 'END', 'OF', 'ALL', 'CORRESPONDING',
              ],
              tokenizer: {
                root: [
                  [/^\s{6}\*.*$/, 'comment'],
                  [/"[^"]*"/, 'string'],
                  [/'[^']*'/, 'string'],
                  [/\b[0-9]+\b/, 'number'],
                  [/\b(PIC|PICTURE)\s+[A-Z0-9()V.+-]+/i, 'type'],
                  [/[A-Z][A-Z0-9-]*/i, {
                    cases: {
                      '@keywords': 'keyword',
                      '@default': 'identifier',
                    },
                  }],
                ],
              },
            });
          }
        }}
      />
    </div>
  );
}

export default Editor;
