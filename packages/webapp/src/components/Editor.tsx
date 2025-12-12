interface EditorProps {
  value: string;
  onChange?: (value: string) => void;
  language: 'cobol' | 'java';
  readOnly?: boolean;
  placeholder?: string;
}

function Editor({ value, onChange, language, readOnly, placeholder }: EditorProps) {
  return (
    <div className="editor-container">
      <textarea
        className="textarea-editor"
        value={value}
        onChange={(e) => onChange?.(e.target.value)}
        readOnly={readOnly}
        placeholder={placeholder}
        spellCheck={false}
        data-language={language}
      />
    </div>
  );
}

export default Editor;
