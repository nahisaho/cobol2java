import type { ConversionResult } from '@cobol2java/core';

interface StatusBarProps {
  result: ConversionResult | null;
  isConverting: boolean;
}

function StatusBar({ result, isConverting }: StatusBarProps) {
  if (isConverting) {
    return (
      <footer className="status-bar">
        <div className="status-item">Converting...</div>
      </footer>
    );
  }

  if (!result) {
    return (
      <footer className="status-bar">
        <div className="status-item">Ready</div>
        <div className="status-item">COBOL2Java v0.3.0</div>
      </footer>
    );
  }

  const hasErrors = result.errors.length > 0;
  const hasWarnings = result.warnings.length > 0;

  return (
    <footer className="status-bar">
      <div className="status-item">
        {hasErrors ? (
          <span className="status-error">
            ✕ {result.errors.length} error(s)
          </span>
        ) : (
          <span className="status-success">✓ Converted successfully</span>
        )}
        {hasWarnings && (
          <span className="status-warning" style={{ marginLeft: '12px' }}>
            ⚠ {result.warnings.length} warning(s)
          </span>
        )}
      </div>

      <div className="status-item">
        {result.metadata.programName && (
          <span style={{ marginRight: '16px' }}>
            Program: {result.metadata.programName}
          </span>
        )}
        <span style={{ marginRight: '16px' }}>
          Lines: {result.metadata.linesConverted}
        </span>
        <span>Duration: {result.metadata.durationMs}ms</span>
      </div>
    </footer>
  );
}

export default StatusBar;
