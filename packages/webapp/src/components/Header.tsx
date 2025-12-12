interface HeaderProps {
  onConvert: () => void;
  isConverting: boolean;
  samples: string[];
  selectedSample: string;
  onSampleChange: (sample: string) => void;
  hasOutput: boolean;
  onCopy: () => void;
  onDownload: () => void;
  springBoot: boolean;
  onSpringBootChange: (value: boolean) => void;
  packageName: string;
  onPackageNameChange: (value: string) => void;
}

function Header({
  onConvert,
  isConverting,
  samples,
  selectedSample,
  onSampleChange,
  hasOutput,
  onCopy,
  onDownload,
  springBoot,
  onSpringBootChange,
  packageName,
  onPackageNameChange,
}: HeaderProps) {
  return (
    <header className="header">
      <h1>
        <span>⚙️</span> COBOL<span>2</span>Java
      </h1>

      <div className="header-actions">
        <div className="sample-select">
          <label htmlFor="sample">Sample:</label>
          <select
            id="sample"
            className="select"
            value={selectedSample}
            onChange={(e) => onSampleChange(e.target.value)}
          >
            {samples.map((sample) => (
              <option key={sample} value={sample}>
                {sample}
              </option>
            ))}
          </select>
        </div>

        <div className="options-group">
          <label className="checkbox-label">
            <input
              type="checkbox"
              checked={springBoot}
              onChange={(e) => onSpringBootChange(e.target.checked)}
            />
            Spring Boot
          </label>
          <input
            type="text"
            className="package-input"
            placeholder="Package name"
            value={packageName}
            onChange={(e) => onPackageNameChange(e.target.value)}
          />
        </div>

        <button
          className="btn btn-primary"
          onClick={onConvert}
          disabled={isConverting}
        >
          {isConverting ? 'Converting...' : '▶ Convert'}
        </button>

        {hasOutput && (
          <>
            <button className="btn btn-secondary" onClick={onCopy}>
              📋 Copy
            </button>
            <button className="btn btn-secondary" onClick={onDownload}>
              ⬇ Download
            </button>
          </>
        )}
      </div>
    </header>
  );
}

export default Header;
