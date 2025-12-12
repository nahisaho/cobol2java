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
  springBatch: boolean;
  onSpringBatchChange: (value: boolean) => void;
  generateValidation: boolean;
  onGenerateValidationChange: (value: boolean) => void;
  packageName: string;
  onPackageNameChange: (value: string) => void;
  onFileUpload: (content: string) => void;
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
  springBatch,
  onSpringBatchChange,
  generateValidation,
  onGenerateValidationChange,
  packageName,
  onPackageNameChange,
  onFileUpload,
}: HeaderProps) {
  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (file) {
      const reader = new FileReader();
      reader.onload = (event) => {
        const content = event.target?.result as string;
        onFileUpload(content);
      };
      reader.readAsText(file);
    }
  };

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

        <label className="btn btn-secondary file-upload">
          📁 Upload
          <input
            type="file"
            accept=".cbl,.cob,.cobol,.txt"
            onChange={handleFileChange}
            style={{ display: 'none' }}
          />
        </label>

        <div className="options-group">
          <label className="checkbox-label">
            <input
              type="checkbox"
              checked={springBoot}
              onChange={(e) => onSpringBootChange(e.target.checked)}
            />
            Spring Boot
          </label>
          <label className="checkbox-label">
            <input
              type="checkbox"
              checked={springBatch}
              onChange={(e) => onSpringBatchChange(e.target.checked)}
            />
            Spring Batch
          </label>
          <label className="checkbox-label">
            <input
              type="checkbox"
              checked={generateValidation}
              onChange={(e) => onGenerateValidationChange(e.target.checked)}
            />
            Validation
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
