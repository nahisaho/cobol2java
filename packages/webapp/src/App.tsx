import { useState, useCallback } from 'react';
import { convert, type ConversionResult, ErrorSeverity } from '@cobol2java/core';
import Editor from './components/Editor';
import Header from './components/Header';
import StatusBar from './components/StatusBar';

const SAMPLE_COBOL = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       AUTHOR. COBOL2Java Demo.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE    PIC X(20) VALUE "HELLO, WORLD!".
       01 WS-COUNTER    PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Starting program...".
           PERFORM GREET-PARAGRAPH.
           DISPLAY "Program complete.".
           STOP RUN.

       GREET-PARAGRAPH.
           DISPLAY WS-MESSAGE.
           ADD 1 TO WS-COUNTER.
           DISPLAY WS-COUNTER.
`;

const SAMPLES: Record<string, string> = {
  'Hello World': SAMPLE_COBOL,
  'Fibonacci': `       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-N          PIC 9(3) VALUE 10.
       01 WS-I          PIC 9(3) VALUE 0.
       01 WS-FIB-PREV   PIC 9(10) VALUE 0.
       01 WS-FIB-CURR   PIC 9(10) VALUE 1.
       01 WS-FIB-NEXT   PIC 9(10) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Fibonacci sequence (first " WS-N.
           PERFORM CALCULATE-FIBONACCI.
           STOP RUN.

       CALCULATE-FIBONACCI.
           MOVE 0 TO WS-I.
           MOVE 0 TO WS-FIB-PREV.
           MOVE 1 TO WS-FIB-CURR.
           DISPLAY WS-FIB-PREV.
           ADD 1 TO WS-I.
           PERFORM UNTIL WS-I >= WS-N
               DISPLAY WS-FIB-CURR
               COMPUTE WS-FIB-NEXT = WS-FIB-PREV + WS-FIB-CURR
               MOVE WS-FIB-CURR TO WS-FIB-PREV
               MOVE WS-FIB-NEXT TO WS-FIB-CURR
               ADD 1 TO WS-I
           END-PERFORM.
`,
  'Grade Checker': `       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADE-CHECKER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SCORE      PIC 9(3) VALUE 85.
       01 WS-GRADE      PIC X(1).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           EVALUATE TRUE
               WHEN WS-SCORE >= 90
                   MOVE "A" TO WS-GRADE
               WHEN WS-SCORE >= 80
                   MOVE "B" TO WS-GRADE
               WHEN WS-SCORE >= 70
                   MOVE "C" TO WS-GRADE
               WHEN WS-SCORE >= 60
                   MOVE "D" TO WS-GRADE
               WHEN OTHER
                   MOVE "F" TO WS-GRADE
           END-EVALUATE.
           DISPLAY "Score: " WS-SCORE " Grade: " WS-GRADE.
           STOP RUN.
`,
};

function App() {
  const [cobolSource, setCobolSource] = useState(SAMPLE_COBOL);
  const [javaOutput, setJavaOutput] = useState('');
  const [isConverting, setIsConverting] = useState(false);
  const [result, setResult] = useState<ConversionResult | null>(null);
  const [selectedSample, setSelectedSample] = useState('Hello World');

  const handleConvert = useCallback(async () => {
    if (!cobolSource.trim()) return;

    setIsConverting(true);
    setResult(null);
    setJavaOutput('');

    try {
      const conversionResult = await convert(cobolSource, {
        llmProvider: 'none',
        packageName: 'com.example',
        javaVersion: 17,
      });

      setResult(conversionResult);
      setJavaOutput(conversionResult.java);
    } catch (error) {
      setResult({
        java: '',
        className: '',
        errors: [{ code: 'CVT000', line: 0, message: String(error), severity: ErrorSeverity.FATAL }],
        warnings: [],
        metadata: {
          programName: '',
          linesConverted: 0,
          durationMs: 0,
          llmProvider: 'none',
          timestamp: new Date().toISOString(),
        },
      });
    } finally {
      setIsConverting(false);
    }
  }, [cobolSource]);

  const handleSampleChange = (sampleName: string) => {
    setSelectedSample(sampleName);
    setCobolSource(SAMPLES[sampleName] || '');
    setJavaOutput('');
    setResult(null);
  };

  const handleCopy = useCallback(() => {
    if (javaOutput) {
      navigator.clipboard.writeText(javaOutput);
    }
  }, [javaOutput]);

  const handleDownload = useCallback(() => {
    if (javaOutput && result) {
      const blob = new Blob([javaOutput], { type: 'text/java' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `${result.className || 'Converted'}.java`;
      a.click();
      URL.revokeObjectURL(url);
    }
  }, [javaOutput, result]);

  return (
    <div className="app">
      <Header
        onConvert={handleConvert}
        isConverting={isConverting}
        samples={Object.keys(SAMPLES)}
        selectedSample={selectedSample}
        onSampleChange={handleSampleChange}
        hasOutput={!!javaOutput}
        onCopy={handleCopy}
        onDownload={handleDownload}
      />

      <main className="main">
        <div className="panel">
          <div className="panel-header">
            <h2>COBOL Source</h2>
          </div>
          <div className="panel-content">
            <Editor
              value={cobolSource}
              onChange={setCobolSource}
              language="cobol"
              placeholder="Paste your COBOL code here..."
            />
          </div>
        </div>

        <div className="panel">
          <div className="panel-header">
            <h2>Java Output</h2>
          </div>
          <div className="panel-content">
            {isConverting ? (
              <div className="loading">
                <div className="spinner" />
                Converting...
              </div>
            ) : (
              <Editor
                value={javaOutput}
                language="java"
                readOnly
                placeholder="Click 'Convert' to generate Java code..."
              />
            )}
          </div>
        </div>
      </main>

      <StatusBar result={result} isConverting={isConverting} />
    </div>
  );
}

export default App;
