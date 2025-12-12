/**
 * COBOL Diagnostics Provider
 * 
 * Provides diagnostics (errors, warnings, hints) for COBOL files
 */

import * as vscode from 'vscode';

export class CobolDiagnosticsProvider {
  private diagnosticCollection: vscode.DiagnosticCollection;
  private debounceTimer: NodeJS.Timeout | undefined;

  constructor() {
    this.diagnosticCollection = vscode.languages.createDiagnosticCollection('cobol2java');
  }

  /**
   * Analyze document and update diagnostics
   */
  analyze(document: vscode.TextDocument): void {
    // Debounce rapid changes
    if (this.debounceTimer) {
      clearTimeout(this.debounceTimer);
    }

    this.debounceTimer = setTimeout(() => {
      this.doAnalyze(document);
    }, 300);
  }

  /**
   * Perform analysis
   */
  private doAnalyze(document: vscode.TextDocument): void {
    const diagnostics: vscode.Diagnostic[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    // Check for common issues
    this.checkDivisionStructure(lines, diagnostics);
    this.checkDataItemDefinitions(lines, diagnostics);
    this.checkParagraphReferences(lines, text, diagnostics);
    this.checkDeprecatedConstructs(lines, diagnostics);
    this.checkModernizationHints(lines, diagnostics);

    this.diagnosticCollection.set(document.uri, diagnostics);
  }

  /**
   * Check DIVISION structure
   */
  private checkDivisionStructure(lines: string[], diagnostics: vscode.Diagnostic[]): void {
    const divisions = {
      identification: false,
      environment: false,
      data: false,
      procedure: false,
    };

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i]?.toUpperCase() || '';
      
      if (line.includes('IDENTIFICATION DIVISION')) divisions.identification = true;
      if (line.includes('ENVIRONMENT DIVISION')) divisions.environment = true;
      if (line.includes('DATA DIVISION')) divisions.data = true;
      if (line.includes('PROCEDURE DIVISION')) divisions.procedure = true;
    }

    // Check required divisions
    if (!divisions.identification) {
      diagnostics.push(
        new vscode.Diagnostic(
          new vscode.Range(0, 0, 0, 0),
          'IDENTIFICATION DIVISION が見つかりません',
          vscode.DiagnosticSeverity.Error
        )
      );
    }

    if (!divisions.procedure) {
      diagnostics.push(
        new vscode.Diagnostic(
          new vscode.Range(0, 0, 0, 0),
          'PROCEDURE DIVISION が見つかりません',
          vscode.DiagnosticSeverity.Error
        )
      );
    }
  }

  /**
   * Check data item definitions
   */
  private checkDataItemDefinitions(lines: string[], diagnostics: vscode.Diagnostic[]): void {
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i] || '';
      
      // Check for missing PIC clause
      const levelMatch = line.match(/^\s*(\d{2})\s+(\w[\w-]*)\s*\.?\s*$/);
      if (levelMatch) {
        const level = parseInt(levelMatch[1] || '0');
        const name = levelMatch[2];
        
        // Non-group items (not 01, 66, 77, 88) should have PIC or VALUE
        if (![1, 66, 77, 88].includes(level) && name !== 'FILLER') {
          // Check if next line has subordinate items
          const nextLine = lines[i + 1] || '';
          const nextLevelMatch = nextLine.match(/^\s*(\d{2})\s+/);
          if (!nextLevelMatch || parseInt(nextLevelMatch[1] || '0') <= level) {
            diagnostics.push(
              new vscode.Diagnostic(
                new vscode.Range(i, 0, i, line.length),
                `データ項目 ${name} にPIC句がありません（グループ項目でない場合）`,
                vscode.DiagnosticSeverity.Warning
              )
            );
          }
        }
      }

      // Check for invalid PIC clause
      const picMatch = line.match(/PIC(?:TURE)?\s+([^\s.]+)/i);
      if (picMatch) {
        const pic = picMatch[1] || '';
        if (!this.isValidPicture(pic)) {
          diagnostics.push(
            new vscode.Diagnostic(
              new vscode.Range(i, line.indexOf(pic), i, line.indexOf(pic) + pic.length),
              `無効なPIC句: ${pic}`,
              vscode.DiagnosticSeverity.Error
            )
          );
        }
      }
    }
  }

  /**
   * Check paragraph references
   */
  private checkParagraphReferences(
    lines: string[],
    text: string,
    diagnostics: vscode.Diagnostic[]
  ): void {
    // Find all paragraph definitions
    const paragraphs = new Set<string>();
    const sectionPattern = /^[\s\d]{6}\s(\w[\w-]*)\s+SECTION\s*\./i;
    const paragraphPattern = /^[\s\d]{6}\s(\w[\w-]*)\s*\.\s*$/;

    for (const line of lines) {
      const sectionMatch = line.match(sectionPattern);
      if (sectionMatch) paragraphs.add(sectionMatch[1]!.toUpperCase());
      
      const paragraphMatch = line.match(paragraphPattern);
      if (paragraphMatch) paragraphs.add(paragraphMatch[1]!.toUpperCase());
    }

    // Check PERFORM references
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i] || '';
      const performMatch = line.match(/PERFORM\s+([\w-]+)(?:\s+THRU\s+([\w-]+))?/i);
      
      if (performMatch) {
        const target1 = performMatch[1]!.toUpperCase();
        const target2 = performMatch[2]?.toUpperCase();

        if (!paragraphs.has(target1) && !this.isReservedWord(target1)) {
          diagnostics.push(
            new vscode.Diagnostic(
              new vscode.Range(i, line.indexOf(performMatch[1]!), i, line.indexOf(performMatch[1]!) + performMatch[1]!.length),
              `未定義のパラグラフ/セクション: ${performMatch[1]}`,
              vscode.DiagnosticSeverity.Error
            )
          );
        }

        if (target2 && !paragraphs.has(target2)) {
          diagnostics.push(
            new vscode.Diagnostic(
              new vscode.Range(i, line.lastIndexOf(performMatch[2]!), i, line.lastIndexOf(performMatch[2]!) + performMatch[2]!.length),
              `未定義のパラグラフ/セクション: ${performMatch[2]}`,
              vscode.DiagnosticSeverity.Error
            )
          );
        }
      }
    }
  }

  /**
   * Check for deprecated constructs
   */
  private checkDeprecatedConstructs(lines: string[], diagnostics: vscode.Diagnostic[]): void {
    const deprecatedPatterns: Array<{ pattern: RegExp; message: string; suggestion: string }> = [
      {
        pattern: /\bGO\s+TO\b/i,
        message: 'GO TO 文は非推奨です',
        suggestion: 'PERFORM文への置き換えを検討してください',
      },
      {
        pattern: /\bALTER\b/i,
        message: 'ALTER文は非推奨です',
        suggestion: '構造化プログラミングへの置き換えを検討してください',
      },
      {
        pattern: /\bEXHIBIT\b/i,
        message: 'EXHIBIT文は非推奨です',
        suggestion: 'DISPLAY文を使用してください',
      },
      {
        pattern: /\bTRACE\b/i,
        message: 'TRACE文は非推奨です',
        suggestion: 'デバッガまたはログ機能を使用してください',
      },
    ];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i] || '';
      
      // Skip comments
      if (line.length > 6 && line[6] === '*') continue;

      for (const { pattern, message, suggestion } of deprecatedPatterns) {
        if (pattern.test(line)) {
          const diagnostic = new vscode.Diagnostic(
            new vscode.Range(i, 0, i, line.length),
            `${message}\n💡 ${suggestion}`,
            vscode.DiagnosticSeverity.Warning
          );
          diagnostic.tags = [vscode.DiagnosticTag.Deprecated];
          diagnostics.push(diagnostic);
        }
      }
    }
  }

  /**
   * Check for modernization hints
   */
  private checkModernizationHints(lines: string[], diagnostics: vscode.Diagnostic[]): void {
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i] || '';
      
      // Skip comments
      if (line.length > 6 && line[6] === '*') continue;

      // Hint: EXEC SQL can be modernized to Spring Data JPA
      if (/EXEC\s+SQL/i.test(line)) {
        const diagnostic = new vscode.Diagnostic(
          new vscode.Range(i, 0, i, line.length),
          '埋め込みSQLはSpring Data JPAに変換可能です',
          vscode.DiagnosticSeverity.Hint
        );
        diagnostic.code = 'modernize-sql';
        diagnostics.push(diagnostic);
      }

      // Hint: EXEC CICS can be modernized
      if (/EXEC\s+CICS/i.test(line)) {
        const diagnostic = new vscode.Diagnostic(
          new vscode.Range(i, 0, i, line.length),
          'CICS文はSpring Webに変換可能です',
          vscode.DiagnosticSeverity.Hint
        );
        diagnostic.code = 'modernize-cics';
        diagnostics.push(diagnostic);
      }

      // Hint: File I/O can be modernized
      if (/\b(READ|WRITE|OPEN|CLOSE)\s+[\w-]+(-FILE|FILE)\b/i.test(line)) {
        const diagnostic = new vscode.Diagnostic(
          new vscode.Range(i, 0, i, line.length),
          'ファイルI/OはJava NIO/Spring Batchに変換可能です',
          vscode.DiagnosticSeverity.Hint
        );
        diagnostic.code = 'modernize-file-io';
        diagnostics.push(diagnostic);
      }
    }
  }

  /**
   * Validate PIC clause
   */
  private isValidPicture(pic: string): boolean {
    // Basic validation
    const validPattern = /^[9AXSV()\-+.,/$*ZBCR]+$/i;
    return validPattern.test(pic);
  }

  /**
   * Check if word is COBOL reserved word
   */
  private isReservedWord(word: string): boolean {
    const reserved = new Set([
      'VARYING', 'TIMES', 'UNTIL', 'WITH', 'TEST', 'BEFORE', 'AFTER',
    ]);
    return reserved.has(word);
  }

  /**
   * Clear diagnostics for a document
   */
  clear(document: vscode.TextDocument): void {
    this.diagnosticCollection.delete(document.uri);
  }

  /**
   * Dispose resources
   */
  dispose(): void {
    this.diagnosticCollection.dispose();
    if (this.debounceTimer) {
      clearTimeout(this.debounceTimer);
    }
  }
}
