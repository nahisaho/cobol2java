/**
 * COBOL CodeLens Provider
 * 
 * Provides inline code lens for COBOL structure elements
 */

import * as vscode from 'vscode';

interface CobolStructure {
  type: 'division' | 'section' | 'paragraph' | 'program';
  name: string;
  line: number;
}

export class CobolCodeLensProvider implements vscode.CodeLensProvider {
  private _onDidChangeCodeLenses = new vscode.EventEmitter<void>();
  readonly onDidChangeCodeLenses = this._onDidChangeCodeLenses.event;

  provideCodeLenses(
    document: vscode.TextDocument,
    _token: vscode.CancellationToken
  ): vscode.CodeLens[] {
    const codeLenses: vscode.CodeLens[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    const structures = this.parseStructures(lines);

    for (const structure of structures) {
      const range = new vscode.Range(structure.line, 0, structure.line, 0);

      // Add "Convert to Java" lens for PROGRAM-ID
      if (structure.type === 'program') {
        codeLenses.push(
          new vscode.CodeLens(range, {
            title: '$(file-code) Java変換',
            command: 'cobol2java.convert',
            tooltip: 'このプログラムをJavaに変換',
          })
        );
        codeLenses.push(
          new vscode.CodeLens(range, {
            title: '$(preview) プレビュー',
            command: 'cobol2java.showPreview',
            tooltip: 'Java変換をプレビュー',
          })
        );
      }

      // Add structure info lens for divisions
      if (structure.type === 'division') {
        codeLenses.push(
          new vscode.CodeLens(range, {
            title: `$(symbol-class) ${structure.name} Division`,
            command: 'cobol2java.showStructureInfo',
            arguments: [structure],
            tooltip: `${structure.name} Divisionの情報を表示`,
          })
        );
      }

      // Add section navigation
      if (structure.type === 'section') {
        codeLenses.push(
          new vscode.CodeLens(range, {
            title: `$(symbol-method) ${structure.name}`,
            command: 'cobol2java.goToReferences',
            arguments: [structure.name, document.uri],
            tooltip: `${structure.name}への参照を検索`,
          })
        );
      }

      // Add paragraph conversion hint
      if (structure.type === 'paragraph') {
        codeLenses.push(
          new vscode.CodeLens(range, {
            title: '$(arrow-right) → method()',
            command: 'cobol2java.showMethodPreview',
            arguments: [structure.name],
            tooltip: `${structure.name}のJavaメソッド変換を表示`,
          })
        );
      }
    }

    return codeLenses;
  }

  /**
   * Parse COBOL structures from lines
   */
  private parseStructures(lines: string[]): CobolStructure[] {
    const structures: CobolStructure[] = [];

    const patterns = {
      program: /PROGRAM-ID\.\s+(\w+)/i,
      division: /(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION/i,
      section: /(\w+)\s+SECTION\s*\./i,
      paragraph: /^(?:\s{7}|\t)(\w+(?:-\w+)*)\s*\.\s*$/,
    };

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i] || '';
      
      // Skip comment lines
      if (line.length > 6 && line[6] === '*') continue;

      // Check for PROGRAM-ID
      const programMatch = line.match(patterns.program);
      if (programMatch) {
        structures.push({
          type: 'program',
          name: programMatch[1]!,
          line: i,
        });
        continue;
      }

      // Check for DIVISION
      const divisionMatch = line.match(patterns.division);
      if (divisionMatch) {
        structures.push({
          type: 'division',
          name: divisionMatch[1]!,
          line: i,
        });
        continue;
      }

      // Check for SECTION
      const sectionMatch = line.match(patterns.section);
      if (sectionMatch) {
        structures.push({
          type: 'section',
          name: sectionMatch[1]!,
          line: i,
        });
        continue;
      }

      // Check for paragraph (only in procedure division area)
      const paragraphMatch = line.match(patterns.paragraph);
      if (paragraphMatch && this.isInProcedureDivision(lines, i)) {
        const name = paragraphMatch[1]!;
        // Exclude reserved words
        if (!this.isReservedWord(name)) {
          structures.push({
            type: 'paragraph',
            name,
            line: i,
          });
        }
      }
    }

    return structures;
  }

  /**
   * Check if current line is in PROCEDURE DIVISION
   */
  private isInProcedureDivision(lines: string[], currentLine: number): boolean {
    for (let i = currentLine - 1; i >= 0; i--) {
      const line = lines[i] || '';
      if (/PROCEDURE\s+DIVISION/i.test(line)) return true;
      if (/DATA\s+DIVISION/i.test(line)) return false;
    }
    return false;
  }

  /**
   * Check if name is a COBOL reserved word
   */
  private isReservedWord(name: string): boolean {
    const reserved = new Set([
      'ACCEPT', 'ADD', 'CALL', 'CLOSE', 'COMPUTE', 'DELETE',
      'DISPLAY', 'DIVIDE', 'EVALUATE', 'EXIT', 'GO', 'IF',
      'INITIALIZE', 'INSPECT', 'MOVE', 'MULTIPLY', 'OPEN',
      'PERFORM', 'READ', 'RETURN', 'REWRITE', 'SEARCH', 'SET',
      'SORT', 'START', 'STOP', 'STRING', 'SUBTRACT', 'UNSTRING',
      'WRITE', 'END-IF', 'END-PERFORM', 'END-EVALUATE',
    ]);
    return reserved.has(name.toUpperCase());
  }

  /**
   * Refresh code lenses
   */
  refresh(): void {
    this._onDidChangeCodeLenses.fire();
  }
}
