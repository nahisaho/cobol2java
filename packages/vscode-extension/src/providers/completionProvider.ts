/**
 * COBOL Completion Provider
 * 
 * Provides code completion for COBOL files
 */

import * as vscode from 'vscode';

export class CobolCompletionProvider implements vscode.CompletionItemProvider {
  provideCompletionItems(
    document: vscode.TextDocument,
    position: vscode.Position,
    _token: vscode.CancellationToken,
    _context: vscode.CompletionContext
  ): vscode.CompletionItem[] {
    const line = document.lineAt(position.line).text;
    const prefix = line.substring(0, position.character).toUpperCase();

    const items: vscode.CompletionItem[] = [];

    // Division completions
    if (prefix.includes('DIVISION') || this.isAtLineStart(prefix)) {
      items.push(...this.getDivisionCompletions());
    }

    // Section completions
    if (prefix.includes('SECTION') || prefix.includes('DATA') || prefix.includes('PROCEDURE')) {
      items.push(...this.getSectionCompletions());
    }

    // Statement completions
    if (this.isInProcedureDivision(document, position.line)) {
      items.push(...this.getStatementCompletions());
    }

    // Data definition completions
    if (this.isInDataDivision(document, position.line)) {
      items.push(...this.getDataDefinitionCompletions());
    }

    // Variable completions
    items.push(...this.getVariableCompletions(document));

    // Paragraph completions for PERFORM
    if (prefix.includes('PERFORM')) {
      items.push(...this.getParagraphCompletions(document));
    }

    return items;
  }

  /**
   * Get DIVISION completions
   */
  private getDivisionCompletions(): vscode.CompletionItem[] {
    const divisions = [
      {
        label: 'IDENTIFICATION DIVISION',
        snippet: 'IDENTIFICATION DIVISION.\n       PROGRAM-ID. ${1:PROGRAM-NAME}.\n       AUTHOR. ${2:Author}.',
        doc: 'プログラムの識別情報を定義',
        java: 'クラス定義 (@Entity, @Service等)',
      },
      {
        label: 'ENVIRONMENT DIVISION',
        snippet: 'ENVIRONMENT DIVISION.\n       CONFIGURATION SECTION.\n       INPUT-OUTPUT SECTION.\n       FILE-CONTROL.',
        doc: '環境設定とファイル制御を定義',
        java: 'リソース設定 (@Value, application.properties)',
      },
      {
        label: 'DATA DIVISION',
        snippet: 'DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       ${0}',
        doc: 'データ定義',
        java: 'フィールド定義',
      },
      {
        label: 'PROCEDURE DIVISION',
        snippet: 'PROCEDURE DIVISION.\n       ${1:MAIN-LOGIC}.\n           ${0}\n           STOP RUN.',
        doc: '手続き部',
        java: 'メソッド定義',
      },
    ];

    return divisions.map((d) => {
      const item = new vscode.CompletionItem(d.label, vscode.CompletionItemKind.Keyword);
      item.insertText = new vscode.SnippetString(d.snippet);
      item.documentation = new vscode.MarkdownString(`${d.doc}\n\n**Java変換**: ${d.java}`);
      item.sortText = '0' + d.label;
      return item;
    });
  }

  /**
   * Get SECTION completions
   */
  private getSectionCompletions(): vscode.CompletionItem[] {
    const sections = [
      { label: 'WORKING-STORAGE SECTION', doc: 'ワーキングストレージ', java: 'インスタンス変数' },
      { label: 'LOCAL-STORAGE SECTION', doc: 'ローカルストレージ', java: 'ローカル変数' },
      { label: 'LINKAGE SECTION', doc: 'リンケージ', java: 'メソッドパラメータ' },
      { label: 'FILE SECTION', doc: 'ファイル', java: 'ファイルハンドル / Entity' },
      { label: 'SCREEN SECTION', doc: 'スクリーン', java: 'DTO / ViewModel' },
      { label: 'CONFIGURATION SECTION', doc: '構成', java: '設定クラス' },
      { label: 'INPUT-OUTPUT SECTION', doc: '入出力', java: 'リソース設定' },
    ];

    return sections.map((s) => {
      const item = new vscode.CompletionItem(s.label, vscode.CompletionItemKind.Keyword);
      item.insertText = new vscode.SnippetString(`${s.label}.\n       ${0}`);
      item.documentation = new vscode.MarkdownString(`${s.doc}\n\n**Java変換**: ${s.java}`);
      return item;
    });
  }

  /**
   * Get statement completions
   */
  private getStatementCompletions(): vscode.CompletionItem[] {
    const statements = [
      { label: 'PERFORM', snippet: 'PERFORM ${1:paragraph-name}', icon: vscode.CompletionItemKind.Method },
      { label: 'PERFORM THRU', snippet: 'PERFORM ${1:start} THRU ${2:end}', icon: vscode.CompletionItemKind.Method },
      { label: 'PERFORM VARYING', snippet: 'PERFORM ${1:paragraph}\n           VARYING ${2:idx} FROM 1 BY 1\n           UNTIL ${2:idx} > ${3:limit}', icon: vscode.CompletionItemKind.Method },
      { label: 'IF-END-IF', snippet: 'IF ${1:condition}\n           ${2:statements}\n       END-IF', icon: vscode.CompletionItemKind.Keyword },
      { label: 'EVALUATE', snippet: 'EVALUATE ${1:variable}\n           WHEN ${2:value}\n               ${3:statements}\n           WHEN OTHER\n               ${4:default}\n       END-EVALUATE', icon: vscode.CompletionItemKind.Keyword },
      { label: 'MOVE', snippet: 'MOVE ${1:source} TO ${2:dest}', icon: vscode.CompletionItemKind.Keyword },
      { label: 'COMPUTE', snippet: 'COMPUTE ${1:result} = ${2:expression}', icon: vscode.CompletionItemKind.Keyword },
      { label: 'ADD', snippet: 'ADD ${1:value} TO ${2:dest}', icon: vscode.CompletionItemKind.Keyword },
      { label: 'SUBTRACT', snippet: 'SUBTRACT ${1:value} FROM ${2:dest}', icon: vscode.CompletionItemKind.Keyword },
      { label: 'MULTIPLY', snippet: 'MULTIPLY ${1:a} BY ${2:b} GIVING ${3:result}', icon: vscode.CompletionItemKind.Keyword },
      { label: 'DIVIDE', snippet: 'DIVIDE ${1:a} BY ${2:b} GIVING ${3:result}', icon: vscode.CompletionItemKind.Keyword },
      { label: 'READ', snippet: 'READ ${1:file-name}\n           AT END\n               SET ${2:eof-flag} TO TRUE\n       END-READ', icon: vscode.CompletionItemKind.Keyword },
      { label: 'WRITE', snippet: 'WRITE ${1:record-name} FROM ${2:data}', icon: vscode.CompletionItemKind.Keyword },
      { label: 'OPEN INPUT', snippet: 'OPEN INPUT ${1:file-name}', icon: vscode.CompletionItemKind.Keyword },
      { label: 'OPEN OUTPUT', snippet: 'OPEN OUTPUT ${1:file-name}', icon: vscode.CompletionItemKind.Keyword },
      { label: 'CLOSE', snippet: 'CLOSE ${1:file-name}', icon: vscode.CompletionItemKind.Keyword },
      { label: 'DISPLAY', snippet: 'DISPLAY "${1:message}" ${2:variable}', icon: vscode.CompletionItemKind.Keyword },
      { label: 'ACCEPT', snippet: 'ACCEPT ${1:variable} FROM ${2:DATE|TIME|DAY}', icon: vscode.CompletionItemKind.Keyword },
      { label: 'STRING', snippet: 'STRING ${1:source-1} DELIMITED BY ${2:delimiter}\n           INTO ${3:dest}\n       END-STRING', icon: vscode.CompletionItemKind.Keyword },
      { label: 'UNSTRING', snippet: 'UNSTRING ${1:source}\n           DELIMITED BY ${2:delimiter}\n           INTO ${3:dest-1} ${4:dest-2}\n       END-UNSTRING', icon: vscode.CompletionItemKind.Keyword },
      { label: 'CALL', snippet: "CALL '${1:PROGRAM}' USING ${2:args}", icon: vscode.CompletionItemKind.Method },
      { label: 'STOP RUN', snippet: 'STOP RUN', icon: vscode.CompletionItemKind.Keyword },
    ];

    return statements.map((s) => {
      const item = new vscode.CompletionItem(s.label, s.icon);
      item.insertText = new vscode.SnippetString(s.snippet);
      return item;
    });
  }

  /**
   * Get data definition completions
   */
  private getDataDefinitionCompletions(): vscode.CompletionItem[] {
    const definitions = [
      { label: '01 Group Item', snippet: '01 ${1:GROUP-NAME}.\n           05 ${2:FIELD-NAME} PIC ${3:X(10)}.' },
      { label: '01 Elementary Item', snippet: '01 ${1:ITEM-NAME} PIC ${2:X(10)}.' },
      { label: '05 Field', snippet: '05 ${1:FIELD-NAME} PIC ${2:X(10)}.' },
      { label: '88 Condition', snippet: '88 ${1:CONDITION-NAME} VALUE ${2:"Y"}.' },
      { label: 'OCCURS', snippet: '05 ${1:TABLE-NAME} OCCURS ${2:10} TIMES.\n               10 ${3:ELEMENT} PIC ${4:X(20)}.' },
      { label: 'REDEFINES', snippet: '05 ${1:NEW-NAME} REDEFINES ${2:ORIGINAL} PIC ${3:9(5)}.' },
      { label: 'PIC X (Alphanumeric)', snippet: 'PIC X(${1:10})' },
      { label: 'PIC 9 (Numeric)', snippet: 'PIC 9(${1:5})' },
      { label: 'PIC S9V99 (Decimal)', snippet: 'PIC S9(${1:7})V99' },
      { label: 'VALUE', snippet: 'VALUE ${1:SPACES}' },
    ];

    return definitions.map((d) => {
      const item = new vscode.CompletionItem(d.label, vscode.CompletionItemKind.Snippet);
      item.insertText = new vscode.SnippetString(d.snippet);
      return item;
    });
  }

  /**
   * Get variable completions from document
   */
  private getVariableCompletions(document: vscode.TextDocument): vscode.CompletionItem[] {
    const text = document.getText();
    const items: vscode.CompletionItem[] = [];
    const seen = new Set<string>();

    // Find data item definitions
    const pattern = /^\s*\d{2}\s+([\w-]+)/gm;
    let match;

    while ((match = pattern.exec(text)) !== null) {
      const name = match[1]!;
      if (!seen.has(name) && name !== 'FILLER') {
        seen.add(name);
        const item = new vscode.CompletionItem(name, vscode.CompletionItemKind.Variable);
        item.detail = 'データ項目';
        items.push(item);
      }
    }

    return items;
  }

  /**
   * Get paragraph completions for PERFORM
   */
  private getParagraphCompletions(document: vscode.TextDocument): vscode.CompletionItem[] {
    const text = document.getText();
    const items: vscode.CompletionItem[] = [];
    const seen = new Set<string>();

    // Find paragraphs
    const paragraphPattern = /^[\s\d]{6}\s([\w-]+)\s*\.\s*$/gm;
    let match;

    while ((match = paragraphPattern.exec(text)) !== null) {
      const name = match[1]!;
      if (!seen.has(name)) {
        seen.add(name);
        const item = new vscode.CompletionItem(name, vscode.CompletionItemKind.Function);
        item.detail = 'パラグラフ';
        items.push(item);
      }
    }

    // Find sections
    const sectionPattern = /\b([\w-]+)\s+SECTION\s*\./gi;
    while ((match = sectionPattern.exec(text)) !== null) {
      const name = match[1]!;
      if (!seen.has(name)) {
        seen.add(name);
        const item = new vscode.CompletionItem(name, vscode.CompletionItemKind.Module);
        item.detail = 'セクション';
        items.push(item);
      }
    }

    return items;
  }

  /**
   * Check if position is at line start
   */
  private isAtLineStart(prefix: string): boolean {
    return prefix.trim().length === 0 || prefix.trim().length < 20;
  }

  /**
   * Check if in PROCEDURE DIVISION
   */
  private isInProcedureDivision(document: vscode.TextDocument, line: number): boolean {
    const text = document.getText(new vscode.Range(0, 0, line, 0));
    return /PROCEDURE\s+DIVISION/i.test(text) && !/DATA\s+DIVISION/i.test(text.slice(text.lastIndexOf('PROCEDURE')));
  }

  /**
   * Check if in DATA DIVISION
   */
  private isInDataDivision(document: vscode.TextDocument, line: number): boolean {
    const text = document.getText(new vscode.Range(0, 0, line, 0));
    const dataIdx = text.toUpperCase().lastIndexOf('DATA DIVISION');
    const procIdx = text.toUpperCase().lastIndexOf('PROCEDURE DIVISION');
    return dataIdx >= 0 && (procIdx < 0 || dataIdx > procIdx);
  }
}
