/**
 * COBOL Hover Provider
 * 
 * Provides hover information for COBOL elements
 */

import * as vscode from 'vscode';

export class CobolHoverProvider implements vscode.HoverProvider {
  provideHover(
    document: vscode.TextDocument,
    position: vscode.Position,
    _token: vscode.CancellationToken
  ): vscode.Hover | null {
    const line = document.lineAt(position.line).text;
    const wordRange = document.getWordRangeAtPosition(position, /[\w-]+/);
    
    if (!wordRange) return null;

    const word = document.getText(wordRange).toUpperCase();

    // Check for COBOL keywords
    const keywordInfo = this.getKeywordInfo(word);
    if (keywordInfo) {
      return new vscode.Hover(keywordInfo, wordRange);
    }

    // Check for data item definitions
    const dataItemInfo = this.getDataItemInfo(document, word);
    if (dataItemInfo) {
      return new vscode.Hover(dataItemInfo, wordRange);
    }

    // Check for paragraph/section references
    const paragraphInfo = this.getParagraphInfo(document, word);
    if (paragraphInfo) {
      return new vscode.Hover(paragraphInfo, wordRange);
    }

    return null;
  }

  /**
   * Get COBOL keyword documentation
   */
  private getKeywordInfo(word: string): vscode.MarkdownString | null {
    const keywords: Record<string, { desc: string; java: string; example?: string }> = {
      'PERFORM': {
        desc: 'サブルーチン（パラグラフ/セクション）を実行',
        java: 'メソッド呼び出し / for/while ループ',
        example: 'PERFORM CALC-TOTAL THRU CALC-END',
      },
      'MOVE': {
        desc: '値を変数に代入',
        java: '代入文 (=)',
        example: 'MOVE WS-VALUE TO WS-RESULT',
      },
      'IF': {
        desc: '条件分岐',
        java: 'if 文',
        example: 'IF WS-FLAG = "Y" ... END-IF',
      },
      'EVALUATE': {
        desc: '複数条件分岐（switch相当）',
        java: 'switch 式 / if-else チェーン',
        example: 'EVALUATE WS-CODE\n  WHEN "A" ...\n  WHEN OTHER ...\nEND-EVALUATE',
      },
      'COMPUTE': {
        desc: '算術計算',
        java: '算術式',
        example: 'COMPUTE WS-TOTAL = WS-A + WS-B * WS-C',
      },
      'ADD': {
        desc: '加算',
        java: '+=演算子',
        example: 'ADD WS-A TO WS-TOTAL',
      },
      'SUBTRACT': {
        desc: '減算',
        java: '-=演算子',
        example: 'SUBTRACT WS-A FROM WS-TOTAL',
      },
      'MULTIPLY': {
        desc: '乗算',
        java: '*=演算子',
        example: 'MULTIPLY WS-A BY WS-B GIVING WS-RESULT',
      },
      'DIVIDE': {
        desc: '除算',
        java: '/=演算子',
        example: 'DIVIDE WS-A BY WS-B GIVING WS-RESULT REMAINDER WS-REM',
      },
      'READ': {
        desc: 'ファイルからレコードを読み込み',
        java: 'BufferedReader.readLine() / JPA findById()',
        example: 'READ INPUT-FILE INTO WS-RECORD',
      },
      'WRITE': {
        desc: 'ファイルにレコードを書き込み',
        java: 'BufferedWriter.write() / JPA save()',
        example: 'WRITE OUTPUT-RECORD FROM WS-DATA',
      },
      'OPEN': {
        desc: 'ファイルを開く',
        java: 'new FileReader/FileWriter',
        example: 'OPEN INPUT INPUT-FILE',
      },
      'CLOSE': {
        desc: 'ファイルを閉じる',
        java: '.close() / try-with-resources',
        example: 'CLOSE INPUT-FILE OUTPUT-FILE',
      },
      'ACCEPT': {
        desc: '入力を受け取る（日付/時間/ユーザー入力）',
        java: 'LocalDate.now() / Scanner.nextLine()',
        example: 'ACCEPT WS-DATE FROM DATE',
      },
      'DISPLAY': {
        desc: 'コンソールに出力',
        java: 'System.out.println()',
        example: 'DISPLAY "Total: " WS-TOTAL',
      },
      'STRING': {
        desc: '文字列連結',
        java: 'StringBuilder.append()',
        example: 'STRING WS-A DELIMITED BY SIZE INTO WS-B',
      },
      'UNSTRING': {
        desc: '文字列分割',
        java: 'String.split()',
        example: 'UNSTRING WS-LINE DELIMITED BY "," INTO WS-A WS-B WS-C',
      },
      'INSPECT': {
        desc: '文字列操作（カウント/置換）',
        java: 'String.replace() / Pattern',
        example: 'INSPECT WS-DATA REPLACING ALL "A" BY "B"',
      },
      'CALL': {
        desc: '外部プログラムを呼び出し',
        java: 'メソッド呼び出し / @Service 呼び出し',
        example: 'CALL "SUBPROG" USING WS-PARM',
      },
      'COPY': {
        desc: 'コピーブックをインクルード',
        java: 'import 文 / 共通クラス',
        example: 'COPY CUSTOMER-RECORD.',
      },
      'REDEFINES': {
        desc: 'メモリ領域の再定義（Union相当）',
        java: '型変換 / DTO マッピング',
        example: '05 WS-NUM-A REDEFINES WS-CHAR-A PIC 9(5).',
      },
      'OCCURS': {
        desc: '配列定義',
        java: '配列 / List<T>',
        example: '05 WS-TABLE OCCURS 10 TIMES.',
      },
      'PIC': {
        desc: 'データ型定義（PICTURE句）',
        java: 'String / BigDecimal / int',
        example: '05 WS-AMOUNT PIC S9(9)V99.',
      },
      'PICTURE': {
        desc: 'データ型定義（PIC句の完全形）',
        java: 'String / BigDecimal / int',
        example: '05 WS-NAME PICTURE X(30).',
      },
    };

    const info = keywords[word];
    if (!info) return null;

    const md = new vscode.MarkdownString();
    md.appendMarkdown(`### ${word}\n\n`);
    md.appendMarkdown(`**説明**: ${info.desc}\n\n`);
    md.appendMarkdown(`**Java変換**: \`${info.java}\`\n\n`);
    if (info.example) {
      md.appendMarkdown(`**例**:\n\`\`\`cobol\n${info.example}\n\`\`\`\n`);
    }
    md.isTrusted = true;

    return md;
  }

  /**
   * Get data item definition info
   */
  private getDataItemInfo(
    document: vscode.TextDocument,
    name: string
  ): vscode.MarkdownString | null {
    const text = document.getText();
    
    // Find data item definition
    const patterns = [
      new RegExp(`^\\s*(\\d{2})\\s+${this.escapeRegex(name)}\\s+(?:PIC|PICTURE)\\s+([^.]+)\\.`, 'mi'),
      new RegExp(`^\\s*(\\d{2})\\s+${this.escapeRegex(name)}\\s+VALUE\\s+([^.]+)\\.`, 'mi'),
      new RegExp(`^\\s*(\\d{2})\\s+${this.escapeRegex(name)}\\s+OCCURS\\s+(\\d+)\\s+TIMES`, 'mi'),
    ];

    for (const pattern of patterns) {
      const match = text.match(pattern);
      if (match) {
        const level = match[1];
        const definition = match[2];
        
        const md = new vscode.MarkdownString();
        md.appendMarkdown(`### データ項目: ${name}\n\n`);
        md.appendMarkdown(`**レベル**: ${level}\n\n`);
        md.appendMarkdown(`**定義**: \`${definition?.trim()}\`\n\n`);
        md.appendMarkdown(this.getJavaTypeMapping(definition || ''));
        md.isTrusted = true;
        
        return md;
      }
    }

    return null;
  }

  /**
   * Get paragraph/section info
   */
  private getParagraphInfo(
    document: vscode.TextDocument,
    name: string
  ): vscode.MarkdownString | null {
    const text = document.getText();
    const lines = text.split('\n');

    // Find paragraph definition
    const paragraphPattern = new RegExp(`^\\s{7}${this.escapeRegex(name)}\\s*\\.\\s*$`, 'mi');
    const sectionPattern = new RegExp(`${this.escapeRegex(name)}\\s+SECTION\\s*\\.`, 'i');

    let isParagraph = false;
    let isSection = false;
    let definitionLine = -1;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i] || '';
      if (paragraphPattern.test(line)) {
        isParagraph = true;
        definitionLine = i;
        break;
      }
      if (sectionPattern.test(line)) {
        isSection = true;
        definitionLine = i;
        break;
      }
    }

    if (!isParagraph && !isSection) return null;

    // Count references
    const referenceCount = (text.match(new RegExp(`PERFORM\\s+${this.escapeRegex(name)}`, 'gi')) || []).length;

    const md = new vscode.MarkdownString();
    md.appendMarkdown(`### ${isSection ? 'セクション' : 'パラグラフ'}: ${name}\n\n`);
    md.appendMarkdown(`**定義行**: ${definitionLine + 1}\n\n`);
    md.appendMarkdown(`**参照回数**: ${referenceCount}\n\n`);
    md.appendMarkdown(`**Java変換**: \`private void ${this.toJavaMethodName(name)}()\`\n`);
    md.isTrusted = true;

    return md;
  }

  /**
   * Get Java type mapping for COBOL PIC
   */
  private getJavaTypeMapping(pic: string): string {
    pic = pic.toUpperCase();
    
    if (pic.includes('X')) {
      const match = pic.match(/X\(?(\d+)\)?/);
      const len = match ? parseInt(match[1] || '1') : 1;
      return `**Java型**: \`String\` (長さ: ${len})\n`;
    }
    
    if (pic.includes('9')) {
      const hasDecimal = pic.includes('V') || pic.includes('.');
      const hasSigned = pic.includes('S');
      
      if (hasDecimal) {
        return `**Java型**: \`BigDecimal\`${hasSigned ? ' (符号付き)' : ''}\n`;
      }
      
      const match = pic.match(/9\(?(\d+)\)?/);
      const digits = match ? parseInt(match[1] || '1') : 1;
      
      if (digits <= 9) {
        return `**Java型**: \`int\` (${digits}桁)\n`;
      } else if (digits <= 18) {
        return `**Java型**: \`long\` (${digits}桁)\n`;
      } else {
        return `**Java型**: \`BigInteger\` (${digits}桁)\n`;
      }
    }

    return '';
  }

  /**
   * Convert COBOL name to Java method name
   */
  private toJavaMethodName(cobolName: string): string {
    return cobolName
      .toLowerCase()
      .split('-')
      .map((part, i) => i === 0 ? part : part.charAt(0).toUpperCase() + part.slice(1))
      .join('');
  }

  /**
   * Escape regex special characters
   */
  private escapeRegex(str: string): string {
    return str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  }
}
