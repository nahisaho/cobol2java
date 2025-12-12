/**
 * COBOL Code Actions Provider
 * 
 * Provides quick fixes and refactoring actions for COBOL files
 */

import * as vscode from 'vscode';

export class CobolCodeActionsProvider implements vscode.CodeActionProvider {
  static readonly providedCodeActionKinds = [
    vscode.CodeActionKind.QuickFix,
    vscode.CodeActionKind.Refactor,
    vscode.CodeActionKind.RefactorExtract,
  ];

  provideCodeActions(
    document: vscode.TextDocument,
    range: vscode.Range | vscode.Selection,
    context: vscode.CodeActionContext,
    _token: vscode.CancellationToken
  ): vscode.CodeAction[] {
    const actions: vscode.CodeAction[] = [];

    // Quick fixes for diagnostics
    for (const diagnostic of context.diagnostics) {
      actions.push(...this.getQuickFixesForDiagnostic(document, diagnostic));
    }

    // Refactoring actions for selection
    if (!range.isEmpty) {
      actions.push(...this.getRefactoringActions(document, range));
    }

    // Line-specific actions
    actions.push(...this.getLineActions(document, range));

    return actions;
  }

  /**
   * Get quick fixes for a diagnostic
   */
  private getQuickFixesForDiagnostic(
    document: vscode.TextDocument,
    diagnostic: vscode.Diagnostic
  ): vscode.CodeAction[] {
    const actions: vscode.CodeAction[] = [];

    // GO TO -> PERFORM conversion
    if (diagnostic.message.includes('GO TO')) {
      const action = new vscode.CodeAction(
        'GO TO を PERFORM に変換',
        vscode.CodeActionKind.QuickFix
      );
      action.diagnostics = [diagnostic];
      action.edit = new vscode.WorkspaceEdit();
      
      const line = document.lineAt(diagnostic.range.start.line).text;
      const fixed = line.replace(/GO\s+TO\s+([\w-]+)/gi, 'PERFORM $1');
      
      action.edit.replace(
        document.uri,
        new vscode.Range(diagnostic.range.start.line, 0, diagnostic.range.start.line, line.length),
        fixed
      );
      action.isPreferred = true;
      actions.push(action);
    }

    // Modernize SQL hint
    if (diagnostic.code === 'modernize-sql') {
      const action = new vscode.CodeAction(
        'Spring Data JPA変換をプレビュー',
        vscode.CodeActionKind.QuickFix
      );
      action.diagnostics = [diagnostic];
      action.command = {
        command: 'cobol2java.previewSqlModernization',
        title: 'SQL変換プレビュー',
        arguments: [document.uri, diagnostic.range.start.line],
      };
      actions.push(action);
    }

    // Modernize CICS hint
    if (diagnostic.code === 'modernize-cics') {
      const action = new vscode.CodeAction(
        'Spring Web変換をプレビュー',
        vscode.CodeActionKind.QuickFix
      );
      action.diagnostics = [diagnostic];
      action.command = {
        command: 'cobol2java.previewCicsModernization',
        title: 'CICS変換プレビュー',
        arguments: [document.uri, diagnostic.range.start.line],
      };
      actions.push(action);
    }

    return actions;
  }

  /**
   * Get refactoring actions for selection
   */
  private getRefactoringActions(
    document: vscode.TextDocument,
    range: vscode.Range
  ): vscode.CodeAction[] {
    const actions: vscode.CodeAction[] = [];
    const selectedText = document.getText(range);

    // Extract to paragraph
    if (this.canExtractToParagraph(selectedText)) {
      const action = new vscode.CodeAction(
        'パラグラフに抽出',
        vscode.CodeActionKind.RefactorExtract
      );
      action.command = {
        command: 'cobol2java.extractToParagraph',
        title: 'パラグラフに抽出',
        arguments: [document.uri, range],
      };
      actions.push(action);
    }

    // Inline paragraph
    if (this.isPerformStatement(selectedText)) {
      const action = new vscode.CodeAction(
        'パラグラフをインライン化',
        vscode.CodeActionKind.Refactor
      );
      action.command = {
        command: 'cobol2java.inlineParagraph',
        title: 'パラグラフをインライン化',
        arguments: [document.uri, range, selectedText],
      };
      actions.push(action);
    }

    // Convert to EVALUATE
    if (this.isMultipleIfStatements(selectedText)) {
      const action = new vscode.CodeAction(
        'IF文をEVALUATEに変換',
        vscode.CodeActionKind.Refactor
      );
      action.command = {
        command: 'cobol2java.convertToEvaluate',
        title: 'EVALUATEに変換',
        arguments: [document.uri, range],
      };
      actions.push(action);
    }

    return actions;
  }

  /**
   * Get line-specific actions
   */
  private getLineActions(
    document: vscode.TextDocument,
    range: vscode.Range
  ): vscode.CodeAction[] {
    const actions: vscode.CodeAction[] = [];
    const line = document.lineAt(range.start.line).text;

    // Convert PERFORM THRU to single PERFORM
    if (/PERFORM\s+[\w-]+\s+THRU\s+[\w-]+/i.test(line)) {
      const action = new vscode.CodeAction(
        'PERFORM THRU を単一PERFORMに変換',
        vscode.CodeActionKind.Refactor
      );
      action.command = {
        command: 'cobol2java.convertPerformThru',
        title: 'PERFORM THRU変換',
        arguments: [document.uri, range.start.line],
      };
      actions.push(action);
    }

    // Add missing PIC clause
    const levelMatch = line.match(/^\s*(\d{2})\s+([\w-]+)\s*\.?\s*$/);
    if (levelMatch && !['01', '66', '77', '88'].includes(levelMatch[1]!)) {
      const action = new vscode.CodeAction(
        'PIC句を追加',
        vscode.CodeActionKind.QuickFix
      );
      action.edit = new vscode.WorkspaceEdit();
      const insertPos = line.indexOf(levelMatch[2]!) + levelMatch[2]!.length;
      action.edit.insert(
        document.uri,
        new vscode.Position(range.start.line, insertPos),
        ' PIC X(10)'
      );
      actions.push(action);
    }

    // Convert numeric operation to COMPUTE
    if (/\b(ADD|SUBTRACT|MULTIPLY|DIVIDE)\b/i.test(line)) {
      const action = new vscode.CodeAction(
        'COMPUTEに変換',
        vscode.CodeActionKind.Refactor
      );
      action.command = {
        command: 'cobol2java.convertToCompute',
        title: 'COMPUTEに変換',
        arguments: [document.uri, range.start.line],
      };
      actions.push(action);
    }

    // Generate Java equivalent comment
    const action = new vscode.CodeAction(
      'Java変換コメントを追加',
      vscode.CodeActionKind.Refactor
    );
    action.command = {
      command: 'cobol2java.addJavaEquivalentComment',
      title: 'Java変換コメント追加',
      arguments: [document.uri, range.start.line],
    };
    actions.push(action);

    return actions;
  }

  /**
   * Check if selection can be extracted to paragraph
   */
  private canExtractToParagraph(text: string): boolean {
    const lines = text.trim().split('\n');
    return lines.length >= 2 && !text.includes('DIVISION') && !text.includes('SECTION');
  }

  /**
   * Check if selection is a PERFORM statement
   */
  private isPerformStatement(text: string): boolean {
    return /^\s*PERFORM\s+[\w-]+/i.test(text.trim());
  }

  /**
   * Check if selection has multiple IF statements
   */
  private isMultipleIfStatements(text: string): boolean {
    const ifCount = (text.match(/\bIF\b/gi) || []).length;
    return ifCount >= 2;
  }
}
