import * as vscode from 'vscode';
import { CobolParser, JavaGenerator, GeneratorOptions } from '@cobol2java/core';

let outputChannel: vscode.OutputChannel;
let previewPanel: vscode.WebviewPanel | undefined;

export function activate(context: vscode.ExtensionContext) {
  outputChannel = vscode.window.createOutputChannel('COBOL2Java');
  outputChannel.appendLine('COBOL2Java extension activated');

  // Register commands
  context.subscriptions.push(
    vscode.commands.registerCommand('cobol2java.convert', convertCurrentFile),
    vscode.commands.registerCommand('cobol2java.convertToClipboard', convertToClipboard),
    vscode.commands.registerCommand('cobol2java.detectDialect', detectDialect),
    vscode.commands.registerCommand('cobol2java.showPreview', showPreview)
  );

  outputChannel.appendLine('All commands registered');
}

export function deactivate() {
  outputChannel?.dispose();
  previewPanel?.dispose();
}

/**
 * Get generator options from workspace configuration
 */
function getGeneratorOptions(): GeneratorOptions {
  const config = vscode.workspace.getConfiguration('cobol2java');
  
  return {
    packageName: config.get<string>('packageName', 'com.example.converted'),
    javaVersion: config.get<number>('javaVersion', 17),
    springBoot: config.get<boolean>('springBoot', false),
    springBatch: config.get<boolean>('springBatch', false),
  };
}

/**
 * Convert current COBOL file to Java
 */
async function convertCurrentFile(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showWarningMessage('アクティブなエディタがありません');
    return;
  }

  const document = editor.document;
  if (!isCobolFile(document)) {
    vscode.window.showWarningMessage('COBOLファイルを開いてください');
    return;
  }

  try {
    await vscode.window.withProgress(
      {
        location: vscode.ProgressLocation.Notification,
        title: 'COBOL → Java 変換中...',
        cancellable: false,
      },
      async () => {
        const source = document.getText();
        const result = await convertCobolToJava(source);

        // Determine output path
        const config = vscode.workspace.getConfiguration('cobol2java');
        const outputDir = config.get<string>('outputDirectory', '');
        
        let outputPath: vscode.Uri;
        if (outputDir) {
          const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
          if (workspaceFolder) {
            outputPath = vscode.Uri.joinPath(
              workspaceFolder.uri,
              outputDir,
              `${result.className}.java`
            );
          } else {
            outputPath = vscode.Uri.file(
              document.uri.fsPath.replace(/\.(cob|cbl|cobol|cpy)$/i, '.java')
            );
          }
        } else {
          outputPath = vscode.Uri.file(
            document.uri.fsPath.replace(/\.(cob|cbl|cobol|cpy)$/i, '.java')
          );
        }

        // Write the Java file
        const encoder = new TextEncoder();
        await vscode.workspace.fs.writeFile(outputPath, encoder.encode(result.code));

        // Open the generated file
        const doc = await vscode.workspace.openTextDocument(outputPath);
        await vscode.window.showTextDocument(doc, vscode.ViewColumn.Beside);

        vscode.window.showInformationMessage(
          `変換完了: ${result.className}.java`
        );
        
        outputChannel.appendLine(`Converted ${document.fileName} to ${outputPath.fsPath}`);
      }
    );
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    vscode.window.showErrorMessage(`変換エラー: ${message}`);
    outputChannel.appendLine(`Error: ${message}`);
  }
}

/**
 * Convert and copy to clipboard
 */
async function convertToClipboard(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showWarningMessage('アクティブなエディタがありません');
    return;
  }

  const document = editor.document;
  if (!isCobolFile(document)) {
    vscode.window.showWarningMessage('COBOLファイルを開いてください');
    return;
  }

  try {
    const source = document.getText();
    const result = await convertCobolToJava(source);

    await vscode.env.clipboard.writeText(result.code);
    vscode.window.showInformationMessage('Javaコードをクリップボードにコピーしました');
    
    outputChannel.appendLine(`Copied converted Java code to clipboard`);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    vscode.window.showErrorMessage(`変換エラー: ${message}`);
    outputChannel.appendLine(`Error: ${message}`);
  }
}

/**
 * Detect COBOL dialect
 */
async function detectDialect(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showWarningMessage('アクティブなエディタがありません');
    return;
  }

  const document = editor.document;
  if (!isCobolFile(document)) {
    vscode.window.showWarningMessage('COBOLファイルを開いてください');
    return;
  }

  try {
    const source = document.getText();
    const parser = new CobolParser();
    const ast = parser.parse(source);

    // Detect dialect from AST metadata or heuristics
    const dialect = detectDialectFromSource(source);
    
    vscode.window.showInformationMessage(
      `検出されたダイアレクト: ${dialect}`
    );
    
    outputChannel.appendLine(`Detected dialect: ${dialect}`);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    vscode.window.showErrorMessage(`解析エラー: ${message}`);
    outputChannel.appendLine(`Error: ${message}`);
  }
}

/**
 * Show live preview of Java conversion
 */
async function showPreview(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showWarningMessage('アクティブなエディタがありません');
    return;
  }

  const document = editor.document;
  if (!isCobolFile(document)) {
    vscode.window.showWarningMessage('COBOLファイルを開いてください');
    return;
  }

  // Create or show preview panel
  if (!previewPanel) {
    previewPanel = vscode.window.createWebviewPanel(
      'cobol2javaPreview',
      'COBOL → Java プレビュー',
      vscode.ViewColumn.Beside,
      {
        enableScripts: true,
        retainContextWhenHidden: true,
      }
    );

    previewPanel.onDidDispose(() => {
      previewPanel = undefined;
    });
  }

  // Update preview
  await updatePreview(document);

  // Listen for document changes
  const changeDisposable = vscode.workspace.onDidChangeTextDocument((e) => {
    if (e.document === document && previewPanel) {
      updatePreviewDebounced(document);
    }
  });

  previewPanel.onDidDispose(() => {
    changeDisposable.dispose();
  });
}

let updatePreviewTimeout: NodeJS.Timeout | undefined;

function updatePreviewDebounced(document: vscode.TextDocument): void {
  if (updatePreviewTimeout) {
    clearTimeout(updatePreviewTimeout);
  }
  updatePreviewTimeout = setTimeout(() => {
    updatePreview(document);
  }, 500);
}

async function updatePreview(document: vscode.TextDocument): Promise<void> {
  if (!previewPanel) return;

  try {
    const source = document.getText();
    const result = await convertCobolToJava(source);

    previewPanel.webview.html = getPreviewHtml(result.code, result.className);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    previewPanel.webview.html = getErrorHtml(message);
  }
}

/**
 * Core conversion function
 */
async function convertCobolToJava(source: string): Promise<{ code: string; className: string; batchConfig?: unknown }> {
  const parser = new CobolParser();
  const ast = parser.parse(source);

  const options = getGeneratorOptions();
  const generator = new JavaGenerator(options);
  
  return generator.generate(ast);
}

/**
 * Check if document is a COBOL file
 */
function isCobolFile(document: vscode.TextDocument): boolean {
  const ext = document.fileName.toLowerCase();
  return (
    ext.endsWith('.cob') ||
    ext.endsWith('.cbl') ||
    ext.endsWith('.cobol') ||
    ext.endsWith('.cpy') ||
    document.languageId === 'cobol'
  );
}

/**
 * Detect COBOL dialect from source code
 */
function detectDialectFromSource(source: string): string {
  const upperSource = source.toUpperCase();

  // Check for IBM Enterprise COBOL markers
  if (
    upperSource.includes('EXEC CICS') ||
    upperSource.includes('EXEC SQL') ||
    upperSource.includes('CBL ') ||
    upperSource.includes('PROCESS ')
  ) {
    return 'IBM Enterprise COBOL';
  }

  // Check for Micro Focus markers
  if (
    upperSource.includes('$SET') ||
    upperSource.includes('>>SOURCE') ||
    upperSource.includes('WORKING-STORAGE SECTION') &&
    upperSource.includes('78 ')
  ) {
    return 'Micro Focus COBOL';
  }

  // Check for GnuCOBOL markers
  if (
    upperSource.includes('>>SOURCE FORMAT') ||
    upperSource.includes('*>') ||
    upperSource.includes('>>DEFINE')
  ) {
    return 'GnuCOBOL';
  }

  // Check for COBOL-85 compliance
  if (
    upperSource.includes('IDENTIFICATION DIVISION') &&
    upperSource.includes('ENVIRONMENT DIVISION') &&
    upperSource.includes('DATA DIVISION') &&
    upperSource.includes('PROCEDURE DIVISION')
  ) {
    return 'COBOL-85 Standard';
  }

  return 'Unknown/Generic COBOL';
}

/**
 * Generate preview HTML
 */
function getPreviewHtml(code: string, className: string): string {
  const escapedCode = escapeHtml(code);
  
  return `<!DOCTYPE html>
<html lang="ja">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>COBOL → Java プレビュー</title>
  <style>
    body {
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      margin: 0;
      padding: 16px;
      background-color: var(--vscode-editor-background);
      color: var(--vscode-editor-foreground);
    }
    h2 {
      margin: 0 0 16px 0;
      font-size: 16px;
      color: var(--vscode-textLink-foreground);
    }
    pre {
      background-color: var(--vscode-textCodeBlock-background);
      padding: 16px;
      border-radius: 4px;
      overflow: auto;
      font-family: 'Fira Code', 'Consolas', 'Courier New', monospace;
      font-size: 13px;
      line-height: 1.5;
      white-space: pre-wrap;
      word-wrap: break-word;
    }
    .status {
      display: flex;
      align-items: center;
      gap: 8px;
      margin-bottom: 16px;
      padding: 8px 12px;
      background-color: var(--vscode-inputValidation-infoBackground);
      border-radius: 4px;
    }
    .status-icon {
      color: var(--vscode-inputValidation-infoForeground);
    }
  </style>
</head>
<body>
  <div class="status">
    <span class="status-icon">✓</span>
    <span>変換成功: ${className}.java</span>
  </div>
  <h2>${className}.java</h2>
  <pre><code>${escapedCode}</code></pre>
</body>
</html>`;
}

/**
 * Generate error HTML
 */
function getErrorHtml(message: string): string {
  const escapedMessage = escapeHtml(message);
  
  return `<!DOCTYPE html>
<html lang="ja">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>COBOL → Java プレビュー</title>
  <style>
    body {
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      margin: 0;
      padding: 16px;
      background-color: var(--vscode-editor-background);
      color: var(--vscode-editor-foreground);
    }
    .error {
      padding: 16px;
      background-color: var(--vscode-inputValidation-errorBackground);
      border: 1px solid var(--vscode-inputValidation-errorBorder);
      border-radius: 4px;
      color: var(--vscode-inputValidation-errorForeground);
    }
    .error-title {
      display: flex;
      align-items: center;
      gap: 8px;
      margin-bottom: 8px;
      font-weight: bold;
    }
  </style>
</head>
<body>
  <div class="error">
    <div class="error-title">
      <span>⚠</span>
      <span>変換エラー</span>
    </div>
    <p>${escapedMessage}</p>
  </div>
</body>
</html>`;
}

/**
 * Escape HTML special characters
 */
function escapeHtml(text: string): string {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;');
}
