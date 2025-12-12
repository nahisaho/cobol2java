/**
 * COBOL2Java VS Code Extension
 * 
 * Provides COBOL to Java conversion with GitHub Copilot LLM assistance
 */

import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs/promises';
import { convert, type ConversionOptions, type ErrorInfo } from 'cobol2java-core';

// Diagnostics collection for validation errors
let diagnosticCollection: vscode.DiagnosticCollection;

/**
 * Extension activation
 */
export function activate(context: vscode.ExtensionContext) {
  console.log('COBOL2Java extension is now active');

  // Create diagnostics collection
  diagnosticCollection = vscode.languages.createDiagnosticCollection('cobol2java');
  context.subscriptions.push(diagnosticCollection);

  // Register commands
  context.subscriptions.push(
    vscode.commands.registerCommand('cobol2java.convert', handleConvert),
    vscode.commands.registerCommand('cobol2java.convertWithCopilot', handleConvertWithCopilot),
    vscode.commands.registerCommand('cobol2java.validate', handleValidate),
    vscode.commands.registerCommand('cobol2java.previewStatement', handlePreviewStatement)
  );

  // Register code lens provider for COBOL files
  context.subscriptions.push(
    vscode.languages.registerCodeLensProvider(
      { language: 'cobol', scheme: 'file' },
      new CobolCodeLensProvider()
    )
  );

  // Register hover provider for COBOL files
  context.subscriptions.push(
    vscode.languages.registerHoverProvider(
      { language: 'cobol', scheme: 'file' },
      new CobolHoverProvider()
    )
  );

  // Auto-validate on save
  context.subscriptions.push(
    vscode.workspace.onDidSaveTextDocument((document) => {
      if (document.languageId === 'cobol') {
        validateAndUpdateDiagnostics(document);
      }
    })
  );

  // Validate open COBOL files
  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument((document) => {
      if (document.languageId === 'cobol') {
        validateAndUpdateDiagnostics(document);
      }
    })
  );
}

/**
 * Extension deactivation
 */
export function deactivate() {
  diagnosticCollection?.dispose();
  console.log('COBOL2Java extension is now deactivated');
}

/**
 * Handle basic convert command
 */
async function handleConvert(uri?: vscode.Uri) {
  const sourceUri = uri || vscode.window.activeTextEditor?.document.uri;
  if (!sourceUri) {
    vscode.window.showErrorMessage('No COBOL file selected');
    return;
  }

  await performConversion(sourceUri, false);
}

/**
 * Handle convert with Copilot command
 */
async function handleConvertWithCopilot(uri?: vscode.Uri) {
  const sourceUri = uri || vscode.window.activeTextEditor?.document.uri;
  if (!sourceUri) {
    vscode.window.showErrorMessage('No COBOL file selected');
    return;
  }

  await performConversion(sourceUri, true);
}

/**
 * Handle validate command
 */
async function handleValidate(uri?: vscode.Uri) {
  const sourceUri = uri || vscode.window.activeTextEditor?.document.uri;
  if (!sourceUri) {
    vscode.window.showErrorMessage('No COBOL file selected');
    return;
  }

  try {
    const source = await fs.readFile(sourceUri.fsPath, 'utf-8');
    const result = await convert(source, { llmProvider: 'none' });

    if (result.errors.length > 0) {
      const errorMessages = result.errors.map((e: ErrorInfo) => `${e.code}: ${e.message}`).join('\n');
      vscode.window.showErrorMessage(`Validation errors:\n${errorMessages}`);
    } else if (result.warnings.length > 0) {
      const warnMessages = result.warnings.map((w: ErrorInfo) => `${w.code}: ${w.message}`).join('\n');
      vscode.window.showWarningMessage(`Validation warnings:\n${warnMessages}`);
    } else {
      vscode.window.showInformationMessage('COBOL source is valid');
    }
  } catch (err) {
    vscode.window.showErrorMessage(`Validation failed: ${err}`);
  }
}

/**
 * Perform the actual conversion
 */
async function performConversion(sourceUri: vscode.Uri, useCopilot: boolean) {
  const config = vscode.workspace.getConfiguration('cobol2java');
  const packageName = config.get<string>('packageName', 'com.example');
  const springBoot = config.get<boolean>('springBoot', false);
  const outputDir = config.get<string>('outputDirectory', './output');
  const copilotModel = config.get<string>('copilotModel', 'gpt-4o');

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: 'Converting COBOL to Java',
      cancellable: false,
    },
    async (progress: vscode.Progress<{ message?: string }>) => {
      try {
        progress.report({ message: 'Reading source file...' });
        const source = await fs.readFile(sourceUri.fsPath, 'utf-8');

        const options: ConversionOptions = {
          packageName,
          springBoot,
          llmProvider: useCopilot ? 'copilot' : 'none',
          llmModel: useCopilot ? copilotModel : undefined,
        };

        progress.report({ message: useCopilot ? 'Converting with Copilot...' : 'Converting...' });
        const result = await convert(source, options);

        // Determine output path
        const workspaceFolder = vscode.workspace.getWorkspaceFolder(sourceUri);
        const outputPath = workspaceFolder
          ? path.join(workspaceFolder.uri.fsPath, outputDir, `${result.className}.java`)
          : path.join(path.dirname(sourceUri.fsPath), outputDir, `${result.className}.java`);

        // Create output directory
        await fs.mkdir(path.dirname(outputPath), { recursive: true });

        // Write output file
        await fs.writeFile(outputPath, result.java, 'utf-8');

        // Show success message with action
        const action = await vscode.window.showInformationMessage(
          `Successfully converted to ${result.className}.java`,
          'Open File',
          'Show in Explorer'
        );

        if (action === 'Open File') {
          const doc = await vscode.workspace.openTextDocument(outputPath);
          await vscode.window.showTextDocument(doc);
        } else if (action === 'Show in Explorer') {
          await vscode.commands.executeCommand('revealInExplorer', vscode.Uri.file(outputPath));
        }

        // Show warnings if any
        if (result.warnings.length > 0) {
          const warnMessages = result.warnings.map((w: ErrorInfo) => `${w.code}: ${w.message}`).join('\n');
          vscode.window.showWarningMessage(`Conversion completed with warnings:\n${warnMessages}`);
        }
      } catch (err) {
        vscode.window.showErrorMessage(`Conversion failed: ${err}`);
      }
    }
  );
}

/**
 * Code Lens Provider for COBOL files
 */
class CobolCodeLensProvider implements vscode.CodeLensProvider {
  provideCodeLenses(document: vscode.TextDocument): vscode.CodeLens[] {
    const lenses: vscode.CodeLens[] = [];

    // Add code lens at the top of the file
    const topOfDocument = new vscode.Range(0, 0, 0, 0);
    
    lenses.push(
      new vscode.CodeLens(topOfDocument, {
        title: '$(play) Convert to Java',
        command: 'cobol2java.convert',
        arguments: [document.uri],
      }),
      new vscode.CodeLens(topOfDocument, {
        title: '$(sparkle) Convert with Copilot',
        command: 'cobol2java.convertWithCopilot',
        arguments: [document.uri],
      }),
      new vscode.CodeLens(topOfDocument, {
        title: '$(check) Validate',
        command: 'cobol2java.validate',
        arguments: [document.uri],
      })
    );

    return lenses;
  }
}

/**
 * Hover Provider for COBOL files - shows Java preview on hover
 */
class CobolHoverProvider implements vscode.HoverProvider {
  async provideHover(
    document: vscode.TextDocument,
    position: vscode.Position
  ): Promise<vscode.Hover | undefined> {
    const line = document.lineAt(position.line).text.trim();
    
    // Skip comments and empty lines
    if (line.startsWith('*') || line.length === 0) {
      return undefined;
    }

    // Get the statement for preview
    const statementPatterns = [
      /^(MOVE\s+.+)/i,
      /^(ADD\s+.+)/i,
      /^(SUBTRACT\s+.+)/i,
      /^(MULTIPLY\s+.+)/i,
      /^(DIVIDE\s+.+)/i,
      /^(COMPUTE\s+.+)/i,
      /^(DISPLAY\s+.+)/i,
      /^(PERFORM\s+.+)/i,
      /^(IF\s+.+)/i,
      /^(EVALUATE\s+.+)/i,
    ];

    for (const pattern of statementPatterns) {
      const match = line.match(pattern);
      if (match) {
        try {
          const result = await convert(match[1] + '.', { llmProvider: 'none' });
          if (result.java) {
            const markdown = new vscode.MarkdownString();
            markdown.appendCodeblock(result.java.substring(0, 500), 'java');
            return new vscode.Hover(markdown);
          }
        } catch {
          // Ignore conversion errors in hover
        }
        break;
      }
    }

    return undefined;
  }
}

/**
 * Validate document and update diagnostics
 */
async function validateAndUpdateDiagnostics(document: vscode.TextDocument): Promise<void> {
  const diagnostics: vscode.Diagnostic[] = [];
  
  try {
    const source = document.getText();
    const result = await convert(source, { llmProvider: 'none' });

    // Convert errors to diagnostics
    for (const error of result.errors) {
      const line = (error.line ?? 1) - 1;
      const range = new vscode.Range(
        new vscode.Position(line, 0),
        new vscode.Position(line, document.lineAt(line).text.length)
      );
      diagnostics.push(
        new vscode.Diagnostic(
          range,
          `${error.code}: ${error.message}`,
          vscode.DiagnosticSeverity.Error
        )
      );
    }

    // Convert warnings to diagnostics
    for (const warning of result.warnings) {
      const line = (warning.line ?? 1) - 1;
      const validLine = Math.min(line, document.lineCount - 1);
      const range = new vscode.Range(
        new vscode.Position(validLine, 0),
        new vscode.Position(validLine, document.lineAt(validLine).text.length)
      );
      diagnostics.push(
        new vscode.Diagnostic(
          range,
          `${warning.code}: ${warning.message}`,
          vscode.DiagnosticSeverity.Warning
        )
      );
    }
  } catch (err) {
    // Add a general error if conversion fails completely
    const range = new vscode.Range(0, 0, 0, 0);
    diagnostics.push(
      new vscode.Diagnostic(
        range,
        `Validation error: ${err}`,
        vscode.DiagnosticSeverity.Error
      )
    );
  }

  diagnosticCollection.set(document.uri, diagnostics);
}

/**
 * Handle preview statement command (for code actions)
 */
async function handlePreviewStatement() {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const line = editor.document.lineAt(editor.selection.active.line).text;
  try {
    const result = await convert(line + '.', { llmProvider: 'none' });
    
    const panel = vscode.window.createWebviewPanel(
      'cobol2javaPreview',
      'Java Preview',
      vscode.ViewColumn.Beside,
      {}
    );

    panel.webview.html = `
      <!DOCTYPE html>
      <html>
      <head>
        <style>
          body { font-family: 'Consolas', monospace; padding: 16px; }
          pre { background: #1e1e1e; color: #d4d4d4; padding: 16px; border-radius: 4px; }
          h3 { color: #569cd6; }
        </style>
      </head>
      <body>
        <h3>COBOL Statement:</h3>
        <pre>${escapeHtml(line)}</pre>
        <h3>Java Translation:</h3>
        <pre>${escapeHtml(result.java)}</pre>
      </body>
      </html>
    `;
  } catch (err) {
    vscode.window.showErrorMessage(`Preview failed: ${err}`);
  }
}

/**
 * Escape HTML for webview
 */
function escapeHtml(text: string): string {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;');
}
