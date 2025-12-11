/**
 * COBOL2Java VS Code Extension
 * 
 * Provides COBOL to Java conversion with GitHub Copilot LLM assistance
 */

import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs/promises';
import { convert, type ConversionOptions, type ErrorInfo } from '@cobol2java/core';

/**
 * Extension activation
 */
export function activate(context: vscode.ExtensionContext) {
  console.log('COBOL2Java extension is now active');

  // Register commands
  context.subscriptions.push(
    vscode.commands.registerCommand('cobol2java.convert', handleConvert),
    vscode.commands.registerCommand('cobol2java.convertWithCopilot', handleConvertWithCopilot),
    vscode.commands.registerCommand('cobol2java.validate', handleValidate)
  );

  // Register code lens provider for COBOL files
  context.subscriptions.push(
    vscode.languages.registerCodeLensProvider(
      { language: 'cobol', scheme: 'file' },
      new CobolCodeLensProvider()
    )
  );
}

/**
 * Extension deactivation
 */
export function deactivate() {
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
