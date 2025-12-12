/**
 * COBOL2Java Diff Report Generator
 *
 * 変換前後のコード比較ドキュメントを生成
 */

import { CobolAst } from '../parser.js';

export interface DiffReportOptions {
  /** レポートタイトル */
  title?: string;
  /** 詳細なマッピング情報を含めるか */
  detailed?: boolean;
  /** HTML形式で出力するか */
  html?: boolean;
  /** 行番号を含めるか */
  lineNumbers?: boolean;
  /** 構造比較を含めるか */
  structureComparison?: boolean;
  /** 変数マッピングを含めるか */
  variableMapping?: boolean;
}

export interface DiffMapping {
  cobolLine: number;
  cobolCode: string;
  javaLine: number;
  javaCode: string;
  type: 'declaration' | 'statement' | 'comment' | 'structure';
  description: string;
}

export interface VariableMapping {
  cobolName: string;
  javaName: string;
  cobolType: string;
  javaType: string;
  cobolPic?: string;
  notes?: string;
}

export interface StructureMapping {
  cobolStructure: string;
  javaStructure: string;
  description: string;
}

export interface DiffReport {
  title: string;
  timestamp: string;
  cobolSource: string;
  javaSource: string;
  mappings: DiffMapping[];
  variableMappings: VariableMapping[];
  structureMappings: StructureMapping[];
  statistics: DiffStatistics;
}

export interface DiffStatistics {
  cobolLines: number;
  javaLines: number;
  cobolStatements: number;
  javaStatements: number;
  variableCount: number;
  paragraphCount: number;
  methodCount: number;
  conversionComplexity: 'low' | 'medium' | 'high';
}

/**
 * 差分レポートを生成
 */
export function generateDiffReport(
  cobolSource: string,
  javaSource: string,
  ast: CobolAst,
  options: DiffReportOptions = {}
): DiffReport {
  const {
    title = 'COBOL to Java Conversion Report',
    detailed = true,
    structureComparison = true,
    variableMapping = true,
  } = options;

  const cobolLines = cobolSource.split('\n');
  const javaLines = javaSource.split('\n');

  // 統計情報を計算
  const statistics = calculateStatistics(cobolSource, javaSource, ast);

  // マッピング情報を抽出
  const mappings = detailed ? extractMappings(cobolLines, javaLines, ast) : [];

  // 変数マッピング
  const variableMappings = variableMapping
    ? extractVariableMappings(ast)
    : [];

  // 構造マッピング
  const structureMappings = structureComparison
    ? extractStructureMappings(ast)
    : [];

  return {
    title,
    timestamp: new Date().toISOString(),
    cobolSource,
    javaSource,
    mappings,
    variableMappings,
    structureMappings,
    statistics,
  };
}

/**
 * 統計情報を計算
 */
function calculateStatistics(
  cobolSource: string,
  javaSource: string,
  ast: CobolAst
): DiffStatistics {
  const cobolLines = cobolSource.split('\n').filter((l) => l.trim()).length;
  const javaLines = javaSource.split('\n').filter((l) => l.trim()).length;

  const cobolStatements = countCobolStatements(cobolSource);
  const javaStatements = countJavaStatements(javaSource);

  // Use ast.dataItems and ast.paragraphs directly (root level in CobolAst)
  const variableCount = ast.dataItems?.length || 0;
  const paragraphCount = ast.paragraphs?.length || 0;
  
  // Javaメソッド数をカウント
  const methodMatches = javaSource.match(/\b(public|private|protected)\s+\w+\s+\w+\s*\(/g);
  const methodCount = methodMatches ? methodMatches.length : 0;

  // 変換の複雑さを判定
  let conversionComplexity: 'low' | 'medium' | 'high' = 'low';
  if (variableCount > 50 || paragraphCount > 20) {
    conversionComplexity = 'high';
  } else if (variableCount > 20 || paragraphCount > 10) {
    conversionComplexity = 'medium';
  }

  return {
    cobolLines,
    javaLines,
    cobolStatements,
    javaStatements,
    variableCount,
    paragraphCount,
    methodCount,
    conversionComplexity,
  };
}

/**
 * COBOLステートメント数をカウント
 */
function countCobolStatements(source: string): number {
  const verbs = [
    'MOVE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'COMPUTE',
    'IF', 'PERFORM', 'GO', 'CALL', 'READ', 'WRITE', 'DISPLAY',
    'ACCEPT', 'OPEN', 'CLOSE', 'EVALUATE', 'SEARCH', 'STRING',
    'UNSTRING', 'INSPECT', 'SET', 'INITIALIZE', 'STOP',
  ];
  
  let count = 0;
  const upperSource = source.toUpperCase();
  
  for (const verb of verbs) {
    const regex = new RegExp(`\\b${verb}\\b`, 'g');
    const matches = upperSource.match(regex);
    count += matches ? matches.length : 0;
  }
  
  return count;
}

/**
 * Javaステートメント数をカウント
 */
function countJavaStatements(source: string): number {
  // セミコロンで終わる行をカウント（大まかな近似）
  const matches = source.match(/;\s*$/gm);
  return matches ? matches.length : 0;
}

/**
 * マッピング情報を抽出
 */
function extractMappings(
  cobolLines: string[],
  javaLines: string[],
  ast: CobolAst
): DiffMapping[] {
  const mappings: DiffMapping[] = [];

  // IDENTIFICATION DIVISION → class declaration
  mappings.push({
    cobolLine: 1,
    cobolCode: 'IDENTIFICATION DIVISION.',
    javaLine: findLineContaining(javaLines, 'public class'),
    javaCode: javaLines[findLineContaining(javaLines, 'public class') - 1] || '',
    type: 'structure',
    description: 'プログラム識別部からJavaクラス宣言への変換',
  });

  // PROGRAM-ID → class name
  const programIdLine = findLineContaining(cobolLines, 'PROGRAM-ID');
  if (programIdLine > 0) {
    mappings.push({
      cobolLine: programIdLine,
      cobolCode: cobolLines[programIdLine - 1] || '',
      javaLine: findLineContaining(javaLines, 'public class'),
      javaCode: javaLines[findLineContaining(javaLines, 'public class') - 1] || '',
      type: 'declaration',
      description: 'PROGRAM-ID からクラス名への変換',
    });
  }

  // WORKING-STORAGE SECTION → fields
  const wsLine = findLineContaining(cobolLines, 'WORKING-STORAGE');
  if (wsLine > 0) {
    mappings.push({
      cobolLine: wsLine,
      cobolCode: 'WORKING-STORAGE SECTION.',
      javaLine: findLineContaining(javaLines, 'private'),
      javaCode: '// Instance fields',
      type: 'structure',
      description: '作業場所節からインスタンスフィールドへの変換',
    });
  }

  // PROCEDURE DIVISION → methods
  const procLine = findLineContaining(cobolLines, 'PROCEDURE DIVISION');
  if (procLine > 0) {
    mappings.push({
      cobolLine: procLine,
      cobolCode: 'PROCEDURE DIVISION.',
      javaLine: findLineContaining(javaLines, 'public void'),
      javaCode: javaLines[findLineContaining(javaLines, 'public void') - 1] || '',
      type: 'structure',
      description: '手続き部からメソッド群への変換',
    });
  }

  return mappings;
}

/**
 * 指定文字列を含む行番号を検索
 */
function findLineContaining(lines: string[], search: string): number {
  const searchUpper = search.toUpperCase();
  for (let i = 0; i < lines.length; i++) {
    if (lines[i]?.toUpperCase().includes(searchUpper)) {
      return i + 1;
    }
  }
  return 0;
}

/**
 * 変数マッピングを抽出
 */
function extractVariableMappings(ast: CobolAst): VariableMapping[] {
  const mappings: VariableMapping[] = [];

  // Use ast.dataItems directly (root level in CobolAst)
  const items = ast.dataItems || [];
  
  for (const item of items) {
    if (item.name && item.level !== 88) {
      const javaName = convertToJavaIdentifier(item.name);
      const javaType = convertPicToJavaType(item.pic || '');
      
      mappings.push({
        cobolName: item.name,
        javaName,
        cobolType: `Level ${item.level}`,
        javaType,
        cobolPic: item.pic,
        notes: item.value ? `初期値: ${item.value}` : undefined,
      });
    }
  }

  return mappings;
}

/**
 * 構造マッピングを抽出
 */
function extractStructureMappings(ast: CobolAst): StructureMapping[] {
  const mappings: StructureMapping[] = [];

  // Division mappings
  if (ast.identificationDivision) {
    mappings.push({
      cobolStructure: 'IDENTIFICATION DIVISION',
      javaStructure: 'Class declaration with annotations',
      description: 'プログラムのメタデータをクラスレベルのアノテーションに変換',
    });
  }

  if (ast.dataItems && ast.dataItems.length > 0) {
    mappings.push({
      cobolStructure: 'WORKING-STORAGE SECTION',
      javaStructure: 'Private instance fields',
      description: 'COBOLのデータ項目をJavaフィールドに変換',
    });
  }

  if (ast.procedureDivision) {
    mappings.push({
      cobolStructure: 'PROCEDURE DIVISION',
      javaStructure: 'Public and private methods',
      description: '段落をメソッドに、PERFORMをメソッド呼び出しに変換',
    });

    // Use ast.paragraphs directly (root level in CobolAst)
    const paragraphs = ast.paragraphs || [];
    for (const para of paragraphs) {
      mappings.push({
        cobolStructure: `Paragraph: ${para.name}`,
        javaStructure: `Method: ${convertToJavaIdentifier(para.name)}()`,
        description: 'COBOLの段落をJavaメソッドに変換',
      });
    }
  }

  return mappings;
}

/**
 * COBOL名をJava識別子に変換
 */
function convertToJavaIdentifier(cobolName: string): string {
  return cobolName
    .toLowerCase()
    .replace(/-/g, '_')
    .replace(/[^a-z0-9_]/g, '');
}

/**
 * PIC句をJava型に変換
 */
function convertPicToJavaType(pic: string): string {
  if (!pic) return 'Object';

  const upperPic = pic.toUpperCase();

  if (upperPic.includes('9') && upperPic.includes('V')) {
    return 'BigDecimal';
  }
  if (upperPic.includes('9')) {
    const digits = (upperPic.match(/9/g) || []).length;
    if (digits <= 4) return 'short';
    if (digits <= 9) return 'int';
    if (digits <= 18) return 'long';
    return 'BigInteger';
  }
  if (upperPic.includes('X') || upperPic.includes('A')) {
    return 'String';
  }

  return 'Object';
}

/**
 * 差分レポートをMarkdown形式で出力
 */
export function formatDiffReportAsMarkdown(report: DiffReport): string {
  const lines: string[] = [
    `# ${report.title}`,
    '',
    `**生成日時**: ${report.timestamp}`,
    '',
    '## 変換統計',
    '',
    '| 項目 | COBOL | Java |',
    '|------|-------|------|',
    `| 有効行数 | ${report.statistics.cobolLines} | ${report.statistics.javaLines} |`,
    `| ステートメント数 | ${report.statistics.cobolStatements} | ${report.statistics.javaStatements} |`,
    `| 変数/フィールド数 | ${report.statistics.variableCount} | - |`,
    `| 段落/メソッド数 | ${report.statistics.paragraphCount} | ${report.statistics.methodCount} |`,
    '',
    `**変換複雑度**: ${report.statistics.conversionComplexity}`,
    '',
  ];

  // 構造マッピング
  if (report.structureMappings.length > 0) {
    lines.push('## 構造マッピング', '');
    lines.push('| COBOL構造 | Java構造 | 説明 |');
    lines.push('|-----------|----------|------|');
    for (const mapping of report.structureMappings) {
      lines.push(
        `| ${mapping.cobolStructure} | ${mapping.javaStructure} | ${mapping.description} |`
      );
    }
    lines.push('');
  }

  // 変数マッピング
  if (report.variableMappings.length > 0) {
    lines.push('## 変数マッピング', '');
    lines.push('| COBOL変数 | Java変数 | COBOL型 | Java型 | PIC |');
    lines.push('|-----------|----------|---------|--------|-----|');
    for (const mapping of report.variableMappings.slice(0, 20)) {
      lines.push(
        `| ${mapping.cobolName} | ${mapping.javaName} | ${mapping.cobolType} | ${mapping.javaType} | ${mapping.cobolPic || '-'} |`
      );
    }
    if (report.variableMappings.length > 20) {
      lines.push(`| ... | ... | ... | ... | ... |`);
      lines.push(`*他 ${report.variableMappings.length - 20} 件の変数*`);
    }
    lines.push('');
  }

  // 詳細マッピング
  if (report.mappings.length > 0) {
    lines.push('## コードマッピング', '');
    for (const mapping of report.mappings) {
      lines.push(`### ${mapping.description}`, '');
      lines.push('**COBOL** (行 ' + mapping.cobolLine + '):');
      lines.push('```cobol');
      lines.push(mapping.cobolCode);
      lines.push('```');
      lines.push('');
      lines.push('**Java** (行 ' + mapping.javaLine + '):');
      lines.push('```java');
      lines.push(mapping.javaCode);
      lines.push('```');
      lines.push('');
    }
  }

  return lines.join('\n');
}

/**
 * 差分レポートをHTML形式で出力
 */
export function formatDiffReportAsHtml(report: DiffReport): string {
  const escapeHtml = (text: string): string =>
    text
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;');

  return `<!DOCTYPE html>
<html lang="ja">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${escapeHtml(report.title)}</title>
  <style>
    body { font-family: 'Segoe UI', sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; }
    h1 { color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; }
    h2 { color: #34495e; margin-top: 30px; }
    table { width: 100%; border-collapse: collapse; margin: 20px 0; }
    th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }
    th { background-color: #3498db; color: white; }
    tr:nth-child(even) { background-color: #f9f9f9; }
    pre { background: #f4f4f4; padding: 15px; border-radius: 5px; overflow-x: auto; }
    .cobol { background: #fff3e0; }
    .java { background: #e3f2fd; }
    .stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; }
    .stat-card { background: #fff; border-radius: 8px; padding: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }
    .stat-value { font-size: 2em; color: #3498db; font-weight: bold; }
    .stat-label { color: #666; }
    .complexity-low { color: #27ae60; }
    .complexity-medium { color: #f39c12; }
    .complexity-high { color: #e74c3c; }
  </style>
</head>
<body>
  <h1>${escapeHtml(report.title)}</h1>
  <p><strong>生成日時:</strong> ${report.timestamp}</p>

  <h2>変換統計</h2>
  <div class="stats">
    <div class="stat-card">
      <div class="stat-value">${report.statistics.cobolLines}</div>
      <div class="stat-label">COBOL 行数</div>
    </div>
    <div class="stat-card">
      <div class="stat-value">${report.statistics.javaLines}</div>
      <div class="stat-label">Java 行数</div>
    </div>
    <div class="stat-card">
      <div class="stat-value">${report.statistics.variableCount}</div>
      <div class="stat-label">変数/フィールド数</div>
    </div>
    <div class="stat-card">
      <div class="stat-value">${report.statistics.methodCount}</div>
      <div class="stat-label">メソッド数</div>
    </div>
    <div class="stat-card">
      <div class="stat-value complexity-${report.statistics.conversionComplexity}">${report.statistics.conversionComplexity.toUpperCase()}</div>
      <div class="stat-label">変換複雑度</div>
    </div>
  </div>

  <h2>構造マッピング</h2>
  <table>
    <tr><th>COBOL構造</th><th>Java構造</th><th>説明</th></tr>
    ${report.structureMappings
      .map(
        (m) =>
          `<tr><td>${escapeHtml(m.cobolStructure)}</td><td>${escapeHtml(m.javaStructure)}</td><td>${escapeHtml(m.description)}</td></tr>`
      )
      .join('\n    ')}
  </table>

  <h2>変数マッピング</h2>
  <table>
    <tr><th>COBOL変数</th><th>Java変数</th><th>COBOL型</th><th>Java型</th><th>PIC</th></tr>
    ${report.variableMappings
      .slice(0, 30)
      .map(
        (m) =>
          `<tr><td>${escapeHtml(m.cobolName)}</td><td>${escapeHtml(m.javaName)}</td><td>${escapeHtml(m.cobolType)}</td><td>${escapeHtml(m.javaType)}</td><td>${escapeHtml(m.cobolPic || '-')}</td></tr>`
      )
      .join('\n    ')}
  </table>

  <h2>ソースコード比較</h2>
  <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 20px;">
    <div>
      <h3>COBOL (元ソース)</h3>
      <pre class="cobol"><code>${escapeHtml(report.cobolSource)}</code></pre>
    </div>
    <div>
      <h3>Java (変換後)</h3>
      <pre class="java"><code>${escapeHtml(report.javaSource)}</code></pre>
    </div>
  </div>
</body>
</html>`;
}
