/**
 * COBOL2Java Javadoc Generator
 *
 * COBOLソースからJavadocコメントを自動生成
 */

import { CobolAst } from '../parser.js';

export interface JavadocOptions {
  /** 詳細なドキュメントを生成するか */
  detailed?: boolean;
  /** 元のCOBOLコードをコメントに含めるか */
  includeOriginalCode?: boolean;
  /** 著者名 */
  author?: string;
  /** バージョン */
  version?: string;
  /** @since タグの値 */
  since?: string;
  /** カスタムタグ */
  customTags?: Record<string, string>;
  /** 日本語でドキュメントを生成するか */
  japanese?: boolean;
}

export interface ClassJavadoc {
  description: string;
  author?: string;
  version?: string;
  since?: string;
  see?: string[];
  deprecated?: string;
  customTags?: Record<string, string>;
}

export interface MethodJavadoc {
  description: string;
  params: Array<{ name: string; description: string }>;
  returns?: string;
  throws?: Array<{ type: string; description: string }>;
  see?: string[];
  deprecated?: string;
}

export interface FieldJavadoc {
  description: string;
  cobolPic?: string;
  cobolLevel?: number;
  cobolName?: string;
}

export interface GeneratedJavadocs {
  classDoc: ClassJavadoc;
  methodDocs: Map<string, MethodJavadoc>;
  fieldDocs: Map<string, FieldJavadoc>;
}

/**
 * ASTからJavadocを生成
 */
export function generateJavadocs(
  ast: CobolAst,
  options: JavadocOptions = {}
): GeneratedJavadocs {
  const {
    detailed = true,
    includeOriginalCode = false,
    author = 'COBOL2Java Converter',
    version = '1.0.0',
    since = '1.0.0',
    customTags = {},
    japanese = true,
  } = options;

  const programId = ast.identificationInfo?.programId || ast.programName || 'UnknownProgram';
  const programAuthor = ast.identificationInfo?.author;
  const dateWritten = ast.identificationInfo?.dateWritten;

  // クラスドキュメント
  const classDoc: ClassJavadoc = {
    description: japanese
      ? `${programId} - COBOLプログラムから変換されたJavaクラス。\n` +
        (detailed
          ? `このクラスは元のCOBOLプログラムのビジネスロジックをJavaで実装しています。`
          : '')
      : `${programId} - Java class converted from COBOL program.`,
    author: programAuthor || author,
    version,
    since,
    customTags: {
      ...customTags,
      ...(dateWritten ? { 'original-date': dateWritten } : {}),
    },
  };

  // メソッドドキュメント
  const methodDocs = new Map<string, MethodJavadoc>();

  // Use ast.paragraphs directly (root level in CobolAst)
  const paragraphs = ast.paragraphs || [];
  for (const para of paragraphs) {
    const methodName = convertToJavaMethodName(para.name);
    const isMainParagraph = para.name.toLowerCase().includes('main') ||
      paragraphs.indexOf(para) === 0;

    methodDocs.set(methodName, {
      description: generateMethodDescription(para.name, isMainParagraph, japanese, detailed),
      params: [],
      returns: japanese ? 'なし' : 'void',
      throws: [],
      see: isMainParagraph ? [] : [convertToJavaMethodName(paragraphs[0]?.name || 'main')],
    });
  }

  // デフォルトのmain/runメソッド
  if (!methodDocs.has('run')) {
    methodDocs.set('run', {
      description: japanese
        ? 'プログラムのメインエントリーポイント。元のCOBOLプログラムの実行ロジックを呼び出します。'
        : 'Main entry point for the program. Invokes the original COBOL program execution logic.',
      params: [],
      returns: japanese ? 'なし' : 'void',
    });
  }

  // フィールドドキュメント
  const fieldDocs = new Map<string, FieldJavadoc>();

  // Use ast.dataItems directly (root level in CobolAst)
  const items = ast.dataItems || [];
  for (const item of items) {
    if (item.name && item.level !== 88) {
      const fieldName = convertToJavaFieldName(item.name);
      
      fieldDocs.set(fieldName, {
        description: generateFieldDescription(item, japanese, detailed),
        cobolPic: item.pic,
        cobolLevel: item.level,
        cobolName: item.name,
      });
    }
  }

  return { classDoc, methodDocs, fieldDocs };
}

/**
 * メソッド説明を生成
 */
function generateMethodDescription(
  paragraphName: string,
  isMain: boolean,
  japanese: boolean,
  detailed: boolean
): string {
  const cleanName = paragraphName.replace(/-/g, ' ').toLowerCase();

  if (japanese) {
    if (isMain) {
      return detailed
        ? `メイン処理を実行します。\n${cleanName}段落から変換されたメソッドで、プログラムの主要なビジネスロジックを含みます。`
        : `${cleanName}段落の処理を実行します。`;
    }
    return detailed
      ? `${cleanName}段落の処理を実行します。\n元のCOBOL PERFORM文から呼び出されるサブルーチン相当の処理です。`
      : `${cleanName}段落の処理を実行します。`;
  }

  if (isMain) {
    return detailed
      ? `Executes the main processing logic.\nConverted from ${cleanName} paragraph, contains the primary business logic.`
      : `Executes the ${cleanName} paragraph logic.`;
  }
  return detailed
    ? `Executes the ${cleanName} paragraph logic.\nSubroutine equivalent called from COBOL PERFORM statements.`
    : `Executes the ${cleanName} paragraph logic.`;
}

/**
 * フィールド説明を生成
 */
function generateFieldDescription(
  item: { name?: string; level?: number; picture?: string; value?: string },
  japanese: boolean,
  detailed: boolean
): string {
  const name = item.name || 'unknown';
  const pic = item.picture;
  const level = item.level;
  const value = item.value;

  if (japanese) {
    let desc = `${name.replace(/-/g, ' ')}`;
    if (detailed) {
      if (pic) {
        desc += `\n@cobol.pic ${pic}`;
      }
      if (level) {
        desc += `\n@cobol.level ${level}`;
      }
      if (value) {
        desc += `\n初期値: ${value}`;
      }
    }
    return desc;
  }

  let desc = `Field: ${name.replace(/-/g, ' ')}`;
  if (detailed) {
    if (pic) {
      desc += `\n@cobol.pic ${pic}`;
    }
    if (level) {
      desc += `\n@cobol.level ${level}`;
    }
    if (value) {
      desc += `\nInitial value: ${value}`;
    }
  }
  return desc;
}

/**
 * COBOL段落名をJavaメソッド名に変換
 */
function convertToJavaMethodName(cobolName: string): string {
  const parts = cobolName.toLowerCase().split('-');
  return parts
    .map((part, i) => (i === 0 ? part : part.charAt(0).toUpperCase() + part.slice(1)))
    .join('');
}

/**
 * COBOLデータ名をJavaフィールド名に変換
 */
function convertToJavaFieldName(cobolName: string): string {
  return convertToJavaMethodName(cobolName);
}

/**
 * ClassJavadocをフォーマット
 */
export function formatClassJavadoc(doc: ClassJavadoc): string {
  const lines: string[] = ['/**'];
  
  // 説明
  for (const line of doc.description.split('\n')) {
    lines.push(` * ${line}`);
  }
  lines.push(' *');

  // 標準タグ
  if (doc.author) {
    lines.push(` * @author ${doc.author}`);
  }
  if (doc.version) {
    lines.push(` * @version ${doc.version}`);
  }
  if (doc.since) {
    lines.push(` * @since ${doc.since}`);
  }
  if (doc.deprecated) {
    lines.push(` * @deprecated ${doc.deprecated}`);
  }
  if (doc.see) {
    for (const ref of doc.see) {
      lines.push(` * @see ${ref}`);
    }
  }

  // カスタムタグ
  if (doc.customTags) {
    for (const [tag, value] of Object.entries(doc.customTags)) {
      lines.push(` * @${tag} ${value}`);
    }
  }

  lines.push(' */');
  return lines.join('\n');
}

/**
 * MethodJavadocをフォーマット
 */
export function formatMethodJavadoc(doc: MethodJavadoc): string {
  const lines: string[] = ['  /**'];
  
  // 説明
  for (const line of doc.description.split('\n')) {
    lines.push(`   * ${line}`);
  }
  
  // パラメータ
  if (doc.params.length > 0) {
    lines.push('   *');
    for (const param of doc.params) {
      lines.push(`   * @param ${param.name} ${param.description}`);
    }
  }

  // 戻り値
  if (doc.returns && doc.returns !== 'void' && doc.returns !== 'なし') {
    lines.push(`   * @return ${doc.returns}`);
  }

  // 例外
  if (doc.throws && doc.throws.length > 0) {
    for (const ex of doc.throws) {
      lines.push(`   * @throws ${ex.type} ${ex.description}`);
    }
  }

  // 参照
  if (doc.see && doc.see.length > 0) {
    for (const ref of doc.see) {
      lines.push(`   * @see #${ref}()`);
    }
  }

  // 非推奨
  if (doc.deprecated) {
    lines.push(`   * @deprecated ${doc.deprecated}`);
  }

  lines.push('   */');
  return lines.join('\n');
}

/**
 * FieldJavadocをフォーマット
 */
export function formatFieldJavadoc(doc: FieldJavadoc): string {
  const lines: string[] = ['  /**'];
  
  // 説明
  for (const line of doc.description.split('\n')) {
    lines.push(`   * ${line}`);
  }

  lines.push('   */');
  return lines.join('\n');
}

/**
 * Javaコードに Javadoc を挿入
 */
export function insertJavadocsIntoCode(
  javaCode: string,
  javadocs: GeneratedJavadocs
): string {
  let result = javaCode;

  // クラスJavadocを挿入
  const classPattern = /(public\s+class\s+\w+)/;
  const classMatch = result.match(classPattern);
  if (classMatch) {
    const classJavadoc = formatClassJavadoc(javadocs.classDoc);
    result = result.replace(classPattern, `${classJavadoc}\n$1`);
  }

  // メソッドJavadocを挿入
  for (const [methodName, doc] of javadocs.methodDocs) {
    const methodPattern = new RegExp(
      `(\\s+)(public|private|protected)\\s+(void|\\w+)\\s+${methodName}\\s*\\(`,
      'g'
    );
    const methodJavadoc = formatMethodJavadoc(doc);
    result = result.replace(methodPattern, `$1${methodJavadoc}\n$1$2 $3 ${methodName}(`);
  }

  // フィールドJavadocを挿入
  for (const [fieldName, doc] of javadocs.fieldDocs) {
    const fieldPattern = new RegExp(
      `(\\s+)(private|protected|public)\\s+(\\w+)\\s+${fieldName}\\s*[;=]`,
      'g'
    );
    const fieldJavadoc = formatFieldJavadoc(doc);
    result = result.replace(fieldPattern, `$1${fieldJavadoc}\n$1$2 $3 ${fieldName};`);
  }

  return result;
}
