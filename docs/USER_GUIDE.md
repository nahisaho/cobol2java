# COBOL2Java ユーザーガイド

## 目次

1. [はじめに](#はじめに)
2. [インストール](#インストール)
3. [クイックスタート](#クイックスタート)
4. [CLI使用方法](#cli使用方法)
5. [Webアプリケーション](#webアプリケーション)
6. [VS Code拡張機能](#vs-code拡張機能)
7. [プログラマティックAPI](#プログラマティックapi)
8. [設定オプション](#設定オプション)
9. [COBOLダイアレクト対応](#cobolダイアレクト対応)
10. [トラブルシューティング](#トラブルシューティング)

---

## はじめに

COBOL2Javaは、レガシーCOBOLプログラムをモダンなJavaコードに変換するツールです。

### 特徴

- 🔄 **完全な構造変換**: DIVISION、SECTION、段落をJavaクラス/メソッドに変換
- 🎯 **型安全な変換**: PIC句からJava型への正確なマッピング
- 🌐 **複数のダイアレクト対応**: IBM Enterprise COBOL、Micro Focus、GnuCOBOL
- 🚀 **Spring Boot/Batch対応**: エンタープライズJava形式での出力
- 📝 **Javadoc自動生成**: ドキュメント付きのクリーンなコード

---

## インストール

### 前提条件

- Node.js 20 LTS以上
- pnpm 9以上

### npmからインストール

```bash
# CLIツール
npm install -g @cobol2java/cli

# ライブラリとして使用
npm install @cobol2java/core
```

### ソースからビルド

```bash
git clone https://github.com/nahisaho/cobol2java.git
cd cobol2java
pnpm install
pnpm build
```

---

## クイックスタート

### 1. 単一ファイル変換

```bash
cobol2java convert input.cob -o output.java
```

### 2. ディレクトリ一括変換

```bash
cobol2java convert ./cobol-src -o ./java-output --recursive
```

### 3. Webアプリで変換

```bash
cd packages/webapp
pnpm dev
# http://localhost:5173 にアクセス
```

---

## CLI使用方法

### 基本コマンド

```bash
cobol2java <command> [options]
```

### コマンド一覧

| コマンド | 説明 |
|---------|------|
| `convert` | COBOLファイルをJavaに変換 |
| `analyze` | COBOLファイルを解析（変換なし） |
| `batch` | 複数ファイルを一括変換 |
| `--help` | ヘルプを表示 |
| `--version` | バージョンを表示 |

### convert コマンド

```bash
cobol2java convert <input> [options]

オプション:
  -o, --output <path>       出力ファイル/ディレクトリ
  -p, --package <name>      Javaパッケージ名 (default: "com.example.converted")
  -j, --java-version <ver>  Javaバージョン (11, 17, 21)
  --spring-boot             Spring Boot形式で出力
  --spring-batch            Spring Batch形式で出力
  --dialect <type>          COBOLダイアレクト (auto, ibm, microfocus, gnucobol)
  -v, --verbose             詳細ログを表示
```

### 使用例

```bash
# 基本変換
cobol2java convert program.cob -o Program.java

# パッケージ名を指定
cobol2java convert program.cob -o src/main/java -p com.company.legacy

# Spring Boot形式で出力
cobol2java convert program.cob --spring-boot -p com.company.app

# バッチ変換
cobol2java batch ./cobol-src -o ./java-output -p com.company --recursive
```

---

## Webアプリケーション

### 起動方法

```bash
cd packages/webapp
pnpm dev
```

### 機能

- **コードエディタ**: Monaco Editorベースのシンタックスハイライト
- **リアルタイムプレビュー**: 入力と同時に変換結果を表示
- **設定パネル**: パッケージ名、Javaバージョン、Spring対応の設定
- **ダウンロード**: 変換後のJavaファイルをダウンロード

### スクリーンショット

```
┌─────────────────────────────────────────────────────────┐
│  COBOL2Java Converter                           [設定]  │
├──────────────────────┬──────────────────────────────────┤
│  COBOL Source        │  Java Output                     │
│  ┌────────────────┐  │  ┌────────────────────────────┐  │
│  │ IDENTIFICATION │  │  │ package com.example;       │  │
│  │ DIVISION.      │  │  │                            │  │
│  │ PROGRAM-ID.    │  │  │ public class HelloWorld {  │  │
│  │   HELLO.       │  │  │   public void run() {      │  │
│  │ ...            │  │  │     // ...                 │  │
│  └────────────────┘  │  └────────────────────────────┘  │
│                      │                                  │
│  [変換] [クリア]     │  [コピー] [ダウンロード]         │
└──────────────────────┴──────────────────────────────────┘
```

---

## VS Code拡張機能

### インストール

```bash
code --install-extension cobol2java-0.3.0.vsix
```

### コマンド

| コマンド | ショートカット | 説明 |
|---------|---------------|------|
| `COBOL2Java: Convert to Java` | `Ctrl+Shift+J` | 現在のファイルを変換 |
| `COBOL2Java: Show Preview` | `Ctrl+Shift+P` | ライブプレビュー表示 |
| `COBOL2Java: Convert to Clipboard` | - | クリップボードにコピー |
| `COBOL2Java: Detect Dialect` | - | ダイアレクト検出 |

### 設定

```json
{
  "cobol2java.packageName": "com.company.legacy",
  "cobol2java.javaVersion": 17,
  "cobol2java.springBoot": true,
  "cobol2java.outputDirectory": "src/main/java"
}
```

---

## プログラマティックAPI

### 基本使用法

```typescript
import { CobolParser, JavaGenerator } from '@cobol2java/core';

// パース
const parser = new CobolParser();
const ast = parser.parse(cobolSource);

// 変換
const generator = new JavaGenerator({
  packageName: 'com.example',
  javaVersion: 17,
  springBoot: false,
});
const result = await generator.generate(ast);

console.log(result.code);      // Javaコード
console.log(result.className); // クラス名
```

### 便利な関数

```typescript
import { convert } from '@cobol2java/core';

// ワンライナー変換
const result = await convert(cobolSource, {
  packageName: 'com.example',
  javaVersion: 17,
});
```

### バッチ変換

```typescript
import { batchConvert } from '@cobol2java/core';

const result = await batchConvert({
  inputDir: './cobol-src',
  outputDir: './java-output',
  packageName: 'com.example',
  concurrency: 4,
  onProgress: (progress) => {
    console.log(`${progress.percentage}% 完了`);
  },
});

console.log(`成功: ${result.successCount}/${result.totalFiles}`);
```

### 差分レポート

```typescript
import { generateDiffReport, formatDiffReportAsHtml } from '@cobol2java/core';

const report = generateDiffReport(cobolSource, javaSource, ast);
const html = formatDiffReportAsHtml(report);
```

### Javadoc生成

```typescript
import { generateJavadocs, insertJavadocsIntoCode } from '@cobol2java/core';

const javadocs = generateJavadocs(ast, { japanese: true });
const documentedCode = insertJavadocsIntoCode(javaCode, javadocs);
```

---

## 設定オプション

### GeneratorOptions

| オプション | 型 | デフォルト | 説明 |
|-----------|-----|----------|------|
| `packageName` | string | `com.example.converted` | Javaパッケージ名 |
| `javaVersion` | number | `17` | ターゲットJavaバージョン |
| `springBoot` | boolean | `false` | Spring Boot形式で出力 |
| `springBatch` | boolean | `false` | Spring Batch形式で出力 |

### BatchConversionOptions

| オプション | 型 | デフォルト | 説明 |
|-----------|-----|----------|------|
| `inputDir` | string | 必須 | 入力ディレクトリ |
| `outputDir` | string | 必須 | 出力ディレクトリ |
| `pattern` | string | `**/*.{cob,cbl}` | ファイルパターン |
| `concurrency` | number | `4` | 並列処理数 |
| `continueOnError` | boolean | `true` | エラー時に続行 |

---

## COBOLダイアレクト対応

### 自動検出

```typescript
import { detectDialect } from '@cobol2java/core';

const dialect = detectDialect(cobolSource);
// 'IBM Enterprise COBOL' | 'Micro Focus COBOL' | 'GnuCOBOL' | 'COBOL-85 Standard'
```

### 対応ダイアレクト

| ダイアレクト | 特徴 |
|-------------|------|
| IBM Enterprise COBOL | EXEC CICS, EXEC SQL, CBL/PROCESS指令 |
| Micro Focus COBOL | $SET, >>SOURCE, 78レベル |
| GnuCOBOL | *> コメント, >>DEFINE |
| COBOL-85 Standard | 標準的なDIVISION構造 |

---

## トラブルシューティング

### よくあるエラー

#### ParseError: Unexpected token

```
原因: COBOLソースの構文エラー
対策: ソースコードの構文を確認してください
```

#### GenerationError: Unsupported PIC clause

```
原因: 未対応のPIC句形式
対策: 標準的なPIC形式に変更するか、issueを報告してください
```

#### ダイアレクト検出が正しくない

```
対策: --dialect オプションで明示的に指定
例: cobol2java convert program.cob --dialect ibm
```

### デバッグモード

```bash
DEBUG=cobol2java:* cobol2java convert program.cob
```

### ログレベル設定

```bash
cobol2java convert program.cob --log-level debug
```

---

## サポート

- **GitHub Issues**: [https://github.com/nahisaho/cobol2java/issues](https://github.com/nahisaho/cobol2java/issues)
- **ドキュメント**: [https://nahisaho.github.io/cobol2java](https://nahisaho.github.io/cobol2java)

---

## ライセンス

MIT License
