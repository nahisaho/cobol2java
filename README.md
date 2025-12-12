# COBOL2Java

<div align="center">

**COBOL から Java への変換ツール - LLM アシスト付き**

[![CI](https://github.com/your-org/cobol2java/actions/workflows/ci.yml/badge.svg)](https://github.com/your-org/cobol2java/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Tests](https://img.shields.io/badge/tests-438%20passed-brightgreen)
![Coverage](https://img.shields.io/badge/coverage-85%25-green)
![Node](https://img.shields.io/badge/node-%3E%3D20.0.0-blue)
![TypeScript](https://img.shields.io/badge/TypeScript-5.4-blue)

[ユーザーガイド](docs/USER_GUIDE.md) • [アーキテクチャ](docs/ARCHITECTURE.md) • [API ドキュメント](docs/api/)

</div>

---

## 概要

COBOL2Java は、レガシー COBOL コードを現代的な Java コードに変換するツールです。ルールベースの変換と LLM（大規模言語モデル）アシストを組み合わせた ハイブリッドアプローチを採用しています。

## ✨ 特徴

| 機能 | 説明 |
|------|------|
| 🔄 **完全な構造変換** | DIVISION、SECTION、段落をJavaクラス/メソッドに変換 |
| 🎯 **型安全な変換** | PIC句からJava型への正確なマッピング |
| 🌐 **複数のダイアレクト対応** | IBM Enterprise COBOL、Micro Focus、GnuCOBOL |
| 🚀 **Spring Boot/Batch対応** | エンタープライズJava形式での出力 |
| 🤖 **LLM アシスト** | OpenAI, Claude, Ollama, GitHub Copilot による高品質変換 |
| 📝 **Javadoc自動生成** | ドキュメント付きのクリーンなコード |
| 🖥️ **複数のインターフェース** | CLI、Webアプリ、VS Code拡張 |

## 📦 パッケージ構成

\`\`\`
packages/
├── core/              # コアライブラリ (パーサー、ジェネレーター)
├── cli/               # コマンドラインツール
├── webapp/            # Webアプリケーション (React)
└── vscode-extension/  # VS Code 拡張機能
\`\`\`

## 🚀 クイックスタート

### インストール

\`\`\`bash
git clone https://github.com/your-org/cobol2java.git
cd cobol2java
pnpm install
pnpm build
\`\`\`

### CLI で変換

\`\`\`bash
pnpm --filter @cobol2java/cli start -- convert input.cob -o output.java
\`\`\`

### Webアプリで変換

\`\`\`bash
cd packages/webapp
pnpm dev
\`\`\`

### プログラマティックAPI

\`\`\`typescript
import { CobolParser, JavaGenerator } from '@cobol2java/core';

const parser = new CobolParser();
const ast = parser.parse(cobolSource);

const generator = new JavaGenerator({
  packageName: 'com.example',
  javaVersion: 17,
  springBoot: true,
});

const result = await generator.generate(ast);
console.log(result.code);
\`\`\`

## 📋 サポートされるCOBOL構文

| カテゴリ | COBOL構文 | Java変換先 |
|---------|-----------|------------|
| **データ型** | PIC 9(n), PIC X(n), COMP-1/2/3 | int, String, BigDecimal |
| **入出力** | DISPLAY, ACCEPT | System.out.println, Scanner |
| **算術** | ADD, SUBTRACT, MULTIPLY, DIVIDE | +, -, *, / |
| **制御** | IF/ELSE, EVALUATE/WHEN | if/else, switch/case |
| **ループ** | PERFORM UNTIL/VARYING | while, for |
| **文字列** | STRING, UNSTRING, INSPECT | concat, split, replace |

## 📊 パフォーマンス

| 指標 | 値 |
|------|-----|
| スループット | ~2,000+ 変換/秒 |
| テスト | 438 パス ✅ |

## 📚 ドキュメント

- [ユーザーガイド](docs/USER_GUIDE.md)
- [アーキテクチャ](docs/ARCHITECTURE.md)
- [CHANGELOG](CHANGELOG.md)
- [CONTRIBUTING](CONTRIBUTING.md)

## 📄 ライセンス

MIT License
