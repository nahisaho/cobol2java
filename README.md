# COBOL2Java

COBOL から Java への変換ツール - LLM アシスト付き

[![CI](https://github.com/your-org/cobol2java/actions/workflows/ci.yml/badge.svg)](https://github.com/your-org/cobol2java/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Tests](https://img.shields.io/badge/tests-54%20passed-brightgreen)

## 概要

COBOL2Java は、レガシー COBOL コードを現代的な Java コードに変換するツールです。ルールベースの変換と LLM（大規模言語モデル）アシストを組み合わせた ハイブリッドアプローチを採用しています。

## 特徴

- 🔄 **COBOL-85 互換** - 主要な COBOL 構文をサポート
- 🤖 **LLM アシスト** - OpenAI, Claude, Ollama によるスマート変換
- 🏃 **CLI ファースト** - CI/CD パイプラインへの統合が容易
- 📊 **ベンチマーク** - COBOLEval による品質評価
- 🌱 **Spring Boot 対応** - エンタープライズ Java への変換
- 📝 **型マッピング** - PIC句からBigDecimal/int/String等へ自動変換

## クイックスタート

```bash
# インストール
pnpm install

# ビルド
pnpm build

# 変換
pnpm --filter cli start -- convert examples/hello-world.cbl -o output/

# テスト
pnpm test:run

# 検証のみ
pnpm --filter cli start -- validate examples/hello-world.cbl

# ベンチマーク
pnpm --filter cli start -- benchmark
```

## 使い方

### 基本的な変換

```bash
# examples/hello-world.cbl を output/ に変換
pnpm --filter cli start -- convert examples/hello-world.cbl -o output/

# グローバルにインストールした場合
cobol2java convert input.cbl -o ./output
```

### LLM アシスト付き変換

```bash
# OpenAI
export OPENAI_API_KEY=sk-...
cobol2java convert input.cbl --llm openai

# Claude
export ANTHROPIC_API_KEY=sk-...
cobol2java convert input.cbl --llm claude

# Ollama (ローカル)
cobol2java convert input.cbl --llm ollama --model llama3.2
```

### Spring Boot コード生成

```bash
cobol2java convert input.cbl --spring-boot --package com.myapp
```

## プロジェクト構成

```
packages/
├── core/    # 変換ライブラリ
├── cli/     # コマンドラインツール
├── parser/  # COBOL パーサー (Rust)
└── web/     # Web インターフェース
```

## 開発

```bash
# 依存関係インストール
pnpm install

# ビルド
pnpm build

# テスト
pnpm test

# 型チェック
pnpm typecheck
```

## ドキュメント

- [Getting Started](docs/getting-started.md)
- [API Reference](docs/api.md)
- [Architecture](storage/features/cobol-java-converter/design.md)
- [Requirements](storage/features/cobol-java-converter/requirements.md)

## ライセンス

MIT License
