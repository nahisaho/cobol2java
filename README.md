# COBOL2Java

COBOL から Java への変換ツール - LLM アシスト付き

[![CI](https://github.com/your-org/cobol2java/actions/workflows/ci.yml/badge.svg)](https://github.com/your-org/cobol2java/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Tests](https://img.shields.io/badge/tests-66%20passed-brightgreen)
![Pass@1](https://img.shields.io/badge/Pass@1-100%25-brightgreen)

## 概要

COBOL2Java は、レガシー COBOL コードを現代的な Java コードに変換するツールです。ルールベースの変換と LLM（大規模言語モデル）アシストを組み合わせた ハイブリッドアプローチを採用しています。

## 特徴

- 🔄 **COBOL-85 互換** - 主要な COBOL 構文をサポート
- 🤖 **LLM アシスト** - OpenAI, Claude, Ollama, GitHub Copilot によるスマート変換
- 🏃 **CLI ファースト** - CI/CD パイプラインへの統合が容易
- 📊 **ベンチマーク** - サンプル変換による品質評価
- 🌱 **Spring Boot 対応** - エンタープライズ Java への変換
- 📝 **型マッピング** - PIC句からBigDecimal/int/String等へ自動変換
- 🧩 **VS Code 拡張** - エディタ内でのCOBOL変換（Copilot連携）

## サポートされるCOBOL構文

| カテゴリ | 構文 | 変換先 |
|---------|------|--------|
| **データ型** | PIC 9, PIC X, PIC A, COMP-1/2/3 | int, String, BigDecimal |
| **入出力** | DISPLAY, ACCEPT | System.out.println, Scanner |
| **代入** | MOVE, COMPUTE, INITIALIZE | = |
| **算術** | ADD, SUBTRACT, MULTIPLY, DIVIDE | +, -, *, / |
| **制御** | IF/ELSE/END-IF | if/else |
| **制御** | EVALUATE/WHEN/END-EVALUATE | switch/case |
| **ループ** | PERFORM UNTIL, PERFORM VARYING | while, for |
| **手続き** | PERFORM paragraph, STOP RUN, GOBACK, EXIT | メソッド呼び出し, return |
| **文字列** | STRING...INTO, INSPECT REPLACING | + (連結), replace() |
| **その他** | SET, CONTINUE | 代入, コメント |

## クイックスタート

```bash
# インストール
pnpm install

# ビルド
pnpm build

# 変換
pnpm --filter cli start -- convert examples/hello-world.cbl -o output/

# テスト
pnpm test

# 検証のみ
pnpm --filter cli start -- validate examples/hello-world.cbl

# ベンチマーク
pnpm --filter cli start -- benchmark --mode examples --verbose
```

## 使い方

### 基本的な変換

```bash
# examples/hello-world.cbl を output/ に変換
pnpm --filter cli start -- convert examples/hello-world.cbl -o output/

# パッケージ名を指定
pnpm --filter cli start -- convert input.cbl -o output/ --package com.mycompany
```

### LLM アシスト付き変換

```bash
# OpenAI
export OPENAI_API_KEY=sk-...
pnpm --filter cli start -- convert input.cbl --llm openai

# Claude
export ANTHROPIC_API_KEY=sk-...
pnpm --filter cli start -- convert input.cbl --llm claude

# Ollama (ローカル)
pnpm --filter cli start -- convert input.cbl --llm ollama --model llama3.2

# GitHub Copilot (VS Code 拡張機能内のみ)
# VS Code でコマンドパレットから "COBOL2Java: Convert with Copilot" を実行
```

### Spring Boot コード生成

```bash
pnpm --filter cli start -- convert input.cbl --spring-boot --package com.myapp
```

### ベンチマーク

```bash
# サンプルファイルでベンチマーク
pnpm --filter cli start -- benchmark --mode examples --verbose

# COBOLEval データセットでベンチマーク (LLM必須)
pnpm --filter cli start -- benchmark --mode coboleval --llm openai --limit 10
```

## プロジェクト構成

```
packages/
├── core/    # 変換ライブラリ (パーサー、ジェネレーター、LLMクライアント)
├── cli/     # コマンドラインツール
└── web/     # VS Code 拡張機能

examples/    # サンプル COBOL ファイル
├── hello-world.cbl      # 基本的なHello World
├── calculate-tax.cbl    # 税計算 (DIVIDE, SUBTRACT)
├── fibonacci.cbl        # フィボナッチ数列 (PERFORM UNTIL)
├── grade-checker.cbl    # 成績判定 (IF/ELSE)
└── status-checker.cbl   # ステータス判定 (EVALUATE/WHEN)
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

# Lint
pnpm lint
```

## VS Code 拡張機能

`packages/web/` に VS Code 拡張機能が含まれています。

### 機能

- COBOLファイルを開いた状態で「Convert to Java」コマンド
- GitHub Copilot 連携による高品質変換
- COBOL シンタックスハイライト

### インストール

```bash
cd packages/web
pnpm install
pnpm build
# .vsix ファイルを生成して VS Code にインストール
```

## ドキュメント

- [Architecture](storage/features/cobol-java-converter/design.md) - C4アーキテクチャ設計
- [Requirements](storage/features/cobol-java-converter/requirements.md) - EARS要件
- [Tasks](storage/features/cobol-java-converter/tasks.md) - 実装タスク
- [ADR](storage/features/cobol-java-converter/adr.md) - アーキテクチャ決定記録

## ベンチマーク結果

| 指標 | 結果 |
|------|------|
| サンプルファイル | 5/5 (100%) |
| 変換成功率 | 100% |
| コンパイル成功率 | 100% |
| 実行成功率 | 100% |
| テスト | 66/66 passed |

## ライセンス

MIT License
