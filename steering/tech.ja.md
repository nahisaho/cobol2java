# Technology Stack

**Project**: COBOL2Java
**Last Updated**: 2025-12-11
**Status**: Decided

---

## Overview

COBOL2Javaは、TypeScript/Node.jsをコアに、Rustによる高速パーサーを組み合わせたハイブリッド構成を採用します。LLM連携はOpenAI/Claude/Ollamaをサポートし、CI/CDパイプラインへの統合を重視したCLIファーストの設計です。

## Core Technology Stack

### Primary Language: TypeScript

| 項目 | 選択 | 理由 |
|------|------|------|
| 言語 | TypeScript 5.x | 型安全性、エコシステム、開発効率 |
| ランタイム | Node.js 20 LTS | 安定性、長期サポート |
| パッケージマネージャ | pnpm | 高速、ディスク効率 |

### Parser: Rust + tree-sitter

| 項目 | 選択 | 理由 |
|------|------|------|
| パーサー言語 | Rust | 高速、メモリ安全 |
| パーサーライブラリ | tree-sitter | 増分パース、エラー耐性 |
| Node.js連携 | napi-rs | Rust→Node.jsバインディング |

### LLM Integration

| プロバイダー | モデル | 用途 |
|--------------|--------|------|
| OpenAI | GPT-4o, GPT-4o-mini | クラウドLLM（デフォルト） |
| Anthropic | Claude 3.5 Sonnet | 高品質変換 |
| Ollama | Llama 3.2, CodeLlama | ローカルLLM（オフライン） |

### Testing & Quality

| 項目 | 選択 | 理由 |
|------|------|------|
| テストフレームワーク | Vitest | 高速、ESM対応 |
| E2Eテスト | COBOLEval | 146問ベンチマーク |
| Linter | ESLint + Biome | コード品質 |
| Formatter | Biome | 高速フォーマット |
| CI | GitHub Actions | GitHubネイティブ |

### Output Generation

| 項目 | 選択 | 理由 |
|------|------|------|
| Javaバージョン | Java 17 LTS | 長期サポート |
| Java Style | Google Java Style | 標準的 |
| ビルドツール | Maven / Gradle | 両対応 |

---

## Architecture Decisions

### ADR-001: TypeScript + Rust ハイブリッド構成

**Status**: Accepted

**Context**: COBOL→Java変換は、構文解析（CPU集約）とLLM呼び出し（I/O集約）の両方が必要。

**Decision**: 
- TypeScript: CLI、LLM連携、コード生成、テスト
- Rust: COBOLパーサー（tree-sitter-cobol）

**Consequences**:
- ✅ TypeScriptの開発効率とRustの高速パースを両立
- ✅ napi-rsによるシームレスな連携
- ⚠️ ビルド複雑性の増加（cargo + npm）

### ADR-002: Monorepo構成

**Status**: Accepted

**Context**: CLI、コアライブラリ、Webインターフェースを分離しつつ統一管理したい。

**Decision**: pnpm workspacesによるMonorepo

```
packages/
├── core/       # 変換ロジック（ライブラリ）
├── cli/        # CLIツール
├── web/        # Webインターフェース（将来）
└── parser/     # Rust COBOLパーサー
```

**Consequences**:
- ✅ コード共有が容易
- ✅ 統一されたバージョン管理
- ⚠️ 初期セットアップの複雑性

### ADR-003: COBOLEvalベースの品質保証

**Status**: Accepted

**Context**: 変換品質の客観的評価が必要。

**Decision**: Zorse/COBOLEval（146問）をベンチマークスイートとして採用。

**Consequences**:
- ✅ 定量的な品質評価
- ✅ リグレッションテストとして機能
- ⚠️ 実運用COBOLとは異なる可能性

---

## Dependencies

### Core Dependencies

```json
{
  "dependencies": {
    "@anthropic-ai/sdk": "^0.25.0",
    "commander": "^12.0.0",
    "openai": "^4.50.0",
    "ollama": "^0.5.0",
    "tree-sitter": "^0.21.0",
    "zod": "^3.23.0"
  },
  "devDependencies": {
    "@types/node": "^20.0.0",
    "typescript": "^5.5.0",
    "vitest": "^1.6.0",
    "biome": "^1.8.0"
  }
}
```

### Rust Dependencies (Cargo.toml)

```toml
[dependencies]
tree-sitter = "0.22"
tree-sitter-cobol = "0.1"  # カスタムまたはコミュニティ版
napi = "2"
napi-derive = "2"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

---

## Development Environment

### Required Tools

| ツール | バージョン | インストール |
|--------|-----------|-------------|
| Node.js | 20.x | nvm install 20 |
| pnpm | 9.x | npm install -g pnpm |
| Rust | 1.75+ | rustup |
| GnuCOBOL | 3.2 | apt install gnucobol |

### Setup Commands

```bash
# リポジトリクローン
git clone https://github.com/your-org/COBOL2Java.git
cd COBOL2Java

# 依存関係インストール
pnpm install

# Rustパーサービルド
cd packages/parser && cargo build --release

# テスト実行
pnpm test

# ベンチマーク実行
pnpm benchmark
```

---

## Deployment

### CLI Distribution

| 方式 | 対象 | コマンド |
|------|------|----------|
| npm | Node.js環境 | npm install -g cobol2java |
| 単体実行可能ファイル | バイナリ配布 | pkg / nexe |
| Docker | コンテナ環境 | docker run cobol2java |

### CI/CD Pipeline

```yaml
# .github/workflows/ci.yml
name: CI
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: pnpm/action-setup@v3
      - uses: actions/setup-node@v4
        with:
          node-version: 20
      - run: pnpm install
      - run: pnpm test
      - run: pnpm benchmark
```

---

*このドキュメントはMUSUBI SDD Article VI (Project Memory) に準拠しています*
