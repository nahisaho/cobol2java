# Architecture Decision Records (ADR)

**Feature**: cobol-java-converter  
**Created**: 2025-12-11

---

## ADR-001: TypeScript + Rust ハイブリッド構成

### Status
**Accepted**

### Context
COBOL→Java変換には以下の2つの処理が必要：
1. **構文解析**: CPU集約的、パフォーマンス重要
2. **LLM連携・コード生成**: I/O集約的、開発効率重要

単一言語での実装には以下の課題がある：
- TypeScript単体: パーサーのパフォーマンスが不十分
- Rust単体: LLM連携やCLI開発の効率が低下

### Decision
**TypeScript（Node.js）とRustのハイブリッド構成を採用する**

- **TypeScript**: CLI、LLM連携、コード生成、テスト
- **Rust**: COBOLパーサー（tree-sitter-cobol）、napi-rsでNode.jsと連携

### Consequences
#### Positive
- TypeScriptの開発効率とRustの高速パースを両立
- napi-rsによるシームレスなJS/Rust連携
- TypeScriptエコシステム（npm、各種ツール）を活用可能

#### Negative
- ビルド複雑性の増加（cargo + npm）
- CI/CDでRustビルド環境が必要
- デバッグ時にTypeScript/Rust境界の問題追跡が困難

#### Risks
- tree-sitter-cobolの成熟度が低い場合、カスタム文法定義が必要

---

## ADR-002: pnpm Workspaces による Monorepo 構成

### Status
**Accepted**

### Context
プロジェクトは以下のパッケージで構成される：
- `core`: 変換ロジック（ライブラリ）
- `cli`: コマンドラインツール
- `parser`: Rust COBOLパーサー
- `web`: Webインターフェース（将来）

これらを個別リポジトリで管理すると：
- バージョン同期が困難
- コード共有にnpm publish/installが必要
- CIが複雑化

### Decision
**pnpm workspacesによるMonorepo構成を採用する**

```
packages/
├── core/       # cobol2java-core
├── cli/        # cobol2java (bin)
├── parser/     # @cobol2java/parser
└── web/        # @cobol2java/web
```

### Consequences
#### Positive
- 統一されたバージョン管理
- パッケージ間の参照が容易（workspace:*）
- 単一のCIパイプライン

#### Negative
- 初期セットアップの複雑性
- 大規模になるとビルド時間が増加

---

## ADR-003: LLM プロバイダー抽象化レイヤー

### Status
**Accepted**

### Context
複数のLLMプロバイダーをサポートする必要がある：
- OpenAI (GPT-4)
- Anthropic (Claude)
- Ollama (ローカルLLM)

各プロバイダーのAPIは異なり、直接使用すると：
- コードの重複
- プロバイダー切り替えが困難
- テストが複雑化

### Decision
**LLMクライアント抽象化レイヤーを導入する**

```typescript
interface LLMClient {
  complete(prompt: string, options?: CompletionOptions): Promise<string>;
  isAvailable(): Promise<boolean>;
}

// 実装
class OpenAIClient implements LLMClient { ... }
class ClaudeClient implements LLMClient { ... }
class OllamaClient implements LLMClient { ... }
class NoopClient implements LLMClient { ... }  // LLMなしモード
```

### Consequences
#### Positive
- プロバイダー切り替えが容易
- テスト時にモック実装が使用可能
- 新規プロバイダー追加が容易

#### Negative
- 抽象化レイヤーのメンテナンスコスト
- プロバイダー固有機能が使いにくい

#### Note
Article VIII（Anti-Abstraction Gate）との整合性：
- **許可される抽象化**: 複数プロバイダーサポートのための抽象化
- 単一プロバイダーのラッパーではなく、マルチプロバイダー対応

---

## ADR-004: COBOLEval をベンチマークスイートとして採用

### Status
**Accepted**

### Context
変換品質の評価には客観的な基準が必要。選択肢：
1. 独自テストスイートを作成
2. 既存のCOBOLベンチマークを使用
3. 実運用COBOLコードを使用

### Decision
**Zorse/COBOLEval（146問）をベンチマークスイートとして採用する**

- HumanEvalのCOBOL版
- テストケース付き
- MIT License
- Python正解実装が付属（参照用）

### Consequences
#### Positive
- 定量的な品質評価（Pass@k）
- リグレッションテストとして機能
- コミュニティで共有されたベンチマーク

#### Negative
- 実運用COBOLとは異なる可能性（アルゴリズム中心）
- 146問では網羅性に限界

#### Mitigation
- 実運用テストケースを追加で収集
- ベンチマーク結果と実運用結果を区別して報告

---

## ADR-005: Google Java Style ガイドの採用

### Status
**Accepted**

### Context
生成されるJavaコードのスタイルを統一する必要がある。選択肢：
1. Google Java Style Guide
2. Oracle Code Conventions
3. カスタムスタイル

### Decision
**Google Java Style Guideを採用する**

- 広く使われている
- google-java-formatツールで自動整形可能
- 明確な仕様がある

### Consequences
#### Positive
- 業界標準に準拠
- 自動整形ツールが利用可能
- レビュアーにとって馴染みやすい

#### Negative
- 一部のCOBOL命名規則（ハイフン区切り）との相性が悪い

---

## ADR-006: CLI ファーストアプローチ

### Status
**Accepted**

### Context
Article II（CLI Interface Mandate）に準拠する必要がある。また：
- CI/CDパイプラインへの統合が主要ユースケース
- バッチ処理が必要
- Webインターフェースは後回し可能

### Decision
**CLIを最優先で実装し、Webはphase 2以降とする**

CLI機能：
- `cobol2java convert` - 変換実行
- `cobol2java validate` - 構文検証
- `cobol2java benchmark` - ベンチマーク実行
- `cobol2java init` - 設定ファイル生成

### Consequences
#### Positive
- Article II完全準拠
- スクリプト化・自動化が容易
- 早期にCI/CD統合可能

#### Negative
- 非技術者にとっては使いにくい
- Webインターフェースの開発が遅れる

---

## ADR-007: エラーハンドリング戦略

### Status
**Accepted**

### Context
COBOL変換では様々なエラーが発生する：
1. 構文エラー（パース失敗）
2. 未対応構文（変換不可）
3. LLM接続エラー
4. 意味的エラー（型不一致等）

### Decision
**Graceful Degradation（優雅な機能縮退）戦略を採用する**

```typescript
enum ErrorSeverity {
  FATAL,      // 変換中止
  ERROR,      // 部分的に失敗、続行
  WARNING,    // 警告のみ、続行
  INFO        // 情報のみ
}
```

- 構文エラー → FATAL（変換中止）
- 未対応構文 → WARNING + TODO コメント生成
- LLM接続エラー → WARNING + ルールベースにフォールバック
- 軽微な問題 → INFO + 続行

### Consequences
#### Positive
- 部分的な変換結果を得られる
- エラー発生時も作業を継続可能
- エラーレポートが詳細

#### Negative
- 不完全な変換結果が生成される可能性
- ユーザーが警告を見落とす可能性

---

## ADR-008: Java 17 LTS をターゲットバージョンとする

### Status
**Accepted**

### Context
生成するJavaコードのターゲットバージョンを決定する必要がある。選択肢：
- Java 11 LTS（古いが広く使用）
- Java 17 LTS（現在の推奨LTS）
- Java 21 LTS（最新LTS）

### Decision
**Java 17 LTSをデフォルトターゲットとする**（オプションで11/21も対応）

理由：
- 2024年時点で最も広く採用されているLTS
- Records、Sealed Classesなどの近代的機能が使用可能
- エンタープライズ環境での採用が進んでいる

### Consequences
#### Positive
- モダンJava機能を活用可能
- 多くの企業環境で実行可能
- 十分なサポート期間（2029年まで）

#### Negative
- Java 11環境では一部機能が使えない
- Java 21の最新機能は使えない

---

## 決定ログサマリー

| ADR | 決定内容 | 日付 |
|-----|----------|------|
| ADR-001 | TypeScript + Rust ハイブリッド | 2025-12-11 |
| ADR-002 | pnpm Workspaces Monorepo | 2025-12-11 |
| ADR-003 | LLMプロバイダー抽象化 | 2025-12-11 |
| ADR-004 | COBOLEvalベンチマーク採用 | 2025-12-11 |
| ADR-005 | Google Java Style採用 | 2025-12-11 |
| ADR-006 | CLIファーストアプローチ | 2025-12-11 |
| ADR-007 | Graceful Degradationエラー戦略 | 2025-12-11 |
| ADR-008 | Java 17 LTSターゲット | 2025-12-11 |

---

*このドキュメントはMUSUBI SDD Article VI (Project Memory) に準拠しています*
