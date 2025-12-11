# COBOL2Java アーキテクチャ

**Version**: 1.2.0  
**Last Updated**: 2025-12-12

---

## 1. アーキテクチャ概要

### 1.1 システム全体図

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           COBOL2Java System                              │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐                  │
│  │    CLI      │    │    Web      │    │    API      │   Presentation   │
│  │  Package    │    │  Package    │    │  (Future)   │   Layer          │
│  └──────┬──────┘    └──────┬──────┘    └──────┬──────┘                  │
│         │                  │                  │                          │
│         └──────────────────┼──────────────────┘                          │
│                            │                                             │
│                            ▼                                             │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │                       Core Package                               │    │
│  │  ┌───────────┐  ┌────────────────┐  ┌───────────────┐           │    │
│  │  │  Parser   │→ │ Transformation │→ │   Generator   │           │    │
│  │  │           │  │    Engine      │  │               │           │    │
│  │  └───────────┘  └───────┬────────┘  └───────────────┘           │    │
│  │                         │                                        │    │
│  │                         ▼                                        │    │
│  │               ┌─────────────────────┐                           │    │
│  │               │    LLM Integration  │                           │    │
│  │               │  ┌─────┐ ┌───────┐  │                           │    │
│  │               │  │OpenAI│ │Claude │  │                           │    │
│  │               │  └─────┘ └───────┘  │                           │    │
│  │               │  ┌──────┐ ┌───────┐ │                           │    │
│  │               │  │Ollama│ │Copilot│ │                           │    │
│  │               │  └──────┘ └───────┘ │                           │    │
│  │               └─────────────────────┘                           │    │
│  └─────────────────────────────────────────────────────────────────┘    │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### 1.2 技術スタック

| レイヤー | 技術 | 理由 |
|---------|------|------|
| 言語 | TypeScript 5.x | 型安全性、エコシステム |
| ランタイム | Node.js 20 LTS | 安定性、長期サポート |
| パッケージ管理 | pnpm workspaces | 高速、ディスク効率 |
| テスト | Vitest | 高速、ESM対応 |
| リンター | ESLint + Biome | コード品質 |

---

## 2. モノレポ構成

### 2.1 パッケージ構成

```
COBOL2Java/
├── packages/
│   ├── core/              # コア変換ライブラリ
│   │   ├── src/
│   │   │   ├── index.ts         # パブリックAPI
│   │   │   ├── converter.ts     # 変換オーケストレーター
│   │   │   ├── parser.ts        # COBOL AST パーサー
│   │   │   ├── generator.ts     # Java コード生成器
│   │   │   ├── errors.ts        # エラー処理
│   │   │   ├── transform/       # 変換ルール
│   │   │   │   ├── index.ts
│   │   │   │   └── rules.ts     # 130+ パターン
│   │   │   └── llm/             # LLM統合
│   │   │       ├── index.ts
│   │   │       └── copilot.ts   # GitHub Copilot
│   │   └── __tests__/           # テスト (128 tests)
│   │
│   ├── cli/               # CLIツール (将来)
│   │   └── src/
│   │       └── index.ts
│   │
│   └── web/               # Webインターフェース (将来)
│       └── src/
│           └── index.ts
│
├── steering/              # プロジェクトメモリ
│   ├── product.ja.md      # プロダクトコンテキスト
│   ├── structure.ja.md    # プロジェクト構造
│   ├── tech.ja.md         # 技術スタック
│   └── rules/             # ガバナンスルール
│
├── storage/               # SDD成果物
│   ├── specs/             # 仕様書
│   ├── changes/           # 変更履歴
│   └── features/          # 機能仕様
│
├── docs/                  # ドキュメント
│   ├── concept.md         # コンセプト
│   ├── architecture.md    # アーキテクチャ（本ドキュメント）
│   └── getting-started.md # クイックスタート
│
├── pnpm-workspace.yaml    # ワークスペース設定
├── tsconfig.json          # TypeScript設定
└── vitest.config.ts       # テスト設定
```

### 2.2 パッケージ依存関係

```
┌───────────┐     ┌───────────┐
│    CLI    │────▶│           │
└───────────┘     │           │
                  │   Core    │
┌───────────┐     │           │
│    Web    │────▶│           │
└───────────┘     └───────────┘
                       │
                       ▼
               ┌───────────────┐
               │ External LLMs │
               │ (OpenAI,      │
               │  Claude,      │
               │  Ollama)      │
               └───────────────┘
```

---

## 3. Core パッケージ詳細

### 3.1 処理フロー

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         変換パイプライン                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│   COBOL Source                                                           │
│       │                                                                  │
│       ▼                                                                  │
│   ┌─────────────────────────────────────────────────────────────┐       │
│   │                        Parser                                │       │
│   │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐        │       │
│   │  │  IDENT  │  │  ENV    │  │  DATA   │  │  PROC   │        │       │
│   │  │ DIVISION│  │ DIVISION│  │ DIVISION│  │ DIVISION│        │       │
│   │  └─────────┘  └─────────┘  └─────────┘  └─────────┘        │       │
│   └───────────────────────────────┬─────────────────────────────┘       │
│                                   │                                      │
│                                   ▼                                      │
│                              CobolAst                                    │
│   ┌──────────────────────────────────────────────────────────────┐      │
│   │  {                                                            │      │
│   │    programName: "HELLO",                                      │      │
│   │    dataItems: [...],                                          │      │
│   │    fileDefinitions: [...],                                    │      │
│   │    paragraphs: [...],                                         │      │
│   │    conditionNames: [...]                                      │      │
│   │  }                                                            │      │
│   └───────────────────────────────┬──────────────────────────────┘      │
│                                   │                                      │
│                                   ▼                                      │
│   ┌─────────────────────────────────────────────────────────────┐       │
│   │                   Transformation Engine                      │       │
│   │                                                              │       │
│   │  ┌────────────────────────────────────────────────────────┐ │       │
│   │  │              Rule-Based Transform                       │ │       │
│   │  │  • DATA_TYPE_MAPPINGS (15+ types)                       │ │       │
│   │  │  • STATEMENT_RULES (130+ patterns)                      │ │       │
│   │  │  • INTRINSIC_FUNCTIONS (60+ functions)                  │ │       │
│   │  └────────────────────────────────────────────────────────┘ │       │
│   │                           │                                  │       │
│   │                           ▼                                  │       │
│   │  ┌────────────────────────────────────────────────────────┐ │       │
│   │  │              LLM-Assisted Transform (Optional)          │ │       │
│   │  │  • Complex business logic understanding                 │ │       │
│   │  │  • Code optimization suggestions                        │ │       │
│   │  │  • Pattern recognition                                  │ │       │
│   │  └────────────────────────────────────────────────────────┘ │       │
│   └───────────────────────────────┬─────────────────────────────┘       │
│                                   │                                      │
│                                   ▼                                      │
│   ┌─────────────────────────────────────────────────────────────┐       │
│   │                       Generator                              │       │
│   │  • Java class structure                                      │       │
│   │  • Import statements                                         │       │
│   │  • Field declarations                                        │       │
│   │  • Method generation                                         │       │
│   │  • File handlers (if needed)                                 │       │
│   └───────────────────────────────┬─────────────────────────────┘       │
│                                   │                                      │
│                                   ▼                                      │
│                            Java Source                                   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### 3.2 主要コンポーネント

#### Parser (`parser.ts`)

COBOLソースコードを抽象構文木（AST）に変換します。

```typescript
// 主要インターフェース
interface CobolAst {
  programName?: string;
  dataItems: DataItem[];
  fileDefinitions: FileDefinition[];
  paragraphs: Paragraph[];
  conditionNames: ConditionName[];
  errors: ErrorInfo[];
}

interface DataItem {
  level: number;        // 01-88
  name: string;         // 変数名
  pic?: string;         // PIC句
  value?: string;       // VALUE句
  occurs?: number;      // OCCURS句
  usage?: string;       // USAGE句
  redefines?: string;   // REDEFINES句
  indexed?: string[];   // INDEXED BY句
}

interface FileDefinition {
  selectName: string;   // SELECT file-name
  assignTo: string;     // ASSIGN TO target
  organization?: string;
  accessMode?: string;
  recordKey?: string;
  fileStatus?: string;
}
```

#### Transformation Engine (`transform/rules.ts`)

COBOLステートメントをJavaコードに変換するルールエンジンです。

```typescript
// ステートメント変換ルール（130+）
const STATEMENT_RULES = [
  // エラーハンドラー（優先度高）
  { pattern: /NOT INVALID KEY/, transform: '// Success branch' },
  { pattern: /INVALID KEY/, transform: '// Error handler' },
  
  // 基本ステートメント
  { pattern: /MOVE (.+) TO (.+)/, transform: '$2 = $1' },
  { pattern: /COMPUTE (.+) = (.+)/, transform: '$1 = $2' },
  { pattern: /DISPLAY (.+)/, transform: 'System.out.println($1)' },
  // ...
];

// 組込み関数（60+）
const INTRINSIC_FUNCTIONS = {
  'LENGTH': 'String.valueOf($1).length()',
  'TRIM': '$1.trim()',
  'UPPER-CASE': '$1.toUpperCase()',
  'CURRENT-DATE': 'LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))',
  // ...
};
```

#### Generator (`generator.ts`)

ASTと変換結果からJavaソースコードを生成します。

```typescript
class JavaGenerator {
  generate(ast: CobolAst, transformedStatements: string[]): string {
    // 1. パッケージ宣言
    // 2. インポート文
    // 3. クラス宣言
    // 4. フィールド（DataItems → Java fields）
    // 5. メソッド（Paragraphs → Java methods）
    // 6. ファイルハンドラー（FileDefinitions → I/O）
  }
}
```

---

## 4. LLM統合アーキテクチャ

### 4.1 LLMプロバイダー抽象化

```
┌─────────────────────────────────────────────────────────────────┐
│                     LLM Client Interface                         │
├─────────────────────────────────────────────────────────────────┤
│  interface LLMClient {                                           │
│    complete(prompt: string): Promise<string>;                    │
│    analyze(code: string): Promise<AnalysisResult>;               │
│  }                                                               │
└───────────────────────────────────────────────────────────────── ┘
           │              │              │              │
           ▼              ▼              ▼              ▼
    ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐
    │  OpenAI  │  │  Claude  │  │  Ollama  │  │  Copilot │
    │  Client  │  │  Client  │  │  Client  │  │  Client  │
    └──────────┘  └──────────┘  └──────────┘  └──────────┘
         │              │              │              │
         ▼              ▼              ▼              ▼
    ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐
    │  GPT-4o  │  │  Claude  │  │  Llama   │  │  GitHub  │
    │  GPT-4o- │  │  3.5     │  │  3.2     │  │  Copilot │
    │  mini    │  │  Sonnet  │  │  CodeLlama│  │  API     │
    └──────────┘  └──────────┘  └──────────┘  └──────────┘
```

### 4.2 LLMの活用シーン

| シーン | 用途 | 使用LLM |
|-------|------|---------|
| 複雑なビジネスロジック | 意図理解、最適化 | GPT-4o, Claude |
| コメント生成 | Javadoc生成 | GPT-4o-mini |
| パターン認識 | 未知構文の推論 | Claude |
| ローカル処理 | オフライン変換 | Ollama |

---

## 5. データフロー

### 5.1 変換データフロー

```
┌────────────────────────────────────────────────────────────────┐
│                      変換データフロー                           │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Input: COBOL Source                                           │
│   ┌─────────────────────────────────────────────────────────┐  │
│   │  IDENTIFICATION DIVISION.                                │  │
│   │  PROGRAM-ID. HELLO.                                      │  │
│   │  DATA DIVISION.                                          │  │
│   │  WORKING-STORAGE SECTION.                                │  │
│   │  01 WS-NAME PIC X(20).                                   │  │
│   │  PROCEDURE DIVISION.                                     │  │
│   │      MOVE "World" TO WS-NAME.                            │  │
│   │      DISPLAY "Hello, " WS-NAME.                          │  │
│   │      STOP RUN.                                           │  │
│   └─────────────────────────────────────────────────────────┘  │
│                              │                                  │
│                              ▼                                  │
│   AST (Abstract Syntax Tree)                                    │
│   ┌─────────────────────────────────────────────────────────┐  │
│   │  {                                                       │  │
│   │    programName: "HELLO",                                 │  │
│   │    dataItems: [                                          │  │
│   │      { level: 1, name: "WS-NAME", pic: "X(20)" }         │  │
│   │    ],                                                    │  │
│   │    paragraphs: [                                         │  │
│   │      { name: "MAIN", statements: [...] }                 │  │
│   │    ]                                                     │  │
│   │  }                                                       │  │
│   └─────────────────────────────────────────────────────────┘  │
│                              │                                  │
│                              ▼                                  │
│   Transformed Statements                                        │
│   ┌─────────────────────────────────────────────────────────┐  │
│   │  [                                                       │  │
│   │    "wsName = \"World\";",                                │  │
│   │    "System.out.println(\"Hello, \" + wsName);",          │  │
│   │    "System.exit(0);"                                     │  │
│   │  ]                                                       │  │
│   └─────────────────────────────────────────────────────────┘  │
│                              │                                  │
│                              ▼                                  │
│   Output: Java Source                                           │
│   ┌─────────────────────────────────────────────────────────┐  │
│   │  package com.example;                                    │  │
│   │                                                          │  │
│   │  public class Hello {                                    │  │
│   │      private String wsName = "";                         │  │
│   │                                                          │  │
│   │      public void main() {                                │  │
│   │          wsName = "World";                               │  │
│   │          System.out.println("Hello, " + wsName);         │  │
│   │          System.exit(0);                                 │  │
│   │      }                                                   │  │
│   │  }                                                       │  │
│   └─────────────────────────────────────────────────────────┘  │
│                                                                 │
└────────────────────────────────────────────────────────────────┘
```

---

## 6. エラー処理アーキテクチャ

### 6.1 エラー分類

```
┌─────────────────────────────────────────────────────────────┐
│                      エラー分類                              │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────────────────────────────────────────────┐   │
│  │              ErrorSeverity                            │   │
│  ├──────────────────────────────────────────────────────┤   │
│  │  ERROR    │ 変換不可能な致命的エラー                 │   │
│  │  WARNING  │ 変換可能だが注意が必要                   │   │
│  │  INFO     │ 情報メッセージ                           │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐   │
│  │              ErrorCode                                │   │
│  ├──────────────────────────────────────────────────────┤   │
│  │  PARSE_ERROR          │ 構文解析エラー               │   │
│  │  UNSUPPORTED_SYNTAX   │ 未対応構文                   │   │
│  │  TYPE_MISMATCH        │ 型不一致                     │   │
│  │  LLM_ERROR            │ LLM呼び出しエラー            │   │
│  │  GENERATION_ERROR     │ コード生成エラー             │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

### 6.2 エラー伝播

```
Parser Error → Transformation Error → Generation Error
     │                  │                    │
     ▼                  ▼                    ▼
┌─────────────────────────────────────────────────────┐
│              ConversionResult.errors                 │
│  [                                                   │
│    { code: "PARSE_ERROR", line: 10, message: "..." }│
│    { code: "UNSUPPORTED", line: 25, message: "..." }│
│  ]                                                   │
└─────────────────────────────────────────────────────┘
```

---

## 7. テストアーキテクチャ

### 7.1 テスト構成

```
┌─────────────────────────────────────────────────────────────┐
│                     テストピラミッド                         │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│                        ┌─────────┐                          │
│                       ╱           ╲                         │
│                      ╱  E2E Tests  ╲                        │
│                     ╱   COBOLEval   ╲                       │
│                    ╱    Benchmark    ╲                      │
│                   ├───────────────────┤                     │
│                  ╱                     ╲                    │
│                 ╱   Integration Tests   ╲                   │
│                ╱    (converter.test.ts)  ╲                  │
│               ├───────────────────────────┤                 │
│              ╱                             ╲                │
│             ╱        Unit Tests             ╲               │
│            ╱   transform.test.ts (101)       ╲              │
│           ╱    parser.test.ts (11)            ╲             │
│          ╱     generator.test.ts (8)           ╲            │
│         └───────────────────────────────────────┘           │
│                                                              │
│  Total: 128 tests                                           │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

### 7.2 テストファイル構成

| ファイル | テスト数 | 内容 |
|---------|---------|------|
| `transform.test.ts` | 101 | 変換ルール全般 |
| `parser.test.ts` | 11 | AST解析 |
| `generator.test.ts` | 8 | Javaコード生成 |
| `converter.test.ts` | 8 | 統合変換 |

---

## 8. デプロイメントアーキテクチャ

### 8.1 配布形態

```
┌─────────────────────────────────────────────────────────────┐
│                     配布形態                                 │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌───────────────┐                                          │
│  │     npm       │  npm install -g cobol2java               │
│  │   Package     │  - Node.js環境向け                       │
│  └───────────────┘  - 最も一般的                            │
│                                                              │
│  ┌───────────────┐                                          │
│  │   Standalone  │  単体実行可能ファイル                    │
│  │   Binary      │  - Node.js不要                           │
│  └───────────────┘  - pkg / nexe                            │
│                                                              │
│  ┌───────────────┐                                          │
│  │    Docker     │  docker run cobol2java                   │
│  │   Container   │  - コンテナ環境向け                      │
│  └───────────────┘  - CI/CDパイプライン統合                 │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

### 8.2 CI/CDパイプライン

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
      - run: pnpm benchmark  # COBOLEval
```

---

## 9. セキュリティ考慮事項

### 9.1 LLM利用時のセキュリティ

| 考慮事項 | 対策 |
|---------|------|
| APIキー管理 | 環境変数での管理、.envファイルをgitignore |
| データプライバシー | ローカルLLM（Ollama）オプション提供 |
| コード漏洩 | 機密コードはローカル変換を推奨 |

### 9.2 生成コードの検証

```
┌─────────────────────────────────────────────────────────────┐
│                   生成コード検証フロー                       │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  Generated Java Code                                         │
│         │                                                    │
│         ▼                                                    │
│  ┌───────────────┐                                          │
│  │  Syntax Check │  javac による構文検証                    │
│  └───────┬───────┘                                          │
│          │                                                   │
│          ▼                                                   │
│  ┌───────────────┐                                          │
│  │  Unit Tests   │  JUnit によるテスト                      │
│  └───────┬───────┘                                          │
│          │                                                   │
│          ▼                                                   │
│  ┌───────────────┐                                          │
│  │  Comparison   │  COBOL実行結果との比較                   │
│  └───────────────┘                                          │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

## 参考リンク

- [コンセプト](./concept.md)
- [クイックスタート](./getting-started.md)
- [GitHub Repository](https://github.com/nahisaho/cobol2java)
