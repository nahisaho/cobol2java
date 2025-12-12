# COBOL2Java API リファレンス

## Core パッケージ

### convert()

COBOL ソースコードを Java に変換するメイン関数。

```typescript
function convert(source: string, options?: ConversionOptions): ConversionResult
```

#### パラメータ

| 名前 | 型 | 説明 |
|------|-----|------|
| source | string | COBOL ソースコード |
| options | ConversionOptions | 変換オプション（オプショナル） |

#### ConversionOptions

```typescript
interface ConversionOptions {
  className?: string;          // 生成クラス名
  packageName?: string;        // パッケージ名
  springBoot?: boolean;        // Spring Boot形式
  generateRepository?: boolean; // JPA Repository生成
  generateValidation?: boolean; // Bean Validation追加
  llmProvider?: LLMProvider;   // LLMプロバイダー
}
```

#### ConversionResult

```typescript
interface ConversionResult {
  javaCode: string;            // 生成されたJavaコード
  ast: CobolAst;              // 解析されたAST
  errors: ErrorInfo[];        // エラー情報
  warnings: ErrorInfo[];      // 警告情報
}
```

---

### CobolParser

COBOL ソースコードをパースして AST を生成。

```typescript
class CobolParser {
  parse(source: string): CobolAst
}
```

#### 使用例

```typescript
import { CobolParser } from '@cobol2java/core';

const parser = new CobolParser();
const ast = parser.parse(cobolSource);

console.log(`プログラムID: ${ast.programId}`);
console.log(`データ項目数: ${ast.dataItems.length}`);
console.log(`段落数: ${ast.paragraphs.length}`);
```

#### CobolAst

```typescript
interface CobolAst {
  programId?: string;
  dataItems: DataItem[];
  paragraphs: Paragraph[];
  fileDefinitions: FileDefinition[];
  copyStatements: CopyStatement[];
  errors: ErrorInfo[];
}
```

---

### JavaGenerator

AST から Java コードを生成。

```typescript
class JavaGenerator {
  constructor(ast: CobolAst, options?: GeneratorOptions)
  generate(): string
}
```

#### GeneratorOptions

```typescript
interface GeneratorOptions {
  className?: string;
  packageName?: string;
  springBoot?: boolean;
  generateBatchTasklet?: boolean;
  generateRepository?: boolean;
  generateValidation?: boolean;
}
```

---

### detectDialect()

COBOL ソースコードから方言を検出。

```typescript
function detectDialect(source: string): DialectInfo
```

#### DialectInfo

```typescript
interface DialectInfo {
  dialect: CobolDialect;    // 'ibm' | 'microfocus' | 'gnucobol' | 'cobol85'
  confidence: number;       // 0-100
  markers: string[];        // 検出されたマーカー
  features: string[];       // 使用されている機能
}
```

---

## LLM クライアント

### createLLMClient()

LLM クライアントを作成。

```typescript
function createLLMClient(provider: LLMProvider): LLMClient
```

#### LLMProvider

```typescript
type LLMProvider = 'copilot' | 'openai' | 'claude' | 'ollama' | 'mock';
```

#### LLMClient インターフェース

```typescript
interface LLMClient {
  readonly provider: string;
  complete(prompt: string, options?: CompletionOptions): Promise<string>;
  isAvailable(): Promise<boolean>;
}
```

### OpenAI クライアント

```typescript
import { OpenAIClient } from '@cobol2java/core';

// 基本的な使用
const client = new OpenAIClient('your-api-key');

// 設定付き
const client = new OpenAIClient({
  apiKey: 'your-api-key',
  model: 'gpt-4o',
  maxRetries: 3,
  retryDelayMs: 1000,
});

// Azure OpenAI
const azureClient = new OpenAIClient({
  apiKey: 'your-azure-key',
  baseUrl: 'https://your-resource.openai.azure.com',
  azureDeployment: 'gpt-4o',
  apiVersion: '2024-02-15-preview',
});
```

### Claude クライアント

```typescript
import { ClaudeClient } from '@cobol2java/core';

const client = new ClaudeClient({
  apiKey: 'your-anthropic-key',
  model: 'claude-3-5-sonnet-20241022',
  maxRetries: 3,
});
```

### Ollama クライアント

```typescript
import { OllamaClient } from '@cobol2java/core';

const client = new OllamaClient({
  baseUrl: 'http://localhost:11434',
  model: 'llama3.2',
});

// ストリーミング
for await (const chunk of client.streamComplete(prompt)) {
  process.stdout.write(chunk);
}
```

---

## エラー処理

### ErrorInfo

```typescript
interface ErrorInfo {
  code: string;
  message: string;
  severity: ErrorSeverity;
  line?: number;
  column?: number;
  source?: string;
}

type ErrorSeverity = 'error' | 'warning' | 'info';
```

### エラーコード

| コード | 説明 |
|--------|------|
| PARSE_ERROR | パースエラー |
| UNKNOWN_STATEMENT | 未知のステートメント |
| INVALID_PIC | 無効なPIC句 |
| GENERATION_ERROR | コード生成エラー |
| LLM_ERROR | LLM API エラー |

---

## Transform モジュール

### TypeMapper

COBOL 型を Java 型にマッピング。

```typescript
import { TypeMapper } from '@cobol2java/core';

const javaType = TypeMapper.mapPicToJava('9(5)V99');
// => 'BigDecimal'
```

### StatementTransformer

COBOL ステートメントを Java に変換。

```typescript
import { StatementTransformer } from '@cobol2java/core';

const transformer = new StatementTransformer();
const javaCode = transformer.transform(cobolStatement);
```
