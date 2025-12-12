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

---

## Security モジュール

### SecurityLimits

入力サイズと複雑さの制限を設定。

```typescript
import { SecurityLimits, checkLimits, withTimeout } from '@cobol2java/core';

// デフォルト制限
const limits: SecurityLimits = {
  maxInputSize: 1024 * 1024,       // 1MB
  maxLineLength: 10000,            // 10,000文字
  maxLines: 100000,                // 100,000行
  maxNestingDepth: 50,             // ネスト深さ50
  maxIdentifierLength: 256,        // 識別子256文字
  maxDataItems: 10000,             // データ項目10,000個
  maxParagraphs: 5000,             // 段落5,000個
  timeoutMs: 30000,                // 30秒タイムアウト
};

// 制限チェック
const result = checkLimits(cobolSource, limits);
if (!result.valid) {
  console.error('制限超過:', result.violations);
}

// タイムアウト付き実行
const javaCode = await withTimeout(
  () => convert(cobolSource),
  30000,
  'Conversion'
);
```

### Input Validation

入力値の検証。

```typescript
import { 
  validateInput, 
  validateIdentifier, 
  validateFilePath 
} from '@cobol2java/core';

// 入力検証
const inputResult = validateInput(source, { maxSize: 1024 * 1024 });
if (!inputResult.valid) {
  console.error(inputResult.error);
}

// 識別子検証
const idResult = validateIdentifier('CUSTOMER-NAME');
// => { valid: true }

// ファイルパス検証
const pathResult = validateFilePath('/safe/path/file.cbl');
// => { valid: true }
```

### Sanitization

出力のサニタイズ。

```typescript
import { 
  sanitizeInput,
  sanitizeForJavaString,
  sanitizeForJavaIdentifier,
  sanitizeForJavaPackage,
  redactSensitive
} from '@cobol2java/core';

// 制御文字除去
const clean = sanitizeInput(dirtyInput);

// Java文字列エスケープ
const escaped = sanitizeForJavaString('Hello "World"');
// => 'Hello \"World\"'

// Java識別子変換
const identifier = sanitizeForJavaIdentifier('CUSTOMER-NAME');
// => 'customerName'

// パッケージ名変換
const pkg = sanitizeForJavaPackage('com.example.my-app');
// => 'com.example.myapp'

// 機密情報マスク
const safe = redactSensitive('api_key=sk-abc123...');
// => 'api_key=[REDACTED]...'
```

---

## Performance モジュール

### Cache

汎用キャッシュ（LRU/LFU/FIFO）。

```typescript
import { Cache, AstCache, CodeCache } from '@cobol2java/core';

// 基本キャッシュ
const cache = new Cache<string>({
  maxEntries: 1000,
  ttlMs: 300000,           // 5分TTL
  evictionPolicy: 'lru',   // 'lru' | 'lfu' | 'fifo'
});

cache.set('key', 'value');
const value = cache.get('key');

// キャッシュ統計
const stats = cache.getStats();
console.log(`ヒット率: ${(stats.hitRate * 100).toFixed(1)}%`);

// AST専用キャッシュ
const astCache = new AstCache({ maxEntries: 500 });
astCache.set('program.cbl', ast);

// コード専用キャッシュ
const codeCache = new CodeCache({ maxEntries: 500 });
codeCache.set('key', ast, 'public class Program {}');
```

### Streaming

大規模ファイルのストリーミング処理。

```typescript
import { 
  ChunkedProcessor,
  LineTransform,
  StringBuilder,
  streamToString
} from '@cobol2java/core';

// チャンク処理
const processor = new ChunkedProcessor<string>({
  chunkSize: 100,
  onChunk: async (items) => {
    console.log(`処理中: ${items.length}件`);
  },
  onComplete: async () => {
    console.log('完了');
  },
});

for (const item of items) {
  await processor.add(item);
}
await processor.flush();

// 行変換ストリーム
const lineTransform = new LineTransform((line, lineNumber) => {
  return `${lineNumber}: ${line}`;
});

readStream.pipe(lineTransform).pipe(writeStream);

// 高速文字列構築
const sb = new StringBuilder();
sb.append('Hello').append(' ').append('World');
console.log(sb.toString()); // => 'Hello World'

// ストリームを文字列に変換
const content = await streamToString(readableStream);
```

### Profiling

パフォーマンス計測。

```typescript
import { 
  Timer,
  Profiler,
  globalProfiler,
  measureTime,
  ThroughputCalculator
} from '@cobol2java/core';

// 単純タイマー
const timer = new Timer();
timer.start();
// ... 処理
timer.stop();
console.log(`経過時間: ${timer.getDuration()}ms`);

// マーク付きタイマー
timer.mark('parse-start');
// ... パース処理
timer.mark('parse-end');
console.log(`パース時間: ${timer.between('parse-start', 'parse-end')}ms`);

// プロファイラー
const profiler = new Profiler();
profiler.start('convert');
// ... 変換処理
profiler.end('convert');
console.log(profiler.getReport());

// グローバルプロファイラー
globalProfiler.start('operation');
// ... 処理
globalProfiler.end('operation');

// ラッパー関数
const [result, duration] = measureTime(() => heavyComputation());
console.log(`処理時間: ${duration}ms`);

// 非同期版
const [asyncResult, asyncDuration] = await measureTimeAsync(
  async () => await asyncOperation()
);

// スループット計算
const calc = new ThroughputCalculator(5000); // 5秒ウィンドウ
calc.record(10);
await delay(100);
calc.record(10);
console.log(`スループット: ${calc.getThroughput().toFixed(1)}/秒`);
```
