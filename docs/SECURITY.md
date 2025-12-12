# Security Guide

COBOL2Java プロジェクトのセキュリティガイドです。

## セキュリティモジュール

`cobol2java-core` には包括的なセキュリティ機能が含まれています。

### 入力バリデーション

```typescript
import { validateInput, validateIdentifier, validateFilePath } from 'cobol2java-core';

// COBOL ソース入力のバリデーション
const result = validateInput(cobolSource, {
  limits: { maxInputSize: 5 * 1024 * 1024 }, // 5MB 制限
  strictCobol: true, // 厳密な COBOL 検証
  checkDangerousPatterns: true, // 危険パターン検出
});

if (!result.valid) {
  console.error('Validation errors:', result.errors);
}

// 識別子のバリデーション
const idResult = validateIdentifier('CUSTOMER-NAME');

// ファイルパスのバリデーション
const pathResult = validateFilePath(userProvidedPath);
```

### サニタイゼーション

```typescript
import { 
  sanitizeInput,
  sanitizeForJavaString,
  sanitizeForJavaIdentifier,
  sanitizeForLog,
  redactSensitive
} from 'cobol2java-core';

// 入力のサニタイズ
const cleanInput = sanitizeInput(rawInput, {
  removeNullBytes: true,
  normalizeLineEndings: true,
  removeControlChars: true,
  removeBom: true,
});

// Java 文字列リテラル用エスケープ
const javaString = sanitizeForJavaString(userInput);

// Java 識別子への変換
const javaId = sanitizeForJavaIdentifier('CUSTOMER-NAME'); // "CUSTOMER_NAME"

// ログ出力用サニタイズ（ログインジェクション防止）
const logSafe = sanitizeForLog(userMessage);

// 機密情報のリダクト
const safeOutput = redactSensitive(debugOutput);
```

### セキュリティリミット

```typescript
import { checkLimits, DEFAULT_LIMITS, STRICT_LIMITS, withTimeout } from 'cobol2java-core';

// デフォルトリミットの確認
const limitCheck = checkLimits(input);
if (!limitCheck.valid) {
  console.error('Limit violations:', limitCheck.violations);
}

// 厳格なリミット（信頼できない入力用）
const strictCheck = checkLimits(untrustedInput, STRICT_LIMITS);

// タイムアウト付き処理
const result = await withTimeout(
  convert(source, options),
  30000, // 30秒
  'conversion'
);
```

## デフォルトセキュリティ設定

| 設定 | デフォルト値 | 厳格モード |
|------|-------------|-----------|
| 最大入力サイズ | 10 MB | 1 MB |
| 最大行数 | 100,000 | 10,000 |
| 最大行長 | 500 文字 | 200 文字 |
| 最大ネスト深度 | 50 | 20 |
| パースタイムアウト | 30 秒 | 10 秒 |
| 生成タイムアウト | 60 秒 | 30 秒 |

## 検出される危険パターン

- **スクリプトインジェクション**: `<script>` タグの検出
- **SQL インジェクション**: SQL キーワードパターン
- **コマンドインジェクション**: バッククォート、$() 構文
- **パストラバーサル**: `../` シーケンス
- **Null バイト**: `\x00` 文字
- **Unicode 方向オーバーライド**: RTL/LTR 制御文字

## Web アプリケーション向けセキュリティ

### CSP (Content Security Policy)

```html
<meta http-equiv="Content-Security-Policy" 
      content="default-src 'self'; script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline';">
```

### CORS 設定

API サーバーでは適切な CORS ヘッダーを設定してください：

```typescript
// Express.js の例
app.use(cors({
  origin: ['https://your-domain.com'],
  methods: ['GET', 'POST'],
  allowedHeaders: ['Content-Type'],
}));
```

## 推奨プラクティス

### 1. 入力の検証

すべてのユーザー入力を処理前に検証します：

```typescript
const validation = validateInput(userInput, {
  limits: STRICT_LIMITS,
  checkDangerousPatterns: true,
});

if (!validation.valid) {
  throw new Error('Invalid input');
}
```

### 2. 出力のエスケープ

生成されたコードを表示する際は適切にエスケープします：

```typescript
const safeHtml = sanitizeInput(generatedCode, { escapeHtml: true });
```

### 3. ログの保護

ログ出力前にサニタイズします：

```typescript
logger.info(`Processing: ${sanitizeForLog(fileName)}`);
```

### 4. 機密情報の保護

デバッグ出力から機密情報を除去します：

```typescript
const safeDebug = redactSensitive(JSON.stringify(config));
```

## 脆弱性報告

セキュリティ上の問題を発見した場合は、公開イシューではなく、直接メールにてご連絡ください。

## 監査ログ

本番環境では以下のイベントをログに記録することを推奨します：

- 変換リクエスト（タイムスタンプ、入力サイズ）
- バリデーション失敗（エラーコード、入力の概要）
- セキュリティ警告（検出されたパターン）
- タイムアウト発生

```typescript
import { sanitizeForLog, redactSensitive } from 'cobol2java-core';

function logConversion(input: string, result: ConversionResult) {
  const logEntry = {
    timestamp: new Date().toISOString(),
    inputSize: input.length,
    success: result.errors.length === 0,
    duration: result.metadata.durationMs,
    warnings: result.warnings.map(w => sanitizeForLog(w.message)),
  };
  
  logger.info(redactSensitive(JSON.stringify(logEntry)));
}
```
