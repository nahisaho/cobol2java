# サンプル03: パフォーマンス最適化

このサンプルでは、キャッシュとプロファイリングを使用した高性能変換を示します。

## シナリオ

大量のCOBOLファイルを一括変換する際に、キャッシュとパフォーマンス計測を活用します。

## 使用するモジュール

```typescript
import {
  convert,
  Cache,
  AstCache,
  CodeCache,
  Profiler,
  ThroughputCalculator,
  ChunkedProcessor,
  measureTimeAsync,
} from 'cobol2java-core';
import { readdir, readFile, writeFile } from 'fs/promises';
import { join } from 'path';
```

## キャッシュ付き一括変換

```typescript
// キャッシュ設定
const astCache = new AstCache({ 
  maxEntries: 1000,
  ttlMs: 600000  // 10分
});

const codeCache = new CodeCache({
  maxEntries: 1000,
  ttlMs: 600000
});

// プロファイラー設定
const profiler = new Profiler();
const throughput = new ThroughputCalculator(5000);

async function batchConvert(inputDir: string, outputDir: string): Promise<void> {
  profiler.start('batch-convert');
  
  // ファイル一覧取得
  profiler.start('list-files');
  const files = (await readdir(inputDir))
    .filter(f => f.endsWith('.cbl') || f.endsWith('.cob'));
  profiler.end('list-files');
  
  console.log(`${files.length}ファイルを変換します`);
  
  // チャンク処理で変換
  const processor = new ChunkedProcessor<string>({
    chunkSize: 10,
    onChunk: async (chunk) => {
      await Promise.all(chunk.map(async (file) => {
        await convertFile(inputDir, outputDir, file);
        throughput.record(1);
      }));
      console.log(`スループット: ${throughput.getThroughput().toFixed(1)}ファイル/秒`);
    },
  });
  
  for (const file of files) {
    await processor.add(file);
  }
  await processor.flush();
  
  profiler.end('batch-convert');
  
  // レポート出力
  console.log('\n=== パフォーマンスレポート ===');
  console.log(profiler.getReport());
  console.log('\n=== キャッシュ統計 ===');
  console.log('ASTキャッシュ:', astCache.getStats());
  console.log('コードキャッシュ:', codeCache.getStats());
}

async function convertFile(
  inputDir: string, 
  outputDir: string, 
  filename: string
): Promise<void> {
  const inputPath = join(inputDir, filename);
  const source = await readFile(inputPath, 'utf8');
  
  // ソースハッシュでキャッシュキー生成
  const cacheKey = hashSource(source);
  
  // キャッシュチェック
  const cached = codeCache.get(cacheKey);
  if (cached) {
    console.log(`[キャッシュヒット] ${filename}`);
    await writeOutput(outputDir, filename, cached.code);
    return;
  }
  
  // 変換実行
  profiler.start(`convert-${filename}`);
  const [result, duration] = await measureTimeAsync(async () => {
    return convert(source, {
      className: toClassName(filename),
      packageName: 'com.example.converted',
    });
  });
  profiler.end(`convert-${filename}`);
  
  console.log(`[変換完了] ${filename} (${duration.toFixed(0)}ms)`);
  
  // キャッシュ保存
  astCache.set(cacheKey, result.ast);
  codeCache.set(cacheKey, result.ast, result.javaCode);
  
  // 出力
  await writeOutput(outputDir, filename, result.javaCode);
}

function hashSource(source: string): string {
  // 簡易ハッシュ（本番ではcryptoを使用）
  let hash = 0;
  for (let i = 0; i < source.length; i++) {
    hash = ((hash << 5) - hash) + source.charCodeAt(i);
    hash |= 0;
  }
  return hash.toString(16);
}

function toClassName(filename: string): string {
  return filename
    .replace(/\.(cbl|cob)$/i, '')
    .split(/[-_]/)
    .map(part => part.charAt(0).toUpperCase() + part.slice(1).toLowerCase())
    .join('');
}

async function writeOutput(
  outputDir: string, 
  filename: string, 
  javaCode: string
): Promise<void> {
  const javaFilename = filename.replace(/\.(cbl|cob)$/i, '.java');
  await writeFile(join(outputDir, javaFilename), javaCode);
}

// 実行
batchConvert('./cobol-sources', './java-output')
  .then(() => console.log('変換完了'))
  .catch(console.error);
```

## 出力例

```
150ファイルを変換します
[変換完了] CUSTOMER.cbl (45ms)
[変換完了] ORDER.cbl (52ms)
[変換完了] PRODUCT.cbl (38ms)
スループット: 21.3ファイル/秒
[キャッシュヒット] CUSTOMER.cbl
[変換完了] INVENTORY.cbl (41ms)
...

=== パフォーマンスレポート ===
┌──────────────────┬────────┬────────┬────────┬───────┐
│ Operation        │ Calls  │ Total  │ Avg    │ Max   │
├──────────────────┼────────┼────────┼────────┼───────┤
│ batch-convert    │ 1      │ 7234ms │ 7234ms │ 7234ms│
│ list-files       │ 1      │ 12ms   │ 12ms   │ 12ms  │
│ convert-*        │ 150    │ 6892ms │ 46ms   │ 125ms │
└──────────────────┴────────┴────────┴────────┴───────┘

=== キャッシュ統計 ===
ASTキャッシュ: { totalEntries: 148, hits: 45, misses: 150, hitRate: 0.23 }
コードキャッシュ: { totalEntries: 148, hits: 45, misses: 150, hitRate: 0.23 }
```

## ポイント

1. **AstCache/CodeCache**: 重複変換を回避
2. **ChunkedProcessor**: メモリ効率の良いバッチ処理
3. **Profiler**: 詳細な処理時間の計測
4. **ThroughputCalculator**: リアルタイムのスループット表示
5. **measureTimeAsync**: 個別処理の計測
