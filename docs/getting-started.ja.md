# COBOL2Java クイックスタートガイド

このガイドでは、COBOL2Java を使用して COBOL プログラムを Java に変換する方法を説明します。

## インストール

```bash
# リポジトリをクローン
git clone https://github.com/nahisaho/cobol2java.git
cd cobol2java

# 依存関係をインストール
pnpm install

# ビルド
pnpm build
```

## 基本的な使い方

### CLI での変換

```bash
# 単一ファイルの変換
pnpm cobol2java convert input.cbl -o Output.java

# Spring Boot プロジェクトとして出力
pnpm cobol2java convert input.cbl -o Output.java --spring-boot

# ディレクトリ内の全ファイルを変換
pnpm cobol2java convert ./cobol-sources -o ./java-output
```

### プログラムでの使用

```typescript
import { convert } from 'cobol2java-core';

const cobolSource = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.
`;

const result = convert(cobolSource);
console.log(result.javaCode);
```

## 変換オプション

| オプション | 説明 |
|-----------|------|
| `className` | 生成するJavaクラス名 |
| `packageName` | Javaパッケージ名 |
| `springBoot` | Spring Boot形式で生成 |
| `generateRepository` | JPA Repository生成 |
| `generateValidation` | Bean Validation追加 |

### Spring Boot プロジェクトの生成

```typescript
const result = convert(cobolSource, {
  className: 'CustomerService',
  packageName: 'com.example.customer',
  springBoot: true,
  generateRepository: true,
  generateValidation: true,
});
```

## 対応している COBOL 機能

### データ型マッピング

| COBOL | Java |
|-------|------|
| PIC X(n) | String |
| PIC 9(n) | int/long |
| PIC 9(n)V9(m) | BigDecimal |
| COMP-3 | BigDecimal |
| COMP/BINARY | int/long |

### ステートメント

- MOVE → 代入
- COMPUTE → 算術演算
- IF/EVALUATE → if/switch
- PERFORM → メソッド呼び出し/ループ
- DISPLAY → System.out.println
- ACCEPT → Scanner入力
- READ/WRITE → ファイル操作

## COBOL 方言対応

COBOL2Java は複数の COBOL 方言を自動検出し、適切に変換します：

- **IBM Enterprise COBOL**: EXEC SQL, EXEC CICS, JSON操作
- **Micro Focus COBOL**: SCREEN SECTION, $SET
- **GnuCOBOL**: ALLOCATE/FREE, TRY/CATCH
- **COBOL-85**: 標準 ANSI COBOL

```typescript
import { detectDialect } from 'cobol2java-core';

const dialectInfo = detectDialect(cobolSource);
console.log(`検出された方言: ${dialectInfo.dialect}`);
console.log(`信頼度: ${dialectInfo.confidence}%`);
```

## 次のステップ

- [API リファレンス](./api-reference.ja.md)
- [サンプルコード](./examples/README.md)
- [FAQ](./faq.ja.md)
