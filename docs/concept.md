# COBOL2Java コンセプト

**Version**: 1.0.0  
**Last Updated**: 2025-12-13

---

## 1. ビジョン

### 1.1 ミッションステートメント

> **メインフレームのレガシーCOBOL資産を、AI支援によって高品質なJavaコードに変換し、エンタープライズシステムのモダナイゼーションを加速する。**

COBOL2Javaは、ルールベースの確実な変換とLLM（大規模言語モデル）の柔軟な理解力を組み合わせ、開発者が信頼できるCOBOL→Java変換を実現します。

### 1.2 なぜCOBOL2Javaが必要か

```
┌─────────────────────────────────────────────────────────────┐
│                    現在の課題                                │
├─────────────────────────────────────────────────────────────┤
│  👴 COBOL開発者の高齢化（平均年齢55歳以上）                  │
│  💰 手動変換は高コスト（1万行あたり数千万円）                 │
│  ⏱️ 変換期間が長い（数ヶ月〜数年）                           │
│  ⚠️ 変換ミスによる業務停止リスク                             │
│  📊 品質評価の客観基準がない                                 │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                  COBOL2Javaの解決策                          │
├─────────────────────────────────────────────────────────────┤
│  🤖 LLM + 構文解析のハイブリッド変換                         │
│  📐 240+ の変換ルールによる確実な構文変換                    │
│  ✅ COBOLEval 146問による定量的品質保証                      │
│  🔧 CI/CDパイプライン統合対応                                │
│  🆓 オープンソースで導入障壁を低減                           │
└─────────────────────────────────────────────────────────────┘
```

---

## 2. コンセプト

### 2.1 ハイブリッドアプローチ

COBOL2Javaは「ルールベース変換」と「LLM補助変換」を組み合わせたハイブリッドアプローチを採用しています。

```
┌──────────────────────────────────────────────────────────────┐
│                     COBOL ソースコード                        │
└──────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                      Parser (AST生成)                         │
│  ┌────────────────────────────────────────────────────────┐  │
│  │ • IDENTIFICATION DIVISION 解析                          │  │
│  │ • ENVIRONMENT DIVISION 解析（SELECT, FD）              │  │
│  │ • DATA DIVISION 解析（01-88レベル, OCCURS, REDEFINES）  │  │
│  │ • PROCEDURE DIVISION 解析（パラグラフ, ステートメント） │  │
│  └────────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                   Transformation Engine                       │
│  ┌─────────────────────────┐  ┌─────────────────────────┐   │
│  │   Rule-Based Transform   │  │    LLM-Assisted         │   │
│  │   (確実な構文変換)       │  │    (複雑ロジック理解)   │   │
│  │                          │  │                          │   │
│  │  • 240+ Statement Rules  │  │  • OpenAI GPT-4o        │   │
│  │  • 80+ Intrinsic Funcs   │  │  • Claude 3.5 Sonnet    │   │
│  │  • 15+ Data Types        │  │  • Ollama (ローカル)    │   │
│  │  • 20+ Error Handlers    │  │  • GitHub Copilot       │   │
│  └─────────────────────────┘  └─────────────────────────┘   │
└──────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                     Java Generator                            │
│  ┌────────────────────────────────────────────────────────┐  │
│  │ • Java 17 LTS 準拠コード生成                            │  │
│  │ • Google Java Style フォーマット                        │  │
│  │ • Maven / Gradle 両対応                                 │  │
│  │ • 必要なインポート文の自動生成                          │  │
│  └────────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                     Java ソースコード                         │
└──────────────────────────────────────────────────────────────┘
```

### 2.2 変換の原則

| 原則 | 説明 |
|------|------|
| **正確性優先** | ルールベース変換で確実に動作するコードを生成 |
| **可読性重視** | 人間が理解・保守しやすいJavaコードを生成 |
| **段階的変換** | 一度に全てを変換せず、検証しながら進める |
| **品質保証** | ベンチマークテストによる定量的な品質評価 |

### 2.3 対応COBOLダイアレクト

```
┌─────────────────────────────────────────┐
│           対応COBOLダイアレクト          │
├─────────────────────────────────────────┤
│  ✅ COBOL-85 (ANSI標準)                 │
│  ✅ Enterprise COBOL (IBM)              │
│  ✅ GnuCOBOL                            │
│  ✅ Micro Focus COBOL                   │
└─────────────────────────────────────────┘
```

---

## 3. 主要機能

### 3.1 構文変換（240+ ルール）

#### 基本ステートメント

```cobol
*> COBOL
MOVE WS-NAME TO WS-OUTPUT.
COMPUTE WS-TOTAL = WS-PRICE * WS-QUANTITY.
DISPLAY "Hello, World!".
```

```java
// Java
wsOutput = wsName;
wsTotal = wsPrice.multiply(wsQuantity);
System.out.println("Hello, World!");
```

#### 制御構造

```cobol
*> COBOL
IF WS-AGE > 18
    PERFORM ADULT-PROCESS
ELSE
    PERFORM MINOR-PROCESS
END-IF.

PERFORM LOOP-PARA UNTIL WS-COUNT > 10.
```

```java
// Java
if (wsAge > 18) {
    adultProcess();
} else {
    minorProcess();
}

while (!(wsCount > 10)) {
    loopPara();
}
```

### 3.2 組込み関数（80+ 関数）

| カテゴリ | 関数例 |
|---------|--------|
| 文字列 | LENGTH, TRIM, UPPER-CASE, LOWER-CASE, REVERSE, CONCATENATE |
| 数値 | NUMVAL, NUMVAL-C, ABS, SQRT, SIN, COS, TAN, LOG, LOG10 |
| 日付 | CURRENT-DATE, INTEGER-OF-DATE, DATE-OF-INTEGER |
| Unicode | ULENGTH, UPOS, USUBSTR, UVALID |
| 統計 | MAX, MIN, SUM, MEAN, MEDIAN, VARIANCE, STANDARD-DEVIATION |
| その他 | ORD, CHAR, MOD, REM, FACTORIAL, RANDOM |

### 3.3 データ型マッピング

| COBOL PIC | Java Type | 説明 |
|-----------|-----------|------|
| `9(n)` | `int` / `long` | 整数 |
| `S9(n)` | `int` / `long` | 符号付き整数 |
| `9(n)V9(m)` | `BigDecimal` | 小数点付き数値 |
| `X(n)` | `String` | 英数字 |
| `A(n)` | `String` | 英字 |
| `COMP` / `BINARY` | `int` / `long` | バイナリ整数 |
| `COMP-3` / `PACKED-DECIMAL` | `BigDecimal` | パック10進数 |

### 3.4 エラーハンドリング

```cobol
*> COBOL
READ INPUT-FILE
    AT END
        SET EOF-FLAG TO TRUE
    NOT AT END
        PERFORM PROCESS-RECORD
END-READ.

COMPUTE WS-RESULT = WS-A / WS-B
    ON SIZE ERROR
        DISPLAY "Division overflow"
    NOT ON SIZE ERROR
        DISPLAY "Success"
END-COMPUTE.
```

```java
// Java
try {
    record = inputFile.readRecord();
    if (record == null) {
        eofFlag = true;
    } else {
        processRecord();
    }
} catch (IOException e) {
    eofFlag = true;
}

try {
    wsResult = wsA.divide(wsB);
    System.out.println("Success");
} catch (ArithmeticException e) {
    System.out.println("Division overflow");
}
```

### 3.5 オブジェクト指向COBOL

```cobol
*> OO-COBOL
CLASS-ID. CUSTOMER-ACCOUNT INHERITS BASE-ACCOUNT.
METHOD-ID. CALCULATE-INTEREST.
  INVOKE SELF "GET-BALANCE" RETURNING WS-BALANCE.
  COMPUTE WS-INTEREST = WS-BALANCE * WS-RATE.
END METHOD.
```

```java
// Java
public class CustomerAccount extends BaseAccount {
    public void calculateInterest() {
        wsBalance = this.getBalance();
        wsInterest = wsBalance.multiply(wsRate);
    }
}
```

### 3.6 埋め込みSQL/CICS

```cobol
*> Embedded SQL
EXEC SQL
    SELECT NAME, BALANCE 
    INTO :WS-NAME, :WS-BALANCE
    FROM CUSTOMERS WHERE ID = :WS-ID
END-EXEC.

*> CICS
EXEC CICS RECEIVE MAP('MENU01') INTO(WS-DATA) END-EXEC.
```

```java
// Java (JDBC)
String sql = "SELECT NAME, BALANCE FROM CUSTOMERS WHERE ID = ?";
PreparedStatement stmt = connection.prepareStatement(sql);
stmt.setString(1, wsId);
ResultSet rs = stmt.executeQuery();

// Java (CICS Java API)
Map<String, Object> wsData = cicsTransaction.receiveMap("MENU01");
```

---

## 4. ターゲットユーザー

### 4.1 プライマリユーザー

| ペルソナ | 役割 | 主なユースケース |
|---------|------|-----------------|
| **メインフレーム開発者** | シニアCOBOL開発者 | 既存COBOLコードのJava移行 |
| **エンタープライズアーキテクト** | システム設計者 | モダナイゼーション戦略策定 |
| **DevOpsエンジニア** | CI/CD担当者 | 変換パイプラインの自動化 |

### 4.2 ユースケース

```
┌─────────────────────────────────────────────────────────────┐
│                     典型的なユースケース                      │
├─────────────────────────────────────────────────────────────┤
│  1. バッチ処理プログラムのJava移行                           │
│  2. CICS/IMS連携部分の変換                                   │
│  3. 帳票出力ロジックの変換                                   │
│  4. レガシーAPIのモダナイゼーション                          │
│  5. クラウド移行の前段階としての変換                         │
└─────────────────────────────────────────────────────────────┘
```

---

## 5. 品質保証

### 5.1 COBOLEvalベンチマーク

COBOL2Javaは、Open Mainframe Project（Linux Foundation）のZorseプロジェクトから取得した**COBOLEval 146問**をベンチマークとして使用しています。

```
┌─────────────────────────────────────────┐
│          品質保証プロセス                │
├─────────────────────────────────────────┤
│  1. ユニットテスト (128+ tests)         │
│  2. 変換ルール検証                       │
│  3. COBOLEvalベンチマーク                │
│  4. 実運用COBOLでの検証                  │
└─────────────────────────────────────────┘
```

### 5.2 現在のカバレッジ

| Division | Coverage | 対応状況 |
|----------|----------|---------|
| IDENTIFICATION | 14% | PROGRAM-ID対応 |
| ENVIRONMENT | 15% | SELECT, FD対応 |
| DATA | 50% | 主要句対応 |
| PROCEDURE | 85% | 主要ステートメント対応 |
| **Overall** | **~70%** | エンタープライズ向け拡張余地あり |

---

## 6. 今後のロードマップ

### Phase 1（完了）
- ✅ 基本構文変換エンジン
- ✅ 主要ステートメント対応
- ✅ 組込み関数対応
- ✅ テストスイート構築

### Phase 2（進行中）
- ⏳ COPY REPLACING対応
- ⏳ SPECIAL-NAMES対応
- ⏳ Report Writer対応
- ⏳ INSPECT対応

### Phase 3（計画中）
- 📋 Webインターフェース
- 📋 IDE連携（VS Code拡張）
- 📋 エンタープライズ機能

---

## 参考リンク

- [アーキテクチャ設計](./architecture.md)
- [クイックスタート](./getting-started.md)
- [GitHub Repository](https://github.com/nahisaho/cobol2java)
