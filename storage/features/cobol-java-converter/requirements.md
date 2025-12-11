# COBOL2Java コンバーター要件定義

**Feature**: cobol-java-converter  
**Version**: 1.0  
**Created**: 2025-12-11  
**Status**: Draft

---

## 概要

COBOLソースコードをJavaソースコードに変換するツール。LLMと構文解析のハイブリッドアプローチを採用し、COBOLEvalベンチマークによる品質検証を行う。

---

## ステークホルダー

| ステークホルダー | 関心事項 |
|-----------------|---------|
| メインフレーム開発者 | COBOLコードの近代化、Java移行 |
| エンタープライズアーキテクト | レガシーシステムのモダナイゼーション |
| DevOpsエンジニア | CI/CDパイプラインへの統合 |

---

## 機能要件 (EARS形式)

### FR-001: 基本変換機能

#### REQ-CVT-001: COBOL入力処理
**Type**: Ubiquitous  
**Statement**: The COBOL2Java converter SHALL accept COBOL source code as input from file path or standard input.

**Acceptance Criteria**:
- [ ] ファイルパスからCOBOLソースを読み込める
- [ ] 標準入力からCOBOLソースを読み込める
- [ ] UTF-8およびEBCDICエンコーディングをサポート
- [ ] .cbl, .cob, .cobol 拡張子をサポート

#### REQ-CVT-002: Java出力生成
**Type**: Ubiquitous  
**Statement**: The COBOL2Java converter SHALL generate syntactically valid Java source code as output.

**Acceptance Criteria**:
- [ ] 出力されたJavaコードがコンパイル可能
- [ ] Javaコーディング規約（Google Java Style）に準拠
- [ ] 適切なパッケージ構造を生成
- [ ] Javadocコメントを含む

#### REQ-CVT-003: IDENTIFICATION DIVISION変換
**Type**: Ubiquitous  
**Statement**: The COBOL2Java converter SHALL convert COBOL IDENTIFICATION DIVISION to Java class declaration.

**Acceptance Criteria**:
- [ ] PROGRAM-ID → クラス名に変換
- [ ] AUTHOR → @author Javadocタグに変換
- [ ] DATE-WRITTEN → @since Javadocタグに変換

#### REQ-CVT-004: DATA DIVISION変換
**Type**: Ubiquitous  
**Statement**: The COBOL2Java converter SHALL convert COBOL DATA DIVISION to Java field declarations.

**Acceptance Criteria**:
- [ ] PIC 9 → int/long に変換
- [ ] PIC X → String に変換
- [ ] COMP-2 → double に変換
- [ ] OCCURS ... TIMES → 配列に変換
- [ ] 01-49レベル → クラス/フィールド階層に変換

#### REQ-CVT-005: PROCEDURE DIVISION変換
**Type**: Ubiquitous  
**Statement**: The COBOL2Java converter SHALL convert COBOL PROCEDURE DIVISION to Java methods.

**Acceptance Criteria**:
- [ ] PERFORM → for/while ループに変換
- [ ] IF/ELSE → if/else に変換
- [ ] MOVE → 代入文に変換
- [ ] COMPUTE → 算術式に変換
- [ ] CALL → メソッド呼び出しに変換

---

### FR-002: イベント駆動要件

#### REQ-EVT-001: 変換エラー検出
**Type**: Event-driven  
**Statement**: WHEN a syntax error is detected in the input COBOL code, the COBOL2Java converter SHALL report the error with line number and error description.

**Acceptance Criteria**:
- [ ] エラー発生行番号を報告
- [ ] エラーの種類を分類（構文/意味/未対応）
- [ ] 修正提案を可能な限り提示
- [ ] 複数エラーをまとめて報告

#### REQ-EVT-002: 未対応構文検出
**Type**: Event-driven  
**Statement**: WHEN an unsupported COBOL construct is encountered, the COBOL2Java converter SHALL generate a TODO comment in the Java output and continue processing.

**Acceptance Criteria**:
- [ ] 未対応構文に // TODO: コメントを生成
- [ ] 元のCOBOLコードをコメントとして保持
- [ ] 未対応構文のサマリーを出力終了時に表示

#### REQ-EVT-003: ファイルI/O変換
**Type**: Event-driven  
**Statement**: WHEN COBOL file operations (OPEN, READ, WRITE, CLOSE) are encountered, the COBOL2Java converter SHALL convert them to Java NIO or java.io operations.

**Acceptance Criteria**:
- [ ] OPEN → Files.newBufferedReader/Writer
- [ ] READ → BufferedReader.readLine()
- [ ] WRITE → BufferedWriter.write()
- [ ] CLOSE → try-with-resources または close()

---

### FR-003: 状態駆動要件

#### REQ-STA-001: バッチモード
**Type**: State-driven  
**Statement**: WHILE operating in batch mode, the COBOL2Java converter SHALL process multiple files without user interaction.

**Acceptance Criteria**:
- [ ] ディレクトリ内の全COBOLファイルを処理
- [ ] 並列処理オプションをサポート
- [ ] 進捗表示（verbose モード）
- [ ] 処理結果サマリーを出力

#### REQ-STA-002: LLM補助モード
**Type**: State-driven  
**Statement**: WHILE LLM assistance is enabled, the COBOL2Java converter SHALL use LLM for complex pattern recognition and code optimization.

**Acceptance Criteria**:
- [ ] OpenAI/Claude API連携
- [ ] ローカルLLM（Ollama）連携
- [ ] LLM無しでも基本変換が動作
- [ ] LLM呼び出し回数/コストの追跡

---

### FR-004: エラーハンドリング要件

#### REQ-ERR-001: 無効入力処理
**Type**: Unwanted behavior  
**Statement**: IF the input is not valid COBOL source code, THEN the COBOL2Java converter SHALL exit with error code 1 and display a descriptive error message.

**Acceptance Criteria**:
- [ ] 終了コード 1 で終了
- [ ] エラーメッセージを stderr に出力
- [ ] 入力ファイル名をエラーメッセージに含む

#### REQ-ERR-002: LLM接続エラー
**Type**: Unwanted behavior  
**Statement**: IF LLM service is unavailable, THEN the COBOL2Java converter SHALL fall back to rule-based conversion and log a warning.

**Acceptance Criteria**:
- [ ] フォールバック変換を実行
- [ ] 警告メッセージをログ出力
- [ ] フォールバック使用をサマリーに記録

---

### FR-005: オプション機能要件

#### REQ-OPT-001: Spring Boot出力
**Type**: Optional feature  
**Statement**: WHERE Spring Boot output is enabled, the COBOL2Java converter SHALL generate Spring Boot compatible code with annotations.

**Acceptance Criteria**:
- [ ] @Service, @Repository アノテーション生成
- [ ] application.properties テンプレート生成
- [ ] pom.xml / build.gradle 生成

#### REQ-OPT-002: テストコード生成
**Type**: Optional feature  
**Statement**: WHERE test generation is enabled, the COBOL2Java converter SHALL generate JUnit 5 test cases for converted methods.

**Acceptance Criteria**:
- [ ] JUnit 5 テストクラス生成
- [ ] 元のCOBOLテストケースを移植
- [ ] AssertJアサーション使用

---

## 非機能要件

### NFR-001: パフォーマンス
**Statement**: The COBOL2Java converter SHALL process 1,000 lines of COBOL code in under 10 seconds (without LLM).

### NFR-002: 変換品質
**Statement**: The COBOL2Java converter SHALL achieve at least 80% pass rate on COBOLEval benchmark (146 problems).

### NFR-003: 互換性
**Statement**: The COBOL2Java converter SHALL support GnuCOBOL 3.x compatible COBOL syntax.

---

## CLI インターフェース (Article II準拠)

```bash
# 基本変換
cobol2java convert input.cbl -o Output.java

# バッチ変換
cobol2java convert ./cobol-src/ -o ./java-src/ --batch

# LLM補助有効
cobol2java convert input.cbl --llm openai --model gpt-4

# Spring Boot出力
cobol2java convert input.cbl --spring-boot

# テスト生成
cobol2java convert input.cbl --generate-tests

# ベンチマーク実行
cobol2java benchmark --dataset coboleval

# ヘルプ
cobol2java --help
```

---

## トレーサビリティマトリクス (Article V準拠)

| 要件ID | 設計 | 実装 | テスト |
|--------|------|------|--------|
| REQ-CVT-001 | D-001 | TBD | T-001 |
| REQ-CVT-002 | D-002 | TBD | T-002 |
| REQ-CVT-003 | D-003 | TBD | T-003 |
| REQ-CVT-004 | D-004 | TBD | T-004 |
| REQ-CVT-005 | D-005 | TBD | T-005 |
| REQ-EVT-001 | D-006 | TBD | T-006 |
| REQ-EVT-002 | D-007 | TBD | T-007 |
| REQ-EVT-003 | D-008 | TBD | T-008 |
| REQ-STA-001 | D-009 | TBD | T-009 |
| REQ-STA-002 | D-010 | TBD | T-010 |
| REQ-ERR-001 | D-011 | TBD | T-011 |
| REQ-ERR-002 | D-012 | TBD | T-012 |
| REQ-OPT-001 | D-013 | TBD | T-013 |
| REQ-OPT-002 | D-014 | TBD | T-014 |

---

## 承認

| 役割 | 名前 | 日付 | 署名 |
|------|------|------|------|
| プロダクトオーナー | | | |
| テックリード | | | |
| QAリード | | | |

---

*このドキュメントはMUSUBI SDD Article IV (EARS Format) に準拠しています*
