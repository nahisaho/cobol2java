# COBOL2Java プロジェクト可能性調査報告書

**調査日**: 2025-12-11  
**データソース**: Open Mainframe Project / Zorse  
**調査者**: COBOL2Java Team

---

## 1. エグゼクティブサマリー

### 結論: プロジェクト実現可能性は **高い** ✅

Zorse プロジェクトから取得したデータと技術的分析に基づき、COBOL から Java への LLM ベースの変換プロジェクトは実現可能と判断しました。

---

## 2. Zorse プロジェクト概要

### 2.1 プロジェクトの目的
- **組織**: Open Mainframe Project (Linux Foundation)
- **目的**: メインフレーム言語（COBOL、JCL、Assembler）向けの LLM トレーニングデータセットの構築
- **ライセンス**: Apache-2.0 / MIT

### 2.2 利用可能なリポジトリ

| リポジトリ | 内容 | Stars | ライセンス |
|-----------|------|-------|-----------|
| [zorse-project/zorse](https://github.com/zorse-project/zorse) | LLMトレーニングツール | 7 | Apache-2.0 |
| [zorse-project/COBOLEval](https://github.com/zorse-project/COBOLEval) | COBOL評価ベンチマーク | 41 | MIT |

---

## 3. COBOLEval データセット分析

### 3.1 データセット統計

```
総問題数: 146
テストケース総数: 821
```

### 3.2 データ型分布

| 戻り値型 | テストケース数 | 割合 |
|----------|---------------|------|
| Int | 229 | 27.9% |
| Bool | 208 | 25.3% |
| String | 185 | 22.5% |
| List<Int> | 130 | 15.8% |
| List<String> | 43 | 5.2% |
| Float | 16 | 1.9% |
| List<Float> | 10 | 1.2% |

### 3.3 データ構造

各問題は以下の構造を持つ:

```json
{
  "task_id": "HumanEval/X",
  "prompt": "COBOL プログラムテンプレート（IDENTIFICATION DIV〜WORKING-STORAGE）",
  "entry_point": "function_name",
  "canonical_solution": "Python での正解実装",
  "tests": [
    {
      "test": "COBOL テストプログラム全体",
      "result": {"value": "期待値", "type_": "型"}
    }
  ]
}
```

---

## 4. COBOLコード特性分析

### 4.1 COBOL構造パターン

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HAS-CLOSE-ELEMENTS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       LINKAGE SECTION.
       01 LINKED-ITEMS.
           05 L-NUMBERS OCCURS 5 TIMES INDEXED BY NI COMP-2.
           05 L-THRESHOLD COMP-2.
           05 RESULT PIC 9.

       WORKING-STORAGE SECTION.
       
       PROCEDURE DIVISION.
       * ここに実装が入る
       GOBACK.
```

### 4.2 COBOL → Java 変換対応表

| COBOL要素 | Java対応 | 複雑度 |
|-----------|----------|--------|
| IDENTIFICATION DIVISION | class declaration | 低 |
| DATA DIVISION | field declarations | 中 |
| PIC 9 / PIC X | int / String | 低 |
| COMP-2 | double | 低 |
| OCCURS ... TIMES | array[] | 中 |
| LINKAGE SECTION | method parameters | 中 |
| PERFORM VARYING | for loop | 中 |
| IF/ELSE/END-IF | if/else | 低 |
| MOVE | assignment | 低 |
| CALL | method invocation | 中 |
| GOBACK/STOP RUN | return/System.exit | 低 |

---

## 5. 技術的実現可能性

### 5.1 変換アプローチ

#### オプション A: ルールベース変換 + LLM補助
- **利点**: 予測可能な結果、高い精度
- **欠点**: ルール作成に時間がかかる

#### オプション B: 純粋LLMベース変換
- **利点**: 柔軟性が高い、コンテキスト理解
- **欠点**: 一貫性の問題、ハルシネーションリスク

#### オプション C: ハイブリッドアプローチ（推奨）
- **構文解析**: tree-sitter-cobol による AST 解析
- **変換**: ルールベース + LLM による最適化
- **検証**: COBOLEval テストスイートによる自動検証

### 5.2 既存技術の活用

| 技術 | 用途 | 成熟度 |
|------|------|--------|
| tree-sitter-cobol | COBOL構文解析 | 実験的 |
| GnuCOBOL | COBOL実行/検証 | 成熟 |
| OpenAI/Claude API | LLM変換 | 成熟 |
| COBOLEval | 変換品質評価 | 利用可能 |

### 5.3 GPT-4のCOBOL性能（参考）

COBOLEvalベンチマークによる評価結果:
```
Pass@1: 10.27% (GPT-4)
```

**注**: この低いスコアは、LLMがCOBOLを「書く」能力を示しており、COBOLを「読む」（→Javaに変換）タスクとは異なります。

---

## 6. プロジェクト実装計画

### Phase 1: 基盤構築（2週間）
- [ ] COBOL パーサーの選定・実装
- [ ] 基本的な変換ルールの実装
- [ ] 評価フレームワークの構築

### Phase 2: コア変換機能（4週間）
- [ ] データ型変換の実装
- [ ] 制御構造の変換
- [ ] ファイル操作の変換
- [ ] LLM補助機能の統合

### Phase 3: 最適化・検証（2週間）
- [ ] COBOLEval による品質評価
- [ ] Java コードの最適化
- [ ] エッジケースの対応

### Phase 4: プロダクション準備（2週間）
- [ ] CLI/API インターフェース
- [ ] ドキュメンテーション
- [ ] CI/CD パイプライン

---

## 7. リスク分析

### 高リスク
| リスク | 影響度 | 対策 |
|--------|--------|------|
| 複雑なCOBOL構文の未対応 | 高 | 段階的に対応範囲を拡大 |
| LLMハルシネーション | 中 | 構文検証レイヤーの追加 |

### 中リスク
| リスク | 影響度 | 対策 |
|--------|--------|------|
| 実行時の意味的等価性 | 中 | 包括的なテストスイート |
| パフォーマンス問題 | 中 | プロファイリングと最適化 |

### 低リスク
| リスク | 影響度 | 対策 |
|--------|--------|------|
| ライセンス問題 | 低 | OSS ライセンスの確認済み |

---

## 8. 結論と推奨事項

### 8.1 総合評価

| 評価項目 | スコア | コメント |
|----------|--------|----------|
| データ利用可能性 | ⭐⭐⭐⭐⭐ | COBOLEval + Stack v2 |
| 技術的実現可能性 | ⭐⭐⭐⭐☆ | 既存ツールを活用可能 |
| 市場需要 | ⭐⭐⭐⭐⭐ | メインフレームモダナイゼーション需要大 |
| 差別化要因 | ⭐⭐⭐⭐☆ | LLM+検証の組み合わせ |

### 8.2 推奨事項

1. **プロジェクト開始を推奨**: Zorseのデータセットと評価フレームワークは、COBOL→Java変換プロジェクトの基盤として十分

2. **ハイブリッドアプローチの採用**: 純粋なLLMではなく、構文解析+LLMの組み合わせが最も実用的

3. **COBOLEvalを活用**: 146問題のベンチマークスイートは、変換品質の客観的評価に最適

4. **段階的な対応**: 基本的なCOBOL構文から始め、徐々に複雑なパターンに対応

---

## 9. 取得データの保存場所

```
storage/cobol-samples/
├── COBOLEval/           # HumanEval → COBOL ベンチマーク
│   ├── data/
│   │   ├── CobolEval.jsonl    # 146問題のCOBOLベンチマーク
│   │   └── HumanEval.jsonl    # 元のPythonベンチマーク
│   ├── src/                   # Python→COBOL トランスパイラ（Rust）
│   └── scripts/               # 評価スクリプト
│
└── zorse/               # LLMトレーニングデータツール
    └── stack.py         # The Stack v2からCOBOLデータをダウンロード
```

---

## 10. 次のステップ

1. [ ] `#sdd-requirements cobol-java-converter` でEARS要件を定義
2. [ ] `#sdd-design cobol-java-converter` でC4 + ADR設計を作成
3. [ ] 技術スタック（`steering/tech.ja.md`）の更新
4. [ ] プロダクトコンテキスト（`steering/product.ja.md`）の更新

---

*このレポートは MUSUBI SDD フレームワークに基づいて作成されました*
