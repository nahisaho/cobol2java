# COBOL2Java サンプルコード

このディレクトリには、COBOL2Java を使用した変換サンプルが含まれています。

## サンプル一覧

### 基本サンプル

1. [Hello World](./01-hello-world/) - 最も基本的な変換例
2. [データ型変換](./02-data-types/) - COBOL データ型から Java への変換
3. [制御構造](./03-control-flow/) - IF/EVALUATE/PERFORM の変換

### 中級サンプル

4. [ファイル処理](./04-file-io/) - ファイル入出力の変換
5. [テーブル処理](./05-tables/) - OCCURS/配列の変換
6. [サブプログラム](./06-subprograms/) - CALL/LINKAGE の変換

### 高度なサンプル

7. [EXEC SQL](./07-exec-sql/) - 埋め込みSQL から JPA への変換
8. [EXEC CICS](./08-exec-cics/) - CICS から Spring への変換
9. [バッチ処理](./09-batch/) - Spring Batch Tasklet への変換

## クイック実行

```bash
# サンプルディレクトリに移動
cd examples/01-hello-world

# 変換を実行
pnpm cobol2java convert input.cbl -o Output.java

# 結果を確認
cat Output.java
```

## サンプルの構造

各サンプルディレクトリには以下のファイルが含まれています：

```
examples/
└── 01-hello-world/
    ├── input.cbl        # 変換元 COBOL ソース
    ├── expected.java    # 期待される Java 出力
    └── README.md        # サンプルの説明
```
