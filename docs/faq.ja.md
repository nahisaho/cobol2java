# COBOL2Java FAQ

よくある質問と回答をまとめました。

## 一般的な質問

### Q: どのような COBOL プログラムを変換できますか？

A: COBOL2Java は以下をサポートしています：

- **標準 COBOL-85** 構文
- **IBM Enterprise COBOL** (EXEC SQL, EXEC CICS を含む)
- **Micro Focus COBOL**
- **GnuCOBOL**

複雑なレガシーコードも、LLM を活用した変換により高い精度で Java に変換できます。

### Q: 生成される Java コードの品質は？

A: 生成されるコードは以下の特徴があります：

- **読みやすい**: 意味のある変数名とメソッド名
- **保守しやすい**: 適切なコメントと構造
- **モダン**: Java 17+ の機能を活用
- **Spring Boot 対応**: オプションで Spring Boot プロジェクトとして生成

### Q: LLM は必須ですか？

A: いいえ。COBOL2Java は LLM なしでもルールベースの変換が可能です。

ただし、複雑なビジネスロジックや EXEC SQL/CICS の変換には LLM を使用すると精度が向上します。

---

## 技術的な質問

### Q: COMP-3 (パック10進数) はどう変換されますか？

A: `BigDecimal` に変換されます。

```cobol
01 WS-AMOUNT PIC S9(7)V99 COMP-3.
```

↓

```java
private BigDecimal wsAmount = BigDecimal.ZERO;
```

### Q: OCCURS (配列) はどう変換されますか？

A: Java の配列または `List` に変換されます。

```cobol
01 WS-TABLE.
   05 WS-ITEM OCCURS 10 TIMES PIC X(10).
```

↓

```java
private String[] wsItem = new String[10];
```

### Q: EXEC SQL はどう変換されますか？

A: JPA Repository パターンに変換されます。

```cobol
EXEC SQL
  SELECT CUSTOMER_NAME INTO :WS-NAME
  FROM CUSTOMERS
  WHERE CUSTOMER_ID = :WS-ID
END-EXEC.
```

↓

```java
@Autowired
private CustomerRepository customerRepository;

Customer customer = customerRepository.findById(wsId)
    .orElseThrow(() -> new NotFoundException("Customer not found"));
String wsName = customer.getCustomerName();
```

### Q: COPY 文はどう処理されますか？

A: COPY 文は検出され、対応するインクルードファイルの内容が展開されます。
インクルードファイルが見つからない場合は警告が出力されます。

---

## トラブルシューティング

### Q: 変換エラーが発生します

以下を確認してください：

1. **COBOL ソースが正しい形式か**: 固定形式（1-6: 番号、7: 継続、8-72: コード）または自由形式
2. **文字エンコーディング**: UTF-8 を推奨
3. **不完全な構文**: 閉じられていない IF/END-IF など

### Q: 生成された Java コードがコンパイルできません

以下を確認してください：

1. **Java バージョン**: Java 17+ が必要
2. **依存関係**: Spring Boot を使用する場合は適切な依存関係を追加
3. **パッケージ名**: 正しいパッケージ名が指定されているか

### Q: パフォーマンスが遅いです

以下を試してください：

1. **LLM なしモード**: 単純な変換には LLM を使わない
2. **ローカル LLM**: Ollama でローカル LLM を使用
3. **バッチ処理**: 複数ファイルを一括処理

---

## ライセンスと貢献

### Q: ライセンスは何ですか？

A: MIT ライセンスです。商用利用も可能です。

### Q: 貢献したいのですが

A: プルリクエストを歓迎します！

1. リポジトリをフォーク
2. 機能ブランチを作成
3. テストを追加
4. プルリクエストを作成

詳細は [CONTRIBUTING.md](../CONTRIBUTING.md) を参照してください。
