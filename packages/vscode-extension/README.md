# COBOL2Java VS Code Extension

COBOLコードをJavaに変換するVS Code拡張機能です。

## 機能

### 🔄 COBOL → Java 変換

- **ファイル変換**: アクティブなCOBOLファイルをJavaファイルに変換
- **クリップボードにコピー**: 変換結果をクリップボードにコピー
- **ライブプレビュー**: リアルタイムで変換結果をプレビュー

### 🔍 ダイアレクト検出

COBOLソースコードから自動的にダイアレクトを検出します：
- IBM Enterprise COBOL
- Micro Focus COBOL
- GnuCOBOL
- COBOL-85 Standard

### 🎨 シンタックスハイライト

COBOLファイル（.cob, .cbl, .cobol, .cpy）に対応した構文ハイライト

## インストール

### マーケットプレイスから

1. VS Codeを開く
2. 拡張機能パネルを開く (Ctrl+Shift+X)
3. "COBOL2Java" を検索
4. インストールをクリック

### VSIXから

```bash
code --install-extension cobol2java-0.1.0.vsix
```

## 使い方

### コマンド

| コマンド | ショートカット | 説明 |
|---------|---------------|------|
| `COBOL2Java: Convert to Java` | `Ctrl+Shift+J` | COBOLファイルをJavaに変換 |
| `COBOL2Java: Show Preview` | `Ctrl+Shift+P` | ライブプレビューを表示 |
| `COBOL2Java: Convert to Clipboard` | - | クリップボードにコピー |
| `COBOL2Java: Detect Dialect` | - | ダイアレクトを検出 |

### コンテキストメニュー

COBOLファイルを右クリックして「Convert to Java」を選択

## 設定

| 設定 | デフォルト | 説明 |
|-----|----------|------|
| `cobol2java.packageName` | `com.example.converted` | 生成されるJavaクラスのパッケージ名 |
| `cobol2java.javaVersion` | `17` | ターゲットJavaバージョン (11, 17, 21) |
| `cobol2java.springBoot` | `false` | Spring Boot形式で生成 |
| `cobol2java.springBatch` | `false` | Spring Batch形式で生成 |
| `cobol2java.outputDirectory` | `""` | 出力ディレクトリ（空の場合は元ファイルと同じ場所） |

### 設定例

```json
{
  "cobol2java.packageName": "com.company.legacy",
  "cobol2java.javaVersion": 17,
  "cobol2java.springBoot": true,
  "cobol2java.outputDirectory": "src/main/java"
}
```

## 対応ファイル

- `.cob` - COBOL ソースファイル
- `.cbl` - COBOL ソースファイル
- `.cobol` - COBOL ソースファイル  
- `.cpy` - COBOL コピーブック

## 要件

- VS Code 1.85.0 以上
- Node.js 20 LTS 以上

## 開発

### ビルド

```bash
cd packages/vscode-extension
pnpm install
pnpm run compile
```

### パッケージング

```bash
pnpm run package
```

### テスト

```bash
pnpm run test
```

## ライセンス

MIT

## 関連リンク

- [COBOL2Java Core](../core/)
- [COBOL2Java CLI](../cli/)
- [COBOL2Java Web](../web/)
