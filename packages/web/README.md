# COBOL2Java - VS Code Extension

COBOL から Java への変換を VS Code で直接実行できる拡張機能です。

## 機能

- 📄 **COBOL ファイル変換** - .cbl, .cob ファイルを Java に変換
- 🤖 **Copilot 連携** - GitHub Copilot Language Model API を使用した高品質変換
- ✅ **検証機能** - COBOL ソースの構文検証
- 🎨 **Code Lens** - ファイル上部にクイックアクションボタン

## コマンド

| コマンド | 説明 |
|----------|------|
| `COBOL2Java: Convert COBOL to Java` | 基本的なルールベース変換 |
| `COBOL2Java: Convert COBOL to Java (with Copilot)` | Copilot LLM アシスト付き変換 |
| `COBOL2Java: Validate COBOL Source` | 構文検証のみ実行 |

## 使い方

1. COBOL ファイル (.cbl, .cob) を開く
2. コマンドパレット (`Cmd/Ctrl+Shift+P`) を開く
3. `COBOL2Java: Convert` と入力して実行
4. 変換された Java ファイルが output ディレクトリに生成される

### 右クリックメニュー

COBOL ファイルを右クリックすると、コンテキストメニューに「Convert COBOL to Java」が表示されます。

### Code Lens

COBOL ファイルを開くと、ファイルの先頭に以下のアクションボタンが表示されます：

- ▶️ Convert to Java
- ✨ Convert with Copilot
- ✓ Validate

## 設定

| 設定 | デフォルト | 説明 |
|------|-----------|------|
| `cobol2java.packageName` | `com.example` | 生成される Java のパッケージ名 |
| `cobol2java.springBoot` | `false` | Spring Boot アノテーション付きで生成 |
| `cobol2java.outputDirectory` | `./output` | 出力ディレクトリ |
| `cobol2java.copilotModel` | `gpt-4o` | Copilot で使用するモデル |

## 必要要件

- VS Code 1.95.0 以降
- GitHub Copilot 拡張機能 (Copilot 連携使用時)

## インストール

### VSIX からインストール

```bash
# ビルド
cd packages/web
pnpm install
pnpm build
npx @vscode/vsce package

# VS Code にインストール
code --install-extension cobol2java-0.3.0.vsix
```

### マーケットプレイスからインストール (将来予定)

VS Code Extension Marketplace で「COBOL2Java」を検索

## サポートされる COBOL 構文

- IDENTIFICATION DIVISION, PROGRAM-ID
- DATA DIVISION, WORKING-STORAGE SECTION
- PIC 9, PIC X, PIC A, COMP-1/2/3
- PROCEDURE DIVISION
- IF/ELSE/END-IF
- EVALUATE/WHEN/END-EVALUATE
- PERFORM, PERFORM UNTIL, PERFORM VARYING
- MOVE, COMPUTE, ADD, SUBTRACT, MULTIPLY, DIVIDE
- DISPLAY, ACCEPT
- STRING, INSPECT REPLACING
- SET, INITIALIZE, CONTINUE
- STOP RUN, GOBACK, EXIT

## ライセンス

MIT License
