# Contributing to COBOL2Java

COBOL2Java へのコントリビューションを歓迎します！

## 開発環境のセットアップ

```bash
# リポジトリをフォーク & クローン
git clone https://github.com/your-username/cobol2java.git
cd cobol2java

# 依存関係をインストール
pnpm install

# ビルド
pnpm build

# テスト実行
pnpm test
```

## 開発ワークフロー

### ブランチ戦略

- `main` - 安定版リリース
- `develop` - 開発ブランチ
- `feature/*` - 新機能
- `fix/*` - バグ修正
- `docs/*` - ドキュメント更新

### コミットメッセージ

[Conventional Commits](https://www.conventionalcommits.org/) に従ってください：

```
feat: 新機能の追加
fix: バグ修正
docs: ドキュメントの更新
test: テストの追加・修正
refactor: リファクタリング
chore: ビルド・ツール関連
```

### プルリクエスト

1. フォークからブランチを作成
2. 変更を実装
3. テストを追加
4. ビルドが通ることを確認 (`pnpm build && pnpm test`)
5. プルリクエストを作成

## プロジェクト構造

```
packages/
├── core/       # コア変換ライブラリ
├── cli/        # コマンドラインツール
├── web/        # Web API
└── webapp/     # Web アプリケーション
```

## テスト

```bash
# 全テスト実行
pnpm test

# 特定パッケージのテスト
pnpm --filter cobol2java-core test

# ウォッチモード
pnpm test:watch

# カバレッジ
pnpm test:coverage
```

## コードスタイル

- TypeScript を使用
- ESLint + Prettier でフォーマット
- 型安全性を重視

## 質問・議論

- [GitHub Issues](https://github.com/nahisaho/cobol2java/issues) - バグ報告、機能リクエスト
- [GitHub Discussions](https://github.com/nahisaho/cobol2java/discussions) - 質問、議論

## ライセンス

コントリビューションは MIT ライセンスの下で公開されます。
