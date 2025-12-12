# リリース手順

## バージョニング

このプロジェクトは[セマンティックバージョニング](https://semver.org/lang/ja/)に従います。

- **Major** (x.0.0): 破壊的変更
- **Minor** (0.x.0): 新機能（後方互換）
- **Patch** (0.0.x): バグ修正

## リリースプロセス

### 1. 事前準備

```bash
# テストを実行
pnpm test

# ビルドを確認
pnpm build

# リントを確認
pnpm lint
```

### 2. バージョン更新

以下のファイルでバージョンを更新：

```bash
# ルート
package.json

# 各パッケージ
packages/core/package.json
packages/cli/package.json
packages/cli/src/index.ts  # CLI version表示

# ドキュメント
CHANGELOG.md
```

### 3. CHANGELOGの更新

```markdown
## [0.x.0] - YYYY-MM-DD

### Added
- 新機能の説明

### Changed
- 変更点の説明

### Fixed
- 修正点の説明
```

### 4. コミットとタグ

```bash
# 変更をコミット
git add -A
git commit -m "chore: release v0.x.0"

# タグを作成
git tag -a v0.x.0 -m "Release v0.x.0"

# プッシュ
git push origin main --tags
```

### 5. GitHub Release

タグをプッシュすると自動的に：
1. GitHub Actionsがリリースワークフローを実行
2. テストとビルドを実行
3. GitHub Releaseを作成
4. npmへのpublish (dry-run)

### 6. npm公開 (手動)

```bash
# coreパッケージを公開
cd packages/core
pnpm publish --access public

# CLIパッケージを公開
cd ../cli
pnpm publish --access public
```

## CLIのインストール確認

```bash
# グローバルインストール
npm install -g @cobol2java/cli

# 動作確認
cobol2java --version
cobol2java --help
```

## Docker イメージ

```bash
# ビルド
docker build -t cobol2java .

# 実行
docker run --rm cobol2java --help
```

## ロールバック

問題が発生した場合：

```bash
# タグを削除
git tag -d v0.x.0
git push origin :refs/tags/v0.x.0

# npmから非推奨化
npm deprecate @cobol2java/core@0.x.0 "問題の説明"
npm deprecate @cobol2java/cli@0.x.0 "問題の説明"
```

## チェックリスト

リリース前の確認事項：

- [ ] すべてのテストが通過
- [ ] ビルドが成功
- [ ] CHANGELOGが更新済み
- [ ] バージョン番号が全ファイルで一致
- [ ] ドキュメントが最新
- [ ] 破壊的変更がある場合はマイグレーションガイド作成
