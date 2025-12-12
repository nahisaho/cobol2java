# COBOL2Java Documentation

このディレクトリには、COBOL2Javaプロジェクトの技術ドキュメントが含まれています。

## ドキュメント一覧

| ドキュメント | 説明 |
|-------------|------|
| [concept.md](./concept.md) | 製品コンセプトとビジョン |
| [architecture.md](./architecture.md) | アーキテクチャ設計とパターン |
| [getting-started.md](./getting-started.md) | クイックスタートガイド |
| [api.md](./api.md) | APIリファレンス |
| [cli.md](./cli.md) | CLIコマンドリファレンス |
| [contributing.md](./contributing.md) | コントリビューションガイド |

## クイックスタート

```bash
# インストール
npm install cobol2java-core

# CLIインストール
npm install -g cobol2java-cli
```

```typescript
import { convert } from 'cobol2java-core';

const result = await convert(cobolSource, {
  packageName: 'com.example',
  generateTests: true,
  springBoot: true
});

console.log(result.java);
```

## 関連リソース

- [GitHub Repository](https://github.com/nahisaho/cobol2java)
- [Steering Documents](../steering/) - プロジェクト運営ドキュメント
- [Templates](../templates/) - ドキュメントテンプレート
