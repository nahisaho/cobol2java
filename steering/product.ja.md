# Product Context

**Project**: COBOL2Java
**Last Updated**: 2025-12-12
**Version**: 1.2

---

## Product Vision

**Vision Statement**: メインフレームのレガシーCOBOL資産を、AI支援によって高品質なJavaコードに変換し、エンタープライズシステムのモダナイゼーションを加速する。

> COBOL2Javaは、LLMと構文解析のハイブリッドアプローチにより、企業が保有する膨大なCOBOL資産をJavaエコシステムに移行することを支援します。手動変換の数ヶ月を数日に短縮し、変換品質をCOBOLEvalベンチマークで定量的に保証します。

**Mission**: ルールベースの確実な変換とLLMの柔軟な理解力を組み合わせ、開発者が信頼できるCOBOL→Java変換を実現する。

> 構文解析によるエラーのない変換基盤の上に、LLMによるコード最適化とパターン認識を重ねることで、機械的な正確性と人間的な可読性を両立します。

---

## Product Overview

### What is COBOL2Java?

COBOL2Javaは、COBOLソースコードをJavaソースコードに自動変換するオープンソースツールです。

企業のメインフレームシステムには、数十年にわたって蓄積されたCOBOLコードが大量に存在します。これらのシステムは今も金融、保険、政府機関の基幹業務を支えていますが、COBOL開発者の高齢化、保守コストの増大、クラウド移行の要請により、モダナイゼーションが急務となっています。

COBOL2Javaは、Open Mainframe Project（Linux Foundation）のZorseプロジェクトから取得したCOBOLデータセットを活用し、146問のCOBOLEvalベンチマークによる品質検証を行います。これにより、変換結果の信頼性を定量的に示すことができます。

### Problem Statement

**Problem**: COBOLからJavaへの手動変換は、高コスト・長期間・高リスク

> - COBOLプログラマーの平均年齢は55歳以上、新規採用が困難
> - 手動変換は1万行あたり数ヶ月、コストは数千万円規模
> - 変換ミスによる業務停止リスク（金融機関では1時間停止で数億円の損失）
> - 変換品質の客観的評価基準がない

### Solution

**Solution**: LLM＋構文解析のハイブリッド変換とベンチマーク検証

> - **ルールベース変換**: 確実な構文変換（DATA DIVISION → Javaフィールド等）
> - **LLM補助変換**: 複雑なビジネスロジックの理解と最適化
> - **品質保証**: COBOLEval 146問による定量的評価
> - **CLI対応**: CI/CDパイプラインへの統合が容易

---

## Target Users

### Primary Users

#### User Persona 1: メインフレーム開発者（田中さん）

**Demographics**:

- **Role**: シニアCOBOL開発者
- **Organization Size**: 大企業（従業員5,000人以上）
- **Technical Level**: COBOL熟練、Java中級

**Goals**:

- 既存COBOLコードをJavaに移行したい
- 変換後のコードが正しく動作することを確認したい
- Javaチームに引き継げる品質のコードを生成したい

**Pain Points**:

- 手動変換は時間がかかりすぎる
- 既存の変換ツールは高価で導入障壁が高い
- 変換結果の品質に不安がある

**Use Cases**:

- バッチ処理プログラムのJava移行
- CICS/IMS連携部分の変換
- 帳票出力ロジックの変換

---

#### User Persona 2: エンタープライズアーキテクト（佐藤さん）

**Demographics**:

- **Role**: システムアーキテクト
- **Organization Size**: 大企業（従業員10,000人以上）
- **Technical Level**: 設計・アーキテクチャに精通

**Goals**:

- レガシーシステムのモダナイゼーション戦略を策定したい
- クラウド移行のロードマップを作成したい

**Pain Points**:

- 変換コスト・期間の見積もりが難しい
- ベンダー依存を避けたい

**Use Cases**:

- 変換可能性のPoC実施
- 大規模変換プロジェクトの計画立案

---

### Secondary Users

- **DevOpsエンジニア**: CI/CDパイプラインへのCOBOL2Java統合
- **QAエンジニア**: 変換結果の検証・テスト

---

## Market & Business Context

### Market Opportunity

**Market Size**: COBOLモダナイゼーション市場は年間数十億ドル規模

**Target Market**: 金融機関、保険会社、政府機関、大手製造業

> 世界のCOBOLコードは約2,200億行と推定され、その約80%が金融・保険業界で稼働しています。メインフレームのクラウド移行需要の高まりにより、COBOL変換市場は急成長しています。

### Business Model

**Revenue Model**: オープンソース（Apache-2.0）+ エンタープライズサポート

> 基本機能はオープンソースとして無料提供。大規模変換プロジェクトへのコンサルティング、カスタム変換ルールの開発、SLAサポートを有料で提供。

**Pricing Tiers**:

- **Community Edition**: 無料 - 基本変換機能、CLIツール、コミュニティサポート
- **Enterprise Edition**: 要問合せ - プライベートLLM対応、カスタムルール、SLAサポート

### Competitive Landscape

| Competitor | Strengths | Weaknesses | Our Differentiation |
| --- | --- | --- | --- |
| IBM watsonx Code Assistant | IBM製品との統合 | 高コスト、ベンダーロックイン | OSS、ベンダー中立 |
| Micro Focus Enterprise | 実績豊富 | ライセンス費用が高額 | LLM活用、低コスト |
| Amazon Q transform | AWS統合 | AWS依存 | マルチクラウド対応 |

---

## Core Product Capabilities

### Must-Have Features (MVP)

1. **基本COBOL→Java変換**
   - **Description**: DATA DIVISION、PROCEDURE DIVISIONの基本変換
   - **User Value**: 手動変換の工数を大幅削減
   - **Priority**: P0 (Critical)

2. **CLIインターフェース**
   - **Description**: コマンドラインからの変換実行
   - **User Value**: CI/CDパイプラインへの統合
   - **Priority**: P0 (Critical)

3. **COBOLEvalベンチマーク**
   - **Description**: 146問のベンチマークによる品質評価
   - **User Value**: 変換品質の定量的な確認
   - **Priority**: P0 (Critical)

### High-Priority Features (Post-MVP)

4. **LLM補助変換**
   - **Description**: OpenAI/Claude/OllamaによるLLM支援
   - **User Value**: 複雑なロジックの高品質変換
   - **Priority**: P1 (High)

5. **Spring Boot出力**
   - **Description**: Spring Boot互換コードの生成
   - **User Value**: モダンJavaフレームワークへの直接統合
   - **Priority**: P1 (High)

### Future Features (Roadmap)

6. **バッチ処理最適化**
   - **Description**: COBOLバッチ処理のSpring Batch変換
   - **User Value**: [Why users need it]
   - **Priority**: P2 (Medium)

7. **{{FEATURE_7}}**
   - **Description**: [What it does]
   - **User Value**: [Why users need it]
   - **Priority**: P3 (Low)

---

## Product Principles

### Design Principles

1. **{{PRINCIPLE_1}}**
   - [Description of what this means for product decisions]

2. **{{PRINCIPLE_2}}**
   - [Description]

3. **{{PRINCIPLE_3}}**
   - [Description]

**Examples**:

- **Simplicity First**: Favor simple solutions over complex ones
- **User Empowerment**: Give users control and flexibility
- **Speed & Performance**: Fast response times (< 200ms)

### User Experience Principles

1. **{{UX_PRINCIPLE_1}}**
   - [How this guides UX decisions]

2. **{{UX_PRINCIPLE_2}}**
   - [How this guides UX decisions]

**Examples**:

- **Progressive Disclosure**: Show advanced features only when needed
- **Accessibility First**: WCAG 2.1 AA compliance
- **Mobile-First**: Design for mobile, enhance for desktop

---

## Success Metrics

### Key Performance Indicators (KPIs)

#### Business Metrics

| Metric                              | Target            | Measurement    |
| ----------------------------------- | ----------------- | -------------- |
| **Monthly Active Users (MAU)**      | {{MAU_TARGET}}    | [How measured] |
| **Monthly Recurring Revenue (MRR)** | ${{MRR_TARGET}}   | [How measured] |
| **Customer Acquisition Cost (CAC)** | ${{CAC_TARGET}}   | [How measured] |
| **Customer Lifetime Value (LTV)**   | ${{LTV_TARGET}}   | [How measured] |
| **Churn Rate**                      | < {{CHURN_RATE}}% | [How measured] |

#### Product Metrics

| Metric                       | Target                | Measurement    |
| ---------------------------- | --------------------- | -------------- |
| **Daily Active Users (DAU)** | {{DAU_TARGET}}        | [How measured] |
| **Feature Adoption Rate**    | > {{ADOPTION_RATE}}%  | [How measured] |
| **User Retention (Day 7)**   | > {{RETENTION_RATE}}% | [How measured] |
| **Net Promoter Score (NPS)** | > {{NPS_TARGET}}      | [How measured] |

#### Technical Metrics

| Metric                      | Target  | Measurement             |
| --------------------------- | ------- | ----------------------- |
| **API Response Time (p95)** | < 200ms | Monitoring dashboard    |
| **Uptime**                  | 99.9%   | Status page             |
| **Error Rate**              | < 0.1%  | Error tracking (Sentry) |
| **Page Load Time**          | < 2s    | Web vitals              |

---

## Product Roadmap

### Phase 1: MVP (Months 1-3)

**Goal**: Launch minimum viable product

**Features**:

- [Feature 1]
- [Feature 2]
- [Feature 3]

**Success Criteria**:

- [Criterion 1]
- [Criterion 2]

---

### Phase 2: Growth (Months 4-6)

**Goal**: Achieve product-market fit

**Features**:

- [Feature 4]
- [Feature 5]
- [Feature 6]

**Success Criteria**:

- [Criterion 1]
- [Criterion 2]

---

### Phase 3: Scale (Months 7-12)

**Goal**: Scale to {{USER_TARGET}} users

**Features**:

- [Feature 7]
- [Feature 8]
- [Feature 9]

**Success Criteria**:

- [Criterion 1]
- [Criterion 2]

---

## User Workflows

### Primary Workflow 1: {{WORKFLOW_1_NAME}}

**User Goal**: {{USER_GOAL}}

**Steps**:

1. User [action 1]
2. System [response 1]
3. User [action 2]
4. System [response 2]
5. User achieves [goal]

**Success Criteria**:

- User completes workflow in < {{TIME}} minutes
- Success rate > {{SUCCESS_RATE}}%

---

### Primary Workflow 2: {{WORKFLOW_2_NAME}}

**User Goal**: {{USER_GOAL}}

**Steps**:

1. [Step 1]
2. [Step 2]
3. [Step 3]

**Success Criteria**:

- [Criterion 1]
- [Criterion 2]

---

## Business Domain

### Domain Concepts

Key concepts and terminology used in this domain:

1. **{{CONCEPT_1}}**: [Definition and importance]
2. **{{CONCEPT_2}}**: [Definition and importance]
3. **{{CONCEPT_3}}**: [Definition and importance]

**Example for SaaS Authentication**:

- **Identity Provider (IdP)**: Service that authenticates users
- **Single Sign-On (SSO)**: One login for multiple applications
- **Multi-Factor Authentication (MFA)**: Additional verification step

### Business Rules

1. **{{RULE_1}}**
   - [Description of business rule]
   - **Example**: [Concrete example]

2. **{{RULE_2}}**
   - [Description]
   - **Example**: [Example]

**Example for E-commerce**:

- **Inventory Reservation**: Reserved items held for 10 minutes during checkout
- **Refund Window**: Refunds allowed within 30 days of purchase

---

## Constraints & Requirements

### Business Constraints

- **Budget**: ${{BUDGET}}
- **Timeline**: {{TIMELINE}}
- **Team Size**: {{TEAM_SIZE}} engineers
- **Launch Date**: {{LAUNCH_DATE}}

### Compliance Requirements

- **{{COMPLIANCE_1}}**: [Description, e.g., GDPR, SOC 2, HIPAA]
- **{{COMPLIANCE_2}}**: [Description]
- **Data Residency**: [Requirements, e.g., EU data stays in EU]

### Non-Functional Requirements

- **Performance**: API response < 200ms (95th percentile)
- **Availability**: 99.9% uptime SLA
- **Scalability**: Support {{CONCURRENT_USERS}} concurrent users
- **Security**: OWASP Top 10 compliance
- **Accessibility**: WCAG 2.1 AA compliance

---

## Stakeholders

### Internal Stakeholders

| Role                    | Name                 | Responsibilities                  |
| ----------------------- | -------------------- | --------------------------------- |
| **Product Owner**       | {{PO_NAME}}          | Vision, roadmap, priorities       |
| **Tech Lead**           | {{TECH_LEAD_NAME}}   | Architecture, technical decisions |
| **Engineering Manager** | {{EM_NAME}}          | Team management, delivery         |
| **QA Lead**             | {{QA_LEAD_NAME}}     | Quality assurance, testing        |
| **Design Lead**         | {{DESIGN_LEAD_NAME}} | UX/UI design                      |

### External Stakeholders

| Role                        | Name        | Responsibilities            |
| --------------------------- | ----------- | --------------------------- |
| **Customer Advisory Board** | [Members]   | Product feedback            |
| **Investors**               | [Names]     | Funding, strategic guidance |
| **Partners**                | [Companies] | Integration, co-marketing   |

---

## Go-to-Market Strategy

### Launch Strategy

**Target Launch Date**: {{LAUNCH_DATE}}

**Launch Phases**:

1. **Private Beta** ({{START_DATE}} - {{END_DATE}})
   - Invite-only, 50 beta users
   - Focus: Gather feedback, fix critical bugs

2. **Public Beta** ({{START_DATE}} - {{END_DATE}})
   - Open signup
   - Focus: Validate product-market fit

3. **General Availability** ({{LAUNCH_DATE}})
   - Full public launch
   - Focus: Acquisition and growth

### Marketing Channels

- **{{CHANNEL_1}}**: [Strategy, e.g., Content marketing, SEO]
- **{{CHANNEL_2}}**: [Strategy, e.g., Social media, Twitter/LinkedIn]
- **{{CHANNEL_3}}**: [Strategy, e.g., Paid ads, Google/Facebook]
- **{{CHANNEL_4}}**: [Strategy, e.g., Partnerships, integrations]

---

## Risk Assessment

### Product Risks

| Risk       | Probability     | Impact          | Mitigation            |
| ---------- | --------------- | --------------- | --------------------- |
| {{RISK_1}} | High/Medium/Low | High/Medium/Low | [Mitigation strategy] |
| {{RISK_2}} | High/Medium/Low | High/Medium/Low | [Mitigation strategy] |

**Example Risks**:

- **Low adoption**: Users don't understand value → Clear onboarding, demos
- **Performance issues**: System slow at scale → Load testing, optimization
- **Security breach**: Data compromised → Security audit, penetration testing

---

## Customer Support

### Support Channels

- **Email**: support@{{COMPANY}}.com
- **Chat**: In-app live chat (business hours)
- **Documentation**: docs.{{COMPANY}}.com
- **Community**: Forum/Discord/Slack

### Support SLA

| Tier              | Response Time | Resolution Time |
| ----------------- | ------------- | --------------- |
| **Critical (P0)** | < 1 hour      | < 4 hours       |
| **High (P1)**     | < 4 hours     | < 24 hours      |
| **Medium (P2)**   | < 24 hours    | < 3 days        |
| **Low (P3)**      | < 48 hours    | Best effort     |

---

## Product Analytics

### Analytics Tools

- **{{ANALYTICS_TOOL_1}}**: [Purpose, e.g., Google Analytics, Mixpanel]
- **{{ANALYTICS_TOOL_2}}**: [Purpose, e.g., Amplitude, Heap]

### Events to Track

| Event               | Description            | Purpose           |
| ------------------- | ---------------------- | ----------------- |
| `user_signup`       | New user registration  | Track acquisition |
| `feature_used`      | User uses core feature | Track engagement  |
| `payment_completed` | User completes payment | Track conversion  |
| `error_occurred`    | User encounters error  | Track reliability |

---

## Localization & Internationalization

### Supported Languages

- **Primary**: English (en-US)
- **Secondary**: [Languages, e.g., Japanese (ja-JP), Spanish (es-ES)]

### Localization Strategy

- **UI Strings**: i18n framework (next-intl, react-i18next)
- **Date/Time**: Locale-aware formatting
- **Currency**: Multi-currency support
- **Right-to-Left (RTL)**: Support for Arabic, Hebrew (if needed)

---

## Data & Privacy

### Data Collection

**What data we collect**:

- User account information (email, name)
- Usage analytics (anonymized)
- Error logs (for debugging)

**What data we DON'T collect**:

- [Sensitive data we avoid, e.g., passwords (only hashed), payment details (tokenized)]

### Privacy Policy

- **GDPR Compliance**: Right to access, delete, export data
- **Data Retention**: [Retention period, e.g., 90 days for logs]
- **Third-Party Sharing**: [Who we share data with, why]

---

## Integrations

### Existing Integrations

| Integration       | Purpose   | Priority |
| ----------------- | --------- | -------- |
| {{INTEGRATION_1}} | [Purpose] | P0       |
| {{INTEGRATION_2}} | [Purpose] | P1       |

### Planned Integrations

| Integration       | Purpose   | Timeline |
| ----------------- | --------- | -------- |
| {{INTEGRATION_3}} | [Purpose] | Q2 2025  |
| {{INTEGRATION_4}} | [Purpose] | Q3 2025  |

---

## Changelog

### Version 1.1 (Planned)

- [Future product updates]

---

**Last Updated**: 2025-12-11
**Maintained By**: {{MAINTAINER}}
