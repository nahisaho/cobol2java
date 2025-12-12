# サンプル02: セキュリティ付き変換

このサンプルでは、セキュリティモジュールを使用した安全なCOBOL変換を示します。

## COBOLソース

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-PROCESS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CUSTOMER-ID     PIC X(10).
       01 WS-CUSTOMER-NAME   PIC X(50).
       01 WS-CREDIT-LIMIT    PIC 9(7)V99.
       
       PROCEDURE DIVISION.
           ACCEPT WS-CUSTOMER-ID.
           IF WS-CUSTOMER-ID = SPACES
               DISPLAY "INVALID CUSTOMER ID"
               STOP RUN
           END-IF.
           MOVE "JOHN DOE" TO WS-CUSTOMER-NAME.
           COMPUTE WS-CREDIT-LIMIT = 50000.00.
           DISPLAY "CUSTOMER: " WS-CUSTOMER-NAME.
           STOP RUN.
```

## セキュリティ付き変換コード

```typescript
import { 
  convert, 
  checkLimits, 
  validateInput,
  sanitizeInput,
  withTimeout,
  SecurityLimits 
} from 'cobol2java-core';

async function secureConvert(source: string): Promise<string> {
  // 1. 入力サニタイズ
  const cleanSource = sanitizeInput(source);
  
  // 2. 入力検証
  const inputValidation = validateInput(cleanSource, { maxSize: 1024 * 1024 });
  if (!inputValidation.valid) {
    throw new Error(`入力検証エラー: ${inputValidation.error}`);
  }
  
  // 3. 制限チェック
  const limits: SecurityLimits = {
    maxInputSize: 1024 * 1024,
    maxLineLength: 10000,
    maxLines: 100000,
    maxNestingDepth: 50,
    maxIdentifierLength: 256,
    maxDataItems: 10000,
    maxParagraphs: 5000,
    timeoutMs: 30000,
  };
  
  const limitCheck = checkLimits(cleanSource, limits);
  if (!limitCheck.valid) {
    throw new Error(`制限超過: ${limitCheck.violations.join(', ')}`);
  }
  
  // 4. タイムアウト付き変換
  const result = await withTimeout(
    () => convert(cleanSource, {
      className: 'CustomerProcess',
      packageName: 'com.example.customer',
      springBoot: true,
    }),
    limits.timeoutMs,
    'COBOL変換'
  );
  
  return result.javaCode;
}

// 使用例
const cobolSource = `...`; // 上記COBOLソース
const javaCode = await secureConvert(cobolSource);
console.log(javaCode);
```

## 出力されるJavaコード

```java
package com.example.customer;

import org.springframework.stereotype.Component;
import java.math.BigDecimal;

@Component
public class CustomerProcess {
    private String wsCustomerId = "";
    private String wsCustomerName = "";
    private BigDecimal wsCreditLimit = BigDecimal.ZERO;
    
    public void execute() {
        // ACCEPT WS-CUSTOMER-ID
        wsCustomerId = System.console().readLine();
        
        if (wsCustomerId.trim().isEmpty()) {
            System.out.println("INVALID CUSTOMER ID");
            return;
        }
        
        wsCustomerName = "JOHN DOE";
        wsCreditLimit = new BigDecimal("50000.00");
        System.out.println("CUSTOMER: " + wsCustomerName);
    }
}
```

## ポイント

1. **sanitizeInput**: 制御文字やnullバイトを除去
2. **validateInput**: サイズ制限をチェック
3. **checkLimits**: 複雑さの制限をチェック
4. **withTimeout**: 無限ループを防止
