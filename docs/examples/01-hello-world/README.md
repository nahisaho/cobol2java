# Hello World サンプル

最も基本的な COBOL プログラムから Java への変換例です。

## COBOL ソース (input.cbl)

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       AUTHOR. COBOL2JAVA SAMPLE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(20) VALUE "Hello, World!".
       
       PROCEDURE DIVISION.
       MAIN-PROC.
           DISPLAY WS-MESSAGE.
           STOP RUN.
```

## 変換コマンド

```bash
pnpm cobol2java convert input.cbl -o Hello.java
```

## 生成される Java (Hello.java)

```java
/**
 * Converted from COBOL program: HELLO
 * Author: COBOL2JAVA SAMPLE
 */
public class Hello {
    // Working Storage
    private String wsMessage = "Hello, World!";
    
    /**
     * Main entry point
     */
    public void execute() {
        mainProc();
    }
    
    /**
     * MAIN-PROC paragraph
     */
    private void mainProc() {
        System.out.println(wsMessage);
    }
}
```

## 解説

### データ型の変換

| COBOL | Java |
|-------|------|
| `PIC X(20)` | `String` |

`PIC X(n)` は固定長文字列を表しますが、Java では通常の `String` に変換されます。

### VALUE 句

`VALUE "Hello, World!"` は Java のフィールド初期化に変換されます。

### DISPLAY ステートメント

`DISPLAY` は `System.out.println()` に変換されます。

### STOP RUN

`STOP RUN` はメソッドの終了を意味し、通常は生成されたメソッドの末尾となります。
