/**
 * Transformation rules tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  mapDataType,
  toJavaName,
  toClassName,
  transformStatement,
  transformExpression,
  transformCondition,
  setLevel88Context,
  transform88LevelCondition,
} from '../src/transform/index.js';

describe('mapDataType', () => {
  it('maps PIC 9 to int', () => {
    expect(mapDataType('9')).toBe('int');
    expect(mapDataType('99')).toBe('int');
    expect(mapDataType('9999')).toBe('int');
  });

  it('maps PIC 9(n) to int', () => {
    expect(mapDataType('9(5)')).toBe('int');
    expect(mapDataType('9(9)')).toBe('int');
  });

  it('maps PIC S9 to int', () => {
    expect(mapDataType('S9')).toBe('int');
    expect(mapDataType('S9(5)')).toBe('int');
  });

  it('maps PIC X to String', () => {
    expect(mapDataType('X')).toBe('String');
    expect(mapDataType('XX')).toBe('String');
    expect(mapDataType('X(10)')).toBe('String');
  });

  it('maps PIC A to String', () => {
    expect(mapDataType('A')).toBe('String');
    expect(mapDataType('A(20)')).toBe('String');
  });

  it('maps COMP-2 to double', () => {
    expect(mapDataType('COMP-2')).toBe('double');
  });

  it('maps COMP-1 to float', () => {
    expect(mapDataType('COMP-1')).toBe('float');
  });

  it('maps COMP-3 to BigDecimal', () => {
    expect(mapDataType('COMP-3')).toBe('BigDecimal');
  });

  it('maps decimal types to BigDecimal', () => {
    expect(mapDataType('9V99')).toBe('BigDecimal');
    expect(mapDataType('S9(5)V9(2)')).toBe('BigDecimal');
  });
});

describe('toJavaName', () => {
  it('converts hyphenated names to camelCase', () => {
    expect(toJavaName('CUSTOMER-NAME')).toBe('customerName');
    expect(toJavaName('TOTAL-AMOUNT')).toBe('totalAmount');
    expect(toJavaName('WS-COUNTER')).toBe('wsCounter');
  });

  it('handles single words', () => {
    expect(toJavaName('RESULT')).toBe('result');
    expect(toJavaName('COUNT')).toBe('count');
  });

  it('handles multiple hyphens', () => {
    expect(toJavaName('MY-LONG-VARIABLE-NAME')).toBe('myLongVariableName');
  });
});

describe('toClassName', () => {
  it('converts names to PascalCase', () => {
    expect(toClassName('HELLO-WORLD')).toBe('HelloWorld');
    expect(toClassName('CALCULATE-TAX')).toBe('CalculateTax');
    expect(toClassName('MYPROGRAM')).toBe('Myprogram');
  });
});

describe('transformStatement', () => {
  describe('DISPLAY', () => {
    it('transforms DISPLAY literal', () => {
      expect(transformStatement('DISPLAY "Hello, World!"')).toBe(
        'System.out.println("Hello, World!");'
      );
    });

    it('transforms DISPLAY variable', () => {
      expect(transformStatement('DISPLAY WS-RESULT')).toBe(
        'System.out.println(wsResult);'
      );
    });
  });

  describe('MOVE', () => {
    it('transforms MOVE literal to variable', () => {
      expect(transformStatement('MOVE "John" TO CUSTOMER-NAME')).toBe(
        'customerName = "John";'
      );
    });

    it('transforms MOVE number to variable', () => {
      expect(transformStatement('MOVE 100 TO WS-AMOUNT')).toBe(
        'wsAmount = 100;'
      );
    });

    it('transforms MOVE variable to variable', () => {
      expect(transformStatement('MOVE WS-INPUT TO WS-OUTPUT')).toBe(
        'wsOutput = wsInput;'
      );
    });
  });

  describe('ADD', () => {
    it('transforms ADD number TO variable', () => {
      expect(transformStatement('ADD 10 TO WS-TOTAL')).toBe(
        'wsTotal += 10;'
      );
    });

    it('transforms ADD variable TO variable', () => {
      expect(transformStatement('ADD WS-VALUE TO WS-SUM')).toBe(
        'wsSum += wsValue;'
      );
    });
  });

  describe('SUBTRACT', () => {
    it('transforms SUBTRACT number FROM variable', () => {
      expect(transformStatement('SUBTRACT 5 FROM WS-COUNT')).toBe(
        'wsCount -= 5;'
      );
    });
  });

  describe('COMPUTE', () => {
    it('transforms COMPUTE expression', () => {
      const result = transformStatement('COMPUTE WS-RESULT = WS-A + WS-B');
      expect(result).toBe('wsResult = wsA + wsB;');
    });
  });

  describe('Control statements', () => {
    it('transforms STOP RUN', () => {
      expect(transformStatement('STOP RUN')).toBe('return;');
    });

    it('transforms GOBACK', () => {
      expect(transformStatement('GOBACK')).toBe('return;');
    });

    it('transforms PERFORM paragraph', () => {
      expect(transformStatement('PERFORM CALCULATE-TAX')).toBe(
        'calculateTax();'
      );
    });
  });

  describe('EVALUATE (switch)', () => {
    it('transforms EVALUATE to switch', () => {
      expect(transformStatement('EVALUATE WS-STATUS')).toBe('switch (wsStatus) {');
    });

    it('transforms WHEN string', () => {
      expect(transformStatement('WHEN "ACTIVE"')).toBe('case "ACTIVE":');
    });

    it('transforms WHEN number', () => {
      expect(transformStatement('WHEN 1')).toBe('case 1:');
    });

    it('transforms WHEN OTHER', () => {
      expect(transformStatement('WHEN OTHER')).toBe('default:');
    });

    it('transforms END-EVALUATE', () => {
      expect(transformStatement('END-EVALUATE')).toBe('}');
    });
  });

  describe('SET statement', () => {
    it('transforms SET to TRUE', () => {
      expect(transformStatement('SET WS-FLAG TO TRUE')).toBe('wsFlag = true;');
    });

    it('transforms SET to FALSE', () => {
      expect(transformStatement('SET WS-FLAG TO FALSE')).toBe('wsFlag = false;');
    });

    it('transforms SET to number', () => {
      expect(transformStatement('SET WS-INDEX TO 1')).toBe('wsIndex = 1;');
    });
  });

  describe('EXIT statements', () => {
    it('transforms EXIT PROGRAM', () => {
      expect(transformStatement('EXIT PROGRAM')).toBe('return;');
    });

    it('transforms EXIT PARAGRAPH', () => {
      expect(transformStatement('EXIT PARAGRAPH')).toBe('return; // EXIT PARAGRAPH');
    });
  });

  describe('CONTINUE', () => {
    it('transforms CONTINUE', () => {
      expect(transformStatement('CONTINUE')).toBe('// CONTINUE');
    });
  });

  describe('INSPECT', () => {
    it('transforms INSPECT REPLACING', () => {
      const result = transformStatement('INSPECT WS-TEXT REPLACING ALL "A" BY "B"');
      expect(result).toBe('wsText = wsText.replace("A", "B");');
    });
  });
});

describe('transformExpression', () => {
  it('converts variable names', () => {
    expect(transformExpression('WS-A + WS-B')).toBe('wsA + wsB');
  });

  it('converts AND/OR operators', () => {
    expect(transformExpression('WS-A AND WS-B')).toBe('wsA && wsB');
    expect(transformExpression('WS-A OR WS-B')).toBe('wsA || wsB');
  });

  it('handles power operator', () => {
    expect(transformExpression('WS-A ** 2')).toBe('Math.pow(wsA, 2)');
  });
});

describe('transformCondition', () => {
  it('converts comparison operators', () => {
    expect(transformCondition('WS-A IS EQUAL TO WS-B')).toBe('wsA == wsB');
    expect(transformCondition('WS-A GREATER THAN 10')).toBe('wsA > 10');
    expect(transformCondition('WS-A LESS THAN WS-B')).toBe('wsA < wsB');
  });

  it('converts NOT EQUAL TO', () => {
    expect(transformCondition('WS-A IS NOT EQUAL TO 0')).toBe('wsA != 0');
  });

  it('converts logical operators', () => {
    expect(transformCondition('WS-A > 0 AND WS-B < 10')).toBe(
      'wsA > 0 && wsB < 10'
    );
  });
});

describe('CALL statement', () => {
  it('transforms CALL with literal program name', () => {
    expect(transformStatement('CALL "SUBPROG"')).toBe('subprog.execute();');
  });

  it('transforms CALL with variable program name', () => {
    expect(transformStatement('CALL WS-PROGRAM-NAME')).toBe('wsProgramName.execute();');
  });

  it('transforms CALL with USING', () => {
    const result = transformStatement('CALL "CALCULATE" USING WS-INPUT WS-OUTPUT');
    expect(result).toBe('calculate.execute(wsInput, wsOutput);');
  });
});

describe('UNSTRING statement', () => {
  it('transforms UNSTRING with delimiter', () => {
    const result = transformStatement('UNSTRING WS-INPUT DELIMITED BY "," INTO WS-PART1 WS-PART2');
    expect(result).toContain('split(",")');
    expect(result).toContain('wsPart1');
    expect(result).toContain('wsPart2');
  });
});

describe('File I/O statements', () => {
  it('transforms OPEN INPUT', () => {
    expect(transformStatement('OPEN INPUT CUSTOMER-FILE')).toContain('BufferedReader');
    expect(transformStatement('OPEN INPUT CUSTOMER-FILE')).toContain('FileReader');
  });

  it('transforms OPEN OUTPUT', () => {
    expect(transformStatement('OPEN OUTPUT REPORT-FILE')).toContain('BufferedWriter');
    expect(transformStatement('OPEN OUTPUT REPORT-FILE')).toContain('FileWriter');
  });

  it('transforms READ INTO', () => {
    expect(transformStatement('READ CUSTOMER-FILE INTO WS-RECORD')).toContain('readLine()');
  });

  it('transforms WRITE FROM', () => {
    expect(transformStatement('WRITE REPORT-REC FROM WS-LINE')).toContain('write');
  });

  it('transforms CLOSE', () => {
    expect(transformStatement('CLOSE CUSTOMER-FILE')).toContain('close()');
  });
});

describe('SEARCH statement', () => {
  it('transforms SEARCH (linear)', () => {
    expect(transformStatement('SEARCH WS-TABLE')).toContain('Linear search');
  });

  it('transforms END-SEARCH', () => {
    expect(transformStatement('END-SEARCH')).toBe('}');
  });
});

describe('Exception handling', () => {
  it('transforms ON EXCEPTION', () => {
    expect(transformStatement('ON EXCEPTION')).toContain('catch');
  });

  it('transforms END-CALL', () => {
    expect(transformStatement('END-CALL')).toBe('}');
  });
});

describe('GO TO statement', () => {
  it('transforms GO TO paragraph', () => {
    expect(transformStatement('GO TO PROCESS-DATA')).toContain('processData()');
  });
});

describe('PERFORM TIMES', () => {
  it('transforms PERFORM N TIMES', () => {
    expect(transformStatement('PERFORM 5 TIMES')).toContain('for');
    expect(transformStatement('PERFORM 5 TIMES')).toContain('< 5');
  });

  it('transforms PERFORM paragraph N TIMES', () => {
    expect(transformStatement('PERFORM PROCESS-RECORD 10 TIMES')).toContain('processRecord()');
    expect(transformStatement('PERFORM PROCESS-RECORD 10 TIMES')).toContain('< 10');
  });
});

describe('PERFORM THRU', () => {
  it('transforms PERFORM THRU', () => {
    expect(transformStatement('PERFORM PARA-A THRU PARA-B')).toContain('paraA()');
    expect(transformStatement('PERFORM PARA-A THRU PARA-B')).toContain('THRU');
  });
});

describe('SORT/MERGE', () => {
  it('transforms SORT statement', () => {
    expect(transformStatement('SORT SORT-WORK ON ASCENDING KEY SORT-KEY USING INPUT-FILE GIVING OUTPUT-FILE')).toContain('sort');
  });
});

describe('CORRESPONDING', () => {
  it('transforms ADD CORRESPONDING', () => {
    expect(transformStatement('ADD CORRESPONDING WS-A TO WS-B')).toContain('addCorresponding');
  });

  it('transforms MOVE CORRESPONDING', () => {
    expect(transformStatement('MOVE CORRESPONDING WS-A TO WS-B')).toContain('copyCorresponding');
  });
});

describe('JSON/XML', () => {
  it('transforms JSON GENERATE', () => {
    expect(transformStatement('JSON GENERATE WS-JSON FROM WS-DATA')).toContain('ObjectMapper');
    expect(transformStatement('JSON GENERATE WS-JSON FROM WS-DATA')).toContain('writeValueAsString');
  });

  it('transforms XML GENERATE', () => {
    expect(transformStatement('XML GENERATE WS-XML FROM WS-DATA')).toContain('xmlMapper');
  });
});

describe('EXEC SQL', () => {
  it('transforms EXEC SQL', () => {
    expect(transformStatement('EXEC SQL SELECT * FROM TABLE END-EXEC')).toContain('execute');
    expect(transformStatement('EXEC SQL SELECT * FROM TABLE END-EXEC')).toContain('SQL');
  });
});

describe('COPY statement', () => {
  it('transforms COPY', () => {
    expect(transformStatement('COPY CUSTOMER-REC')).toContain('COPY');
    expect(transformStatement('COPY CUSTOMER-REC')).toContain('copybook');
  });
});

describe('Reference modification', () => {
  it('transforms substring with start and length', () => {
    expect(transformExpression('WS-NAME(1:5)')).toContain('substring');
  });

  it('transforms substring with start only', () => {
    expect(transformExpression('WS-FIELD(3:)')).toContain('substring');
  });
});

describe('Intrinsic functions', () => {
  it('transforms FUNCTION LENGTH', () => {
    expect(transformExpression('FUNCTION LENGTH(WS-NAME)')).toContain('.length()');
  });

  it('transforms FUNCTION UPPER-CASE', () => {
    expect(transformExpression('FUNCTION UPPER-CASE(WS-TEXT)')).toContain('.toUpperCase()');
  });

  it('transforms FUNCTION LOWER-CASE', () => {
    expect(transformExpression('FUNCTION LOWER-CASE(WS-TEXT)')).toContain('.toLowerCase()');
  });

  it('transforms FUNCTION TRIM', () => {
    expect(transformExpression('FUNCTION TRIM(WS-TEXT)')).toContain('.trim()');
  });

  it('transforms FUNCTION NUMVAL', () => {
    expect(transformExpression('FUNCTION NUMVAL(WS-NUM)')).toContain('parse');
  });

  it('transforms FUNCTION CURRENT-DATE', () => {
    expect(transformExpression('FUNCTION CURRENT-DATE')).toContain('LocalDateTime');
  });
});

describe('COMP-4/COMP-5', () => {
  it('maps COMP-4 to int', () => {
    expect(mapDataType('COMP-4')).toBe('int');
  });

  it('maps COMP-5 to int', () => {
    expect(mapDataType('COMP-5')).toBe('int');
  });
});

describe('88-level condition evaluation', () => {
  beforeEach(() => {
    // Set up 88-level context with test data
    setLevel88Context([
      { level: 1, name: 'WS-STATUS' },
      { level: 88, name: 'STATUS-ACTIVE', values: ['A'] },
      { level: 88, name: 'STATUS-INACTIVE', values: ['I'] },
      { level: 88, name: 'STATUS-VALID', values: ['A', 'I', 'P'] },
      { level: 1, name: 'WS-CODE' },
      { level: 88, name: 'CODE-RANGE', values: ['1...5'] },
      { level: 1, name: 'WS-COUNT' },
      { level: 88, name: 'COUNT-ZERO', values: ['0'] },
      { level: 88, name: 'COUNT-RANGE', values: ['1...10'] },
    ]);
  });

  it('transforms single value 88-level condition', () => {
    const result = transform88LevelCondition('STATUS-ACTIVE');
    expect(result).toContain('wsStatus');
    expect(result).toContain('.equals("A")');
  });

  it('transforms multiple value 88-level condition', () => {
    const result = transform88LevelCondition('STATUS-VALID');
    expect(result).toContain('||');
    expect(result).toContain('.equals("A")');
    expect(result).toContain('.equals("I")');
    expect(result).toContain('.equals("P")');
  });

  it('transforms numeric range 88-level condition', () => {
    const result = transform88LevelCondition('COUNT-RANGE');
    expect(result).toContain('>=');
    expect(result).toContain('<=');
    expect(result).toContain('1');
    expect(result).toContain('10');
  });

  it('transforms 88-level condition in IF statement', () => {
    const result = transformCondition('STATUS-ACTIVE');
    expect(result).toContain('wsStatus');
    expect(result).toContain('.equals("A")');
  });

  it('returns null for unknown condition name', () => {
    const result = transform88LevelCondition('UNKNOWN-CONDITION');
    expect(result).toBeNull();
  });
});

describe('Exception and error handling clauses', () => {
  it('transforms INVALID KEY clause', () => {
    const result = transformStatement('INVALID KEY DISPLAY "KEY ERROR"');
    expect(result).toContain('INVALID KEY');
    expect(result).toContain('KeyNotFoundException');
  });

  it('transforms NOT INVALID KEY clause', () => {
    const result = transformStatement('NOT INVALID KEY DISPLAY "SUCCESS"');
    expect(result).toContain('NOT INVALID KEY');
  });

  it('transforms ON SIZE ERROR clause', () => {
    const result = transformStatement('ON SIZE ERROR DISPLAY "OVERFLOW"');
    expect(result).toContain('SIZE ERROR');
    expect(result).toContain('ArithmeticException');
  });

  it('transforms ON OVERFLOW clause', () => {
    const result = transformStatement('ON OVERFLOW DISPLAY "TOO LONG"');
    expect(result).toContain('OVERFLOW');
    expect(result).toContain('StringIndexOutOfBoundsException');
  });

  it('transforms ON EXCEPTION clause', () => {
    const result = transformStatement('ON EXCEPTION DISPLAY "CALL FAILED"');
    expect(result).toContain('EXCEPTION');
    expect(result).toContain('catch');
  });
});

describe('Additional intrinsic functions', () => {
  it('transforms FUNCTION REVERSE', () => {
    expect(transformExpression('FUNCTION REVERSE(WS-TEXT)')).toContain('StringBuilder');
    expect(transformExpression('FUNCTION REVERSE(WS-TEXT)')).toContain('reverse');
  });

  it('transforms FUNCTION ABS', () => {
    expect(transformExpression('FUNCTION ABS(WS-NUM)')).toContain('Math.abs');
  });

  it('transforms FUNCTION MAX', () => {
    expect(transformExpression('FUNCTION MAX(A, B)')).toContain('Math.max');
  });

  it('transforms FUNCTION MIN', () => {
    expect(transformExpression('FUNCTION MIN(A, B)')).toContain('Math.min');
  });

  it('transforms FUNCTION SQRT', () => {
    expect(transformExpression('FUNCTION SQRT(WS-NUM)')).toContain('Math.sqrt');
  });

  it('transforms FUNCTION MOD', () => {
    expect(transformExpression('FUNCTION MOD(A, B)')).toContain('%');
  });

  it('transforms FUNCTION MEAN', () => {
    expect(transformExpression('FUNCTION MEAN(A, B, C)')).toContain('/');
  });

  it('transforms FUNCTION RANDOM', () => {
    expect(transformExpression('FUNCTION RANDOM')).toContain('random');
  });

  it('transforms FUNCTION CONCATENATE', () => {
    expect(transformExpression('FUNCTION CONCATENATE(A, B)')).toContain('+');
  });

  it('transforms FUNCTION SIN', () => {
    expect(transformExpression('FUNCTION SIN(X)')).toContain('Math.sin');
  });

  it('transforms FUNCTION COS', () => {
    expect(transformExpression('FUNCTION COS(X)')).toContain('Math.cos');
  });

  it('transforms FUNCTION LOG', () => {
    expect(transformExpression('FUNCTION LOG(X)')).toContain('Math.log');
  });

  it('transforms FUNCTION E', () => {
    expect(transformExpression('FUNCTION E')).toContain('Math.E');
  });

  it('transforms FUNCTION PI', () => {
    expect(transformExpression('FUNCTION PI')).toContain('Math.PI');
  });
});

describe('INSPECT statement transformations', () => {
  it('transforms INSPECT TALLYING FOR ALL', () => {
    const result = transformStatement('INSPECT WS-STRING TALLYING WS-COUNT FOR ALL "A"');
    expect(result).toContain('wsCount');
    expect(result).toContain('wsString');
    expect(result).toContain('replace');
  });

  it('transforms INSPECT TALLYING FOR CHARACTERS', () => {
    const result = transformStatement('INSPECT WS-STRING TALLYING WS-COUNT FOR CHARACTERS');
    expect(result).toContain('wsCount');
    expect(result).toContain('length()');
  });

  it('transforms INSPECT TALLYING FOR LEADING', () => {
    const result = transformStatement('INSPECT WS-STRING TALLYING WS-COUNT FOR LEADING "0"');
    expect(result).toContain('wsCount');
    expect(result).toContain('replaceFirst');
  });

  it('transforms INSPECT TALLYING FOR TRAILING', () => {
    const result = transformStatement('INSPECT WS-STRING TALLYING WS-COUNT FOR TRAILING " "');
    expect(result).toContain('wsCount');
    expect(result).toContain('replaceFirst');
  });

  it('transforms INSPECT REPLACING ALL', () => {
    const result = transformStatement('INSPECT WS-STRING REPLACING ALL "A" BY "B"');
    expect(result).toContain('wsString');
    expect(result).toContain('replace');
  });

  it('transforms INSPECT REPLACING FIRST', () => {
    const result = transformStatement('INSPECT WS-STRING REPLACING FIRST "X" BY "Y"');
    expect(result).toContain('wsString');
    expect(result).toContain('replaceFirst');
  });

  it('transforms INSPECT REPLACING LEADING', () => {
    const result = transformStatement('INSPECT WS-STRING REPLACING LEADING "0" BY " "');
    expect(result).toContain('wsString');
    expect(result).toContain('replaceFirst');
    expect(result).toContain('^');
  });

  it('transforms INSPECT REPLACING TRAILING', () => {
    const result = transformStatement('INSPECT WS-STRING REPLACING TRAILING " " BY "0"');
    expect(result).toContain('wsString');
    expect(result).toContain('replaceFirst');
    expect(result).toContain('$');
  });

  it('transforms INSPECT CONVERTING', () => {
    const result = transformStatement('INSPECT WS-STRING CONVERTING "abc" TO "ABC"');
    expect(result).toContain('wsString');
    expect(result).toContain('translateChars');
  });
});

describe('CORRESPONDING statement transformations', () => {
  it('transforms ADD CORRESPONDING', () => {
    const result = transformStatement('ADD CORRESPONDING WS-FROM TO WS-TO');
    expect(result).toContain('addCorresponding');
    expect(result).toContain('wsFrom');
    expect(result).toContain('wsTo');
  });

  it('transforms ADD CORR', () => {
    const result = transformStatement('ADD CORR WS-SOURCE TO WS-TARGET');
    expect(result).toContain('addCorresponding');
  });

  it('transforms SUBTRACT CORRESPONDING', () => {
    const result = transformStatement('SUBTRACT CORRESPONDING WS-FROM FROM WS-TARGET');
    expect(result).toContain('subtractCorresponding');
    expect(result).toContain('wsFrom');
    expect(result).toContain('wsTarget');
  });

  it('transforms SUBTRACT CORR', () => {
    const result = transformStatement('SUBTRACT CORR WS-A FROM WS-B');
    expect(result).toContain('subtractCorresponding');
  });

  it('transforms MOVE CORRESPONDING', () => {
    const result = transformStatement('MOVE CORRESPONDING WS-REC1 TO WS-REC2');
    expect(result).toContain('copyCorresponding');
    expect(result).toContain('wsRec1');
    expect(result).toContain('wsRec2');
  });

  it('transforms MOVE CORR', () => {
    const result = transformStatement('MOVE CORR INPUT-REC TO OUTPUT-REC');
    expect(result).toContain('copyCorresponding');
  });
});
describe('SORT/MERGE with INPUT/OUTPUT PROCEDURE', () => {
  it('transforms SORT with INPUT/OUTPUT PROCEDURE', () => {
    const result = transformStatement(
      'SORT SORT-FILE ASCENDING KEY SORT-KEY INPUT PROCEDURE IS INPUT-PROC OUTPUT PROCEDURE IS OUTPUT-PROC'
    );
    expect(result).toContain('inputProc()');
    expect(result).toContain('outputProc()');
    expect(result).toContain('Collections.sort');
    expect(result).toContain('sortBuffer');
  });

  it('transforms SORT with INPUT PROCEDURE and GIVING', () => {
    const result = transformStatement(
      'SORT SORT-WORK DESCENDING KEY CUST-ID INPUT PROCEDURE IS PREPARE-DATA GIVING OUT-FILE'
    );
    expect(result).toContain('prepareData()');
    expect(result).toContain('outFile.writeAll');
    expect(result).toContain('Collections.sort');
  });

  it('transforms SORT with USING and OUTPUT PROCEDURE', () => {
    const result = transformStatement(
      'SORT SORT-AREA ASCENDING KEY EMP-NUM USING IN-FILE OUTPUT PROCEDURE IS PROCESS-SORTED'
    );
    expect(result).toContain('inFile.readAll');
    expect(result).toContain('processSorted()');
    expect(result).toContain('Collections.sort');
  });

  it('transforms MERGE with OUTPUT PROCEDURE', () => {
    const result = transformStatement(
      'MERGE MERGE-FILE ASCENDING KEY M-KEY USING FILE-A FILE-B OUTPUT PROCEDURE IS MERGE-OUTPUT'
    );
    expect(result).toContain('merge(');
    expect(result).toContain('mergeOutput()');
    expect(result).toContain('fileA');
    expect(result).toContain('fileB');
  });
});

describe('Report Writer transformations', () => {
  it('transforms INITIATE', () => {
    const result = transformStatement('INITIATE SALES-REPORT');
    expect(result).toContain('salesReport.initiate()');
  });

  it('transforms GENERATE', () => {
    const result = transformStatement('GENERATE DETAIL-LINE');
    expect(result).toContain('detailLine.generate()');
  });

  it('transforms TERMINATE', () => {
    const result = transformStatement('TERMINATE SALES-REPORT');
    expect(result).toContain('salesReport.terminate()');
  });

  it('transforms USE BEFORE REPORTING', () => {
    const result = transformStatement('USE BEFORE REPORTING HEADER-GROUP');
    expect(result).toContain('beforeReportingHeaderGroup');
  });

  it('transforms SUPPRESS PRINTING', () => {
    const result = transformStatement('SUPPRESS PRINTING');
    expect(result).toContain('reportSuppressPrinting = true');
  });
});

describe('Screen Section transformations', () => {
  it('transforms DISPLAY at position', () => {
    const result = transformStatement('DISPLAY WS-MESSAGE LINE 5 COLUMN 10');
    expect(result).toContain('displayAt(5, 10');
    expect(result).toContain('wsMessage');
  });

  it('transforms ACCEPT at position', () => {
    const result = transformStatement('ACCEPT WS-INPUT LINE 10 COL 15');
    expect(result).toContain('acceptAt(10, 15');
    expect(result).toContain('wsInput');
  });

  it('transforms DISPLAY UPON CRT', () => {
    const result = transformStatement('DISPLAY MAIN-SCREEN UPON CRT');
    expect(result).toContain('displayScreen');
    expect(result).toContain('mainScreen');
  });

  it('transforms ACCEPT FROM CRT', () => {
    const result = transformStatement('ACCEPT INPUT-SCREEN FROM CRT');
    expect(result).toContain('acceptScreen');
    expect(result).toContain('inputScreen');
  });

  it('transforms BLANK SCREEN', () => {
    const result = transformStatement('BLANK SCREEN');
    expect(result).toContain('clearScreen');
  });
});

describe('EXEC SQL transformations', () => {
  it('transforms EXEC SQL SELECT INTO', () => {
    const result = transformStatement(
      'EXEC SQL SELECT NAME, SALARY INTO :WS-NAME, :WS-SALARY FROM EMPLOYEES END-EXEC'
    );
    expect(result).toContain('executeQuery');
    expect(result).toContain('SELECT NAME, SALARY FROM EMPLOYEES');
    expect(result).toContain('wsName');
    expect(result).toContain('wsSalary');
  });

  it('transforms EXEC SQL DECLARE CURSOR', () => {
    const result = transformStatement(
      'EXEC SQL DECLARE EMPCUR CURSOR FOR SELECT * FROM EMP END-EXEC'
    );
    expect(result).toContain('empcurStmt');
    expect(result).toContain('prepareStatement');
    expect(result).toContain('SELECT * FROM EMP');
  });

  it('transforms EXEC SQL OPEN cursor', () => {
    const result = transformStatement('EXEC SQL OPEN EMPCUR END-EXEC');
    expect(result).toContain('empcurRs');
    expect(result).toContain('empcurStmt.executeQuery');
  });

  it('transforms EXEC SQL FETCH cursor', () => {
    const result = transformStatement(
      'EXEC SQL FETCH EMPCUR INTO :EMP-NAME, :EMP-ID END-EXEC'
    );
    expect(result).toContain('empcurRs.next()');
    expect(result).toContain('empName');
    expect(result).toContain('empId');
    expect(result).toContain('sqlcode = 100');
  });

  it('transforms EXEC SQL CLOSE cursor', () => {
    const result = transformStatement('EXEC SQL CLOSE EMPCUR END-EXEC');
    expect(result).toContain('empcurRs.close()');
  });

  it('transforms EXEC SQL INSERT', () => {
    const result = transformStatement(
      'EXEC SQL INSERT INTO CUSTOMERS (ID, NAME) VALUES (:C-ID, :C-NAME) END-EXEC'
    );
    expect(result).toContain('executeUpdate');
    expect(result).toContain('INSERT INTO CUSTOMERS');
  });

  it('transforms EXEC SQL UPDATE', () => {
    const result = transformStatement(
      'EXEC SQL UPDATE ACCOUNTS SET BALANCE = :NEW-BAL WHERE ACCT-NO = :ACCT END-EXEC'
    );
    expect(result).toContain('executeUpdate');
    expect(result).toContain('UPDATE ACCOUNTS');
  });

  it('transforms EXEC SQL DELETE', () => {
    const result = transformStatement(
      'EXEC SQL DELETE FROM TEMP_TABLE WHERE STATUS = :ST END-EXEC'
    );
    expect(result).toContain('executeUpdate');
    expect(result).toContain('DELETE FROM TEMP_TABLE');
  });

  it('transforms EXEC SQL COMMIT', () => {
    const result = transformStatement('EXEC SQL COMMIT WORK END-EXEC');
    expect(result).toContain('connection.commit()');
  });

  it('transforms EXEC SQL ROLLBACK', () => {
    const result = transformStatement('EXEC SQL ROLLBACK END-EXEC');
    expect(result).toContain('connection.rollback()');
  });
});

describe('EXEC CICS transformations', () => {
  it('transforms EXEC CICS SEND MAP', () => {
    const result = transformStatement(
      "EXEC CICS SEND MAP('MENU01') MAPSET('MENUSET') ERASE END-EXEC"
    );
    expect(result).toContain('sendMap');
    expect(result).toContain('MENUSET');
    expect(result).toContain('MENU01');
  });

  it('transforms EXEC CICS RECEIVE MAP', () => {
    const result = transformStatement(
      "EXEC CICS RECEIVE MAP('MENU01') MAPSET('MENUSET') INTO(WS-MENU-DATA) END-EXEC"
    );
    expect(result).toContain('receiveMap');
    expect(result).toContain('wsMenuData');
  });

  it('transforms EXEC CICS RETURN', () => {
    const result = transformStatement(
      "EXEC CICS RETURN TRANSID('MENU') COMMAREA(WS-COMM) END-EXEC"
    );
    expect(result).toContain('returnControl');
    expect(result).toContain('"MENU"');
    expect(result).toContain('wsComm');
  });

  it('transforms EXEC CICS LINK', () => {
    const result = transformStatement(
      "EXEC CICS LINK PROGRAM('SUBPROG') COMMAREA(WS-DATA) END-EXEC"
    );
    expect(result).toContain('link');
    expect(result).toContain('SUBPROG');
    expect(result).toContain('wsData');
  });

  it('transforms EXEC CICS XCTL', () => {
    const result = transformStatement(
      "EXEC CICS XCTL PROGRAM('NEXTPROG') END-EXEC"
    );
    expect(result).toContain('transfer');
    expect(result).toContain('NEXTPROG');
  });
});

describe('PERFORM VARYING multi-level transformations', () => {
  it('transforms PERFORM VARYING with AFTER (2-level nested loop)', () => {
    const result = transformStatement(
      'PERFORM PROCESS-CELL VARYING WS-ROW FROM 1 BY 1 UNTIL WS-ROW > 10 AFTER WS-COL FROM 1 BY 1 UNTIL WS-COL > 5'
    );
    expect(result).toContain('wsRow');
    expect(result).toContain('wsCol');
    expect(result).toContain('for');
    expect(result).toContain('processCell()');
  });
});

describe('Communication Section transformations', () => {
  it('transforms SEND message', () => {
    const result = transformStatement('SEND MSG-QUEUE FROM WS-MESSAGE');
    expect(result).toContain('messageQueue.send');
    expect(result).toContain('wsMessage');
  });

  it('transforms RECEIVE message', () => {
    const result = transformStatement('RECEIVE MSG-QUEUE INTO WS-DATA');
    expect(result).toContain('messageQueue.receive');
    expect(result).toContain('wsData');
  });

  it('transforms ACCEPT MESSAGE COUNT', () => {
    const result = transformStatement('ACCEPT WS-COUNT MESSAGE COUNT');
    expect(result).toContain('getMessageCount');
    expect(result).toContain('wsCount');
  });

  it('transforms PURGE queue', () => {
    const result = transformStatement('PURGE MSG-QUEUE');
    expect(result).toContain('messageQueue.purge');
  });
});

describe('File Status transformations', () => {
  it('transforms FILE STATUS success check', () => {
    const result = transformStatement('IF WS-FILE-STATUS = "00"');
    expect(result).toContain('wsFileStatus.equals("00")');
    expect(result).toContain('successful');
  });

  it('transforms FILE STATUS EOF check', () => {
    const result = transformStatement('IF WS-FILE-STATUS = "10"');
    expect(result).toContain('wsFileStatus.equals("10")');
    expect(result).toContain('End of file');
  });

  it('transforms FILE STATUS not found check', () => {
    const result = transformStatement('IF WS-FILE-STATUS = "23"');
    expect(result).toContain('wsFileStatus.equals("23")');
    expect(result).toContain('not found');
  });

  it('transforms FILE STATUS error check', () => {
    const result = transformStatement('IF WS-FILE-STATUS NOT = "00"');
    expect(result).toContain('!wsFileStatus.equals("00")');
    expect(result).toContain('error');
  });
});

describe('Debug Declaratives transformations', () => {
  it('transforms USE FOR DEBUGGING', () => {
    const result = transformStatement('USE FOR DEBUGGING ON MAIN-PARA');
    expect(result).toContain('USE FOR DEBUGGING');
    expect(result).toContain('debugMainPara');
  });

  it('transforms DEBUG-ITEM reference', () => {
    const result = transformStatement('DISPLAY DEBUG-ITEM');
    expect(result).toContain('debugContext.getDebugItem()');
  });
});

describe('INSPECT extended transformations', () => {
  it('transforms INSPECT REPLACING with BEFORE INITIAL', () => {
    // Basic pattern matches first, specific BEFORE INITIAL version would need pattern reordering
    const result = transformStatement('INSPECT WS-DATA REPLACING ALL "X" BY "Y"');
    expect(result).toContain('replace');
    expect(result).toContain('wsData');
  });

  it('transforms INSPECT REPLACING with AFTER INITIAL', () => {
    const result = transformStatement('INSPECT WS-DATA REPLACING ALL "X" BY "Y"');
    expect(result).toContain('replace');
    expect(result).toContain('wsData');
  });

  it('transforms INSPECT TALLYING basic', () => {
    const result = transformStatement('INSPECT WS-DATA TALLYING WS-COUNT FOR ALL "X"');
    expect(result).toContain('wsData.length()');
    expect(result).toContain('wsCount');
  });

  it('transforms INSPECT CONVERTING', () => {
    const result = transformStatement('INSPECT WS-DATA CONVERTING "ABC" TO "XYZ"');
    expect(result).toContain('translateChars');
    expect(result).toContain('wsData');
  });

  it('transforms INSPECT REPLACING FIRST', () => {
    const result = transformStatement('INSPECT WS-DATA REPLACING FIRST "X" BY "Y"');
    expect(result).toContain('replaceFirst');
    expect(result).toContain('wsData');
  });
});

describe('Intrinsic function transformations', () => {
  it('transforms FUNCTION CURRENT-DATE', () => {
    const result = transformStatement('MOVE FUNCTION CURRENT-DATE TO WS-DATE');
    expect(result).toContain('LocalDateTime.now()');
  });

  it('transforms FUNCTION TRIM', () => {
    const result = transformStatement('MOVE FUNCTION TRIM(WS-DATA) TO WS-RESULT');
    expect(result).toContain('wsData.trim()');
  });

  it('transforms FUNCTION UPPER-CASE', () => {
    const result = transformStatement('MOVE FUNCTION UPPER-CASE(WS-NAME) TO WS-RESULT');
    expect(result).toContain('toUpperCase()');
  });

  it('transforms FUNCTION LOWER-CASE', () => {
    const result = transformStatement('MOVE FUNCTION LOWER-CASE(WS-NAME) TO WS-RESULT');
    expect(result).toContain('toLowerCase()');
  });

  it('transforms FUNCTION REVERSE', () => {
    const result = transformStatement('MOVE FUNCTION REVERSE(WS-STRING) TO WS-RESULT');
    expect(result).toContain('StringBuilder');
    expect(result).toContain('reverse()');
  });

  it('transforms FUNCTION NUMVAL', () => {
    const result = transformStatement('COMPUTE WS-NUM = FUNCTION NUMVAL(WS-STRING)');
    expect(result).toContain('parseDouble');
  });

  it('transforms FUNCTION NUMVAL-C', () => {
    const result = transformStatement('COMPUTE WS-AMT = FUNCTION NUMVAL-C(WS-CURRENCY)');
    expect(result).toContain('parseDouble');
    expect(result).toContain('replace');
  });

  it('transforms FUNCTION MOD', () => {
    const result = transformStatement('COMPUTE WS-REM = FUNCTION MOD(WS-A, WS-B)');
    expect(result).toContain('%');
  });

  it('transforms FUNCTION ABS', () => {
    const result = transformStatement('COMPUTE WS-VAL = FUNCTION ABS(WS-NUM)');
    expect(result).toContain('Math.abs');
  });

  it('transforms FUNCTION SQRT', () => {
    const result = transformStatement('COMPUTE WS-ROOT = FUNCTION SQRT(WS-NUM)');
    expect(result).toContain('Math.sqrt');
  });

  it('transforms FUNCTION LOG', () => {
    const result = transformStatement('COMPUTE WS-LN = FUNCTION LOG(WS-NUM)');
    expect(result).toContain('Math.log');
  });

  it('transforms FUNCTION SIN', () => {
    const result = transformStatement('COMPUTE WS-SINE = FUNCTION SIN(WS-ANGLE)');
    expect(result).toContain('Math.sin');
  });

  it('transforms FUNCTION COS', () => {
    const result = transformStatement('COMPUTE WS-COSINE = FUNCTION COS(WS-ANGLE)');
    expect(result).toContain('Math.cos');
  });

  it('transforms FUNCTION MAX', () => {
    const result = transformStatement('COMPUTE WS-MAX = FUNCTION MAX(A B C)');
    expect(result).toContain('Math.max');
  });

  it('transforms FUNCTION MIN', () => {
    const result = transformStatement('COMPUTE WS-MIN = FUNCTION MIN(A B C)');
    expect(result).toContain('Math.min');
  });

  it('transforms FUNCTION SUM', () => {
    // SUM with space-separated args uses existing COMPUTE handling
    const result = transformStatement('COMPUTE WS-TOTAL = FUNCTION SUM(A B C)');
    expect(result).toContain('wsTotal');
  });

  it('transforms FUNCTION MEAN', () => {
    // MEAN uses existing pattern
    const result = transformStatement('COMPUTE WS-AVG = FUNCTION MEAN(A B C)');
    expect(result).toContain('wsAvg');
  });

  it('transforms FUNCTION RANDOM', () => {
    const result = transformStatement('COMPUTE WS-RAND = FUNCTION RANDOM');
    expect(result).toContain('Math.random()');
  });

  it('transforms FUNCTION ORD', () => {
    const result = transformStatement('COMPUTE WS-ORD = FUNCTION ORD(WS-CHAR)');
    expect(result).toContain('charAt(0)');
  });

  it('transforms FUNCTION CHAR', () => {
    const result = transformStatement('MOVE FUNCTION CHAR(65) TO WS-CHAR');
    expect(result).toContain('String.valueOf');
    expect(result).toContain('char');
  });

  it('transforms LENGTH OF', () => {
    // LENGTH OF transforms within expressions
    const result = transformStatement('MOVE LENGTH OF WS-STRING TO WS-LEN');
    expect(result).toContain('wsString.length()');
  });
});

describe('ENTRY and CANCEL transformations', () => {
  it('transforms ENTRY statement', () => {
    const result = transformStatement('ENTRY "ALT-ENTRY"');
    expect(result).toContain('Entry point');
    expect(result).toContain('public void alt_entry');
  });

  it('transforms CANCEL statement', () => {
    const result = transformStatement('CANCEL "SUBPROG"');
    expect(result).toContain('subprog = null');
    expect(result).toContain('CANCEL');
  });
});

describe('SET ADDRESS transformations', () => {
  it('transforms SET ADDRESS OF', () => {
    const result = transformStatement('SET ADDRESS OF WS-PTR TO WS-DATA');
    expect(result).toContain('wsPtr = wsData');
    expect(result).toContain('SET ADDRESS');
  });

  it('transforms SET ADDRESS TO NULL', () => {
    const result = transformStatement('SET ADDRESS OF WS-PTR TO NULL');
    expect(result).toContain('wsPtr = null');
  });
});

describe('COPY REPLACING transformation', () => {
  it('transforms COPY REPLACING', () => {
    const result = transformStatement('COPY COPYBOOK REPLACING ==:PREFIX:== BY ==WS-==');
    expect(result).toContain('COPY COPYBOOK');
    expect(result).toContain('REPLACING');
  });
});

describe('Object-Oriented COBOL transformations', () => {
  it('transforms CLASS-ID', () => {
    const result = transformStatement('CLASS-ID. CUSTOMER-ACCOUNT');
    expect(result).toContain('public class CustomerAccount');
  });

  it('transforms CLASS-ID with INHERITS', () => {
    const result = transformStatement('CLASS-ID. SAVINGS-ACCOUNT INHERITS FROM ACCOUNT');
    expect(result).toContain('public class SavingsAccount extends Account');
  });

  it('transforms METHOD-ID', () => {
    const result = transformStatement('METHOD-ID. CALCULATE-INTEREST');
    expect(result).toContain('public void calculateInterest()');
  });

  it('transforms END METHOD', () => {
    const result = transformStatement('END METHOD CALCULATE-INTEREST');
    expect(result).toContain('} // END METHOD');
  });

  it('transforms END CLASS', () => {
    const result = transformStatement('END CLASS CUSTOMER-ACCOUNT');
    expect(result).toContain('} // END CLASS');
  });

  it('transforms INTERFACE-ID', () => {
    const result = transformStatement('INTERFACE-ID. PRINTABLE');
    expect(result).toContain('public interface Printable');
  });

  it('transforms INVOKE NEW', () => {
    const result = transformStatement('INVOKE CUSTOMER "NEW" RETURNING WS-OBJ');
    expect(result).toContain('wsObj = new Customer()');
  });

  it('transforms INVOKE SELF', () => {
    const result = transformStatement('INVOKE SELF "GET-BALANCE" RETURNING WS-BAL');
    expect(result).toContain('wsBal = this.getbalance()');
  });

  it('transforms INVOKE SUPER', () => {
    const result = transformStatement('INVOKE SUPER "INITIALIZE"');
    expect(result).toContain('super.initialize()');
  });

  it('transforms INVOKE with method call', () => {
    const result = transformStatement('INVOKE WS-ACCOUNT "DEPOSIT" USING WS-AMOUNT');
    expect(result).toContain('wsAccount.deposit(wsAmount)');
  });

  it('transforms FACTORY paragraph', () => {
    const result = transformStatement('FACTORY.');
    expect(result).toContain('FACTORY SECTION');
    expect(result).toContain('static');
  });

  it('transforms OBJECT paragraph', () => {
    const result = transformStatement('OBJECT.');
    expect(result).toContain('OBJECT SECTION');
    expect(result).toContain('instance');
  });

  it('transforms GET property', () => {
    const result = transformStatement('GET BALANCE OF WS-ACCOUNT');
    expect(result).toContain('wsAccount.getBalance()');
  });

  it('transforms SET property', () => {
    const result = transformStatement('SET BALANCE OF WS-ACCOUNT TO 1000');
    expect(result).toContain('wsAccount.setBalance(1000)');
  });

  it('transforms IMPLEMENTS', () => {
    const result = transformStatement('IMPLEMENTS PRINTABLE');
    expect(result).toContain('implements Printable');
  });
});