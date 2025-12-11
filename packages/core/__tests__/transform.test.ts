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
