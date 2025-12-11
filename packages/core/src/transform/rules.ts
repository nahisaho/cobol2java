/**
 * COBOL to Java Transformation Rules
 * 
 * Mapping rules for converting COBOL constructs to Java
 */

/**
 * Data type mapping from COBOL PIC clauses to Java types
 */
export interface DataTypeMapping {
  cobolPic: RegExp;
  javaType: string;
  defaultValue: string;
  description: string;
}

/**
 * COBOL PIC clause to Java type mappings
 */
export const DATA_TYPE_MAPPINGS: DataTypeMapping[] = [
  // Integer types
  {
    cobolPic: /^9+$/,
    javaType: 'int',
    defaultValue: '0',
    description: 'Unsigned integer',
  },
  {
    cobolPic: /^S9+$/,
    javaType: 'int',
    defaultValue: '0',
    description: 'Signed integer',
  },
  {
    cobolPic: /^9\((\d+)\)$/,
    javaType: 'int',
    defaultValue: '0',
    description: 'Unsigned integer with size',
  },
  {
    cobolPic: /^S9\((\d+)\)$/,
    javaType: 'int',
    defaultValue: '0',
    description: 'Signed integer with size',
  },
  // Long for large integers
  {
    cobolPic: /^S?9\((\d{10,})\)$/,
    javaType: 'long',
    defaultValue: '0L',
    description: 'Large integer (10+ digits)',
  },
  // Decimal types
  {
    cobolPic: /^S?9+V9+$/,
    javaType: 'BigDecimal',
    defaultValue: 'BigDecimal.ZERO',
    description: 'Decimal with implied decimal point (simple)',
  },
  {
    cobolPic: /^S?9\(\d+\)V9+$/,
    javaType: 'BigDecimal',
    defaultValue: 'BigDecimal.ZERO',
    description: 'Decimal with size and simple decimal (e.g., 9(7)V99)',
  },
  {
    cobolPic: /^S?9+V9\(\d+\)$/,
    javaType: 'BigDecimal',
    defaultValue: 'BigDecimal.ZERO',
    description: 'Decimal with simple int and sized decimal',
  },
  {
    cobolPic: /^S?9\(\d+\)V9\(\d+\)$/,
    javaType: 'BigDecimal',
    defaultValue: 'BigDecimal.ZERO',
    description: 'Decimal with size specification',
  },
  // String types
  {
    cobolPic: /^X+$/,
    javaType: 'String',
    defaultValue: '""',
    description: 'Alphanumeric',
  },
  {
    cobolPic: /^X\((\d+)\)$/,
    javaType: 'String',
    defaultValue: '""',
    description: 'Alphanumeric with size',
  },
  {
    cobolPic: /^A+$/,
    javaType: 'String',
    defaultValue: '""',
    description: 'Alphabetic',
  },
  {
    cobolPic: /^A\((\d+)\)$/,
    javaType: 'String',
    defaultValue: '""',
    description: 'Alphabetic with size',
  },
  // Computational types
  {
    cobolPic: /COMP$/,
    javaType: 'int',
    defaultValue: '0',
    description: 'Computational (binary)',
  },
  {
    cobolPic: /COMP-1$/,
    javaType: 'float',
    defaultValue: '0.0f',
    description: 'Single-precision floating point',
  },
  {
    cobolPic: /COMP-2$/,
    javaType: 'double',
    defaultValue: '0.0',
    description: 'Double-precision floating point',
  },
  {
    cobolPic: /COMP-3$/,
    javaType: 'BigDecimal',
    defaultValue: 'BigDecimal.ZERO',
    description: 'Packed decimal',
  },
  // IBM COBOL COMP-4 and COMP-5 (binary types)
  {
    cobolPic: /COMP-4$/,
    javaType: 'int',
    defaultValue: '0',
    description: 'IBM Binary (same as COMP)',
  },
  {
    cobolPic: /COMP-5$/,
    javaType: 'int',
    defaultValue: '0',
    description: 'IBM Native binary (platform-dependent)',
  },
  {
    cobolPic: /BINARY$/,
    javaType: 'int',
    defaultValue: '0',
    description: 'Binary integer',
  },
  {
    cobolPic: /BINARY-CHAR$/,
    javaType: 'byte',
    defaultValue: '(byte) 0',
    description: 'Binary char (1 byte)',
  },
  {
    cobolPic: /BINARY-SHORT$/,
    javaType: 'short',
    defaultValue: '(short) 0',
    description: 'Binary short (2 bytes)',
  },
  {
    cobolPic: /BINARY-LONG$/,
    javaType: 'int',
    defaultValue: '0',
    description: 'Binary long (4 bytes)',
  },
  {
    cobolPic: /BINARY-DOUBLE$/,
    javaType: 'long',
    defaultValue: '0L',
    description: 'Binary double (8 bytes)',
  },
  {
    cobolPic: /POINTER$/,
    javaType: 'Object',
    defaultValue: 'null',
    description: 'Pointer type',
  },
  {
    cobolPic: /INDEX$/,
    javaType: 'int',
    defaultValue: '0',
    description: 'Index type',
  },
];

/**
 * Map a COBOL PIC clause to a Java type
 */
export function mapDataType(picClause: string): string {
  const normalized = picClause.toUpperCase().replace(/\s+/g, '');
  
  for (const mapping of DATA_TYPE_MAPPINGS) {
    if (mapping.cobolPic.test(normalized)) {
      return mapping.javaType;
    }
  }
  
  // Default to String for unknown types
  return 'String';
}

/**
 * Convert COBOL name to Java camelCase
 */
export function toJavaName(cobolName: string): string {
  return cobolName
    .toLowerCase()
    .split('-')
    .map((word, index) => 
      index === 0 ? word : word.charAt(0).toUpperCase() + word.slice(1)
    )
    .join('');
}

/**
 * Convert COBOL name to Java PascalCase (for class names)
 */
export function toClassName(cobolName: string): string {
  return cobolName
    .toLowerCase()
    .split('-')
    .map(word => word.charAt(0).toUpperCase() + word.slice(1))
    .join('');
}

/**
 * Statement transformation rules
 */
export interface StatementRule {
  pattern: RegExp;
  transform: (match: RegExpMatchArray) => string;
  description: string;
}

/**
 * COBOL statement to Java statement mappings
 */
export const STATEMENT_RULES: StatementRule[] = [
  // Error/exception handling clauses - must be at top to match before DISPLAY
  // NOT patterns must come before non-NOT patterns
  // NOT INVALID KEY clause
  {
    pattern: /NOT\s+INVALID\s+KEY\s+(.+)/gi,
    transform: (match) => {
      const statement = transformStatement(match[1]!) || match[1];
      return `// NOT INVALID KEY - file operation succeeded\n${statement}`;
    },
    description: 'Not invalid key (success) handler',
  },
  // INVALID KEY clause (for indexed/relative file operations)
  {
    pattern: /INVALID\s+KEY\s+(.+)/gi,
    transform: (match) => {
      const statement = transformStatement(match[1]!) || match[1];
      return `// INVALID KEY - file operation failed\ncatch (KeyNotFoundException e) { ${statement} }`;
    },
    description: 'Invalid key handler',
  },
  // NOT ON SIZE ERROR
  {
    pattern: /NOT\s+ON\s+SIZE\s+ERROR\s+(.+)/gi,
    transform: (match) => {
      const statement = transformStatement(match[1]!) || match[1];
      return `// NOT ON SIZE ERROR - arithmetic succeeded\n${statement}`;
    },
    description: 'Not on size error handler',
  },
  // ON SIZE ERROR (arithmetic overflow)
  {
    pattern: /ON\s+SIZE\s+ERROR\s+(.+)/gi,
    transform: (match) => {
      const statement = transformStatement(match[1]!) || match[1];
      return `// ON SIZE ERROR\ncatch (ArithmeticException e) { ${statement} }`;
    },
    description: 'Size error handler',
  },
  // NOT ON OVERFLOW
  {
    pattern: /NOT\s+ON\s+OVERFLOW\s+(.+)/gi,
    transform: (match) => {
      const statement = transformStatement(match[1]!) || match[1];
      return `// NOT ON OVERFLOW - string operation succeeded\n${statement}`;
    },
    description: 'Not on overflow handler',
  },
  // ON OVERFLOW (STRING/UNSTRING)
  {
    pattern: /ON\s+OVERFLOW\s+(.+)/gi,
    transform: (match) => {
      const statement = transformStatement(match[1]!) || match[1];
      return `// ON OVERFLOW\ncatch (StringIndexOutOfBoundsException e) { ${statement} }`;
    },
    description: 'Overflow handler',
  },
  // NOT ON EXCEPTION
  {
    pattern: /NOT\s+ON\s+EXCEPTION\s+(.+)/gi,
    transform: (match) => {
      const statement = transformStatement(match[1]!) || match[1];
      return `// NOT ON EXCEPTION - call succeeded\n${statement}`;
    },
    description: 'Not on exception handler',
  },
  // ON EXCEPTION (CALL)
  {
    pattern: /ON\s+EXCEPTION\s+(.+)/gi,
    transform: (match) => {
      const statement = transformStatement(match[1]!) || match[1];
      return `// ON EXCEPTION\ncatch (Exception e) { ${statement} }`;
    },
    description: 'Exception handler',
  },
  // UNSTRING statement (string split) - must be before MOVE to prevent INTO matching
  {
    pattern: /UNSTRING\s+(\w[\w-]*)\s+DELIMITED\s+(?:BY\s+)?"([^"]+)"\s+INTO\s+(.+?)(?:\s+ON\s+OVERFLOW|\s*\.?\s*$)/gi,
    transform: (match) => {
      const source = toJavaName(match[1]!);
      const delimiter = match[2]!;
      const targetsRaw = match[3]!.replace(/\.$/, '');
      const targets = targetsRaw.split(/\s+/).filter(t => t && !t.match(/^(WITH|POINTER|COUNT|TALLYING)$/i));
      const targetVars = targets.map(t => toJavaName(t)).join(', ');
      return `String[] _parts = ${source}.split("${delimiter}"); // Assign to: ${targetVars}`;
    },
    description: 'Unstring with literal delimiter',
  },
  {
    pattern: /UNSTRING\s+(\w[\w-]*)\s+DELIMITED\s+(?:BY\s+)?(\w[\w-]*)\s+INTO\s+(.+?)(?:\s+ON\s+OVERFLOW|\s*\.?\s*$)/gi,
    transform: (match) => {
      const source = toJavaName(match[1]!);
      const delimiter = toJavaName(match[2]!);
      const targetsRaw = match[3]!.replace(/\.$/, '');
      const targets = targetsRaw.split(/\s+/).filter(t => t && !t.match(/^(WITH|POINTER|COUNT|TALLYING)$/i));
      const targetVars = targets.map(t => toJavaName(t)).join(', ');
      return `String[] _parts = ${source}.split(${delimiter}); // Assign to: ${targetVars}`;
    },
    description: 'Unstring with variable delimiter',
  },
  // STRING statement (concatenation) - must be before MOVE to prevent INTO matching
  {
    pattern: /STRING\s+(.+?)\s+DELIMITED\s+(?:BY\s+)?(?:SIZE|SPACE|"[^"]*")\s+INTO\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const parts = match[1]!.split(/\s+/).filter(p => p && !p.match(/^DELIMITED$/i));
      const target = toJavaName(match[2]!);
      const javaExprs = parts.map(p => {
        if (p.startsWith('"') && p.endsWith('"')) return p;
        return toJavaName(p);
      });
      return `${target} = ${javaExprs.join(' + ')};`;
    },
    description: 'String concatenation',
  },
  // DISPLAY statement - order matters: more specific patterns first
  {
    pattern: /DISPLAY\s+"([^"]+)"\s+(\w[\w-]*)/gi,
    transform: (match) => `System.out.println("${match[1]}" + ${toJavaName(match[2]!)});`,
    description: 'Display literal string followed by variable',
  },
  {
    pattern: /DISPLAY\s+"([^"]+)"/gi,
    transform: (match) => `System.out.println("${match[1]}");`,
    description: 'Display literal string',
  },
  {
    pattern: /DISPLAY\s+(\w[\w-]*)/gi,
    transform: (match) => `System.out.println(${toJavaName(match[1]!)});`,
    description: 'Display variable',
  },
  // MOVE statement
  {
    pattern: /MOVE\s+"([^"]+)"\s+TO\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[2]!)} = "${match[1]}";`,
    description: 'Move literal to variable',
  },
  {
    pattern: /MOVE\s+(\d+)\s+TO\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[2]!)} = ${match[1]};`,
    description: 'Move number to variable',
  },
  {
    pattern: /MOVE\s+(\w[\w-]*)\s+TO\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[2]!)} = ${toJavaName(match[1]!)};`,
    description: 'Move variable to variable',
  },
  // ADD statement - GIVING forms first (more specific)
  {
    pattern: /ADD\s+(\w[\w-]*)\s+TO\s+(\w[\w-]*)\s+GIVING\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[3]!)} = ${toJavaName(match[1]!)} + ${toJavaName(match[2]!)};`,
    description: 'Add variable to variable giving result',
  },
  {
    pattern: /ADD\s+(\d+)\s+TO\s+(\w[\w-]*)\s+GIVING\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[3]!)} = ${match[1]} + ${toJavaName(match[2]!)};`,
    description: 'Add number to variable giving result',
  },
  // ADD statement - simple forms
  {
    pattern: /ADD\s+(\d+)\s+TO\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[2]!)} += ${match[1]};`,
    description: 'Add number to variable',
  },
  {
    pattern: /ADD\s+(\w[\w-]*)\s+TO\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[2]!)} += ${toJavaName(match[1]!)};`,
    description: 'Add variable to variable',
  },
  // SUBTRACT statement - GIVING forms first
  {
    pattern: /SUBTRACT\s+(\w[\w-]*)\s+FROM\s+(\w[\w-]*)\s+GIVING\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[3]!)} = ${toJavaName(match[2]!)} - ${toJavaName(match[1]!)};`,
    description: 'Subtract variable from variable giving result',
  },
  {
    pattern: /SUBTRACT\s+(\d+)\s+FROM\s+(\w[\w-]*)\s+GIVING\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[3]!)} = ${toJavaName(match[2]!)} - ${match[1]};`,
    description: 'Subtract number from variable giving result',
  },
  // SUBTRACT statement - simple forms
  {
    pattern: /SUBTRACT\s+(\d+)\s+FROM\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[2]!)} -= ${match[1]};`,
    description: 'Subtract number from variable',
  },
  {
    pattern: /SUBTRACT\s+(\w[\w-]*)\s+FROM\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[2]!)} -= ${toJavaName(match[1]!)};`,
    description: 'Subtract variable from variable',
  },
  // MULTIPLY statement
  {
    pattern: /MULTIPLY\s+(\w[\w-]*)\s+BY\s+(\w[\w-]*)\s+GIVING\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[3]!)} = ${toJavaName(match[1]!)} * ${toJavaName(match[2]!)};`,
    description: 'Multiply with GIVING',
  },
  // DIVIDE statement
  {
    pattern: /DIVIDE\s+(\w[\w-]*)\s+BY\s+(\w[\w-]*)\s+GIVING\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[3]!)} = ${toJavaName(match[1]!)} / ${toJavaName(match[2]!)};`,
    description: 'Divide with GIVING',
  },
  // COMPUTE statement
  {
    pattern: /COMPUTE\s+(\w[\w-]*)\s*=\s*(.+?)(?:\.|$)/gi,
    transform: (match) => {
      const expr = transformExpression(match[2]!);
      return `${toJavaName(match[1]!)} = ${expr};`;
    },
    description: 'Compute expression',
  },
  // IF/ELSE/END-IF statements
  {
    pattern: /IF\s+(.+)/gi,
    transform: (match) => {
      const condition = transformCondition(match[1]!);
      return `if (${condition}) {`;
    },
    description: 'If statement',
  },
  {
    pattern: /ELSE\s*$/gi,
    transform: () => '} else {',
    description: 'Else statement',
  },
  {
    pattern: /END-IF/gi,
    transform: () => '}',
    description: 'End if',
  },
  // STOP RUN / GOBACK
  {
    pattern: /STOP\s+RUN/gi,
    transform: () => 'return;',
    description: 'Stop run',
  },
  {
    pattern: /GOBACK/gi,
    transform: () => 'return;',
    description: 'Go back',
  },
  // PERFORM VARYING (for loop) - most specific, must be first
  {
    pattern: /PERFORM\s+(\w[\w-]*)\s+VARYING\s+(\w[\w-]*)\s+FROM\s+(\d+)\s+BY\s+(\d+)\s+UNTIL\s+(\w[\w-]*)\s*>\s*(\d+)/gi,
    transform: (match) => {
      const para = toJavaName(match[1]!);
      const idx = toJavaName(match[2]!);
      const from = match[3];
      const by = match[4];
      const limit = match[6];
      return `for (int ${idx} = ${from}; ${idx} <= ${limit}; ${idx} += ${by}) {\n            ${para}();\n        }`;
    },
    description: 'Perform varying loop',
  },
  // PERFORM TIMES (fixed iteration loop)
  {
    pattern: /PERFORM\s+(\w[\w-]*)\s+(\d+)\s+TIMES/gi,
    transform: (match) => {
      const para = toJavaName(match[1]!);
      const times = match[2];
      return `for (int _i = 0; _i < ${times}; _i++) {\n            ${para}();\n        }`;
    },
    description: 'Perform N times with paragraph',
  },
  {
    pattern: /PERFORM\s+(\d+)\s+TIMES/gi,
    transform: (match) => {
      return `for (int _i = 0; _i < ${match[1]}; _i++) {`;
    },
    description: 'Perform N times inline',
  },
  // PERFORM THRU (paragraph range)
  {
    pattern: /PERFORM\s+(\w[\w-]*)\s+THRU\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const from = toJavaName(match[1]!);
      const thru = toJavaName(match[2]!);
      return `${from}(); // THRU ${thru} - execute paragraphs ${from} through ${thru}`;
    },
    description: 'Perform paragraph range',
  },
  // PERFORM WITH TEST AFTER (do-while)
  {
    pattern: /PERFORM\s+WITH\s+TEST\s+AFTER\s+UNTIL\s+(.+)/gi,
    transform: (match) => {
      const condition = transformCondition(match[1]!);
      return `do {`;
    },
    description: 'Perform with test after (do-while)',
  },
  // PERFORM UNTIL (while loop header) - more specific than simple PERFORM
  {
    pattern: /PERFORM\s+UNTIL\s+(.+)/gi,
    transform: (match) => {
      const condition = transformCondition(match[1]!);
      return `while (!(${condition})) {`;
    },
    description: 'Perform until loop',
  },
  // END-PERFORM
  {
    pattern: /END-PERFORM/gi,
    transform: () => '}',
    description: 'End perform block',
  },
  // PERFORM paragraph (simple) - least specific, must be last
  {
    pattern: /PERFORM\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[1]!)}();`,
    description: 'Perform paragraph',
  },
  // EVALUATE (switch statement)
  {
    pattern: /EVALUATE\s+(.+)/gi,
    transform: (match) => {
      const expr = transformExpression(match[1]!.replace(/\s+TRUE\s*$/i, '').trim());
      // Check if evaluating TRUE (condition-based switch)
      if (match[1]!.trim().toUpperCase() === 'TRUE') {
        return `// EVALUATE TRUE (use if/else chain)`;
      }
      return `switch (${expr}) {`;
    },
    description: 'Evaluate statement (switch)',
  },
  {
    pattern: /WHEN\s+"([^"]+)"/gi,
    transform: (match) => `case "${match[1]}":`,
    description: 'When string literal',
  },
  {
    pattern: /WHEN\s+(\d+)/gi,
    transform: (match) => `case ${match[1]}:`,
    description: 'When numeric literal',
  },
  {
    pattern: /WHEN\s+OTHER/gi,
    transform: () => 'default:',
    description: 'When other (default)',
  },
  {
    pattern: /END-EVALUATE/gi,
    transform: () => '}',
    description: 'End evaluate',
  },
  // INITIALIZE statement
  {
    pattern: /INITIALIZE\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[1]!)} = getDefaultValue(${toJavaName(match[1]!)}); // TODO: Initialize to default`,
    description: 'Initialize variable',
  },
  // ACCEPT statement (input)
  {
    pattern: /ACCEPT\s+(\w[\w-]*)\s+FROM\s+(?:CONSOLE|COMMAND-LINE)/gi,
    transform: (match) => `${toJavaName(match[1]!)} = scanner.nextLine();`,
    description: 'Accept from console',
  },
  {
    pattern: /ACCEPT\s+(\w[\w-]*)/gi,
    transform: (match) => `${toJavaName(match[1]!)} = scanner.nextLine();`,
    description: 'Accept input',
  },
  // EXIT statement
  {
    pattern: /EXIT\s+PARAGRAPH/gi,
    transform: () => 'return; // EXIT PARAGRAPH',
    description: 'Exit paragraph',
  },
  {
    pattern: /EXIT\s+SECTION/gi,
    transform: () => 'return; // EXIT SECTION',
    description: 'Exit section',
  },
  {
    pattern: /EXIT\s+PROGRAM/gi,
    transform: () => 'return;',
    description: 'Exit program',
  },
  // CONTINUE (no-op)
  {
    pattern: /CONTINUE\s*$/gi,
    transform: () => '// CONTINUE',
    description: 'Continue (no operation)',
  },
  // SET statement
  {
    pattern: /SET\s+(\w[\w-]*)\s+TO\s+TRUE/gi,
    transform: (match) => `${toJavaName(match[1]!)} = true;`,
    description: 'Set to true',
  },
  {
    pattern: /SET\s+(\w[\w-]*)\s+TO\s+FALSE/gi,
    transform: (match) => `${toJavaName(match[1]!)} = false;`,
    description: 'Set to false',
  },
  {
    pattern: /SET\s+(\w[\w-]*)\s+TO\s+(\d+)/gi,
    transform: (match) => `${toJavaName(match[1]!)} = ${match[2]};`,
    description: 'Set to number',
  },
  // INSPECT TALLYING (count occurrences)
  {
    pattern: /INSPECT\s+(\w[\w-]*)\s+TALLYING\s+(\w[\w-]*)\s+FOR\s+ALL\s+"([^"]+)"/gi,
    transform: (match) => {
      const source = toJavaName(match[1]!);
      const counter = toJavaName(match[2]!);
      const searchStr = match[3];
      return `${counter} = ${source}.length() - ${source}.replace("${searchStr}", "").length();`;
    },
    description: 'Inspect tallying all occurrences',
  },
  // INSPECT TALLYING FOR CHARACTERS
  {
    pattern: /INSPECT\s+(\w[\w-]*)\s+TALLYING\s+(\w[\w-]*)\s+FOR\s+CHARACTERS/gi,
    transform: (match) => {
      const source = toJavaName(match[1]!);
      const counter = toJavaName(match[2]!);
      return `${counter} = ${source}.length();`;
    },
    description: 'Inspect tallying characters (total length)',
  },
  // INSPECT TALLYING FOR LEADING
  {
    pattern: /INSPECT\s+(\w[\w-]*)\s+TALLYING\s+(\w[\w-]*)\s+FOR\s+LEADING\s+"([^"]+)"/gi,
    transform: (match) => {
      const source = toJavaName(match[1]!);
      const counter = toJavaName(match[2]!);
      const searchStr = match[3];
      return `${counter} = ${source}.length() - ${source}.replaceFirst("^${searchStr}+", "").length();`;
    },
    description: 'Inspect tallying leading occurrences',
  },
  // INSPECT TALLYING FOR TRAILING
  {
    pattern: /INSPECT\s+(\w[\w-]*)\s+TALLYING\s+(\w[\w-]*)\s+FOR\s+TRAILING\s+"([^"]+)"/gi,
    transform: (match) => {
      const source = toJavaName(match[1]!);
      const counter = toJavaName(match[2]!);
      const searchStr = match[3];
      return `${counter} = ${source}.length() - ${source}.replaceFirst("${searchStr}+$", "").length();`;
    },
    description: 'Inspect tallying trailing occurrences',
  },
  // INSPECT REPLACING
  {
    pattern: /INSPECT\s+(\w[\w-]*)\s+REPLACING\s+ALL\s+"([^"]+)"\s+BY\s+"([^"]+)"/gi,
    transform: (match) => {
      const target = toJavaName(match[1]!);
      return `${target} = ${target}.replace("${match[2]}", "${match[3]}");`;
    },
    description: 'Inspect replacing all',
  },
  // INSPECT REPLACING FIRST
  {
    pattern: /INSPECT\s+(\w[\w-]*)\s+REPLACING\s+FIRST\s+"([^"]+)"\s+BY\s+"([^"]+)"/gi,
    transform: (match) => {
      const target = toJavaName(match[1]!);
      return `${target} = ${target}.replaceFirst("${match[2]}", "${match[3]}");`;
    },
    description: 'Inspect replacing first occurrence',
  },
  // INSPECT REPLACING LEADING
  {
    pattern: /INSPECT\s+(\w[\w-]*)\s+REPLACING\s+LEADING\s+"([^"]+)"\s+BY\s+"([^"]+)"/gi,
    transform: (match) => {
      const target = toJavaName(match[1]!);
      return `${target} = ${target}.replaceFirst("^${match[2]}+", "${match[3]}");`;
    },
    description: 'Inspect replacing leading',
  },
  // INSPECT REPLACING TRAILING
  {
    pattern: /INSPECT\s+(\w[\w-]*)\s+REPLACING\s+TRAILING\s+"([^"]+)"\s+BY\s+"([^"]+)"/gi,
    transform: (match) => {
      const target = toJavaName(match[1]!);
      return `${target} = ${target}.replaceFirst("${match[2]}+$", "${match[3]}");`;
    },
    description: 'Inspect replacing trailing',
  },
  // INSPECT CONVERTING
  {
    pattern: /INSPECT\s+(\w[\w-]*)\s+CONVERTING\s+"([^"]+)"\s+TO\s+"([^"]+)"/gi,
    transform: (match) => {
      const target = toJavaName(match[1]!);
      const from = match[2]!;
      const to = match[3]!;
      // Character-by-character translation (like tr command)
      return `${target} = translateChars(${target}, "${from}", "${to}"); // TODO: implement translateChars`;
    },
    description: 'Inspect converting characters',
  },
  // ADD CORRESPONDING / CORR (must be before simpler ADD patterns)
  {
    pattern: /ADD\s+(?:CORRESPONDING|CORR)\s+(\w[\w-]*)\s+TO\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const source = toJavaName(match[1]!);
      const target = toJavaName(match[2]!);
      return `${target}.addCorresponding(${source}); // ADD CORRESPONDING`;
    },
    description: 'Add corresponding fields',
  },
  // SUBTRACT CORRESPONDING / CORR
  {
    pattern: /SUBTRACT\s+(?:CORRESPONDING|CORR)\s+(\w[\w-]*)\s+FROM\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const source = toJavaName(match[1]!);
      const target = toJavaName(match[2]!);
      return `${target}.subtractCorresponding(${source}); // SUBTRACT CORRESPONDING`;
    },
    description: 'Subtract corresponding fields',
  },
  // MOVE CORRESPONDING / CORR (must be before simpler MOVE patterns)
  {
    pattern: /MOVE\s+(?:CORRESPONDING|CORR)\s+(\w[\w-]*)\s+TO\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const source = toJavaName(match[1]!);
      const target = toJavaName(match[2]!);
      return `${target}.copyCorresponding(${source}); // MOVE CORRESPONDING`;
    },
    description: 'Move corresponding fields',
  },
  // SEARCH statement (linear search)
  {
    pattern: /SEARCH\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const table = toJavaName(match[1]!);
      return `// SEARCH ${table}: Linear search through table`;
    },
    description: 'Search table (linear)',
  },
  // SEARCH ALL statement (binary search)
  {
    pattern: /SEARCH\s+ALL\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const table = toJavaName(match[1]!);
      return `// SEARCH ALL ${table}: Binary search (requires sorted table)`;
    },
    description: 'Search all (binary search)',
  },
  // AT END clause for SEARCH
  {
    pattern: /AT\s+END/gi,
    transform: () => '// AT END: No match found',
    description: 'At end clause',
  },
  // WHEN clause for SEARCH
  {
    pattern: /WHEN\s+(\w[\w-]*)\s*(?:\((\w[\w-]*)\))?\s*=\s*(\w[\w-]*|\d+|"[^"]+")/gi,
    transform: (match) => {
      const field = match[2] ? `${toJavaName(match[1]!)}[${toJavaName(match[2]!)}]` : toJavaName(match[1]!);
      const value = match[3]!.startsWith('"') ? match[3] : (/^\d+$/.test(match[3]!) ? match[3] : toJavaName(match[3]!));
      return `if (${field} == ${value}) {`;
    },
    description: 'When condition in search',
  },
  // END-SEARCH
  {
    pattern: /END-SEARCH/gi,
    transform: () => '}',
    description: 'End search block',
  },
  // CALL statement (subprogram call)
  {
    pattern: /CALL\s+"([^"]+)"\s+USING\s+(.+?)(?:\s+ON\s+|$)/gi,
    transform: (match) => {
      const program = match[1]!.toLowerCase().replace(/-/g, '');
      const params = match[2]!.split(/\s+/).filter(p => p && !p.match(/^(BY|REFERENCE|VALUE|CONTENT)$/i));
      const javaParams = params.map(p => toJavaName(p)).join(', ');
      return `${program}.execute(${javaParams});`;
    },
    description: 'Call subprogram with parameters',
  },
  {
    pattern: /CALL\s+"([^"]+)"/gi,
    transform: (match) => {
      const program = match[1]!.toLowerCase().replace(/-/g, '');
      return `${program}.execute();`;
    },
    description: 'Call subprogram without parameters',
  },
  {
    pattern: /CALL\s+(\w[\w-]*)\s+USING\s+(.+?)(?:\s+ON\s+|$)/gi,
    transform: (match) => {
      const program = toJavaName(match[1]!);
      const params = match[2]!.split(/\s+/).filter(p => p && !p.match(/^(BY|REFERENCE|VALUE|CONTENT)$/i));
      const javaParams = params.map(p => toJavaName(p)).join(', ');
      return `${program}.execute(${javaParams});`;
    },
    description: 'Call variable subprogram with parameters',
  },
  {
    pattern: /CALL\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const program = toJavaName(match[1]!);
      return `${program}.execute();`;
    },
    description: 'Call variable subprogram',
  },
  // ON EXCEPTION / NOT ON EXCEPTION for CALL
  {
    pattern: /ON\s+EXCEPTION/gi,
    transform: () => 'catch (Exception e) {',
    description: 'On exception handler',
  },
  {
    pattern: /NOT\s+ON\s+EXCEPTION/gi,
    transform: () => '// NOT ON EXCEPTION: Success path',
    description: 'Not on exception (success)',
  },
  // END-CALL
  {
    pattern: /END-CALL/gi,
    transform: () => '}',
    description: 'End call block',
  },
  // OPEN statement (file I/O)
  {
    pattern: /OPEN\s+INPUT\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const file = toJavaName(match[1]!);
      return `${file}Reader = new BufferedReader(new FileReader(${file}Path));`;
    },
    description: 'Open file for input',
  },
  {
    pattern: /OPEN\s+OUTPUT\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const file = toJavaName(match[1]!);
      return `${file}Writer = new BufferedWriter(new FileWriter(${file}Path));`;
    },
    description: 'Open file for output',
  },
  {
    pattern: /OPEN\s+I-O\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const file = toJavaName(match[1]!);
      return `${file}Stream = new RandomAccessFile(${file}Path, "rw");`;
    },
    description: 'Open file for input/output',
  },
  // CLOSE statement (file I/O)
  {
    pattern: /CLOSE\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const file = toJavaName(match[1]!);
      return `${file}Reader.close(); // or ${file}Writer.close()`;
    },
    description: 'Close file',
  },
  // READ statement (file I/O)
  {
    pattern: /READ\s+(\w[\w-]*)\s+INTO\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const file = toJavaName(match[1]!);
      const record = toJavaName(match[2]!);
      return `${record} = ${file}Reader.readLine();`;
    },
    description: 'Read file into variable',
  },
  {
    pattern: /READ\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const file = toJavaName(match[1]!);
      return `${file}Record = ${file}Reader.readLine();`;
    },
    description: 'Read file',
  },
  // END-READ
  {
    pattern: /END-READ/gi,
    transform: () => '}',
    description: 'End read block',
  },
  // WRITE statement (file I/O)
  {
    pattern: /WRITE\s+(\w[\w-]*)\s+FROM\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const record = toJavaName(match[1]!);
      const source = toJavaName(match[2]!);
      return `${record}Writer.write(${source}); ${record}Writer.newLine();`;
    },
    description: 'Write to file from variable',
  },
  {
    pattern: /WRITE\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const record = toJavaName(match[1]!);
      return `${record}Writer.write(${record}); ${record}Writer.newLine();`;
    },
    description: 'Write record to file',
  },
  // END-WRITE
  {
    pattern: /END-WRITE/gi,
    transform: () => '}',
    description: 'End write block',
  },
  // REWRITE statement (file I/O)
  {
    pattern: /REWRITE\s+(\w[\w-]*)\s+FROM\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const record = toJavaName(match[1]!);
      const source = toJavaName(match[2]!);
      return `// REWRITE: Update record in place\n${record}Stream.seek(currentPosition); ${record}Stream.writeBytes(${source});`;
    },
    description: 'Rewrite record from variable',
  },
  // AT END clause for READ
  {
    pattern: /AT\s+END\s+(.+)/gi,
    transform: (match) => {
      const statement = transformStatement(match[1]!) || match[1];
      return `// AT END\nif (line == null) { ${statement} }`;
    },
    description: 'At end of file',
  },
  // NOT AT END clause for READ
  {
    pattern: /NOT\s+AT\s+END/gi,
    transform: () => '// NOT AT END: Record was read successfully',
    description: 'Not at end (record read)',
  },
  // GO TO statement (unconditional branch)
  {
    pattern: /GO\s+TO\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const target = toJavaName(match[1]!);
      return `// GO TO ${target} - converted to method call\n${target}(); return;`;
    },
    description: 'Go to paragraph',
  },
  // GO TO DEPENDING ON (computed goto)
  {
    pattern: /GO\s+TO\s+(.+)\s+DEPENDING\s+ON\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const targets = match[1]!.split(/\s+/).map(t => toJavaName(t));
      const selector = toJavaName(match[2]!);
      const cases = targets.map((t, i) => `case ${i + 1}: ${t}(); break;`).join('\n            ');
      return `switch (${selector}) {\n            ${cases}\n        }`;
    },
    description: 'Go to depending on',
  },
  // NEXT SENTENCE (legacy)
  {
    pattern: /NEXT\s+SENTENCE/gi,
    transform: () => '// NEXT SENTENCE - continue to next statement',
    description: 'Next sentence',
  },
  // SORT statement
  {
    pattern: /SORT\s+(\w[\w-]*)\s+(?:ON\s+)?(?:ASCENDING|DESCENDING)\s+(?:KEY\s+)?(\w[\w-]*)\s+USING\s+(\w[\w-]*)\s+GIVING\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const sortWork = toJavaName(match[1]!);
      const key = toJavaName(match[2]!);
      const input = toJavaName(match[3]!);
      const output = toJavaName(match[4]!);
      return `// SORT: ${sortWork}\nCollections.sort(${input}, Comparator.comparing(r -> r.${key}));\n${output} = ${input};`;
    },
    description: 'Sort file',
  },
  // MERGE statement
  {
    pattern: /MERGE\s+(\w[\w-]*)\s+(?:ON\s+)?(?:ASCENDING|DESCENDING)\s+(?:KEY\s+)?(\w[\w-]*)\s+USING\s+(.+?)\s+GIVING\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const mergeWork = toJavaName(match[1]!);
      const key = toJavaName(match[2]!);
      const inputs = match[3]!.split(/\s+/).map(i => toJavaName(i)).join(', ');
      const output = toJavaName(match[4]!);
      return `// MERGE: ${mergeWork} with key ${key}\n${output} = merge(${inputs}, Comparator.comparing(r -> r.${key}));`;
    },
    description: 'Merge files',
  },
  // START statement (indexed file positioning)
  {
    pattern: /START\s+(\w[\w-]*)\s+KEY\s+(?:IS\s+)?(?:EQUAL\s+TO|=|GREATER\s+THAN|>|NOT\s+LESS\s+THAN|>=)\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const file = toJavaName(match[1]!);
      const key = toJavaName(match[2]!);
      return `${file}.positionTo(${key}); // START - position indexed file`;
    },
    description: 'Start indexed file',
  },
  // DELETE statement (indexed/relative file)
  {
    pattern: /DELETE\s+(\w[\w-]*)\s+RECORD/gi,
    transform: (match) => {
      const file = toJavaName(match[1]!);
      return `${file}.deleteCurrentRecord();`;
    },
    description: 'Delete record from file',
  },
  // RELEASE statement (sort input)
  {
    pattern: /RELEASE\s+(\w[\w-]*)\s+FROM\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const record = toJavaName(match[1]!);
      const source = toJavaName(match[2]!);
      return `sortBuffer.add(${source}); // RELEASE ${record}`;
    },
    description: 'Release record to sort',
  },
  // RETURN statement (sort output)
  {
    pattern: /RETURN\s+(\w[\w-]*)\s+INTO\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const file = toJavaName(match[1]!);
      const target = toJavaName(match[2]!);
      return `${target} = sortBuffer.poll(); // RETURN from ${file}`;
    },
    description: 'Return record from sort',
  },
  // ROUNDED option
  {
    pattern: /(.+)\s+ROUNDED/gi,
    transform: (match) => {
      const statement = transformStatement(match[1]!) || match[1];
      return `${statement} // ROUNDED`;
    },
    description: 'Rounded arithmetic',
  },
  // ON SIZE ERROR
  {
    pattern: /ON\s+SIZE\s+ERROR\s+(.+)/gi,
    transform: (match) => {
      const handler = transformStatement(match[1]!) || match[1];
      return `// ON SIZE ERROR\ntry { /* arithmetic */ } catch (ArithmeticException e) { ${handler} }`;
    },
    description: 'On size error handler',
  },
  // NOT ON SIZE ERROR
  {
    pattern: /NOT\s+ON\s+SIZE\s+ERROR/gi,
    transform: () => '// NOT ON SIZE ERROR: Arithmetic succeeded',
    description: 'Not on size error',
  },
  // COPY statement (copybook)
  {
    pattern: /COPY\s+(\w[\w-]*)(?:\s+REPLACING\s+(.+))?/gi,
    transform: (match) => {
      const copybook = toJavaName(match[1]!);
      const replacing = match[2] ? ` // REPLACING ${match[2]}` : '';
      return `// COPY ${match[1]}${replacing} - include copybook`;
    },
    description: 'Copy copybook',
  },
  // ENTRY statement (alternate entry point)
  {
    pattern: /ENTRY\s+"([^"]+)"/gi,
    transform: (match) => {
      const entryName = match[1]!.toLowerCase().replace(/-/g, '');
      return `// ENTRY "${match[1]}"\npublic void ${entryName}() {`;
    },
    description: 'Entry point',
  },
  // CANCEL statement (unload program)
  {
    pattern: /CANCEL\s+"([^"]+)"/gi,
    transform: (match) => {
      const program = match[1]!.toLowerCase().replace(/-/g, '');
      return `${program} = null; // CANCEL - unload program`;
    },
    description: 'Cancel (unload) program',
  },
  // ENABLE/DISABLE (MQ/CICS)
  {
    pattern: /ENABLE\s+(\w+)/gi,
    transform: (match) => `// ENABLE ${match[1]}`,
    description: 'Enable facility',
  },
  {
    pattern: /DISABLE\s+(\w+)/gi,
    transform: (match) => `// DISABLE ${match[1]}`,
    description: 'Disable facility',
  },
  // USE statement (declaratives)
  {
    pattern: /USE\s+(?:GLOBAL\s+)?(?:AFTER\s+)?(?:STANDARD\s+)?(?:ERROR|EXCEPTION)\s+PROCEDURE\s+ON\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const file = toJavaName(match[1]!);
      return `// USE ERROR PROCEDURE ON ${match[1]} - file error handler`;
    },
    description: 'Use error procedure',
  },
  // TRANSFORM statement (legacy)
  {
    pattern: /TRANSFORM\s+(\w[\w-]*)\s+CHARACTERS\s+FROM\s+"([^"]+)"\s+TO\s+"([^"]+)"/gi,
    transform: (match) => {
      const target = toJavaName(match[1]!);
      const from = match[2];
      const to = match[3];
      return `// TRANSFORM (legacy)\nfor (int _i = 0; _i < "${from}".length(); _i++) { ${target} = ${target}.replace("${from}".charAt(_i), "${to}".charAt(_i)); }`;
    },
    description: 'Transform characters (legacy)',
  },
  // TALLYING clause for INSPECT
  {
    pattern: /INSPECT\s+(\w[\w-]*)\s+TALLYING\s+(\w[\w-]*)\s+FOR\s+(?:CHARACTERS|ALL|LEADING)\s*$/gi,
    transform: (match) => {
      const source = toJavaName(match[1]!);
      const counter = toJavaName(match[2]!);
      return `${counter} = ${source}.length(); // INSPECT TALLYING`;
    },
    description: 'Inspect tallying characters',
  },
  // INSPECT CONVERTING
  {
    pattern: /INSPECT\s+(\w[\w-]*)\s+CONVERTING\s+"([^"]+)"\s+TO\s+"([^"]+)"/gi,
    transform: (match) => {
      const target = toJavaName(match[1]!);
      const from = match[2];
      const to = match[3];
      return `// INSPECT CONVERTING\nfor (int _i = 0; _i < "${from}".length(); _i++) {\n    ${target} = ${target}.replace(String.valueOf("${from}".charAt(_i)), String.valueOf("${to}".charAt(_i)));\n}`;
    },
    description: 'Inspect converting',
  },
  // ALLOCATE statement (dynamic memory)
  {
    pattern: /ALLOCATE\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const target = toJavaName(match[1]!);
      return `${target} = new ${toClassName(match[1]!)}(); // ALLOCATE`;
    },
    description: 'Allocate memory',
  },
  // FREE statement (dynamic memory)
  {
    pattern: /FREE\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const target = toJavaName(match[1]!);
      return `${target} = null; // FREE`;
    },
    description: 'Free memory',
  },
  // JSON GENERATE (must be before GENERATE)
  {
    pattern: /JSON\s+GENERATE\s+(\w[\w-]*)\s+FROM\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const target = toJavaName(match[1]!);
      const source = toJavaName(match[2]!);
      return `${target} = new ObjectMapper().writeValueAsString(${source}); // JSON GENERATE`;
    },
    description: 'Generate JSON',
  },
  // JSON PARSE
  {
    pattern: /JSON\s+PARSE\s+(\w[\w-]*)\s+INTO\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const source = toJavaName(match[1]!);
      const target = toJavaName(match[2]!);
      return `${target} = new ObjectMapper().readValue(${source}, ${toClassName(match[2]!)}.class); // JSON PARSE`;
    },
    description: 'Parse JSON',
  },
  // XML GENERATE (must be before GENERATE)
  {
    pattern: /XML\s+GENERATE\s+(\w[\w-]*)\s+FROM\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const target = toJavaName(match[1]!);
      const source = toJavaName(match[2]!);
      return `${target} = xmlMapper.writeValueAsString(${source}); // XML GENERATE`;
    },
    description: 'Generate XML',
  },
  // XML PARSE
  {
    pattern: /XML\s+PARSE\s+(\w[\w-]*)\s+PROCESSING\s+PROCEDURE\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const source = toJavaName(match[1]!);
      const handler = toJavaName(match[2]!);
      return `// XML PARSE\nSAXParserFactory.newInstance().newSAXParser().parse(new InputSource(new StringReader(${source})), ${handler});`;
    },
    description: 'Parse XML with SAX',
  },
  // GENERATE statement (Report Writer)
  {
    pattern: /GENERATE\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const report = toJavaName(match[1]!);
      return `${report}.generate(); // GENERATE report line`;
    },
    description: 'Generate report line',
  },
  // INITIATE statement (Report Writer)
  {
    pattern: /INITIATE\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const report = toJavaName(match[1]!);
      return `${report}.initiate(); // INITIATE report`;
    },
    description: 'Initiate report',
  },
  // TERMINATE statement (Report Writer)
  {
    pattern: /TERMINATE\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const report = toJavaName(match[1]!);
      return `${report}.terminate(); // TERMINATE report`;
    },
    description: 'Terminate report',
  },
  // INVOKE statement (OO COBOL)
  {
    pattern: /INVOKE\s+(\w[\w-]*)\s+"([^"]+)"(?:\s+USING\s+(.+?))?(?:\s+RETURNING\s+(\w[\w-]*))?/gi,
    transform: (match) => {
      const obj = toJavaName(match[1]!);
      const method = match[2]!.toLowerCase().replace(/-/g, '');
      const params = match[3] ? match[3].split(/\s+/).map(p => toJavaName(p)).join(', ') : '';
      const ret = match[4] ? `${toJavaName(match[4])} = ` : '';
      return `${ret}${obj}.${method}(${params});`;
    },
    description: 'Invoke method (OO)',
  },
  // JSON GENERATE
  {
    pattern: /JSON\s+GENERATE\s+(\w[\w-]*)\s+FROM\s+(\w[\w-]*)/gi,
    transform: (match) => {
      const target = toJavaName(match[1]!);
      const source = toJavaName(match[2]!);
      return `${target} = new ObjectMapper().writeValueAsString(${source}); // JSON GENERATE`;
    },
    description: 'Generate JSON',
  },
  // EXEC SQL (embedded SQL)
  {
    pattern: /EXEC\s+SQL\s+(.+?)\s+END-EXEC/gi,
    transform: (match) => {
      const sql = match[1]!.trim();
      return `// EXEC SQL\nstatement.execute("${sql.replace(/"/g, '\\"')}");`;
    },
    description: 'Execute SQL',
  },
  // EXEC CICS
  {
    pattern: /EXEC\s+CICS\s+(.+?)\s+END-EXEC/gi,
    transform: (match) => {
      const cmd = match[1]!.trim();
      return `// EXEC CICS ${cmd}\ncicsTransaction.execute("${cmd}");`;
    },
    description: 'Execute CICS command',
  },
];

/**
 * COBOL Intrinsic Functions mapping to Java
 */
export const INTRINSIC_FUNCTIONS: Record<string, (args: string[]) => string> = {
  'LENGTH': (args) => `${args[0]}.length()`,
  'BYTE-LENGTH': (args) => `${args[0]}.getBytes().length`,
  'UPPER-CASE': (args) => `${args[0]}.toUpperCase()`,
  'LOWER-CASE': (args) => `${args[0]}.toLowerCase()`,
  'REVERSE': (args) => `new StringBuilder(${args[0]}).reverse().toString()`,
  'TRIM': (args) => args.length > 1 ? `${args[0]}.trim()` : `${args[0]}.trim()`,
  'NUMVAL': (args) => `Double.parseDouble(${args[0]}.trim())`,
  'NUMVAL-C': (args) => `Double.parseDouble(${args[0]}.trim().replace(",", "").replace("$", ""))`,
  'INTEGER': (args) => `(int) Math.floor(${args[0]})`,
  'INTEGER-PART': (args) => `(int) ${args[0]}`,
  'ABS': (args) => `Math.abs(${args[0]})`,
  'MAX': (args) => `Math.max(${args.join(', ')})`,
  'MIN': (args) => `Math.min(${args.join(', ')})`,
  'MOD': (args) => `${args[0]} % ${args[1]}`,
  'REM': (args) => `${args[0]} % ${args[1]}`,
  'SUM': (args) => args.length === 1 ? args[0]! : `(${args.join(' + ')})`,
  'SQRT': (args) => `Math.sqrt(${args[0]})`,
  'LOG': (args) => `Math.log(${args[0]})`,
  'LOG10': (args) => `Math.log10(${args[0]})`,
  'EXP': (args) => `Math.exp(${args[0]})`,
  'SIN': (args) => `Math.sin(${args[0]})`,
  'COS': (args) => `Math.cos(${args[0]})`,
  'TAN': (args) => `Math.tan(${args[0]})`,
  'ASIN': (args) => `Math.asin(${args[0]})`,
  'ACOS': (args) => `Math.acos(${args[0]})`,
  'ATAN': (args) => `Math.atan(${args[0]})`,
  'CURRENT-DATE': () => `java.time.LocalDateTime.now().format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmssSSnnnn"))`,
  'WHEN-COMPILED': () => `"${new Date().toISOString().replace(/[-:T.Z]/g, '').substring(0, 21)}"`,
  'DATE-OF-INTEGER': (args) => `java.time.LocalDate.ofEpochDay(${args[0]} - 1).format(java.time.format.DateTimeFormatter.BASIC_ISO_DATE)`,
  'INTEGER-OF-DATE': (args) => `(int) java.time.LocalDate.parse(String.valueOf(${args[0]}), java.time.format.DateTimeFormatter.BASIC_ISO_DATE).toEpochDay() + 1`,
  'DAY-OF-INTEGER': (args) => `java.time.LocalDate.ofEpochDay(${args[0]} - 1).format(java.time.format.DateTimeFormatter.ofPattern("yyyyDDD"))`,
  'INTEGER-OF-DAY': (args) => `(int) java.time.LocalDate.parse(String.valueOf(${args[0]}), java.time.format.DateTimeFormatter.ofPattern("yyyyDDD")).toEpochDay() + 1`,
  'ORD': (args) => `(int) ${args[0]}.charAt(0)`,
  'ORD-MAX': (args) => `IntStream.range(0, ${args.length}).boxed().max(Comparator.comparing(i -> ${args.map((a, i) => `i == ${i} ? ${a}`).join(' : ')})).orElse(0) + 1`,
  'ORD-MIN': (args) => `IntStream.range(0, ${args.length}).boxed().min(Comparator.comparing(i -> ${args.map((a, i) => `i == ${i} ? ${a}`).join(' : ')})).orElse(0) + 1`,
  'CHAR': (args) => `String.valueOf((char) ${args[0]})`,
  'CONCATENATE': (args) => args.join(' + '),
  'SUBSTITUTE': (args) => {
    if (args.length >= 3) {
      return `${args[0]}.replace(${args[1]}, ${args[2]})`;
    }
    return args[0]!;
  },
  'SUBSTITUTE-CASE': (args) => {
    if (args.length >= 3) {
      return `${args[0]}.replaceAll("(?i)" + ${args[1]}, ${args[2]})`;
    }
    return args[0]!;
  },
  'DISPLAY-OF': (args) => `${args[0]}`, // National to display
  'NATIONAL-OF': (args) => `${args[0]}`, // Display to national
  'RANDOM': (args) => args.length > 0 ? `new Random(${args[0]}).nextDouble()` : `Math.random()`,
  'MEDIAN': (args) => `getMedian(Arrays.asList(${args.join(', ')}))`,
  'MEAN': (args) => `(${args.join(' + ')}) / ${args.length}`,
  'VARIANCE': (args) => `getVariance(Arrays.asList(${args.join(', ')}))`,
  'STANDARD-DEVIATION': (args) => `Math.sqrt(getVariance(Arrays.asList(${args.join(', ')})))`,
  'RANGE': (args) => `(Math.max(${args.join(', ')}) - Math.min(${args.join(', ')}))`,
  'PRESENT-VALUE': (args) => `getPresentValue(${args[0]}, Arrays.asList(${args.slice(1).join(', ')}))`,
  'ANNUITY': (args) => `${args[0]} / (1 - Math.pow(1 + ${args[0]}, -${args[1]}))`,
  'TEST-NUMVAL': (args) => `isNumeric(${args[0]}) ? 0 : 1`,
  'TEST-NUMVAL-C': (args) => `isNumericCurrency(${args[0]}) ? 0 : 1`,
  // Additional commonly used functions
  'INSPECT': (args) => args[0]!, // Placeholder - complex statement
  'STRING': (args) => args.join(' + '), // String concatenation
  'UNSTRING': (args) => `${args[0]}.split("${args[1] || ","}")`, // String split
  'YEAR-TO-YYYY': (args) => {
    if (args.length >= 3) {
      return `convertYear(${args[0]}, ${args[1]}, ${args[2]})`;
    }
    return `(${args[0]} < 50 ? 2000 + ${args[0]} : 1900 + ${args[0]})`;
  },
  'DATE-TO-YYYYMMDD': (args) => `convertDateToYYYYMMDD(${args.join(', ')})`,
  'DAY-TO-YYYYDDD': (args) => `convertDayToYYYYDDD(${args.join(', ')})`,
  'SECONDS-FROM-FORMATTED-TIME': (args) => `java.time.LocalTime.parse(${args[1]}).toSecondOfDay()`,
  'SECONDS-PAST-MIDNIGHT': () => `java.time.LocalTime.now().toSecondOfDay()`,
  'FACTORIAL': (args) => `factorial(${args[0]})`,
  'E': () => `Math.E`,
  'PI': () => `Math.PI`,
  'SIGN': (args) => `(int) Math.signum(${args[0]})`,
  'FORMATTED-DATE': (args) => `formatDate(${args.join(', ')})`,
  'FORMATTED-TIME': (args) => `formatTime(${args.join(', ')})`,
  'FORMATTED-DATETIME': (args) => `formatDateTime(${args.join(', ')})`,
  'TEST-DATE-YYYYMMDD': (args) => `isValidDate(${args[0]}) ? 0 : 1`,
  'TEST-DAY-YYYYDDD': (args) => `isValidDayOfYear(${args[0]}) ? 0 : 1`,
  'LOCALE-DATE': (args) => `java.time.LocalDate.parse(${args[0]}).format(java.time.format.DateTimeFormatter.ofLocalizedDate(java.time.format.FormatStyle.MEDIUM))`,
  'LOCALE-TIME': (args) => `java.time.LocalTime.parse(${args[0]}).format(java.time.format.DateTimeFormatter.ofLocalizedTime(java.time.format.FormatStyle.MEDIUM))`,
  'BOOLEAN-OF-INTEGER': (args) => `${args[0]} != 0`,
  'INTEGER-OF-BOOLEAN': (args) => `${args[0]} ? 1 : 0`,
  'HEX-OF': (args) => `Integer.toHexString(${args[0]})`,
  'HEX-TO-CHAR': (args) => `String.valueOf((char) Integer.parseInt(${args[0]}, 16))`,
  'ULENGTH': (args) => `${args[0]}.codePointCount(0, ${args[0]}.length())`, // Unicode length
  'UPOS': (args) => `${args[0]}.offsetByCodePoints(0, ${args[1]})`, // Unicode position
  'USUBSTR': (args) => `${args[0]}.substring(${args[0]}.offsetByCodePoints(0, ${args[1]} - 1), ${args[0]}.offsetByCodePoints(0, ${args[1]} - 1 + ${args[2]}))`,
  'UVALID': (args) => `java.text.Normalizer.isNormalized(${args[0]}, java.text.Normalizer.Form.NFC) ? 0 : 1`,
  'USUPPLEMENTARY': (args) => `${args[0]}.codePointAt(${args[1]} - 1) > 0xFFFF ? 1 : 0`,
  'COMBINED-DATETIME': (args) => `java.time.LocalDateTime.of(java.time.LocalDate.parse(String.valueOf(${args[0]}), java.time.format.DateTimeFormatter.BASIC_ISO_DATE), java.time.LocalTime.ofSecondOfDay((long) ${args[1]}))`,
};

/**
 * Transform COBOL reference modification to Java substring
 * WS-FIELD(start:length) -> wsField.substring(start-1, start-1+length)
 */
export function transformReferenceModification(expr: string): string {
  // Pattern: IDENTIFIER(start:length) or IDENTIFIER(start:)
  const refModPattern = /(\w[\w-]*)\s*\(\s*(\d+|\w[\w-]*)\s*:\s*(\d+|\w[\w-]*)?\s*\)/gi;
  
  return expr.replace(refModPattern, (match, identifier, start, length) => {
    const javaVar = toJavaName(identifier);
    const javaStart = /^\d+$/.test(start) ? start : toJavaName(start);
    
    if (length === undefined || length === '') {
      // (start:) means from start to end
      return `${javaVar}.substring(${javaStart} - 1)`;
    }
    
    const javaLength = /^\d+$/.test(length) ? length : toJavaName(length);
    
    // Convert COBOL 1-based to Java 0-based
    if (/^\d+$/.test(javaStart) && /^\d+$/.test(javaLength)) {
      const startNum = parseInt(javaStart, 10);
      const lengthNum = parseInt(javaLength, 10);
      return `${javaVar}.substring(${startNum - 1}, ${startNum - 1 + lengthNum})`;
    }
    
    return `${javaVar}.substring(${javaStart} - 1, ${javaStart} - 1 + ${javaLength})`;
  });
}

/**
 * Transform COBOL intrinsic function to Java
 * FUNCTION LENGTH(WS-FIELD) -> wsField.length()
 * FUNCTION CURRENT-DATE -> LocalDateTime.now()...
 */
export function transformIntrinsicFunction(expr: string): string {
  // Pattern 1: FUNCTION FUNC-NAME(args) - with arguments
  const funcPatternWithArgs = /FUNCTION\s+([A-Z][A-Z0-9-]*)\s*\(([^)]*)\)/gi;
  
  let result = expr.replace(funcPatternWithArgs, (match, funcName, argsStr) => {
    const normalizedName = funcName.toUpperCase();
    const transformer = INTRINSIC_FUNCTIONS[normalizedName];
    
    if (!transformer) {
      return `/* FUNCTION ${funcName} not supported */ ${match}`;
    }
    
    // Parse arguments
    const args = argsStr
      .split(',')
      .map((arg: string) => arg.trim())
      .filter((arg: string) => arg.length > 0)
      .map((arg: string) => {
        // Transform variable names in args
        if (arg.startsWith('"') || arg.startsWith("'") || /^\d/.test(arg)) {
          return arg; // Literal
        }
        return toJavaName(arg);
      });
    
    return transformer(args);
  });
  
  // Pattern 2: FUNCTION FUNC-NAME (without arguments) - for functions like CURRENT-DATE
  const funcPatternNoArgs = /FUNCTION\s+([A-Z][A-Z0-9-]*)(?!\s*\()/gi;
  
  result = result.replace(funcPatternNoArgs, (match, funcName) => {
    const normalizedName = funcName.toUpperCase();
    const transformer = INTRINSIC_FUNCTIONS[normalizedName];
    
    if (!transformer) {
      return `/* FUNCTION ${funcName} not supported */ ${match}`;
    }
    
    return transformer([]);
  });
  
  return result;
}

/**
 * Transform COBOL expression to Java expression
 */
export function transformExpression(cobolExpr: string): string {
  let expr = cobolExpr.trim();
  
  // Transform intrinsic functions first
  expr = transformIntrinsicFunction(expr);
  
  // Transform reference modifications
  expr = transformReferenceModification(expr);
  
  // Replace COBOL operators with Java operators
  expr = expr.replace(/\*\*/g, '^'); // Power (will need Math.pow)
  expr = expr.replace(/\bAND\b/gi, '&&');
  expr = expr.replace(/\bOR\b/gi, '||');
  expr = expr.replace(/\bNOT\b/gi, '!');
  
  // Convert variable names (but not already converted ones)
  // Skip Java qualified names (contain dots), Java keywords, and already camelCase names
  expr = expr.replace(/\b([A-Z][A-Z0-9-]*[A-Z0-9])\b/g, (match, group, offset, fullStr) => {
    if (/^\d/.test(match)) return match; // Skip numbers
    // Skip if it looks like a Java method call or already camelCase
    if (/^[a-z]/.test(match)) return match;
    // Skip if preceded by a dot (part of Java qualified name like java.time.LocalDateTime)
    if (offset > 0 && fullStr[offset - 1] === '.') return match;
    // Skip if followed by a dot (part of Java qualified name)
    if (fullStr[offset + match.length] === '.') return match;
    return toJavaName(match);
  });
  
  // Handle power function
  if (expr.includes('^')) {
    expr = expr.replace(/(\w+)\s*\^\s*(\w+)/g, 'Math.pow($1, $2)');
  }
  
  return expr;
}

/**
 * Transform a COBOL statement to Java
 */
export function transformStatement(cobolStatement: string): string | null {
  const trimmed = cobolStatement.trim();
  
  for (const rule of STATEMENT_RULES) {
    // Reset regex lastIndex for global patterns
    rule.pattern.lastIndex = 0;
    const match = rule.pattern.exec(trimmed);
    if (match) {
      return rule.transform(match);
    }
  }
  
  return null; // No matching rule
}

/**
 * IF statement structure
 */
export interface IfBlock {
  condition: string;
  thenStatements: string[];
  elseStatements?: string[];
}

/**
 * Parse COBOL IF statement
 */
export function parseIfStatement(lines: string[]): IfBlock | null {
  const ifLine = lines[0]?.trim();
  if (!ifLine?.toUpperCase().startsWith('IF ')) {
    return null;
  }
  
  // Extract condition
  const conditionMatch = ifLine.match(/IF\s+(.+?)(?:\s+THEN)?$/i);
  if (!conditionMatch) return null;
  
  const condition = transformCondition(conditionMatch[1]!);
  const thenStatements: string[] = [];
  const elseStatements: string[] = [];
  
  let inElse = false;
  
  for (let i = 1; i < lines.length; i++) {
    const line = lines[i]?.trim().toUpperCase() ?? '';
    
    if (line === 'ELSE') {
      inElse = true;
      continue;
    }
    if (line === 'END-IF' || line === 'END-IF.') {
      break;
    }
    
    const transformed = transformStatement(lines[i]!);
    if (transformed) {
      if (inElse) {
        elseStatements.push(transformed);
      } else {
        thenStatements.push(transformed);
      }
    }
  }
  
  return { condition, thenStatements, elseStatements };
}

/**
 * Context for 88-level condition lookups
 * Should be set during transformation with data items from parser
 */
let level88Context: Map<string, { parentName: string; values: string[] }> = new Map();

/**
 * Set the 88-level condition context for transformations
 */
export function setLevel88Context(dataItems: Array<{ level: number; name: string; values?: string[] }>): void {
  level88Context = new Map();
  let currentParent: string | undefined;
  
  for (const item of dataItems) {
    if (item.level !== 88) {
      currentParent = item.name;
    } else if (currentParent && item.values) {
      level88Context.set(item.name.toUpperCase(), {
        parentName: currentParent,
        values: item.values,
      });
    }
  }
}

/**
 * Transform 88-level condition name to Java condition
 */
export function transform88LevelCondition(conditionName: string): string | null {
  const upper = conditionName.toUpperCase().replace(/-/g, '-');
  const condInfo = level88Context.get(upper);
  
  if (!condInfo) {
    return null;
  }
  
  const javaVar = toJavaName(condInfo.parentName);
  const values = condInfo.values;
  
  if (values.length === 0) {
    return null;
  }
  
  if (values.length === 1) {
    const val = values[0]!;
    // Check if it's a range (from...to format)
    if (val.includes('...')) {
      const [from, to] = val.split('...');
      // Range comparison
      if (/^\d+$/.test(from!) && /^\d+$/.test(to!)) {
        return `(${javaVar} >= ${from} && ${javaVar} <= ${to})`;
      }
      // Character range
      return `(${javaVar}.compareTo("${from}") >= 0 && ${javaVar}.compareTo("${to}") <= 0)`;
    }
    // Single value
    if (/^\d+$/.test(val)) {
      return `${javaVar} == ${val}`;
    }
    return `${javaVar}.equals("${val}")`;
  }
  
  // Multiple values - build OR conditions
  const conditions = values.map(val => {
    if (val.includes('...')) {
      const [from, to] = val.split('...');
      if (/^\d+$/.test(from!) && /^\d+$/.test(to!)) {
        return `(${javaVar} >= ${from} && ${javaVar} <= ${to})`;
      }
      return `(${javaVar}.compareTo("${from}") >= 0 && ${javaVar}.compareTo("${to}") <= 0)`;
    }
    if (/^\d+$/.test(val)) {
      return `${javaVar} == ${val}`;
    }
    return `${javaVar}.equals("${val}")`;
  });
  
  return `(${conditions.join(' || ')})`;
}

/**
 * Transform COBOL condition to Java condition
 */
export function transformCondition(cobolCondition: string): string {
  let cond = cobolCondition.trim();
  
  // Check for 88-level condition name (single identifier that might be a condition)
  const singleIdMatch = cond.match(/^([A-Z][\w-]*)$/i);
  if (singleIdMatch) {
    const level88Result = transform88LevelCondition(singleIdMatch[1]!);
    if (level88Result) {
      return level88Result;
    }
  }
  
  // Check for NOT condition-name
  const notCondMatch = cond.match(/^NOT\s+([A-Z][\w-]*)$/i);
  if (notCondMatch) {
    const level88Result = transform88LevelCondition(notCondMatch[1]!);
    if (level88Result) {
      return `!(${level88Result})`;
    }
  }
  
  // Replace comparison operators (order matters - more specific patterns first)
  cond = cond.replace(/\bIS\s+NOT\s+EQUAL\s+TO\b/gi, '!=');
  cond = cond.replace(/\bNOT\s+EQUAL\s+TO\b/gi, '!=');
  cond = cond.replace(/\bIS\s+EQUAL\s+TO\b/gi, '==');
  cond = cond.replace(/\bEQUAL\s+TO\b/gi, '==');
  cond = cond.replace(/\bEQUALS\b/gi, '==');
  cond = cond.replace(/\bGREATER\s+THAN\s+OR\s+EQUAL\s+TO\b/gi, '>=');
  cond = cond.replace(/\bLESS\s+THAN\s+OR\s+EQUAL\s+TO\b/gi, '<=');
  cond = cond.replace(/\bIS\s+GREATER\s+THAN\b/gi, '>');
  cond = cond.replace(/\bGREATER\s+THAN\b/gi, '>');
  cond = cond.replace(/\bIS\s+LESS\s+THAN\b/gi, '<');
  cond = cond.replace(/\bLESS\s+THAN\b/gi, '<');
  cond = cond.replace(/\bIS\s+NUMERIC\b/gi, '.matches("\\\\d+")');
  
  // Replace logical operators (after comparison operators)
  cond = cond.replace(/\bAND\b/gi, '&&');
  cond = cond.replace(/\bOR\b/gi, '||');
  // Note: NOT is tricky - only replace standalone NOT, not part of "NOT EQUAL"
  cond = cond.replace(/\bNOT\s+(?!equal|=)/gi, '!');
  
  // Convert variable names
  cond = cond.replace(/\b([A-Z][A-Z0-9-]+)\b/gi, (match) => {
    if (/^\d/.test(match)) return match;
    return toJavaName(match);
  });
  
  return cond;
}
