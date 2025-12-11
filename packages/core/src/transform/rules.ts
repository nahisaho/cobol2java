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
];

/**
 * Transform COBOL expression to Java expression
 */
export function transformExpression(cobolExpr: string): string {
  let expr = cobolExpr.trim();
  
  // Replace COBOL operators with Java operators
  expr = expr.replace(/\*\*/g, '^'); // Power (will need Math.pow)
  expr = expr.replace(/\bAND\b/gi, '&&');
  expr = expr.replace(/\bOR\b/gi, '||');
  expr = expr.replace(/\bNOT\b/gi, '!');
  
  // Convert variable names
  expr = expr.replace(/\b([A-Z][A-Z0-9-]*[A-Z0-9])\b/gi, (match) => {
    if (/^\d/.test(match)) return match; // Skip numbers
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
 * Transform COBOL condition to Java condition
 */
export function transformCondition(cobolCondition: string): string {
  let cond = cobolCondition.trim();
  
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
