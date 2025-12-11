/**
 * COBOL Parser
 * 
 * Parses COBOL source code into an AST
 */

import { type ErrorInfo, createError, ErrorSeverity } from './errors.js';

/**
 * COBOL AST node types
 */
export type CobolNodeType =
  | 'program'
  | 'identification_division'
  | 'environment_division'
  | 'data_division'
  | 'procedure_division'
  | 'section'
  | 'paragraph'
  | 'statement'
  | 'data_item'
  | 'file_description'
  | 'literal'
  | 'identifier';

/**
 * Data item definition
 */
export interface DataItem {
  level: number;
  name: string;
  pic?: string;
  value?: string;
  values?: string[]; // For 88-level conditions with THRU/THROUGH
  occurs?: number;
  usage?: string;
  redefines?: string;
  indexed?: string[];
  key?: string;
  parentName?: string; // For 88-level conditions
}

/**
 * File definition from ENVIRONMENT and DATA divisions
 */
export interface FileDefinition {
  selectName: string;           // SELECT file-name
  assignTo: string;             // ASSIGN TO target
  organization?: 'SEQUENTIAL' | 'INDEXED' | 'RELATIVE' | 'LINE SEQUENTIAL';
  accessMode?: 'SEQUENTIAL' | 'RANDOM' | 'DYNAMIC';
  recordKey?: string;           // RECORD KEY for indexed files
  alternateKeys?: string[];     // ALTERNATE RECORD KEY
  fileStatus?: string;          // FILE STATUS variable
  fdName?: string;              // FD name
  recordName?: string;          // Record layout name
  recordSize?: number | { min: number; max: number };
  blockSize?: number;
  labelRecords?: 'STANDARD' | 'OMITTED';
}

/**
 * Condition name (88-level) mapping
 */
export interface ConditionName {
  name: string;
  parentName: string;
  values: string[];
  thruValues?: Array<{ from: string; to: string }>;
}

/**
 * Paragraph definition
 */
export interface Paragraph {
  name: string;
  statements: string[];
  startLine: number;
}

/**
 * COBOL AST node
 */
export interface CobolNode {
  type: CobolNodeType;
  name?: string;
  value?: string;
  children?: CobolNode[];
  startLine?: number;
  endLine?: number;
  raw?: string;
}

/**
 * COBOL AST root
 */
export interface CobolAst {
  type: 'program';
  programName?: string;
  identificationDivision?: CobolNode;
  environmentDivision?: CobolNode;
  dataDivision?: CobolNode;
  procedureDivision?: CobolNode;
  dataItems: DataItem[];
  paragraphs: Paragraph[];
  fileDefinitions: FileDefinition[];
  errors: ErrorInfo[];
  raw: string;
}

/**
 * Parse result
 */
export interface ParseResult {
  ast: CobolAst;
  errors: ErrorInfo[];
}

/**
 * COBOL Parser
 * 
 * @remarks
 * Enhanced implementation with data division and procedure division parsing.
 */
export class CobolParser {
  /**
   * Parse COBOL source code
   * 
   * @param source - COBOL source code
   * @returns COBOL AST
   */
  parse(source: string): CobolAst {
    const errors: ErrorInfo[] = [];
    const lines = source.split('\n');
    const dataItems: DataItem[] = [];
    const paragraphs: Paragraph[] = [];
    const fileDefinitions: FileDefinition[] = [];
    
    let programName: string | undefined;
    let hasIdentification = false;
    let hasProcedure = false;
    let currentSection: 'identification' | 'environment' | 'data' | 'procedure' | null = null;
    let currentSubSection: 'file-control' | 'working-storage' | 'file' | null = null;
    let currentParagraph: Paragraph | null = null;
    let currentFileDefinition: Partial<FileDefinition> | null = null;
    let selectBuffer = '';  // Buffer for multi-line SELECT statements

    for (let i = 0; i < lines.length; i++) {
      const rawLine = lines[i] ?? '';
      const line = this.cleanLine(rawLine);
      
      // Skip comments and empty lines
      if (line.startsWith('*') || line === '') {
        continue;
      }

      const upperLine = line.toUpperCase();

      // Detect divisions
      if (upperLine.includes('IDENTIFICATION DIVISION')) {
        hasIdentification = true;
        currentSection = 'identification';
        continue;
      }
      if (upperLine.includes('ENVIRONMENT DIVISION')) {
        currentSection = 'environment';
        currentSubSection = null;
        continue;
      }
      if (upperLine.includes('FILE-CONTROL')) {
        currentSubSection = 'file-control';
        continue;
      }
      if (upperLine.includes('DATA DIVISION')) {
        currentSection = 'data';
        currentSubSection = null;
        continue;
      }
      if (upperLine.includes('FILE SECTION')) {
        currentSubSection = 'file';
        continue;
      }
      if (upperLine.includes('WORKING-STORAGE SECTION')) {
        currentSubSection = 'working-storage';
        continue;
      }
      if (upperLine.includes('PROCEDURE DIVISION')) {
        hasProcedure = true;
        currentSection = 'procedure';
        currentSubSection = null;
        continue;
      }

      // Parse based on current section
      if (currentSection === 'identification') {
        const programIdMatch = upperLine.match(/PROGRAM-ID\.\s*(\w[\w-]*)/);
        if (programIdMatch) {
          programName = programIdMatch[1];
        }
      }

      // Parse ENVIRONMENT DIVISION - FILE-CONTROL
      if (currentSection === 'environment' && currentSubSection === 'file-control') {
        // Buffer SELECT statements (can span multiple lines)
        if (upperLine.includes('SELECT') && !selectBuffer) {
          selectBuffer = line;
        } else if (selectBuffer) {
          selectBuffer += ' ' + line;
        }
        
        // Parse complete SELECT statement (ends with period)
        if (selectBuffer && selectBuffer.trim().endsWith('.')) {
          const fileDef = this.parseSelectStatement(selectBuffer);
          if (fileDef) {
            fileDefinitions.push(fileDef as FileDefinition);
          }
          selectBuffer = '';
        }
      }

      if (currentSection === 'data') {
        // Parse FD statements in FILE SECTION
        if (currentSubSection === 'file' && upperLine.match(/^\s*FD\s+/)) {
          this.parseFdStatement(line, fileDefinitions);
        }
        
        const dataItem = this.parseDataItem(line);
        if (dataItem) {
          dataItems.push(dataItem);
        }
      }

      if (currentSection === 'procedure') {
        // Check for END-* statements first (they are not paragraphs)
        if (upperLine.match(/^END-(?:PERFORM|IF|EVALUATE|READ|WRITE)/)) {
          if (currentParagraph && line.trim()) {
            currentParagraph.statements.push(line.trim());
          }
          continue;
        }
        
        // Check for paragraph (line ending with period, no keywords)
        const paragraphMatch = upperLine.match(/^(\w[\w-]*)\s*\.?\s*$/);
        if (paragraphMatch && !this.isKeyword(paragraphMatch[1]!)) {
          if (currentParagraph) {
            paragraphs.push(currentParagraph);
          }
          currentParagraph = {
            name: paragraphMatch[1]!,
            statements: [],
            startLine: i + 1,
          };
        } else if (currentParagraph && line.trim()) {
          // Add statement to current paragraph
          // Join continuation lines (COBOL statements end with period)
          const trimmedLine = line.trim();
          const lastStatement = currentParagraph.statements.length > 0
            ? currentParagraph.statements[currentParagraph.statements.length - 1]
            : null;
          
          // Check if this line starts with a COBOL verb (new statement)
          const isNewStatement = upperLine.match(/^(MOVE|ADD|SUBTRACT|MULTIPLY|DIVIDE|COMPUTE|IF|ELSE|PERFORM|DISPLAY|ACCEPT|READ|WRITE|OPEN|CLOSE|STOP|GO|EXIT|CALL|EVALUATE|WHEN|END-EVALUATE|STRING|UNSTRING|INITIALIZE|SEARCH|INSPECT|SET|CONTINUE)/);
          
          // If last statement doesn't end with period and this isn't a new statement,
          // join with the previous line
          if (lastStatement && !lastStatement.endsWith('.') && !isNewStatement) {
            currentParagraph.statements[currentParagraph.statements.length - 1] = 
              lastStatement + ' ' + trimmedLine;
          } else {
            currentParagraph.statements.push(trimmedLine);
          }
        }
      }
    }

    // Add last paragraph
    if (currentParagraph) {
      paragraphs.push(currentParagraph);
    }

    // Validate basic structure
    if (!hasIdentification) {
      errors.push(
        createError('CVT001', 'Missing IDENTIFICATION DIVISION', {
          severity: ErrorSeverity.WARNING,
          line: 1,
          suggestion: 'Add IDENTIFICATION DIVISION at the beginning of the program',
        })
      );
    }

    if (!hasProcedure) {
      errors.push(
        createError('CVT002', 'Missing PROCEDURE DIVISION', {
          severity: ErrorSeverity.WARNING,
          line: 1,
          suggestion: 'Add PROCEDURE DIVISION for executable code',
        })
      );
    }

    return {
      type: 'program',
      programName,
      dataItems,
      paragraphs,
      fileDefinitions,
      errors,
      raw: source,
    };
  }

  /**
   * Parse SELECT...ASSIGN statement
   * Example: SELECT CUSTOMER-FILE ASSIGN TO "CUSTFILE"
   *          ORGANIZATION IS INDEXED
   *          ACCESS MODE IS DYNAMIC
   *          RECORD KEY IS CUST-ID
   *          FILE STATUS IS WS-FILE-STATUS.
   */
  private parseSelectStatement(statement: string): Partial<FileDefinition> | null {
    const upperStatement = statement.toUpperCase();
    
    // Extract SELECT file-name
    const selectMatch = upperStatement.match(/SELECT\s+(\w[\w-]*)/);
    if (!selectMatch) return null;
    
    const fileDef: Partial<FileDefinition> = {
      selectName: selectMatch[1]!,
    };
    
    // ASSIGN TO
    const assignMatch = statement.match(/ASSIGN\s+(?:TO\s+)?["']?(\w[\w.-]*)["']?/i);
    if (assignMatch) {
      fileDef.assignTo = assignMatch[1]!;
    }
    
    // ORGANIZATION
    const orgMatch = upperStatement.match(/ORGANIZATION\s+(?:IS\s+)?(SEQUENTIAL|INDEXED|RELATIVE|LINE\s+SEQUENTIAL)/);
    if (orgMatch) {
      fileDef.organization = orgMatch[1]!.replace(/\s+/g, ' ') as FileDefinition['organization'];
    }
    
    // ACCESS MODE
    const accessMatch = upperStatement.match(/ACCESS\s+(?:MODE\s+)?(?:IS\s+)?(SEQUENTIAL|RANDOM|DYNAMIC)/);
    if (accessMatch) {
      fileDef.accessMode = accessMatch[1] as FileDefinition['accessMode'];
    }
    
    // RECORD KEY
    const keyMatch = upperStatement.match(/RECORD\s+KEY\s+(?:IS\s+)?(\w[\w-]*)/);
    if (keyMatch) {
      fileDef.recordKey = keyMatch[1];
    }
    
    // ALTERNATE RECORD KEY(s)
    const altKeyPattern = /ALTERNATE\s+RECORD\s+KEY\s+(?:IS\s+)?(\w[\w-]*)/gi;
    const altKeys: string[] = [];
    let altMatch;
    while ((altMatch = altKeyPattern.exec(upperStatement)) !== null) {
      altKeys.push(altMatch[1]!);
    }
    if (altKeys.length > 0) {
      fileDef.alternateKeys = altKeys;
    }
    
    // FILE STATUS
    const statusMatch = upperStatement.match(/FILE\s+STATUS\s+(?:IS\s+)?(\w[\w-]*)/);
    if (statusMatch) {
      fileDef.fileStatus = statusMatch[1];
    }
    
    return fileDef;
  }

  /**
   * Parse FD (File Description) statement
   * Example: FD CUSTOMER-FILE
   *          BLOCK CONTAINS 0 RECORDS
   *          RECORD CONTAINS 100 CHARACTERS
   *          LABEL RECORDS ARE STANDARD.
   */
  private parseFdStatement(line: string, fileDefinitions: FileDefinition[]): void {
    const upperLine = line.toUpperCase();
    
    // Extract FD file-name
    const fdMatch = upperLine.match(/FD\s+(\w[\w-]*)/);
    if (!fdMatch) return;
    
    const fdName = fdMatch[1]!;
    
    // Find matching SELECT statement and update
    const existingDef = fileDefinitions.find(
      fd => fd.selectName?.toUpperCase() === fdName.toUpperCase()
    );
    
    if (existingDef) {
      existingDef.fdName = fdName;
      
      // BLOCK CONTAINS
      const blockMatch = upperLine.match(/BLOCK\s+CONTAINS\s+(\d+)/);
      if (blockMatch) {
        existingDef.blockSize = parseInt(blockMatch[1]!, 10);
      }
      
      // RECORD CONTAINS (fixed or variable)
      const recordMatch = upperLine.match(/RECORD\s+CONTAINS\s+(?:(\d+)\s+TO\s+)?(\d+)/);
      if (recordMatch) {
        if (recordMatch[1]) {
          existingDef.recordSize = {
            min: parseInt(recordMatch[1], 10),
            max: parseInt(recordMatch[2]!, 10),
          };
        } else {
          existingDef.recordSize = parseInt(recordMatch[2]!, 10);
        }
      }
      
      // LABEL RECORDS
      const labelMatch = upperLine.match(/LABEL\s+RECORDS?\s+(?:ARE?\s+)?(STANDARD|OMITTED)/);
      if (labelMatch) {
        existingDef.labelRecords = labelMatch[1] as 'STANDARD' | 'OMITTED';
      }
    }
  }

  /**
   * Clean a COBOL source line
   */
  private cleanLine(line: string): string {
    // Remove sequence number area (columns 1-6) if present
    if (line.length > 6 && /^\d{6}/.test(line)) {
      line = line.substring(6);
    }
    // Remove indicator area (column 7) comments
    if (line.length > 0 && line[0] === '*') {
      return '';
    }
    return line.trim();
  }

  /**
   * Parse a data item definition
   */
  private parseDataItem(line: string): DataItem | null {
    const upperLine = line.toUpperCase();
    
    // Parse 88-level condition names first
    // Pattern: 88 CONDITION-NAME VALUE 'Y' or VALUE 'A' THRU 'Z'.
    const level88Match = upperLine.match(
      /^88\s+(\w[\w-]*)\s+VALUES?\s+(.+?)\.?\s*$/i
    );
    
    if (level88Match) {
      const dataItem: DataItem = {
        level: 88,
        name: level88Match[1]!,
        values: this.parseLevel88Values(level88Match[2]!),
      };
      return dataItem;
    }
    
    // Try full pattern with PIC and VALUE
    // Pattern: 01 WS-NAME PIC X(10) VALUE "TEST".
    // Allow decimal points in VALUE by using a different approach
    const fullMatch = upperLine.match(
      /^(\d{1,2})\s+(\w[\w-]*)\s+PIC(?:TURE)?\s+(\S+)\s+VALUE\s+(.+?)\.?\s*$/i
    );
    
    if (fullMatch) {
      const dataItem: DataItem = {
        level: parseInt(fullMatch[1]!, 10),
        name: fullMatch[2]!,
        pic: fullMatch[3]!,
      };
      
      // Clean up value - remove trailing period and quotes
      let value = fullMatch[4]!.trim();
      if (value.endsWith('.')) {
        value = value.slice(0, -1);
      }
      value = value.replace(/^["']|["']$/g, '');
      dataItem.value = value;
      
      // Check for OCCURS
      const occursMatch = upperLine.match(/OCCURS\s+(\d+)/i);
      if (occursMatch) {
        dataItem.occurs = parseInt(occursMatch[1]!, 10);
      }
      
      // Check for REDEFINES
      const redefinesMatch = upperLine.match(/REDEFINES\s+(\w[\w-]*)/i);
      if (redefinesMatch) {
        dataItem.redefines = redefinesMatch[1];
      }
      
      return dataItem;
    }
    
    // Try pattern with REDEFINES
    const redefinesMatch = upperLine.match(
      /^(\d{1,2})\s+(\w[\w-]*)\s+REDEFINES\s+(\w[\w-]*)\s*(?:PIC(?:TURE)?\s+(\S+))?\.?\s*$/i
    );
    
    if (redefinesMatch) {
      const dataItem: DataItem = {
        level: parseInt(redefinesMatch[1]!, 10),
        name: redefinesMatch[2]!,
        redefines: redefinesMatch[3],
      };
      if (redefinesMatch[4]) {
        dataItem.pic = redefinesMatch[4];
      }
      return dataItem;
    }
    
    // Try pattern with OCCURS and INDEXED BY
    const occursIndexedMatch = upperLine.match(
      /^(\d{1,2})\s+(\w[\w-]*)\s+(?:PIC(?:TURE)?\s+(\S+)\s+)?OCCURS\s+(\d+)(?:\s+TIMES)?(?:\s+INDEXED\s+BY\s+(\w[\w-]*))?(?:\s+ASCENDING\s+KEY\s+(?:IS\s+)?(\w[\w-]*))?\.?\s*$/i
    );
    
    if (occursIndexedMatch) {
      const dataItem: DataItem = {
        level: parseInt(occursIndexedMatch[1]!, 10),
        name: occursIndexedMatch[2]!,
        occurs: parseInt(occursIndexedMatch[4]!, 10),
      };
      if (occursIndexedMatch[3]) {
        dataItem.pic = occursIndexedMatch[3];
      }
      if (occursIndexedMatch[5]) {
        dataItem.indexed = [occursIndexedMatch[5]];
      }
      if (occursIndexedMatch[6]) {
        dataItem.key = occursIndexedMatch[6];
      }
      return dataItem;
    }
    
    // Try pattern with PIC but no VALUE
    const picOnlyMatch = upperLine.match(
      /^(\d{1,2})\s+(\w[\w-]*)\s+PIC(?:TURE)?\s+(\S+?)\.?\s*$/i
    );
    
    if (picOnlyMatch) {
      return {
        level: parseInt(picOnlyMatch[1]!, 10),
        name: picOnlyMatch[2]!,
        pic: picOnlyMatch[3]!,
      };
    }
    
    // Try simpler pattern: 01 FILLER or 01 WS-GROUP
    const simpleMatch = upperLine.match(/^(\d{1,2})\s+(\w[\w-]*)\s*\.?$/);
    if (simpleMatch) {
      return {
        level: parseInt(simpleMatch[1]!, 10),
        name: simpleMatch[2]!,
      };
    }
    
    return null;
  }

  /**
   * Check if a word is a COBOL keyword
   */
  private isKeyword(word: string): boolean {
    const keywords = [
      'IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE',
      'DIVISION', 'SECTION', 'WORKING-STORAGE', 'LINKAGE',
      'FILE', 'INPUT-OUTPUT', 'FILE-CONTROL', 'SELECT',
      'IF', 'ELSE', 'END-IF', 'PERFORM', 'MOVE', 'ADD',
      'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'COMPUTE',
      'DISPLAY', 'ACCEPT', 'STOP', 'GOBACK',
    ];
    return keywords.includes(word.toUpperCase());
  }

  /**
   * Parse 88-level condition values
   * Handles: VALUE 'A' 'B' 'C', VALUE 'A' THRU 'Z', VALUE 1 2 3
   */
  private parseLevel88Values(valueStr: string): string[] {
    const values: string[] = [];
    
    // Handle THRU/THROUGH ranges
    const thruPattern = /(?:['"]([^'"]+)['"]|(\d+))\s+(?:THRU|THROUGH)\s+(?:['"]([^'"]+)['"]|(\d+))/gi;
    let thruMatch;
    let processedStr = valueStr;
    
    while ((thruMatch = thruPattern.exec(valueStr)) !== null) {
      const fromVal = thruMatch[1] || thruMatch[2];
      const toVal = thruMatch[3] || thruMatch[4];
      values.push(`${fromVal}...${toVal}`); // Range notation
      processedStr = processedStr.replace(thruMatch[0], '');
    }
    
    // Handle individual values (quoted strings or numbers)
    const valuePattern = /['"]([^'"]+)['"]|(\d+)/g;
    let match;
    while ((match = valuePattern.exec(processedStr)) !== null) {
      values.push(match[1] || match[2]!);
    }
    
    return values;
  }
}
