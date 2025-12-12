/**
 * IMS (Information Management System) Support for COBOL
 * 
 * Handles IMS DL/I calls and converts to modern data access patterns
 */

/**
 * IMS DL/I function codes
 */
export type DliFunction = 
  | 'GU'   // Get Unique
  | 'GN'   // Get Next
  | 'GNP'  // Get Next within Parent
  | 'GHU'  // Get Hold Unique
  | 'GHN'  // Get Hold Next
  | 'GHNP' // Get Hold Next within Parent
  | 'ISRT' // Insert
  | 'DLET' // Delete
  | 'REPL' // Replace
  | 'CHKP' // Checkpoint
  | 'XRST' // Extended Restart
  | 'SYNC' // Sync Point
  | 'ROLB' // Rollback
  | 'ROLL' // Roll
  | 'PCB'  // PCB Call
  | 'TERM' // Terminate;

/**
 * IMS PCB (Program Communication Block)
 */
export interface ImsPcb {
  name: string;
  dbdName?: string;
  type: 'DB' | 'TP' | 'GSAM' | 'ALT';
  segments: ImsSegment[];
}

/**
 * IMS Segment definition
 */
export interface ImsSegment {
  name: string;
  parentName?: string;
  sequenceField?: string;
  fields: ImsField[];
}

/**
 * IMS Field definition
 */
export interface ImsField {
  name: string;
  type: 'C' | 'P' | 'X' | 'Z'; // Character, Packed, Hex, Zoned
  start: number;
  length: number;
}

/**
 * IMS DL/I Call
 */
export interface DliCall {
  function: DliFunction;
  pcbName: string;
  segmentName?: string;
  ioArea?: string;
  ssa?: ImsSsa[];
}

/**
 * IMS SSA (Segment Search Argument)
 */
export interface ImsSsa {
  segmentName: string;
  commandCodes?: string[];
  qualification?: {
    fieldName: string;
    operator: '=' | '>' | '<' | '>=' | '<=' | '!=';
    value: string;
  };
}

/**
 * IMS DL/I Parser
 */
export class ImsParser {
  private pcbs = new Map<string, ImsPcb>();

  /**
   * Parse CBLTDLI or AIBTDLI call
   */
  parseDliCall(statement: string): DliCall | null {
    // CALL 'CBLTDLI' USING function, pcb, io-area, ssa...
    const match = statement.match(
      /CALL\s+['"]?(?:CBLTDLI|AIBTDLI)['"]?\s+USING\s+(.+)/i
    );

    if (!match) return null;

    const args = this.parseArguments(match[1]!);
    if (args.length < 2) return null;

    const func = this.parseFunctionCode(args[0]!);
    const pcbName = args[1]!;

    const call: DliCall = {
      function: func,
      pcbName,
    };

    // Parse IO area if present
    if (args.length > 2 && !this.isSSA(args[2]!)) {
      call.ioArea = args[2];
    }

    // Parse SSAs
    const ssaStart = call.ioArea ? 3 : 2;
    call.ssa = [];
    for (let i = ssaStart; i < args.length; i++) {
      const ssa = this.parseSSA(args[i]!);
      if (ssa) call.ssa.push(ssa);
    }

    return call;
  }

  /**
   * Parse function code
   */
  private parseFunctionCode(code: string): DliFunction {
    // Remove quotes, trim, and handle common variable naming patterns
    let upper = code.toUpperCase().replace(/['"]/g, '').trim();
    
    // Handle common patterns like GN-FUNC, ISRT-FUNCTION, etc.
    if (upper.includes('-')) {
      const prefix = upper.split('-')[0]!;
      // Check if prefix is a valid function code
      const functionMap: Record<string, DliFunction> = {
        'GU': 'GU', 'GN': 'GN', 'GNP': 'GNP',
        'GHU': 'GHU', 'GHN': 'GHN', 'GHNP': 'GHNP',
        'ISRT': 'ISRT', 'DLET': 'DLET', 'REPL': 'REPL',
        'CHKP': 'CHKP', 'XRST': 'XRST', 'SYNC': 'SYNC',
        'ROLB': 'ROLB', 'ROLL': 'ROLL', 'PCB': 'PCB', 'TERM': 'TERM',
      };
      if (functionMap[prefix]) {
        return functionMap[prefix]!;
      }
    }
    
    const functionMap: Record<string, DliFunction> = {
      'GU': 'GU', 'GN': 'GN', 'GNP': 'GNP',
      'GHU': 'GHU', 'GHN': 'GHN', 'GHNP': 'GHNP',
      'ISRT': 'ISRT', 'DLET': 'DLET', 'REPL': 'REPL',
      'CHKP': 'CHKP', 'XRST': 'XRST', 'SYNC': 'SYNC',
      'ROLB': 'ROLB', 'ROLL': 'ROLL', 'PCB': 'PCB', 'TERM': 'TERM',
    };
    return functionMap[upper] || 'GU';
  }

  /**
   * Parse call arguments
   */
  private parseArguments(argsStr: string): string[] {
    return argsStr.split(/\s+/).filter(a => a.trim());
  }

  /**
   * Check if argument is an SSA
   */
  private isSSA(arg: string): boolean {
    // SSAs typically have specific patterns
    return /^\w+-SSA|SSA-\w+|UNQUAL-SSA|QUAL-SSA/i.test(arg);
  }

  /**
   * Parse SSA (Segment Search Argument)
   */
  private parseSSA(ssaVar: string): ImsSsa | null {
    // SSA format: SEGMENTNAME*cmdcodes(field op value)
    // For now, just capture the segment name pattern
    const match = ssaVar.match(/^(\w+)(?:-SSA)?/i);
    if (match) {
      return {
        segmentName: match[1]!,
      };
    }
    return null;
  }

  /**
   * Register a PCB definition
   */
  registerPcb(pcb: ImsPcb): void {
    this.pcbs.set(pcb.name.toUpperCase(), pcb);
  }

  /**
   * Get PCB by name
   */
  getPcb(name: string): ImsPcb | undefined {
    return this.pcbs.get(name.toUpperCase());
  }
}

/**
 * Convert IMS DL/I call to Spring Data operation
 */
export function convertDliToSpringData(call: DliCall): string {
  switch (call.function) {
    case 'GU':
    case 'GHU':
      return convertGetUnique(call);
    case 'GN':
    case 'GHN':
      return convertGetNext(call);
    case 'GNP':
    case 'GHNP':
      return convertGetNextWithinParent(call);
    case 'ISRT':
      return convertInsert(call);
    case 'DLET':
      return convertDelete(call);
    case 'REPL':
      return convertReplace(call);
    case 'CHKP':
    case 'SYNC':
      return 'entityManager.flush(); // Checkpoint/Sync';
    case 'ROLB':
    case 'ROLL':
      return 'transactionManager.rollback(status); // Rollback';
    default:
      return `// TODO: Convert ${call.function} call`;
  }
}

function convertGetUnique(call: DliCall): string {
  const segment = call.ssa?.[0]?.segmentName || 'Entity';
  const repo = toCamelCase(segment) + 'Repository';
  const entity = toCamelCase(segment);
  
  if (call.ssa?.[0]?.qualification) {
    const qual = call.ssa[0].qualification;
    return `${entity} = ${repo}.findBy${toPascalCase(qual.fieldName)}(${toCamelCase(qual.value)}).orElse(null);`;
  }
  
  return `${entity} = ${repo}.findById(key).orElse(null);`;
}

function convertGetNext(call: DliCall): string {
  const segment = call.ssa?.[0]?.segmentName || 'Entity';
  const iterator = toCamelCase(segment) + 'Iterator';
  const entity = toCamelCase(segment);
  
  return `if (${iterator}.hasNext()) {\n    ${entity} = ${iterator}.next();\n} else {\n    // End of data\n}`;
}

function convertGetNextWithinParent(call: DliCall): string {
  const segment = call.ssa?.[0]?.segmentName || 'Entity';
  const parentSegment = call.ssa?.[1]?.segmentName || 'Parent';
  const iterator = toCamelCase(segment) + 'Iterator';
  const entity = toCamelCase(segment);
  
  return `// Get next ${segment} within ${parentSegment}\nif (${iterator}.hasNext()) {\n    ${entity} = ${iterator}.next();\n}`;
}

function convertInsert(call: DliCall): string {
  const segment = call.ssa?.[0]?.segmentName || 'Entity';
  const repo = toCamelCase(segment) + 'Repository';
  const entity = toCamelCase(segment);
  
  return `${repo}.save(${entity});`;
}

function convertDelete(call: DliCall): string {
  const segment = call.ssa?.[0]?.segmentName || 'Entity';
  const repo = toCamelCase(segment) + 'Repository';
  const entity = toCamelCase(segment);
  
  return `${repo}.delete(${entity});`;
}

function convertReplace(call: DliCall): string {
  const segment = call.ssa?.[0]?.segmentName || 'Entity';
  const repo = toCamelCase(segment) + 'Repository';
  const entity = toCamelCase(segment);
  
  return `${repo}.save(${entity}); // Update`;
}

function toPascalCase(name: string): string {
  return name.split(/[-_]/).map(part => 
    part.charAt(0).toUpperCase() + part.slice(1).toLowerCase()
  ).join('');
}

function toCamelCase(name: string): string {
  const pascal = toPascalCase(name);
  return pascal.charAt(0).toLowerCase() + pascal.slice(1);
}

/**
 * Generate entity class from IMS segment
 */
export function generateEntityFromSegment(segment: ImsSegment, parentSegment?: ImsSegment): string {
  const className = toPascalCase(segment.name);
  const lines: string[] = [];

  lines.push('package com.example.entity;');
  lines.push('');
  lines.push('import jakarta.persistence.*;');
  lines.push('import java.math.BigDecimal;');
  lines.push('');
  lines.push('@Entity');
  lines.push(`@Table(name = "${segment.name.toUpperCase()}")`);
  lines.push(`public class ${className} {`);
  lines.push('');
  lines.push('    @Id');
  lines.push('    @GeneratedValue(strategy = GenerationType.IDENTITY)');
  lines.push('    private Long id;');
  lines.push('');

  // Parent relationship
  if (parentSegment) {
    const parentClass = toPascalCase(parentSegment.name);
    lines.push('    @ManyToOne(fetch = FetchType.LAZY)');
    lines.push(`    @JoinColumn(name = "${parentSegment.name.toUpperCase()}_ID")`);
    lines.push(`    private ${parentClass} ${toCamelCase(parentSegment.name)};`);
    lines.push('');
  }

  // Fields
  for (const field of segment.fields) {
    const javaType = mapImsTypeToJava(field.type, field.length);
    const fieldName = toCamelCase(field.name);
    
    lines.push(`    @Column(name = "${field.name.toUpperCase()}", length = ${field.length})`);
    lines.push(`    private ${javaType} ${fieldName};`);
    lines.push('');
  }

  // Getters and setters
  lines.push('    // Getters and setters');
  lines.push('    public Long getId() { return id; }');
  lines.push('    public void setId(Long id) { this.id = id; }');
  lines.push('');

  for (const field of segment.fields) {
    const javaType = mapImsTypeToJava(field.type, field.length);
    const fieldName = toCamelCase(field.name);
    const pascalName = toPascalCase(field.name);
    
    lines.push(`    public ${javaType} get${pascalName}() { return ${fieldName}; }`);
    lines.push(`    public void set${pascalName}(${javaType} ${fieldName}) { this.${fieldName} = ${fieldName}; }`);
    lines.push('');
  }

  lines.push('}');

  return lines.join('\n');
}

function mapImsTypeToJava(type: string, length: number): string {
  switch (type) {
    case 'P': // Packed decimal
      return 'BigDecimal';
    case 'Z': // Zoned decimal
      return length > 9 ? 'Long' : 'Integer';
    case 'X': // Hex/binary
      return 'byte[]';
    case 'C': // Character
    default:
      return 'String';
  }
}
