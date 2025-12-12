/**
 * JCL (Job Control Language) Parser
 * 
 * Parses IBM JCL to understand job execution context for COBOL programs
 */

/**
 * JCL Job definition
 */
export interface JclJob {
  name: string;
  class?: string;
  msgClass?: string;
  region?: string;
  time?: string;
  steps: JclStep[];
  procedures: JclProcedure[];
  datasets: JclDataset[];
  symbols: Map<string, string>;
}

/**
 * JCL Step definition
 */
export interface JclStep {
  name: string;
  program?: string;
  procedure?: string;
  ddStatements: JclDDStatement[];
  condition?: JclCondition;
  region?: string;
  time?: string;
}

/**
 * JCL DD (Data Definition) statement
 */
export interface JclDDStatement {
  ddName: string;
  dsn?: string;
  disp?: {
    status: 'NEW' | 'OLD' | 'SHR' | 'MOD';
    normalDisp?: 'DELETE' | 'KEEP' | 'PASS' | 'CATLG' | 'UNCATLG';
    abnormalDisp?: 'DELETE' | 'KEEP' | 'CATLG' | 'UNCATLG';
  };
  unit?: string;
  space?: {
    unit: 'TRK' | 'CYL' | 'bytes';
    primary: number;
    secondary?: number;
    directory?: number;
  };
  dcb?: {
    recfm?: string;
    lrecl?: number;
    blksize?: number;
    dsorg?: string;
  };
  sysout?: string;
  dummy?: boolean;
  instream?: string[];
}

/**
 * JCL Condition
 */
export interface JclCondition {
  code: number;
  operator: 'GT' | 'GE' | 'EQ' | 'NE' | 'LT' | 'LE';
  stepName?: string;
}

/**
 * JCL Procedure
 */
export interface JclProcedure {
  name: string;
  steps: JclStep[];
  parameters: Map<string, string>;
}

/**
 * JCL Dataset reference
 */
export interface JclDataset {
  name: string;
  ddName: string;
  stepName: string;
  type: 'input' | 'output' | 'update' | 'temporary';
}

/**
 * JCL Parser
 */
export class JclParser {
  private lines: string[] = [];
  private currentIndex = 0;
  private symbols = new Map<string, string>();

  /**
   * Parse JCL source
   */
  parse(source: string): JclJob {
    this.lines = source.split('\n');
    this.currentIndex = 0;
    this.symbols.clear();

    const job: JclJob = {
      name: '',
      steps: [],
      procedures: [],
      datasets: [],
      symbols: this.symbols,
    };

    while (this.currentIndex < this.lines.length) {
      const line = this.lines[this.currentIndex]!;
      
      // Skip comments and empty lines
      if (this.isComment(line) || line.trim() === '') {
        this.currentIndex++;
        continue;
      }

      // Parse JOB statement
      if (this.isJobStatement(line)) {
        this.parseJobStatement(line, job);
      }
      // Parse EXEC statement
      else if (this.isExecStatement(line)) {
        const step = this.parseExecStatement(line);
        job.steps.push(step);
        
        // Collect datasets from DD statements
        step.ddStatements.forEach(dd => {
          if (dd.dsn && !dd.dummy) {
            job.datasets.push({
              name: dd.dsn,
              ddName: dd.ddName,
              stepName: step.name,
              type: this.inferDatasetType(dd),
            });
          }
        });
      }
      // Parse SET statement (symbol definition)
      else if (this.isSetStatement(line)) {
        this.parseSetStatement(line);
      }
      // Parse PROC statement
      else if (this.isProcStatement(line)) {
        const proc = this.parseProcedure(line);
        job.procedures.push(proc);
      }

      this.currentIndex++;
    }

    return job;
  }

  /**
   * Check if line is a comment
   */
  private isComment(line: string): boolean {
    return line.startsWith('//*') || line.startsWith('/*');
  }

  /**
   * Check if line is a JOB statement
   */
  private isJobStatement(line: string): boolean {
    return /^\/\/\w+\s+JOB\s/i.test(line);
  }

  /**
   * Check if line is an EXEC statement
   */
  private isExecStatement(line: string): boolean {
    return /^\/\/\w*\s+EXEC\s/i.test(line);
  }

  /**
   * Check if line is a SET statement
   */
  private isSetStatement(line: string): boolean {
    return /^\/\/\s*SET\s/i.test(line);
  }

  /**
   * Check if line is a PROC statement
   */
  private isProcStatement(line: string): boolean {
    return /^\/\/\w+\s+PROC\s/i.test(line);
  }

  /**
   * Check if line is a DD statement
   */
  private isDDStatement(line: string): boolean {
    return /^\/\/\w+\s+DD\s/i.test(line);
  }

  /**
   * Parse JOB statement
   */
  private parseJobStatement(line: string, job: JclJob): void {
    const match = line.match(/^\/\/(\w+)\s+JOB\s+(.*)$/i);
    if (match) {
      job.name = match[1]!;
      const params = this.parseParameters(match[2]!);
      
      job.class = params.get('CLASS');
      job.msgClass = params.get('MSGCLASS');
      job.region = params.get('REGION');
      job.time = params.get('TIME');
    }
  }

  /**
   * Parse EXEC statement
   */
  private parseExecStatement(line: string): JclStep {
    const match = line.match(/^\/\/(\w*)\s+EXEC\s+(.*)$/i);
    const stepName = match?.[1] || `STEP${this.currentIndex}`;
    const params = this.parseParameters(match?.[2] || '');

    const step: JclStep = {
      name: stepName,
      ddStatements: [],
    };

    // Check if it's a program or procedure
    const pgm = params.get('PGM');
    const proc = params.get('PROC') || (!pgm ? params.entries().next().value?.[0] : undefined);

    if (pgm) {
      step.program = pgm;
    } else if (proc) {
      step.procedure = proc;
    }

    step.region = params.get('REGION');
    step.time = params.get('TIME');

    // Parse condition
    const cond = params.get('COND');
    if (cond) {
      step.condition = this.parseCondition(cond);
    }

    // Parse following DD statements
    this.currentIndex++;
    while (this.currentIndex < this.lines.length) {
      const ddLine = this.lines[this.currentIndex]!;
      
      if (this.isDDStatement(ddLine)) {
        const dd = this.parseDDStatement(ddLine);
        step.ddStatements.push(dd);
      } else if (ddLine.startsWith('//') && !this.isComment(ddLine)) {
        // New statement, back up
        this.currentIndex--;
        break;
      }
      
      this.currentIndex++;
    }

    return step;
  }

  /**
   * Parse DD statement
   */
  private parseDDStatement(line: string): JclDDStatement {
    const match = line.match(/^\/\/(\w+)\s+DD\s+(.*)$/i);
    const ddName = match?.[1] || 'UNKNOWN';
    const params = this.parseParameters(match?.[2] || '');

    const dd: JclDDStatement = {
      ddName,
    };

    // DSN (Dataset Name)
    dd.dsn = params.get('DSN') || params.get('DSNAME');

    // DISP (Disposition)
    const disp = params.get('DISP');
    if (disp) {
      dd.disp = this.parseDisposition(disp);
    }

    // UNIT
    dd.unit = params.get('UNIT');

    // SPACE
    const space = params.get('SPACE');
    if (space) {
      dd.space = this.parseSpace(space);
    }

    // DCB
    const dcb = params.get('DCB');
    if (dcb) {
      dd.dcb = this.parseDCB(dcb);
    }

    // SYSOUT
    dd.sysout = params.get('SYSOUT');

    // DUMMY
    if (params.has('DUMMY') || line.includes(' DUMMY')) {
      dd.dummy = true;
    }

    // Instream data
    if (line.includes('DD *') || line.includes('DD DATA')) {
      dd.instream = this.parseInstreamData();
    }

    return dd;
  }

  /**
   * Parse SET statement
   */
  private parseSetStatement(line: string): void {
    const match = line.match(/^\/\/\s*SET\s+(\w+)=(.+)$/i);
    if (match) {
      this.symbols.set(match[1]!, match[2]!.trim());
    }
  }

  /**
   * Parse procedure
   */
  private parseProcedure(line: string): JclProcedure {
    const match = line.match(/^\/\/(\w+)\s+PROC\s*(.*)$/i);
    const procName = match?.[1] || 'UNKNOWN';
    const params = this.parseParameters(match?.[2] || '');

    const proc: JclProcedure = {
      name: procName,
      steps: [],
      parameters: params,
    };

    // Parse procedure steps
    this.currentIndex++;
    while (this.currentIndex < this.lines.length) {
      const procLine = this.lines[this.currentIndex]!;
      
      if (/^\/\/\s+PEND/i.test(procLine)) {
        break;
      }
      
      if (this.isExecStatement(procLine)) {
        const step = this.parseExecStatement(procLine);
        proc.steps.push(step);
      }
      
      this.currentIndex++;
    }

    return proc;
  }

  /**
   * Parse instream data
   */
  private parseInstreamData(): string[] {
    const data: string[] = [];
    this.currentIndex++;
    
    while (this.currentIndex < this.lines.length) {
      const line = this.lines[this.currentIndex]!;
      
      if (line === '/*' || line.startsWith('//')) {
        this.currentIndex--;
        break;
      }
      
      data.push(line);
      this.currentIndex++;
    }
    
    return data;
  }

  /**
   * Parse JCL parameters
   */
  private parseParameters(paramStr: string): Map<string, string> {
    const params = new Map<string, string>();
    
    // Handle parenthesized values
    let cleaned = paramStr;
    const parens: string[] = [];
    cleaned = cleaned.replace(/\([^)]+\)/g, (match) => {
      parens.push(match);
      return `__PAREN${parens.length - 1}__`;
    });

    // Split by comma
    const parts = cleaned.split(',').map(p => p.trim());
    
    for (const part of parts) {
      // Restore parentheses
      let restored = part;
      parens.forEach((p, i) => {
        restored = restored.replace(`__PAREN${i}__`, p);
      });

      const eqIdx = restored.indexOf('=');
      if (eqIdx > 0) {
        const key = restored.substring(0, eqIdx).trim().toUpperCase();
        const value = restored.substring(eqIdx + 1).trim();
        params.set(key, value);
      } else if (restored.trim()) {
        // Positional parameter
        params.set(restored.trim().toUpperCase(), '');
      }
    }

    return params;
  }

  /**
   * Parse DISP parameter
   */
  private parseDisposition(disp: string): JclDDStatement['disp'] {
    const parts = disp.replace(/[()]/g, '').split(',');
    
    const statusMap: Record<string, 'NEW' | 'OLD' | 'SHR' | 'MOD'> = {
      NEW: 'NEW', OLD: 'OLD', SHR: 'SHR', MOD: 'MOD',
    };
    
    const normalDispMap: Record<string, 'DELETE' | 'KEEP' | 'PASS' | 'CATLG' | 'UNCATLG'> = {
      DELETE: 'DELETE', KEEP: 'KEEP', PASS: 'PASS', CATLG: 'CATLG', UNCATLG: 'UNCATLG',
    };

    const abnormalDispMap: Record<string, 'DELETE' | 'KEEP' | 'CATLG' | 'UNCATLG'> = {
      DELETE: 'DELETE', KEEP: 'KEEP', CATLG: 'CATLG', UNCATLG: 'UNCATLG',
    };

    return {
      status: statusMap[parts[0]?.toUpperCase() || 'SHR'] || 'SHR',
      normalDisp: normalDispMap[parts[1]?.toUpperCase() || ''],
      abnormalDisp: abnormalDispMap[parts[2]?.toUpperCase() || ''],
    };
  }

  /**
   * Parse SPACE parameter
   */
  private parseSpace(space: string): JclDDStatement['space'] {
    const match = space.match(/\((\w+),\((\d+)(?:,(\d+))?(?:,(\d+))?\)\)/);
    if (match) {
      const unitMap: Record<string, 'TRK' | 'CYL' | 'bytes'> = {
        TRK: 'TRK', CYL: 'CYL',
      };
      
      return {
        unit: unitMap[match[1]?.toUpperCase() || 'TRK'] || 'TRK',
        primary: parseInt(match[2]!, 10),
        secondary: match[3] ? parseInt(match[3], 10) : undefined,
        directory: match[4] ? parseInt(match[4], 10) : undefined,
      };
    }
    return undefined;
  }

  /**
   * Parse DCB parameter
   */
  private parseDCB(dcb: string): JclDDStatement['dcb'] {
    const params = this.parseParameters(dcb.replace(/[()]/g, ''));
    
    return {
      recfm: params.get('RECFM'),
      lrecl: params.has('LRECL') ? parseInt(params.get('LRECL')!, 10) : undefined,
      blksize: params.has('BLKSIZE') ? parseInt(params.get('BLKSIZE')!, 10) : undefined,
      dsorg: params.get('DSORG'),
    };
  }

  /**
   * Parse COND parameter
   */
  private parseCondition(cond: string): JclCondition | undefined {
    const match = cond.match(/\((\d+),(GT|GE|EQ|NE|LT|LE)(?:,(\w+))?\)/i);
    if (match) {
      return {
        code: parseInt(match[1]!, 10),
        operator: match[2]!.toUpperCase() as JclCondition['operator'],
        stepName: match[3],
      };
    }
    return undefined;
  }

  /**
   * Infer dataset type from DD statement
   */
  private inferDatasetType(dd: JclDDStatement): JclDataset['type'] {
    if (!dd.disp) return 'input';
    
    switch (dd.disp.status) {
      case 'NEW':
        return 'output';
      case 'MOD':
        return 'update';
      case 'OLD':
        return dd.disp.normalDisp === 'DELETE' ? 'temporary' : 'update';
      default:
        return 'input';
    }
  }

  /**
   * Substitute symbols in a string
   */
  substituteSymbols(text: string): string {
    let result = text;
    this.symbols.forEach((value, key) => {
      result = result.replace(new RegExp(`&${key}`, 'gi'), value);
    });
    return result;
  }
}

/**
 * Generate Spring Batch configuration from JCL
 */
export function generateBatchConfigFromJcl(job: JclJob): string {
  const lines: string[] = [];
  
  lines.push('package com.example.batch;');
  lines.push('');
  lines.push('import org.springframework.batch.core.*;');
  lines.push('import org.springframework.batch.core.job.builder.JobBuilder;');
  lines.push('import org.springframework.batch.core.step.builder.StepBuilder;');
  lines.push('import org.springframework.context.annotation.Bean;');
  lines.push('import org.springframework.context.annotation.Configuration;');
  lines.push('');
  lines.push('@Configuration');
  lines.push(`public class ${toPascalCase(job.name)}JobConfig {`);
  lines.push('');

  // Generate Job bean
  lines.push('    @Bean');
  lines.push(`    public Job ${toCamelCase(job.name)}Job(JobRepository jobRepository,`);
  
  const stepBeans = job.steps.map(s => `${toCamelCase(s.name)}Step`);
  lines.push(`            ${stepBeans.map(s => `Step ${s}`).join(', ')}) {`);
  lines.push(`        return new JobBuilder("${job.name}", jobRepository)`);
  
  if (stepBeans.length > 0) {
    lines.push(`            .start(${stepBeans[0]})`);
    for (let i = 1; i < stepBeans.length; i++) {
      lines.push(`            .next(${stepBeans[i]})`);
    }
  }
  
  lines.push('            .build();');
  lines.push('    }');
  lines.push('');

  // Generate Step beans
  for (const step of job.steps) {
    lines.push('    @Bean');
    lines.push(`    public Step ${toCamelCase(step.name)}Step(JobRepository jobRepository,`);
    lines.push('            PlatformTransactionManager transactionManager) {');
    lines.push(`        return new StepBuilder("${step.name}", jobRepository)`);
    lines.push(`            .tasklet(${toCamelCase(step.name)}Tasklet(), transactionManager)`);
    lines.push('            .build();');
    lines.push('    }');
    lines.push('');
  }

  lines.push('}');
  
  return lines.join('\n');
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
