/**
 * DB2 SQL Support for COBOL
 * 
 * Handles embedded SQL parsing and Java/Spring Data conversion
 */

/**
 * DB2 SQL statement types
 */
export type Db2StatementType = 
  | 'SELECT'
  | 'INSERT'
  | 'UPDATE'
  | 'DELETE'
  | 'DECLARE'
  | 'OPEN'
  | 'FETCH'
  | 'CLOSE'
  | 'COMMIT'
  | 'ROLLBACK'
  | 'CALL';

/**
 * Parsed DB2 SQL statement
 */
export interface Db2Statement {
  type: Db2StatementType;
  sql: string;
  hostVariables: string[];
  cursorName?: string;
  tableName?: string;
  columns?: string[];
  whereClause?: string;
}

/**
 * DB2 Cursor definition
 */
export interface Db2Cursor {
  name: string;
  sql: string;
  hostVariables: string[];
  withHold: boolean;
  forUpdate: boolean;
}

/**
 * DB2 SQL Parser for COBOL embedded SQL
 */
export class Db2Parser {
  private cursors = new Map<string, Db2Cursor>();

  /**
   * Parse EXEC SQL statement
   */
  parseExecSql(statement: string): Db2Statement | null {
    const cleaned = this.cleanStatement(statement);
    const upper = cleaned.toUpperCase();

    // DECLARE CURSOR
    if (upper.includes('DECLARE') && upper.includes('CURSOR')) {
      return this.parseDeclareCursor(cleaned);
    }

    // OPEN CURSOR
    if (upper.startsWith('OPEN ')) {
      return this.parseOpenCursor(cleaned);
    }

    // FETCH
    if (upper.startsWith('FETCH ')) {
      return this.parseFetch(cleaned);
    }

    // CLOSE
    if (upper.startsWith('CLOSE ')) {
      return this.parseClose(cleaned);
    }

    // SELECT
    if (upper.startsWith('SELECT ')) {
      return this.parseSelect(cleaned);
    }

    // INSERT
    if (upper.startsWith('INSERT ')) {
      return this.parseInsert(cleaned);
    }

    // UPDATE
    if (upper.startsWith('UPDATE ')) {
      return this.parseUpdate(cleaned);
    }

    // DELETE
    if (upper.startsWith('DELETE ')) {
      return this.parseDelete(cleaned);
    }

    // COMMIT
    if (upper.startsWith('COMMIT')) {
      return { type: 'COMMIT', sql: cleaned, hostVariables: [] };
    }

    // ROLLBACK
    if (upper.startsWith('ROLLBACK')) {
      return { type: 'ROLLBACK', sql: cleaned, hostVariables: [] };
    }

    // CALL (stored procedure)
    if (upper.startsWith('CALL ')) {
      return this.parseCall(cleaned);
    }

    return null;
  }

  /**
   * Clean EXEC SQL statement
   */
  private cleanStatement(statement: string): string {
    return statement
      .replace(/EXEC\s+SQL\s*/gi, '')
      .replace(/END-EXEC\.?/gi, '')
      .replace(/\s+/g, ' ')
      .trim();
  }

  /**
   * Extract host variables from SQL
   */
  private extractHostVariables(sql: string): string[] {
    const matches = sql.match(/:[\w-]+/g) || [];
    return [...new Set(matches.map(m => m.substring(1)))];
  }

  /**
   * Parse DECLARE CURSOR
   */
  private parseDeclareCursor(sql: string): Db2Statement {
    const match = sql.match(/DECLARE\s+(\w+)\s+CURSOR\s+(WITH\s+HOLD\s+)?FOR\s+(.+)/i);
    
    if (match) {
      const cursorName = match[1]!;
      const withHold = !!match[2];
      const selectSql = match[3]!;
      const forUpdate = /FOR\s+UPDATE/i.test(selectSql);

      const cursor: Db2Cursor = {
        name: cursorName,
        sql: selectSql,
        hostVariables: this.extractHostVariables(selectSql),
        withHold,
        forUpdate,
      };
      
      this.cursors.set(cursorName.toUpperCase(), cursor);

      return {
        type: 'DECLARE',
        sql,
        hostVariables: cursor.hostVariables,
        cursorName,
      };
    }

    return { type: 'DECLARE', sql, hostVariables: [] };
  }

  /**
   * Parse OPEN CURSOR
   */
  private parseOpenCursor(sql: string): Db2Statement {
    const match = sql.match(/OPEN\s+(\w+)/i);
    const cursorName = match?.[1] || '';
    const cursor = this.cursors.get(cursorName.toUpperCase());

    return {
      type: 'OPEN',
      sql,
      hostVariables: cursor?.hostVariables || [],
      cursorName,
    };
  }

  /**
   * Parse FETCH
   */
  private parseFetch(sql: string): Db2Statement {
    const match = sql.match(/FETCH\s+(\w+)\s+INTO\s+(.+)/i);
    
    return {
      type: 'FETCH',
      sql,
      hostVariables: this.extractHostVariables(sql),
      cursorName: match?.[1],
    };
  }

  /**
   * Parse CLOSE
   */
  private parseClose(sql: string): Db2Statement {
    const match = sql.match(/CLOSE\s+(\w+)/i);
    
    return {
      type: 'CLOSE',
      sql,
      hostVariables: [],
      cursorName: match?.[1],
    };
  }

  /**
   * Parse SELECT
   */
  private parseSelect(sql: string): Db2Statement {
    const tableMatch = sql.match(/FROM\s+(\w+)/i);
    const columnsMatch = sql.match(/SELECT\s+(.+?)\s+(?:INTO|FROM)/i);
    const whereMatch = sql.match(/WHERE\s+(.+?)(?:ORDER|GROUP|$)/i);

    return {
      type: 'SELECT',
      sql,
      hostVariables: this.extractHostVariables(sql),
      tableName: tableMatch?.[1],
      columns: columnsMatch?.[1]?.split(',').map(c => c.trim()),
      whereClause: whereMatch?.[1]?.trim(),
    };
  }

  /**
   * Parse INSERT
   */
  private parseInsert(sql: string): Db2Statement {
    const tableMatch = sql.match(/INSERT\s+INTO\s+(\w+)/i);

    return {
      type: 'INSERT',
      sql,
      hostVariables: this.extractHostVariables(sql),
      tableName: tableMatch?.[1],
    };
  }

  /**
   * Parse UPDATE
   */
  private parseUpdate(sql: string): Db2Statement {
    const tableMatch = sql.match(/UPDATE\s+(\w+)/i);
    const whereMatch = sql.match(/WHERE\s+(.+)/i);

    return {
      type: 'UPDATE',
      sql,
      hostVariables: this.extractHostVariables(sql),
      tableName: tableMatch?.[1],
      whereClause: whereMatch?.[1]?.trim(),
    };
  }

  /**
   * Parse DELETE
   */
  private parseDelete(sql: string): Db2Statement {
    const tableMatch = sql.match(/DELETE\s+FROM\s+(\w+)/i);
    const whereMatch = sql.match(/WHERE\s+(.+)/i);

    return {
      type: 'DELETE',
      sql,
      hostVariables: this.extractHostVariables(sql),
      tableName: tableMatch?.[1],
      whereClause: whereMatch?.[1]?.trim(),
    };
  }

  /**
   * Parse CALL (stored procedure)
   */
  private parseCall(sql: string): Db2Statement {
    return {
      type: 'CALL',
      sql,
      hostVariables: this.extractHostVariables(sql),
    };
  }

  /**
   * Get cursor by name
   */
  getCursor(name: string): Db2Cursor | undefined {
    return this.cursors.get(name.toUpperCase());
  }

  /**
   * Get all cursors
   */
  getAllCursors(): Db2Cursor[] {
    return Array.from(this.cursors.values());
  }
}

/**
 * Convert DB2 statement to Spring Data JPA
 */
export function convertToSpringData(stmt: Db2Statement): string {
  switch (stmt.type) {
    case 'SELECT':
      return convertSelectToSpringData(stmt);
    case 'INSERT':
      return convertInsertToSpringData(stmt);
    case 'UPDATE':
      return convertUpdateToSpringData(stmt);
    case 'DELETE':
      return convertDeleteToSpringData(stmt);
    case 'FETCH':
      return convertFetchToSpringData(stmt);
    case 'COMMIT':
      return 'transactionManager.commit(status);';
    case 'ROLLBACK':
      return 'transactionManager.rollback(status);';
    default:
      return `// TODO: Convert ${stmt.type}\n// ${stmt.sql}`;
  }
}

function convertSelectToSpringData(stmt: Db2Statement): string {
  const table = stmt.tableName || 'Entity';
  const repo = toCamelCase(table) + 'Repository';
  
  if (stmt.whereClause) {
    const methodName = generateQueryMethodName(stmt.whereClause);
    const params = stmt.hostVariables.map(v => toCamelCase(v)).join(', ');
    return `${repo}.${methodName}(${params})`;
  }
  
  return `${repo}.findAll()`;
}

function convertInsertToSpringData(stmt: Db2Statement): string {
  const table = stmt.tableName || 'Entity';
  const repo = toCamelCase(table) + 'Repository';
  const entity = toCamelCase(table);
  
  return `${repo}.save(${entity})`;
}

function convertUpdateToSpringData(stmt: Db2Statement): string {
  const table = stmt.tableName || 'Entity';
  const repo = toCamelCase(table) + 'Repository';
  const entity = toCamelCase(table);
  
  return `${repo}.save(${entity}); // UPDATE`;
}

function convertDeleteToSpringData(stmt: Db2Statement): string {
  const table = stmt.tableName || 'Entity';
  const repo = toCamelCase(table) + 'Repository';
  
  if (stmt.whereClause) {
    const methodName = 'deleteBy' + generateQueryMethodName(stmt.whereClause).replace('findBy', '');
    const params = stmt.hostVariables.map(v => toCamelCase(v)).join(', ');
    return `${repo}.${methodName}(${params})`;
  }
  
  return `${repo}.deleteAll()`;
}

function convertFetchToSpringData(stmt: Db2Statement): string {
  const cursor = stmt.cursorName || 'cursor';
  const iterator = toCamelCase(cursor) + 'Iterator';
  
  return `if (${iterator}.hasNext()) {\n    result = ${iterator}.next();\n}`;
}

function generateQueryMethodName(whereClause: string): string {
  // Extract column names from WHERE clause
  const columns = whereClause.match(/(\w+)\s*=/g)?.map(m => 
    m.replace(/\s*=/, '').trim()
  ) || [];
  
  if (columns.length === 0) return 'findAll';
  
  const methodName = 'findBy' + columns.map(c => 
    c.split('_').map(part => 
      part.charAt(0).toUpperCase() + part.slice(1).toLowerCase()
    ).join('')
  ).join('And');
  
  return methodName;
}

function toCamelCase(name: string): string {
  return name.split(/[-_]/).map((part, i) => 
    i === 0 ? part.toLowerCase() : part.charAt(0).toUpperCase() + part.slice(1).toLowerCase()
  ).join('');
}

/**
 * Generate Spring Data Repository interface
 */
export function generateRepositoryInterface(tableName: string, statements: Db2Statement[]): string {
  const entityName = toPascalCase(tableName);
  const repoName = entityName + 'Repository';
  
  const lines: string[] = [];
  lines.push('package com.example.repository;');
  lines.push('');
  lines.push('import org.springframework.data.jpa.repository.JpaRepository;');
  lines.push('import org.springframework.data.jpa.repository.Query;');
  lines.push('import org.springframework.stereotype.Repository;');
  lines.push(`import com.example.entity.${entityName};`);
  lines.push('import java.util.List;');
  lines.push('import java.util.Optional;');
  lines.push('');
  lines.push('@Repository');
  lines.push(`public interface ${repoName} extends JpaRepository<${entityName}, Long> {`);
  lines.push('');

  // Generate custom query methods
  const methods = new Set<string>();
  for (const stmt of statements) {
    if (stmt.tableName?.toUpperCase() === tableName.toUpperCase() && stmt.whereClause) {
      const methodName = generateQueryMethodName(stmt.whereClause);
      if (!methods.has(methodName)) {
        methods.add(methodName);
        const params = stmt.hostVariables.map((v, i) => 
          `${inferParamType(v)} ${toCamelCase(v)}`
        ).join(', ');
        
        if (stmt.type === 'SELECT') {
          lines.push(`    List<${entityName}> ${methodName}(${params});`);
        } else if (stmt.type === 'DELETE') {
          lines.push(`    void deleteBy${methodName.replace('findBy', '')}(${params});`);
        }
        lines.push('');
      }
    }
  }

  lines.push('}');
  
  return lines.join('\n');
}

function toPascalCase(name: string): string {
  return name.split(/[-_]/).map(part => 
    part.charAt(0).toUpperCase() + part.slice(1).toLowerCase()
  ).join('');
}

function inferParamType(varName: string): string {
  const upper = varName.toUpperCase();
  if (upper.includes('DATE') || upper.includes('TIME')) return 'LocalDateTime';
  if (upper.includes('AMT') || upper.includes('AMOUNT') || upper.includes('PRICE')) return 'BigDecimal';
  if (upper.includes('CNT') || upper.includes('COUNT') || upper.includes('NUM') || upper.includes('ID')) return 'Long';
  return 'String';
}
