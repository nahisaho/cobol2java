/**
 * Enterprise Features Tests
 * 
 * Tests for JCL, DB2, and IMS support
 */

import { describe, it, expect } from 'vitest';
import {
  JclParser,
  generateBatchConfigFromJcl,
} from '../src/enterprise/jcl-parser.js';
import {
  Db2Parser,
  convertToSpringData,
  generateRepositoryInterface,
} from '../src/enterprise/db2-support.js';
import {
  ImsParser,
  convertDliToSpringData,
  generateEntityFromSegment,
} from '../src/enterprise/ims-support.js';

describe('JCL Parser', () => {
  const parser = new JclParser();

  it('should parse JOB statement', () => {
    const jcl = `//MYJOB    JOB (ACCT),'BATCH JOB',CLASS=A,MSGCLASS=X
//*`;
    
    const job = parser.parse(jcl);
    
    expect(job.name).toBe('MYJOB');
    expect(job.class).toBe('A');
    expect(job.msgClass).toBe('X');
  });

  it('should parse EXEC PGM statement', () => {
    const jcl = `//MYJOB    JOB (ACCT),'TEST'
//STEP1    EXEC PGM=COBPROG,REGION=4M
//`;
    
    const job = parser.parse(jcl);
    
    expect(job.steps.length).toBe(1);
    expect(job.steps[0]?.name).toBe('STEP1');
    expect(job.steps[0]?.program).toBe('COBPROG');
    expect(job.steps[0]?.region).toBe('4M');
  });

  it('should parse DD statements', () => {
    const jcl = `//MYJOB    JOB (ACCT),'TEST'
//STEP1    EXEC PGM=MYPROG
//INPUT    DD DSN=MY.INPUT.FILE,DISP=SHR
//OUTPUT   DD DSN=MY.OUTPUT.FILE,DISP=(NEW,CATLG,DELETE)
//SYSOUT   DD SYSOUT=*
//`;
    
    const job = parser.parse(jcl);
    const step = job.steps[0]!;
    
    expect(step.ddStatements.length).toBe(3);
    
    const inputDD = step.ddStatements.find(d => d.ddName === 'INPUT');
    expect(inputDD?.dsn).toBe('MY.INPUT.FILE');
    expect(inputDD?.disp?.status).toBe('SHR');
    
    const outputDD = step.ddStatements.find(d => d.ddName === 'OUTPUT');
    expect(outputDD?.disp?.status).toBe('NEW');
    expect(outputDD?.disp?.normalDisp).toBe('CATLG');
    expect(outputDD?.disp?.abnormalDisp).toBe('DELETE');
    
    const sysoutDD = step.ddStatements.find(d => d.ddName === 'SYSOUT');
    expect(sysoutDD?.sysout).toBe('*');
  });

  it('should identify dataset types', () => {
    const jcl = `//MYJOB    JOB (ACCT),'TEST'
//STEP1    EXEC PGM=MYPROG
//INPUT    DD DSN=MY.INPUT.FILE,DISP=SHR
//OUTPUT   DD DSN=MY.OUTPUT.FILE,DISP=(NEW,CATLG)
//UPDATE   DD DSN=MY.UPDATE.FILE,DISP=(MOD,KEEP)
//`;
    
    const job = parser.parse(jcl);
    
    const inputDs = job.datasets.find(d => d.ddName === 'INPUT');
    expect(inputDs?.type).toBe('input');
    
    const outputDs = job.datasets.find(d => d.ddName === 'OUTPUT');
    expect(outputDs?.type).toBe('output');
    
    const updateDs = job.datasets.find(d => d.ddName === 'UPDATE');
    expect(updateDs?.type).toBe('update');
  });

  it('should generate Spring Batch config', () => {
    const jcl = `//MYJOB    JOB (ACCT),'TEST'
//STEP1    EXEC PGM=PROG1
//STEP2    EXEC PGM=PROG2
//`;
    
    const job = parser.parse(jcl);
    const config = generateBatchConfigFromJcl(job);
    
    expect(config).toContain('@Configuration');
    expect(config).toContain('MyjobJobConfig');
    expect(config).toContain('step1Step');
    expect(config).toContain('step2Step');
    expect(config).toContain('.start(step1Step)');
    expect(config).toContain('.next(step2Step)');
  });
});

describe('DB2 Parser', () => {
  const parser = new Db2Parser();

  it('should parse SELECT statement', () => {
    const sql = 'EXEC SQL SELECT NAME, AGE INTO :WS-NAME, :WS-AGE FROM CUSTOMER WHERE ID = :WS-ID END-EXEC';
    
    const stmt = parser.parseExecSql(sql);
    
    expect(stmt?.type).toBe('SELECT');
    expect(stmt?.tableName).toBe('CUSTOMER');
    expect(stmt?.hostVariables).toContain('WS-NAME');
    expect(stmt?.hostVariables).toContain('WS-AGE');
    expect(stmt?.hostVariables).toContain('WS-ID');
  });

  it('should parse INSERT statement', () => {
    const sql = 'EXEC SQL INSERT INTO CUSTOMER (NAME, AGE) VALUES (:WS-NAME, :WS-AGE) END-EXEC';
    
    const stmt = parser.parseExecSql(sql);
    
    expect(stmt?.type).toBe('INSERT');
    expect(stmt?.tableName).toBe('CUSTOMER');
    expect(stmt?.hostVariables).toContain('WS-NAME');
    expect(stmt?.hostVariables).toContain('WS-AGE');
  });

  it('should parse UPDATE statement', () => {
    const sql = 'EXEC SQL UPDATE CUSTOMER SET NAME = :WS-NAME WHERE ID = :WS-ID END-EXEC';
    
    const stmt = parser.parseExecSql(sql);
    
    expect(stmt?.type).toBe('UPDATE');
    expect(stmt?.tableName).toBe('CUSTOMER');
    expect(stmt?.whereClause).toContain('ID = :WS-ID');
  });

  it('should parse DELETE statement', () => {
    const sql = 'EXEC SQL DELETE FROM CUSTOMER WHERE ID = :WS-ID END-EXEC';
    
    const stmt = parser.parseExecSql(sql);
    
    expect(stmt?.type).toBe('DELETE');
    expect(stmt?.tableName).toBe('CUSTOMER');
  });

  it('should parse DECLARE CURSOR', () => {
    const sql = 'EXEC SQL DECLARE CUST_CURSOR CURSOR FOR SELECT * FROM CUSTOMER WHERE STATUS = :WS-STATUS END-EXEC';
    
    const stmt = parser.parseExecSql(sql);
    
    expect(stmt?.type).toBe('DECLARE');
    expect(stmt?.cursorName).toBe('CUST_CURSOR');
    expect(stmt?.hostVariables).toContain('WS-STATUS');
    
    const cursor = parser.getCursor('CUST_CURSOR');
    expect(cursor).toBeDefined();
    expect(cursor?.withHold).toBe(false);
  });

  it('should parse COMMIT and ROLLBACK', () => {
    expect(parser.parseExecSql('EXEC SQL COMMIT END-EXEC')?.type).toBe('COMMIT');
    expect(parser.parseExecSql('EXEC SQL ROLLBACK END-EXEC')?.type).toBe('ROLLBACK');
  });

  it('should convert to Spring Data', () => {
    const selectSql = 'EXEC SQL SELECT NAME FROM CUSTOMER WHERE ID = :WS-ID END-EXEC';
    const stmt = parser.parseExecSql(selectSql)!;
    const java = convertToSpringData(stmt);
    
    expect(java).toContain('customerRepository');
    expect(java).toContain('findBy');
  });

  it('should generate repository interface', () => {
    const sql1 = 'EXEC SQL SELECT * FROM CUSTOMER WHERE ID = :WS-ID END-EXEC';
    const sql2 = 'EXEC SQL DELETE FROM CUSTOMER WHERE STATUS = :WS-STATUS END-EXEC';
    
    const stmt1 = parser.parseExecSql(sql1)!;
    const stmt2 = parser.parseExecSql(sql2)!;
    
    const repo = generateRepositoryInterface('CUSTOMER', [stmt1, stmt2]);
    
    expect(repo).toContain('CustomerRepository');
    expect(repo).toContain('JpaRepository<Customer, Long>');
    expect(repo).toContain('findById');
  });
});

describe('IMS Parser', () => {
  const parser = new ImsParser();

  it('should parse GU (Get Unique) call', () => {
    const stmt = "CALL 'CBLTDLI' USING GU-FUNC DB-PCB CUSTOMER-SEG CUST-SSA";
    
    const call = parser.parseDliCall(stmt);
    
    expect(call?.function).toBe('GU');
    expect(call?.pcbName).toBe('DB-PCB');
  });

  it('should parse GN (Get Next) call', () => {
    const stmt = "CALL 'CBLTDLI' USING GN-FUNC DB-PCB ORDER-SEG";
    
    const call = parser.parseDliCall(stmt);
    
    expect(call?.function).toBe('GN');
  });

  it('should parse ISRT (Insert) call', () => {
    const stmt = "CALL 'CBLTDLI' USING ISRT-FUNC DB-PCB NEW-SEG";
    
    const call = parser.parseDliCall(stmt);
    
    expect(call?.function).toBe('ISRT');
  });

  it('should convert DL/I to Spring Data', () => {
    const guCall = {
      function: 'GU' as const,
      pcbName: 'DB-PCB',
      ssa: [{ segmentName: 'CUSTOMER' }],
    };
    
    const java = convertDliToSpringData(guCall);
    
    expect(java).toContain('customerRepository');
    expect(java).toContain('findBy');
  });

  it('should generate entity from segment', () => {
    const segment = {
      name: 'CUSTOMER',
      fields: [
        { name: 'CUST-ID', type: 'C' as const, start: 1, length: 10 },
        { name: 'CUST-NAME', type: 'C' as const, start: 11, length: 30 },
        { name: 'BALANCE', type: 'P' as const, start: 41, length: 8 },
      ],
    };
    
    const entity = generateEntityFromSegment(segment);
    
    expect(entity).toContain('@Entity');
    expect(entity).toContain('class Customer');
    expect(entity).toContain('private String custId');
    expect(entity).toContain('private String custName');
    expect(entity).toContain('private BigDecimal balance');
  });
});
