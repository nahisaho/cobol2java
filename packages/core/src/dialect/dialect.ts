/**
 * COBOL Dialect Detection and Support
 * 
 * Supports: IBM Enterprise COBOL, Micro Focus, GnuCOBOL, COBOL-85
 */

/**
 * Supported COBOL dialects
 */
export type CobolDialect = 
  | 'ibm'           // IBM Enterprise COBOL
  | 'microfocus'    // Micro Focus COBOL
  | 'gnucobol'      // GnuCOBOL (OpenCOBOL)
  | 'cobol85'       // ANSI COBOL-85 Standard
  | 'unknown';

/**
 * Dialect detection result
 */
export interface DialectInfo {
  dialect: CobolDialect;
  confidence: number;  // 0-100
  markers: string[];   // Detected dialect markers
  features: string[];  // Dialect-specific features used
}

/**
 * Dialect-specific keywords and patterns
 */
const DIALECT_PATTERNS: Record<CobolDialect, { keywords: string[]; patterns: RegExp[] }> = {
  ibm: {
    keywords: [
      'EXEC SQL', 'EXEC CICS', 'EXEC DLI', 'END-EXEC',
      'GOBACK', 'ENTRY', 'SERVICE RELOAD',
      'DISPLAY-1', 'DBCS', 'NATIONAL',
      'JSON GENERATE', 'JSON PARSE',
      'XML GENERATE', 'XML PARSE',
      'FUNCTION CURRENT-DATE', 'FUNCTION LOCALE-DATE',
    ],
    patterns: [
      /EXEC\s+SQL/i,
      /EXEC\s+CICS/i,
      /EXEC\s+DLI/i,
      /COMP-5/i,
      /POINTER/i,
      /PROCEDURE-POINTER/i,
      /ADDRESS\s+OF/i,
      /LENGTH\s+OF/i,
      /FUNCTION\s+NATIONAL-OF/i,
      /BINARY-\d+/i,
    ],
  },
  microfocus: {
    keywords: [
      '$SET', 'SOURCEFORMAT', 'CONSTANT',
      'SCREEN SECTION', 'REPORT SECTION',
      'VALIDATE', 'VALID', 'INVALID',
      'CURSOR', 'CRT STATUS',
      'ACCEPT FROM CONSOLE',
      'DISPLAY UPON CONSOLE',
    ],
    patterns: [
      /\$SET\s+/i,
      /\$IF/i,
      /\$END-IF/i,
      /\$REGION/i,
      /LINKAGE\s+TYPE\s+IS/i,
      /CALL-CONVENTION/i,
      /PROGRAM-ID\.\s+\w+\s+IS\s+INITIAL/i,
      /SCREEN\s+SECTION/i,
    ],
  },
  gnucobol: {
    keywords: [
      'ALLOCATE', 'FREE',
      'LOCAL-STORAGE SECTION',
      'REPOSITORY',
      'INTERFACE-ID', 'CLASS-ID', 'FACTORY', 'OBJECT',
      'METHOD-ID', 'PROPERTY-ID',
      'TRY', 'CATCH', 'FINALLY', 'END-TRY',
      'RAISE', 'EXCEPTION-OBJECT',
    ],
    patterns: [
      />>SOURCE\s+FORMAT/i,
      />>DEFINE/i,
      /FUNCTION\s+RANDOM/i,
      /FUNCTION\s+E\b/i,
      /FUNCTION\s+PI\b/i,
      /ALLOCATE/i,
      /FREE\s+/i,
      /\bTRY\b/i,
      /\bCATCH\b/i,
      /\bFINALLY\b/i,
      /END-TRY/i,
    ],
  },
  cobol85: {
    keywords: [
      'IDENTIFICATION DIVISION',
      'ENVIRONMENT DIVISION',
      'DATA DIVISION',
      'PROCEDURE DIVISION',
      'WORKING-STORAGE SECTION',
      'FILE SECTION',
    ],
    patterns: [
      /IDENTIFICATION\s+DIVISION/i,
      /PROCEDURE\s+DIVISION/i,
      /PIC\s+[X9A]+/i,
      /MOVE\s+\w+\s+TO/i,
      /PERFORM\s+\w+/i,
    ],
  },
  unknown: {
    keywords: [],
    patterns: [],
  },
};

/**
 * Dialect-specific feature mappings for Java conversion
 */
export const DIALECT_FEATURES: Record<CobolDialect, Record<string, string>> = {
  ibm: {
    'COMP-5': 'int/long (native binary)',
    'EXEC SQL': 'JDBC/JPA Repository',
    'EXEC CICS': 'Spring Transactional Service',
    'EXEC DLI': 'IMS DB access (custom)',
    'JSON GENERATE': 'Jackson ObjectMapper',
    'JSON PARSE': 'Jackson ObjectMapper',
    'XML GENERATE': 'JAXB Marshaller',
    'XML PARSE': 'JAXB Unmarshaller',
    'POINTER': 'Reference type',
    'NATIONAL': 'String (UTF-16)',
  },
  microfocus: {
    'SCREEN SECTION': 'Swing/JavaFX UI',
    'VALIDATE': 'Bean Validation',
    'CURSOR': 'ResultSet cursor',
    '$SET SOURCEFORMAT': 'Free format handling',
  },
  gnucobol: {
    'ALLOCATE': 'new Object()',
    'FREE': 'null assignment (GC)',
    'TRY': 'try-catch block',
    'CATCH': 'catch clause',
    'RAISE': 'throw exception',
  },
  cobol85: {
    'PERFORM': 'method call/loop',
    'MOVE': 'assignment',
    'COMPUTE': 'arithmetic',
  },
  unknown: {},
};

/**
 * Detect COBOL dialect from source code
 */
export function detectDialect(source: string): DialectInfo {
  const upperSource = source.toUpperCase();
  const scores: Record<CobolDialect, { score: number; markers: string[] }> = {
    ibm: { score: 0, markers: [] },
    microfocus: { score: 0, markers: [] },
    gnucobol: { score: 0, markers: [] },
    cobol85: { score: 0, markers: [] },
    unknown: { score: 0, markers: [] },
  };

  // Check each dialect's patterns
  for (const [dialect, { keywords, patterns }] of Object.entries(DIALECT_PATTERNS)) {
    if (dialect === 'unknown' || dialect === 'cobol85') continue;

    // Check keywords
    for (const keyword of keywords) {
      if (upperSource.includes(keyword.toUpperCase())) {
        scores[dialect as CobolDialect].score += 10;
        scores[dialect as CobolDialect].markers.push(keyword);
      }
    }

    // Check patterns
    for (const pattern of patterns) {
      if (pattern.test(source)) {
        scores[dialect as CobolDialect].score += 15;
        const match = source.match(pattern);
        if (match && !scores[dialect as CobolDialect].markers.includes(match[0])) {
          scores[dialect as CobolDialect].markers.push(match[0]);
        }
      }
    }
  }

  // COBOL-85 is base level - only used if no dialect-specific features found
  // No baseline score added

  // Find best match
  let bestDialect: CobolDialect = 'cobol85';
  let bestScore = 0;

  for (const [dialect, { score }] of Object.entries(scores)) {
    if (dialect !== 'unknown' && dialect !== 'cobol85' && score > bestScore) {
      bestScore = score;
      bestDialect = dialect as CobolDialect;
    }
  }

  // Calculate confidence (normalize to 100)
  const maxPossibleScore = 200;
  const confidence = Math.min(100, Math.round((bestScore / maxPossibleScore) * 100));

  // Get features used
  const features: string[] = [];
  const featureMap = DIALECT_FEATURES[bestDialect];
  for (const marker of scores[bestDialect].markers) {
    const feature = featureMap[marker.toUpperCase()];
    if (feature && !features.includes(feature)) {
      features.push(feature);
    }
  }

  return {
    dialect: bestDialect,
    confidence,
    markers: scores[bestDialect].markers.slice(0, 10),
    features,
  };
}

/**
 * Get dialect-specific conversion hints
 */
export function getDialectHints(dialect: CobolDialect): string[] {
  const hints: string[] = [];

  switch (dialect) {
    case 'ibm':
      hints.push('Use JDBC or JPA for EXEC SQL statements');
      hints.push('Convert EXEC CICS to Spring @Transactional methods');
      hints.push('Use Jackson for JSON operations');
      hints.push('Handle COMP-5 as native int/long');
      break;
    case 'microfocus':
      hints.push('Convert SCREEN SECTION to Swing/JavaFX');
      hints.push('Use Bean Validation for VALIDATE');
      hints.push('Handle $SET directives as preprocessor');
      break;
    case 'gnucobol':
      hints.push('ALLOCATE/FREE map to standard Java object lifecycle');
      hints.push('TRY/CATCH map directly to Java exceptions');
      hints.push('Object-oriented features map to Java classes');
      break;
    case 'cobol85':
      hints.push('Standard COBOL - use basic Java patterns');
      hints.push('PERFORM loops become for/while');
      hints.push('Paragraphs become private methods');
      break;
    default:
      hints.push('Unknown dialect - using standard conversion');
  }

  return hints;
}
