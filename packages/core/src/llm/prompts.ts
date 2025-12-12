/**
 * Prompt Templates for COBOL2Java LLM Operations
 *
 * Structured prompt templates for various COBOL-to-Java conversion tasks.
 * Each template is designed to produce consistent, high-quality outputs.
 */

/**
 * Template variable types
 */
export interface TemplateVariables {
  [key: string]: string | number | boolean | undefined;
}

/**
 * Prompt template with metadata
 */
export interface PromptTemplate {
  /** Template identifier */
  id: string;
  /** Human-readable name */
  name: string;
  /** Description of the template's purpose */
  description: string;
  /** System prompt for the LLM */
  systemPrompt: string;
  /** User prompt template with {{variable}} placeholders */
  userPromptTemplate: string;
  /** Required variables for this template */
  requiredVariables: string[];
  /** Recommended temperature */
  temperature: number;
  /** Recommended max tokens */
  maxTokens: number;
}

/**
 * Render a prompt template with variables
 */
export function renderPrompt(
  template: PromptTemplate,
  variables: TemplateVariables
): { systemPrompt: string; userPrompt: string } {
  // Validate required variables
  for (const required of template.requiredVariables) {
    if (!(required in variables) || variables[required] === undefined) {
      throw new Error(`Missing required variable: ${required}`);
    }
  }

  // Render user prompt with variable substitution
  let userPrompt = template.userPromptTemplate;
  for (const [key, value] of Object.entries(variables)) {
    userPrompt = userPrompt.replace(
      new RegExp(`\\{\\{${key}\\}\\}`, 'g'),
      String(value ?? '')
    );
  }

  return {
    systemPrompt: template.systemPrompt,
    userPrompt,
  };
}

// ============================================================================
// COBOL Analysis Templates
// ============================================================================

/**
 * Template for analyzing COBOL code structure
 */
export const COBOL_ANALYSIS_TEMPLATE: PromptTemplate = {
  id: 'cobol-analysis',
  name: 'COBOL Code Analysis',
  description: 'Analyze COBOL code structure and identify conversion challenges',
  systemPrompt: `You are an expert COBOL analyst. Your task is to analyze COBOL source code and provide a structured analysis including:

1. **Program Overview**: Purpose and main functionality
2. **Data Structures**: Key data items and their usage
3. **Control Flow**: Main processing logic and sections/paragraphs
4. **External Dependencies**: File I/O, database calls, external program calls
5. **Conversion Challenges**: Potential issues for Java conversion
6. **Recommended Approach**: Best strategy for conversion

Provide output in a structured format. Be precise and technical.`,
  userPromptTemplate: `Analyze the following COBOL program:

\`\`\`cobol
{{cobolSource}}
\`\`\`

{{additionalContext}}`,
  requiredVariables: ['cobolSource'],
  temperature: 0.3,
  maxTokens: 4096,
};

/**
 * Template for identifying COBOL patterns
 */
export const COBOL_PATTERN_TEMPLATE: PromptTemplate = {
  id: 'cobol-patterns',
  name: 'COBOL Pattern Detection',
  description: 'Identify common COBOL patterns for optimized conversion',
  systemPrompt: `You are an expert in COBOL pattern recognition. Identify common patterns in COBOL code that have well-known Java equivalents.

Look for:
- Report generation patterns (PROCEDURE DIVISION with WRITE statements)
- Table handling patterns (SEARCH, SEARCH ALL)
- String manipulation patterns (STRING, UNSTRING, INSPECT)
- Arithmetic patterns (COMPUTE, ADD, SUBTRACT, MULTIPLY, DIVIDE)
- Date handling patterns (ACCEPT DATE, date computations)
- File processing patterns (READ, WRITE, REWRITE)
- Error handling patterns (ON SIZE ERROR, AT END)
- Copybook usage patterns

Output as a JSON array of detected patterns with:
- patternType: The type of pattern
- location: Line numbers or section names
- javaEquivalent: Recommended Java approach
- complexity: 'simple' | 'moderate' | 'complex'`,
  userPromptTemplate: `Detect patterns in this COBOL code:

\`\`\`cobol
{{cobolSource}}
\`\`\`

Return only valid JSON array.`,
  requiredVariables: ['cobolSource'],
  temperature: 0.1,
  maxTokens: 2048,
};

// ============================================================================
// Code Conversion Templates
// ============================================================================

/**
 * Main COBOL to Java conversion template
 */
export const COBOL2JAVA_TEMPLATE: PromptTemplate = {
  id: 'cobol2java',
  name: 'COBOL to Java Conversion',
  description: 'Convert COBOL code to idiomatic Java',
  systemPrompt: `You are an expert COBOL to Java converter. Convert COBOL code to clean, idiomatic Java following these rules:

## Type Mappings
- PIC X(n) → String
- PIC 9(n) → int or long (depending on size)
- PIC 9(n)V9(m) / PIC S9(n)V9(m) COMP-3 → BigDecimal
- PIC S9(n) COMP / COMP-5 → int or long
- OCCURS clause → Java arrays or List<>
- 88-level conditions → boolean methods or enums
- REDEFINES → Java class with multiple representations

## Java Conventions
- Use camelCase for methods and variables
- Use PascalCase for classes
- Add @Override annotations where appropriate
- Use Optional<> for nullable values
- Use BigDecimal for financial calculations
- Add JavaDoc comments for public methods

## Output Format
Output only valid Java code. Include all necessary imports.
Do not include explanations unless the code is complex.`,
  userPromptTemplate: `Convert this COBOL code to Java:

\`\`\`cobol
{{cobolSource}}
\`\`\`

{{conversionNotes}}`,
  requiredVariables: ['cobolSource'],
  temperature: 0.2,
  maxTokens: 8192,
};

/**
 * Spring Boot specific conversion template
 */
export const COBOL2SPRINGBOOT_TEMPLATE: PromptTemplate = {
  id: 'cobol2springboot',
  name: 'COBOL to Spring Boot Conversion',
  description: 'Convert COBOL code to Spring Boot service',
  systemPrompt: `You are an expert COBOL to Spring Boot converter. Convert COBOL programs to Spring Boot services following these patterns:

## Spring Boot Conventions
- Use @Service for business logic classes
- Use @Repository for data access
- Use @RestController for API endpoints
- Use constructor injection for dependencies
- Use @Transactional for database operations
- Use Lombok annotations (@Data, @Builder, @Slf4j)

## File I/O Mapping
- Sequential file → Spring Batch ItemReader/ItemWriter
- Indexed file (VSAM) → JPA Repository
- Report files → Thymeleaf templates or PDF generation

## Error Handling
- Use custom exceptions extending RuntimeException
- Use @ExceptionHandler for API error responses
- Log with SLF4J (@Slf4j)

## Output Structure
Provide complete Java classes with all annotations and imports.
Include application.yml snippets if configuration is needed.`,
  userPromptTemplate: `Convert this COBOL program to a Spring Boot service:

\`\`\`cobol
{{cobolSource}}
\`\`\`

Target Spring Boot version: {{springVersion}}
Package name: {{packageName}}
{{additionalRequirements}}`,
  requiredVariables: ['cobolSource'],
  temperature: 0.2,
  maxTokens: 8192,
};

// ============================================================================
// Code Optimization Templates
// ============================================================================

/**
 * Template for optimizing generated Java code
 */
export const JAVA_OPTIMIZATION_TEMPLATE: PromptTemplate = {
  id: 'java-optimization',
  name: 'Java Code Optimization',
  description: 'Optimize generated Java code for readability and performance',
  systemPrompt: `You are a Java code optimization expert. Review and optimize the provided Java code:

## Optimization Goals
1. **Readability**: Clear naming, proper formatting, good comments
2. **Performance**: Efficient algorithms, proper data structures
3. **Maintainability**: SOLID principles, DRY, clean architecture
4. **Modern Java**: Use Java 17+ features where appropriate

## Specific Improvements
- Replace verbose patterns with Stream API
- Use switch expressions instead of switch statements
- Use record classes for data containers
- Use sealed classes for type hierarchies
- Optimize string concatenation
- Use appropriate collection types

## Output Format
Provide the optimized code with inline comments explaining significant changes.`,
  userPromptTemplate: `Optimize this Java code:

\`\`\`java
{{javaSource}}
\`\`\`

Focus areas: {{focusAreas}}`,
  requiredVariables: ['javaSource'],
  temperature: 0.3,
  maxTokens: 8192,
};

/**
 * Template for adding Java tests
 */
export const JAVA_TEST_TEMPLATE: PromptTemplate = {
  id: 'java-test-generation',
  name: 'Java Test Generation',
  description: 'Generate JUnit 5 tests for Java code',
  systemPrompt: `You are a Java testing expert. Generate comprehensive JUnit 5 tests for the provided Java code.

## Testing Guidelines
- Use JUnit 5 (@Test, @DisplayName, @BeforeEach, etc.)
- Use AssertJ for fluent assertions
- Use Mockito for mocking dependencies
- Cover edge cases and error conditions
- Use parameterized tests for multiple inputs
- Test both positive and negative scenarios

## Test Structure
- Use @Nested for grouping related tests
- Use descriptive @DisplayName annotations
- Follow AAA pattern (Arrange, Act, Assert)
- One assertion per test when practical

## Output Format
Provide complete test class with all imports.`,
  userPromptTemplate: `Generate JUnit 5 tests for:

\`\`\`java
{{javaSource}}
\`\`\`

Focus on: {{testFocus}}`,
  requiredVariables: ['javaSource'],
  temperature: 0.3,
  maxTokens: 8192,
};

// ============================================================================
// Documentation Templates
// ============================================================================

/**
 * Template for generating documentation
 */
export const DOCUMENTATION_TEMPLATE: PromptTemplate = {
  id: 'documentation',
  name: 'Code Documentation',
  description: 'Generate documentation for COBOL and Java code',
  systemPrompt: `You are a technical documentation expert. Generate clear, comprehensive documentation for code.

## Documentation Standards
- Start with a high-level overview
- Explain business purpose and context
- Document inputs, outputs, and side effects
- Include usage examples
- Note any assumptions or limitations
- Use Markdown formatting

## For COBOL Code
- Explain the program's purpose
- Document data structures
- Describe file layouts
- List external dependencies

## For Java Code
- Generate JavaDoc comments
- Document public API
- Include @param, @return, @throws
- Add usage examples`,
  userPromptTemplate: `Generate documentation for:

\`\`\`{{language}}
{{sourceCode}}
\`\`\`

Documentation style: {{style}}`,
  requiredVariables: ['sourceCode', 'language'],
  temperature: 0.4,
  maxTokens: 4096,
};

// ============================================================================
// Template Registry
// ============================================================================

/**
 * All available prompt templates
 */
export const PROMPT_TEMPLATES: Record<string, PromptTemplate> = {
  'cobol-analysis': COBOL_ANALYSIS_TEMPLATE,
  'cobol-patterns': COBOL_PATTERN_TEMPLATE,
  'cobol2java': COBOL2JAVA_TEMPLATE,
  'cobol2springboot': COBOL2SPRINGBOOT_TEMPLATE,
  'java-optimization': JAVA_OPTIMIZATION_TEMPLATE,
  'java-test-generation': JAVA_TEST_TEMPLATE,
  'documentation': DOCUMENTATION_TEMPLATE,
};

/**
 * Get a prompt template by ID
 */
export function getPromptTemplate(id: string): PromptTemplate | undefined {
  return PROMPT_TEMPLATES[id];
}

/**
 * List all available template IDs
 */
export function listTemplates(): string[] {
  return Object.keys(PROMPT_TEMPLATES);
}
