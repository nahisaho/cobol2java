import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['packages/**/__tests__/**/*.test.ts'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      include: ['packages/*/src/**/*.ts'],
      exclude: [
        '**/__tests__/**',
        '**/bin/**',
        '**/vscode-extension/**',
        '**/webapp/**',
        // Exclude hard-to-test modules
        '**/cli/src/**',         // CLI commands require process/file I/O
        '**/web/src/**',         // VS Code extension
        '**/llm/**',             // LLM clients require external services
        '**/batch/**',           // Batch converter requires file system
      ],
      thresholds: {
        statements: 80,
        branches: 75,
        functions: 55,  // Lower threshold for functions due to error handlers and edge case code
        lines: 80,
      },
    },
    testTimeout: 30000,
  },
});
