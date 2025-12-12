import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

export default defineConfig({
  plugins: [react()],
  base: process.env.BASE_URL || './',
  build: {
    outDir: 'dist',
    sourcemap: true,
    rollupOptions: {
      external: [
        'vscode',
        'fs', 'path', 'crypto', 'stream', 'events',
        'glob',
        'node:fs', 'node:path', 'node:url', 'node:events', 'node:stream',
        'node:string_decoder', 'node:fs/promises', 'node:crypto',
      ],
    },
  },
  server: {
    port: 3000,
  },
  resolve: {
    alias: {
      // Provide empty stub for vscode module in browser
      vscode: '/src/stubs/vscode.ts',
    },
  },
  optimizeDeps: {
    exclude: ['glob'],
  },
});
