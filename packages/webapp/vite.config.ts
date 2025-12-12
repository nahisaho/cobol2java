import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

export default defineConfig({
  plugins: [react()],
  base: './',
  build: {
    outDir: 'dist',
    sourcemap: true,
    rollupOptions: {
      external: ['vscode'],
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
});
