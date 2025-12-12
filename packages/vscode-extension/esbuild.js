const esbuild = require('esbuild');

const production = process.argv.includes('--production');
const watch = process.argv.includes('--watch');

/** @type {import('esbuild').BuildOptions} */
const buildOptions = {
  entryPoints: ['src/extension.ts'],
  bundle: true,
  format: 'cjs',
  minify: production,
  sourcemap: !production,
  sourcesContent: false,
  platform: 'node',
  outfile: 'dist/extension.js',
  external: ['vscode'],
  logLevel: 'info',
  plugins: [
    {
      name: 'watch-plugin',
      setup(build) {
        build.onEnd((result) => {
          if (result.errors.length === 0) {
            console.log('[watch] Build succeeded');
          }
        });
      },
    },
  ],
};

async function main() {
  try {
    if (watch) {
      const ctx = await esbuild.context(buildOptions);
      await ctx.watch();
      console.log('[watch] Watching for changes...');
    } else {
      await esbuild.build(buildOptions);
    }
  } catch (error) {
    console.error('Build failed:', error);
    process.exit(1);
  }
}

main();
