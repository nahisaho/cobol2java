# COBOL2Java CLI Docker Image
#
# Build: docker build -t cobol2java .
# Run:   docker run -v $(pwd):/data cobol2java convert /data/input.cbl -o /data/output.java

FROM node:20-alpine AS builder

WORKDIR /app

# Install pnpm
RUN corepack enable && corepack prepare pnpm@9 --activate

# Copy package files
COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY packages/core/package.json ./packages/core/
COPY packages/cli/package.json ./packages/cli/

# Install dependencies
RUN pnpm install --frozen-lockfile

# Copy source code
COPY packages/core/ ./packages/core/
COPY packages/cli/ ./packages/cli/
COPY tsconfig.json ./

# Build
RUN pnpm build

# Production image
FROM node:20-alpine AS runner

WORKDIR /app

# Install pnpm
RUN corepack enable && corepack prepare pnpm@9 --activate

# Copy built files
COPY --from=builder /app/package.json ./
COPY --from=builder /app/pnpm-lock.yaml ./
COPY --from=builder /app/pnpm-workspace.yaml ./
COPY --from=builder /app/packages/core/package.json ./packages/core/
COPY --from=builder /app/packages/core/dist ./packages/core/dist
COPY --from=builder /app/packages/cli/package.json ./packages/cli/
COPY --from=builder /app/packages/cli/dist ./packages/cli/dist

# Install production dependencies only
RUN pnpm install --prod --frozen-lockfile

# Create data directory for mounting
RUN mkdir /data
WORKDIR /data

# Set entrypoint
ENTRYPOINT ["node", "/app/packages/cli/dist/bin/cobol2java.js"]
CMD ["--help"]
