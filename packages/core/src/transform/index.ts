/**
 * Transform module exports
 */

export {
  DATA_TYPE_MAPPINGS,
  STATEMENT_RULES,
  mapDataType,
  toJavaName,
  toClassName,
  transformStatement,
  transformExpression,
  transformCondition,
  parseIfStatement,
  setLevel88Context,
  transform88LevelCondition,
  clearTransformCache,
  getTransformCacheStats,
  type DataTypeMapping,
  type StatementRule,
  type IfBlock,
} from './rules.js';
