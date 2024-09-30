#ifndef _TDS_PARSER_H
#define _TDS_PARSER_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/**
 * Parser and lexer are bundled together in this header to avoid include order
 * errors, since `_lexer.h` must be included AFTER `_parser.h`.
 */

// clang-format off
#include "_parser.h"
#include "_lexer.h"
// clang-format on

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _TDS_PARSER_H */
