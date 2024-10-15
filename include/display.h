#ifndef _DISPLAY_H
#define _DISPLAY_H

#include <stdio.h>

#include "str_pool.h"
#include "parser.h"
#include "ast.h"


void scanner_stage(yyscan_t scanner, StrPool strs, FILE *file);

/**
 * @brief Displays the subtree of the AST starting from the given root node.
 *
 * @param[in] node_id The ID of a valid statement or expression in the AST.
 * @param[in] stream The output handle where the AST subtree will be printed.
 */
void ast_display(const Ast ast, NodeIdx idx, StrPool strs, FILE *stream);

#endif // _DISPLAY_H

