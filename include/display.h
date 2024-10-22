#ifndef _DISPLAY_H
#define _DISPLAY_H

#include <stdio.h>

#include "str_pool.h"
#include "parser.h"
#include "ast.h"

/**
 * @brief Displays the tokens produced by scanner. Consumes scanner.
 *
 * @param[in] scanner The lexer instance to query for tokens.
 * @param[in] stream The output handle where to print the tokens.
 */
void display_toks(yyscan_t scanner, StrPool strs, FILE *file);

/**
 * @brief Displays the subtree of the AST starting from the given root node.
 *
 * @param[in] node_id The ID of a valid statement or expression in the AST.
 * @param[in] stream The output handle where the AST subtree will be printed.
 */
void display_ast(const Ast ast, StrPool strs, NodeIdx indent, FILE *stream);

const char *str_type(Type type);

#endif // _DISPLAY_H

