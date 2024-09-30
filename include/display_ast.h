#ifndef DISPLAY_AST_H
#define DISPLAY_AST_H

#include "ast.h"
#include "str_pool.h"

/**
 * @brief Displays the subtree of the AST starting from the given root node.
 *
 * @param[in] node_id The ID of a valid statement or expression in the AST.
 * @param[in] stream The output handle where the AST subtree will be printed.
 */
void ast_display(const Ast ast, NodeIdx idx, StrPool strs, FILE *stream);

#endif // DISPLAY_AST_H