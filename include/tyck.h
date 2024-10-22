#ifndef _TYCK_H
#define _TYCK_H

#include <stdbool.h>

#include "ast.h"
#include "str_pool.h"

/**
 * @brief Perform type checking in the AST
 *
 * @param[in] ast The ast to do the type checking
 * @param[in] strs String pool for the AST
 *
 * @returns true if type checking was successful, false otherwse
 */
bool tyck(const Ast ast, StrPool strs);

#endif /* _TYCK_H */
