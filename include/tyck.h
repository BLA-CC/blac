#ifndef _TYCK_H
#define _TYCK_H

#include <stdbool.h>

#include "ast.h"
#include "str_pool.h"

bool tyck(const Ast ast, StrPool strs);

#endif /* _TYCK_H */
