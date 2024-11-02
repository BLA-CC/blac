#ifndef _CONSTEVAL_H
#define _CONSTEVAL_H

#include <stdint.h>

#include "ast.h"
#include "ir.h"

int32_t consteval(Ast ast, NodeIdx i, const GlobalVec *globals);

#endif /* _CONSTEVAL_H */
