#ifndef _SYM_TABLE
#define _SYM_TABLE

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "common.h"
#include "ast.h"
#include "str_pool.h"
#include "vec.h"

typedef struct {
    Type base;
    NodeIdx params; // if params is NO_NODE, this is a basic type
} TypeInfo;

typedef struct {
    uint32_t loc;
} IrInfo;

typedef struct {
    StrIdx sym;
    uint32_t scope;
    TypeInfo type_info;
    IrInfo ir_info;
} SymInfo;

Vec_Proto(SymInfo)

typedef struct {
    uint32_t cur_scope;
    SymInfoVec symbols;
} SymTable;

/**
 * @brief Pushes a new scope to the symbol table.
 *
 * This function creates a new scope in the symbol table, which is used
 * to track variable declarations and their visibility in nested code blocks.
 *
 * @param[in,out] self Pointer to the SymTable structure.
 */
void symtable_push_scope(SymTable *self);

/**
 * @brief Pops the current scope from the symbol table.
 *
 * This function removes the most recent scope, discarding all symbols
 * declared within that scope.
 *
 * @param[in,out] self Pointer to the SymTable structure.
 * @param[in,out] vstack_top If not NULL, should point to a variable that is
 *                being used to simulate a stack during ir generation.
 */
void symtable_pop_scope(SymTable *self, uint32_t *vstack_top);

/**
 * @brief Adds a symbol to the current scope in the symbol table.
 *
 * This function inserts a new symbol with the given identifier and type
 * into the current scope of the symbol table. If the symbol already exists
 * in the current scope, it is not re-added and the function returns false.
 *
 * @param[in,out] self Pointer to the SymTable structure.
 * @param[in] ident String pool index of the symbol identifier.
 * @param[in] type_info Data type of the symbol being added.
 * @return true if the symbol was successfully added, false otherwise.
 */
bool symtable_put_symbol(
    SymTable *self,
    const StrIdx ident,
    const TypeInfo type_info,
    const IrInfo ir_info);

/**
 * @brief Retrieves a symbol from the symbol table.
 *
 * This function looks up a symbol by its identifier within the current
 * and any enclosing scopes. If found, it returns the symbol's information.
 *
 * @param[in] self Pointer to the SymTable structure.
 * @param[in] sym String pool index of the symbol identifier.
 * @return Pointer to the SymInfo structure containing the symbol's information,
 *         or NULL if the symbol is not found.
 */
SymInfo *symtable_get_symbol(SymTable *self, StrIdx sym);

/**
 * @brief Releases the memory allocated for the symbol table.
 *
 * This function frees all resources associated with the symbol table,
 * including its internal data structures. The symbol table should not
 * be used after calling this function.
 *
 * @param[in,out] self Pointer to the SymTable structure.
 */
void symtable_release(SymTable *self);

/**
 * @brief Displays the contents of the symbol table.
 *
 * This function prints the symbols currently stored in the symbol table,
 * along with their scopes and types. It uses the string pool to resolve
 * symbol identifiers to human-readable strings.
 * TODO: print types
 *
 * @param[in] self Pointer to the SymTable structure.
 * @param[in] strs Pointer to the string pool for resolving symbol identifiers.
 */
void symtable_display(SymTable *self, StrPool *strs);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _SYM_TABLE */
