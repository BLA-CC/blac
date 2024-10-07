#include "str_pool.h"
#include "vec.h"
#include <stdint.h>


typedef struct {
  StrIdx sym;
  uint32_t scope;
  Type type;
} SymInfo;
Vec_Proto(SymInfo)

typedef struct {
    uint32_t cur_scope;
    SymInfoVec symbols;
} SymTable;


void symtable_push_scope(SymTable *self);

void symtable_pop_scope(SymTable *self);

// True si se agregó bien
bool symtable_put_symbol(SymTable *self, const StrIdx ident, const Type type);

SymInfo *symnode_get_symbol(SymTable *self, StrIdx sym);

void symtable_release(SymTable *self);

void symtable_display(SymTable *self, StrPool *strs);