#include "common.h"
#include "str_pool.h"
#include "vec.h"
#include <stdint.h>

typedef enum {
  TypeG_VAR,
  TypeG_FUN,
} TypeG;

Vec_Proto(Type);

typedef struct {
  TypeG tg;
  union {
    Type var;
    TypeVec fun;
  } type;
} TypeInfo;

typedef struct {
  StrIdx sym;
  uint32_t scope;
  TypeInfo type_info;
} SymInfo;

Vec_Proto(SymInfo)

typedef struct {
    uint32_t cur_scope;
    SymInfoVec symbols;
} SymTable;


void symtable_push_scope(SymTable *self);

void symtable_pop_scope(SymTable *self);

bool symtable_put_symbol(SymTable *self, const StrIdx ident, const TypeInfo type);

SymInfo *symnode_get_symbol(SymTable *self, StrIdx sym);

void symtable_release(SymTable *self);

void symtable_display(SymTable *self, StrPool *strs);
