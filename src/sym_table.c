#include "sym_table.h"
#include "str_pool.h"
#include <stdint.h>
#include <stdio.h>

Vec_Impl(SymInfo);

void symtable_push_scope(SymTable *self) {
    self->cur_scope++;
}

void symtable_pop_scope(SymTable *self) {
    assert(self->cur_scope > 0);

    SymInfoVec *symbols = &self->symbols;

    while ((symbols->len > 0) &&
           (symbols->elems[symbols->len - 1].scope == self->cur_scope)) {
        SymInfoVec_pop(symbols);
    }
    self->cur_scope--;
}

bool symtable_put_symbol(
    SymTable *self,
    const StrIdx ident,
    const TypeInfo type_info) {

    for (int32_t i = self->symbols.len - 1; i >= 0; i--) {
        if (self->cur_scope != self->symbols.elems[i].scope) {
            break;
        } else if (ident == self->symbols.elems[i].sym) {
            return false;
        }
    }

    SymInfo new = { .sym = ident,
                    .scope = self->cur_scope,
                    .type_info = type_info };

    SymInfoVec_push(&self->symbols, new);
    return true;
}

SymInfo *symtable_get_symbol(SymTable *self, StrIdx sym) {
    for (int32_t i = self->symbols.len - 1; i >= 0; i--) {
        if (self->symbols.elems[i].sym == sym) {
            return &self->symbols.elems[i];
        }
    }
    return NULL;
}

void symtable_release(SymTable *self) {
    SymInfoVec_free(&self->symbols);
}

void symtable_display(SymTable *self, StrPool *strs) {
    printf("Symtable length %d\n", self->symbols.len);
    printf("\tCurrent scope %d\n\tSymbols:\n", self->cur_scope);
    for (uint32_t i = 0; i < self->symbols.len; i++) {
        SymInfo *s = &self->symbols.elems[i];
        printf("\t\t%s: %d", StrPool_get(strs, s->sym), s->scope);
        if (s->type_info.params != NO_NODE) {
            printf("(fun)\n");
        } else {
            printf("(var)\n");
        }
    }
}
