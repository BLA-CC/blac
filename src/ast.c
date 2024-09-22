#include "ast.h"
#include "vec.h"
#include <string.h>

Vec_Impl(AstNode);

NodeIdx AstBuilder_mk_list(
    AstNodeVec *ast_nodes,
    AstNodeVec *list_nodes,
    uint32_t begin
) {
    uint32_t list_len = list_nodes->len - begin;
    if (list_len == 0) {
        return NO_NODE;
    }

    AstNodeVec_reserve(ast_nodes, list_len + 1);
    AstNode *ast_end = &ast_nodes->elems[ast_nodes->len];

    ast_end->kind = AstNodeKind_LIST;
    ast_end->data.LIST.begin = ast_nodes->len + 1;
    ast_end->data.LIST.end = ast_nodes->len + 1 + list_len;

    memcpy(ast_end + 1,
           &list_nodes->elems[begin],
           list_len * sizeof(AstNode));
    ast_nodes->len += list_len + 1;
    list_nodes->len -= list_len;

    return ast_nodes->len - list_len - 1;
}
