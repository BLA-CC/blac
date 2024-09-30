#include "ast.h"

void Ast_release(Ast *ast) {
    free(ast->nodes);
}

Vec_Impl(AstNode);

AstNodeFull_List Ast_full_prog(Ast ast) {
    assert(ast.len != 0);
    AstNode *root = &ast.nodes[0];
    assert(root->kind == AstNodeKind_PROG);
    return (AstNodeFull_List){ .begin = root->data.lhs, .end = root->data.rhs };
}

AstNodeFull_List Ast_full_block(Ast ast, NodeIdx idx) {
    assert(idx < ast.len);
    AstNode *node = &ast.nodes[idx];
    assert(node->kind == AstNodeKind_BLOCK);
    return (AstNodeFull_List){ .begin = node->data.lhs, .end = node->data.rhs };
}

AstNodeFull_VarDecl Ast_full_var_decl(Ast ast, NodeIdx idx) {
    assert(idx < ast.len);
    AstNode *node = &ast.nodes[idx];
    assert(
        node->kind == AstNodeKind_VAR_DECL ||
        node->kind == AstNodeKind_VAR_DECL_INIT);

    AstNodeFull_VarDecl result;
    if (node->kind == AstNodeKind_VAR_DECL_INIT) {
        result = Ast_full_var_decl(ast, node->data.lhs);
        result.init_expr = node->data.rhs;
    } else {
        result.type = node->data.lhs;
        result.ident = node->data.rhs;
        result.init_expr = NO_NODE;
    }
    return result;
}

AstNodeFull_MethDecl Ast_full_meth_decl(Ast ast, NodeIdx idx) {
    assert(idx < ast.len);
    AstNode *node = &ast.nodes[idx];
    assert(
        node->kind == AstNodeKind_METH_DECL ||
        node->kind == AstNodeKind_METH_DECL_IMPL);

    AstNodeFull_MethDecl result;
    if (node->kind == AstNodeKind_METH_DECL_IMPL) {
        result = Ast_full_meth_decl(ast, node->data.lhs);
        result.body = node->data.rhs;
    } else {
        assert(node->data.rhs < ast.len);
        AstNode *proto_node = &ast.nodes[node->data.rhs];
        assert(proto_node->kind == AstNodeKind_METH_PROTO);
        assert(proto_node->data.lhs < ast.len);
        AstNode *params_node = &ast.nodes[proto_node->data.lhs];
        assert(params_node->kind == AstNodeKind_LIST);
        result.ret_type = proto_node->data.rhs;
        result.ident = node->data.lhs;
        result.params_begin = params_node->data.lhs;
        result.params_end = params_node->data.rhs;
        result.body = NO_NODE;
    }
    return result;
}

AstNodeFull_Asgn Ast_full_asgn(Ast ast, NodeIdx idx) {
    assert(idx < ast.len);
    AstNode *node = &ast.nodes[idx];
    assert(node->kind == AstNodeKind_ASGN);
    return (AstNodeFull_Asgn){ .target = node->data.lhs,
                               .expr = node->data.rhs };
}

AstNodeFull_If Ast_full_if(Ast ast, NodeIdx idx) {
    assert(idx < ast.len);
    AstNode *node = &ast.nodes[idx];
    assert(
        node->kind == AstNodeKind_IF_SMP || node->kind == AstNodeKind_IF_ALT);
    AstNodeFull_If result = { .cond = node->data.lhs,
                              .then_b = node->data.rhs };
    if (node->kind == AstNodeKind_IF_ALT) {
        result.else_b = node->data.rhs + 1;
    } else {
        result.else_b = NO_NODE;
    }
    return result;
}

AstNodeFull_While Ast_full_while(Ast ast, NodeIdx idx) {
    assert(idx < ast.len);
    AstNode *node = &ast.nodes[idx];
    assert(node->kind == AstNodeKind_WHILE);
    return (AstNodeFull_While){ .cond = node->data.lhs,
                                .body = node->data.rhs };
}

AstNodeFull_MethCall Ast_full_meth_call(Ast ast, NodeIdx idx) {
    assert(idx < ast.len);
    AstNode *node = &ast.nodes[idx];
    assert(node->kind == AstNodeKind_METH_CALL);
    assert(node->data.rhs < ast.len);
    AstNode *params_node = &ast.nodes[node->data.rhs];
    return (AstNodeFull_MethCall){
        .meth_ident = node->data.lhs,
        .args_begin = params_node->data.lhs,
        .args_end = params_node->data.rhs,
    };
}

AstNodeFull_UnOp Ast_full_unop(Ast ast, NodeIdx idx) {
    assert(idx < ast.len);
    AstNode *node = &ast.nodes[idx];
    assert(node->kind == AstNodeKind_UNM || node->kind == AstNodeKind_NEG);
    return (AstNodeFull_UnOp){ .arg = node->data.lhs, .op = (UnOp)node->kind };
}

AstNodeFull_BinOp Ast_full_binop(Ast ast, NodeIdx idx) {
    assert(idx < ast.len);
    AstNode *node = &ast.nodes[idx];
    assert(node->kind >= AstNodeKind_MUL);
    return (AstNodeFull_BinOp){ .lhs = node->data.lhs,
                                .rhs = node->data.rhs,
                                .op = (BinOp)node->kind };
}
