#include "tyck.h"
#include "ast.h"
#include "sym_table.h"
#include "display.h"

typedef struct {
    SymTable sym_table;
    Type ret_type;
    bool has_main;
    bool had_error;
} Tyck;

static void tyck_report_mismatch(Location loc, Type expected, Type found) {
    fprintf(
        stderr,
        "Error: Type mismatch (in line %d, col %d). Expected '%s' but found "
        "'%s'.\n",
        loc.line,
        loc.col,
        str_type(expected),
        str_type(found));
}

static void tyck_report_double_decl(Location loc, StrPool *strs, StrIdx ident) {
    fprintf(
        stderr,
        "Error: Redeclaration of '%s' (in line %d, col %d).\n",
        StrPool_get(strs, ident),
        loc.line,
        loc.col);
}

static void tyck_report_undefined(Location loc, StrPool *strs, StrIdx ident) {
    fprintf(
        stderr,
        "Error: Undefined symbol '%s' (in line %d, col %d).\n",
        StrPool_get(strs, ident),
        loc.line,
        loc.col);
}

static void tyck_report_misc(Location *loc, char *msg) {
    fprintf(stderr, "Error: %s", msg);

    if (loc != NULL) {
        fprintf(stderr, " (in line %d, col %d)", loc->line, loc->col);
    }

    fprintf(stderr, ".\n");
}

////////////////////////////////////////////////////////////////////////////////

static Type tyck_expr(Tyck *tyck, Ast ast, StrPool strs, NodeIdx idx, bool is_const);

static Type tyck_meth_call(Tyck *tyck, Ast ast, StrPool strs, NodeIdx idx) {
    AstNode *node = &ast.nodes[idx];
    AstNodeFull_MethCall meth_call = Ast_full_meth_call(ast, idx);

    SymInfo *sym_info =
        symtable_get_symbol(&tyck->sym_table, meth_call.meth_ident);

    if (sym_info == NULL) {
        tyck->had_error = true;
        tyck_report_undefined(node->loc, &strs, meth_call.meth_ident);
        return Type_NONE;
    }

    if (sym_info->type_info.params == NO_NODE) {
        tyck->had_error = true;
        tyck_report_misc(&node->loc, "Cannot call a basic type");
        return Type_NONE;
    }

    AstNodeFull_List args = { meth_call.args_begin, meth_call.args_end };
    AstNodeFull_List params = Ast_full_list(ast, sym_info->type_info.params);
    uint32_t args_len = args.end - args.begin;
    uint32_t params_len = params.end - params.begin;

    if (args_len != params_len) {
        tyck->had_error = true;
        tyck_report_misc(&node->loc, "Incorrect argument count");
        return Type_NONE;
    }

    Type ret_type = sym_info->type_info.base;

    for (uint32_t i = 0; i < params_len; i++) {
        Type arg_type = tyck_expr(tyck, ast, strs, args.begin + i, false);
        if (arg_type == Type_NONE) { return Type_NONE; }

        Type param_type = ast.nodes[params.begin + i].data.lhs;
        if (arg_type != param_type) {
            tyck->had_error = true;
            tyck_report_mismatch(ast.nodes[args.begin + i].loc, param_type, arg_type);
            ret_type = Type_NONE;
        }
    }

    return ret_type;
}

static Type tyck_expr(Tyck *tyck, Ast ast, StrPool strs, NodeIdx idx, bool is_const) {
    AstNode *node = &ast.nodes[idx];

    switch (node->kind) {
    case AstNodeKind_METH_CALL: {
        if (is_const) {
            tyck->had_error = true;
            tyck_report_misc(&node->loc, "Cannot use runtime value to initialize a constant");
            return Type_NONE;
        }
        return tyck_meth_call(tyck, ast, strs, idx);
    }

    case AstNodeKind_VAR: {
        StrIdx ident = node->data.lhs;
        SymInfo *sym_info = symtable_get_symbol(&tyck->sym_table, ident);

        if (sym_info == NULL) {
            tyck->had_error = true;
            tyck_report_undefined(node->loc, &strs, ident);
            return Type_NONE;
        }

        if (sym_info->type_info.params != NO_NODE) {
            tyck->had_error = true;
            tyck_report_misc(&node->loc, "Methods cannot be used as values");
            return Type_NONE;
        }

        return sym_info->type_info.base;
    }

    case AstNodeKind_INT_LIT: return Type_INT;
    case AstNodeKind_BOOL_LIT: return Type_BOOL;

    case AstNodeKind_UNM:
    case AstNodeKind_NEG: {
        NodeIdx operand = node->data.lhs;
        Type expected_type = node->kind == AstNodeKind_UNM ? Type_INT : Type_BOOL;

        Type operand_type = tyck_expr(tyck, ast, strs, operand, is_const);
        if (operand_type == Type_NONE) { return Type_NONE; }

        if (operand_type != expected_type) {
            tyck->had_error = true;
            tyck_report_mismatch(ast.nodes[operand].loc, expected_type, operand_type);
        }

        return operand_type;
    }

    case AstNodeKind_MUL:
    case AstNodeKind_DIV:
    case AstNodeKind_MOD:
    case AstNodeKind_ADD:
    case AstNodeKind_SUB:
    case AstNodeKind_LT:
    case AstNodeKind_GT:
    case AstNodeKind_EQ:
    case AstNodeKind_AND:
    case AstNodeKind_OR: {
        NodeIdx lhs = node->data.lhs;
        NodeIdx rhs = node->data.rhs;

        Type lhs_type = tyck_expr(tyck, ast, strs, lhs, is_const);
        if (lhs_type == Type_NONE) { return Type_NONE; }

        // == is overloaded on both basic types
        Type expected_type;
        if (node->kind == AstNodeKind_EQ) {
            expected_type = lhs_type;
        } else {
            expected_type = node->kind <= AstNodeKind_GT ? Type_INT : Type_BOOL;
            if (lhs_type != expected_type) {
                tyck->had_error = true;
                tyck_report_mismatch(ast.nodes[lhs].loc, expected_type, lhs_type);
            }
        }

        Type rhs_type = tyck_expr(tyck, ast, strs, rhs, is_const);
        if (rhs_type == Type_NONE) { return Type_NONE; }

        if (rhs_type != expected_type) {
            tyck->had_error = true;
            tyck_report_mismatch(ast.nodes[rhs].loc, expected_type, rhs_type);
        }

        return node->kind <= AstNodeKind_SUB ? Type_INT : Type_BOOL;
    }

    default:
        unreachable;
    }
}

static bool tyck_stmt(Tyck *tyck, Ast ast, StrPool strs, NodeIdx idx) {
    AstNode *node = &ast.nodes[idx];

    switch (node->kind) {
    case AstNodeKind_BLOCK: {
        AstNodeFull_List block = Ast_full_block(ast, idx);
        bool has_ret = false;

        symtable_push_scope(&tyck->sym_table);
        for (NodeIdx i = block.begin; i < block.end; i++) {
            has_ret = has_ret || tyck_stmt(tyck, ast, strs, i);
        }
        symtable_pop_scope(&tyck->sym_table, NULL);

        return has_ret;
    }

    case AstNodeKind_VAR_DECL_INIT: {
        AstNodeFull_VarDecl var_decl = Ast_full_var_decl(ast, idx);

        if (!symtable_put_symbol(
                &tyck->sym_table,
                var_decl.ident,
                (TypeInfo){ .base = var_decl.type, .params = NO_NODE },
                (IrInfo){ 0 })) {
            tyck->had_error = true;
            tyck_report_double_decl(node->loc, &strs, var_decl.ident);

            // if it is a double declaration, a type mismatch cannot be reported
            return false;
        }

        Type expr_type = tyck_expr(tyck, ast, strs, var_decl.init_expr, false);
        if (expr_type != Type_NONE && expr_type != var_decl.type) {
            tyck->had_error = true;
            tyck_report_mismatch(ast.nodes[var_decl.init_expr].loc, var_decl.type, expr_type);
        }

        return false;
    }

    case AstNodeKind_ASGN: {
        AstNodeFull_Asgn asgn = Ast_full_asgn(ast, idx);

        SymInfo *sym_info = symtable_get_symbol(&tyck->sym_table, asgn.target);

        if (sym_info == NULL) {
            tyck->had_error = true;
            tyck_report_undefined(node->loc, &strs, asgn.target);
            return false;
        }

        if (sym_info->type_info.params != NO_NODE) {
            tyck->had_error = true;
            tyck_report_misc(&node->loc, "Cannot assign to method");
            return false;
        }

        Type expr_type = tyck_expr(tyck, ast, strs, asgn.expr, false);
        if (expr_type != Type_NONE && expr_type != sym_info->type_info.base) {
            tyck->had_error = true;
            tyck_report_mismatch(ast.nodes[asgn.expr].loc, sym_info->type_info.base, expr_type);
        }

        return false;
    }

    case AstNodeKind_IF_SMP:
    case AstNodeKind_IF_ALT: {
        AstNodeFull_If iff = Ast_full_if(ast, idx);

        Type cond_type = tyck_expr(tyck, ast, strs, iff.cond, false);
        if (cond_type != Type_NONE && cond_type != Type_BOOL) {
            tyck->had_error = true;
            tyck_report_mismatch(ast.nodes[iff.cond].loc, Type_BOOL, cond_type);
        }

        bool then_has_ret = tyck_stmt(tyck, ast, strs, iff.then_b);
        bool else_has_ret = false;

        if (iff.else_b != NO_NODE) {
            else_has_ret = tyck_stmt(tyck, ast, strs, iff.else_b);
        }

        return then_has_ret && else_has_ret;
    }

    case AstNodeKind_WHILE: {
        AstNodeFull_While whilee = Ast_full_while(ast, idx);

        Type cond_type = tyck_expr(tyck, ast, strs, whilee.cond, false);
        if (cond_type != Type_NONE && cond_type != Type_BOOL) {
            tyck->had_error = true;
            tyck_report_mismatch(ast.nodes[whilee.cond].loc, Type_BOOL, cond_type);
        }

        tyck_stmt(tyck, ast, strs, whilee.body);

        return false;
    }
    case AstNodeKind_RET: {
        NodeIdx expr_idx = node->data.lhs;

        if (expr_idx != NO_NODE) {
            Type expr_type = tyck_expr(tyck, ast, strs, expr_idx, false);

            if (expr_idx != Type_NONE && expr_type != tyck->ret_type) {
                tyck->had_error = true;
                tyck_report_mismatch(ast.nodes[expr_idx].loc, tyck->ret_type, expr_type);
            }
        } else if (tyck->ret_type != Type_VOID) {
            tyck->had_error = true;
            tyck_report_mismatch(ast.nodes[expr_idx].loc, tyck->ret_type, Type_VOID);
        }

        return true;
    }

    case AstNodeKind_METH_CALL: {
        tyck_meth_call(tyck, ast, strs, idx);
        return false;
    }
    default:
        unreachable;
    }
}

static void tyck_global_decl(Tyck *tyck, Ast ast, StrPool strs, NodeIdx idx) {
    AstNodeFull_VarDecl var_decl = Ast_full_var_decl(ast, idx);

    if (!symtable_put_symbol(
            &tyck->sym_table,
            var_decl.ident,
            (TypeInfo){ .base = var_decl.type, .params = NO_NODE },
            (IrInfo){ 0 })) {
        tyck->had_error = true;
        tyck_report_double_decl(ast.nodes[idx].loc, &strs, var_decl.ident);

        // if it is a double declaration, a type mismatch cannot be reported
        return;
    }

    Type expr_type = tyck_expr(tyck, ast, strs, var_decl.init_expr, true);
    if (expr_type != Type_NONE && expr_type != var_decl.type) {
        tyck->had_error = true;
        tyck_report_mismatch(ast.nodes[var_decl.init_expr].loc, var_decl.type, expr_type);
    }
}

static void tyck_meth_decl(Tyck *tyck, Ast ast, StrPool strs, NodeIdx idx) {
    AstNodeFull_MethDecl meth_decl = Ast_full_meth_decl(ast, idx);

    // before checking, to allow recursive calls
    if (!symtable_put_symbol(
            &tyck->sym_table,
            meth_decl.ident,
            (TypeInfo){ .base = meth_decl.ret_type,
                        .params = meth_decl.params },
            (IrInfo){ 0 })) {
        tyck->had_error = true;
        tyck_report_double_decl(ast.nodes[idx].loc, &strs, meth_decl.ident);
    }

    AstNodeFull_List params = Ast_full_list(ast, meth_decl.params);

    if (strcmp(StrPool_get(&strs, meth_decl.ident), "main") == 0) {
        tyck->has_main = true;

        if (params.end > params.begin) {
            tyck->had_error = true;
            tyck_report_misc(&ast.nodes[idx].loc, "main method shouldn't have arguments");
        }

        if (meth_decl.body == NO_NODE) {
            tyck->had_error = true;
            tyck_report_misc(&ast.nodes[idx].loc, "missing main method implementation");
        }
    }

    // Nothing to check if there is no body
    if (meth_decl.body == NO_NODE) { return; }

    symtable_push_scope(&tyck->sym_table); // parameter scope

    for (NodeIdx i = params.begin; i < params.end; i++) {
        AstNode *param_node = &ast.nodes[i];
        assert(param_node->kind == AstNodeKind_PARAM);
        Type param_type = param_node->data.lhs;
        Type param_name = param_node->data.rhs;

        if (!symtable_put_symbol(
                &tyck->sym_table,
                param_name,
                (TypeInfo){ .base = param_type },
                (IrInfo){ 0 })) {
            tyck->had_error = true;
            tyck_report_double_decl(param_node->loc, &strs, param_name);
        }
    }

    tyck->ret_type = meth_decl.ret_type;

    assert(ast.nodes[meth_decl.body].kind == AstNodeKind_BLOCK);
    tyck_stmt(tyck, ast, strs, meth_decl.body);

    symtable_pop_scope(&tyck->sym_table, NULL); // parameter scope
}

static void tyck_prog(Tyck *tyck, Ast ast, StrPool strs) {
    AstNodeFull_List prog = Ast_full_prog(ast);

    for (uint32_t i = prog.begin; i < prog.end; i++) {
        AstNode *node = &ast.nodes[i];
        switch (node->kind) {
        case AstNodeKind_VAR_DECL_INIT:
        case AstNodeKind_VAR_DECL:
            tyck_global_decl(tyck, ast, strs, i);
            break;

        case AstNodeKind_METH_DECL_IMPL:
        case AstNodeKind_METH_DECL:
            tyck_meth_decl(tyck, ast, strs, i);
            break;

        default:
            unreachable;
        }
    }

    if (!tyck->has_main) {
        tyck->had_error = true;
        tyck_report_misc(NULL, "main method not defined");
    }
}

bool tyck(const Ast ast, StrPool strs) {
    Tyck tyck = { 0 };

    tyck_prog(&tyck, ast, strs);

    SymInfoVec_free(&tyck.sym_table.symbols);

    return !tyck.had_error;
}
