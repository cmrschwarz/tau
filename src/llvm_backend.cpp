#include "llvm_backend.hpp"
extern "C" {
#include "thread_context.h"
#include "utils/pool.h"
#include "tauc.h"
int llvm_backend_init_globals()
{
    // InitializeAllTargetInfos();
    LLVMInitializeNativeTarget();
    // InitializeAllTargetMCs();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();
    return 0;
}
void llvm_backend_fin_globals()
{
    llvm::llvm_shutdown();
}

llvm_backend* llvm_backend_new(thread_context* tc)
{
    LLVMBackend* b =
        (LLVMBackend*)pool_alloc(&tc->permmem, sizeof(LLVMBackend));
    if (!b) return NULL;
    if (LLVMBackend::InitLLVMBackend(b, tc)) return NULL;
    return b;
}

void llvm_backend_delete(llvm_backend* llvmb)
{
    LLVMBackend::FinLLVMBackend((LLVMBackend*)llvmb);
}

llvm_error llvm_backend_emit_module(
    llvm_backend* llvmb, mdg_node** start, mdg_node** end, ureg startid,
    ureg endid, ureg private_sym_count, llvm_module** mod)
{
    return ((LLVMBackend*)llvmb)
        ->createLLVMModule(
            start, end, startid, endid, private_sym_count, (LLVMModule**)mod);
}

void llvm_free_module(llvm_module* mod)
{
    delete (LLVMModule*)mod;
}
int llvm_link_modules(llvm_module** start, llvm_module** end, char* output_path)
{
    printf("linking {");
    for (LLVMModule** n = (LLVMModule**)start; !ptreq(n, end); n++) {
        printf("%s%s", (**n).module_str.c_str(), ptreq(n + 1, end) ? "" : ", ");
    }
    puts("}");
    llvm_error lle =
        linkLLVMModules((LLVMModule**)start, (LLVMModule**)end, output_path);
    if (lle) return ERR;
    return OK;
}
}

// CPP
LLVMBackend::LLVMBackend(thread_context* tc)
    : _tc(tc), _context(), _builder(_context),
      _global_value_store(atomic_ureg_load(&TAUC.node_ids), NULL)
{
    std::string err;
    auto target_triple = LLVMGetDefaultTargetTriple();
    auto target = llvm::TargetRegistry::lookupTarget(target_triple, err);
    if (err.length() > 0) { // TODO: properly
        llvm::errs() << err << "\n";
    }
    llvm::TargetOptions opt;
    _tm = target->createTargetMachine(
        target_triple, LLVMGetHostCPUName(), LLVMGetHostCPUFeatures(), opt,
        llvm::Optional<llvm::Reloc::Model>());
    _reg_size = _tm->getMCRegisterInfo()->getRegClass(0).RegsSize;
}

int LLVMBackend::InitLLVMBackend(LLVMBackend* llvmb, thread_context* tc)
{
    void* res = new (llvmb) LLVMBackend(tc); // placement new is noexcept
    if (!res) return ERR;
    if (llvmb->setup()) return ERR;
    return OK;
}
llvm_error LLVMBackend::setup()
{
    for (ureg i = 0; i < PRIMITIVE_COUNT; i++) {
        addPrimitive(PRIMITIVES[i].type_id, PRIMITIVES[i].sym.node.pt_kind);
    }
    return LLE_OK;
}
void LLVMBackend::FinLLVMBackend(LLVMBackend* llvmb)
{
}

void LLVMBackend::addPrimitive(ureg id, primitive_kind pk)
{
    llvm::Type* t;
    switch (pk) {
        case PT_INT:
        case PT_UINT: t = _builder.getInt32Ty(); break;
        case PT_BINARY_STRING:
        case PT_STRING: t = _builder.getInt8PtrTy(); break;
        case PT_FLOAT: t = _builder.getFloatTy(); break;
        case PT_VOID: t = _builder.getVoidTy(); break;
        default: assert(false); return;
    }
    _global_value_store[id] = (void*)t;
}
llvm_error LLVMBackend::createLLVMModule(
    mdg_node** start, mdg_node** end, ureg startid, ureg endid,
    ureg private_sym_count, LLVMModule** module)
{
    // create our LLVMModule wrapper thingy for linking
    LLVMModule* m = new LLVMModule();
    printf("generating {");
    for (mdg_node** n = start; n != end; n++) {
        m->module_str += (**n).name;
        fputs((**n).name, stdout);
        if (n + 1 != end) {
            fputs(", ", stdout);
            m->module_str += "&";
        }
    }
    m->name = "foo.obj";
    puts("}");
    *module = m;
    // init id space
    _mod_startid = startid;
    _mod_endid = endid;
    if (_global_value_store.size() <= endid) {
        _global_value_store.resize(endid + 1, NULL);
    }
    _private_sym_count = private_sym_count;
    // i really hope this doesn't realloc
    _local_value_store.assign(_private_sym_count, NULL);
    // create actual module
    _mod = new (std::nothrow) llvm::Module(m->name, _context);
    if (!_mod) return LLE_OK;
    llvm_error lle = addModulesIR(start, end);
    if (lle) return lle;
    return emitModule(m->name);
}

llvm_error LLVMBackend::addModulesIR(mdg_node** start, mdg_node** end)
{
    for (mdg_node** n = start; n != end; n++) {
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &(**n).open_scopes);
        for (open_scope* osc = (open_scope*)aseglist_iterator_next(&it); osc;
             osc = (open_scope*)aseglist_iterator_next(&it)) {
            llvm_error lle = genMdgNodeIR((ast_node*)osc, NULL);
            if (lle) return lle;
        }
    }
    return LLE_OK;
}
llvm_error LLVMBackend::addAstBodyIR(ast_body* b)
{
    for (ast_node** n = b->elements; *n; n++) {
        llvm_error lle = genMdgNodeIR(*n, NULL);
        if (lle) return lle;
    }
    return LLE_OK;
}
void** LLVMBackend::lookupAstElem(ureg id)
{
    if (id >= UREGH_MAX) {
        return &_local_value_store[id - UREGH_MAX];
    }
    else {
        return &_global_value_store[id];
    }
}
void LLVMBackend::storeAstElem(ureg id, void* val)
{
    *lookupAstElem(id) = val;
}
llvm_error LLVMBackend::lookupValue(ureg id, ast_node* n, llvm::Value** val)
{
    auto v = (llvm::Value**)lookupAstElem(id);
    if (*v) {
        *val = *v;
        return LLE_OK;
    }
    return genMdgNodeIR(n, val);
}
llvm_error LLVMBackend::lookupType(ureg id, ast_elem* e, llvm::Type** type)
{
    auto t = (llvm::Type**)lookupAstElem(id);
    if (*t) {
        *type = *t;
        return LLE_OK;
    }
    assert(false); // TODO: getPointerTo(), yadda yadda
    return LLE_FATAL;
}
llvm_error LLVMBackend::lookupCType(ast_elem* e, llvm::Type** t)
{
    switch (e->kind) {
        case PRIMITIVE:
            *t = *(llvm::Type**)lookupAstElem(((primitive*)e)->type_id);
            break;
        default: {
            assert(false); // TODO
        }
    }
    return LLE_OK;
}
llvm::Type* LLVMBackend::lookupPrimitive(primitive_kind pk)
{
    return (llvm::Type*)_global_value_store[PRIMITIVES[0].type_id + pk];
}
llvm_error LLVMBackend::genMdgNodeIR(ast_node* n, llvm::Value** val)
{
    // TODO: proper error handling
    llvm_error lle;
    switch (n->kind) {
        case OSC_MODULE:
        case OSC_EXTEND: return addAstBodyIR(&((open_scope*)n)->scp.body);
        case SC_FUNC: return genFunctionIR((sc_func*)n);
        case EXPR_OP_BINARY: return genBinaryOpIR((expr_op_binary*)n, val);
        case EXPR_LITERAL: {
            expr_literal* l = (expr_literal*)n;
            switch (n->pt_kind) {
                case PT_INT:
                case PT_UINT: {
                    auto c = llvm::ConstantInt::get(
                        (llvm::IntegerType*)lookupPrimitive(n->pt_kind),
                        l->value.str, 10);
                    if (!c) return LLE_FATAL;
                    if (val) *val = c;
                    return LLE_OK;
                }
                case PT_STRING: {
                    auto c = _builder.CreateGlobalStringPtr(l->value.str);
                    if (!c) return LLE_FATAL;
                    if (val) *val = c;
                    return LLE_OK;
                }
                /* case PT_FLOAT:
                     return llvm::ConstantFP::get(
                         _context,
                         llvm::APFloat::convertFromString(
                             l->value.str,
                             llvm::APFloatBase::roundingMode::rmTowardZero));*/
                default: assert(false);
            }
        }
        case SYM_PARAM: {
            sym_param* p = (sym_param*)n;
            sc_func* f = (sc_func*)p->sym.declaring_st->owning_node;
            ureg param_nr = p - f->params;
            llvm::Function* fn;
            lle = lookupValue(f->id, (ast_node*)f, (llvm::Value**)&fn);
            if (lle) return lle;
            llvm::Argument* a = fn->arg_begin() + param_nr;
            if (!a) return LLE_FATAL;
            assert(val);
            *val = a;
            return LLE_OK;
        }
        case EXPR_IDENTIFIER: {
            return genMdgNodeIR(
                (ast_node*)((expr_identifier*)n)->value.sym, val);
        }
        case EXPR_RETURN: {
            llvm::Value* v;
            lle = genMdgNodeIR(((expr_break*)n)->value, &v);
            if (lle) return lle;
            if (!_builder.CreateRet(v)) return LLE_FATAL;
            assert(!val);
            return LLE_OK;
        }
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            llvm::Value** args = (llvm::Value**)pool_alloc(
                &_tc->permmem, sizeof(llvm::Value*) * c->arg_count);
            if (!args) return LLE_FATAL;
            for (ureg i = 0; i < c->arg_count; i++) {
                lle = genMdgNodeIR(c->args[i], &args[i]);
                if (lle) return lle;
            }
            llvm::ArrayRef<llvm::Value*> args_arr_ref(
                args, args + c->arg_count);
            llvm::Value* callee;
            lle = lookupValue(c->target->id, (ast_node*)c->target, &callee);
            if (lle) return lle;
            auto call = _builder.CreateCall(callee, args_arr_ref);
            if (!call) return LLE_FATAL;
            if (val) *val = call;
            return LLE_OK;
        }
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: {
            if (ast_node_flags_get_const(n->flags)) {
                // TODO (ctfe)
                assert(false);
            }
            sym_var* var = (sym_var*)n;
            ast_node_kind k = var->sym.declaring_st->owning_node->kind;
            if (k == ELEM_MDG_NODE || k == OSC_MODULE || k == OSC_EXTEND) {
                // global var
            }
            else if (k == SC_STRUCT) {
                // struct var
            }
            else {
                // local var
                llvm::Type* t;
                lle = lookupCType(var->ctype, &t);
                if (lle) return lle;
                auto alloc = _builder.CreateAlloca(t);
                if (!alloc) return LLE_FATAL;
                if (n->kind == SYM_VAR_INITIALIZED) {
                    llvm::Value* v;
                    lle = genMdgNodeIR(
                        ((sym_var_initialized*)n)->initial_value, &v);
                    if (lle) return lle;
                    if (!_builder.CreateStore(v, alloc, false))
                        return LLE_FATAL;
                }
                auto load = _builder.CreateLoad(alloc);
                if (!load) return LLE_FATAL;
                storeAstElem(var->var_id, load);
                if (val) *val = load;
                return LLE_OK;
            }
        }
        case SYM_IMPORT_GROUP:
        case SYM_IMPORT_MODULE: return LLE_OK;
        default: assert(false);
    }
    assert(false);
    return LLE_FATAL;
}
llvm_error LLVMBackend::genBinaryOpIR(expr_op_binary* b, llvm::Value** val)
{
    if (b->op->kind == SC_FUNC) {
        assert(false); // TODO
    }
    llvm::Value *lhs, *rhs;
    llvm_error lle = genMdgNodeIR(b->lhs, &lhs);
    if (lle) return lle;
    lle = genMdgNodeIR(b->rhs, &rhs);
    if (lle) return lle;
    assert(val);
    switch (b->node.op_kind) {
        case OP_ADD: *val = _builder.CreateAdd(lhs, rhs); break;
        case OP_SUB: *val = _builder.CreateSub(lhs, rhs); break;
        case OP_MUL: *val = _builder.CreateMul(lhs, rhs); break;
        case OP_DIV: *val = _builder.CreateSDiv(lhs, rhs); break;
        case OP_MOD: *val = _builder.CreateSRem(lhs, rhs); break;
        default: assert(false);
    }
    if (!*val) return LLE_FATAL;
    return LLE_OK;
}
llvm_error LLVMBackend::genFunctionIR(sc_func* fn)
{
    llvm::FunctionType* func_sig;
    llvm::Type* ret_type;
    llvm_error lle = lookupCType(fn->return_ctype, &ret_type);
    if (lle) return lle;

    if (fn->param_count != 0) {
        llvm::Type** params = (llvm::Type**)pool_alloc(
            &_tc->permmem, sizeof(llvm::Type*) * fn->param_count);
        if (!params) return LLE_FATAL;
        for (ureg i = 0; i < fn->param_count; i++) {
            lle = lookupCType(fn->params[i].ctype, &params[i]);
            if (lle) return lle;
        }
        llvm::ArrayRef<llvm::Type*> params_array_ref(
            params, params + fn->param_count);
        func_sig = llvm::FunctionType::get(ret_type, params_array_ref, false);
        if (!func_sig) return LLE_FATAL;
    }
    else {
        func_sig = llvm::FunctionType::get(ret_type, false);
        if (!func_sig) return LLE_FATAL;
    }

    if (*fn->scp.body.elements) {
        auto lt = llvm::Function::ExternalLinkage;
        if (fn->id >= UREGH_MAX) lt = llvm::Function::InternalLinkage;
        llvm::Function* func =
            llvm::Function::Create(func_sig, lt, fn->scp.sym.name, _mod);
        if (!func) return LLE_FATAL;
        storeAstElem(fn->id, func);
        llvm::BasicBlock* func_block =
            llvm::BasicBlock::Create(_context, "", func);
        if (!func_block) return LLE_FATAL;
        _builder.SetInsertPoint(func_block);
        return addAstBodyIR(&fn->scp.body);
    }
    else {
        llvm::Constant* func =
            _mod->getOrInsertFunction(fn->scp.sym.name, func_sig);
        assert(func);
        storeAstElem(fn->id, func);
        return LLE_OK;
    }
}

llvm_error LLVMBackend::emitModule(const std::string& obj_name)
{
    std::error_code EC;
    llvm::raw_fd_ostream file_stream{obj_name, EC, llvm::sys::fs::F_None};
    _mod->setDataLayout(_tm->createDataLayout());
    // Output the bitcode file to std out
    llvm::legacy::PassManager pass;
    auto file_type = llvm::TargetMachine::CGFT_ObjectFile;

    if (_tm->addPassesToEmitFile(pass, file_stream, nullptr, file_type)) {
        llvm::errs() << "TheTargetMachine can't emit a file of this type\n";
        return LLE_FATAL;
    }
    pass.run(*_mod);
    file_stream.flush();
    return LLE_OK;
}
llvm_error
linkLLVMModules(LLVMModule** start, LLVMModule** end, char* output_path)
{
    // ureg args_count = 10 + (end - start);
    std::vector<const char*> args;
    args.push_back(""); // argv[0] -> programm location, ignored
    args.push_back("--dynamic-linker");
    args.push_back("/lib64/ld-linux-x86-64.so.2");
    args.push_back("/usr/lib/x86_64-linux-gnu/crt1.o");
    args.push_back("/usr/lib/x86_64-linux-gnu/crti.o");
    args.push_back("/usr/lib/x86_64-linux-gnu/crtn.o");
    args.push_back("/lib/x86_64-linux-gnu/libc.so.6");
    args.push_back("/usr/lib/x86_64-linux-gnu/libc_nonshared.a");
    for (LLVMModule** i = start; i != end; i++) {
        args.push_back((**i).name.c_str());
    }
    args.push_back("-o");
    args.push_back("a.out");
    llvm::ArrayRef<const char*> arr_ref(&args[0], args.size());
    lld::elf::link(arr_ref, false);
    return LLE_OK;
}