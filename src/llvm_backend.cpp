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

llvm_backend_error llvm_backend_emit_module(
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
    llvm_backend_error be =
        linkLLVMModules((LLVMModule**)start, (LLVMModule**)end, output_path);
    if (be) return ERR;
    return OK;
}
}

// CPP
LLVMBackend::LLVMBackend(thread_context* tc)
    : _tc(tc), _context(), _builder(_context),
      _global_value_store(atomic_ureg_load(&TAUC.node_ids), NULL),
      _err(LLVMBE_OK)

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
llvm_backend_error LLVMBackend::setup()
{
    for (ureg i = 0; i < PRIMITIVE_COUNT; i++) {
        addPrimitive(PRIMITIVES[i].type_id, PRIMITIVES[i].sym.node.pt_kind);
        if (_err) return _err;
    }
    return LLVMBE_OK;
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
llvm_backend_error LLVMBackend::createLLVMModule(
    mdg_node** start, mdg_node** end, ureg startid, ureg endid,
    ureg private_sym_count, LLVMModule** module)
{
    _err = LLVMBE_OK;
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
    _private_sym_count = private_sym_count;
    if (_global_value_store.size() <= endid) {
        _global_value_store.resize(endid + 1, NULL);
    }
    // i really hope this doesn't realloc
    _local_value_store.assign(_private_sym_count, NULL);
    // create actual module
    _mod = new (std::nothrow) llvm::Module(m->name, _context);
    if (!_mod) return LLVMBE_FATAL;
    addModulesIR(start, end);
    if (_err) return _err;
    emitModule(m->name);
    return _err;
}

void LLVMBackend::addModulesIR(mdg_node** start, mdg_node** end)
{
    for (mdg_node** n = start; n != end; n++) {
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &(**n).open_scopes);
        for (open_scope* osc = (open_scope*)aseglist_iterator_next(&it); osc;
             osc = (open_scope*)aseglist_iterator_next(&it)) {
            genMdgNodeIR((ast_node*)osc);
            if (_err) return;
        }
    }
}
void LLVMBackend::addAstBodyIR(ast_body* b)
{
    for (ast_node** n = b->elements; *n; n++) {
        genMdgNodeIR(*n);
        if (_err) return;
    }
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
llvm::Value* LLVMBackend::lookupAstNodeIR(ureg id, ast_node* n)
{
    auto v = (llvm::Value**)lookupAstElem(id);
    if (*v) return *v;
    *v = genMdgNodeIR(n);
    assert(*v);
    return *v;
}
llvm::Type* LLVMBackend::lookupAstTypeIR(ureg id, ast_elem* e)
{
    auto t = (llvm::Type**)lookupAstElem(id);
    if (*t) return *t;
    assert(false); // TODO: getPointerTo(), yadda yadda
    return NULL;
}
llvm::Type* LLVMBackend::lookupCTypeIR(ast_elem* e)
{
    llvm::Type* t;
    switch (e->kind) {
        case PRIMITIVE:
            t = *(llvm::Type**)lookupAstElem(((primitive*)e)->type_id);
            break;
        default: {
            assert(false); // TODO
        }
    }
    return t;
}
llvm::Type* LLVMBackend::lookupPrimitive(primitive_kind pk)
{
    return (llvm::Type*)_global_value_store[PRIMITIVES[0].type_id + pk];
}
llvm::Value* LLVMBackend::genMdgNodeIR(ast_node* n)
{
    // TODO: proper error handling
    switch (n->kind) {
        case OSC_MODULE:
        case OSC_EXTEND: addAstBodyIR(&((open_scope*)n)->scp.body); return NULL;
        case SC_FUNC: return genFunctionIR((sc_func*)n);
        case EXPR_OP_BINARY: return genBinaryOpIR((expr_op_binary*)n);
        case EXPR_LITERAL: {
            expr_literal* l = (expr_literal*)n;
            switch (n->pt_kind) {
                case PT_INT:
                case PT_UINT:
                    return llvm::ConstantInt::get(
                        (llvm::IntegerType*)lookupPrimitive(n->pt_kind),
                        l->value.str, 10);
                case PT_STRING:
                    return _builder.CreateGlobalStringPtr(l->value.str);
                /* case PT_FLOAT:
                     return llvm::ConstantFP::get(
                         _context,
                         llvm::APFloat::convertFromString(
                             l->value.str,
                             llvm::APFloatBase::roundingMode::rmTowardZero));*/
                default: assert(false);
            }
        }
        case EXPR_RETURN: {
            return _builder.CreateRet(genMdgNodeIR(((expr_break*)n)->value));
        }
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            llvm::Value** args = (llvm::Value**)pool_alloc(
                &_tc->permmem, sizeof(llvm::Value*) * c->arg_count);
            if (!args) {
                _err = LLVMBE_FATAL;
                return NULL;
            }
            for (ureg i = 0; i < c->arg_count; i++) {
                args[i] = genMdgNodeIR(c->args[i]);
                if (_err) return NULL;
            }
            llvm::ArrayRef<llvm::Value*> args_arr_ref(
                args, args + c->arg_count);
            return _builder.CreateCall(
                lookupAstNodeIR(c->target->id, (ast_node*)c->target),
                args_arr_ref);
        }
        default: break;
    }
    return NULL;
}
llvm::Value* LLVMBackend::genBinaryOpIR(expr_op_binary* b)
{
    if (b->op->kind == SC_FUNC) {
        assert(false); // TODO
    }
    switch (b->node.op_kind) {
        case OP_ADD: {
            auto lhs = genMdgNodeIR(b->lhs);
            if (_err) return NULL;
            auto rhs = genMdgNodeIR(b->rhs);
            if (_err) return NULL;
            return _builder.CreateAdd(lhs, rhs);
        }
        default: assert(false);
    }
}
llvm::Value* LLVMBackend::genFunctionIR(sc_func* fn)
{
    llvm::FunctionType* func_sig;
    auto ret_type = lookupCTypeIR(fn->return_ctype);
    if (_err) return NULL;
    assert(ret_type);

    if (fn->param_count != 0) {
        llvm::Type** params = (llvm::Type**)pool_alloc(
            &_tc->permmem, sizeof(llvm::Type*) * fn->param_count);
        if (!params) {
            _err = LLVMBE_FATAL;
            return NULL;
        }
        for (ureg i = 0; i < fn->param_count; i++) {
            params[i] = lookupCTypeIR(fn->params[i].ctype);
            if (_err) return NULL;
        }
        llvm::ArrayRef<llvm::Type*> params_array_ref(
            params, params + fn->param_count);
        func_sig = llvm::FunctionType::get(ret_type, params_array_ref, false);
    }
    else {
        func_sig = llvm::FunctionType::get(ret_type, false);
    }

    assert(func_sig);
    storeAstElem(fn->signature_id, func_sig);

    if (*fn->scp.body.elements) {
        auto lt = llvm::Function::ExternalLinkage;
        llvm::Function* func =
            llvm::Function::Create(func_sig, lt, fn->scp.sym.name, _mod);
        assert(func);
        storeAstElem(fn->id, func);
        llvm::BasicBlock* func_block =
            llvm::BasicBlock::Create(_context, "", func);
        assert(func);
        _builder.SetInsertPoint(func_block);
        addAstBodyIR(&fn->scp.body);
        return func;
    }
    else {
        llvm::Constant* func =
            _mod->getOrInsertFunction(fn->scp.sym.name, func_sig);
        storeAstElem(fn->id, func);
        assert(func);
        return func;
    }
}

void LLVMBackend::emitModule(const std::string& obj_name)
{
    std::error_code EC;
    llvm::raw_fd_ostream file_stream{obj_name, EC, llvm::sys::fs::F_None};
    _mod->setDataLayout(_tm->createDataLayout());
    // Output the bitcode file to std out
    llvm::legacy::PassManager pass;
    auto file_type = llvm::TargetMachine::CGFT_ObjectFile;

    if (_tm->addPassesToEmitFile(pass, file_stream, nullptr, file_type)) {
        llvm::errs() << "TheTargetMachine can't emit a file of this type\n";
        _err = LLVMBE_FATAL;
        return;
    }
    pass.run(*_mod);
    file_stream.flush();
    return;
}
llvm_backend_error
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
    return LLVMBE_OK;
}