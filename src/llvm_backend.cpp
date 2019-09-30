#include "llvm_backend.hpp"
#include <memory>
extern "C" {
#include "thread_context.h"
#include "utils/pool.h"
#include "tauc.h"
#include <utils/debug_utils.h>
int llvm_backend_init_globals()
{
    // InitializeAllTargetInfos();
    LLVMInitializeNativeTarget();
    // InitializeAllTargetMCs();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();
    const char* args[] = {"tauc",
                          /*"-time-passes",*/
                          /*"--debug-pass=Structure"*/};

    llvm::cl::ParseCommandLineOptions(sizeof(args) / sizeof(char*), args);
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
    if (LLVMBackend::Initialize(b, tc)) return NULL;
    return b;
}

void llvm_backend_delete(llvm_backend* llvmb)
{
    LLVMBackend::Finalize((LLVMBackend*)llvmb);
}

llvm_error llvm_backend_emit_module(
    llvm_backend* llvmb, mdg_node** start, mdg_node** end, ureg startid,
    ureg endid, ureg private_sym_count, llvm_module** mod)
{
    return ((LLVMBackend*)llvmb)
        ->createLLVMModule(
            start, end, startid, endid, private_sym_count, (LLVMModule**)mod);
}

int llvm_delete_objs(llvm_module** start, llvm_module** end)
{
    llvm_error lle = removeObjs((LLVMModule**)start, (LLVMModule**)end);
    if (lle) return ERR;
    return OK;
}

void llvm_free_module(llvm_module* mod)
{
    delete (LLVMModule*)mod;
}
int llvm_link_modules(llvm_module** start, llvm_module** end, char* output_path)
{
    tprintf("linking {");
    for (LLVMModule** n = (LLVMModule**)start; !ptreq(n, end); n++) {
        tprintf(
            "%s%s", (**n).module_str.c_str(), ptreq(n + 1, end) ? "" : ", ");
    }
    tput("} ");
    llvm_error lle;
    TIME(lle = linkLLVMModules(
             (LLVMModule**)start, (LLVMModule**)end, output_path););
    tflush();
    if (lle) return ERR;
    return OK;
}
int llvm_initialize_primitive_information()
{
    // TODO: properly figure out the plattform information
    const ureg reg_size = 8;
    PRIMITIVES[PT_UINT].size = reg_size;
    PRIMITIVES[PT_INT].size = reg_size;
    PRIMITIVES[PT_FLOAT].size = reg_size;

    PRIMITIVES[PT_STRING].size = reg_size;
    PRIMITIVES[PT_BINARY_STRING].size = reg_size;
    PRIMITIVES[PT_VOID].size = 0;

    for (ureg i = 0; i < PRIMITIVE_COUNT; i++) {
        PRIMITIVES[i].alignment = PRIMITIVES[i].size;
    }
    PRIMITIVES[PT_VOID].alignment = 1;
    return OK;
}
}

// CPP

LLVMBackend::LLVMBackend(thread_context* tc)
    : _tc(tc), _context(), _builder(_context),
      _global_value_store(atomic_ureg_load(&tc->t->node_ids), NULL)
{
    std::string err;
    auto target_triple = "x86_64-pc-linux-gnu"; // LLVMGetDefaultTargetTriple();
    auto target = llvm::TargetRegistry::lookupTarget(target_triple, err);
    if (err.length() > 0) { // TODO: properly
        llvm::errs() << err << "\n";
    }
    llvm::TargetOptions opt;
    opt.ThreadModel = llvm::ThreadModel::POSIX;
    llvm::Optional<llvm::CodeModel::Model> CM = llvm::CodeModel::Small;
    llvm::CodeGenOpt::Level OptLevel = llvm::CodeGenOpt::None;

    _target_machine = target->createTargetMachine(
        target_triple, LLVMGetHostCPUName(), LLVMGetHostCPUFeatures(), opt,
        llvm::Optional<llvm::Reloc::Model>(), CM, OptLevel);
    _data_layout = new llvm::DataLayout(_target_machine->createDataLayout());
}

LLVMBackend::~LLVMBackend()
{
    ureg f = 0;
    std::sort(_globals_not_to_free.begin(), _globals_not_to_free.end());
    _globals_not_to_free.push_back(_global_value_store.size());
    for (ureg i : _globals_not_to_free) {
        while (f != i) {
            auto val = (llvm::Value*)_global_value_store[f];
            if (val) val->deleteValue();
            f++;
        }
        f++;
    }
    delete _data_layout;
    delete _target_machine;
}

int LLVMBackend::Initialize(LLVMBackend* llvmb, thread_context* tc)
{
    void* res = new (llvmb) LLVMBackend(tc); // placement new is noexcept
    if (!res) return ERR;
    if (llvmb->setup()) return ERR;
    return OK;
}
llvm_error LLVMBackend::setup()
{
    addPrimitives();
    return LLE_OK;
}
void LLVMBackend::Finalize(LLVMBackend* llvmb)
{
    llvmb->~LLVMBackend();
}

void LLVMBackend::addPrimitives()
{
    for (ureg i = 0; i < PRIMITIVE_COUNT; i++) {
        llvm::Type* t;
        switch (i) {
            case PT_INT:
            // llvm expects bits, we store bytes (for now)
            case PT_UINT: t = _builder.getIntNTy(PRIMITIVES[i].size * 8); break;
            case PT_BINARY_STRING:
            case PT_STRING: t = _builder.getInt8PtrTy(); break;
            case PT_FLOAT: {
                if (PRIMITIVES[i].size == 4) {
                    t = _builder.getFloatTy();
                }
                else if (PRIMITIVES[i].size == 8) {
                    t = _builder.getDoubleTy();
                }
                else {
                    // TODO: decide on supported architectures and handle this
                    // accordingly
                    assert(false);
                    return;
                }
            } break;
            case PT_VOID_PTR: t = _builder.getVoidTy()->getPointerTo(); break;
            case PT_VOID: t = _builder.getVoidTy(); break;
            case PT_TYPE:
            case PT_UNREACHABLE: t = NULL; break;
            default: assert(false); return;
        }
        _primitive_types[i] = t;
    }
}

llvm_error LLVMBackend::createLLVMModule(
    mdg_node** start, mdg_node** end, ureg startid, ureg endid,
    ureg private_sym_count, LLVMModule** module)
{
    // create our LLVMModule wrapper thingy for linking
    LLVMModule* m = new LLVMModule();
    if (!m) return LLE_FATAL;
    tprintf("generating {");
    for (mdg_node** n = start; n != end; n++) {
        m->module_str += (**n).name;
        tprintf("%s", (**n).name);
        if (n + 1 != end) {
            tprintf(", ");
            m->module_str += "&";
        }
    }
    m->module_obj = m->module_str + ".obj";
    tput("} ");

    *module = m;
    _mod_handle = m;
    // init id space
    _mod_startid = startid;
    _mod_endid = endid;
    if (_global_value_store.size() <= endid) {
        _global_value_store.resize(endid + 1, NULL);
        _global_value_init_flags.assign(endid + 1, false);
    }
    _private_sym_count = private_sym_count;
    // i really hope this doesn't realloc
    _local_value_store.assign(_private_sym_count, NULL);
    _reset_after_emit.clear();
    // create actual module
    _module = new (std::nothrow) llvm::Module(m->module_str, _context);
    if (!_module) return LLE_OK;
    _module->setTargetTriple(_target_machine->getTargetTriple().str());
    _module->setDataLayout(*_data_layout);
    llvm_error lle;
    TIME(lle = genModules(start, end););
    tflush();
    if (lle) return lle;
    TIME(lle = emitModule(););
    tflush();
    // PERF: instead of this last minute checking
    // just have different buffers for the different reset types
    for (ureg id : _reset_after_emit) {
        if (isGlobalIDInModule(id)) {
            // freeing the module will delete these
            _global_value_store[id] = NULL;
            continue;
        }
        auto val = (llvm::Value*)_global_value_store[id];
        if (llvm::isa<llvm::GlobalVariable>(*val)) {
            ((llvm::GlobalVariable*)val)->removeFromParent();
        }
        else {
            assert(llvm::isa<llvm::Function>(*val));
            ((llvm::Function*)val)->removeFromParent();
        }
    }
    delete _module;
    return lle;
}

llvm_error LLVMBackend::genModules(mdg_node** start, mdg_node** end)
{
    for (mdg_node** n = start; n != end; n++) {
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &(**n).open_scopes);
        for (open_scope* osc = (open_scope*)aseglist_iterator_next(&it); osc;
             osc = (open_scope*)aseglist_iterator_next(&it)) {
            for (ast_node** n = osc->sc.body.elements; *n; n++) {
                llvm_error lle = genAstNode(*n, NULL, NULL);
                if (lle) return lle;
            }
        }
    }
    return LLE_OK;
}
llvm_error LLVMBackend::genAstBody(ast_body* b, bool continues_afterwards)
{
    ControlFlowContext& ctx = _control_flow_ctx.back();
    ctx.continues_afterwards = true;
    for (ast_node** n = b->elements; *n; n++) {
        if (!*(n + 1)) ctx.continues_afterwards = continues_afterwards;
        llvm_error lle = genAstNode(*n, NULL, NULL);
        if (lle) return lle;
    }
    return LLE_OK;
}
void** LLVMBackend::lookupAstElem(ureg id)
{
    if (isLocalID(id)) {
        return &_local_value_store[id - UREGH_MAX];
    }
    return &_global_value_store[id];
}
llvm::Value** LLVMBackend::lookupVariableRaw(ureg id)
{
    auto gv = (llvm::GlobalValue**)lookupAstElem(id);
    if (isGlobalID(id) && *gv) {
        if (!_global_value_init_flags[id]) {
            _global_value_init_flags[id] = true;
            _module->getGlobalList().push_back((llvm::GlobalVariable*)*gv);
        }
    }
    return (llvm::Value**)gv;
}
llvm::Function** LLVMBackend::lookupFunctionRaw(ureg id)
{
    auto fn = (llvm::Function**)lookupAstElem(id);
    if (isGlobalID(id) && *fn) {
        if (!_global_value_init_flags[id]) {
            _global_value_init_flags[id] = true;
            _module->getFunctionList().push_back(*fn);
        }
    }
    return fn;
}
llvm::Type** LLVMBackend::lookupTypeRaw(ureg id)
{
    return (llvm::Type**)lookupAstElem(id);
}
llvm_error LLVMBackend::lookupCType(ast_elem* e, llvm::Type** t, ureg* align)
{
    switch (e->kind) {
        case PRIMITIVE: {
            primitive_kind kind = ((primitive*)e)->sym.node.pt_kind;
            if (t) *t = _primitive_types[kind];
            if (align) *align = PRIMITIVES[kind].alignment;
        } break;
        case SC_STRUCT: {
            sc_struct* st = (sc_struct*)e;
            llvm::Type** tp = lookupTypeRaw(st->id);
            if (*tp) {
                if (t) *t = *tp;
            }
            else {
                if (isGlobalID(st->id) && isIDInModule(st->id)) {
                    _globals_not_to_free.push_back(st->id);
                }
                // PERF: we could steal the array of elements for our types here
                // this might be too big because of nested structs etc. but it's
                // definitely big enough
                auto members = (llvm::Type**)pool_alloc(
                    &_tc->permmem,
                    sizeof(llvm::Type*) * st->sc.body.symtab->decl_count);
                ureg memcnt = 0;
                for (ast_node** i = st->sc.body.elements; *i; i++) {
                    // TODO: usings
                    if ((**i).kind == SYM_VAR ||
                        (**i).kind == SYM_VAR_INITIALIZED) {
                        lookupCType(
                            ((sym_var*)*i)->ctype, &members[memcnt], NULL);
                        memcnt++;
                    }
                }
                llvm::ArrayRef<llvm::Type*> member_types{members, memcnt};
                auto strct =
                    llvm::StructType::create(_context, member_types, "", false);
                if (!strct) return LLE_FATAL;
                *tp = strct;
                if (t) *t = strct;
            }
            if (align) {
                *align = _data_layout->getStructLayout((llvm::StructType*)*tp)
                             ->getAlignment();
            }
        } break;
        case TYPE_POINTER: {
            if (align) *align = PRIMITIVES[PT_VOID_PTR].alignment;
            if (!t) return LLE_OK;
            llvm_error lle = lookupCType(((type_pointer*)e)->base, t, NULL);
            if (lle) return lle;
            *t = (**t).getPointerTo();
        } break;
        default: {
            assert(false); // TODO
            if (t) *t = NULL; // to silcence -Wmaybe-uninitialized :(
        }
    }
    return LLE_OK;
}
ControlFlowContext* LLVMBackend::getTartetCFC(ast_node* target)
{
    switch (target->kind) {
        case EXPR_BLOCK: {
            return (ControlFlowContext*)((expr_block*)target)->control_flow_ctx;
        }
        case EXPR_LOOP: {
            return (ControlFlowContext*)((expr_loop*)target)->control_flow_ctx;
        }
        default: {
            assert(false);
            return (ControlFlowContext*)NULL;
        }
    }
}
bool LLVMBackend::isIDInModule(ureg id)
{
    return isLocalID(id) || isGlobalIDInModule(id);
}
bool LLVMBackend::isGlobalIDInModule(ureg id)
{
    return (_mod_startid <= id && id < _mod_endid);
}
bool LLVMBackend::isLocalID(ureg id)
{
    return id >= UREGH_MAX;
}
bool LLVMBackend::isGlobalID(ureg id)
{
    return !isLocalID(id);
}
llvm_error LLVMBackend::genIfBranch(ast_node* branch)
{
    ControlFlowContext& ctx = _control_flow_ctx.back();
    _builder.SetInsertPoint(ctx.first_block);
    llvm_error lle;
    bool ret;
    if (branch->kind == EXPR_BLOCK) {
        // TODO: we might need to cast the expr block return type?
        auto eb = (expr_block*)branch;
        eb->control_flow_ctx = &ctx;
        ret = (eb->ctype != UNREACHABLE_ELEM);
        lle = genAstBody(&eb->body, ret);
        if (lle) return lle;
    }
    else if (ctx.value) {
        llvm::Value* v;
        ret = true;
        ctx.continues_afterwards = true;
        lle = genAstNode(branch, NULL, &v);
        if (lle) return lle;
        if (!_builder.CreateAlignedStore(v, ctx.value, ctx.value_align))
            return LLE_FATAL;
    }
    else {
        // TODO: proper handling for unreachable
        ret = (get_resolved_ast_node_ctype(branch) != UNREACHABLE_ELEM);
        ctx.continues_afterwards = ret;
        lle = genAstNode(branch, NULL, NULL);
        if (lle) return lle;
    }
    if (ret) {
        _builder.CreateBr(ctx.following_block);
    }
    return LLE_OK;
}
llvm_error LLVMBackend::genScopeValue(ast_elem* ctype, ControlFlowContext& ctx)
{
    if (ctype != VOID_ELEM && ctype != UNREACHABLE_ELEM) {
        llvm::Type* t;
        llvm_error lle = lookupCType(ctype, &t, &ctx.value_align);
        if (lle) return lle;
        auto all = new llvm::AllocaInst(t, 0, nullptr, ctx.value_align, "");
        if (!all) return LLE_FATAL;
        ctx.value = all;
        _builder.Insert(all);
    }
    else {
        ctx.value = NULL;
        ctx.value_align = 0;
    }
    return LLE_OK;
}
llvm_error
LLVMBackend::genAstNode(ast_node* n, llvm::Value** vl, llvm::Value** vl_loaded)
{
    // TODO: proper error handling
    llvm_error lle;
    switch (n->kind) {
        case OSC_MODULE:
        case OSC_EXTEND:
            assert(!vl);
            return LLE_OK; // these are handled by the osc iterator
        case SC_STRUCT: return lookupCType((ast_elem*)n, NULL, NULL);
        case SC_FUNC: return genFunction((sc_func*)n, (llvm::Function**)vl);
        case EXPR_OP_BINARY:
            return genBinaryOp((expr_op_binary*)n, vl, vl_loaded);
        case EXPR_OP_UNARY: return genUnaryOp((expr_op_unary*)n, vl, vl_loaded);
        case EXPR_PARENTHESES:
            return genAstNode(((expr_parentheses*)n)->child, vl, vl_loaded);
        case EXPR_LITERAL: {
            expr_literal* l = (expr_literal*)n;
            switch (n->pt_kind) {
                case PT_INT:
                case PT_UINT: {
                    auto t = (llvm::IntegerType*)_primitive_types[n->pt_kind];
                    auto c = llvm::ConstantInt::get(t, l->value.str, 10);
                    if (!c) return LLE_FATAL;
                    if (vl) *vl = c;
                    if (vl_loaded) *vl_loaded = c;
                    return LLE_OK;
                }
                case PT_STRING: {
                    auto c = _builder.CreateGlobalStringPtr(l->value.str);
                    if (!c) return LLE_FATAL;
                    if (vl) *vl = c;
                    if (vl_loaded) *vl_loaded = c;
                    return LLE_OK;
                }
                case PT_FLOAT: {
                    auto c = llvm::ConstantFP::get(
                        _context,
                        *new llvm::APFloat(
                            llvm::APFloatBase::IEEEsingle(), l->value.str));
                    if (vl) *vl = c;
                    if (vl_loaded) *vl_loaded = c;
                    return LLE_OK;
                }
                default: assert(false);
            }
        }
        case SYM_PARAM: {
            sym_param* p = (sym_param*)n;
            sc_func* f = (sc_func*)p->sym.declaring_st->owning_node;
            ureg param_nr = p - f->params;
            llvm::Function* fn;
            lle = genFunction(f, &fn);
            if (lle) return lle;
            llvm::Argument* a = fn->arg_begin() + param_nr;
            if (!a) return LLE_FATAL;
            assert(!vl && vl_loaded);
            *vl_loaded = a;
            return LLE_OK;
        }
        case EXPR_IDENTIFIER: {
            return genAstNode(
                (ast_node*)((expr_identifier*)n)->value.sym, vl, vl_loaded);
        }
        case EXPR_BREAK:
        case EXPR_RETURN: {
            auto eb = (expr_break*)n;
            ControlFlowContext* tgt_ctx;
            bool continues = _control_flow_ctx.back().continues_afterwards;
            auto ast_val = eb->value;
            llvm::Value* v;
            if (n->kind == EXPR_BREAK) {
                tgt_ctx = getTartetCFC(eb->target);
            }
            else {
                tgt_ctx = &_control_flow_ctx.front();
                if (tgt_ctx->following_block == NULL) {
                    if (!continues && tgt_ctx == &_control_flow_ctx.back()) {
                        if (ast_val) {
                            _control_flow_ctx.back().continues_afterwards =
                                true;
                            lle = genAstNode(ast_val, NULL, &v);
                            _control_flow_ctx.back().continues_afterwards =
                                false;
                            if (lle) return lle;
                            _builder.CreateRet(v);
                            return LLE_OK;
                        }
                        else {
                            _builder.CreateRetVoid();
                            return LLE_OK;
                        }
                    }
                    else {
                        auto curr_ib = _builder.GetInsertBlock();
                        auto curr_ip = _builder.GetInsertPoint();
                        _builder.SetInsertPoint(
                            tgt_ctx->first_block,
                            tgt_ctx->first_block->getInstList().begin());
                        genScopeValue(
                            _curr_fn_ast_node->return_ctype, *tgt_ctx);
                        tgt_ctx->following_block =
                            llvm::BasicBlock::Create(_context, "", _curr_fn);
                        _builder.SetInsertPoint(curr_ib, curr_ip);
                        for (auto& c : _control_flow_ctx) {
                            if (c.following_block == NULL)
                                c.following_block = tgt_ctx->following_block;
                        }
                    }
                }
            }
            if (ast_val) {
                _control_flow_ctx.back().continues_afterwards = true;
                // todo: where do we cast?
                lle = genAstNode(ast_val, NULL, &v);
                _control_flow_ctx.back().continues_afterwards = false;
                if (lle) return lle;
                if (!_builder.CreateAlignedStore(
                        v, tgt_ctx->value, tgt_ctx->value_align))
                    return LLE_FATAL;
            }
            _control_flow_ctx.back().continues_afterwards = continues;
            assert(!vl && !vl_loaded);
            if (!continues) {
                _builder.CreateBr(tgt_ctx->following_block);
                _builder.SetInsertPoint(tgt_ctx->following_block);
            }
            return LLE_OK;
        }
        case EXPR_LOOP: {
            expr_loop* l = (expr_loop*)n;
            ControlFlowContext* ctx = &_control_flow_ctx.back();
            llvm::BasicBlock* following_block;
            if (ctx->continues_afterwards) {
                following_block = llvm::BasicBlock::Create(
                    _context, "", _curr_fn, ctx->following_block);
            }
            else {
                following_block = ctx->following_block;
            }
            _control_flow_ctx.emplace_back();
            ctx = &_control_flow_ctx.back();
            l->control_flow_ctx = ctx;
            ctx->following_block = following_block;
            lle = genScopeValue(l->ctype, *ctx);
            if (lle) return lle;
            ctx->first_block = llvm::BasicBlock::Create(
                _context, "", _curr_fn, following_block);
            if (!ctx->first_block) return LLE_FATAL;
            if (!_builder.CreateBr(ctx->first_block)) return LLE_FATAL;
            _builder.SetInsertPoint(ctx->first_block);
            lle = genAstBody(&l->body, true);
            if (lle) return lle;
            if (!_builder.CreateBr(ctx->first_block)) return LLE_FATAL;
            _builder.SetInsertPoint(following_block);
            if (vl) *vl = ctx->value;
            if (vl_loaded) {
                *vl_loaded =
                    _builder.CreateAlignedLoad(ctx->value, ctx->value_align);
                if (*vl_loaded) return LLE_FATAL;
            }
            _control_flow_ctx.pop_back();
            return LLE_OK;
        }
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            assert(c->target->sym.node.kind == SC_FUNC); // prevent macros
            llvm::Value** args = (llvm::Value**)pool_alloc(
                &_tc->permmem, sizeof(llvm::Value*) * c->arg_count);
            if (!args) return LLE_FATAL;
            for (ureg i = 0; i < c->arg_count; i++) {
                lle = genAstNode(c->args[i], NULL, &args[i]);
                if (lle) return lle;
            }
            llvm::ArrayRef<llvm::Value*> args_arr_ref(
                args, args + c->arg_count);
            llvm::Function* callee;
            lle = genFunction((sc_func*)c->target, &callee);
            if (lle) return lle;
            auto call = _builder.CreateCall(callee, args_arr_ref);
            if (!call) return LLE_FATAL;
            if (vl) *vl = call;
            if (vl_loaded) *vl_loaded = call;
            return LLE_OK;
        }
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: {
            if (ast_flags_get_const(n->flags)) {
                // TODO (ctfe)
                assert(false);
            }
            sym_var* var = (sym_var*)n;
            llvm::Type* t;
            ureg align;
            lle = lookupCType(var->ctype, &t, &align);
            if (lle) return lle;
            llvm::Value** llvar = lookupVariableRaw(var->var_id);
            if (!*llvar) {
                ast_node_kind k = var->sym.declaring_st->owning_node->kind;
                llvm::Value* var_val;
                if (k == ELEM_MDG_NODE || k == OSC_MODULE || k == OSC_EXTEND) {
                    // global var
                    llvm::GlobalVariable::LinkageTypes lt;
                    if (isLocalID(var->var_id)) {
                        lt = llvm::GlobalVariable::InternalLinkage;
                    }
                    else {
                        lt = llvm::GlobalVariable::ExternalLinkage;
                        _reset_after_emit.push_back(var->var_id);
                    }
                    llvm::Constant* init = NULL;
                    if (n->kind == SYM_VAR_INITIALIZED &&
                        isIDInModule(var->var_id)) {
                        llvm::Value* v;
                        lle = genAstNode(
                            ((sym_var_initialized*)n)->initial_value, NULL, &v);
                        if (lle) return lle;
                        if (!(init = llvm::dyn_cast<llvm::Constant>(v))) {
                            assert(false); // must be constant, TODO: error
                            return LLE_FATAL;
                        }
                    }
                    var_val = (llvm::Value*)new llvm::GlobalVariable(
                        *_module, t, false, lt, init, var->sym.name);
                    if (!var_val) return LLE_FATAL;
                }
                else {
                    // These should be handled by member access
                    assert(k != SC_STRUCT);
                    // local var
                    auto all = new llvm::AllocaInst(
                        t, 0, nullptr, align, "" /* var->sym.name*/);
                    if (!all) return LLE_FATAL;
                    var_val = all;
                    _builder.Insert(all);
                    if (n->kind == SYM_VAR_INITIALIZED) {
                        llvm::Value* v;
                        lle = genAstNode(
                            ((sym_var_initialized*)n)->initial_value, NULL, &v);
                        if (lle) return lle;
                        if (!_builder.CreateAlignedStore(
                                v, var_val, align, false))
                            return LLE_FATAL;
                    }
                }
                *llvar = var_val;
                if (isGlobalID(var->var_id)) {
                    _global_value_init_flags[var->var_id] = true;
                }
            }
            if (vl_loaded) {
                *vl_loaded = _builder.CreateAlignedLoad(*llvar, align);
                if (!*vl_loaded) return LLE_FATAL;
            }
            if (vl) *vl = *llvar;
            return LLE_OK;
        }
        case EXPR_SCOPE_ACCESS: {
            expr_scope_access* esa = (expr_scope_access*)n;
            return genAstNode((ast_node*)esa->target.sym, vl, vl_loaded);
        }
        case EXPR_MEMBER_ACCESS: {
            auto ema = (expr_member_access*)n;
            llvm::Value* v;
            lle = genAstNode(ema->lhs, &v, NULL);
            if (lle) return lle;
            auto st = (sc_struct*)ema->target.sym->declaring_st->owning_node;
            assert(((ast_node*)st)->kind == SC_STRUCT);
            ureg align;
            lle = lookupCType((ast_elem*)st, NULL, &align);
            if (lle) return lle;
            ureg idx = 0;
            for (ast_node** i = st->sc.body.elements;; i++) {
                assert(*i);
                if (*i == (ast_node*)ema->target.sym) break;
                if ((**i).kind == SYM_VAR || (**i).kind == SYM_VAR_INITIALIZED)
                    idx++;
            }
            assert(vl);
            auto gep = _builder.CreateStructGEP(v, idx);
            if (vl_loaded) {
                *vl_loaded = _builder.CreateAlignedLoad(gep, align);
                if (!*vl_loaded) return LLE_FATAL;
            }
            if (vl) *vl = gep;
            return LLE_OK;
        }
        case SYM_IMPORT_GROUP:
        case SYM_IMPORT_MODULE: return LLE_OK;
        case EXPR_IF: {
            expr_if* i = (expr_if*)n;
            llvm::Value* cond;
            lle = genAstNode(i->condition, NULL, &cond);
            if (lle) return lle;

            ControlFlowContext* ctx = &_control_flow_ctx.back();
            llvm::BasicBlock* following_block;
            if (ctx->continues_afterwards) {
                following_block = llvm::BasicBlock::Create(
                    _context, "", _curr_fn, ctx->following_block);
            }
            else {
                following_block = ctx->following_block;
            }
            _control_flow_ctx.emplace_back();
            ctx = &_control_flow_ctx.back();
            lle = genScopeValue(i->ctype, *ctx);
            if (lle) return lle;
            ctx->following_block = following_block;
            if (i->else_body) {
                auto if_block = llvm::BasicBlock::Create(
                    _context, "", _curr_fn, following_block);
                if (!if_block) return LLE_FATAL;
                auto else_block = llvm::BasicBlock::Create(
                    _context, "", _curr_fn, following_block);
                if (!else_block) return LLE_FATAL;
                _builder.CreateCondBr(cond, if_block, else_block);
                ctx->first_block = if_block;
                lle = genIfBranch(i->if_body);
                if (lle) return lle;
                ctx->first_block = else_block;
                lle = genIfBranch(i->else_body);
                if (lle) return lle;
            }
            else {
                auto if_block = llvm::BasicBlock::Create(
                    _context, "", _curr_fn, following_block);
                if (!if_block) return LLE_FATAL;
                if (!_builder.CreateCondBr(cond, if_block, following_block))
                    return LLE_FATAL;
                ctx->first_block = if_block;
                lle = genIfBranch(i->if_body);
                if (lle) return lle;
            }
            auto val = ctx->value;
            ureg align = ctx->value_align;
            _control_flow_ctx.pop_back();
            _builder.SetInsertPoint(following_block);
            if (!vl && !vl_loaded) return LLE_OK;
            assert(val);
            if (vl) *vl = val;
            if (vl_loaded) {
                *vl_loaded = _builder.CreateAlignedLoad(val, align);
                if (!*vl_loaded) return LLE_FATAL;
            }
            return LLE_OK;
        }
        default: assert(false);
    }
    assert(false);
    return LLE_FATAL;
}
llvm_error LLVMBackend::genUnaryOp(
    expr_op_unary* u, llvm::Value** vl, llvm::Value** vl_loaded)
{
    if (u->op->kind == SC_FUNC) {
        assert(false); // TODO
    }
    llvm_error lle;
    llvm::Value* child;
    switch (u->node.op_kind) {
        case OP_POST_INCREMENT: {
            llvm::Value* child_loaded;
            lle = genAstNode(u->child, &child, &child_loaded);
            if (lle) return lle;
            if (vl) *vl = child;
            if (vl_loaded) *vl_loaded = child_loaded;
            llvm::Value* add = _builder.CreateNSWAdd(
                child_loaded,
                llvm::ConstantInt::get(_primitive_types[PT_INT], 1));
            if (!add) return LLE_FATAL;
            if (!_builder.CreateAlignedStore(
                    add, child, child->getPointerAlignment(*_data_layout)))
                return LLE_FATAL;
            return LLE_OK;
        }
        case OP_ADDRESS_OF: {
            lle = genAstNode(u->child, &child, NULL);
            if (vl) *vl = child;
            if (vl_loaded) *vl_loaded = child;
            return LLE_OK;
        }
        case OP_DEREF: {
            llvm_error lle = genAstNode(u->child, NULL, &child);
            if (lle) return lle;
            auto load = _builder.CreateAlignedLoad(
                child, child->getPointerAlignment(*_data_layout));
            if (!load) return LLE_FATAL;
            if (vl) *vl = child;
            if (vl_loaded) *vl_loaded = load;
            return LLE_OK;
        }
        default: assert(false); return LLE_FATAL;
    }
    assert(!vl);
    return LLE_FATAL;
}
llvm_error LLVMBackend::genBinaryOp(
    expr_op_binary* b, llvm::Value** vl, llvm::Value** vl_loaded)
{
    if (b->op->kind == SC_FUNC) {
        assert(false); // TODO
    }
    llvm::Value *lhs, *rhs;
    llvm_error lle;
    if (b->node.op_kind == OP_ASSIGN) {
        lle = genAstNode(b->lhs, &lhs, NULL);
    }
    else {
        lle = genAstNode(b->lhs, NULL, &lhs);
    }
    lle = genAstNode(b->rhs, NULL, &rhs);
    if (lle) return lle;

    if (lle) return lle;
    llvm::Value* v;
    switch (b->node.op_kind) {
        case OP_ADD: v = _builder.CreateNSWAdd(lhs, rhs); break;
        case OP_SUB: v = _builder.CreateSub(lhs, rhs); break;
        case OP_MUL: v = _builder.CreateMul(lhs, rhs); break;
        case OP_DIV: v = _builder.CreateSDiv(lhs, rhs); break;
        case OP_MOD: v = _builder.CreateSRem(lhs, rhs); break;
        case OP_GREATER_THAN: v = _builder.CreateICmpSGT(lhs, rhs); break;
        case OP_LESS_THAN: v = _builder.CreateICmpSLT(lhs, rhs); break;
        case OP_EQUAL: v = _builder.CreateICmpEQ(lhs, rhs); break;
        case OP_ASSIGN: {
            // TODO: get align from ctype
            ureg align = lhs->getPointerAlignment(*_data_layout);
            // TODO: resolver has to make sure lhs is an lvalue
            _builder.CreateAlignedStore(rhs, lhs, align);
            if (vl_loaded) v = _builder.CreateAlignedLoad(lhs, align);
        } break;
        default: assert(false); return LLE_FATAL;
    }
    if (vl_loaded) *vl_loaded = v;
    assert(!vl);
    return LLE_OK;
}
char* name_mangle(sc_func* fn)
{
    symbol_table* st = fn->sc.body.symtab;
    while (st->owning_node->kind != ELEM_MDG_NODE) st = st->parent;
    mdg_node* n = (mdg_node*)st->owning_node;
    if (n->parent == NULL) return fn->sc.sym.name;
    ureg mnl = strlen(n->name);
    ureg fnl = strlen(fn->sc.sym.name);
    char* res = (char*)malloc(mnl + fnl + 3); // TODO: manage mem properly
    memcpy(res, n->name, mnl);
    memcpy(res + mnl, "::", 2);
    memcpy(res + mnl + 2, fn->sc.sym.name, fnl);
    *(res + mnl + fnl + 2) = '\0';
    return res;
}
llvm_error LLVMBackend::genFunction(sc_func* fn, llvm::Function** llfn)
{
    llvm::Function** res = lookupFunctionRaw(fn->id);
    if (*res) {
        if (llfn) *llfn = *res;
        return LLE_OK;
    }
    llvm::FunctionType* func_sig;
    llvm::Type* ret_type;
    llvm_error lle = lookupCType(fn->return_ctype, &ret_type, NULL);
    if (lle) return lle;

    if (fn->param_count != 0) {
        llvm::Type** params = (llvm::Type**)pool_alloc(
            &_tc->permmem, sizeof(llvm::Type*) * fn->param_count);
        if (!params) return LLE_FATAL;
        for (ureg i = 0; i < fn->param_count; i++) {
            lle = lookupCType(fn->params[i].ctype, &params[i], NULL);
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
    auto func_name_mangled = name_mangle(fn);
    // TODO: ugly hack,  use a proper extern function ast node
    bool extern_func = (fn->sc.body.srange == SRC_RANGE_INVALID);
    if (extern_func || !isIDInModule(fn->id)) {
        auto func = (llvm::Function*)llvm::Function::Create(
            func_sig, llvm::GlobalVariable::ExternalLinkage,
            _data_layout->getProgramAddressSpace(), func_name_mangled);
        _module->getFunctionList().push_back(func);
        if (isGlobalID(fn->id)) {
            _global_value_init_flags[fn->id] = true;
            _reset_after_emit.push_back(fn->id);
        }
        if (!func) return LLE_FATAL;
        *res = func;
        if (llfn) *llfn = func;
        return LLE_OK;
    }
    auto lt = llvm::Function::ExternalLinkage;
    if (isLocalID(fn->id)) {
        lt = llvm::Function::InternalLinkage;
    }
    else {
        _global_value_init_flags[fn->id] = true;
        _reset_after_emit.push_back(fn->id);
    }

    llvm::Function* func =
        llvm::Function::Create(func_sig, lt, func_name_mangled, _module);
    if (!func) return LLE_FATAL;
    *res = func;
    if (llfn) *llfn = func;
    llvm::BasicBlock* func_block = llvm::BasicBlock::Create(_context, "", func);
    if (!func_block) return LLE_FATAL;
    assert(_control_flow_ctx.size() == 0);
    _control_flow_ctx.emplace_back();
    ControlFlowContext& ctx = _control_flow_ctx.back();
    _builder.SetInsertPoint(func_block);
    // lle = genScopeValue(fn->return_ctype, ctx);
    // if (lle) return lle;
    ctx.first_block = func_block;
    ctx.following_block = NULL;

    _curr_fn = func;
    _curr_fn_ast_node = fn;
    _builder.SetInsertPoint(func_block);
    lle = genAstBody(&fn->sc.body, false);
    if (ctx.following_block) {
        if (_builder.GetInsertBlock() != ctx.following_block) {
            // _builder.CreateBr(ctx.following_block);
            _builder.SetInsertPoint(ctx.following_block);
        }
        if (!ctx.value) {
            _builder.CreateRetVoid();
        }
        else {
            auto load = _builder.CreateAlignedLoad(ctx.value, ctx.value_align);
            if (!load) return LLE_FATAL;
            _builder.CreateRet(load);
        }
    }
    _control_flow_ctx.pop_back();
    return lle;
}
llvm_error LLVMBackend::emitModuleIR()
{
    std::error_code EC;
    llvm::raw_fd_ostream ir_stream{_mod_handle->module_str + ".ll", EC,
                                   llvm::sys::fs::F_None};
    _module->print(ir_stream, nullptr, true, true);
    ir_stream.flush();
    return LLE_OK;
}
llvm_error
LLVMBackend::emitModuleToFile(llvm::TargetLibraryInfoImpl* tlii, bool emit_asm)
{
    std::error_code ec;
    auto file_type = emit_asm ? llvm::TargetMachine::CGFT_AssemblyFile
                              : llvm::TargetMachine::CGFT_ObjectFile;
    llvm::raw_fd_ostream file_stream{emit_asm ? _mod_handle->module_str + ".asm"
                                              : _mod_handle->module_obj,
                                     ec, llvm::sys::fs::F_None};
    llvm::legacy::PassManager CodeGenPasses;
    CodeGenPasses.add(llvm::createTargetTransformInfoWrapperPass(
        _target_machine->getTargetIRAnalysis()));

    CodeGenPasses.add(new llvm::TargetLibraryInfoWrapperPass(*tlii));

    if (_target_machine->addPassesToEmitFile(
            CodeGenPasses, file_stream, nullptr, file_type)) {
        llvm::errs() << "TheTargetMachine can't emit a file of this type\n";
        return LLE_FATAL;
    }
    CodeGenPasses.run(*_module);
    file_stream.flush();
    return LLE_OK;
}
llvm_error LLVMBackend::emitModule()
{
    tprintf("emmitting %s ", _mod_handle->module_str.c_str());

    llvm::Triple TargetTriple(_module->getTargetTriple());
    std::unique_ptr<llvm::TargetLibraryInfoImpl> TLII(
        new llvm::TargetLibraryInfoImpl(TargetTriple));
    llvm::legacy::PassManager PerModulePasses;
    PerModulePasses.add(llvm::createTargetTransformInfoWrapperPass(
        _target_machine->getTargetIRAnalysis()));

    llvm::legacy::FunctionPassManager PerFunctionPasses(_module);
    PerFunctionPasses.add(llvm::createTargetTransformInfoWrapperPass(
        _target_machine->getTargetIRAnalysis()));

    // CreatePasses(PerModulePasses, PerFunctionPasses);
    llvm::PassManagerBuilder pmb{};
    pmb.OptLevel = 0;
    // pmb.VerifyInput = true;
    // pmb.VerifyOutput = true;
    pmb.DisableTailCalls = true;
    pmb.DisableUnitAtATime = true;
    pmb.DisableUnrollLoops = true;
    pmb.SLPVectorize = false;
    pmb.LoopVectorize = false;
    pmb.RerollLoops = false;
    pmb.DisableGVNLoadPRE = true;
    pmb.VerifyInput = true;
    pmb.MergeFunctions = false;
    pmb.PrepareForLTO = false;
    pmb.PrepareForThinLTO = false;
    pmb.PerformThinLTO = false;

    pmb.Inliner = llvm::createAlwaysInlinerLegacyPass(false);

    PerModulePasses.add(new llvm::TargetLibraryInfoWrapperPass(*TLII));

    _target_machine->adjustPassManager(pmb);

    PerFunctionPasses.add(new llvm::TargetLibraryInfoWrapperPass(*TLII));

    pmb.populateFunctionPassManager(PerFunctionPasses);
    pmb.populateModulePassManager(PerModulePasses);

    PerFunctionPasses.doInitialization();
    for (llvm::Function& F : *_module)
        if (!F.isDeclaration()) PerFunctionPasses.run(F);
    PerFunctionPasses.doFinalization();

    PerModulePasses.run(*_module);
    llvm_error lle = LLE_OK;
    if (_tc->t->emit_asm) lle = emitModuleToFile(TLII.get(), true);
    if (!lle && _tc->t->emit_exe) emitModuleToFile(TLII.get(), false);
    if (!lle && _tc->t->emit_ll) emitModuleIR();
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
        args.push_back((**i).module_obj.c_str());
    }
    args.push_back("-o");
    args.push_back(output_path);
    llvm::ArrayRef<const char*> arr_ref(&args[0], args.size());
    lld::elf::link(arr_ref, false);
    return LLE_OK;
}

llvm_error removeObjs(LLVMModule** start, LLVMModule** end)
{
    for (LLVMModule** i = start; i != end; i++) {
        unlink((**i).module_obj.c_str());
    }
    return LLE_OK;
}
