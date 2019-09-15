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
      _global_value_store(atomic_ureg_load(&TAUC.node_ids), NULL)
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
    for (ureg i = 0; i != _global_value_store.size(); i++) {
        auto val = (llvm::Value*)_global_value_store[i];
        if (val) val->deleteValue();
    }
    delete _data_layout;
    delete _target_machine;
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
    addPrimitives();
    return LLE_OK;
}
void LLVMBackend::FinLLVMBackend(LLVMBackend* llvmb)
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
            case PT_VOID: t = _builder.getVoidTy(); break;
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
    m->name = m->module_str + ".obj";
    tput("} ");

    *module = m;
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
    _module = new (std::nothrow) llvm::Module(m->name, _context);
    if (!_module) return LLE_OK;
    _module->setTargetTriple(_target_machine->getTargetTriple().str());
    _module->setDataLayout(*_data_layout);
    llvm_error lle;
    TIME(lle = addModulesIR(start, end););
    tflush();
    if (lle) return lle;
    TIME(lle = emitModule(m->name););
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
            ((llvm::Function*)val)->removeFromParent();
        }
    }
    delete _module;
    return lle;
}

llvm_error LLVMBackend::addModulesIR(mdg_node** start, mdg_node** end)
{
    for (mdg_node** n = start; n != end; n++) {
        aseglist_iterator it;
        aseglist_iterator_begin(&it, &(**n).open_scopes);
        for (open_scope* osc = (open_scope*)aseglist_iterator_next(&it); osc;
             osc = (open_scope*)aseglist_iterator_next(&it)) {
            for (ast_node** n = osc->scp.body.elements; *n; n++) {
                llvm_error lle = getAstNodeIR(*n, false, NULL);
                if (lle) return lle;
            }
        }
    }
    return LLE_OK;
}
llvm_error LLVMBackend::addAstBodyIR(ast_body* b, bool continues_afterwards)
{
    ControlFlowContext& ctx = _control_flow_ctx.back();
    ctx.continues_afterwards = true;
    for (ast_node** n = b->elements; *n; n++) {
        if (!*(n + 1)) ctx.continues_afterwards = false;
        llvm_error lle = getAstNodeIR(*n, false, NULL);
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
            llvm::Type** tp = lookupTypeRaw(((sc_struct*)e)->id);
            if (*tp) {
                if (t) *t = *tp;
            }
            else {
                sc_struct* st = (sc_struct*)e;
                // PERF: we could steal the array of elements for our types here
                // this might be too big because of nested structs etc. but it's
                // definitely big enough
                auto members = (llvm::Type**)pool_alloc(
                    &_tc->permmem,
                    sizeof(llvm::Type*) * st->scp.body.symtab->decl_count);
                ureg memcnt = 0;
                for (ast_node** i = st->scp.body.elements; *i; i++) {
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
            break;
        }
        default: {
            assert(false); // TODO
            if (t) *t = NULL; // to silcence -Wmaybe-uninitialized :(
        }
    }
    return LLE_OK;
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
ControlFlowContext& getTartetCFC(ast_node* target)
{
    switch (target->kind) {
        case EXPR_BLOCK: {
            return *(ControlFlowContext*)((expr_block*)target)
                        ->control_flow_ctx;
        }
        case EXPR_LOOP: {
            return *(ControlFlowContext*)((expr_loop*)target)->control_flow_ctx;
        }
        default: {
            assert(false);
            return *(ControlFlowContext*)NULL;
        }
    }
}
llvm_error LLVMBackend::addIfBranch(ast_node* branch)
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
        lle = addAstBodyIR(&eb->body, ret);
        if (lle) return lle;
    }
    else if (ctx.value) {
        llvm::Value* v;
        ret = true;
        ctx.continues_afterwards = true;
        lle = getAstNodeIR(branch, true, &v);
        if (lle) return lle;
        if (!_builder.CreateAlignedStore(v, ctx.value, ctx.value_align))
            return LLE_FATAL;
    }
    else {
        // TODO: proper handling for unreachable
        ret = (branch->kind != EXPR_RETURN);
        ctx.continues_afterwards = ret;
        lle = getAstNodeIR(branch, false, NULL);
        if (lle) return lle;
    }
    if (ret) {
        _builder.CreateBr(ctx.following_block);
    }
    return LLE_OK;
}
llvm_error
LLVMBackend::createScopeValue(ast_elem* ctype, ControlFlowContext& ctx)
{
    if (ctype != VOID_ELEM && ctype != UNREACHABLE_ELEM) {
        llvm::Type* t;
        llvm_error lle = lookupCType(ctype, &t, &ctx.value_align);
        if (lle) return lle;
        ctx.value = new llvm::AllocaInst(
            t, 0, nullptr, ctx.value_align, "", _builder.GetInsertBlock());
        if (!ctx.value) return LLE_FATAL;
    }
    else {
        ctx.value = NULL;
        ctx.value_align = 0;
    }
    return LLE_OK;
}
llvm_error LLVMBackend::getAstNodeIR(ast_node* n, bool load, llvm::Value** vl)
{
    // TODO: proper error handling
    llvm_error lle;
    switch (n->kind) {
        case OSC_MODULE:
        case OSC_EXTEND:
            assert(!vl);
            return LLE_OK; // these are handled by the osc iterator
        case SC_STRUCT: return lookupCType((ast_elem*)n, NULL, NULL);
        case SC_FUNC: return genFunctionIR((sc_func*)n, (llvm::Function**)vl);
        case EXPR_OP_BINARY: return genBinaryOpIR((expr_op_binary*)n, vl);
        case EXPR_LITERAL: {
            expr_literal* l = (expr_literal*)n;
            switch (n->pt_kind) {
                case PT_INT:
                case PT_UINT: {
                    auto t = (llvm::IntegerType*)_primitive_types[n->pt_kind];
                    auto c = llvm::ConstantInt::get(t, l->value.str, 10);
                    if (!c) return LLE_FATAL;
                    if (vl) *vl = c;
                    return LLE_OK;
                }
                case PT_STRING: {
                    auto c = _builder.CreateGlobalStringPtr(l->value.str);
                    if (!c) return LLE_FATAL;
                    if (vl) *vl = c;
                    return LLE_OK;
                }
                case PT_FLOAT: {
                    auto c = llvm::ConstantFP::get(
                        _context,
                        *new llvm::APFloat(
                            llvm::APFloatBase::IEEEsingle(), l->value.str));
                    if (vl) *vl = c;
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
            lle = genFunctionIR(f, &fn);
            if (lle) return lle;
            llvm::Argument* a = fn->arg_begin() + param_nr;
            if (!a) return LLE_FATAL;
            assert(vl);
            *vl = a;
            return LLE_OK;
        }
        case EXPR_IDENTIFIER: {
            return getAstNodeIR(
                (ast_node*)((expr_identifier*)n)->value.sym, load, vl);
        }
        case EXPR_RETURN: {
            ControlFlowContext& fn_ctx = _control_flow_ctx.front();
            auto ast_val = ((expr_break*)n)->value;
            llvm::Value* v;
            assert(!_control_flow_ctx.back().continues_afterwards);
            if (ast_val) {
                _control_flow_ctx.back().continues_afterwards = true;
                // todo: where do we cast?
                lle = getAstNodeIR(ast_val, true, &v);
                _control_flow_ctx.back().continues_afterwards = false;
                if (lle) return lle;
                if (!_builder.CreateAlignedStore(
                        v, fn_ctx.value, fn_ctx.value_align))
                    return LLE_FATAL;
            }
            _builder.CreateBr(fn_ctx.following_block);
            _builder.SetInsertPoint(fn_ctx.following_block);
            assert(!vl);
            return LLE_OK;
        }
        case EXPR_BREAK: {
            assert(false);
            return LLE_OK;
        }
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            llvm::Value** args = (llvm::Value**)pool_alloc(
                &_tc->permmem, sizeof(llvm::Value*) * c->arg_count);
            if (!args) return LLE_FATAL;
            for (ureg i = 0; i < c->arg_count; i++) {
                lle = getAstNodeIR(c->args[i], true, &args[i]);
                if (lle) return lle;
            }
            llvm::ArrayRef<llvm::Value*> args_arr_ref(
                args, args + c->arg_count);
            llvm::Function* callee;
            lle = genFunctionIR(c->target, &callee);
            if (lle) return lle;
            auto call = _builder.CreateCall(callee, args_arr_ref);
            if (!call) return LLE_FATAL;
            if (vl) *vl = call;
            return LLE_OK;
        }
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: {
            if (ast_node_flags_get_const(n->flags)) {
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
                        lle = getAstNodeIR(
                            ((sym_var_initialized*)n)->initial_value, true, &v);
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
                    var_val = new llvm::AllocaInst(
                        t, 0, nullptr, align, "" /* var->sym.name*/,
                        _builder.GetInsertBlock());
                    if (!var_val) return LLE_FATAL;
                    if (n->kind == SYM_VAR_INITIALIZED) {
                        llvm::Value* v;
                        lle = getAstNodeIR(
                            ((sym_var_initialized*)n)->initial_value, true, &v);
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
            if (!vl) return LLE_OK;
            if (load) {
                llvm::LoadInst* load =
                    _builder.CreateAlignedLoad(*llvar, align);
                if (!load) return LLE_FATAL;
                *vl = load;
            }
            else {
                *vl = *llvar;
            }
            return LLE_OK;
        }
        case EXPR_SCOPE_ACCESS: {
            expr_scope_access* esa = (expr_scope_access*)n;
            return getAstNodeIR((ast_node*)esa->target.sym, load, vl);
        }
        case EXPR_MEMBER_ACCESS: {
            auto ema = (expr_member_access*)n;
            llvm::Value* v;
            lle = getAstNodeIR(ema->lhs, false, &v);
            if (lle) return lle;
            auto st = (sc_struct*)ema->target.sym->declaring_st->owning_node;
            assert(((ast_node*)st)->kind == SC_STRUCT);
            ureg align;
            lle = lookupCType((ast_elem*)st, NULL, &align);
            if (lle) return lle;
            ureg idx = 0;
            for (ast_node** i = st->scp.body.elements;; i++) {
                assert(*i);
                if (*i == (ast_node*)ema->target.sym) break;
                if ((**i).kind == SYM_VAR || (**i).kind == SYM_VAR_INITIALIZED)
                    idx++;
            }
            assert(vl);
            auto gep = _builder.CreateStructGEP(v, idx);
            if (load) {
                *vl = _builder.CreateAlignedLoad(gep, align);
                return LLE_OK;
            }
            *vl = gep;
            return LLE_OK;
        }
        case SYM_IMPORT_GROUP:
        case SYM_IMPORT_MODULE: return LLE_OK;
        case EXPR_IF: {
            expr_if* i = (expr_if*)n;
            llvm::Value* cond;
            lle = getAstNodeIR(i->condition, true, &cond);
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
            lle = createScopeValue(i->ctype, *ctx);
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
                lle = addIfBranch(i->if_body);
                if (lle) return lle;
                ctx->first_block = else_block;
                lle = addIfBranch(i->else_body);
                if (lle) return lle;
            }
            else {
                auto if_block = llvm::BasicBlock::Create(
                    _context, "", _curr_fn, following_block);
                if (!if_block) return LLE_FATAL;
                if (!_builder.CreateCondBr(cond, if_block, following_block))
                    return LLE_FATAL;
                ctx->first_block = if_block;
                lle = addIfBranch(i->if_body);
                if (lle) return lle;
            }
            auto val = ctx->value;
            ureg align = ctx->value_align;
            _control_flow_ctx.pop_back();
            _builder.SetInsertPoint(following_block);
            if (!vl) return LLE_OK;
            assert(val);
            if (!load) {
                *vl = val;
                return LLE_OK;
            }
            *vl = _builder.CreateAlignedLoad(val, align);
            if (!*vl) return LLE_FATAL;
            return LLE_OK;
        }
        default: assert(false);
    }
    assert(false);
    return LLE_FATAL;
}

llvm_error LLVMBackend::genBinaryOpIR(expr_op_binary* b, llvm::Value** vl)
{
    if (b->op->kind == SC_FUNC) {
        assert(false); // TODO
    }
    llvm::Value *lhs, *rhs;
    llvm_error lle = getAstNodeIR(b->rhs, true, &rhs);
    if (lle) return lle;
    lle = getAstNodeIR(b->lhs, (b->node.op_kind != OP_ASSIGN), &lhs);
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
            _builder.CreateAlignedStore(rhs, lhs, align);
            if (vl) v = _builder.CreateAlignedLoad(lhs, align);
        } break;
        default: assert(false); return LLE_FATAL;
    }
    if (vl) *vl = v;
    return LLE_OK;
}
char* name_mangle(sc_func* fn)
{
    symbol_table* st = fn->scp.body.symtab;
    while (st->owning_node->kind != ELEM_MDG_NODE) st = st->parent;
    mdg_node* n = (mdg_node*)st->owning_node;
    if (n == TAUC.mdg.root_node) return fn->scp.sym.name;
    ureg mnl = strlen(n->name);
    ureg fnl = strlen(fn->scp.sym.name);
    char* res = (char*)malloc(mnl + fnl + 3); // TODO: manage mem properly
    memcpy(res, n->name, mnl);
    memcpy(res + mnl, "::", 2);
    memcpy(res + mnl + 2, fn->scp.sym.name, fnl);
    *(res + mnl + fnl + 2) = '\0';
    return res;
}
llvm_error LLVMBackend::genFunctionIR(sc_func* fn, llvm::Function** llfn)
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
    if (!*fn->scp.body.elements || !isIDInModule(fn->id)) {
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
    lle = createScopeValue(fn->return_ctype, ctx);
    if (lle) return lle;
    ctx.first_block = func_block;
    ctx.following_block = llvm::BasicBlock::Create(_context, "", func);
    if (!ctx.following_block) return LLE_FATAL;

    _curr_fn = func;
    _builder.SetInsertPoint(func_block);
    lle = addAstBodyIR(&fn->scp.body, false);

    if (_builder.GetInsertBlock() != ctx.following_block) {
        _builder.CreateBr(ctx.following_block);
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
    _control_flow_ctx.pop_back();
    return lle;
}
llvm_error LLVMBackend::emitModuleIR(const std::string& ll_name)
{
    std::error_code EC;
    llvm::raw_fd_ostream ir_stream{ll_name, EC, llvm::sys::fs::F_None};
    _module->print(ir_stream, nullptr, true, true);
    ir_stream.flush();
    return LLE_OK;
}
llvm_error LLVMBackend::emitModule(const std::string& obj_name)
{
    tprintf("emmitting %s ", (char*)obj_name.c_str());
    if (true) {
        emitModuleIR(obj_name.substr(0, obj_name.size() - 3) + "ll");
    }
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

    llvm::legacy::PassManager CodeGenPasses;
    CodeGenPasses.add(llvm::createTargetTransformInfoWrapperPass(
        _target_machine->getTargetIRAnalysis()));

    CodeGenPasses.add(new llvm::TargetLibraryInfoWrapperPass(*TLII));

    auto file_type = llvm::TargetMachine::CGFT_ObjectFile;
    // file_type = llvm::TargetMachine::CGFT_AssemblyFile;
    std::error_code EC;
    llvm::raw_fd_ostream file_stream{obj_name, EC, llvm::sys::fs::F_None};
    if (_target_machine->addPassesToEmitFile(
            CodeGenPasses, file_stream, nullptr, file_type)) {
        llvm::errs() << "TheTargetMachine can't emit a file of this type\n";
        return LLE_FATAL;
    }
    PerFunctionPasses.doInitialization();
    for (llvm::Function& F : *_module)
        if (!F.isDeclaration()) PerFunctionPasses.run(F);
    PerFunctionPasses.doFinalization();

    PerModulePasses.run(*_module);

    CodeGenPasses.run(*_module);
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

llvm_error removeObjs(LLVMModule** start, LLVMModule** end)
{
    for (LLVMModule** i = start; i != end; i++) {
        unlink((**i).name.c_str());
    }
    return LLE_OK;
}
