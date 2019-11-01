#include "llvm_backend.hpp"
#include <memory>
#include <fstream>
#include <iostream>

static PPRunner* PP_RUNNER;
extern "C" {
#include "utils/ptrlist.h"
#include "thread_context.h"
#include "utils/pool.h"
#include "tauc.h"
#include <utils/debug_utils.h>

static ureg globals_refcount = 0;
int llvm_backend_init_globals()
{
    globals_refcount++;
    if (globals_refcount != 1) return OK;
    // InitializeAllTargetInfos();
    LLVMInitializeNativeTarget();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();
    const char* args[] = {"tauc",
                          /*"-time-passes",*/
                          /*"--debug-pass=Structure"*/};

    llvm::cl::ParseCommandLineOptions(sizeof(args) / sizeof(char*), args);
    PP_RUNNER = new (std::nothrow) PPRunner();
    if (!PP_RUNNER) {
        llvm::llvm_shutdown();
        globals_refcount--;
        return ERR;
    }
    return OK;
}
void llvm_backend_fin_globals()
{
    globals_refcount--;
    if (globals_refcount != 0) return;
    llvm::llvm_shutdown();
    delete PP_RUNNER;
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
llvm_error llvm_backend_init_module(
    llvm_backend* llvmb, mdg_node** start, mdg_node** end, llvm_module** mod)
{
    return ((LLVMBackend*)llvmb)->initModule(start, end, (LLVMModule**)mod);
}

llvm_error llvm_backend_run_pp(
    llvm_backend* llvmb, ureg private_sym_count, ptrlist* resolve_nodes)
{
    return ((LLVMBackend*)llvmb)->runPP(private_sym_count, resolve_nodes);
}

llvm_error llvm_backend_reserve_symbols(
    llvm_backend* llvmb, ureg private_sym_count, ureg public_sym_count)
{
    return ((LLVMBackend*)llvmb)
        ->reserveSymbols(private_sym_count, public_sym_count);
}
void llvm_backend_remap_local_id(llvm_backend* llvmb, ureg old_id, ureg new_id)
{
    ((LLVMBackend*)llvmb)->remapLocalID(old_id, new_id);
}

llvm_error llvm_backend_emit_module(
    llvm_backend* llvmb, ureg startid, ureg endid, ureg priv_sym_count)
{
    return ((LLVMBackend*)llvmb)->emit(startid, endid, priv_sym_count);
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
pp_resolve_node**
llvm_backend_lookup_pp_resolve_node(llvm_backend* llvmb, ureg id)
{
    return ((LLVMBackend*)llvmb)->lookupPPResolveNode(id);
}
} // CPP
static inline void link_dll(PPRunner* pp, const char* path)
{

    auto l = llvm::orc::DynamicLibrarySearchGenerator::Load(path, '\0');
    if (!l) {
        llvm::errs() << l.takeError() << "\n";
        assert(false);
    }
    pp->exec_session.getMainJITDylib().setGenerator(l.get());
    /* exec_session.getMainJITDylib().define(llvm::orc::absoluteSymbols(
        {{"puts", llvm::orc::pointerToJITTargetAddress(&puts)},
         {"printf", llvm::orc::pointerToJITTargetAddress(&printf)}}));*/
}
static inline void link_lib(PPRunner* pp, const char* path)
{
    auto& dl = pp->exec_session.createJITDylib(path);
    auto file{std::move(llvm::MemoryBuffer::getFile(path).get())};
    auto err = pp->obj_link_layer.add(
        dl, std::move(file), pp->exec_session.allocateVModule());
    if (err) {
        llvm::errs() << err << "\n";
        assert(false);
    }
}
PPRunner::PPRunner()
    : exec_session(), obj_link_layer(exec_session, []() {
          return llvm::make_unique<llvm::SectionMemoryManager>();
      })
{

    link_dll(this, "/lib64/ld-linux-x86-64.so.2");
    link_dll(this, "/lib/x86_64-linux-gnu/libc.so.6");
    // link_lib(this, "/usr/lib/x86_64-linux-gnu/libc_nonshared.a");
}
PPRunner::~PPRunner()
{
}
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
    opt.DataSections = true;
    llvm::Optional<llvm::CodeModel::Model> CM = llvm::CodeModel::Small;
    llvm::CodeGenOpt::Level OptLevel = llvm::CodeGenOpt::None;
    llvm::Optional<llvm::Reloc::Model> rm{llvm::Reloc::Model::PIC_};
    _target_machine = target->createTargetMachine(
        target_triple, LLVMGetHostCPUName(), LLVMGetHostCPUFeatures(), opt,
        std::move(rm), CM, OptLevel);
    _data_layout = new llvm::DataLayout(_target_machine->createDataLayout());
    _mod_dylib = NULL;
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
                    // TODO: decide on supported architectures and handle
                    // this accordingly
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
llvm_error LLVMBackend::reserveSymbols(ureg priv_sym_count, ureg pub_sym_count)
{
    if (_local_value_store.size() < priv_sym_count) {
        _local_value_store.resize(priv_sym_count, NULL);
        _local_value_state.resize(priv_sym_count, NOT_GENERATED);
    }
    if (_global_value_store.size() < pub_sym_count) {
        _global_value_store.resize(pub_sym_count, NULL);
        _global_value_state.resize(pub_sym_count, NOT_GENERATED);
    }
    return LLE_OK;
}
llvm_error
LLVMBackend::initModule(mdg_node** start, mdg_node** end, LLVMModule** module)
{
    LLVMModule* m = new LLVMModule();
    if (!m) return LLE_FATAL;
    for (mdg_node** n = start; n != end; n++) {
        m->module_str += (**n).name;
        if (n + 1 != end) {
            m->module_str += "&";
        }
    }
    m->module_obj = m->module_str + ".obj";
    *module = m;
    _mod_handle = m;
    _mods_start = start;
    _mods_end = end;
    return LLE_OK;
}
void LLVMBackend::resetAfterEmit()
{
    for (ureg id : _reset_after_emit) {
        ureg pid = id;
        ValueState* state;
        llvm::Value** val;
        if (isGlobalID(id)) {
            state = &_global_value_state[pid];
            val = (llvm::Value**)&_global_value_store[pid];
        }
        else {
            pid -= PRIV_SYMBOL_OFFSET;
            state = &_local_value_state[pid];
            val = (llvm::Value**)&_local_value_store[pid];
        }
        if (llvm::isa<llvm::GlobalVariable>(**val)) {
            if (*state == IMPL_ADDED) {
                *state = IMPL_DESTROYED;
            }
            else if (*state == PP_IMPL_ADDED) {
                *state = PP_IMPL_DESTROYED;
            }
            else if (*state == STUB_ADDED) {
                ((llvm::GlobalVariable*)*val)->removeFromParent();
                *state = STUB_GENERATED;
            }
            else {
                assert(*state == PP_STUB_ADDED);
                ((llvm::GlobalVariable*)*val)->removeFromParent();
                *state = PP_STUB_GENERATED;
            }
        }
        else {
            assert(llvm::isa<llvm::Function>(*val));
            ((llvm::Function*)*val)->removeFromParent();
            if (*state == IMPL_ADDED) {
                ((llvm::Function*)*val)->deleteBody();
                *state = STUB_GENERATED;
            }
            else if (*state == PP_IMPL_ADDED) {
                ((llvm::Function*)*val)->deleteBody();
                *state = PP_STUB_GENERATED;
            }
            else if (*state == STUB_ADDED) {
                *state = STUB_GENERATED;
            }
            else {
                assert(*state == PP_STUB_ADDED);
                *state = PP_STUB_GENERATED;
            }
        }
    }
    _reset_after_emit.clear();
}
llvm_error LLVMBackend::runPP(ureg private_sym_count, ptrlist* resolve_nodes)
{
    // init id space
    _mod_startid = 0;
    _mod_endid = 0;
    _private_sym_count = private_sym_count;
    if (reserveSymbols(private_sym_count, 0)) return LLE_FATAL;
    // create name
    std::string num =
        std::to_string(_pp_count.fetch_add(1, std::memory_order_relaxed));
    std::string pp_func_name = "__pp_func_" + num;
    // create actual module
    _module = new (std::nothrow) llvm::Module("__pp_mod_" + num, _context);
    if (!_module) return LLE_OK;
    _module->setTargetTriple(_target_machine->getTargetTriple().str());
    _module->setDataLayout(*_data_layout);
    _pp_mode = true;
    bool is_void;
    llvm_error lle = genPPFunc(pp_func_name.c_str(), resolve_nodes);
    if (lle) return lle;

    // emit
    lle = emitModule();
    if (lle) return lle;

    auto mainfn = PP_RUNNER->exec_session.lookup(
        llvm::orc::JITDylibSearchList(
            {{&PP_RUNNER->exec_session.getMainJITDylib(), true}}),
        PP_RUNNER->exec_session.intern(pp_func_name));

    if (!mainfn) {
        llvm::errs() << mainfn.takeError() << "\n";
        assert(false);
    }

    auto jit_func = (void (*)())(mainfn.get()).getAddress();
    jit_func();

    resetAfterEmit();
    delete _module;
    return lle;
}
llvm_error LLVMBackend::emit(ureg startid, ureg endid, ureg private_sym_count)
{
    _mod_dylib = NULL;
    // init id space
    _pp_mode = false;
    _mod_startid = startid;
    _mod_endid = endid;
    _private_sym_count = private_sym_count;
    if (reserveSymbols(private_sym_count, endid)) return LLE_FATAL;
    // create actual module
    _module =
        new (std::nothrow) llvm::Module(_mod_handle->module_str, _context);
    if (!_module) return LLE_OK;
    _module->setTargetTriple(_target_machine->getTargetTriple().str());
    _module->setDataLayout(*_data_layout);
    llvm_error lle;
    tprintf("generating {%s}", _mod_handle->module_str.c_str());
    TIME(lle = genModules(););
    tflush();
    if (lle) return lle;
    emitModuleIR();
    TIME(lle = emitModule(););
    tflush();
    // PERF: instead of this last minute checking
    // just have different buffers for the different reset types
    resetAfterEmit();
    delete _module;
    _local_value_store.assign(_private_sym_count, NULL);
    _local_value_state.assign(_private_sym_count, NOT_GENERATED);
    return lle;
}
void LLVMBackend::remapLocalID(ureg old_id, ureg new_id)
{
    assert(isLocalID(old_id));
    old_id -= PRIV_SYMBOL_OFFSET;
    if (isLocalID(new_id)) {
        new_id -= PRIV_SYMBOL_OFFSET;
        _local_value_store[new_id] = _local_value_store[old_id];
        _local_value_state[new_id] = _local_value_state[old_id];
    }
    else {
        _global_value_store[new_id] = _local_value_store[old_id];
        _global_value_state[new_id] = _local_value_state[old_id];
    }
    _local_value_store[old_id] = NULL;
    _local_value_state[old_id] = NOT_GENERATED;
}
pp_resolve_node** LLVMBackend::lookupPPResolveNode(ureg id)
{
    if (isLocalID(id)) {
        id -= PRIV_SYMBOL_OFFSET;
        if (_local_value_state[id] > PP_RN_GENERATED) return NULL;
        return (pp_resolve_node**)&_local_value_store[id];
    }
    else {
        if (_global_value_state[id] > PP_RN_GENERATED) return NULL;
        return (pp_resolve_node**)&_global_value_state[id];
    }
}
llvm_error LLVMBackend::genPPFunc(const char* func_name, ptrlist* resolve_nodes)
{
    llvm::FunctionType* func_sig;
    llvm_error lle;
    func_sig = llvm::FunctionType::get(_primitive_types[PT_VOID], false);
    if (!func_sig) return LLE_FATAL;

    llvm::Function* func = llvm::Function::Create(
        func_sig, llvm::Function::ExternalLinkage, func_name, _module);
    if (!func) return LLE_FATAL;
    llvm::BasicBlock* func_block = llvm::BasicBlock::Create(_context, "", func);
    if (!func_block) return LLE_FATAL;
    assert(_control_flow_ctx.size() == 0);
    _control_flow_ctx.emplace_back();
    ControlFlowContext& ctx = _control_flow_ctx.back();
    _builder.SetInsertPoint(func_block);
    ctx.first_block = func_block;
    ctx.following_block = NULL;
    _curr_fn = func;
    // only used in genScopeValue and thats never called here
    _curr_fn_ast_node = NULL;
    _builder.SetInsertPoint(func_block);

    ctx.continues_afterwards = true;
    pli it = pli_begin(resolve_nodes);
    for (auto n = (pp_resolve_node*)pli_next(&it); n;
         n = (pp_resolve_node*)pli_next(&it)) {
        if (n->node->kind == EXPR_PP) {
            auto expr = (expr_pp*)n->node;
            if (n->result_used && expr->ctype != VOID_ELEM &&
                expr->ctype != UNREACHABLE_ELEM) {
                llvm::Type* ret_type;
                ureg size;
                ureg align;
                lle = lookupCType(expr->ctype, &ret_type, &align, &size);
                if (lle) return lle;
                assert(align <= REG_BYTES); // TODO
                if (size <= sizeof(expr->result_buffer)) {
                    expr->result_buffer.true_res_buffer =
                        (void*)&expr->result_buffer.data[0];
                }
                else {
                    // TODO: use tempmem for this
                    expr->result_buffer.true_res_buffer = malloc(size);
                }
                llvm::Value *val, *tgt;
                lle = genAstNode(expr->pp_expr, NULL, &val);
                if (lle) return lle;
                auto res = llvm::ConstantInt::get(
                    _primitive_types[PT_UINT],
                    (ureg)expr->result_buffer.true_res_buffer);
                auto resptr = llvm::ConstantExpr::getBitCast(
                    res, ret_type->getPointerTo());
                if (!_builder.CreateStore(val, resptr)) return LLE_FATAL;
            }
            else {
                lle = genAstNode(expr->pp_expr, NULL, NULL);
                if (lle) return lle;
            }
        }
        else {
            lle = genAstNode(n->node, NULL, NULL);
            if (lle) return lle;
        }
    }
    it = pli_begin(resolve_nodes);
    for (auto n = (pp_resolve_node*)pli_next(&it); n;
         n = (pp_resolve_node*)pli_next(&it)) {
        if (n->node->kind == EXPR_PP) {
            auto expr = (expr_pp*)n->node;
            if (n->result_used && expr->ctype != VOID_ELEM &&
                expr->ctype != UNREACHABLE_ELEM) {
                expr->result = (void*)&expr->result_buffer.true_res_buffer;
            }
        }
    }
    if (lle) return lle;
    assert(!ctx.following_block && !ctx.value);
    _builder.CreateRetVoid();
    _control_flow_ctx.pop_back();
    return lle;
}
llvm_error LLVMBackend::genModules()
{
    for (mdg_node** n = _mods_start; n != _mods_end; n++) {
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
llvm_error LLVMBackend::genAstBody(
    ast_body* b, bool continues_afterwards, bool* end_reachable)
{
    ControlFlowContext& ctx = _control_flow_ctx.back();
    ctx.continues_afterwards = true;
    if (end_reachable) *end_reachable = true;
    for (ast_node** n = b->elements; *n; n++) {
        bool last = (!*(n + 1));
        if (last) ctx.continues_afterwards = continues_afterwards;
        llvm_error lle = genAstNode(*n, NULL, NULL);
        if (lle) return lle;
        if (last && end_reachable) {
            ast_elem* ctype = get_resolved_ast_node_ctype(*n);
            *end_reachable = (ctype != UNREACHABLE_ELEM);
        }
    }
    return LLE_OK;
}
void** LLVMBackend::lookupAstElem(ureg id)
{
    if (isLocalID(id)) {
        return &_local_value_store[id - PRIV_SYMBOL_OFFSET];
    }
    return &_global_value_store[id];
}
ValueState* LLVMBackend::lookupValueState(ureg id)
{
    if (isLocalID(id)) return &_local_value_state[id - PRIV_SYMBOL_OFFSET];
    return &_global_value_state[id];
}
llvm::Type** LLVMBackend::lookupTypeRaw(ureg id)
{
    return (llvm::Type**)lookupAstElem(id);
}
llvm_error
LLVMBackend::lookupCType(ast_elem* e, llvm::Type** t, ureg* align, ureg* size)
{
    switch (e->kind) {
        case PRIMITIVE: {
            primitive_kind kind = ((primitive*)e)->sym.node.pt_kind;
            if (t) *t = _primitive_types[kind];
            if (align) *align = PRIMITIVES[kind].alignment;
            if (size) *size = PRIMITIVES[kind].size;
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
                // PERF: we could steal the array of elements for our types
                // here this might be too big because of nested structs etc.
                // but it's definitely big enough
                auto members = (llvm::Type**)pool_alloc(
                    &_tc->permmem,
                    sizeof(llvm::Type*) * st->sc.body.symtab->decl_count);
                ureg memcnt = 0;
                for (ast_node** i = st->sc.body.elements; *i; i++) {
                    // TODO: usings, static members, etc.
                    if ((**i).kind == SYM_VAR ||
                        (**i).kind == SYM_VAR_INITIALIZED) {
                        if (ast_flags_get_static((**i).flags)) {
                            llvm_error lle = genAstNode(*i, NULL, NULL);
                            if (lle) return lle;
                        }
                        lookupCType(
                            ((sym_var*)*i)->ctype, &members[memcnt], NULL,
                            NULL);
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
            if (size) {
                *size = _data_layout->getTypeAllocSize((llvm::StructType*)*t);
            }
        } break;
        case TYPE_POINTER: {
            // this is the alignment requirement of the pointer itself, not
            // the base type
            if (align) *align = PRIMITIVES[PT_VOID_PTR].alignment;
            if (size) *size = PRIMITIVES[PT_VOID_PTR].size;
            if (!t) return LLE_OK;
            llvm_error lle =
                lookupCType(((type_pointer*)e)->base, t, NULL, NULL);
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
    return id >= PRIV_SYMBOL_OFFSET;
}
bool LLVMBackend::isGlobalID(ureg id)
{
    return !isLocalID(id);
}
bool LLVMBackend::isPPSymbolGlobal(symbol* sym)
{
    if (sym->declaring_st->owning_node->kind != OSC_EXTEND) return false;
    auto am = ast_flags_get_access_mod(sym->node.flags);
    return (am == AM_PUBLIC || am == AM_PROTECTED);
}
llvm_error LLVMBackend::getFollowingBlock(llvm::BasicBlock** following_block)
{
    ControlFlowContext* ctx = &_control_flow_ctx.back();
    llvm::BasicBlock* fb;
    if (ctx->continues_afterwards) {
        fb = llvm::BasicBlock::Create(
            _context, "", _curr_fn, ctx->following_block);
    }
    else if (ctx->following_block) {
        fb = ctx->following_block;
    }
    else {
        fb = llvm::BasicBlock::Create(_context, "", _curr_fn);
        auto curr_ib = _builder.GetInsertBlock();
        auto curr_ip = _builder.GetInsertPoint();
        _builder.SetInsertPoint(
            ctx->first_block, ctx->first_block->getInstList().begin());
        genScopeValue(_curr_fn_ast_node->return_ctype, *ctx);
        _builder.SetInsertPoint(curr_ib, curr_ip);
        auto c = _control_flow_ctx.end();
        while (true) {
            if (c->following_block == NULL) c->following_block = fb;
            if (&*c == _curr_fn_control_flow_ctx) break;
            --c;
        }
        ctx->following_block = fb;
    }
    *following_block = fb;
    return LLE_OK;
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
        llvm_error lle = lookupCType(ctype, &t, &ctx.value_align, NULL);
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
LLVMBackend::buildConstant(ast_elem* ctype, void* data, llvm::Constant** res)
{
    switch (ctype->kind) {
        case SC_STRUCT: {
            llvm::StructType* st;
            lookupCType(ctype, (llvm::Type**)&st, NULL, NULL);
            auto struct_layout = _data_layout->getStructLayout(st);
            ureg elem_count = st->getNumElements();
            auto elems = new std::vector<llvm::Constant*>(elem_count);
            ast_node** st_elem = ((sc_struct*)ctype)->sc.body.elements;
            for (ureg i = 0; i < elem_count; i++) {
                while (true) {
                    if ((**st_elem).kind == SYM_VAR ||
                        (**st_elem).kind == SYM_VAR_INITIALIZED) {
                        break;
                    }
                    st_elem++;
                    assert(*st_elem);
                }
                auto v = (sym_var*)*st_elem;
                assert(v->var_id == i);
                llvm::Constant* c;
                llvm_error lle = buildConstant(
                    v->ctype, ptradd(data, struct_layout->getElementOffset(i)),
                    &c);
                if (lle) return lle;
                (*elems)[i] = c;
                st_elem++;
            }
            *res = llvm::ConstantStruct::get(st, *elems);
            if (!*res) return LLE_FATAL;
            return LLE_OK;
        }
        case PRIMITIVE: {
            auto pt = (primitive*)ctype;
            primitive_kind pk = pt->sym.node.pt_kind;
            switch (pk) {
                case PT_INT:
                case PT_UINT: {
                    auto t = (llvm::IntegerType*)
                        _primitive_types[pt->sym.node.pt_kind];
                    auto c =
                        llvm::ConstantInt::get(t, *(ureg*)data, (pk == PT_INT));
                    if (!c) return LLE_FATAL;
                    *res = c;
                    return LLE_OK;
                }
                case PT_STRING: {
                    auto c = _builder.CreateGlobalStringPtr(*(char**)data);
                    if (!c) return LLE_FATAL;
                    *res = c;
                    return LLE_OK;
                }
                case PT_FLOAT: {
                    auto c = llvm::ConstantFP::get(
                        _context,
                        *new llvm::APFloat(
                            llvm::APFloatBase::IEEEsingle(), *(float*)data));
                    if (!c) return LLE_FATAL;
                    *res = c;
                    return LLE_OK;
                }
                default: assert(false);
            }
        }
        default: assert(false); return LLE_FATAL;
    }
}
llvm_error
LLVMBackend::genVariable(ast_node* n, llvm::Value** vl, llvm::Value** vl_loaded)
{
    if (ast_flags_get_const(n->flags)) assert(false); // TODO (ctfe)
    sym_var* var = (sym_var*)n;
    llvm::Type* t;
    ureg align;
    llvm_error lle = lookupCType(var->ctype, &t, &align, NULL);
    if (lle) return lle;
    auto state = lookupValueState(var->var_id);
    bool generated = true;
    llvm::Value** llvar = (llvm::Value**)lookupAstElem(var->var_id);
    if (*state == IMPL_ADDED || *state == STUB_ADDED ||
        *state == PP_IMPL_ADDED || *state == PP_STUB_ADDED) {
    }
    else if (!_pp_mode && *state == PP_STUB_GENERATED) {
        (**llvar).deleteValue();
        generated = false;
    }
    else if (*state == STUB_GENERATED) {
        assert(isGlobalID(var->var_id));
        *state = STUB_ADDED;
        _module->getGlobalList().push_back((llvm::GlobalVariable*)*llvar);
        _reset_after_emit.push_back(var->var_id);
    }
    else if (*state == PP_STUB_GENERATED) {
        assert(isLocalID(var->var_id));
        assert(!isPPSymbolGlobal((symbol*)var));
        *state = PP_STUB_ADDED;
        _reset_after_emit.push_back(var->var_id);
    }
    else {
        assert(*state == NOT_GENERATED);
        *state = _pp_mode ? PP_IMPL_ADDED : IMPL_ADDED;
        generated = false;
    }
    if (!generated) {
        ast_node_kind k = var->sym.declaring_st->owning_node->kind;
        llvm::Value* var_val;
        if (k == ELEM_MDG_NODE || k == OSC_MODULE || k == OSC_EXTEND) {
            // global var
            llvm::GlobalVariable::LinkageTypes lt;
            if (isLocalID(var->var_id)) {
                if (_pp_mode) {
                    _reset_after_emit.push_back(var->var_id);
                    lt = llvm::GlobalVariable::ExternalLinkage;
                }
                else {
                    lt = llvm::GlobalVariable::InternalLinkage;
                }
            }
            else {
                lt = llvm::GlobalVariable::ExternalLinkage;
                _reset_after_emit.push_back(var->var_id);
            }
            llvm::Constant* init = NULL;
            if (n->kind == SYM_VAR_INITIALIZED && isIDInModule(var->var_id)) {
                llvm::Value* v;
                lle = genAstNode(
                    ((sym_var_initialized*)n)->initial_value, NULL, &v);
                if (lle) return lle;
                if (!(init = llvm::dyn_cast<llvm::Constant>(v))) {
                    assert(false); // must be constant, TODO: error
                    return LLE_FATAL;
                }
            }
            auto gv = new llvm::GlobalVariable(
                *_module, t, false, lt, init, var->sym.name);
            if (!gv) return LLE_FATAL;
            gv->setAlignment(align);
            var_val = gv;
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
                llvm::Value* init_val;
                lle = genAstNode(
                    ((sym_var_initialized*)n)->initial_value, NULL, &init_val);
                if (lle) return lle;
                if (!_builder.CreateAlignedStore(
                        init_val, var_val, align, false))
                    return LLE_FATAL;
            }
        }
        *llvar = var_val;
    }
    if (vl_loaded) {
        *vl_loaded = _builder.CreateAlignedLoad(*llvar, align);
        if (!*vl_loaded) return LLE_FATAL;
    }
    if (vl) *vl = *llvar;
    return LLE_OK;
}
llvm_error
LLVMBackend::genAstNode(ast_node* n, llvm::Value** vl, llvm::Value** vl_loaded)
{
    assert(
        ast_flags_get_resolved(n->flags) ||
        ast_elem_is_open_scope((ast_elem*)n));
    // TODO: proper error handling
    llvm_error lle;
    switch (n->kind) {
        case EXPR_PP: {
            if (!vl && !vl_loaded) return LLE_OK;
            auto epp = (expr_pp*)n;
            if (!epp->result) return genAstNode(epp->pp_expr, vl, vl_loaded);
            assert(!vl);
            return buildConstant(
                epp->ctype, epp->result, (llvm::Constant**)vl_loaded);
        }
        case OSC_MODULE:
        case OSC_EXTEND:
            assert(!vl);
            return LLE_OK; // these are handled by the osc iterator
        case SC_STRUCT: return lookupCType((ast_elem*)n, NULL, NULL, NULL);
        case SC_FUNC: return genFunction((sc_func*)n, vl);
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
                    assert(!vl);
                    if (vl_loaded) *vl_loaded = c;
                    return LLE_OK;
                }
                case PT_STRING: {
                    auto c = _builder.CreateGlobalStringPtr(l->value.str);
                    if (!c) return LLE_FATAL;
                    assert(!vl);
                    if (vl_loaded) *vl_loaded = c;
                    return LLE_OK;
                }
                case PT_FLOAT: {
                    auto c = llvm::ConstantFP::get(
                        _context,
                        *new llvm::APFloat(
                            llvm::APFloatBase::IEEEsingle(), l->value.str));
                    assert(!vl);
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
#if DEBUG
            auto state = *lookupValueState(f->id);
            assert(state == IMPL_ADDED || state == PP_IMPL_ADDED);
#endif
            lle = genFunction(f, (llvm::Value**)&fn);
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
                tgt_ctx = _curr_fn_control_flow_ctx;
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
                        auto c = _control_flow_ctx.end();
                        while (true) {
                            if (c->following_block == NULL)
                                c->following_block = tgt_ctx->following_block;
                            if (&*c == _curr_fn_control_flow_ctx) break;
                            --c;
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
            llvm::BasicBlock* following_block;
            lle = getFollowingBlock(&following_block);
            if (lle) return lle;
            _control_flow_ctx.emplace_back();
            ControlFlowContext* ctx = &_control_flow_ctx.back();
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
                if (!*vl_loaded) return LLE_FATAL;
            }
            _control_flow_ctx.pop_back();
            return LLE_OK;
        }
        case EXPR_BLOCK: {
            expr_block* b = (expr_block*)n;
            llvm::BasicBlock* following_block;
            lle = getFollowingBlock(&following_block);
            if (lle) return lle;
            _control_flow_ctx.emplace_back();
            ControlFlowContext* ctx = &_control_flow_ctx.back();
            b->control_flow_ctx = ctx;
            ctx->following_block = following_block;
            lle = genScopeValue(b->ctype, *ctx);
            if (lle) return lle;
            ctx->first_block = _builder.GetInsertBlock();
            lle = genAstBody(&b->body, true);
            if (lle) return lle;
            if (!_builder.CreateBr(following_block)) return LLE_FATAL;
            _builder.SetInsertPoint(following_block);
            if (vl) *vl = ctx->value;
            if (vl_loaded) {
                *vl_loaded =
                    _builder.CreateAlignedLoad(ctx->value, ctx->value_align);
                if (!*vl_loaded) return LLE_FATAL;
            }
            _control_flow_ctx.pop_back();
            return LLE_OK;
        }
        case EXPR_CALL: {
            expr_call* c = (expr_call*)n;
            llvm::Value** args = (llvm::Value**)pool_alloc(
                &_tc->permmem, sizeof(llvm::Value*) * c->arg_count);
            if (!args) return LLE_FATAL;
            for (ureg i = 0; i < c->arg_count; i++) {
                lle = genAstNode(c->args[i], NULL, &args[i]);
                if (lle) return lle;
            }
            llvm::ArrayRef<llvm::Value*> args_arr_ref(
                args, args + c->arg_count);
            llvm::Value* callee;
            lle = genFunction((sc_func*)c->target.fn, &callee);
            if (lle) return lle;
            auto call = _builder.CreateCall(callee, args_arr_ref);
            if (!call) return LLE_FATAL;
            if (vl) *vl = call;
            if (vl_loaded) *vl_loaded = call;
            return LLE_OK;
        }
        case SYM_VAR:
        case SYM_VAR_INITIALIZED: {
            return genVariable(n, vl, vl_loaded);
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
            lle = lookupCType((ast_elem*)st, NULL, &align, NULL);
            if (lle) return lle;

            assert(
                ema->target.sym->node.kind == SYM_VAR ||
                ema->target.sym->node.kind == SYM_VAR_INITIALIZED);
            ureg idx = ((sym_var*)ema->target.sym)->var_id;
            assert(vl || vl_loaded);
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
            llvm::BasicBlock* following_block;
            lle = getFollowingBlock(&following_block);
            if (lle) return lle;
            _control_flow_ctx.emplace_back();
            ControlFlowContext* ctx = &_control_flow_ctx.back();
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
    llvm::Value *lhs, *lhs_flat, *rhs;
    llvm_error lle;
    if (b->node.op_kind == OP_ASSIGN) {
        lle = genAstNode(b->lhs, &lhs_flat, NULL);
    }
    else if (b->node.op_kind == OP_ADD_ASSIGN) {
        lle = genAstNode(b->lhs, &lhs_flat, &lhs);
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
        case OP_SUB: v = _builder.CreateNSWSub(lhs, rhs); break;
        case OP_MUL: v = _builder.CreateNSWMul(lhs, rhs); break;
        case OP_DIV: v = _builder.CreateSDiv(lhs, rhs); break;
        case OP_MOD: v = _builder.CreateSRem(lhs, rhs); break;
        case OP_GREATER_THAN: v = _builder.CreateICmpSGT(lhs, rhs); break;
        case OP_LESS_THAN: v = _builder.CreateICmpSLT(lhs, rhs); break;
        case OP_EQUAL: v = _builder.CreateICmpEQ(lhs, rhs); break;
        case OP_ASSIGN: {
            ureg align;
            lookupCType(
                get_resolved_ast_node_ctype(b->lhs), NULL, &align, NULL);
            _builder.CreateAlignedStore(rhs, lhs_flat, align);
            if (vl_loaded) v = _builder.CreateAlignedLoad(lhs_flat, align);
        } break;
        case OP_ADD_ASSIGN: {
            ureg align;
            lookupCType(
                get_resolved_ast_node_ctype(b->lhs), NULL, &align, NULL);
            v = _builder.CreateNSWAdd(lhs, rhs);
            if (!v) return LLE_FATAL;
            if (!_builder.CreateAlignedStore(v, lhs_flat, align))
                return LLE_FATAL;
            if (vl_loaded) v = _builder.CreateAlignedLoad(lhs_flat, align);
        } break;
        default: assert(false); return LLE_FATAL;
    }
    if (vl_loaded) *vl_loaded = v;
    assert(!vl);
    return LLE_OK;
}
// TODO: do this properly
std::string name_mangle(sc_func* fn, const llvm::DataLayout& dl)
{
    std::string name = fn->sc.sym.name;
    symbol_table* st = fn->sc.body.symtab;
    while (st->owning_node->kind != ELEM_MDG_NODE) st = st->parent;
    mdg_node* n = (mdg_node*)st->owning_node;
    while (n->parent != NULL) {
        name = n->name + ("_" + name);
        n = n->parent;
    }
    if (name != "main") {
        name = name + "_" + std::to_string(fn->param_count);
    }
    std::string MangledName;
    {
        llvm::raw_string_ostream MangledNameStream(MangledName);
        llvm::Mangler::getNameWithPrefix(MangledNameStream, name, dl);
    }
    return MangledName;
}

llvm_error LLVMBackend::genFunction(sc_func* fn, llvm::Value** llfn)
{
    auto res = (llvm::Value**)lookupAstElem(fn->id);
    auto state = lookupValueState(fn->id);

    if (*state == IMPL_ADDED || *state == STUB_ADDED ||
        *state == PP_IMPL_ADDED || *state == PP_STUB_ADDED) {
        if (llfn) *llfn = *res;
        return LLE_OK;
    }
    if (!_pp_mode && *state == PP_STUB_GENERATED) {
        (**res).deleteValue();
    }
    else if (*state == STUB_GENERATED || *state == PP_STUB_GENERATED) {
        *state = (*state == STUB_GENERATED) ? STUB_ADDED : PP_STUB_ADDED;
        _module->getFunctionList().push_back((llvm::Function*)*res);
        _reset_after_emit.push_back(fn->id);
        if (llfn) *llfn = *res;
        return LLE_OK;
    }
    else if (*state == PP_IMPL_DESTROYED) {
        // we disabled this in favor of using stubs (--> PP_IMPL_GENERATED)
        assert(false);
        auto fun = PP_RUNNER->exec_session.lookup(
            llvm::orc::JITDylibSearchList(
                {{&PP_RUNNER->exec_session.getMainJITDylib(), true}}),
            PP_RUNNER->exec_session.intern(name_mangle(fn, *_data_layout)));
        if (!fun) {
            llvm::errs() << fun.takeError() << "\n";
            assert(false);
        }
        auto old_fn = (llvm::Function*)*res;
        auto cnst = llvm::ConstantInt::get(
            _primitive_types[PT_UINT], fun.get().getAddress());
        auto fnptr = llvm::ConstantExpr::getBitCast(
            cnst, old_fn->getFunctionType()->getPointerTo());
        if (llfn) *llfn = fnptr;
        *res = fnptr;
        *state = PP_STUB_ADDED;
        return LLE_OK;
    }
    else if (*state == PP_STUB_ADDED) {
        assert(_pp_mode);
        if (llfn) *llfn = *res;
        return LLE_OK;
    }
    else {
        assert(*state == NOT_GENERATED || *state == PP_IMPL_DESTROYED);
    }

    llvm::Function* func;
    llvm::FunctionType* func_sig;
    llvm::Type* ret_type;
    llvm_error lle = lookupCType(fn->return_ctype, &ret_type, NULL, NULL);
    if (lle) return lle;

    if (fn->param_count != 0) {
        llvm::Type** params = (llvm::Type**)pool_alloc(
            &_tc->permmem, sizeof(llvm::Type*) * fn->param_count);
        if (!params) return LLE_FATAL;
        for (ureg i = 0; i < fn->param_count; i++) {
            lle = lookupCType(fn->params[i].ctype, &params[i], NULL, NULL);
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
    // TODO: ugly hack,  use a proper extern function ast node
    bool extern_func = (fn->sc.body.srange == SRC_RANGE_INVALID);
    auto func_name_mangled = extern_func ? std::string(fn->sc.sym.name)
                                         : name_mangle(fn, *_data_layout);
    if (extern_func || !isIDInModule(fn->id)) {
        func = (llvm::Function*)llvm::Function::Create(
            func_sig, llvm::GlobalVariable::ExternalLinkage,
            _data_layout->getProgramAddressSpace(), func_name_mangled);
        if (!func) return LLE_FATAL;
        _module->getFunctionList().push_back(func);
        *state = STUB_ADDED;
        if (isGlobalID(fn->id) || _pp_mode) {
            _reset_after_emit.push_back(fn->id);
        }
        *res = func;
        if (llfn) *llfn = func;
        return LLE_OK;
    }
    auto lt = (isLocalID(fn->id) && !_pp_mode)
                  ? llvm::Function::InternalLinkage
                  : llvm::Function::ExternalLinkage;
    func = llvm::Function::Create(func_sig, lt, func_name_mangled, _module);
    if (!func) return LLE_FATAL;
    *res = func;
    if (llfn) *llfn = func;
    *state = (_pp_mode && isLocalID(fn->id)) ? PP_IMPL_ADDED : IMPL_ADDED;
    _reset_after_emit.push_back(fn->id);
    llvm::BasicBlock* func_block = llvm::BasicBlock::Create(_context, "", func);
    if (!func_block) return LLE_FATAL;
    ureg cfcsize = _control_flow_ctx.size();
    _control_flow_ctx.emplace_back();
    ControlFlowContext& ctx = _control_flow_ctx.back();

    // lle = genScopeValue(fn->return_ctype, ctx);
    // if (lle) return lle;
    ctx.first_block = func_block;
    ctx.following_block = NULL;
    auto prev_fn = _curr_fn;
    auto prev_fn_ast_node = _curr_fn_ast_node;
    auto prev_fn_cfc = _curr_fn_control_flow_ctx;
    auto prev_blk = _builder.GetInsertBlock();
    auto prev_pos = _builder.GetInsertPoint();
    _builder.SetInsertPoint(func_block);
    _curr_fn = func;
    _curr_fn_ast_node = fn;
    _curr_fn_control_flow_ctx = &ctx;
    _builder.SetInsertPoint(func_block);
    bool end_reachable = true;
    lle = genAstBody(&fn->sc.body, false, &end_reachable);
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
    else if (end_reachable) {
        assert(!ctx.value);
        _builder.CreateRetVoid();
    }
    _control_flow_ctx.pop_back();
    assert(_control_flow_ctx.size() == cfcsize);
    _curr_fn = prev_fn;
    _curr_fn_ast_node = prev_fn_ast_node;
    _curr_fn_control_flow_ctx = prev_fn_cfc;
    if (prev_blk) {
        _builder.SetInsertPoint(prev_blk, prev_pos);
    }
    return lle;
}
llvm_error LLVMBackend::emitModuleIR()
{
    std::error_code EC;
    std::string filepath = (_module->getName() + ".ll").str();
    llvm::raw_fd_ostream ir_stream{filepath.c_str(), EC, llvm::sys::fs::F_None};
    _module->print(ir_stream, nullptr, true, true);
    ir_stream.flush();
    return LLE_OK;
}
llvm_error LLVMBackend::emitModuleToStream(
    llvm::TargetLibraryInfoImpl* tlii, llvm::raw_pwrite_stream* stream,
    bool emit_asm)
{
    std::error_code ec;
    auto file_type = llvm::TargetMachine::CGFT_ObjectFile;
    if (emit_asm) file_type = llvm::TargetMachine::CGFT_AssemblyFile;
    llvm::legacy::PassManager CodeGenPasses;

    CodeGenPasses.add(llvm::createTargetTransformInfoWrapperPass(
        _target_machine->getTargetIRAnalysis()));

    CodeGenPasses.add(new llvm::TargetLibraryInfoWrapperPass(*tlii));

    if (_target_machine->addPassesToEmitFile(
            CodeGenPasses, *stream, nullptr, file_type)) {
        llvm::errs() << "TheTargetMachine can't emit a file of this type\n";
        return LLE_FATAL;
    }
    CodeGenPasses.run(*_module);
    stream->flush();
    return LLE_OK;
}
llvm_error
LLVMBackend::emitModuleToFile(llvm::TargetLibraryInfoImpl* tlii, bool emit_asm)
{
    std::error_code ec;
    std::string name = _mod_handle->module_obj;
    if (emit_asm) name = (_module->getName() + ".asm").str();
    llvm::raw_fd_ostream file_stream{name, ec, llvm::sys::fs::F_None};
    return emitModuleToStream(tlii, &file_stream, emit_asm);
}
llvm_error LLVMBackend::emitModuleToPP(
    llvm::TargetLibraryInfoImpl* tlii, bool write_out_file)
{
    // output IR module to a memory buffer file
    llvm::SmallVector<char, 0> obj_sv;
    llvm::raw_svector_ostream obj_sv_stream{obj_sv};
    llvm_error lle = emitModuleToStream(tlii, &obj_sv_stream, false);
    if (lle) return lle;
    if (write_out_file) {
        std::error_code ec;
        llvm::raw_fd_ostream file_stream{_mod_handle->module_obj, ec,
                                         llvm::sys::fs::F_None};
        file_stream.write(obj_sv.begin(), obj_sv.size());
    }
    std::unique_ptr<llvm::MemoryBuffer> obj_svmb{
        new llvm::SmallVectorMemoryBuffer{std::move(obj_sv)}};
    // link that mem buffer file
    PP_RUNNER->obj_link_layer.add(
        PP_RUNNER->exec_session.getMainJITDylib(), std::move(obj_svmb),
        PP_RUNNER->exec_session.allocateVModule());
    return LLE_OK;
}

llvm_error LLVMBackend::emitModule()
{
    if (!_pp_mode) {
        tprintf("emmitting {%s} ", _mod_handle->module_str.c_str());
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
    // pmb.DisableUnitAtATime = true;
    pmb.DisableUnrollLoops = true;
    pmb.SLPVectorize = false;
    pmb.LoopVectorize = false;
    pmb.RerollLoops = false;
    pmb.DisableGVNLoadPRE = true;
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
    for (llvm::Function& F : *_module) {
        if (!F.isDeclaration()) {
            PerFunctionPasses.run(F);
        }
    }
    PerFunctionPasses.doFinalization();
    PerModulePasses.run(*_module);
    llvm_error lle = LLE_OK;
    if (!_pp_mode) {
        if (!lle && _tc->t->emit_ll) emitModuleIR();
        if (_tc->t->emit_asm) lle = emitModuleToFile(TLII.get(), true);
        // if (!lle && _tc->t->emit_exe) emitModuleToFile(TLII.get(), false);
        if (!lle) emitModuleToPP(TLII.get(), _tc->t->emit_exe);
    }
    else {
        emitModuleIR();
        emitModuleToFile(TLII.get(), true);
        emitModuleToPP(TLII.get(), false);
    }
    return lle;
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
