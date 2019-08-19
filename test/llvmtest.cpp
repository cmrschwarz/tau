//===- examples/ModuleMaker/ModuleMaker.cpp - Example project ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This programs is a simple example that creates an LLVM module "from scratch",
// emitting it as a bitcode file to standard out.  This is just to show how
// LLVM projects work and to demonstrate some of the LLVM APIs.
//
//===----------------------------------------------------------------------===//

#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/CodeGenCWrappers.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/IR/LegacyPassManager.h>
#include <lld/Common/Driver.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/IRBuilder.h>
#include <unistd.h>

using namespace llvm;
Module* createTestModule(LLVMContext& ctx)
{
    llvm::IRBuilder<> builder(ctx);
    llvm::Module* mod = new llvm::Module("hello world", ctx);

    llvm::FunctionType* main_func_type =
        llvm::FunctionType::get(builder.getInt32Ty(), false);
    llvm::Function* main_func = llvm::Function::Create(
        main_func_type, llvm::Function::ExternalLinkage, "main", mod);
    llvm::BasicBlock* main_block = llvm::BasicBlock::Create(ctx, "", main_func);
    builder.SetInsertPoint(main_block);

    auto printf_args =
        *new llvm::ArrayRef<Type*>{builder.getInt8Ty()->getPointerTo()};

    llvm::FunctionType* printf_type =
        llvm::FunctionType::get(builder.getInt32Ty(), printf_args, true);

    llvm::FunctionCallee printf_func =
        mod->getOrInsertFunction("printf", printf_type);

    Value* add_lhs = ConstantInt::get(Type::getInt32Ty(ctx), 7);
    Value* add_rhs = ConstantInt::get(Type::getInt32Ty(ctx), 10);
    Value* add = builder.CreateAdd(add_lhs, add_rhs);
    auto args = *new llvm::ArrayRef<Value*>{
        builder.CreateGlobalStringPtr("7 + 10 = %i.\n"), add};

    builder.CreateCall(printf_func, args);
    builder.CreateRetVoid();

    return mod;
}
raw_fd_ostream* createFileStream(const char* filename)
{
    std::error_code EC;
    return new raw_fd_ostream(filename, EC, sys::fs::F_None);
}
int createObjectFileFromModule(Module* mod, raw_fd_ostream* file_stream)
{
    // InitializeAllTargetInfos();
    LLVMInitializeNativeTarget();
    // InitializeAllTargetMCs();
    InitializeNativeTargetAsmParser();
    InitializeNativeTargetAsmPrinter();
    auto target_triple = LLVMGetDefaultTargetTriple();
    std::string err;
    auto target = TargetRegistry::lookupTarget(target_triple, err);
    if (err.length() > 0) {
        errs() << err << "\n";
        return 1;
    }
    TargetOptions opt;
    auto tm = target->createTargetMachine(
        target_triple, LLVMGetHostCPUName(), LLVMGetHostCPUFeatures(), opt,
        Optional<Reloc::Model>());
    mod->setDataLayout(tm->createDataLayout());
    // Output the bitcode file to std   out
    legacy::PassManager pass;
    auto FileType = TargetMachine::CGFT_ObjectFile;

    if (tm->addPassesToEmitFile(pass, *file_stream, nullptr, FileType)) {
        errs() << "TheTargetMachine can't emit a file of this type\n";
        return 1;
    }
    pass.run(*mod);
    file_stream->flush();
    return 0;
}
int link_obj_to_executable(const char* obj_path, const char* exe_path)
{
    const char* args[] = {"", // argv[0] -> programm location, ignored
                          "--dynamic-linker",
                          "/lib64/ld-linux-x86-64.so.2",
                          "/usr/lib/x86_64-linux-gnu/crt1.o",
                          "/usr/lib/x86_64-linux-gnu/crti.o",
                          "/usr/lib/x86_64-linux-gnu/crtn.o",
                          "/lib/x86_64-linux-gnu/libc.so.6",
                          "/usr/lib/x86_64-linux-gnu/libc_nonshared.a",
                          obj_path,
                          "-o",
                          exe_path};
    size_t arg_count = sizeof(args) / sizeof(char*);
    ArrayRef<const char*> array_ref_args(args, arg_count);
    return lld::elf::link(array_ref_args, false);
}
int delete_file(const char* filepath)
{
    unlink(filepath);
}
extern "C" {
int llvmtest_main()
{
    const char* obj = "./temp/foo.obj";
    const char* exe = "./temp/foo";
    LLVMContext ctx;
    Module* mod = createTestModule(ctx);
    raw_fd_ostream* stream = createFileStream(obj);
    createObjectFileFromModule(mod, stream);
    delete mod;
    delete stream;

    link_obj_to_executable(obj, exe);
    delete_file(obj);
    system(exe);
    return 0;
}
}
