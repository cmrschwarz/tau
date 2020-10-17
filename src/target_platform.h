// clang-format off
// target lists completely stolen from llvm's
// http://llvm.org/doxygen/Triple_8h_source.html

typedef enum arch_kind_e { 			   
    ARCH_UNKNOWN,   
    ARCH_ARM, 			 // arm (little endian): arm, armv.*, xscale
    ARCH_ARMEB,          // arm (big endian): armeb
    ARCH_AARCH64,        // aarch64 (little endian): aarch64
    ARCH_AARCH64_BE, 	 // aarch64 (big endian): aarch64_be
    ARCH_AARCH64_32, 	 // aarch64 (little endian) ILP32: aarch64_32
    ARCH_ARC, 			 // arc: synopsys arc
    ARCH_AVR, 			 // avr: atmel avr microController
    ARCH_BPFEL, 		 // ebpf or extended bpf or 64-bit BPF (little endian)
    ARCH_BPFEB, 		 // ebpf or extended bpf or 64-bit BPF (big endian)
    ARCH_HEXAGON, 		 // hexagon: hexagon
    ARCH_MIPS, 			 // mips: mips, mipsallegrex, mipsr6
    ARCH_MIPSEL, 		 // mipsel: mipsel, mipsallegrexe, mipsr6el
    ARCH_MIPS64, 		 // mips64: mips64, mips64r6, mipsn32, mipsn32r6
    ARCH_MIPS64EL, 		 // mips64el: mips64el, mips64r6el, mipsn32el, mipsn32r6el
    ARCH_MSP430, 		 // msp430: msp430
    ARCH_PPC, 			 // ppc: powerpc
    ARCH_PPC64, 		 // ppc64: powerpc64, ppu
    ARCH_PPC64LE, 		 // ppc64le: powerpc64le
    ARCH_R600, 			 // r600: amd gpus hd2xxX - HD6XXX
    ARCH_AMDGCN, 		 // amdgcn: amd gcn gpus
    ARCH_RISCV32, 		 // risc-v (32-bit): riscv32
    ARCH_RISCV64, 		 // risc-v (64-bit): riscv64
    ARCH_SPARC, 		 // sparc: sparc
    ARCH_SPARCV9, 		 // sparcv9: sparcv9
    ARCH_SPARCEL, 		 // sparc: (endianness = little). NB: 'Sparcle' is a CPU variant
    ARCH_SYSTEMZ, 		 // systemz: s390x
    ARCH_TCE, 			 // tce (http://tce.cs.tUt.fi/): tce
    ARCH_TCELE, 		 // tce little endian (http://tce.cs.tut.fi/): tcele
    ARCH_THUMB, 		 // thumb (little endian): thumb, thumbv.*
    ARCH_THUMBEB, 		 // thumb (big endian): thumbeb
    ARCH_X86, 			 // x86: i[3-9]86
    ARCH_X86_64, 		 // x86-64: amd64, x86_64
    ARCH_XCORE, 		 // xcore: xcore
    ARCH_NVPTX, 		 // nvptx: 32-bit
    ARCH_NVPTX64, 		 // nvptx: 64-bit
    ARCH_LE32, 			 // le32: generic little-endian 32-bit CPU (PNaCl)
    ARCH_LE64, 			 // le64: generic little-endian 64-bit CPU (PNaCl)
    ARCH_AMDIL, 		 // amdil
    ARCH_AMDIL64, 		 // amdil with 64-bit pointers
    ARCH_HSAIL, 		 // amd hsail
    ARCH_HSAIL64, 		 // amd hsail with 64-bit pointers
    ARCH_SPIR, 			 // spir: standard portable IR for OpenCL 32-bit version
    ARCH_SPIR64, 		 // spir: standard portable IR for OpenCL 64-bit version
    ARCH_KALIMBA, 		 // kalimba: generic kalimba
    ARCH_SHAVE, 		 // shave: movidius vector VLIW processors
    ARCH_LANAI, 		 // lanai: lanai 32-bit
    ARCH_WASM32, 		 // webassembly with 32-bit pointers
    ARCH_WASM64, 		 // webassembly with 64-bit pointers
    ARCH_RENDERSCRIPT32, // 32-bit renderscript
    ARCH_RENDERSCRIPT64, // 64-bit renderscript
    ARCH_LAST_ARCH = ARCH_RENDERSCRIPT64
}arch_kind;

typedef enum os_kind_e{
    OS_UNKNOWN,
 
    OS_ANANAS,
    OS_CLOUDABI,
    OS_DARWIN,
    OS_DRAGONFLY,
    OS_FREEBSD,
    OS_FUCHSIA,
    OS_IOS,
    OS_KFREEBSD,
    OS_LINUX,
    OS_LV2,           // PS3
    OS_MACOSX,
    OS_NETBSD,
    OS_OPENBSD,
    OS_SOLARIS,
    OS_WIN32,
    OS_HAIKU,
    OS_MINIX,
    OS_RTEMS,
    OS_NACL,           // Native Client
    OS_CNK,            // BG/P Compute-Node Kernel
    OS_AIX,
    OS_CUDA,           // NVIDIA CUDA
    OS_NVCL,           // NVIDIA OpenCL
    OS_AMDHSA,         // AMD HSA Runtime
    OS_PS4,
    OS_ELFIAMCU,
    OS_TVOS,           // Apple tvOS
    OS_WATCHOS,        // Apple watchOS
    OS_MESA3D,
    OS_CONTIKI,
    OS_AMDPAL,         // AMD PAL Runtime
    OS_HERMITCORE,     // HermitCore Unikernel/Multikernel
    OS_HURD,           // GNU/Hurd
    OS_WASI,           // Experimental WebAssembly OS
    OS_EMSCRIPTEN,

    OS_LAST_OS = OS_EMSCRIPTEN
}os_kind;

typedef enum object_format_kind_e {
    OBJECT_FORMAT_UNKNOWN,

    OBJECT_FORMAT_COFF,
    OBJECT_FORMAT_ELF,
    OBJECT_FORMAT_MACHO,
    OBJECT_FORMAT_WASM,
    OBJECT_FORMAT_XCOFF,
} object_format_kind;

// clang-format on

typedef struct target_platform_s {
    arch_kind arch;
    os_kind os;
    object_format_kind object_format;
} target_platform;

arch_kind parse_arch_kind(char* str);
os_kind parse_os_kind(char* str);
object_format_kind parse_object_format_kind(char* str);

void target_platform_get_host(target_platform* tp);

void target_platform_set_unknown(target_platform* tp);
void target_platform_fill_gaps(
    target_platform* target, target_platform* source);
char* target_plattform_get_default_output_path(target_platform* tgt);
