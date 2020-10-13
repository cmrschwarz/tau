#include "plattform.h"
#include "stdio.h"
#include "stdlib.h"
#include <assert.h>
#if HOST_OS_LINUX
#   include <signal.h>
#elif HOST_OS_WINDOWS
#   include "plattform/windows/sane_windows.h"
#endif
void panic(const char* message)
{
    // exit with an error to indicate something went horribly wrong
    // this is only called if we have no idea what else to do, hence the name
    fputs("fatal error: ", stderr);
    fputs(message, stderr);
    fputc('\n', stderr);
    fflush(stderr);
    fflush(stdout);
    assert(false);
    exit(EXIT_FAILURE);
}

void debugbreak()
{
#   if HOST_OS_LINUX
    raise(SIGTRAP);
#   elif HOST_OS_WINDOWS
    DebugBreak();
#   else
#   error tauc has no implementation for debugbreak on the current plattform
#   endif   
}
