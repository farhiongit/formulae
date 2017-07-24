#ifndef LDWRAPPER_H
#define LDWRAPPER_H
#pragma once

#ifdef WIN32

#include <direct.h>
#include <windows.h>

#define LIBHANDLER HINSTANCE
#define LIBOPEN(str) LoadLibrary(TEXT((str)));
#define LIBCHECK(hdler) (hdler)
#define LIBLOAD(hdler, func) GetProcAddress((hdler), (func))

#else

#include <sys/types.h>
#include <dlfcn.h>

#define LIBHANDLER void*
#define LIBOPEN(str) ldopen((str), RTLD_LAZY)
#define LIBCHECK(hdler) (hdler)
#define LIBLOAD(hdler, func) GetProcAddress((hdler), (func))

#endif


#endif
