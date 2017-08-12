#CC				= gcc
CC				= clang -ferror-limit=5
#CC				= tcc
#CC				= pcc -g -O -I/usr/local/include -I/usr/include/linux -I/usr/local/include/c++/4.4.2 -I/usr/local/include/c++/4.4.2/* -D_DEBUG -Wl,-r/usr/local/lib 
WARNINGS	= -Wall -pedantic -Werror -pedantic-errors
CSTD      = #-std=c99
#COMPILE		= -pipe -O3 -fPIC
DEBUG		 = -g
#PROC_OPT        = -march=i686
LD_OPT		= -s
CFLAGS  = $(TEMP) $(DEBUG) $(WARNINGS) $(COMPILE) $(PROC_OPT) $(CSTD) -I../tools -I../dates -I/usr/share/gnulib/lib
LDFLAGS  = -L../dates -static

all: formulae dl_test.so.1.0

debug: formulae
	nemiver formulae &

formulae: getopts.o formulae.o main.o ../dates/libtm.a
	$(CC) -L../dates -rdynamic formulae.o getopts.o main.o -o formulae -lgsl -lgslcblas -lm -ldl -ltm
# Option -rdynamic used to link the executbale : it passes the flag -export-dynamic to the linker.
# This instructs the linker to add all symbols, not only used ones, to the dynamic symbol table.
# the global symbols in the executable will also be used to resolve references in a dynamically loaded library by this executable.

	nm -Col --extern-only --defined-only ./formulae

formulae.o: formulae.c ../getopts/getopts.h formulae.h
	$(CC) -c $(CFLAGS) -I/usr/include/gsl formulae.c -o formulae.o
	nm -Col --extern-only --defined-only ./formulae.o

getopts.o: ../tools/getopts.h ../tools/getopts.c
	$(CC) -c $(CFLAGS) ../tools/getopts.c -o getopts.o
	nm -Col --extern-only --defined-only ./getopts.o

main.o: main.c ../getopts/getopts.h formulae.h ../dates/dates.h
	$(CC) -c $(CFLAGS) main.c -o main.o
	nm -Col --extern-only --defined-only ./main.o

dl_test.o: dl_test.c formulae.h
	$(CC) $(CFLAGS) -fPIC -c dl_test.c

dl_test.so.1.0: dl_test.o
	$(CC) -shared -Wl,-soname,libdl_test.so.1 -o dl_test.so.1.0 dl_test.o
	nm -Col --extern-only --defined-only ./dl_test.so.1.0
