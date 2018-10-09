PROJECT := tauc
#CFLAGS := -g -Wall -O0 #DEBUG
CFLAGS:= -O3 -Wall   #RELEASE
SRCDIR := src
BINDIR := bin
OBJDIR := $(BINDIR)/obj
DEPSFILE := $(BINDIR)/dependencies
EXECFILE := $(BINDIR)/$(PROJECT)

SHELL := /bin/sh
ROOT := $(shell pwd)
SRCS := $(shell find $(SRCDIR) -name "*.c")
OBJS := $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%.o, $(SRCS))

all: $(PROJECT)
$(PROJECT): $(DEPSFILE) $(OBJS)
	@cd $(BINDIR)
	@gcc $(CFLAGS) $(LDFLAGS) -pthread -o $(EXECFILE) $(OBJS)

run: $(PROJECT)
	@$(EXECFILE)

$(DEPSFILE): 
	@mkdir -p $(OBJDIR)
	@find $(SRCDIR) -name "*.c" | sed -e 's|^$(SRCDIR)\(.*\)\.c|\1\t\1|' \
		-e 's|\(.*\)\t\(.*\)\/\(.*\)|\
		gcc -MT $(OBJDIR)\2/\3.o -MM $(SRCDIR)\1.c \
		printf "\\t@mkdir -p $(OBJDIR)\2 \&\& cd $(OBJDIR)\2 \&\& "\
		printf "gcc $(CFLAGS) -c $(ROOT)/$(SRCDIR)\1.c\\n"|'\
		|sh > $(DEPSFILE)

clean:
	rm -rf $(BINDIR)


ifneq (clean, $(MAKECMDGOALS))
-include $(DEPSFILE)
endif
