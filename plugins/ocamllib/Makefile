
PATH := $(PATH):/home/harvey/links/deriving

define PROJ_ocaml_list.cmxs
OCAMLLDFLAGS = -shared
RESULT = ocaml_list.cmxs
SOURCES = ocaml_list.ml
INCDIRS = $(EXTRA_INCDIRS) /home/harvey/dynlinx/links
LIBDIRS = $(EXTRA_LIBDIRS) /home/harvey/dynlinx/links
endef
export PROJ_ocaml_list.cmxs

define PROJ_ocaml_random.cmxs
OCAMLLDFLAGS = -shared
RESULT = ocaml_random.cmxs
SOURCES = ocaml_random.ml
INCDIRS = $(EXTRA_INCDIRS) /home/harvey/dynlinx/links
LIBDIRS = $(EXTRA_LIBDIRS) /home/harvey/dynlinx/links
endef
export PROJ_ocaml_random.cmxs

define PROJ_ocaml_string.cmxs
OCAMLLDFLAGS = -shared
RESULT = ocaml_string.cmxs
SOURCES = ocaml_string.ml
INCDIRS = $(EXTRA_INCDIRS) /home/harvey/dynlinx/links
LIBDIRS = $(EXTRA_LIBDIRS) /home/harvey/dynlinx/links
endef
export PROJ_ocaml_string.cmxs

define PROJ_ocaml_char.cmxs
OCAMLLDFLAGS = -shared
RESULT = ocaml_char.cmxs
SOURCES = ocaml_char.ml
INCDIRS = $(EXTRA_INCDIRS) /home/harvey/dynlinx/links
LIBDIRS = $(EXTRA_LIBDIRS) /home/harvey/dynlinx/links
endef
export PROJ_ocaml_char.cmxs

define PROJ_ocaml_complex.cmxs
OCAMLLDFLAGS = -shared -linkall
RESULT = ocaml_complex.cmxs
SOURCES = ocaml_complex.ml
INCDIRS = $(EXTRA_INCDIRS) /home/harvey/dynlinx/links
LIBDIRS = $(EXTRA_LIBDIRS) /home/harvey/dynlinx/links
endef
export PROJ_ocaml_complex.cmxs

define PROJ_ocaml_queue.cmxs
OCAMLLDFLAGS = -shared
RESULT = ocaml_queue.cmxs
SOURCES = ocaml_queue.ml
INCDIRS = $(EXTRA_INCDIRS) /home/harvey/dynlinx/links
LIBDIRS = $(EXTRA_LIBDIRS) /home/harvey/dynlinx/links
endef
export PROJ_ocaml_queue.cmxs

define PROJ_ocaml_stack.cmxs
OCAMLLDFLAGS = -shared
RESULT = ocaml_stack.cmxs
SOURCES = ocaml_stack.ml
INCDIRS = $(EXTRA_INCDIRS) /home/harvey/dynlinx/links
LIBDIRS = $(EXTRA_LIBDIRS) /home/harvey/dynlinx/links
endef
export PROJ_ocaml_stack.cmxs

define PROJ_ocaml_hashtbl.cmxs
OCAMLLDFLAGS = -shared
RESULT = ocaml_hashtbl.cmxs
SOURCES = ocaml_hashtbl.ml
INCDIRS = $(EXTRA_INCDIRS) /home/harvey/dynlinx/links
LIBDIRS = $(EXTRA_LIBDIRS) /home/harvey/dynlinx/links
endef
export PROJ_ocaml_hashtbl.cmxs

ifndef SUBPROJS
  export SUBPROJS = ocaml_list.cmxs ocaml_random.cmxs 			\
  		ocaml_string.cmxs ocaml_complex.cmxs ocaml_char.cmxs	\
  		ocaml_queue.cmxs ocaml_stack.cmxs ocaml_hashtbl.cmxs
endif

all: nc

OCAMLMAKEFILE = ./OCamlMakefile
export $(OCAMLMAKEFILE)

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@