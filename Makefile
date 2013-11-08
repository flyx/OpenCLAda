ifndef PREFIX
  PREFIX=$(dir $(shell dirname `which gnatls`))
endif
LIBDIR ?= ${PREFIX}/lib
DESTDIR ?=
GNATFLAGS ?=
ADA_PROJECT_DIR ?= ${PREFIX}/lib/gnat
GPRBUILD = gprbuild ${GNATFLAGS} -p

GL_BACKEND := windows
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  GL_BACKEND := quartz
endif
ifeq ($(UNAME), Linux)
  GL_BACKEND := x11
endif

all: compile

compile:
	mkdir -p lib
	mkdir -p obj
	${GPRBUILD} -P opencl-cl_gl.gpr -XWindowing_System=${GL_BACKEND}

clean:
	rm -rf ./obj ./bin ./lib

tests:
	mkdir -p bin
	${GPRBUILD} -P opencl_test.gpr -XWindowing_System=${GL_BACKEND} -XGLFW_Version=2

.PHONY: tests
