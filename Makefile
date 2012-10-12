ifndef PREFIX
  PREFIX=$(dir $(shell dirname `which gnatls`))
endif
LIBDIR ?= ${PREFIX}/lib
DESTDIR ?=
GNATFLAGS ?=
ADA_PROJECT_DIR ?= ${PREFIX}/lib/gnat
GPRBUILD = gprbuild ${GNATFLAGS} -p

GL_BACKEND := Windows
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  GL_BACKEND := MacOSX
endif
ifeq ($(UNAME), Linux)
  GL_BACKEND := Linux
endif

compile:
	mkdir -p lib
	mkdir -p obj
	${GPRBUILD} -P opencl.gpr -XGL_Backend=${GL_BACKEND} -XCL_GL=Yes

uninstall:
	rm -rf ${DESTDIR}/${PREFIX}/include/openclada ${DESTDIR}/${LIBDIR}/openclada ${DESTDIR}/${ADA_PROJECT_DIR}/openclada*.gpr

install: compile uninstall
	mkdir -p ${DESTDIR}/${PREFIX}/include/openclada
	mkdir -p ${DESTDIR}/${LIBDIR}/openclada
	mkdir -p ${DESTDIR}/${ADA_PROJECT_DIR}

	cp -r lib/* ${DESTDIR}/${LIBDIR}/openclada

	cp -f src/cl.ad* ${DESTDIR}/${PREFIX}/include/openclada
	cp -f src/cl-*.ad* ${DESTDIR}/${PREFIX}/include/openclada
	chmod -w ${DESTDIR}/${PREFIX}/include/openclada/*.ad?
	cp openclada.gpr ${DESTDIR}/${ADA_PROJECT_DIR}
	cp openclada-cl_gl.gpr ${DESTDIR}/${ADA_PROJECT_DIR}
all: compile

clean:
	rm -rf ./obj ./bin ./lib

tests:
	mkdir -p bin
	${GPRBUILD} -P opencl.gpr -XOS=${OS} -XTests=Yes

.PHONY: tests
