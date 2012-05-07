ifndef PREFIX
  PREFIX=$(dir $(shell dirname `which gnatls`))
endif
LIBDIR ?= ${PREFIX}/lib
DESTDIR ?=
GNATFLAGS ?=
ADA_PROJECT_DIR ?= ${PREFIX}/lib/gnat
GPRBUILD = gprbuild ${GNATFLAGS} -p

OS := Windows
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  OS := MacOSX
endif
ifeq ($(UNAME), Linux)
  OS := Linux
endif

compile:
	mkdir -p lib
	mkdir -p obj
	${GPRBUILD} -P openclada.gpr -XOS=${OS}

uninstall:
	rm -rf ${DESTDIR}/${PREFIX}/include/openclada ${DESTDIR}/${LIBDIR}/openclada ${DESTDIR}/${ADA_PROJECT_DIR}/openclada.gpr

install: compile uninstall
	mkdir -p ${DESTDIR}/${PREFIX}/include/openclada
	mkdir -p ${DESTDIR}/${LIBDIR}/openclada
	mkdir -p ${DESTDIR}/${ADA_PROJECT_DIR}

	cp -r lib/* ${DESTDIR}/${LIBDIR}/openclada

	cp -f src/cl.ad* ${DESTDIR}/${PREFIX}/include/openclada
	cp -f src/cl-*.ad* ${DESTDIR}/${PREFIX}/include/openclada
	chmod -w ${DESTDIR}/${PREFIX}/include/openclada/*.ad?
	cp openclada.gpr ${DESTDIR}/${ADA_PROJECT_DIR}
all: compile

clean:
	rm -rf ./obj ./bin ./lib

tests: compile
	mkdir -p bin
	${GPRBUILD} -P openclada_tests.gpr -XOS=${OS}

.PHONY: tests
