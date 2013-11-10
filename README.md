# OpenCLAda - An Ada binding for the OpenCL host API

## About
This is OpenCLAda, a thick Ada binding for the OpenCL host API.
This binding enables you to write OpenCL hosts in Ada. It does **not**
enable you to write OpenCL kernels in Ada.

## Prerequisites

OpenCLAda currently supports MacOSX, Linux and Windows. You need to have

 - a GNAT compiler
 - an OpenCL implementation
 - optionally OpenGLAda (if you want to use the cl_gl extension)

available on your system. GNAT GPL Edition is available at
[the AdaCore website][7]. OpenCL is
usually available from your hardware vendor. On MacOSX, it's already part of
the operating system. On Windows, you'll need an `OpenCL.lib` file to link
against. This is usually not part of the OpenCL implementation, but can be
acquired as part of an SDK from your hardware vendor (eg the
[AMD APP SDK][6]).

[OpenGLAda][8] is required for OpenCL's
cl_gl extension. Just download its source and make sure the path to `opengl.gpr`
is included in the `ADA_PROJECT_PATH` environment variable. To compile the tests,
you also need the [GLFW library][9] version 2, which is used
for window construction in the cl_gl tests.

## Compilation

On MacOSX and Linux, open a terminal, navigate to the OpenCLAda directory and do:

	$ make

On Windows, it could work the same way if you're using MinGW or Cygwin.
However, I didn't try either one. Anyway, to compile without make, just do

	$ gnatmake -p -Popencl-cl_gl.gpr -XWindowing_System=windows

*Note: The variable __Windowing_System__ is shared with OpenGLAda. You have to
provide it even when compiling without OpenGL support because it defines the way
OpenCLAda links with your system libraries.*

On Windows, the compiler needs to find the `OpenCL.lib` file mentioned above. If
you're unsure how to achieve this, just copy it into `C:\GNAT\[version]\lib` or
wherever you installed your GNAT compiler.

*Note: The availability of an OpenCL implementation will not be tested when
building OpenCLAda. So if you want to make sure that OpenCL is available,
build the tests and see if they are linked properly (see below).*

If you want to build OpenCLAda without the cl_gl extension, do

   $ gprbuild -p -P opencl.gpr -XWindowing_System={windows|quartz|x11}

*Note: The makefile does not support switching off cl_gl, because I'm lazy.*

## Installation

OpenCLAda is just a wrapper library and does not include an installation routine.
You can just add it to your project.

## Tests

OpenCLAda comes with some tests (or rather, examples). I wrote them to test
some of the basic functionality of the API. You can build them with

	$ make tests

or

	$ gnatmake -p -P opencl_test.gpr -XGL_Backend={windows|quartz|x11}
	
A basic "hello world" example is also included. After compilation,
the executables will be located in the `bin` directory. They can only be
executed in the `bin` directory, as they load some OpenCL kernel files through
relative paths.

## Usage

There is some
[overview over the OpenCLAda API on the Wiki][1], which is outdated and will
shortly replaced by a new documentation section on the [homepage][2].
For more information, please consult the [Khronos OpenCL API Registry][3].

## Contributing

You're welcome to contribute code or file bug reports on the
[project's page on GitHub][4].

## License

This code is distributed under the terms of the [ISC License][5], which you can
find in the file `COPYING`.

 [1]: https://github.com/flyx/OpenCLAda/wiki/Overview
 [2]: http://flyx.github.io/OpenCLAda
 [3]: http://www.khronos.org/registry/cl/
 [4]: https://github.com/flyx/OpenCLAda
 [5]: http://opensource.org/licenses/ISC
 [6]: http://developer.amd.com/tools-and-sdks/heterogeneous-computing/amd-accelerated-parallel-processing-app-sdk/
 [7]: http://libre.adacore.com/libre/download/
 [8]: https://flyx.github.io/OpenGLAda
 [9]: http://www.glfw.org/