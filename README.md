# OpenCLAda - An Ada binding for the OpenCL host API

## About
This is OpenCLAda, a thick Ada binding for the OpenCL host API.
This binding enables you to write OpenCL hosts in Ada. It does **not**
enable you to write OpenCL kernels in Ada.

## Prerequisites

OpenCLAda currently supports MacOSX, Linux and Windows. You need to have
 - a GNAT compiler
 - an OpenCL implementation
available on your system. GNAT GPL Edition is available at
[the AdaCore website](http://libre.adacore.com/libre/download/). OpenCL is
usually available from your hardware vendor. On MacOSX, it's already part of
the operating system. On Windows, you'll need an `OpenCL.lib` file to link against.
This is usually not part of the OpenCL implementation, but can be acquired as
part of an SDK from your hardware vendor (eg the
[AMD APP SDK](http://developer.amd.com/SDKS/AMDAPPSDK/Pages/default.aspx)).

## Compilation

Under MacOSX and Linux, open a terminal,
navigate to the OpenCLAda directory and do:

	$ make

On Windows, it could work the same way if you're using MinGW or Cygwin.
However, I didn't either one. Anyway, to compile without make, just do

	$ gnatmake -P openclada.gpr -XOS=Windows

The compiler needs to find the `OpenCL.lib` file mentioned above. If you're
unsure how to achieve this, just copy it into `C:\GNAT\2011\lib` or wherever
you installed your GNAT compiler.

*Note: As this will build a static library, the availability of an OpenCL
implementation will not be tested when building OpenCLAda. So if you want to
make sure that OpenCL is available, build the tests and see if they are linked
properly (see below).*

## Installation

On MacOSX, Linux and MinGW / Cygwin, you can install the library with:

	$ make install

On Windows, there is no standard way to install the library; probably the
best thing you can do is just including OpenCLAda in your project.

## Tests

OpenCLAda comes with some tests (or rather, examples). I wrote them to test
some of the basic functionality of the API. You can build them with

	$ make tests

or

	$ gnatmake -P openclada_tests.gpr -XOS=Windows
	
on Windows. A basic "hello world" example is also included. After compilation,
the executables will be located in the `bin` directory.

## Usage

There is some overview over the OpenCLAda API on the Wiki (TBD). For more
information, please consult the
[Khronos OpenCL API Registry](http://www.khronos.org/registry/cl/).

## Contributing

As this is my first real Ada project, there might be some flaws in the code.
You're welcome to contribute code or file bug reports on the
[project's page on GitHub](https://github.com/flyx86/openclada).

## License

This code is distributed under the terms of the
Simplified BSD License, which you can find in the file
`LICENSE`.
