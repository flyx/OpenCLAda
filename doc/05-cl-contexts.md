---
layout : default
platforms :
 - CL.Contexts
 - CL.Programs
 - CL.Kernels
permalink : cl-contexts.html
---

# Contexts, Programs and Kernels

## The package `CL.Contexts`

The first object you have to create in order to use any OpenCL functionality is
a context, which is a container for the data and kernels you'll work with. You
can create a context for use with one or multiple OpenCL devices.

## The package `CL.Programs`

You compile your OpenCL C code into a `Program` object. The source can either be
passed as string or loaded from a file. After successful compilation, you can
also query the compiled binaries and cache them somewhere so you don't need to
compile them again later. Note that the binaries are platform-dependent. You
have to associate your programs with a context.

You have to explicitly build your program by calling `Build` after loading the
sources. You can query the status of your program with `Status`.

## The package `CL.Kernels`

The entry point(s) of your program are so-called kernels. They can be
data-parallel (executed mulitple times on a set of identical data) and
task-parallel (execution of multiple different kernels at the same time).

After loading a kernel from a `Program` object, you have to set its parameters.
For parameters of scalar or vector OpenCL types, use the generic function
`Set_Kernel_Argument` instantiated with the desired type and argument index
(0-based). If you want to pass an OpenCL object (e.g. a buffer or picture) to
the kernel, use `Set_Kernel_Argument_Object` instead.