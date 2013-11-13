---
layout : default
packages :
 - CL_GL
permalink : cl_gl.html
---

# The `CL_GL` extension

This extension can be used by referencing the the project file
`opencl-cl_gl.gpr`. It enables you to share texture and renderbuffer objects
from OpenGL with OpenCL. It depends on [OpenGLAda][1]. This extension provides
child classes to some of the OpenCL class types to enable the memory object
sharing capabilities.

To create an OpenCL context that allows this, you have to have an active OpenGL
context (in an active window or fullscreen). Then, you can create your context
with

    CL.Contexts.CL_GL.Constructors.Create

This context class does not look so different from the usual context class on its
own; but it is actually quite different on the inside, because it is linked to
the OpenGL context that was active when you created it. You can now create a
specialized command queue with it by means of

    CL.Command_Queues.CL_GL.Constructors.Create

Again, this queue does not look very different from a normal queue. But now, you
can use the image creation functions in

    CL.Memory.Images.CL_GL

These require an OpenGL enabled command queue, which you have just created. You
can now create OpenCL image objects from OpenGL texture and renderbuffer objects.
To manipulate these objects, you have to acquire them from OpenGL and afterwards
release them with

    CL.Queueing.CL_GL.Acquire_GL_Objects
    CL.Queueing.CL_GL.Release_GL_Objects

 [1]: http://flyx.github.io/OpenGLAda