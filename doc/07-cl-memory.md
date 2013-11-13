---
layout : default
packages :
 - CL.Memory
 - CL.Memory.Buffers
 - CL.Memory.Images
permalink : cl-memory.html
---

# Memory Objects

## Types

There are currently two types of memory objects: Buffers and images. Buffers
contain generic data, while images contain image data. They differ primarily in
how you access them in OpenCL C.  The type hierarchy looks as follows:

            Memory_Object
             ^         ^
             │         │
    Buffer───┘        Image
                      ^   ^
                      │   │
                 Image2D Image3D

(*TODO*: Learn how to write SVG and make this pretty.)

`Memory_Object` and `Image` are abstract types and cannot be instantiated.

## Loading and Retrieving Data

You can access the data of memory objects with the subroutines defined in
`CL.Queueing.Memory_Objects`. This is a generic package and is to be
instantiated with the base element type and the element array type which contains
the data on the Ada side. When you define an array type you want to use with
this package, use

<?prettify lang=ada?>

    pragma Convention (C, Your_Array_Type);

on it.