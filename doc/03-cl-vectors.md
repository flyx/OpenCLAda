---
layout : default
packages :
 - CL.Vectors
permalink : cl-vectors.html
---

# The package `CL.Vectors`

This package defines the vector types available in the OpenCL C programming
language. They are used primarily for data transfer, but element-wise basic
operations `+`, `-`, `*` and `/` are also available. The types suffixed with
`_Vector` are arrays of indefinite length; you can also use them as data
containers for transfer operations.

The actual OpenCL vector types (like `Char2`, `Int4` etc) are records; you can
access their components by using `.S (Index)`. This is necessary because the
types need to be tranferred by value to OpenCL, but the convention
`C_Pass_By_Copy` is only available for record types, not for arrays. This is
also similar to OpenCL C, where you can also access the elements with `.s (i)`.

I tried to make it possible to access the elements with `.X`, `.X` and so on
(like it works in OpenCL C), which would theoretically possible by using
`pragma Unchecked_Union`, but a GNAT bug currently prevents this from working.

You can initialize the fixed vector types with the overloaded function
`CL_Vector`, for instance:

<?prettify lang=ada?>

    My_Int2 : CL.Vectors.Int2 := CL.Vectors.CL_Vector (23, 42);

Array types for the fixed vector types are also available:

<?prettify lang=ada?>

    My_Int2_Array : CL.Vectors.Int2_Array
      := CL.Vectors.New_Array ( (1, 2), (3, 4), (5, 6) );