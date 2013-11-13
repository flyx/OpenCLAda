---
layout    : default
title     : API Documentation
permalink : index.html
---

# API Documentation

## Overview

The OpenCL host API has, although it is provided in plain C, already a decent
object-oriented underlying data model. OpenCLAda wraps this object-oriented
interface in tagged types. Some significant differences to the plain C API are:

 * Automatic reference counting. The C API provides a reference-counting
   mechanism, which is leveraged by using controlled types. This means that you
   don't need to worry about explicitly freeing the OpenCL memory object you
   create; this is done automatically when your references to the memory go out
   of scope.
 * OpenCL uses lots of enumerations to access data fields of an object.
   OpenCLAda provides one getter function per data field, which leads to better
   code readability.

## More on Objects and Reference Counting

All objects that can be created during runtime have construction function in a
nested `Constructors` package. The purpose of this package is mainly to prevent
the construction functions being primitive operations on the types. The object
types are not meant to be derived by the user.

All object types are references to actual OpenCL objects. When you assign the
value of one object variable to another, you do not copy the object, but merely
a reference - which will automatically increase the reference count. Every time
a variable referencing an object goes out of scope, the reference count of the
object is decreased, and if it reaches zero, the object will be released as soon
as it's not used internally anymore.

## Data Types and Transfer

Usually, your OpenCL kernels will process some kind of data. You have to transfer
this data to OpenCL memory in advance, and retrieve the result afterwards. To be
sure that your data is correctly formatted in memory, use only the data types
defined by OpenCLAda as base types for memory transfer (see the packages [CL][1]
and [CL.Vectors][2] for details).

Usually, you want to declare array types to transfer chunks of data. OpenCLAda's
data transfer functions are generic which makes it possible to use them with any
array type. Apart from using the basic OpenCL types as base type, you should make
sure to use `pragma Convention (C, Array_Type)` on your array type. For details
see the package [CL.Memory][3].

 [1]: {{ site.baseurl }}/doc/cl.html
 [2]: {{ site.baseurl }}/doc/cl-vectors.html
 [3]: {{ site.baseurl }}/doc/cl-memory.html