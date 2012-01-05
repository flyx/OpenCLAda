---
layout: default
title: Ada Binding Design Guidelines
---

# Ada Binding Design Guidelines

## 1 Introduction

### 1.1 Abstract

This is a guide for building thick wrappers of C libraries for Ada. I developed these guidelines
while building OpenCLAda and plan to use them in future project. The goal of this guide is to
help developers write consistent and easy-to-use wrappers for C libraries.

There are two kinds of information in this guide. First, there is advice on how to deal with
certain patterns that are usually found in C libraries, and more generally, with the common
constructs in a C header. Second, there is information on what works and what doesn't work when
calling C libraries from Ada. If you do not have experience with writing a wrapper, this guide
may help you avoid some mistakes.

This guide assumes that you are using Ada 2005. Guidelines and code examples adhere the
[Ada Quality and Style Guide](http://en.wikibooks.org/wiki/Ada_Style_Guide). Ada's object
oriented features are used where they seem appropriate. The reader should have basic knowledge
of the handling of tagged types and the Ada language in general.

If you want to give feedback for or contribute to this guide, feel free to contact me. I usually
hang around in the `#ada` channel on [Freenode](http://freenode.net/irc_servers.shtml) under the
nick `flyx`.

### 1.2 Why should I write a thick wrapper?

This is a very important question. The easy alternative to writing a thick wrapper is - obviously -
writing a thin one. This can be done automatically by SWIG, the Simplified Wrapper and Interface
Generator. Ada support for SWIG is available [here](http://www.auroraux.org/index.php/Swig).

There are several benefits of having a thin instead of a thick wrapper:

 * You do not need to provide any documentation. Using the wrapper works exactly the same way as
   using the original library - just in a different programming language. When you write a thick
   wrapper, you have to provide some documentation yourself.
 * As mentioned, a thin wrapper can be created automatically. Writing a thick wrapper takes much
   more time and is more error-prone.
 * When you're writing a thick wrapper, you have to design an interface yourself. Usually, when
   you're writing a thick wrapper, you want it to be used by other developers, especially when
   you're doing open source software. If you design your interface poorly, no-one will use your
   work, so you really should put some work in designing the interface.

The last point is the main reason why this guide exists - it wants to help you create a decent
interface for your thick wrapper. Now let's have a look at the merits of having a thick wrapper:

 * A thin wrapper is not very adaish. It may have some type safety, but only to the level that
   is supported by C. All Ada features that are absent in C can obviously not be used in a
   thin wrapper. This makes working with the wrapper quite a pain and usually leads to the
   developer writing some own glue code to make working with the wrapper easier. This glue code
   could as well be integrated to the wrapper, making it a thick one.
 * You can provide additional functionality in a thick wrapper that is absent in the original
   library. Whether this makes sense depends on the library you are wrapping.

Especially the first point in this list is of importance. A poor interface is a sin in software
engineering and should be avoided as soon as you plan to use the library in some real project
that goes beyond some 100 lines of code. Of course, there may be times when a thick wrapper
simply is not worth the effort. The decision is yours.

## 2 Basics

### 2.1 Getting started

The first thing you should do when starting to write your wrapper is to get to know how the
library works. You are not just translating code (as you would for a thin wrapper), so you
have to know the architecture and purpose of all the parts of the library.

I suggest to have a thin wrapper within your thick wrapper that holds all the imported
functions from the C library. This does not need to be exposed, so I usually put it into
a private package that is only visible to the wrapper, like this one:

{% highlight ada %}
private package CL.API is
   
   -- here be imported functions
   
end CL.API;
{% endhighlight %}

### 2.2 Primitive and enumeration types

The primitive types provided by the C language are available in the Ada standard library under
`Interfaces.C`. If there are `typedef`s in the C header that use those types, create appropriate
types or subtypes in the Ada wrapper.

A special case are boolean values: As C does not provide a boolean type, most C headers define
constants for `FALSE` and `TRUE` mapping to 0 and 1 respectively. Then, a numeric type is used
in cases where a boolean has to be provided or is returned. You can wrap this pseudo-boolean as
follows:

{% highlight ada %}
type Bool is new Boolean;
for Bool use (False => 0, True => 1);
for Bool'Size use Interfaces.C.int'Size;
{% endhighlight %}

In this case, the C type `int` is used for passing boolean values. You should not expose this
type in your wrapper. Instead, just case any values from the standard Ada `Boolean` to this
`Bool` type and back when calling the C functions.

What works for booleans also works for all other enumeration types. In most C libraries,
enumerations are defined by using a numeric type like `int` for transportation and constant
values defined via `#define` for the possible values. It may look like this:

{% highlight c %}
/* Comment that states what this "enumeration" is used for */
#define VALUE1   0x0001
#define VALUE2   0x0002
#define VALUE3   0x0003
#define VALUE4   0x0010
#define VALUE5   0x0020
{% endhighlight %}

This can be translated into Ada like this:

{% highlight ada %}
type Some_Type is (Value1, Value2, Value3, Value4, Value5);
for Some_Type use (Value1 => 16#0001#,
                   Value2 => 16#0002#,
                   Value3 => 16#0003#,
                   Value4 => 16#0010#,
                   Value5 => 16#0020#);
for Some_Type'Size use Interfaces.C.int'Size;
{% endhighlight %}

As before, the transporting type may vary.

### 2.3 Structs

Structs in C are like records in Ada, so this should be easy. Right? Wrong.

The main problem here is that in Ada, record types are passed by reference. In C, structs are passed
by value. So, when you define a record that should be passed to a C function, you have to tell the
Ada compiler that you want it passed by value:

{% highlight ada %}
type Point is record
   X : Interfaces.C.int;
   Y : Interfaces.C.int;
end record;
pragma Convention (C_Pass_By_Copy, Point);
{% endhighlight %}

Of course, this isn't needed in the case that a C function expects a *pointer* to a struct as
parameter. Instead of defining the parameter passing convention for the record, you can also
define it when importing the C function (see below). If you don't want to define the Ada record
to be passed by value, you can use `pragma Convention (C, Point);` instead. This ensures that
the layout of the record corresponds with what the C function expects.

### 2.4 Pointers

To be continued...