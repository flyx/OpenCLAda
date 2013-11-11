---
layout    : default
title     : Home
permalink : index.html
---

## About

*OpenCLAda* is a thick wrapper for the [OpenCL Host API][1]. It enables you to
write OpenCL hosts in Ada. It does *not* enable you to write OpenCL kernels in
Ada. It currently only supports OpenCL 1.0 and the `cl_gl` extension. Support
for newer versions and more extensions may be added in the future.

For information on how to setup and use OpenCLAda, visit the
[documentation section][2]. Please note that this documentation is just an
addendum to [the official OpenCL reference][6] and only explains how OpenCLAda
wraps the features OpenCL provides.

## Getting the Code

You can get the code from the [public GitHub repository][3]. Releases are
available as [tags][4] of the repository.

## License

OpenCLAda is distributed under the terms of the [ISC License][4].


 [1]: http://www.khronos.org/opencl/
 [2]: {{ site.baseurl }}/doc/index.html
 [3]: https://github.com/flyx/OpenCLAda
 [4]: https://github.com/flyx/OpenCLAda/releases
 [5]: http://opensource.org/licenses/ISC
 [6]: http://www.khronos.org/registry/cl/sdk/1.1/docs/man/xhtml/