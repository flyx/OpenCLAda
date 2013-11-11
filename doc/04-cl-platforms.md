---
layout : default
packages :
 - CL.Platforms
permalink : cl-platforms.html
---

# The package `CL.Platforms`

This package contains all functionality to query the available OpenCL platforms,
their devices and capabilities. The two main types are `Platform` and `Device`.
A typical usage pattern would be:

<?prettify lang=ada?>

    declare
       -- Just take the first available platform. If there are multiple
       -- platforms available, you should choose between them.
       My_Platform : CL.Platforms.Platform := CL.Platforms.List (1);
       
       GPU_Devices : My_Platform.Devices
         (CL.Platforms.Device_Kind'(GPU => True, others => False) );
    begin
       -- do something
    end;

You can query the supported OpenCL version and extensions from the platform;
each device has a vast list of attributes which can be queried by the getter
functions provided in this package.