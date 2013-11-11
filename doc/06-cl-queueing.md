---
layout : default
packages :
 - CL.Command_Queues
 - CL.Queueing
 - CL.Events
permalink : cl-queueing.html
---

# The Command Queue

## The package `CL.Command_Queues`

To execute your kernels, you have to enqueue them in a command queue. You can
create a command queue for the target device by providing a context created for
that device. With this command queue, you can execute all kernels an access all
memory object linked to this context.

## The package `CL.Queueing`

Enqueueing tasks can be done with the functions in `CL.Queueing` and its child
package `CL.Queueing.Memory_Objects`. The base package can execute a kernel in
data-parallel mode (as kernel) or only once (as task). The child package can
enqueue actions to transfer memory objects from and to OpenCL memory.

## The package `CL.Events`

For synchronization between kernels and tasks executed by the command queue, you
can use the `Event` type. Each instance of these type signals whether the action
it is linked to has already been finished.