Level 06
===

In this tutorial, we are going to use gdb to debug a Linux process while
exploiting it with PoE. We leave other platforms and debuggers as an exercise
to the reader.

### Attaching to the target

We need to find the id of the process we want to debug, and then attach gdb to
it.

To find the id, we can use `pid` function in PoE:

```
 act debug_act():
 dbgascii itoa(pid())
 pause()

submit:
 return debug_act()
 ```

After running the above PoE script, you will see the process id printed in the
output. And then, you can use that pid to attach gdb to the process:

```
 gdb
 (gdb) attach 1234
 Attaching to process 1234
 (gdb) info functions
0x0000000100000000  _mh_execute_header
 0x0000000100003e70  vuln
0x0000000100003f00  main
 ...
 ```

where `1234` is the process id you obtained from the PoE script.

### Choosing your breakpoints

However, just attaching gdb to the process is not enough, as our PoE script
will continue to run and leave us no time to set breakpoints.

Therefore, we need to use `pause` function in the PoE script at some point, so
that we have time to set breakpoints in gdb. A good place to pause the PoE
script is right after getting the pid, as shown in the above PoE script.

### Example

Here is an illustration of the process of attaching a debugger to the process to
debug:

![Example of debugging using PoE to exploit a program with pwndbg on the
right](debugging_example.png "PoE process debugging")