Level 02
===

PoE is a language that describes the interaction between a user and a
program. Therefore, your PoE code is useful only when it runs against a certain
program, sends inputs to the program, and receives output messages from the
program.

To understand how PoE works, we will use a simple [program](Program.fs) that
takes in a user input, and returns a flag string as output when the user input
matches the solution string ("I Love PoE"). Thus, your goal is to write a PoE
that can interact with the program and obtain the flag.

You can simply solve this challenge by hand:
```
$ dotnet run
Welcome to the world of PoE!
Type "I Love PoE" to solve this challenge.
> I Love PoE
Good, this is your flag: POE_IS_AWESOME
```

Now let's suppose you want to programatically get the exact flag string, i.e.,
`POE_IS_AWESOME`. Roughly, you will perform the following steps.

1. First, you need to read the console messages until you hit the sring `> `.
2. Next, you write the string `I Love PoE` to the console.
3. You then read until you hit the string `: `.
4. Finally, read the flag until a newline character is observed.

The above steps are encoded with a PoE as follows.
```
act exploit():
    read("> ") // step 1
    bv key = "I Love PoE"
    write(key . 0ahs) // step 2
    read(": ") // step 3
    bv flag = rtrim(read(0ahs)) // step 4 (rtrim to trim the newline)
    return flag

submit:
    return exploit()
```

- The `read` function receives data from the target program until the given
  bitvector value is observed, and returns the received value as a
  bitvector. `read` operates in three different ways depending on the input
  type.

  1. When the input is a positive number `N`, then it tries to receive `N`
     bytes from the target program, and returns the given data as a
     bitvector. In case the program does not produce `N` bytes, it will wait
     until a timeout (5 sec. by default) is reached.

  1. When the input is a negative number, then it tries to receive the whole
     data from the target program until there's no more. It returns a
     bitvector as output.

  1. When the input is a bitvector `V`, then it tries to receive the data
     from the target program until it encounters the `V`. For example,
     `read("abc")` will receive from the target program one byte at a time,
     and see if the received data ends with the string `abc`. If so, it will
     stop receiving more data, and returns the data that it obtained so
     far. This is useful when the target program sequentially sends multiple
     data; you can receive those messages at once.

- The `write` function sends a bitvector value to the target program.

- The `act` keyword defines an "act", which is a function that defines a session
  with the target program/network. That is, any reads and writes in an act will
  be performed within the same session. For example, one can create multiple
  "act"s when multiple network connections are required.

- The `submit` block is like a main function of PoE. Typically, it contains a
  single call to an act, but when your exploit requires multiple sessions, you
  can call multiple acts in a submit block. The return value of a submit block
  is specially considered as a submitted flag: `PoE.Replayer` will print out the
  flag value to the console.

## Testing PoE

You can test the above PoE with `PoE.Replayer` using the following commands.

1. First, compile the Level-02 program.
   ```
   cd tutorial/Level-02
   dotnet build
   ```

2. Go back to the root directory, and run the following command to run
   `PoE.Replayer`.

   - On Windows:
     ```
     > poe stdin Level-02.poe bin\Debug\net5.0\Level-02.exe
     ```

   - On Linux:
     ```
     $ poe stdin Level-02.poe bin/Debug/net5.0/Level-02
     ```

When looking at the output, the obtained flag should be printed out in both a
hexadecimal and an ASCII format. If you want to see the entire messages between
the `PoE.Replayer` and the Level-02 program, you can enable the verbose mode by
giving `-v` option after `stdin` command. For example, on Windows,

```
> poe stdin -v Level-02.poe bin\Debug\net5.0\Level-02.exe
Welcome to the world of PoE!
Type "I Love PoE" to solve this challenge.
>
I Love PoE
Good, this is your flag:
POE_IS_AWESOME
hex   = {504f455f49535f415745534f4d45}
ascii = {POE_IS_AWESOME}
```

The yellow texts are the ones that you read from the program, and the blue ones
are what you wrote to the program.

## Debugging

While writing a PoE, you may want to debug your program by printing out
values. For this purpose, PoE provides two different statements: `dbgascii` and
`dbghex`.

#### `dbgascii (expr)`

`dbgascii` can print out the value of any PoE expression (`expr`) in an ASCII
string. For example, `dbgascii 41hs` will print out `A` in the terminal.

#### `dbghex (expr)`

`dbghex` can print out the value of any PoE expression (`expr`) in a hexadecimal
string. For example, `dbghex "A"` will print out `41` in the terminal.

#### Hello World Example

```
submit:
    dbgascii "hello world"
    return ()
```

This PoE will not do anything but simply print out the "hello world" message to
the console. Save the code in a file `test.poe` and run:

```
> poe stdin test.poe /bin/cat
```

Note that you can replace `/bin/cat` with any valid executable file, as the PoE
interpreter will never spawn the program anyways; the PoE code does not contain
any act, so it will not execute the given program.
