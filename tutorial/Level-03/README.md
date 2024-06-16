Level 03
===

In this tutorial, you will use a slightly more complicated
[program](Program.fs), which generates a random challenge. Your goal is to write
PoE code that can solve the challenge. Let's first compile the program, and see
how it runs.

```
$ cd Level-03
$ dotnet run
Welcome to the world of PoE!
Type "8O8sjBBk0m" to solve this challenge.
> xxx
Wrong

$ dotent run
Welcome to the world of PoE!
Type "PVLJdfWa59" to solve this challenge.
> PVLJdfWa59
THIS_IS_FLAG
```

You can obtain the flag only when you gave the correct random string to the
program. So your goal is to extract the random challenge string from the
program's output, and send the same string to the program to get the flag.  The
following PoE does the job.

```
act exploit():
    read("Type \"")
    bv key = read(10) // The challenge is 10-byte long.
    read(-1) // consume the rest output.
    write(key . "\n") // send the challenge
    bv flag = rtrim(read(0ahs)) // read the flag and trim the newline at the end
    return flag

submit:
    return exploit()
```

In the submit declaration, there is nothing more than just a call to the act
`exploit`. So it will launch a process and execute the act to obtain the flag.
In the first line of the act, `read` function will consume data stream until it
meets `Type "`. The very next byte will be the first character of the random
challenge. So we read 10 bytes using the second `read`. Next, we call `read`
once more to consume the rest of messages, and send the challenge back to the
program using `write`. Note, here we append a newline character as the program
expects an input that terminates with a newline. Finally, we read the flag
string and use `rtrim` function to trim the newline at the end of the
message. The returned flag will then be returned again by the `submit`, and the
interpreter will output the flag in the terminal.

You can test the PoE by running the interpreter with the following command:

- On Windows
```
> poe stdin Level-03.poe bin\Debug\net5.0\Level-03.exe
```

- On Linux
```
> poe stdin Level-03.poe bin/Debug/net5.0/Level-03
```
