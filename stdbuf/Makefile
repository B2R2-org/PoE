main: stdbuf.c
	mkdir -p lib
	# for linux x86 and x86-64
	# gcc -Wall -fPIC -shared -o lib/stdbuf-linux-x86 stdbuf.c -m32
	# gcc -Wall -fPIC -shared -o lib/stdbuf-linux-x64 stdbuf.c -m64
	# default instruction
	gcc -Wall -fPIC -shared -o lib/stdbuf stdbuf.c

