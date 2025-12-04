#include <stdio.h>
#include <stdlib.h>

void vuln(){
    char buffer[32];
    if (fgets(buffer, 0x100, stdin) != NULL) {
        printf("%s, you have a great name.\n", buffer);
    } else {
        printf("Error reading input.\n");
    }
}

int main() {
    printf("You can join me at %p\n",main);
    puts("who are you ? \n");
    vuln();
    return 0;
}