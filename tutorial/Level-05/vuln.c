#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void showme()
{
    char* buf;
    long size;
    FILE *f = fopen("flag.txt", "r");
    if (!f) return;
    fseek(f, 0, SEEK_END);
    size = ftell(f);
    fseek(f, 0, SEEK_SET);
    buf = (char*) malloc(size + 1);
    fread(buf, 1, size, f);
    printf("%s\n", buf);
    fclose(f);
}

int main(void)
{
#if __unix__
    char buf[32];
    char key[32];
#else
    char key[32];
    char buf[32];
#endif
    long long keyprefix;
    long long bufprefix;
    memset(key, 0, 32);
    fgets(buf, 41, stdin);
    bufprefix = *(long long*) buf;
    keyprefix = *(long long*) key;
    if (bufprefix == 0xfeedfacecafebeef && keyprefix == 0xfeedfacecafebeef)
        showme();
    else
        puts("Wrong password.");
    return 0;
}
