// This file issues an typeerror, but nevermind as we are still detecting printf as system function call.
int secret = 666;

void bar(int b) {
    secret = b;

    return;
}

int foo(int p) {
#ifdef A
    p = 0;
#endif
#ifdef B
    return p;
#else
    return 0;
#endif
}

void main () {
    int x, y;
    int returnSite;

    int z = 0;

    x = secret;
    y = 0;

#ifdef C
    x = 0;
#endif

#ifdef D

     y = foo(x);

#endif

    returnSite = y;

    printf("%s\n", &returnSite);
    return;
 }



