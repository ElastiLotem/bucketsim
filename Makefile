CFLAGS=-Wall -O2 -Wextra

randombucket: randombucket.c
	gcc -o $@ $< ${CFLAGS} -lm && ./randombucket
