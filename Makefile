CFLAGS=-Wall -O2 -Wextra

.PHONY: default run

default: runrandombucket runlocalityunits

runrandombucket: randombucket
	./$<

runlocalityunits: localityunits
	./$<

randombucket: randombucket.c
	gcc -o $@ $< ${CFLAGS} -lm

localityunits: localityunits.c
	gcc -o $@ $< ${CFLAGS} -lm
