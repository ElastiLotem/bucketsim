CFLAGS=-Wall -O2 -g -Wextra -I${HOME}/Elastifile/elfs/

.PHONY: default run

default: runrandombucket runlocalityunits

runrandombucket: randombucket
	./$<

runlocalityunits: localityunits
	./$<

randombucket: randombucket.c
	gcc -o $@ $^ ${CFLAGS} -lm

localityunits: localityunits.c ${HOME}/Elastifile/elfs/infra/dstruct/hash.c
	gcc -o $@ $^ ${CFLAGS} -lm
