CFLAGS=-Wall -O2 -g -Wextra -I${HOME}/git/elfs-system/elfs

.PHONY: default run

default: runrandombucket runlocalityunits

runrandombucket: randombucket
	./$<

runlocalityunits: localityunits
	./$<

randombucket: randombucket.c
	gcc -o $@ $^ ${CFLAGS} -lm

localityunits: localityunits.c ${HOME}/git/elfs-system/elfs/infra/dstruct/hash.c
	gcc -o $@ $^ ${CFLAGS} -lm
