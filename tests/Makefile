
include ../Makefile.config

all:test

test:
	gprbuild -p -j0 -XLIBRARY_TYPE=static
	bin/spawn_manager-tests-main | tee  results.txt
	diff  golden-test.out  test.out


