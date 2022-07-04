
include ../Makefile.config

all:test

test:
	gprbuild -p -j0 -P spawn_manager-tests.gpr -XLIBRARY_TYPE=static
	bin/spawn_manager-tests-main | tee  results.txt
	diff  golden-test.out  test.out


installation:
	gprbuild -p -j0 -P spawn_manager-test_installed.gpr

edit:
	gnatstudio -P  spawn_manager-tests.gpr -XLIBRARY_TYPE=static
