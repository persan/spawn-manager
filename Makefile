
-include Makefile.config

PROJECT=spawn_manager

_docdir=${PREFIX}/share/${PROJECT}/doc
all:compile test

help:
	@echo "help       Print this text"
	@echo "compile    Compile the project"
	@echo "variables  Print the configuration"
	@echo "install    Installs the project"
	@echo "tag        Makes a tag and a .tar-ball for release"


compile:
	gprbuild -p -P${PROJECT}-server.gpr -XLIBRARY_TYPE=static
	gprbuild -p -P${PROJECT}.gpr        -XLIBRARY_TYPE=relocatable
	gprbuild -p -Pbin/${PROJECT}-helper.gpr




variables:
	@echo "PREFIX=${PREFIX}"
	@echo "PATH=${PATH}"


install:
	rm _ -rf
	gprinstall -a -f -p -P${PROJECT}-server.gpr  --build-name=static      -XLIBRARY_TYPE=static      --prefix=${DESTDIR}${PREFIX} --mode=usage
	gprinstall -a -f -p -P${PROJECT}.gpr         --build-name=static      -XLIBRARY_TYPE=static      --prefix=${DESTDIR}${PREFIX}
	gprinstall -a -f -p -P${PROJECT}.gpr         --build-name=relocatable -XLIBRARY_TYPE=relocatable --prefix=${DESTDIR}${PREFIX}
	mkdir -p ${DESTDIR}/${_docdir}
	cp README.md ${DESTDIR}/${_docdir}

uninstall:
	-gprinstall -P${PROJECT}-server.gpr  --uninstall  -XLIBRARY_TYPE=static      --prefix=${DESTDIR}${PREFIX} --mode=usage
	-gprinstall -P${PROJECT}.gpr         --uninstall  -XLIBRARY_TYPE=static      --prefix=${DESTDIR}${PREFIX}
	-gprinstall -P${PROJECT}.gpr         --uninstall  -XLIBRARY_TYPE=relocatable --prefix=${DESTDIR}${PREFIX}
	-rm -rf ${DESTDIR}/${_docdir}


test:
	${MAKE} -C tests

test-i:
	${MAKE} -C tests installation

Makefile.config:Makefile
	@echo "PREFIX=$(shell dirname $(shell dirname $(shell which gnatls)))"   >$@
	@echo "export PATH=${CURDIR}/bin:${PATH}" >>$@
	@echo "TAG=$(shell ./version-helper.py)"  >>$@

tag:compile test
	@if [[ -n "`git status --porcelain`" ]] ; then\
		echo "Folder is not clean";\
		git status;\
		exit 1;\
	fi
	bin/helper
	grep "`bin/helper -v`-`date +%Y%m%d`" README.md >/dev/null
	git tag  "`bin/helper -v`-`date +%Y%m%d`"
	git push
	git push --all

clean:
	git clean -xdf
