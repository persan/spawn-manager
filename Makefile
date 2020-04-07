
-include Makefile.config

PROJECT=spawn_manager

_docdir=${PREFIX}/share/${PROJECT}/doc
help:
	@echo "help       Print this text"
	@echo "compile    Compile the project"
	@echo "setup      Configure the Project"
	@echo "variables  Print the configuration"
	@echo "install    Installs the project"
	@echo "tag        Makes a tag and a .tar-ball for release"

all:compile test

compile:
	gprbuild -p -P${PROJECT}-server.gpr -XLIBRARY_TYPE=static
	gprbuild -p -P${PROJECT}.gpr        -XLIBRARY_TYPE=relocatable
	gprbuild -p -Pbin/${PROJECT}-helper.gpr


setup:
	rm -f Makefile.config
	make Makefile.config


variables:
	@echo "PREFIX=${PREFIX}"
	@echo "PATH=${PATH}"


install:
	rm _ -rf
	gprinstall -a -f -p -P${PROJECT}-server.gpr  --build-var=LIBRARY_TYPE --build-name=static      -XLIBRARY_TYPE=static      --prefix=${DESTDIR}${PREFIX} --mode=usage
	gprinstall -a -f -p -P${PROJECT}.gpr         --build-var=LIBRARY_TYPE --build-name=static      -XLIBRARY_TYPE=static      --prefix=${DESTDIR}${PREFIX}
	gprinstall -a -f -p -P${PROJECT}.gpr         --build-var=LIBRARY_TYPE --build-name=relocatable -XLIBRARY_TYPE=relocatable --prefix=${DESTDIR}${PREFIX}
	mkdir -p ${DESTDIR}/${_docdir}
	cp ReadMe ${DESTDIR}/${_docdir}


uninstall:
	gprinstall -u -f -p -P${PROJECT}-server.gpr  --build-var=LIBRARY_TYPE --build-name=static --prefix=${DESTDIR}${PREFIX} --mode=usage
	gprinstall -u -f -p -P${PROJECT}.gpr  --build-var=LIBRARY_TYPE --build-name=relocatable --prefix=${DESTDIR}${PREFIX}
	gprinstall -u -f -p -P${PROJECT}.gpr  --build-var=LIBRARY_TYPE --build-name=static   --prefix=${DESTDIR}${PREFIX}

clean:
	rm -rf .obj bin lib

test:
	${MAKE} -C tests


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
