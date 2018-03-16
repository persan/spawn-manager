
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

setup:
	rm -f Makefile.config
	make Makefile.config


variables:
	@echo "PREFIX=${PREFIX}"
	@echo "URL=${URL}"
	@echo "TAGS=${TAGS}"
	@echo "TAG=${TAG}"
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
	gprbuild -p ${PROJECT}-tests.gpr -XLIBRARY_TYPE=static
	bin/${PROJECT}-tests-main | tee test_log.txt


Makefile.config:Makefile
	@echo "PREFIX=$(shell dirname $(shell dirname $(shell which gnatls)))"   >$@
	@echo "URL=$(shell svn info . | grep -e "^URL" | cut -f 2 -d " ")"      >>$@ 2>/dev/null
	@echo "TAGS=$(dir $(shell svn info . | grep -e "^URL" | cut -f 2 -d " "))tags/"    >>$@ 2>/dev/null
	@echo "TAG=$(shell ./version-helper.py)"  >>$@
	@echo "export PATH:=\$${PREFIX}/bin:\$${PATH}" >>$@

tag:
	rm -rf ${PROJECT}-src-${TAG}
	if [[ -n  "$(shell svn stat .)" ]] ; then echo "Direcory not clean"; exit -1 ; fi
	if [[ -z "${MSG}" ]] ; then  echo "no commit message"; exit -1; fi
	svn cp ${URL} ${TAGS}${TAG} "-m${MSG}"
	svn export ${TAGS}${TAG} ${PROJECT}-src-${TAG}
	#svn export . ${PROJECT}-src-${TAG}
	#${MAKE} -C ${PROJECT}-src-${TAG} all
	tar -czf ${PROJECT}-src-${TAG}.tgz ${PROJECT}-src-${TAG}
	rm -rf ${PROJECT}-src-${TAG}

dist:
	curl -F project=devenv  -F build=release -F os=source -F arch=src http://saabworks/upload.php -F artefact=@${PROJECT}-src-${TAG}.tgz
