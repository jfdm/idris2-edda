##  Makefile

IDRIS := idris2
LIB   := edda
OPTS  :=

.PHONY: doc test clobber check clean lib install

install: lib
	${IDRIS} ${OPTS} --install ${LIB}.ipkg

lib:
	${IDRIS} ${OPTS} --build ${LIB}.ipkg

clean:
	${IDRIS} --clean ${LIB}.ipkg
	find . -name "*~" -delete

check: clobber
	${IDRIS} --typecheck ${LIB}.ipkg

clobber : clean
	find . -name "*.ibc" -delete

test:
	${IDRIS} --testpkg ${LIB}.ipkg

doc:
	${IDRIS} --mkdoc ${LIB}.ipkg
