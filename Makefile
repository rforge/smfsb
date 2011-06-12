# Makefile
# Makefile for package checking building, installing, uninstalling, etc.

VERSION=0.2

check:
	R CMD check pkg

build:
	R CMD build pkg

install:
	make build
	R CMD INSTALL smfsb_$(VERSION).tar.gz

install-pkg:
	R CMD INSTALL pkg

remove:
	R CMD REMOVE smfsb

clean:
	rm -rf *~ smfsb_*.tar.gz pkg.Rcheck


update:
	svn update
	svn log|less

commit:
	svn commit
	make update



# eof

