.PHONY: vendor clean test-sbcl test-ros binary-sbcl binary install

PREFIX?=/usr/local
lisps := $(shell find .  -type f \( -iname \*.asd -o -iname \*.lisp \))

all: binary

# Clean -----------------------------------------------------------------------
clean:
	rm -rf bin

# Vendor ----------------------------------------------------------------------
vendor: vendor/pmdb.lisp vendor/quickutils.lisp
vendor/pmdb.lisp:
	cp ~/.lisp/pmdb.lisp vendor/pmdb.lisp
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load "make-quickutils.lisp"  --non-interactive

# Build -----------------------------------------------------------------------
bin:
	mkdir -p bin

binary-sbcl: bin $(lisps)
	sbcl --noinform --load "build.lisp"

binary-ros: bin $(lisps)
	ros run -- --noinform --load "build.lisp"

binary: binary-sbcl

# Tests -----------------------------------------------------------------------

test-sbcl: $(lisps)
	sbcl --noinform --load "test.lisp"

test-ros: $(lisps)
	ros run -- --load "test.lisp"

test: test-sbcl

# Install ---------------------------------------------------------------------
install:
	cp bin/ap* $(PREFIX)/bin/
