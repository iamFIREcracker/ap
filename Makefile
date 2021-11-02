.PHONY: vendor clean test-sbcl test-ros binary-sbcl binary install

PREFIX?=/usr/local
lisps := $(shell find .  -type f \( -iname \*.asd -o -iname \*.lisp \))

all: binary

# Clean -----------------------------------------------------------------------
.PHONY: clean
clean:
	rm -rf bin

# Vendor ----------------------------------------------------------------------
vendor: vendor/pmdb.lisp vendor/quickutils.lisp
vendor/pmdb.lisp:
	cp ~/.lisp/pmdb.lisp vendor/pmdb.lisp
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load "make-quickutils.lisp"  --non-interactive

# Info ------------------------------------------------------------------------
.PHONY: lisp-info
lisp-info:
	sbcl --noinform --quit \
		--load "build/info.lisp"

.PHONY: lisp-info-ros
lisp-info-ros:
	ros run \
		--load "build/info.lisp"

# Build -----------------------------------------------------------------------
.PHONY: binary
binary: binary-sbcl

.PHONY: binary-sbcl
binary-sbcl: bin $(lisps)
	sbcl --noinform --quit \
		--load "build/setup.lisp" \
		--load "build/build.lisp"

.PHONY: binary-ros
binary-ros: bin $(lisps)
	ros run \
		--load "build/setup.lisp" \
		--load "build/build.lisp"

bin:
	mkdir -p bin

# Tests -----------------------------------------------------------------------
.PHONY: test
test: test-sbcl

.PHONY: test-sbcl
test-sbcl: $(lisps)
	sbcl --noinform --quit \
		--load "build/setup.lisp" \
		--load "build/test.lisp"

.PHONY: test-ros
test-ros: $(lisps)
	ros run \
		--load "build/setup.lisp" \
		--load "build/test.lisp"

# Install ---------------------------------------------------------------------
.PHONY: install
install:
	cp bin/ap* $(PREFIX)/bin/
