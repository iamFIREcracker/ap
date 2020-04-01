.PHONY: vendor clean test-sbcl test-ros binary-sbcl binary install

PREFIX?=/usr/local
lisps := $(shell find .  -type f \( -iname \*.asd -o -iname \*.lisp \))

cl-print-version-args := --eval '\
	(progn \
		(print (lisp-implementation-version)) \
		(terpri))'

cl-test-args := --eval '\
	(progn \
		(ql:quickload :ap/tests :verbose T) \
		(let ((exit-code 0)) \
			(handler-case (asdf:test-system :ap) \
				(error (c) \
					(format T "~&~A~%" c) \
					(setf exit-code 1))) \
			(uiop:quit exit-code)))'

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
	sbcl --noinform --load "src/build.lisp"

binary-ros: bin $(lisps)
	ros run -- --noinform --load "src/build.lisp"

binary: binary-sbcl

# Tests -----------------------------------------------------------------------

test: test-sbcl

test-sbcl: $(lisps)
	sbcl --noinform $(cl-test-args)

test-ros: $(lisps)
	ros run $(cl-print-version-args) $(cl-test-args)

# Install ---------------------------------------------------------------------
install:
	cp bin/ap* $(PREFIX)/bin/
