.PHONY: docs test test-sbcl test-ccl test-ecl test-abcl
heading_printer = $(shell which heading || echo 'true')
sourcefiles = $(shell ffind --full-path --literal .lisp)

# Vendor ----------------------------------------------------------------------
vendor: vendor/quickutils.lisp
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && ros run -L sbcl --load make-quickutils.lisp  --eval '(quit)'


# Documentation ---------------------------------------------------------------
DOCUMENTATION.markdown: $(sourcefiles)
	sbcl --noinform --load make-docs.lisp  --eval '(quit)'

docs: DOCUMENTATION.markdown


# Testing ---------------------------------------------------------------------
test: test-sbcl test-ccl test-ecl test-abcl

test-sbcl:
	$(heading_printer) computer 'SBCL'
	sbcl --load test/run.lisp

test-ccl:
	$(heading_printer) slant 'CCL'
	ccl --load test/run.lisp

test-ecl:
	$(heading_printer) roman 'ECL'
	ecl --load test/run.lisp

test-abcl:
	$(heading_printer) broadway 'ABCL'
	abcl --load test/run.lisp
