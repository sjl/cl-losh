.PHONY: docs test test-sbcl test-ccl test-ecl test-abcl
heading_printer = $(shell which heading || echo 'true')
sourcefiles = $(shell ffind --full-path --literal .lisp)

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
	# oh for fucks sake, use -- for longopts come on
	ecl -load test/run.lisp

test-abcl:
	$(heading_printer) broadway 'ABCL'
	abcl --load test/run.lisp
