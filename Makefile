.PHONY: docs

sourcefiles = $(shell ffind --full-path --literal .lisp)

vendor: vendor/quickutils.lisp
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && ros run -L sbcl --load make-quickutils.lisp  --eval '(quit)'

DOCUMENTATION.markdown: $(sourcefiles)
	sbcl --noinform --load make-docs.lisp  --eval '(quit)'

docs: DOCUMENTATION.markdown

