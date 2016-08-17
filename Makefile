.PHONY:

sourcefiles = $(shell ffind --full-path --literal .lisp)

quickutils.lisp: make-quickutils.lisp
	sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

DOCUMENTATION.markdown: $(sourcefiles)
	sbcl --noinform --load make-docs.lisp  --eval '(quit)'

