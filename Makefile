.PHONY: test load clean

QUICKLISP_HOME ?= $(HOME)/quicklisp
XDG_CACHE_HOME := $(CURDIR)/.cache
SBCL := XDG_CACHE_HOME=$(XDG_CACHE_HOME) sbcl --dynamic-space-size 2048 --noinform --no-userinit --non-interactive
LOAD := --load $(QUICKLISP_HOME)/setup.lisp --eval '(push (truename ".") asdf:*central-registry*)'

test:
	@$(SBCL) $(LOAD) \
		--eval '(asdf:load-system :sigil-cl/t)' \
		--eval '(multiple-value-bind (ok pass fail) (sigil-cl/t:run-tests) (declare (ignore pass fail)) (uiop:quit (if ok 0 1)))' \
		2>&1 | grep -E "^(=|---|  [✓✗]|TOTAL)"

load:
	@$(SBCL) $(LOAD) \
		--eval '(asdf:load-system :sigil-cl)' \
		--eval '(format t "~%sigil-cl loaded OK~%")' \
		2>&1 | tail -5

clean:
	@find . -name '*.fasl' -delete
	@rm -rf .cache
