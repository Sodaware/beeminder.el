CASK  ?= cask
EMACS ?= emacs
ELS    = beeminder.el beeminder-settings.el beeminder-org.el beeminder-client.el beeminder-api.el
ELCS   = $(ELS:.el=.elc)

all: $(ELCS)

test: clean all
	${CASK} exec ert-runner

install:
	${CASK} install

%.elc: %.el
	$(EMACS) -Q -batch -L . -f batch-byte-compile $<

clean:
	rm -f $(ELCS)

.PHONY:	all clean test
