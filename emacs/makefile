SOURCE_EMACS_CONFIG = ~/garen/emacs
# -include makefile.userHOSTNAME := $(shell hostname -s)
BUILDDIR = ../build
CURRENT := $(shell git rev-parse HEAD)

EMACSDIR = emacs-config
SNIPPETS = $(EMACSDIR)/snippets/text-mode
.PHONY: import install help package clean clean-package clean-build configure install force

.DEFAULTGOAL = help

help:
	@echo "Emacs Configuration Builder"
	@echo "	  install	  -- installs a configured distribution."
	@echo "	  configure	  -- configures the package in perperation for installtion."
	@echo "	  "
	@echo "Internal targets (for building the distribution:)"
	@echo "	  import	  -- copies and munges tychoish's current configuration"
	@echo "	  package	  -- builds the tarball from the current state of the repo."
	@echo "	  clean-build	  -- removes temp files from the build directory."
	@echo "	  clean-packages  -- removes package files from the build directory."

# -include makefile.Packaging

######################################################################
#
# user targets for building .emacs package. Internal.
#
######################################################################

import:
	@echo "This target builds a tarball from tychoish's current .emacs"
	@echo "configuration."
	@echo ""
	rm -rf $(EMACSDIR)
	mkdir $(EMACSDIR)/
	cp -R -L $(SOURCE_EMACS_CONFIG)/* $(EMACSDIR)
	rm -rf $(EMACSDIR)/pylookup/python*-html $(EMACSDIR)/pylookup/pylookup.db $(EMACSDIR)/pylookup/.git* $(EMACSDIR)/helm/.git*
	rm -rf $(EMACSDIR)/archive $(EMACSDIR)/config/archive $(EMACSDIR)/backup/* $(EMACSDIR)/bookmarks/*
	rm -rf $(SNIPPETS)/markdown-mode/{linode*,rhizome*,tychoish*,wiki-sig} $(SNIPPETS)/{*gpl*,ciwp*} $(SNIPPETS)/rst-mode/linode*
	rm -rf $(EMACSDIR)/*.elc
	mv $(EMACSDIR)/xgen-deft.el $(EMACSDIR)/work-deft.el
	cp $(EMACSDIR)/config/arendt.el $(EMACSDIR)/config/hostname.template
	sed -i -r -e "s/(.*(w3m|tex|slime).*)/\;\; \1/" $(EMACSDIR)/config/hostname.template
	mv -f $(EMACSDIR)/tycho-mail-setup-template.el $(EMACSDIR)/tycho-mail-setup.el
	rm -rf $(EMACSDIR)/config/archive/ $(EMACSDIR)/layout/* $(EMACSDIR)/archive
	for i in `find $(EMACSDIR) -name "*.el"`; do 	sed -i -r -e "s@/home/tychoish@~@g" -e "s@assemblage@wiki@g" -e "s@~/garen/@~/@g" -e "s@xgen@work@g" -e "s@foucault@remote@g" -e "s@vpn@example.net@g" $$i; done

package:$(BUILDDIR)/emacs-config/emacs $(BUILDDIR)/emacs-config/makefile emacs-config-$(CURRENT).tar.gz
#	cp docs/stuwmpwm-readme.rst $(BUILDDIR)/emacs-config/README
#	cp COPYING $(BUILDDIR)/emacs-config/
$(BUILDDIR)/emacs-config/emacs:$(EMACSDIR)
	mkdir -p $@
	cp -R $</* $@
$(BUILDDIR)/emacs-config/makefile:makefile.user
	cp $< $@
emacs-config-$(CURRENT).tar.gz:emacs-config/
	tar -C $(BUILDDIR) -czvf emacs-config-$(CURRENT).tar.gz emacs-config/

######################################################################
#
# Cleanup releated targets for removing files
#
######################################################################

clean-packages:
	rm -rf $(BUILDDIR)/*.tar.gz
clean-build:
	rm -rf $(BUILDDIR)/emacs-config
