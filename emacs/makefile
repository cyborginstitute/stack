SOURCE_EMACS_CONFIG = ~/garen/emacs
# -include makefile.user
HOSTNAME := $(shell hostname -s)
BUILDDIR = ../build
CURRENT := $(shell git rev-parse HEAD)

EMACSDIR = emacs-config
SNIPPETS = $(EMACSDIR)/snippets/text-mode

.PHONY: import install help package clean clean-package clean-build configure install 

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

import:$(EMACSDIR) $(EMACSDIR)/work-deft.el $(EMACSDIR)/tycho-mail-setup-template.el $(EMACSDIR)/config/hostname.template $(SNIPPETS) $(EMACSDIR)/pylookup
	for i in `find $(EMACSDIR) -name "*.el"`; do \
		sed -i -r -e "s@/home/tychoish@~@g" \
		        -e "s@assemblage@wiki@g" -e "s@~/garen/@~/@g" \
			-e "s@xgen@work@g" -e "s@foucault@remote@g" \
			-e "s@vpn@example.net@g" -e "s@cyborginstitute.net@example.net@g" $$i; \
	done
	rm -rf $@/archive $@/config/archive $@/backup/* $@/bookmarks/* $@/helm/.git*
	rm -rf $@/*.elc
$(EMACSDIR):$(SOURCE_EMACS_CONFIG) 
	mkdir -p $@/
	rsync -r -L -v --delete $</ $@
$(EMACSDIR)/pylookup:$(EMACSDIR)
	rm -rf $@/python*-html $@/pylookup.db $@/*.zip $@/.git* 
$(SNIPPETS):$(EMACSDIR)
	rm -rf $(SNIPPETS)/markdown-mode/{linode*,rhizome*,tychoish*,wiki-sig} $(SNIPPETS)/{*gpl*,ciwp*} $(SNIPPETS)/rst-mode/

$(EMACSDIR)/xgen-deft.el:$(EMACSDIR)
$(EMACSDIR)/tycho-mail-setup.el:$(EMACSDIR)
$(EMACSDIR)/config/arendt.el:$(EMACSDIR)

$(EMACSDIR)/work-deft.el:$(EMACSDIR)/xgen-deft.el
	mv $< $@
$(EMACSDIR)/tycho-mail-setup-template.el:$(EMACSDIR)/tycho-mail-setup.el
	mv -f $< $@
$(EMACSDIR)/config/hostname.template:$(EMACSDIR)/config/arendt.el
	cp $< $@
	sed -i -r -e "s/(.*(w3m|tex|slime).*)/\;\; \1/" $@

package:$(BUILDDIR)/$(EMACSDIR)/emacs $(BUILDDIR)/$(EMACSDIR)/makefile $(EMACSDIR)-$(CURRENT).tar.gz
$(BUILDDIR)/$(EMACSDIR)/emacs:$(EMACSDIR)
	mkdir -p $@
	cp -R $</* $@
$(BUILDDIR)/$(EMACSDIR)/makefile:makefile.user
	cp $< $@
$(EMACSDIR)-$(CURRENT).tar.gz:$(EMACSDIR)/
	tar -C $(BUILDDIR) -czvf $(EMACSDIR)-$(CURRENT).tar.gz $(EMACSDIR)/

######################################################################
#
# Cleanup releated targets for removing files
#
######################################################################

clean-packages:
	rm -rf $(BUILDDIR)/*.tar.gz
clean-build:
	rm -rf $(BUILDDIR)/$(EMACSDIR)
