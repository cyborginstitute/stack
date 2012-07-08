SOURCE_STUMP_CONFIG = ~/garen/stumpwm
HOSTNAME := $(shell hostname -s)
BUILDDIR = ../build
CURRENT := $(shell git rev-parse HEAD)

STUMPDIR = stumpwm-config

.PHONY: import install help package clean clean-package clean-build configure install force

.DEFAULTGOAL = help

help:
	@echo "Targets for building the distribution:"
	@echo "	  import	  -- copies and munges tychoish's current configuration"
	@echo "	  package	  -- builds the tarball from the current state of the repo."
	@echo "	  clean-build	  -- removes temp files from the build directory."
	@echo "	  clean-packages  -- removes package files from the build directory."

######################################################################
#
# Packaging targets for building StumpWM config packages. Internal.
#
######################################################################

import:$(STUMPDIR) $(STUMPDIR)/build.txt

.PHONY:$(STUMPDIR)/tycho-settings.lisp $(STUMPDIR)/tycho-system.lisp

$(STUMPDIR):
	rm -rf $(STUMPDIR)
	mkdir $(STUMPDIR)/
	cp -R $(SOURCE_STUMP_CONFIG)/* $(STUMPDIR)
	rm -rf $(STUMPDIR)/config/archive/ $(STUMPDIR)/layout/* $(STUMPDIR)/archive
	rm -rf $(STUMPDIR)/notifications.lisp $(STUMPDIR)/wifi.lisp $(STUMPDIR)/window-tags.lisp $(STUMPDIR)/undocumented.lisp $(STUMPDIR)/colors-wombat.lisp $(STUMPDIR)/app-menu.lisp $(STUMPDIR)/mem.lisp $(STUMPDIR)/battery.lisp
	for i in `find $(STUMPDIR) -name "*.lisp"`; do sed -i -r -e "s@/home/tychoish/garen@~@g" -e "s@/home/tychoish@~@g" -e "s@xgen@work@g" -e "s@foucault@remote@g" -e "s@cyborginstitute@example@g" -e "s/tychoish@/whoami@/" $$i; done
	make $(STUMPDIR)/tycho-settings.lisp $(STUMPDIR)/tycho-system.lisp
$(STUMPDIR)/build.txt:$(STUMPDIR)/config/hostname.template
	rm -f $@ 
	echo $(CURRENT) >$@
$(STUMPDIR)/config/arendt.lisp:$(STUMPDIR)
$(STUMPDIR)/config/hostname.template:$(STUMPDIR)/config/arendt.lisp
	cp $< $@
	echo -e "\n\n ;; TODO once you have a default desktop layout that you want to us\n ;; saved, amend uncomment the following line:" >> $(STUMPDIR)/config/hostname.template
	sed -i -r -e "s/arendt/HOSTNAME/g" -e "s/^(\(run-commands)(.*)$$/\;\; \1\2/g" $@
$(STUMPDIR)/tycho-settings.lisp:
	sed -i -r -e 's/(\(load-module "mpd"\))/;; \1/g' -e 's/(\(load-module "battery-portable"\))/;; \1/g' $@
$(STUMPDIR)/tycho-system.lisp:
	sed -i -e "s/--server-file=tychoish/-a emacs/" -e "s/--server-file=work/-a emacs/" $@
	@echo "[DEBUG] $@"

#
# Package Building Targets
#

package: $(BUILDDIR)/$(STUMPDIR)-$(CURRENT).tar.gz

$(BUILDDIR)/$(STUMPDIR)/stumpwm/:$(STUMPDIR)/build.txt
	mkdir -p $(BUILDDIR)/$(STUMPDIR)/stumpwm/
	cp -R $(STUMPDIR)/* $(BUILDDIR)/$(STUMPDIR)/stumpwm
$(BUILDDIR)/$(STUMPDIR)/makefile:makefile.user
	cp $< $@
$(BUILDDIR)/$(STUMPDIR)/README:../docs/source/stumpwm.txt
	cp $< $@
$(BUILDDIR)/$(STUMPDIR)/COPYING:../COPYING
	cp $< $@
$(BUILDDIR)/$(STUMPDIR)-$(CURRENT).tar.gz: $(BUILDDIR)/$(STUMPDIR)/stumpwm/ $(BUILDDIR)/$(STUMPDIR)/makefile $(BUILDDIR)/$(STUMPDIR)/COPYING $(BUILDDIR)/$(STUMPDIR)/README
	tar -C $(BUILDDIR) -czvf $@ $(STUMPDIR)/


######################################################################
#
# Cleanup releated targets for removing files
#
######################################################################

clean-build:
	rm -rf $(BUILDDIR)/$(STUMPDIR)/
clean-all:
	rm -rf $(BUILDDIR)/$(STUMPDIR)/ $(STUMPDIR)
