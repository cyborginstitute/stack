HOSTNAME := $(shell hostname -s)
BUILDDIR = ../build
CURRENT := $(shell git rev-parse HEAD)

$(BUILDDIR)/emacs-config.tar.gz:$(BUILDDIR)/emacs-config-$(CURRENT).tar.gz
	ln -s emacs-config-$(CURRENT).tar.gz emacs-config.tar.gz
	mv emacs-config.tar.gz $@

$(BUILDDIR)/stumpwm-config.tar.gz:$(BUILDDIR)/stumpwm-config-$(CURRENT).tar.gz
	ln -s stumpwm-config-$(CURRENT).tar.gz stumpwm-config.tar.gz 
	mv stumpwm-config.tar.gz $@

docs/build/dirhtml/:
	make -C docs/ dirhtml

$(BUILDDIR)/stumpwm-config-$(CURRENT).tar.gz:
	make -C stumpwm/ package

$(BUILDDIR)/emacs-config-$(CURRENT).tar.gz:
	make -C emacs/ package

#############
# Shortcuts #
#############

all: $(BUILDDIR)/stumpwm-config.tar.gz $(BUILDDIR)/emacs-config.tar.gz docs/build/dirhtml/

docs:docs/build/dirhtml/

push:
	rsync -azrc build/*.tar.gz download.cyborginstitute.net:/srv/www/cyborginstitute/public/download/
	rsync -azrc emacs/docs/build/dirhtml/ cyborginstitute.net:/srv/www/cyborginstitute/public/projects/stack/

clean:
	rm -rf docs/build/dirhtml/
	rm -rf $(BUILDDIR)/stumpwm-config.tar.gz
	rm -rf $(BUILDDIR)/emacs-config.tar.gz
