######################################################################
#
#   Makefile for configuraing and maintaining buildbot.
# 
######################################################################

# neccessary makefile/user plumbing

BUILDBOTDIR = ~/buildbot
.PHONY: master

help:
	@echo "A makefile for installing and confiugring a buildbot."
	@echo "(Some assembly required.)"
	@echo ""
	@echo "Targets:"
	@echo "     master         create a master in $(BUILDBOTDIR)/master"
	@echo "     slave          create a slave named 'slave'"
	@echo "     slave-[name]   create additional slaves as needed, with 'slave-[name]' names"
	@echo ""
	@echo "     upgrade        upgrade the master to the latest version"
	@echo "     start          start the build master"
	@echo "     stop           stop the build master"
	@echo "     restart        restart the build master"
	@echo "     reconfig       force the buld master to load a new config"

######################################################################
#
#   Targets for creating an configuring a buildmaster
# 
######################################################################

master: $(BUILDBOTDIR)/master $(BUILDBOTDIR)/master.cfg $(BUILDBOTDIR)/master/config

$(BUILDBOTDIR):
	mkdir $@
$(BUILDBOTDIR)/master:$(BUILDBOTDIR)
	buildbot create-master master
config:buildbot
	ln -s `pwd`/$< $@
$(BUILDBOTDIR)/master/config:config
	mv $< $@
master.cfg:$(BUILDBOTDIR)
	ln -s config/master.cfg master.cfg
$(BUILDBOTDIR)/master.cfg:master.cfg
	mv $< $@


######################################################################
#
#   Targets for configuring buildslaves
# 
######################################################################

slave: $(BUILDBOTDIR)/slave

$(BUILDBOTDIR)/slave:
	buildslave create-slave slave slave slave
slave-%:
	cd $(BUILDBOTDIR) ; buildslave create-slave $@ $@ $@


######################################################################
#
#   Targets for Controling Buildbot in Normal Operation
# 
######################################################################

upgrade:upgrade-master
start stop restart reconfig upgrade-master: 
	cd $(BUILDBOTDIR)/master ; buildbot $@
