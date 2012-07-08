# 
# Publication system
#

include makefile.docs

publish: $(BUILDDIR)/publish
	$(MAKE) -j2 $(BUILDDIR)/publish/stack.pdf $(BUILDDIR)/publish/stack.epub

$(BUILDDIR)/dirhtml:dirhtml
$(BUILDDIR)/latex/stack.tex:latex
$(BUILDDIR)/epub/tychoishPersonalStack.epub:epub
$(BUILDDIR)/latex/stack.pdf:$(BUILDDIR)/latex/stack.tex
$(BUILDDIR)/publish/stack.epub:$(BUILDDIR)/epub/tychoishPersonalStack.epub
	cp $< $@
$(BUILDDIR)/publish:$(BUILDDIR)/dirhtml
	mkdir -p $@
	cp -R $</* $@
$(BUILDDIR)/publish/stack.pdf:$(BUILDDIR)/latex/stack.pdf
	cp $< $@
