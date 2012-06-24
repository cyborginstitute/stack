==================================================
Desktop Configuration Files "The 'tychoish' Stack"
==================================================

About
-----

This repository contains configuration files, templates, and a build
system for desktop and personal computing tools. Configuration
currently exists or is forthcoming for the following platforms and
environments:

- Emacs
- StumpWM
- Z Shell (i.e. ``zsh``)
- irssi
- Sphinx (documentation)
- Mutt
- Buildbot 

Using this Repository
---------------------

If you're interested in *building* packages, validating the build
process of an existing package from stack, or in submitting changes
for inclusion in a future release of stack, you can use this
repository. *However,* in most cases you'll want to download packages
as described in the next section.

Packages include Makefiles to describe and automate their
production. An ``import`` target, derives the ``[package]-config``
contents from `tychoish's <http://tychoish.com/folk/tychoish>`_
working system. A ``package`` target builds these files into a
``tar.gz`` containing a makefile for installing the package (safely
without overwriting existing configuration.) The filneame of the
tarball contains the git commit identifier (i.e. SHA-1 hash,) that
reflects the state of this repository at the time of
packaging. [#potential-inconsistency]_

Download and Use Packages from Stack
------------------------------------

If you're interested in *using* one of the configuration included in
this repository, the packages themselves provide the best interface to
this configuration. See the `documentation <http://cyborginstitute.com/projects/stack/>`_,
and the `downloads page <http://cyborginstitute.com/projects/stack/downloads>`_
for more information.

.. [#potential-inconsistency] If you/tychoish import/s or modifies
   files without committing the change, then the same identifier
   can--theoretically--refer to different binaries.
