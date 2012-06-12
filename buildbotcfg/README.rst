===============================================
Cyborg Institute Default Buildbot Configuration
===============================================

Synopsis
--------

This base configuration provides a very clear, very sparse framework
for developing your very own `Buildbot`_  configuration, in a sane,
organized and manageable fashion without potentially confusing
defaults. This base configuration is directly derived from the
`metabbotcfg`_ configuration file

Full documentation will eventually reside at
`cyborginstitute.org/projects/stack/buildbot`_, which currently
mirrors the README.

Use
---

1. Make a directory for your all buildbot files. Such as: :: 

      mkdir ~/buildbot
   
2. Make a sub-directory for all "build master" configuration. This is
   where the primary logic and code for your build bot config
   lives. For example: ::
   
      mkdir ~/buildbot/master
      
3. Copy this directory into the ``~/buildbot/master/``
   directory. Modify the following as needed: :: 
   
      mv ~/downloads/buildbotcfg ~/buildbot/master/config

4. Create a symbolic link to make everything work as needed: ::

      ln -s ~/buildbot/master/config/master.cfg ~/buildbot/master/master.cfg

5. In the ``~/buildbot/master/`` directory run the ``upgrade-master``
   command as follows: ::

      buildbot upgrade-master

6. Make modifications to all ``.py`` files in this directory as
   needed. The comments and the full documentation will help. Indeed,
   seeing an existing buildbot configuration like the `metabbotcfg`_
   or the `Mozilla Buildbot Configurations`_ will be useful for
   developing your own build system.

   You should make a practice of storing changes to the
   ``~/buildbot/master/config`` files in a revision control system. 
   
7. Start the buildbot in the ``~/buildbot/master/`` directory: ::

      buildbot start 

Rejoice. You have a buildbot. Also familiarize yourself with the
`Buildbot`_ documentation for more information about setting up your
own buildbot configuration

If you have any issues with this, please send a message to the `cyborg
institute listserv`_

.. _`Buildbot`: http://buildbot.net
.. _`cyborginstitute.org/projects/stack/buildbot`: http://cyborginstitute.org/projects/stack/buildbot
.. _`metabbotcfg`: https://github.com/buildbot/metabbotcfg
.. _`Mozilla Buildbot Configurations`: https://github.com/mozilla/buildbot-configs
.. _`cyborg institute listserv`: http://lists.cyborginstitute.net/listinfo/institute
