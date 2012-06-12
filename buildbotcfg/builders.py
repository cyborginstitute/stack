"""
To add a builder:

1. define a factory function below.

2. apend a BuilderConfig object to builders

3. add the name of the bulder to the builder_names list in the
   schedulers.py file.
"""

import textwrap
import itertools

from buildbot.process import factory
from buildbot.steps.source import Git
from buildbot.steps.shell import Compile, Test, ShellCommand
from buildbot.steps.transfer import FileDownload
from buildbot.steps.python_twisted import Trial
from buildbot.steps.python import PyFlakes
from buildbot.config import BuilderConfig

builders = []

# slaves seem to have a hard time fetching from github, so retry every 5
# seconds, 5 times
GIT_RETRY = (5,5)

def institute_site_factory():
    f = factory.BuildFactory()

    # check out the source
    f.addStep(Git(repourl='gitosis@foucault.cyborginstitute.net:institute.git', mode='copy', retry=GIT_RETRY))
    f.addStep(ShellCommand(command=["make", "html"]))

    return f

builders.append(BuilderConfig(name="institute-site",
                              slavenames=["sphinx"],
                              factory=institute_site_factory()))
