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

# slaves seem to have a hard time fetching from github, so retry every 5
# seconds, 5 times
GIT_RETRY = (5,5)

def sphinx_factory(repo, target='publish', branch='master'):
    f = factory.BuildFactory()

    # check out the source
    f.addStep(Git(repourl=repo,
                  branch=branch,
                  retry=GIT_RETRY))

    # run the build process.
    f.addStep(ShellCommand(command=["make", target]))

######################################################################

builder_names = []
polled_builder_names = []

builders = []

def add_builder(name, target, repo, branch, schedule, slave):


    builders.append(BuilderConfig(name=name,
                                  slavenames=[slave],
                                  factory=sphinx_factory(repo, target, branch)))

    if schedule == 'standard':
        builder_names.append(name)
    elif schedule =='polled':
        polled_builder_names.append(name)

institute_repo = 'gitosis@foucault.cyborginstitute.net:instititue.git'

add_builder('institute-site', 'html', institute_repo, 'master', 'standard', 'sphinx')
