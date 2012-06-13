from buildbot.schedulers.basic import SingleBranchScheduler
from buildbot.schedulers.forcesched import ForceScheduler, FixedParameter, StringParameter
from buildbot.schedulers.timed import Periodic
from buildbot.schedulers.trysched import Try_Userpass

from buildbot.changes.filter import ChangeFilter

builder_names = [ "institute-site", "institute-internal" ]
polled_builder_names = [ "polled-builder" ]

all_builder_names = builder_names + polled_builder_names

schedulers = []

manual_builder = ChangeFilter(project=builder_names, branch='master')
schedulers.append(SingleBranchScheduler(name="all", 
                                        treeStableTimer=None,
                                        change_filter=manual_builder,
                                        builderNames=builder_names))

polled_builder = ChangeFilter(project='polled-docs', branch='master')
schedulers.append(SingleBranchScheduler(name="mongodb", 
                                        treeStableTimer=None,
                                        change_filter=polled_filter,
                                        builderNames=polled_builder_names))

schedulers.append(ForceScheduler(name="force",
                                 repository=FixedParameter(name="repository"),
                                 branch=StringParameter(name="branch", default="master"),
                                 project=FixedParameter(name="project", default=""),
                                 properties=[],
                                 builderNames=all_builder_names))
