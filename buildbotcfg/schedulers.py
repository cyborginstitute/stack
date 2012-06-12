from buildbot.schedulers.basic import SingleBranchScheduler
from buildbot.schedulers.forcesched import ForceScheduler, FixedParameter, StringParameter
from buildbot.schedulers.timed import Periodic
from buildbot.schedulers.trysched import Try_Userpass

# from institutebb.slaves import slaves
from institutebb import builders
from institutebb import changes

schedulers = []
schedulers.append(SingleBranchScheduler(name="all", 
                                        branch='master',
                                        treeStableTimer=None,
                                        builderNames=builders.builder_names))

schedulers.append(SingleBranchScheduler(name="mongodb", 
                                        treeStableTimer=None,
                                        change_filter=changes.mongodb_filter,
                                        builderNames=builders.polled_builder_names))

schedulers.append(ForceScheduler(name="force",
                                 repository=FixedParameter(name="repository"),
                                 branch=StringParameter(name="branch", default="master"),
                                 project=FixedParameter(name="project", default=""),
                                 properties=[],
                                 builderNames=builders.builder_names))
