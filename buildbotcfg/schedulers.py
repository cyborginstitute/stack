from buildbot.schedulers.basic import Scheduler
from buildbot.schedulers.forcesched import ForceScheduler, FixedParameter, StringParameter

from buildbot.schedulers.timed import Periodic
from buildbot.schedulers.trysched import Try_Userpass
from institutebb.slaves import slaves
from institutebb import builders

schedulers = []
builder_names = [ 'institute-site' ]

schedulers.append(Scheduler(name="all", 
                            branch='master',
                            treeStableTimer=None,
                            builderNames=builder_names))

schedulers.append(ForceScheduler(name="force",
                                 repository=FixedParameter(name="repository"),
                                 branch=StringParameter(name="branch", default="master"),
                                 project=FixedParameter(name="project", default=""),
                                 properties=[],
                                 builderNames=builder_names))
