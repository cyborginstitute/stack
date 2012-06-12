from buildbot.changes.filter import ChangeFilter
from buildbot.changes.gitpoller import GitPoller

change_source = []
change_source.append(GitPoller('git://github.com/mongodb/docs', 
                         branch='master', 
                         pollInterval=120, 
                         project="mongodb-docs"))

mongodb_filter = ChangeFilter(project = 'mongodb', 
                                     branch = 'master')
