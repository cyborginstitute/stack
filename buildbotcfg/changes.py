from buildbot.changes.filter import ChangeFilter
from buildbot.changes.gitpoller import GitPoller

change_source = []

def add_change_source(project, repo, branch='master', interval=300):
    branchtag = '-' + branch

    if project.endswith(branchtag):
        workingdir = '/tmp/buildbot/' + project
    else:
        workingdir = '/tmp/buildbot/' + project + branchtag

    change_source.append(GitPoller(repo,
                         branch=branch,
                         workdir=workingdir,
                         pollInterval=interval,
                         project=project))

add_change_source('institute-site', 'gitosis@foucault.cyborginstitute.net:institute.git')
