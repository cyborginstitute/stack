status = []

from buildbot.status import html
from buildbot.status.web.authz import Authz
from buildbot.status.web.auth import BasicAuth

users = [ ('dev', 'bbot!')] # it's not *that* secret..
authz = Authz(auth=BasicAuth(users),
	forceBuild='auth',
)
status.append(html.WebStatus(
		http_port=8010,
		authz=authz,
		order_console_by_time=True,
		revlink="http://github.com/cyborginstitute/institute/commit/%s",
		change_hook_dialects={ 'github' : True },
		))

from buildbot.status import words
status.append(words.IRC(host="irc.oftc.net", nick="institute-bb",
				notify_events={
					'successToFailure' : 1,
					'failureToSuccess' : 1,
				},
                              channels=["#institute"]))
