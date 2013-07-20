Second Look
===========

Second Look is an endpoint for a GitHub post-receive hook. It will send an email
to one or more developers based on the contents of the new commits.

Status
------
In progress :)

But when it's done...
---------------------

Setup
-----
Set a [post-receive hook](https://help.github.com/articles/post-receive-hooks)
on your GitHub repository to
> ec2-54-213-58-191.us-west-2.compute.amazonaws.com


Usage
-----
Simply include an @user or user@domain pattern in a commit message, which
correspond to GitHub usernames and email addresses. Those people will be
notified by email to review your commit.

Example:

> $ git commit -m "Buncha hacks. Make sure I don't bomb the Russians, @chebert"

Issues
------
GitHub may change the format of the [POST
payload](https://help.github.com/articles/post-receive-hooks#the-payload) (in
fact, it's currently out of date!). This server would be more "reliable" if it
were written in a dynamically typed language that simply tries to access the
"commits.messages" field (which is probably not going to change). Each to his
own, and I personally would rather leverage static typing, and so each time the
POST payload format changes, the server breaks. Rest assured, though, that these
occurrences should be few and far between, and I will promptly fix them when
they do occur.
