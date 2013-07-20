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
