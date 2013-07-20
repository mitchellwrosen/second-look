Second Look
===========
Second Look is an endpoint for a GitHub post-receive hook. It will send an email
to one or more developers based on the contents of the new commits, requesting
code review.

I stole this idea from [reviewthis](https://github.com/supermatter/reviewthis),
which you should definitely go check out.

Status/TODOs
------------
* ~~send email to \username~~
* send email to user@domain
* send multiple emails, one per matched pattern

Setup
-----
Set a [post-receive hook](https://help.github.com/articles/post-receive-hooks)
on your GitHub repository to
> ec2-54-213-58-191.us-west-2.compute.amazonaws.com

Usage
-----
Simply include a \user pattern in a commit message, which corresponds to a
GitHub username. That person will be notified by email to review your commit.

Example:

> $ git commit -m "Buncha hacks. Make sure I don't bomb the Russians, \chebert"

\username? What gives? Why not @username? Everybody's doin' it.
---------------------------------------------------------------
GitHub will already automatically send out a (very ugly) email when you mention
a user with @, by default. \ is simply in honor of Haskell.

You say GitHub comes with this feature out-of-the-box? Then what's the point of all this?
-----------------------------------------------------------------------------------------
I truly was not aware of the @username feature of GitHub until I started testing
this application. That being said, I think the emails sent by Second Look are
much nicer looking :). Try it out!

Known Issues
------------
GitHub may change the format of the [POST
payload](https://help.github.com/articles/post-receive-hooks#the-payload) (in
fact, it's currently out of date!). This server would be more "reliable" if it
were written in a dynamically typed language that simply tries to access the
"commits.messages" field (which is probably not going to change). Each to his
own, and I personally would rather leverage static typing, and so every time the
POST payload format changes, the server breaks. Rest assured, though, that these
occurrences should be few and far between, and I will promptly fix them when
they do occur.
