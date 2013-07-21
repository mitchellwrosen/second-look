# Second Look
Second Look is an endpoint for a GitHub post-receive hook. It will send an email to one or more developers based on the
contents of the new commits, requesting a code review.

I blatantly stole this idea from [reviewthis](https://github.com/supermatter/reviewthis), which you should definitely go
check out.

## Setup
Set a [post-receive hook](https://help.github.com/articles/post-receive-hooks) on your GitHub repository to
> ec2-54-213-58-191.us-west-2.compute.amazonaws.com

## Usage
Simply include one or more **\user** or **\user@domain** patterns in a commit message, which correspond to GitHub
usernames and email addresses. Those people will be notified by email to review your commit.

Example:

> $ git commit -m "Buncha hacks. Make sure I don't bomb the Russians, \chebert"
>
> ... work work work ...
>
> $ git commit -m "more hax \chebert \steve@helpme.com"

### \username? What gives? Why not @username? Everybody's doin' it.
GitHub will already automatically send out an email when you mention a user with @, by default.

### GitHub comes with this feature out-of-the-box? Then what's the point of all this?
I truly was not aware of the @username feature of GitHub until I started testing this application :(. That being said, I
think the emails sent by Second Look are much nicer looking :). Try it out!

### Known Issues
GitHub may change the format of the [POST payload](https://help.github.com/articles/post-receive-hooks#the-payload) (in
fact, it's currently out of date!). When this happens, the server breaks. Rest assured, though, that these occurrences
should be few and far between, and I will promptly fix them when they do occur.
