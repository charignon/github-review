# GReview: code review from the comfort of Emacs.

This package contains a handful of Emacs commands to review github pull request
wihtout leaving Emacs.

With `greview/start-review` you can pull the content of a pull request into a
buffer as a diff with comments corresponding to the PR description.
In that buffer you can add comment (global and inline) that you want to make on
the pull request. Once done, you can submit your review with one of
`greview/approve`, `greview/comment` and `greview/reject`.

## Installation

TBD

## Customization

If you use github entreprise, you can use the `greview-host` custom variable to
configure the endpoint of your github entreprise installation.
