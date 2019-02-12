# greview: Github code reviews with Emacs.

This package contains a handful of Emacs commands to review github pull request
wihtout leaving Emacs.


Start by calling `greview-start-review` and entering a Pull Request url. You will, then be prompted with a buffer containing the diff of the pull request. The diff contains the description of the PR at the top with line starting with `~`. For example:

```diff
~ Top level previous comment
diff --git a/content/reference/google-closure-library.adoc b/content/reference/google-closure-library.adoc
index 58baa4b..eae7707 100644
--- a/content/reference/google-closure-library.adoc
+++ b/content/reference/google-closure-library.adoc
@@ -18,7 +18,7 @@ rich-text editing, and UI widgets/controls.

  ,,* http://google.github.io/closure-library/api/[Google Closure Library
  API Reference]
-* http://www.closurecheatsheet.com/[Closure Cheatsheet] - abridged API
+* https://github.com/kuzmisin/closurecheatsheet[Closure Cheatsheet] - abridged API
  with usage examples
  [[try-the-wrapper-libraries-first]]
```

You can add comments at the top level by writing lines starting with `#` after the PR description and before the beginning of the diff.

You can add comments inline by adding lines starting with `#` inline. All these features are demonstrated in the example below.
```diff
~ Top level previous comment
# This is a global comment at the top of the file
# with multiple
# lines and will be submitted as a top level review comment
diff --git a/content/reference/google-closure-library.adoc b/content/reference/google-closure-library.adoc
index 58baa4b..eae7707 100644
--- a/content/reference/google-closure-library.adoc
+++ b/content/reference/google-closure-library.adoc
@@ -18,7 +18,7 @@ rich-text editing, and UI widgets/controls.

  ,,* http://google.github.io/closure-library/api/[Google Closure Library
  API Reference]
-* http://www.closurecheatsheet.com/[Closure Cheatsheet] - abridged API
+* https://github.com/kuzmisin/closurecheatsheet[Closure Cheatsheet] - abridged API
# And a comment inline about
# a specific line
# ```with some
# code```
  with usage examples
# Some other comment inline

  [[try-the-wrapper-libraries-first]]
```

Once done, you can submit your review with one of `greview-approve`, `greview-comment` and `greview-reject`.

## Installation

TODO How to install (to complete after MELPA PR is merged)

## Configuration

`greview` needs a github token to act on your behalf for fetching PRs and
submitting reviews.

Refer to https://github.com/magit/ghub to set up a GHE token compatible with
`ghub`, the application to use should be `greview`.

tl;dr: add to your ~/.authinfo.gpg something like the following:

```
machine api.github.com login yourlogin^greview password MYTOKENGOESHERE
```

## Customization

If you use github entreprise, you can use the `greview-host` custom variable to
configure the endpoint of your github entreprise installation.
