(load-file "./test/test-helper.el")
(require 'a)
(require 'github-review)
(require 'buttercup)

(describe "github-review"
  :var (ghub-get
        ghub-post)
  (before-each
    (setf
     ;; Prevent the tests from hitting the network
     (symbol-function 'ghub-get)
     (lambda (&rest _)
       (error "Cannot make network call in tests"))
     (symbol-function 'ghub-post)
     (lambda (&rest _)
       (error "Cannot make network call in tests"))))

  (defconst expected-review-tl-comment "~ title
~ in
~ three
~ lines
~
~ body
~ part
~ @foo: a comment
~ Reviewed by @babar[APPROVED]: LGTM
a
b
c")

  (defconst simple-context-expected-review "~ title
~ in
~ three
~ lines
~
~ body
~ part
a
b
c")

  (describe "diff parsing"

    (describe "github-review-hunk?"
      (it "can correctly identify a hunk"
        (expect (github-review-hunk? "foo") :to-be nil)
        (expect (github-review-hunk? "@@ b/foo") :to-be t)))

    (defconst example-review-deleted-file "# comment test
~ remove bad
~
~
diff --git a/bar b/bar
deleted file mode 100644
index 8ad537d..0000000
--- a/bar
+++ /dev/null
@@ -1,5 +0,0 @@
-sdasdas
-dsadasdsad
-sadasdasdas
-as
-
# Comment test")

    (defconst expected-review-deleted-file
      '((body . "comment test")
        (comments
         ((position . 5)
          (body . "Comment test")
          (path . "bar")))))

    (defconst example-review-deleted-comment-haskell "# comment test
~ remove bad
~
~
diff --git a/bar b/bar
deleted file mode 100644
index 8ad537d..0000000
--- a/bar
+++ /dev/null
@@ -1,5 +0,0 @@
--- This is a comment
-dsadasdsad
-sadasdasdas
-as
-
# Comment test
diff --git a/hledger-lib/Hledger/Reports/MultiBalanceReport.hs b/hledger-lib/Hledger/Reports/MultiBalanceReport.hs
index 9eced0230..4512bb335 100644
--- a/hledger-lib/Hledger/Reports/MultiBalanceReport.hs
+++ b/hledger-lib/Hledger/Reports/MultiBalanceReport.hs
@@ -47,8 +47,6 @@ import Hledger.Reports.BalanceReport
 --
 --   * the full account name
 --
---   * the leaf account name
---
 --   * the account's depth
# here too
 --
 --   * A list of amounts, one for each column.
@@ -60,8 +58,8 @@ import Hledger.Reports.BalanceReport
 -- 3. the column totals, and the overall grand total (or zero for
 -- cumulative/historical reports) and grand average.

-type MultiBalanceReport    = PeriodicReport AccountLeaf MixedAmount
-type MultiBalanceReportRow = PeriodicReportRow AccountLeaf MixedAmount
+type MultiBalanceReport    = PeriodicReport AccountName MixedAmount
+type MultiBalanceReportRow = PeriodicReportRow AccountName MixedAmount

 -- type alias just to remind us which AccountNames might be depth-clipped, below.
 type ClippedAccountName = AccountName
")

    (defconst expected-review-deleted-comment-haskell
      '((body . "comment test")
        (comments
         ((position . 5)
          (body . "Comment test")
          (path . "bar"))
         ((position . 6)
          (body . "here too")
          (path . "hledger-lib/Hledger/Reports/MultiBalanceReport.hs")))))

    (defconst examplediff "# This is a global comment at the top of the file
# with multiple
# lines
diff --git a/content/reference/google-closure-library.adoc b/content/reference/google-closure-library.adoc
index 58baa4b..eae7707 100644
--- a/content/reference/google-closure-library.adoc
+++ b/content/reference/google-closure-library.adoc
@@ -18,7 +18,7 @@ rich-text editing, and UI widgets/controls.
# comment on zeroth line

# comment on first line
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
")

    (defconst example-previous-comments "~ Top level previous comment
# This is a global comment at the top of the file
# with multiple
# lines
diff --git a/content/reference/google-closure-library.adoc b/content/reference/google-closure-library.adoc
index 58baa4b..eae7707 100644
--- a/content/reference/google-closure-library.adoc
+++ b/content/reference/google-closure-library.adoc
@@ -18,7 +18,7 @@ rich-text editing, and UI widgets/controls.

  ,,* http://google.github.io/closure-library/api/[Google Closure Library
  API Reference]
-* http://www.closurecheatsheet.com/[Closure Cheatsheet] - abridged API
+* https://github.com/kuzmisin/closurecheatsheet[Closure Cheatsheet] - abridged API
~ inline previous
~ comments
# And a comment inline about
# a specific line
# ```with some
# code```
  with usage examples
# Some other comment inline

  [[try-the-wrapper-libraries-first]]
")

    (defconst example-no-comment "# This is a global comment at the top of the file
# with multiple
# lines
diff --git a/content/reference/google-closure-library.adoc b/content/reference/google-closure-library.adoc
index 58baa4b..eae7707 100644
--- a/content/reference/google-closure-library.adoc
+++ b/content/reference/google-closure-library.adoc
@@ -18,7 +18,7 @@ rich-text editing, and UI widgets/controls.

  ,,* http://google.github.io/closure-library/api/[Google Closure Library
  API Reference]
-* http://www.closurecheatsheet.com/[Closure Cheatsheet] - abridged API
  with usage examples

  [[try-the-wrapper-libraries-first]]
")

    (defconst complex-review-expected
      '((body . "This is a global comment at the top of the file\nwith multiple\nlines")
        (comments
         ((position . 1)
          (body . "comment on zeroth line\ncomment on first line")
          (path . "content/reference/google-closure-library.adoc"))
         ((position . 5)
          (body . "And a comment inline about\na specific line\n```with some\ncode```")
          (path . "content/reference/google-closure-library.adoc"))
         ((position . 6)
          (body . "Some other comment inline")
          (path . "content/reference/google-closure-library.adoc")))))

    (defconst complex-review-expected-no-comment-on-zeroth-and-first-line
      '((body . "This is a global comment at the top of the file\nwith multiple\nlines")
        (comments
         ((position . 5)
          (body . "And a comment inline about\na specific line\n```with some\ncode```")
          (path . "content/reference/google-closure-library.adoc"))
         ((position . 6)
          (body . "Some other comment inline")
          (path . "content/reference/google-closure-library.adoc")))))


    (describe "github-review-parse-review-lines"
      (it "can parse a complex code review"
        (let* ((actual (github-review-parse-review-lines (split-string examplediff "\n"))))
          (expect actual :to-equal complex-review-expected)))
      (it "can parse a code review with no comment"
        (let* ((actual (github-review-parse-review-lines (split-string example-no-comment "\n")))
               (expected '((body . "This is a global comment at the top of the file\nwith multiple\nlines"))))
          (expect actual :to-equal expected)))
      (it "can parse a code review with deleted files"
        (let* ((actual (github-review-parse-review-lines (split-string example-review-deleted-file "\n"))))
          (expect actual :to-equal expected-review-deleted-file)))
      (it "can parse a code review with a removed comment in haskell"
        (let* ((actual (github-review-parse-review-lines (split-string example-review-deleted-comment-haskell "\n"))))
          (expect actual :to-equal expected-review-deleted-comment-haskell)))
      (it "can parse a code review with previous comments but ignores it"
        (let* ((actual (github-review-parse-review-lines (split-string example-previous-comments "\n"))))
          (expect actual :to-equal complex-review-expected-no-comment-on-zeroth-and-first-line)))))

  (describe "PR name inference from review filename and url"

    (describe "github-review-pr-from-fname"
      (it "can parse fname and infer pr name"
        (expect
         (a-equal
          (github-review-pr-from-fname "/tmp/charignon___testgheapi___2.diff")
          '((num . "2")
            (repo . "testgheapi")
            (owner . "charignon"))))))

    (describe "github-review-pr-from-url"
      (it "can parse url and infer pr details"
        (expect
         (a-equal
          (github-review-pr-from-url "https://github.com/charignon/testgheapi/pull/2")
          '((num . "2")
            (repo . "testgheapi")
            (owner . "charignon")))))))

  (describe "diff formatting"

    (defconst simple-context
      `((object . ((title . "title\nin\nthree\nlines")
                   (body . "body\npart")))
        (diff . ((message . "a\nb\nc")))))

    (defconst context-with-tl-comments
      `((object . ((title . "title\nin\nthree\nlines")
                   (body . "body\npart")))
        (top-level-comments ((user . ((login . "foo")))
                             (body . "a comment")))
        (reviews ((user . ((login . "babar")))
                  (state . "APPROVED")
                  (body . "LGTM")))
        (diff . ((message . "a\nb\nc")))))

    (describe "github-review-format-diff"
      (it "can format a simple diff"
        (expect (github-review-format-diff simple-context)
                :to-equal simple-context-expected-review))
      (it "can format a diff with top level comments and review"
        (expect (github-review-format-diff context-with-tl-comments)
                :to-equal expected-review-tl-comment))))
  (describe "entrypoints"
    (describe "github-review-start"
      :var (github-review-save-diff
            github-review-get-pr-obj
            github-review-get-pr-diff
            diff)

      (before-each
        (setf
         ;; Mock all the I/O for the test
         (symbol-function 'github-review-save-diff)
         (lambda (_ value)
           (setq diff value))

         (symbol-function 'github-review-get-pr-diff)
         (lambda (_ cb)
           (funcall cb `((message . "a\nb\nc"))))

         (symbol-function 'github-review-get-issue-comments)
         (lambda (_ cb)
           (funcall cb `(((user . ((login . "foo")))
                          (body . "a comment")))))

         (symbol-function 'github-review-get-reviews)
         (lambda (_ cb)
           (funcall cb `(((user . ((login . "babar")))
                          (state . "APPROVED")
                          (body . "LGTM")))))))

      (describe "no top level comments are present"
        (before-each
          (setf (symbol-function 'github-review-get-pr-object)
                (lambda (_ cb)
                  (funcall cb
                           '((body . "body\npart")
                             (comments . 0)
                             (review_comments . 0)
                             (title . "title\nin\nthree\nlines"))))))
        (it "can render a diff"
          (deferred:sync! (github-review-start "https://github.com/charignon/github-review/pull/6"))
          (expect diff :to-equal simple-context-expected-review)))

      (describe "with top level comments"
        (before-each
          (setf (symbol-function 'github-review-get-pr-object)
                (lambda (_ cb)
                  (funcall cb
                           '((body . "body\npart")
                             (comments . 1)
                             (review_comments . 1)
                             (title . "title\nin\nthree\nlines"))))))
        (it "can render a diff"
          (let ((github-review-fetch-top-level-and-review-comments t))
            (deferred:sync! (github-review-start "https://github.com/charignon/github-review/pull/6"))
            (expect diff :to-equal expected-review-tl-comment))))

      (describe "with review that has no top level comment"
        (before-each
          (setf
           (symbol-function 'github-review-get-reviews)
           (lambda (_ cb)
             (funcall cb `(((user . ((login . "babar")))
                            (state . "APPROVED")
                            (body . "")))))
           (symbol-function 'github-review-get-pr-object)
           (lambda (_ cb)
             (funcall cb
                      '((body . "body\npart")
                        (comments . 0)
                        (review_comments . 1)
                        (title . "title\nin\nthree\nlines"))))))
        (it "does not show it"
          (let ((github-review-fetch-top-level-and-review-comments t))
            (deferred:sync! (github-review-start "https://github.com/charignon/github-review/pull/6"))
            (expect diff :to-equal simple-context-expected-review)))))))

(describe "Api host computation"
  (it "defaults to api.github.com"
    (expect (github-review-api-host '()) :to-equal "api.github.com"))
  (it "can be overriden"
    (expect (github-review-api-host (a-alist 'apihost "api.github.biz")) :to-equal "api.github.biz")))

(provide 'github-review-test)

;;; github-review-test.el ends here
