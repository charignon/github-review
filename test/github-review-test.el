(load-file "./test/test-helper.el")
(require 'a)
(require 'github-review)
(require 'buttercup)

(describe "github-review"
  :var (ghub-get
        ghub-post
        ghub-graphql)
  (before-each
    (setf
     ;; Prevent the tests from hitting the network
     (symbol-function 'ghub-get)
     (lambda (&rest _)
       (error "Cannot make network call in tests -> ghub-get"))
     (symbol-function 'ghub-post)
     (lambda (&rest _)
       (error "Cannot make network call in tests -> ghub-post"))
     (symbol-function 'ghub-graphql)
     (lambda (&rest _)
       (error "Cannot make network call in tests -> ghub-graphql"))))

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
  (defconst simple-context-expected-review-with-review "~ title
~ in
~ three
~ lines
~
~ body
~ part
~ Reviewed by @babar[APPROVED]: LGTM
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

    (defconst example-diff-before-comments-in-code-line "diff --git a/hledger-lib/Hledger/Reports/MultiBalanceReport.hs b/hledger-lib/Hledger/Reports/MultiBalanceReport.hs
index 9eced0230..4512bb335 100644
--- a/hledger-lib/Hledger/Reports/MultiBalanceReport.hs
+++ b/hledger-lib/Hledger/Reports/MultiBalanceReport.hs

-type MultiBalanceReport    = PeriodicReport AccountLeaf MixedAmount
-type MultiBalanceReportRow = PeriodicReportRow AccountLeaf MixedAmount
+type MultiBalanceReport    = PeriodicReport AccountName MixedAmount
+type MultiBalanceReportRow = PeriodicReportRow AccountName MixedAmount

 -- type alias just to remind us which AccountNames might be depth-clipped, below.
 type ClippedAccountName = AccountName
")

    (defconst example-diff-after-comments-in-code-line "diff --git a/hledger-lib/Hledger/Reports/MultiBalanceReport.hs b/hledger-lib/Hledger/Reports/MultiBalanceReport.hs
index 9eced0230..4512bb335 100644
--- a/hledger-lib/Hledger/Reports/MultiBalanceReport.hs
+++ b/hledger-lib/Hledger/Reports/MultiBalanceReport.hs

-type MultiBalanceReport    = PeriodicReport AccountLeaf MixedAmount
-type MultiBalanceReportRow = PeriodicReportRow AccountLeaf MixedAmount
~ Reviewed by @babar[COMMENTED]: Very interesting change
~ we should move forward
+type MultiBalanceReport    = PeriodicReport AccountName MixedAmount
+type MultiBalanceReportRow = PeriodicReportRow AccountName MixedAmount
~ Reviewed by @babar[COMMENTED]: Change this code

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
          (expect (a-equal actual complex-review-expected))))
      (it "can parse a code review with no comment"
        (let* ((actual (github-review-parse-review-lines (split-string example-no-comment "\n")))
               (expected '((body . "This is a global comment at the top of the file\nwith multiple\nlines"))))
          (expect (a-equal actual expected))))
      (it "can parse a code review with deleted files"
        (let* ((actual (github-review-parse-review-lines (split-string example-review-deleted-file "\n"))))
          (expect (a-equal actual expected-review-deleted-file))))
      (it "can parse a code review with a removed comment in haskell"
        (let* ((actual (github-review-parse-review-lines (split-string example-review-deleted-comment-haskell "\n"))))
          (expect (a-equal actual expected-review-deleted-comment-haskell))))
      (it "can parse a code review with previous comments but ignores it"
        (let* ((actual (github-review-parse-review-lines (split-string example-previous-comments "\n"))))
          (expect (a-equal actual complex-review-expected-no-comment-on-zeroth-and-first-line))))))

  (describe "PR name inference from review filename and url"

    (describe "github-review-pr-from-fname"
      (it "can parse fname and infer pr name"
        (expect
         (a-equal
          (github-review-pr-from-fname "/tmp/charignon___testgheapi___2___60b73b28cc8b1b97ae5474b799219388952b9fc7.diff")
          '((sha . "60b73b28cc8b1b97ae5474b799219388952b9fc7")
            (num . "2")
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

    (defconst simple-diff
      (a-alist 'message "a\nb\nc"))

    (defconst simple-pr
      (a-alist 'title  "title\nin\nthree\nlines"
               'bodyText "body\npart"))

    (defconst pr-with-tl-comments
      (a-alist 'title  "title\nin\nthree\nlines"
               'bodyText "body\npart"
               'comments (a-alist
                          'nodes
                          (list (a-alist
                                 'author (a-alist 'login "foo")
                                 'bodyText "a comment")))
               'reviews (a-alist
                          'nodes
                          (list (a-alist
                                 'author (a-alist 'login "babar")
                                 'bodyText "LGTM"
                                 'state "APPROVED")))))

    (defconst review-with-comments
      (a-alist 'author (a-alist 'login "babar")
               'state "COMMENTED"
               'bodyText ""
               'comments (a-alist
                          'nodes
                          (list (a-alist 'bodyText "Very interesting change\nwe should move forward"
                                         'originalPosition 2)
                                (a-alist 'bodyText "Change this code"
                                         'originalPosition 4)))))

    (describe "github-review-format-diff"
      (it "can format a simple diff"
        (expect (a-equal (github-review-format-diff simple-diff simple-pr)simple-context-expected-review)))
      (it "can format a diff with top level comments and review"
        (expect (a-equal (github-review-format-diff simple-diff pr-with-tl-comments)  expected-review-tl-comment))))

    (describe "github-review-place-review-comments"
      (before-all
        (setq github-review-comment-pos nil)
        (setq github-review-view-comments-in-code-lines nil))
      (it "can include PR comments made in code lines"
        (expect (github-review-place-review-comments example-diff-before-comments-in-code-line review-with-comments)
                :to-equal
                example-diff-after-comments-in-code-line))
      (it "`github-review-comment-pos' should have increased to 3 because we have 2 comments with 3 lines"
        (expect github-review-comment-pos :to-equal 3))))

  (describe "entrypoints"
    (describe "github-review-start"
      :var (github-review-save-diff
            github-review-get-diff
            github-review-get-pr-info
            diff)

      (before-each
         ;; Mock all the I/O for the test
         (setf
         (symbol-function 'github-review-save-diff)
         (lambda (_ value)
           (setq diff value))

         (symbol-function 'github-review-get-diff)
         (lambda (_ cb)
           (funcall cb `((message . "a\nb\nc"))))))

      (describe "no top level comments are present"
        (before-each
          (setf (symbol-function 'github-review-get-pr-info)
                (lambda (_ cb)
                  (funcall cb
                           (a-alist
                            'data (a-alist
                                   'repository
                                   (a-alist
                                    'pullRequest
                                    (a-alist
                                     'title  "title\nin\nthree\nlines"
                                     'bodyText "body\npart"
                                     'headRef (a-alist
                                               'target
                                               (a-alist
                                                'oid
                                                "examplesha"))))))))))

        (it "can render a diff"
          (deferred:sync! (github-review-start "https://github.com/charignon/github-review/pull/6"))
          (expect diff :to-equal simple-context-expected-review)))

      (describe "with top level comments"
        (before-each
          (setf (symbol-function 'github-review-get-pr-info)
                (lambda (_ cb)
                  (funcall cb
                           (a-alist
                            'data (a-alist
                                   'repository
                                   (a-alist
                                    'pullRequest
                                    (a-alist
                                     'title  "title\nin\nthree\nlines"
                                     'bodyText "body\npart"
                                     'comments (a-alist
                                                'nodes
                                                (list (a-alist
                                                       'author (a-alist 'login "foo")
                                                       'bodyText "a comment")))
                                     'reviews (a-alist
                                               'nodes
                                               (list (a-alist
                                                      'author (a-alist 'login "babar")
                                                      'bodyText "LGTM"
                                                      'state "APPROVED")))
                                     'headRef (a-alist
                                               'target
                                               (a-alist
                                                'oid
                                                "examplesha"))))))))))
        (it "can render a diff"
          (let ((github-review-fetch-top-level-and-review-comments t))
            (deferred:sync! (github-review-start "https://github.com/charignon/github-review/pull/6"))
            (expect diff :to-equal expected-review-tl-comment))))

      (describe "with review that has no top level comment"
        (before-each
          (setf (symbol-function 'github-review-get-pr-info)
                (lambda (_ cb)
                  (funcall cb
                           (a-alist
                            'data (a-alist
                                   'repository
                                   (a-alist
                                    'pullRequest
                                    (a-alist
                                     'title  "title\nin\nthree\nlines"
                                     'bodyText "body\npart"
                                     'reviews (a-alist
                                               'nodes
                                               (list (a-alist
                                                      'author (a-alist 'login "babar")
                                                      'bodyText "LGTM"
                                                      'state "APPROVED")))
                                     'headRef (a-alist
                                               'target
                                               (a-alist
                                                'oid
                                                "examplesha"))))))))))
        (it "does not show it"
          (deferred:sync! (github-review-start "https://github.com/charignon/github-review/pull/6"))
          (expect diff :to-equal simple-context-expected-review-with-review))))))

(describe "Api host computation"
  (it "defaults to api.github.com"
    (expect (github-review-api-host '()) :to-equal "api.github.com"))
  (it "can be overriden"
    (expect (github-review-api-host (a-alist 'apihost "api.github.biz")) :to-equal "api.github.biz")))

(provide 'github-review-test)

;;; github-review-test.el ends here
