(load-file "./test/test-helper.el")
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

    (defconst examplediff "# This is a global comment at the top of the file
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
         ((position . 6)
          (body . "Some other comment inline")
          (path . "content/reference/google-closure-library.adoc"))
         ((position . 5)
          (body . "And a comment inline about\na specific line\n```with some\ncode```")
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
      (it "can parse a code review with previous comments but ignores it"
        (let* ((actual (github-review-parse-review-lines (split-string example-previous-comments "\n"))))
          (expect actual :to-equal complex-review-expected)))))

  (describe "PR name inference from review filename and url"

    (describe "github-review-pr-from-fname"
      (it "can parse fname and infer pr name"
        (expect (github-review-pr-from-fname "/tmp/charignon___testgheapi___2.diff") :to-equal
                '((num . "2")
                  (repo . "testgheapi")
                  (owner . "charignon")))))

    (describe "github-review-pr-from-url"
      (it "can parse url and infer pr details"
        (expect (github-review-pr-from-url "https://github.com/charignon/testgheapi/pull/2") :to-equal
                '((num . "2")
                  (repo . "testgheapi")
                  (owner . "charignon"))))))

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

  (describe "callback logic"

    (describe "github-review-chain-calls"
      (it "can execute a chain of one function"
        (let ((res 8))
          (github-review-chain-calls
           ;; No argument
           nil

           ;; Final callback set res to the content of the 'foo key
           (lambda (ctx)
             (setq res (github-review-a-get ctx 'foo)))

           ;; Function simulate an api call, takes arg and callback
           ;; and populate the 'foo key with the result
           '(((function . (lambda (arg cb) (funcall cb 3)))
              (key . foo))))
          (expect res :to-equal 3)))
      (it "can execute a chain of two functions"
        (let ((res 8))
          (github-review-chain-calls
           ;; No argument
           nil

           ;; Final callback set res to the content of the 'foo key
           (lambda (ctx)
             (setq res (github-review-a-get ctx 'foo)))

           '(
             ;; Function simulate an api call, takes arg and callback
             ;; and populate the 'foo key with the result
             ((function . (lambda (arg cb) (funcall cb 3)))
              (key . foo))
             ;; Function increment the content of the key foo
             ((function . (lambda (arg cb) (funcall cb 42)))
              (callback . (lambda (arg ctx)
                            (github-review-a-assoc ctx
                                                   'foo
                                                   (+ 1
                                                      (github-review-a-get ctx 'foo))))))))
          (expect res :to-equal 4)))))

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
          (github-review-start "https://github.com/charignon/github-review/pull/6")
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
            (github-review-start "https://github.com/charignon/github-review/pull/6")
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
            (github-review-start "https://github.com/charignon/github-review/pull/6")
            (expect diff :to-equal simple-context-expected-review)))))))

(provide 'github-review-test)

;;; github-review-test.el ends here
