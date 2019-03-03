(load-file "./test/test-helper.el")
(require 'github-review)
(require 'buttercup)

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
      (expect actual :to-equal complex-review-expected))))


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
              (owner . "charignon")))))

(describe "github-review-format-diff"
  (it "can format a diff"
    (expect (github-review-format-diff "a\nb\nc" "title\nin\nthree\nlines" "body\npart") :to-equal
            "~ title
~ in
~ three
~ lines
~
~ body
~ part
a
b
c")))
