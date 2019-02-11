(require 'greview)
(require 'buttercup)

(describe "greview-hunk?"
  (it "can correctly identify a hunk"
    (expect (greview-hunk? "foo") :to-be nil)
    (expect (greview-hunk? "@@ b/foo") :to-be t)))

(setq examplediff "# This is a global comment at the top of the file
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

(setq example-previous-comments "~ Top level previous comment
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
(setq example-no-comment "# This is a global comment at the top of the file
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

(describe "greview-parse-review-lines"
  (it "can parse a complex code review"
    (let* ((actual (greview-parse-review-lines (split-string examplediff "\n"))))
      (expect actual :to-equal complex-review-expected)))
  (it "can parse a code review with no comment"
    (let* ((actual (greview-parse-review-lines (split-string example-no-comment "\n")))
           (expected '((body . "This is a global comment at the top of the file\nwith multiple\nlines"))))
      (expect actual :to-equal expected)))
  (it "can parse a code review with previous comments but ignores it"
    (let* ((actual (greview-parse-review-lines (split-string example-previous-comments "\n"))))
      (expect actual :to-equal complex-review-expected))))


(describe "greview-pr-from-fname"
  (it "can parse fname and infer pr name"
    (expect (greview-pr-from-fname "/tmp/charignon___testgheapi___2.diff") :to-equal
            '((num . "2")
              (repo . "testgheapi")
              (owner . "charignon")))))

(describe "greview-format-diff"
  (it "can format a diff"
    (expect (greview-format-diff "a\nb\nc" "title\nin\nthree\nlines" "body\npart") :to-equal
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
