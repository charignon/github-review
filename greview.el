;;; greview.el --- GitHub based code review -*- lexical-binding: t -*-
;; Author: Laurent Charignon <l.charignon@gmail.com>
;; Maintainer: Laurent Charignon <l.charignon@gmail.com>
;; Keywords: git, tools, vc, github
;; Homepage: https://github.com/charignon/greview
;; Package-Requires: ((emacs "25") (s "1.12.0") (ghub "2.0") (dash "2.11.0"))
;; Package-Version: 0.1

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;; Commentary:
;;
;;`greview` lets you submit GitHub code review with Emacs.
;;
;; With `greview-start-review` you can pull the content of a pull request into a buffer
;; as a diff with comments corresponding to the PR description.
;; In that buffer you can add comment (global and inline) that you want to make on the pull request.
;; Once done, you can submit these comments as a code review with one of:
;; - `greview-approve`
;; - `greview-comment`
;; - `greview-reject`.

;;; Code:

(require 'ghub)
(require 's)
(require 'dash)

;;;;;;;;;;;;;;;;;;;
;; Customization ;;
;;;;;;;;;;;;;;;;;;;

(defgroup greview nil
  "Write and submit GitHub code reviews from within Emacs."
  :group 'tools)

(defcustom greview-review-folder "/tmp"
  "Folder in which to store the code review files."
  :group 'greview
  :type 'string)

(defcustom greview-host "api.github.com"
  "Host for the GitHub api."
  :group 'greview
  :type 'string)

(defconst greview-diffheader '(("Accept" . "application/vnd.github.v3.diff"))
  "Header for requesting diffs from GitHub.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Alist utilities to treat associative lists as immutable data structures  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun greview-a-copy (alist)
  "Return a copy of an alist ALIST."
  (copy-alist alist))

(defun greview-a-assoc (alist key value)
  "Return copy of ALIST where KEY is associated with VALUE."
  (let* ((dup (greview-a-copy alist)))
    (setf (alist-get key dup) value)
    dup))

(defun greview-a-dissoc (alist key)
  "Return copy of ALIST where KEY is removed."
  (let* ((dup (greview-a-copy alist)))
    (setf (alist-get key dup nil t) nil)
    dup))

(defun greview-a-get (alist key)
  "Return value associated with KEY in ALIST."
  (alist-get key alist))

(defun greview-a-empty ()
  "Return an empty alist."
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication with GitHub ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun greview-get-pr (pr-alist needs-diff callback)
  "Get a pull request or its diff.
PR-ALIST is an alist representing a PR,
NEEDS-DIFF t to return a diff nil to return the pr object
CALLBACK to call back when done."
  (ghub-get (format "/repos/%s/%s/pulls/%s"
                    (greview-a-get pr-alist 'owner )
                    (greview-a-get pr-alist 'repo )
                    (greview-a-get pr-alist 'num )) nil
                    :unpaginate t
                    :headers (if needs-diff greview-diffheader '())
                    :auth 'greview
                    :host greview-host
                    :callback callback
                    :errorback (lambda (&rest _) (message "Error talking to GitHub"))))

(defun greview-get-pr-object (pr-alist callback)
  "Get a pr object given PR-ALIST an alist representing a PR.
CALLBACK is called with the result"
  (greview-get-pr pr-alist nil callback))

(defun greview-get-pr-diff (pr-alist callback)
  "Get the diff for a pr, given PR-ALIST an alist representing a PR.
CALLBACK is called with the result"
  (greview-get-pr pr-alist t callback))


(defun greview-post-review (pr-alist review callback)
  "Submit a code review.
PR-ALIST is an alist representing a PR
REVIEW is the review alist
CALLBACK will be called back when done"
  (ghub-post (format "/repos/%s/%s/pulls/%s/reviews"
                     (greview-a-get pr-alist 'owner )
                     (greview-a-get pr-alist 'repo )
                     (greview-a-get pr-alist 'num )) nil
                     :auth 'greview
                     :payload review
                     :host greview-host
                     :errorback (lambda (&rest _) (message "Error talking to GitHub"))
                     :callback callback))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code review file parsing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun greview-hunk? (l)
  "Return t if L, a string mark the start of a greview-hunk."
  (string-prefix-p "@@" l))

(defun greview-start-of-file? (l)
  "Return t if L, a string mark the start of a file."
  (string-prefix-p "+++" l))

(defun greview-comment? (l)
  "Return t if L, a string, is a comment."
  (string-prefix-p "# " l))

(defun greview-previous-comment? (l)
  "Return t if L, a string, is a comment from previous review."
  (string-prefix-p "~ " l))

(defun greview-file-path (l)
  "Extract the file path in L, a string.
L should looks like +++ b/content/reference/google-closure-library.adoc"
  (substring l 6))

(defun greview-comment-text (l)
  "Extract the text from L, a string representing a comment."
  (substring l 2))

(defun greview-merge-comment (acc new-comment)
  "Reducing function to merge comments together.
ACC is an alist representing the state of the reduction
NEW-COMMENT is a comment to consider"
  (let* ((lastcomment (greview-a-get acc 'lastcomment))
         (merged (greview-a-get acc 'merged)))
    (cond
     ;; First comment encountered
     ((equal nil lastcomment)
      (greview-a-assoc acc 'lastcomment new-comment))

     ;; Can merge the current comment with the last comment
     ;; if they have the same fields but different content
     ;; that is, if they talk about the same file and position
     ((and (equal (greview-a-get new-comment 'path) (greview-a-get lastcomment 'path))
           (equal (greview-a-get new-comment 'position) (greview-a-get lastcomment 'position)))
      (let* ((new-body (concat (greview-a-get lastcomment 'body) "\n" (greview-a-get new-comment 'body))))
        (greview-a-assoc acc 'lastcomment (greview-a-assoc new-comment 'body new-body))))

     ;; Cannot merge the current comment with the last comment
     (t (greview-a-assoc (greview-a-assoc acc 'merged (cons lastcomment merged)) 'lastcomment new-comment)))))


(defun greview-merge-comments (comments)
  "Takes COMMENTS, inline comments and return a merged list of comments.
COMMENTS on the same file, same pos are coallesced"
  (let* ((acc (-> (greview-a-empty) (greview-a-assoc 'lastcomment nil) (greview-a-assoc 'merged '())))
         (acc-reduced (-reduce-from #'greview-merge-comment acc comments)))
    (cons (greview-a-get acc-reduced 'lastcomment)
          (greview-a-get acc-reduced 'merged))))

(defun greview-normalize-comment (c)
  "Normalize the order of entries in the alist C, representing a comment.
needed to avoid writing convoluted tests"
  `((position . ,(greview-a-get c 'position))
    (body . ,(greview-a-get c 'body))
    (path . ,(greview-a-get c 'path))))

(defun greview-parse-line (acc l)
    "Reducer function to parse lines in a code review.
parse, goes through lines in a diff return an alist with body and comments
L is a line from the diff.
ACC is an alist accumulating state."
    (let* ((pos (greview-a-get acc 'pos))
           (body (greview-a-get acc 'body))
           (path (greview-a-get acc 'path))
           (comments (greview-a-get acc 'comments))
           (top-level? (equal nil pos))
           (in-file? (not top-level?)))
      (cond
       ;; Previous comments are ignored and don't affect the parsing
       ((greview-previous-comment? l) acc)

       ;; First cgreview-hunk
       ((and top-level? (greview-hunk? l))
        (greview-a-assoc acc 'pos 0))

       ;; Start of file
       ((greview-start-of-file? l)
        (greview-a-assoc (greview-a-assoc acc 'pos nil) 'path (greview-file-path l)))

       ;; Global Comments
       ((and top-level? (greview-comment? l))
        (greview-a-assoc acc 'body (concat body (greview-comment-text l) "\n")))

       ;; Local Comments
       ((and in-file? (greview-comment? l))
        (greview-a-assoc
         acc
         'comments
         (cons
          (-> (greview-a-empty)
              (greview-a-assoc 'position pos)
              (greview-a-assoc 'path path)
              (greview-a-assoc 'body (greview-comment-text l)))
          comments)))

       ;; Any other line in a file
       (in-file? (greview-a-assoc acc 'pos (+ 1 pos)))

       (t acc))))

(defun greview-parse-review-lines (lines)
  "Parse LINES corresponding to a code review."
  (let* ((acc (-> (greview-a-empty)
                  (greview-a-assoc 'path nil)
                  (greview-a-assoc 'pos nil)
                  (greview-a-assoc 'body "")
                  (greview-a-assoc 'comments ())))
         (parsed-data (-reduce-from #'greview-parse-line acc lines))
         (parsed-comments (greview-a-get parsed-data 'comments))
         (parsed-body (s-trim-right (greview-a-get parsed-data 'body)))
         (merged-comments (if (equal nil parsed-comments)
                              nil
                            (greview-merge-comments (reverse parsed-comments)))))
    (if (equal nil merged-comments)
        `((body . ,parsed-body))
      `((body . ,parsed-body)
        (comments . ,(-map #'greview-normalize-comment merged-comments))))))
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun greview-pr-from-fname (buffer-fname)
  "Extract a pr alist from BUFFER-FNAME."
  (let* ((fname (car (last (s-split "/" buffer-fname)))))
    (save-match-data
      (and (string-match "\\(.*\\)___\\(.*\\)___\\([0-9]+\\)\.diff" fname)
           (let* ((pr-alist  (-> (greview-a-empty)
                                 (greview-a-assoc 'owner (match-string 1 fname))
                                 (greview-a-assoc 'repo  (match-string 2 fname))
                                 (greview-a-assoc 'num   (match-string 3 fname)))))
             pr-alist)))))

(defun greview-pr-from-url (url)
  "Extract a pr alist from a pull request URL."
  (save-match-data
    (and (string-match ".*/\\(.*\\)/\\(.*\\)/pull/\\([0-9]+\\)" url)
         (let* ((pr-alist  (-> (greview-a-empty)
                               (greview-a-assoc 'owner (match-string 1 url))
                               (greview-a-assoc 'repo  (match-string 2 url))
                               (greview-a-assoc 'num   (match-string 3 url)))))
           pr-alist))))


(defun greview-save-diff (pr-alist diff)
  "Save a DIFF (string) to a temp file named after pr specified by PR-ALIST."
  (find-file (format "%s/%s___%s___%s.diff"
                     greview-review-folder
                     (greview-a-get pr-alist 'owner)
                     (greview-a-get pr-alist 'repo)
                     (greview-a-get pr-alist 'num)))
  (erase-buffer)
  (insert diff)
  (save-buffer))

(defun greview-parsed-review-from-current-buffer ()
  "Return a code review given the current buffer containing a diff."
  (-> (buffer-substring-no-properties (point-min) (point-max))
      (split-string "\n")
      (greview-parse-review-lines)))

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

(defun greview-submit-review (kind)
  "Submit a code review of KIND.
This function infers the PR name based on the current filename"
  (let* ((pr-alist (greview-pr-from-fname (buffer-file-name)))
         (parsed-review (greview-parsed-review-from-current-buffer)))
  (message "Submitting review, this may take a while ...")
  (greview-get-pr-object
   pr-alist
   (lambda (v &rest _)
     (let* ((head-sha (greview-a-get (greview-a-get v 'head) 'sha))
            (review   (-> parsed-review
                          (greview-a-assoc 'commit_id head-sha)
                          (greview-a-assoc 'event kind))))
       (greview-post-review
        pr-alist
        review (lambda (&rest _)
                 (message "Done submitting review"))))))))

(defun greview-to-comments (text)
  "Convert TEXT, a string to a string where each line is prefixed by ~."
  (s-join "\n" (-map (lambda (x) (concat "~ " x)) (s-split "\n" text))))

(defun greview-format-diff (diff title body)
  "Formats a diff to save it for review.
DIFF TITLE and BODY are strings"
  (concat
   (greview-to-comments title) "\n~" "\n"
   (greview-to-comments body) "\n"
   diff))

;;;;;;;;;;;;;;;;;;;;;
;; User facing API ;;
;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun greview-start-review (url)
  "Start review given PR URL."
  (interactive "sPR URL:")
  (let* ((pr-alist (greview-pr-from-url url)))
    (greview-get-pr-diff
     pr-alist
     ;; Get the diff
     (lambda (v &rest _)
       (let ((diff (greview-a-get v 'message)))
         (greview-get-pr-object
          pr-alist
          ;; Get the title and body
          (lambda (v &rest _)
            (let* ((body (greview-a-get v 'body))
                   (title (greview-a-get v 'title))
                   (txt (greview-format-diff diff title body)))
              ;; Write everything to a file
              (greview-save-diff pr-alist txt)))))))))

;;;###autoload
(defun greview-approve ()
  "Approve a PR (to be run from a buffer corresponding to a review)."
  (interactive)
  (greview-submit-review "APPROVE"))

;;;###autoload
(defun greview-reject ()
  "Reject a PR (to be run from a buffer corresponding to a review)."
  (interactive)
  (greview-submit-review "REQUEST_CHANGES"))

;;;###autoload
(defun greview-comment ()
  "Comment on a PR (to be run from a buffer corresponding to a review)."
  (interactive)
  (greview-submit-review "COMMENT"))

(provide 'greview)

;;; greview.el ends here
