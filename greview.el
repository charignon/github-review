;;; greview.el --- Github based code review with emacs -*- lexical-binding: t -*-
;; Author: Laurent Charignon <l.charignon@gmail.com>
;; Maintainer: Laurent Charignon <l.charignon@gmail.com>
;; Version: 1.0
;; Package-Requires: (dependencies)
;; Keywords: tools

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

;; Github based code review with Emacs

;;; Code:

(require 'ghub)
(require 's)
(require 'dash)

;;;;;;;;;;;;;;;;;;;
;; Customization ;;
;;;;;;;;;;;;;;;;;;;

(defgroup greview nil
  "Write and submit github code reviews from within Emacs."
  :group 'tools)

(defcustom greview-review-folder "/tmp"
  "Folder in which to store the code review files."
  :group 'greview
  :type 'string)

(defcustom greview-host "api.github.com"
  "Host for the github api."
  :group 'greview
  :type 'string)

(defconst diffheader '(("Accept" . "application/vnd.github.v3.diff")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Alist utilities to treat associative lists as immutable data structures  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun a/copy (alist)
  "Return a copy of an alist ALIST."
  (copy-alist alist))

;; Assoc into an alist
(defun a/assoc (alist key value)
  "Return copy of ALIST where KEY is associated with VALUE."
  (let* ((dup (a/copy alist)))
    (setf (alist-get key dup) value)
    dup))

(defun a/dissoc (alist key)
  "Return copy of ALIST where KEY is removed."
  (let* ((dup (a/copy alist)))
    (setf (alist-get key dup nil t) nil)
    dup))

(defun a/get (alist key)
  "Return value associated with KEY in ALIST."
  (alist-get key alist))

(defun a/empty ()
  "Return an empty alist."
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication with github ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-pr (pr-alist needs-diff callback)
  "Get a pull request or its diff.
PR-ALIST is an alist representing a PR,
NEEDS-DIFF t to return a diff nil to return the pr object
CALLBACK to call back when done."
  (ghub-get (format "/repos/%s/%s/pulls/%s"
                    (a/get pr-alist 'owner )
                    (a/get pr-alist 'repo )
                    (a/get pr-alist 'num )) nil
                    :unpaginate t
                    :headers (if needs-diff diffheader '())
                    :auth 'greview
                    :host greview-host
                    :callback callback))

(defun get-pr-object (pr-alist callback)
  "Get a pr object given PR-ALIST an alist representing a PR.
CALLBACK is called with the result"
  (get-pr pr-alist nil callback))

(defun get-pr-diff (pr-alist callback)
  "Get the diff for a pr, given PR-ALIST an alist representing a PR.
CALLBACK is called with the result"
  (get-pr pr-alist t callback))


(defun post-review (pr-alist review callback)
  "Submit a code review.
PR-ALIST is an alist representing a PR
REVIEW is the review alist
CALLBACK will be called back when done"
  (ghub-post (format "/repos/%s/%s/pulls/%s/reviews"
                     (a/get pr-alist 'owner )
                     (a/get pr-alist 'repo )
                     (a/get pr-alist 'num )) nil
                     :auth 'greview
                     :payload review
                     :host greview-host
                     :callback callback))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code review file parsing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hunk? (l)
  "Return t if L, a string mark the start of a hunk."
  (string-prefix-p "@@" l))

(defun start-of-file? (l)
  "Return t if L, a string mark the start of a file."
  (string-prefix-p "+++" l))

(defun comment? (l)
  "Return t if L, a string, is a comment."
  (string-prefix-p "# " l))

(defun previous-comment? (l)
  "Return t if L, a string, is a comment from previous review."
  (string-prefix-p "~ " l))

(defun file-path (l)
  "Extract the file path in L, a string.
L should looks like +++ b/content/reference/google-closure-library.adoc"
  (substring l 6))

(defun comment-text (l)
  "Extract the text from L, a string representing a comment."
  (substring l 2))

(defun merge-comment (acc new-comment)
  "Reducing function to merge comments together.
ACC is an alist representing the state of the reduction
NEW-COMMENT is a comment to consider"
  (let* ((lastcomment (a/get acc 'lastcomment))
         (merged (a/get acc 'merged)))
    (cond
     ;; First comment encountered
     ((equal nil lastcomment)
      (a/assoc acc 'lastcomment new-comment))

     ;; Can merge the current comment with the last comment
     ;; if they have the same fields but different content
     ;; that is, if they talk about the same file and position
     ((and (equal (a/get new-comment 'path) (a/get lastcomment 'path))
           (equal (a/get new-comment 'position) (a/get lastcomment 'position)))
      (let* ((new-body (concat (a/get lastcomment 'body) "\n" (a/get new-comment 'body))))
        (a/assoc acc 'lastcomment (a/assoc new-comment 'body new-body))))

     ;; Cannot merge the current comment with the last comment
     (t (a/assoc (a/assoc acc 'merged (cons lastcomment merged)) 'lastcomment new-comment)))))


(defun merge-comments (comments)
  "Takes COMMENTS, inline comments and return a merged list of comments.
COMMENTS on the same file, same pos are coallesced"
  (let* ((acc (-> (a/empty) (a/assoc 'lastcomment nil) (a/assoc 'merged '())))
         (acc-reduced (-reduce-from #'merge-comment acc comments)))
    (cons (a/get acc-reduced 'lastcomment)
          (a/get acc-reduced 'merged))))

(defun normalize-comment (c)
  "Normalize the order of entries in the alist C, representing a comment.
needed to avoid writing convoluted tests"
  `((position . ,(a/get c 'position))
    (body . ,(a/get c 'body))
    (path . ,(a/get c 'path))))

(defun parse-review-lines (lines)
  "Parse LINES corresponding to a code review."
  (defun parse-line (acc l)
    "Reducer function to parse lines in a code review.
parse, goes through lines in a diff return an alist with body and comments
L is a line from the diff.
ACC is an alist accumulating state."
    (let* ((pos (a/get acc 'pos))
           (body (a/get acc 'body))
           (path (a/get acc 'path))
           (comments (a/get acc 'comments))
           (top-level? (equal nil pos))
           (in-file? (not top-level?)))
      (cond
       ;; Previous comments are ignored and don't affect the parsing
       ((previous-comment? l) acc)

       ;; First chunk
       ((and top-level? (hunk? l))
        (a/assoc acc 'pos 0))

       ;; Start of file
       ((start-of-file? l)
        (a/assoc (a/assoc acc 'pos nil) 'path (file-path l)))

       ;; Global Comments
       ((and top-level? (comment? l))
        (a/assoc acc 'body (concat body (comment-text l) "\n")))

       ;; Local Comments
       ((and in-file? (comment? l))
        (a/assoc
         acc
         'comments
         (cons
          (-> (a/empty)
              (a/assoc 'position pos)
              (a/assoc 'path path)
              (a/assoc 'body (comment-text l)))
          comments)))

       ;; Any other line in a file
       (in-file? (a/assoc acc 'pos (+ 1 pos)))

       (t acc))))

  (let* ((acc (-> (a/empty)
                  (a/assoc 'path nil)
                  (a/assoc 'pos nil)
                  (a/assoc 'body "")
                  (a/assoc 'comments ())))
         (parsed-data (-reduce-from #'parse-line acc lines))
         (parsed-comments (a/get parsed-data 'comments))
         (parsed-body (s-trim-right (a/get parsed-data 'body)))
         (merged-comments (if (equal nil parsed-comments)
                              nil
                            (merge-comments (reverse parsed-comments)))))
    (if (equal nil merged-comments)
        `((body . ,parsed-body))
      `((body . ,parsed-body)
        (comments . ,(-map #'normalize-comment merged-comments))))))
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pr-from-fname (buffer-fname)
  "Extract a pr alist from BUFFER-FNAME."
  (let* ((fname (car (last (s-split "/" buffer-fname)))))
    (save-match-data
      (and (string-match "\\(.*\\)___\\(.*\\)___\\(.*\\)\.diff" fname)
           (let* ((pr-alist  (-> (a/empty)
                                 (a/assoc 'owner (match-string 1 fname))
                                 (a/assoc 'repo  (match-string 2 fname))
                                 (a/assoc 'num   (match-string 3 fname)))))
             pr-alist)))))

(defun pr-from-url (url)
  "Extract a pr alist from a pull request URL."
  (save-match-data
    (and (string-match ".*/\\(.*\\)/\\(.*\\)/pull/\\(.*\\)" url)
         (let* ((pr-alist  (-> (a/empty)
                               (a/assoc 'owner (match-string 1 url))
                               (a/assoc 'repo  (match-string 2 url))
                               (a/assoc 'num   (match-string 3 url)))))
           pr-alist))))


(defun save-diff (pr-alist diff)
  "Save a DIFF (string) to a temp file named after pr specified by PR-ALIST."
  (find-file (format "%s/%s___%s___%s.diff"
                     greview-review-folder
                     (a/get pr-alist 'owner)
                     (a/get pr-alist 'repo)
                     (a/get pr-alist 'num)))
  (erase-buffer)
  (insert diff)
  (save-buffer))

(defun parsed-review-from-current-buffer ()
  "Return a code review given the current buffer containing a diff."
  (-> (buffer-substring-no-properties (point-min) (point-max))
      (split-string "\n")
      (parse-review-lines)))

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

(defun submit-review (kind)
  "Submit a code review of KIND.
This function infers the PR name based on the current filename"
  (let* ((pr-alist (pr-from-fname (buffer-file-name)))
         (parsed-review (parsed-review-from-current-buffer)))
  (get-pr-object
   pr-alist
   (lambda (v &rest _)
     (let* ((head-sha (alist-get 'sha (alist-get 'head v)))
            (review   (-> parsed-review
                          (a/assoc 'commit_id head-sha)
                          (a/assoc 'event kind))))
       (post-review
        pr-alist
        review (lambda (value &rest _)
                 (message "OK"))))))))

(defun to-comments (text)
  "Convert TEXT, a string to a string where each line is prefixed by ~."
  (s-join "\n" (-map (lambda (x) (concat "~ " x)) (s-split "\n" text))))

(defun format-diff (diff title body)
  "Formats a diff to save it for review.
DIFF TITLE and BODY are strings"
  (concat
   (to-comments title) "\n~" "\n"
   (to-comments body) "\n"
   diff))

;;;;;;;;;;;;;;;;;;;;;
;; User facing API ;;
;;;;;;;;;;;;;;;;;;;;;

(defun greview/start-review (url)
  "Start review given PR URL."
  (interactive "sPR URL:")
  (let* ((pr-alist (pr-from-url url)))
    (get-pr-diff
     pr-alist
     ;; Get the diff
     (lambda (v &rest _)
       (let ((diff (alist-get 'message v)))
         (get-pr-object
          pr-alist
          ;; Get the title and body
          (lambda (v &rest _)
            (let* ((body (alist-get 'body v))
                   (title (alist-get 'title v))
                   (txt (format-diff diff title body)))
              ;; Write everything to a file
              (save-diff pr-alist txt)))))))))

(defun greview/approve ()
  "Approve a PR (to be run from a buffer corresponding to a review)."
  (interactive)
  (submit-review "APPROVE"))

(defun greview/reject ()
  "Reject a PR (to be run from a buffer corresponding to a review)."
  (interactive)
  (submit-review "REQUEST_CHANGES"))

(defun greview/comment ()
  "Comment on a PR (to be run from a buffer corresponding to a review)."
  (interactive)
  (submit-review "COMMENT"))

(provide 'greview)

;;; greview.el ends here
