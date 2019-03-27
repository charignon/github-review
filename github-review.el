;;; github-review.el --- GitHub based code review -*- lexical-binding: t -*-
;; Author: Laurent Charignon <l.charignon@gmail.com>
;; Maintainer: Laurent Charignon <l.charignon@gmail.com>
;; Keywords: git, tools, vc, github
;; Homepage: https://github.com/charignon/github-review
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
;;`github-review` lets you submit GitHub code review with Emacs.
;;
;; With `github-review-start` you can pull the content of a pull request into a buffer
;; as a diff with comments corresponding to the PR description.
;; In that buffer you can add comment (global and inline) that you want to make on the pull request.
;; Once done, you can submit these comments as a code review with one of:
;; - `github-review-approve`
;; - `github-review-comment`
;; - `github-review-reject`.

;;; Code:

(require 'ghub)
(require 's)
(require 'dash)

;;;;;;;;;;;;;;;;;;;
;; Customization ;;
;;;;;;;;;;;;;;;;;;;

(defgroup github-review nil
  "Write and submit GitHub code reviews from within Emacs."
  :group 'tools)

(defcustom github-review-review-folder "/tmp"
  "Folder in which to store the code review files."
  :group 'github-review
  :type 'string)

(defcustom github-review-host "api.github.com"
  "Host for the GitHub api."
  :group 'github-review
  :type 'string)

(defcustom github-review-fetch-top-level-and-review-comments nil
  "If t, fetch the top level and review comments."
  :group 'github-review
  :type 'boolean)

(defconst github-review-diffheader '(("Accept" . "application/vnd.github.v3.diff"))
  "Header for requesting diffs from GitHub.")

;; Only repo scope needed to read PRs and submit reviews
(defvar github-review-github-token-scopes '(repo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Alist utilities to treat associative lists as immutable data structures  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun github-review-a-copy (alist)
  "Return a copy of an alist ALIST."
  (copy-alist alist))

(defun github-review-a-assoc (alist key value)
  "Return copy of ALIST where KEY is associated with VALUE."
  (let* ((dup (github-review-a-copy alist)))
    (setf (alist-get key dup) value)
    dup))

(defun github-review-a-dissoc (alist key)
  "Return copy of ALIST where KEY is removed."
  (let* ((dup (github-review-a-copy alist)))
    (setf (alist-get key dup nil t) nil)
    dup))

(defun github-review-a-get (alist key)
  "Return value associated with KEY in ALIST."
  (alist-get key alist))

(defun github-review-a-empty ()
  "Return an empty alist."
  '())

(defconst github-review-url-scheme
  '((get-pr . "/repos/%s/%s/pulls/%s")
    (get-inline-comments . "/repos/%s/%s/pulls/%s/comments")
    (get-review-comments . "/repos/%s/%s/pulls/%s/reviews")
    (get-issue-comments . "/repos/%s/%s/issues/%s/comments")
    (submit-review . "/repos/%s/%s/pulls/%s/reviews")))

(defun github-review-format-pr-url (kind pr-alist)
  "Format a url for accessing the pr.
KIND is the kind of information to request.
PR-ALIST is an alist represenging the PR"
  (format (github-review-a-get github-review-url-scheme kind)
          (github-review-a-get pr-alist 'owner )
          (github-review-a-get pr-alist 'repo )
          (github-review-a-get pr-alist 'num )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication with GitHub ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun github-review-get-pr (pr-alist needs-diff callback)
  "Get a pull request or its diff.
PR-ALIST is an alist representing a PR,
NEEDS-DIFF t to return a diff nil to return the pr object
CALLBACK to call back when done."
  (ghub-get (github-review-format-pr-url 'get-pr pr-alist)
             nil
             :unpaginate t
             :headers (if needs-diff github-review-diffheader '())
             :auth 'github-review
             :host github-review-host
             :callback callback
             :errorback (lambda (&rest _) (message "Error talking to GitHub"))))

(defun github-review-get-pr-object (pr-alist callback)
  "Get a pr object given PR-ALIST an alist representing a PR.
CALLBACK is called with the result"
  (github-review-get-pr pr-alist nil callback))

(defun github-review-get-pr-diff (pr-alist callback)
  "Get the diff for a pr, given PR-ALIST an alist representing a PR.
CALLBACK is called with the result"
  (github-review-get-pr pr-alist t callback))

(defun github-review-post-review (pr-alist review callback)
  "Submit a code review.
PR-ALIST is an alist representing a PR
REVIEW is the review alist
CALLBACK will be called back when done"
  (ghub-post (github-review-format-pr-url 'submit-review pr-alist)
             nil
             :auth 'github-review
             :payload review
             :host github-review-host
             :errorback (lambda (&rest _) (message "Error talking to GitHub"))
             :callback callback))

(defun github-review-get-inline-comments (pr-alist callback)
  "Get the inline comments on a pr.
PR-ALIST is an alist representing a PR
CALLBACK will be called back when done"
  (ghub-get (github-review-format-pr-url 'get-inline-comments pr-alist)
            nil
            :auth 'github-review
            :host github-review-host
            :errorback (lambda (&rest _) (message "Error talking to GitHub"))
            :callback callback))

(defun github-review-get-reviews (pr-alist callback)
  "Get the code reviews on a PR.
PR-ALIST is an alist representing a PR
CALLBACK will be called back when done"
  (ghub-get (github-review-format-pr-url 'get-review-comments pr-alist)
            nil
            :auth 'github-review
            :host github-review-host
            :errorback (lambda (&rest _) (message "Error talking to GitHub"))
            :callback callback))

(defun github-review-get-issue-comments (pr-alist callback)
  "Get the top level comments on a PR.
PR-ALIST is an alist representing a PR
CALLBACK will be called back when done"
  (ghub-get (github-review-format-pr-url 'get-issue-comments pr-alist)
            nil
            :auth 'github-review
            :host github-review-host
            :errorback (lambda (&rest _) (message "Error talking to GitHub"))
            :callback callback))

(defun github-review-chain-call (arg ctx cb)
  "Run one function of a chain of functions.
See `github-review-chain-calls' for more information.
ARG is passed as the first argument of every callback in the chain.
CB is called if the chain is done.
CTX is propagated between calls in the chain.
See github-review-chain-api-calls for more information"
  (let ((chain (github-review-a-get ctx 'chain)))
  (if (eq nil chain)
    (funcall cb ctx) ;; Last function in the chain
    (let* ((nextnode (car chain))
           (fn (github-review-a-get nextnode 'function))
           (fncallback (github-review-a-get nextnode 'callback))
           (key (github-review-a-get nextnode 'key))
           (newctx (github-review-a-assoc ctx 'chain (cdr chain))))
      (funcall fn arg
               (lambda (v &rest _)
                 (github-review-chain-call
                  arg
                 (if key
                     (github-review-a-assoc newctx key v)
                     (funcall fncallback v newctx))
                 cb)))))))

(defun github-review-chain-calls (arg cb chain)
  "Run a chain of functions.
CHAIN is a list of ALIST containing 'function and ('callback or 'key)
callback are called with ARG and a CTX (alist) in that order.
KEY can be used instead of callback if the result should be stored in key.
The result of the call to the callback should be the new CTX.
ARG is the first argument that every function in the chain will be taking
CB is the final callback to call with the resulting aggregate object

For an example of how to use it, look at the tests"
  (github-review-chain-call arg `((chain . ,chain)) cb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code review file parsing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun github-review-hunk? (l)
  "Return t if L, a string mark the start of a github-review-hunk."
  (string-prefix-p "@@" l))

(defun github-review-non-null-filename-hunk-line? (l)
  "Return t if L, a string is filename hunk like not representing /dev/null."
  (and (or (string-prefix-p "+++" l)
           (string-prefix-p "---" l))
       (not (string-prefix-p "/dev/null" (substring l 4)))))

(defun github-review-comment? (l)
  "Return t if L, a string, is a comment."
  (string-prefix-p "# " l))

(defun github-review-previous-comment? (l)
  "Return t if L, a string, is a comment from previous review."
  (string-prefix-p "~ " l))

(defun github-review-file-path (l)
  "Extract the file path in L, a string.
L should looks like +++ b/content/reference/google-closure-library.adoc"
  (substring l 6))

(defun github-review-comment-text (l)
  "Extract the text from L, a string representing a comment."
  (substring l 2))

(defun github-review-merge-comment (acc new-comment)
  "Reducing function to merge comments together.
ACC is an alist representing the state of the reduction
NEW-COMMENT is a comment to consider"
  (let* ((lastcomment (github-review-a-get acc 'lastcomment))
         (merged (github-review-a-get acc 'merged)))
    (cond
     ;; First comment encountered
     ((equal nil lastcomment)
      (github-review-a-assoc acc 'lastcomment new-comment))

     ;; Can merge the current comment with the last comment
     ;; if they have the same fields but different content
     ;; that is, if they talk about the same file and position
     ((and (equal (github-review-a-get new-comment 'path) (github-review-a-get lastcomment 'path))
           (equal (github-review-a-get new-comment 'position) (github-review-a-get lastcomment 'position)))
      (let* ((new-body (concat (github-review-a-get lastcomment 'body) "\n" (github-review-a-get new-comment 'body))))
        (github-review-a-assoc acc 'lastcomment (github-review-a-assoc new-comment 'body new-body))))

     ;; Cannot merge the current comment with the last comment
     (t (github-review-a-assoc (github-review-a-assoc acc 'merged (cons lastcomment merged)) 'lastcomment new-comment)))))

(defun github-review-merge-comments (comments)
  "Takes COMMENTS, inline comments and return a merged list of comments.
COMMENTS on the same file, same pos are coallesced"
  (let* ((acc (-> (github-review-a-empty) (github-review-a-assoc 'lastcomment nil) (github-review-a-assoc 'merged '())))
         (acc-reduced (-reduce-from #'github-review-merge-comment acc comments)))
    (cons (github-review-a-get acc-reduced 'lastcomment)
          (github-review-a-get acc-reduced 'merged))))

(defun github-review-normalize-comment (c)
  "Normalize the order of entries in the alist C, representing a comment.
needed to avoid writing convoluted tests"
  `((position . ,(github-review-a-get c 'position))
    (body . ,(github-review-a-get c 'body))
    (path . ,(github-review-a-get c 'path))))

(defun github-review-parse-line (acc l)
  "Reducer function to parse lines in a code review.
Analyzes one line in a diff return an alist with two entries: body and comments
L is a line from the diff.
ACC is an alist accumulating parsing state."
  (let* ((pos (github-review-a-get acc 'pos))
         (body (github-review-a-get acc 'body))
         (path (github-review-a-get acc 'path))
         (comments (github-review-a-get acc 'comments))
         (top-level? (equal nil pos))
         (in-file? (not top-level?)))
    (cond
     ;; Previous comments are ignored and don't affect the parsing
     ((github-review-previous-comment? l) acc)

     ;; First cgithub-review-hunk
     ((and top-level? (github-review-hunk? l))
      (github-review-a-assoc acc 'pos 0))

     ;; Start of file
     ((github-review-non-null-filename-hunk-line? l)
      (github-review-a-assoc (github-review-a-assoc acc 'pos nil) 'path (github-review-file-path l)))

     ;; Global Comments
     ((and top-level? (github-review-comment? l))
      (github-review-a-assoc acc 'body (concat body (github-review-comment-text l) "\n")))

     ;; Local Comments
     ((and in-file? (github-review-comment? l))
      (github-review-a-assoc
       acc
       'comments
       (cons
        (-> (github-review-a-empty)
            (github-review-a-assoc 'position pos)
            (github-review-a-assoc 'path path)
            (github-review-a-assoc 'body (github-review-comment-text l)))
        comments)))

     ;; Any other line in a file
     (in-file? (github-review-a-assoc acc 'pos (+ 1 pos)))

     (t acc))))

(defun github-review-parse-review-lines (lines)
  "Parse LINES corresponding to a code review."
  (let* ((acc (-> (github-review-a-empty)
                  (github-review-a-assoc 'path nil)
                  (github-review-a-assoc 'pos nil)
                  (github-review-a-assoc 'body "")
                  (github-review-a-assoc 'comments ())))
         (parsed-data (-reduce-from #'github-review-parse-line acc lines))
         (parsed-comments (github-review-a-get parsed-data 'comments))
         (parsed-body (s-trim-right (github-review-a-get parsed-data 'body)))
         (merged-comments (if (equal nil parsed-comments)
                              nil
                            (github-review-merge-comments (reverse parsed-comments)))))
    (if (equal nil merged-comments)
        `((body . ,parsed-body))
      `((body . ,parsed-body)
        (comments . ,(-map #'github-review-normalize-comment merged-comments))))))
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun github-review-pr-from-fname (buffer-fname)
  "Extract a pr alist from BUFFER-FNAME."
  (let* ((fname (car (last (s-split "/" buffer-fname)))))
    (save-match-data
      (and (string-match "\\(.*\\)___\\(.*\\)___\\([0-9]+\\)\.diff" fname)
           (let* ((pr-alist  (-> (github-review-a-empty)
                                 (github-review-a-assoc 'owner (match-string 1 fname))
                                 (github-review-a-assoc 'repo  (match-string 2 fname))
                                 (github-review-a-assoc 'num   (match-string 3 fname)))))
             pr-alist)))))

(defun github-review-pr-from-url (url)
  "Extract a pr alist from a pull request URL."
  (save-match-data
    (and (string-match ".*/\\(.*\\)/\\(.*\\)/pull/\\([0-9]+\\)" url)
         (let* ((pr-alist  (-> (github-review-a-empty)
                               (github-review-a-assoc 'owner (match-string 1 url))
                               (github-review-a-assoc 'repo  (match-string 2 url))
                               (github-review-a-assoc 'num   (match-string 3 url)))))
           pr-alist))))

(defun github-review-save-diff (pr-alist diff)
  "Save a DIFF (string) to a temp file named after pr specified by PR-ALIST."
  (find-file (format "%s/%s___%s___%s.diff"
                     github-review-review-folder
                     (github-review-a-get pr-alist 'owner)
                     (github-review-a-get pr-alist 'repo)
                     (github-review-a-get pr-alist 'num)))
  (erase-buffer)
  (insert diff)
  (save-buffer))

(defun github-review-parsed-review-from-current-buffer ()
  "Return a code review given the current buffer containing a diff."
  (-> (buffer-substring-no-properties (point-min) (point-max))
      (split-string "\n")
      (github-review-parse-review-lines)))

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

(defun github-review-submit-review (kind)
  "Submit a code review of KIND.
This function infers the PR name based on the current filename"
  (let* ((pr-alist (github-review-pr-from-fname (buffer-file-name)))
         (parsed-review (github-review-parsed-review-from-current-buffer)))
    (message "Submitting review, this may take a while ...")
    (github-review-get-pr-object
     pr-alist
     (lambda (v &rest _)
       (let* ((head-sha (github-review-a-get (github-review-a-get v 'head) 'sha))
              (review   (-> parsed-review
                            (github-review-a-assoc 'commit_id head-sha)
                            (github-review-a-assoc 'event kind))))
         (github-review-post-review
          pr-alist
          review (lambda (&rest _)
                   (message "Done submitting review"))))))))

(defun github-review-to-comments (text)
  "Convert TEXT, a string to a string where each line is prefixed by ~."
  (s-join "\n" (-map (lambda (x) (concat "~ " x)) (s-split "\n" text))))

(defun github-review-format-top-level-comment (com)
  "Format a top level COM objectto string."
  (let ((username (github-review-a-get (github-review-a-get com 'user) 'login))
        (body (github-review-a-get com 'body)))
    (format "@%s: %s" username body)))

(defun github-review-format-review (review)
  "Format a REVIEW object to string."
  (let ((username (github-review-a-get (github-review-a-get review 'user) 'login))
        (state (github-review-a-get review 'state))
        (body (github-review-a-get review 'body)))
    (format "Reviewed by @%s[%s]: %s" username state body)))

(defun github-review-format-diff (ctx)
  "Formats a diff to save it for review.
CTX is the result of a callback chain to get information about a PR.
See ‘github-review-start’ for more information"
  (let* ((ob (github-review-a-get ctx 'object))
         (title (github-review-a-get ob 'title))
         (body (github-review-a-get ob 'body))
         (top-level-comments (github-review-a-get ctx 'top-level-comments))
         (reviews (-reject
                   (lambda (x)
                     (string= (github-review-a-get x 'body) ""))
                   (github-review-a-get ctx 'reviews)))
         (diff (-> ctx (github-review-a-get 'diff) (github-review-a-get 'message))))
  (concat
   (github-review-to-comments title)
   "\n~"
   "\n"
   ;; Github PR body contains \n\r for new lines
   (github-review-to-comments (s-replace "\r" "" body))
   "\n"
   (when top-level-comments
     (concat (s-join
              "\n"
              (-map
               #'github-review-to-comments
               (-map #'github-review-format-top-level-comment top-level-comments)))
             "\n"))
   (when reviews
     (concat (s-join
              "\n"
              (-map
               #'github-review-to-comments
               (-map #'github-review-format-review reviews)))
             "\n"))
   diff)))

;;;;;;;;;;;;;;;;;;;;;
;; User facing API ;;
;;;;;;;;;;;;;;;;;;;;;

(defun github-review-chain ()
  "Return alist representing sequence of operations to get a PR's information.
Gets the PR diff, object, top level comments, and code reviews."
  ;; Get the diff
  `(((function . github-review-get-pr-diff)
     (key . diff))

    ;; ... the PR object
    ((function . github-review-get-pr-object)
     (callback . (lambda (v ctx &rest _)
                   (let* ((comms (github-review-a-get v 'comments))
                          (review_comments (github-review-a-get v 'review_comments))
                          (chain (github-review-a-get v 'chain)))
                     ;; And top level comments if there is any
                     (when (and github-review-fetch-top-level-and-review-comments
                                (> comms 0))
                       (setq chain (cons `((function . github-review-get-issue-comments)
                                           (key . top-level-comments))
                                         chain)))
                     ;; And code review if there is any
                     (when (and github-review-fetch-top-level-and-review-comments
                                (> review_comments 0))
                       (setq chain (cons `((function . github-review-get-reviews)
                                           (key . reviews))
                                         chain)))
                     (-> ctx
                         (github-review-a-assoc 'object v)
                         (github-review-a-assoc 'chain chain))))))))


(defun github-review-start-internal (pr-alist)
  "Start review given PR URL given PR-ALIST."
    (github-review-chain-calls
     pr-alist
     ;; Callback when done
     (lambda (ctx)
       (github-review-save-diff
        pr-alist
        (github-review-format-diff ctx)))

     (github-review-chain)
     ))

;;;###autoload
(defun github-review-forge-pr-at-point ()
  "Review the forge pull request at point."
  (interactive)
  (let* ((pullreq (forge-pullreq-at-point))
         (repo (forge-get-repository pullreq))
         (owner (oref repo owner))
         (name (oref repo name))
         (number (oref pullreq number))
         (pr-alist (-> (github-review-a-empty)
                       (github-review-a-assoc 'owner owner)
                       (github-review-a-assoc 'repo  name)
                       (github-review-a-assoc 'num   number))))
         (github-review-start-internal pr-alist)))

;;;###autoload
(defun github-review-start (url)
  "Start review given PR URL."
  (interactive "sPR URL: ")
  (let* ((pr-alist (github-review-pr-from-url url)))
    (github-review-start-internal pr-alist)))


;;;###autoload
(defun github-review-approve ()
  "Approve a PR (to be run from a buffer corresponding to a review)."
  (interactive)
  (github-review-submit-review "APPROVE"))

;;;###autoload
(defun github-review-reject ()
  "Reject a PR (to be run from a buffer corresponding to a review)."
  (interactive)
  (github-review-submit-review "REQUEST_CHANGES"))

;;;###autoload
(defun github-review-comment ()
  "Comment on a PR (to be run from a buffer corresponding to a review)."
  (interactive)
  (github-review-submit-review "COMMENT"))

(provide 'github-review)

;;; github-review.el ends here
