;;; github-review.el --- GitHub based code review -*- lexical-binding: t -*-
;; Author: Laurent Charignon <l.charignon@gmail.com>
;; Maintainer: Laurent Charignon <l.charignon@gmail.com>
;; Keywords: git, tools, vc, github
;; Homepage: https://github.com/charignon/github-review
;; Package-Requires: ((emacs "25.1") (s "1.12.0") (ghub "2.0") (dash "2.11.0") (deferred "0.5.1") (a "0.1.1"))
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

(require 'a)
(require 'dash)
(require 'deferred)
(require 'ghub)
(require 's)

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

(defcustom github-review-fetch-top-level-and-review-comments t
  "If t, fetch the top level and review comments."
  :group 'github-review
  :type 'boolean)

(defconst github-review-diffheader '(("Accept" . "application/vnd.github.v3.diff"))
  "Header for requesting diffs from GitHub.")

;; Only repo scope needed to read PRs and submit reviews
(defvar github-review-github-token-scopes '(repo))

(defvar github-review-mode-hook nil
  "Mode hook for `github-review-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Alist utilities to treat associative lists as immutable data structures  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst github-review-url-scheme
  (a-alist 'get-pr "/repos/%s/%s/pulls/%s"
           'get-inline-comments "/repos/%s/%s/pulls/%s/comments"
           'get-review-comments "/repos/%s/%s/pulls/%s/reviews"
           'get-issue-comments "/repos/%s/%s/issues/%s/comments"
           'submit-review "/repos/%s/%s/pulls/%s/reviews"))

(defun github-review-format-pr-url (kind pr-alist)
  "Format a url for accessing the pr.
KIND is the kind of information to request.
PR-ALIST is an alist represenging the PR"
  (let-alist pr-alist
    (format (a-get github-review-url-scheme kind) .owner .repo .num)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication with GitHub ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun github-review-api-host (pr-alist)
  "Return the api host for a PR-ALIST."
  (a-get pr-alist 'apihost github-review-host))

(defun github-review-errback (&rest m)
  "Error callback, displays the error message M."
  (message "Error talking to GitHub: %s" m))

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
            :host (github-review-api-host pr-alist)
            :callback callback
            :errorback #'github-review-errback))

(defun github-review-get-pr-object (pr-alist callback)
  "Get a pr object given PR-ALIST an alist representing a PR.
CALLBACK is called with the result"
  (github-review-get-pr pr-alist nil callback))

(defun github-review-get-pr-diff (pr-alist callback)
  "Get the diff for a pr, given PR-ALIST an alist representing a PR.
CALLBACK is called with the result"
  (github-review-get-pr pr-alist t callback))

(defun github-review-get-pr-deferred (pr-alist needs-diff)
  "Get a pull request or its diff.
PR-ALIST is an alist representing a PR,
NEEDS-DIFF t to return a diff nil to return the pr object
return a deferred object"
  (let ((d (deferred:new #'identity)))
    (if needs-diff
        (github-review-get-pr-diff pr-alist (apply-partially (lambda (d v &rest _)  (deferred:callback-post d v)) d))
      (github-review-get-pr-object pr-alist (apply-partially (lambda (d v &rest _)  (deferred:callback-post d v)) d)))
    d))


(defun github-review-post-review (pr-alist review callback)
  "Submit a code review.
PR-ALIST is an alist representing a PR
REVIEW is the review alist
CALLBACK will be called back when done"
  (ghub-post (github-review-format-pr-url 'submit-review pr-alist)
             nil
             :auth 'github-review
             :payload review
             :host (github-review-api-host pr-alist)
             :errorback #'github-review-errback
             :callback callback))

(defun github-review-get-inline-comments (pr-alist callback)
  "Get the inline comments on a pr.
PR-ALIST is an alist representing a PR
CALLBACK will be called back when done"
  (ghub-get (github-review-format-pr-url 'get-inline-comments pr-alist)
            nil
            :auth 'github-review
            :host (github-review-api-host pr-alist)
            :errorback #'github-review-errback
            :callback callback))

(defun github-review-get-reviews (pr-alist callback)
  "Get the code reviews on a PR.
PR-ALIST is an alist representing a PR
CALLBACK will be called back when done"
  (ghub-get (github-review-format-pr-url 'get-review-comments pr-alist)
            nil
            :auth 'github-review
            :host (github-review-api-host pr-alist)
            :errorback #'github-review-errback
            :callback callback))

(defun github-review-get-issue-comments (pr-alist callback)
  "Get the top level comments on a PR.
PR-ALIST is an alist representing a PR
CALLBACK will be called back when done"
  (ghub-get (github-review-format-pr-url 'get-issue-comments pr-alist)
            nil
            :auth 'github-review
            :host (github-review-api-host pr-alist)
            :errorback #'github-review-errback
            :callback callback))

(defun github-review-get-reviews-deferred (pr-alist)
  "Get the code reviews on a PR.
PR-ALIST is an alist representing a PR
returns a deferred object"
  (let ((d (deferred:new #'identity)))
    (github-review-get-reviews pr-alist (apply-partially (lambda (d v &rest _)  (deferred:callback-post d v)) d)) d))

(defun github-review-get-issue-comments-deferred (pr-alist)
  "Get the top level comments on a PR.
PR-ALIST is an alist representing a PR
CALLBACK will be called back when done
return a deferred object"
  (let ((d (deferred:new #'identity)))
    (github-review-get-issue-comments pr-alist (apply-partially (lambda (d v &rest _)  (deferred:callback-post d v)) d)) d))

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

(defun github-review-is-start-of-file-hunk? (l)
  "Return t if L, a string that start with 'diff' marking the start of a file hunk."
  (string-prefix-p "diff" l))

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
  (let-alist acc
    (cond
     ;; First comment encountered
     ((equal nil .lastcomment)
      (a-assoc acc 'lastcomment new-comment))

     ;; Can merge the current comment with the last comment
     ;; if they have the same fields but different content
     ;; that is, if they talk about the same file and position
     ((and (equal (a-get new-comment 'path) .lastcomment.path)
           (equal (a-get new-comment 'position) .lastcomment.position))
      (let* ((new-body (concat .lastcomment.body "\n" (a-get new-comment 'body))))
        (a-assoc acc 'lastcomment (a-assoc new-comment 'body new-body))))

     ;; Cannot merge the current comment with the last comment
     (t (a-assoc acc 'merged (cons .lastcomment .merged) 'lastcomment new-comment)))))

(defun github-review-merge-comments (comments)
  "Takes COMMENTS, inline comments and return a merged list of comments.
COMMENTS on the same file, same pos are coallesced"
  (let* ((acc (a-alist 'lastcomment nil 'merged '()))
         (acc-reduced (-reduce-from #'github-review-merge-comment acc comments)))
    (cons (a-get acc-reduced 'lastcomment)
          (a-get acc-reduced 'merged))))

(defun github-review-normalize-comment (c)
  "Normalize the order of entries in the alist C, representing a comment.
needed to avoid writing convoluted tests"
  (let-alist c
    `((position . ,.position)
      (body . ,.body)
      (path . ,.path))))

(defun github-review-parse-line (acc l)
  "Reducer function to parse lines in a code review.
Analyzes one line in a diff return an alist with two entries: body and comments
L is a line from the diff.
ACC is an alist accumulating parsing state."
  (let-alist acc
    (let* ((top-level? (equal nil .pos))
           (in-file? (not top-level?)))
      (cond
       ;; Previous comments are ignored and don't affect the parsing
       ((github-review-previous-comment? l) acc)

       ;; First cgithub-review-hunk
       ((and top-level? (github-review-hunk? l))
        (a-assoc acc 'pos 0))

       ;; Start of file
       ((and top-level? (github-review-non-null-filename-hunk-line? l)
             (a-assoc acc 'pos nil 'path (github-review-file-path l))))

       ;; Global Comments
       ((and top-level? (github-review-comment? l))
        (a-assoc acc 'body (concat .body (github-review-comment-text l) "\n")))

       ;; Local Comments
       ((and in-file? (github-review-comment? l))
        (a-assoc
         acc
         'comments
         (cons
          ;; `max` here is to deal with comments at the top of a file (zeroth line), intended to give feedback
          ;; on a file overall and not any particular line
          ;; For such comments we report it on on the first line
          (a-alist 'position (max .pos 1)
                   'path .path
                   'body (github-review-comment-text l))
          .comments)))

       ;; Header before the filenames, restart the position
       ((github-review-is-start-of-file-hunk? l) (a-assoc acc 'pos nil))

       ;; Any other line in a file
       (in-file? (a-assoc acc 'pos (+ 1 .pos)))

       (t acc)))))

(defun github-review-parse-review-lines (lines)
  "Parse LINES corresponding to a code review."
  (let* ((acc (a-alist 'path nil
                       'pos nil
                       'body ""
                       'comments ()))
         (parsed-data (-reduce-from #'github-review-parse-line acc lines))
         (parsed-comments (a-get parsed-data 'comments))
         (parsed-body (s-trim-right (a-get parsed-data 'body)))
         (merged-comments (if (equal nil parsed-comments)
                              nil
                            (github-review-merge-comments (reverse parsed-comments)))))
    (if (equal nil merged-comments)
        `((body . ,parsed-body))
      `((body . ,parsed-body)
        (comments . ,(reverse (-map #'github-review-normalize-comment merged-comments)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun github-review-pr-from-fname (buffer-fname)
  "Extract a pr alist from BUFFER-FNAME."
  (let ((fname (car (last (s-split "/" buffer-fname)))))
    (save-match-data
      (and (string-match "\\(.*\\)___\\(.*\\)___\\([0-9]+\\)\.diff" fname)
           (a-alist 'num   (match-string 3 fname)
                    'repo  (match-string 2 fname)
                    'owner (match-string 1 fname))))))


(defun github-review-pr-from-url (url)
  "Extract a pr alist from a pull request URL."
  (save-match-data
    (and (string-match ".*/\\(.*\\)/\\(.*\\)/pull/\\([0-9]+\\)" url)
         (a-alist 'num   (match-string 3 url)
                  'repo  (match-string 2 url)
                  'owner (match-string 1 url)))))

(defun github-review-save-diff (pr-alist diff)
  "Save a DIFF (string) to a temp file named after pr specified by PR-ALIST."
  (let-alist pr-alist
    (find-file (format "%s/%s___%s___%s.diff" github-review-review-folder .owner .repo .num))
    (erase-buffer)
    (insert diff)
    (save-buffer)
    (github-review-mode)))

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
       (let* ((head-sha (a-get-in v '(head sha)))
              (review (a-assoc parsed-review 'commit_id head-sha 'event kind)))
         (github-review-post-review
          pr-alist
          review (lambda (&rest _)
                   (message "Done submitting review"))))))))

(defun github-review-to-comments (text)
  "Convert TEXT, a string to a string where each line is prefixed by ~."
  (s-join "\n" (-map (lambda (x) (concat "~ " x)) (s-split "\n" text))))

(defun github-review-format-top-level-comment (com)
  "Format a top level COM objectto string."
  (let-alist com
    (format "@%s: %s" .user.login .body)))

(defun github-review-format-review (review)
  "Format a REVIEW object to string."
  (let-alist review
    (format "Reviewed by @%s[%s]: %s" .user.login .state .body)))

(defun github-review-format-diff (ctx)
  "Formats a diff to save it for review.
CTX is the result of a callback chain to get information about a PR.
See ‘github-review-start’ for more information"
  (let-alist ctx
    (concat
     (github-review-to-comments .object.title)
     "\n~"
     "\n"
     ;; Github PR body contains \n\r for new lines
     (github-review-to-comments (s-replace "\r" "" .object.body))
     "\n"
     (when .top-level-comments
       (concat (s-join
                "\n"
                (-map
                 #'github-review-to-comments
                 (-map #'github-review-format-top-level-comment .top-level-comments)))
               "\n"))
     (when-let ((reviews (-reject (lambda (x) (string= (a-get x 'body) "")) .reviews)))
       (concat (s-join
                "\n"
                (-map
                 #'github-review-to-comments
                 (-map #'github-review-format-review reviews)))
               "\n"))
     .diff.message)))

;;;;;;;;;;;;;;;;;;;;;
;; User facing API ;;
;;;;;;;;;;;;;;;;;;;;;

(defun github-review-start-internal (pr-alist)
  "Start review given PR URL given PR-ALIST."
  (deferred:$
    (deferred:parallel
      ;; Get the diff
      (lambda () (github-review-get-pr-deferred pr-alist t))
      ;; And the PR object
      (lambda () (github-review-get-pr-deferred pr-alist nil))
      (when github-review-fetch-top-level-and-review-comments
        ;; And the top level comments
        (lambda () (github-review-get-issue-comments-deferred pr-alist)))
      (when github-review-fetch-top-level-and-review-comments
        ;; And the previous reviews
        (lambda () (github-review-get-reviews-deferred pr-alist))))
    (deferred:nextc it
      (lambda (x)
        (let* ((diff (-first-item x))
               (pr-object (-second-item x))
               (comms (a-get pr-object 'comments))
               (review_comments (a-get pr-object 'review_comments))
               (issues-comments (when (and (> comms 0) github-review-fetch-top-level-and-review-comments) (-third-item x)))
               (reviews (when (and (> review_comments 0) github-review-fetch-top-level-and-review-comments) github-review-fetch-top-level-and-review-comments (-fourth-item x))))
          (github-review-save-diff
           pr-alist
           (github-review-format-diff (a-alist 'diff diff
                                               'object pr-object
                                               'top-level-comments issues-comments
                                               'reviews reviews))))))
    (deferred:error it
      (lambda (err)
        (message "Got an error from the GitHub API %s!" err)))))


;;;###autoload
(defun github-review-forge-pr-at-point ()
  "Review the forge pull request at point."
  (interactive)
  (let* ((pullreq (or (forge-pullreq-at-point) (forge-current-topic)))
         (repo    (forge-get-repository pullreq))
         (owner   (oref repo owner))
         (name    (oref repo name))
         (apihost (oref repo apihost))
         (number  (oref pullreq number)))
    (github-review-start-internal (a-alist 'owner   owner
                                           'repo    name
                                           'apihost apihost
                                           'num     number))))

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

;;;###autoload
(define-derived-mode github-review-mode
  diff-mode "Code Review"
  "Major mode for code review"
  (setq mode-name "Code Review"
        major-mode 'github-review-mode)
  (run-mode-hooks 'github-review-mode-hook))

(provide 'github-review)
;;; github-review.el ends here
