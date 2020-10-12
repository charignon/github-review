;;; github-review.el --- GitHub based code review -*- lexical-binding: t -*-
;; Author: Laurent Charignon <l.charignon@gmail.com>
;; Maintainer: Laurent Charignon <l.charignon@gmail.com>
;; Keywords: git, tools, vc, github
;; Homepage: https://github.com/charignon/github-review
;; Package-Requires: ((emacs "25") (s "1.12.0") (ghub "2.0") (dash "2.11.0") (deferred "0.5.1"))
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
(require 'deferred)

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

(defcustom github-review-save-commit-id-in-buffer t
  "If t, the retrieved diff buffer will contain the diff's and its base's sha."
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

(defun github-review-is-pr (id-alist)
  (not (null (github-review-a-get id-alist 'num))))

(defconst github-review-url-scheme
  '((get-pr . "/repos/%s/%s/pulls/%s")
    (get-commit . "/repos/%s/%s/commits/%s")
    (get-commit-comments . "/repos/%s/%s/commits/%s/comments")
    (get-inline-comments . "/repos/%s/%s/pulls/%s/comments")
    (get-review-comments . "/repos/%s/%s/pulls/%s/reviews")
    (get-issue-comments . "/repos/%s/%s/issues/%s/comments")
    (submit-commit-comments . "/repos/%s/%s/commits/%s/comments")
    (submit-review . "/repos/%s/%s/pulls/%s/reviews")))

(defun github-review-format-url (kind id-alist)
  "Format a url for accessing either a pr or commit.
KIND is the kind of information to request.
ID-ALIST is an alist represenging the object"
  (format (github-review-a-get github-review-url-scheme kind)
          (github-review-a-get id-alist 'owner )
          (github-review-a-get id-alist 'repo )
          (github-review-a-get id-alist (if (github-review-is-pr id-alist) 'num 'sha))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication with GitHub ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun github-review-api-host (id-alist)
  "Return the api host for a ID-ALIST."
  (or (github-review-a-get id-alist 'apihost) github-review-host))

(defun github-review-get (id-alist needs-diff callback)
  "Get a pull request or a commit or one's diff.
ID-ALIST is an alist representing the PR or COMMIT,
NEEDS-DIFF t to return a diff nil to return the requested object,
CALLBACK to call back when done."
  (ghub-get (github-review-format-url
	     (if (github-review-is-pr id-alist) 'get-pr 'get-commit) id-alist)
            nil
            :unpaginate t
            :headers (if needs-diff github-review-diffheader '())
            :auth 'github-review
            :host (github-review-api-host id-alist)
            :callback callback
            :errorback (lambda (&rest _) (message "Error talking to GitHub"))))

(defun github-review-get-object (id-alist callback)
  "Get a pr object given ID-ALIST an alist representing a PR or COMMIT,
CALLBACK is called with the result"
  (github-review-get id-alist nil callback))

(defun github-review-get-diff (id-alist callback)
  "Get the diff for a pr, given ID-ALIST an alist representing a PR or COMMIT,
CALLBACK is called with the result"
  (github-review-get id-alist t callback))

(defun github-review-get-deferred (id-alist needs-diff)
  "Get a pull request or its diff.
ID-ALIST is an alist representing a PR or COMMIT,
NEEDS-DIFF t to return a diff nil to return the pr object,
return a deferred object"
  (let ((d (deferred:new #'identity)))
    (if needs-diff
        (github-review-get-diff id-alist (apply-partially (lambda (d v &rest _)  (deferred:callback-post d v)) d))
      (github-review-get-object id-alist (apply-partially (lambda (d v &rest _)  (deferred:callback-post d v)) d)))
    d))

(defun github-review-post-review (id-alist review callback)
  "Submit a code review.
ID-ALIST is an alist representing a PR or COMMIT,
REVIEW is the review alist,
CALLBACK will be called back when done"
  (ghub-post (github-review-format-url
	      (if (github-review-is-pr id-alist) 'submit-review 'submit-commit-comments) id-alist)
             nil
             :auth 'github-review
             :payload review
             :host (github-review-api-host id-alist)
             :errorback (lambda (&rest _) (message "Error talking to GitHub"))
             :callback callback))

(defun github-review-get-inline-comments (id-alist callback)
  "Get the inline comments on a pr.
ID-ALIST is an alist representing a PR or COMMIT,
CALLBACK will be called back when done"
  (ghub-get (github-review-format-url 'get-inline-comments id-alist)
            nil
            :auth 'github-review
            :host (github-review-api-host id-alist)
            :errorback (lambda (&rest _) (message "Error talking to GitHub"))
            :callback callback))

(defun github-review-get-reviews (id-alist callback)
  "Get the code reviews on a PR.
ID-ALIST is an alist representing a PR or COMMIT,
CALLBACK will be called back when done"
  (ghub-get (github-review-format-url 'get-review-comments id-alist)
            nil
            :auth 'github-review
            :host (github-review-api-host id-alist)
            :errorback (lambda (&rest _) (message "Error talking to GitHub"))
            :callback callback))

(defun github-review-get-issue-comments (id-alist callback)
  "Get the top level comments on a PR.
ID-ALIST is an alist representing a PR or COMMIT,
CALLBACK will be called back when done"
  (ghub-get (github-review-format-url 'get-issue-comments id-alist)
            nil
            :auth 'github-review
            :host (github-review-api-host id-alist)
            :errorback (lambda (&rest _) (message "Error talking to GitHub"))
            :callback callback))

(defun github-review-get-reviews-deferred (id-alist)
  "Get the code reviews on a PR.
ID-ALIST is an alist representing a PR or COMMIT,
returns a deferred object"
  (let ((d (deferred:new #'identity)))
    (github-review-get-reviews id-alist (apply-partially (lambda (d v &rest _)  (deferred:callback-post d v)) d)) d))

(defun github-review-get-issue-comments-deferred (id-alist)
  "Get the top level comments on a PR.
ID-ALIST is an alist representing a PR or COMMIT,
CALLBACK will be called back when done
return a deferred object"
  (let ((d (deferred:new #'identity)))
    (github-review-get-issue-comments id-alist (apply-partially (lambda (d v &rest _)  (deferred:callback-post d v)) d)) d))

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
     ((and top-level? (github-review-non-null-filename-hunk-line? l)
           (github-review-a-assoc (github-review-a-assoc acc 'pos nil) 'path (github-review-file-path l))))

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
            ;; `max` here is to deal with comments at the top of a file (zeroth line), intended to give feedback
            ;; on a file overall and not any particular line
            ;; For such comments we report it on on the first line
            (github-review-a-assoc 'position (max pos 1))
            (github-review-a-assoc 'path path)
            (github-review-a-assoc 'body (github-review-comment-text l)))
        comments)))

     ;; Header before the filenames, restart the position
     ((github-review-is-start-of-file-hunk? l) (github-review-a-assoc acc 'pos nil))

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
        (comments . ,(reverse (-map #'github-review-normalize-comment merged-comments)))))))
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

(defun github-review-commit-from-fname (buffer-fname)
  "Extract a commit alist from BUFFER-FNAME."
  (let* ((fname (car (last (s-split "/" buffer-fname)))))
    (save-match-data
      (and (string-match "\\(.*\\)___\\(.*\\)___commit___\\(.+\\)\.diff" fname)
           (let* ((commit-alist  (-> (github-review-a-empty)
                                     (github-review-a-assoc 'owner (match-string 1 fname))
                                     (github-review-a-assoc 'repo  (match-string 2 fname))
                                     (github-review-a-assoc 'sha (match-string 3 fname)))))
             commit-alist)))))

(defun github-review-pr-from-url (url)
  "Extract a pr alist from a pull request URL."
  (save-match-data
    (and (string-match ".*/\\(.*\\)/\\(.*\\)/pull/\\([0-9]+\\)" url)
         (let* ((pr-alist  (-> (github-review-a-empty)
                               (github-review-a-assoc 'owner (match-string 1 url))
                               (github-review-a-assoc 'repo  (match-string 2 url))
                               (github-review-a-assoc 'num   (match-string 3 url)))))
           pr-alist))))


(defun github-review-commit-from-url (url)
  "Extract a commit alist from a commit URL."
  (save-match-data
    (and (string-match ".*/\\(.*\\)/\\(.*\\)/commit/\\(.+\\)" url)
         (let* ((commit-alist  (-> (github-review-a-empty)
                                   (github-review-a-assoc 'owner (match-string 1 url))
                                   (github-review-a-assoc 'repo  (match-string 2 url))
                                   (github-review-a-assoc 'sha (match-string 3 url)))))
           commit-alist))))

(defun github-review-pr-commit-from-url (url)
  "Extract a commit alist from a PR commit URL."
  (save-match-data
    (and (string-match ".*/\\(.*\\)/\\(.*\\)/pull/.*/commits/\\(.+\\)" url)
         (let* ((commit-alist  (-> (github-review-a-empty)
                                   (github-review-a-assoc 'owner (match-string 1 url))
                                   (github-review-a-assoc 'repo  (match-string 2 url))
                                   (github-review-a-assoc 'sha (match-string 3 url)))))
           commit-alist))))

(defun github-review-save-diff (id-alist diff ids)
  "Save a DIFF (string) to a temp file named after pr or commit sha specified by ID-ALIST.
IDS stands for alist of the diff and its parent's sha:s, when supplied the two becomes
buffer-local variables"
  (let* ((is-pr (github-review-is-pr id-alist))
	 (buffer-read-only))
    (find-file (format (if is-pr "%s/%s___%s___%s.diff" "%s/%s___%s___commit___%s.diff")
                       github-review-review-folder
                       (github-review-a-get id-alist 'owner)
                       (github-review-a-get id-alist 'repo)
                       (github-review-a-get id-alist (if is-pr 'num 'sha))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert diff)
    (when github-review-save-commit-id-in-buffer
      (when (not (null ids))
	(set-variable 'gh-commit-id       (first  ids) t)
	(set-variable 'gh-first-parent-id (second ids) t)
	(insert
	 (concat "\n\n"
      		 ";;; Local Variables:\n"
		 ";;; gh-commit-id: "    gh-commit-id       "\n"
		 ";;; gh-first-parent: " gh-first-parent-id "\n"
		 ";;; End:\n"))))
    (save-buffer)
    ;; Fixme: would it make sense to run the new mode
    ;;        as an user-customizable option, a sort of a hook.
    ;;        At this point some might prefer a plain (diff-mode). Others
    ;;        check out the parent commit, start building etc.
    ;;        Sure, that could be done through (add-hook 'github-review-mode-hook ... :-))
    (github-review-mode)))

(defun github-review-parsed-review-from-current-buffer ()
  "Return a code review given the current buffer containing a diff."
  (-> (buffer-substring-no-properties (point-min) (point-max))
      (split-string "\n")
      (github-review-parse-review-lines)))

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;


(defun github-review-submit-review-commit ()
  "Submit a code review of KIND.
This function infers the PR name based on the current filename"
  (let* ((commit-alist (github-review-commit-from-fname (buffer-file-name)))
         (parsed-review (github-review-parsed-review-from-current-buffer)))
    (message "Submitting review, this may take a while ...")
    (let* ((parsed-body (github-review-a-get parsed-review 'body))
           (comments (github-review-a-get parsed-review 'comments)))
      (when (and parsed-body (not (string-empty-p parsed-body)))
	(github-review-post-review commit-alist `((body . ,parsed-body))
				   (lambda (&rest _)
                                     (message "Done submitting commit top-level comments"))))
      (cl-loop for x in comments collect (github-review-post-review
                                          commit-alist
                                          x (lambda (&rest _)
                                              (message "Done submitting commit diff comments")))))))



(defun github-review-submit-review (kind)
  "Submit a code review of KIND.
This function infers the PR name based on the current filename"
  (if (s-contains? "___commit___" (buffer-file-name))
      (github-review-submit-review-commit)
    (let* ((id-alist (github-review-pr-from-fname (buffer-file-name)))
           (parsed-review (github-review-parsed-review-from-current-buffer)))
      (message "Submitting review, this may take a while ...")
      (github-review-get-object
       id-alist
       (lambda (v &rest _)
         (let* ((head-sha (github-review-a-get (github-review-a-get v 'head) 'sha))
                (review   (-> parsed-review
                              (github-review-a-assoc 'commit_id head-sha)
                              (github-review-a-assoc 'event kind))))
           (github-review-post-review
            id-alist
            review (lambda (&rest _)
                     (message "Done submitting review %S" review)))))))))

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
         (body (or (github-review-a-get ob 'body) (github-review-a-get (github-review-a-get ob 'commit) 'message)))
         (top-level-comments (github-review-a-get ctx 'top-level-comments))
         (reviews (-reject
                   (lambda (x)
                     (string= (github-review-a-get x 'body) ""))
                   (github-review-a-get ctx 'reviews)))
         (diff (-> ctx (github-review-a-get 'diff) (github-review-a-get 'message))))
    (concat
     (when title
       (concat
        (github-review-to-comments title)
        "\n~"
        "\n"))
     ;; Github PR body contains \n\r for new lines
     (when body
       (concat
        (github-review-to-comments (s-replace "\r" "" body))
        "\n"))
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

(defun github-review-start-internal (pr-alist)
  "Start review given PR URL given PR-ALIST."
  (deferred:$
    (deferred:parallel
      ;; Get the diff
      (lambda () (github-review-get-deferred pr-alist t))
      ;; And the PR object
      (lambda () (github-review-get-deferred pr-alist nil))
      (when github-review-fetch-top-level-and-review-comments
        ;; And the top level comments
        (lambda () (github-review-get-issue-comments-deferred pr-alist)))
      (when github-review-fetch-top-level-and-review-comments
        ;; And the previous reviews
        (lambda () (github-review-get-reviews-deferred pr-alist))))
    (deferred:nextc it
      (lambda (x)
        (let* ((diff (-> x (elt 0)))
               (pr-object (-> x (elt 1)))
               (comms (github-review-a-get pr-object 'comments))
	       (commit-id (github-review-a-get (github-review-a-get pr-object 'head) 'sha))
	       (parent-id (github-review-a-get (github-review-a-get pr-object 'base) 'sha))
               (review_comments (github-review-a-get pr-object 'review_comments))
               (issues-comments (when (and (> comms 0) github-review-fetch-top-level-and-review-comments) (-> x (elt 2))))
               (reviews (when (and (> review_comments 0) github-review-fetch-top-level-and-review-comments) github-review-fetch-top-level-and-review-comments (-> x (elt 3)))))
          (github-review-save-diff
           pr-alist
           (github-review-format-diff (-> (github-review-a-empty)
                                          (github-review-a-assoc 'diff diff)
                                          (github-review-a-assoc 'object pr-object)
                                          (github-review-a-assoc 'top-level-comments issues-comments)
                                          (github-review-a-assoc 'reviews reviews)))
	   (list commit-id parent-id)))))
    (deferred:error it
      (lambda (err)
        (message "Got an error from the GitHub API %s!" err)))))


;;;###autoload
(defun github-review-forge-pr-at-point ()
  "Review the forge pull request at point."
  (interactive)
  (let* ((pullreq (or (forge-pullreq-at-point) (forge-current-topic)))
         (repo (forge-get-repository pullreq))
         (owner (oref repo owner))
         (name (oref repo name))
         (apihost (oref repo apihost))
         (number (oref pullreq number))
         (pr-alist (-> (github-review-a-empty)
                       (github-review-a-assoc 'owner owner)
                       (github-review-a-assoc 'repo  name)
                       (github-review-a-assoc 'apihost apihost)
                       (github-review-a-assoc 'num   number))))
    (github-review-start-internal pr-alist)))

(defun github-review-is-commit-url? (str)
  "Return t if the STR look like a commit URL."
  (s-contains? "/commit/" str))

(defun github-review-is-pr-commit-url? (str)
  "Return t if the STR look like a PR commit URL."
  (s-contains? "/commits/" str))

(defun github-review-commit-start-internal (commit-alist)
  "Start review given commit URL given COMMIT-ALIST."
  (deferred:$
    (deferred:parallel
      ;; Get the diff
      (lambda () (github-review-get-deferred commit-alist t))
      ;; And the PR object
      (lambda () (github-review-get-deferred commit-alist nil))
      (when github-review-fetch-top-level-and-review-comments
        ;; And the top level comments
        (lambda () (github-review-get-commit-comments-deferred commit-alist))))
    (deferred:nextc it
      (lambda (x)
        (let* ((diff (-> x (elt 0)))
               (commit-object (-> x (elt 1)))
	       (issues-comments (when (and t github-review-fetch-top-level-and-review-comments) (-> x (elt 2))))
	       (commit-id (alist-get 'sha commit-object))
	       (parent-id (alist-get 'sha (first (alist-get 'parents commit-object)))))
          (github-review-save-diff
           commit-alist
           (github-review-format-diff (-> (github-review-a-empty)
                                          (github-review-a-assoc 'diff diff)
                                          (github-review-a-assoc 'object commit-object)
					  (github-review-a-assoc 'top-level-comments issues-comments)))
	   (list commit-id parent-id)))))
    (deferred:error it
      (lambda (err)
        (message "Got an error from the GitHub API %s!" err)))))

(defun github-review-get-commit-comments (commit-alist callback)
  "Get the top level comments on a COMMIT.
COMMIT-ALIST is an alist representing a COMMIT,
CALLBACK will be called back when done"
  (ghub-get (github-review-format-url 'get-commit-comments commit-alist)
            nil
            :auth 'github-review
            :host (github-review-api-host commit-alist)
            :errorback (lambda (&rest _) (message "Error talking to GitHub"))
            :callback callback))

(defun github-review-get-commit-comments-deferred (commit-alist)
  "Get the top level comments on a COMMIT.
COMMIT-ALIST is an alist representing a COMMIT,
CALLBACK will be called back when done,
return a deferred object"
  (let ((d (deferred:new #'identity)))
    (github-review-get-commit-comments commit-alist (apply-partially (lambda (d v &rest _)  (deferred:callback-post d v)) d)) d))


;;;###autoload
(defun github-review-start (url)
  "Start review given PR URL."
  (interactive "sPR or commit URL: ")
  (cond ((github-review-is-commit-url? url) (let* ((commit-alist (github-review-commit-from-url url)))
                                             (github-review-commit-start-internal commit-alist)))
        ((github-review-is-pr-commit-url? url) (let* ((commit-alist (github-review-pr-commit-from-url url)))
                                             (github-review-commit-start-internal commit-alist)))
        (t (let* ((pr-alist (github-review-pr-from-url url)))
             (github-review-start-internal pr-alist))))
  (message "current buffer %s" (buffer-name)))

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
  (setq mode-name "Code Review")
  (setq major-mode 'github-review-mode)
  (run-mode-hooks 'github-review-mode-hook))

(provide 'github-review)
;;; github-review.el ends here
