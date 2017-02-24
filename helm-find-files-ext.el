;;; helm-find-files-ext.el --- Extension to helm-find-files  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'helm-files)

(defvar helm-ff-ext-skipping-dots-recenter t)
;; helper functions
(defvar helm-ff-expand-valid-only-p t)
(defvar helm-ff-sort-expansions-p nil)
(defvar helm-ff-ignore-case-p t)

(defvar helm-ff--invalid-dir nil)

(defun helm-ff--generate-case-ignore-pattern (pattern)
  (let (head (ci-pattern ""))
    (dotimes (i (length pattern) ci-pattern)
      (setq head (aref pattern i))
      (cond
       ((and (<= head ?z) (>= head ?a))
        (setq ci-pattern (format "%s[%c%c]" ci-pattern (upcase head) head)))
       ((and (<= head ?Z) (>= head ?A))
        (setq ci-pattern (format "%s[%c%c]" ci-pattern head (downcase head))))
       (:else
        (setq ci-pattern (format "%s%c" ci-pattern head)))))))

(defun helm-ff--try-expand-fname (candidate)
  (let ((dirparts (split-string candidate "/"))
        valid-dir
        fnames)
    (catch 'break
      (while dirparts
        (if (file-directory-p (concat valid-dir (car dirparts) "/"))
            (setq valid-dir (concat valid-dir (pop dirparts) "/"))
          (throw 'break t))))
    (setq fnames (cons candidate (helm-ff--try-expand-fname-1 valid-dir dirparts)))
    (if helm-ff-sort-expansions-p
        (sort fnames
              (lambda (f1 f2) (or (file-directory-p f1)
                              (not (file-directory-p f2)))))
      fnames)))

(defun helm-ff--try-expand-fname-1 (parent children)
  (if children
      (if (equal children '(""))
          (and (file-directory-p parent) `(,(concat parent "/")))
        (when (file-directory-p parent)
          (apply 'nconc
                 (mapcar
                  (lambda (f)
                    (or (helm-ff--try-expand-fname-1 f (cdr children))
                        (unless helm-ff-expand-valid-only-p
                          (and (file-directory-p f)
                               `(,(concat f "/" (mapconcat 'identity
                                                           (cdr children)
                                                           "/")))))))
                  (directory-files parent t
                                   (concat "^"
                                           (if helm-ff-ignore-case-p
                                               (helm-ff--generate-case-ignore-pattern
                                                (car children))
                                             (car children))))))))
    `(,(concat parent (and (file-directory-p parent) "/")))))

(defun helm-ff--try-expand-fname-first (orig-func &rest args)
  (let* ((candidate (car args))
         (collection (helm-ff--try-expand-fname candidate)))
    (if (and (> (length collection) 1)
             (not (file-exists-p candidate)))
        (with-helm-alive-p
          (when (helm-file-completion-source-p)
            (helm-set-pattern
             (helm-comp-read "Expand Path to: " collection :allow-nest t))))
      (apply orig-func args))))

(defun helm-find-files-get-candidates-ext (&optional require-match)
  "Create candidate list for `helm-source-find-files'."
  (let* ((path          (helm-ff-set-pattern helm-pattern))
         (dir-p         (file-accessible-directory-p path))
         basedir
         invalid-basedir
         non-essential
         (tramp-verbose helm-tramp-verbose)) ; No tramp message when 0.
    ;; Tramp check if path is valid without waiting a valid
    ;; connection and may send a file-error.
    (setq helm--ignore-errors (file-remote-p path))
    (set-text-properties 0 (length path) nil path)
    ;; Issue #118 allow creation of newdir+newfile.
    (unless (or
             ;; A tramp file name not completed.
             (string= path "Invalid tramp file name")
             ;; An empty pattern
             (string= path "")
             (and (string-match-p ":\\'" path)
                  (helm-ff-tramp-postfixed-p path))
             ;; Check if base directory of PATH is valid.
             (helm-aif (file-name-directory path)
                 ;; If PATH is a valid directory IT=PATH,
                 ;; else IT=basedir of PATH.
                 (file-directory-p it)))
      ;; BASEDIR is invalid, that's mean user is starting
      ;; to write a non--existing path in minibuffer
      ;; probably to create a 'new_dir' or a 'new_dir+new_file'.
      (setq invalid-basedir t))
    ;; Don't set now `helm-pattern' if `path' == "Invalid tramp file name"
    ;; like that the actual value (e.g /ssh:) is passed to
    ;; `helm-ff-tramp-hostnames'.
    (unless (or (string= path "Invalid tramp file name")
                ;; Ext: remove invalid-basedir
                )      ; Leave  helm-pattern unchanged.
      (setq helm-ff-auto-update-flag  ; [1]
            ;; Unless auto update is disabled at startup or
            ;; interactively, start auto updating only at third char.
            (unless (or (null helm-ff-auto-update-initial-value)
                        (null helm-ff--auto-update-state)
                        ;; But don't enable auto update when
                        ;; deleting backward.
                        helm-ff--deleting-char-backward
                        (and dir-p (not (string-match-p "/\\'" path))))
              (or (>= (length (helm-basename path)) 3) dir-p)))
      ;; At this point the tramp connection is triggered.
      (setq helm-pattern (helm-ff--transform-pattern-for-completion path))
      ;; This have to be set after [1] to allow deleting char backward.
      (setq basedir (expand-file-name
                     (if (and dir-p helm-ff-auto-update-flag)
                         ;; Add the final "/" to path
                         ;; when `helm-ff-auto-update-flag' is enabled.
                         (file-name-as-directory path)
                       (if (string= path "")
                           "/" (file-name-directory path)))))
      (setq helm-ff-default-directory
            (if (string= helm-pattern "")
                (expand-file-name "/")  ; Expand to "/" or "c:/"
              ;; If path is an url *default-directory have to be nil.
              (unless (or (string-match helm-ff-url-regexp path)
                          (and ffap-url-regexp
                               (string-match ffap-url-regexp path)))
                basedir))))
    (when (and (string-match ":\\'" path)
               (file-remote-p basedir nil t))
      (setq helm-pattern basedir))
    (cond ((string= path "Invalid tramp file name")
           (or (helm-ff-tramp-hostnames) ; Hostnames completion.
               (prog2
                   ;; `helm-pattern' have not been modified yet.
                   ;; Set it here to the value of `path' that should be now
                   ;; "Invalid tramp file name" and set the candidates list
                   ;; to ("Invalid tramp file name") to make `helm-pattern'
                   ;; match single candidate "Invalid tramp file name".
                   (setq helm-pattern path)
                   ;; "Invalid tramp file name" is now printed
                   ;; in `helm-buffer'.
                   (list path))))
          ((or (and (file-regular-p path)
                    (eq last-repeatable-command 'helm-execute-persistent-action))
               ;; `ffap-url-regexp' don't match until url is complete.
               (string-match helm-ff-url-regexp path)
               (and ffap-url-regexp (string-match ffap-url-regexp path)))
           (list path))
          ;; Ext: list all possible expansions
          ((or invalid-basedir
               (and (not (file-exists-p path)) (string-match "/$" path)))
           (helm-ff--try-expand-fname path))
          ((string= path "") (helm-ff-directory-files "/" t))
          ;; Check here if directory is accessible (not working on Windows).
          ((and (file-directory-p path) (not (file-readable-p path)))
           (list (format "file-error: Opening directory permission denied `%s'" path)))
          ;; A fast expansion of PATH is made only if `helm-ff-auto-update-flag'
          ;; is enabled.
          ((and dir-p helm-ff-auto-update-flag)
           (helm-ff-directory-files path t))
          (t (append (unless (or require-match
                                 ;; When `helm-ff-auto-update-flag' has been
                                 ;; disabled, whe don't want PATH to be added on top
                                 ;; if it is a directory.
                                 dir-p)
                       (list path))
                     (helm-ff-directory-files basedir t))))))

(defun helm-ff--transform-pattern-for-completion-ext (pattern)
  "Maybe return PATTERN with it's basename modified as a regexp.
This happen only when `helm-ff-fuzzy-matching' is enabled.
This provide a similar behavior as `ido-enable-flex-matching'.
See also `helm--mapconcat-pattern'.
If PATTERN is an url returns it unmodified.
When PATTERN contain a space fallback to multi-match.
If basename contain one or more space fallback to multi-match.
If PATTERN is a valid directory name,return PATTERN unchanged."
  ;; handle bad filenames containing a backslash.
  (setq pattern (helm-ff-handle-backslash pattern))
  (let ((bn      (helm-basename pattern))
        (bd      (or (helm-basedir pattern) ""))
        ;; Trigger tramp connection with file-directory-p.
        (dir-p   (or (file-directory-p pattern)
                     (string-match "/$" pattern)))
        (tramp-p (cl-loop for (m . f) in tramp-methods
                          thereis (string-match m pattern))))
    ;; Ext: set invalid
    (setq helm-ff--invalid-dir (not (file-exists-p bd)))
    ;; Always regexp-quote base directory name to handle
    ;; crap dirnames such e.g bookmark+
    ;; Ext: fuzzy match -- in order to bypass `helm-mm-match'
    (substring
     (replace-regexp-in-string
      "/"
      ".*/"
      (cond
       ((or (and dir-p tramp-p (string-match ":\\'" pattern))
            (string= pattern "")
            (and dir-p (<= (length bn) 2))
            ;; Fix Issue #541 when BD have a subdir similar
            ;; to BN, don't switch to match plugin
            ;; which will match both.
            (and dir-p (string-match (regexp-quote bn) bd)))
        ;; Use full PATTERN on e.g "/ssh:host:".
        (regexp-quote pattern))
       ;; Prefixing BN with a space call multi-match completion.
       ;; This allow showing all files/dirs matching BN (Issue #518).
       ;; FIXME: some multi-match methods may not work here.
       (dir-p (concat (regexp-quote bd) " " (regexp-quote bn)))
       ((or (not (helm-ff-fuzzy-matching-p))
            (string-match "\\s-" bn))    ; Fall back to multi-match.
        (concat (regexp-quote bd) bn))
       ((or (string-match "[*][.]?.*" bn) ; Allow entering wilcard.
            (string-match "/$" pattern)   ; Allow mkdir.
            (string-match helm-ff-url-regexp pattern)
            (and (string= helm-ff-default-directory "/") tramp-p))
        ;; Don't treat wildcards ("*") as regexp char.
        ;; (e.g ./foo/*.el => ./foo/[*].el)
        (concat (regexp-quote bd)
                (replace-regexp-in-string "[*]" "[*]" bn)))
       (t (concat (regexp-quote bd)
                  (if (>= (length bn) 2) ; wait 2nd char before concating.
                      (helm--mapconcat-pattern bn)
                    (concat ".*" (regexp-quote bn))))))))))

(defun helm-find-files-1-ext (fname &optional preselect)
  "Find FNAME with `helm' completion.
Like `find-file' but with `helm' support.
Use it for non--interactive calls of `helm-find-files'."
  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))
  (setq helm-find-files--toggle-bookmark nil)
  (let* ( ;; Be sure we don't erase the precedent minibuffer if some.
         (helm-ff-auto-update-initial-value
          (and helm-ff-auto-update-initial-value
               (not (minibuffer-window-active-p (minibuffer-window)))))
         (tap (thing-at-point 'filename))
         (def (and tap (or (file-remote-p tap)
                           (expand-file-name tap)))))
    (helm-set-local-variable 'helm-follow-mode-persistent nil)
    (unless helm-source-find-files
      (setq helm-source-find-files (helm-make-source
                                       "Find Files" 'helm-source-ffiles)))
    (mapc (lambda (hook)
            (add-hook 'helm-after-update-hook hook))
          '(helm-ff-move-to-first-real-candidate
            helm-ff-update-when-only-one-matched
            helm-ff-auto-expand-to-home-or-root))
    (unwind-protect
        (helm :sources 'helm-source-find-files
              :input fname
              :case-fold-search helm-file-name-case-fold-search
              :preselect preselect
              :ff-transformer-show-only-basename
              helm-ff-transformer-show-only-basename
              :default def
              :prompt "Find files or url: "
              :buffer "*helm find files*")
      (helm-attrset 'resume (lambda ()
                              (setq helm-ff-default-directory
                                    helm-ff-default-directory
                                    helm-ff-last-expanded
                                    helm-ff-last-expanded))
                    helm-source-find-files)
      (setq helm-ff-default-directory nil)
      ;; Ext: reset to nil
      (setq helm-ff--invalid-dir nil))))

(defun helm-ff-filter-candidate-one-by-one-ext (file)
  "`filter-one-by-one' Transformer function for `helm-source-find-files'."
  ;; Handle boring files
  (unless (and helm-ff-skip-boring-files
               (cl-loop for r in helm-boring-file-regexp-list
                        ;; Prevent user doing silly thing like
                        ;; adding the dotted files to boring regexps (#924).
                        thereis (and (not (string-match "\\.$" file))
                                     (string-match r file))))
    ;; Handle tramp files.
    (if (and (string-match helm-tramp-file-name-regexp helm-pattern)
             helm-ff-tramp-not-fancy)
        (if helm-ff-transformer-show-only-basename
            (if (helm-dir-is-dot file)
                file
              (cons (or (helm-ff-get-host-from-tramp-invalid-fname file)
                        (helm-basename file))
                    file))
          file)
      ;; Now highlight.
      (let* ((disp (if (and helm-ff-transformer-show-only-basename
                            ;; Ext: Don't show basename if not valid
                            (not helm-ff--invalid-dir)
                            (not (helm-dir-is-dot file))
                            (not (and ffap-url-regexp
                                      (string-match ffap-url-regexp file)))
                            (not (string-match helm-ff-url-regexp file)))
                       (or (helm-ff-get-host-from-tramp-invalid-fname file)
                           (helm-basename file)) file))
             (attr (file-attributes file))
             (type (car attr)))

        (cond ((string-match "file-error" file) file)
              ( ;; A not already saved file.
               (and (stringp type)
                    (not (helm-ff-valid-symlink-p file))
                    (not (string-match "^\.#" (helm-basename file))))
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-invalid-symlink) t)
                     file))
              ;; A dotted directory symlinked.
              ((and (helm-ff-dot-file-p file) (stringp type))
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-dotted-symlink-directory) t)
                     file))
              ;; A dotted directory.
              ((helm-ff-dot-file-p file)
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-dotted-directory) t)
                     file))
              ;; A symlink.
              ((stringp type)
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-symlink) t)
                     file))
              ;; A directory.
              ((eq t type)
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-directory) t)
                     file))
              ;; An executable file.
              ((and attr (string-match "x" (nth 8 attr)))
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-executable) t)
                     file))
              ;; A file.
              ((and attr (null type))
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-file) t)
                     file))
              ;; A non--existing file.
              (t
               (cons (helm-ff-prefix-filename
                      (propertize disp 'face 'helm-ff-file) nil 'new-file)
                     file)))))))

(defun helm-ff--skip-dots (orig-func &rest args)
  (apply orig-func args)
  (let ((src (helm-get-current-source))
        (flag nil))
    (while (and (not (helm-end-of-source-p))
                (helm-dir-is-dot (helm-get-selection nil nil src)))
      (helm-next-line)
      (setq flag t))
    (when (and (helm-end-of-source-p)
               (helm-dir-is-dot (helm-get-selection nil nil src)))
      (helm-previous-line))
    (and helm-ff-ext-skipping-dots-recenter
         flag
         (with-helm-window
           (recenter-top-bottom 0)))))

;;;###autoload
(defun helm-ff-ext-enable-zsh-path-expansion (enable)
  (interactive)
  (if enable
      (advice-add 'helm-ff-kill-or-find-buffer-fname
                  :around
                  #'helm-ff--try-expand-fname-first)
    (advice-remove 'helm-ff-kill-or-find-buffer-fname
                   #'helm-ff--try-expand-fname-first)))
;;;###autoload
(defun helm-ff-ext-enable-auto-path-expansion (enable)
  (interactive)
  (dolist (func '(helm-find-files-get-candidates
                  helm-ff--transform-pattern-for-completion
                  helm-find-files-1
                  helm-ff-filter-candidate-one-by-one))
    (let ((new-func (intern (format "%s-ext" (symbol-name func)))))
      (if enable
          (advice-add func :override new-func)
        (advice-remove func new-func)))))

;;;###autoload
(defun helm-ff-ext-enable-skipping-dots (enable)
  (interactive)
  (if enable
      (advice-add 'helm-ff-move-to-first-real-candidate
                  :around
                  'helm-ff--skip-dots)
    (advice-remove 'helm-ff-move-to-first-real-candidate
                   'helm-ff--skip-dots)))

(provide 'helm-find-files-ext)
;;; helm-find-files-ext.el ends here
