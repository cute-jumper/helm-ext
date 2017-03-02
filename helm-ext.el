;;; helm-ext.el --- Helm extensions                  -*- lexical-binding: t; -*-

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

(require 'helm-ext-ff)
(require 'helm-ext-minibuffer)

;;;###autoload
(defun helm-ext-ff-enable-zsh-path-expansion (enable)
  (interactive)
  (if enable
      (advice-add 'helm-ff-kill-or-find-buffer-fname
                  :around
                  #'helm-ext-ff--try-expand-fname-first)
    (advice-remove 'helm-ff-kill-or-find-buffer-fname
                   #'helm-ext-ff--try-expand-fname-first)))
;;;###autoload
(defun helm-ext-ff-enable-auto-path-expansion (enable)
  (interactive)
  (if enable
      (advice-add 'helm-find-files-1
                  :around
                  'helm-ext-find-files-1)
    (advice-remove 'helm-find-files-1
                   'helm-ext-find-files-1))
  (dolist (func '(helm-find-files-get-candidates
                  helm-ff--transform-pattern-for-completion
                  helm-ff-filter-candidate-one-by-one))
    (let ((new-func (intern (format "%s-ext" (symbol-name func)))))
      (if enable
          (advice-add func :override new-func)
        (advice-remove func new-func)))))

;;;###autoload
(defun helm-ext-ff-enable-skipping-dots (enable)
  (interactive)
  (if enable
      (advice-add 'helm-ff-move-to-first-real-candidate
                  :around
                  'helm-ext-ff-skip-dots)
    (advice-remove 'helm-ff-move-to-first-real-candidate
                   'helm-ext-ff-skip-dots)))

;;;###autoload
(defun helm-ext-minibuffer-enable-header-line-maybe (enable)
  (if enable
      (add-hook 'helm-minibuffer-set-up-hook 'helm-ext--use-header-line-maybe)
    (remove-hook 'helm-minibuffer-set-up-hook 'helm-ext--use-header-line-maybe)))

(provide 'helm-ext)
;;; helm-ext.el ends here
