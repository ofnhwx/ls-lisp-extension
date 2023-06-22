;;; ls-lisp-extension.el --- ls-lisp をちょっと改造

;; Copyright (C) 2021 by Yuta Fujita

;; Author: Yuta Fujita <ofnhwx@komunan.net>
;; URL: https://github.com/ofnhwx/ls-lisp-extension
;; Version: 0.04
;; Package-Requires: ((f "0.20.0") (s "1.12.0"))

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

;;; Code:

(eval-and-compile
  (require 'ls-lisp)
  (require 'cl-lib)
  (require 'f)
  (require 's))

(defgroup ls-lisp-extension ()
  "`ls-lisp' をちょっと改造"
  :group 'ls-lisp)

(defface ls-lisp-extension-dired-directory-face
  '((t :foreground "#4f97d7"))
  "ディレクトリ"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-symlink-face
  '((t :foreground "#28def0"))
  "シンボリックリンク"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-hidden-file-face
  '((t :foreground "#707070"))
  "隠しファイル"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-uid-face
  '((t :foreground "#90ee90"))
  "ユーザーID"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-gid-face
  '((t :foreground "#90ee90"))
  "グループID"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-modes-d-face
  '((t :foreground "#4f97d7"))
  "モード: d(ディレクトリ)"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-modes-l-face
  '((t :foreground "#28def0"))
  "モード: l(リンク)"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-modes-r-face
  '((t :foreground "#eeee00"))
  "モード: r(読込可)"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-modes-w-face
  '((t :foreground "#ed2200"))
  "モード: w(書込可)"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-modes-x-face
  '((t :foreground "#00ee00"))
  "モード: x(実行可)"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-modes-s-face
  '((t :foreground "#9f79ee"))
  "モード: s(setuid/setgid + 実行可)"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-modes-S-face
  '((t :foreground "#9f79ee"))
  "モード: S(setuid/setgid)"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-modes-t-face
  '((t :foreground "#9f79ee"))
  "モード: t(sticky + 実行可)"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-modes-T-face
  '((t :foreground "#9f79ee"))
  "モード: T(sticky)"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-modes---face
  '((t :foreground "#707070"))
  "モード: -(その他)"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-size-over-1t-face
  '((t :foreground "#ff7f00" :underline t))
  "1TiB以上のファイルサイズ"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-size-over-1g-face
  '((t :foreground "#ff7f00"))
  "1GiB以上のファイルサイズ"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-size-over-1m-face
  '((t :foreground "#ee7600"))
  "1MiB以上のファイルサイズ"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-size-over-1k-face
  '((t :foreground "#cd6600"))
  "1KiB以上のファイルサイズ"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-size-default-face
  '((t :foreground "#8b4500"))
  "1KiB未満のファイルサイズ"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-time-over-1y-face
  '((t :foreground "#00688b"))
  "1年以上経過したファイル日時"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-time-over-1m-face
  '((t :foreground "#009acd"))
  "1ヶ月以上経過したファイル日時"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-time-over-1w-face
  '((t :foreground "#00b2ee"))
  "1週間以上経過したファイル日時"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-time-over-1d-face
  '((t :foreground "#00bfff"))
  "1日以上経過したファイル日時"
  :group 'ls-lisp-extension)

(defface ls-lisp-extension-dired-time-default-face
  '((t :foreground "#00bfff" :underline t))
  "1日以内のファイル日時"
  :group 'ls-lisp-extension)

(defun ls-lisp-extension-format (args)
  (let* ((file-name  (nth 0 args))
         (file-attr  (nth 1 args))
         (file-size  (nth 2 args))
         (switches   (nth 3 args))
         (time-index (nth 4 args))
         (file-type  (nth 0 file-attr))
         (file-uid   (nth 2 file-attr))
         (file-gid   (nth 3 file-attr))
         (file-modes (nth 8 file-attr)))
    (setq file-name (ls-lisp-extension-format-file-name file-name file-type file-modes))
    (if (stringp file-type)
        (setf (nth 0 file-attr) (propertize (f-short file-type) 'font-lock-face 'ls-lisp-extension-dired-symlink-face)))
    (setf (nth 2 file-attr) (propertize (format "%s" file-uid) 'font-lock-face 'ls-lisp-extension-dired-uid-face))
    (setf (nth 3 file-attr) (propertize (format "%s" file-gid) 'font-lock-face 'ls-lisp-extension-dired-gid-face))
    (setf (nth 8 file-attr) (ls-lisp-extension-format-file-mods file-modes))
    (list file-name file-attr file-size switches time-index)))

(defun ls-lisp-extension-hidden-file-p (file-name)
  (or (s-starts-with? "." file-name)
      (s-starts-with? "#" file-name)
      (s-starts-with? "~" file-name)))

(defun ls-lisp-extension-format-file-name (file-name file-type file-modes)
  (let ((face (cond
               ((ls-lisp-extension-hidden-file-p file-name) 'ls-lisp-extension-dired-hidden-file-face)
               ((eq file-type t)                            'ls-lisp-extension-dired-directory-face)
               ((stringp file-type)                         'ls-lisp-extension-dired-symlink-face)
               ((s-contains? "x" file-modes)                'ls-lisp-extension-dired-modes-x-face))))
    (if face
        (propertize file-name 'font-lock-face face)
      file-name)))

(defun ls-lisp-extension-format-file-mods (file-modes)
  (mapconcat (lambda (c)
               (let ((face (cl-case c
                             (?d 'ls-lisp-extension-dired-modes-d-face)
                             (?l 'ls-lisp-extension-dired-modes-l-face)
                             (?r 'ls-lisp-extension-dired-modes-r-face)
                             (?w 'ls-lisp-extension-dired-modes-w-face)
                             (?x 'ls-lisp-extension-dired-modes-x-face)
                             (?s 'ls-lisp-extension-dired-modes-s-face)
                             (?S 'ls-lisp-extension-dired-modes-S-face)
                             (?t 'ls-lisp-extension-dired-modes-t-face)
                             (?T 'ls-lisp-extension-dired-modes-T-face)
                             (?- 'ls-lisp-extension-dired-modes---face)
                             (t  'ls-lisp-extension-dired-modes---face))))
                 (propertize (string c) 'font-lock-face face)))
             file-modes ""))

(defun ls-lisp-extension-format-time (fn file-attr time-index)
  (let* ((formated-time (funcall fn file-attr time-index))
         (time (nth (or time-index 5) file-attr))
         (diff (time-subtract time nil))
         (face (cond
                ((time-less-p diff -31536000) 'ls-lisp-extension-dired-time-over-1y-face)
                ((time-less-p diff -2592000)  'ls-lisp-extension-dired-time-over-1m-face)
                ((time-less-p diff -604800)   'ls-lisp-extension-dired-time-over-1w-face)
                ((time-less-p diff -86400)    'ls-lisp-extension-dired-time-over-1d-face)
                (t                            'ls-lisp-extension-dired-time-default-face))))
    (propertize formated-time 'font-lock-face face)))

(defun ls-lisp-extension-format-file-size (fn file-size human-readable)
  (let ((formated-file-size (funcall fn file-size human-readable))
        (face (cond
               ((>= file-size 1099511627776) 'ls-lisp-extension-dired-size-over-1t-face)
               ((>= file-size 1073741824)    'ls-lisp-extension-dired-size-over-1g-face)
               ((>= file-size 1048576)       'ls-lisp-extension-dired-size-over-1m-face)
               ((>= file-size 1024)          'ls-lisp-extension-dired-size-over-1k-face)
               (t                            'ls-lisp-extension-dired-size-default-face))))
    (propertize formated-file-size 'font-lock-face face)))

;;;###autoload
(defun ls-lisp-extension-on ()
  (interactive)
  (advice-add 'ls-lisp-format :filter-args #'ls-lisp-extension-format)
  (advice-add 'ls-lisp-format-time :around #'ls-lisp-extension-format-time)
  (advice-add 'ls-lisp-format-file-size :around #'ls-lisp-extension-format-file-size))

;;;###autoload
(defun ls-lisp-extension-off ()
  (interactive)
  (advice-remove 'ls-lisp-format #'ls-lisp-extension-format)
  (advice-remove 'ls-lisp-format-time #'ls-lisp-extension-format-time)
  (advice-remove 'ls-lisp-format-file-size #'ls-lisp-extension-format-file-size))

(provide 'ls-lisp-extension)

;;; ls-lisp-extension.el ends here
