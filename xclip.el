;;; xclip.el --- use xclip to copy&paste             -*- lexical-binding: t; -*-

;; Copyright (C) 2007, 2012, 2013  Free Software Foundation, Inc.

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: convenience, tools
;; Created: 2007-12-30
;; Version: 1.3

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

;; This package allows emacs to copy to and paste from the X clipboard
;; when running in xterm. It uses the external command-line tool xclip
;; found on http://xclip.sourceforge.net.
;;
;; To use: (xclip-mode 1)

;;; Code:

(defcustom xclip-program "xclip"
  "Name of the xclip program."
  :type 'string
  :group 'killing)

(defcustom xclip-select-enable-clipboard t
  "Non-nil means cutting and pasting uses the clipboard.
This is in addition to, but in preference to, the primary selection."
  :type 'boolean
  :group 'killing)

(defcustom xclip-use-pbcopy&paste (and xclip-select-enable-clipboard
                                       (eq system-type 'darwin)
                                       (executable-find "pbcopy")
                                       t)
  "Non-nil means using pbcopy and pbpaste instead of xclip.
If non-nil `xclip-program' is ignored."
  :type 'boolean
  :group 'killing)

(defvar xclip-last-selected-text-clipboard nil
  "The value of the CLIPBOARD X selection from xclip.")

(defvar xclip-last-selected-text-primary nil
  "The value of the PRIMARY X selection from xclip.")

(defun xclip-set-selection (type data)
  "TYPE is a symbol: primary, secondary and clipboard.

See also `x-set-selection'."
  (let* ((process-connection-type nil)
         (proc (cond
                (xclip-use-pbcopy&paste
                 (start-file-process "pbcopy" nil "pbcopy"))
                ((getenv "DISPLAY")
                 (start-file-process "xclip" nil xclip-program
                                     "-selection" (symbol-name type))))))
    (when proc
      (process-send-string proc data)
      (process-send-eof proc))
    data))

(defun xclip-select-text (text)
  "See `x-select-text'."
  (xclip-set-selection 'primary text)
  (setq xclip-last-selected-text-primary text)
  (when xclip-select-enable-clipboard
    (xclip-set-selection 'clipboard text)
    (setq xclip-last-selected-text-clipboard text)))

(defun xclip-selection-value ()
  "See `x-selection-value'."
  (let ((clip-text (when xclip-select-enable-clipboard
                     (with-output-to-string
                       (cond
                        (xclip-use-pbcopy&paste
                         (process-file "pbpaste" nil standard-output nil))
                        ((getenv "DISPLAY")
                         (process-file xclip-program nil standard-output nil
                                       "-o" "-selection" "clipboard")))))))
    (setq clip-text
          (cond                         ; Check clipboard selection.
           ((or (not clip-text) (string= clip-text ""))
            (setq xclip-last-selected-text-clipboard nil))
           ((eq clip-text xclip-last-selected-text-clipboard)
            nil)
           ((string= clip-text xclip-last-selected-text-clipboard)
            ;; Record the newer string so subsequent calls can use the
            ;; `eq' test.
            (setq xclip-last-selected-text-clipboard clip-text)
            nil)
           (t (setq xclip-last-selected-text-clipboard clip-text))))
    (or clip-text
        (when (and (not xclip-use-pbcopy&paste) (getenv "DISPLAY"))
          (let ((primary-text (with-output-to-string
                                (process-file xclip-program nil
                                              standard-output nil "-o"))))
            (setq primary-text
                  (cond                 ; Check primary selection.
                   ((or (not primary-text) (string= primary-text ""))
                    (setq xclip-last-selected-text-primary nil))
                   ((eq primary-text xclip-last-selected-text-primary)
                    nil)
                   ((string= primary-text xclip-last-selected-text-primary)
                    ;; Record the newer string so subsequent calls can
                    ;; use the `eq' test.
                    (setq xclip-last-selected-text-primary primary-text)
                    nil)
                   (t (setq xclip-last-selected-text-primary primary-text))))
            primary-text)))))

(defun turn-on-xclip ()
  (setq interprogram-cut-function 'xclip-select-text)
  (setq interprogram-paste-function 'xclip-selection-value))

;;;###autoload
(define-minor-mode xclip-mode
  "Minor mode to use the `xclip' program to copy&paste."
  :global t
  (if xclip-mode
      (progn
        (or xclip-use-pbcopy&paste
            (executable-find xclip-program)
            (signal 'file-error (list "Searching for program"
                                      xclip-program "no such file")))
        ;; NOTE: See `tty-run-terminal-initialization' and term/README
        (add-hook 'terminal-init-xterm-hook 'turn-on-xclip))
    (remove-hook 'terminal-init-xterm-hook 'turn-on-xclip)))

(provide 'xclip)
;;; xclip.el ends here
