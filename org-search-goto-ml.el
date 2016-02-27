;;; org-search-goto-ml.el --- Use multiline search to go to locations in your org buffers

;; Copyright (C) 2011

;; Author: Tom <adatgyujto@gmail.com>
;; Keywords:

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
;; Usage: M-x osg, then start typing
;;
;; Select from the matches with up/down/pgup/pgdown and press enter
;; to go that location
;; (you can navigate the history with M-p/M-n)
;;
;; If the search string contains several strings separated by spaces then
;; these substrings can appear in any order in the results.
;;
;; The search handles an org heading and its text as one block, so
;; the substrings can match in any part of an org entry.
;;
;; If the heading is not matching, only the body, the heading is added
;; to the results anyway to indicate which entry is matched.

;;; History:
;;
;; 2011-12-11: Tom <adatgyujto@gmail.com> posted the original version
;; of org-search-goto on the org-mode mailing list:
;; <https://lists.gnu.org/archive/html/emacs-orgmode/2011-12/msg00196.html>. He
;; said it was a variant of org-occur-goto, which was also created by him:
;; <https://www.emacswiki.org/emacs/org-occur-goto.el>.

;; 2011-12-15: Tom posted org-search-goto-ml:
;; <https://lists.gnu.org/archive/html/emacs-orgmode/2011-12/msg00515.html>.

;; 2012-01-09: anon made minor fixes to the copy of
;; org-search-goto-ml.el on EmacsWiki:
;; <https://www.emacswiki.org/emacs/org-search-goto-ml.el>.

;; 2016-02-27: Adam Porter made a minor fix and uploaded it to a
;; GitHub repo: <https://github.com/alphapapa/org-search-goto>.


;;; Code:


(require 'cl)


(defvar org-search-goto-idle-delay 0.5)

(setq org-search-goto-max-results 50)

(defvar org-search-goto-match-face 'match)

(defvar org-search-goto-header-face 'underline)

(defvar org-search-goto-warning-face 'font-lock-warning-face)

(defvar org-search-goto-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<down>") 'org-search-goto-next-line)
    (define-key map (kbd "<up>") 'org-search-goto-previous-line)
    (define-key map (kbd "<prior>") 'org-search-goto-previous-page)
    (define-key map (kbd "<next>") 'org-search-goto-next-page)
    map))





(defvar org-search-goto-buffer-name "*org search goto*")

(defvar org-search-goto-history-list nil)

(defvar org-search-goto-org-buffers nil)

(defvar org-search-goto-history-list nil)

(defvar org-search-goto-line-info nil)

(defvar org-search-goto-orig-window nil)

(defvar org-search-goto-orig-buffer nil)


(defun org-search-goto-previous-line ()
  (interactive)
  (org-search-goto-move-selection 'forward-line -1))


(defun org-search-goto-next-line ()
  (interactive)
  (org-search-goto-move-selection 'forward-line 1))


(defun org-search-goto-previous-page ()
  (interactive)
  (org-search-goto-move-selection 'scroll-down))


(defun org-search-goto-next-page ()
  (interactive)
  (org-search-goto-move-selection 'scroll-up))


(defun org-search-goto-move-selection (movefunc &optional movearg)
  (let ((win (get-buffer-window org-search-goto-buffer-name)))
    (if win
        (with-selected-window win
          (condition-case nil
              (funcall movefunc movearg)
            (beginning-of-buffer (goto-char (point-min)))
            (end-of-buffer (goto-char (point-max))))

          (setq org-search-goto-line-info (get-text-property (line-beginning-position) 'org-search-goto-line-info))))))


;; adapted from EmacsWiki: http://www.emacswiki.org/emacs/StringPermutations
(defun org-search-goto-list-permutations (l)
  (if (null l)
      (list '())
    (mapcan #'(lambda( a )
                (mapcan #'(lambda( p )
                            (list (cons a p)))
                        (org-search-goto-list-permutations (remove* a l :count 1))))
            l)))



(defun org-search-goto-check-input ()
  (when (sit-for org-search-goto-idle-delay)
    (let ((input (split-string (minibuffer-contents) " " t)))
      (unless (equal input org-search-goto-current-input)
        (setq org-search-goto-current-input input)

        (with-selected-window (get-buffer-window org-search-goto-buffer-name)
          (erase-buffer))

        (when input
          (let ((number-of-words (length input))
                (result-count 0)
                (buffers org-search-goto-org-buffers)
                prematch)

            (setq prematch (mapconcat (lambda (m)
                                        (concat "\\(" (regexp-quote m) "\\)"))
                                      input
                                      "\\|"))

            (move-overlay org-search-goto-overlay
                          (line-end-position)
                          (line-end-position)
                          (current-buffer))

            (let ((cursor-type nil))
              (redisplay t)
              (unwind-protect
                  (while (and buffers
                              (< result-count org-search-goto-max-results))
                    (let ((buffer (pop buffers)))
                      (with-current-buffer buffer
                        (save-excursion
                          (goto-char (point-min))
                          (let ((header-not-printed (buffer-name)))
                            (while (and (< result-count org-search-goto-max-results)
                                        (re-search-forward prematch nil t))
                              (let* ((start (save-excursion
                                              (save-match-data
                                                (outline-previous-heading)
                                                (point))))
                                     (end (save-match-data
                                            (save-excursion
                                              (outline-next-heading)
                                              (point))))
                                     rest
                                     (match t)
                                     matches)

                                (let ((i 1))
                                  (mapc (lambda (m)
                                          (unless (match-string i)
                                            (push m rest))
                                          (incf i))
                                        input))

                                (while (and match
                                            (let* ((pos (line-beginning-position))
                                                   (info (assoc pos matches))
                                                   (match-pos (cons (- (match-beginning 0) pos)
                                                                    (- (match-end 0) pos))))
                                              (if info
                                                  (setcdr info (cons match-pos (cdr info)))
                                                (push (list pos match-pos) matches)))
                                            rest
                                            (goto-char start))
                                  (setq match (search-forward (pop rest) end t)))

                                (when match
                                  (setq matches (sort matches (lambda (a b)
                                                                (< (car a) (car b)))))

                                  (setq match
                                        (mapconcat
                                         (lambda (info)
                                           (goto-char (car info))
                                           (let ((str (buffer-substring (line-beginning-position)
                                                                        (line-end-position))))
                                             (dolist (m (cdr info))
                                               (put-text-property (car m) (cdr m) 'face org-search-goto-match-face
                                                                  str))
                                             str))
                                         matches
                                         " ... "))

                                  (goto-char (caar matches))

                                  (unless (= start (point))
                                    (goto-char start)
                                    (setq match (concat (buffer-substring (line-beginning-position)
                                                                          (line-end-position))
                                                        " ... "
                                                        match)))

                                  (let ((line-num (1+ (count-lines (point) (point-min)))))
                                    (with-current-buffer org-search-goto-buffer-name
                                      (when header-not-printed
                                        (insert (propertize header-not-printed 'face org-search-goto-header-face) "\n")
                                        (setq header-not-printed nil))

                                      (insert (format "%7d:" line-num) match)
                                      (put-text-property (line-beginning-position) (1+ (line-beginning-position))
                                                         'org-search-goto-line-info (list 'buffer buffer 'line line-num))
                                      (insert "\n")))

                                  (incf result-count))

                                (goto-char end))))))))

                (delete-overlay org-search-goto-overlay)))

            (with-selected-window (get-buffer-window org-search-goto-buffer-name)
              (goto-char (point-min))
              (org-search-goto-next-line))

            (if (and (= result-count org-search-goto-max-results)
                     (sit-for 0.2))
                (message (propertize "Too many matches, keep typing to narrow it down more"
                                     'face org-search-goto-warning-face)))))))))



(defun org-search-goto ()
  (interactive)

  (setq org-search-goto-overlay (make-overlay (point) (point)))
  (overlay-put org-search-goto-overlay 'after-string (concat "  " (propertize "searching..." 'face org-search-goto-warning-face)))

  (setq org-search-goto-org-buffers
        (delete-if 'null
                   (mapcar (lambda (b)
                             (with-current-buffer b
                               (if (eq major-mode 'org-mode)
                                   b)))
                           (buffer-list))))

  (let ((cursor-in-non-selected-windows 'box))
    (save-window-excursion
      (add-hook 'post-command-hook 'org-search-goto-check-input)

      (setq org-search-goto-current-input nil)
      (setq org-search-goto-line-info nil)
      (setq org-search-goto-orig-window (selected-window))
      (setq org-search-goto-orig-buffer (current-buffer))

      (if (equal (buffer-name) org-search-goto-buffer-name)
          (kill-buffer))
      (save-selected-window
        (pop-to-buffer org-search-goto-buffer-name)
        (erase-buffer))

      (unwind-protect
          (let ((minibuffer-local-map org-search-goto-map))
            (read-string "Search for: " nil 'org-search-goto-history-list))

        (remove-hook 'post-command-hook 'org-search-goto-check-input))))

  (if (not org-search-goto-line-info)
      (message "No match on this line.")

    (switch-to-buffer (plist-get org-search-goto-line-info 'buffer))
    (goto-line (plist-get org-search-goto-line-info 'line))
    (when (outline-invisible-p)
      (save-excursion
        (outline-previous-visible-heading 1)
        (org-show-subtree)))))





(provide 'org-search-goto-ml)
;;; org-search-goto-ml.el ends here
