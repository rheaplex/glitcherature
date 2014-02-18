;;; glitcherature-mode.el --- A minor mode to glitch text.
;;
;; Copyright (c) 2014 Rob Myers <rob@robmyers.org>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage
;;
;; To use, save glitcherature-mode.el to a directory in your load-path.
;;
;; (require 'glitcherature-mode)
;; (add-hook 'text-mode-hook 'glitcherature-mode)
;;
;; or
;;
;; M-x glitcherature-mode
;;
;; Dependencies
;;
;; The ocr function requires that gocr be installed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable features individually

(defcustom glitcherature-line-font-size 6
  "The size to rasterize text at for OCR"
  :type '(integer)
  :group 'glitcherature-mode)

(defcustom glitcherature-case-probability 2
  "The default 1/n probability of changing letter cases"
  :type '(integer)
  :group 'glitcherature-mode)

(defcustom glitcherature-line-length 80
  "The line length for various operations"
  :type '(integer)
  :group 'glitcherature-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-char (source)
  "Pick one character from source at random"
  (let ((index (random (length source))))
    (substring source index (+ index 1))))

(defun random-char-run (source length)
  "Generate a string of length of one character randomly chosen from source"
  (let ((char (random-char source))
        (chars '()))
    (dotimes (i length) (push char chars))
    (apply 'concat chars)))

(defun random-chars-run (source length)
  "Generate a string of length characters randomly chosen from source"
  (let ((chars '()))
    (dotimes (i length) (push (random-char source) chars))
    (apply 'concat chars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OCR Text substitution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun glitcherature-ocr-line (line line-font-size)
  (shell-command-to-string (format "pbmtextps -fontsize %d \"%s\" | gocr -"
                                   line-font-size line)))

(defun glitcherature-ocr-replace-text (start end font-size)
  "Rasterise the text then OCR it, replacing the original text"
  (interactive "r\np")
  (let* ((original-text (buffer-substring-no-properties start end))
         (original-lines (split-string original-text "\n"))
         (line-font-size (if (and current-prefix-arg
                                  (not (consp current-prefix-arg)))
                             font-size
                           glitcherature-line-font-size))
         (glitched-text (mapconcat
                         (lambda (line)
                           (glitcherature-ocr-line line line-font-size))
                         original-lines
                         "")))
    (delete-region start end)
    (insert glitched-text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun glitcherature-random-run (count chars)
  "Insert a run of randomly chosen characters from chars.
   The count is taken from the numeric prefix arg, or the default."
  (interactive "p\nsCharacters to choose from: ")
  (let ((reps (if (and current-prefix-arg
                       (not (consp current-prefix-arg)))
                  count
                glitcherature-line-length)))
    (insert (random-chars-run chars reps))))

(defun glitcherature-sub-space-runs (start end max-run-length chars)
  "Replace spaces with a random run of chars of max numeric prefix arg length"
  (interactive "r\np\nsCharacters to choose from: ")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (while (re-search-forward " +" nil t)
        (replace-match (random-char-run chars
                                        (+ (random max-run-length) 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrappers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun glitcherature-wrap-words (start end probability before after)
  "Wrap words in the region or after point with before and after
   1/probability of the time"
  (interactive "r\np\nsBefore: \nsAfter: ")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (while (re-search-forward " +" nil t)
        (if (= (random probability) 0)
            (replace-match (format "%s%s%s" before (match-string 0) after)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun glitcherature-random-letter-case (start end probability)
  "Change each letter in the region or after point to uppercase
   1/probability of the time"
  (interactive "r\np")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((prob (if (and current-prefix-arg
                       (not (consp current-prefix-arg)))
                  probability
                  glitcherature-case-probability)))
        (while (re-search-forward "\\w" nil t)
          (if (= (random prob) 0)
              (replace-match (upcase (match-string 0)))))))))

(defun glitcherature-random-word-case (start end probability)
  "Change each word in the region or after point to upper or lower case
   upper case is 1/probability of the time"
  (interactive "r\np")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((prob (if (and current-prefix-arg
                       (not (consp current-prefix-arg)))
                  probability
                  glitcherature-case-probability)))
        (while (re-search-forward "\\w+" nil t)
          (if (= (random prob) 0)
              (replace-match (upcase (match-string 0)))
            (replace-match (downcase (match-string 0)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun glitcherature-ascii-bin-char (char)
  "Convert the byte representing the character to a binary string"
  (let ((num (string-to-char char))
        (bin '()))
    (dotimes (i 8) 
      (push (if (= 1 (logand (lsh num (- i)) 1)) "1" "0") bin))
    (apply 'concat bin)))

(defun glitcherature-ascii-bin (start end)
  "Convert the byte representing each character in the region or after point
   to a binary string"
(interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (while (re-search-forward "." nil t)
            (replace-match (glitcherature-ascii-bin-char (match-string 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insertions and deletions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Insert/overwrite n spaces

;; Delete % spaces

;; Insert n newlines

;; Insert/overwrite n random chars

;; Insert inappropriate hyphenation

;; Substitute random typing errors (near letters on keyboard)

;; Substitute typing errors then autocorrect

;; Substiture % letters, vowels, consonants

;; Shift % letters > unicode range (add number, convert to char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1337
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst glitcherature-leet-lookup
  '(("a". ("@" "/\\\\" "/-\\\\" "λ"))
    ("b" . ("8" "|3" "6" "]3"))
    ("c" . ("(" "{" "<" "©"))
    ("d" . ("|)" "|]" "])" "∂"))
    ("e" . ("3" "£" "€" "="))
    ("f" . ("ʃ" "|=" "]=" ")="))
    ("g" . ("6" "9" "&" "C-"))
    ("h" . ("|-|" "#" "}{" ")-("))
    ("i" . ("!" "1" "|" "`|"))
    ("j" . ("_|" "]" "_/" "_)"))
    ("k" . ("|<" "|{" "|X" "]<"))
    ("l" . ("1" "7" "|_" "|"))
    ("m" . ("44" "/\\\\/\\\\" "|\\\\/|" "|v|"))
    ("n" . ("|\\\\|" "/\\\\/" "И" "~"))
    ("o" . ("()" "[]" "0" "Ø"))
    ("p" . ("|*" "?" "9" "|\""))
    ("q" . ("0_" "0" "(2" "¶"))
    ("r" . ("®" "Я" "I^" "|2"))
    ("s" . ("$" "5" "§" "_\-"))
    ("t" . ("7" "+" "†" "|["))
    ("u" . ("\\\\/" "|_|" "μ" "/_/"))
    ("v" . ("\\\\\\\\//" "\\\\/" "√" "V"))
    ("w" . ("vv" "\\\\/\\\\/" "Ш" "\\\\^/"))
    ("x" . ("%" "><" "*" "Ж"))
    ("y" . ("`/" "\"/" "`(" "-/"))
    ("z" . ("2" "3" "`/_" "%"))))

(defconst glitcherature-leet-kinds-count
  (safe-length (assoc "a" glitcherature-leet-lookup)))

;; Make sure all lists are the same length
(mapc (lambda (choices) (assert (= (safe-length choices)
                                glitcherature-leet-kinds-count)))
      glitcherature-leet-lookup)

;; Make sure no symbol or text appears twice in the same column
;; for the same letter
(dotimes (i (- glitcherature-leet-kinds-count 1))
  (let ((column (mapcar (lambda (row) (nth i row))
                        glitcherature-leet-lookup)))
    (assert (= (safe-length column)
               (safe-length glitcherature-leet-lookup)))))

(defun glitcherature-sub-leet-vowels (start end probability)
  "Replace vowels after point or in the current region
   with 1337 1/probability of the time"
  (interactive "r\np")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((column (random glitcherature-leet-kinds-count)))
        (while (re-search-forward "[aeiou]" nil t)
          (if (= (random probability) 0)
              (replace-match (nth column
                                  (assoc (match-string 0)
                                         glitcherature-leet-lookup)))))))))

(defun glitcherature-sub-leet (start end probability)
  "Replace letters after point or in the current region
   with 1337 1/probability of the time"
  (interactive "r\np")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((column (random glitcherature-leet-kinds-count)))
      (while (re-search-forward "[a-z]" nil t)
        (if (= (random probability) 0)
            (replace-match (nth column
                                (assoc (match-string 0)
                                       glitcherature-leet-lookup)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst glitcherature-mode-keymap (make-keymap))

(define-key glitcherature-mode-keymap (kbd "C-c C-g b")
'glitcherature-ascii-bin)
(define-key glitcherature-mode-keymap (kbd "C-c C-g l")
  'glitcherature-sub-leet)
(define-key glitcherature-mode-keymap (kbd "C-c C-g o")
  'glitcherature-ocr-replace-text)
(define-key glitcherature-mode-keymap (kbd "C-c C-g r")
  'glitcherature-random-run)
(define-key glitcherature-mode-keymap (kbd "C-c C-g v")
  'glitcherature-sub-leet-vowels)
(define-key glitcherature-mode-keymap (kbd "C-c C-g w")
  'glitcherature-wrap-words)

;;;###autoload
(define-minor-mode glitcherature-mode "Glitch text in various ways"
  :lighter " glitch"
  :keymap glitcherature-mode-keymap
  :group 'glitcherature-mode)

(provide 'glitcherature-mode)
