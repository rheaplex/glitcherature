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
  "The default 1/n probability of changing letter or word cases"
  :type '(integer)
  :group 'glitcherature-mode)

(defcustom glitcherature-line-length 80
  "The line length for various operations"
  :type '(integer)
  :group 'glitcherature-mode)

(defcustom glitcherature-alteration-probability 10
  "The default 1/n probability for various insert/delete operations"
  :type '(integer)
  :group 'glitcherature-mode)

(defcustom glitcherature-insert-count 10
  "The default number of items to insert for various operations"
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
  "Generate a string of the given length repeating a character randomly chosen
   from source"
  (let ((char (random-char source))
        (chars '()))
    (dotimes (i length) (push char chars))
    (apply 'concat chars)))

(defun random-chars-run (source length)
  "Generate a string of length characters randomly chosen from source"
  (let ((chars '()))
    (dotimes (i length) (push (random-char source) chars))
    (apply 'concat chars)))

(defun repeat-run (char length)
  "Generate a string of char repeated length times"
  (let ((chars '()))
    (dotimes (i length) (push char chars))
    (apply 'concat chars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OCR Text substitution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun glitcherature-ocr-line (line line-font-size)
  "Render the line of text at line-font size and get the OCR results.
   Requires that gocr be installed."
  (shell-command-to-string (format "pbmtextps -fontsize %d \"%s\" | gocr -"
                                   line-font-size line)))

(defun glitcherature-ocr-replace-text (start end font-size)
  "Rasterise the text in the region then OCR it, replacing the original text.
   The size of the rasterized text is set by the prefix arg, or defaults to
   a suitably small value (e.g. 6)."
  (interactive "r\np")
  (if (region-active-p)
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
        (insert glitched-text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun glitcherature-random-run (count chars)
  "Insert a run of randomly chosen characters from chars.
   The count is taken from the numeric prefix arg, or uses a default value
   where the prefix argument is not supplied."
  (interactive "p\nsCharacters to choose from: ")
  (let ((reps (if (and current-prefix-arg
                       (not (consp current-prefix-arg)))
                  count
                glitcherature-line-length)))
    (insert (random-chars-run chars reps))))

(defun glitcherature-sub-space-runs (start end max-run-length chars)
  "Replace spaces in the region with a random run of characters specified
   interactively.
   Each run will be between one and the numeric prefix arg (or a default value)
   in length"
  (interactive "r\np\nsCharacters to choose from: ")
  (if (region-active-p)
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (while (re-search-forward " +" nil t)
            (replace-match (random-char-run chars
                                            (+ (random max-run-length) 1))))))))

(defun glitcherature-repeat-run (start end probability max-run-length)
  "Repeat existing characters in the region between 1 and an interactively
   specified number of times 1/probability (specified as the prefix argument
   or using a default value) of the time"
  (interactive "r\np\nnMax run length: ")
  (if (region-active-p)
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (let ((prob (if (and current-prefix-arg
                               (not (consp current-prefix-arg)))
                          probability
                        glitcherature-alteration-probability)))
            (while (re-search-forward "." nil t)
              (narrow-to-region start (region-end))
              (if (= (random prob) 0)
                  (replace-match
                   (repeat-run (match-string 0)
                               (+ 1 (random max-run-length)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrappers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun glitcherature-wrap-words (start end probability before after)
  "Wrap words in the region with interactively specified before and after
   values 1/probability (specified as the prefix argument or using a default
   value) of the time"
  (interactive "r\np\nsBefore: \nsAfter: ")
  (if (region-active-p)
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (while (re-search-forward " +" nil t)
            (if (= (random probability) 0)
                (replace-match (format "%s%s%s" before (match-string 0)
                                       after))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun glitcherature-random-letter-case (start end probability)
  "Change each letter in the region to uppercase 1/probability (specified as
   the prefix argument or using a default value) of the time"
  (interactive "r\np")
  (if (region-active-p)
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (let ((prob (if (and current-prefix-arg
                               (not (consp current-prefix-arg)))
                          probability
                        glitcherature-case-probability)))
            (while (re-search-forward "\\w" nil t)
              (if (= (random prob) 0)
                  (replace-match (upcase (match-string 0))))))))))

(defun glitcherature-random-word-case (start end probability)
  "Change each word in the region to upper or lower case.
   upper case is 1/probability (specified as the prefix argument or using a
   default value) of the time"
  (interactive "r\np")
  (if (region-active-p)
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
            (replace-match (downcase (match-string 0))))))))))

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
  "Convert the byte representing each character in the region to a binary
   string"
  (interactive "r")
  (if (region-active-p)
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (while (re-search-forward "." nil t)
            (replace-match (glitcherature-ascii-bin-char (match-string 0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insertions and deletions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun glitcherature-spaces-delete (start end probability)
  "Delete each space in the region 1/probability (specified as the prefix
   argument or using a default value) of the time"
  (interactive "r\np")
  (if (region-active-p)
      (let ((prob (if (and current-prefix-arg
                           (not (consp current-prefix-arg)))
                      probability
                    glitcherature-alteration-probability)))
        (save-excursion
          (save-restriction
            (narrow-to-region start end)
            (while (re-search-forward " " nil t)
              (if (= (random prob) 0)
                  (replace-match ""))))))))

(defun glitcherature-insert-count (start end count delete fun)
  "Call fun count times and insert at random positions in the region"
  (if (region-active-p)
      (let ((amount (if (and current-prefix-arg
                             (not (consp current-prefix-arg)))
                        count
                      glitcherature-insert-count)))
        (save-excursion
          (save-restriction
            (dotimes (i amount)
              (narrow-to-region start (region-end))
              (let ((pos (random (- (region-end) start))))
                (if delete
                    (delete-char))
                (goto-char pos)
                (insert (fun)))))))))

(defun glitcherature-newlines-insert (start end count)
  "Insert prefix (or default) newlines into the region"
  (interactive "r\np")
  (glitcherature-insert-count start end count nil (lambda () "\n")))

(defun glitcherature-newlines-overwrite (start end count)
  "Replace prefix (or default) characters in the region with newlines"
  (interactive "r\np")
  (glitcherature-insert-count start end count t (lambda () "\n")))

(defun glitcherature-spaces-insert (start end count)
  "Insert prefix (or default) spaces into the region"
  (interactive "r\np")
  (glitcherature-insert-count start end count nil (lambda () " ")))

(defun glitcherature-spaces-overwrite (start end count)
  "Replace prefix (or default) with spaces into the region"
  (interactive "r\np")
  (glitcherature-insert-count start end count t (lambda () " ")))

(defun random-ascii-character ()
  "Generate a random printable ASCII character"
  (format "%c" (+ 32 (random 94))))

(defun random-unicode-character ()
  "Generate a random unicode character (within Emacs' limits)"
  (format "%c" (random 4194303)))

(defun glitcherature-ascii-insert (start end count)
  "Insert prefix (or default) ascii characters into the region"
  (interactive "r\np")
  (glitcherature-insert-count start end count nil 'random-ascii-character))

(defun glitcherature-ascii-overwrite (start end count)
  "Replace prefix (or default) with ascii characters into the region"
  (interactive "r\np")
  (glitcherature-insert-count start end count t 'random-ascii-character))

(defun glitcherature-unicode-insert (start end count)
  "Insert prefix (or default) unicode characters into the region"
  (interactive "r\np")
  (glitcherature-insert-count start end count nil 'random-unicode-character))

(defun glitcherature-unicode-overwrite (start end count)
  "Replace prefix (or default) with unicode characters into the region"
  (interactive "r\np")
  (glitcherature-insert-count start end count t 'random-unicode-character))

;; Substiture % letters, vowels, consonants

;; Insert inappropriate hyphenation

;; Substitute random typing errors (near letters on keyboard)

;; Substitute typing errors then autocorrect

;; Shift % letters > unicode range (add number, convert to char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-glitcherature
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Successive application on paragraphs

;; Successive application on sentences, specify max random count of sentences
;; as prefix argument

;; Recursive application, larger then smaller containing regions

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
  "Replace vowels in the current region with 1337
   1/probability (specified as the prefix argument or using a
   default value) of the time"
  (interactive "r\np")
  (if (region-active-p)
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (let ((column (random glitcherature-leet-kinds-count)))
            (while (re-search-forward "[aeiou]" nil t)
              (if (= (random probability) 0)
                  (replace-match (or (nth 1
                                          (assoc (downcase (match-string 0))
                                                 glitcherature-leet-lookup))
                                     (match-string 0))))))))))

(defun glitcherature-sub-leet (start end probability)
  "Replace letters in the current region with 1337 1/probability
   (specified as the prefix argument or using a default value) of the time"
  (interactive "r\np")
  (if (region-active-p)
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (let ((column (random glitcherature-leet-kinds-count)))
            (while (re-search-forward "[a-z]" nil t)
              (if (= (random probability) 0)
                  (replace-match (or (nth 1
                                          (assoc (downcase (match-string 0))
                                                 glitcherature-leet-lookup))
                                 (match-string 0))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst glitcherature-mode-keymap (make-keymap))

(define-key glitcherature-mode-keymap (kbd "C-c C-g a")
  'glitcherature-ascii-insert)
(define-key glitcherature-mode-keymap (kbd "C-c C-g A")
  'glitcherature-ascii-overwrite)
(define-key glitcherature-mode-keymap (kbd "C-c C-g b")
'glitcherature-ascii-bin)
(define-key glitcherature-mode-keymap (kbd "C-c C-g c")
  'glitcherature-random-letter-case)
(define-key glitcherature-mode-keymap (kbd "C-c C-g C")
  'glitcherature-random-word-case)
(define-key glitcherature-mode-keymap (kbd "C-c C-g i")
  'glitcherature-spaces-insert)
(define-key glitcherature-mode-keymap (kbd "C-c C-g l")
  'glitcherature-sub-leet)
(define-key glitcherature-mode-keymap (kbd "C-c C-g n")
  'glitcherature-newlines-insert)
(define-key glitcherature-mode-keymap (kbd "C-c C-g N")
  'glitcherature-newlines-overwrite)
(define-key glitcherature-mode-keymap (kbd "C-c C-g o")
  'glitcherature-ocr-replace-text)
(define-key glitcherature-mode-keymap (kbd "C-c C-g O")
  'glitcherature-spaces-overwrite)
(define-key glitcherature-mode-keymap (kbd "C-c C-g r")
  'glitcherature-random-run)
(define-key glitcherature-mode-keymap (kbd "C-c C-g R")
  'glitcherature-repeat-run)
(define-key glitcherature-mode-keymap (kbd "C-c C-g s")
  'glitcherature-sub-space-runs)
(define-key glitcherature-mode-keymap (kbd "C-c C-g S")
  'glitcherature-spaces-delete)
(define-key glitcherature-mode-keymap (kbd "C-c C-g u")
  'glitcherature-unicode-insert)
(define-key glitcherature-mode-keymap (kbd "C-c C-g U")
  'glitcherature-unicode-overwrite)
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
