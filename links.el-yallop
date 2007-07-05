;; Mode for editing Links files.
;; Will eventually provide at least:
;;   * syntax highlighting
;;   * indentation
;;   * interpreter integration (send commands to interpreter, jump to
;;     error in source code buffer)
;;   * block navigation
;; and perhaps
;;   * context-sensitive completion (ala Slime, nxml-mode)
;;   * type checking as you type (ala Java mode in Eclipse)
;;   * context-sensitive help
;;   * jump to definition (M-.)
;;   * other stuff

(defun links-mode ()
  "Major mode for editing Links files

Key bindings
\\{links-mode-map}
"	; TODO: expand this docstring
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'links-mode
 	mode-name "Links"
 	indent-line-function 'links-indent-line)
    (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(links-font-lock-keywords t))
  ;;(set (make-local-variable 'imenu-generic-expression) 'links-mode-imenu-thing)
  (set-syntax-table links-mode-syntax-table)
  (use-local-map links-mode-map)
  
  ;;   ;; last thing to do
  (run-hooks 'links-mode-hook))


(defun links-indent-line ())

(defun links-internal-fontlock-set (face &rest disjuncts)
  (cons (format "\\<\\(%s\\)\\>" (regexp-opt disjuncts)) face))

(defvar links-font-lock-keywords
  (list 
   ;; builtin functions/operators
   (links-internal-fontlock-set 'font-lock-function-name-face
				"string_of_bool" "float_of_int" "float_of_string" "int_of_string"
				"bool_of_string" "string_of_int" "string_of_float"
				"pre" "abs")
   ;; keywords
   (links-internal-fontlock-set 'font-lock-builtin-face
				"exit" "def" "defrec" "let" "letrec" "in" "and"
				"fun" "if" "then" "else" "case" "of" "or" "for"
				"sort_up" "sort_down" "table" "from" "with"
				"unique" "order" "asc" "desc" "database")
   ;; constants
   (links-internal-fontlock-set 'font-lock-constant-face
				"true" "false")

   ;; types
   (links-internal-fontlock-set 'font-lock-type-face
				"bool" "int" "float" "string")))
   
(defvar links-mode-font-lock-thing 
  '(links-font-lock-keywords 		; keywords
    nil 				; keywords-only
    nil					; case-fold
    nil                       	       	; syntax-alist
    nil)				; syntax-begin
 "Links mode highlighting information")

(defvar links-mode-map
  (let ((links-mode-map (make-keymap)))
    (define-key links-mode-map "\C-j" 'newline-and-indent)
    (define-key links-mode-map "\C-c\C-c" 'links-evaluate-region)
    links-mode-map))




(defvar links-mode-syntax-table 
  (let ((links-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" links-mode-syntax-table)
    (modify-syntax-entry ?( "(" links-mode-syntax-table)
    (modify-syntax-entry ?{ "(" links-mode-syntax-table)
    (modify-syntax-entry ?[ "(" links-mode-syntax-table)
    (modify-syntax-entry ?< "(" links-mode-syntax-table)
    (modify-syntax-entry ?) ")" links-mode-syntax-table)
    (modify-syntax-entry ?} ")" links-mode-syntax-table)
    (modify-syntax-entry ?] ")" links-mode-syntax-table)
    (modify-syntax-entry ?> ")" links-mode-syntax-table)

    links-mode-syntax-table))

(defvar links-mode-abbrev-table nil "Links mode abbrev table")
(defvar links-mode-imenu-thing nil "Links mode imenu information")
(defvar links-mode-hook nil "Links mode hook")


(defvar links-program-name "/home/s0456219/code/links/gilles/2005-01-10/code 0.6.5 (no-opt)/slinks")

(defun run-links ()
  "Run an inferior Links process, input and output via buffer *slinks*."
  (interactive)
  (switch-to-buffer-other-window (make-comint "slinks" links-program-name nil "-i"))
  )

(defun inferior-links-mode ()
  "Major mode for interacting with the Links interpreter"
  (interactive)
  (require 'comint)
  (comint-mode)
  (setq comint-prompt-regexp "^ *? *")
  (use-local-map links-mode-map)
  (run-hooks 'links-mode-hook))

(defun links-evaluate-region ()
  (interactive)
  (let ((beg nil)
	(end nil))
    (if mark-active
	(setq beg (region-beginning)
	      end (region-end))
      (setq beg (line-beginning-position)
	    end (line-end-position)))
    (save-excursion
      (send-region "slinks" beg end)
      (send-string "slinks" ";;\n")
      (display-buffer "*slinks*"))))

(provide 'links-mode)
