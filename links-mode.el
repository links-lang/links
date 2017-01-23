;;; links-mode.el -- A first gasp at an Emacs mode for Links
;;;                  (Horribly broken, for sure.)
;;; by Ezra Cooper 2007
;;; updated by Stefan Fehrenbach 2015
;;; Many bits copped from caml.el by Leroy et al.
;;;
;;; Things that work:
;;;    * some syntax highlighting
;;;    * (un)comment-region
;;;
;;; Things that don't work:
;;;    * syntax highlighting of XML quasis.
;;;
(require 'compile)

(defgroup links nil
  "Links-mode customization.")

(defcustom links-mode-hook nil
  "Hook run when entering Links mode."
  :type 'hook)

(defcustom links-executable "links"
  "Path to Links executable."
  :type '(string))

(defcustom links-cli-arguments ""
  "Command line arguments (as a string) passed to links."
  :type '(string))

(defvar links-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\  " " st)
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?( "()" st)
    (modify-syntax-entry ?) ")(" st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    (modify-syntax-entry ?[ "(]" st)
    (modify-syntax-entry ?] ")[" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?' "w" st)
    st))

;; Can be generated with `links --print-keywords`.
;; TODO We should do that automatically as part of the build process somehow.
(defconst links-keywords
  '(
    "alien"
    "as"
    "case"
    "client"
    "database"
    "default"
    "delete"
    "do"
    "else"
    "escape"
    "false"
    "for"
    "forall"
    "from"
    "fun"
    "formlet"
    "handle"
    "handler"
    "if"
    "in"
    "open"
    "yields"
    "insert"
    "linfun"
    "module"
    "mu"
    "native"
    "nu"
    "offer"
    "orderby"
    "op"
    "page"
    "query"
    "readonly"
    "receive"
    "returning"
    "select"
    "server"
    "set"
    "shallowhandle"
    "shallowhandler"
    "sig"
    "spawn"
    "spawnAngel"
    "spawnClient"
    "spawnDemon"
    "spawnWait"
    "switch"
    "table"
    "TableHandle"
    "true"
    "typename"
    "update"
    "values"
    "var"
    "where"
    "with"
    ))

(defconst links-font-lock-keywords
  (list
   ;; comments
   '("\\(^\\|[^</]\\)#.*$" . font-lock-comment-face)
   ;; XML forests
   '("<#>.*</#>" . font-lock-xml-face)
   ;; XML tags
   '("</?[a-z][^>]*>" 0 font-lock-xml-face t)
   ;; XML escapes (attributes)
   '("\"{[^}]*}\"" 0 font-lock-normal-face t)
   ;; special operations
   `(,(regexp-opt links-keywords 'words) . font-lock-keyword-face)
   ;; types & variant tags
   '("\\<[A-Z][A-Za-z0-9_]*\\>" . font-lock-type-face)
   ;; variable names
   '("\\<\\(var\\) +\\([a-z][A-Za-z0-9_]*\\)\\>"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
   ;; function names
   '("\\<\\(fun\\|sig\\) +\\([a-z][A-Za-z0-9_]*\\)\\>"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
   ;; handler names
   '("\\<\\(open handler\\|handler\\|open shallowhandler\\|shallowhandler\\) +\\([a-z][A-Za-z0-9_]*\\)\\>"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
   ;; type operators
   '("\\(-\\|~\\)\\(>\\|@\\)" . font-lock-function-name-face)
   '("\\(-\\|~\\)\\([a-z]+\\)\\(-\\|~\\)\\(>\\|@\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-function-name-face)
     (4 font-lock-function-name-face))
   ))

(defun initialize-font-lock-defaults ()
  (setq-local font-lock-defaults
              '(links-font-lock-keywords)))

(defun links-compilation-errors ()
  ;; TODO: fix our compilation-error-regexp-alist use
  ;;
  ;; For some reason adding to c-e-r-a and c-e-r-a-a does not work. Only adding
  ;; to c-e-r-a also does not work. I tried these and a cople more things. If
  ;; you understand how this works, please tell me/fix it.
  ;;
  ;; (add-to-list 'compilation-error-regexp-alist 'links-parse)
  ;; (add-to-list 'compilation-error-regexp-alist 'links-type)
  ;; (add-to-list 'compilation-error-regexp-alist
  ;;              '(links-parse "^*** Parse error: \\(.*\\):\\([0-9]+\\)$" 1 2))
  ;; (add-to-list 'compilation-error-regexp-alist-alist
  ;;              '(links-type "^\\(.*\\):\\([0-9]+\\): Type error:" 1 2))
  ;;
  ;;
  ;; For now, if you don't care about breaking error parsing for all other
  ;; compilers, you can just uncomment the next lines. They replace the whole
  ;; c-e-r-a without even using c-e-r-a-a. This, of course, breaks all other
  ;; users. You have been warned.
  ;;
  ;; (setq compilation-error-regexp-alist
  ;;       '(("^*** Parse error: \\(.*\\):\\([0-9]+\\)$" 1 2)
  ;;         ("^\\(.*\\):\\([0-9]+\\): Type error:" 1 2)))
  )

(defun links-compile-and-run-file ()
  "Compile the current file in Links. This may execute sideeffecting code, so be careful."
  (interactive)
  (let ((command (combine-and-quote-strings
                  `(,links-executable
                    ,links-cli-arguments
                    ,buffer-file-name))))
    (compile command)))

(defvar links-mode-map
  (let ((m (make-keymap)))
    (define-key m (kbd "C-c C-k") 'links-compile-and-run-file)
    m))

;;;###autoload
(define-derived-mode links-mode prog-mode "Links"
  "Major mode for Links."
  :syntax-table links-mode-syntax-table
  (setq-local mode-name "Links")
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local require-final-newline t)
  (initialize-font-lock-defaults)
  (links-compilation-errors)
  (run-hooks 'links-mode-hook)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.links\\'" . links-mode))

(provide 'links-mode)
