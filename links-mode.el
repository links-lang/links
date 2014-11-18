;;; links-mode.el -- A first gasp at an Emacs mode for Links
;;;                  (Horribly broken, for sure.)
;;; by Ezra Cooper 2007
;;; Many bits copped from caml.el by Leroy et al.
;;;
;;; Things that work:
;;;    * some syntax highlighting
;;;    * (un)comment-region
;;;
;;; Things that don't work:
;;;    * syntax highlighting of XML quasis.
;;;
;;; Report problems to e.e.k.cooper@sms.ed.ac.uk

(defvar links-mode-hook nil)

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

(defconst links-keywords
  '(
    "case"
    "database"
    "else"
    "for"
    "form"
    "from"
    "fun"
    "if"
    "infixl"
    "infixr"
    "insert"
    "op"
    "query"
    "sig"
    "switch"
    "table"
    "typename"
    "values"
    "var"
    "where"
    "with"
    "yields"
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
   ;; type operators
   ;; TODO other arrow types, and decide on a face
   '("->" . font-lock-function-name-face)
   ))

(defun initialize-font-lock-defaults ()
  (setq-local font-lock-defaults
              '(links-font-lock-keywords)))

(define-derived-mode links-mode fundamental-mode "Links"
  "Major mode for Links."
  :syntax-table links-mode-syntax-table
  (setq-local mode-name "Links")
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local require-final-newline t)
  (initialize-font-lock-defaults)
  (run-hooks 'links-mode-hook)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.links\\'" . links-mode))

(provide 'links-mode)
