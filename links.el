;;; links.el -- A first gasp at an Emacs mode for Links
;;;             (Horribly broken, for sure.)
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


(defvar links-mode-syntax-table nil
  "Syntax table for links-mode buffers")
(if links-mode-syntax-table
    ()
  (setq links-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" links-mode-syntax-table)
  (modify-syntax-entry ?# "<" links-mode-syntax-table)
  (modify-syntax-entry ?\n ">" links-mode-syntax-table)
  (modify-syntax-entry ?{ "(}" links-mode-syntax-table)
  (modify-syntax-entry ?} "){" links-mode-syntax-table)
  (modify-syntax-entry ?_ "w" links-mode-syntax-table)
  )

(defconst links-font-lock-keywords
  (list
   ; comments
   '("[^</]#.*$" . font-lock-comment-face)
   ; XML forests
   '("<#>.*</#>" . font-lock-xml-face)
   ; XML tags
   '("</?[a-z][^>]*>" 0 font-lock-xml-face t)
   ; XML escapes
   '("\"{[^}]*}\"" 0 font-lock-normal-face t)
   ; declarations
   '("\\<fun\\|var\\|sig\\>" . font-lock-type-face)
   ; special operations
   '("\\<\\(table\\|with\\|from\\|for\\|where\\|switch\\|case\\|database\\|if\\|else\\|insert\\|values\\|form\\|yields\\)\\>" 
     . font-lock-keyword-face)
   ; types & variant tags
   '("\\<[A-Z][A-Za-z0-9_]*\\>" . font-lock-function-name-face)
   ; variable names
   '("\\<\\(fun\\|var\\|sig\\) +\\([a-z][A-Za-z0-9_]*\\)\\>" 
     2 font-lock-variable-name-face)
   ; variable names
   '("\\<\\(l:name\\)=\"\\([a-z][A-Za-z0-9_]*\\)\"" 
     2 font-lock-variable-name-face t)
   ; type operators
   '("->" . font-lock-function-name-face)
   ))

(defun initialize-font-lock-defaults ()
  (make-face 'Firebrick)
  (set-face-foreground 'Firebrick "Firebrick")
  (make-face 'RosyBrown)
  (set-face-foreground 'RosyBrown "RosyBrown")
  (make-face 'Purple)
  (set-face-foreground 'Purple "Purple")
  (make-face 'MidnightBlue)
  (set-face-foreground 'MidnightBlue "MidnightBlue")
  (make-face 'DarkGoldenRod)
  (set-face-foreground 'DarkGoldenRod "DarkGoldenRod")
  (make-face 'DarkOliveGreen)
  (set-face-foreground 'DarkOliveGreen "DarkOliveGreen4")
  (make-face 'CadetBlue)
  (set-face-foreground 'CadetBlue "CadetBlue")
  (make-face 'Black)
  (set-face-foreground 'Black "Black")
  
  (setq font-lock-comment-face 'Firebrick)
  (setq font-lock-type-face 'Purple)
  (setq font-lock-keyword-face 'MidnightBlue)
  (setq font-lock-variable-name-face 'DarkGoldenRod)
  (setq font-lock-function-name-face 'DarkOliveGreen)
  (setq font-lock-xml-face 'RosyBrown)
  (setq font-lock-normal-face 'Black)
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(links-font-lock-keywords nil nil ((?' . "w") (?_ . "w"))))
  )

(defun links-mode ()
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'links-mode)
  (setq mode-name "Links")

  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "#")

  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  
  (initialize-font-lock-defaults)

  (run-hooks 'links-mode-hook)
  )

(provide 'links)
