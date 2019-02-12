;;; links-mode.el --- simple major mode for editing Links. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright: 2019, Jan Stolarek

;; Author: Jan Stolarek (jan.stolarek@ed.ac.uk)
;; Version: 0.1
;; Created: 12 Feb 2019
;; Keywords: languages
;; Homepage: http://links-lang.org/

;; This file is not part of GNU Emacs.

;;; License: GPL2 - see LICENSE.md in root directory of Links project

;;; Commentary:

;; A simple mode for editing Links source files.  Supports:
;;
;;   * highlighting of keywords, base types and built-in functions
;;
;;   * commenting/uncommenting a region using M-;

;;; Code:

;; several categories of words to colour.  Order matters, words ordered by
;; length.

;; list of Links keywords generated from tokens recognized by the lexer
(defconst links-keywords
  '("shallowhandler" "shallowhandle" "spawnAngelAt" "delete_left" "spawnClient"
    "determined" "lensselect" "spawnAngel" "otherwise" "returning" "spawnWait"
    "tablekeys" "database" "lensdrop" "lensjoin" "readonly" "typename" "default"
    "formlet" "handler" "lensget" "lensput" "orderby" "receive" "spawnAt"
    "client" "delete" "escape" "forall" "handle" "insert" "linfun" "module"
    "native" "select" "server" "switch" "update" "values" "yields" "alien"
    "false" "offer" "query" "raise" "spawn" "table" "where" "case" "else" "from"
    "lens" "open" "page" "true" "with" "for" "fun" "set" "sig" "try" "var"
    "as" "by" "do" "if" "in" "mu" "nu" "on" "op"))

;; list of Links primitive types generated from primary_datatype profuction in
;; the parser
(defconst links-primitive-types
  '("Database" "XmlItem" "String" "Float" "Bool" "Char" "Int"))

;; list of Links built-in functions generated using @builtins directive in REPL
;; (excluding operators)
(defconst links-builtins
  '("domRemoveAttributeFromRef" "jsRequestAnimationFrame" "domGetAttributeFromRef"
    "domGetNodeValueFromRef" "domGetStyleAttrFromRef" "domSetAttributeFromRef"
    "domSetStyleAttrFromRef" "serverTimeMilliseconds" "domGetChildrenFromRef"
    "domGetPropertyFromRef" "domSetPropertyFromRef" "registerEventHandlers"
    "domGetTagNameFromRef" "domInsertBeforeRef" "domReplaceChildren"
    "jsLoadGlobalObject" "jsSaveGlobalObject" "domAppendChildRef"
    "getDatabaseConfig" "getTargetElement" "unsafePickleCont" "variantToXmlItem"
    "xmlItemToVariant" "InsertReturning" "debugChromiumGC" "domHasAttribute"
    "getDocumentNode" "replaceDocument" "serveWebsockets" "addStaticRoute"
    "appendChildren" "getFromElement" "getTargetValue" "getTextContent"
    "jsCanvasHeight" "jsGetContext2D" "jsSetFillColor" "jsSetOnKeyDown"
    "readFromSocket" "unsafeAddRoute" "addAttributes" "connectSocket"
    "debugGetStats" "floatToString" "getAttributes" "getChildNodes"
    "getInputValue" "isElementNode" "jsCanvasWidth" "jsSetInterval"
    "jsStrokeStyle" "stringToFloat" "writeToSocket" "domSetAnchor"
    "getAttribute" "getNamespace" "getToElement" "hasAttribute" "insertBefore"
    "jsCanvasFont" "jsFillCircle" "jsSaveCanvas" "jsSetOnEvent" "spawnAngelAt"
    "variantToXml" "xmlToVariant" "closeSocket" "environment" "getCharCode"
    "getNodeById" "intToString" "jsBeginPath" "jsClearRect" "jsClosePath"
    "jsDrawImage" "jsLineWidth" "jsSetOnLoad" "jsTranslate" "newClientAP"
    "newServerAP" "nextSibling" "replaceNode" "spawnClient" "strContains"
    "stringToInt" "stringToXml" "strunescape" "textContent" "unsafe_cast"
    "InsertRows" "childNodes" "clientTime" "firstChild" "floatNotEq"
    "floatToInt" "floatToXml" "getTagName" "intToFloat" "javascript"
    "jsFillRect" "jsFillText" "objectType" "parentNode" "removeNode"
    "servePages" "serverTime" "spawnAngel" "spawnWait'" "attribute" "cloneNode"
    "dateToInt" "dumpTypes" "getCookie" "getTarget" "intToDate" "jsRestore"
    "setCookie" "spawnWait" "strescape" "swapNodes" "debugObj" "getPageX"
    "getPageY" "getValue" "haveMail" "intToXml" "isXDigit" "jsLineTo" "jsMoveTo"
    "jsStroke" "objectEq" "parseXml" "redirect" "stringEq" "ceiling" "explode"
    "floatEq" "getTime" "implode" "isAlnum" "isAlpha" "isBlank" "isDigit"
    "isLower" "isUpper" "jsScale" "makeXml" "negatef" "receive" "request"
    "spawnAt" "sysexit" "toLower" "toUpper" "AsList" "Concat" "accept" "cancel"
    "charAt" "gensym" "isNull" "jsFill" "jsSave" "length" "ltilde" "negate"
    "random" "stilde" "strlen" "strsub" "verify" "close" "crypt" "debug" "error"
    "event" "floor" "intEq" "jsArc" "newAP" "print" "sleep" "spawn" "there"
    "tilde" "Cons" "Send" "drop" "dump" "exit" "here" "link" "recv" "self"
    "send" "show" "sqrt" "take" "Nil" "chr" "cos" "log" "max" "min" "mod" "new"
    "not" "ord" "sin" "tan" "hd" "tl"))

;; Syntax table for links-mode
(defvar links-mode-syntax-table nil "Syntax table for `links-mode'.")
(setq links-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; Links comments run from # to EOL
        (modify-syntax-entry ?#  "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)
        synTable))

;; create the list for font-lock.
(setq links-font-lock-keywords
      (let* (
            ;; Links keywords
            (x-keywords links-keywords       )
            (x-types    links-primitive-types)
            (x-builtins links-builtins       )

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp    (regexp-opt x-types    'words))
            (x-builtins-regexp (regexp-opt x-builtins 'words))
            )

        `(
          ;; assign fonts to various categories of words
          (,x-types-regexp    . font-lock-type-face         )
          (,x-builtins-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face      )
          )))

;;;###autoload
(define-derived-mode links-mode prog-mode "links mode"
  "Major mode for editing Links source files"

  ;; syntax highlighting
  (setq font-lock-defaults '(links-font-lock-keywords))

  ;; syntax table
  (set-syntax-table links-mode-syntax-table)

  ;; comment delimiters for M-;
  (setq-local comment-start "# ")
  (setq-local comment-end   ""  )
  )

;; Automatically use links-mode for .links files.
;;;###autoload
(push '("\\.links$" . links-mode) auto-mode-alist)

;; add the mode to the `features' list
(provide 'links-mode)

;;; links-mode.el ends here
