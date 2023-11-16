(lang dune 2.7)
;; dune language version (NOTE this comment applies to the above line; it is intentionally placed here!)
(using menhir 2.0)                 ;; enable use of menhir
(name links)                       ;; package name
(implicit_transitive_deps false)   ;; hides transitive dependencies in libraries/executables/tests
(generate_opam_files true)         ;; generate .opam package files using the descriptions defined in this file.


;;
;; Project metainformation
;;

(source
 (github links-lang/links))
(homepage
  https://links-lang.org)
(authors
  "The Links Team <links-dev@inf.ed.ac.uk>")
(maintainers
  "Daniel Hillerström <daniel.hillerstrom@ed.ac.uk>")
(license
  GPL-3.0-only)
(documentation
  "https://links-lang.org/quick-help.html")

;;
;; The Links Programmming System
;;

(package
 (name links)
 (synopsis "The Links Programming Language")
 (description "Links is a functional programming language designed to make web programming easier.")
 (depends (ocaml (>= : 4.14.0))
          (dune-configurator (>= : 3.8))
           ppx_deriving
          (ppx_deriving_yojson (>= 3.3))
           base64
           linenoise
           ANSITerminal
          (lwt (>= 5.0.0))
           cohttp
           cohttp-lwt
           cohttp-lwt-unix
           conduit-lwt-unix
           uri
           tls
           websocket
           websocket-lwt-unix
           safepass
           result
           ocamlfind
          (menhir (>= 20210419))
          (ppx_sexp_conv (>= v0.16.0))
          (calendar (>= 2.0.4))
          (rdf_lwt (>= 0.13.0)))
 (tags
  ("web programming" "tierless" "multi-tier" "effect handlers" "effect typing" "session types"
   "concurrency" "webpage" "extensible data types" "language-integrated queries")))

;;
;; Links PostgreSQL driver
;;

(package
 (name links-postgresql)
 (synopsis "Postgresql database driver for the Links Programming Language")
 (description "Postgresql database driver for the Links Programming Language")
 (depends (links (= version))
          postgresql)
 (tags
   ("postgresql" "database")))

;;
;; Links SQLite3 driver
;;

(package
 (name links-sqlite3)
 (synopsis "SQLite database driver for the Links Programming Language")
 (description "SQLite database driver for the Links Programming Language")
 (depends (links (= version))
          sqlite3)
 (tags
   ("sqlite" "database")))

;;
;; Links MySQL driver
;;

(package
 (name links-mysql)
 (synopsis "MySQL database driver for the Links Programming Language")
 (description "MySQL database driver for the Links Programming Language")
 (depends (links (= version))
          conf-mysql
          mysql8)
 (tags
   ("mysql" "database")))



; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

