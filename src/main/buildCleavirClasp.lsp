(format t "Building cleavir clasp full version~%")
(format t "Loading ASDF...~%")
(require :asdf)
(format t "Loading :clasp-cleavir system~%")
(require :clasp-cleavir)
(load "sys:kernel;cleavir;cmpclasp.lisp")
(clasp-cleavir:compile-full-cleavir)
(core:quit)
