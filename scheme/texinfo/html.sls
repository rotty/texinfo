#!r6rs

(library (texinfo html)
  (export stexi->shtml stexi-ref-resolvers)
  (import (except (rnrs base)
                  error string-copy string->list string-for-each
                  map for-each)
          (rnrs io simple)
          (spells misc)
          (srfi :1 lists)
          (srfi :8 receive)
          (srfi :13 strings)
          (spells alist)
          (spells ascii)
          (spells misc)
          (spells format)
          (srfi :39 parameters)
          (only (spells error) make-error-signaller)
          (spells include)
          (wak ssax tree-trans)
          (texinfo)
          (texinfo warn))

  (define error (make-error-signaller "texinfo html"))
  
  (include-file ((texinfo private) html)))
