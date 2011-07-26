#!r6rs

(library (wak texinfo html)
  (export stexi->shtml stexi-ref-resolvers stexi-anchor-renamers)
  (import (except (rnrs base)
                  error string-copy string->list string-for-each
                  map for-each)
          (rnrs unicode)
          (rnrs io simple)
          (srfi :1 lists)
          (srfi :8 receive)
          (only (srfi :13 strings)
                string-map
                string-contains)
          (srfi :39 parameters)
          (spells alist)
          (spells misc)
          (spells format)
          (only (spells error) make-error-signaller)
          (spells include)
          (wak ssax tree-trans)
          (wak texinfo)
          (wak texinfo warn))

  (define error (make-error-signaller "texinfo html"))
  
  (include-file ((wak texinfo private) html)))
