#lang racket/base

(require racket/pretty)

(pretty-print 
  '(tlist ((tlist ((regular-id lambda)) tlist ((ellipsis-template (pattern-id name))) tlist ((pattern-id body1)) ellipsis-template (pattern-id body2))) ellipsis-template (pattern-id val))
  
)
