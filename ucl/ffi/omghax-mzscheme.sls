#lang scheme/base

;; Seriously, this is a ridiculously ugly hack
;; As far as I can tell, I can't import the '#%foreign
;;   module using PLTs R6RS import syntax
;; So I created this little file to let me grab the
;;   functions I need
(require '#%foreign)
(provide ffi-call ffi-callback)
