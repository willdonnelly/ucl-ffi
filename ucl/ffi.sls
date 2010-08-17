#!r6rs

;; UCL-FFI
;;  Foreign Function Interface Yay!
;;
;; TYPES
;;   The primitive types exposed by UCL-FFI are all numbers,
;;     with the exception of the pointer type.
;;   The usual menagerie of numeric types are supported, with
;;     the following sizes:
;;
;;     * float   - 4 bytes
;;     * double  - 8 bytes
;;     * char    - 1 byte
;;     * short   - 2 bytes
;;     * int     - 4 bytes
;;     * long    - 4 bytes or 8 bytes
;;     * pointer - 4 bytes or 8 bytes
;;     * string  - see pointer
;;
;;   The long/pointer size distinction occurs on 64-bit systems,
;;     as expected, and all other sizes are constant.
;;   Just in case, though, a SIZEOF procedure is provided which
;;     will return the size, in bytes, of a data type.
;;
;;   The type symbols are the same as the above list of types,
;;     except that the char, short, int, and long types are
;;     all prefixed with a 'u' or 's' character to indicated
;;     signedness.
;;
;;   The string type deserves special mention. It is only valid
;;     for function types. When used as an argument, you should
;;     pass in a Scheme string, and memory will be allocated for
;;     it before entering the function, and freed after it returns.
;;     When used as a return type, a memory region will be read
;;     into a Scheme string, and then the original region will
;;     be freed. If this is not a desirable behavior, the functions
;;     STRING-CLONE and STRING-READ can be used directly, and
;;     the pointer type should be used instead.
;;
;; FUNCTIONS
;;   (LOAD-LIBRARY name ...)
;;     Takes a list of possible library names, and attempts
;;       to load them in order until one is successful. If
;;       it succeeds, it returns a library reference, and
;;       otherwise it returns #f.
;;
;;   (GET-FUNCTION lib name args ret)
;;     Extracts a symbol from the library and interprets it
;;       as a function with the given argument and return
;;       types. Returns a scheme procedure which can be
;;       called as usual.
;;
;;   (PTR-FUNCTION ptr args ret)
;;     Convert the given pointer into a function which can
;;       be called from Scheme code. Basically the same as
;;       GET-FUNCTION, but it doesn't look up the function
;;       in a library.
;;
;;   (MAKE-CALLBACK args ret fn)
;;     Wraps the given scheme procedure in a trampoline and
;;       sets up the necessary type conversions so that it
;;       can be passed to C code. Returns a pointer to the
;;       newly allocated function.
;;
;; MEMORY
;;   (MALLOC bytes)
;;     Allocates the requested number of bytes, and returns
;;       a pointer. The memory will not be zeroed.
;;
;;   (FREE pointer)
;;     Exactly what it says on the tin.
;;
;;   NULL
;;     The null pointer (it's different across implementations).
;;
;;   NULL-PTR?
;;     Is a given pointer null?
;;
;;   (POINTER-SET! pointer offset type value)
;;     Writes a value of the given type at a certain offset into
;;       a memory region. The offset is always given in bytes,
;;       regardless of the type of value being written.
;;
;;   (POINTER-GET pointer offset type)
;;     Reads back a value from a memory region. More-or-less
;;       guaranteed to read back in data the same as it was
;;       written out, but you can't always rely on a newly
;;       read pointer object being EQUAL? to the old one.
;;
;;   (POINTER->INTEGER pointer)
;;     Converts a pointer to an integer.
;;   (INTEGER->POINTER integer)
;;     Look up one entry. Guess what this one does.
;;   (POINTER? pointer)
;;     You're smart, figure it out.
;;
;;   (STRING-CLONE str)
;;     Allocate a new memory region and fill it with a null-terminated
;;       sequence of characters representing the given string. The
;;       characters are converted using the native transcoder.
;;
;;   (STRING-READ ptr)
;;     The inverse of STRING-CLONE. Takes a memory region and reads the
;;       contents as a Scheme string. Does not free the pointer, so you'll
;;       have to do that yourself.

(library (ucl ffi)
  (export load-library get-function ptr-function make-callback
          malloc free null null-ptr? pointer-set! pointer-get
          pointer->integer integer->pointer pointer?
          string-clone string-read sizeof)
  (import (rnrs) (ucl ffi types) (ucl ffi memory) (ucl ffi functions))

  ;; list the integers in the half-open range [x, y)
  (define (range x y) (do ((i (- y 1) (- i 1)) (a '() (cons i a))) ((< i x) a)))

  (define (string-clone str)
    (let* ((vec (string->bytevector str (native-transcoder)))
           (chs (bytevector->u8-list vec))
           (len (length chs))
           (mem (malloc (+ len 1))))
      (define (set-char ix ch)
        (pointer-set! mem ix 'uchar ch))
      (for-each set-char (range 0 len) chs)
      (pointer-set! mem len 'uchar 0)
      mem))

  (define (string-read ptr)
    (define (get-char off) (pointer-get ptr off 'uchar))
    (define (strlen off) (if (equal? (get-char off) 0) off (strlen (+ 1 off))))
    (let* ((len (strlen 0))
           (ixs (range 0 len))
           (chs (map get-char ixs))
           (vec (u8-list->bytevector chs)))
      (bytevector->string vec (native-transcoder))))


  ;; CLEANUP - Do cleanup after evaluating an expression
  (define-syntax cleanup (syntax-rules () ((_ c e) (let ((r e)) c r))))

  ;; FUNCTION-CONVERT - Do some type conversion around the given function
  ;;                    essentially just makes strings into pointers and
  ;;                    vice versa, and then handles freeing them later
  (define (function-convert fn args ret)
    (define (arg-convert t v) (if (equal? t 'string) (string-clone v) v))
    (define (arg-unconvert t v) (if (equal? t 'string) (free v) v))
    (define (ret-convert t v)
      (if (equal? t 'string) (cleanup (free v) (string-read v)) v))
    (lambda vals
      (define cvals (map arg-convert args vals))
      (ret-convert ret
        (cleanup (for-each arg-unconvert args cvals)
          (apply fn cvals)))))

  (define (get-function lib name args ret)
    (function-convert (raw-get-function lib name args ret) args ret))
  (define (ptr-function ptr args ret)
    (function-convert (raw-ptr-function ptr args ret) args ret))
)
