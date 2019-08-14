#lang typed/racket/base

(require bitsyntax
         racket/list
         racket/match
         "../types.rkt")

(module+ test
  (require typed/rackunit
           #;syntax/macro-testing
           ))

(define-syntax nbt/end
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (x) (kf #t))
       ([(= NBT-TAG-END :: unsigned integer bytes 1) (rest :: binary)]
        (ks (nbt:end) rest))
       (else
        (kf)))]
    [(_ #f v)
     (begin
       (ann v nbt:end)
       (ann (bytes NBT-TAG-END) BitString))]))

(module+ test
  (test-case "nbt/end"
    (check-equal? (bit-string [(nbt:end) :: (nbt/end)])
                  #"\x0")
    (check-equal? (bit-string-case #"\x0"
                    ([(v :: (nbt/end))]
                     v))
                  (nbt:end))
    (check-equal? (bit-string-case #"\x1"
                    ([(v :: (nbt/end))] v)
                    (else 'invalid))
                  'invalid)))

#|
TODO: Write name bit-syntax macro.
TODO: Write tag bit-syntax macro.
TODO: Write NBT element macro.
|#

#;
(define-syntax template
  (syntax-rules ()
    [(_ #t input ks kf)
     (void)]
    [(_ #f v)
     (void)]))

(define-syntax entry-name
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (x) (kf #t))
       ([(len :: integer unsigned big-endian bytes 2)
         (bstr :: binary bytes len)
         (rest :: binary)]
        (let ([str (bytes->string/utf-8 (bit-string->bytes bstr))])
          (if str
              (ks str rest)
              (kf))))
       (else
        (kf)))]
    [(_ #f v)
     (bit-string [(string-length v) :: integer big-endian bytes 2]
                 [(string->bytes/utf-8 v) :: binary])]))

(module+ test
  (test-case "entry-name"
    (check-equal? (bit-string ["" :: (entry-name)])
                  #"\x0\x0")
    (check-equal? (bit-string->bytes (bit-string ["abcde" :: (entry-name)]))
                  #"\x0\x05abcde")
    (check-equal? (bit-string-case #"\x0\x0"
                    ([(s :: (entry-name))] s))
                  "")
    (check-equal? (bit-string-case #"\x0\x05abcde"
                    ([(s :: (entry-name))] s))
                  "abcde")))

(define-syntax nbt/byte
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (x) (kf #t))
       ([(= NBT-TAG-BYTE :: unsigned integer bytes 1)
         (name :: (entry-name))
         (val :: signed integer bytes 1)
         (rest :: binary)]
        (ks (nbt:byte name val) rest))
       (else (kf)))]
    [(_ #f v)
     (match-let ([(struct nbt:byte (name val)) v])
       (bit-string [NBT-TAG-BYTE :: integer bytes 1]
                   [name :: (entry-name)]
                   [val :: integer bytes 1]))]))

(module+ test
  (test-case "nbt/byte"
    (check-equal? (bit-string->bytes (bit-string [(nbt:byte "by" 14) :: (nbt/byte)]))
                  #"\1\0\2by\16")
    (check-equal? (bit-string-case #"\1\0\2by\16"
                    ([(by :: (nbt/byte))] by))
                  (nbt:byte "by" 14))))


(define-syntax nbt/short
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (x) (kf #t))
       ([(= NBT-TAG-SHORT :: unsigned integer bytes 1)
         (name :: (entry-name))
         (val :: signed integer bytes 2 big-endian)
         (rest :: binary)]
        (ks (nbt:short name val) rest))
       (else (kf)))]
    [(_ #f v)
     (match-let ([(struct nbt:short (name val)) v])
       (bit-string [NBT-TAG-SHORT :: integer bytes 1]
                   [name :: (entry-name)]
                   [val :: integer bytes 2 big-endian]))]))

(module+ test
  (test-case "nbt/short"
    (check-equal? (bit-string->bytes (bit-string [(nbt:short "sh" 14) :: (nbt/short)]))
                  #"\2\0\2sh\0\16")
    (check-equal? (bit-string-case #"\2\0\2sh\0\16"
                    ([(sh :: (nbt/short))] sh))
                  (nbt:short "sh" 14))))

(define-syntax nbt/int
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (x) (kf #t))
       ([(= NBT-TAG-INT :: unsigned integer bytes 1)
         (name :: (entry-name))
         (val :: signed integer bytes 4 big-endian)
         (rest :: binary)]
        (ks (nbt:int name val) rest))
       (else (kf)))]
    [(_ #f v)
     (match-let ([(struct nbt:int (name val)) v])
       (bit-string [NBT-TAG-INT :: integer bytes 1]
                   [name :: (entry-name)]
                   [val :: integer bytes 4 big-endian]))]))

(module+ test
  (test-case "nbt/int"
    (check-equal? (bit-string->bytes (bit-string [(nbt:int "in" 14) :: (nbt/int)]))
                  #"\3\0\2in\0\0\0\16")
    (check-equal? (bit-string-case #"\3\0\2in\0\0\0\16"
                    ([(in :: (nbt/int))] in))
                  (nbt:int "in" 14))))

(define-syntax nbt/long
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (x) (kf #t))
       ([(= NBT-TAG-LONG :: unsigned integer bytes 1)
         (name :: (entry-name))
         (val :: signed integer bytes 8 big-endian)
         (rest :: binary)]
        (ks (nbt:long name val) rest))
       (else (kf)))]
    [(_ #f v)
     (match-let ([(struct nbt:long (name val)) v])
       (bit-string [NBT-TAG-LONG :: integer bytes 1]
                   [name :: (entry-name)]
                   [val :: integer bytes 8 big-endian]))]))

(module+ test
  (test-case "nbt/long"
    (check-equal? (bit-string->bytes (bit-string [(nbt:long "lo" 14) :: (nbt/long)]))
                  #"\4\0\2lo\0\0\0\0\0\0\0\16")
    (check-equal? (bit-string-case #"\4\0\2lo\0\0\0\0\0\0\0\16"
                    ([(lo :: (nbt/long))] lo))
                  (nbt:long "lo" 14))))

;; TODO: implement float

(define-syntax nbt/double
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (x) (kf #t))
       ([(= NBT-TAG-DOUBLE :: unsigned integer bytes 1)
         (name :: (entry-name))
         (val :: float)
         (rest :: binary)]
        (ks (nbt:double name val) rest))
       (else (kf)))]
    [(_ #f v)
     (match-let ([(struct nbt:double (name val)) v])
       (bit-string [NBT-TAG-DOUBLE :: integer bytes 1]
                   [name :: (entry-name)]
                   [val :: float]))]))

(module+ test
  (test-case "nbt/double"
    (check-equal? (bit-string->bytes (bit-string [(nbt:double "db" 14.0) :: (nbt/double)]))
                  #"\4\0\2db\0\0\0\0\0\0\0\16")
    (check-equal? (bit-string-case #"\4\0\2db\0\0\0\0\0\0\0\16"
                    ([(db :: (nbt/double))] db))
                  (nbt:double "db" 14.0))))


(define-syntax nbt/byte-array
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (x) (kf #t))
       ([(= NBT-TAG-BYTE-ARRAY  :: unsigned integer bytes 1)
         (name :: (entry-name))
         (cnt :: signed integer bytes 4)
         (bys :: binary bytes cnt)
         (rest :: binary)]
        (ks (nbt:byte-array name (bytes->list (bit-string->bytes bys))) rest))
       (else (kf)))]
    [(_ #f v)
     (match-let ([(struct nbt:byte-array (name bys)) v])
       (bit-string [NBT-TAG-BYTE-ARRAY :: integer bytes 1]
                   [name :: (entry-name)]
                   [(length bys) :: integer bytes 4]
                   [(list->bytes bys) :: binary]))]))

(module+ test
  (test-case "nbt/byte-array"
    (check-equal? (bit-string->bytes (bit-string [(nbt:byte-array "ba" '(1 4 11)) :: (nbt/byte-array)]))
                  #"\7\0\2ba\0\0\0\3\1\4\xb")
    (check-equal? (bit-string-case #"\7\0\2ba\0\0\0\3\1\4\xb"
                    ([(ba :: (nbt/byte-array))] ba))
                  (nbt:byte-array "ba" '(1 4 11)))))

(define-syntax nbt/string
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (x) (kf #t))
       ([(= NBT-TAG-STRING  :: unsigned integer bytes 1)
         (name :: (entry-name))
         (len :: signed integer bytes 2)
         (bys :: binary bytes len)
         (rest :: binary)]
        (ks (nbt:string name (bytes->string/utf-8 (bit-string->bytes bys))) rest))
       (else (kf)))]
    [(_ #f v)
     (match-let ([(struct nbt:string (name str)) v])
       (bit-string [NBT-TAG-STRING :: integer bytes 1]
                   [name :: (entry-name)]
                   [(string-length str) :: integer big-endian bytes 2]
                   [(string->bytes/utf-8 str) :: binary]))]))

(module+ test
  (test-case "nbt/string"
    (check-equal? (bit-string->bytes (bit-string [(nbt:string "str" "text") :: (nbt/string)]))
                  #"\x8\0\3str\0\4text")
    (check-equal? (bit-string-case #"\x8\0\3str\0\4text"
                    ([(str :: (nbt/string))] str))
                  (nbt:string "str" "text"))))

(define-syntax nbt/element
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (x) (kf #t))
       ([(by :: (nbt/byte)) (rest :: binary)]
        (ks (ann by NBTElement) rest))
       ([(sh :: (nbt/short)) (rest :: binary)]
        (ks (ann sh NBTElement) rest))
       ([(in :: (nbt/int)) (rest :: binary)]
        (ks (ann in NBTElement) rest))
       ([(lo :: (nbt/long)) (rest :: binary)]
        (ks (ann lo NBTElement) rest))
       ([(ba :: (nbt/byte-array)) (rest :: binary)]
        (ks (ann ba NBTElement) rest))
       ;; TODO add float and double
       ([(st :: (nbt/string)) (rest :: binary)]
        (ks (ann st NBTElement) rest))
       ;; TODO add list
       ([(co :: (nbt/compound)) (rest :: binary)]
        (ks (ann co NBTElement) rest))
       ;; TODO add int-array and long-array
       (else (kf)))]
    [(_ #f v)
     (begin
       (ann v NBTElement)
       (cond
         [(nbt:byte? v) (bit-string [v :: (nbt/byte)])]
         [(nbt:short? v) (bit-string [v :: (nbt/short)])]
         [(nbt:int? v) (bit-string [v :: (nbt/int)])]
         [(nbt:long? v) (bit-string [v :: (nbt/long)])]
         [(nbt:byte-array? v) (bit-string [v :: (nbt/byte-array)])]
         ;; TODO add float and double
         [(nbt:string? v) (bit-string [v :: (nbt/string)])]
         ;; TODO add list
         [(nbt:compound? v) (bit-string [v :: (nbt/compound)])]
         ;; TODO add int-array and long-array
         ))]))

(define-syntax nbt/list
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case
       #:on-short (λ (x) (kf #t))
       ([]))]
    [(_ #f v)
     (void)]))

(: try-to-parse-nbt-element (BitString (NBTElement BitString -> Any) (Boolean -> Any) -> Any))
(define (try-to-parse-nbt-element bs ks kf)
  (bit-string-case bs
    #:on-short (λ (x) (kf #t))
    ([(e :: (nbt/element)) (rest :: binary)]
     (ks e rest))
    (else (kf))))

(define-syntax nbt/compound
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (x) (kf #t))
       ([(= NBT-TAG-COMPOUND :: integer bytes 1)
         (name :: (entry-name))
         (rest :: binary)]
        (let loop ([acc : (Listof NBTElement) empty]
                   [rest rest])
          (printf "~a ~a\n" acc rest)
          (flush-output)
          (bit-string-case rest
            #:on-short (λ (x) (kf))
            #;
            ([(e :: (nbt/element)) (next-rest :: binary)]
             (loop (cons e acc) next-rest))
            ([(:: (nbt/end)) (next-rest :: binary)]
             (ks (nbt:compound name (reverse acc)) next-rest))
            (else (kf))))))]
    [(_ #f v)
     (void)]))

(module+ test
  (test-case "nbt/compound"
    (define tn (nbt:compound "hello world" (list (nbt:string "name" "Bananrama"))))
    (define tb #"\x0a\x00\x0b\x68\x65\x6c\x6c\x6f\x20\x77\x6f\x72\x6c\x64\x08\x00\x04\x6e\x61\x6d\x65\x00\x09\x42\x61\x6e\x61\x6e\x72\x61\x6d\x61\x00")
    (check-equal? (bit-string-case tb ([(nc :: (nbt/compound))] nc))
                  tn)))
