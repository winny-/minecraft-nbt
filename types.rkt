#lang typed/racket/base

(module+ test
  (require typed/rackunit))

(require bitsyntax)

(provide NBT
         NBTElement
         ListofNBTElement
         nbt:end
         nbt:byte
         nbt:short
         nbt:int
         nbt:long
         nbt:float
         nbt:double
         nbt:byte-array
         nbt:string
         nbt:list
         nbt:compound
         nbt:int-array
         nbt:long-array
         NBT-TAG-END
         NBT-TAG-BYTE
         NBT-TAG-SHORT
         NBT-TAG-INT
         NBT-TAG-LONG
         NBT-TAG-FLOAT
         NBT-TAG-DOUBLE
         NBT-TAG-BYTE-ARRAY
         NBT-TAG-STRING
         NBT-TAG-LIST
         NBT-TAG-COMPOUND
         NBT-TAG-INT-ARRAY
         NBT-TAG-LONG-ARRAY)

(define-type NBT nbt)
(define-type NBTElement (U nbt:byte nbt:short nbt:int nbt:long
                           nbt:float nbt:double
                           nbt:byte-array
                           nbt:string
                           nbt:list nbt:compound
                           nbt:int-array nbt:long-array))
(define-type ListofNBTElement
  (U (Listof nbt:byte) (Listof nbt:short) (Listof nbt:int) (Listof nbt:long)
     (Listof nbt:float) (Listof nbt:double)
     (Listof nbt:byte-array)
     (Listof nbt:string)
     (Listof nbt:list) (Listof nbt:compound)
     (Listof nbt:int-array) (Listof nbt:long-array)))

(struct nbt ()
  #:transparent)
(struct nbt:end nbt ()
  #:transparent)
(struct nbt:element nbt ()
  #:transparent)
(struct nbt:byte nbt ([name : String]
                      [value : Integer])
  #:transparent)
(struct nbt:short nbt ([name : String]
                       [value : Integer])
  #:transparent)
(struct nbt:int nbt ([name : String]
                     [value : Integer])
  #:transparent)
(struct nbt:long nbt ([name : String]
                      [value : Integer])
  #:transparent)
(struct nbt:float nbt ([name : String]
                       [value : Float])
  #:transparent)
(struct nbt:double nbt ([name : String]
                        [value : Float])
  #:transparent)
(struct nbt:byte-array nbt ([name : String]
                            [value : (Listof Integer)])
  #:transparent)
(struct nbt:string nbt ([name : String]
                        [value : String])
  #:transparent)
(struct nbt:list nbt ([name : String]
                      [value : ListofNBTElement])
  #:transparent)
(struct nbt:compound nbt ([name : String]
                          [value : (Listof NBTElement)])
  #:transparent)
(struct nbt:int-array nbt ([name : String]
                           [value : (Listof Integer)])
  #:transparent)
(struct nbt:long-array nbt ([name : String]
                            [value : (Listof Integer)])
  #:transparent)

(module+ test
  (void))

(define NBT-TAG-END 0)
(define NBT-TAG-BYTE 1)
(define NBT-TAG-SHORT 2)
(define NBT-TAG-INT 3)
(define NBT-TAG-LONG 4)
(define NBT-TAG-FLOAT 5)
(define NBT-TAG-DOUBLE 6)
(define NBT-TAG-BYTE-ARRAY 7)
(define NBT-TAG-STRING 8)
(define NBT-TAG-LIST 9)
(define NBT-TAG-COMPOUND 10)
(define NBT-TAG-INT-ARRAY 11)
(define NBT-TAG-LONG-ARRAY 12)
