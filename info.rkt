#lang info
(define collection "minecraft-nbt")
(define deps '("base" "typed-racket-lib" "bitsyntax"))
(define build-deps '("scribble-lib" "racket-doc" "typed-racket-doc" "rackunit-typed"))
(define scribblings '(("scribblings/minecraft-nbt.scrbl" ())))
(define pkg-desc "Minecraft NBT parser and writer")
(define version "0.0")
(define pkg-authors '(winny))
