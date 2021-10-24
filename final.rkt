;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; <Yu Li> <20871326>
;; CS 115 Fall 2020
;; Final Assessment
;; ***************************************************
;;

;; Ask Racket to load the features in this other file.  Do not delete
;; this line under any circumstances.
(require "drawinglib.rkt")

;; The following data definitions are included purely for your convenience.
;; Do not uncomment the (define-struct ...) lines -- the structure types
;; are defined for you in drawinglib.rkt, and you can use them here.

;; A Colour is a Str
;; Requires: the string is a legal colour name
  
;; (define-struct circle (centre radius colour))
;; A Circle is a (make-circle Posn Num Colour)
;; Requires: radius >= 0
  
;; (define-struct rect (centre width height colour))
;; A Rect is a (make-rect Posn Num Num Colour)
;; Requires: width, height >= 0

;; A Shape is one of:
;; * A Circle
;; * A Rect

;; (define-struct transform (dx dy scale angle))
;; A Transform is a (make-transform Num Num Num Num)
;; Requires: scale >= 0
  
;; (define-struct group (T children))
;; A Group is a (make-group Transform (listof Node))

;; A Node is one of:
;; * A Shape
;; * A Group

;; Problem 1 
(define circle1 (make-circle (make-posn 50 150) 40 "Medium Forest Green"))
(define circle2 (make-circle (make-posn 100 150) 75 "Orange Red"))
(define circle3 (make-circle (make-posn 150 150) 100 "Gold"))
(define rect1 (make-rect (make-posn 50 100) 20 20 "Royal Blue"))
(define rect2 (make-rect (make-posn 100 100) 30 40 "Steel Blue"))
(define rect3 (make-rect (make-posn 150 100) 60 70 "Silver"))
;;Shapes to be included in my-drawing

(define my-drawing (list circle1 circle2 circle3 rect1 rect2 rect3))
;; my-drawing constant needed for Problem 1


;; Problem 2
;; part (a)
;; (swatches loc) consumes loc, a (listof Colour), and produces a (listof Rect)
;; containing a row of squares coloured according to the colours in loc.
;; Each square should have dimensions (50X50). If loc is non-empty, the first
;; square should have its top-left corner at (0,0). If two squares are
;; consecutive, the right edge of one must exactly touch the left edge of the
;; next one.
;; swatches: (listof Colour) -> (listof Rect)
;; Examples:
(check-expect (swatches (list "Dark Olive Green" "Thistle" "Salmon"))
              (list (make-rect (make-posn 25 25) 50 50 "Dark Olive Green")
                    (make-rect (make-posn 75 25) 50 50 "Thistle")
                    (make-rect (make-posn 125 25) 50 50 "Salmon")))
(check-expect (swatches (list "Medium Forest Green" "Gold"))
              (list (make-rect (make-posn 25 25) 50 50 "Medium Forest Green")
                    (make-rect (make-posn 75 25) 50 50 "Gold")))

(define (swatches loc)
  (foldr (lambda (c l)
           (cons (make-rect
                  (make-posn (- (* 50 (length loc)) (* (length l) 50) 25) 25)
                  50 50 c) l))
         empty loc))

;; Tests:
(check-expect (swatches empty) empty)
(check-expect (swatches (list "Gold"))
              (list (make-rect (make-posn 25 25) 50 50 "Gold")))
(check-expect (swatches (list "Gold" "Gold" "Gold"))
              (list (make-rect (make-posn 25 25) 50 50 "Gold")
                    (make-rect (make-posn 75 25) 50 50 "Gold")
                    (make-rect (make-posn 125 25) 50 50 "Gold")))
(check-expect (swatches (list "Gold" "Gold" "Thistle"))
              (list (make-rect (make-posn 25 25) 50 50 "Gold")
                    (make-rect (make-posn 75 25) 50 50 "Gold")
                    (make-rect (make-posn 125 25) 50 50 "Thistle")))
(check-expect (swatches (list "Gold" "Gold" "Thistle" "Red"))
              (list (make-rect (make-posn 25 25) 50 50 "Gold")
                    (make-rect (make-posn 75 25) 50 50 "Gold")
                    (make-rect (make-posn 125 25) 50 50 "Thistle")
                    (make-rect (make-posn 175 25) 50 50 "Red")))

;; part(b)
;; (marquee columns rows) consumes two natural numbers, columns and rows and
;; produces a (listof Shape) consisting of a rectangular arrangement of yellow
;; circles on top of a black rectangle. The circles should all have radius 10
;; and touch each other. The arrangement should be rows circles tall and
;; columns circles across, and the rectangle should fit the circles exactly.
;; marquee: Nat Nat -> (listof Shape)
;; Requires: columns >= 2
;;           rows >= 2

(define (marquee columns rows)
  (local
    [;; (list-of-centers columns rows) consumes two natural numbers, columns
     ;; and rows to produce a (listof Posn) that corresponds to the centers
     ;; of the yellow circles needed in the marquee function.
     ;; list-of-centers: Nat Nat -> (listof Posn)
     (define (list-of-centers columns rows)
       (append (build-list columns (lambda (s) (make-posn (+ (* 20 s) 10) 10)))
               (build-list columns (lambda (s) (make-posn (+ (* 20 s) 10)
                                                          (- (* 20 rows) 10))))
               (build-list (- rows 2)
                           (lambda (s) (make-posn 10 (+ (* 20 s) 30))))
               (build-list (- rows 2)
                           (lambda (s) (make-posn (- (* 20 columns) 10)
                                                       (+ (* 20 s) 30))))))

     (define list-of-circles
       (map (lambda (p) (make-circle p 10 "Yellow")) 
            (list-of-centers columns rows)))
     ;; The circles needed in the marquee function.

     (define rectangle
       (make-rect (make-posn (* 10 columns) (* 10 rows))
                  (* 20 columns) (* 20 rows) "Black"))
     ;; The rectangle needed in the marquee function.
     ]

    (append (list rectangle) list-of-circles)))


;; Problem 3
;; part(a)
;; (translate los dx dy) consumes los, a (listof Shape), together with
;; two numbers dx and dy. The function produces a new (listof Shape) identical
;; to the original list, except that each shape's centre (x,y) has been changed
;; to (x+dx, y+dy).
;; translate: (listof Shape) Num Num -> (listof Shape)
;; Examples:
(check-expect (translate
               (list (make-rect (make-posn 25 25) 50 50 "Dark Olive Green")
                     (make-rect (make-posn 75 25) 50 50 "Thistle")
                     (make-rect (make-posn 125 25) 50 50 "Salmon")) 1 1)
              (list (make-rect (make-posn 26 26) 50 50 "Dark Olive Green")
                    (make-rect (make-posn 76 26) 50 50 "Thistle")
                    (make-rect (make-posn 126 26) 50 50 "Salmon")))
(check-expect (translate
               (list (make-rect (make-posn 25 25) 50 50 "Medium Forest Green")
                     (make-rect (make-posn 75 25) 50 50 "Gold")) 5 5)
               (list (make-rect (make-posn 30 30) 50 50 "Medium Forest Green")
                     (make-rect (make-posn 80 30) 50 50 "Gold")))

(define (translate los dx dy)
  (map (lambda (s) (cond
                     [(circle? s)
                      (make-circle
                       (make-posn (+ (posn-x (circle-centre s)) dx)
                                  (+ (posn-y (circle-centre s)) dy))
                       (circle-radius s) (circle-colour s))] 
                     [else
                      (make-rect
                       (make-posn (+ (posn-x (rect-centre s)) dx)
                                  (+ (posn-y (rect-centre s)) dy))
                       (rect-width s) (rect-height s) (rect-colour s))])) los))

;; Tests:
(check-expect (translate empty 6 7) empty)
(check-expect (translate (list (make-circle (make-posn 6 7) 10 "Gold")) 4 5)
              (list (make-circle (make-posn 10 12) 10 "Gold")))
(check-expect (translate
               (list (make-rect (make-posn 125 25) 50 50 "Salmon")) 10 20)
              (list (make-rect (make-posn 135 45) 50 50 "Salmon")))
(check-expect (translate (list
                          (make-circle (make-posn 6 7) 10 "Gold")
                          (make-rect (make-posn 125 25) 50 50 "Salmon")
                          (make-rect (make-posn 75 25) 50 50 "Thistle")) 10 2)
              (list
               (make-circle (make-posn 16 9) 10 "Gold")
               (make-rect (make-posn 135 27) 50 50 "Salmon")
               (make-rect (make-posn 85 27) 50 50 "Thistle")))
(check-expect (translate (list
                          (make-circle (make-posn 6 7) 10 "Gold")
                          (make-rect (make-posn 125 25) 50 50 "Salmon")
                          (make-rect (make-posn 75 25) 50 50 "Thistle"))
                         10.1 2.1)
              (list 
               (make-circle (make-posn 16.1 9.1) 10 "Gold")
               (make-rect (make-posn 135.1 27.1) 50 50 "Salmon")
               (make-rect (make-posn 85.1 27.1) 50 50 "Thistle")))
(check-within (translate (list
                          (make-circle (make-posn 6 7) 10 "Gold")
                          (make-rect (make-posn 125 25) 50 50 "Salmon")
                          (make-rect (make-posn 75 25) 50 50 "Thistle"))
                         pi pi)
              (list
               (make-circle (make-posn (+ 6 pi) (+ 7 pi)) 10 "Gold")
               (make-rect (make-posn (+ 125 pi) (+ 25 pi)) 50 50 "Salmon")
               (make-rect (make-posn (+ 75 pi) (+ 25 pi)) 50 50 "Thistle"))
              0.0001)
(check-expect (translate
               (list (make-circle (make-posn 6 7) 10 "Gold")
                     (make-circle (make-posn 7 15) 5 "Yellow")
                     (make-circle (make-posn 10 25) 5 "Red")) 4 5)
              (list (make-circle (make-posn 10 12) 10 "Gold")
                    (make-circle (make-posn 11 20) 5 "Yellow")
                    (make-circle (make-posn 14 30) 5 "Red")))
(check-expect (translate
               (list (make-rect (make-posn 25 25) 50 50 "Medium Forest Green")
                     (make-rect (make-posn 75 25) 50 50 "Gold")
                     (make-rect (make-posn 75 25) 50 50 "Gold")) 5 5)
               (list (make-rect (make-posn 30 30) 50 50 "Medium Forest Green")
                     (make-rect (make-posn 80 30) 50 50 "Gold")
                     (make-rect (make-posn 80 30) 50 50 "Gold")))
(check-expect (translate (list
                          (make-circle (make-posn 6 7) 10 "Gold")
                          (make-circle (make-posn 6 7) 10 "Gold")
                          (make-rect (make-posn 125 25) 50 50 "Salmon")
                          (make-rect (make-posn 75 25) 50 50 "Thistle")) 10 2)
              (list
               (make-circle (make-posn 16 9) 10 "Gold")
               (make-circle (make-posn 16 9) 10 "Gold")
               (make-rect (make-posn 135 27) 50 50 "Salmon")
               (make-rect (make-posn 85 27) 50 50 "Thistle")))
(check-expect (translate
               (list (make-circle (make-posn 6 7) 10 "Gold")
                     (make-circle (make-posn 6 7) 10 "Gold")
                     (make-circle (make-posn 6 7) 10 "Gold")) 4 5)
              (list (make-circle (make-posn 10 12) 10 "Gold")
                    (make-circle (make-posn 10 12) 10 "Gold")
                    (make-circle (make-posn 10 12) 10 "Gold")))
(check-expect (translate
               (list (make-rect (make-posn 25 25) 50 50 "Medium Forest Green")
                     (make-rect (make-posn 25 25) 50 50 "Medium Forest Green"))
               5 5)
               (list (make-rect (make-posn 30 30) 50 50 "Medium Forest Green")
                     (make-rect (make-posn 30 30) 50 50 "Medium Forest Green")))
(check-expect (translate (list
                          (make-circle (make-posn 6 7) 10 "Gold")
                          (make-rect (make-posn 125 25) 50 50 "Salmon")
                          (make-rect (make-posn 75 25) 50 50 "Thistle")) -10 -2)
              (list
               (make-circle (make-posn -4 5) 10 "Gold")
               (make-rect (make-posn 115 23) 50 50 "Salmon")
               (make-rect (make-posn 65 23) 50 50 "Thistle")))
(check-expect (translate (list
                          (make-circle (make-posn 6 7) 10 "Gold")
                          (make-rect (make-posn 125 25) 50 50 "Salmon")
                          (make-rect (make-posn 75 25) 50 50 "Thistle")) -10 2)
              (list
               (make-circle (make-posn -4 9) 10 "Gold")
               (make-rect (make-posn 115 27) 50 50 "Salmon")
               (make-rect (make-posn 65 27) 50 50 "Thistle")))
(check-expect (translate (list
                          (make-circle (make-posn 6 7) 10 "Gold")
                          (make-rect (make-posn 125 25) 50 50 "Salmon")
                          (make-rect (make-posn 75 25) 50 50 "Thistle")) 10 -2)
              (list
               (make-circle (make-posn 16 5) 10 "Gold")
               (make-rect (make-posn 135 23) 50 50 "Salmon")
               (make-rect (make-posn 85 23) 50 50 "Thistle")))
(check-expect (translate (list
                          (make-circle (make-posn 6 7) 10 "Gold")
                          (make-rect (make-posn 125 25) 50 50 "Salmon")
                          (make-rect (make-posn 75 25) 50 50 "Thistle")) 0 0)
              (list
               (make-circle (make-posn 6 7) 10 "Gold")
               (make-rect (make-posn 125 25) 50 50 "Salmon")
               (make-rect (make-posn 75 25) 50 50 "Thistle")))


;; part (b)
;; (hit-shapes los pos) consumes los, a (listof Shape), and a Posn, pos,
;; to produce a new (listof Shape), consisting of those elements of los that
;; contain pos in their interiors.
;; hit-shapes: (listof Shape) Posn -> (listof Shape)
;; Examples:
(check-expect (hit-shapes (list (make-circle (make-posn 0 0) 1 "Gold"))
                          (make-posn 1 1)) empty)
(check-expect (hit-shapes (list (make-circle (make-posn 0 0) 1 "Gold"))
                          (make-posn 0 0))
              (list (make-circle (make-posn 0 0) 1 "Gold")))

(define (hit-shapes los pos)
  (local
    [;; (inside-circle? pos circ) consumes a posn, pos, and a Circle, circ, to
     ;; determine whether pos is in the interior of circ.
     ;; inside-circle?: Posn Circle -> Bool
     (define (inside-circle? pos circ)
       (< (+ (sqr (- (posn-x pos) (posn-x (circle-centre circ))))
             (sqr (- (posn-y pos) (posn-y (circle-centre circ)))))
          (sqr (circle-radius circ))))

     ;; (inside-rect? pos rec) consumes a posn, pos, and a Rect, rec, to
     ;; determine whether pos is in the interior of rec.
     ;; inside-rect?: Posn Rect -> Bool
     (define (inside-rect? pos rec)
       (and
        (< (abs (- (posn-x pos) (posn-x (rect-centre rec))))
           (/ (rect-width rec) 2))
        (< (abs (- (posn-y pos) (posn-y (rect-centre rec))))
           (/ (rect-height rec) 2))))]
     
    (cond
    [(empty? los) empty]
    [(and (circle? (first los))
          (inside-circle? pos (first los)))
     (cons (first los) (hit-shapes (rest los) pos))]
    [(and (rect? (first los))
          (inside-rect? pos (first los)))
     (cons (first los) (hit-shapes (rest los) pos))]
    [else (hit-shapes (rest los) pos)])))

;; Tests:
(check-expect (hit-shapes (list (make-circle (make-posn 0 0) 1 "Gold"))
                          (make-posn 1 0)) empty)
(check-expect (hit-shapes
               (list (make-rect (make-posn 25 25) 50 50 "Dark Olive Green")
                     (make-rect (make-posn 75 25) 50 50 "Thistle")
                     (make-rect (make-posn 125 25) 50 50 "Salmon"))
               (make-posn 50 20)) empty)
(check-expect (hit-shapes
               (list (make-rect (make-posn 25 25) 50 50 "Dark Olive Green")
                     (make-rect (make-posn 75 25) 50 50 "Thistle")
                     (make-rect (make-posn 125 25) 50 50 "Salmon"))
               (make-posn 49 49))
              (list (make-rect (make-posn 25 25) 50 50 "Dark Olive Green")))
(check-expect (hit-shapes empty (make-posn 0 0)) empty)
(check-expect (hit-shapes
               (list (make-rect (make-posn 25 25) 50 50 "Dark Olive Green"))
               (make-posn 49 49))
              (list (make-rect (make-posn 25 25) 50 50 "Dark Olive Green")))
(check-expect (hit-shapes
               (list (make-rect (make-posn 25 25) 50 50 "Dark Olive Green"))
               (make-posn 51 49)) empty)
(check-expect (hit-shapes (list
                           (make-circle (make-posn 6 7) 10 "Gold")
                           (make-rect (make-posn 125 25) 50 50 "Salmon")
                           (make-rect (make-posn 75 25) 50 50 "Thistle"))
                          (make-posn 126 26))
              (list (make-rect (make-posn 125 25) 50 50 "Salmon")))
(check-expect (hit-shapes (list
                           (make-circle (make-posn 6 7) 10 "Gold")
                           (make-rect (make-posn 125 25) 50 50 "Salmon")
                           (make-rect (make-posn 75 25) 50 50 "Thistle"))
                          (make-posn 2000 26)) empty)
(check-expect (hit-shapes (list
                           (make-circle (make-posn 6 7) 10 "Gold")
                           (make-rect (make-posn 125 25) 50 50 "Salmon")
                           (make-rect (make-posn 125 25) 50 50 "Salmon")
                           (make-rect (make-posn 75 25) 50 50 "Thistle"))
                          (make-posn 126 26))
              (list (make-rect (make-posn 125 25) 50 50 "Salmon")
                    (make-rect (make-posn 125 25) 50 50 "Salmon")))
(check-expect (hit-shapes (list
                           (make-circle (make-posn 6 7) 10 "Gold")
                           (make-circle (make-posn 6 7) 10 "Gold")
                           (make-rect (make-posn 125 25) 50 50 "Salmon")
                           (make-rect (make-posn 75 25) 50 50 "Thistle"))
                          (make-posn 2000 26)) empty)
(check-expect (hit-shapes (list
                           (make-circle (make-posn 6 7) 10 "Gold")
                           (make-circle (make-posn 6 7) 10 "Gold")
                           (make-rect (make-posn 125 25) 50 50 "Salmon")
                           (make-rect (make-posn 75 25) 50 50 "Thistle"))
                          (make-posn 0 0))
              (list (make-circle (make-posn 6 7) 10 "Gold")
                    (make-circle (make-posn 6 7) 10 "Gold")))
(check-expect (hit-shapes
               (list (make-circle (make-posn 6 7) 10 "Gold")
                     (make-circle (make-posn 6 7) 10 "Gold")
                     (make-circle (make-posn 6 7) 10 "Gold"))
               (make-posn 0 0))
              (list (make-circle (make-posn 6 7) 10 "Gold")
                     (make-circle (make-posn 6 7) 10 "Gold")
                     (make-circle (make-posn 6 7) 10 "Gold")))
(check-expect (hit-shapes
               (list (make-circle (make-posn 6 7) 10 "Gold")
                     (make-circle (make-posn 6 7) 10 "Gold")
                     (make-circle (make-posn 6 7) 10 "Gold"))
               (make-posn 2000 0)) empty)
(check-expect (hit-shapes
               (list (make-rect (make-posn 25 25) 50 50 "Medium Forest Green")
                     (make-rect (make-posn 25 25) 50 50 "Medium Forest Green"))
               (make-posn 7 7))
              (list (make-rect (make-posn 25 25) 50 50 "Medium Forest Green")
                     (make-rect (make-posn 25 25) 50 50 "Medium Forest Green")))
(check-expect (hit-shapes
               (list (make-rect (make-posn 25 25) 50 50 "Medium Forest Green")
                     (make-rect (make-posn 25 25) 50 50 "Medium Forest Green"))
               (make-posn 700 7)) empty)
(check-expect (hit-shapes (list
                           (make-circle (make-posn 125 56) 10 "Gold")
                           (make-rect (make-posn 125 25) 50 50 "Salmon")
                           (make-rect (make-posn 124 25) 150 50 "Thistle"))
                          (make-posn 125 48))
              (list
               (make-circle (make-posn 125 56) 10 "Gold")
               (make-rect (make-posn 125 25) 50 50 "Salmon")
               (make-rect (make-posn 124 25) 150 50 "Thistle")))


;; Problem 4
;; (tree n) consumes a natural number n and produces a (listof Node)
;; representing a Level n Pythagoras Tree. 
;; tree: Nat -> (listof Node)

(define (tree n)
  (local
    [(define top-right-transform (make-transform 50 -100 (/ 1 (sqrt 2)) 45))
     ;the transform needed to transform (tree (sub1 n)) to the top right.

     (define top-left-transform (make-transform -50 -100 (/ 1 (sqrt 2)) -45))
     ;the transform needed to transform (tree (sub1 n)) to the top left.

     (define tree-zero (make-rect (make-posn 0 0) 100 100 "Black"))
     ;level 0 tree
     ]
    
    (cond
    [(zero? n) (list tree-zero)]
    [else
     (list    
      (make-group top-right-transform (tree (sub1 n)))
      (make-group top-left-transform (tree (sub1 n)))
      tree-zero)])))


