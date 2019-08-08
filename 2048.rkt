#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(provide (all-defined-out))
(define *side* 4)                        ; Side-length of the grid
(define (f2048)                        
(define *tile-that-wins* 2048)           ; You win when you get a tile = this number
(define *magnification* 2)               ; Scales the game board


;Numbers can be displayed with substiture text. Just edit this table...
(define *text*
  '((0 "")
    (2 "2")))

;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
; Color scheme
; From https://github.com/gabrielecirulli/2048/blob/master/style/main.css


(define *grid-color* (color #xbb #xad #xa0))

(define *default-tile-bg-color* (color #x3c #x3a #x32))
(define *default-tile-fg-color* 'white)


(define *tile-bg-colors*
  (map (lambda (x)
         (match-define (list n r g b) x)
         (list n (color r g b)))
       '((0 #xcc #xc0 #xb3)
         (2 #xee #xe4 #xda)
         (4 #xed #xe0 #xc8)
         (8 #xf2 #xb1 #x79)
         (16 #xf5 #x95 #x63)
         (32 #xf6 #x7c #x5f)
         (64 #xf6 #x5e #x3b)
         (128 #xed #xcf #x72)
         (256 #xed #xcc #x61)
         (512 #xed #xc8 #x50)
         (1024 #xed #xc5 #x3f)
         (2048 #xed #xc2 #x2e))))

(define *tile-fg-colors*
  '((0 dimgray)
    (2 dimgray)
    (4 dimgray)
    (8 white)
    (16 white)
    (32 white)
    (64 white)
    (128 white)
    (256 white)
    (512 white)
    (1024 white)
    (2048 white)))

(define *text-size* 30)
(define *max-text-width* 40)
(define *tile-side* 50)
(define *grid-spacing* 5)
(define *grid-side* (+ (* *side* *tile-side*) (* (add1 *side*) *grid-spacing*)))

(define (set-side! n)
  (set! *side* n))


;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
;Rows are represented as lists with zeroes representing empty spots 




;Slide items towards the head of the list, doubling adjacent pairs when no item is a 0.
;Eg :- '(2 1 4 5)---->'(2 1 4 5)
;Eg :- '(2 2 2 3 3 3 4 4 4)----->'(4 2 6 3 8 4)
(define (combine lst)
  (if (<= (length lst) 1) lst
  (if (= (first lst) (second lst)) (cons (* 2 (first lst)) (combine (drop lst 2)))
      (cons (car lst) (combine (cdr lst))))))



;slides all the numbers(not zeroes) in a list through zeroes towards left followed by combining them as above
;and padding the obtained list with zeroes such that the length of the list is same as before
;Eg :- '(2 0 0 3)---->'(2 3 0 0)
;Eg :- '(2 2 2 0 3 3)---->'(4 2 6 0 0 0)
(define (slide-left lst)
  (let* ([l (filter (lambda (x) (not (= x 0))) lst)]
         [p (combine l)])
    (append p (build-list (- (length lst) (length p)) (lambda (x) 0)))))



;same as the above function but sliding and combining occurs towards right and the padding is done towards left
;Eg :- '(2 4 3)---->'(2 4 3)
;Eg :- '(2 0 2 2 3 3 4 4)---->'(0 0 0 0 2 4 6 8)
(define (slide-right lst)
  (reverse (slide-left (reverse lst))))


;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
;We use a special-representation for transitions in a row
;Moves take the form '(number initial-position final-position)




;Convert a row into the special-representaiton without any sliding
;Eg :- '(2 4 6 8)---->'((2 0 0) (4 1 1) (6 2 2) (8 3 3))
;Eg :- '(0 2 2 4)---->'((2 1 1) (2 2 2) (4 3 3)) 
(define (moves-1d-none l)
  (let ([j -1])
    (filter list? (for/list ([i l])
                    (begin (set! j (+ j 1)) (if (not (= i 0)) (list i j j) 0))))))



;represents the nonzero numbers in a list as a list of lists such as '(number initial-pos final-pos).
;the initial-pos and final-pos are accordingly the positions of the number before squeezing the list 
;and after squeezing the list towards left with zero-indexing.
;Eg :- '(2 2)---->'((2 0 0) (2 1 0))
;Eg :- '(2 2 3 3 0 4)---->'((2 0 0) (2 1 0) (3 2 1) (3 3 1) (4 5 2))
(define (moves-row-left row)
  (define (helper row last i j)
  (if (null? row) 
      null
      (let ([head (car row)])
        (cond [(zero? head) (helper (cdr row) last (add1 i) j)]
              [(equal? last head) 
               (cons (list head i j)
                     (helper (cdr row) #f (add1 i) j))]
              [else (cons (list head i (add1 j))
                          (helper (cdr row) head (add1 i) (add1 j)))]))))
  (helper row #f 0 -1))



;used to flip the positions of nonzero numbers in our representation of list of lists '(number initial-pos final-pos)
;such that the final list contains lists of '(number (- *side* initial-pos 1) (- *side* final-pos 1))
;Eg :- (reverse-moves '((2 0 0) (2 1 0)) 4)---->'((2 3 3) (2 2 3))
;Eg :- (reverse-moves '((2 0 0) (2 2 0) (3 3 1)) 4)---->'((2 3 3) (2 1 3) (3 0 2))
(define (reverse-moves moves n)
  (define (flip i) (- n i 1))
  (map (lambda (m)
         (match-define (list a b c) m)
         (list a (flip b) (flip c)))
       moves))



;same as moves-row-left except that squeezing is done towards right
;Eg :- '(2 2)---->'((2 0 1) (2 1 1))
;Eg :- '(2 0 2 4)---->'((2 0 2) (2 2 2) (4 3 3))
(define (moves-row-right row)
  (let* ([n *side*])
  (define (helper row n)
    (reverse-moves (moves-row-left (reverse row)) n))
    (helper row n)))


;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
;Lift the special representation for transitions up to two dimensions...



;coverts special-representation to list containing lists of (number (row-number init-coloumn-number) (row-number final-coloumn-number))
;Eg :- (add-row-coord 1 '((2 0 0) (2 1 0)))---->'((2 (1 0) (1 0)) (2 (1 1) (1 0)))
;Eg :- (add-row-coord 1 '((3 0 0) (2 1 1) (4 3 2)))---->'((3 (1 0) (1 0)) (2 (1 1) (1 1)) (4 (1 3) (1 2)))
(define (add-row-coord i rows)
  (for/list ([r rows])
    (match-define (list a b c) r)
    (list a (list i b) (list i c))))



;converts list containing containing lists of (number (init-row-number init-coloumn-number) (final-row-number final-coloumn-number))
;to list containing lists of (number (init-coloumn-number init-row-number) (final-coloumn-number final-row-number))
;Eg :- '((2 (1 0) (1 0)) (2 (1 1) (1 0)))---->'((2 (0 1) (0 1)) (2 (1 1) (0 1)))
;Eg :- '((2 (0 1) (2 1)) (3 (1 1) (0 1) (4 (2 1) (3 2)))---->'((2 (1 0) (1 2)) (3 (1 1) (1 0) (4 (1 2) (2 3)))
(define (transpose-moves moves)
  (for/list ([m moves])
    (match-define (list v (list a b) (list c d)) m)
    (list v (list b a) (list d c))))



;changes list containing list of rows to list of columns
;Eg :- '((1 2 3) (4 5 6))---->'((1 4) (2 5) (3 6))
;Eg :- '((1 1 1) (2 2 2) (3 3 3))---->'((1 2 3) (1 2 3) (1 2 3))
(define (transpose grid)
  (apply map list grid))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;the following four functions are the final configurations of the grid accordingly when the user applies the arrow keys.

(define (left grid)
  (map slide-left grid))

(define (right grid)
  (map slide-right grid))

(define (up grid)
  ((compose transpose left transpose) grid))

(define (down grid)
  ((compose transpose right transpose) grid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;We'll use these operations to animate the sliding of the tiles.


;Higher-order-function which processes a grid
;grid is basically a list of lists each representing a row
(define (moves-grid-action grid action)
  (let ([n (length (first grid))])
    (apply append
           (for/list ([row grid]
                      [i (in-range n)])
             (add-row-coord i (action row))))))

;returns the special representation by moving the grid left
(define (move-grid-left grid)
  (moves-grid-action grid moves-row-left))


;returns the special representation by moving the grid right
(define (move-grid-right grid)
  (moves-grid-action grid moves-row-right))

;returns the special representation by moving the grid up
(define (move-grid-up grid)
  ((compose transpose-moves move-grid-left transpose) grid))

;returns the special representation by moving the grid down
(define (move-grid-down grid)
  ((compose transpose-moves move-grid-right transpose) grid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;

; The Game


; an image-procedure is a procedure of no arguments that produces an image

; a world contains:
; state is a list of rows, each row being a list
; winning-tile is a number or #f
; frames is a (list-of image-procedure)


(struct world (state winning-tile frames) #:transparent)


;counts the number of zeroes in the list
;Eg :- (count-zeros '(1 2 0 0 4 7))---->2
(define (count-zeros l)
  (length (filter (lambda (x) (= x 0)) l)))



;the absolute index of the nth zero in lst
;Eg :- (index-of-nth-zero '(0 2 0 4) 2)----> 2
(define (indexof-nth-zero l n)
  (let ([lg (length l)])
 (define (helper l n count)
   (cond [(= n count) (- (- lg (length l)) 1)]
         [(= (car l) 0) (helper (cdr l) n (+ count 1))]
         [#t (helper (cdr l) n count)]))
    (helper l n 0)))


; Given an arrow key return the operations to change the state and produce the sliding animation.
(define (key->op a-key)
  (cond [(key=? a-key "left") (list left move-grid-left)]
        [(key=? a-key "right") (list right move-grid-right)]
        [(key=? a-key "up") (list up move-grid-up)]
        [(key=? a-key "down") (list down move-grid-down)]
        [else (list #f #f)]))

; memoizing using hash-tables
(define memo-hash (make-hash))
; to avoid regeneration of tiles which were already generated once
(define (make-tile n)
  (when (not (hash-has-key? memo-hash n))
    (hash-set! memo-hash n (overlay (tile-text n) (plain-tile n))))
  (hash-ref memo-hash n))

;Chop a list into a list of sub-lists of length n. Used to move from
;a flat representation of the grid into a list of rows.
(define (chop lst n)
    (if (= n (length lst)) (list lst)
        (cons (take lst n) (chop (drop lst n) n))))


;The game finishes when no matter which way you slide, the board doesn't change.
(define (finished? state)
  (let ([n *side*])
    (define (helper state n)
      (let ([grid (chop state n)])
        (for/and ([op (list left right up down)])
          (equal? grid (op grid)))))
    (helper state n)))


;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
;Graphics

;Look-up the (i,j)th element in the flat representation.
(define (square/ij state i j)
  (list-ref state (+ (* *side* i) j)))


;(interpolate 0.0 a b) -> a
;(interpolate 1.0 a b) -> b
(define (interpolate k a b)
  (+ (* (- 1 k) a)
     (* k b)))

(define (lookup key lst default)
  (let ([value (assoc key lst)])
    (if value (second value) default)))



;Make a tile without a number on it in the appropriate color.
(define (plain-tile n)
  (square *tile-side* 
          'solid 
          (lookup n *tile-bg-colors* *default-tile-bg-color*)))


; Place a tile on an image of the grid at (i,j)
(define (place-tile/ij tile i j grid-image)
  (define (pos k)
    (+ (* (add1 k) *grid-spacing*)
       (* k *tile-side*)))
  (underlay/xy grid-image (pos j) (pos i) tile))


; Make text for a tile
(define (tile-text n)
  (let* ([t (text (lookup n *text* (number->string n))
                  *text-size*
                  (lookup n *tile-fg-colors* *default-tile-fg-color*))]
         [side (max (image-width t) (image-height t))])
    (scale (if (> side *max-text-width*) (/ *max-text-width* side) 1) t)))


(define *last-state* null) ;cache the previous grid to avoid
(define *last-grid* null)  ;sensless regeneration

(define (state->image state)
  (unless (equal? state *last-state*)
    (set! *last-grid*
          (for*/fold ([im (square *grid-side* 'solid *grid-color*)])
            ([i (in-range *side*)]
             [j (in-range *side*)])
            (place-tile/ij (make-tile (square/ij state i j))
                           i j
                           im)))
    (set! *last-state* state))
  *last-grid*)

(define *empty-grid-image*
  (state->image (make-list (sqr *side*) 0)))


;Convert the special representation of moves into a single frame in an animation at time k,
;where k is between 0.0 (start state) and 1.0 (final state).
(define (moves->frame moves k)
  (for*/fold ([grid *empty-grid-image*])
    ([m moves])
    (match-define (list value (list i1 j1) (list i2 j2)) m)
    (place-tile/ij (make-tile value)
                   (interpolate k i1 i2) (interpolate k j1 j2)
                   grid)))


; Animation of simultaneously moving tiles
(define (animate-moving-tiles state op)
  (let ([grid (chop state *side*)])
    (build-list 9 (lambda (i) 
                    (lambda () 
                      (moves->frame (op grid) 
                                    (* 0.1 (add1 i))))))))


; Animation of a tile appearing in a previously blank square
(define (animate-appearing-tile state value index)
  (let ([start (state->image state)]
        [tile (make-tile value)]
        [i (quotient index *side*)]
        [j (remainder index *side*)])
    (build-list 4 (λ (m) 
                    (λ () 
                      (place-tile/ij (overlay 
                                      (scale (* 0.2 (add1 m)) tile)
                                      (plain-tile 0))
                                     i j
                                     start))))))


(define (won-game? state)
  (= (apply max state) *tile-that-wins*))


;Banner overlay text: e.g. You won! / Game Over, etc.
(define (banner txt state)
  (let ([color 'black])
  (define (helper txt state color)
  (let ([b-text (text txt (floor (* (/ *side* 4) 30)) color)])
    (overlay
     b-text
     (rectangle (* 1.2 (image-width b-text))
                (* 1.4 (image-height b-text))
                'solid 'white)
     (state->image state))))
    (helper txt state color)))



;Respond to a key-press
(define (change w a-key)
  (match-define (world st wt frames) w)
  (match-define (list op moves-op) (key->op a-key))
  (cond [op
         (let* ([grid (chop st *side*)]
                [after-slide (flatten (op grid))])
           (if (equal? after-slide st) w
               (let* ([replace (random (count-zeros after-slide))]
                      [index (indexof-nth-zero after-slide (+ 1 replace))]
                      [value (new-tile)]
                      [new-state (list-set after-slide index value)])
                 
                 (world new-state #f (append frames (animate-moving-tiles st moves-op) (animate-appearing-tile after-slide value index))))))]
         [else w]))                                 


;There's a 90% chance that a new tile will be a two; 10% a four.
(define (new-tile)
  (let ([k (random 100)])
    (if (< k 15) 4 2)))

; Create a random initial game-board with two non-zeros (2 or 4) and the rest 0s.
; E.g. '(0 0 0 0 0 2 0 0 2 0 0 0 0 0 0 0)
(define (initial-state)
  (let ([side *side*])
    (shuffle (append (list (new-tile) (new-tile)) (build-list (- (sqr side) 2) (lambda (x) 0))))))

(define (all-state)
  '(2 4 8 0 32 0 0 64 128 0 256 0 512 0 1024 2048))

;If there are frames, shows the next one. Otherwise shows the steady state.
(define (show-world w)
  (match-define (world st wt frames) w)
  (let ([board (if (null? frames)
                   (cond [(finished? st) (banner "Game Over" st)]
                         [else (state->image st)])
                   ((car frames)))])
    (scale *magnification* board)))

;Move to the next frame in the animation.
(define (advance-frame w)
  (match-define (world state wt frames) w)
  (if (null? frames)
      w
      (world state wt (rest frames))))

;The event loop
  (big-bang (world (initial-state) #f '())
            (to-draw show-world)
            (on-key change)
            (on-tick advance-frame 0.01)
            (name "2048")))









































                     


                     















(define (bbground)
  (rectangle 1000 500 "solid" (color #xbb #xad #xa0 )))


;;startscreen2048
(define (2048-start)
  (let* ((no(number->string *side*))
         (p1(place-image (text  (string-append "(" no "x" no ")")  50 "black") 500 250 (bbground)))
         (p2(place-image (text "<"  90 "brown") 300 250 p1))
         (p3(place-image (text ">"  90 "brown") 700 250 p2))
         (rec(rectangle 500 50 "solid" "coral"))
         (p4(place-image rec 500 350 p3))
         (p5(place-image (text "Start Game" 40 "white") 500 350 p4))
         (p6(place-image rec 500 410 p5))
         (p7(place-image (text "How To Play ?" 40 "white") 500 410 p6))
         (p8(place-image (text "2048 MERGE" 80 "SaddleBrown") 500 100 p7))
         (rec1 (rectangle 90 90 "outline" "brown"))
         (p9 (place-image rec1 300 250 p8))
         (p10 (place-image rec1 700 250 p9)))            
 p10))
(define (2048-start-region? x y)
  (cond [(and (>= x 250) (<= x 750) (<= y 375)(>= y 325)) #t]
        [else #f]))
(define (2048-credits-region? x y)
  (cond [(and (>= x 250) (<= x 750) (<= y 435)(>= y 385)) #t]
        [else #f]))
(define (2048-larrow-region? x y)
  (cond [(and (>= x 255) (<= x 345) (>= y 205)(<= y 295)) #t]
        [else #f]))
(define (2048-rarrow-region? x y)
  (cond [(and (>= x 655) (<= x 745) (>= y 205)(<= y 295)) #t]
        [else #f]))

(define (2048-credits)
  (let* ((p1 (place-image (text " Use your arrow keys to move the tiles" 30 "white")
                         500 100 (bbground)))
         (p2 (place-image (text "When two tiles with the same number touch," 30 "white")
                         500 150 p1))
         (p3 (place-image (text "they merge into one!" 30 "white")
                         500 200 p2))
         (rec(rectangle 400 50 "solid" "coral"))
         (p4 (place-image rec 500 300 p3))
         (p5 (place-image (text "MAIN MENU" 40 "white") 500 300 p4))         
         )
    p5))
(define (credit-menu? x y)
  (cond [(and (>= x 300) (<= x 700) (>= y 275)(<= y 325)) #t]
        [else #f]))

(define (draw2048 t)
  (cond [(= t 0) (2048-start)]
        [(= t 1) (2048-credits)]))

(define (m2048 t x y k)
  (if (eq? k "button-down")
      (cond [(and (= t 0) (2048-credits-region? x y)) (set! s2048 1) s2048]
            [(and (= t 0) (2048-start-region? x y))  (f2048) s2048]
            [(and (= t 1) (if (credit-menu? x y) (begin (set! t 0) t) t))]
            [(and (= t 0) (2048-larrow-region? x y)) (if (= *side* 2) t (begin  (set! *side* (- *side* 1)) t))]
            [(and (= t 0) (2048-rarrow-region? x y)) (if (= *side* 9) t (begin (set! *side* (+ *side* 1)) t))]
            [else t])
      t))

(define s2048 0)
(define (start2)
  (big-bang s2048
            (to-draw draw2048)
            (on-mouse m2048)
            (name "2048")))

