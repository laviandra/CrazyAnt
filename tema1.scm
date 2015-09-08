#lang racket
;SCHIMBA AICI FISIERUL DE INTRARE
(define in (open-input-file "test.in"))

;citeste o linie si o transforma in lista
(define fstline (string->list (read-line in)))

(define space? (lambda(c) (char=? c #\space)))
(define mark? (lambda(c) (char=? c #\!)))
(define point? (lambda(c) (char=? c #\.)))
(define food? (lambda(c) (char=? c #\F)))

(define getLineL 
  ;returneaza lista cu caractere din nr linii
  (lambda(f)
    (if (space? (car f)) '()
        (cons (car f) (getLineL (cdr f))))))
;returneaza nr linii
(define (getLine init) (string->number (list->string (getLineL init)))) 
(define getCol
  ;returneaza nr de coloane
  (lambda(f)
    (if (space? (car f)) (string->number (list->string (cdr f)))
        (getCol (cdr f)))))
;intoarce o lista cu numere reprezentate ca stringuri
(define parseLine
  (lambda(l p)
    (if (null? l) p
        (if (space? (car l)) (parseLine (cdr l) (cons '() p))
        (parseLine (cdr l) (cons (cons (car l) (car p)) (cdr p)))))))
;intoarce numerele dintr-o linie
(define (getNr lin) (map string->number (reverse (map list->string (map reverse (parseLine lin '(())))))))

(define size (getNr fstline))
;citeste matrice
(define readMat
  (lambda(l i lim)
    (if (equal? i (car lim)) l
        (readMat (cons (string->list (read-line in)) l) (+ i 1) lim))))

;transforma matricea in vector
(define harta (list->vector (map list->vector (reverse (readMat '() 0 size)))))
;ia primul element dintr-o lista
(define (getX lst) (car lst))
;ia al doilea element dintr-o lista
(define (getY lst) (car (cdr lst)))

(define lstline (string->list (read-line in)))
(define ant_pos (list (getX (getNr lstline)) (getY (getNr lstline))))
(define life (car (cdr (cdr (getNr lstline)))))
(define food_val 0)

;====================== START OF GUI ===================================
(require racket/gui/base)
(define in_ant (open-input-file "ant2.jpg"))
(define in_food (open-input-file "beer2.png"))
(define in_grass (open-input-file "grass2.jpg"))
(define in_mark (open-input-file "mark2.png"))
(define furnica (make-object bitmap% in_ant))
(define food (make-object bitmap% in_food))
(define grass (make-object bitmap% in_grass))
(define mark (make-object bitmap% in_mark))
(define frame (new frame% [label "Antsy"]
                    [height 800]
                    [width 600]))
(define my-draw 
   (lambda(canvas dc i j lim)
     (if (food?  (vector-ref (vector-ref harta i) j))
         (send dc draw-bitmap food (* 100 j) (* 100 i))
         (if (mark? (vector-ref (vector-ref harta i) j))
             (send dc draw-bitmap mark (* 100 j) (* 100 i))
                 (send dc draw-bitmap grass (* 100 j) (* 100 i))))
    (if (and (equal? i (getX ant_pos)) (equal? j (getY ant_pos)))
        (send dc draw-bitmap furnica (* 100 j) (* 100 i)) #f)
      
     (if (and (equal? i (- (getX lim) 1)) (equal? j  (- (getY lim) 1))) #t
     (if (equal? j (- (car (cdr lim)) 1)) (my-draw canvas dc (+ i 1) 0 lim)
         (my-draw canvas dc i (+ j 1) lim))
     )))
 
 (define draw_function 
   (lambda (canvas dc)
     (my-draw canvas dc 0 0 size)))
 
 (define board (new canvas% [parent frame]
      [style '(border vscroll hscroll)]
      [min-width 700]
      [min-height 500]
      [paint-callback draw_function]
     ))
 (send board init-auto-scrollbars (* 100 (car size)) (* 100 (car (cdr size)))  0 0)
 (define panel5 (new horizontal-panel% [parent frame]
                      [alignment '(center center)]))
(define panel (new horizontal-panel% [parent frame]
                     [alignment '(center center)]
                    ))
  
  (define panel2 (new vertical-panel% [parent panel]
                      [vert-margin 25]))
  (define panel3 (new vertical-panel% [parent panel]
                      [vert-margin 25]))
  (define panel4 (new vertical-panel% [parent panel]
                      [vert-margin 25]
                      ))
 
  (define life_msg (new message% [parent panel2]
                        [label "Life remaining"]))
  (define life_msg_val (new text-field% [parent panel2]
                            [label "Life"]
                            [init-value (number->string life) ]
                            [enabled #f]))
  (define beh_msg (new message% [parent panel2]
                        [label "Choose your behaviour: "]))
  (define food_msg (new message% [parent panel3]
                        [label "Food"]))
  (define food_msg_val (new text-field% [parent panel3]
                            [label "Food"]
                            [init-value (number->string food_val)]
                            [enabled #f]))
  (define behaviour (new text-field% [parent panel3]
                            [label "1 or 2"]
                            [init-value (number->string 1)]
                            [enabled #t]))
  
  (define btn1 (new button% [parent panel4]
             [label "Next"]
               [callback (lambda (button event) 
                           (behave (if (equal? (send behaviour get-value) "1") function1 function2)))]))
(define previous (new text-field% [parent panel4]
                            [label "Previous\nmove"]
                            [init-value "0" ]
                            [enabled #f]))
(define status (new message% [parent panel5]
                    [label "Still alive"]
                    [auto-resize #t]))
 (send frame show #t)
 
; ======================== Behaviour =================================
 (define canGoUp 
   (lambda(x y)
     (if (> x 0) #t #f)))
 (define canGoDown
   (lambda(x y)
     (if (< x (- (getX size) 1)) #t #f)))
 (define canGoLeft
   (lambda(x y)
     (if (> y 0) #t #f)))
 (define canGoRight
   (lambda(x y)
     (if (< y (- (getY size) 1)) #t #f)))
 
 ;inspecteaza pozitiile din matrice din jurul (x,y)
 (define getUp
   (lambda(x y)
     (if (canGoUp x y)
       (vector-ref (vector-ref harta (- x 1)) y) #\w)))
 
 (define getDown
   (lambda(x y)
     (if (canGoDown x y) 
         (vector-ref (vector-ref harta (+ x 1)) y) #\w)))
 
 (define getLeft
   (lambda(x y)
     (if (canGoLeft x y)
       (vector-ref (vector-ref harta x) (- y 1)) #\w)))
 
 (define getRight
   (lambda(x y)
     (if (canGoRight x y)
       (vector-ref (vector-ref harta x) (+ y 1)) #\w)))
 
 ;sterge urma sau mancare
 (define removeMark
   (lambda (x y)
     (vector-set! (vector-ref harta x) y #\.)))
 ;intoarce o pozitie daca exista o urma/mancare in jurul (x, y)
 (define getMarkPos
   (lambda(x y test)
     (if (test (getUp x y)) (goUp x y)
         (if (test (getLeft x y)) (goLeft x y)
             (if (test (getRight x y)) (goRight x y)
                 (if (test (getDown x y)) (goDown x y)
                     #\w
                     ))))))
      
 (define behave
   (lambda(f)
   (cond 
     [(< (string->number (send life_msg_val get-value)) 1) (send status set-label "Stop trying to move me! I am already dead!!!") ] 
     [(not (equal? (getMarkPos (getX ant_pos) (getY ant_pos) food?) #\w)) 
      (begin
           (removeMark (getX ant_pos) (getY ant_pos))
           (send food_msg_val set-value (number->string (+ (string->number (send food_msg_val get-value)) 10)) )
           (send life_msg_val set-value (number->string (+ (string->number (send life_msg_val get-value)) 10)) )
           )]
     [(not (equal? (getMarkPos (getX ant_pos) (getY ant_pos) mark?) #\w))
         (removeMark (getX ant_pos) (getY ant_pos))
         ]
     [else   (f)]
     )
    
     (send life_msg_val set-value (number->string (- (string->number (send life_msg_val get-value)) 1)) )
     (send board on-paint)))
      
 (define goUp
   (lambda(x y)
      (begin (set! ant_pos (cons (- x 1) (cons y ant_pos))) (send previous set-value "0"))))
 (define goDown
   (lambda(x y)
      (begin (set! ant_pos (cons (+ x 1) (cons y ant_pos))) (send previous set-value "1"))))
 (define goLeft
   (lambda(x y)
     (begin (set! ant_pos (cons x (cons (- y 1) ant_pos))) (send previous set-value "2"))))
 (define goRight
   (lambda(x y)
     (begin (set! ant_pos (cons x (cons (+ y 1) ant_pos))) (send previous set-value "3"))))


 (define function1
   (lambda()
     (let ((r (random 2)) (prev (string->number (send previous get-value)))  (x (getX ant_pos)) (y (getY ant_pos)))
       (cond
         [(or (equal? prev 0)  (equal? prev 1))
                              (if (and (equal? r 0) (canGoLeft x y))
                                  (goLeft x y)
                                  (if (canGoRight x y) 
                                      (goRight x y) 
                                      (if (canGoDown x y) (goDown x y) (goUp x y))
                                      ))]
          [(or (equal? prev 3) (equal? prev 2))
                              (if (and (equal? r 0) (canGoUp x y))
                                  (goUp x y)
                                  (if (canGoDown x y) (goDown x y) 
                                      (if (canGoLeft x y) (goLeft x y) (goRight x y))))]
          ))))
 (define move
   ;misca furnica in prima pozitie posibila
   (lambda(x y)
     (cond
       [(canGoRight x y) (goRight x y)]
       [(canGoLeft x y) (goLeft x y)]
       [(canGoUp x y) (goUp x y)]
       [(canGoDown x y) (goDown x y)]
       )))
 (define function2
   (lambda()
     (let ((r (random 3)) (prev (string->number (send previous get-value))) (x (getX ant_pos)) (y (getY ant_pos)))
       (cond 
         [(equal? prev 0) (cond
                            [(and (equal? r 0) (canGoUp x y)) (goUp x y)]
                            [(and (equal? r 1) (canGoLeft x y)) (goLeft x y)]
                            [(and (equal? r 2) (canGoRight x y)) (goRight x y)]
                            [else  (move x y)]
                            )]
         [(equal? prev 1) (cond
                            [(and (equal? r 0) (canGoDown x y)) (goDown x y)]
                            [(and (equal? r 1) (canGoLeft x y)) (goLeft x y)]
                            [(and (equal? r 2) (canGoRight x y)) (goRight x y)]
                            [else (move x y)]
                            )]
         [(equal? prev 2) (cond
                            [(and (equal? r 0) (canGoLeft x y)) (goLeft x y)]
                            [(and (equal? r 1) (canGoUp x y)) (goUp x y)]
                            [(and (equal? r 2) (canGoDown x y)) (goDown x y)]
                            [else (move x y) ]
                            )]
         [(equal? prev 3) (cond
                            [(and (equal? r 0) (canGoRight x y)) (goRight x y)]
                            [(and (equal? r 1) (canGoUp x y)) (goUp x y)]
                            [(and (equal? r 2) (canGoDown x y)) (goDown x y)]
                            [else (move x y)]
                            )]
         ))))
         
       
     
                                  
     
 