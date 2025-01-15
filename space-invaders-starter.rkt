;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define BLANK (square 0 "solid" "white"))

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define-struct point (x y))
;; Point is (make-point Number Number)
;; interp. a point in an XoY coordinate system

(define P0 (make-point 0 0))
(define P1 (make-point 100 100))
(define P2 (make-point 400 100))

#;
(define (fn-for-point p)
  (... (point-x p)
       (point-y p)))

(define LOP0 empty)
(define LOP1 (list P1))
(define LOP2 (list P1 P2))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

(define-struct keyboard (key pressed?))
;; Keyboard is (make-keyboard String Boolean)
;; represents the keyboard and whether the given key is pressed or not

(define K0 (make-keyboard "left" true))
(define K1 (make-keyboard "right" false))
(define K2 (make-keyboard " " true))

#;
(define (fn-for-keyboard ke)
  (... (keyboard-key ke)
       (keyboard-pressed? ke)))


(define KEYBOARD
  (list (make-keyboard " " false)
        (make-keyboard "left" false)
        (make-keyboard "right" false)))
  

;; GameState -> GameState
;; start the world with (main (make-game empty empty T1))
;; 
(define (main gs)
  (big-bang gs                      ; GameState
    (on-tick   tock)                ; GameState -> GameState
    (to-draw   render)              ; GameState -> Image
    (stop-when stop-game?)          ; GameState -> Boolean
    (on-key    handle-key)))        ; GameState KeyEvent -> GameState
        

;; ------------------------ UPDATES PER TICK LOGIC
;; GameState -> GameState
;; produce the next placement for the game elements on the next tick
(define (tock gs)
  (remove-collisions (make-game (update-invaders-list (generate-new-invaders (game-invaders gs))) (update-missile-list (game-missiles gs)) (game-tank gs))))

;; GameState -> GameState
;; removes the collisions between the invaders and the missiles

(define (remove-collisions gs)
  (make-game (remove-all-invader-collisions (game-invaders gs) (game-missiles gs))
             (remove-all-missile-collisions (game-invaders gs) (game-missiles gs))
             (game-tank gs)))
           
;; ListOfInvaders ListOfMissile -> ListOfInvaders
;; removes collisions with all the missiles in the given list

(check-expect (remove-all-invader-collisions (list I1 I2) (list M1 M2)) (list I2)) 
(check-expect (remove-all-invader-collisions (list I1 I2) (list M1 M1)) (list I1 I2))

(define (remove-all-invader-collisions loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (remove-all-invader-collisions (remove-invader-collisions loi (first lom)) (rest lom))]))


; ListOfInvaders Missile -> ListOfInvaders
;; removes the invaders that have collided with the given missile

(check-expect (remove-invader-collisions (list I1 I2) M2) (list I2)) 
(check-expect (remove-invader-collisions (list I1 I2) M1) (list I1 I2))

;;(define (remove-list-collisions loi missile) empty) ; stub

(define (remove-invader-collisions loi missile)
  (cond [(empty? loi) empty]
        [else
         (if (not (is-collision? (first loi) missile)) 
             (cons (first loi) (remove-invader-collisions (rest loi) missile))
             (remove-invader-collisions (rest loi) missile))]))



;; ListOfInvaders ListOfMissile -> ListOfMissile
;; removes collisions with all the missiles in the given list

(check-expect (remove-all-missile-collisions (list I1 I2) (list M1 M2)) (list M1)) 
(check-expect (remove-all-missile-collisions (list I1 I2) (list M1 M1)) (list M1 M1))
              
(define (remove-all-missile-collisions loi lom)
  (cond [(empty? loi) lom]
        [(empty? lom) empty]
        [else
         (remove-all-missile-collisions (rest loi) (remove-missile-collisions lom (first loi)))]))
          


;; ListOfMissile Invader -> ListOfMissile
;; removes the missiles that have collided with the given invader

(check-expect (remove-missile-collisions (list M1 M2) I1) (list M1)) 
(check-expect (remove-missile-collisions (list M1 M2) I2) (list M1 M2))

;;(define (remove-list-collisions loi missile) empty) ; stub

(define (remove-missile-collisions lom invader)
  (cond [(empty? lom) empty]
        [else
         (if (not (is-collision? invader (first lom))) 
             (cons (first lom) (remove-missile-collisions (rest lom) invader))
             (remove-missile-collisions (rest lom) invader))]))




;; Invader Missile -> Boolean
;; checks if the 2 images (rectangles) are colliding with each other

(check-expect (is-collision? I1 M2) true)
(check-expect (is-collision? I1 M1) false)

(define (is-collision? i m)
  (interval-collision? (get-points INVADER (make-point (invader-x i) (invader-y i)))
                       (get-points MISSILE (make-point (missile-x m) (missile-y m)))))


;; ListOfPoint ListOfPoint -> Boolean
;; checks if the point intervals of 2 images collide
;; if the x intervals and the y intervals overlap, then we have collision

(check-expect (interval-collision? (list (make-point 10 20) (make-point 50 70)) (list (make-point 15 25) (make-point 90 110))) false)
(check-expect (interval-collision? (list (make-point 10 20) (make-point 50 70)) (list (make-point 15 25) (make-point 65 110))) true)

;;(define (interval-collision? lop1 lop2) false) ; stub

(define (interval-collision? lop1 lop2)

  (and (and (<= (point-x (first lop1)) (point-y (first lop2))) (<= (point-x (first lop2)) (point-y (first lop1))))
       (and (<= (point-x (first (rest lop1))) (point-y (first (rest lop2)))) (<= (point-x (first (rest lop2))) (point-y (first (rest lop1)))))))

;; Image Point -> ListOfPoints
;; returns the coordinates of the 4 points of the image rectangle
;; returns the information in a (list [x1, x2], [y1, y2])

(check-expect (get-points TANK  (make-point 50 (- WIDTH TANK-HEIGHT/2)))
              (list (make-point
                     (- 50 (/ (image-width TANK) 2))
                     (+ 50 (/ (image-width TANK) 2)))
                    (make-point (- (- WIDTH TANK-HEIGHT/2) (/ (image-height TANK) 2))
                                (+ (- WIDTH TANK-HEIGHT/2) (/ (image-height TANK) 2)))))

(check-expect (get-points MISSILE  (make-point 100 100))
              (list (make-point
                     (- 100 (/ (image-width MISSILE) 2))
                     (+ 100 (/ (image-width MISSILE) 2)))
                    (make-point (- 100 (/ (image-height MISSILE) 2))
                                (+ 100 (/ (image-height MISSILE) 2)))))
;;(define (get-points img, pos) empty) ; stub

(define (get-points img pos)
  (list (make-point (- (point-x pos) (/ (image-width img) 2)) (+ (point-x pos) (/ (image-width img) 2)))
        (make-point (- (point-y pos) (/ (image-height img) 2)) (+ (point-y pos) (/ (image-height img) 2)))))

  


;; ------------------- INVADERS LOGIC

;; ListOfInvaders -> ListOfInvaders
;; generates a new invader randomly and place it on a random x position at the top of the screen

(check-random (generate-new-invaders empty) 
              (if (= (random INVADE-RATE) 1)
                  (cons (make-invader (random WIDTH) (/ (image-height INVADER) 2) INVADER-X-SPEED) empty)
                  empty))

(define (generate-new-invaders loi)
  (if (= (random INVADE-RATE) 1)
      (cons (make-invader (random WIDTH) (/ (image-height INVADER) 2) INVADER-X-SPEED) loi)
      loi))


;; ListOfInvaders -> ListOfInvaders
;; updates the invaders list and both adds and moves the existing invaders
;;(define (update-invaders-list loi) empty)

(define (update-invaders-list loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-invader (first loi))
               (update-invaders-list (rest loi)))]))


;; Invader -> Invader
;; updates the position of the given invader
(check-expect (move-invader (make-invader (/ (image-width INVADER) 2) 10 INVADER-X-SPEED)) (make-invader (+ (/ (image-width INVADER) 2) 1.5) 11.5 INVADER-X-SPEED))
(check-expect (move-invader (make-invader (/ (image-width INVADER) 2) 10 (- INVADER-X-SPEED))) (make-invader (+ (/ (image-width INVADER) 2) 1.5) 11.5 INVADER-X-SPEED))
(check-expect (move-invader (make-invader 100 100 INVADER-X-SPEED)) (make-invader 101.5 101.5 INVADER-X-SPEED))
(check-expect (move-invader (make-invader 100 100 (- INVADER-X-SPEED))) (make-invader 98.5 101.5 (- INVADER-X-SPEED)))
(check-expect (move-invader (make-invader (- WIDTH (/ (image-width INVADER) 2)) 100 INVADER-X-SPEED)) (make-invader (- WIDTH (/ (image-width INVADER) 2) 1.5) 101.5 (- INVADER-X-SPEED)))
(check-expect (move-invader (make-invader (- WIDTH (/ (image-width INVADER) 2)) 100 (- INVADER-X-SPEED))) (make-invader (- WIDTH (/ (image-width INVADER) 2) 1.5) 101.5 (- INVADER-X-SPEED)))

;;(define (move-invader i) (make-invader 0 0 0))

(define (move-invader i)
  (make-invader (+ (invader-x i) (get-correct-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (get-correct-dx i)))

;; Invader -> Number
;; returns the correct dx corresponding to the invader's direction

(check-expect (get-correct-dx (make-invader (/ (image-width INVADER) 2) 10 -5)) 5)
(check-expect (get-correct-dx (make-invader (/ (image-width INVADER) 2) 10 5)) 5)
(check-expect (get-correct-dx (make-invader 100 100 10)) 10)
(check-expect (get-correct-dx (make-invader 100 100 -10)) -10)
(check-expect (get-correct-dx (make-invader (- WIDTH (/ (image-width INVADER) 2)) 100 10)) -10)
(check-expect (get-correct-dx (make-invader (- WIDTH (/ (image-width INVADER) 2)) 100 -10)) -10)

;;(define (get-correct-dx i) 0)

(define (get-correct-dx i)
  (if (or (and (<= (invader-x i) (/ (image-width INVADER) 2))
               (< (invader-dx i) 0))
          (and (>= (invader-x i) (- WIDTH (/ (image-width INVADER) 2)))
               (> (invader-dx i) 0)))
      (- (invader-dx i))
      (invader-dx i)))

;; --------------------------- MISSILE LOGIC

;; ListOfMissile -> ListOfMissile
;; updates the list of missiles and either moves them towards the top of the screen, or removes them from the list
;;(define (update-missile-list lom) (make-game empty empty empty)) ; stub

(define (update-missile-list lom)
  (cond [(empty? lom) empty]
        [else
         (if (< (missile-y (first lom)) 0)
             (update-missile-list (rest lom))
             (cons (update-missile (first lom))
                   (update-missile-list (rest lom))))]))  


;; Missile -> Missile
;; updates the missile's position based on it's speed

(check-expect (update-missile (make-missile 10 0)) (make-missile 10 -10))
(check-expect (update-missile (make-missile 10 10)) (make-missile 10 0))
(check-expect (update-missile (make-missile 10 100)) (make-missile 10 90))

(define (update-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))




;; ---------------------------------- RENDER LOGIC
;; GameState -> Image
;; renders the full scene to the screen
(define (render gs)
  (render-invaders gs (game-invaders gs)))


;; ListOfInvader -> Image
;; renders the invaders to the screen

(define (render-invaders gs loi)
  (cond [(empty? loi) (render-missiles gs (game-missiles gs))]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders gs (rest loi)))]))

;; GameState -> Image
;; renders the missiles and the tank to the screen

(define (render-missiles gs lom)
  (cond [(empty? lom) (render-tank gs)]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles gs (rest lom)))]))

;; GameState -> Image
;; renders the tank to the screen

(define (render-tank gs)
  (place-image TANK
               (tank-x (game-tank gs))
               (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))





;; ---------------------- GAME END LOGIC

;; GameState -> Boolean
;; stops the game if an invader reaches the bottom of the screen

(check-expect (stop-game? G0) false)
(check-expect (stop-game? (make-game (list (make-invader 100 100 1.5)) (list (make-missile 10 10)) T1)) false)
(check-expect (stop-game? (make-game (list (make-invader 100 (- HEIGHT (image-height INVADER)) 1.5)) (list (make-missile 10 10)) T1)) false)
(check-expect (stop-game? (make-game (list (make-invader 100 (- HEIGHT (/ (image-height INVADER) 2)) 1.5)) (list (make-missile 10 10)) T1)) true)

;;(define (stop-game? gs) false) ;stub

(define (stop-game? gs)
  (invader-hit? (game-invaders gs)))


;; ListOfInvader -> Boolean
;; checks if any of the invaders from the list touched the bottom

(check-expect (invader-hit? empty) false)
(check-expect (invader-hit? (list (make-invader 100 100 1.5))) false)
(check-expect (invader-hit? (list (make-invader 100 (- HEIGHT (/ (image-height INVADER) 2)) 1.5))) true)
(check-expect (invader-hit? (list (make-invader 100 HEIGHT 1.5))) true)           

(define (invader-hit? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) (- HEIGHT (/ (image-height INVADER) 2)))
             true
             (invader-hit? (rest loi)))]))



              
;; ------------- ON KEY LOGIC
;; GameState KeyEvent -> GameState
(define (handle-key gs ke)
  (cond [(key=? ke "left") (make-game (game-invaders gs) (game-missiles gs) (move-tank (make-tank (tank-x (game-tank gs)) -1)))]
        [(key=? ke "right") (make-game (game-invaders gs) (game-missiles gs) (move-tank (make-tank (tank-x (game-tank gs)) 1)))]
        [(key=? ke " ")
         (make-game (game-invaders gs) (add-missile gs) (game-tank gs))]
        [else gs]))

  

;; --------------------------- TANK LOGIC

;; Tank -> Tank
;; returns the next position of the given tank

(check-expect (move-tank (make-tank 50 1)) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (move-tank (make-tank 50 -1)) (make-tank (- 50 TANK-SPEED) -1))
(check-expect (move-tank (make-tank TANK-WIDTH/2 1)) (make-tank (+ TANK-WIDTH/2 TANK-SPEED) 1))
(check-expect (move-tank (make-tank TANK-WIDTH/2 -1)) (make-tank TANK-WIDTH/2 -1))
(check-expect (move-tank (make-tank (- WIDTH TANK-WIDTH/2) 1)) (make-tank (- WIDTH TANK-WIDTH/2) 1))
(check-expect (move-tank (make-tank (- WIDTH TANK-WIDTH/2) -1)) (make-tank (- WIDTH TANK-WIDTH/2 TANK-SPEED) -1))

;;(define (move-tank t) (make-tank 0 0)) ; stub

(define (move-tank t)
  (cond [(= (tank-dir t) 1)
         (if (< (tank-x t) (- WIDTH TANK-WIDTH/2))
             (make-tank (+ (tank-x t) TANK-SPEED) 1)
             t
             )]
        [(= (tank-dir t) -1)
         (if (> (tank-x t) TANK-WIDTH/2)
             (make-tank (- (tank-x t) TANK-SPEED) -1)
             t
             )]))



;; GameState -> ListOfMissile
;; adds the missile to the list when the tank is shooting
(check-expect (add-missile G0) (list (make-missile (tank-x (game-tank G0)) (- HEIGHT TANK-HEIGHT/2))))

(define (add-missile gs)
  (cons (make-missile (tank-x (game-tank gs))
                      (- HEIGHT TANK-HEIGHT/2))
        (game-missiles gs)))

(main G0)