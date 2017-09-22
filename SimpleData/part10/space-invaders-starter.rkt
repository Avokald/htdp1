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

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles t))
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
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))



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

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G5 (make-game (list (make-invader (/ WIDTH 2) 0 1)) empty T0))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; Game -> Game
;; start the world with (main G0)

(define (main game)
  (big-bang game
            (on-tick next-game)     ; Game -> Game
            (to-draw render-game)   ; Game -> Image
            (on-key handle-key)     ; Game -> Game
            (stop-when stop-game))) ; Game -> Boolean



;; ===============================================================================
;; ADVANCE PART

;; Game -> Game
;; produces a game in a next tick of big-bang, changing values' accordingly

;(define (next-game game) game) ; stub

(define (next-game game)
  (rm-hit-invaders-missiles (game-wo-hits game)))


;; Game -> Game
;; advances a game without rocket and invaders' destruction.
; |||
#;
(check-expect (game-wo-hits G0)
              (make-game (cons (make-invader 150 0 10) empty)
                         empty
                         (make-tank (+ (/ WIDTH 2) 2) 1) ))
; |||
#;
(check-expect (game-wo-hits (make-game
                             (cons (make-invader 150 (/ HEIGHT 2) -20) empty)
                             (cons (make-missile 150 300) empty)
                             T1))
              (make-game (cons
                          (make-invader 130 (+ (/ HEIGHT 2) 20) -20)
                          (cons
                           (make-invader 150 0 -5)
                           empty))
                         (cons (make-missile 150 (+ 300 MISSILE-SPEED)) empty)
                         (make-tank 52 1)))

(define (game-wo-hits game)
  (make-game (advance-invaders (game-invaders game))
             (advance-missiles (game-missiles game))                
             (advance-tank (game-t game))))


;; ========================================================================
;; INVADERS PART

;; ListOfInvader -> ListOfInvader
;; Produces ListOfInvader advancing every invader on the screen
;; and adding one invader if all of them are already half way to the bottom

(define (advance-invaders invaders)
  (mk-new-invader (advance-wo-invaders invaders)))

;; ListOfInvader -> ListOfInvader
;; Adds one invader if all other are already half way to the bottom
;; !!!
(define (mk-new-invader invaders) invaders) ; stub


;; ListOfInvader -> ListOfInvader
;; just advances Invaders further on the screen
; |||
#;
(check-expect (advance-wo-invaders empty)
              (cons (make-invader 150 0 10) empty))
; |||
#; 
(check-expect (advance-wo-invaders (cons (make-invader 150 (/ HEIGHT 2) -20) empty))
              (cons
               (make-invader 130 (+ (/ HEIGHT 2) 20) -20)
               (cons
                (make-invader 150 0 -5)
                empty)))

;(define (advance-wo-invaders invader) invader) ; stub

;; <template from invaders>
(define (advance-wo-invaders invaders)
  (cond [(empty? invaders) empty]
        [else
         (cons (advance-invader (first invaders))
               (advance-wo-invaders (rest invaders)))]))


;; Invader -> Invader
;; advances a given invader to his new position according to his coordinates and speed
(check-expect (advance-invader (make-invader 150 100 12))
              (make-invader 162 112 12))
(check-expect (advance-invader (make-invader 300 200 -15))
              (make-invader 285 215 -15))
(check-expect (advance-invader (make-invader WIDTH 100 20))
              (make-invader (+ WIDTH -20) 120 -20))
(check-expect (advance-invader (make-invader WIDTH 100 -20))
              (make-invader (+ WIDTH -20) 120 -20))
(check-expect (advance-invader (make-invader 0 200 -15))
              (make-invader 15 215 15))
(check-expect (advance-invader (make-invader 0 200 15))
              (make-invader 15 215 15))

;(define (advance-invader invader) invader) ; stub

(define (advance-invader invader)
  (advance-rev-invader (check-invader invader)))


;; Invader -> Invader
;; checks if invader is on the screen, else changes it's directon
(check-expect (check-invader (make-invader 0 0 1)) (make-invader 0 0 1))
(check-expect (check-invader (make-invader 0 0 -1)) (make-invader 0 0 1))
(check-expect (check-invader (make-invader WIDTH 0 10)) (make-invader WIDTH 0 -10))
(check-expect (check-invader (make-invader WIDTH 0 -5)) (make-invader WIDTH 0 -5))
(check-expect (check-invader (make-invader (/ WIDTH 2) (/ HEIGHT 2) 10)) (make-invader (/ WIDTH 2) (/ HEIGHT 2) 10))

;(define (check-invader invader) invader) ; stub

(define (check-invader invader)
  (if (or (and (>= (invader-x invader) WIDTH) (>= (invader-dx invader) 0))
          (and (<= (invader-x invader) 0) (<= (invader-dx invader) 0)))
      (make-invader (invader-x invader) (invader-y invader) (* (invader-dx invader) -1))
      invader))


;; Invader -> Invader
;; advances checked invader to the next position
(check-expect (advance-rev-invader (make-invader 150 100 12))
              (make-invader 162 112 12))
(check-expect (advance-rev-invader (make-invader 300 200 -15))
              (make-invader 285 215 -15))

;(define (advance-rev-invader invader) invader) ; stub

;; <template from invader>
(define (advance-rev-invader invader)
  (make-invader (+ (invader-x invader) (invader-dx invader))
                (+ (invader-y invader) (abs (invader-dx invader)))
                (invader-dx invader)))


;; ===================================================================================
;; MISSILES PART

;; ListOfMissiles -> ListOfMissiles
;; removes the missiles that are already beyond the screen
(check-expect (rm-far-lom empty) empty)
(check-expect (rm-far-lom (list (make-missile (/ WIDTH 2) (/ HEIGHT 2))))
              (list (make-missile (/ WIDTH 2) (/ HEIGHT 2))))
(check-expect (rm-far-lom (list (make-missile (/ WIDTH 3) 0))) empty)
(check-expect (rm-far-lom (list (make-missile 0 HEIGHT) (make-missile 150 -1)))
              (list (make-missile 0 HEIGHT)))
(check-expect (rm-far-lom (list (make-missile WIDTH 220) (make-missile 75 1)))
              (list (make-missile WIDTH 220) (make-missile 75 1)))

;(define (rm-far-lom lom) lom) ; stub

(define (rm-far-lom lom)
  (cond [(empty? lom) empty]
        [else
         (if (<= (missile-y (first lom)) 0)
             (rm-far-lom (rest lom))
             (cons (first lom) (rm-far-lom (rest lom))))]))

;; ListOfMissiles -> ListOfMissiles
;; Advances the position of missiles and removes one that reached the top of the screen
(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list (make-missile 150 200) (make-missile 20 0)))
              (list (make-missile 150 190)))

;(define (advance-missiles missiles) empty) ; stub

(define (advance-missiles missiles)
  (rm-far-lom (advance-missiles-wo-rm missiles)))


;; ListOfMissiles -> ListOfMissiles
;; Advances the position of missiles

(check-expect (advance-missiles-wo-rm empty) empty)
(check-expect (advance-missiles-wo-rm (cons (make-missile 150 300) empty))
              (cons (make-missile 150 290) empty))
(check-expect (advance-missiles-wo-rm
               (cons (make-missile 100 100) (cons (make-missile 190 20) empty)))
              (cons (make-missile 100 90) (cons (make-missile 190 10) empty)))

;(define (advance-missiles-wo-rm missiles) missiles) ; stub

(define (advance-missiles-wo-rm missiles)
  (cond [(empty? missiles) empty]
        [else
         (cons (advance-missile  (first missiles))
               (advance-missiles-wo-rm (rest missiles)))]))

;; Missile -> Missile
;; advances the position missile
(check-expect (advance-missile (make-missile 150 300)) (make-missile 150 (- 300 MISSILE-SPEED)))
(check-expect (advance-missile (make-missile 0 10)) (make-missile 0 0))

;(define (advance-missile missile) missile) ; stub

;; <template from missile>
(define (advance-missile missile)
  (make-missile (missile-x missile) (- (missile-y missile) MISSILE-SPEED)))


;; ===================================================================================
;; TANK PART

;; Tank -> Tank
;; advances the position of tank by its speed
(check-expect (advance-tank (make-tank 50 1)) (make-tank 52 1))
(check-expect (advance-tank (make-tank 50 -1)) (make-tank 48 -1))

;(define (advance-tank t) t) ; stub

;; <template from tank>
(define (advance-tank t)
  (cond [(brick? t)
         (make-tank (tank-x t) (* (tank-dir t) -1))]
        [else
         (make-tank (+ (tank-x t)
                       (* (tank-dir t) TANK-SPEED))
                    (tank-dir t))]))

;; Tank -> Boolean
;; checks if tank is still on the screen
(check-expect (brick? (make-tank (/ WIDTH 2) 1)) false)
(check-expect (brick? (make-tank WIDTH 1)) true)
(check-expect (brick? (make-tank WIDTH -1)) false)
(check-expect (brick? (make-tank 0 -1)) true)
(check-expect (brick? (make-tank 0 1)) false)

;(define (brick? t) false) ; stub

(define (brick? t)
  (or (and (>= (tank-x t) WIDTH) (= (tank-dir t) 1))
      (and (<= (tank-x t) 0) (= (tank-dir t) -1))))


 
;; Game -> Game
;; Deletes invaders and rockets that were destroyed
;; !!!

(define (rm-hit-invaders-missiles game)
  (make-game (rm-missiles-from-invaders (game-invaders game) (game-missiles game))
             (rm-invaders-from-missiles (game-invaders game) (game-missiles game))
             (game-t game)))

;; ListOfInvader ListOfMissile -> ListOfInvader
;; produces list of invaders removing ones, that have the same values of coordinates as missiles in the list of missiles
(check-expect (rm-missiles-from-invaders empty empty) empty)
(check-expect (rm-missiles-from-invaders (list (make-invader 0 0 1)) empty) (list (make-invader 0 0 1)))
(check-expect (rm-missiles-from-invaders empty (list (make-missile 0 0))) empty)
(check-expect (rm-missiles-from-invaders (list (make-invader 0 0 1))
                                         (list (make-missile 0 0)))
              empty)
(check-expect (rm-missiles-from-invaders (list (make-invader 0 0 1))
                                         (list (make-missile 0 9)))
              empty)
(check-expect (rm-missiles-from-invaders (list (make-invader 0 0 1)
                                               (make-invader 100 100 2))
                                         (list (make-missile 100 93)))
              (list (make-invader 0 0 1)))
(check-expect (rm-missiles-from-invaders (list (make-invader 50 50 3))
                                         (list (make-missile 42 49)
                                               (make-missile 0 20)))
              empty)
(check-expect (rm-missiles-from-invaders (list (make-invader 130 20 2)
                                               (make-invader 220 0 1)
                                               (make-invader 56 34 8))
                                         (list (make-missile 49 32)
                                               (make-missile 220 0)
                                               (make-missile 0 0)))
              (list (make-invader 130 20 2)))

;(define (rm-missiles-from-invaders invaders missiles) empty) ; stub

(define (rm-missiles-from-invaders invaders missiles)
  (cond [(empty? invaders) empty]
        [(empty? missiles) invaders]
        [else
         (rm-missiles-from-invaders (rm-missile-from-invaders invaders (first missiles)) (rest missiles))]))

;; ListOfInvader Missile -> ListOfInvader
;; checks if Missile hit any of the given invaders, produces a list of unhit invaders
(check-expect (rm-missile-from-invaders empty (make-missile 0 0)) empty)
(check-expect (rm-missile-from-invaders (list (make-invader 100 100 1))
                                        (make-missile 0 0))
              (list (make-invader 100 100 1)))
(check-expect (rm-missile-from-invaders (list (make-invader 0 0 2))
                                        (make-missile 0 0))
              empty)
(check-expect (rm-missile-from-invaders (list (make-invader 0 0 3)
                                              (make-invader 100 200 1)
                                              (make-invader WIDTH HEIGHT 2))
                                        (make-missile 98 201))
              (list (make-invader 0 0 3)
                    (make-invader WIDTH HEIGHT 2)))

;(define (rm-missile-from-invaders invaders missile) empty) ; stub

(define (rm-missile-from-invaders invaders missile)
  (cond [(empty? invaders) empty]
        [else
         (if (< (sqrt (+ (sqr (- (invader-x (first invaders))
                                 (missile-x missile)))
                         (sqr (- (invader-y (first invaders))
                                 (missile-y missile)))))
                10)
             (rest invaders)
             (cons (first invaders) (rm-missile-from-invaders (rest invaders) missile)))]))


  
;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; produces list of missiles removing ones, that have the same values of coordinates as invaders in the list of invaders
;; !!!
(check-expect (rm-invaders-from-missiles empty empty) empty)
(check-expect (rm-invaders-from-missiles (list (make-invader 0 0 1))
                                         empty)
              empty)
(check-expect (rm-invaders-from-missiles empty
                                         (list (make-missile 0 0)))
              (list (make-missile 0 0)))
(check-expect (rm-invaders-from-missiles (list (make-invader 0 0 1))
                                         (list (make-missile 0 0)))
              empty)
(check-expect (rm-invaders-from-missiles (list (make-invader 0 0 1))
                                         (list (make-missile 0 9)))
              empty)
(check-expect (rm-invaders-from-missiles (list (make-invader 100 93 1))
                                         (list (make-missile 0 0)
                                               (make-missile 100 100)))
              (list (make-missile 0 0)))
(check-expect (rm-invaders-from-missiles (list (make-invader 50 50 3))
                                         (list (make-missile 42 49)
                                               (make-missile 0 20)))
              (list (make-missile 0 20)))
(check-expect (rm-invaders-from-missiles (list (make-invader 130 20 2)
                                               (make-invader 220 0 1)
                                               (make-invader 56 34 8))
                                         (list (make-missile 49 32)
                                               (make-missile 220 0)
                                               (make-missile 0 0)))
              (list (make-missile 0 0)))

;(define (rm-invaders-from-missiles invaders missiles) missiles) ; stub

(define (rm-invaders-from-missiles invaders missiles)
  (cond [(empty? invaders) missiles]
        [(empty? missiles) empty]
        [else
         (rm-invaders-from-missiles
          (rest invaders)
          (rm-invader-from-missiles (first invaders) missiles))]))


;; Invader ListOfMissile -> ListOfMissile
;; checks if Invader hit any of the given missiles, produces a list of unhit missiles
(check-expect (rm-invader-from-missiles (make-invader 0 0 1) empty) empty)
(check-expect (rm-invader-from-missiles (make-invader 100 100 1)
                                        (list (make-missile 0 0)))
              (list (make-missile 0 0)))
(check-expect (rm-invader-from-missiles (make-invader 0 0 2)
                                        (list (make-missile 0 0)))
              empty)
(check-expect (rm-invader-from-missiles (make-invader 0 0 3)
                                        (list (make-missile 0 0)
                                              (make-missile 100 200)
                                              (make-missile WIDTH HEIGHT)))
              (list (make-missile 100 200)
                    (make-missile WIDTH HEIGHT)))

;(define (rm-invader-from-missiles invader missiles) empty) ; stub

(define (rm-invader-from-missiles invader missiles)
  (cond [(empty? missiles) empty]
        [else
         (if (< (sqrt (+ (sqr (- (missile-x (first missiles))
                                 (invader-x invader)))
                         (sqr (- (missile-y (first missiles))
                                 (invader-y invader)))))
                10)
             (rest missiles)
             (cons (first missiles) (rm-invader-from-missiles invader (rest missiles))))]))




;; ==============================================================================
;; RENDER PART

;; Game -> Image
;; produces an image of the game at the exact moment

(check-expect (render-game G0)
              (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

(check-expect (render-game G1)
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
 
(check-expect (render-game G2)
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2)
                           (place-image MISSILE 150 300
                                        (place-image INVADER 150 100
                                                     BACKGROUND))))

;(define (render-game game) BACKGROUND) ; stub

;; <template from game>
(define (render-game game)
  (render-loinvader (game-invaders game)
                    (render-lom (game-missiles game)
                                (render-tank (game-t game) BACKGROUND))))


;; ListOfInvader -> Image
;; renders image of invaders

(check-expect (render-loinvader empty BACKGROUND) BACKGROUND)
(check-expect (render-loinvader (cons I1 empty) BACKGROUND)
              (place-image INVADER 150 100 BACKGROUND))
(check-expect (render-loinvader (cons I1 (cons I2 (cons I3 empty))) BACKGROUND)
              (place-image INVADER 150 100
                           (place-image INVADER 150 HEIGHT
                                        (place-image INVADER 150 (+ HEIGHT 10) BACKGROUND))))
              
;(define (render-loinvader loi bg) INVADER) ; stub

(define (render-loinvader loi bg)
  (cond [(empty? loi) bg]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi))
                      (render-loinvader (rest loi) bg))]))


;; ListOfMissile -> ListOfMissile
;; renders image of missiles
(check-expect (render-lom empty BACKGROUND) BACKGROUND)
(check-expect (render-lom (cons M1 empty) BACKGROUND)
              (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))
(check-expect (render-lom (cons (make-missile 100 200)
                                (cons (make-missile 130 120)
                                      (cons (make-missile 210 230) empty)))
                          BACKGROUND)
              (place-image MISSILE 100 200
                           (place-image MISSILE 130 120
                                        (place-image MISSILE 210 230 BACKGROUND))))
                          
;(define (render-lom lom) MISSILE) ; stub

(define (render-lom lom bg)
  (cond [(empty? lom) bg]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-lom (rest lom) bg))]))


;; Tank -> Image
;; renders Tank
(check-expect (render-tank T0 BACKGROUND)
              (place-image TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-tank T1 BACKGROUND)
              (place-image TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;(define (render-tank t) TANK) ; stub

(define (render-tank t bg)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) bg))

  
;; Game -> Game
;; produces a game changing values of position and creating objects according to the pressed key
(check-expect (handle-key G0 " ")
              (make-game (game-invaders G0)
                         (list (make-missile (tank-x (game-t G0)) (- HEIGHT TANK-HEIGHT/2)))
                         (game-t G0)))
(check-expect (handle-key G0 "o") G0)

(define (handle-key game key-pressed)
  (cond [(key=? " " key-pressed)
         (make-game (game-invaders game)
                    (crt-new-missile (game-missiles game) (game-t game))
                    (game-t game))]
        [else game]))


;; ListOfMissiles Tank-> ListOfMissiles
;; produces a new instance of missile according to the position of tank
(define (crt-new-missile lom t)
  (cons (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT/2)) lom))


;; Game -> Boolean
;; Checks if the game should end considering the position of enemies
(check-expect (stop-game G0) false)
(check-expect (stop-game (make-game (list (make-invader (/ WIDTH 2) HEIGHT 10)) empty T0)) true)
(check-expect (stop-game (make-game (list (make-invader WIDTH 0 32) (make-invader 30 HEIGHT 2)) empty T1)) true)

;(define (stop-game game) false) ; stub

(define (stop-game game)
  (check-loinvader (game-invaders game)))


;; ListOfInvader -> Boolean
;; produces true if any invaders reached the bottom of the screen
(check-expect (check-loinvader empty) false)
(check-expect (check-loinvader (list (make-invader 0 0 1))) false)
(check-expect (check-loinvader (list (make-invader (/ WIDTH 2) HEIGHT 10))) true)
(check-expect (check-loinvader (list (make-invader 200 120 5)
                                     (make-invader 100 (- HEIGHT 1) 9))) false)

;(define (check-loinvader loi) false) ; stub

(define (check-loinvader loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (check-loinvader (rest loi)))]))





