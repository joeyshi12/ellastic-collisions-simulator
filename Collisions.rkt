;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Particle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

;; Physics

;; =================
;; Constants:

(define WIDTH 500)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))
(define RADIUS 10)
(define PARTICLE-IMG (circle RADIUS "solid" "red"))
(define FPS 300)
(define DT (/ 1 FPS))
(define GRAVITY (* 10 FPS))

;; =================
;; Data definitions:

(@HtDD Particle)
(define-struct particle (x y vx vy))
;; Particle is (make-particle Number Number Number Number)
;; interp. x-coordinate in pixels should be between [RADIUS, (- WIDTH RADIUS)]
;;         y-coordinate in pixels should be between [RADIUS, (- HEIGHT RADIUS)]
;;         vx in pixels per tick is the velocity for the x-coordinate
;;         vy in pixels per tick is the velocity for the y-coordinate

(define P1 (make-particle 20 300 (* 3 FPS) (* -3 FPS)))
(define P2 (make-particle 250 250 (* -5.5 FPS) (* 2.5 FPS)))
(define P3 (make-particle 100 300 (* 2.5 FPS) (* -2.5 FPS)))
(define P4 (make-particle 200 100 (* 0.2 FPS) (* 2.5 FPS)))
(define P5 (make-particle 80 100 (* 1.5 FPS) (* -0.5 FPS)))


(@dd-template-rules compound) ;4 fields
(define (fn-for-particle p)
  (... (particle-x p)
       (particle-y p)
       (particle-vx p)
       (particle-vy p)))










(@HtDD ListOfParticle)
;; ListOfParticle is one-of:
;; - empty
;; - (cons Particle ListOfParticle)
;; interp. a list of particles

(define LOP1 empty)
(define LOP2 (cons P1 LOP1))
(define LOP3 (cons P2 LOP2))
(define LOP4 (cons P3 LOP3))
(define LOP5 (cons P4 LOP4))
(define LOP6 (cons P5 LOP5))

(@dd-template-rules one-of           ;2 cases
                    atomic-distinct  ;empty
                    compound         ;cons
                    ref              ;(first lop) is Particle
                    self-ref)        ;(rest lop) is ListOfParticle

(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else (... (fn-for-particle (first lop))
                   (fn-for-lop (rest lop)))]))










;; =================
;; Functions:

(@HtDF main)
(@signature ListOfParticle -> ListOfParticle)
;; start the world with (main LOP6)
;; 
(define (main lop)
  (big-bang lop                             ; ListOfParticle
    (on-tick   next-particles DT)   ; ListOfParticle -> ListOfParticle
    (to-draw   render-particles)))  ; ListOfParticle -> Image










(@HtDF next-particles)
(@signature ListOfParticle -> ListOfParticle)
;; produce a list of (next-particle p) for each p in lop
(check-expect (next-particles empty) empty)

;(define (next-particles lop) lop) ;stub

(@template ListOfParticle)
(define (next-particles lop)
  (cond [(empty? lop) empty]
        [else
         (cons (next-particle (first lop))
               (next-particles (rest lop)))]))










(@HtDF next-particle)
(@signature Particle -> Particle)
;; produce next p by taking (x,y)+(vx,vy)*DT as new (x,y) position


;(define (next-particle p) p) ;stub

(@template Particle)
(define (next-particle p)
  (make-particle (+ (particle-x p) (* (next-velocity-x p) DT))
                 (+ (particle-y p) (* (next-velocity-y p) DT))
                 (next-velocity-x p)
                 (next-velocity-y p)))










(@HtDF next-velocity-x)
(@signature Particle -> Number)
;; produce next vx by taking vx + ax*DT as new vx


;(define (next-velocity-x p) 0) ;stub

(@template Particle)
(define (next-velocity-x p)
  (cond [(> (particle-x p) (- WIDTH RADIUS))
         (- (abs (particle-vx p)))]
        [(< (particle-x p) RADIUS)
         (abs (particle-vx p))]
        [else
         (particle-vx p)]))










(@HtDF next-velocity-y)
(@signature Particle -> Number)
;; produce next vy by taking vy + ay*DT as new vy


;(define (next-velocity-y p) 0) ;stub

(@template Particle)
(define (next-velocity-y p)
  (cond [(> (particle-y p) (- HEIGHT RADIUS))
         (- (abs (particle-vy p)))]
        [(< (particle-y p) RADIUS)
         (abs (particle-vy p))]
        [else
         (+ (particle-vy p) (* GRAVITY DT))]))









(@HtDF render-particles)
(@signature ListOfParticle -> Image)
;; render each p in lop on MTS
(check-expect (render-particles LOP1) MTS)
(check-expect (render-particles LOP3)
              (place-image PARTICLE-IMG
                           (particle-x P1)
                           (particle-y P1)
                           (place-image PARTICLE-IMG
                                        (particle-x P2)
                                        (particle-y P2)
                                        MTS)))

;(define (render-particles lop) empty-image) ;stub

(define (render-particles lop)
  (cond [(empty? lop) MTS]
        [else
         (place-particle (first lop) (render-particles (rest lop)))]))










(@HtDF place-particle)
(@signature Particle Image -> Image)
;; place PARTICLE-IMG at Particle (x,y) position on img
(check-expect (place-particle P1 MTS)
              (place-image PARTICLE-IMG
                           (particle-x P1)
                           (particle-y P1)
                           MTS))

;(define (place-particle p img) empty-image) ;stub

(define (place-particle p img)
  (place-image PARTICLE-IMG
               (particle-x p)
               (particle-y p)
               img))