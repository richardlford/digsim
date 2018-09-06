(require-extension lookup-table)
(require-extension format)

(define params 
    (alist->dict
    '(
    (damping-coefficient .  8.88)   ; Damping force per velocity [N/m/s]
    (gravity             .  9.88)   ; Acceleration due to gravity [m/sec**2)
    (mass                .   1.0)   ; Mass suspended from spring [Kg]
    (spring-coefficient  . 39.47)   ; Restoring force per position [N/m]
    (x_ic                .   0.0)   ; Initial position of suspended mass [m]
    (xd_ic               .   0.0)   ; Initial velocity of suspended mass [m/sec]
    (tstop               .   2.5)
    (dt                  .  0.01))))

(define (diffeq x xd)
    (let* (
            (sc (dict-ref params 'spring-coefficient))
            (dc (dict-ref params 'damping-coefficient))
            (mass (dict-ref params 'mass))
            (grav (dict-ref params 'gravity)))
        (- 0.0 (/ (+ (* sc x) (* dc xd)) mass) grav)))

(define (state->string state)
    (format "~{~6,5,2E~3_~}~%" state))

 ; xdd := -(spring_coefficient*x + damping_coefficient*xd)/mass - gravity;

 (define (sim-impl time x xd output-lines)
    (if (> time (dict-ref params 'tstop))
        output-lines
        (let (
             (line (state->string (list time x xd)))
             (dt (dict-ref params 'dt))
             (xdd (diffeq x xd)))
            
             (sim-impl
                (+ time dt)
                (+ x (* xd dt))
                (+ xd (* xdd dt))
                (append output-lines (list line)) ))))

(begin
    (let (  (tstart 0.0)
            (x (dict-ref params 'x_ic))
            (xd (dict-ref params 'xd_ic)))
         (display (sim-impl tstart x xd '()))))