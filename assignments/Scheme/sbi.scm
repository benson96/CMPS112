#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;;
;; Jinxuan Jiang
;; jjiang17@ucsc.edu
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;
(define *stderr* (current-error-port))



(define (cont) (void))

(define *run-file*
    (let-values (((dirpath basepath root?)
        (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *error-message*)) list)
    (newline *error-message*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)


(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((infile (read inputfile)))
                  (close-input-port inputfile)
                         infile))))


(define *fTable* (make-hash))
(define *vTable* (make-hash))
(define *lTable* (make-hash))



(define (fget key)
        (hash-ref *fTable* key))
(define (vget key)
        (hash-ref *vTable* key))
(define (fput! key value)
        (hash-set! *fTable* key value))
(define (lget key)
        (hash-ref *lTable* key))
(define (vput! key value)
        (hash-set! *vTable* key value))
 (define (lput! key value)
        (hash-set! *lTable* key value))



(define (dimHelp st)
        (begin(vput! (caar st) (make-vector (process (cadar st))))
        (fput! (caar st) (lambda(x)
        (vector-ref (vget (caar st))(- x 1)))))
)
(define val1 1)
(define (DIM str)
    (cont)
    (if( not(null? str))
    (begin(vput! (caar str) (make-vector (process (cadar str))))
    (cont)
    (fput! (caar str) (lambda(x) 
    (cont)
    (vector-ref (vget (caar str))(- x val1)))))
    (exit))
)


(define (LET str)
(define adder 1)
  (if (pair? (car str))
(begin
     (vector-set! (vget
        (caar str)) (- (process (cadar str )) 
            adder) (process (cadr str )))
    )
    (begin
     (let ((final (process (cadr str ))))
       (vput! (car str) final)
     ))
     
     ))


(define (GOTO label infile)
(define stt "Could not find label")
    (if(not(null? label))
      
    (begin((writeTo infile (lget (car label)))))
      (display stt))
)

(define (read.. val)
(let ((object (read)))
      (cond [(eof-object? object)(vput! 'inputcount -1)]
       [(number? object)(vput! (car val) object)
        (vput! 'inputcount (+ (vget 'inputcount) 1))]
          [else ((display "not a valid number: ~a~n" object))] )) 
) 

(define (numSplit val) 
    (vput! (car val) (cont))
        (read.. val)(when (not (null? 
(cdr val)))(numSplit (cdr val)))
   )

(define (INPUT val)
(define incrementer 1)
(when (not (null? (car val)))
   (vput! 'inputcount (- incrementer incrementer))
   (numSplit val))
)

(define (process myData)
  (if (pair? myData)
  (apply 
        (fget (car myData)) 
        (map process (cdr myData)))
    (begin    
      (cond ((not(number? myData)) (vget myData))               
          (else myData)))
    
    
    )  
)

(define (PRINT myData)
(if (not (null?  myData) )
     (begin
          (if  (not(string? (car myData)))        
            (display (process (car myData))) 
            (display (car myData))        
          )         
          (PRINT (cdr myData))
      )
         (newline)))

(define (IF regex label infile)
    (helpIF.. regex str)
    (exitH '(regex e1 e2 e3) label infile)
)
(define (helpIF regex str )
    (cond (not( regex '<))
        (cont)
    )
    (cond (not( regex "="))
        (cont)
    )
    (cond (not( regex ">"))
        (cont)
    )
    (cond (not( regex "<="))
        (cont)
    )
    (cond (not( regex ">="))
        (cont)
    )
    (cond (not( regex "<>"))
        (cont)
    )
)
(define myf 'if)
(define gto 'goto)
(for-each
    (lambda (pair)
            (fput! (car pair) (cadr pair)))
    `(
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (log10_2 0.301029995663981195213738894724493026768189881)
        
        
       
        (%       ,(lambda (x y) (- x (* (div x y) y))))
         (<=      ,(lambda (x y) (<= x y)))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+ ,+) (- ,-) (* ,*) (abs ,abs) 
         (=       ,(lambda (x y) (eqv? x y)))
        (>=      ,(lambda (x y) (>= x y)))
        (/       ,(lambda (x y)  (/ x (if (equal? y 0) 0.0 y))))
        (<       ,(lambda (x y) (< x y)))
        (>       ,(lambda (x y) (> x y)))
      
        (<>      ,(lambda (x y) (not (equal? x y))))
        (^       ,(lambda (x y) (expt x y)))
         (sqrt ,sqrt) (sin ,sin)  (cos ,cos) (tan ,tan)
         (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (ceil ,ceiling) (floor ,floor) (exp ,exp)
        (log     ,(lambda(x)(log (if (equal? x 0) 0.0 x))))
        (atan    ,(lambda(x)(atan (if (equal? x 0) 0.0 x))))
       
        (if      ,(cont))   
        (let     ,LET)
        (print   ,PRINT)
        (input   ,INPUT)
        
        (dim     ,DIM)
        (goto    ,GOTO) (asin ,asin) (acos ,acos) (round ,round)
     ))
(for-each
    (lambda (pair)
            (vput! (car pair) (cadr pair)))
    `(
        (define addNum 1)
        (inputcount (- addNum addNum))
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        
     ))


(define (find vec)
    (when (not (null? vec))
        (when (number? (caar vec))
           (if (null? (cdar vec))
              (cont)
              (if (symbol? (cadar vec))
                 (lput! (cadar vec) (caar vec))
                  (cont)))
        (find (cdr vec)))))

(define (exit.. in infile)
(when (not (hash-has-key? *fTable* (car in)))
        (display (car in))(display " invalid\n")
         (usage-exit)))

(define (exitH in infile addf)
(define add1 1)
(define tr #t)
(if (not(null? in))
  (begin (exit.. in infile)
  (cond ((eqv? (car in) gto)
      (writeTo infile (- (lget (cadr in)) add1)))
    ((eqv? (car in) myf)
      (if (equal? tr (process (cadr in)))
       
        (writeTo infile (-(lget(caddr in)) add1))
        (writeTo infile (+ addf add1)))
    )  
    (else
      ((fget(car in))(cdr in))
      (writeTo infile (+ addf add1))
    )))
    (writeTo infile (+ addf add1)))
)

(define (writeTo infile addf)
        (when (not(>= addf (length infile)))
        (let ((str (list-ref infile addf)))
             (cond ((equal? (length str) (+ 1 2))
             (set! str (cddr str))(exitH(car str)infile addf))
        ((and (equal? (length str) (+ 1 1)) (list? (cadr str)))
        (set! str(cdr str))(exitH(car str)infile addf))
               (else(writeTo infile (+ addf 1))))))
)

(define (main arglist)
    (if (and (not(null? arglist)) (null? (cdr arglist)))
       
        (let* ((sbprogfile (car arglist))
               (infile (readlist-from-inputfile sbprogfile)))
               (find infile)
               (writeTo infile 0)
               )
                (usage-exit)
               ))

(main (vector->list (current-command-line-arguments)))
