(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(define eternity
  (lambda (x)
    (eternity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
	   (build (second (first pair))
		  (second pair)))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
		   (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
	(➕ (length* (first pora))
	    (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
	(➕ (✕ (weight* (first pora)) 2)
	    (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
		   (shuffle (second pora)))))))

(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
	(cond
	  ((even? n) (C (➗ n 2)))
	  (else (C (add1 (✕ 3 n)))))))))

(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
	       (A n (sub1 m)))))))

(define last-try
  (lambda (x)
    (and (will-stop? last-try)
	 (eternity x))))

(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))

(lambda (l)
  (cond
    ((null? l) 0)
    (else
      (add1
	((lambda (l)
	   (cond
	     ((null? l) 0)
	     (else (add1
		     (eternity (cdr l))))))
	 (cdr l))))))

(lambda (l)
  (cond
    ((null? l) 0)
    (else
      (add1
	((lambda (l)
	   (cond
	     ((null? l) 0)
	     (else
	       (add1
		 ((lambda (l)
		    (cond
		      ((null? l) 0)
		      (else
			(add1
			  (eternity
			    (cdr l))))))
		  (cdr l))))))
	 (cdr l))))))

((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)

((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
	((null? l) 0)
	(else (add1 (g (cdr l)))))))
  eternity))

((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
	((null? l) 0)
	(else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
	 ((null? l) 0)
	 (else (add1 (length (cdr l)))))))
   eternity)))

((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

((lambda (mk-length)
   (mk-length
     (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

((lambda (mk-length)
   (mk-length
     (mk-length
       (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

((lambda (mk-length)
   (mk-length
     (mk-length
       (mk-length
	 (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
	       ((mk-length eternity)
		(cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond
	  ((null? l) 0)
	  (else (add1 (length (cdr l)))))))
    (mk-length mk-length))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else
	 (add1
	   ((lambda (x)
	      ((mk-length mk-length) x))
	    (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond
	  ((null? l) 0)
	  (else
	    (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
	    ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
