(defclass card()
  ((rank :initarg :rank :reader rank
	 :documentation "The rank of the card")
   (suit :initarg :suit :reader suit
	 :documentation "The suit of the card"))
  (:documentation "The class of playing cards"))

  (defconstant *ranks*
	       '(ace 2 3 4 5 6 7 8 9 10 jack queen king)
	       "A list of the 13 ranks in a deck of cards.")

  (defconstant *suits*
	       '(spades diamonds hearts clubs)
	       "A list of suits in a deck of cards.")

(setq AC (make-instance 'card :rank 'Ace :suit 'Clubs))
(setq 2C (make-instance 'card :rank '2 :suit 'Clubs))
(setq 3C (make-instance 'card :rank '3 :suit 'Clubs))
(setq 4C (make-instance 'card :rank '4 :suit 'Clubs))
(setq 5C (make-instance 'card :rank '5 :suit 'Clubs))
(setq 6C (make-instance 'card :rank '6 :suit 'Clubs))
(setq 7C (make-instance 'card :rank '7 :suit 'Clubs))
(setq 8C (make-instance 'card :rank '8 :suit 'Clubs))
(setq 9C (make-instance 'card :rank '9 :suit 'Clubs))
(setq 10C (make-instance 'card :rank '10 :suit 'Clubs))
(setq JC (make-instance 'card :rank 'Jack :suit 'Clubs))
(setq QC (make-instance 'card :rank 'Queen :suit 'Clubs))
(setq KC (make-instance 'card :rank 'King :suit 'Clubs))
(setq AD (make-instance 'card :rank 'Ace :suit 'Diamonds))
(setq 2D (make-instance 'card :rank '2 :suit 'Diamonds))
(setq 3D (make-instance 'card :rank '3 :suit 'Diamonds))
(setq 4D (make-instance 'card :rank '4 :suit 'Diamonds))
(setq 5D (make-instance 'card :rank '5 :suit 'Diamonds))
(setq 6D (make-instance 'card :rank '6 :suit 'Diamonds))
(setq 7D (make-instance 'card :rank '7 :suit 'Diamonds))
(setq 8D (make-instance 'card :rank '8 :suit 'Diamonds))
(setq 9D (make-instance 'card :rank '9 :suit 'Diamonds))
(setq 10D (make-instance 'card :rank '10 :suit 'Diamonds))
(setq JD (make-instance 'card :rank 'Jack :suit 'Diamonds))
(setq QD (make-instance 'card :rank 'Queen :suit 'Diamonds))
(setq KD (make-instance 'card :rank 'King :suit 'Diamonds))
(setq AH (make-instance 'card :rank 'Ace :suit 'Hearts))
(setq 2H (make-instance 'card :rank '2 :suit 'Hearts))
(setq 3H (make-instance 'card :rank '3 :suit 'Hearts))
(setq 4H (make-instance 'card :rank '4 :suit 'Hearts))
(setq 5H (make-instance 'card :rank '5 :suit 'Hearts))
(setq 6H (make-instance 'card :rank '6 :suit 'Hearts))
(setq 7H (make-instance 'card :rank '7 :suit 'Hearts))
(setq 8H (make-instance 'card :rank '8 :suit 'Hearts))
(setq 9H (make-instance 'card :rank '9 :suit 'Hearts))
(setq 10H (make-instance 'card :rank '10 :suit 'Hearts))
(setq JH (make-instance 'card :rank 'Jack :suit 'Hearts))
(setq QH (make-instance 'card :rank 'Queen :suit 'Hearts))
(setq KH (make-instance 'card :rank 'King :suit 'Hearts))
(setq AS (make-instance 'card :rank 'Ace :suit 'Spades))
(setq 2S (make-instance 'card :rank '2 :suit 'Spades))
(setq 3S (make-instance 'card :rank '3 :suit 'Spades))
(setq 4S (make-instance 'card :rank '4 :suit 'Spades))
(setq 5S (make-instance 'card :rank '5 :suit 'Spades))
(setq 6S (make-instance 'card :rank '6 :suit 'Spades))
(setq 7S (make-instance 'card :rank '7 :suit 'Spades))
(setq 8S (make-instance 'card :rank '8 :suit 'Spades))
(setq 9S (make-instance 'card :rank '9 :suit 'Spades))
(setq 10S (make-instance 'card :rank '10 :suit 'Spades))
(setq JS (make-instance 'card :rank 'Jack :suit 'Spades))
(setq QS (make-instance 'card :rank 'Queen :suit 'Spades))
(setq KS (make-instance 'card :rank 'King :suit 'Spades))

(defconstant *deck* (list ac 2c 3c 4c 5c 6c 7c 8c 9c 10c jc qc kc
			  ad 2d 3d 4d 5d 6d 7d 8d 9d 10d jd qd kd
			  ah 2h 3h 4h 5h 6h 7h 8h 9h 10h jh qh kh
			  as 2s 3s 4s 5s 6s 7s 8s 9s 10s js qs ks))

; *deck* actually is the list of 52 cards
;  To create that list, create 4 smaller lists containing
;     each rank for its corresponding suit.
;    (AH, 2H, 3H ... QH KH)
;    (AS, 2S, ... QS KS)
;    (AD ... KD)
;    (AC ... KC)
;

(defmethod print-object ((o (eql 'Clubs)) stream)
  (princ 'C stream))
(defmethod print-object ((o (eql 'Diamonds)) stream)
  (princ 'D stream))
(defmethod print-object ((o (eql 'Hearts)) stream)
  (princ 'H stream))
(defmethod print-object ((o (eql 'Spades)) stream)
  (princ 'S stream))

(defmethod print-object ((o (eql 'Ace)) stream)
  (princ 'A stream))
(defmethod print-object ((o (eql 'Jack)) stream)
  (princ 'J stream))
(defmethod print-object ((o (eql 'Queen)) stream)
  (princ 'Q stream))
(defmethod print-object ((o (eql 'King)) stream)
  (princ 'K stream))

(defmethod print-object ((c card) stream)
  (format stream "~a~a" (rank c) (suit c)))

(defun shuffle ()
	    (dotimes (i 52)
             (let (( j (random 52)))
	      (psetf (nth i *deck*) (nth j *deck*)
		     (nth j *deck*) (nth i *deck*)))))


(defgeneric color (c)
   (:documentation "Returns the color of a card.")
   (:method ((c card))
	    (case (suit c)
	      ((diamonds hearts) 'red)
	      ((clubs spades) 'black))))

(defclass pile ()
 ((cards :initform '() :accessor cards
	 :documentation
	 "A list of the cards in the pile"))
 (:documentation "A pile of cards"))

(defmethod top ((p pile))
  (car (cards p)))

(defclass face-up-pile (pile)
  ()
  (:documentation "A pile of face-up cards"))

(defmethod print-object ((p face-up-pile) stream)
  (if (cards p)
    (princ (car (cards p)) stream)
    (princ '_ stream)))

(defclass face-down-pile (pile)
  ()
  (:documentation "A pile of face-down cards"))

(defmethod print-object ((p face-down-pile) stream)
  (if (cards p)
;    (dolist (n (cards p))
      (princ 'X stream)
    (princ '_ stream)))

(defclass two-way-pile (pile)
  ((upcards :initform '() :accessor upcards
	    :documentation "A second list of cards"))
  (:documentation
    "A pile with some face-up cards on top of some face-down cards"))

(defgeneric emptyup (p)
	    (:documentation
	      "Returns T if the two-way-pile has no face-up cards")
	    (:method ((p two-way-pile))
		     (null (upcards p))))

; The book wants us to define prin-rev for the upcards.
(defmethod print-object ((p two-way-pile) stream)
;  (cond ((upcards p) (print-object (upcards p) stream))
;	((cards p) 
;	 (dolist (n (cards p))
;         (princ 'X stream)))
;	(t (princ '_))))
   (dolist (n (cards p))
     (princ 'X stream))
   (print-object (upcards p) stream))

(defclass foundation-pile (face-up-pile)
  ()
  (:documentation "The victory pile going from Ace to King of the same suit.")
  )

(defclass stock (face-down-pile)
  ()
  )

(defclass waste (face-up-pile)
  ()
  )

(defmethod print-object ((p waste) stream)
  (print-object (car (cards p)) stream))

(defclass tableau-pile (two-way-pile)
  ()
  (:documentation "Down cards and Up cards of red and black cards."))


; When we change the cards of any of these accessors,
;  we type something like
;    (cards (aref (foundations *layout*) 0) ).
;  Otherwise, if we don't use that cards accessor,
;    we merely end up with (AC 2C 3C..) and we use print-object for
;    lists rather than print-object for pile.
(defclass layout ()
  ((foundations
     :accessor foundations
     :initform (let ((a (make-array 4)))
		 (dotimes (i 4)
		   (setf (aref a i)
			 (make-instance
			   'foundation-pile)))
    a))
   (tableau
     :accessor tableau
     :initform (let ((a (make-array 7)))
		 (dotimes (i 7)
		   (setf (aref a i)
			 (make-instance 'tableau-pile)))
		 a))
   (stock :accessor stock
	  :initform (make-instance 'stock))
   (waste :accessor waste
	  :initform (make-instance 'waste)))
  (:documentation "The layout of a Klondike game."))

(defmethod print-object ((l layout) stream)
  (let ((tableauList (tableau l)))
      (print-object (foundations l) stream)
      (fresh-line) 
    (dotimes (i 7)
      (princ 'T)
      (princ i)
      (princ " ")
      (print-object (aref tableauList i) stream)
      (fresh-line))
    (princ 'Stock)
    (princ " ")
    (print-object (stock l) stream)
    (fresh-line)
    (princ 'Waste)
    (princ " ")
    (print-object (waste l) stream)
    (fresh-line)))
	   

(setq *layout* '())

(defun deal ()
  (setf (cards (stock *layout*) ) *deck*)
  (let ( (tableauList (tableau *layout*)) )
    (dotimes (i 7)
      (let ( (currentTableau (aref tableauList i ))) 
	(dotimes (j i )
	  (setf (cards currentTableau)
		(append (cards currentTableau) 
			(list (pop (cards (stock *layout*))))))
	  )
	(setf (upcards currentTableau)
	      (append (upcards currentTableau)
		      (list (pop (cards (stock *layout*))))))))))

(defgeneric moveto (p1 p2)
	    (:documentation
	      "Moves the appropriate card(s) from pile P1 to pile P2."))

(defgeneric legal (c p)
	    (:documentation
	      "Returns T if putting C on P is legal;
	      NIL otherwise."))

(defmethod legal ((sourcePile tableau-pile) (destinationPile tableau-pile))
  (cond ( (eql NIL (upcards destinationPile))
	  (eql (rank (car (upcards sourcePile))) 'king))
	(t (let ( (highCard (car (last (upcards destinationPile))))
		  (lowCard (car (upcards sourcePile))))
	     (and (eql (position (rank lowCard) *ranks*)
		      (- (position (rank highCard) *ranks*) 1))
		  (not (eql (color lowCard) (color highCard))))))))

(defmethod legal ((sourcePile stock) (destinationPile waste))
  (if (cards sourcePile)
    t
    NIL))

(defmethod legal ((sourcePile waste) (destinationPile tableau-pile))
  (cond ( (eql NIL (upcards destinationPile))
	  (eql (rank (car (cards sourcePile))) 'king))
	(t (let ( (highCard (car (last (upcards destinationPile))))
		  (lowCard (car (cards sourcePile))))
	     (and (eql (position (rank lowCard) *ranks*)
		      (- (position (rank highCard) *ranks*) 1))
		  (not (eql (color lowCard) (color highCard))))))))

(defmethod legal ((sourcePile waste) (destinationPile foundation-pile))
  (cond ( (eql NIL (cards destinationPile))
	  (eql (rank (car (cards sourcePile))) 'ace))
	(t (let ( (lowCard (car (cards destinationPile)))
		  (highCard (car (cards sourcePile))))
	     (and (eql (position (rank lowCard) *ranks*)
		       (- (position (rank highCard) *ranks*) 1))
		  (eql (color lowCard) (color highCard)))))))
	 
(defmethod legal ((sourcePile tableau-pile) (destinationPile foundation-pile))
  (cond ( (eql NIL (cards destinationPile))
	  (eql (rank (car (last (upcards sourcePile)))) 'ace))
	(t (let ( (lowCard (car (cards destinationPile)))
		  (highCard (car (last (upcards sourcePile)))))
	     (and (eql (position (rank lowCard) *ranks*)
		       (- (position (rank highCard) *ranks*) 1))
		  (eql (color lowCard) (color highCard)))))))

(defmethod moveto ((sourcePile stock) (destinationPile waste))
  (if (legal sourcePile destinationPile)
    (setf (cards destinationPile)
	  (append (list (pop (cards sourcePile)))
		  (cards destinationPile)))))

(defmethod moveto ((sourcePile tableau-pile) (destinationPile tableau-pile))
  (if (legal sourcePile destinationPile)
    (setf (upcards destinationPile)
	  (append (upcards destinationPile) (upcards sourcePile))
	  (upcards sourcePile) (last (cards sourcePile))
	  (cards sourcePile) (butlast (cards sourcePile)))
    (princ "Illegal tableau to tableau move")))

(defmethod moveto ((sourcePile waste) (destinationPile tableau-pile))
  (if (legal sourcePile destinationPile)
    (setf (upcards destinationPile)
	  (append (upcards destinationPile) (list (pop (cards sourcePile)))))
    (princ "Illegal waste to tableau move")))

(defmethod moveto ((sourcePile tableau-pile) (destinationPile foundation-pile))
  (if (legal sourcePile destinationPile)
    (setf (cards destinationPile)
	  (append (last (upcards sourcePile)) (cards destinationPile))
	  (upcards sourcePile) (last (cards sourcePile))
	  (cards sourcePile) (butlast (cards sourcePile)))
    (princ "Illegal tableau to foundation move")))

(defmethod moveto ((sourcePile waste) (destinationPile foundation-pile))
  (if (legal sourcePile destinationPile)
    (setf (cards destinationPile)
	  (append (list (pop (cards sourcePile))) (cards destinationPile)))
    (princ "Illegal waste to foundation move")))
	  

(defun translate (input)
  (cond ((eql input 'T0) (aref (tableau *layout*) 0))
	((eql input 'T1) (aref (tableau *layout*) 1))
	((eql input 'T2) (aref (tableau *layout*) 2))
	((eql input 'T3) (aref (tableau *layout*) 3))
	((eql input 'T4) (aref (tableau *layout*) 4))
	((eql input 'T5) (aref (tableau *layout*) 5))
	((eql input 'T6) (aref (tableau *layout*) 6))
	((eql input 'F0) (aref (foundations *layout*) 0))
	((eql input 'F1) (aref (foundations *layout*) 1))
	((eql input 'F2) (aref (foundations *layout*) 2))
	((eql input 'F3) (aref (foundations *layout*) 3))
	((eql input 'stock) (stock *layout*))
	((eql input 'waste) (waste *layout*))
	(t NIL)))


(defun play ()
  (setf *layout* (make-instance 'layout))
  (shuffle)
  (deal)
  (let (move)
    (loop
      (print *layout*)
      (setf move (read))
      (when (eq move 'quit) (return))
      (moveto (translate (read)) (translate (read)))))
   (if (= 52 (+ (size (aref (foundations *layout*) 0))
                (size (aref (foundations *layout*) 1))
                (size (aref (foundations *layout*) 2))
                (size (aref (foundations *layout*) 3))))
        (princ "You win!!!")
        (prince "Sorry, you lose."))
    (values))

