;ID that references student's name
(defconstant +ID+ "Michael Winter")

;function that prints student ID, course number, and assignment number

(defun id (course assignment)
(if
  (and (integerp course)(integerp assignment))
  (progn
  (princ "Name: ")(princ +ID+)(terpri)
  (princ "Course: ICS")(princ course)(terpri)
  (princ "Assignment: ")(princ assignment)
  )))


; wizards_game part 1

;Holds description for rooms
(defparameter *nodes* '((living-room (you are in the living-room.
                            a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                        (bedroom (you are in the bedroom. 
                            a sorceress sleeps soundly.)) ;added 
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))))

;describes location
(defun describe-location (location nodes)
"describes location"
   (cadr (assoc location nodes)))

;Links the locations together 
(defparameter *edges* '((living-room (garden west door)  
                                     (attic upstairs ladder))
                        (garden (living-room east door)
                                     (bedroom south door))
                        (attic (living-room downstairs ladder))
                        (bedroom (garden north door))))
						
;describes the different paths to take from this location
(defun describe-path (edge)
	"prints the different paths to take from this location"
  `(there is a,(caddr edge) going,(cadr edge) from here.))

 ;maps locations to edges
(defun describe-paths (location edges)
"maps locations to edges"
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

  ;list of valid objects to 'pick up'
(defparameter *objects* '(whiskey bucket frog chain pillow left-gauntlet right-gauntlet reinforced-helmet chain-mail full-suit-of-armor))

;indicates where in the world the objects lie
(defparameter *object-locations* '((whiskey living-room)
									(left-gauntlet living-room);added
                                   (bucket living-room)
								   (right-gauntlet attic)
                                   (pillow bedroom);added
								   (reinforced-helmet bedroom);added
                                   (chain garden);
								   (chain-mail garden);added								   
                                   (frog garden)))

;updates objects presence
(defun objects-at (loc objs obj-loc)
"updates objects presence"
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

;describes the object
(defun describe-objects (loc objs obj-loc)
"describes the object"
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;instantiates location initially in the living-room
(defparameter *location* 'living-room)

;returns the current location, possible paths to take, and possible objects to pick up
(defun look ()
"returns the current location, possible paths to take, and possible objects to pick up"
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))


;moves you from location by taking valid paths from current location to a new location
(defun walk (direction)
	"moves you from location by taking valid paths from current location to a new location"
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

;picks up the object from the location
(defun pickup (object)
	"picks up the object from the location"
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t '(you cannot get that.))))

;shows what items you are carrying
(defun inventory ()
	"shows what items you are carrying"
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;checks if you have item
(defun have (object) 
	"checks to see if item is in the inventory"
    (member object (cdr (inventory))))

;  wizards_game part 2
;interactive function that asks for user input, appends parenthesis to user input and restricts commands available for use	
(defun game-repl ()
	"interactive function that asks for user input, appends parenthesis to user input and restricts commands available for use"
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))
;helper function that is able to read words and transforms them into lists
(defun game-read ()
"helper function that is able to read words and transforms them into lists"
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory help h ?))

;prints out availiable commands and hints
(defun ? ()
	(princ "Availiable commands: pickup - use to pickup items you see")
	(terpri)
	(princ "inventory - check to see which items you already have")
	(terpri)
	(princ "look - to see your surroundings")
	(terpri)
	(princ "walk - if you see an available path type walk and path (east, west, etc.) to head that rout")
	(terpri)
	(princ "weld - weld a chain to something to reach places you couldn't previously... type: weld ____ _____ and see what happens")
	(terpri)
	(princ "combine - ever wanted to be a knight? Collect the 4 parts and combine them in the garden!")
	(terpri)
	(princ "dunk - if you have something to hold water you might be able to dunk it in a well or something...")
	(terpri)
	(princ "splash - no one sleeps forever.. unless there dead, are there any dead people here?")
)

;only evaluates if the commands are allowed
(defun game-eval (sexp)
	";only evaluates if the commands are allowed"
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

;tweaks text so that it follows english rules of grammar (proper capitalization, etc)
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;print that uses tweak-text presentation
(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))

;Macro 	
(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
            '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))

(defparameter *chain-welded* nil)

;welds chain to bucket
(game-action weld chain bucket attic
             (if (and (have 'bucket) (not *chain-welded*))
                 (progn (setf *chain-welded* 't)
                        '(the chain is now securely welded to the bucket.))
               '(you do not have a bucket.)))

(defparameter *bucket-filled* nil)

;dunks bucket to fill it with water
(game-action dunk bucket well garden
             (if *chain-welded* 
                 (progn (setf *bucket-filled* 't)
                        '(the bucket is now full of water))
               '(the water level is too low to reach.)))

;combine all the pieces
(game-action combine chain-mail reinforced-helmet garden
			(if (and(have 'left-gauntlet)(have 'right-gauntlet) (have 'reinforced-helmet) (have 'chain-mail))
				(progn (push (list 'full-suit-of-armor 'body) *object-locations*)
						(setf *objects* '(whiskey bucket frog chain pillow full-suit-of-armor)) 
						'(The full suit is yours.... its almost too heavy))
			'(you do not have all the pieces)))
			
;win or lose game
(game-action splash bucket wizard living-room
             (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
                   ((have 'frog) '(the wizard awakens and sees that you stole his frog. 
                                   he is so upset he banishes you to the 
                                   netherworlds- you lose! the end.))
                   (t '(the wizard awakens from his slumber and greets you warmly. 
                        he hands you the magic low-carb donut- you win! the end.))))

;Macro to add new object to the world	
(defmacro new-object (new-object location)
	;make sure object is not already created and location exists
	`(cond ((and(not(member ,new-object *objects*)) (assoc ,location *nodes*)) 
		;update objects list, and object-locations
		(nconc *objects* (cdr ',new-object))
		(nconc *object-locations*  (list(cons ,new-object (cons ,location ()))))
		)
		(t(princ "object already exists or location is not valid"))
	)
)
						
;Macro to add new location to the world
(defmacro new-location (new-location description)
	;make sure that new location does not already exist
	`(cond((and(not(assoc ,new-location *nodes*))(listp ,description))
		;update *nodes*
		(nconc *nodes* (list(cons ,new-location(cons ,description ())))))
	)
)
						
;Macro to add new path between locations
(defmacro new-path (location1 location2 direction via)
	;check to make sure that both locations exist
	`(cond
		((and(assoc ,location1 *nodes*) (assoc ,location2 *nodes*))
		;add path to the elements of the list
		(let ((tail (cdr(find ,location1 *edges* :key #'car))))
		(princ tail)
		(push `(,,location1 . ((,,location2 ,,direction ,,via) . ,tail))*edges*)
		)
	))
)

(defmacro add-new (location1 description &optional (location2 nil))
	`(if (not(,location2)))
	`(new-location ,location1 ,description)
	`((new-location ,location1 ,description) (new-path ,location1 ,location2 blank door)))
	
;macro that expands into the new-object, new-path, and new-location
(defmacro add-to-world (location1 description &optional location2 direction via object)
		`(cond
			;no 2nd location is given so just add the location with no path
			((not(,location2)) 
				(new-location ,location1 ,description))
			;you want to add a path
			((and(,location1)(,location2)(,direction)(,via))
				(new-path ,location1 ,location2 ,direction ,via))
			;you want to add a location with a path
			((and(,location1)(,location2)(,direction)(,via)(,description))
				(new-location ,location1 ,description)
				(new-path ,location1 ,location2 ,direction ,via))
			;you want to add a location with a path and an object
			((and(,location1)(,location2)(,direction)(,via)(,description)(,object))
				(new-location ,location1 ,description)
				(new-path ,location1 ,location2 ,direction ,via)
				(new-object ,object ,location1))))