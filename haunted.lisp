;; Haunted House - Common Lisp version
;; adapted from "Write your own adventure programs" by Usborne.

;; list of the locations
(defparameter *nodes*
  '((dark-corner
     "dark corner"
     (south corner-of-house)
     (east overgrown-garden))

    (overgrown-garden
     "overgrown garden"
     (west dark-corner)
     (east large-woodpile))

    (large-woodpile
     "large woodpile"
     (west overgrown-garden)
     (east yard-by-rubbish))

    (yard-by-rubbish
     "yard by rubbish"
     (south scullery-door)
     (west large-woodpile)
     (east weedpatch))

    (weedpatch
     "weedpatch"
     (west yard-by-rubbish)
     (east forest))

    (forest
     "forest"
     (west weedpatch)
     (east thick-forest))

    (thick-forest
     "thick forest"
     (south clearing-by-house)
     (west forest)
     (east blasted-tree))

    (blasted-tree
     "blasted tree"
     (south path)
     (west thick-forest))

    (corner-of-house
     "corner of house"
     (north dark-corner)
     (south side-of-house))

    (entrance-to-kitchen
     "entrance to kitchen"
     (south back-of-hallway)
     (east kitchen))

    (kitchen
     "kitchen and grimy cooker"
     (west entrance-to-kitchen)
     (east scullery-door))

    (scullery-door
     "scullery door"
     (north yard-by-rubbish)
     (west kitchen))

    (room-with-dust
     "room with inches of dust"
     (east rear-turret-room)
     (down bottom-staircase))

    (rear-turret-room
     "rear turret room"
     (west room-with-dust))

    (clearing-by-house
     "clearing by house"
     (north thick-forest)
     (east path))

    (path
     "path"
     (north blasted-tree)
     (south clifftop)
     (west clearing-by-house))

    (side-of-house
     "side of the house"
     (north corner-of-house)
     (south crumbling-wall))

    (back-of-hallway
     "back of the hallway"
     (north entrance-to-kitchen)
     (south gloomy-passage))

    (dark-alcove
     "dark alcove"
     (south pool-of-light)
     (east small-dark-room))

    (small-dark-room
     "small dark room"
     (west dark-alcove)
     (east bottom-staircase))

    (bottom-staircase
     "bottom of a spiral staircase"
     (west small-dark-room)
     (up room-with-dust))

    (wide-passage
     "wide passage"
     (south trophy-room)
     (east slippery-steps))

    (slippery-steps
     "slippery steps"
     (south barred-cellar)
     (west wide-passage)
     (up wide-passage)
     (down barred-cellar))

    (clifftop
     "clifftop"
     (north path)
     (south cliff-path-1))

    (crumbling-wall
     "near a crumbling wall"
     (north side-of-house))

    (gloomy-passage
     "gloomy passage"
     (north back-of-hallway)
     (south front-hall))

    (pool-of-light
     "pool of light"
     (north dark-alcove)
     (south sitting-room)
     (east vaulted-hallway))

    (vaulted-hallway
     "impressive vaulted hallway"
     (west pool-of-light)
     (east thick-door))

    (thick-door
     "hall by thick wooden door"
     (west vaulted-hallway)
     (east trophy-room))

    (trophy-room
     "trophy room"
     (north wide-passage)
     (south dining-room)
     (west thick-door))

    (barred-cellar
     "cellar with barred window"
     (north slippery-steps)
     (south coffin-cellar))

    (cliff-path-1
     "cliff path"
     (north clifftop)
     (south cliff-path-2))

    (cupboard
     "cupboard with hanging coat"
     (south closet))

    (front-hall
     "front hall"
     (north gloomy-passage)
     (south front-lobby)
     (east sitting-room))

    (sitting-room
     "sitting room"
     (north pool-of-light)
     (south evil-library)
     (west front-hall))

    (secret-room
     "secret room"
     (south study))

    (marble-stairs
     "steep marble stairs"
     (north thick-door)
     (south cobwebby-room)
     (up cobwebby-room)
     (down thick-door))

    (dining-room
     "dining room"
     (north trophy-room))

    (coffin-cellar
     "deep cellar with coffin"
     (north barred-cellar))

    (cliff-path-2
     "cliff path"
     (north cliff-path-1)
     (south cliff-path-3))

    (closet
     "closet"
     (north cupboard)
     (east front-lobby))

    (front-lobby
     "front lobby"
     (north front-hall)
     (south front-porch)
     (west closet))

    (evil-library
     "library of evil books"
     (north sitting-room)
     (east study))

    (study
     "study with desk and hole in the wall"
     (west evil-library))

    (cobwebby-room
     "weird cobwebby room"
     (north marble-stairs)
     (south upper-gallery)
     (east cold-chamber))

    (cold-chamber
     "very cold chamber"
     (west cobwebby-room)
     (east spooky-room))

    (spooky-room
     "spooky room"
     (west cold-chamber))

    (cliff-path-3
     "cliff path by marsh"
     (north cliff-path-2)
     (south soggy-path))

    (verandah
     "rubble-strewn verandah"
     (south twisted-railings)
     (east front-porch))

    (front-porch
     "front porch"
     (north front-lobby)
     (south iron-gate-path)
     (west verandah))

    (front-tower
     "front tower"
     (east sloping-corridor))

    (sloping-corridor
     "sloping corridor"
     (west front-tower)
     (east upper-gallery))

    (upper-gallery
     "upper gallery"
     (north cobwebby-room)
     (west sloping-corridor))

    (marsh-by-wall
     "marsh by wall"
     (south fallen-brickwork))

    (marsh
     "marsh"
     (south stone-arch)
     (west marsh-by-wall))

    (soggy-path
     "soggy path"
     (north cliff-path-3)
     (west marsh))

    (twisted-railings
     "by twisted railings"
     (north verandah)
     (east iron-gate-path))

    (iron-gate-path
     "path through iron gate"
     (north front-porch)
     (west twisted-railings)
     (east railings))

    (railings
     "by railings"
     (west iron-gate-path)
     (east beneath-front-tower))

    (beneath-front-tower
     "beneath the front tower"
     (west railings)
     (east debris))

    (debris
     "debris from crumbling facade"
     (west beneath-front-tower)
     (east fallen-brickwork))

    (fallen-brickwork
     "large fallen brickwork"
     (north marsh-by-wall)
     (west debris)
     (east stone-arch))

    (stone-arch
     "rotten stone arch"
     (north marsh)
     (west fallen-brickwork)
     (east crumbling-clifftop))

    (crumbling-clifftop
     "crumbling clifftop"
     (west stone-arch))

    ;; NOTE: connected to barred-cellar after digging the bars
    (hole-in-wall
     "hole in the wall"
     (north slippery-steps)
     (south coffin-cellar)
     (east cliff-path-1))

    ;; NOTE: connected to study after swinging the axe
    (study-secret-room
     "study with secret room"
     (north secret-room)
     (west evil-library))

    ;; NOTE: connected to thick-door after unlocking the door
    (huge-open-door
     "Huge open door"
     (south marble-stairs)
     (west vaulted-hallway)
     (east trophy-room))))

;; location functions
(defun location-name (loc)
  (cadr (assoc loc *nodes*)))

(defun location-edges (loc)
  (cddr (assoc loc *nodes*)))

;; items
(defstruct item name location value)
(defun item-in-backpack-p (item)
  (eq (item-location item) 'backpack))

;; functions to find items
(defun find-item (id)
  (cdr (assoc id *items*)))

(defun find-item-by-name (name)
  (labels ((find-by-name (lst)
             (cond ((null lst) lst)
                   ((string= (item-name (cdar lst)) name)
                    (cdar lst))
                   (t
                    (find-by-name (cdr lst))))))
    (find-by-name *items*)))

(defun find-items-by-location (location)
  (remove-if (lambda (item)
               (not (eq (item-location item) location)))
             (mapcar #'cdr *items*)))

;; macro to conveniently represent items
(defmacro new-item (id name location value)
  `(cons ',id (make-item :name ,name :location ',location :value ,value)))

(defparameter *items*
  (list (new-item painting "painting" spooky-room nil)
        (new-item ring "ring" coffin-cellar t)
        (new-item spells "magic spells" secret-room nil)
        (new-item goblet "goblet" front-tower nil)
        (new-item scroll "scroll" rear-turret-room nil)
        (new-item coins "coins" dark-alcove nil)
        (new-item statue "statue" thick-door nil)
        (new-item candlestick "candlestick" evil-library nil)
        (new-item matches "matches" kitchen nil)
        (new-item vacuum "vacuum" gloomy-passage nil)
        (new-item batteries "batteries" pool-of-light nil)
        (new-item shovel "shovel" weedpatch nil)
        (new-item axe "axe" large-woodpile nil)
        (new-item rope "rope" blasted-tree nil)
        (new-item boat "boat" cliff-path-3 nil)
        (new-item aerosol "aerosol" debris nil)
        (new-item candle "candle" study t)
        (new-item key "key" cupboard t)

        ;; fake items
        (new-item north "north" nil nil)
        (new-item south "south" nil nil)
        (new-item west "west" nil nil)
        (new-item east "east" nil nil)
        (new-item up "up" nil nil)
        (new-item down "down" nil nil)
        (new-item door "door" nil nil)
        (new-item bats "bats" nil nil)
        (new-item ghosts "ghosts" nil nil)
        (new-item drawer "drawer" nil nil)
        (new-item desk "desk" nil nil)
        (new-item coat "coat" nil nil)
        (new-item rubbish "rubbish" nil nil)
        (new-item coffin "coffin" nil nil)
        (new-item books "books" nil nil)
        (new-item xzanfar "xzanfar" nil nil)
        (new-item wall "wall" nil nil)))

(defparameter *current-location* 'iron-gate-path)
(defparameter *light-on* nil)
(defparameter *light-time* 60)
(defparameter *exit* nil)

(defun handle-help (loc verb rest)
  (format nil "Words I know:~%~{~a~^~%~}"
          (mapcar #'car *allowed-commands*)))

(defun handle-inventory (loc verb rest)
  (let ((items (mapcar #'item-name (find-items-by-location 'backpack))))
    (cond ((null items)
           "You are carrying nothing.")
          (t
           (format nil "You are carrying: ~{~a~^, ~}." items)))))

(defun handle-go (loc verb rest)
  (let ((next (assoc verb (location-edges loc))))
    (cond ((and (eq *current-location* 'blasted-tree)
                (item-value (find-item 'rope)))
           (setf (item-value (find-item 'rope)) nil)
           "CRASH!!! You fell out of the tree.")
          ((and (eq *current-location* 'cobwebby-room)
                (item-value (find-item 'ghosts)))
           "Ghosts will not let you move!")
          ((and (eq *current-location* 'cold-chamber)
                (item-in-backpack-p (find-item 'painting))
                (item-value (find-item 'xzanfar)))
           "A magical barrier to the west.")
          ((and (eq *current-location* 'pool-of-light)
                (not *light-on*)
                (or (eq verb 'north)
                    (eq verb 'east)))
           "You need a light.")
          ((and (eq *current-location* 'marsh)
                (not (item-in-backpack-p (find-item 'boat))))
           "You are stuck.")
          ((and (not (or (eq *current-location* 'cliff-path-3)
                         (eq *current-location* 'marsh-by-wall)
                         (eq *current-location* 'marsh)
                         (eq *current-location* 'soggy-path)))
                (item-in-backpack-p (find-item 'boat)))
           "You can't carry the boat.")
          ((and (or (eq *current-location* 'vaulted-hallway)
                    (eq *current-location* 'thick-door)
                    (eq *current-location* 'trophy-room))
                (not *light-on*))
           "Too dark to move.")
          (next
           (setf *current-location* (edge-location next))
           "OK")
          (t
           "You can't go that way."))))

(defun handle-get (loc verb rest)
  (let ((item (find-item-by-name rest)))
    (cond ((or (null item)
               (item-value item))
           (format nil "What ~a?" rest))
          ((item-in-backpack-p item)
           "You already have it.")
          ((not (eq (item-location item) *current-location*))
           "It isn't here.")
          (t
           (setf (item-location item) 'backpack)
           (format nil "You have the ~a." rest)))))

(defun handle-open (loc verb rest)
  (cond ((and (eq *current-location* 'study)
              (or (string= rest "drawer")
                  (string= rest "desk")))
         (setf (item-value (find-item 'candle)) nil)
         "Drawer open")
        ((and (eq *current-location* 'thick-door)
              (string= rest "door"))
         "It's locked")
        ((and (eq *current-location* 'coffin-cellar)
              (string= rest "coffin"))
         (setf (item-value (find-item 'ring)) nil)
         "That's creepy")
        (t
         "You can't open it.")))

(defun handle-examine (loc verb rest)
  (cond ((and (eq *current-location* 'cupboard)
              (string= rest "coat"))
         (setf (item-value (find-item 'key)) nil)
         "There is something here!")
        ((and (eq *current-location* 'yard-by-rubbish)
              (string= rest "rubbish"))
         "That's disgusting.")
        ((and (eq *current-location* 'study)
              (or (string= rest "drawer")
                  (string= rest "desk")))
         "There is a drawer.")
        ((and (eq *current-location* 'study)
              (string= rest "wall"))
         "There is something beyond...")
        ((or (string= rest "books")
             (string= rest "scroll"))
         (handle-read loc verb rest))
        ((string= rest "coffin")
         (handle-open loc verb rest))
        (t
         "You cannot examine that.")))

(defun handle-read (loc verb rest)
  (cond ((and (eq *current-location* 'evil-library)
              (string= rest "books"))
         "They are demonic works.")
        ((and (or (string= rest "spells")
                  (string= rest "magic spells"))
              (item-in-backpack-p (find-item 'spells))
              (not (item-value (find-item 'xzanfar))))
         "Say this word with care 'xzanfar'.")
        ((and (string= rest "scroll")
              (item-in-backpack-p (find-item 'scroll)))
         "The script is in an alien tongue.")
        (t
         "You cannot read it.")))

(defun random-choice (lst)
  (nth (random (length lst)) lst))

(defun handle-say (loc verb rest)
  (cond ((or (not (string= rest "xzanfar"))
             (not (item-in-backpack-p (find-item 'spells))))
         (format nil "Ok, '~a'." rest))
        (t
         (if (not (eq *current-location* 'cold-chamber))
             (setf *current-location* (random-choice (mapcar #'car *locations*)))
             (setf (item-value (find-item 'xzanfar)) t))
         "*** Magic Occurs ***")))

(defun handle-dig (loc verb rest)
  (let ((shovel (find-item 'shovel)))
    (cond ((and (eq loc 'barred-cellar)
                (item-in-backpack-p shovel))
           (add-location 'barred-cellar
                         (find-location 'hole-in-wall))
           "Dug the bars out.")
          (shovel
           "You made a hole.")
          (t
           "You can't dig with bare hands."))))

(defun handle-swing (loc verb rest)
  (cond ((and (eq loc 'study)
              (string= rest "axe")
              (item-in-backpack-p (find-item 'axe)))
         (add-location 'study
                       (find-location 'study-secret-room))
         "You broke the thin wall.")
        ((string= rest "rope")
         (let ((rope (find-item 'rope)))
           (cond ((item-in-backpack-p rope)
                  "You swung it")
                 ((eq *current-location* 'blasted-tree)
                  "This is no time to play games.")
                 (t
                  "No rope to swing."))))
        ((string= rest "axe")
         (cond ((item-in-backpack-p (find-item 'axe))
                "WHOOSH!")
               (t
                "No axe to swing.")))
        (t nil)))

(defun handle-climb (loc verb rest)
  (let ((rope (find-item 'rope)))
    (cond ((not (string= rest "rope"))
           "You can't do that.")
          ((item-in-backpack-p rope)
           "It isn't attached to anything!")
          ((item-value rope)
           (setf (item-value rope) nil)
           "Going down.")
          (t
           (setf (item-value rope) t)
           "You see a thick forest and a cliff at south."))))

(defun handle-light (loc verb rest)
  (cond ((not (string= rest "candle"))
         "Light what?")
        ((not (item-in-backpack-p (find-item 'candle)))
         "No candle to light.")
        ((not (item-in-backpack-p (find-item 'matches)))
         "Nothing to light it with.")
        ((not (item-in-backpack-p (find-item 'candlestick)))
         "It would burn your hands!")
        (t
         (setf *light-on* t)
         "It casts a flickering light.")))

(defun handle-unlight (loc verb rest)
  (cond (*light-on*
         (setf *light-on* nil)
         "Extinguished.")
        (t
         "The Light is already off.")))

(defun handle-spray (loc verb rest)
  (let ((bats (find-item 'bats)))
    (cond ((not (item-in-backpack-p (find-item 'aerosol)))
           "You can't spray anthing.")
          ((and (not (string= rest "bats"))
                (not (item-value bats)))
           "HISS...")
          (t
           (setf (item-value bats) nil)
           "Pfft! Got them."))))

(defun handle-use (loc verb rest)
  (cond ((and (string= rest "vacuum")
              (item-in-backpack-p (find-item 'vacuum)))
         (let ((ghosts (find-item 'ghosts)))
           (cond ((not (item-in-backpack-p (find-item 'batteries)))
                  "No batteries for the vacuum.")
                 ((and (eq *current-location* 'cobwebby-room)
                       (item-value ghosts))
                  (setf (item-value ghosts) nil)
                  (setf (item-value (find-item 'down)) t)
                  "WHIZZ - Vacuumed the ghosts up!")
                 (t
                  "Nothing to vacuum."))))
        (t nil)))

(defun handle-unlock (loc verb rest)
  (cond ((and (eq *current-location* 'thick-door)
              (string= rest "door")
              (item-in-backpack-p (find-item 'key)))
         (add-location 'thick-door (find-location 'huge-open-door))
         "The key turns!")
        (t
         "Nothing to unlock.")))

(defun handle-drop (loc verb rest)
  (let ((item (find-item-by-name rest)))
    (cond ((not item)
           "Cannot find it.")
          ((item-in-backpack-p item)
           (setf (item-location item) *current-location*)
           "Done.")
          (t
           "You don't carry that object."))))

(defun handle-score (loc verb rest)
  (let ((score (length (find-items-by-location 'backpack))))
    (cond ((< score 17)
           (format nil "Your score is ~a." score))
          ((eq *current-location* 'iron-gate-path)
           (setf *exit* t)
           (format nil "Double score for reaching here!~%Your score is ~a." (* 2 score)))
          (t
           (format nil "You have everything~%Return to the gate for the final score.")))))

(defun handle-exit (loc verb rest)
  (setf *exit* t)
  "Bye!")

;; allowed commands
(defparameter *allowed-commands*
  `((help ,#'handle-help)
    (inventory ,#'handle-inventory)
    (north ,#'handle-go)
    (south ,#'handle-go)
    (west ,#'handle-go)
    (east ,#'handle-go)
    (up ,#'handle-go)
    (down ,#'handle-go)
    (get ,#'handle-get)
    (open ,#'handle-open)
    (examine ,#'handle-examine)
    (read ,#'handle-read)
    (say ,#'handle-say)
    (dig ,#'handle-dig)
    (swing ,#'handle-swing)
    (climb ,#'handle-climb)
    (light ,#'handle-light)
    (unlight ,#'handle-unlight)
    (spray ,#'handle-spray)
    (use ,#'handle-use)
    (unlock ,#'handle-unlock)
    (drop ,#'handle-drop)
    (score ,#'handle-score)
    (exit ,#'handle-exit)))

(defparameter *verb-aliases*
  '((n north)
    (s south)
    (w west)
    (e east)
    (u up)
    (d down)
    (i inventory)
    (carrying inventory)
    (take get)
    (quit exit)
    (leave drop)))

(defun get-handler (verb commands)
  (cadr (assoc verb commands)))

(defun whitespace-char-p (x)
  (or (char= #\space x)
      (not (graphic-char-p x))))

(defun string-split (string delimp)
  "Split a string in words delimited by the predicate delimp"
  (let ((len (length string)))
    (labels ((maybe-add (a b parts)
               (cond ((= a b) parts)
                     (t
                      (cons (subseq string a b)
                            parts))))
             (tokens (a b parts)
               (cond ((= b len)
                      (reverse (maybe-add a b parts)))
                     ((funcall delimp (char string b))
                      (tokens (1+ b) (1+ b)
                              (maybe-add a b parts)))
                     (t
                      (tokens a (1+ b) parts)))))
      (tokens 0 0 nil))))

(defun make-query (verb words)
  (cons verb words))
(defun query-verb (query)
  (car query))
(defun query-rest (query)
  (cdr query))

(defun resolve-alias (verb)
  (let ((alias (assoc verb *verb-aliases*)))
    (cond ((not alias) verb)
          (t
           (resolve-alias (cadr alias))))))

(defun parse-query (string)
  (let ((len (length string)))
    (if (and (> len 0)
             (equal (char string 0) #\'))
        ;; NOTE: special handling for 'words -> 'say words
        (make-query 'say (subseq string 1 len))
        (labels ((parse-list (slist)
                   (cond ((null slist)
                          (make-query nil ""))
                         ;; NOTE: special handling of the go command: it's
                         ;; basically ignored and the next token is used as a
                         ;; verb. So go north becomes simply north.
                         ((and (string= (car slist) "go")
                               (not (null (cdr slist))))
                          (parse-list (cdr slist)))
                         (t
                          (make-query (resolve-alias (intern (string-upcase (car slist))))
                                      (format nil "~{~a~^ ~}" (cdr slist)))))))
          (parse-list (string-split string #'whitespace-char-p))))))

(defun game-read ()
  (let ((string (read-line *standard-input* nil)))
    (if string
        (parse-query string)
        (make-query 'exit ""))))

(defun game-loop ()
  (unless *exit*
    (let ((loc *current-location*))
      ;; PRINT
      ;; location description
      (format t "Your location: ~a.~%" (location-name loc))
      (format t "Exits: ~{~a~^, ~}.~%" (mapcar (lambda (edge)
                                                 (string-downcase
                                                  (prin1-to-string
                                                   (edge-direction edge))))
                                               (location-edges loc)))

      ;; visible items
      (let ((items (remove-if (lambda (item)
                                (item-value item))
                              (find-items-by-location loc))))
        (when (not (null items))
          (format t "You see: ~{~a~^, ~}.~%" (mapcar #'item-name items))))

      ;; candle status
      (when *light-on*
        (when (= *light-time* 10)
          (format t "Your candle is waning!~%"))
        (when (= *light-time* 1)
          (format t "Your candle is out!~%")))

      ;; READ
      (princ ">>> ")
      (finish-output)
      (let* ((query (game-read))
             (verb (query-verb query))
             (rest (query-rest query))
             (handler (get-handler verb *allowed-commands*)))

        ;; EVAL
        (cond ((not verb))
              ((not handler)
               (format t "WARNING: no handler for ~a ~a~%" verb rest))
              ((and (eq *current-location* 'rear-turret-room)
                    (not (item-value (find-item 'bats)))
                    (not (eq verb 'use))
                    (= (random 3) 0))
               ;; NOTE: bats prevent any command but 'use
               (format t "Bats attacking!~%"))
              (t
               ;; set ghosts with a 50% change in cobwebby-room unless
               ;; they have been vacuumed already
               (when (and (eq *current-location* 'cobwebby-room)
                          (not (item-value (find-item 'down)))
                          (= (random 2) 0))
                 (setf (item-value (find-item 'ghosts)) t))

               ;; update the light
               (when *light-on*
                 (setf *light-time* (1- *light-time*))
                 (when (= *light-time* 0)
                   (setf *light-on* nil)))

               ;; execute the command
               (princ (or (funcall handler loc verb rest)
                          "You can't do that."))
               (fresh-line)))))

    ;; LOOP
    (game-loop)))

(format t "Haunted House~%")
(format t "-------------~%")
(game-loop)
