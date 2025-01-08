;; Haunted House - Common Lisp version
;; adapted from "Write your own adventure programs" by Usborne.

;; program variables
(defvar *allowed-commands* nil "Allowed commands")
(defvar *nodes*            nil "Visitable locations")
(defvar *items*            nil "Interactive items")
(defvar *words*            nil "Recognized words")
(defvar *verbs*            nil "Recognized verbs")

;; player's variables
(defvar *item-locations*   nil "Location of the item")
(defvar *item-hidden*      nil "Hidden status for the item")
(defvar *player-nodes*     nil "Additional locations")
(defvar *current-location* nil "Location of the player")
(defvar *light-on*         nil "Light is on")
(defvar *light-time*       nil "Light time left")
(defvar *top-of-tree*      nil "At the top of the tree")
(defvar *bats-active*      nil "Bats active")
(defvar *ghosts-appear*    nil "Ghosts appear")
(defvar *ghosts-vacuumed*  nil "Ghosts vacuumed")
(defvar *spell-discovered* nil "Spell discovered")
(defvar *quest-won*        nil "Quest won")

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

;; interactive items
;; the player can get them and move them
(defparameter *items*
  '((painting "painting" spooky-room)
    (ring "ring" coffin-cellar :hidden)
    (magic-spells "magic spells" secret-room)
    (goblet "goblet" front-tower)
    (scroll "scroll" rear-turret-room)
    (coins "coins" dark-alcove)
    (statue "statue" thick-door)
    (candlestick "candlestick" evil-library)
    (matches "matches" kitchen)
    (vacuum "vacuum" gloomy-passage)
    (batteries "batteries" pool-of-light)
    (shovel "shovel" weedpatch)
    (axe "axe" large-woodpile)
    (rope "rope" blasted-tree)
    (boat "boat" cliff-path-3)
    (aerosol "aerosol" debris)
    (candle "candle" study :hidden)
    (key "key" cupboard :hidden)))

;; non-interactive items
(defparameter *words*
  '(("bats" bats)
    ("books" books)
    ("coat" coat)
    ("coffin" coffin)
    ("desk" desk)
    ("door" door)
    ("drawer" drawer)
    ("ghosts" ghosts)
    ("rubbish" rubbish)
    ("spells" spells)
    ("wall" wall)
    ("xzanfar" xzanfar)))

;; verbs and aliases
(defparameter *verbs*
  '(("climb" climb)
    ("d" down)
    ("dig" dig)
    ("down" down)
    ("drop" drop)
    ("e" east)
    ("east" east)
    ("ex" examine)
    ("exa" examine)
    ("examine" examine)
    ("exit" exit)
    ("get" get)
    ("help" help)
    ("i" inventory)
    ("inv" inventory)
    ("inventory" inventory)
    ("load" load)
    ("light" light)
    ("n" north)
    ("north" north)
    ("open" open)
    ("read" read)
    ("s" south)
    ("save" save)
    ("say" say)
    ("score" score)
    ("spray" spray)
    ("swing" swing)
    ("u" up)
    ("unlight" unlight)
    ("unlock" unlock)
    ("up" up)
    ("use" use)
    ("w" west)
    ("west" west)
  ))

;; location functions
(defun find-location (loc)
  (or (assoc loc *player-nodes*)
      (assoc loc *nodes*)))

(defun location-name (loc)
  (cadr (find-location loc)))

(defun location-edges (loc)
  (cddr (find-location loc)))

(defun replace-location (old new)
  (setf *player-nodes* (acons old (cdr (find-location new)) *player-nodes*)))

;; item functions
(defun item-name (item)
  (cadr (assoc item *items*)))

(defun item-location (item)
  (gethash item *item-locations*))

(defun set-item-location (item location)
  (setf (gethash item *item-locations*) location))

(defun item-hidden (item)
  (gethash item *item-hidden*))

(defun set-item-hidden (item value)
  (setf (gethash item *item-hidden*) value))

(defun item-in-backpack (item)
  (eq (item-location item) 'backpack))

(defun items-at (loc)
  (remove-if-not (lambda (item)
                   (eq (item-location item) loc))
                 (mapcar #'car *items*)))

;; misc functions
(defun find-word (str)
  "Search a string among the items and the recognized words"
  (labels ((find-item (lst)
             (cond ((null lst) lst)
                   ((string= (cadar lst) str)
                    (caar lst))
                   (t
                    (find-item (cdr lst))))))
    (or (find-item *items*)
        (cadr (assoc str *words* :test #'string=)))))

(defun is-item (sym)
  "Check if a symbol is an item"
  (assoc sym *items*))

(defun find-verb (str)
  "Find a string among the recognized verbs"
  (cadr (assoc str *verbs* :test #'string=)))

(defun random-choice (lst)
  (nth (random (length lst)) lst))

;; verb handlers
(defun handle-help (loc word wordstr default)
  ;; TODO: make a meaningful simple help instead of iterating words
  (format nil "Words I know:~%~{~a~^~%~}"
          (mapcar #'car *allowed-commands*)))

(defun handle-inventory (loc word wordstr default)
  (let ((items (mapcar #'item-name (items-at 'backpack))))
    (format nil "You are carrying ~:[nothing~;~{~a~^, ~}~]." items items)))

(defun handle-go (loc dir wordstr default)
  (let ((next (cadr (assoc dir (location-edges loc)))))
    (cond ((and (eq loc 'blasted-tree)
                *top-of-tree*)
           (setf *top-of-tree* nil)
           "CRASH!!! You fell out of the tree.")
          ((and (eq loc 'cobwebby-room)
                *ghosts-appear*)
           "Ghosts will not let you move!")
          ((and (eq loc 'cold-chamber)
                (eq dir 'west)
                (item-in-backpack 'painting)
                *spell-discovered*)
           "A magical barrier to the west.")
          ((and (eq loc 'pool-of-light)
                (not *candle-lit*)
                (or (eq dir 'north)
                    (eq dir 'east)))
           "You need a light.")
          ((and (eq loc 'marsh)
                (not (item-in-backpack 'boat)))
           "You are stuck.")
          ((and (not (find loc '(cliff-path-3 marsh-by-wall marsh soggy-path)))
                (item-in-backpack 'boat))
           "You can't carry the boat.")
          ((and (find loc '(vaulted-hallway thick-door trophy-room))
                (not *candle-lit*))
           "Too dark to move.")
          (next
           (setf *current-location* next)
           "OK!")
          (t
           "You can't go that way."))))

(defun handle-get (loc item wordstr default)
  (cond ((or (not (is-item item))
             (item-hidden item))
         (format nil "What ~a?" wordstr))
        ((item-in-backpack item)
         "You already have it.")
        ((eq (item-location item) loc)
         (set-item-location item 'backpack)
         (format nil "You have the ~a." (item-name item)))
        (t
         "It isn't here.")))

(defun handle-open (loc word wordstr default)
  (cond ((and (eq loc 'study)
              (find word '(drawer desk)))
         (set-item-hidden 'candle nil)
         "Drawer open.")
        ((and (eq loc 'thick-door)
              (eq word 'door))
         "It's locked.")
        ((and (eq loc 'coffin-cellar)
              (eq word 'coffin))
         (set-item-hidden 'ring nil)
         "That's creepy.")
        (t
         default)))

(defun handle-examine (loc word wordstr default)
  (cond ((not word)
         "You see what you might expect!")
        ((and (eq loc 'cupboard)
              (eq word 'coat))
         (set-item-hidden 'key nil)
         "There is something here!")
        ((and (eq loc 'yard-by-rubbish)
              (eq word 'rubbish))
         "That's disgusting.")
        ((and (eq loc 'study)
              (find word '(drawer desk)))
         "There is a drawer.")
        ((and (eq loc 'study)
              (eq word 'wall))
         "There is something beyond...")
        ((find word '(books scroll spells magic-spells))
         (handle-read loc word wordstr default))
        ((eq word 'coffin)
         (handle-open loc word wordstr default))
        (t
         "Nothing special.")))

(defun handle-read (loc word wordstr default)
  (cond ((and (eq loc 'evil-library)
              (eq word 'books))
         "They are demonic works.")
        ((and (find word '(spells magic-spells))
              (item-in-backpack 'magic-spells)
              (not *spell-discovered*))
         (setf *spell-discovered* t)
         "Say this word with care 'xzanfar'.")
        ((eq word 'scroll)
         "The script is in an alien tongue.")
        (t
         default)))

(defun handle-say (loc word wordstr default)
  (cond ((or (not (eq word 'xzanfar))
             (not *spell-discovered*))
         (format nil "Ok, '~a'." wordstr))
        (t
         (when (not (eq loc 'cold-chamber))
           (setf *current-location* (random-choice (mapcar #'car *nodes*))))
         "*** Magic Occurs ***")))

(defun handle-dig (loc word wordstr default)
  (cond ((and (eq loc 'barred-cellar)
              (item-in-backpack 'shovel))
         (replace-location 'barred-cellar 'hole-in-wall)
         "Dug the bars out.")
        ((item-in-backpack 'shovel)
         "You made a hole.")
        (t
         default)))

(defun handle-swing (loc item itemstr default)
  (cond ((and (eq loc 'study)
              (eq item 'axe)
              (item-in-backpack item))
         (replace-location 'study 'study-secret-room)
         "You broke the thin wall.")
        ((and (eq item 'axe)
              (item-in-backpack 'item))
         "WHOOSH!")
        ((and (eq item 'rope)
              (item-in-backpack item))
         "You swung it.")
        ((and (eq loc 'blasted-tree)
              (eq item 'rope)
              (eq (item-location item) loc))
         "This is no time to play games.")
        (t
         default)))

(defun handle-climb (loc word wordstr default)
  (cond ((not (eq word 'rope))
         default)
        ((item-in-backpack 'rope)
         "It isn't attached to anything!")
        (*top-of-tree*
         (setf *top-of-tree* nil)
         "Going down.")
        ((and (eq loc 'blasted-tree)
              (eq (item-location 'rope) loc))
         (setf *top-of-tree* t)
         "You see a thick forest and a cliff at south.")
        (t
         default)))

(defun handle-light (loc item itemstr default)
  (cond ((not (eq item 'candle))
         default)
        ((not (item-in-backpack 'candle))
         "No candle to light.")
        ((not (item-in-backpack 'matches))
         "Nothing to light it with.")
        ((not (item-in-backpack 'candlestick))
         "It would burn your hands!")
        (t
         (setf *candle-lit* t)
         "It casts a flickering light.")))

(defun handle-unlight (loc item itemstr default)
  (cond (*candle-lit*
         (setf *candle-lit* nil)
         "Extinguished.")
        (t
         default)))

(defun handle-spray (loc word wordstr default)
  (cond ((not (item-in-backpack 'aerosol))
         "You can't spray anthing.")
        ((and (not (eq word 'bats))
              (not *bats-present*))
         "HISS...")
        (t
         (setf *bats-present* nil)
         "Pfft! Got them.")))

(defun handle-use (loc item itemstr default)
  (cond ((eq item 'vacuum)
         (cond ((not (item-in-backpack 'batteries))
                "No batteries for the vacuum.")
               ((and (eq loc 'cobwebby-room)
                     *ghosts-appear*)
                (setf *ghosts-appear* nil)
                (setf *vacuum-on* t)
                "WHIZZ - Vacuumed the ghosts up!")
               (t
                default)))
        (t
         default)))

(defun handle-unlock (loc word wordstr default)
  (cond ((and (eq loc 'thick-door)
              (eq word 'door)
              (item-in-backpack 'key))
         (replace-location 'thick-door 'huge-open-door)
         "The key turns!")
        (t
         default)))

(defun handle-drop (loc item itemstr default)
  (cond (item
         (set-item-location item loc)
         "Done.")
        (t
         default)))

(defun handle-score (loc word wordstr default)
  (let ((score (length (items-at 'backpack))))
    (cond ((< score 17)
           (format nil "Your score is ~a." score))
          ((eq loc 'iron-gate-path)
           (setf *quest-won* t)
           (format nil "Double score for reaching here!~%Your score is ~a." (* 2 score)))
          (t
           (format nil "You have everything, return to the gate for the final score.")))))

;; allowed commands
;; flags:
;;   :not-in-backpack - callable if an item is not in pocket
(defparameter *allowed-commands*
  `((help ,#'handle-help)
    (inventory ,#'handle-inventory)
    (north ,#'handle-go)
    (south ,#'handle-go)
    (west ,#'handle-go)
    (east ,#'handle-go)
    (up ,#'handle-go)
    (down ,#'handle-go)
    (get ,#'handle-get :not-in-backpack)
    (open ,#'handle-open)
    (examine ,#'handle-examine)
    (read ,#'handle-read)
    (say ,#'handle-say)
    (dig ,#'handle-dig)
    (swing ,#'handle-swing :not-in-backpack)
    (climb ,#'handle-climb :not-in-backpack)
    (light ,#'handle-light)
    (unlight ,#'handle-unlight)
    (spray ,#'handle-spray)
    (use ,#'handle-use)
    (unlock ,#'handle-unlock)
    (drop ,#'handle-drop)
    (score ,#'handle-score)))

(defun input (prompt)
  (fresh-line)
  (princ prompt)
  (force-output)
  (read-line *standard-input* nil))

(defun game-save (name)
  "Save the game state to a file"
  (flet ((hash-to-alist (hash)
           (let ((alist nil))
             (maphash (lambda (key value)
                        (setf alist (acons key value alist)))
                      hash)
             alist)))
    (handler-case
        (with-open-file (out name :direction :output :if-exists :supersede)
          (let ((alist `((item-locations ,@(hash-to-alist *item-locations*))
                         (item-hidden ,@(hash-to-alist *item-hidden*))
                         (player-nodes ,@*player-nodes*)
                         (current-location ,*current-location*)
                         (candle-lit ,*candle-lit*)
                         (candle-time ,*candle-time*)
                         (top-of-tree ,*top-of-tree*)
                         (bats-present ,*bats-present*)
                         (ghosts-appear ,*ghosts-appear*)
                         (vacuum-on ,*vacuum-on*)
                         (spell-discovered ,*spell-discovered*)
                         (quest-won ,*quest-won*))))
            (write alist :stream out)
            t))
      (error (c)
        (declare (ignore c))
        nil))))

(defun game-load (name)
  "Load the game state from a file"
  (flet ((alist-to-hash (alist)
           (let ((hash (make-hash-table)))
             (mapc (lambda (keyval)
                     (setf (gethash (car keyval) hash) (cdr keyval)))
                   alist)
             hash)))
    (handler-case
        (with-open-file (in name :direction :input)
          (let ((alist (read in)))
            (setf *item-locations* (alist-to-hash (cdr (assoc 'item-locations alist))))
            (setf *item-hidden* (alist-to-hash (cdr (assoc 'item-hidden alist))))
            (setf *player-nodes* (cdr (assoc 'player-nodes alist)))
            (setf *current-location* (cadr (assoc 'current-location alist)))
            (setf *candle-lit* (cadr (assoc 'candle-lit alist)))
            (setf *candle-time* (cadr (assoc 'candle-time alist)))
            (setf *top-of-tree* (cadr (assoc 'top-of-tree alist)))
            (setf *bats-present* (cadr (assoc 'bats-present alist)))
            (setf *ghosts-appear* (cadr (assoc 'ghosts-appear alist)))
            (setf *vacuum-on* (cadr (assoc 'vacuum-on alist)))
            (setf *spell-discovered* (cadr (assoc 'spell-discovered alist)))
            (setf *quest-won* (cadr (assoc 'quest-won alist))))
          t)
      (error (c)
        (declare (ignore c))
        nil))))

(defun game-init ()
  (setf *item-locations* (make-hash-table))
  (setf *item-hidden* (make-hash-table))
  (mapc (lambda (item)
          (let ((id (car item))
                (location (caddr item))
                (flags (cdddr item)))
            (setf (gethash id *item-locations*) location)
            (when (find :hidden flags)
              (setf (gethash id *item-hidden*) t))))
        *items*)
  (setf *player-nodes* nil)
  (setf *current-location* 'iron-gate-path)
  (setf *candle-lit* nil)
  (setf *candle-time* 60)
  (setf *top-of-tree* nil)
  (setf *bats-present* nil)
  (setf *ghosts-appear* nil)
  (setf *vacuum-on* nil)
  (setf *spell-discovered* nil)
  (setf *quest-won* nil))

(defun game-loop (message)
  (cond (*quest-won*
         (princ "HOORRAAAY!") (terpri)
         (terpri)
         (terpri))
        ;; (*quest-failed*
        ;;  (princ ":(") (terpri))
        (t
         ;; banner
         (terpri)
         (terpri)
         (format t "~40:@<~a~>~%" "Haunted House")
         (princ "========================================")
         (terpri)
         (terpri)
         (format t "~a~%Your location: ~a.~%Exits: ~{~a~^, ~}.~%"
                 message
                 (location-name *current-location*)
                 (mapcar (lambda (couple)
                           (string-downcase
                            (prin1-to-string
                             (car couple))))
                         (location-edges *current-location*)))

         ;; visible items
         (let ((items (remove-if #'item-hidden (items-at *current-location*))))
           (when items
             (format t "You see: ~{~a~^, ~}.~%" (mapcar #'item-name items))))

         ;; update the candle status
         (when *candle-lit*
           (when (= *candle-time* 10)
             (format t "Your candle is waning!~%"))
           (when (= *candle-time* 1)
             (format t "Your candle is out!~%")))

         (terpri)

         (let* ((string (input "What will you do? "))
                (len (length string))
                (ws (position #\Space string))
                (verb (find-verb (subseq string 0 (or ws len))))
                (row (assoc verb *allowed-commands*))
                (handler (cadr row))
                (flags (cddr row))
                (target (subseq string (if ws (1+ ws) len)))
                (word (if (eq handler #'handle-go)
                          verb
                          (or (find-word target)
                              (find-word (concatenate 'string target "s"))))))
           (cond ((eq verb 'exit)
                  (princ "Bye...")
                  (terpri))
                 ((eq verb 'save)
                  (let ((filename (input "Please enter the filename: ")))
                    (game-loop (if (game-save filename)
                                   "Ok, carry on."
                                   "Cannot save the game."))))
                 ((eq verb 'load)
                  (let ((filename (input "Please enter the filename: ")))
                    (game-loop (if (game-load filename)
                                   "Ok, carry on."
                                   "Cannot load the game."))))
                 ((and verb (not handler))
                  (format t "WARNING: missing handler for verb ~a~%" verb))
                 ((and (not verb)
                       (not word))
                  (game-loop (format nil "You cannot ~a." string)))
                 ((not handler)
                  (game-loop "Try something else."))
                 ((and (eq *current-location* 'rear-turret-room)
                       (not (find verb '(use spray)))
                       *bats-present*)
                  (game-loop "Bats attacking!"))
                 (t
                  ;; set bats with a 33% chance in the rear-turret-room
                  (when (and (eq *current-location* 'rear-turret-room)
                             (not *bats-present*)
                             (= (random 3) 0))
                    (setf *bats-present* t))

                  ;; set ghosts with a 50% chance in cobwebby-room unless
                  ;; they have been vacuumed already
                  (when (and (eq *current-location* 'cobwebby-room)
                             (not *vacuum-on*)
                             (= (random 2) 0))
                    (setf *ghosts-appear* t))

                  ;; update the light
                  (when *candle-lit*
                    (setf *candle-time* (1- *candle-time*))
                    (when (= *candle-time* 0)
                      (setf *candle-lit* nil)))

                  ;; execute the handler
                  (let ((default (if word "Pardon?" "I need two words.")))
                    (cond ((and (not (find :not-in-backpack flags))
                                (is-item word)
                                (not (item-in-backpack word)))
                           (game-loop (format nil "You do not have the ~a." target)))
                          (t
                           (game-loop (funcall handler *current-location* word target default)))))))))))

(defun game-start ()
  (game-init)
  (game-loop "Good luck on your quest!"))
