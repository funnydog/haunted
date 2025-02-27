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
(defvar *candle-lit*       nil "Candle is lit")
(defvar *candle-time*      nil "Light time left")
(defvar *top-of-tree*      nil "At the top of the tree")
(defvar *bats-present*     nil "Bats present")
(defvar *ghosts-appear*    nil "Ghosts appear")
(defvar *vacuum-on*        nil "Vacuum is switched on")
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

    (front-porch
     "front porch"
     (north front-lobby)
     (south iron-gate-path)
     (west verandah))

    ;; NOTE: replaces front-porch after entering the house
    (closed-front-porch
     "front porch"
     (south iron-gate-path)
     (west verandah))

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
  '((painting spooky-room)
    (ring coffin-cellar :hidden)
    (magic-spells secret-room)
    (goblet front-tower)
    (scroll rear-turret-room)
    (coins dark-alcove)
    (statue thick-door)
    (candlestick evil-library)
    (matches kitchen)
    (vacuum gloomy-passage)
    (batteries pool-of-light)
    (shovel weedpatch)
    (axe large-woodpile)
    (rope blasted-tree)
    (boat cliff-path-3)
    (aerosol debris)
    (candle study :hidden)
    (key cupboard :hidden)))

;; non-interactive items
(defparameter *words*
  '((painting "painting")
    (ring "ring")
    (magic-spells "magic spells")
    (goblet "goblet")
    (scroll "scroll")
    (coins "coins")
    (statue "statue")
    (candlestick "candlestick")
    (matches "matches")
    (vacuum "vacuum")
    (batteries "batteries")
    (shovel "shovel")
    (axe "axe")
    (rope "rope")
    (boat "boat")
    (aerosol "aerosol")
    (candle "candle")
    (key "key")
    (bats "bats")
    (books "books")
    (coat "coat")
    (coffin "coffin")
    (desk "desk")
    (door "door")
    (drawer "drawer")
    (ghosts "ghosts")
    (rubbish "rubbish")
    (spells "spells")
    (wall "wall")
    (xzanfar "xzanfar")))

;; verbs and aliases
(defparameter *verbs*
  '((climb "climb")
    (down "d")
    (dig "dig")
    (down "down")
    (drop "drop")
    (east "e")
    (east "east")
    (examine "ex")
    (examine "exa")
    (examine "examine")
    (exit "exit")
    (get "get")
    (help "help")
    (inventory "i")
    (inventory "inv")
    (inventory "inventory")
    (load "load")
    (light "light")
    (north "n")
    (north "north")
    (open "open")
    (read "read")
    (south "s")
    (save "save")
    (say "say")
    (score "score")
    (spray "spray")
    (swing "swing")
    (up "u")
    (unlight "unlight")
    (unlock "unlock")
    (up "up")
    (use "use")
    (west "w")
    (west "west")
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
  (cadr (assoc item *words*)))

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

(defun itemp (id)
  (assoc id *items*))

(defun items-at (loc)
  (remove-if-not (lambda (item)
                   (eq (item-location item) loc))
                 (mapcar #'car *items*)))

;; string functions
(defun search-string (str lst)
  (cond ((null lst) nil)
        ((string= (cadar lst) str)
         (caar lst))
        (t
         (search-string str (cdr lst)))))

(defun find-word (str)
  "Search a string among the items and the recognized words"
  (search-string str *words*))

(defun find-verb (str)
  "Find a string among the recognized verbs"
  (search-string str *verbs*))

(defun random-choice (lst)
  "Choose a random element of lst"
  (nth (random (length lst)) lst))

;; verb handlers
(defun handle-help (loc word wordstr default)
  ;; TODO: make a meaningful simple help instead of iterating words
  (format nil "Words I know: ~{~a~^, ~}."
          (mapcar #'cadr *verbs*)))

(defun handle-inventory (loc word wordstr default)
  (let ((items (mapcar #'item-name (items-at 'backpack))))
    (if (null items)
        "You are carrying nothing."
        (format nil "You are carrying ~{~a~^, ~}." items))))

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
                (member dir '(north east)))
           "You need a light.")
          ((and (eq loc 'marsh)
                (not (item-in-backpack 'boat)))
           "You are stuck.")
          ((and (not (member loc '(cliff-path-3 marsh-by-wall marsh soggy-path)))
                (item-in-backpack 'boat))
           "You can't carry the boat.")
          ((and (member loc '(vaulted-hallway thick-door trophy-room))
                (not *candle-lit*))
           "Too dark to move.")
          ((and (eq next 'front-lobby)
                (eq dir 'north))
           (setf *current-location* next)
           (replace-location 'front-porch 'closed-front-porch)
           "The door slams shut!")
          (next
           (setf *current-location* next)
           "OK!")
          (t
           "You can't go that way."))))

(defun handle-get (loc item wordstr default)
  (cond ((or (not (itemp item))
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
  (cond ((and (eq loc 'cupboard)
              (eq word 'coat))
         (set-item-hidden 'key nil)
         "Something here!")
        ((and (eq loc 'yard-by-rubbish)
              (eq word 'rubbish))
         "That's disgusting.")
        ((and (eq loc 'study)
              (member word '(drawer desk)))
         "There is a drawer.")
        ((and (eq loc 'study)
              (eq word 'wall))
         "There is something beyond...")
        ((member word '(books scroll spells magic-spells))
         (handle-read loc word wordstr default))
        ((eq word 'coffin)
         (handle-open loc word wordstr default))
        (t
         default)))

(defun handle-read (loc word wordstr default)
  (cond ((and (eq loc 'evil-library)
              (eq word 'books))
         "They are demonic works.")
        ((and (member word '(spells magic-spells))
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
              (item-in-backpack 'shovel)
              (not (member 'barred-cellar *player-nodes*)))
         (replace-location 'barred-cellar 'hole-in-wall)
         "Dug the bars out.")
        ((item-in-backpack 'shovel)
         "You made a hole.")
        (t
         default)))

(defun handle-swing (loc item itemstr default)
  (cond ((and (eq loc 'blasted-tree)
              (not (item-in-backpack 'rope)))
         "This is no time to play games.")
        ((and (eq item 'rope)
              (item-in-backpack item))
         "You swung it.")
        ((and (eq loc 'study)
              (eq item 'axe)
              (item-in-backpack item)
              (not (member 'study *player-nodes*)))
         (replace-location 'study 'study-secret-room)
         "You broke the thin wall.")
        ((and (eq item 'axe)
              (item-in-backpack 'item))
         "WHOOSH!")
        (t
         default)))

(defun handle-climb (loc word wordstr default)
  (cond ((and (eq word 'rope)
              (item-in-backpack word))
         "It isn't attached to anything!")
        ((and (eq word 'rope)
              (not (item-in-backpack word))
              (eq loc 'blasted-tree)
              (not *top-of-tree*))
         (setf *top-of-tree* t)
         "You see a thick forest and a cliff at south.")
        ((and (eq word 'rope)
              (not (item-in-backpack word))
              (eq loc 'blasted-tree)
              *top-of-tree*)
         (setf *top-of-tree* nil)
         "Going down!")
        (t
         default)))

(defun handle-light (loc item itemstr default)
  (cond ((or (not (eq item 'candle))
             (not (item-in-backpack 'candle)))
         default)
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
  (cond ((and (eq word 'aerosol)
              (item-in-backpack word)
              *bats-present*)
         (setf *bats-present* nil)
         "PFFT! Got them.")
        ((and (eq? word 'aerosol)
              (item-in-backpack word))
         "HISSSS...")
        (t
         default)))

(defun handle-use (loc item itemstr default)
  (cond ((and (eq item 'vacuum)
              (item-in-backpack 'vacuum)
              (item-in-backpack 'batteries))
         (setf *vacuum-on* t)
         (cond (*ghosts-appear*
                (setf *ghosts-appear* nil)
                "WHIZZ - Vacuumed the ghosts up!")
               (t
                "Switched on.")))
        (t
         default)))

(defun handle-unlock (loc word wordstr default)
  (cond ((and (eq loc 'study)
              (member word '(drawer desk)))
         (handle-open loc word wordstr default))
        ((and (eq loc 'thick-door)
              (eq word 'door)
              (item-in-backpack 'key)
              (member 'thick-door *player-nodes*))
         (replace-location 'thick-door 'huge-open-door)
         "The key turns!")
        (t
         default)))

(defun handle-drop (loc item itemstr default)
  (cond ((item-in-backpack item)
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
             (remove-if-not #'cdr alist)))
         (writeln (sexp)
           (write sexp)
           (terpri)))
    (handler-case
        (with-open-file (*standard-output* name :direction :output :if-exists :supersede)
          (writeln (hash-to-alist *item-locations*))
          (writeln (hash-to-alist *item-hidden*))
          (writeln *player-nodes*)
          (writeln *current-location*)
          (writeln *candle-lit*)
          (writeln *candle-time*)
          (writeln *top-of-tree*)
          (writeln *bats-present*)
          (writeln *ghosts-appear*)
          (writeln *vacuum-on*)
          (writeln *spell-discovered*)
          (writeln *quest-won*)
          t)
      (error (c)
        (format t "An error occurred: ~a.~%" c)
        nil))))

(defun game-load (name)
  "Load the game state from a file"
  (flet ((alist-to-hash (alist hash)
           (clrhash hash)
           (mapc (lambda (keyval)
                   (setf (gethash (car keyval) hash) (cdr keyval)))
                 alist)))
    (handler-case
        (with-open-file (*standard-input* name :direction :input)
          (alist-to-hash (read) *item-locations*)
          (alist-to-hash (read) *item-hidden*)
          (setf *player-nodes* (read))
          (setf *current-location* (read))
          (setf *candle-lit* (read))
          (setf *candle-time* (read))
          (setf *top-of-tree* (read))
          (setf *bats-present* (read))
          (setf *ghosts-appear* (read))
          (setf *vacuum-on* (read))
          (setf *spell-discovered* (read))
          (setf *quest-won* (read))
          t)
      (error (c)
        (format t "An error occurred: ~a.~%" c)
        nil))))

(defun game-init ()
  (setf *item-locations* (make-hash-table))
  (setf *item-hidden* (make-hash-table))
  (loop for (id location . flags) in *items* do
    (setf (gethash id *item-locations*) location)
    (when (member :hidden flags)
      (setf (gethash id *item-hidden*) t)))
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

         (terpri)

         (let* ((string (input "What will you do? "))
                (ws (position #\Space string))
                (verb (find-verb (if ws (subseq string 0 ws) string)))
                (row (assoc verb *allowed-commands*))
                (handler (cadr row))
                (flags (cddr row))
                (target (subseq string (if ws (1+ ws) (length string))))
                (word (or (find-word target)
                          (find-word (concatenate 'string target "s"))))
                (word (if (and (eq handler #'handle-go)
                               (not word))
                          verb
                          word)))
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
                  (format t "ERROR: missing handler for verb ~a~%" verb))
                 ((and (not verb) (not word))
                  (game-loop "You don't make sense."))
                 ((not handler)
                  (game-loop (format nil "You can't '~a'." string)))
                 ((and *bats-present*
                       (eq *current-location* 'rear-turret-room)
                       (not (= (random 3) 0))
                       (not (eq verb 'spray)))
                  (game-loop "Bats attacking!"))
                 (t
                  ;; set ghosts with a 50% chance in cobwebby-room
                  (when (and (eq *current-location* 'cobwebby-room)
                             (= (random 2) 0)
                             (not *vacuum-on*))
                    (setf *ghosts-appear* t))

                  ;; update the light
                  (when *candle-lit*
                    (setf *candle-time* (1- *candle-time*))
                    (when (zerop *candle-time*)
                      (setf *candle-lit* nil)))

                  ;; execute the handler
                  (let* ((default (cond ((not (item-in-backpack word))
                                         (format nil "You don't have '~a'." target))
                                        ((string= target "")
                                         "I need two words.")
                                        ((not word)
                                         "That's silly.")
                                        (t
                                         "What.")))
                         (message (funcall handler *current-location* word target default)))
                    (game-loop (cond ((= *candle-time* 10)
                                      "Your candle is waning!")
                                     ((= *candle-time* 1)
                                      "Your candle is out!")
                                     (t
                                      message))))))))))

(defun game-start ()
  (game-init)
  (game-loop "Good luck on your quest!"))
