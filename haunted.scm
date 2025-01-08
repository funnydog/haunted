;; The Haunted House - scheme edition
;; Adapted from "Write your own adventure programs" by Usborne.

;; player's variables
(define *player-nodes*   '())
(define *item-locations* (make-eq-hashtable))
(define *item-hidden*    (make-eq-hashtable))
(define *current-location*)

;; game variables
(define *front-door-open*  #t)          ; the front door is open
(define *candle-lit*       #f)          ; the candle is lit
(define *candle-time*      60)          ; candle time left
(define *top-of-tree*      #f)          ; true if at the top of three
(define *ghosts-appear*    #f)          ; true if ghosts appeared
(define *vacuum-on*        #f)          ; true if ghosts are vacuumed
(define *bats-present*     #f)          ; true if bats are flying
(define *spell-discovered* #f)          ; true if spell discovered
(define *quest-won*        #f)          ; true if quest is won

;; rooms association list
(define *nodes*
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

    (closed-front-porch
     "front porch"
     (north front-lobby)
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

;; items association list
(define *items*
  '((painting "painting" spooky-room)
    (ring "ring" coffin-cellar hidden)
    (spells "magic spells" secret-room)
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
    (candle "candle" study hidden)
    (key "key" cupboard hidden)))

(define *words*
  '((bats "bats")
    (books "books")
    (coat "coat")
    (coffin "coffin")
    (desk "desk")
    (door "door")
    (drawer "drawer")
    (ghosts "ghosts")
    (rubbish "rubbish")
    (spells "spells")
    (walls "walls")
    (xzanfar "xzanfar")))

(define *verbs*
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
    (light "light")
    (load "load")
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
    (west "west")))

;; location functions
(define (find-location id)
  (or (assq id *player-nodes*)
      (assq id *nodes*)))

(define (location-name id)
  (let ((loc (find-location id)))
    (and loc (cadr loc))))

(define (location-edges id)
  (let ((loc (find-location id)))
    (and loc (cddr loc))))

(define (replace-location! old-id new-id)
  (let ((new (find-location new-id)))
    (set! *player-nodes* (cons (cons old-id (cdr new))
                               *player-nodes*))))

;; item functions
(define (item-name id)
  (let ((item (assq id *items*)))
    (and item (cadr item))))

(define (item-location id)
  (hashtable-ref *item-locations* id #f))

(define (item-location-set! item-id loc-id)
  (hashtable-set! *item-locations* item-id loc-id))

(define (item-hidden? id)
  (hashtable-ref *item-hidden* id #f))

(define (item-hidden-set! id value)
  (hashtable-set! *item-hidden* id value))

(define (item-in-backpack? id)
  (eq? (item-location id) 'backpack))

(define (is-item? id)
  (and (assq id *items*) #t))

(define (items-at loc-id)
  (filter (lambda (item-id)
            (eq? (item-location item-id) loc-id))
          (map car *items*)))

;; string functions
(define (search-string str lst)
  (cond ((null? lst) #f)
        ((equal? (cadr (car lst)) str)
         (car (car lst)))
        (else
         (search-string str (cdr lst)))))

(define (find-word str)
  (or (search-string str *items*)
      (search-string str *words*)))

(define (find-verb str)
  (search-string str *verbs*))

;; misc functions
(define (random-choice lst)
  (list-ref lst (random (length lst))))

;; verb handlers
(define (handle-help loc word wordstr default)
  (format #f "Words i know: ~{~a~^, ~}"
          (map cadr *verbs*)))

(define (handle-inventory loc word wordstr default)
  (let ((items (map item-name (items-at 'backpack))))
    (if (null? items)
        "You are carrying nothing."
        (format #f "You are carrying ~{~a~^, ~}." items))))

(define (handle-go loc dir wordstr default)
  (let* ((entry (assq dir (location-edges loc)))
         (next (and entry (cadr entry))))
    (cond ((and (eq? loc 'blasted-tree)
                *top-of-tree*)
           (set! *top-of-tree* #f)
           "CRASH!!! You fell out of the tree.")
          ((and (eq? loc 'cobwebby-room)
                *ghosts-appear*)
           "Ghosts will not let you move!")
          ((and (eq? loc 'cold-chamber)
                (eq? dir 'west)
                (item-in-backpack? (find-item 'painting))
                *spell-discovered*)
           "A magical barrier to the west.")
          ((and (eq? loc 'pool-of-light)
                (not *candle-lit*)
                (or (member dir '(north east))))
           "You need a light.")
          ((and (eq? loc 'marsh)
                (not (item-in-backpack? 'boat)))
           "You are stuck.")
          ((and (not (member loc '(cliff-path-3 marsh-by-wall marsh soggy-path)))
                (item-in-backpack? 'boat))
           "You can't carry the boat.")
          ((and (member loc '(valuted-hallway thick-door trophy-room))
                (not *candle-lit*))
           "Too dark to move")
          ((and (eq? next 'front-lobby)
                (eq? dir 'north))
           (set! *current-location* next)
           (set! *front-door-open* #f)
           (replace-location! 'front-porch 'closed-front-porch)
           "The door slams shut!")
          (next
           (set! *current-location* next)
           "OK.")
          (else
           "You can't go that way."))))

(define (handle-get loc item itemstr default)
  (cond ((or (not (is-item? item))
             (item-hidden? item))
         (format #f "What ~a?" itemstr))
        ((item-in-backpack? item)
         "You already have it.")
        ((eq? (item-location item) loc)
         (item-location-set! item 'backpack)
         (format #f "You have the ~a." (item-name item)))
        (else
         "It isn't here.")))

(define (handle-open loc word wordstr default)
  (cond ((and (eq? loc 'study)
              (member word '(drawer desk)))
         (item-hidden-set! 'candle #f)
         "Drawer open.")
        ((and (eq? loc 'thick-door)
              (eq? word 'door))
         "It's locked.")
        ((and (eq? loc 'coffin-cellar)
              (eq? word 'coffin))
         (item-hidden-set! 'ring #f)
         "That's creepy.")
        (else
         default)))

(define (handle-examine loc word wordstr default)
  (cond ((and (eq? loc 'cupboard)
              (eq? word 'coat))
         (item-hidden-set! 'key #f)
         "Something here!")
        ((and (eq? loc 'yard-by-rubbish)
              (eq? word 'rubbish))
         "That's disgusting.")
        ((and (eq? loc 'study)
              (member word '(drawer desk)))
         "There is a drawer.")
        ((and (eq? loc 'study)
              (eq? word 'wall))
         "There is something beyond...")
        ((member word '(books scroll spells magic-spells))
         (handle-read loc word wordstr default))
        ((eq? word 'coffin)
         (handle-open loc word wordstr default))
        (else
         default)))

(define (handle-read loc word wordstr default)
  (cond ((and (eq? loc 'evil-library)
              (eq? word 'books))
         "They are demonic works.")
        ((and (member word '(spells magic-spells))
              (item-in-backpack? 'magic-spells)
              (not *spell-discovered*))
         (set! *spell-discovered* #t)
         "Say this word with care 'xzanfar'.")
        ((eq? word scroll)
         "The script is in an alien tongue.")
        (else
         default)))

(define (handle-say loc word wordstr default)
  (cond ((or (not (eq? word 'xzanfar))
             (not *spell-discovered*))
         (format #f "Ok, '~a'." wordstr))
        (else
         (when (not (eq? loc 'cold-chamber))
           (set! *current-location*
             (random-choice (map car *nodes*))))
         "*** Magic Occurs ***")))

(define (handle-dig loc word wordstr default)
  (cond ((and (eq? loc 'barred-cellar)
             (item-in-backpack? 'shovel))
         (replace-location! 'barred-cellar 'hole-in-wall)
         "Dug the bars out.")
        ((item-in-backpack? 'shovel)
         "You made a hole.")
        (else
         default)))

(define (handle-swing loc word wordstr default)
  (cond ((and (eq? loc 'blasted-tree)
              (not (item-in-backpack? 'rope)))
         "This is no time to play games.")
        ((and (eq? word 'rope)
              (item-in-backpack? 'rope))
         "You swung it.")
        ((and (eq? loc 'study)
              (eq? word 'axe)
              (item-in-backpack? word))
         (replace-location! 'study 'study-secret-room)
         "You broke the thin wall.")
        ((and (eq? word 'axe)
              (item-in-backpack? word))
         "WHOOSH!")
        (else
         default)))

(define (handle-climb loc word wordstr default)
  (cond ((and (eq? word 'rope)
              (item-in-backpack? word))
         "It isn't attached to anything!")
        ((and (eq? word 'rope)
              (not (item-in-backpack? word))
              (eq? loc 'blasted-tree)
              (not *top-of-tree*))
         (set! *top-of-tree* #t)
         "You see thick forest and cliff south.")
        ((and (eq? word 'rope)
              (not (item-in-backpack? word))
              (eq? loc 'blasted-tree)
              *top-of-tree*)
         (set! *top-of-tree* #f)
         "Going down!")
        (else
         default)))

(define (handle-light loc item itemstr default)
  (cond ((or (not (eq? item 'candle))
             (not (item-in-backpack 'candle)))
         default)
        ((not (item-in-backpack? 'matches))
         "Nothing to light it with.")
        ((not (item-in-backpack? 'candlestick))
         "It would burn your hands!")
        (else
         (set! *candle-lit* #t)
         "It casts a flickering light.")))

(define (handle-unlight loc item itemstr default)
  (cond (*candle-lit*
         (set! *candle-lit* #f)
         "Extinguished.")
        (else
         default)))

(define (handle-spray loc word wordstr default)
  (cond ((and (eq? word 'aerosol)
              (item-in-backpack? word)
              *bats-present*)
         (set! *bats-present* #f)
         "PFFT! Got them.")
        ((and (eq? word 'aerosol)
              (item-in-backpack? word))
         "HISSSS...")
        (else
         default)))

(define (handle-use loc item itemstr default)
  (cond ((and (eq? item 'vacuum)
              (item-in-backpack? 'vacuum)
              (item-in-backpack? 'batteries))
         (set! *vacuum-on* #t)
         (cond (*ghosts-appear*
                (set! *ghosts-appear #f)
                "WHIZZ - Vacuumed the ghosts up!")
               (else
                "Switched on.")))
        (t
         default)))

(define (handle-unlock loc word wordstr default)
  (cond ((and (eq? loc 'study)
              (member word '(drawer desk)))
         (handle-open loc word wordstr default))
        ((and (eq? loc 'thick-door)
              (eq? word 'door)
              (item-in-backpack? 'key))
         (replace-location! 'thick-door 'huge-open-door)
         "The key turns!")
        (else
         default)))

(define (handle-drop loc item itemstr default)
  (cond ((item-in-backpack? item)
         (item-location-set! item loc)
         "Done.")
        (else
         default)))

(define (handle-score loc item itemstr default)
  (let ((score (length (items-at 'backpack))))
    (cond ((< score 17)
           (format #f "Your score is ~a." score))
          ((eq? loc 'iron-gate-path)
           (set! *quest-won* #t)
           (format #f "Double score for reaching here!~%Your score is ~a." (* 2 score)))
          (else
           (format #f "You have everything, return to the gate for the final score.")))))

;; list of the allowed commands and their handler
(define *allowed-commands*
  `((help ,handle-help)
    (inventory ,handle-inventory)
    (north ,handle-go)
    (south ,handle-go)
    (west ,handle-go)
    (east ,handle-go)
    (up ,handle-go)
    (down ,handle-go)
    (get ,handle-get)
    (open ,handle-open)
    (examine ,handle-examine)
    (read ,handle-read)
    (say ,handle-say)
    (dig ,handle-dig)
    (swing ,handle-swing)
    (climb ,handle-climb)
    (light ,handle-light)
    (unlight ,handle-unlight)
    (spray ,handle-spray)
    (use ,handle-use)
    (drop ,handle-drop)
    (score ,handle-score)))

(define (input prompt)
  (display prompt)
  (let ((line (get-line (current-input-port))))
    (if (eof-object? line)
        "exit"
        line)))

(define (game-save filename)
  (define (hashtable->alist ht)
    (let-values (((keys values) (hashtable-entries ht)))
      (filter cdr (map cons
                       (vector->list keys)
                       (vector->list values)))))
  (define (writeln obj)
    (write obj)
    (newline))

  (guard (con
          ((error? con)
           (display-condition con)
           (newline)
           #f)
          (else #f))
    (with-output-to-file filename
      (lambda ()
        (writeln (hashtable->alist *item-locations*))
        (writeln (hashtable->alist *item-hidden*))
        (writeln *player-nodes*)
        (writeln *current-location*)
        (writeln *front-door-open*)
        (writeln *candle-lit*)
        (writeln *candle-time*)
        (writeln *top-of-tree*)
        (writeln *bats-present*)
        (writeln *ghosts-appear*)
        (writeln *vacuum-on*)
        (writeln *spell-discovered*)
        (writeln *quest-won*)
        #t)
      'replace)))

(define (game-load filename)
  (define (alist->hashtable! alist ht)
    (hashtable-clear! ht)
    (for-each (lambda (kv)
                (hashtable-set! ht (car kv) (cdr kv)))
              alist))
  (guard (con
          ((error? con)
           (display-condition con)
           (newline)
           #f)
          (else #f))
    (with-input-from-file filename
      (lambda ()
        (alist->hashtable! (read) *item-locations*)
        (alist->hashtable! (read) *item-hidden*)
        (set! *player-nodes* (read))
        (set! *current-location* (read))
        (set! *front-door-open* (read))
        (set! *candle-lit* (read))
        (set! *candle-time* (read))
        (set! *top-of-tree* (read))
        (set! *bats-present* (read))
        (set! *ghosts-appear* (read))
        (set! *vacuum-on* (read))
        (set! *spell-discovered* (read))
        (set! *quest-won* (read))))
    #t))

(define (game-init)
  (hashtable-clear! *item-locations*)
  (hashtable-clear! *item-hidden*)
  (for-each (lambda (item)
              (let ((id (car item))
                    (loc (caddr item))
                    (hidden (if (member 'hidden (cdddr item)) #t #f)))
                (hashtable-set! *item-locations* id loc)
                (hashtable-set! *item-hidden* id hidden)))
            *items*)
  (set! *player-nodes* '())
  (set! *current-location* 'iron-gate-path)
  (set! *front-door-open* #t)
  (set! *candle-lit* #f)
  (set! *candle-time* 60)
  (set! *top-of-tree* #f)
  (set! *bats-present* #t)
  (set! *ghosts-appear* #f)
  (set! *vacuum-on* #f)
  (set! *spell-discovered* #f)
  (set! *quest-won* #f))

(define (string-position string char)
  (let ((len (string-length string)))
    (let test-char ((n 0))
      (cond ((= n len) #f)
            ((char=? (string-ref string n) char) n)
            (else
             (test-char (+ n 1)))))))

(define (game-loop message)
  (cond (*quest-won*
         (display "HOOORRAAAY!")
         (newline))
        (else
         (newline)
         (newline)
         (display "Haunted House") (newline)
         (display "-------------") (newline)
         (display "Your location") (newline)
         (display (location-name *current-location*)) (newline)
         (format #t "Exits: ~{~a~^, ~}~%"
                 (map car
                      (location-edges *current-location*)))
         (for-each (lambda (item)
                     (format #t "You can see ~a here.~%"
                             (item-name item)))
                   (filter (lambda (item)
                             (not (item-hidden? item)))
                           (items-at *current-location*)))
         (display "=======================")
         (newline)
         (display message)
         (newline)
         (let* ((string (input "What will you do now? "))
                (ws (string-position string #\space))
                (verb (find-verb (if ws (substring string 0 ws) string)))
                (row (assq verb *allowed-commands*))
                (handler (and row (cadr row)))
                (flags (if row (cddr row) '()))
                (target (if ws (substring string (+ ws 1) (string-length string)) ""))
                (word (or (find-word target)
                          (find-word (string-append target "s"))))
                (word (if (and (eq? handler handle-go) (not word))
                          verb
                          word)))
           (cond ((eq? verb 'exit)
                  (display "Bye...")
                  (newline))
                 ((eq? verb 'save)
                  (let ((filename (input "Please enter the filename: ")))
                    (game-loop (if (game-save filename)
                                   "Ok, carry on."
                                   "Cannot save the game."))))
                 ((eq? verb 'load)
                  (let ((filename (input "Please enter the filename: ")))
                    (game-loop (if (game-load filename)
                                   "Ok, carry on."
                                   "Cannot load the game."))))
                 ((and verb (not handler))
                  (format #t "ERROR: missing handler for verb ~a~%" verb))
                 ((and (not verb) (not word))
                  (game-loop "You don't make sense."))
                 ((not handler)
                  (game-loop (format #f "You can't '~a'." string)))
                 ((and *bats-present*
                       (eq? *current-location* 'rear-turret-room)
                       (not (= (random 3) 0))
                       (not (eq? verb 'spray)))
                  (game-loop "Bats attacking!"))
                 (else
                  (when (and (eq? *current-location* 'cobwebby-room)
                             (= (random 2) 0)
                             (not *vacuum-on*))
                    (set! *ghosts-appear* #t))

                  (when *candle-lit*
                    (set! *candle-time* (- *candle-time* 1))
                    (when (= *candle-time* 0)
                      (set! *candle-lit* #f)))

                  (let* ((default (cond ((not (item-in-backpack? word))
                                         (format #f "You don't have '~a'." target))
                                        ((string=? target "")
                                         "I need two words.")
                                        ((not word)
                                         "That's silly.")
                                        (else
                                         "What.")))
                         (message (handler *current-location*
                                           word
                                           target
                                           default)))
                    (game-loop (cond ((= *candle-time* 10)
                                      "Your candle is waning!")
                                     ((= *candle-time* 1)
                                      "Your candle is out!")
                                     (else message))))))))))

(define (game-start)
  (game-init)
  (game-loop "OK."))
