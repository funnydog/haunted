;; The Haunted House - scheme edition
;; Adapted from "Write your own adventure programs" by Usborne.

;; room record definition
(define-record-type room
  (fields description exits))

;; syntax rule to make a new room without quoting
(define-syntax new-room
  (syntax-rules ()
    ((_ id description rooms ...)
     `(id ,(make-room description '(rooms ...))))))

;; rooms association list
(define *rooms*
  (list (new-room dark-corner
                  "dark corner"
                  (south corner-of-house)
                  (east overgrown-garden))

        (new-room overgrown-garden
                  "overgrown garden"
                  (west dark-corner)
                  (east large-woodpile))

        (new-room large-woodpile
                  "large woodpile"
                  (west overgrown-garden)
                  (east yard-by-rubbish))

        (new-room yard-by-rubbish
                  "yard by rubbish"
                  (south scullery-door)
                  (west large-woodpile)
                  (east weedpatch))

        (new-room weedpatch
                  "weedpatch"
                  (west yard-by-rubbish)
                  (east forest))

        (new-room forest
                  "forest"
                  (west weedpatch)
                  (east thick-forest))

        (new-room thick-forest
                  "thick forest"
                  (south clearing-by-house)
                  (west forest)
                  (east blasted-tree))

        (new-room blasted-tree
                  "blasted tree"
                  (south path)
                  (west thick-forest))

        (new-room corner-of-house
                  "corner of house"
                  (north dark-corner)
                  (south side-of-house))

        (new-room entrance-to-kitchen
                  "entrance to kitchen"
                  (south back-of-hallway)
                  (east kitchen))

        (new-room kitchen
                  "kitchen and grimy cooker"
                  (west entrance-to-kitchen)
                  (east scullery-door))

        (new-room scullery-door
                  "scullery door"
                  (north yard-by-rubbish)
                  (west kitchen))

        (new-room room-with-dust
                  "room with inches of dust"
                  (east rear-turret-room)
                  (down bottom-staircase))

        (new-room rear-turret-room
                  "rear turret room"
                  (west room-with-dust))

        (new-room clearing-by-house
                  "clearing by house"
                  (north thick-forest)
                  (east path))

        (new-room path
                  "path"
                  (north blasted-tree)
                  (south clifftop)
                  (west clearing-by-house))

        (new-room side-of-house
                  "side of the house"
                  (north corner-of-house)
                  (south crumbling-wall))

        (new-room back-of-hallway
                  "back of the hallway"
                  (north entrance-to-kitchen)
                  (south gloomy-passage))

        (new-room dark-alcove
                  "dark alcove"
                  (south pool-of-light)
                  (east small-dark-room))

        (new-room small-dark-room
                  "small dark room"
                  (west dark-alcove)
                  (east bottom-staircase))

        (new-room bottom-staircase
                  "bottom of a spiral staircase"
                  (west small-dark-room)
                  (up room-with-dust))

        (new-room wide-passage
                  "wide passage"
                  (south trophy-room)
                  (east slippery-steps))

        (new-room slippery-steps
                  "slippery steps"
                  (south barred-cellar)
                  (west wide-passage)
                  (up wide-passage)
                  (down barred-cellar))

        (new-room clifftop
                  "clifftop"
                  (north path)
                  (south cliff-path-1))

        (new-room crumbling-wall
                  "near a crumbling wall"
                  (north side-of-house))

        (new-room gloomy-passage
                  "gloomy passage"
                  (north back-of-hallway)
                  (south front-hall))

        (new-room pool-of-light
                  "pool of light"
                  (north dark-alcove)
                  (south sitting-room)
                  (east vaulted-hallway))

        (new-room vaulted-hallway
                  "impressive vaulted hallway"
                  (west pool-of-light)
                  (east thick-door))

        (new-room thick-door
                  "hall by thick wooden door"
                  (west vaulted-hallway)
                  (east trophy-room))

        (new-room trophy-room
                  "trophy room"
                  (north wide-passage)
                  (south dining-room)
                  (west thick-door))

        (new-room barred-cellar
                  "cellar with barred window"
                  (north slippery-steps)
                  (south coffin-cellar))

        (new-room cliff-path-1
                  "cliff path"
                  (north clifftop)
                  (south cliff-path-2))

        (new-room cupboard
                  "cupboard with hanging coat"
                  (south closet))

        (new-room front-hall
                  "front hall"
                  (north gloomy-passage)
                  (south front-lobby)
                  (east sitting-room))

        (new-room sitting-room
                  "sitting room"
                  (north pool-of-light)
                  (south evil-library)
                  (west front-hall))

        (new-room secret-room
                  "secret room"
                  (south study))

        (new-room marble-stairs
                  "steep marble stairs"
                  (north thick-door)
                  (south cobwebby-room)
                  (up cobwebby-room)
                  (down thick-door))

        (new-room dining-room
                  "dining room"
                  (north trophy-room))

        (new-room coffin-cellar
                  "deep cellar with coffin"
                  (north barred-cellar))

        (new-room cliff-path-2
                  "cliff path"
                  (north cliff-path-1)
                  (south cliff-path-3))

        (new-room closet
                  "closet"
                  (north cupboard)
                  (east front-lobby))

        (new-room front-lobby
                  "front lobby"
                  (north front-hall)
                  (south front-porch)
                  (west closet))

        (new-room evil-library
                  "library of evil books"
                  (north sitting-room)
                  (east study))

        (new-room study
                  "study with desk and hole in the wall"
                  (west evil-library))

        (new-room cobwebby-room
                  "weird cobwebby room"
                  (north marble-stairs)
                  (south upper-gallery)
                  (east cold-chamber))

        (new-room cold-chamber
                  "very cold chamber"
                  (west cobwebby-room)
                  (east spooky-room))

        (new-room spooky-room
                  "spooky room"
                  (west cold-chamber))

        (new-room cliff-path-3
                  "cliff path by marsh"
                  (north cliff-path-2)
                  (south soggy-path))

        (new-room verandah
                  "rubble-strewn verandah"
                  (south twisted-railings)
                  (east front-porch))

        (new-room front-porch
                  "front porch"
                  (north front-lobby)
                  (south iron-gate-path)
                  (west verandah))

        (new-room front-tower
                  "front tower"
                  (east sloping-corridor))

        (new-room sloping-corridor
                  "sloping corridor"
                  (west front-tower)
                  (east upper-gallery))

        (new-room upper-gallery
                  "upper gallery"
                  (north cobwebby-room)
                  (west sloping-corridor))

        (new-room marsh-by-wall
                  "marsh by wall"
                  (south fallen-brickwork))

        (new-room marsh
                  "marsh"
                  (south stone-arch)
                  (west marsh-by-wall))

        (new-room soggy-path
                  "soggy path"
                  (north cliff-path-3)
                  (west marsh))

        (new-room twisted-railings
                  "by twisted railings"
                  (north verandah)
                  (east iron-gate-path))

        (new-room iron-gate-path
                  "path through iron gate"
                  (north front-porch)
                  (west twisted-railings)
                  (east railings))

        (new-room railings
                  "by railings"
                  (west iron-gate-path)
                  (east beneath-front-tower))

        (new-room beneath-front-tower
                  "beneath the front tower"
                  (west railings)
                  (east debris))

        (new-room debris
                  "debris from crumbling facade"
                  (west beneath-front-tower)
                  (east fallen-brickwork))

        (new-room fallen-brickwork
                  "large fallen brickwork"
                  (north marsh-by-wall)
                  (west debris)
                  (east stone-arch))

        (new-room stone-arch
                  "rotten stone arch"
                  (north marsh)
                  (west fallen-brickwork)
                  (east crumbling-clifftop))

        (new-room crumbling-clifftop
                  "crumbling clifftop"
                  (west stone-arch))

        ;; NOTE: connected to barred-cellar after digging the bars
        (new-room hole-in-wall
                  "hole in the wall"
                  (north slippery-steps)
                  (south coffin-cellar)
                  (east cliff-path-1))

        ;; NOTE: connected to study after swinging the axe
        (new-room study-secret-room
                  "study with secret room"
                  (north secret-room)
                  (west evil-library))

        ;; NOTE: connected to thick-door after unlocking the door
        (new-room huge-open-door
                  "Huge open door"
                  (south marble-stairs)
                  (west vaulted-hallway)
                  (east trophy-room))))

;; edge functions for exits
(define (edge-direction edge)
  (car edge))
(define (edge-location edge)
  (cadr edge))

;; functions to find rooms
(define (find-room id)
  "Find a room by id"
  (cadr (or (assq id *rooms*) '(#f #f))))

(define (add-room! new-id room)
  "Add a previous room with the new id"
  (set! *rooms* (cons (list new-id room)
                      *rooms*)))

;; map function
(define (make-dot rooms)
  (let ((ids (map car rooms))
        (rooms (map cadr rooms)))
    (define (sym->dotname sym)
      (list->string
       (map (lambda (c)
              (if (or (char-alphabetic? c)
                      (char-numeric? c))
                  c
                  #\_))
            (string->list (symbol->string sym)))))

    ;; open the directed graph
    (format #t "digraph {~%")
    ;; print the nodes
    (for-each (lambda (id room)
                (format #t "~a[label=\"~a\"];~%"
                        (sym->dotname id)
                        (room-description room)))
              ids rooms)
    ;; print the edges
    (for-each (lambda (id room)
                (let ((src (sym->dotname id)))
                  (for-each (lambda (edge)
                              (format #t "~a -> ~a [label=\"~a\"];~%"
                                      src
                                      (sym->dotname (edge-location edge))
                                      (edge-direction edge)))
                            (room-exits room))))
              ids rooms)
    ;; close the directed graph
    (format #t "}~%")))

;; item record definition
(define-record-type item
  (fields name
          (mutable location)
          (mutable value)))

(define (item-in-backpack? item)
  (eq? (item-location item) 'backpack))

;; syntax-rule to make a new item without quoting
(define-syntax new-item
  (syntax-rules ()
    ((_ id name location value)
     `(id ,(make-item name 'location value)))))

;; items association list
(define *items*
  (list (new-item painting "painting" spooky-room #f)
        (new-item ring "ring" coffin-cellar #t)
        (new-item spells "magic spells" secret-room #f)
        (new-item goblet "goblet" front-tower #f)
        (new-item scrooll "scroll" rear-turret-room #f)
        (new-item coins "coins" dark-alcove #f)
        (new-item statue "statue" thick-door #f)
        (new-item candlestick "candlestick" evil-library #f)
        (new-item matches "matches" kitchen #f)
        (new-item vacuum "vacuum" gloomy-passage #f)
        (new-item batteries "batteries" pool-of-light #f)
        (new-item shovel "shovel" weedpatch #f)
        (new-item axe "axe" large-woodpile #f)
        (new-item rope "rope" blasted-tree #f)
        (new-item boat "boat" cliff-path-3 #f)
        (new-item aerosol "aerosol" debris #f)
        (new-item candle "candle" study #t)
        (new-item key "key" cupboard #t)
        (new-item north "north" #f #f)
        (new-item south "south" #f #f)
        (new-item west "west" #f #f)
        (new-item east "east" #f #f)
        (new-item up "up" #f #f)
        (new-item down "down" #f #f)
        (new-item door "door" #f #f)
        (new-item bats "bats" #f #f)
        (new-item ghosts "ghosts" #f #f)
        (new-item drawer "drawer" #f #f)
        (new-item desk "desk" #f #f)
        (new-item coat "coat" #f #f)
        (new-item rubbish "rubbish" #f #f)
        (new-item coffin "coffin" #f #f)
        (new-item books "books" #f #f)
        (new-item xzanfar "xzanfar" #f #f)
        (new-item wall "wall" #f #f)))

;; functions to find items
(define (find-item id)
  "Return the first item with the given id"
  (cadr (or (assq id *items*) '(#f #f))))

(define (find-item-by-name name)
  "Return the first pair (id, item) where the item has the given name"
  (let loop ((items *items*))
    (cond ((null? items) '(#f #f))
          ((equal? (item-name (cadar items)) name)
           (car items))
          (else
           (loop (cdr items))))))

(define (find-items-by-location location)
  "Return the list of items at a given location"
  (filter (lambda (item)
            (eq? (item-location item) location))
          (map cadr *items*)))

;; self check routines
(define (room-check rooms)
  "Consistency check for the rooms"
  (for-each (lambda (id room)
              ;; connectivity check
              (for-each (lambda (edge)
                          (when (not (assq (edge-location edge) rooms))
                            (format #t "WARNING: room \"~a\" has a broken edge: ~a~%"
                                    (room-description room)
                                    edge)))
                        (room-exits room))
              ;; duplicated id check
              (when (not (eq? (cadr (assq id rooms))
                              room))
                (format #t "WARNING: room \"~a\" with already used id '~a~%"
                        (room-description room)
                        id)))
            (map car rooms)
            (map cadr rooms)))

(define (item-check items rooms)
  "Consistency check for the items"
  (for-each (lambda (id item)
              ;; check the if the location of the item is valid
              (when (and (item-location item)
                         (not (assq (item-location item) rooms)))
                (format #t "WARNING: object \"~a\" with unknown location: ~a~%"
                        (item-name item)
                        (item-location item)))
              ;; check if the item id was already used
              (when (not (eq? (cadr (assq id items))
                              item))
                (format #t "WARNING: item \"~a\" with already used id '~a~%"
                        (item-name item)
                        id)))
            (map car items)
            (map cadr items)))

;; player variables
(define *location* 'iron-gate-path)
(define *light-on* #f)
(define *light-time* 60)
(define *exit* #f)

(define (handle-help room verb words)
  (format #f "Words i know:~%~{~a~%~}"
          (map command-id *allowed-commands*)))

(define (handle-carrying room verb words)
  (let ((items (map item-name (find-items-by-location 'backpack))))
    (cond ((null? items)
           "You are carrying nothing.")
          (else
           (format #f "You are carrying: ~{~a~^, ~}." items)))))

(define (handle-go room verb words)
  (let* ((verb (cond ((eq? verb 'go)
                      (car (find-item-by-name words)))
                     ((eq? verb 'u) 'up)
                     ((eq? verb 'd) 'down)
                     ((eq? verb 'n) 'north)
                     ((eq? verb 's) 'south)
                     ((eq? verb 'w) 'west)
                     ((eq? verb 'e) 'east)
                     (else verb)))
         (next (assq verb (room-exits room))))
    (cond ((and (eq? *location* 'blasted-tree)
                (item-value (find-item 'rope)))
           (item-value-set! (find-item 'rope) #f)
           "CRASH!!! You fell out of the tree")
          ((and (eq? *location* 'upper-gallery)
                (item-value (find-item 'ghosts)))
           "Ghosts will not let you move!")
          ((and (eq? *location* 'cold-chamber)
                (item-in-backpack? (find-item 'painting))
                (item-value (find-item 'xzanfar)))
           "A magical barrier to the west")
          ((and (eq? *location* 'pool-of-light)
                (not *light-on*)
                (or (eq? verb 'north)
                    (eq? verb 'east)))
           "You need a light")
          ((and (eq? *location* 'marsh)
                (not (item-in-backpack? (find-item 'boat))))
           "You are stuck")
          ((and (not (or (eq? *location* 'cliff-path-3)
                         (eq? *location* 'marsh-by-wall)
                         (eq? *location* 'marsh)
                         (eq? *location* 'soggy-path)))
                (item-in-backpack? (find-item 'boat)))
           "You can't carry the boat")
          ((and (or (eq? *location* 'valuted-hallway)
                    (eq? *location* 'thick-door)
                    (eq? *location* 'trophy-room))
                (not *light-on*))
           "Too dark to move")
          (next
           (set! *location* (edge-location next))
           "OK")
          (else
           "You can't go that way"))))

(define (handle-get room verb words)
  (let ((item (cadr (find-item-by-name words))))
    (cond ((or (not item) (item-value item))
           (format #f "What ~a?" words))
          ((item-in-backpack? item)
           "You already have it")
          ((not (eq? (item-location item) *location*))
           "It isn't here")
          (else
           (item-location-set! item 'backpack)
           (format #f "You have the ~a" words)))))

(define (handle-open room verb word)
  (cond ((and (eq? *location* 'study)
              (or (equal? word "drawer")
                  (equal? word "desk")))
         (item-value-set! (find-item 'candle) #f)
         "Drawer open")
        ((and (eq? *location* 'thick-door)
              (equal? word "door"))
         "It's locked")
        ((and (eq? *location* 'coffin-cellar)
              (equal? word "coffin"))
         (item-value-set! (find-item 'ring) #f)
         "That's creepy")
        (else
         "You can't open it.")))

(define (handle-examine room verb word)
  (cond ((and (eq? *location* 'cupboard)
              (equal? word "coat"))
         (item-value-set! (find-item 'key) #f)
         "There is something here!")
        ((and (eq? *location* 'yard-by-rubbish)
              (equal? word "rubbish"))
         "That's disgusting")
        ((and (eq? *location* 'study)
              (or (equal? word "drawer")
                  (equal? word "desk")))
         "There is a drawer")
        ((or (equal? word "books")
             (equal? word "scroll"))
         (handle-read room verb word))
        ((and (eq? *location* 'study)
              (equal? word "wall"))
         "There is something beyond...")
        ((equal? word "coffin")
         (handle-open room verb word))
        (else
         "You cannot examine that.")))

(define (handle-read room verb word)
  (cond ((and (eq? *location* 'evil-library)
              (equal? word "books"))
         "They are demonic works")
        ((and (or (equal? word "spells")
                  (equal? word "magic spells"))
              (item-in-backpack? (find-item 'spells))
              (not (item-value (find-item 'xzanfar))))
         "Use this word with care 'xzanfar'")
        ((and (equal? word "scroll")
              (item-in-backpack? (find-item 'scroll)))
         "The script is in an alien tongue")
        (else
         "You cannot read it.")))

(define (random-choice lst)
    (list-ref lst (random (length lst))))

(define (handle-say room verb word)
  (cond ((or (not (equal? word "xzanfar"))
             (not (item-in-backpack? (find-item 'spells))))
         (format #f "Ok, '~a'." word))
        (else
           (if (not (eq? *location* 'cold-chamber))
               (set! *location* (random-choice (map car *rooms*)))
               (item-value-set! (find-item 'xzanfar) #t))
           "*** Magic Occurs ***")))

(define (handle-dig room verb word)
  (let ((shovel (find-item 'shovel)))
    (cond ((not (item-in-backpack? shovel))
           "You can't dig with bare hands.")
          ((and (eq? *location* 'barred-cellar)
                (not (assq 'east (room-exits room))))
           (add-room! 'barred-cellar (find-room 'hole-in-wall))
           "Dug the bars out.")
          (else
           "You made a hole."))))

(define (handle-swing room verb word)
  (cond ((equal? word "rope")
         (let ((rope (find-item 'rope)))
           (cond ((and (eq? *location* 'blasted-tree)
                       (not (item-in-backpack? rope)))
                  "This is no time to play games.")
                 ((item-in-backpack? rope)
                  "You swung it.")
                 (else
                  "No rope to swing."))))
        ((equal? word "axe")
         (let ((axe (find-item 'axe)))
           (cond ((not (item-in-backpack? axe))
                  "No axe to swing.")
                 ((and (eq? *location* 'study)
                       (not (assq 'north (room-exits room))))
                  (add-room! 'study (find-room 'study-secret-room))
                  "You broke the thin wall.")
                 (else
                  "Woosh!."))))
        (else
         (format #f "No way to ~a ~a" verb word))))

(define (handle-climb room verb word)
  (let ((rope (find-item 'rope)))
    (cond ((not (equal? word "rope"))
           "You can't do that.")
          ((item-in-backpack? rope)
           "It isn't attached to anything!")
          ((item-value rope)
           (item-value-set! rope #f)
           "Going down.")
          (else
           (item-value-set! rope #t)
           "You see a thick forest and a cliff at south."))))

(define (handle-light room verb word)
  (cond ((not (equal? word "candle"))
         (format #f "You cannot light a ~a." word))
        ((not (item-in-backpack? (find-item 'candle)))
         "No candle to light.")
        ((not (item-in-backpack? (find-item 'matches)))
         "Nothing to light it with.")
        ((not (item-in-backpack? (find-item 'candlestick)))
         "It would burn your hands!")
        (else
         (set! *light-on* #t)
         "It casts a flickering light.")))

(define (handle-unlight room verb word)
  (cond (*light-on*
         (set! *light-on* #f)
         "Extinguished.")
        (else
         "The light is already off.")))

(define (handle-spray room verb word)
  (let ((bats (find-item 'bats)))
    (cond ((not (item-in-backpack? (find-item 'aerosol)))
           "You can't spray anything.")
          ((and (not (equal? word "bats"))
                (not (item-value "bats")))
           "HISS...")
          (else
           (item-value-set (find-item 'bats) #f)
           "Pfft! Got them."))))

(define (handle-use room verb word)
  (cond ((and (equal? word "vacuum")
              (item-in-backpack? (find-item 'vacuum)))
         (let ((ghosts (find-item 'ghosts)))
           (cond ((not (item-in-backpack? (find-item 'batteries)))
                  "No batteries for the vacuum.")
                 ((and (eq? *location* 'cobwebby-room)
                       (item-value ghosts))
                  (item-value-set! ghosts #f)
                  (item-value-set! (find-item 'down) #t)
                  "WHIZZ - Vacuumed the ghosts up!")
                 (else
                  "Nothing to vacuum."))))
        (else
         (format #f "You cannot ~a ~a." verb word))))

(define (handle-unlock room verb word)
  (let ((door (find-item 'door)))
    (cond ((and (eq? *location* 'thick-door)
                (equal? word "door")
                (not (assq 'south (room-exits room)))
                (item-in-backpack? (find-item 'key)))
           (add-room! 'thick-door (find-room 'huge-open-door))
           "The key turns!")
          (else
           "Nothing to unlock."))))

(define (handle-drop room verb word)
  (let ((item (find-item-by-description word)))
    (cond ((not item)
           "Cannot find it.")
          ((not (item-in-backpack? item))
           "No such object in your backpack.")
          (else
           (item-location-set! item *location*)
           "Done."))))

(define (handle-score room verb word)
  (let ((score (length (find-items-by-location 'backpack))))
    (cond ((not (equal? score 17))
           (format #f "Your score is ~a." score))
          ((eq? *location* 'iron-gate-path)
           (format #f "Double score for reaching here!~%Your score is ~a." (* 2 score)))
          (else
           (format #f "You have everything~%Return to the gate for the final score.")))))

(define (handle-exit room verb word)
  (set! *exit* #t)
  "Bye!")

;; list of the allowed commands and their handler
(define *allowed-commands*
  `((help ,handle-help)
    (carrying ,handle-carrying)
    (inventory ,handle-carrying)
    (inv ,handle-carrying)
    (i ,handle-carrying)
    (go ,handle-go)
    (n ,handle-go)
    (s ,handle-go)
    (w ,handle-go)
    (e ,handle-go)
    (u ,handle-go)
    (d ,handle-go)
    (north ,handle-go)
    (south ,handle-go)
    (west ,handle-go)
    (east ,handle-go)
    (up ,handle-go)
    (down ,handle-go)
    (get ,handle-get)
    (take ,handle-get)
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
    (unlock ,handle-unlock)
    (leave ,handle-drop)
    (drop ,handle-drop)
    (score ,handle-score)
    (exit ,handle-exit)))

(define (command-id cmd)
  (car cmd))
(define (command-handler cmd)
  (cadr cmd))

(define (string-split string char-delimiter?)
  (define (maybe-add a b parts)
    "Add a substring if a != b"
    (cond ((= a b) parts)
          (else
           (cons (substring string a b)
                 parts))))

  (let ((length (string-length string)))
    (let loop ((a 0) (b 0) (parts '()))
      (cond ((< b length)
             (cond ((char-delimiter? (string-ref string b))
                    (loop (+ b 1) (+ b 1)
                          (maybe-add a b parts)))
                   (else
                    (loop a (+ b 1) parts))))
            (else
             (reverse (maybe-add a b parts)))))))

(define (string->query string)
  (let ((tokens (string-split string char-whitespace?)))
    (cond ((null? tokens)
           '(#f ""))
          (else
           `(,(string->symbol (car tokens))
             ,(format #f "~{~a~^ ~}" (cdr tokens)))))))

(define (game-read)
  (let ((string (get-line (current-input-port))))
    (cond ((string? string)
           (string->query string))
          (else
           '(exit "")))))

(define (query-verb cmd)
  (car cmd))
(define (query-rest cmd)
  (cadr cmd))

(define (game-loop)
  (unless *exit*
    (let ((room (find-room *location*)))
      ;; PRINT
      ;; room description and exits
      (format #t "Your location: ~a.~%" (room-description room))
      (format #t "Exits: ~{~a~^, ~}~%" (map edge-direction (room-exits room)))

      ;; visible items
      (let ((visible-items (filter (lambda (item)
                                     (not (item-value item)))
                                   (find-items-by-location *location*))))
        (when (not (null? visible-items))
          (format #t "You see: ~{~a~^, ~}.~%"
                  (map item-name visible-items))))

      ;; candle status
      (when *light-on*
        (when (= *light-time* 10)
          (format #t "Your candle is waning!"))
        (when (= *light-time* 1)
          (format #t "Your candle is out!")))

      ;; READ
      (format #t ">>> ")
      (let* ((query (game-read))
             (verb (query-verb query))
             (rest (query-rest query))
             (cmd (assq verb *allowed-commands*)))

        ;; EVAL
        (cond ((and (eq? *location* 'rear-turret-room)
                    (not (item-value (find-item 'bats)))
                    (not (eq? verb 'use))
                    (= (random 3) 0))
               ;; NOTE: bats prevent any command but 'use
               (format #t "Bats attacking!~%"))
              ((not cmd)
               (format #t "You can't ~a ~a~%" verb rest))
              (else
               ;; set ghosts with a 50% chance in cobwebby-room unless
               ;; they have been vacuumed already
               (when (and (eq? *location* 'cobwebby-room)
                          (not (item-value (find-item 'down)))
                          (= (random 2) 0))
                 (item-value-set! (find-item 'ghosts) #t))

               ;; update the light
               (when *light-on*
                 (set! *light-on* (- *light-on* 1))
                 (when (= *light-on* 0)
                   (set! *light-on* #f)))

               ;; execute the command
               (format #t "~a~%" ((command-handler cmd) room verb rest)))))

      ;; LOOP
      (game-loop))))

(room-check *rooms*)
(item-check *items* *rooms*)
(format #t "Haunted House~%")
(format #t "-------------~%")
(game-loop)
(exit 0)
