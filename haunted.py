#!/usr/bin/env python3
#
# The Haunted House
# adapted from "Write your own adventure programs" by Usborne.

from typing import Callable

import random

WORDS = [
    "",                                         # 0
    "PAINTING",                                 # 1
    "RING",                                     # 2
    "MAGIC SPELLS",                             # 3
    "GOBLET",                                   # 4
    "SCROLL",                                   # 5
    "COINS",                                    # 6
    "STATUE",                                   # 7
    "CANDLESTICK",                              # 8
    "MATCHES",                                  # 9
    "VACUUM",                                   # 10
    "BATTERIES",                                # 11
    "SHOVEL",                                   # 12
    "AXE",                                      # 13
    "ROPE",                                     # 14
    "BOAT",                                     # 15
    "AEROSOL",                                  # 16
    "CANDLE",                                   # 17
    "KEY",                                      # 18
    "NORTH",                                    # 19
    "SOUTH",                                    # 20
    "WEST",                                     # 21
    "EAST",                                     # 22
    "UP",                                       # 23
    "DOWN",                                     # 24
    "DOOR",                                     # 25
    "BATS",                                     # 26
    "GHOSTS",                                   # 27
    "DRAWER",                                   # 28
    "DESK",                                     # 29
    "COAT",                                     # 30
    "RUBBISH",                                  # 31
    "COFFIN",                                   # 32
    "BOOKS",                                    # 33
    "XZANFAR",                                  # 34
    "WALL",                                     # 35
    "SPELLS",                                   # 36
]

OBJECTS = [
    (0,    False),                              # 0
    (46,   False),                              # 1
    (38,   True),                               # 2
    (35,   False),                              # 3
    (50,   False),                              # 4
    (13,   False),                              # 5
    (18,   False),                              # 6
    (28,   False),                              # 7
    (42,   False),                              # 8
    (10,   False),                              # 9
    (25,   False),                              # 10
    (26,   False),                              # 11
    (4,    False),                              # 12
    (2,    False),                              # 13
    (7,    False),                              # 14
    (47,   False),                              # 15
    (60,   False),                              # 16
    (43,   True),                               # 17
    (32,   True),                               # 18
]

ROOMS = [
    ("DARK CORNER",                    "SE"),   # 0
    ("OVERGROWN GARDEN",               "WE"),   # 1
    ("BY LARGE WOODPILE",              "WE"),   # 2
    ("YARD BY RUBBISH",                "SWE"),  # 3
    ("WEEDPATCH",                      "WE"),   # 4
    ("FOREST",                         "WE"),   # 5
    ("THICK FOREST",                   "SWE"),  # 6
    ("BLASTED TREE",                   "WS"),   # 7
    ("CORNER OF HOUSE",                "NS"),   # 8
    ("ENTRANCE TO KITCHEN",            "SE"),   # 9
    ("KITCHEN & GRIMY COOKER",         "WE"),   # 10
    ("SCULLERY DOOR",                  "NW"),   # 11
    ("ROOM WITH INCHES OF DUST",       "SE"),   # 12
    ("REAR TURRET ROOM",               "W"),    # 13
    ("CLEARING BY HOUSE",              "NE"),   # 14
    ("PATH",                           "NSW"),  # 15
    ("SIDE OF THE HOUSE",              "NS"),   # 16
    ("BACK OF HALLWAY",                "NS"),   # 17
    ("DARK ALCOVE",                    "SE"),   # 18
    ("SMALL DARK ROOM",                "WE"),   # 19
    ("BOTTOM OF SPIRAL STAIRCASE",     "NWUD"), # 20
    ("WIDE PASSAGE",                   "SE"),   # 21
    ("SLIPPERY STEPS",                 "WSUD"), # 22
    ("CLIFFTOP",                       "NS"),   # 23
    ("NEAR CRUMBLING WALL",            "N"),    # 24
    ("GLOOMY PASSAGE",                 "NS"),   # 25
    ("POOL OF LIGHT",                  "NSE"),  # 26
    ("IMPRESSIVE VAULTED HALLWAY",     "WE"),   # 27
    ("HALL BY THICK WOODEN DOOR",      "WE"),   # 28
    ("TROPHY ROOM",                    "NSW"),  # 29
    ("CELLAR WITH BARRED WINDOW",      "NS"),   # 30
    ("CLIFF PATH",                     "NS"),   # 31
    ("CUPBOARD WITH HANGING COAT",     "S"),    # 32
    ("FRONT HALL",                     "NSE"),  # 33
    ("SITTING ROOM",                   "NSW"),  # 34
    ("SECRET ROOM",                    "S"),    # 35
    ("STEEP MARBLE STAIRS",            "NSUD"), # 36
    ("DINING ROOM",                    "N"),    # 37
    ("DEEP CELLAR WITH COFFIN",        "N"),    # 38
    ("CLIFF PATH",                     "NS"),   # 39
    ("CLOSET",                         "NE"),   # 40
    ("FRONT LOBBY",                    "NW"),   # 41
    ("LIBRARY OF EVIL BOOKS",          "NE"),   # 42
    ("STUDY WITH DESK & HOLE IN WALL", "W"),    # 43
    ("WEIRD COBWEBBY ROOM",            "NSE"),  # 44
    ("VERY COLD CHAMBER",              "WE"),   # 45
    ("SPOOKY ROOM",                    "W"),    # 46
    ("CLIFF PATH BY MARSH",            "NS"),   # 47
    ("RUBBLE-STREWN VERANDAH",         "SE"),   # 48
    ("FRONT PORCH",                    "NSW"),  # 49
    ("FRONT TOWER",                    "E"),    # 50
    ("SLOPING CORRIDOR",               "WE"),   # 51
    ("UPPER GALLERY",                  "NW"),   # 52
    ("MARSH BY WALL",                  "S"),    # 53
    ("MARSH",                          "SW"),   # 54
    ("SOGGY PATH",                     "NW"),   # 55
    ("BY TWISTED RAILINGS",            "NE"),   # 56
    ("PATH THROUGH IRON GATE",         "NWE"),  # 57
    ("BY RAILINGS",                    "WE"),   # 58
    ("BENEATH THE FRONT TOWER",        "WE"),   # 59
    ("DEBRIS FROM CRUMBLING FACADE",   "WE"),   # 60
    ("LARGE FALLEN BRICKWORK",         "NWE"),  # 61
    ("ROTTING STONE ARCH",             "NWE"),  # 62
    ("CRUMBLING CLIFFTOP",             "W"),    # 63
]

VERBS = [
    "",                                         # 0
    "HELP",                                     # 1
    "CARRYING?",                                # 2
    "GO",                                       # 3
    "N",                                        # 4
    "S",                                        # 5
    "W",                                        # 6
    "E",                                        # 7
    "U",                                        # 8
    "D",                                        # 9
    "GET",                                      # 10
    "TAKE",                                     # 11
    "OPEN",                                     # 12
    "EXAMINE",                                  # 13
    "READ",                                     # 14
    "SAY",                                      # 15
    "DIG",                                      # 16
    "SWING",                                    # 17
    "CLIMB",                                    # 18
    "LIGHT",                                    # 19
    "UNLIGHT",                                  # 20
    "SPRAY",                                    # 21
    "USE",                                      # 22
    "UNLOCK",                                   # 23
    "LEAVE",                                    # 24
    "SCORE",                                    # 25
]

class World:
    def __init__(self) -> None:
        self.handlers = [
            self.verb_help,
            self.verb_carrying,
            self.verb_go,
            self.verb_go,
            self.verb_go,
            self.verb_go,
            self.verb_go,
            self.verb_go,
            self.verb_go,
            self.verb_get,
            self.verb_get,
            self.verb_open,
            self.verb_examine,
            self.verb_read,
            self.verb_say,
            self.verb_dig,
            self.verb_swing,
            self.verb_climb,
            self.verb_light,
            self.verb_unlight,
            self.verb_spray,
            self.verb_use,
            self.verb_unlock,
            self.verb_leave,
            self.verb_score,
        ]

        self.rooms: list[str] = []
        self.exits: list[str] = []
        for room_name, room_exits in ROOMS:
            self.rooms.append(room_name)
            self.exits.append(room_exits)

        self.item_location: dict[int,int] = {}
        self.item_hidden: dict[int,bool] = {}
        self.backpack: dict[int,bool] = {}
        for i, (location, hidden) in enumerate(OBJECTS):
            self.item_location[i] = location
            if hidden:
                self.item_hidden[i] = True

        self.light_on = False
        self.light_time = 60
        self.room = 57
        self.bats_attacking = False
        self.vacuum_on = False
        self.ghosts_present = False
        self.top_of_tree = False
        self.spell_discovered = False
        self.front_door_shut = False
        self.thick_door_open = False
        self.game_end = False

    def run(self) -> None:
        self.message = "OK"
        while not self.game_end:
            print("HAUNTED HOUSE")
            print("-------------")
            print(f"YOUR LOCATION\n{ self.rooms[self.room] }")
            print("EXITS: " + ",".join(r for r in self.exits[self.room]))
            for item, loc in self.item_location.items():
                if not item in self.item_hidden and loc == self.room:
                    print(f"I SEE { WORDS[item] } HERE.")
            print("="*26)
            print(self.message)

            query = input("WHAT WILL YOU DO NOW? ").upper()
            whitespace = query.find(" ")
            if whitespace >= 0:
                verb = query[:whitespace]
                word = query[whitespace+1:]
            else:
                verb = query
                word = ""

            try:
                vb = VERBS.index(verb)
            except ValueError:
                vb = 0
            try:
                ob = WORDS.index(word)
            except ValueError:
                ob = 0

            self.message = "WHAT?"
            if word and not ob:
                self.message = "THAT'S SILLY"
            if not word:
                self.message = "I NEED TWO WORDS"
            if not vb and not ob:
                self.message = "YOU DON'T MAKE SENSE"
            elif not vb:
                self.message = f"YOU CAN'T '{ query }'"
            if vb and ob and not self.backpack.get(ob):
                self.message = f"YOU DON'T HAVE '{ word }'"

            if not vb:
                continue

            if self.bats_attacking and self.room==13 and vb!=21 \
               and random.randint(0,2)==0:
                self.message = "BATS ATTACKING!"
                continue

            if self.room==44 and random.randint(0,1)==0 and not self.vacuum_on:
                self.ghosts_present = True

            if self.light_on:
                self.light_time -=1
                if self.light_time == 0:
                    self.light_on = False

            self.handlers[vb-1](vb, ob, word)

            if self.light_on and self.light_time == 10:
                self.message = "YOUR CANDLE IS WANING!"
            if self.light_on and self.light_time == 1:
                self.message = "YOUR CANDLE IS OUT!"

    def verb_help(self, vb: int, ob: int, word: str) -> None:
        print("WORDS I KNOW:")
        print(", ".join(VERBS))
        self.message = "OK"

    def verb_carrying(self, vb: int, ob: int, word: str) -> None:
        print("YOU ARE CARRYING:")
        print(", ".join(WORDS[i] for i in self.backpack.keys()))

    def verb_go(self, vb: int, ob: int, word: str) -> None:
        dir = 0
        if vb==4 or ob==19:
            dir = 1
        elif vb==5 or ob==20:
            dir = 2
        elif vb==6 or ob==21:
            dir = 3
        elif vb==7 or ob==22:
            dir = 4
        elif vb==8 or ob==23:
            dir = 5
        elif vb==9 or ob==24:
            dir = 6

        if self.room == 20:
            if dir == 5:
                dir = 1
            if dir == 6:
                dir = 3
        elif self.room == 22:
            if dir == 6:
                dir = 2
            if dir == 5:
                dir = 3
        elif self.room == 36:
            if dir == 6:
                dir = 1
            if dir == 5:
                dir = 2

        if self.top_of_tree:
            self.message = "CRASH! YOU FELL OUT OF THE TREE"
            top_of_tree = False
            return

        if self.ghosts_present and self.room == 52:
            self.message = "GHOSTS WILL NOT LET YOU MOVE"
            return

        if self.room == 45 and 1 in self.backpack and self.spell_discovered:
            self.message = "A MAGICAL BARRIER TO THE WEST"
            return

        if self.room == 26 and not self.light_on and (dir == 1 or dir == 4):
            self.message = "YOU NEED A LIGHT"
            return

        if self.room == 54 and not 15 in self.backpack:
            self.message = "YOU ARE STUCK"
            return

        if 15 in self.backpack and not self.room in (47, 53, 54, 55):
            self.message = "YOU CAN'T CARRY THE BOAT"
            return

        if self.room > 26 and self.room < 30 and not self.light_on:
            self.message = "TOO DARK TO MOVE"
            return

        if dir < 1:
            self.message = "GO WHERE?"
        elif 1 <= dir <= 4 and "NSWE"[dir-1] in self.exits[self.room]:
            self.message = "OK"
            if dir == 1:
                self.room -= 8
            elif dir == 2:
                self.room += 8
            elif dir == 3:
                self.room -= 1
            else:
                self.room += 1
        else:
            self.message = "YOU CAN'T GO THAT WAY"

        if self.room==41 and not self.front_door_shut:
            self.front_door_shut = True
            self.exits[49] = "SW"
            self.message = "THE DOOR SLAMS SHUT!"

    def verb_get(self, vb: int, ob: int, word: str) -> None:
        if ob in self.backpack:
            self.message = "YOU ALREADY HAVE IT"
        elif ob in self.item_hidden:
            self.message = f"WHAT { word }?"
        elif self.item_location.get(ob) != self.room:
            self.message = "IT ISN'T HERE"
        elif self.item_location.get(ob) == self.room:
            self.backpack[ob] = True
            del self.item_location[ob]
            self.message = f"YOU HAVE THE { word }"

    def verb_open(self, vb: int, ob: int, word: str) -> None:
        if self.room==43 and ob in (28,29):
            del self.item_hidden[17]
            self.message = "DRAWER OPEN"
        elif self.room==28 and ob==25:
            self.message = "IT'S LOCKED"
        elif self.room==38 and ob==32:
            del self.item_hidden[2]
            self.message = "THAT'S CREEPY"

    def verb_read(self, vb: int, ob: int, word: str) -> None:
        if self.room==42 and ob==33:
            self.message = "THEY ARE DEMONIC WORKS"
        elif ob in (3,36) and 3 in self.backpack and not self.spell_discovered:
            self.message = "USE THIS WORD WITH CARE 'XZANFAR'"
        elif ob==5 and ob in self.backpack:
            self.message = "THE SCRIPT IS IN AN ALIEN TONGUE"

    def verb_examine(self, vb: int, ob: int, word: str) -> None:
        if ob==30:
            del self.item_hidden[18]
            self.message = "SOMETHING HERE!"
        elif ob==31:
            self.message = "THAT'S DISGUSTING!"
        elif ob in (28, 29):
            self.message = "THERE IS A DRAWER"
        elif ob in (5, 33):
            self.verb_read(vb, ob, word)
        elif self.room==43 and ob==35:
            self.message = "THERE IS SOMETHING BEYOND..."
        elif word==32:
            self.verb_open(vb, ob, word)

    def verb_say(self, vb: int, ob: int, word: str) -> None:
        self.message = f"OK, '{ word }'"
        if 3 in self.backpack and ob==34:
            self.message = "*MAGIC OCCURS*"
            if self.room!=45:
                self.spell_discovered=True
            else:
                self.room = random.randint(0, 63)

    def verb_dig(self, vb: int, ob: int, word: str) -> None:
        if 12 in self.backpack and self.room==30:
            self.message = "YOU DUG THE BARS OUT"
            self.exits[self.room]="NSE"
            self.rooms[self.room]="HOLE IN WALL"
        elif self.room != 30:
            self.message = "YOU MADE A HOLE"

    def verb_swing(self, vb: int, ob: int, word: str) -> None:
        if not 14 in self.backpack and self.room==7:
            self.message = "THIS IS NO TIME TO PLAY GAMES"
        elif ob==14 and ob in self.backpack:
            self.message = "YOU SWUNG IT"
        elif ob==13 and ob in self.backpack:
            self.message= "WHOOSH!"
        elif ob==13 and ob in self.backpack and self.room==43:
            self.exits[self.room]="WN"
            self.rooms[self.room]="STUDY WITH SECRET SELF.ROOM"
            self.message = "YOU BROKE THE THIN WALL"

    def verb_climb(self, vb: int, ob: int, word: str) -> None:
        if ob==14:
            if ob in self.backpack:
                self.message = "IT ISN'T ATTACHED TO ANYTHING!"
            elif self.room==7 and not self.top_of_tree:
                self.message = "YOU SEE THICK FOREST AND CLIFF SOUTH"
                self.top_of_tree = True
            elif self.room==7 and self.top_of_tree:
                self.message = "GOING DOWN!"
                self.top_of_tree = False

    def verb_light(self, vb: int, ob: int, word: str) -> None:
        if ob==17 and ob in self.backpack:
            if not 8 in self.backpack:
                self.message = "IT WILL BURN YOUR HANDS"
            elif not 9 in self.backpack:
                self.message = "NOTHING TO LIGHT IT WITH"
            else:
                self.message = "IT CASTS A FLICKERING LIGHT"
                self.light_on = True

    def verb_unlight(self, vb: int, ob: int, word: str) -> None:
        if self.light_on:
            self.light_on = False
            self.message = "EXTINGUISHED"

    def verb_spray(self, vb: int, ob: int, word: str) -> None:
        if ob==26 and 16 in self.backpack:
            if self.bats_attacking:
                self.bats_attacking = False
                self.message = "PFFT! GOT THEM"
            else:
                self.message = "HISSSS"

    def verb_use(self, vb: int, ob: int, word: str) -> None:
        if ob==10 and ob in self.backpack and 11 in self.backpack:
            self.vacuum_on = True
            self.message = "SWITCHED ON"
        if self.ghosts_present and self.vacuum_on:
            self.message = "WHIZZ - VACUUMED THE GHOSTS UP!"
            self.ghosts_present = False

    def verb_unlock(self, vb: int, ob: int, word: str) -> None:
        if self.room==43 and ob in (27,28):
            self.verb_open(vb, ob, word)
        elif self.room==28 and ob==25 and not self.thick_door_open and 18 in self.backpack:
            self.thick_door_open = True
            self.exits[self.room]="SEW"
            self.rooms[self.room]="HUGE OPEN DOOR"
            self.message = "THE KEY TURNS!"

    def verb_leave(self, vb: int, ob: int, word: str) -> None:
        if ob in self.backpack:
            del self.backpack[ob]
            self.item_location[ob] = self.room
            self.message = "DONE"

    def verb_score(self, vb: int, ob: int, word: str) -> None:
        score = len(self.backpack)
        if score==17:
            if self.room!=57 and not 15 in self.backpack:
                print("YOU HAVE EVERYTHING")
                print("RETURN TO THE GATE FOR FINAL SCORE")
            elif self.room==57:
                print("DOUBLE SCORE FOR REACHING HERE!")
                score *= 2
                print(f"YOUR SCORE IS { score }")
        if score>18:
            print("WELL DONE! YOU FINISHED THE GAME")
            self.game_end=True

if __name__ == "__main__":
    w = World()
    w.run()
