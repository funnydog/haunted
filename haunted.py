#!/usr/bin/env python3
#
# The Haunted House
# adapted from "Write your own adventure programs" by Usborne.

from typing import Union

import random

OBJECTS = [
    ("PAINTING",     46,   False), # 1
    ("RING",         38,   True),  # 2
    ("MAGIC SPELLS", 35,   False), # 3
    ("GOBLET",       50,   False), # 4
    ("SCROLL",       13,   False), # 5
    ("COINS",        18,   False), # 6
    ("STATUE",       28,   False), # 7
    ("CANDLESTICK",  42,   False), # 8
    ("MATCHES",      10,   False), # 9
    ("VACUUM",       25,   False), # 10
    ("BATTERIES",    26,   False), # 11
    ("SHOVEL",       4,    False), # 12
    ("AXE",          2,    False), # 13
    ("ROPE",         7,    False), # 14
    ("BOAT",         47,   False), # 15
    ("AEROSOL",      60,   False), # 16
    ("CANDLE",       43,   True),  # 17
    ("KEY",          32,   True),  # 18
    ("NORTH",        None, False), # 19
    ("SOUTH",        None, False), # 20
    ("WEST",         None, False), # 21
    ("EAST",         None, False), # 22
    ("UP",           None, False), # 23
    ("DOWN",         None, False), # 24
    ("DOOR",         None, False), # 25
    ("BATS",         None, False), # 26
    ("GHOSTS",       None, False), # 27
    ("DRAWER",       None, False), # 28
    ("DESK",         None, False), # 29
    ("COAT",         None, False), # 30
    ("RUBBISH",      None, False), # 31
    ("COFFIN",       None, False), # 32
    ("BOOKS",        None, False), # 33
    ("XZANFAR",      None, False), # 34
    ("WALL",         None, False), # 35
    ("SPELLS",       None, False), # 36
]

ROOMS = [
    ("DARK CORNER",       "SE"), # 0
    ("OVERGROWN GARDEN",  "WE"), # 1
    ("BY LARGE WOODPILE", "WE"), # 2
    ("YARD BY RUBBISH",   "SWE"), # 3
    ("WEEDPATCH",         "WE"),  # 4
    ("FOREST",            "WE"),  # 5
    ("THICK FOREST",      "SWE"), # 6
    ("BLASTED TREE",      "WS"),  # 7

    ("CORNER OF HOUSE",          "NS"), # 8
    ("ENTRANCE TO KITCHEN",      "SE"), # 9
    ("KITCHEN & GRIMY COOKER",   "WE"), # 10
    ("SCULLERY DOOR",            "NW"), # 11
    ("ROOM WITH INCHES OF DUST", "SE"), # 12
    ("REAR TURRET ROOM",         "W"),  # 13
    ("CLEARING BY HOUSE",        "NE"), # 14
    ("PATH",                     "NSW"), # 15

    ("SIDE OF THE HOUSE",          "NS"), # 16
    ("BACK OF HALLWAY",            "NS"), # 17
    ("DARK ALCOVE",                "SE"), # 18
    ("SMALL DARK ROOM",            "WE"), # 19
    ("BOTTOM OF SPIRAL STAIRCASE", "NWUD"), # 20
    ("WIDE PASSAGE",               "SE"),   # 21
    ("SLIPPERY STEPS",             "WSUD"), # 22
    ("CLIFFTOP",                   "NS"),   # 23

    ("NEAR CRUMBLING WALL",        "N"), # 24
    ("GLOOMY PASSAGE",             "NS"), # 25
    ("POOL OF LIGHT",              "NSE"), # 26
    ("IMPRESSIVE VAULTED HALLWAY", "WE"),  # 27
    ("HALL BY THICK WOODEN DOOR",  "WE"),  # 28
    ("TROPHY ROOM",                "NSW"), # 29
    ("CELLAR WITH BARRED WINDOW",  "NS"),  # 30
    ("CLIFF PATH",                 "NS"),  # 31

    ("CUPBOARD WITH HANGING COAT", "S"), # 32
    ("FRONT HALL",                 "NSE"), # 33
    ("SITTING ROOM",               "NSW"), # 34
    ("SECRET ROOM",                "S"),   # 35
    ("STEEP MARBLE STAIRS",        "NSUD"), # 36
    ("DINING ROOM",                "N"),    # 37
    ("DEEP CELLAR WITH COFFIN",    "N"),    # 38
    ("CLIFF PATH",                 "NS"),   # 39

    ("CLOSET",                         "NE"), # 40
    ("FRONT LOBBY",                    "NSW"), # 41
    ("LIBRARY OF EVIL BOOKS",          "NE"),  # 42
    ("STUDY WITH DESK & HOLE IN WALL", "W"),   # 43
    ("WEIRD COBWEBBY ROOM",            "NSE"), # 44
    ("VERY COLD CHAMBER",              "WE"),  # 45
    ("SPOOKY ROOM",                    "W"),   # 46
    ("CLIFF PATH BY MARSH",            "NS"),  # 47

    ("RUBBLE-STREWN VERANDAH", "SE"), # 48
    ("FRONT PORCH",            "NSW"), # 49
    ("FRONT TOWER",            "E"),   # 50
    ("SLOPING CORRIDOR",       "WE"),  # 51
    ("UPPER GALLERY",          "NW"),  # 52
    ("MARSH BY WALL",          "S"),   # 53
    ("MARSH",                  "SW"),  # 54
    ("SOGGY PATH",             "NW"),  # 55

    ("BY TWISTED RAILINGS",          "NE"), # 56
    ("PATH THROUGH IRON GATE",       "NWE"), # 57
    ("BY RAILINGS",                  "WE"),  # 58
    ("BENEATH THE FRONT TOWER",      "WE"),  # 59
    ("DEBRIS FROM CRUMBLING FACADE", "WE"),  # 60
    ("LARGE FALLEN BRICKWORK",       "NWE"), # 61
    ("ROTTING STONE ARCH",           "NWE"), # 62
    ("CRUMBLING CLIFFTOP",           "W"),   # 63
]

class Room(object):
    def __init__(self, name, directions, items = None):
        self.name = name.strip()
        self.directions = directions.strip()

    def __repr__(self):
        return "[{}, {}]".format(self.description, self.directions)

class Item(object):
    def __init__(self, name: str, location: Union[int,None], hidden: bool) -> None:
        self.name = name
        self.location = location
        self.hidden = hidden

class World(object):
    def __init__(self) -> None:
        self.rooms = []
        for name, dir in ROOMS:
            self.rooms.append(Room(name, dir))

        self.items = {}
        for name, loc, hidden in OBJECTS:
            self.items[name] = Item(name, loc, hidden)

        self.verbs = {
            "HELP": self.verb_help,
            "CARRYING?": self.verb_carrying,
            "GO": self.verb_go,
            "N": self.verb_go,
            "S": self.verb_go,
            "W": self.verb_go,
            "E": self.verb_go,
            "U": self.verb_go,
            "D": self.verb_go,
            "GET": self.verb_get,
            "TAKE": self.verb_get,
            "OPEN": self.verb_open,
            "EXAMINE": self.verb_examine,
            "READ": self.verb_read,
            "SAY": self.verb_say,
            "DIG": self.verb_dig,
            "SWING": self.verb_swing,
            "CLIMB": self.verb_climb,
            "LIGHT": self.verb_light,
            "UNLIGHT": self.verb_unlight,
            "SPRAY": self.verb_spray,
            "USE": self.verb_use,
            "UNLOCK": self.verb_unlock,
            "LEAVE": self.verb_leave,
            "SCORE": self.verb_score,
        }
        self.backpack: dict[str,bool] = {}
        self.light_on = False
        self.light_time = 60
        self.room_idx = 57

    def gameloop(self) -> None:
        print("HAUNTED HOUSE")
        print("-------------")
        self.message = "OK"
        while True:
            self.room = self.rooms[self.room_idx]
            print(f"YOUR LOCATION\n{ self.room.name }")
            print("EXITS: " + ",".join(r for r in self.room.directions))
            for name, obj in self.items.items():
                if not obj.hidden and obj.location == self.room_idx:
                    print(f"I SEE { name } HERE.")
            print("="*26)
            print(self.message)

            query = input("WHAT WILL YOU DO NOW? ").upper()
            words = query.split(" ")
            verb = words[0]
            word = " ".join(words[1:])

            self.message = "WHAT?"
            action = self.verbs.get(verb)
            item = self.items.get(word)
            if item is None:
                self.message = "THAT'S SILLY"
            if word == "":
                self.message = "I NEED TWO WORDS"
            if action is None and item:
                self.message = f"YOU CAN'T '{ query }'"
            if action is None and item:
                self.message = "YOU DON'T MAKE SENSE"
            if action and item and not word in self.backpack:
                self.message = f"YOU DON'T HAVE '{ word }'"

            if self.items["BATS"].hidden and \
               self.room_idx == 13 and random.randint(0,3)==3 and verb != "USE":
                self.message = "BATS ATTACKING!"
                continue

            if self.room_idx == 44 and random.choice((True, False)) and \
               not self.items["DOWN"].hidden:
                self.items["GHOSTS"].hidden = True

            if self.light_on:
                self.light_time -=1

            if self.light_time == 0:
                self.light_on = False

            if action:
                action(verb, word)

            if self.light_on and self.light_time == 10:
                self.message += "\nYOUR CANDLE IS WANING!"
            if self.light_on and self.light_time == 1:
                self.message += "\nYOUR CANDLE IS OUT"

    def verb_help(self, verb: str, word: str) -> None:
        print("WORDS I KNOW:")
        for verb in self.verbs.keys():
            print(verb)
        self.message = "OK"

    def verb_carrying(self, verb: str, word: str) -> None:
        print("YOU ARE CARRYING:")
        for item in self.backpack.keys():
            print(item)

    def verb_go(self, verb: str, word: str) -> None:
        dir = 0
        if verb == "N" or word == "NORTH":
            dir = 1
        elif verb == "S" or word == "SOUTH":
            dir = 2
        elif verb == "W" or word == "WEST":
            dir = 3
        elif verb == "E" or word == "EAST":
            dir = 4
        elif verb == "U" or word == "UP":
            dir = 5
        elif verb == "D" or word == "DOWN":
            dir = 6
        if self.room_idx == 20:
            if dir == 5:
                dir = 1
            if dir == 6:
                dir = 3
        elif self.room_idx == 22:
            if dir == 6:
                dir = 2
            if dir == 5:
                dir = 3
        elif self.room_idx == 36:
            if dir == 6:
                dir = 1
            if dir == 5:
                dir = 2
        if self.items["ROPE"].hidden:
            self.message = "CRASH! YOU FELL OUT OF THE TREE"
            self.items["ROPE"].hidden = False
            return

        if self.items["GHOSTS"].hidden and self.room_idx == 52:
            self.message = "GHOSTS WILL NOT LET YOU MOVE"
            return

        if self.room_idx == 45 and "PAINTING" in self.backpack and \
           not self.items["XZANFAR"].hidden:
            self.message = "A MAGICAL BARRIER TO THE WEST"
            return

        if self.room_idx == 26 and not self.light_on and (dir == 1 or dir == 4):
            self.message = "YOU NEED A LIGHT"
            return

        if self.room_idx == 54 and not "BOAT" in self.backpack:
            self.message = "YOU ARE STUCK"
            return

        if "BOAT" in self.backpack and not self.room_idx in (47, 54, 54, 55):
            self.message = "YOU CAN'T CARRY THE BOAT"
            return

        if self.room_idx > 26 and self.room_idx < 30 and not self.light_on:
            self.message = "TOO DARK TO MOVE"
            return

        if dir == 1 and "N" in self.room.directions:
            self.room_idx -= 8
        elif dir == 2 and "S" in self.room.directions:
            self.room_idx += 8
        elif dir == 3 and "W" in self.room.directions:
            self.room_idx -= 1
        elif dir == 4 and "E" in self.room.directions:
            self.room_idx += 1
        self.message = "OK"

    def verb_get(self, verb: str, word: str) -> None:
        if word in self.backpack:
            self.message = "YOU ALREADY HAVE IT"
        elif not word in self.items or self.items[word].hidden:
            self.message = f"WHAT { word }?"
        elif self.room_idx != self.items[word].location:
            self.message = "IT ISN'T HERE"
        else:
            self.backpack[word] = True
            self.items[word].location = None
            self.message = f"YOU HAVE THE { word }"

    def verb_open(self, verb: str, word: str) -> None:
        if self.room_idx == 43 and (word == "DRAWER" or word == "DESK"):
            self.items["CANDLE"].hidden = False
            self.message = "DRAWER OPEN"
        elif self.room_idx == 28 and word == "DOOR":
            self.message = "IT'S LOCKED"
        elif self.room_idx == 38 and word == "COFFIN":
            self.message = "THAT'S CREEPY"
            self.items["RING"].hidden = False

    def verb_examine(self, verb: str, word: str) -> None:
        if word == "COAT":
            self.items["KEY"].hidden = False
            self.message = "SOMETHING HERE!"
        elif word == "RUBBISH":
            self.message = "THAT'S DISGUSTING"
        elif word == "DRAWER" or word == "DESK":
            self.message = "THERE IS A DRAWER"
        elif word == "BOOKS" or word == "SCROLL":
            self.verb_read(verb, word)
        elif self.room_idx == 43 and word == "WALL":
            self.message = "THERE IS SOMETHING BEYOND..."
        elif word == "COFFIN":
            self.verb_open(verb, word)

    def verb_read(self, verb: str, word: str) -> None:
        if word == "BOOKS" and self.room_idx == 42:
            self.message = "THEY ARE DEMONIC WORKS"
        elif (word == "SPELLS" or word == "MAGIC SPELLS") and \
             "MAGIC SPELLS" in self.backpack and not self.items["XZANFAR"].hidden:
            self.message = "USE THIS WORD WITH CARE 'XZANFAR'"
        elif "SCROLL" in self.backpack and word == "SCROLL":
            self.message = "THE SCRIPT IS IN AN ALIEN TONGUE"

    def verb_say(self, verb: str, word: str) -> None:
        self.message = f"OK, '{ word }'"
        if "MAGIC SPELLS" in self.backpack and word == "XZANFAR":
            self.message = "*MAGIC OCCURS*"
            if self.room_idx == 45:
                self.room_idx = random.randint(0,63)
            else:
                self.items["XZANFAR"].hidden = True

    def verb_dig(self, verb: str, word: str) -> None:
        if not "SHOVEL" in self.backpack:
            pass
        elif self.room_idx == 30:
            self.message = "DUG THE BARS OUT"
            self.room.directions = "NSE"
            self.room.name = "HOLE IN THE WALL"
        else:
            self.message = "YOU MADE A HOLE"

    def verb_swing(self, verb: str, word: str) -> None:
        if not "ROPE" in self.backpack and self.room_idx == 7:
            self.message = "THIS IS NO TIME TO PLAY GAMES"
        elif word == "ROPE" and "ROPE" in self.backpack:
            self.message = "YOU SWUNG IT"
        elif word == "AXE" and "AXE" in self.backpack:
            if self.room_idx == 43:
                self.room.name = "STUDY WITH SECRET ROOM"
                self.room.directions = "WN"
                self.message = "YOU BROKE THE THIN WALL"
            else:
                self.message = "WHOOSH!"

    def verb_climb(self, verb: str, word: str) -> None:
        if word != "ROPE":
            pass
        elif word in self.backpack:
            self.message = "IT ISN'T ATTACHED TO ANYTHING!"
        elif self.room_idx != 7:
            pass
        elif self.items["ROPE"].hidden:
            self.message = "GOING DOWN!"
            self.items["ROPE"].hidden = False
        else:
            self.message = "YOU SEE THICK FOREST AND CLIFF SOUTH"
            self.items["ROPE"].hidden = True

    def verb_light(self, verb: str, word: str) -> None:
        if word != "CANDLE" or not word in self.backpack:
            pass
        elif "CANDLESTICK" in self.backpack and "MATCHES" in self.backpack:
            self.message = "IT CASTS A FLICKERING LIGHT"
            self.light_on = True
        elif not "MATCHES" in self.backpack:
            self.message = "NOTHING TO LIGHT IT WITH"
        elif not "CANDLESTICK" in self.backpack:
            self.message = "IT WILL BURN YOUR HANDS"

    def verb_unlight(self, verb: str, word: str) -> None:
        if self.light_on:
            self.light_on = False
            self.message = "EXTINGUISHED"

    def verb_spray(self, verb: str, word: str) -> None:
        if word != "BATS" or not "AEROSOL" in self.backpack:
            pass
        elif self.items["BATS"].hidden:
            self.items["BATS"].hidden = False
            self.message = "PFFT! GOT THEM"
        else:
            self.message = "HISS"

    def verb_use(self, verb: str, word: str) -> None:
        if word != "VACUUM" or not word in self.backpack \
           or not "BATTERIES" in self.backpack:
            return

        self.message = "SWITCHED ON"
        self.items["DOWN"].hidden = True
        if self.items["GHOSTS"].hidden and self.items["DOWN"].hidden:
            self.message = "WHIZZ - VACUUMED THE GHOSTS UP!"
            self.items["GHOSTS"].hidden = False

    def verb_unlock(self, verb: str, word: str) -> None:
        if self.room_idx == 28 and not self.items["DOOR"].hidden and "KEY" in self.backpack:
            self.items["DOOR"].hidden = True
            self.room.name = "HUGE OPEN DOOR"
            self.room.directions = "SEW"
            self.message = "THE KEY TURNS!"

    def verb_leave(self, verb: str, word: str) -> None:
        if word in self.backpack:
            del self.backpack[word]
            self.items[word].location = self.room_idx
            self.message = "DONE"

    def verb_score(self, verb: str, word: str) -> None:
        score = len(self.backpack)
        if score==17:
            if self.room_idx != 57 and not "BOAT" in self.backpack:
                print("YOU HAVE EVERYTHING")
                print("RETURN TO THE GATE FOR FINAL SCORE")
            elif self.room_idx == 57:
                print("DOUBLE SCORE FOR REACHING HERE!")
                score *= 2
        print(f"YOUR SCORE IS { score }")

if __name__ == "__main__":
    game = World()
    game.gameloop()
