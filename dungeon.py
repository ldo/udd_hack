#+
# Encoding/decoding of dungeon files for udd.
#
# Copyright 2014 by Lawrence D'Oliveiro <ldo@geek-central.gen.nz>.
# Licensed under CC-BY-SA <http://creativecommons.org/licenses/by-sa/4.0/>.
#-

from enum import \
    Enum, \
    IntEnum
import struct

#+
# Useful stuff
#-

def structread(fromfile, decode_struct) :
    "reads sufficient bytes from fromfile to be unpacked according to" \
    " decode_struct, and returns the unpacked results."
    return struct.unpack(decode_struct, fromfile.read(struct.calcsize(decode_struct)))
#end structread

class Bitfield :
    "a bitfield within an integer."

    def __init__(self, shift, width) :
        assert shift >= 0 and width >= 0, "invalid bitfield parameters"
        self.shift = shift
        self.width = width
        self.mask = (1 << width) - 1 << shift
    #end __init__

    def extract(self, val) :
        "extracts the bitfield value from val."
        return \
            (val & self.mask) >> self.shift
    #end extract

    def insert(self, field, val = 0) :
        "returns val with field inserted in bitfield location."
        assert field >= 0 and field < 1 << self.width, "field value will not fit"
        return \
            val | field << self.shift
    #end insert

    def clear(self, val) :
        "returns val with the bitfield location set to all zeroes."
        return \
            val & ~self.mask
    #end clear

#end Bitfield

#+
# Dungeon representation
#-

class DIR(Enum) :
    "spatial directions."
    N = 0 # north
    E = 1 # east
    S = 2 # south
    W = 3 # west
    U = 4 # up
    D = 5 # down
#end DIR

class Dungeon :
    "represents a single dungeon. Use the decode_bytes method to load one" \
    " from the stored file represention, or new_empty to create a new, empty" \
    " one."

    DNAM_SZ = 20 # fixed space for dungeon name, including trailing null
    encoded_data_size = DNAM_SZ + 2 + 8000 # total size of encoded data for a dungeon

    START_CLOSED_REPAIRS = 400 # special start-room code indicating dungeon is closed for repairs

    # the dungeon file contains a byte for each room, with bits representing
    # various room attributes
    ROOM_WEST_SIDE = Bitfield(0, 2) # ROOM_SIDE value for west side
    ROOM_NORTH_SIDE = Bitfield(2, 2) # ROOM_SIDE value for north side
    # south side is of course taken from north side of room to south,
    # and similarly east side from west side of room to east
    ROOM_SPECIAL = Bitfield(4, 4) # special contents for room (0 means none)

    class ROOM_SIDE(IntEnum) :
        "values for ROOM_xxx_SIDE fields."
        OPEN = 0 # room is open on that side
        WALL = 1 # room is walled off on that side
        DOOR = 2 # room passable on that side
        RUBBLE = 3 # room has debris but is passable on that side

        def passable(self) :
            "can player pass through this side."
            return \
                self.value != 1
        #end passable

    #end ROOM_SIDE

    # following are supposed to be dynamic contents only, generated on
    # loading dungeon levels, not saved in dungeon file
    ROOM_MONSTER = Bitfield(8, 1) # there is a monster in the room
    ROOM_TREASURE = Bitfield(9, 1) # there is treasure in the room
    ROOM_TREASURE_BOOBYTRAP = Bitfield(10, 1) # the treasure is booby-trapped

    class SPC(IntEnum) :
        "codes for special room contents."
        NONE = 0 # nothing
        DNS = 1 # down-only staircase
        UPS = 2 # up-only staircase
        UDS = 3 # up/down staircase
        EXC = 4 # Excelsior transporter (goes to random location on selected level)
        PIT = 5 # pit
        TPT = 6 # teleport to random location elsewhere in dungeon
        FTN = 7 # fountain
        ALT = 8 # altar
        DGN1 = 9 # dragon
        DGN2 = 10 # dragon guarding Orb
        ORB = 11 # Orb
        ELV = 12 # elevator (takes you up a level)
        THR = 13 # throne
        SAF = 14 # safe
        RCK = 15 # you find yourself in solid rock when you enter this room
    #end SPC

    class Room :
        "represents a room in the dungeon."

        # attrs defined below

        def __init__(self, parent, l, s, e, code) :
            "decodes the representation of the room state."
            self.parent = parent
            self.l = l
            self.s = s
            self.e = e
            for attr, name, conv in self.attrs :
                setattr(self, attr, conv(getattr(Dungeon, "ROOM_" + name).extract(code)))
            #end for
        #end __init__

        def encode(self) :
            "returns an encoded representation of the room state."
            code = 0
            for attr, name, conv in self.attrs :
                code = getattr(Dungeon, "ROOM_" + name).insert(int(getattr(self, attr)), code)
            #end for
            return \
                code
        #end encode

        def neighbour(self, dir) :
            "returns the neighbouring room in the specified direction," \
            " given by a DIR enum. Returns None if there is no neighbour" \
            " in that direction."
            result = None # to begin with
            if dir == DIR.N :
                if self.s > 0 :
                    result = self.parent.rooms[self.l][self.s - 1][self.e]
                #end if
            elif dir == DIR.E :
                if self.e + 1 < 20 :
                    result = self.parent.rooms[self.l][self.s][self.e + 1]
                #end if
            elif dir == DIR.S :
                if self.s + 1 < 20 :
                    result = self.parent.rooms[self.l][self.s + 1][self.e]
                #end if
            elif dir == DIR.W :
                if self.e > 0 :
                    result = self.parent.rooms[self.l][self.s][self.e - 1]
                #end if
            elif dir == DIR.U :
                if self.l > 0 :
                    result = self.parent.rooms[self.l - 1][self.s][self.e]
                #end if
            elif dir == DIR.D :
                if self.l + 1 < 20 :
                    result = self.parent.rooms[self.l + 1][self.s][self.e]
                #end if
            #end if
            return \
                result
        #end neighbour

        def get_side(self, dir) :
            "returns the ROOM_SIDE value for the side of the room in the specified direction."
            if dir == DIR.N :
                result = self.north_side
            elif dir == DIR.E :
                neighbour = self.neighbour(dir)
                if neighbour != None :
                    result = neighbour.west_side
                else :
                    result = Dungeon.ROOM_SIDE.WALL
                #end if
            elif dir == DIR.W :
                result = self.west_side
            elif dir == DIR.S :
                neighbour = self.neighbour(dir)
                if neighbour != None :
                    result = neighbour.north_side
                else :
                    result = Dungeon.ROOM_SIDE.WALL
                #end if
            else :
                raise ValueError("invalid side")
            #end if
            return \
                result
        #end get_side

        def set_side(self, dir, val) :
            "sets the ROOM_SIDE value for the side of the room in the specified direction."
            if not isinstance(val, Dungeon.ROOM_SIDE) :
                raise ValueError("invalid new value for side")
            #end if
            if dir == DIR.N :
                if self.s > 0 :
                    self.north_side = val
                elif val != Dungeon.ROOM_SIDE.WALL :
                    raise ValueError("north side of northernmost room must be wall")
                #end if
            elif dir == DIR.E :
                neighbour = self.neighbour(dir)
                if neighbour != None :
                    neighbour.west_side = val
                elif val != Dungeon.ROOM_SIDE.WALL :
                    raise ValueError("east side of easternmost room must be wall")
                #end if
            elif dir == DIR.S :
                neighbour = self.neighbour(dir)
                if neighbour != None :
                    neighbour.north_side = val
                elif val != Dungeon.ROOM_SIDE.WALL :
                    raise ValueError("south side of southernmost room must be wall")
                #end if
            elif dir == DIR.W :
                if self.e > 0 :
                    self.west_side = val
                elif val != Dungeon.ROOM_SIDE.WALL :
                    raise ValueError("west side of westernmost room must be wall")
                #end if
            else :
                raise ValueError("invalid side")
            #end if
        #end set_side

        def passable(self, dir) :
            "can player leave room in specified direction."
            result = False # to begin with
            if dir == DIR.N :
                result = self.s > 0 and self.north_side.passable()
            elif dir == DIR.E :
                neighbour = self.neighbour(dir)
                if neighbour != None :
                    result = neighbour.west_side.passable()
                #end if
            elif dir == DIR.S :
                neighbour = self.neighbour(dir)
                if neighbour != None :
                    result = neighbour.north_side.passable()
                #end if
            elif dir == DIR.W :
                result = self.e > 0 and self.west_side.passable()
            elif dir == DIR.U :
                pass # elevator?
            elif dir == DIR.D :
                pass # pit?
            #end if
            return \
                result
        #end passable

        def __repr__(self) :
            return \
                (
                    "[%d, %d, %d] => (%s%s%s%s)"
                %
                    (
                        self.l, self.s, self.e,
                        self.special,
                        (" ", "MONST")[self.monster],
                        (" ", "TREAS")[self.treasure],
                        (" ", "TRAP")[self.treasure_boobytrapped],
                    )
                )
        #end __repr__

    #end Room
    Room.attrs = \
        (
            ("west_side", "WEST_SIDE", ROOM_SIDE),
            ("north_side", "NORTH_SIDE", ROOM_SIDE),
            ("special", "SPECIAL", SPC),
            ("monster", "MONSTER", bool),
            ("treasure", "TREASURE", bool),
            ("treasure_boobytrapped", "TREASURE_BOOBYTRAP", bool),
        )

    def find_special_rooms(self, spc) :
        "iterates over all rooms with the specified special code."
        for l in range(0, 20) :
            for s in range(0, 20) :
                for e in range(0, 20) :
                    room = self.rooms[l][s][e]
                    if room.special == spc :
                        yield room
                    #end if
                #end for
            #end for
        #end for
    #end find_special_rooms

    @classmethod
    def new_empty(cself, name) :
        "creates a Dungeon with no start room, no inner walls and no specials."
        assert len(name) < cself.DNAM_SZ, "name too long"
        result = Dungeon()
        result.name = name
        result.start = None
        result.rooms = []
        for l in range(0, 20) :
            level = []
            for s in range(0, 20) :
                row = []
                for e in range(0, 20) :
                    row.append \
                      (
                        cself.Room
                          (
                            parent = result,
                            l = l,
                            s = s,
                            e = e,
                            code =
                                    cself.ROOM_WEST_SIDE.insert(e == 0)
                                |
                                    cself.ROOM_NORTH_SIDE.insert(s == 0),
                          )
                      )
                #end for
                level.append(row)
            #end for
            result.rooms.append(level)
        #end for
        return \
            result
    #end new_empty

    @classmethod
    def decode_bytes(cself, b) :
        "creates a Dungeon from its encoded representation."
        assert len(b) == cself.encoded_data_size, "wrong size of encoded data for dungeon"
        result = Dungeon()
        name = b[:cself.DNAM_SZ]
        name = name[:name.index(b"\0")]
        result.name = name.decode("ascii")
        start = struct.unpack(">H", b[cself.DNAM_SZ : cself.DNAM_SZ + 2])[0]
        if start == cself.START_CLOSED_REPAIRS :
            result.start = None
        else :
            result.start = (0, start // 20, start % 20)
        #end if
        result.rooms = []
        for l in range(0, 20) :
            level = []
            for s in range(0, 20) :
                row = []
                for e in range(0, 20) :
                    row.append \
                      (
                        cself.Room(result, l, s, e, b[cself.DNAM_SZ + 2 + l * 400 + s * 20 + e])
                      )
                #end for
                level.append(row)
            #end for
            result.rooms.append(level)
        #end for
        return \
            result
    #end decode_bytes

    def encode_bytes(self) :
        "returns the complete encoded representation of the dungeon."
        assert len(self.name) < self.DNAM_SZ, "dungeon name is too long"
        rooms = []
        for l in range(0, 20) :
            for s in range(0, 20) :
                for e in range(0, 20) :
                    rooms.append(self.rooms[l][s][e].encode())
                #end for
            #end for
        #end for
        if self.start != None :
            assert self.start[0] == 0, "start must be on top level"
            start = self.start[1] * 20 + self.start[2]
        else :
            start = self.START_CLOSED_REPAIRS
        #end if
        return \
            (
                struct.pack(">%dsH" % self.DNAM_SZ, self.name.encode("ascii"), start)
            +
                bytes(rooms)
            )
    #end encode_bytes

    def __getitem__(self, c) :
        "allow direct indexing of rooms as Dungeon[l, s, e]."
        assert len(c) == 3, "need exactly 3 coordinates"
        return \
            self.rooms[c[0]][c[1]][c[2]]
    #end __getitem__

#end Dungeon

class DungeonFile :
    "represents the entire contents of a dungeon file. The dungeons field" \
    " is a list of Dungeon instances. Use the load method to create a DungeonFile" \
    " object from the contents of a dungeon file, or the constructor to create" \
    " a new, empty DungeonFile object."

    def __init__(self) :
        self.nr_dungeons = 0
        self.dungeons = []
    #end __init__

    @classmethod
    def load(cself, filename) :
        "loads the contents of a dungeon file into a new DungeonFile object and returns it."
        f = open(filename, "rb")
        nr_dungeons = structread(f, ">I")[0]
        dungeons = []
        for i in range(0, nr_dungeons) :
            data = f.read(Dungeon.encoded_data_size)
            if len(data) < Dungeon.encoded_data_size :
                raise RuntimeError \
                  (
                        "only got %d bytes, expecting %d bytes"
                    %
                        (len(data), Dungeon.encoded_data_size)
                  )
            #end if
            dungeons.append(Dungeon.decode_bytes(data))
        #end for
        result = DungeonFile()
        result.dungeons = dungeons
        return \
            result
    #end load

    def save(self, filename) :
        "encodes and saves the DungeonFile object into a file."
        f = open(filename, "wb")
        f.write(struct.pack(">I", len(self.dungeons)))
        for dungeon in self.dungeons :
            f.write(dungeon.encode_bytes())
        #end for
        f.flush()
        f.close()
    #end save

#+
# Transparent access to dungeons list:
#-

    def __len__(self) :
        return \
            len(self.dungeons)
    #end __len__

    def __getitem__(self, i) :
        return \
            self.dungeons[i]
    #end __getitem__

    def __setitem__(self, i, val) :
        assert isinstance(val, Dungeon)
        self.dungeons[i] = val
    #end __setitem__

    def __delitem__(self, i) :
        del self.dungeons[i]
    #end __delitem__

    def __iter__(self) :
        return \
            iter(self.dungeons)
    #end __iter__

    def append(self, val) :
        assert isinstance(val, Dungeon)
        self.dungeons.append(val)
    #end append

#end DungeonFile
