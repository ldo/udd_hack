#+
# Encoding/decoding of dungeon files for udd.
#
# Copyright 2014 by Lawrence D'Oliveiro <ldo@geek-central.gen.nz>.
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

    def insert(self, field, val) :
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
    "represents a single dungeon."

    DNAM_SZ = 20 # fixed space for dungeon name, including trailing null
    encoded_data_size = DNAM_SZ + 2 + 8000 # total size of encoded data for a dungeon

    # the dungeon file contains a byte for each room, with bits representing
    # various room attributes
    ROOM_WEST_SIDE = Bitfield(0, 2) # ROOM_SIDE value for west side
    ROOM_NORTH_SIDE = Bitfield(2, 2) # ROOM_SIDE value for north side
    # south side is of course taken from north side of room to south,
    # and similarly east side from west side of room to east
    ROOM_SPECIAL = Bitfield(4, 4) # special contents for room (0 means none)

    class ROOM_SIDE(IntEnum) :
        "values for ROOM_xxx_SIDE fields"
        OPEN = 0 # room is open on that side
        WALL = 1 # room is walled off on that side
        DUNNO = 2 # room passable on that side
        RUBBLE = 3 # room has debris but is passable on that side
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
        RCK= 15 # you find yourself in solid rock when you enter this room
    #end SPC

    class Room :
        "represents a room in the dungeon."

        def __init__(self, parent, l, s, e, mask) :
            "decodes the representation of the room state."
            self.parent = parent
            self.l = l
            self.s = s
            self.e = e
            for attr, name, conv in self.attrs :
                setattr(self, attr, conv(getattr(Dungeon, "ROOM_" + name).extract(mask)))
            #end for
        #end __init__

        def encode(self) :
            "returns an encoded representation of the room state."
            mask = 0
            for attr, name, conv in self.attrs :
                mask = getattr(Dungeon, "ROOM_" + name).insert(int(getattr(self, attr)), mask)
            #end for
            return \
                mask
        #end encode

        def neighbour(self, dir) :
            "returns the neighbouring room in the specified direction, given by a DIR enum."
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
    def decode_bytes(cself, b) :
        assert len(b) == cself.encoded_data_size, "wrong size of encoded data for dungeon"
        result = Dungeon()
        name = b[:cself.DNAM_SZ]
        name = name[:name.index(b"\0")]
        result.name = name.decode("ascii")
        start = struct.unpack(">H", b[cself.DNAM_SZ : cself.DNAM_SZ + 2])[0]
        if start == 400 :
            result.start = None # closed for repairs
        else :
            result.start = (start // 20 + 1, start % 20 + 1)
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
            start = self.start[0] * 20 + self.start[1]
        else :
            start = 400 # closed for repairs
        #end if
        return \
            (
                struct.pack(">%dsH" % self.DNAM_SZ, self.name.encode("ascii"), start)
            +
                bytes(rooms)
            )
    #end encode_bytes

#end Dungeon

class DungeonFile :
    "represents the entire contents of a dungeon file." \
    " The dungeons field is a list of Dungeon instances."

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

#end DungeonFile
