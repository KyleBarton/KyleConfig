-- Module to map keyboard keys. Some have been specially mapped (see
-- capslock), so this is meant to represent what's actually on my
-- keyboard
local keysMod = {}
keysMod.CAPS_LOCK = 'f18'
keysMod.ESCAPE = 'escape'
keysMod.LOWER_A = 'a'
keysMod.LOWER_B = 'b'
keysMod.LOWER_C = 'c'
keysMod.LOWER_D = 'd'
keysMod.LOWER_E = 'e'
keysMod.LOWER_F = 'f'
keysMod.LOWER_G = 'g'
keysMod.LOWER_H = 'h'
keysMod.LOWER_I = 'i'
keysMod.LOWER_J = 'j'
keysMod.LOWER_K = 'k'
keysMod.LOWER_L = 'l'
keysMod.LOWER_M = 'm'
keysMod.LOWER_N = 'n'
keysMod.LOWER_O = 'o'
keysMod.LOWER_P = 'p'
keysMod.LOWER_Q = 'q'
keysMod.LOWER_R = 'r'
keysMod.LOWER_S = 's'
keysMod.LOWER_T = 't'
keysMod.LOWER_U = 'u'
keysMod.LOWER_V = 'v'
keysMod.LOWER_W = 'w'
keysMod.LOWER_X = 'x'
keysMod.LOWER_Y = 'y'
keysMod.LOWER_Z = 'z'

keysMod.UPPER_A = 'A'
keysMod.UPPER_B = 'B'
keysMod.UPPER_C = 'C'
keysMod.UPPER_D = 'D'
keysMod.UPPER_E = 'E'
keysMod.UPPER_F = 'F'
keysMod.UPPER_G = 'G'
keysMod.UPPER_H = 'H'
keysMod.UPPER_I = 'I'
keysMod.UPPER_J = 'J'
keysMod.UPPER_K = 'K'
keysMod.UPPER_L = 'L'
keysMod.UPPER_M = 'M'
keysMod.UPPER_N = 'N'
keysMod.UPPER_O = 'O'
keysMod.UPPER_P = 'P'
keysMod.UPPER_Q = 'Q'
keysMod.UPPER_R = 'R'
keysMod.UPPER_S = 'S'
keysMod.UPPER_T = 'T'
keysMod.UPPER_U = 'U'
keysMod.UPPER_V = 'V'
keysMod.UPPER_W = 'W'
keysMod.UPPER_X = 'X'
keysMod.UPPER_Y = 'Y'
keysMod.UPPER_Z = 'Z'
local topLevelMod = {}

-- Change to 'debug' to get more information on the hs console. Might
-- want to find a way to more dynamically set this in the future.
topLevelMod.LOG_LEVEL = 'info'

topLevelMod.keys = keysMod

return topLevelMod
