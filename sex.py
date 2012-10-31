# Sublime2 plugin for SEXIT
# (c) 2012 wisdio SA
# by Wojciech Kaczmarek frk@wisdio.com

from popen2 import popen2
from transform import Transformer

SexPath = "$HOME/hack/sexit/sex"

def do_sex(s0):
  i,o = popen2(SexPath)
  o.write(s0)
  o.close()
  s = i.read()
  i.close()
  return s

class SexCommand(Transformer):
  transformer = do_sex,

# put something like this into your user keymap:
# { "keys": ["super+q"], "command":"sex" }
