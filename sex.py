# Sublime2 plugin for SEXIT
# (c) 2012 wisdio SA
# by Wojciech Kaczmarek frk@wisdio.com

from os import environ
from popen2 import popen2
from transform import Transformer

PluginPath = environ['HOME'] + '/Library/Application\ Support/Sublime\ Text\ 2/Packages/User'
# ^^ spaces must be bashslashed for popen2
SexPath = PluginPath + '/sex'

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
# { "keys": ["shift+ctrl+q"], "command":"sex" }
