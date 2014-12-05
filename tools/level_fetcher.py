import urllib3
import re
import json
import sys

def usage():
  print('USAGE: ./level_fetcher.py <puzzleid>')
  print('options: --nocom')
  print('         --json')
  sys.exit(1)

urlbase = 'http://www.robozzle.com/js/play.aspx?puzzle='

if len(sys.argv) < 2:
  usage()

for arg in sys.argv[2:]:
  if arg not in ['--nocom', '--json']:
    print('unknown option: "' + arg + '"')
    usage()

puzzleid = int(sys.argv[1])

url = urlbase + str(puzzleid)
http = urllib3.PoolManager()
req = http.request('GET', url)

m = re.search('var puzzles = ([^;]+);', str(req.data))
data = m.group(1).replace('\\n', '')

puzzle = dict()
m = re.search('title:\"([a-zA-Z\ \.\!;]+)\"', data)
puzzle['title'] = m.group(1)
m = re.search('board:\"([\ rgbRGB]+)\"', data)
puzzle['board'] = m.group(1).replace(' ', '.')
m = re.search('robotCol:(\d+)', data)
puzzle['robotCol'] = int(m.group(1))
m = re.search('robotRow:(\d+)', data)
puzzle['robotRow'] = int(m.group(1))
m = re.search('robotDir:(\d+)', data)
puzzle['robotDir'] = int(m.group(1))
m = re.search('allowedCommands:(\d)', data)
puzzle['allowedCommands'] = int(m.group(1))
m = re.search('subs:\[(\d{1,2},\d{1,2},\d{1,2},\d{1,2},\d{1,2})\]', data)
puzzle['subs'] = m.group(1)
m = re.search('robozzle.CreateBoard\((\d+), (\d+), (\d+), (\d+)\);', str(req.data))
puzzle['width'] = int(m.group(1))
puzzle['height'] = int(m.group(2))

nocom = '--nocom' in sys.argv[2:]

if '--json' in sys.argv[2:]:
  fname = 'puzzles/p' + str(puzzleid) + '.json'
  f = open(fname, 'w')
  f.write(json.dumps(puzzle))
  f.close()
else:
  fname = 'puzzles/p' + str(puzzleid) + '.rzl'
  f = open(fname, 'w')
  f.write(puzzle['title'] + ('\n' if nocom else '\t# title\n'))
  f.write(str(puzzle['width']) + ('\n' if nocom else '\t\t# board width\n'))
  f.write(str(puzzle['height']) + ('\n' if nocom else '\t\t# board height\n'))
  f.write(str(puzzle['robotCol']) + ('\n' if nocom else '\t\t# robot starting column\n'))
  f.write(str(puzzle['robotRow']) + ('\n' if nocom else '\t\t# robot starting row\n'))
  f.write(str(puzzle['robotDir']) + ('\n' if nocom else '\t\t# robot starting direction\n'))
  f.write(str(puzzle['allowedCommands']) + ('\n' if nocom else '\t\t# allowed commands\n'))
  f.write(puzzle['subs'] + ('\n' if nocom else '\t# subs sizes\n'))
  f.write(puzzle['board'])
  f.close()

print('puzzle exported in "' + fname + '"')
