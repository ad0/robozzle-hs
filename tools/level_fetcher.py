import urllib3
import re
import json
import sys

urlbase = 'http://www.robozzle.com/js/play.aspx?puzzle='

if len(sys.argv) != 2:
  print('USAGE: ./level_fetcher.py <puzzleid>')
  sys.exit(1)
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

fname = 'puzzles/p' + str(puzzleid) + '.rzl'
f = open(fname, 'w')
f.write(puzzle['title'] + '\t# title\n')
f.write(str(puzzle['width']) + '\t\t# board width\n')
f.write(str(puzzle['height']) + '\t\t# board height\n')
f.write(str(puzzle['robotCol']) + '\t\t# robot starting column\n')
f.write(str(puzzle['robotRow']) + '\t\t# robot starting row\n')
f.write(str(puzzle['robotDir']) + '\t\t# robot starting direction\n')
f.write(str(puzzle['allowedCommands']) + '\t\t# allowed commands\n')
f.write(puzzle['subs'] + '\t# subs sizes\n')
f.write(puzzle['board'])
f.close()

print('puzzle exported in "' + fname + '"')
