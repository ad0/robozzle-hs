robozzle-hs
===========

A quick Haskell implementation of the famous RoboZZle puzzle game

Fetching a level
================

```
mkdir puzzles
python3 tools/level_fetcher.py <puzzleid>
```

RoboZZle textual language
=========================

```
PROG ::= FUN
       | FUN PROG

FUN ::= 'F'int ':' '\n' INSTRS

INSTRS ::= <empty>
         | INSTR '\n' INSTRS

INSTR ::= ACTION
        | ACTION 'if' CONDITION

ACTION ::= 'move'
         | 'left'
         | 'right'
         | 'paint' COLOR
         | 'call' 'F'int

CONDITION ::= COLOR

COLOR ::= 'blue'
        | 'green'
        | 'red'
```
