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
PROG ::= SUB | SUB PROG

SUB ::= 'sub' 'F'int ':' '\n' INSTRS

INSTRS ::= <empty> | INSTR '\n' INSTRS

INSTR ::= ACTION
        | ACTION 'if' CONDITION

ACTION ::= 'move' | 'left' | 'right' | 'call' 'F'int

CONDITION ::= 'blue' | 'green' | 'red'
```
