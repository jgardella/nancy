#!/bin/sh
alex gen/Lexer.x -o src/AudiComp/Lexer.hs
happy gen/Parser.y -o src/AudiComp/Parser.hs
