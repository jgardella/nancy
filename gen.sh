#!/bin/sh
alex gen/Lexer.x -o src/Lexer.hs
happy gen/Parser.y -o src/Parser.hs
