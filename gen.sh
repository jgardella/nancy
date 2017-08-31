#!/bin/sh
alex gen/Lexer.x -o src/Nancy/Lexer.hs
happy gen/Parser.y -o src/Nancy/Parser.hs
