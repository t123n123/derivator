#!/bin/bash

ghc main.hs 
./main > doc.tex
latex2png doc.tex
xdg-open doc.png
