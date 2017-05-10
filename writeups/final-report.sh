#/bin/bash

git submodule update --init --recursive
latexmk -pdf final-report
latexmk -xelatex final-slides
