#!/bin/sh

echo "You will have to kill this program with Ctrl-C or your task manager"
mvn  -Prun-on-felix package antrun:run

