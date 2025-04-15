#!/usr/bin/env bash

cutechess-cli -engine cmd=o9-dev \
              -engine cmd=o9-dev \
              -each proto=uci tc=1+0.8 \
              -games 1000 \
              -openings file=/mnt/Data/Chess/StartingPositions/Hyat_4000_openings.epd format=epd order=sequential \
              -repeat 1 \
              -pgnout cutechess.pgn \
              -concurrency 15