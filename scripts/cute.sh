#!/usr/bin/env bash

cutechess-cli -engine cmd=o9-dev \
              -engine cmd=o9-tt2 \
              -each proto=uci tc=1+0.8 \
              -games 2 -rounds 10000 -repeat 2 -maxmoves 250 \
              -openings file=/mnt/Data/Chess/StartingPositions/Hyat_4000_openings.epd format=epd order=sequential \
              -pgnout cutechess.pgn \
              -concurrency 15 \
              -ratinginterval 10 \
              -sprt elo0=0 elo1=10 alpha=0.05 beta=0.05