#!/usr/bin/env bash
cd "$(dirname "$0")" || exit
tee in.txt | node example/target/scala-3.6.3/example-fastopt/main.js stdio | tee log.txt