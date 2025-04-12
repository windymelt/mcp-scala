#!/usr/bin/env bash
cd "$(dirname "$0")" || exit
tee in.txt | node example/.js/target/scala-3.6.3/mcp-scala-fastopt/main.js | tee log.txt