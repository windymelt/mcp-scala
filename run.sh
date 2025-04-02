#!/usr/bin/env bash
cd "$(dirname "$0")" || exit
node core/.js/target/scala-3.6.3/mcp-scala-fastopt/main.js | tee log.txt