#!/usr/bin/env bash

set -e
set -x

mkdir -p data_cache

i="0"
while true; do
  i=$[$i+1]
  baredate=$(date -v -${i}d "+%Y-%m-%d")
  filename=${baredate}.csv.gz
  urlpath=$(date -v -${i}d "+%Y/%Y-%m-%d.csv.gz")
  
  if [ -f "data_cache/$filename" ]; then
    exit 0
  fi

  curl "http://cran-logs.rstudio.com/${urlpath}" > "data_cache/$filename"
  Rscript transform.R ${baredate}
done