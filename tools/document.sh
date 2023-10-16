#!/bin/sh

set -ex

cd $(dirname $0)/..

directory=doc/src/content/docs/examples

rm -rf $directory/*
go run github.com/raviqqe/gherkin2markdown@latest features $directory

for file in $(find $directory -name '*.md' | grep -v smoke); do
  new_file=$(dirname $file)/new_$(basename $file)

  (
    echo ---
    echo title: $(grep -o '^# \(.*\)$' $file | sed 's/# *//')
    echo ---
    cat $file | grep -v '^# '
  ) >$new_file
  mv $new_file $file
done

cd doc

npm install
npm run build
