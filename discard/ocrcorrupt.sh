#!/bin/bash

# pbmtext can take multiple lines

echo "Hello world!
This is some text...
The quick brown fox jumps over the lazy dog." | pbmtext | gocr -

# pbmtextps only takes a single line
# gocr doesn't like being piped the output of pbmtextps

pbmtextps -fontsize 8 "The quick brown fox jumps over the lazy dog." | gocr -
