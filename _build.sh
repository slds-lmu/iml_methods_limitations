#!/bin/sh
set -e # Exit with nonzero exit code if anything fails

# Create references
make -B pdf
# Compile html version 
make -B html

