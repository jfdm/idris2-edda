#!/bin/bash

echo "Creating Dir"
mkdir -p dependancies/
cd dependancies/

echo "Fetching testing"
git clone git@github.com:jfdm/idris2-toolkit.git toolkit
cd toolkit/
idris2 --install toolkit.ipkg
cd ../

cd ../
