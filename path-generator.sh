#!/bin/bash
# Create .path file

BOLD="$(tput bold)"
GREEN="$(tput setaf 2)"
MAGENTA="$(tput setaf 5)"
RED="$(tput setaf 1)"

echo $PATH > .PATH
echo -e $GREEN $PATH $RED '--> '$PWD'/.PATH' 
echo
echo -e $MAGENTA$BOLD 'Write Successful'
