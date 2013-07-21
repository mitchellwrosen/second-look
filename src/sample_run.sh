#!/bin/bash

export SECOND_LOOK_ACCESS_KEY=MY_AWS_ACCESS_KEY
export SECOND_LOOK_SECRET_KEY=MY_AWS_SECRET_KEY

# Alternatively, make.
redo Main

if [ $? == 0 ]; then
   ./Main --debug
fi
