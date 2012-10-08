#!/bin/bash

if [ $# -eq 0 ] ; then
    echo 'please provide a node name'
    exit 0
fi

cd `dirname $0`
exec erl -config ec2_worker -name $1 -setcookie 3fe1f82e60d239d4708decb17e5b7dfb -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -s reloader -s lager -s worker
