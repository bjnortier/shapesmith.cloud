#!/bin/sh
cd `dirname $0`
exec erl -config ec2_master -name 'master@10.114.181.36' -setcookie 3fe1f82e60d239d4708decb17e5b7dfb -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -s reloader -s lager -s worker_master -s api
