# !/bin/sh

erl -config etc/dev/app.config -pa ebin -eval "application:start(gen_serverizer)"
