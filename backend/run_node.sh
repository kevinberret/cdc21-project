#!/bin/bash
cd /data
rebar3 get-deps
cd apps/backend/src/logic/
erlc *.erl

erl -name $(hostname)@$(hostname -i) -setcookie cdc21project

