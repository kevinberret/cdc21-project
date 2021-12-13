#!/bin/bash
cd /data
rebar3 get-deps
cd apps/backend/src/logic/

erl -name $(hostname)@$(hostname -i) -setcookie cdc21project

