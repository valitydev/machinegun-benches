#!/bin/sh

erl -pa _build/default/lib/*/ebin -pa _build/default/checkouts/*/ebin -run machinegunner start "${1}"
