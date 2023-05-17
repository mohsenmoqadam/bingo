PWD := $(shell pwd)
SCP := $(shell which scp)
SED := $(shell which sed)
ES  := $(shell which escript)
VER := $(shell cat ./Version)
FS  := username@file.server.address:~/path.in.home

.PHONY: proto codec compile shell test console-dev rel-dev rel-stage rel-prod

all: proto codec compile

proto:
	$(PWD)/script/gpb -pkgs -I $(PWD)/proto -o-erl $(PWD)/src -o-hrl $(PWD)/include $(PWD)/proto/*.proto

codec:
	$(ES) $(PWD)/script/codec.erl bingo $(PWD)/proto/ $(PWD)/src

compile:
	$(PWD)/script/rebar3 compile

shell:
	$(PWD)/script/rebar3 shell

test:
	## Node_2
	cp ./rebar.config ./rebar.config.bk
	$(SED) -i 's/{bingo, "bingo-version"}/{bingo, "$(VER)"}/g' ./rebar.config
	$(SED) -i 's/rel_name/test_node_2/g' ./rebar.config
	cp ./config/node.sys.config ./config/test_node_2.sys.config
	cp ./config/node.vm.args ./config/test_node_2.vm.args
	$(SED) -i 's/node_name/test_node_2@127.0.0.1/g' ./config/test_node_2.sys.config
	$(SED) -i 's/node_name/test_node_2@127.0.0.1/g' ./config/test_node_2.vm.args
	-$(PWD)/script/rebar3 as test_node_2 release
	-mv ./config/test_node_2.sys.config _build/test_node_2/rel/bingo/releases/$(VER)/sys.config
	-mv ./config/test_node_2.vm.args _build/test_node_2/rel/bingo/releases/$(VER)/vm.args
	mv ./rebar.config.bk ./rebar.config
	## Node_3
	cp ./rebar.config ./rebar.config.bk
	$(SED) -i 's/{bingo, "bingo-version"}/{bingo, "$(VER)"}/g' ./rebar.config
	$(SED) -i 's/rel_name/test_node_3/g' ./rebar.config
	cp ./config/node.sys.config ./config/test_node_3.sys.config
	cp ./config/node.vm.args ./config/test_node_3.vm.args
	$(SED) -i 's/node_name/test_node_3@127.0.0.1/g' ./config/test_node_3.sys.config
	$(SED) -i 's/node_name/test_node_3@127.0.0.1/g' ./config/test_node_3.vm.args
	-$(PWD)/script/rebar3 as test_node_3 release
	-mv ./config/test_node_3.sys.config _build/test_node_3/rel/bingo/releases/$(VER)/sys.config
	-mv ./config/test_node_3.vm.args _build/test_node_3/rel/bingo/releases/$(VER)/vm.args
	mv ./rebar.config.bk ./rebar.config
	## Run	
	-_build/test_node_2/rel/bingo/bin/bingo daemon
	-_build/test_node_3/rel/bingo/bin/bingo daemon
	-$(PWD)/script/rebar3 ct --name 'test_node_1@127.0.0.1' --setcookie bingo-cookie -vvv
	## Stop
	-_build/test_node_2/rel/bingo/bin/bingo stop
	-_build/test_node_3/rel/bingo/bin/bingo stop

console-dev:
	_build/dev/rel/bingo/bin/bingo console

rel-prod:
	$(SED) -i 's/{bingo, "bingo-version"}/{bingo, "$(VER)"}/g' ./rebar.config
	$(PWD)/script/rebar3 as prod release
	$(PWD)/script/rebar3 as prod tar
	$(SED) -i 's/{bingo, "$(VER)"}/{bingo, "bingo-version"}/g' ./rebar.config
    #$(SCP) -P 8522 $(PWD)/_build/prod/rel/bingo/bingo-$(VER).tar.gz $(FS)
	@printf "\nApplication: %s\n" $(PWD)/_build/prod/rel/bingo/bingo-$(VER).tar.gz

rel-stage:
	$(SED) -i 's/{bingo, "bingo-version"}/{bingo, "$(VER)"}/g' ./rebar.config
	$(PWD)/script/rebar3 as stage release
	$(PWD)/script/rebar3 as stage tar
	$(SED) -i 's/{bingo, "$(VER)"}/{bingo, "bingo-version"}/g' ./rebar.config
    #$(SCP) -P 8522 $(PWD)/_build/stage/rel/bingo/bingo-$(VER).tar.gz $(FS)
	@printf "\nApplication: %s\n" $(PWD)/_build/stage/rel/bingo/bingo-$(VER).tar.gz

rel-dev:
	$(PWD)/script/rebar3 as dev release

node:
	cp ./rebar.config ./rebar.config.bk
	$(SED) -i 's/{bingo, "bingo-version"}/{bingo, "$(VER)"}/g' ./rebar.config
	$(SED) -i 's/rel_name/$(name)/g' ./rebar.config
	cp ./config/node.sys.config ./config/$(name).sys.config
	cp ./config/node.vm.args ./config/$(name).vm.args
	$(SED) -i 's/node_name/$(name)@127.0.0.1/g' ./config/$(name).sys.config
	$(SED) -i 's/node_name/$(name)@127.0.0.1/g' ./config/$(name).vm.args
	-$(PWD)/script/rebar3 as $(name) release
	-mv ./config/$(name).sys.config _build/$(name)/rel/bingo/releases/$(VER)/sys.config
	-mv ./config/$(name).vm.args _build/$(name)/rel/bingo/releases/$(VER)/vm.args
	mv ./rebar.config.bk ./rebar.config
	-_build/$(name)/rel/bingo/bin/bingo console
