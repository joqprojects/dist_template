## set the paths for a default setup

all:
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -s boot_board start board_mgr -name b1@home.joqhome.eu -setcookie changeCookie

build:
	erlc -o masternode/repository  */src/*.erl; cp */src/*.app masternode/repository

mainbuild:
	rm -rf */*~ *.beam masternode/ebin/* worker10/ebin/* worker11/ebin/* worker12/ebin/* *~ *.dump;
	erlc -o masternode/repository  */src/*.erl;
	cp */src/*.app masternode/repository;
	cp masternode/repository/* masternode/ebin;
	cp masternode/repository/* worker10/ebin;
	cp masternode/repository/* worker11/ebin;
	cp masternode/repository/* worker12/ebin
clean:
	rm -rf */*~ *.beam */*.dump masternode/repository/* */*ebin/* *~ *.dump

10_start:
	erl -pa ebin -pa repository -s boot_board start board_mgr -name worker10@home.joqhome.eu -setcookie changeCookie

11_start:
	erl -pa ebin -pa repository -s boot_board start board_mgr -name worker11@home.joqhome.eu -setcookie changeCookie

master_start:
	erl -pa ebin -pa repository  -s boot_board start master_node -name masternode@home.joqhome.eu -setcookie changeCookie
test:
	erlc -o ebin *.erl;
	erl -pa ebin -pa repository -s infra_test test -name infra_test@home.joqhome.eu -setcookie changeCookie
