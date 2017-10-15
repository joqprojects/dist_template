## set the paths for a default setup

all:
	erlc -o ebin src/*.erl;
	cp src/*.app ebin;
	erl -pa ebin -s boot_board start master_node -name master_node@home.joqhome.eu -setcookie changeCookie

create:
	erlc -o ../ebin_working_nodes src/*.erl;
	cp src/*.app ../ebin_working_nodes
clean:
	rm -rf */*~ *.beam ebin/*.beam *~ *.dump
test:
	erlc -o ebin src/*.erl;
	erlc -o ebin *.erl;
	cp src/*.app ebin;
	erl -pa ebin -pa repository -s master_test test -name master_node@home.joqhome.eu
