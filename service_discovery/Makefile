## set the paths for a default setup

all:
	erlc -o ebin src/*.erl;
	erl -pa ebin -name main  -s service_discovery start


create:
	erlc -o ../../ebin_working_nodes src/*.erl;
	cp src/*.app ../../ebin_working_nodes

build:
	rm -rf */*~ *.beam ebin/*.beam *~ *.dump;
	erlc -o ../ebin src/*.erl
clean:
	rm -rf */*~ *.beam ebin/*.beam *~ *.dump

test:
	rm -rf */*~ *.beam ebin/*.beam *~ *.dump;
	erlc -o ebin *.erl src/*.erl;
	erl -pa  ebin -s sd_test test -name sd_test@home.joqhome.eu -setcookie changeCookie
