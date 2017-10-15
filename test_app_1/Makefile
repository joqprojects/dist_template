## set the paths for a default setup

all:
	erlc -o ebin src/*.erl;
	erl -pa ebin -s mymath start
#	cp ebin/* ../ebin;
#	erl -pa ebin -pa /home/joq/erlang/lib/ebin -s dbase start

build:
	erlc -o ../masternode/repository src/*.erl;
	cp src/*.app ../masternode/repository 

create:
	erlc -o ../../ebin_working_nodes src/*.erl;
	cp src/*.app ../../ebin_working_nodes

local:
	erlc -o ebin src/*.erl;
	cp src/*.app ebin

clean:
	rm -rf ebin/*.beam */*~ src/*.beam *~ *.dump

start:
	erl -pa ebin -s mymath start -name mymath@home.joqhome.eu
test:
#	erlc -o ebin *.erl src/*.erl;
#	cp src/mymath.app ebin;
	erl -pa ebin -s test_mymath test -name mymath_test@home.joqhome.eu
