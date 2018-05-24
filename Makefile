all: purple formdata

purple: purple.o
	gfortran -static -s -o purple purple.o

formdata: formdata.o
	gfortran -static -s -o formdata formdata.o

purple.o: purple.f95
	gfortran -O2 -c purple.f95

formdata.o: formdata.f95
	gfortran -O2 -c formdata.f95

install:
	sudo cp purple.html /srv/httpd/htdocs
	sudo chown apache:apache /srv/httpd/htdocs/purple.html
	sudo cp purple.pl /srv/httpd/cgi-bin
	sudo chown apache:apache /srv/httpd/cgi-bin/purple.pl
	sudo chmod 0755 /srv/httpd/cgi-bin/purple.pl
	sudo cp purple /srv/httpd/cgi-bin
	sudo chown apache:apache /srv/httpd/cgi-bin/purple
	sudo chmod 0755 /srv/httpd/cgi-bin/purple
	./formdata
	sudo cp relays.dat /srv/httpd/cgi-bin
	sudo chown apache:apache /srv/httpd/cgi-bin/relays.dat
