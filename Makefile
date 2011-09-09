include version.mk

all: compile

compile:
	rm -f src/._* c_src/._* ._*
	rebar compile


package: compile
	rm -rf tmproot
	mkdir -p tmproot/opt/erlyvideo/lib/alsa-$(VERSION)/
	cp -r priv ebin src tmproot/opt/erlyvideo/lib/alsa-$(VERSION)/
	cd tmproot && \
	fpm -s dir -t deb -n erly-alsa -d libasound2 -v $(VERSION) -m "Max Lapshin <max@maxidoors.ru>" opt 
	mv tmproot/*.deb .

upload_package: 
	scp *$(VERSION)* erlyhub@git.erlyvideo.org:/apps/erlyvideo/debian/public/transcoding
	ssh erlyhub@git.erlyvideo.org "cd /apps/erlyvideo/debian ; ./update transcoding"


.PHONY: package upload_package


