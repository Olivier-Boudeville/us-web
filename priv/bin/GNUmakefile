US_WEB_TOP = ../..

.PHONY: all monitor monitor-dev monitor-prod


all:


monitor:
	@./monitor-us-web.sh

monitor-dev:
	@./monitor-us-web.sh us-monitor-for-development.config

monitor-prod:
	@./monitor-us-web.sh us-monitor-for-production.config


include $(US_WEB_TOP)/GNUmakesettings.inc
