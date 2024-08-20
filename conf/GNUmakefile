US_WEB_TOP = ..


.PHONY: clean-local clean-rebar clean-web-log-conf



# Typically to generate various flavours (normal, for Hex, for testing, etc.) of
# rebar.config files:
#
# (note: not centralised in a GNUmakerules-automatic file for all layers, as
# substitutions are layer-specific)
#
%: %.template $(US_WEB_TOP)/GNUmakevars.inc
	@echo "  Generating $@ from $< (VERSION_FOR_OTP being $(VERSION_FOR_OTP), US_COMMON_VERSION_FOR_OTP being $(US_COMMON_VERSION), TRACES_VERSION_FOR_OTP being $(TRACES_VERSION), WOOPER_VERSION_FOR_OTP being $(WOOPER_VERSION), MYRIAD_VERSION_FOR_OTP being $(MYRIAD_VERSION)); this file should be copied to the root of this layer in order to be taken into account."
	@cat $< | sed "s|\"MYRIAD_VERSION_FOR_OTP\"|\"$(MYRIAD_VERSION)\"|g" | sed "s|\"WOOPER_VERSION_FOR_OTP\"|\"$(WOOPER_VERSION)\"|g" | sed "s|\"TRACES_VERSION_FOR_OTP\"|\"$(TRACES_VERSION)\"|g" | sed "s|\"US_COMMON_VERSION_FOR_OTP\"|\"$(US_COMMON_VERSION)\"|g" | sed "s|\"VERSION_FOR_OTP\"|\"$(VERSION_FOR_OTP)\"|g" | sed "s|FIRST_FILES_FOR_REBAR|$(MYRIAD_REBAR_FIRST_FILES)|g" | sed "s|DEBUG_KEY_FOR_REBAR|$(DEBUG_INFO_KEY)|g" | sed "s|INCS_FOR_REBAR|$(MYRIAD_REBAR_INCS)|g" > $@


clean: clean-local


clean-local: clean-rebar clean-web-log-conf
	-@/bin/rm -f $(REBAR_CONF_FILES)


# Removed: -@/bin/rm -rf priv/
clean-rebar:
	-@/bin/rm -f rebar.config


clean-web-log-conf:
	-@/bin/rm -f awstats-for-*.conf awstats-vhost-catchall-for-*.conf


include $(US_WEB_TOP)/GNUmakesettings.inc
