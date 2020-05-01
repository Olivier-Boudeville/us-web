US_WEB_TOP = ..

WEB_IMG_DIR := for-testing/images
WEB_LOG_DIR := for-testing/log


.PHONY: clean-logs


clean: clean-logs
	@/bin/rm -rf $(WEB_IMG_DIR)


clean-logs:
	@cd $(WEB_LOG_DIR) && $(MAKE) -s clean


include $(US_WEB_TOP)/GNUmakesettings.inc
