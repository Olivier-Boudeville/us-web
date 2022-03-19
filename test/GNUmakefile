US_WEB_TOP = ..

.PHONY: all test-ci


# As us_web_otp_application_test.beam depends on
# us_common_otp_application_test.beam:
#
BEAM_DIRS += $(US_COMMON_TOP)/test


# Needed to avoid calling the next, unrelated rule:
all:


# Defined for continuous integration; does not include
# us_web_otp_application_test.erl as in this context (only) the test will hang,
# most probably because the corresponding container is not allowed to open a
# server socket (even on a non-priviledged port).
#
test-ci:
	@echo "  Testing in a context of continuous integration."


include $(US_WEB_TOP)/GNUmakesettings.inc
