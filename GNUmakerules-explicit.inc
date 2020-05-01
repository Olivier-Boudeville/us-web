# Here are gathered explicit (immediate, static) rules for US-web.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: US_WEB_TOP must be set.

.PHONY: default-us-web-rule


# The first non-generic, therefore default, rule:
default-us-web-rule: all
