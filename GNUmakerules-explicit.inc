# Here are gathered explicit (immediate, static) rules for US-Web.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: US_WEB_TOP must be set.

.PHONY: default-us-web-rule create-us-web-checkout


# The first non-generic, therefore default, rule:
default-us-web-rule: all


# Create, in the context of any rebar3-based US build, a _checkouts directory in
# which all Ceylan and US dependencies for the current layer are referenced
# through a symlink, to point to external clones.
#
# This should take care automatically of myriad, wooper, traces, us_common, and
# leec.
#
# (jsx not used anymore, so no symlink for it)
#
create-us-web-checkout:
	@echo "  Creating a checkout directory for US-Web"
	@$(MAKE) -s create-us-common-checkout
	@cd _checkouts && ln -sf ../$(US_COMMON_TOP) && ln -sf ../$(LEEC_TOP)
	@echo "  Please add, in the _checkouts directory, a symlink for cowboy (pointing to a clone of it, typically in ~/Software)."
