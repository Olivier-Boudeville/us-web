.PHONY: clean clean-logs clean-log-archives


clean: clean-logs clean-log-archives
	@echo "Cleaning all traces and logs (archived or not)"
	-@/bin/rm -f us_web.traces


clean-logs:
	-@/bin/rm -f access-*.log error-*.log


clean-log-archives:
	-@/bin/rm -f access-*.log.*.xz error-*.log.*.xz
