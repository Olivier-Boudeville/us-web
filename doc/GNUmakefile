US_WEB_TOP = ..


DOC_BASE_NAME := universal-webserver-technical-manual-english


OVERALL_DOCUMENT_SOURCE := $(DOC_BASE_NAME).rst


CURRENT_US_WEB_HTML := $(DOC_BASE_NAME).html

HTML_TARGET := $(CURRENT_US_WEB_HTML)


CURRENT_US_WEB_PDF := $(DOC_BASE_NAME).pdf

PDF_TARGET := $(CURRENT_US_WEB_PDF)


DOC_GENERATED_FILES := $(HTML_TARGET) $(PDF_TARGET)



PROJECT_CSS := pygments-default.css,us-web.css

PROJECT_ICON := us-web-icon.png


# Read from the user's environment:
GITHUB_PAGES_BASE := $(shell basename "$(US_WEB_WEB_MIRROR_BRANCH)")



.PHONY: all doc html pdf create-links clone-mirror                     \
		export-doc export-doc-to-official export-doc-to-mirror         \
		clean-links-local info info-web



# Default do-nothing target:
all:
	@echo "  Run 'make doc' to generate the documentation of the 'US-Web' layer."


doc: html pdf create-api-doc


html: local-html

pdf: local-pdf


# This link does not need to be in version control:
create-links:
	@/bin/ln -sf $(CURRENT_US_WEB_HTML) index.html



# Creates a separate repository for the GitHub pages branch:
clone-mirror:
	@echo "  Creating a separate repository for website in $(GITHUB_PAGES_BASE)"
	@cd ../../ && git clone https://github.com/Olivier-Boudeville/us-web $(GITHUB_PAGES_BASE) && cd $(GITHUB_PAGES_BASE) && git checkout -b gh-pages
	@echo "Please remove all initial content in that selected 'gh-pages' branch."


export-doc: clean-doc doc export-doc-to-official export-doc-to-mirror


# Drawback: scp will not preserve symlinks, it will duplicate files instead.
#
# rsync would not, but requires a different source layout (copies a full tree,
# not a set of files), so currently not used:
# rsync -avze "ssh $(SP)" $(SOME_WEB_TREE) $(WEB_SRV):$(WEB_ROOT)/US-Web/
#
export-doc-to-official: clean-doc create-links doc
	@echo "   Exporting US-Web documentation to official website ($(WEB_SRV))"
	@/bin/scp -r $(SP) $(WEB_CONTENT) $(WEB_SRV):$(WEB_ROOT)/US-Web


export-doc-to-mirror: clean-doc create-links doc
	@echo "   Exporting US-Web documentation to mirror website in $(US_WEB_WEB_MIRROR_BRANCH)"
	@/bin/cp -rf $(WEB_CONTENT) $(US_WEB_WEB_MIRROR_BRANCH) && cd $(US_WEB_WEB_MIRROR_BRANCH) && git add . && git merge -s ours && git commit -m "us-web doc updated." && git push && git pull --ff-only



info: info-doc info-web


info-web:
	@echo "HTML_TARGET = $(HTML_TARGET)"
	@echo "SP = $(SP)"
	@echo "WEB_CONTENT = $(WEB_CONTENT)"
	@echo "WEB_SRV = $(WEB_SRV)"
	@echo "WEB_ROOT = $(WEB_ROOT)"
	@echo "US_WEB_WEB_MIRROR_BRANCH = $(US_WEB_WEB_MIRROR_BRANCH)"
	@echo "GITHUB_PAGES_BASE = $(GITHUB_PAGES_BASE)"


DOCUTILS_TOP = .

# For MYRIAD_TOP:
include $(US_WEB_TOP)/GNUmakesettings.inc
