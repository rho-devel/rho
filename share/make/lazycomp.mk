## ${R_HOME}/share/make/lazycomp.mk
## Used for all packages except base, tools, datasets, methods

.PHONY: Rsimple Rlazy

$(top_builddir)/library/$(pkg)/R/$(pkg).rdb: all.R
	@$(INSTALL_DATA) all.R $(top_builddir)/library/$(pkg)/R/$(pkg)
	 $(ECHO) "tools:::makeLazyLoading(\"$(pkg)\")" | \
	  R_DEFAULT_PACKAGES=$(DEFPKGS) LC_ALL=C $(R_EXE) > /dev/null; \

Rsimple: mkR mkRsimple
Rlazy: mkR mkRsimple mklazy
