# -----------------------------------------------------------------------------
# $Id: package.mk,v 1.17 2002/07/23 11:23:55 simonmar Exp $

ifneq "$(PACKAGE)" ""

# -----------------------------------------------------------------------------
# Build the package configuration file and tell the compiler about it.

ifeq "$(way)" ""

$(PACKAGE).conf.inplace   : $(PACKAGE).conf.in
	$(CPP) $(RAWCPP_FLAGS) -I$(GHC_INCLUDE_DIR) -x c $(PACKAGE_CPP_OPTS) $< \
		| sed 's/^#.*$$//g' >$@

$(PACKAGE).conf.installed : $(PACKAGE).conf.in
	$(CPP) $(RAWCPP_FLAGS) -I$(GHC_INCLUDE_DIR) -DINSTALLING -x c $(PACKAGE_CPP_OPTS) $< \
		| sed 's/^#.*$$//g' >$@

# we could be more accurate here and add a dependency on
# ghc/driver/package.conf, but that doesn't work too well because of
# make's limited accuracy with modification times: when doing 'make
# boot' in multiple packages, make won't detect that the package
# configuration needs updating if it was updated already in the last
# second.
#
STAMP_PKG_CONF = stamp-pkg-conf
CLEAN_FILES += $(STAMP_PKG_CONF)

boot all :: $(STAMP_PKG_CONF)

$(STAMP_PKG_CONF) : $(PACKAGE).conf.inplace $(PACKAGE).conf.installed
	$(GHC_PKG_INPLACE) --update-package <$(PACKAGE).conf.inplace
	$(GHC_PKG_INPLACE)  -f $(GHC_DRIVER_DIR)/package.conf --update-package <$(PACKAGE).conf.installed
	@touch $(STAMP_PKG_CONF)

CLEAN_FILES += $(PACKAGE).conf.installed $(PACKAGE).conf.inplace 

endif # $(way) == ""

# -----------------------------------------------------------------------------
# Building the static library libHS<pkg>.a

HC 	     	= $(GHC_INPLACE)

SRC_HSC2HS_OPTS += -I.

ifeq "$(NON_HS_PACKAGE)" ""
SRC_HC_OPTS 	+= -package-name $(PACKAGE)
SRC_HC_OPTS 	+= $(GhcLibHcOpts)
SRC_HC_OPTS     += $(patsubst %, -package %, $(PACKAGE_DEPS))
endif

LIBRARY      	= libHS$(PACKAGE)$(_way).a

WAYS         	= $(GhcLibWays)

all :: $(LIBRARY)

# POSSIBLE alternative version using --make:
#
# lib : $(HS_SRCS)
# 	$(GHC_INPLACE) $(HC_OPTS) --make $(HS_SRCS)
# 
# $(LIBNAME) : lib
# 	$(RM) $@
# 	$(AR) $(AR_OPTS) $@ $(HS_OBJS)
# 	$(RANLIB) $@
# 
# %.o : %.hs
# 	$(GHC_INPLACE) $(HC_OPTS) --make $<
# %.o : %.lhs
# 	$(GHC_INPLACE) $(HC_OPTS) --make $<

# -----------------------------------------------------------------------------
# Installation; need to install .hi files as well as libraries

ifeq "$(DLLized)" "YES"
INSTALL_PROGS += $(DLL_NAME)
INSTALL_LIBS += $(patsubst %.a,%_imp.a, $(LIBRARY))
endif

# The interface files are put inside the $(libdir), since they
# might (potentially) be platform specific..

ifacedir = $(libdir)/imports/$(PACKAGE)

# If the lib consists of a hierachy of modules, we must retain the directory
# structure when we install the interfaces.
ifeq "$(HIERARCHICAL_LIB)" "YES"
INSTALL_IFACES_WITH_DIRS += $(HS_IFACES)
ifneq "$(ALL_DIRS)" ""
install ::
	@for i in $(ALL_DIRS); do \
		$(INSTALL_DIR) $(ifacedir)/$$i; \
	done
endif
else
INSTALL_IFACES += $(HS_IFACES)
endif

# -----------------------------------------------------------------------------
# Dependencies

MKDEPENDHS = $(GHC_INPLACE)
SRC_MKDEPENDC_OPTS += $(addprefix -I,$(ALL_DIRS)) -I$(GHC_INCLUDE_DIR)

endif # $(PACKAGE) /= ""

# install library (could be implicitly specified or explicitly, like libHS*_cbits.a)
INSTALL_LIBS  += $(LIBRARY)

#--------------------------------------------------------------
# Building dynamically-linkable libraries for GHCi
#
# Build $(GHCI_LIBRARY) whenever we build $(LIBRARY)
#
# Why?  GHCi can only link .o files (at the moment), not .a files
# so we have to build libFoo.o as well as libFoo.a
#
# Furthermore, GHCi currently never loads 
# profiling libraries (or other non-std ways)

ifneq "$(LIBRARY)" ""

ifeq "$(way)" ""
ifeq "$(GhcWithInterpreter)" "YES"

GHCI_LIBRARY = $(patsubst lib%.a,%.o,$(LIBRARY))

INSTALL_LIBS += $(GHCI_LIBRARY)
CLEAN_FILES  += $(GHCI_LIBRARY)

all :: $(GHCI_LIBRARY)

ifneq "$(DONT_WANT_STD_GHCI_LIB_RULE)" "YES"
# If you don't want to build GHCI_LIBRARY the 'standard' way,
# set DONT_WANT_STD_GHCI_LIB_RULE to YES. The Prelude and
# hslibs/Win32 uses this 'feature', which will go away soon
# when we can use a "fixed" ld.
#
$(GHCI_LIBRARY) : $(LIBOBJS)
	$(LD) -r $(LD_X) -o $@ $(STUBOBJS) $(LIBOBJS)

endif # DONT_WANT_STD_GHCI_LIB_RULE
endif # GhcWithInterpreter
endif # way

# -----------------------------------------------------------------------------
# Doc building with Haddock

ifneq "$(NO_HADDOCK_DOCS)" "YES"

HS_PPS = $(addsuffix .raw-hs, $(basename $(filter-out $(EXCLUDED_HADDOCK_SRCS), $(HS_SRCS))))

HTML_DIR = html
HTML_DOC = $(HTML_DIR)/index.html

ifneq "$(HS_PPS)" ""
html :: $(HTML_DOC)

$(HTML_DOC) : $(HS_PPS)
	@$(INSTALL_DIR) $(HTML_DIR)
	$(HADDOCK) $(HADDOCK_OPTS) -h -o $(HTML_DIR) $(HS_PPS) \
		--dump-interface=$(PACKAGE).haddock \
		$(foreach pkg, $(PACKAGE_DEPS), \
		   --read-interface=../$(pkg),../$(pkg)/$(pkg).haddock)

%.raw-hs : %.lhs
	$(GHC_INPLACE) $(HC_OPTS) -D__HADDOCK__ -E -cpp $< -o $<.tmp && sed -e 's/^#.*//' <$<.tmp >$@

%.raw-hs : %.hs
	$(GHC_INPLACE) $(HC_OPTS) -D__HADDOCK__ -E -cpp $< -o $<.tmp && sed -e 's/^#.*//' <$<.tmp >$@

install-docs :: $(HTML_DOC)
	@$(INSTALL_DIR) $(datadir)/html/$(PACKAGE)
	@for i in $(HTML_DIR)/*; do \
	   echo $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(datadir)/html/$(PACKAGE); \
	   $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(datadir)/html/$(PACKAGE); \
	done

endif # HS_PPS
endif # NO_HADDOCK_DOCS

# -----------------------------------------------------------------------------

endif # $(LIBRARY) /= ""
