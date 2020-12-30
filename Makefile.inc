#TOOLS
BIGLOO = bigloo
BGLAFILE = bglafile
BGLJFILE = bgljfile
BGLTAGS = bgltags
LD = gcc --shared
AR = ar
JAR = jar
RANLIB = ranlib
INSTALL = install
LINK = ln
MAKEINFO = makeinfo

BIGLOO_VERSION := $(shell $(BIGLOO) -eval "(print *bigloo-version*) (exit 0)" -q)
BIGLOOLIBDIR := $(shell $(BIGLOO) -eval "(print *default-lib-dir*) (exit 0)" -q)


#install related variables
DESTDIR = /usr
INSTLIBDIR = $(DESTDIR)/lib
INSTBIGLOOLIBDIR = $(INSTLIBDIR)/bigloo/$(BIGLOO_VERSION)

#C Flags
CURLCFLAGS := $(shell curl-config --cflags)
CCOMMONFLAGS = -fPIC $(CURLCFLAGS) 

#Bigloo Flags
BHEAPFLAGS = -I src/Llib -I $(GENSCMDIR) -unsafe -q -mkaddheap -mkaddlib \
	     -heap-library $(LIBNAME)

BCOMMONFLAGS = -fisa -mkaddlib -fsharing -q \
               -copt '$(CCOMMONFLAGS)' -I src/Llib

BSAFEFLAGS = $(BCOMMONFLAGS) -O6 

BPROFILEFLAGS = $(BCOMMONFLAGS) -p -O6

BUNSAFEFLAGS = $(BCOMMONFLAGS) -Obench

EXTRALIBS := $(shell curl-config --libs)

# directory variables
OBJDIR = objs
DISTDIR = dist
	
BIGLOOSRCS = $(wildcard src/Llib/*.scm) 
TESTSRCS = $(wildcard recette/*.scm)
HEAPSRC = src/Misc/make_lib.scm
DOCSRCS = $(wildcard manual/*.texi)

# object files
_TESTSRCSWOUTDIR = $(foreach src, $(TESTSRCS), $(notdir $(src)))
_SRCSWOUTDIR = $(foreach src, $(BIGLOOSRCS), $(notdir $(src)))
_BASEOBJSWOUTDIR = $(_SRCSWOUTDIR:%.scm=%)

SAFEOBJECTS = $(_BASEOBJSWOUTDIR:%=$(OBJDIR)/%_s.o)

PROFILEOBJECTS = $(_BASEOBJSWOUTDIR:%=$(OBJDIR)/%_p.o)

UNSAFEOBJECTS = $(_BASEOBJSWOUTDIR:%=$(OBJDIR)/%_u.o)



_ESRCSWOUTDIR = $(foreach src, $(HEAPSRC), $(notdir $(src)))
_EBASEOBJSWOUTDIR = $(_ESRCSWOUTDIR:%.scm=%)

ESAFEOBJECTS = $(_EBASEOBJSWOUTDIR:%=$(OBJDIR)/%_s.o)

EUNSAFEOBJECTS = $(_EBASEOBJSWOUTDIR:%=$(OBJDIR)/%_u.o)

TESTOBJECTS = $(TESTSRCS:%.scm=%.o)

TESTCLASSES = $(TESTSRCS:%.scm=%.class)

.SUFFIXES:

$(OBJDIR)/%_s.o:src/Llib/%.scm
	$(BIGLOO)  -c $(BSAFEFLAGS) -o $@ $^

$(OBJDIR)/%_p.o:src/Llib/%.scm
	$(BIGLOO)  -c $(BPROFILEFLAGS) -o $@ $^

$(OBJDIR)/%_u.o:src/Llib/%.scm
	$(BIGLOO)  -c $(BUNSAFEFLAGS) -o $@ $^

$(OBJDIR)/%_s.o:$(GENSCMDIR)/%.scm
	$(BIGLOO)  -c $(BSAFEFLAGS) -o $@ $^

$(OBJDIR)/%_p.o:$(GENSCMDIR)/%.scm
	$(BIGLOO)  -c $(BPROFILEFLAGS) -o $@ $^

$(OBJDIR)/%_u.o:$(GENSCMDIR)/%.scm
	$(BIGLOO)  -c $(BUNSAFEFLAGS) -o $@ $^




recette/%.o:recette/%.scm heap
	$(BIGLOO)  -c $(BSAFEFLAGS) -L $(DISTDIR) -o $@ $<


#targets

all: c 

c: $(DISTDIR) .afile .etags lib 

test: c recette/test$(LIBNAME) 
	./recette/test$(LIBNAME)


manual: manual-html manual-pdf

manual-html : $(DOCSRCS)
	(cd manual; \
	makeinfo --html $(LIBNAME).texi)

manual-pdf : $(DOCSRCS)
	(cd manual; \
	makeinfo --pdf $(LIBNAME).texi)

.afile: $(BIGLOOSRCS)  $(TESTSRCS)
	$(BGLAFILE) -o $@  $^

.recettejfile: $(TESTSRCS)
	$(BGLJFILE) -pbase recette -o $@  -I recette  $(_TESTSRCSWOUTDIR)

.etags: $(BIGLOOSRCS) $(TESTSRCS) $(wildcard src/Llib/*.sch)
	$(BGLTAGS) -o $@ $^

heap: $(DISTDIR)/$(LIBNAME).heap

$(DISTDIR)/$(LIBNAME).heap: $(DISTDIR) $(HEAPSRC)
	$(BIGLOO) $(BHEAPFLAGS) $(HEAPSRC)  -addheap $@

$(OBJDIR)/make_lib_s.o:src/Misc/make_lib.scm
	$(BIGLOO) -c  $(BSAFEFLAGS) -o $@ $^

$(OBJDIR)/make_lib_u.o:src/Misc/make_lib.scm
	$(BIGLOO) -c $(BUNSAFEFLAGS) -o $@ $^


lib: init heap $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).so  \
	       $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).a   \
               $(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).so  \
	       $(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).a   \
               $(DISTDIR)/lib$(LIBNAME)_es-$(VERSION).so \
	       $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).so  \
	       $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).a   \
	       $(DISTDIR)/lib$(LIBNAME)_eu-$(VERSION).so

init: $(DISTDIR)/$(LIBNAME).init 

$(DISTDIR)/$(LIBNAME).init : $(GENERATEDINCLUDES) $(wildcard src/Llib/*.sch) src/Misc/$(LIBNAME).init.in
	cat $^ > $@


$(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).so: $(OBJDIR) $(DISTDIR) $(SAFEOBJECTS)
	$(LD)  -o $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).so $(SAFEOBJECTS) -lc -lm $(EXTRALIBS)

$(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).a : $(OBJDIR) $(DISTDIR) $(SAFEOBJECTS)
	$(AR) qcv $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).a $(SAFEOBJECTS);\
	$(RANLIB) $(DISTDIR)/lib$(LIBNAME)_s-$(VERSION).a

$(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).so: $(OBJDIR) $(DISTDIR) $(PROFILEOBJECTS)
	$(LD) -o $(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).so $(PROFILEOBJECTS) -lc -lm $(EXTRALIBS)

$(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).a : $(OBJDIR) $(DISTDIR) $(PROFILEOBJECTS)
	$(AR) qcv $(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).a $(PROFILEOBJECTS);\
	$(RANLIB) $(DISTDIR)/lib$(LIBNAME)_p-$(VERSION).a

$(DISTDIR)/lib$(LIBNAME)_es-$(VERSION).so : $(OBJDIR) $(DISTDIR) $(ESAFEOBJECTS)
	$(LD)  -o $(DISTDIR)/lib$(LIBNAME)_es-$(VERSION).so $(ESAFEOBJECTS) -lc -lm  $(EXTRALIBS)

$(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).so : $(OBJDIR) $(DISTDIR) $(UNSAFEOBJECTS)
	$(LD)  -o $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).so $(UNSAFEOBJECTS) -lc -lm  $(EXTRALIBS)

$(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).a : $(OBJDIR) $(DISTDIR) $(UNSAFEOBJECTS)
	$(AR) qcv $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).a $(SAFEOBJECTS);\
	$(RANLIB) $(DISTDIR)/lib$(LIBNAME)_u-$(VERSION).a

$(DISTDIR)/lib$(LIBNAME)_eu-$(VERSION).so : $(OBJDIR) $(DISTDIR) $(EUNSAFEOBJECTS)
	$(LD)  -o $(DISTDIR)/lib$(LIBNAME)_eu-$(VERSION).so $(EUNSAFEOBJECTS) -lc -lm  $(EXTRALIBS)


recette/test$(LIBNAME) : $(TESTOBJECTS) lib 
	$(BIGLOO) -static-all-bigloo $(BUNSAFEFLAGS) -L $(DISTDIR) -o $@ $(TESTOBJECTS) $(EXTRALIBS)

$(OBJDIR):
	mkdir $@

$(DISTDIR):
	mkdir $@


check-syntax: .afile
	$(BIGLOO) $(BCOMMONFLAGS) -afile .afile -syntax-check ${CHK_SOURCES}


install: c 
	for file in $(DISTDIR)/*; do \
	  $(INSTALL) $$file $(INSTBIGLOOLIBDIR)/`basename $$file`; \
	done; \

clean:
	rm -f $(TESTOBJECTS);\
	rm -f $(TESTCLASSES);\
	rm -f -r $(OBJDIR);\
	rm -f -r $(DISTDIR);\
	rm -f $(LIBNAME).heap;\
	rm -f $(LIBNAME).init;\
	rm -f .afile;\
	rm -f recette/test$(LIBNAME)



