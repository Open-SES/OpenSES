#  OpenSES makefile

#  based on info in stackoverflow post about makefiles for release and debug versions
#  https://stackoverflow.com/questions/1079832/how-can-i-configure-my-makefile-for-debug-and-release-builds


FCOMP = gfortran
EXE = OpenSES


# Put the .f95 and .FOR files in different lists of source files so that
# we can use slightly different compile flags for them.  We set things up so
# that the .f95 files are compiled first (as they are more likely to have
# been edited and thus raise compile errors).
SRCS1 = 	getver.f95  ancill.f95

SRCS2 =     ACEST2.FOR  DTHTS2.FOR  FLYWHL.FOR   INPUT.FOR  LSINS.FOR  OMEGA5.FOR PINPNT.FOR   RELHUM.FOR   SIMQ2.FOR    TRINS.FOR   \
            AIR.FOR     EERROR.FOR  FORCE1.FOR   INTEXT.FOR MAXMIN.FOR OMGJ1.FOR  PRINT.FOR    RESIST.FOR   SUMARY.FOR   TRSLOP.FOR  \
            ALCHAR.FOR  EQUAT.FOR   FORCEL.FOR   JINS.FOR   MMAX.FOR   OMGJ2.FOR  RKTHRM.FOR   THDERV.FOR   VSINS.FOR    WETBLB.FOR  \
            AMPERE.FOR  CHECKI.FOR  EXTINT.FOR   GARAGE.FOR JULIE.FOR  MMIN.FOR   OMGJ3.FOR    PSI.FOR      RSTREAD.FOR  THNODE.FOR  \
            BEI.FOR     CHECKR.FOR  FCTORL.FOR   GRID.FOR   KEI.FOR    OMEGA1.FOR OMGJ4.FOR    QDERIV.FOR   RSTWRITE.FOR TORQUE.FOR  \
            BER.FOR     CREAT2.FOR  HEAT.FOR     KER.FOR    OMEGA2.FOR OMGJ5.FOR  QSQLP.FOR    SIMERR.FOR   TRAIN.FOR    WLLTM2.FOR  \
            BESSEL.FOR  CREAT3.FOR  FINS.FOR     HEATUP.FOR LATENT.FOR OMEGA3.FOR OMGJ6.FOR    QUARRK.FOR   SIMLAT.FOR   TRANST.FOR  \
            DSES.FOR    FLOWAY.FOR  HOOKUP.FOR   LOCATE.FOR OMEGA4.FOR RATES.FOR  SIMQ1.FOR    TRIDG3.FOR   FILESUBS.FOR



OBJS1 =  $(SRCS1:.f95=.o)
OBJS2 =  $(SRCS2:.FOR=.o)

SRCS =  $(SRCS1)  $(SRCS2)
OBJS =  $(OBJS1)  $(OBJS2)

DBGDIR = debug
DBGEXE = $(DBGDIR)/$(EXE)
DBGOBJS = $(addprefix $(DBGDIR)/, $(OBJS))
DBGFFLAGS1 = -falign-commons -Wno-align-commons -pedantic -ggdb # for .f95 files
DBGFFLAGS2 = -falign-commons -Wno-align-commons -std=legacy -ggdb

RELDIR = release
RELEXE = $(RELDIR)/$(EXE)
RELOBJS = $(addprefix $(RELDIR)/, $(OBJS))
RELFFLAGS1 = -falign-commons -Wno-align-commons -pedantic # for .f95 files
RELFFLAGS2 = -falign-commons -Wno-align-commons -std=legacy

#*** General Purpose Rules ***
no_target:
	@echo '\******** You did not specify a make target \********'
	@echo 'Available Options:'
	@echo '  clean:    deletes all the object and exe files in release and debug folders'
	@echo '  remake:   deletes obj and exe files then builds all/everything again'
	@echo "  prep:     creates debug and release folders if they don't exist"
	@echo '  debug:    makes the debug exe file only (if needed)'
	@echo '  release:  makes the release exe file only (if needed)'
	@echo '  all:      makes the debug and release exe files (if needed)'
	@echo 'N.B. Please run "make prep" first to create the folders, or'
	@echo '     just run "make all".'


# Define some prerequisites for compiling the files, so that the makefile
# knows which routines must be recompiled when their COMMON block files change
#
# Two files involved in doing the heat sink calculation import JULIEC.
JULIE.FOR WLLTM2.FOR : JULIEC
# Seven files involved in doing the heat sink calculation import DSHRHS.
ACEST2.FOR CREAT2.FOR CREAT3.FOR  \
DTHTS2.FOR FORCE1.FOR FORCEL.FOR WLLTM2.FOR : DSHRHS
#
# Most of the input files in OpenSES import DSHARE, we'll put it for all of them.
$(SRCS) : DSHARE


#*** Clean up and remove Object files ***
remake: clean all

clean:
	rm -f $(RELEXE) $(RELOBJS) $(DBGEXE) $(DBGOBJS)

all: prep release debug

prep:
	@mkdir -p $(DBGDIR) $(RELDIR)

debug: $(DBGEXE)

$(DBGEXE): $(DBGOBJS)
	$(FCOMP) -o $(DBGEXE) $(DBGOBJS) -static

$(DBGDIR)/%.o: %.f95
	$(FCOMP) $(DBGFFLAGS1) -c $< -o $@

$(DBGDIR)/%.o: %.FOR
	$(FCOMP) $(DBGFFLAGS2) -c $< -o $@

release: $(RELEXE)

$(RELEXE): $(RELOBJS)
	$(FCOMP) -o $(RELEXE) $(RELOBJS) -static

$(RELDIR)/%.o: %.f95
	$(FCOMP) $(RELFFLAGS1) -c $< -o $@

$(RELDIR)/%.o: %.FOR
	$(FCOMP) $(RELFFLAGS2) -c $< -o $@


