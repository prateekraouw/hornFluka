# ###########################################
#  Generic Makefile for FLUKA applications
#
#  Compiles all *.f files in the current
#  directory and links against FLUKA.
# 
#  Change LFLUKA if you want to link against
#  DPMJET-III (by default it is not linked).
#
#  The executable is called flukamy.
#
#  Use "make clean" to clean up files.
#
# ###########################################
#
#
############## DEFINITIONS ##################
#
FLUKABIN=/opt/FLUKA_CERN/fluka4-4.0/bin
FLUKA=/home/prateek/fluka
FFF=$(FLUKA)/flutil/fff
LFLUKA=$(FLUKA)/flutil/lfluka
#LFLUKA=$(FLUKA)/bin/ldpmqmd
LOGFILE=build.log # Log file name
ARGS=$(MAKECMDGOALS) # Capture the make invocation arguments
# 
#
#
SRCFILES := $(wildcard ./*.f)
OBJECTS := $(patsubst %.f, %.o, $(SRCFILES))
PROGRAM=flukamy
#
#
############# RULES ########################
#
.f.o:
	$(FFF) $<
#
#
############# TARGETS ######################
#
all: $(PROGRAM)
#
#
dpm: LFLUKA=$(FLUKA)/bin/ldpmqmd
dpm: $(PROGRAM)
#
#
$(PROGRAM): $(OBJECTS)
	@echo "Arguments: $(ARGS)" >> $(LOGFILE) 2>&1
	@echo "Linking $(PROGRAM) with objects: $(OBJECTS)" >> $(LOGFILE) 2>&1
	echo $(OBJECTS) >> $(LOGFILE) 2>&1
	$(LFLUKA) -m fluka -o $@ $^ >> $(LOGFILE) 2>&1
#
#
clean:
	@rm -f $(PROGRAM) *.o *.map *.FOR *.mod
	@rm -f $(LOGFILE)
#
#
