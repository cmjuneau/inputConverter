#		Compiller and various options
F77   = gfortran
FFLGS = #-O1 -ffpe-trap=invalid,zero,overflow -fno-automatic
# only for debugging

SRC = src

#
totalConverter = $(SRC)/inputConverter.f90\
	$(SRC)/resetVars.f90\
	$(SRC)/readInput.f90\
	$(SRC)/varConversion.f90\
	$(SRC)/writeInput.f90

SRCTOT = converter.f90
#		Objects
OBJECTS = converter.o

#		Flags
OFLGS = $(FFLGS)
F77FLGSC = $(OFLGS) -c
F77FLGS = $(OFLGS) 
CHKFLGS = $(FFLGS)
#


#
inpConverter.exe: $(OBJECTS)
	$(F77) -o $@ $(F77FLGS) $(OBJECTS) 
	@ clear
	@ echo 'Input Converter was Successfully Compiled'
#
compilation: inpConverter.exe               #Compile and make executable only
#
convert:
	# @ make clean
	@ make
	@ clear
	@ ./inpConverter.exe
#
# clean:
# 	@ clear
# 	@ echo '---> clean:   remove   *.inp *.exe'
# 	@ rm -f  *.inp
#

converter.f90: $(totalConverter)
	cat $(totalConverter) > $@
#
converter.o : converter.f90
	$(F77) $(F77FLGSC) converter.f90
#