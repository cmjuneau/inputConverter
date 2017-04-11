-------------------------------------------------------------------------------
Program Overview and Need
-------------------------------------------------------------------------------
This program converts input files from both CEM and LAQGSM into input files
accepted by the program CEM-LAQ.  CEM-LAQ is the combination of the CEM03.03-F
and LAQGSM03.03-F_noGPL programs.  In combining these programs, the input file
was modified to allow projectiles larger than nucleons to be incident upon the
target nucleus.  The new input file must use 1 of 2 options (described below).
The CEM-LAQ input can have negative energy values to allow users to enter the
particle energy with units of (GeV/nucleon), as is done in LAQGSM03.03-F_noGPL.

1) ZAID: The CEM input is mostly untouched, however the ZAID is used instead
of pname; pname is still usable with this format, however the projectile ZAID
must be used for the projectile definition when incdient projectiles are larger
than nucleons.  The variable "dt0" must be made 0.000 if the value was
previously negative in addition.  This program can make this change in the CEM
input if the input name is listed in the file "cemList.txt".

2) The "Modified" format: The CEM input is re-organized so that particle
definitions (projectile, target), are clustered.  That is, the projectile is
defined in a single line with the mass number (anucl), atomic number (znucl),
and the spin (stin).  The target is defined in a single line as the mass number
(atarg) and atomic number (ztarg).  The incident particle's energy (t0MeV),
energy step size (dt0), and maximum energy (t0Max) are all defined in the next
consecutive lines.  These energy values may be negative to use energy units of
(GeV/nucleon).  The variable pname is changed to the appropriate relation
accepted in CEM-LAQ.

Note that this program has the ability to convert old CEM and LAQGSM inputs to
this format.


-------------------------------------------------------------------------------
Program Use
-------------------------------------------------------------------------------
Ensure that the files "cemList.txt" and "laqList.txt" contain the names of
several files for the respective CEM and LAQGSM input files that need to be
converted.  Also ensure these files are placed into the same directory as all
of the input file names in the lists "cemList.txt" and "laqList.txt".  Place
the input files into the main directory or into the directory with the file
"inpConverter.exe".

The inputConverter program uses a makefile to compile the source code.  The
program can be implemented by typing into the command prompt:

"make convert"

If implementing the program from the main directory.  If the "inpConverter.exe"
file is used, instead type "./inpConverter.exe", with the "inpConverter.exe"
file in the directory with the lists and various inputs being converted.  The
program will then be started.


The program asks the user to specify which format (above) the user wants the
input files to be converted to (ZAID or modified).  The user must enter either
a (Z) or a (M) to pick which input format the program will use.  The user will
then be asked to specify to create a file or to overwrite the current input
file.  The user must enter either a (N) or (O) to create a new file or
overwrite the current input file, respectively.  If the user selects a new
input file (N), the program will insert a ".conv" before the last occurence of
".i" or ".inp" in the file name.  If a ".i" or ".inp" does not exist, the
program simply appends a ".conv.inp" to the file name.

The program does attempt to guess what values are used if the input file ends
unexpectedly.  For example, if the input ended after specifying the angle bins,
the program would assume ihist = 1 and use the average value for the ang(10)
variables.  A message is displayed whenever the program is forced to guess the
remaining portion of the input file.

NOTE: the program does NOT convert CEM input files with more than 1
calculation.  That is, if the "stop" card is instead a particle name, such as
"pipl", the program will ignore this continued input file and overwrite the
previous data, and replace the additional "pname" variable with the appropriate
"stop" card.  This version of the program does not accomodate to these input
files.


-------------------------------------------------------------------------------
Acknowledgements
-------------------------------------------------------------------------------
This program was developed by CMJ, April 2017, under funding provided, in part
by, funding from Los Alamos National Laboratory, under Idaho State University
subcontract number 385443.