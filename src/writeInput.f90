! =============================================================================

 subroutine writeInput(numConversions, fileNum, fileName, overWrite, &
        & formatSpec, type)
   implicit none

! -----------------------------------------------------------------------------
! This subroutine is meant to write the input variables to the 
! input file (the user can specify to create a new file or
! overwrite an existing one).  If writing variables to a new
! file, a ".conv." will be placed between before the ".inp" or
! ".i" if one exists.  If none, exist, a ".conv.inp" is placed at
! the end of the file name regardless.
! 
! Completed Original Devlopment by CMJ, 04/10/2017
! 
! -----------------------------------------------------------------------------
! -------------------------------  Editor  Log  -------------------------------
! -----------------------------------------------------------------------------
! Editor    | Date       | Edit Description
! ----------|------------|-----------------------------------------------------
! CMJ       | 04/10/2017 | Original Program Completion
!           |            | 
!           |            | 
!           |            | 
!           |            | 
! 
! -----------------------------------------------------------------------------


! =====================================
! Variables
! =====================================
   integer :: limc, mspec, mpyld, mchy, misy, mdubl, mang, &
        & ipar1, ipar2, nevType, nPreqType, iPisa, iHist, iTyp, nh, ih, &
        & overWrite, formatSpec, fileNum, lastDotIndx, tempFileNum, j, &
        & JJJ, INSP, iyeld, IGSI, ZAID, ibrems, lastCharIndx, numConversions, &
        & type, zero, one, two, hundred
   real :: stin, T0, tstep(10), pmin, aproj, zproj, cm0, t0MeV, dt0, t0Max, &
        & dtheta, theta1(10), theta2(10), tmin(4), tmax(4), dt(4), ang(10), &
        & dang, atarg, ztarg
   character(LEN = 99) :: tempString, heading(10), pHeading(2), comments(29)
   character(LEN = 30) :: auxFile1, auxFile2, outFile, fileName, tempFile1, &
        & tempFile2, tempFileName
   character(LEN = 10) :: stopString(2)
   character(LEN =  6) :: pnameString, fileType(3)
   character(LEN =  4) :: pname
   character(LEN =  3) :: stat
   logical fileEnding

! =====================================
! Common Blocks
! =====================================
   ! CEM Common Blocks
   common / cemInt  / limc, mspec, mpyld, mchy, misy, &
        & mdubl, mang, ipar1, ipar2, nevType, nPreqType, iPisa, iHist, &
        & iTyp, nh
   common / cemReal / t0MeV, dt0, t0Max, dtheta, theta1, theta2, tmin, &
        & tmax, dt, ang, dang, atarg, ztarg
   common / cemChar / auxFile1, outFile, heading, pname

   ! LAQGSM Common Blocks
   common / laqInt  / JJJ, INSP, iyeld, IGSI, ZAID, ibrems
    common / laqReal / stin, T0, tstep, pmin, aproj, zproj, cm0
   common / laqChar / auxFile2, stat, pnameString

! =====================================
! Data Blocks
! =====================================
   data pHeading / '     ZAID, prot - proton, neut - neutron, pipl - pi+, pimi - pi-, pize - pi0,', &
        & '     gamm - gamma with fixed energy, gamb - bremss. gamma, stop - no more calc.' /
   data fileType / '.conv', '.inp', '.i' /
   data stopString / '-2, -2, -2', 'stop' /
   data fileEnding / .true. /
   data zero, one, two, hundred / 0.d0, 1.d0, 2.d0, 100.d0 /
   data comments &
        & / '/File name for diagnostic output. (30 char.)', & ! comments(1)
        &   '/File name for results of calculation. (30 char.)', & ! comments(2)
        &   '/pname/ projectile particle name:', & ! comments(3)
        &   '/t0mev/  minimum (initial) projectile kinetic energy in MeV; [tgmin for gamb]', & ! comments(4)
        &   '/anucl/  target mass number', & ! comments(5)
        &   '/znucl/  target atomic number', & ! comments(6)
        &   '/limc/   total number of inelastic events, normally 2000-500000', & ! comments(7)
        &   '/dt0/    projectile kinetic energy step-size in MeV [Only 1 energy if <0.]', & ! comments(8)
        &   '/t0max/  maximum (final) projectile kinetic energy in MeV, [tgmax for gamb]', & ! comments(9)
        &   '/dteta/  step, 2-size (degrees) in ejectile angular distributions [mang > 0]', & ! comments(10)
        &   '/mspec/  (0/1,2) if ejectile energy spectra (are not/are) needed', & ! comments(11)
        &   '/mpyld/  (0/1) if particle yield tables (are not/are) needed', & ! comments(12)
        &   '/mchy/   (0/1) if particle channel yields (are not/are) needed', & ! comments(13)
        &   '/misy/   (0/1,2,3) if isotope yields (are not/are) needed', & ! comments(14)
        &   '/mdubl/  (0/1,2) if double differential spectra (are not/are) needed', & ! comments(15)
        &   '/mang/   (0/1,2) if angular distributions (are not/are) needed', & ! comments(16)
        &   '/ipar1,ipar2/ range of ejectile types for spectrum calcs. [Below, ang. bins for mdubl > 0]', & ! comments(17)
        &   '/theta1, theta2, j=1-10/, angles bins (negative number limits no. of angle bins', & ! comments(18)
        &   '/tmin, tmax, dt, j=1-4/', & ! comments(19)
        &   '/nevtype/ number of evaporated particle types (see table in bldatgem.f). ', & ! comments(20)
        &   '/npreqtyp/ number of equilibrium particle types', & ! comments(21)
        &   '/ipisa/  (0/1) next line is not need if ipisa=0', & ! comments(22)
        &   '/PISA angle(10), dang', & ! comments(23)
        &   '/ihist/ (0/1) 0 for no hist tallies, 1 for hist tallies', & ! comments(24)
        &   '/ityp/   Version of the random no. generator used; 1-7 OK; default 1', & ! comments(25)
        &   '/nh/     Lines of text (<11) to be read in; printed on results file (line 2).', & ! comments(26)
        &   '/aproj, zproj, stin/ Projectile mass number, atomic number, and spin', & ! comments(27)
        &   '/anucl, znucl/ Target mass number and atomic number', & ! comments(28)
        &   '/stop/ Stop Signal for CEM Calculation' / ! comments(29)

! =============================================================================

   ! Storing filename temporarily for terminal output
   tempFileName = trim(fileName)

   ! Opening/Preparing the input file
   if ( overWrite.eq.zero ) then

      ! -----  New file  -----

      ! Obtaining file numbers and name for file unit and name display
      tempFileNum = hundred*fileNum

      ! Obtaining indices of where the ".inp" and file end are
      lastDotIndx = INDEX(fileName, trim(fileType(3)), fileEnding)
      lastCharIndx = lnblnk( fileName )

      ! Ensuring file has a ".i"
      if ( lastDotIndx.eq.zero ) then

         ! Setting lastDotIndx to last character of string
         lastDotIndx = lastCharIndx + one

         ! Assigning file type originally present
         tempFile2 = fileType(2)

      else

         ! Assigning file type originally present
         tempFile2 = fileName(lastDotIndx:lastCharIndx)

      endif

      ! Creating start of file name
      tempFile1 = fileName(1:lastDotIndx - one)

      ! Re-naming file
      fileName = trim(tempFile1) // trim(fileType(1)) // trim(tempFile2)

      ! Opening the file if we need to, closing old file
      open(tempFileNum, file = fileName, status = 'unknown')

   elseif ( overWrite.eq.one ) then

      ! Overwrite file
      tempFileNum = fileNum

      ! Closing and re-opening file
      rewind(tempFileNum)

   endif

   ! Writing to the input file based on format specifier, where appropriate
   write (tempFileNum, 100) auxFile1, trim(comments(1))
   write (tempFileNum, 100) outFile, trim(comments(2))

   ! Writing nuclide information and energies
   if ( formatSpec.eq.one ) then

      ! Modify and re-arrange input file
      write (tempFileNum, 500) nint(aproj), nint(zproj), stin, &
           & trim(comments(27))
      write (tempFileNum, 650) nint(atarg), nint(ztarg), trim(comments(28))
      write (tempFileNum, 200) t0MeV, trim(comments(4))
      write (tempFileNum, 200) dt0, trim(comments(8))
      write (tempFileNum, 200) t0Max, trim(comments(9))
      write (tempFileNum, 400) real(limc), trim(comments(7))

   elseif ( formatSpec.eq.two ) then

      ! Only change ZAID for file
      write (tempFileNum, 100) trim(pnameString), trim(comments(3))
      write (tempFileNum, 100) trim(pHeading(1)), ''
      write (tempFileNum, 100) trim(pHeading(2)), ''
      write (tempFileNum, 200) t0MeV, trim(comments(4))
      write (tempFileNum, 300) nint(atarg), trim(comments(5))
      write (tempFileNum, 300) nint(ztarg), trim(comments(6))
      write (tempFileNum, 400) real(limc), trim(comments(7))
      write (tempFileNum, 200) dt0, trim(comments(8))
      write (tempFileNum, 200) t0Max, trim(comments(9))

   else

      ! Incorrect specifier, error
      write(*, 1500) tempFileName

   endif

   ! Writing remaining variables to file
   write (tempFileNum, 250) dtheta, trim(comments(10))
   write (tempFileNum, 350) mspec, trim(comments(11))
   write (tempFileNum, 350) mpyld, trim(comments(12))
   write (tempFileNum, 350) mchy, trim(comments(13))
   write (tempFileNum, 350) misy, trim(comments(14))
   write (tempFileNum, 350) mdubl, trim(comments(15))
   write (tempFileNum, 350) mang, trim(comments(16))
   write (tempFileNum, 600) ipar1, ipar2, trim(comments(17))
   write (tempFileNum, 700) theta1(1), theta2(1), theta1(2), theta2(2), &
        & theta1(3), theta2(3), theta1(4), theta2(4), theta1(5), theta2(5), &
        & theta1(6), theta2(6), theta1(7), theta2(7), theta1(8), theta2(8), &
        & theta1(9), theta2(9), theta1(10), theta2(10), trim(comments(18))
   write (tempFileNum, 800) tmin(1), tmax(1), dt(1), tmin(2), tmax(2), dt(2), &
        & tmin(3), tmax(3), dt(3), tmin(4), tmax(4), dt(4), trim(comments(19))
   write (tempFileNum, 300) nevType, trim(comments(20))
   write (tempFileNum, 300) nPreqType, trim(comments(21))
   write (tempFileNum, 300) iPisa, trim(comments(22))
   if(ipisa.eq.one) then
      write (tempFileNum, 950) ang, dang, trim(comments(23))
   endif
   write (tempFileNum, 300) iHist, trim(comments(24))
   write (tempFileNum, 300) iTyp, trim(comments(25))
   write (tempFileNum, 300) nh, trim(comments(26))
   if ( type.eq.one ) then
      if ( nh.gt.zero ) then
         do j = 1, nh
            write (tempFileNum, 150) trim(heading(j))
         enddo
      endif
   elseif ( type.eq.two ) then

      ! Using LAQGSM file, make input comments.
      write(tempFileNum, 3000) 'Target:', aTarg, zTarg
      write(tempFileNum, 3000) 'Projectile:', aproj, zproj
      write(tempFileNum, 3100) t0MeV, T0
      write(tempFileNum, 3200) trim(outfile), trim(auxFile1)
      write(tempFileNum, 150) trim(heading(5))

   else

      ! Invalid specifier
      write(*, 1600) tempFileName
      stop

   endif

   ! File stop signal
   if ( formatSpec.eq.one ) then

      ! Use -2, -2, -2 format.
      write (tempFileNum, 125) trim(stopString(1)), trim(comments(29))
   else

      ! Use ZAID format.
      write (tempFileNum, 125) trim(stopString(2)), trim(comments(29))

   endif

90 continue

   ! Informing user of the file being converted.
   write(*, 1400) numConversions, trim(tempFileName), trim(fileName)
   close(tempFileNum)
   close(fileNum)
   return

100 format (A, T35, A) 
125 format (A, T20, A)
150 format (A) 
200 format (f12.4, T20, A)
250 format (f5.1, T20, A)
300 format (i3, T20, A)
350 format (i1, T20, A)
400 format (e10.4, T20, A)
500 format (i2, ', ', i2, ', ', f4.1, T20, A)
600 format (i1, ', ', i1, T20, A)
650 format (i3, ', ', i3, T20, A)
700 format (20f8.2, 5X, A)
800 format (12f13.2, 5X, A)
950 format (11f8.2, 5X, A)
1400 format(i3, '. "', A, '" converted to "', A, '".')
1500 format('ERROR: incorrect format specifier passed into "writeInput.f90"' &
     & ' for file "', A, '".')
1600 format('ERROR: invalid file type specifer passed into "writeInput.f90"' &
     & ' for file "', A, '".')
3000 format(A, ' A = ', f5.1, ', Z = ', f5.1)
3100 format('Incident Energy: ', f11.2, ' MeV (', f8.3, ' GeV/nucleon)')
3200 format('Output File: "', A, '", Aux File: "', A, '".')

 end subroutine writeInput
! =============================================================================
