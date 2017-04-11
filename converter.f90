! =============================================================================
  program convertInput
    implicit none

! -----------------------------------------------------------------------------
! This program is intended to allow users of CEM and LAQGSM to convert pre-
! existing input files into input files intened for the current 2017 version of
! CEM03.03.  This program recognizes 2 different types of input format; the re-
! arranged format and the ZAID format.  Both formats are compatible with the
! 2017 version of CEM03.03 (CEM-LAQ).  When input errors exist, this program
! tries to guess the remaining values in the input.
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
    integer :: fileNum, formatSpec, i, j, k, maxFiles, maxLists, num, &
         & stopSignal, overWrite, totConversions, zero, one, two
    character(LEN = 30) :: inputList, fileName, inputLists(2)
    character(LEN =  1) :: skipIt(2), userAns, defaultAns(12), blank
    logical :: default

! =====================================
! Data Blocks
! =====================================
    data stopSignal / 0.d0 /
    data totConversions / 0.d0 /
    data overWrite / 0.d0 /
    data zero, one, two / 0.d0, 1.d0, 2.d0 /
    data maxFiles, maxLists / 99999.d0, 2.d0 /
    data default / .false. /
    data skipIt, blank / 's', 'S', '' /
    data defaultAns / 'M', 'm', 'Z', 'z', 'N', 'n', 'O', 'o', 'Y', 'y', 'N', &
         & 'n' /
    data inputLists / 'cemList.txt', 'laqList.txt' /

! =============================================================================
! Initializing Variables
! =====================================
    ! Statement to user
    write(*, 1020)
    write(*, 1000)
    write(*, 1050)

10  continue

! Determining new input file format
    write(*, 1005)
    if ( default ) then

       userAns = defaultAns(1)
       write(*, 10000) userAns

    else

       read(*, *) userAns

    endif

    if ( userAns.eq.defaultAns(1) .OR. userAns.eq.defaultAns(2) ) then

       ! Change input files to use the modified format
       formatSpec = one
       write(*, 1010) 'modified'

    elseif ( userAns.eq.defaultAns(3) .OR. userAns.eq.defaultAns(4) ) then

       ! Change input files to use the ZAID format
       formatSpec = two
       write(*, 1010) 'ZAID'

    else

       ! Incorrectly Entered Value
       go to 10

    endif
    write(*, 1050)

15  continue

    ! Determining if user wants to overwrite file or not
    write(*, 1007)
    if ( default ) then

       userAns = defaultAns(5)
       write(*, 10000) userAns

    else

       read(*, *) userAns

    endif

    ! Determine user answer
    if ( userAns.eq.defaultAns(7) .OR. userAns.eq.defaultAns(8) ) then

       ! Ensuring user wishes to overwrite file
20     continue
       write(*, 1008)
       read(*, *) userAns

       ! Determining user answer
       if ( userAns.eq.defaultAns(9) .OR. userAns.eq.defaultAns(10) ) then

          ! User wishes to overwrite current inputs

       elseif( userAns.eq.defaultAns(11) .OR. userAns.eq.defaultAns(12) ) then

          ! Do NOT override
          go to 15

       else

          ! Incorrectly Entered Value
          go to 20

       endif

       ! Overwrite current input files
       overWrite = one
       write(*, 1012) 'overwrite the current'

    elseif ( userAns.eq.defaultAns(5) .OR. userAns.eq.defaultAns(6) ) then

       ! Create new input files
       overWrite = zero
       write(*, 1012) 'create a new'

    else

       ! Incorrectly Entered Value
       go to 15

    endif
    write(*, 1050)

! =====================================
! Main Loop
! =====================================
    default = .true.

    do i = 1, maxLists

! Obtaining Input List Names
       write(*, 1050)
       write(*, 1020)
       num = i
       if ( i.eq.one ) then

          if ( default ) then

             inputList = inputLists(i)

          else

             ! Obtaining CEM List Name
             write(*, 1200) 'CEM', skipIt(1)

          endif

       elseif ( i.eq.two ) then

          if ( default ) then

             inputList = inputLists(i)

          else

             ! Obtaining LAQGSM List Name
             write(*, 1200) 'LAQGSM', skipIt(1)
             read(*,*) inputList

          endif

       else

! Have exceeded program limits, need more information.

          ! Obtaining Unknown List Name
          write(*, 1200) blank, skipIt(1)
          read(*,*) inputList

          ! Getting input format from user
          write(*, 1040) trim(inputList)
30        continue
          read(*,*) num

          ! Validating user answer
          if ( num.gt.two ) then

             ! Incorrect response
             write(*, 1045)
             go to 30

          endif

       endif

! Allowing user to skip this kind of file
       if ( inputList.eq.skipIt(1) .OR. inputList.eq.skipIt(2) ) then

          ! Skipping this type of input list; i.e. skipping cem or laqgsm
          ! convertion
          go to 90

       endif

       ! Writing to user what input list is being looked at
       write(*, 1250) trim(inputList)

       ! Opening input List
       open(num, file = inputList, status = 'old')
       write(*, 1050)
       write(*, 1050)
       write(*, 1300) trim(inputList)
       write(*, 1025)

! We must still convert dt0 variable, all others no necessary.
!       ! If CEM input w/ ZAID format, no change, skip.
!       if ( formatSpec.eq.two .AND. num.eq.one ) then
!
!          ! No need to convert file, informing user and skipping inputList
!          write(*, 1030)
!          go to 90
!
!       endif

       ! Converting each input file listed in the inputList
       do j = 1, maxFiles

          ! Determining file number
          fileNum = 10000*num + j

          ! Reading a line from the file
          read(num, *, end = 90) fileName

          ! Checking if blank line was read, if so, start loop over
          if ( len_trim(fileName).eq.0 ) then

             ! Blank line was read
             go to 80

          endif

          ! Resets all variables before reading input
          call resetVars(num)

          ! Reading input file
          call readInput(num, fileNum, fileName, stopSignal)
          if ( stopSignal.ne.zero ) then
             write(*, 1500) trim(fileName)
             stop
          endif

          ! Converts all LAQGSM variables to CEM ones
          call varConversion(num, formatSpec, fileName, stopSignal)
          if ( stopSignal.ne.zero ) then
             write(*, 1600) trim(fileName)
             stop
          endif

          ! Writing input file
          call writeInput(j, fileNum, fileName, overWrite, formatSpec, num)

          totConversions = totConversions + one

80        continue

       enddo

! =====================================
90     continue

! End of Input list, going on to next list
       close(num)

    enddo

! =====================================
! Done with all files, exiting program
! =====================================
    write(*, 1050)
    write(*, 1020)
    write(*, 1100) "Done with"
    write(*, 1700) totConversions
    write(*, 1020)
    write(*, 1050)
    return

! =====================================
! Format Statements
! =====================================
1000 format('Running input converting script')
1005 format('Do you want to Convert to the Modified (M) or the ZAID (Z)' &
     & ' format?')
1007 format('Do you want to overwrite (O) the file or create a new (N)' &
     & ' one?')
1008 format('Are you sure you wish to overwrite files (Y/N)?')
1010 format('I will be converting the input files to follow the "', A, &
     & '" format.')
1012 format('I will ', A, ' file.')
1020 format('=================================')
1025 format('---------------------------------')
1030 format('Current CEM Input Files are valid with the ZAID format.' &
     & '  Conversion not necessary.')
1040 format("We've exceeded my defaults, I need your help.  Is", ' "', A, &
     & '" a CEM (1) or LAQGSM (2) input list?')
1045 format('Please enter either (1) for CEM, or (2) for LAQGSM:')
1050 format('')
1100 format(A, ' input file Conversion.')
1200 format('Please enter the filename for the ', (A), ' Input List' &
     & ' below ("', A, '" to skip):')
1250 format('Checking input list "', A, '".')
1300 format('Reading input files from "', A, '".')
1500 format('ERROR: Input conversions halted at "', A, &
     & '". Please fix the issues and run program again.')
1600 format('ERROR: Unable to determine appropriate input type or format,' &
     & ' halting program at "', A, '".')
1700 format(i3, ' files successfully converted.')
10000 format(A)

  end program convertInput
! =============================================================================
! =============================================================================

  subroutine resetVars(num)
    implicit none

! -----------------------------------------------------------------------------
! This subroutine is intended to reset the variables used within both the CEM
! and LAQGSM input files.  This routine is not necessary to be used, however
! reduces potential bugs in the program.
! 
! Completed Original Devlopment by CMJ, 04/07/2017
! 
! -----------------------------------------------------------------------------
! -------------------------------  Editor  Log  -------------------------------
! -----------------------------------------------------------------------------
! Editor    | Date       | Edit Description
! ----------|------------|-----------------------------------------------------
! CMJ       | 04/07/2017 | Original Program Completion
!           |            | 
!           |            | 
!           |            | 
!           |            | 
! 
! -----------------------------------------------------------------------------

! =====================================
! Variables
! =====================================
    integer :: limc, mspec, mpyld, mchy, misy, mdubl, mang, ipar1, ipar2, &
         & nevType, nPreqType, iPisa, iHist, iTyp, nh, JJJ, INSP, iyeld, &
         & IGSI, ZAID, ibrems, i, num, zero, one, nine, sixtySix
    real :: t0MeV, dt0, t0Max, dtheta, theta1(10), theta2(10), tmin(4), &
         & tmax(4), dt(4), ang(10), dang, atarg, ztarg, stin, T0, tstep(10), &
         & pmin, aproj, zproj, cm0
    character(LEN = 99) :: heading(10)
    character(LEN = 30) :: auxFile1, auxFile2, outFile
    character(LEN =  6) :: pnameString
    character(LEN =  4) :: pname
    character(LEN =  3) :: stat
    character(LEN =  1) :: blank

! =====================================
! Common Blocks for all variables
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
    data zero, one, nine, sixtySix / 0.d0, 1.d0, 9.d0, 66.d0 /
    data blank / '' /

! =============================================================================
! Resetting Variables
! =====================================

    ! Resetting CEM Variables
    atarg = zero
    ztarg = zero
    limc = zero
    mspec = zero
    mpyld = zero
    mchy = zero
    misy = zero
    mdubl = zero
    mang = zero
    ipar1 = one
    ipar2 = nine
    nevType = sixtySix
    iPisa = zero
    iHist = zero
    iTyp = one
    nh = zero
    t0MeV = zero
    dt0 = zero
    t0Max = zero
    dtheta = zero
    dang = zero
    do i = 1, 10
       theta1(i) = zero
       theta2(i) = zero
       ang(i) = zero
       heading(i) = blank
    enddo
    auxFile1 = blank
    outFile = blank
    pname = blank

    ! Resetting LAQGSM Variables
    if ( num.gt.1 ) then

       JJJ = zero
       INSP = zero
       iyeld = zero
       IGSI = zero
       stin = zero
       T0 = zero
       pmin = zero
       aproj = zero
       zproj = zero
       ibrems = zero
       do i = 1, 10
          tstep(i) = zero
       enddo
       auxFile2 = blank
       stat = blank
       pnameString = blank
    endif

    return

  end subroutine resetVars
! =============================================================================
! =============================================================================
  subroutine readInput(num, fileNum, fileName, stopSignal)
    implicit none

! -----------------------------------------------------------------------------
! This subroutine is meant to read the input from the input list.  If there is
! an error in the input file, this subroutine assumes the value if possible and
! warns the user of this assumption.
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
    integer :: limc, mspec, mpyld, mchy, misy, mdubl, mang, ipar1, ipar2, &
         & nevType, nPreqType, iPisa, iHist, iTyp, nh, ih, j, JJJ, INSP, &
         & iyeld, IGSI, ZAID, ibrems, fileNum, num, tempInt, stopSignal, &
         & zero, one, two, four, five, nine, ten, sixtySix
    real :: t0MeV, dt0, t0Max, dtheta, theta1(10), theta2(10), tmin(4), &
         & tmax(4), dt(4), ang(10), dang, atarg, ztarg, stin, T0, tstep(10), &
         & pmin, aproj, zproj, cm0, tempReal
    character(LEN = 99) :: tempString, heading(10)
    character(LEN = 30) :: auxFile1, auxFile2, outFile, fileName
    character(LEN =  6) :: pnameString
    character(LEN =  5) :: tempString1, tempString2, tempString3
    character(LEN =  4) :: pname
    character(LEN =  3) :: stat

! =====================================
! Common Blocks for all variables
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
    data zero, one, two, four, five, nine, ten, sixtySix / 0.d0, 1.d0, 2.d0, &
         & 4.d0, 5.d0, 9.d0, 10.d0, 66.d0 /

! =============================================================================
! Opening and Reading Input file
! =============================================================================

    ! Opening File
    open(fileNum, file = fileName, status = 'old')

    ! Reading input file
    if ( num.eq.one ) then

       ! Reading CEM Input File
       read (fileNum, 1200, end=10) auxFile1
       read (fileNum, 1200, end=10) outfile
       read (fileNum, 1200, end=10) pname
       read (fileNum, 1200, end=10) tempString	! Continuing list of pname options
       read (fileNum, 1200, end=10) tempString	! Continuing list of pname options
       read (fileNum, *, end=10) t0MeV
       read (fileNum, *, end=10) atarg
       read (fileNum, *, end=10) ztarg
       read (fileNum, *, end=10) limc
       read (fileNum, *, end=10) dt0
       read (fileNum, *, end=10) t0Max
       read (fileNum, *, end=20) dtheta
       read (fileNum, *, end=21) mspec
       read (fileNum, *, end=22) mpyld
       read (fileNum, *, end=23) mchy
       read (fileNum, *, end=24) misy
       read (fileNum, *, end=25) mdubl
       read (fileNum, *, end=26) mang
       read (fileNum, *, end=27) ipar1, ipar2
       read (fileNum, *, end=30) (theta1(j), theta2(j), j = 1,10)
       read (fileNum, *, end=31) (tmin(j), tmax(j), dt(j), j = 1,4)
       read (fileNum, *, end=32) nevType
       read (fileNum, *, end=33) nPreqType
       read (fileNum, *, end=34) ipisa
       if ( ipisa.ne.zero ) read (fileNum, *, end=35) ang, dang
       read (fileNum, *, end=36) ihist
       read (fileNum, *, end=37) ityp
       read (fileNum, *, end=38) nh
       if ( nh.gt.zero ) read (fileNum, 1200, end=38) (heading(ih), ih=1,nh)

       ! Stop Signal
       read (fileNum, *, end=100) tempString

       ! Skipping past possible errors in input; should not read past this point.
       go to 100

       ! The following "go to" statements are meant to operate as an
       ! equivalent "switch" statement in C++, however the cases intentionally
       ! fall through to make sure all variables are created (the input was done).

10     continue
       ! Error before reaching mspec and below
       write(*, 1000)
       stopSignal = one
       go to 100

20     continue
       dtheta = five
       write(*, 1100) 'dtheta', dtheta
21     continue
       mspec = one
       write(*, 1150) 'mspec', mspec
22     continue
       mpyld = one
       write(*, 1150) 'mpyld', mpyld
23     continue
       mchy = zero
       write(*, 1150) 'mchy', mchy
24     continue
       misy = one
       write(*, 1150) 'misy', misy
25     continue
       mdubl = one
       write(*, 1150) 'mdubl', mdubl
26     continue
       mang = one
       write(*, 1150) 'mang', mang
27     continue
       ipar1 = one
       ipar2 = nine
       write(*, 1150) 'ipar1', ipar1
       write(*, 1150) 'ipar2', ipar2
30     continue
       ! Angles are not correctly specified
       do j = 1,10
          theta1(j) = ten*j-two
          theta2(j) = ten*j+two
       enddo
       write(*, 1300) 'theta1, theta2', &
            & 'be integer multiples of 10, width of 2'
31     continue
       ! Momentum bins not created
       do j = 1,10
          tmin(j) = ((j-one)/four)*t0MeV
          tmax(j) = (j/four)*t0MeV
          dt(j) = j*one ! 1 MeV/c step size (or 1 MeV KE step size)
       enddo
       write(*, 1300) 'tmin, tmax, dt', 'range from 0 to t0MeV evenly'
32     continue
       nevType = sixtySix
       write(*, 1150) 'nevType', nevType
33     continue
       nPreqType = sixtySix
       write(*, 1150) 'nPreqType', nPreqType
34     continue
       ! Angles are not correctly specified
       ipisa = one
       write(*, 1150) 'ipisa', ipisa
35     continue
       ! Momentum bins not created
       do j = 1,10
          ang(j) = (theta1(j) + theta2(j))/two
       enddo
       dang = (theta2(j) - theta1(j))/two
       write(*, 1300) 'ang(10), dang', &
            & 'be the average of theta1(10) and theta2(10) values.'
36     continue
       ihist = one
       write(*, 1150) 'ihist', ihist
37     continue
       iTyp = one
       write(*, 1150) 'iTyp', iTyp
38     continue
       nh = four
       write(*, 1150) 'nh', nh
39     continue
       write(tempString1, '(f5.1)') aTarg
       write(tempString2, '(f5.1)') zTarg
       write(tempString3, '(f5.1)') t0MeV
       heading(1) = 'Target: A = ' // tempString1 // ', Z = ' // tempString2
       heading(2) = 'Projectile: ' // pname
       heading(3) = 'Incident Energy: ' // tempString3 // ' MeV.'
       heading(4) = 'Output File: ' // trim(outfile) // ', Aux File: ' &
            & // trim(auxFile1) // '.'
       write(*, 1300) 'heading(4)', &
            & 'contain particle and file name information'

    elseif ( num.eq.two ) then

       ! Reading LAQGSM Input File
       read (fileNum, 1200, end=50) outFile
       read (fileNum, 1200, end=50) auxFile1
       read (fileNum, 1200, end=50) auxFile2
       read (fileNum, 1200, end=50) tempString
       read (fileNum, 1200, end=50) tempString
       read (fileNum, 1200, end=50) tempString
       read (fileNum, 1200, end=50) tempString
       read (fileNum, 1200, end=50) tempString
       read (fileNum, 1200, end=50) tempString
       read (fileNum, *, end=51) aproj, atarg, zproj, ztarg, stin, cm0, T0, &
            & limc
       if ( cm0.lt.0.0001) read (fileNum, *, end = 52) ibrems
       read (fileNum, *, end=53) JJJ, tempInt, INSP, iyeld, nevType, IGSI
       read (fileNum, *, end=54) ang, dang
       read (fileNum, *, end=55) tstep, pmin
       ! Everything else is a comment in the input file

       go to 100

50     continue
       write(*, 1500)
       stopSignal = one
51     continue
       write(*, 1550)
       stopSignal = one
52     continue
       ibrems = one
       write(*, 1150) 'ibrems', ibrems
53     continue
       JJJ = one
       INSP = one
       iyeld = one
       nevType = sixtySix
       IGSI = one
       write(*, 1300) 'JJJ, INSP, iyeld, nevType, IGSI', &
            & 'all be 1, nevType 66.'
54     continue
       do j = 1, 10
          ang(j) = j*ten
       enddo
       dang = four
       write(*, 1300) 'theta(10), dtheta', 'be in intervals of 10.'
55     continue
       do j = 1, 10
          tstep(j) = 0.100
       enddo
       pmin = zero
       write(*, 1300) 'tstep(10), pmin', 'be 0.100 and 0.000.' 

    endif

100 continue

! Closing file - done in "writeInput" subroutine; may rewind file to overwrite
    ! close(fileNum)

    return

1000 format('Input Error: no data past t0Max variable.', 1X, &
     & 'Unable to assume remaining varaiables.')
1100 format('Assuming ', A, ' is ', f3.1) ! Real Numberes
1150 format('Assuming ', A, ' is ', i3) ! Integers
1175 format('Assuming ', A, ' is ', e15.7) ! Scientific Notation
1200 format(A)
1300 format(A, ' assumed to ', A '.')
1500 format('Input ended when reading filenames, stopping conversion.')
1550 format('Input ended before all particle data obtained,' &
     & ' stopping conversion.')

  end subroutine readInput
! =============================================================================
! =============================================================================

  subroutine varConversion(type, formatSpec, fileName, stopSignal)
    implicit none

! This subroutine is meant to convert variables from LAQGSM to CEM,
! and no conversion is necessary for original CEM input files.
! 
! Completed Original Devlopment by CMJ, 04/10/2017
! 
! -----  Editor  Log  -----
! Editor    | Date       | Edit Description
! ----------|------------|-----------------------------------------------------
! CMJ       | 04/10/2017 | Original Program Completion
!           |            | 
!           |            | 
!           |            | 
!           |            | 
! 

! =====================================
! Variables
! =====================================
    integer :: limc, mspec, mpyld, mchy, misy, mdubl, mang, ipar1, ipar2, &
         & nevType, nPreqType, iPisa, iHist, iTyp, nh, ih, JJJ, INSP, iyeld, &
         & IGSI, ZAID, ibrems, type, formatSpec, fileNum, num, tempInt, &
         & stopSignal, j, zero, one, two, four, five, nine, ten, fifty
    real :: t0MeV, dt0, t0Max, dtheta, theta1(10), theta2(10), tmin(4), &
         & tmax(4), dt(4), ang(10), dang, atarg, ztarg, stin, T0, tstep(10), &
         &  pmin, aproj, zproj, cm0, tempReal
    character(LEN = 99) :: tempString, heading(10)
    character(LEN = 30) :: auxFile1, auxFile2, outFile, fileName
    character(LEN =  6) :: pnameString
    character(LEN =  5) :: tempString1, tempString2, tempString3
    character(LEN =  4) :: pname, parName(8)
    character(LEN =  3) :: stat
    character(LEN =  2) :: spaces

! ==========================================
! Common Blocks for all variables
! ==========================================
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
    data parname /'prot','neut','pipl','pimi','pize','gamm','gamb', &
         'stop'/ 
    data spaces / '  ' /
    data zero, one, two, four, five, nine, ten, fifty / 0.d0, 1.d0, 2.d0, &
         & 4.d0, 5.d0, 9.d0, 10.d0, 50.d0 /


! =============================================================================

    if ( type.eq.one ) then

! CEM Input; only need to create the ZAID if we want to use the ZAID format.
! Otherwise, we only need to re-arrange the input files.

! Converting for appropriate format
       if ( formatSpec.eq.one ) then

          ! Using the re-arranged input w/ A, Z of incident particle
          if ( pname.eq.parname(1) ) then
             aproj = one
             zproj = one
          elseif ( pname.eq.parname(2) ) then
             aproj = one
             zproj = zero
          elseif ( pname.eq.parname(3) ) then
             aproj = -one
             zproj = one
          elseif ( pname.eq.parname(4) ) then
             aproj = -one
             zproj = -one
          elseif ( pname.eq.parname(5) ) then
             aproj = -one
             zproj = zero
          elseif ( pname.eq.parname(6) ) then
             aproj = zero
             zproj = zero
          elseif ( pname.eq.parname(7) ) then
             aproj = zero
             zproj = one
          endif
          stin = zero

       elseif ( formatSpec.eq.2 ) then

          ! Using the ZAID Format, no changes; pname argument is valid
          pnameString = pname // spaces

       else

          ! Error
          write(*, 1000) 107, formatSpec
          stopSignal = one
          return

       endif

       ! Changing dt0 to match new requirement of alowing negative energies
       if ( dt0.lt.zero ) dt0 = zero

    elseif ( type.eq.two ) then

! LAQGSM Input; need to convert the appropriate variables over

! Converting for appropriate format
       if ( formatSpec.eq.one ) then

          ! Using the re-arranged input w/ A, Z of incident particle
          ! aproj, zproj, stin already established, just re-arrange variables
          ! Create ZAID for print in input comments
          ZAID = 1000*zproj + aproj
          write(pnameString, '(i6)') ZAID

       elseif ( formatSpec.eq.two ) then

          ! Using the ZAID Format, no stin
          if ( cm0.lt.0.0001 .AND. aproj.lt.one ) then

             ! Photon
             if ( ibrems.eq.zero ) then

                pnameString = parname(6) // spaces

             elseif ( ibrems.eq.one ) then

                pnameString = parname(6) // spaces

             endif

          elseif ( aproj.ge.one .AND. zproj.ge.zero ) then

             ! Other projectile
             ZAID = 1000*zproj + aproj
             write(pnameString, '(i6)') ZAID

          else

             ! Error
             write(*, 1000) 154, formatSpec
             stopSignal = one
             return

          endif

       else

          ! Error
          write(*, 1000) 163, formatSpec
          stopSignal = one
          return

       endif

       t0MeV = T0*1000*aproj ! Could use negative value for energy
       dt0 = zero
       t0Max = t0MeV + fifty
       ! limc value read in already
       dtheta = ten
       if ( JJJ.gt.zero ) then
          mpyld = one
       else
          mpyld = zero
       endif
       if ( INSP.gt.zero ) then
          mdubl = one
          mspec = one
       else
          mdubl = zero
          mspec = zero
       endif
       mchy = zero
       misy = iyeld
       mang = one
       ipar1 = one
       ipar2 = nine
       do j = 1, 10
          theta1(j) = ang(j) - two
          if ( j.eq.one .AND. theta1(j).lt.zero ) theta1(j) = zero
          theta2(j) = ang(j) + two
          if ( j.eq.one .AND. theta1(j).eq.zero ) theta2(j) = 2*ang(j)
       enddo
       do j = 1, 4
          tmin(j) = 1000*pmin + ((j-one)*t0MeV)/four
          tmax(j) = 1000*pmin + (j*t0MeV)/four
          dt(j) = 1000*tstep(j)
          if ( dt(j).eq.zero ) dt(j) = one
          if ( dt(j).gt.fifty ) dt(j) = ten ! Maximum of 10 MeV energy steps
       enddo
       ! nevType already defined
       nPreqType = nevType
       ipisa = one
       ! ang(10) and dang already specified, dang must be 5 though.
       dang = five
       ihist = one
       ityp = one
       nh = four
       ! heading(1-4) assignied in "writeInput.f90"

    else

       write(*, 1100)
       stopSignal = one
       return

    endif

    ! Adding conversion note to input file
    if ( nh.lt.ten ) then
       nh = nh + one
       heading(nh) = 'Input file converted using conversion script on file "' &
            & // trim(fileName) // '".'
    endif

    return

1000 format('ERROR: Unexpected format specified in "varConversion.f90"', &
     & ', line ', i3, '; error potentially with LAQGSM cm0 variable, or', &
     & ' projectile size.')
1100 format('ERROR: Unexpected input specified in "varConversion.f90"')
  end subroutine varConversion

! =============================================================================
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
