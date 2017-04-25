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
! CMJ       | 04/25/2017 | Made ZAID Format Standard
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
    ! write(*, 1005)
    ! if ( default ) then
    !
    !    userAns = defaultAns(1)
    !    write(*, 10000) userAns
    !
    ! else
    !
    !    read(*, *) userAns
    !
    ! endif


    ! Default is ZAID - CMJ [04/2017]
    user.Ans = defaultAns(3)


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
