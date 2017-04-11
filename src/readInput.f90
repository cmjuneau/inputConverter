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
