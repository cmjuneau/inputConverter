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
