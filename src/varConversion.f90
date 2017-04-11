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
