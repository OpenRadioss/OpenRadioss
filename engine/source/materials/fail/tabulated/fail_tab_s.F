Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    fail_tab_s            ../engine/source/materials/fail/tabulated/fail_tab_s.F
!||--- called by ------------------------------------------------------
!||    mmain                 ../engine/source/materials/mat_share/mmain.F90
!||    mmain8                ../engine/source/materials/mat_share/mmain8.F
!||    mulaw                 ../engine/source/materials/mat_share/mulaw.F90
!||    mulaw8                ../engine/source/materials/mat_share/mulaw8.F90
!||    usermat_solid         ../engine/source/materials/mat_share/usermat_solid.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE FAIL_TAB_S (FAIL    ,
     1     NEL     ,NUVAR   ,NVARTMP ,UVAR    ,VARTMP   ,
     2     TIME    ,NGL     ,ALDT    ,
     4     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,
     5     DPLA    ,EPSP    ,TSTAR   ,OFF     ,DFMAX   ,TDELE    )
C---------+---------+---+---+--------------------------------------------
      use fail_param_mod
      use table_mat_vinterp_mod
C---------+---------+---+---+--------------------------------------------
C   /FAIL/TAB - tabulated rupture criteria for solids
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include "mvsiz_p.inc"
#include "units_c.inc"
#include "impl1_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NUVAR
      INTEGER, INTENT(IN) :: NVARTMP
      INTEGER, INTENT(IN) :: NEL
      INTEGER NGL(NEL)

      my_real TIME,ALDT(NEL),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   DPLA(NEL),EPSP(NEL),TSTAR(NEL)     
      type (fail_param_) ,intent(in) :: fail    !< failure model data structure
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER, DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
      my_real ,DIMENSION(NEL,NUVAR)   ,INTENT(INOUT) :: UVAR
      my_real :: OFF(NEL), DFMAX(NEL),TDELE(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IDEL,IDEV
      INTEGER :: IFUN_EL,IFUN_TEMP,IFUN_DMG ,IFUN_EPSF
      INTEGER :: SFLAG
      INTEGER, DIMENSION(MVSIZ) :: INDX
      INTEGER :: NINDX,NINDX_2
      INTEGER, DIMENSION(MVSIZ) :: INDX_2
      INTEGER, DIMENSION(NEL,3) :: IPOS
      my_real, DIMENSION(MVSIZ) :: EPSF,DP
      my_real :: DCRIT,DD,DN,EL_REF
      my_real ::P,SIGM,SVM,SXX,SYY,SZZ,XI,THETA, DET
      my_real ,dimension(nel)   :: lambda,fade_expo,size_fac,y_loc,dydx
      my_real ,dimension(nel,3) :: xvec
C=======================================================================
C     INITIALIZATIONS
C-----------------------------------------------
        SFLAG   = INT(FAIL%UPARAM(1))    
        DCRIT   = FAIL%UPARAM(4)
        DD      = FAIL%UPARAM(5)
        DN      = FAIL%UPARAM(6)
        EL_REF  = FAIL%UPARAM(9)
c---
        IDEL = 0
        IDEV = 0
        INDX = 0                                  
        IF (SFLAG == 1) THEN
            IDEL=1
        ELSEIF (SFLAG == 2) THEN
            IDEV =1
        ELSEIF (SFLAG == 3) THEN
            IDEV = 2
        ENDIF
C-------------------------------------------------------------------
      ifun_epsf = 1    ! failure strain table     
      ifun_el   = 3    ! element size function
      ifun_temp = 4    ! temperature scale function
      ifun_dmg  = 5    ! damage function
C-------------------------------------------------------------------
c     Failure strain value - function interpolation
C-------------------------------------------------------------------
        DO I=1,NEL
C---    failure strain interpolation
            P   = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))     
            SXX = SIGNXX(I) - P                                 
            SYY = SIGNYY(I) - P                                 
            SZZ = SIGNZZ(I) - P                                 
            SVM = HALF*(SXX**2 + SYY**2 + SZZ**2)             
     .          + SIGNXY(I)**2+ SIGNZX(I)**2 + SIGNYZ(I)**2    
            SVM = SQRT(THREE*SVM)                               
            SIGM = P / MAX(EM20,SVM)                            
C----
            DET = SXX*SYY*SZZ + TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I)- 
     .            SXX*SIGNYZ(I)**2-SZZ*SIGNXY(I)**2-SYY*SIGNZX(I)**2

            XI = ONE/MAX(EM20,SVM**3)
            XI = HALF*TWENTY7*DET*XI         
            IF(XI < -ONE) XI = -ONE
            IF(XI > ONE) XI = ONE
            THETA = ONE - TWO*ACOS(XI)/PI
            xvec(I,1)=SIGM 
            xvec(I,2)=EPSP(I)
            xvec(I,3)=THETA
        ENDDO
        !
        call table_mat_vinterp(fail%table4d(ifun_epsf),nel,nel,vartmp,xvec,epsf,dydx)
        !
        ! element size scale factor
        if (fail%table4d(ifun_el)%notable > 0) then
          lambda(1:nel) = aldt(i) / el_ref
          call table_mat_vinterp(fail%table4d(ifun_el),nel,nel,vartmp(1:nel,4:4),lambda,size_fac,dydx)
          epsf(1:nel) = epsf(1:nel) * size_fac(1:nel)
        end if
!
        !  temperature scale function
        if (fail%table4d(ifun_temp)%notable > 0) THEN
          call table_mat_vinterp(fail%table4d(ifun_temp),nel,nel,vartmp(1:nel,5:5),tstar,size_fac,dydx)
          epsf(1:nel) = epsf(1:nel) * size_fac(1:nel)
        end if
!
c-------------------------------------------------------------------
c---- element will be deleted 
        IF (IDEL == 1) THEN
            DO I=1,NEL
                IF (OFF(I) < 0.1) OFF(I)=ZERO
                IF (OFF(I) < ONE)  OFF(I)=OFF(I)*FOUR_OVER_5
            ENDDO
        ENDIF
C      
        IF (IDEL == 1) THEN
          nindx   = 0
          nindx_2 = 0
      ! 
          do i=1,nel
            if (sflag == 1 .and. off(i) == one) then
              nindx_2 = nindx_2+1                                                 
              indx_2(nindx_2) = i
            endif
          enddo
!
          if (fail%table4d(ifun_dmg)%notable > 0 ) then
!
#include "vectorize.inc"
            do j=1,nindx_2                           
              i = indx_2(j)            
              xvec(j,1) = uvar(i,1)
              ipos(j,1) = vartmp(i,6)
            enddo
!
            call table_mat_vinterp(fail%table4d(ifun_dmg),nel,nindx_2,ipos,xvec,y_loc,dydx)
!
#include "vectorize.inc" 
          do j=1,nindx_2                           
            i = indx_2(j)  
            dp(i) = y_loc(j)
            vartmp(i,6) = ipos(j,1)
          end do
        else
          do j=1,nindx_2                           
            i = indx_2(j)  
            dp(i) = dn*dd**(one-one/dn)
          end do
        endif
            ! --------------
#include "vectorize.inc"
            DO J=1,NINDX_2
                I = INDX_2(J)
                IF (EPSF(I) > ZERO) UVAR(I,1)=UVAR(I,1)+DP(I)*DPLA(I)/EPSF(I)      
                IF (UVAR(I,1) >= DCRIT) THEN
                    OFF(I)=FOUR_OVER_5
                    NINDX=NINDX+1
                    INDX(NINDX)=I
                    TDELE(I) = TIME                                  
                ENDIF 
            ENDDO
            IF (NINDX > 0 .AND. IMCONV == 1)THEN
                DO J=1,NINDX
#include "lockon.inc"
                    WRITE(IOUT, 1000) NGL(INDX(J))
                    WRITE(ISTDO,1100) NGL(INDX(J)),TIME
#include "lockoff.inc"
                ENDDO
            ENDIF         
        ENDIF
C---- 
        IF (IDEV > 0) THEN ! element deleted when rupture in all integration points
          !
          nindx   = 0
          nindx_2 = 0
          do i=1,nel
            if (off(i) == one .and. (sflag==2 .or. sflag==3)) then
              nindx_2 = nindx_2+1                                                 
              indx_2(nindx_2) = i
              if (uvar(i,1) < dcrit) then
                nindx = nindx+1                                                 
                indx(nindx) = i
              end if
            end if
          end do
!
         if (fail%table4d(ifun_dmg)%notable > 0 ) then
#include "vectorize.inc"
            do j=1,nindx                           
              i = indx(j)            
              xvec(j,1) = uvar(i,1)
              ipos(j,1) = vartmp(i,6)
            enddo
!
            call table_mat_vinterp(fail%table4d(ifun_dmg),nel,nindx,ipos,xvec,y_loc,dydx)
!
#include "vectorize.inc" 
            do j=1,nindx                          
              i = indx(j)  
              dp(i) = y_loc(j)
              vartmp(i,6) = ipos(j,1)
            end do

        else  ! ifun_dmg = 0
          do j=1,nindx                         
            i = indx(j)
            dp(i) = dn*dd**(one-one/dn)           
          enddo
        end if
            ! --------------
        nindx = 0
#include "vectorize.inc"
            DO J=1,NINDX_2
                I = INDX_2(J)    
                IF (UVAR(I,1) < DCRIT)THEN
                    IF (EPSF(I) > ZERO) UVAR(I,1)=UVAR(I,1)+DP(I)*DPLA(I)/EPSF(I)      
                    IF (UVAR(I,1) >= DCRIT) THEN
                        NINDX=NINDX+1
                        INDX(NINDX)=I
                        P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
                        IF (SFLAG == 2) THEN
                            SIGNXX(I) = P
                            SIGNYY(I) = P
                            SIGNZZ(I) = P
                        ENDIF
                    ENDIF
                ELSEIF (SFLAG == 2) THEN   !  UVAR > DCRIT     
                    P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
                    SIGNXX(I) = P
                    SIGNYY(I) = P
                    SIGNZZ(I) = P
                ENDIF
            ENDDO
            ! --------------
            IF (NINDX > 0.AND.IMCONV == 1) THEN
                DO J=1,NINDX
                    I = INDX(J)
#include "lockon.inc"
                    WRITE(IOUT, 2000) NGL(I)
                    WRITE(ISTDO,2100) NGL(I),TIME
#include "lockoff.inc"
                END DO
            ENDIF           
            ! --------------
        ENDIF   ! idev > 0
!
C-------------Maximum Damage storing for output : 0 < DFMAX < 1--------------    
        DO I=1,NEL
            DFMAX(I)= MIN(ONE,MAX(DFMAX(I),UVAR(I,1)/DCRIT))
        ENDDO
C-----------------------------------------------
 1000   FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10)
 1100   FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',1PE12.4)
C     
 2000   FORMAT(1X,' DEVIATORIC STRESS SET TO ZERO',I10)
 2100   FORMAT(1X,' DEVIATORIC STRESS SET TO ZERO',I10,
     .          ' AT TIME :',1PE12.4)
c-----------           
        RETURN
        END
