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
!||    fail_tensstrain_s   ../engine/source/materials/fail/tensstrain/fail_tensstrain_s.F
!||--- called by ------------------------------------------------------
!||    mmain               ../engine/source/materials/mat_share/mmain.F90
!||    mulaw               ../engine/source/materials/mat_share/mulaw.F90
!||    mulaw8              ../engine/source/materials/mat_share/mulaw8.F90
!||    usermat_solid       ../engine/source/materials/mat_share/usermat_solid.F
!||--- calls      -----------------------------------------------------
!||    finter              ../engine/source/tools/curve/finter.F
!||====================================================================
      SUBROUTINE FAIL_TENSSTRAIN_S(FAIL ,
     1           NEL    ,NUVAR  ,NVARTMP,UVAR    ,VARTMP ,
     3           NGL    ,ALDT   ,TSTAR  ,ISMSTR  ,TIME   ,
     4           EPSXX  ,EPSYY  ,EPSZZ  ,EPSXY   ,EPSYZ  ,EPSZX  ,
     5           SIGNXX ,SIGNYY ,SIGNZZ ,SIGNXY  ,SIGNYZ ,SIGNZX ,
     6           EPSP   ,OFF    ,DFMAX   ,TDEL  ,
     7           MFXX   ,MFXY   ,MFXZ   ,MFYX    ,MFYY   ,MFYZ   ,                 
     8           MFZX   ,MFZY   ,MFZZ   ,DMG_SCALE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      use fail_param_mod
      use table_mat_vinterp_mod
C-----------------------------------------------
C   Tensile Strain failure criterion
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C TIME    |  1      | F | R | CURRENT TIME
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
#include "impl1_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL,NUVAR,NVARTMP,ISMSTR
      INTEGER ,DIMENSION(NEL)       :: NGL
      my_real  :: TIME
      my_real ,DIMENSION(NEL) :: SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX,
     .   EPSXX,EPSYY,EPSZZ,EPSXY,EPSYZ,EPSZX,EPSP,ALDT,TSTAR,
     .   MFXX,MFXY,MFXZ,MFYX,MFYY,MFYZ,MFZX,MFZY,MFZZ
      my_real, DIMENSION(NEL), INTENT(INOUT) :: DMG_SCALE
      type (fail_param_) ,intent(inout) :: fail    !< failure model data structure
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER, DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP
      my_real UVAR(NEL,NUVAR), OFF(NEL), DFMAX(NEL),TDEL(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDX,NINDXD,NINDXF,S_FLAG,STRDEF,STRFLAG
      INTEGER ,DIMENSION(NEL) :: INDX,INDXD,INDXF
      my_real :: E1,E2,E3,E4,E5,E6,E42,E52,E62,C,D,EPST,EPST2,
     .       R1,R2,Y,YP,DAV,CC,Y1,I1,I2,I3,Q,R,PHI,
     .       R_INTER,EL_REF,EPSP1,EPSP2,E11,E22,E33,EPSF1,EPSF2
      my_real ,DIMENSION(NEL)   :: EPS11,EPS22,EPS33,EPS12,EPS23,EPS31,EPS_MAX,DAMAGE 
      my_real ,DIMENSION(NEL)   :: LAMBDA
      my_real ,dimension(nel)   :: xvec,dydx
      my_real ,dimension(nel)   :: rfac,rfac2
C=======================================================================
      STRFLAG = 0 
      STRDEF       = FAIL%IPARAM(1)
      S_FLAG       = FAIL%IPARAM(2)
!
      EPSF1        = FAIL%UPARAM(1)
      EPSF2        = FAIL%UPARAM(2)
      EPSP1        = FAIL%UPARAM(3)
      EPSP2        = FAIL%UPARAM(4)
      EL_REF       = FAIL%UPARAM(5)
c
      DAMAGE(:NEL)  = ZERO
      EPS_MAX(:NEL) = ZERO
      NINDXD  = 0  
      NINDXF  = 0  
C-----------------------------------------------
c     Initialization
C-----------------------------------------------
      IF (UVAR(1,1) == ZERO) THEN
          SELECT CASE (S_FLAG)
            CASE (2,3)
              IF (fail%table4d(2)%notable > 0)THEN
                LAMBDA(:) = ALDT(:) / EL_REF
                call table_mat_vinterp(fail%table4d(2),nel,nel,vartmp(1:nel,3:3),lambda,uvar(1:nel,1),dydx)
              ELSE
                UVAR(:,1) = ONE
              ENDIF
            CASE DEFAULT    ! old formulation, backward compatibility only
              UVAR(1:NEL,1) = EPSP1
          END SELECT
      ENDIF
c-----------------------------------------------
      DO I=1,NEL
        IF (OFF(I) < EM01) OFF(I)=ZERO
        IF (OFF(I) < ONE)  OFF(I)=OFF(I)*FOUR_OVER_5
      END DO
c----------------------------------------------
c     Max strain transformation following input definition
c-------------------
      IF (STRDEF == 2) THEN        ! failure defined as engineering strain
        IF (ISMSTR == 10 .or. ISMSTR == 12) THEN
          STRFLAG = 1
        ELSE IF (ISMSTR == 0 .or. ISMSTR == 2 .or. ISMSTR == 4) THEN
c         transform true strain to engineering         
          STRFLAG = 2
        END IF
      ELSE IF (STRDEF == 3) THEN   ! failure defined as true strain
        IF (ISMSTR == 1 .or. ISMSTR == 3 .or. ISMSTR == 11) THEN
c         transform engineering to true strain    
          STRFLAG = 3
        ELSE IF (ISMSTR == 10 .or. ISMSTR == 12) THEN
          STRFLAG = 4
        END IF
      END IF           
c
!
      ! Tag active elements
      nindx = 0  
      do i=1,nel
        if (off(i) == one) then
          nindx = nindx + 1
          indx(nindx) = i  
        end if
      end do
c-------------------------
      SELECT CASE (S_FLAG)
c-------------------------
        CASE (1)  ! Equivalent strain criterion (old). Backward compatibility only
c-------------------
          IF (STRFLAG == 1) THEN      ! Radioss strain is Cauchy-Green
c           transform total grad def strain to engineering
            DO I=1,NEL
              EPS11(I)  = MFXX(I)
              EPS22(I)  = MFYY(I)
              EPS33(I)  = MFZZ(I)
              EPS12(I)  = MFXY(I) + MFYX(I)
              EPS23(I)  = MFZY(I) + MFYZ(I)
              EPS31(I)  = MFXZ(I) + MFZX(I)
            END DO          
          ELSE IF (STRFLAG == 2) THEN     
c           transform engineering strain to true
            DO I=1,NEL
              EPS11(I)  = LOG(EPSXX(I) + ONE)
              EPS22(I)  = LOG(EPSYY(I) + ONE)
              EPS33(I)  = LOG(EPSZZ(I) + ONE)
              EPS12(I)  = LOG(EPSXY(I) + ONE)
              EPS23(I)  = LOG(EPSYZ(I) + ONE)
              EPS31(I)  = LOG(EPSZX(I) + ONE)
            END DO
          ELSE IF (STRFLAG == 3) THEN     
c           transform engineering strain to true
            DO I=1,NEL
              EPS11(I)  = LOG(EPSXX(I) + ONE)
              EPS22(I)  = LOG(EPSYY(I) + ONE)
              EPS33(I)  = LOG(EPSZZ(I) + ONE)
              EPS12(I)  = LOG(EPSXY(I) + ONE)
              EPS23(I)  = LOG(EPSYZ(I) + ONE)
              EPS31(I)  = LOG(EPSZX(I) + ONE)
            END DO
          ELSE IF (STRFLAG == 4) THEN      ! Radioss strain is Cauchy-Green
c           transform total grad def strain to true
            DO I=1,NEL
              EPS11(I)  = LOG(MFXX(I) + ONE)
              EPS22(I)  = LOG(MFYY(I) + ONE)
              EPS33(I)  = LOG(MFZZ(I) + ONE)
              EPS12(I)  = LOG(MFXY(I) + MFYX(I) + ONE)
              EPS23(I)  = LOG(MFXZ(I) + MFZX(I) + ONE)
              EPS31(I)  = LOG(MFZY(I) + MFYZ(I) + ONE)
            END DO
          ELSE
            EPS11(1:NEL)  = EPSXX(1:NEL)
            EPS22(1:NEL)  = EPSYY(1:NEL)
            EPS33(1:NEL)  = EPSZZ(1:NEL)
            EPS12(1:NEL)  = EPSXY(1:NEL)
            EPS23(1:NEL)  = EPSYZ(1:NEL)
            EPS31(1:NEL)  = EPSZX(1:NEL)
          END IF
c         calculate max strain
          DO J=1,NINDX
            I = INDX(J)
              DAV = (EPS11(I)+EPS22(I)+EPS33(I))*THIRD
              E1 = EPS11(I) - DAV
              E2 = EPS22(I) - DAV
              E3 = EPS33(I) - DAV
              E4 = HALF*EPS12(I)
              E5 = HALF*EPS23(I)
              E6 = HALF*EPS31(I)
              E42 = E4*E4
              E52 = E5*E5
              E62 = E6*E6
              C =  (E1*E2 + E1*E3 + E2*E3) - E42 - E52 - E62
              D = - E1*E2*E3 + E1*E52 + E2*E62 + E3*E42 - TWO*E4*E5*E6 
              CC =  C*THIRD
              EPST = SQRT(-CC)
              EPST2 = EPST * EPST
              Y = (EPST2 + C)* EPST + D
              Y1 = -(EPST2 + C)* EPST + D
              IF(ABS(Y) > EM8 .OR.(ABS(Y) < EM8 .AND. ABS(Y1) < ABS(Y)))THEN
                EPST = 1.75 * EPST
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
                
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
C                          
              ENDIF
              EPST       = EPST + DAV
              EPS_MAX(I) = EPST
          ENDDO

c         test failure crit          

          if (fail%table4d(1)%notable > 0) then   ! strain rate
            do j=1,nindx
              i = indx(j)
              xvec(j) = epsp(i)
            enddo
            call table_mat_vinterp(fail%table4d(1),nindx,nel,vartmp(1:nel,1:1),xvec,rfac,dydx)
            rfac(:) = max(rfac(:),em20)
          else
            rfac(:) = one
          endif
          !
          do j=1,nindx
            i = indx(j)
            r1 = epsf1*rfac(j)
            r2 = epsf2*rfac(j)
            if (eps_max(i) > r1 .and. r1 < r2) then
              if (dfmax(i) == zero) then
                nindxd = nindxd + 1  
                indxd(nindxd) = i
#include "lockon.inc"
                WRITE(IOUT, 2002) NGL(I),EPS_MAX(I)
                WRITE(ISTDO,2002) NGL(I),EPS_MAX(I)
#include "lockoff.inc"
              endif
              damage(i) = min(one, (eps_max(i)-r1)/(r2-r1))
            endif
            IF (EPS_MAX(I) >= R2) THEN
                DAMAGE(I)= ONE
                OFF(I)=FOUR_OVER_5
                NINDXF=NINDXF+1
                INDXF(NINDXF)=I
                TDEL(I) = TIME               
#include "lockon.inc"
                WRITE(IOUT, 3000) NGL(I),EPS_MAX(I)
                WRITE(ISTDO,3100) NGL(I),EPS_MAX(I),TIME
#include "lockoff.inc"
            ENDIF  
!           
              IF (EPSP1 > ZERO .OR. EPSP2 > ZERO)THEN
c!              from: http://www.continuummechanics.org/principalstrain.html
                E1  = EPS11(I)
                E2  = EPS22(I)
                E3  = EPS33(I)
                E4  = HALF*EPS12(I)
                E5  = HALF*EPS23(I)
                E6  = HALF*EPS31(I)
                E42 = E4*E4
                E52 = E5*E5
                E62 = E6*E6

                I1 = E1 + E2 + E3
                I2 = E1*E2 + E2*E3 + E3*E1 - E4*E4 - E5*E5 - E6*E6
                I3 = E1*E2*E3 - E1*E52 - E2*E62 - E3*E42 + TWO*E4*E5*E6

                Q  = (THREE*I2 - I1*I1)/NINE
                R  = (TWO*I1*I1*I1-NINE*I1*I2+TWENTY7*I3)/FIFTY4     ! (2*I3^3-9*I1*I2+27*I3)/54
           
                R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
                PHI = ACOS(MAX(R_INTER,-ONE))
           
                E11 = TWO*SQRT(-Q)*COS(PHI/THREE)+THIRD*I1
                E22 = TWO*SQRT(-Q)*COS((PHI+TWO*PI)/THREE)+THIRD*I1
                E33 = TWO*SQRT(-Q)*COS((PHI+FOUR*PI)/THREE)+THIRD*I1
c
                IF (E11 < E22) THEN 
                   R_INTER = E11
                   E11     = E22
                   E22     = R_INTER
                ENDIF 
                IF (E22 < E33)THEN
                   R_INTER = E22
                   E22     = E33
                   E33     = R_INTER
                ENDIF
                IF (E11 < E22)THEN
                   R_INTER = E11
                   E11     = E22
                   E22     = R_INTER
                ENDIF
                DFMAX(I) = MIN(ONE,(E11 / UVAR(I,1)))    
c
                IF(E11 >= UVAR(I,1)) THEN
                  DAMAGE(I)= ONE
                  OFF(I)=FOUR_OVER_5
                  NINDXF=NINDXF+1
                  INDXF(NINDXF)=I
                  TDEL(I) = TIME
#include "lockon.inc"
                    WRITE(IOUT, 6000) NGL(I),E11
                    WRITE(ISTDO,6100) NGL(I),E11,TIME
#include "lockoff.inc"
                ENDIF                                                         
c
                IF (UVAR(I,1) < ZERO .AND. ABS(E11) >= ABS(UVAR(I,1))) THEN   
                  DAMAGE(I)= ONE                                               
                  OFF(I)=FOUR_OVER_5                                                
                  NINDXF=NINDXF+1                                             
                  INDXF(NINDXF)=I                                             
                  TDEL(I) = TIME                                             
#include "lockon.inc"
                    WRITE(IOUT, 6000) NGL(I),E11
                    WRITE(ISTDO,6100) NGL(I),E11,TIME
#include "lockoff.inc"
                ENDIF                         

                IF(EPSP2 > ZERO)THEN          
                  IF(ABS(E22) >= EPSP2) THEN  
                    DAMAGE(I)= ONE             
                    OFF(I)=FOUR_OVER_5              
                    NINDXF=NINDXF+1           
                    INDXF(NINDXF)=I           
                    TDEL(I) = TIME           
#include "lockon.inc"
                      WRITE(IOUT, 7000) NGL(I),E22
                      WRITE(ISTDO,7100) NGL(I),E22,TIME
#include "lockoff.inc"
                  ENDIF
                ENDIF
              ENDIF     ! EPSP1 > ZERO .OR. EPSP2 > ZERO                 
c
          ENDDO
c
c----------------------------------------------------
        CASE (2)  ! Equivalent strain criterion (new)
c----------------------------------------------------
          IF (STRFLAG == 1) THEN      ! Radioss strain is Cauchy-Green
c           transform total grad def strain to engineering
            DO I=1,NEL
              EPS11(I)  = MFXX(I)
              EPS22(I)  = MFYY(I)
              EPS33(I)  = MFZZ(I)
              EPS12(I)  = MFXY(I) + MFYX(I)
              EPS23(I)  = MFZY(I) + MFYZ(I)
              EPS31(I)  = MFXZ(I) + MFZX(I)
            END DO          
          ELSE IF (STRFLAG == 2) THEN     
c           transform engineering strain to true
            DO I=1,NEL
              EPS11(I)  = LOG(EPSXX(I) + ONE)
              EPS22(I)  = LOG(EPSYY(I) + ONE)
              EPS33(I)  = LOG(EPSZZ(I) + ONE)
              EPS12(I)  = LOG(EPSXY(I) + ONE)
              EPS23(I)  = LOG(EPSYZ(I) + ONE)
              EPS31(I)  = LOG(EPSZX(I) + ONE)
            END DO
          ELSE IF (STRFLAG == 3) THEN     
c           transform engineering strain to true
            DO I=1,NEL
              EPS11(I)  = LOG(EPSXX(I) + ONE)
              EPS22(I)  = LOG(EPSYY(I) + ONE)
              EPS33(I)  = LOG(EPSZZ(I) + ONE)
              EPS12(I)  = LOG(EPSXY(I) + ONE)
              EPS23(I)  = LOG(EPSYZ(I) + ONE)
              EPS31(I)  = LOG(EPSZX(I) + ONE)
            END DO
          ELSE IF (STRFLAG == 4) THEN      ! Radioss strain is Cauchy-Green
c           transform total grad def strain to true
            DO I=1,NEL
              EPS11(I)  = LOG(MFXX(I) + ONE)
              EPS22(I)  = LOG(MFYY(I) + ONE)
              EPS33(I)  = LOG(MFZZ(I) + ONE)
              EPS12(I)  = LOG(MFXY(I) + MFYX(I) + ONE)
              EPS23(I)  = LOG(MFXZ(I) + MFZX(I) + ONE)
              EPS31(I)  = LOG(MFZY(I) + MFYZ(I) + ONE)
            END DO
          ELSE
            EPS11(1:NEL)  = EPSXX(1:NEL)
            EPS22(1:NEL)  = EPSYY(1:NEL)
            EPS33(1:NEL)  = EPSZZ(1:NEL)
            EPS12(1:NEL)  = EPSXY(1:NEL)
            EPS23(1:NEL)  = EPSYZ(1:NEL)
            EPS31(1:NEL)  = EPSZX(1:NEL)
          END IF
c         calculate eps max      
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              DAV = (EPS11(I)+EPS22(I)+EPS33(I))*THIRD
              E1 = EPS11(I) - DAV
              E2 = EPS22(I) - DAV
              E3 = EPS33(I) - DAV
              E4 = HALF*EPS12(I)
              E5 = HALF*EPS23(I)
              E6 = HALF*EPS31(I)
              E42 = E4*E4
              E52 = E5*E5
              E62 = E6*E6
              C =  (E1*E2 + E1*E3 + E2*E3) - E42 - E52 - E62
              D = - E1*E2*E3 + E1*E52 + E2*E62 + E3*E42 - TWO*E4*E5*E6 
              CC =  C*THIRD
              EPST = SQRT(-CC)
              EPST2 = EPST * EPST
              Y = (EPST2 + C)* EPST + D
              Y1 = -(EPST2 + C)* EPST + D
              IF(ABS(Y) > EM8 .OR.(ABS(Y) < EM8 .AND. ABS(Y1) < ABS(Y)))THEN
                EPST = 1.75 * EPST
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
                
                EPST2 = EPST * EPST
                Y = (EPST2 + C)* EPST + D
                YP = THREE*EPST2 + C
                IF(YP /= ZERO)EPST = EPST - Y/YP
              ENDIF
              EPST       = EPST + DAV
              EPS_MAX(I) = EPST
            END IF
          ENDDO
c
c         test failure crit
c
        if (fail%table4d(1)%notable > 0) then   ! strain rate
          do j=1,nindx
            i = indx(j)
            xvec(j) = epsp(i)
          enddo
          call table_mat_vinterp(fail%table4d(1),nindx,nel,vartmp(1:nel,1:1),xvec,rfac,dydx)
          rfac(:) = max(rfac(:),em20)
        else
          rfac(:) = one
        endif
        if (fail%table4d(3)%notable > 0) then  ! temperature
          do j=1,nindx
            i = indx(j)
            xvec(j) = tstar(i)
          enddo
          call table_mat_vinterp(fail%table4d(3),nindx,nel,vartmp(1:nel,2:2),xvec,rfac2,dydx)
          rfac2(:) = max(rfac2(:),em20)
        else
          rfac2(:) = one
        endif
        !
        do j=1,nindx
          i = indx(j)
          r1 = epsf1*rfac(j)*rfac2(j)*uvar(i,1)
          r2 = epsf2*rfac(j)*rfac2(j)*uvar(i,1)  
          if (eps_max(i) > r1 .and. r1 < r2) then
            if (dfmax(i) == zero) then
              nindxd = nindxd + 1  
              indxd(nindxd) = i
            endif   
            damage(i) = min(one, (eps_max(i)-r1)/(r2-r1))
          endif
          if (eps_max(i) >= r2) then
            DAMAGE(I)= ONE
            OFF(I)=FOUR_OVER_5
            NINDXF=NINDXF+1
            INDXF(NINDXF)=I
            TDEL(I) = TIME               
#include "lockon.inc"
            WRITE(IOUT, 3000) NGL(I),EPS_MAX(I)
            WRITE(ISTDO,3100) NGL(I),EPS_MAX(I),TIME
#include "lockoff.inc"
          endif

        end do
c-------------------
        CASE (3)    !  Max principal tensile strain. No failure in compression
c-------------------
          DO J=1,NINDX
            I = INDX(J)
            E1  = EPSXX(I)
            E2  = EPSYY(I)
            E3  = EPSZZ(I)
            E4  = HALF*EPSXY(I)
            E5  = HALF*EPSYZ(I)
            E6  = HALF*EPSZX(I)
            E42 = E4*E4
            E52 = E5*E5
            E62 = E6*E6

            I1 = E1 + E2 + E3
            I2 = E1*E2 + E2*E3 + E3*E1 - E4*E4 - E5*E5 - E6*E6
            I3 = E1*E2*E3 - E1*E52 - E2*E62 - E3*E42 + TWO*E4*E5*E6
            Q  = (THREE*I2 - I1*I1)/NINE
            R  = (TWO*I1*I1*I1-NINE*I1*I2+TWENTY7*I3)/FIFTY4     ! (2*I3^3-9*I1*I2+27*I3)/54
            
            R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
            PHI = ACOS(MAX(R_INTER,-ONE))
              
            E11 = TWO*SQRT(-Q)*COS(PHI/THREE)+THIRD*I1
            E22 = TWO*SQRT(-Q)*COS((PHI+TWO*PI)/THREE)+THIRD*I1
            E33 = TWO*SQRT(-Q)*COS((PHI+FOUR*PI)/THREE)+THIRD*I1
c
            IF (STRFLAG == 1) THEN
              E11 = SQRT(E11 + ONE) - ONE
              E22 = SQRT(E22 + ONE) - ONE
              E33 = SQRT(E33 + ONE) - ONE
            ELSE IF (STRFLAG == 2) THEN
              E11 = EXP(E11) - ONE
              E22 = EXP(E22) - ONE
              E33 = EXP(E33) - ONE
            ELSE IF (STRFLAG == 3) THEN
              E11 = LOG(E11 + ONE)
              E22 = LOG(E22 + ONE)
              E33 = LOG(E33 + ONE)
            ELSE IF (STRFLAG == 4) THEN
              E11 = LOG(SQRT(E11+ONE))
              E22 = LOG(SQRT(E22+ONE))
              E33 = LOG(SQRT(E33+ONE))
            END IF
c
            IF (E11 < E22) THEN 
              R_INTER = E11
              E11     = E22
              E22     = R_INTER
            ENDIF 
            IF (E22 < E33)THEN
              R_INTER = E22
              E22     = E33
              E33     = R_INTER
            ENDIF
            IF (E11 < E22)THEN
              R_INTER = E11
              E11     = E22
              E22     = R_INTER
            ENDIF
            EPS_MAX(I) = E11
          ENDDO  ! j=1:nindx
!
!         test failure crit
!
          if (fail%table4d(1)%notable > 0) then   ! strain rate
            do j=1,nindx
              i = indx(j)
              xvec(j) = epsp(i)
            enddo
            call table_mat_vinterp(fail%table4d(1),nindx,nel,vartmp(1:nel,1:1),xvec,rfac,dydx)
            rfac(:) = max(rfac(:),em20)
          else
            rfac(:) = one
          endif
          if (fail%table4d(3)%notable > 0) then  ! temperature
            do j=1,nindx
              i = indx(j)
              xvec(j) = tstar(i)
            enddo
            call table_mat_vinterp(fail%table4d(3),nindx,nel,vartmp(1:nel,2:2),xvec,rfac2,dydx)
            rfac2(:) = max(rfac2(:),em20)
          else
            rfac2(:) = one
          endif
        !
          do j=1,nindx
            i = indx(j)
            r1 = epsf1*rfac(j)*rfac2(j)*uvar(i,1)
            r2 = epsf2*rfac(j)*rfac2(j)*uvar(i,1)  
            IF (EPS_MAX(I) > R1 .AND. R1 < R2 .AND. EPS_MAX(I) > ZERO) THEN
              IF (DFMAX(I) == ZERO) THEN
                NINDXD = NINDXD + 1  
                INDXD(NINDXD) = I
#include "lockon.inc"
                WRITE(IOUT, 2001) NGL(I),EPS_MAX(I)
                WRITE(ISTDO,2001) NGL(I),EPS_MAX(I)
#include "lockoff.inc"
              ENDIF   
              DAMAGE(I) = (EPS_MAX(I)-R1)/(R2-R1)
              DAMAGE(I) = MIN(ONE,DAMAGE(I))
            ENDIF
            IF (EPS_MAX(I) >= R2 .AND. EPS_MAX(I) > ZERO) THEN
             DAMAGE(I)= ONE
              OFF(I) = FOUR_OVER_5
              NINDXF=NINDXF+1
              INDXF(NINDXF)=I
              TDEL(I) = TIME                
#include "lockon.inc"
              WRITE(IOUT, 6000) NGL(I),EPS_MAX(I)
              WRITE(ISTDO,6100) NGL(I),EPS_MAX(I),TIME
#include "lockoff.inc"
            ENDIF
c
          ENDDO   ! do=1,nindx
c---------------
      END SELECT
c--------------------------
c
      IF (NINDXF > 0 .AND. IMCONV == 1) THEN
        DO J=1,NINDXF
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(INDXF(J))
          WRITE(ISTDO,1100) NGL(INDXF(J)),TIME
#include "lockoff.inc"
        END DO
      END IF         
C ---
      DO I=1,NEL
        R1 = EPSF1
        R2 = EPSF2
        IF (R1 < R2) THEN    
          DMG_SCALE(I) = ONE - DAMAGE(I)
        END IF
        DFMAX(I) = MAX(DFMAX(I),DAMAGE(I))  ! Maximum Damage storing for output : 0 < DFMAX < 1
      ENDDO            
c-----------------------------------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10,' AT TIME :',1PE20.13)
 2001 FORMAT(1X,'START DAMAGE (TENS) OF ELEMENT ',I10,', 1st PRINCIPAL STRAIN = ',G11.4)
 2002 FORMAT(1X,'START DAMAGE (TENS) OF ELEMENT ',I10,', EQUIVALENT STRAIN = ',G11.4)
 3000 FORMAT(1X,'FAILURE (TENS) OF ELEMENT ',I10,',   MAX EQUIVALENT STRAIN =',G11.4)
 3100 FORMAT(1X,'FAILURE (TENS) OF ELEMENT ',I10,',   MAX EQUIVALENT STRAIN =',G11.4,
     .          ' AT TIME :',1PE12.4)     
 6000 FORMAT(1X,'FAILURE (TENS) OF ELEMENT ',I10,',   1st PRINCIPAL STRAIN = ',G11.4)
 6100 FORMAT(1X,'FAILURE (TENS) OF ELEMENT ',I10,',   1st PRINCIPAL STRAIN = ',G11.4,
     .          ' AT TIME :',1PE12.4)
 7000 FORMAT(1X,'FAILURE (TENS) OF ELEMENT ',I10,',   2nd PRINCIPAL STRAIN = ',G11.4)
 7100 FORMAT(1X,'FAILURE (TENS) OF ELEMENT ',I10,',   2nd PRINCIPAL STRAIN = ',G11.4,
     .          ' AT TIME :',1PE12.4)
c-----------------------------------------------
      RETURN
      END
