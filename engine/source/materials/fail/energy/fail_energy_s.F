Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
C new (energy  failure model)
!||====================================================================
!||    fail_energy_s           ../engine/source/materials/fail/energy/fail_energy_s.F
!||--- called by ------------------------------------------------------
!||    mmain                   ../engine/source/materials/mat_share/mmain.F90
!||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
!||    mulaw8                  ../engine/source/materials/mat_share/mulaw8.F90
!||    usermat_solid           ../engine/source/materials/mat_share/usermat_solid.F
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    fail_param_mod          ../common_source/modules/mat_elem/fail_param_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      SUBROUTINE FAIL_ENERGY_S(FAIL     ,
     1     NEL      ,NUVAR    ,NVARTMP  ,UVAR     ,VARTMP  ,
     2     TIME     ,TIMESTEP ,NGL      ,EPSP     ,
     3     OFF      ,DFMAX    ,TDELE    ,DMGSCL   ,
     4     SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     5     DEPSXX   ,DEPSYY   ,DEPSZZ   ,DEPSXY   ,DEPSYZ   ,DEPSZX   )    
! ---------------------------- ----------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use fail_param_mod
          use table_mat_vinterp_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER FAILURE PARAMETER ARRAY
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include    "mvsiz_p.inc"
#include    "scr17_c.inc"
#include    "units_c.inc"
#include    "comlock.inc"
#include    "param_c.inc"
#include    "impl1_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUVAR,NVARTMP
      INTEGER, DIMENSION(NEL), INTENT(IN) :: NGL
      my_real, INTENT(IN) :: TIME,TIMESTEP
      my_real, DIMENSION(NEL), INTENT(IN) :: SIGNXX,SIGNYY,SIGNZZ,
     .   SIGNXY,SIGNYZ,SIGNZX,DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,EPSP
      type (fail_param_) ,intent(inout) :: fail    !< failure model data structure
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
      my_real, DIMENSION(NEL), INTENT(INOUT) :: OFF,DFMAX,DMGSCL,TDELE
      my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IDEL,IDEV,IFLAG(MVSIZ),INDX(MVSIZ),IADBUF,NINDX,
     .        NINDEX,INDEX(MVSIZ),IR,IFAIL,JJ,
     .        ISOLID
      my_real :: E1,E2,E3,E4,E5,E6,E42,E52,E62,C,D, EPST,EPST2,
     .       R1,R2,Y,YP,DAV,IE_SP,P,SCALE,CC,RIEF1,RIEF2
      my_real :: EPS_MAX(MVSIZ),DAMAGE(MVSIZ),EPSF1(MVSIZ),EPSF2(MVSIZ)
      my_real :: RFAC(NEL),DYDX(NEL)
C--------------------------------------------------------------
      RIEF1 =  FAIL%UPARAM(1)
      RIEF2 =  FAIL%UPARAM(2)

      DO I=1,NEL
        DAMAGE(I) = ZERO
        UVAR(I,1) = UVAR(I,1) + SIGNXX(I)*DEPSXX(I) + 
     .                          SIGNYY(I)*DEPSYY(I) + 
     .                          SIGNZZ(I)*DEPSZZ(I) + 
     .                          SIGNXY(I)*DEPSXY(I) + 
     .                          SIGNYZ(I)*DEPSYZ(I) + 
     .                          SIGNZX(I)*DEPSZX(I)
      ENDDO
C-----------------------------------------------
      DO I=1,NEL
        IF(OFF(I)<EM01) OFF(I)=ZERO
        IF(OFF(I)<ONE)  OFF(I)=OFF(I)*FOUR_OVER_5
      END DO
CC           
      if (fail%table4d(1)%notable > 0) then
        call table_mat_vinterp(fail%table4d(1),nel,nel,vartmp,epsp,rfac,dydx)
        rfac(:) = max(rfac(:),em20)
      else
        rfac(:) = one
      end if
!
       NINDX=0  
       DO I=1,NEL
        IF(OFF(I)==ONE)THEN
          IE_SP = UVAR(I,1)
          R1 = RIEF1*RFAC(I)
          R2 = RIEF2*RFAC(I)
         IF(IE_SP>R1.AND.R1<R2) THEN
          DAMAGE(I)= (IE_SP-R1)/(R2-R1)
          DAMAGE(I)= MIN(ONE,DAMAGE(I))
         ENDIF
         DFMAX(I) = MAX(DFMAX(I),DAMAGE(I))
         IF(IE_SP>R2) THEN
          DAMAGE(I)= ONE
          OFF(I)=FOUR_OVER_5
          NINDX=NINDX+1
          INDX(NINDX)=I
          TDELE(I) = TIME  
         ENDIF                  
       ENDIF 
      ENDDO
c
      IF(NINDX>0.AND.IMCONV==1)THEN
       DO J=1,NINDX
#include "lockon.inc"
        WRITE(IOUT, 1000) NGL(INDX(J))
        WRITE(ISTDO,1100) NGL(INDX(J)),TIME
#include "lockoff.inc"
        END DO
      END IF         
      DO I=1,NEL
        DMGSCL(I) = ONE - DFMAX(I)
      ENDDO            
C-----------------------------------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',1PE20.13)
CC     
      RETURN
      END
