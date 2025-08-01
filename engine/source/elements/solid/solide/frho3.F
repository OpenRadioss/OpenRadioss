Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    frho3     ../engine/source/elements/solid/solide/frho3.F
!||--- called by ------------------------------------------------------
!||    sforc3    ../engine/source/elements/solid/solide/sforc3.F
!||--- uses       -----------------------------------------------------
!||    ale_mod   ../common_source/modules/ale/ale_mod.F
!||====================================================================
      SUBROUTINE FRHO3(UVAR ,RHO0,DXX, DYY, DZZ,
     .                 FLUX,FLU1,VOLN,NGL,MAT,OFF,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr06_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(*),MAT(*),NEL
      my_real UVAR(NEL,*),FLUX(MVSIZ,6), FLU1(*), VOLN(*), DXX(*),DYY(*),DZZ(*),OFF(*),RHO0(*)
      TARGET :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real DVV, E0(NEL),VAVG,RV,RVP
      INTEGER I, J,COUNT,LIST(NEL),II
      my_real, DIMENSION(:), POINTER :: VOLO,RHON,EINT,DVOL
C-----------------------------------------------
      RHON => UVAR(1:NEL,1)   
      EINT => UVAR(1:NEL,2)   
      VOLO => UVAR(1:NEL,3)   
      DVOL => UVAR(1:NEL,4)   
C
C  incompressible flow avoir plus en cas de besoin
C      INCOMP = 1 not tested
      IF(ALE%GLOBAL%INCOMP == 1)THEN
         E0 = ZERO ! 
         DO I=1,NEL
          DVV=DT1*(DXX(I)+DYY(I)+DZZ(I))
          RHON(I)=RHON(I)-DVV*RHO0(I)
          VAVG=HALF*(VOLN(I)+VOLO(I))
          DVOL(I)=VAVG*DVV
          EINT(I)=EINT(I)*VOLO(I)-E0(I)*DVV*VAVG
         ENDDO
C compresssible flow
      ELSE
         DO I=1,NEL
           RHON(I) = RHON(I)/VOLN(I)
          
           DVOL(I) = VOLN(I) 
     .             - VOLO(I)+0.5*DT1*(FLU1(I)+FLUX(I,1)+FLUX(I,2)
     .             +  FLUX(I,3)+FLUX(I,4)+FLUX(I,5)+FLUX(I,6) )
            VOLO(I)=VOLN(I)
           RHON(I) = MAX(RHON(I) , EM30)
       
        ENDDO 
      ENDIF
C
        COUNT=0
        DO  I=1,NEL
          IF(RHON(I) <= ZERO .AND. OFF(I)/= ZERO)THEN
             COUNT = COUNT + 1
              LIST(COUNT)=I
           ENDIF  
        ENDDO
        DO II=1,COUNT
         I=LIST(II)
#include "lockon.inc"
        WRITE(IOUT,1000) NGL(I),RHON(I)
        WRITE(ISTDO,1000) NGL(I),RHON(I)
#include "lockoff.inc"
 1000   FORMAT('-STOP GAZ ',I8,' HAS NEGATIVE RHO',G12.5)
       ENDDO
C
      RETURN
      END SUBROUTINE FRHO3
      
