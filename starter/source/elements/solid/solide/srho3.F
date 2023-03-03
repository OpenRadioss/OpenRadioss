Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|====================================================================
Chd|  SRHO3                         source/elements/solid/solide/srho3.F
Chd|-- called by -----------
Chd|        S4REFSTA3                     source/elements/solid/solide4/s4refsta3.F
Chd|        SREFSTA3                      source/elements/solid/solide/srefsta3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SRHO3(PM ,VOLO    ,RHON    ,EINT    ,DXX    , 
     .            DYY     ,DZZ     ,VOLN    ,DVOL    ,MAT    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*)
C     REAL
      my_real
     .  PM(NPROPM,*), VOLO(*), RHON(*), EINT(*),
     .  VOLN(*), DVOL(*), DXX(*), DYY(*), DZZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .      RHO0(MVSIZ),DVV
C-----------------------------------------------
       DO I=LFT,LLT
        RHO0(I)=PM(1,MAT(I))
       ENDDO
C      
       IF(ISMSTR==1 .OR. ISMSTR==11)THEN
        DO I=LFT,LLT
          DVV  =  DXX(I)+DYY(I)+DZZ(I)
          DVOL(I) = VOLO(I)*DVV
          RHON(I) = RHON(I)-DVV*RHO0(I)
        ENDDO
       ELSE
        IF (MTN /= 115) THEN
          DO I=LFT,LLT
            DVOL(I)=VOLN(I)-(RHO0(I)/RHON(I))*VOLO(I)
            RHON(I)=RHO0(I)*(VOLO(I)/VOLN(I))
          ENDDO
        ELSE
          DO I=LFT,LLT
            DVOL(I)=VOLN(I)-(RHON(LLT+I)/RHON(I))*VOLO(I)
            RHON(I)=RHON(LLT+I)*(VOLO(I)/VOLN(I))
          ENDDO        
        ENDIF
       END IF

       DO I=LFT,LLT
         EINT(I)=EINT(I)*VOLO(I)
       ENDDO

      RETURN
      END
