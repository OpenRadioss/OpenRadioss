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
Chd|  DTSPH                         source/materials/time_step/dtsph.F
Chd|-- called by -----------
Chd|        DT105LAW                      source/materials/time_step/dt105law.F
Chd|        DT10LAW                       source/materials/time_step/dt10law.F
Chd|        DT14LAW                       source/materials/time_step/dt14law.F
Chd|        DT1LAW                        source/materials/time_step/dt1law.F
Chd|        DT21LAW                       source/materials/time_step/dt21law.F
Chd|        DT34LAW                       source/materials/time_step/dt34law.F
Chd|        DT37LAW                       source/materials/time_step/dt37law.F
Chd|        DT38LAW                       source/materials/time_step/dt138aw.F
Chd|        DT3LAW                        source/materials/time_step/dt3law.F
Chd|        DT42LAW                       source/materials/time_step/dt42law.F
Chd|        DT51LAW                       source/materials/time_step/dt51law.F
Chd|        DT5LAW                        source/materials/time_step/dt5law.F
Chd|        DT6LAW                        source/materials/time_step/dt6law.F
Chd|        DT70LAW                       source/materials/time_step/dt70law.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DTSPH(SSP,PM,GEO,PID,MAT, 
     .     RHO0, VIS, 
     .     DELTAX, VOL, DTX)
C------------------------
C     CALCUL DT ELEMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "com01_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .     SSP(*), PM(NPROPM,*),GEO(NPROPG,*), RHO0(*), VIS(*), 
     .     DELTAX(*), VOL(*), DTX(*)
      INTEGER PID(*),MAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   CX(MVSIZ), QX(MVSIZ), AL(MVSIZ), 
     .    QA, QB, CNS1, CNS2
C=======================================================================
      DO I=LFT,LLT
        CX(I)=SSP(I)
      END DO

      DO I=LFT,LLT
      AL(I)=VOL(I)**THIRD
      END DO

      DO I=LFT,LLT
        QA  =GEO(14,PID(I))
        QB  =GEO(15,PID(I))
        CNS1=GEO(16,PID(I))
        CNS2=GEO(17,PID(I))*SSP(I)*AL(I)*RHO0(I)
        QX(I)=(QB+CNS1)*SSP(I)+(TWO*VIS(I)+CNS2)/(RHO0(I)*DELTAX(I))
      ENDDO

      DO I=LFT,LLT
         DTX(I)=DELTAX(I)/
     .          MAX(EM20,QX(I)+SQRT(QX(I)*QX(I)+CX(I)*CX(I)))
      END DO
c-----------
      RETURN
      END
