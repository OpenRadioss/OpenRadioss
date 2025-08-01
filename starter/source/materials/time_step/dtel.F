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
!||    dtel       ../starter/source/materials/time_step/dtel.F
!||--- called by ------------------------------------------------------
!||    dt105law   ../starter/source/materials/time_step/dt105law.F
!||    dt10law    ../starter/source/materials/time_step/dt10law.F
!||    dt14law    ../starter/source/materials/time_step/dt14law.F
!||    dt15law    ../starter/source/materials/time_step/dt15law.F
!||    dt16law    ../starter/source/materials/time_step/dt16law.F
!||    dt1law     ../starter/source/materials/time_step/dt1law.F
!||    dt21law    ../starter/source/materials/time_step/dt21law.F
!||    dt25law    ../starter/source/materials/time_step/dt25law.F
!||    dt34law    ../starter/source/materials/time_step/dt34law.F
!||    dt37law    ../starter/source/materials/time_step/dt37law.F
!||    dt38law    ../starter/source/materials/time_step/dt138aw.F
!||    dt3law     ../starter/source/materials/time_step/dt3law.F
!||    dt42law    ../starter/source/materials/time_step/dt42law.F
!||    dt51law    ../starter/source/materials/time_step/dt51law.F
!||    dt5law     ../starter/source/materials/time_step/dt5law.F
!||    dt6law     ../starter/source/materials/time_step/dt6law.F
!||====================================================================
      SUBROUTINE DTEL(SSP,PM,GEO,PID,MAT,
     .     RHO0, VIS,
     .     DELTAX, AIRE, VOL, DTX)
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
#include      "com01_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .     SSP(*), PM(NPROPM,*),GEO(NPROPG,*), 
     .     RHO0(*), VIS(*), DELTAX(*), AIRE(*), VOL(*), DTX(*)
      INTEGER PID(*),MAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   CX(MVSIZ), QX(MVSIZ), AL(MVSIZ) 
      my_real
     .   QA, QB, CNS1, CNS2
C-----------------------------------------------
C=======================================================================
      DO I=LFT,LLT
        CX(I)=SSP(I)
        AL(I)=VOL(I)** THIRD
        IF(N2D>0) AL(I)=SQRT(AIRE(I))
      ENDDO
C
      DO I=LFT,LLT                                               
        QA =GEO(14,PID(I))                                       
        QB =GEO(15,PID(I))                                       
        CNS1=GEO(16,PID(I))                                      
        CNS2=GEO(17,PID(I))*SSP(I)*AL(I)*RHO0(I)                 
        QX(I)= (QB+CNS1)*SSP(I)                                  
     .        +(TWO*VIS(I)+CNS2)/(RHO0(I)*MAX(EM20,DELTAX(I))) 
      ENDDO                                                      
C
      DO I=LFT,LLT
         DTX(I)=DELTAX(I)/
     .          MAX(EM20,QX(I)+SQRT(QX(I)*QX(I)+CX(I)*CX(I)))
      END DO
C-----------
      RETURN
      END
!||====================================================================
!||    dt70el    ../starter/source/materials/time_step/dtel.F
!||--- called by ------------------------------------------------------
!||    dt70law   ../starter/source/materials/time_step/dt70law.F
!||====================================================================
      SUBROUTINE DT70EL(SSP,PM,GEO,PID,MAT,
     .     RHO0, VIS,
     .     DELTAX, AIRE, VOL, DTX,IGEO)
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
#include      "com01_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .     SSP(*), PM(NPROPM,*),GEO(NPROPG,*), 
     .     RHO0(*), VIS(*), DELTAX(*), AIRE(*), VOL(*), DTX(*)
      INTEGER PID(*),MAT(*),IGEO(NPROPGI,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   CX(MVSIZ), QX(MVSIZ), AL(MVSIZ) 
      my_real
     .   QA, QB, CNS1, CNS2,FACQ
C-----------------------------------------------
C=======================================================================
       FACQ = ONE
!! qa=qb=zero by default for law70     
      IF(IGEO(31,PID(1)) == 1) FACQ = ZERO
      DO I=LFT,LLT
        CX(I)=SSP(I)
        AL(I)=VOL(I)** THIRD
        IF(N2D>0) AL(I)=SQRT(AIRE(I))
      ENDDO
C
      DO I=LFT,LLT                                               
        QA =FACQ*GEO(14,PID(I))                                       
        QB =FACQ*GEO(15,PID(I))                                       
        CNS1=GEO(16,PID(I))                                      
        CNS2=GEO(17,PID(I))*SSP(I)*AL(I)*RHO0(I)                 
        QX(I)= (QB+CNS1)*SSP(I)                                  
     .        +(TWO*VIS(I)+CNS2)/(RHO0(I)*MAX(EM20,DELTAX(I))) 
      ENDDO                                                      
C
      DO I=LFT,LLT
         DTX(I)=DELTAX(I)/
     .          MAX(EM20,QX(I)+SQRT(QX(I)*QX(I)+CX(I)*CX(I)))
      END DO
C-----------
      RETURN
      END
