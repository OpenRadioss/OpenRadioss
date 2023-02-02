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
Chd|  PDAMP3                        source/elements/beam/pdamp3.F 
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PDAMP3(
     .    PM      ,GEO     ,OFF     ,MAT     ,PID,
     .    NEL     ,NGL     ,EXX     ,EXY     ,EXZ,
     .    KXX     ,KYY     ,KZZ     ,AL      ,FA1,
     .    FA2     ,FA3     ,MA1     ,MA2     ,MA3)
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
#include      "param_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      INTEGER ,DIMENSION(MVSIZ) ,INTENT(IN) :: MAT,PID,NGL
      my_real, DIMENSION(NPROPG,NUMGEO) ,INTENT(IN):: GEO
      my_real, DIMENSION(NPROPM,NUMMAT) ,INTENT(IN):: PM
      my_real, DIMENSION(NEL) , INTENT(IN):: OFF
      my_real, DIMENSION(MVSIZ) ,INTENT(IN):: AL  ,
     .                      EXX,EXY,EXZ,KXX,KYY,KZZ
      my_real, DIMENSION(MVSIZ) ,INTENT(INOUT):: 
     .                      FA1,FA2,FA3,
     .                      MA1,MA2,MA3
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
      my_real
     .   YM(MVSIZ),B1(MVSIZ), B2(MVSIZ), B3(MVSIZ), 
     .   RHO(MVSIZ),G(MVSIZ),DMPM(MVSIZ), DMPF(MVSIZ), 
     .   VOL(MVSIZ),RHOE(MVSIZ),RHOG(MVSIZ),DMM,A1(MVSIZ)
C-----------------------------------------------
C
      DO I=LFT,LLT
        RHO(I) =PM( 1,MAT(I))
        G(I)   =PM(22,MAT(I))
        YM(I)  =PM(20,MAT(I))
C-----------------------------
        A1(I)  =GEO(1,PID(I))
        B1(I)  =GEO(2,PID(I))
        B2(I)  =GEO(18,PID(I))
        B3(I)  =GEO(4,PID(I))
        VOL(I) = A1(I)*AL(I)
        RHOE(I)= SQRT(TWO*YM(I)*RHO(I))         
        RHOG(I)= SQRT(TWO*G(I)*RHO(I))         
      ENDDO
      IF (IMPL_S == 0 .OR. IDYNA > 0) THEN
        DO I=LFT,LLT                    
          DMPM(I)=GEO(16,PID(I))*OFF(I)  
          DMPF(I)=GEO(17,PID(I))*OFF(I)
        ENDDO
      ELSE
        DO I=LFT,LLT                    
          DMPM(I)=ZERO  
          DMPF(I)=ZERO  
        ENDDO 
      ENDIF                                 
C-----------------------------
C     AMORTISSEMENT
C-----------------------------
      DO I=LFT,LLT
        DMM = VOL(I)*RHOE(I)
        FA1(I) = FA1(I) + DMPM(I)*EXX(I)*DMM
        FA2(I) = FA2(I) + DMPF(I)*EXY(I)*DMM
        FA3(I) = FA3(I) + DMPF(I)*EXZ(I)*DMM
        MA1(I) = MA1(I) + DMPF(I)*KXX(I)*B3(I)*AL(I)*RHOG(I)
        MA2(I) = MA2(I) + DMPF(I)*KYY(I)*B1(I)*AL(I)*RHOE(I)
        MA3(I) = MA3(I) + DMPF(I)*KZZ(I)*B2(I)*AL(I)*RHOE(I)
      ENDDO
C-----------------------------------------------
      RETURN
      END SUBROUTINE PDAMP3
