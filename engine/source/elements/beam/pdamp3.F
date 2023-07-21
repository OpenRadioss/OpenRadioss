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
     .    PM      ,GEO     ,OFF     ,IMAT    ,IPID  ,
     .    NEL     ,NGL     ,EXX     ,EXY     ,EXZ   ,
     .    KXX     ,KYY     ,KZZ     ,AL      ,FA1   ,
     .    FA2     ,FA3     ,MA1     ,MA2     ,MA3   ,
     .    DTIME   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,IMAT,IPID
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: DTIME
      my_real ,DIMENSION(NPROPG,NUMGEO) ,INTENT(IN):: GEO
      my_real ,DIMENSION(NPROPM,NUMMAT) ,INTENT(IN):: PM
      my_real ,DIMENSION(NEL) ,INTENT(IN)   :: OFF
      my_real ,DIMENSION(NEL) ,INTENT(IN)   :: AL
      my_real ,DIMENSION(NEL) ,INTENT(IN)   :: EXX,EXY,EXZ,KXX,KYY,KZZ
      my_real ,DIMENSION(NEL) ,INTENT(INOUT):: FA1,FA2,FA3,MA1,MA2,MA3
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
      my_real :: RHO,RHOE,RHOG,YOUNG,G,DMM,IYY,IZZ,IXX,AREA,DMPM,DMPF,DTINV
      my_real :: VOL(NEL)
c=======================================================================
      DTINV = DTIME / MAX(DTIME**2,EM20)  
      IF (IMPL_S > 0 .AND. IDYNA == 0) DTINV = ZERO
c
      RHO  = PM( 1,IMAT)
      YOUNG= PM(20,IMAT)
      G    = PM(22,IMAT)
      AREA = GEO(1,IPID)
      IXX  = GEO(4,IPID)
      IYY  = GEO(2,IPID)
      IZZ  = GEO(18,IPID)
      DMPM = GEO(16,IPID) 
      DMPF = GEO(17,IPID)
      RHOE = SQRT(TWO*YOUNG*RHO) * DTINV       
      RHOG = SQRT(TWO*G *RHO)    * DTINV  
c
      VOL(1:NEL) = AREA*AL(1:NEL)
C-----------------------------
C     AMORTISSEMENT
C-----------------------------
      DO I=1,NEL
        DMM = VOL(I)*RHOE
        FA1(I) = FA1(I) + DMPM*EXX(I)*DMM * OFF(I) 
        FA2(I) = FA2(I) + DMPF*EXY(I)*DMM * OFF(I)
        FA3(I) = FA3(I) + DMPF*EXZ(I)*DMM * OFF(I)
        MA1(I) = MA1(I) + DMPF*KXX(I)*AL(I)*IXX * RHOG * OFF(I)
        MA2(I) = MA2(I) + DMPF*KYY(I)*AL(I)*IYY * RHOE * OFF(I)
        MA3(I) = MA3(I) + DMPF*KZZ(I)*AL(I)*IZZ * RHOE * OFF(I)
      ENDDO
C-----------------------------------------------
      RETURN
      END SUBROUTINE PDAMP3
