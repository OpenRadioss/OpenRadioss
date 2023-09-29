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
     .    IMPL_S  ,IDYNA   ,DT1     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,IMAT,IPID,IMPL_S,IDYNA
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,DIMENSION(NPROPG,NUMGEO) ,INTENT(IN):: GEO
      my_real ,DIMENSION(NPROPM,NUMMAT) ,INTENT(IN):: PM
      my_real ,DIMENSION(NEL) ,INTENT(IN)   :: OFF
      my_real ,DIMENSION(NEL) ,INTENT(IN)   :: AL
      my_real ,DIMENSION(NEL) ,INTENT(IN)   :: EXX,EXY,EXZ,KXX,KYY,KZZ
      my_real ,DIMENSION(NEL) ,INTENT(INOUT):: FA1,FA2,FA3,MA1,MA2,MA3
      my_real ,INTENT(IN)                   :: DT1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
      my_real :: RHO,RHOE,RHOG,YOUNG,G,DMM,IYY,IZZ,IXX,AREA,DTINV
      my_real ,DIMENSION(NEL) :: VOL,DMPM,DMPF
c=======================================================================
      RHO  = PM( 1,IMAT)
      YOUNG= PM(20,IMAT)
      G    = PM(22,IMAT)
      AREA = GEO(1,IPID)
      IXX  = GEO(4,IPID)
      IYY  = GEO(2,IPID)
      IZZ  = GEO(18,IPID)
      RHOE = SQRT(TWO*YOUNG*RHO)  
      RHOG = SQRT(TWO*G *RHO)   
c
      VOL(1:NEL) = AREA*AL(1:NEL)
      DTINV = DT1/MAX(DT1**2,EM20)  
c
      IF (IMPL_S == 0 .OR. IDYNA > 0) THEN
        DO I=1,NEL                    
          DMPM(I)=DTINV*GEO(16,IPID)*OFF(I)  
          DMPF(I)=DTINV*GEO(17,IPID)*OFF(I)
        ENDDO
      ELSE
        DO I=1,NEL                    
          DMPM(I)=ZERO  
          DMPF(I)=ZERO  
        ENDDO 
      ENDIF                                 
C-----------------------------
C     AMORTISSEMENT
C-----------------------------
      DO I=1,NEL
        DMM = VOL(I)*RHOE
        FA1(I) = FA1(I) + DMPM(I)*EXX(I)*DMM
        FA2(I) = FA2(I) + DMPF(I)*EXY(I)*DMM
        FA3(I) = FA3(I) + DMPF(I)*EXZ(I)*DMM
        MA1(I) = MA1(I) + DMPF(I)*KXX(I)*AL(I)*IXX*RHOG
        MA2(I) = MA2(I) + DMPF(I)*KYY(I)*AL(I)*IYY*RHOE
        MA3(I) = MA3(I) + DMPF(I)*KZZ(I)*AL(I)*IZZ*RHOE
      ENDDO
C-----------
      RETURN
      END SUBROUTINE PDAMP3
