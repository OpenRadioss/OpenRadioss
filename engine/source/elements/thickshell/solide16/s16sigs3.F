Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  S16SIGS3                      source/elements/thickshell/solide16/s16sigs3.F
Chd|-- called by -----------
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S16SIGS3(
     1    DXX     ,DYY       ,DZZ       ,D4       ,D5      ,D6      ,
     2    RX      ,RY        ,RZ        ,SX       ,SY      ,SZ      ,
     3    TX      ,TY        ,TZ        ,EINT     ,VOL0    ,SIGS    ,
     4    MXT     ,PM        ,DT1       ,VOL0DP  )
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
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MXT(*) 
      my_real
     1    DXX(*) ,DYY(*) ,DZZ(*) ,D4(*) ,D5(*) ,D6(*) ,
     2    RX(*) ,RY(*) ,RZ(*) ,SX(*) ,SY(*) ,SZ(*) ,
     3    TX(*) ,TY(*) ,TZ(*) ,EINT(*),VOL0(*),SIGS(*), 
     4    PM(NPROPM,*), DT1   
      DOUBLE PRECISION 
     .        VOL0DP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX
      my_real
     .   AA, DXY, DYZ, DZX, DRR, DSS, DTT, DRS, DST, DTR,
     .   SHF,A11,A12,A13,A21,A22,A23,A31,A32,A33,E,NU,D11
C-----------------------------------------------
      MX = MXT(LFT)
      E  = PM(20,MX)
      NU = PM(21,MX)
      DO I=LFT,LLT
        SHF = FIVE_OVER_6
        AA  = EM01 * SIGS(I)* ((ONE +NU) * (ONE -NU-NU)) / (E * (ONE -NU))

        DXY = HALF * D4(I)
        DYZ = HALF * D5(I)
        DZX = HALF * D6(I)

        A11 = DXX(I)*RX(I)+DXY*RY(I)+DZX*RZ(I)
        A12 = DXX(I)*SX(I)+DXY*SY(I)+DZX*SZ(I)
        A13 = DXX(I)*TX(I)+DXY*TY(I)+DZX*TZ(I)
        A21 = DXY*RX(I)+DYY(I)*RY(I)+DYZ*RZ(I)
        A22 = DXY*SX(I)+DYY(I)*SY(I)+DYZ*SZ(I)
        A23 = DXY*TX(I)+DYY(I)*TY(I)+DYZ*TZ(I)
        A31 = DZX*RX(I)+DYZ*RY(I)+DZZ(I)*RZ(I)
        A32 = DZX*SX(I)+DYZ*SY(I)+DZZ(I)*SZ(I)
        A33 = DZX*TX(I)+DYZ*TY(I)+DZZ(I)*TZ(I)

        DRR = RX(I)*A11+RY(I)*A21+RZ(I)*A31
        DSS = SX(I)*A12+SY(I)*A22+SZ(I)*A32
        DTT = TX(I)*A13+TY(I)*A23+TZ(I)*A33
        DTR = TX(I)*A11+TY(I)*A21+TZ(I)*A31
        DRS = RX(I)*A12+RY(I)*A22+RZ(I)*A32
        DST = SX(I)*A13+SY(I)*A23+SZ(I)*A33

        VOL0(I) = VOL0(I) * (ONE + AA)
        VOL0DP(I) = VOL0DP(I) * (ONE + AA)
        EINT(I) = EINT(I) / (1. + AA)
        DSS     = DSS - AA/DT1

        DRS     = DRS * SHF
        DST     = DST * SHF

        A11 = DRR*RX(I)+DRS*SX(I)+DTR*TX(I)
        A12 = DRR*RY(I)+DRS*SY(I)+DTR*TY(I)
        A13 = DRR*RZ(I)+DRS*SZ(I)+DTR*TZ(I)
        A21 = DRS*RX(I)+DSS*SX(I)+DST*TX(I)
        A22 = DRS*RY(I)+DSS*SY(I)+DST*TY(I)
        A23 = DRS*RZ(I)+DSS*SZ(I)+DST*TZ(I)
        A31 = DTR*RX(I)+DST*SX(I)+DTT*TX(I)
        A32 = DTR*RY(I)+DST*SY(I)+DTT*TY(I)
        A33 = DTR*RZ(I)+DST*SZ(I)+DTT*TZ(I)

        DXX(I)= RX(I)*A11+SX(I)*A21+TX(I)*A31
        DYY(I)= RY(I)*A12+SY(I)*A22+TY(I)*A32
        DZZ(I)= RZ(I)*A13+SZ(I)*A23+TZ(I)*A33
        DZX   = RZ(I)*A11+SZ(I)*A21+TZ(I)*A31
        DXY   = RX(I)*A12+SX(I)*A22+TX(I)*A32
        DYZ   = RY(I)*A13+SY(I)*A23+TY(I)*A33

        D4(I) = TWO * DXY 
        D5(I) = TWO * DYZ 
        D6(I) = TWO * DZX
      ENDDO
C-----------
      RETURN
      END
