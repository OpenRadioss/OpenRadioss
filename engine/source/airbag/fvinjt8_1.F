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
Chd|  FVINJT8_1                     source/airbag/fvinjt8_1.F     
Chd|-- called by -----------
Chd|        FVBAG1                        source/airbag/fvbag.F         
Chd|        FV_UP_SWITCH                  source/airbag/fv_up_switch.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FVINJT8_1(NJET   , IBAGJET , RBAGJET ,
     2                    IGEO,     GEO,     PM,
     3                    IVOLU,   RVOLU, DMOUT, DHOUT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NJET, IBAGJET(NIBJET,*), IGEO(NPROPGI,*), IVOLU(*)
C     REAL
      my_real
     .   DMOUT, DHOUT, RBAGJET(NRBJET,*),
     .   GEO(NPROPG,*), PM(NPROPM,*), RVOLU(*)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IJ, IK, KK, I_INJ, I_TYPINJ, NGASES, I_GAS
      my_real
     .   R_IGC1, RMWI, RMWG, RNM, RNMI, RNMG, MW, GMTOT, GMI, DGMOUT, DGEOUT
C----------------------------------------------------------------------------
      R_IGC1=PM(27,IVOLU(66))
      RMWI  = RVOLU(10)
C--------------------------
C     NOMBRE TOTAL DE MOLES
C--------------------------
      GMI = RVOLU(11)
      RNM = GMI*RMWI
      DO IJ=1,NJET
        I_INJ = IABS(IBAGJET(13,IJ))
        I_TYPINJ = IGEO(22,I_INJ)
        NGASES   = IGEO(23,I_INJ)
        DO IK=1,NGASES
          IF (I_TYPINJ==1) THEN
            I_GAS = IGEO(100+(IK-1)*3+1,I_INJ)
          ELSE IF (I_TYPINJ==2) THEN
            I_GAS = IGEO(100+(IK-1)*2+1,I_INJ)
          END IF
          MW   = PM(20,I_GAS)
          RMWG = R_IGC1/MW
          KK=20+(IK-1)*4
          GMTOT= RBAGJET(KK+1,IJ)
          RNMG = GMTOT*RMWG
          RNM  = RNM+RNMG
        ENDDO
      ENDDO
      RVOLU(14)=RNM
C-------------------------------
C     MASSE et ENTHALPY par GAZ
C-------------------------------
      RNMI=GMI*RMWI
      DGMOUT=DMOUT*RNMI/RNM
      DGEOUT=DHOUT*RNMI/RNM
C      DGEIN =DGMIN*RVOLU(63)
      RVOLU(22)=DGEOUT
      RVOLU(24)=DGMOUT
C      RVOLU(64)=DGMIN
C      RVOLU(65)=DGEIN
      RVOLU(11)=RVOLU(11)+DGMOUT*DT1
C
      DO IJ=1,NJET
         I_INJ = IABS(IBAGJET(13,IJ))
         I_TYPINJ = IGEO(22,I_INJ)
         NGASES   = IGEO(23,I_INJ)
         RBAGJET( 9,IJ)=ZERO
         RBAGJET(10,IJ)=ZERO
         DO IK=1,NGASES
          IF (I_TYPINJ==1) THEN
            I_GAS = IGEO(100+(IK-1)*3+1,I_INJ)
          ELSE IF (I_TYPINJ==2) THEN
            I_GAS = IGEO(100+(IK-1)*2+1,I_INJ)
          END IF
          MW   = PM(20,I_GAS)
          RMWG = R_IGC1/MW
          KK=20+(IK-1)*4
          GMTOT= RBAGJET(KK+1,IJ)
          RNMG  =GMTOT*RMWG
          DGMOUT=DMOUT*RNMG/RNM
          DGEOUT=DHOUT*RNMG/RNM
          RBAGJET(KK+1,IJ)=GMTOT+DGMOUT*DT1
          RBAGJET(KK+3,IJ)=DGMOUT
          RBAGJET(KK+4,IJ)=DGEOUT
          RBAGJET( 8,IJ)=RBAGJET( 8,IJ)+DGMOUT*DT1
          RBAGJET( 9,IJ)=RBAGJET( 9,IJ)+DGMOUT
          RBAGJET(10,IJ)=RBAGJET(10,IJ)+DGEOUT
         ENDDO
      ENDDO
      RETURN
      END
