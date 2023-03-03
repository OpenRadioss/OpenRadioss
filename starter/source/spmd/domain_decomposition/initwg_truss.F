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
Chd|  INITWG_TRUSS                  source/spmd/domain_decomposition/initwg_truss.F
Chd|-- called by -----------
Chd|        INITWG                        source/spmd/domain_decomposition/initwg.F
Chd|-- calls ---------------
Chd|        DDWEIGHTS_MOD                 share/modules1/ddweights_mod.F
Chd|====================================================================
      SUBROUTINE INITWG_TRUSS(WD,PM,GEO,IXT,IGEO,
     .            NUMELT,IPM,NUMMAT,NUMGEO,OFF)
C-----------------------------------------------
C            M o d u l e s
C-----------------------------------------------
      USE DDWEIGHTS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  OFF,NUMELT,
     .  NUMMAT,NUMGEO,
     .  IXT(NIXT,*),IGEO(NPROPGI,*),IPM(NPROPMI,*)

C     REAL OU REAL*8
      my_real
     .    PM(NPROPM,*), GEO(NPROPG,*)
      REAL WD(*)
C-----------------------------------------------
      INTEGER  NPN, MID, PID, JHBE, IGT, MLN,
     .    ISTRAIN, ITHK, IHBE, IPLA, ISSN, MTN, I, J, K,L,
     .    NFUNC,MPT,NPTS,NPTT,NPTR,NPTOT,IFLAG,JSROT,
     .    I_MID,I_PID,I_MID_OLD,I_PID_OLD,PUID,MUID,
     .    ELM_TYP,ELM_TYP_OLD,ILAW,ILAW_OLD,TEST_MAT,
     .    I_PRO,ISOL2,MID_OLD,PID_OLD,MUID_OLD,PUID_OLD,
     .    TEST,NFUNC1,NFUNC2

      REAL
     .   WTYPE(9),FWIHBE,FAC8,
     .   TABMAT(3),TABX(3),TIMMAT,NPT,TELT,POIDS,W,
     .   BATOZMULT,TMAT
      
      my_real
     .        CC, INVTREF,A,B,A1,A2
       DATA WTYPE /1.6 ,1. ,1. ,.9 ,1.1 ,1.4 ,0.65 ,.9 ,2.0/     
C-----------------------------------------------  
      DO I = 1, NUMELT
        MID= IXT(1,I)
        MLN= PM(19,MID)
        PID= IXT(4,I)
        ISSN = 0
        IF(GEO(5,PID)/=ZERO) ISSN=1
C   test elem delete
        IF (WD(I+OFF)==0.)
     .    WD(I+OFF) = WTYPE(4)
      ENDDO
      RETURN
      END
