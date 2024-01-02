Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  PORFOR4                       source/airbag/porfor4.F       
Chd|-- called by -----------
Chd|        AIRBAGB1                      source/airbag/airbagb1.F      
Chd|-- calls ---------------
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|====================================================================
      SUBROUTINE PORFOR4(SVTFAC,IM,IPM,PM,STRAIN,P,PEXT,IEL,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPM(NPROPMI,*),IM,IEL,NEL
      my_real
     .   SVTFAC,PM(NPROPM,*),STRAIN(*),P,PEXT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IPORA,IPORP,NFUNC
      my_real
     .   EPSXX,EPSYY,RS,RP,FPORA,FPORP,DERI,FLC,FAC
      my_real GET_U_FUNC
      EXTERNAL GET_U_FUNC
C=======================================================================
      EPSXX = STRAIN(IEL)
      EPSYY = STRAIN(IEL+NEL)
      RS=ONE+EPSXX+EPSYY+EPSXX*EPSYY
      RP=MIN(PEXT/P,ONE)
C
      FLC=ZERO
      NFUNC=IPM(10,IM)+IPM(6,IM)
      IPORA=IPM(10+NFUNC-1,IM)
      IF(IPORA > 0) THEN
         FPORA=PM(164,IM)
         FLC=FPORA*GET_U_FUNC(IPORA,RS,DERI)
      ENDIF
C
      FAC=ZERO
      IPORP=IPM(10+NFUNC-2,IM)
      IF(IPORP > 0) THEN
          FPORP=PM(165,IM)
          FAC=FPORP*GET_U_FUNC(IPORP,RP,DERI)
      ENDIF
C
      SVTFAC=FLC*FAC
C
      RETURN
      END
Chd|====================================================================
Chd|  PORFORM4                      source/airbag/porfor4.F       
Chd|-- called by -----------
Chd|        FVVENT0                       source/airbag/fvvent0.F       
Chd|-- calls ---------------
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|====================================================================
      SUBROUTINE PORFORM4(SVTFAC,IM,IPM,PM,AREA,AREA0,P,PEXT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPM(NPROPMI,*),IM
      my_real
     .   SVTFAC,PM(NPROPM,*),AREA,AREA0,P,PEXT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IPORA,IPORP,NFUNC
      my_real
     .   RS,RP,FPORA,FPORP,DERI,FLC,FAC
      my_real GET_U_FUNC
      EXTERNAL GET_U_FUNC
C=======================================================================
      RS=AREA/AREA0
      RP=MIN(PEXT/P,ONE)
C
      FLC=ZERO
      NFUNC=IPM(10,IM)+IPM(6,IM)
      IPORA=IPM(10+NFUNC-1,IM)
      IF(IPORA > 0) THEN
         FPORA=PM(164,IM)
         FLC=FPORA*GET_U_FUNC(IPORA,RS,DERI)
      ENDIF
C
      FAC=ZERO
      IPORP=IPM(10+NFUNC-2,IM)
      IF(IPORP > 0) THEN
          FPORP=PM(165,IM)
          FAC=FPORP*GET_U_FUNC(IPORP,RP,DERI)
      ENDIF
C
      SVTFAC=FLC*FAC
C
      RETURN
      END
