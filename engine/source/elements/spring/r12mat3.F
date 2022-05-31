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
Chd|  R12MAT3                       source/elements/spring/r12mat3.F
Chd|-- called by -----------
Chd|        R12KE3                        source/elements/spring/r12ke3.F
Chd|-- calls ---------------
Chd|        RKENONL                       source/elements/spring/r4mat3.F
Chd|====================================================================
      SUBROUTINE R12MAT3(JFT    ,JLT    ,GEO    ,KX     ,MGN    ,
     .                   DKX    ,BETA   ,AL0    ,FX     ,DX     ,
     .                   TF     ,NPF    ,POS    ,IGEO   )
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
#include      "impl2_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT    ,JLT, MGN(*),IGEO(NPROPGI,*),NPF(*)
C     REAL ou REAL*8
      my_real
     .   GEO(NPROPG,*), KX(*),BETA(*),DKX(*),AL0(*),
     .   FX(*),DX(*),TF(*),POS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ILEN,ILENG
      INTEGER IECROU(MVSIZ), IFUNC(MVSIZ),IFUNC2(MVSIZ)
C     REAL ou REAL*8
      my_real
     .   A(MVSIZ),FRIC,FF
C-----------------------------------------------
C
      DO I=JFT,JLT
       KX(I)=GEO(2,MGN(I))
      ENDDO
      ILEN = 0
      DO I=JFT,JLT
       A(I)   =GEO(10,MGN(I))
C       KX(I)=A(I)*KX(I)
       ILENG=NINT(GEO(93,MGN(I)))
       IF(ILENG.NE.0) ILEN = 1
      ENDDO
      IF(ILEN.NE.0) THEN
       DO I=JFT,JLT
        ILENG=NINT(GEO(93,MGN(I)))
        IF(ILENG.NE.0)THEN
          KX(I)=KX(I)/AL0(I)
        ENDIF
       ENDDO
      ENDIF
C
      DO I=JFT,JLT
       FRIC= GEO(17,MGN(I))
       IF(FRIC.NE.ZERO)THEN
        DKX(I) = KX(I)*TANH(HALF*FRIC*BETA(I))
       ELSE
        DKX(I)=ZERO
       ENDIF
      ENDDO
      IF (ISMDISP.GT.0.OR.ISPRN.EQ.1) THEN
       DO I=JFT,JLT
        IECROU(I)=NINT(GEO(7,MGN(I)))
        IFUNC(I) =IGEO(101,MGN(I))
        IFUNC2(I)=IGEO(103,MGN(I))
       ENDDO
       CALL RKENONL(JFT    ,JLT    ,KX     ,FX     ,DX     ,
     .              IECROU ,IFUNC  ,IFUNC2 ,A      ,TF     ,
     .              NPF    ,POS    )
      ENDIF
C
      RETURN
      END
