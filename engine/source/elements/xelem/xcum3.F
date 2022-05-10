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
Chd|  XCUM3                         source/elements/xelem/xcum3.F 
Chd|-- called by -----------
Chd|        XFORC3                        source/elements/xelem/xforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE XCUM3(NX,KXX,IXX,UFORC,USTIFM,
     2                 UVISCM,MS,F,STIFN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NX,KXX(NIXX),IXX(*)
C     REAL ou REAL*8
      my_real
     .   UFORC(3,*) ,USTIFM(*) , UVISCM(*),
     .   F(3,*) ,STIFN(*),MS(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, I1, IADNOD
C     REAL ou REAL*8
      my_real
     .       XM, XK, XC, A     
C-----------------------------------------------
      IADNOD=KXX(4)
C-----
C     stiffnesses and viscosities applied to each node
C          were returned by user routine.
      DO K=1,NX
       I1=IXX(IADNOD+K-1)
       XK=USTIFM(K)
       IF (MS(I1).NE.ZERO.AND.XK.NE.ZERO) THEN
        XM= MS(I1)
        XC= UVISCM(K)
        A = XC/SQRT(QUATRE*XK*XM)
        XK= XK/(SQRT(A**2+ UN)-A)**2  
       ELSEIF (XK.NE.ZERO) THEN
       ELSE
        XC= UVISCM(K)
C       XK= 0.5 *XC**2/(2.*MS(I1))
        XK= QUART*XC**2/MAX(EM15,MS(I1))
       ENDIF
       USTIFM(K)=XK
      ENDDO
C-----
      DO K=1,NX
       I1=IXX(IADNOD+K-1)
       F(1,I1)=F(1,I1)+UFORC(1,K)
       F(2,I1)=F(2,I1)+UFORC(2,K)
       F(3,I1)=F(3,I1)+UFORC(3,K)
       STIFN(I1)=STIFN(I1)+USTIFM(K)
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  XCUM3P                        source/elements/xelem/xcum3.F 
Chd|-- called by -----------
Chd|        XFORC3                        source/elements/xelem/xforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE XCUM3P(NX,KXX,IXX,UFORC,USTIFM,
     2                  UVISCM,MS,NISKYL,FSKYI,ISKY)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr23_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NX,KXX(NIXX),IXX(*),NISKYL,ISKY(*)
C     REAL ou REAL*8
      my_real
     .   UFORC(3,*), USTIFM(*), UVISCM(*),
     .   MS(*),FSKYI(LSKYI,NFSKYI)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, I1, IADNOD
C     REAL ou REAL*8
      my_real
     .       XM, XK, XC, A      
C-----------------------------------------------
C     ASSEMBLE
C-------------------------
      IADNOD=KXX(4)
      IF(KDTINT.EQ.0)THEN
C-----
C     total of stiffnesses and viscosities applied to each node
C          were returned by user routine.
      DO K=1,NX
       I1=IXX(IADNOD+K-1)
       XK=USTIFM(K)
       IF (MS(I1).NE.ZERO.AND.XK.NE.ZERO) THEN
        XM= MS(I1)
        XC= UVISCM(K)
        A = XC/SQRT(QUATRE*XK*XM)
        XK= XK/(SQRT(A**2+ UN)-A)**2  
       ELSEIF (XK.NE.ZERO) THEN
       ELSE
        XC= UVISCM(K)
C       XK= 0.5 *XC**2/(2.*MS(I1))
        XK= QUART*XC**2/MAX(EM15,MS(I1))
       ENDIF
       USTIFM(K)=XK
      ENDDO
C-----
Clm   vecteur skyline necessaire pour rotations.
      DO K=1,NX
       I1=IXX(IADNOD+K-1)
       NISKYL=NISKYL+1
       FSKYI(NISKYL,1)=UFORC(1,K)
       FSKYI(NISKYL,2)=UFORC(2,K)
       FSKYI(NISKYL,3)=UFORC(3,K)
       FSKYI(NISKYL,4)=USTIFM(K)
       ISKY(NISKYL)   =I1
      ENDDO
      ELSE
C-----
C     total of stiffnesses and viscosities applied to each node
C          were returned by user routine.
Clm   vecteur skyline necessaire pour rotations.
      DO K=1,NX
       I1=IXX(IADNOD+K-1)
       NISKYL=NISKYL+1
       FSKYI(NISKYL,1)=UFORC(1,K)
       FSKYI(NISKYL,2)=UFORC(2,K)
       FSKYI(NISKYL,3)=UFORC(3,K)
Clm    A = XC/SQRT(2.*XK*XM), XK= XK/(SQRT(A**2+1)-A)**2  
C                          ou XK= 0.5*XC**2/MS(I1)
       FSKYI(NISKYL,4)=USTIFM(K)
       FSKYI(NISKYL,5)=DEUX*UVISCM(K)
       ISKY(NISKYL)   =I1
      ENDDO
      ENDIF
C
      RETURN
      END
