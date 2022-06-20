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
Chd|  RMASS                         source/elements/spring/rmass.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RMASS(IXR  ,GEO  ,MS   ,XIN ,PARTSAV,
     2                 X    ,V    ,IPART,XL  ,MSR    ,
     3                 INR  ,MSRT ,EMS  )
C----------------------------------------------
C     INITIALISATION DES MASSES NODALES
C----------------------------------------------
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
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*),IPART(*)
      my_real
     .   GEO(NPROPG,*), MS(*), XIN(*),X(3,*),V(3,*),PARTSAV(20,*),
     .   XL(*),MSR(3,*),INR(3,*), MSRT(*),EMS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IP,I1,I2,ILENG
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX
      my_real
     .   XI
C---------------------------------------------------------------------
C----------------------------------------------
C     MASSE ELEMENT /2
C----------------------------------------------
      DO I=LFT,LLT
       EMS(I)=HALF*GEO(1,IXR(1,I+NFT))*XL(I)
      ENDDO
C
      IF(IREST_MSELT/=0)THEN
       DO I=LFT,LLT
         MSRT(I)=TWO*EMS(I)
       ENDDO
      END IF
C----------------------------------------------
C     INITIALISATION DES MASSES NODALES
C----------------------------------------------
      IF (IMACH.NE.3) THEN
       DO I=LFT,LLT
        I1 = IXR(2,I+NFT)
        I2 = IXR(3,I+NFT)
C
        XI=HALF*GEO(9,IXR(1,I+NFT))*XL(I)
        MS(I1)=MS(I1)+EMS(I)
        MS(I2)=MS(I2)+EMS(I)
        XIN(I1)=XIN(I1)+XI
        XIN(I2)=XIN(I2)+XI

        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + TWO*EMS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*(X(1,I1)+X(1,I2))
        PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*(X(2,I1)+X(2,I2))
        PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*(X(3,I1)+X(3,I2))
        XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2))
        XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2))
        YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2))
        YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2))
        ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2))
        ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2))
        PARTSAV(5,IP) =PARTSAV(5,IP) + TWO*XI + EMS(I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP) + TWO*XI + EMS(I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP) + TWO*XI + EMS(I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX

        PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*(V(1,I1)+V(1,I2))
        PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*(V(2,I1)+V(2,I2))
        PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*(V(3,I1)+V(3,I2))
        PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS(I) *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .     +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))
       ENDDO
      ELSE
       DO I=LFT,LLT
        I1 = IXR(2,I+NFT)
        I2 = IXR(3,I+NFT)

        XI=HALF*GEO(9,IXR(1,I+NFT))*XL(I)
        MSR(1,I)=EMS(I)
        MSR(2,I)=EMS(I)
        MSR(3,I)=EMS(I)
        INR(1,I)=XI
        INR(2,I)=XI
        INR(3,I)=XI

        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + TWO*EMS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*(X(1,I1)+X(1,I2))
        PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*(X(2,I1)+X(2,I2))
        PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*(X(3,I1)+X(3,I2))
        XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2))
        XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2))
        YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2))
        YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2))
        ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2))
        ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2))
        PARTSAV(5,IP) =PARTSAV(5,IP) + TWO*XI + EMS(I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP) + TWO*XI + EMS(I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP) + TWO*XI + EMS(I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX

        PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*(V(1,I1)+V(1,I2))
        PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*(V(2,I1)+V(2,I2))
        PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*(V(3,I1)+V(3,I2))
        PARTSAV(14,IP)=PARTSAV(14,IP) + HALF* EMS(I) *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .     +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))
       ENDDO
      ENDIF

      RETURN
      END
Chd|====================================================================
Chd|  R23MASS                       source/elements/spring/rmass.F
Chd|-- called by -----------
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R23MASS(IXR  ,GEO  ,MS   ,XIN ,PARTSAV,
     2                 X    ,V    ,IPART,XL  ,MSR    ,
     3                 INR  ,MSRT ,EMS , MASS ,UINER,MTYP)
C----------------------------------------------
C     INITIALISATION DES MASSES NODALES
C----------------------------------------------
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
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*),IPART(*),MTYP
      my_real
     .   GEO(NPROPG,*), MS(*), XIN(*),X(3,*),V(3,*),PARTSAV(20,*),
     .   XL(*),MSR(3,*),INR(3,*), MSRT(*),EMS(*),MASS(*),UINER(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IP,I1,I2,ILENG
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX
      my_real
     .   XI
C---------------------------------------------------------------------
C----------------------------------------------
C     MASSE ELEMENT /2
C----------------------------------------------
       DO I=LFT,LLT
         EMS(I)=HALF*MASS(I)
       ENDDO
C
      IF(IREST_MSELT/=0)THEN
       DO I=LFT,LLT
         MSRT(I)=TWO*EMS(I)
       ENDDO
      END IF
C----------------------------------------------
C     INITIALISATION DES MASSES NODALES
C----------------------------------------------
      IF (IMACH.NE.3) THEN
       DO I=LFT,LLT
        I1 = IXR(2,I+NFT)
        I2 = IXR(3,I+NFT)
C
        IF (MTYP == 114) THEN
          XI=HALF*UINER(I)
        ELSE
          XI=HALF*GEO(2,IXR(1,I+NFT))
        ENDIF
        MS(I1)=MS(I1)+EMS(I)
        MS(I2)=MS(I2)+EMS(I)
        XIN(I1)=XIN(I1)+XI
        XIN(I2)=XIN(I2)+XI

        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + TWO*EMS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*(X(1,I1)+X(1,I2))
        PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*(X(2,I1)+X(2,I2))
        PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*(X(3,I1)+X(3,I2))
        XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2))
        XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2))
        YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2))
        YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2))
        ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2))
        ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2))
        PARTSAV(5,IP) =PARTSAV(5,IP) + TWO*XI + EMS(I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP) + TWO*XI + EMS(I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP) + TWO*XI + EMS(I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX

        PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*(V(1,I1)+V(1,I2))
        PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*(V(2,I1)+V(2,I2))
        PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*(V(3,I1)+V(3,I2))
        PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS(I) *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .     +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))
       ENDDO
      ELSE
       DO I=LFT,LLT
        I1 = IXR(2,I+NFT)
        I2 = IXR(3,I+NFT)
C
        IF (MTYP == 114) THEN
          XI=HALF*UINER(I)
        ELSE
          XI=HALF*GEO(2,IXR(1,I+NFT))
        ENDIF
C
        MSR(1,I)=EMS(I)
        MSR(2,I)=EMS(I)
        MSR(3,I)=EMS(I)
        INR(1,I)=XI
        INR(2,I)=XI
        INR(3,I)=XI

        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + TWO*EMS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*(X(1,I1)+X(1,I2))
        PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*(X(2,I1)+X(2,I2))
        PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*(X(3,I1)+X(3,I2))
        XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2))
        XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2))
        YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2))
        YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2))
        ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2))
        ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2))
        PARTSAV(5,IP) =PARTSAV(5,IP) + TWO*XI + EMS(I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP) + TWO*XI + EMS(I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP) + TWO*XI + EMS(I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX

        PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*(V(1,I1)+V(1,I2))
        PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*(V(2,I1)+V(2,I2))
        PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*(V(3,I1)+V(3,I2))
        PARTSAV(14,IP)=PARTSAV(14,IP) + HALF* EMS(I) *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .     +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))
       ENDDO
      ENDIF

      RETURN
      END
