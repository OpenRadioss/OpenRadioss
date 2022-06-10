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
Chd|  R13MAT3                       source/elements/spring/r13mat3.F
Chd|-- called by -----------
Chd|        R13KE3                        source/elements/spring/r13ke3.F
Chd|        R8KE3                         source/elements/spring/r8ke3.F
Chd|-- calls ---------------
Chd|        RKENONL                       source/elements/spring/r4mat3.F
Chd|====================================================================
      SUBROUTINE R13MAT3(JFT    ,JLT    ,GEO    ,KX      ,KY     ,
     1                   KZ     ,MX     ,MY     ,MZ      ,MGN    ,
     2                   AL0    ,FX     ,FY     ,FZ      ,DX     ,
     3                   DY     ,DZ     ,XMOM   ,YMOM    ,ZMOM   ,
     4                   RX     ,RY     ,RZ     ,TF     ,NPF     ,
     5                   POSX   ,POSY   ,POSZ   ,POSXX  ,POSYY   ,
     6                   POSZZ  ,IGEO   )
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
      INTEGER JFT    ,JLT, MGN(*),NPF(*),IGEO(NPROPGI,*)
C     REAL
      my_real
     .   GEO(NPROPG,*), KX(*), KY(*), KZ(*), MX(*), MY(*), MZ(*),
     .   FX(*),FY(*),FZ(*),DX(*),DY(*),DZ(*),XMOM(*),YMOM(*),ZMOM(*),
     .   RX(*),RY(*),RZ(*),TF(*),POSX(*),POSY(*),POSZ(*),
     .   POSXX(*),POSYY(*),POSZZ(*),AL0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ILEN,ILENG
      INTEGER IECROU(MVSIZ), IFUNC(MVSIZ),IFUNC2(MVSIZ)
C     REAL
      my_real
     .   ALI,A(MVSIZ)
C-----------------------------------------------
C
      DO I=JFT,JLT
       KX(I)=GEO(3,MGN(I))
       KY(I)=GEO(10,MGN(I))
       KZ(I)=GEO(15,MGN(I))
      ENDDO
C---------------------
C     ROTATIONS
C---------------------
      DO I=JFT,JLT
       MX(I)=GEO(19,MGN(I))
       MY(I)=GEO(23,MGN(I))
       MZ(I)=GEO(27,MGN(I))
      ENDDO
      ILEN = 0
      DO I=JFT,JLT
C       KX(I)=GEO(41,MGN(I))*KX(I)
C       KY(I)=GEO(45,MGN(I))*KY(I)
C       KZ(I)=GEO(49,MGN(I))*KZ(I)
C       MX(I)=GEO(53,MGN(I))*MX(I)
C       MY(I)=GEO(57,MGN(I))*MY(I)
C       MZ(I)=GEO(61,MGN(I))*MZ(I)
       ILENG=NINT(GEO(93,MGN(I)))
       IF(ILENG.NE.0) ILEN = 1
      ENDDO
      IF(ILEN.NE.0) THEN
       DO I=JFT,JLT
        ILENG=NINT(GEO(93,MGN(I)))
        IF(ILENG.NE.0)THEN
         ALI = ONE/AL0(I)
         KX(I)=KX(I)*ALI
         KY(I)=KY(I)*ALI
         KZ(I)=KZ(I)*ALI
         MX(I)=MX(I)*ALI
         MY(I)=MY(I)*ALI
         MZ(I)=MZ(I)*ALI
        ENDIF
       ENDDO
      ENDIF
      IF (ISMDISP.GT.0.OR.ISPRN.EQ.1) THEN
C-----------KX------------
       DO I=JFT,JLT
        IECROU(I)=NINT(GEO(7,MGN(I)))
        IFUNC(I) = IGEO(101,MGN(I))
        IFUNC2(I)= IGEO(103,MGN(I))
        A(I)     = GEO(41,MGN(I))
       ENDDO
       CALL RKENONL(JFT    ,JLT    ,KX     ,FX     ,DX     ,
     .              IECROU ,IFUNC  ,IFUNC2 ,A      ,TF     ,
     .              NPF    ,POSX   )
C-----------KY----------
       DO I=JFT,JLT
        IECROU(I)=NINT(GEO(14,MGN(I)))
        IFUNC(I) = IGEO(104,MGN(I))
        IFUNC2(I)= IGEO(106,MGN(I))
        A(I)     = GEO(45,MGN(I))
       ENDDO
       CALL RKENONL(JFT    ,JLT    ,KY     ,FY     ,DY     ,
     .              IECROU ,IFUNC  ,IFUNC2 ,A      ,TF     ,
     .              NPF    ,POSY   )
C-----------KZ-----------
       DO I=JFT,JLT
        IECROU(I)=NINT(GEO(18,MGN(I)))
        IFUNC(I) = IGEO(107,MGN(I))
        IFUNC2(I)= IGEO(109,MGN(I))
        A(I)     = GEO(49,MGN(I))
       ENDDO
       CALL RKENONL(JFT    ,JLT    ,KZ     ,FZ     ,DZ     ,
     .              IECROU ,IFUNC  ,IFUNC2 ,A      ,TF     ,
     .              NPF    ,POSZ   )
C-----------MX------------
       DO I=JFT,JLT
        IECROU(I)=NINT(GEO(22,MGN(I)))
        IFUNC(I) = IGEO(110,MGN(I))
        IFUNC2(I)= IGEO(112,MGN(I))
        A(I)     = GEO(53,MGN(I))
       ENDDO
       CALL RKENONL(JFT    ,JLT    ,MX     ,XMOM   ,RX     ,
     .              IECROU ,IFUNC  ,IFUNC2 ,A      ,TF     ,
     .              NPF    ,POSXX  )
C-----------MY------------
       DO I=JFT,JLT
        IECROU(I)=NINT(GEO(26,MGN(I)))
        IFUNC(I) = IGEO(113,MGN(I))
        IFUNC2(I)= IGEO(115,MGN(I))
        A(I)     = GEO(57,MGN(I))
       ENDDO
       CALL RKENONL(JFT    ,JLT    ,MY     ,YMOM   ,RY     ,
     .              IECROU ,IFUNC  ,IFUNC2 ,A      ,TF     ,
     .              NPF    ,POSYY  )
C-----------MZ------------
       DO I=JFT,JLT
        IECROU(I)=NINT(GEO(30,MGN(I)))
        IFUNC(I) = IGEO(116,MGN(I))
        IFUNC2(I)= IGEO(118,MGN(I))
        A(I)     = GEO(61,MGN(I))
       ENDDO
       CALL RKENONL(JFT    ,JLT    ,MZ     ,ZMOM   ,RZ     ,
     .              IECROU ,IFUNC  ,IFUNC2 ,A      ,TF     ,
     .              NPF    ,POSZZ  )
      ENDIF
C
      RETURN
      END
