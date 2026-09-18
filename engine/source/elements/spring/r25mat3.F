Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you:
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    r25mat3   ../engine/source/elements/spring/r25mat3.F
!||--- called by ------------------------------------------------------
!||    r25ke3    ../engine/source/elements/spring/r25ke3.F
!||--- calls      -----------------------------------------------------
!||    rkenonl   ../engine/source/elements/spring/r4mat3.F
!||====================================================================
      SUBROUTINE R25MAT3(JFT    ,JLT    ,GEO    ,KX      ,KY     ,
     1                   KZ     ,MX     ,MY     ,MZ      ,MGN    ,
     2                   AL0    ,FX     ,FY     ,FZ      ,DX     ,
     3                   DY     ,DZ     ,XMOM   ,YMOM    ,ZMOM   ,
     4                   RX     ,RY     ,RZ     ,TF      ,NPF    ,
     5                   POSX   ,POSY   ,POSZ   ,POSXX   ,POSYY  ,
     6                   POSZZ  ,IGEO   )
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
      INTEGER JFT,JLT,MGN(*),NPF(*),IGEO(NPROPGI,*)
      my_real
     .   GEO(NPROPG,*),KX(*),KY(*),KZ(*),MX(*),MY(*),MZ(*),
     .   FX(*),FY(*),FZ(*),DX(*),DY(*),DZ(*),
     .   XMOM(*),YMOM(*),ZMOM(*),RX(*),RY(*),RZ(*),TF(*),
     .   POSX(*),POSY(*),POSZ(*),POSXX(*),POSYY(*),POSZZ(*),AL0(*)
C-----------------------------------------------
      INTEGER I,ILEN,ILENG
      INTEGER IECROU(MVSIZ),IFUNC(MVSIZ),IFUNC2(MVSIZ)
      my_real ALI,A(MVSIZ)
C-----------------------------------------------
C     /PROP/TYPE25 is axisymmetric.  Expand its four constitutive
C     directions to the six directions consumed by R13SUMG3.
      DO I=JFT,JLT
        KX(I)=GEO(3,MGN(I))
        KY(I)=GEO(10,MGN(I))
        KZ(I)=KY(I)
        MX(I)=GEO(19,MGN(I))
        MY(I)=GEO(23,MGN(I))
        MZ(I)=MY(I)
      ENDDO
C
C     Match the Type 13 length convention.
      ILEN=0
      DO I=JFT,JLT
        ILENG=NINT(GEO(93,MGN(I)))
        IF(ILENG/=0) ILEN=1
      ENDDO
      IF(ILEN/=0)THEN
        DO I=JFT,JLT
          ILENG=NINT(GEO(93,MGN(I)))
          IF(ILENG/=0)THEN
            ALI=ONE/AL0(I)
            KX(I)=KX(I)*ALI
            KY(I)=KY(I)*ALI
            KZ(I)=KZ(I)*ALI
            MX(I)=MX(I)*ALI
            MY(I)=MY(I)*ALI
            MZ(I)=MZ(I)*ALI
          ENDIF
        ENDDO
      ENDIF
C
C     Duplicate the Type 13 component-tangent behaviour for nonlinear
C     implicit work.  Type 25 uses one radial law for both transverse
C     directions and one radial law for both bending directions.
      IF(ISMDISP>0.OR.ISPRN==1)THEN
C------- axial
        DO I=JFT,JLT
          IECROU(I)=IGEO(101,MGN(I))
          IFUNC(I)=IGEO(102,MGN(I))
          IFUNC2(I)=IGEO(103,MGN(I))
          A(I)=GEO(41,MGN(I))
        ENDDO
        CALL RKENONL(JFT,JLT,KX,FX,DX,IECROU,IFUNC,IFUNC2,A,
     .               TF,NPF,POSX)
C------- radial shear, applied to local Y and Z
        DO I=JFT,JLT
          IECROU(I)=IGEO(105,MGN(I))
          IFUNC(I)=IGEO(106,MGN(I))
          IFUNC2(I)=IGEO(107,MGN(I))
          A(I)=GEO(45,MGN(I))
        ENDDO
        CALL RKENONL(JFT,JLT,KY,FY,DY,IECROU,IFUNC,IFUNC2,A,
     .               TF,NPF,POSY)
        CALL RKENONL(JFT,JLT,KZ,FZ,DZ,IECROU,IFUNC,IFUNC2,A,
     .               TF,NPF,POSZ)
C------- torsion
        DO I=JFT,JLT
          IECROU(I)=IGEO(109,MGN(I))
          IFUNC(I)=IGEO(110,MGN(I))
          IFUNC2(I)=IGEO(111,MGN(I))
          A(I)=GEO(53,MGN(I))
        ENDDO
        CALL RKENONL(JFT,JLT,MX,XMOM,RX,IECROU,IFUNC,IFUNC2,A,
     .               TF,NPF,POSXX)
C------- radial bending, applied to local Y and Z
        DO I=JFT,JLT
          IECROU(I)=IGEO(113,MGN(I))
          IFUNC(I)=IGEO(114,MGN(I))
          IFUNC2(I)=IGEO(115,MGN(I))
          A(I)=GEO(57,MGN(I))
        ENDDO
        CALL RKENONL(JFT,JLT,MY,YMOM,RY,IECROU,IFUNC,IFUNC2,A,
     .               TF,NPF,POSYY)
        CALL RKENONL(JFT,JLT,MZ,ZMOM,RZ,IECROU,IFUNC,IFUNC2,A,
     .               TF,NPF,POSZZ)
      ENDIF
C
      RETURN
      END
