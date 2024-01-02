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
Chd|  INELTS                        source/interfaces/inter3d1/inelt.F
Chd|-- called by -----------
Chd|        I1CHK3                        source/interfaces/inter3d1/i1chk3.F
Chd|        I21ELS3                       source/interfaces/inter3d1/i21els3.F
Chd|        I2CHK3                        source/interfaces/inter3d1/i2chk3.F
Chd|        I3STI3                        source/interfaces/inter3d1/i3sti3.F
Chd|        I7STI3                        source/interfaces/inter3d1/i7sti3.F
Chd|        I9STI3                        source/interfaces/int09/i9sti3.F
Chd|-- calls ---------------
Chd|        NORMA1                        source/interfaces/inter3d1/norma1.F
Chd|====================================================================
      SUBROUTINE INELTS(X        ,IRECT ,IXS   ,NINT ,NEL       ,
     .                  I        ,AREA  ,NOINT ,IR   ,SURF_ELTYP,
     .                  SURF_ELEM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINT, NEL, I, NOINT,IR,SURF_ELTYP(*),SURF_ELEM(*)
C     REAL
      my_real
     .   AREA
      INTEGER IRECT(4,*), IXS(NIXS,*)
C     REAL
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IY(4), N, JJ, II, K, NN, KK, IC, IAD,
     .        NUSER, NUSERM
C     REAL
      my_real
     .   N1, N2, N3, DDS
      my_real :: XX1(4), XX2(4),XX3(4),XS1,YS1,ZS1,XC,YC,ZC
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      IC =0
      NEL=0
      IF (I <= 0) RETURN
      IF (SURF_ELTYP(I) /= 1) RETURN
C
      NEL=SURF_ELEM(I)
C-----------------------------------------------
C     VERIFICATION DE L'ORIENTATION DES SEGMENTS
C-----------------------------------------------
       XS1=0.
       YS1=0.
       ZS1=0.
       DO 100 JJ=1,4
       NN=IRECT(JJ,I)
       IY(JJ)=NN
       XX1(JJ)=X(1,NN)
       XX2(JJ)=X(2,NN)
       XX3(JJ)=X(3,NN)
       XS1=XS1+.25*X(1,NN)
       YS1=YS1+.25*X(2,NN)
  100  ZS1=ZS1+.25*X(3,NN)
C
       CALL NORMA1(N1,N2,N3,AREA,XX1,XX2,XX3)
       XC=0.
       YC=0.
       ZC=0.
       DO 110 K=1,8
         KK=IXS(K+1,NEL)
         XC=XC+X(1,KK)
         YC=YC+X(2,KK)
         ZC=ZC+X(3,KK)
 110   CONTINUE
       XC=XC*ONE_OVER_8
       YC=YC*ONE_OVER_8
       ZC=ZC*ONE_OVER_8
       IF(IR/=0) RETURN
       IF(IC>=2)RETURN
       DDS=N1*(XC-XS1)+N2*(YC-YS1)+N3*(ZC-ZS1)
       IF(DDS<0) RETURN
       IF(IY(3)==IY(4)) THEN
        IRECT(1,I)=IY(2)
        IRECT(2,I)=IY(1)
       ELSE
        DO 120 KK=1,4
  120   IRECT(KK,I)=IY(4-KK+1)
       ENDIF
       IF(NINT>0) WRITE (IOUT,1300) I,NOINT
       IF(NINT<0) WRITE (IOUT,1400) I,NOINT
       RETURN
C
 1300 FORMAT(' NODE NUMBERING OF SECONDARY  SEGMENT',I5,' OF INTERFACE',I10,
     + ' REVERSED')
 1400 FORMAT(' NODE NUMBERING OF MAIN SEGMENT',I5,' OF INTERFACE',I10,
     + ' REVERSED')
      END
Chd|====================================================================
Chd|  INELTC                        source/interfaces/inter3d1/inelt.F
Chd|-- called by -----------
Chd|        I1CHK3                        source/interfaces/inter3d1/i1chk3.F
Chd|        I20STI3                       source/interfaces/inter3d1/i20sti3.F
Chd|        I21ELS3                       source/interfaces/inter3d1/i21els3.F
Chd|        I24GAPM                       source/interfaces/inter3d1/i24sti3.F
Chd|        I2CHK3                        source/interfaces/inter3d1/i2chk3.F
Chd|        I3STI3                        source/interfaces/inter3d1/i3sti3.F
Chd|        I7STI3                        source/interfaces/inter3d1/i7sti3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INELTC(NELC ,NELTG ,IS ,SURF_ELTYP, SURF_ELEM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELC, IS, NELTG,SURF_ELTYP(*),SURF_ELEM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ELTYP
C-----------------------------------------------
      NELC=0
      NELTG=0

      IF (IS <= 0) RETURN
      ELTYP = SURF_ELTYP(IS)
      IF (ELTYP == 3) THEN
        NELC = SURF_ELEM(IS)
      ELSEIF (ELTYP == 7) THEN
        NELTG = SURF_ELEM(IS)
      ENDIF
!---
      RETURN
      END
Chd|====================================================================
Chd|  I20NELTS                      source/interfaces/inter3d1/inelt.F
Chd|-- called by -----------
Chd|        I20STI3                       source/interfaces/inter3d1/i20sti3.F
Chd|-- calls ---------------
Chd|        NORMA1                        source/interfaces/inter3d1/norma1.F
Chd|====================================================================
      SUBROUTINE I20NELTS(X        ,IRECT ,IXS   ,NINT ,NEL       ,
     .                    I        ,AREA  ,NOINT ,IR   ,SURF_ELTYP,
     .                    SURF_ELEM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINT, NEL, I, NOINT,IR,SURF_ELTYP(*),SURF_ELEM(*)
C     REAL
      my_real
     .   AREA
      INTEGER IRECT(4), IXS(NIXS,*)
C     REAL
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IY(4), N, JJ, II, K, NN, KK, IC, IAD,
     .        NUSER, NUSERM
C     REAL
      my_real
     .   N1, N2, N3, DDS
      my_real  XX1(4), XX2(4),XX3(4),XS1,YS1,ZS1,XC,YC,ZC
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      IC =0
      NEL=0

      IF (I <= 0) RETURN
      IF (SURF_ELTYP(I) /= 1) RETURN
C
      NEL=SURF_ELEM(I)
C-----------------------------------------------
C     VERIFICATION DE L'ORIENTATION DES SEGMENTS
C-----------------------------------------------
       XS1=0.
       YS1=0.
       ZS1=0.
       DO 100 JJ=1,4
       NN=IRECT(JJ)
       IY(JJ)=NN
       XX1(JJ)=X(1,NN)
       XX2(JJ)=X(2,NN)
       XX3(JJ)=X(3,NN)
       XS1=XS1+.25*X(1,NN)
       YS1=YS1+.25*X(2,NN)
  100  ZS1=ZS1+.25*X(3,NN)
C
       CALL NORMA1(N1,N2,N3,AREA,XX1,XX2,XX3)
       XC=0.
       YC=0.
       ZC=0.
       DO 110 K=1,8
         KK=IXS(K+1,NEL)
         XC=XC+X(1,KK)
         YC=YC+X(2,KK)
         ZC=ZC+X(3,KK)
 110   CONTINUE
       XC=XC*ONE_OVER_8
       YC=YC*ONE_OVER_8
       ZC=ZC*ONE_OVER_8
       IF(IR/=0) RETURN
       IF(IC>=2)RETURN
       DDS=N1*(XC-XS1)+N2*(YC-YS1)+N3*(ZC-ZS1)
       IF(DDS<0) RETURN
       IF(IY(3)==IY(4)) THEN
        IRECT(1)=IY(2)
        IRECT(2)=IY(1)
       ELSE
        DO 120 KK=1,4
  120   IRECT(KK)=IY(4-KK+1)
       ENDIF
       IF(NINT>0) WRITE (IOUT,1300) I,NOINT
       IF(NINT<0) WRITE (IOUT,1400) I,NOINT
       RETURN
C
 1300 FORMAT(' NODE NUMBERING OF SECONDARY  SEGMENT',I5,' OF INTERFACE',I10,
     + ' REVERSED')
 1400 FORMAT(' NODE NUMBERING OF MAIN SEGMENT',I5,' OF INTERFACE',I10,
     + ' REVERSED')
      END
Chd|====================================================================
Chd|  INELTIGEO                     source/interfaces/inter3d1/inelt.F
Chd|-- called by -----------
Chd|        I7STI3                        source/interfaces/inter3d1/i7sti3.F
Chd|-- calls ---------------
Chd|        NORMA1                        source/interfaces/inter3d1/norma1.F
Chd|====================================================================
      SUBROUTINE INELTIGEO(XE     ,IRECT  ,IXS   ,NINT     ,NELIG3D   ,
     .                     I      ,AREA   ,NOINT ,IR       ,SURF_ELTYP_IGE,
     .                     IXIG3D ,KXIG3D ,IGEO  ,SURF_ELEM_IGE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINT, NELIG3D, I, NOINT,IR
C     REAL
      my_real
     .   AREA
      INTEGER IRECT(4,*), IXS(NIXS,*),SURF_ELTYP_IGE(*),
     .        SURF_ELEM_IGE(*),KXIG3D(NIXIG3D,*),IGEO(NPROPGI,*),
     .        IXIG3D(*)
C     REAL
      my_real
     .   XE(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IY(4), N, JJ, II, K, NN, KK, IC, IAD,
     .        NUSER, NUSERM, COIN_IGE(8), PX, PY, PZ, IPID
C     REAL
      my_real
     .   N1, N2, N3, DDS
      my_real :: XX1(4), XX2(4),XX3(4),XC,YC,ZC,XS1,YS1,ZS1
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      IC =0
      NELIG3D=0
      IF (SURF_ELTYP_IGE(I) /= 101) RETURN
C
      NELIG3D=SURF_ELEM_IGE(I)

C-----------------------------------------------
C     VERIFICATION DE L'ORIENTATION DES SEGMENTS
C-----------------------------------------------
       XS1=0.
       YS1=0.
       ZS1=0.
       DO JJ=1,4
         NN=IRECT(JJ,I)
         IY(JJ)=NN
         XX1(JJ)=XE(1,NN)
         XX2(JJ)=XE(2,NN)
         XX3(JJ)=XE(3,NN)
         XS1=XS1+.25*XE(1,NN)
         YS1=YS1+.25*XE(2,NN)
         ZS1=ZS1+.25*XE(3,NN)
       ENDDO
C
       CALL NORMA1(N1,N2,N3,AREA,XX1,XX2,XX3)
       XC=0.
       YC=0.
       ZC=0.

       IPID = KXIG3D(2,NELIG3D)
       PX = IGEO(41,IPID)-1
       PY = IGEO(42,IPID)-1
       PZ = IGEO(43,IPID)-1
       COIN_IGE(1) = (PX+1)*PY+1
       COIN_IGE(2) = (PX+1)*(PY+1)
       COIN_IGE(3) = PX+1
       COIN_IGE(4) = 1
       COIN_IGE(5) = (PX+1)*(PY+1)*PZ+(PX+1)*PY+1
       COIN_IGE(6) = (PX+1)*(PY+1)*(PZ+1)
       COIN_IGE(7) = (PX+1)*(PY+1)*PZ+PX+1
       COIN_IGE(8) = (PX+1)*(PY+1)*PZ+1

       DO K=1,8
         XC=XC+XE(1,IXIG3D(KXIG3D(4,NELIG3D)+COIN_IGE(K)-1))
         YC=YC+XE(2,IXIG3D(KXIG3D(4,NELIG3D)+COIN_IGE(K)-1))
         ZC=ZC+XE(3,IXIG3D(KXIG3D(4,NELIG3D)+COIN_IGE(K)-1))
       ENDDO
       XC=XC*ONE_OVER_8
       YC=YC*ONE_OVER_8
       ZC=ZC*ONE_OVER_8
       IF(IR/=0) RETURN
       IF(IC>=2)RETURN
       DDS=N1*(XC-XS1)+N2*(YC-YS1)+N3*(ZC-ZS1)
       IF(DDS<0) RETURN
       DO  KK=1,4
         IRECT(KK,I)=IY(4-KK+1)
       ENDDO
       IF(NINT>0) WRITE (IOUT,1300) I,NOINT
       IF(NINT<0) WRITE (IOUT,1400) I,NOINT
       RETURN
C
 1300 FORMAT(' NODE NUMBERING OF SECONDARY  SEGMENT',I5,' OF INTERFACE',I10,
     + ' REVERSED')
 1400 FORMAT(' NODE NUMBERING OF MAIN SEGMENT',I5,' OF INTERFACE',I10,
     + ' REVERSED')
      END

