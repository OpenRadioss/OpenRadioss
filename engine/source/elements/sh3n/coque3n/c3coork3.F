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
Chd|  C3COORK3                      source/elements/sh3n/coque3n/c3coork3.F
Chd|-- called by -----------
Chd|        C3KE3                         source/elements/sh3n/coque3n/c3ke3.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        CORTDIR3                      source/elements/shell/coque/cortdir3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE C3COORK3(JFT     ,JLT    ,X     ,IXTG   ,  OFFG,
     2                     GEO    ,PID    ,MAT    ,NGL   ,AREA   ,  
     3                     IREP   ,NPT    ,ISMSTR ,NLAY  ,
     3                     ELBUF_STR,SMSTR,DIR_A  ,DIR_B  ,
     4                     XL2    ,XL3    ,YL3    ,
     5                     R11    ,R12    ,R13    ,R21  ,
     6                     R22    ,R23    ,R31    ,R32   ,R33  ,
     7                     K11    ,K12    ,K13    ,K22   ,K23  ,K33 ,
     8                     M11    ,M12    ,M13    ,M22   ,M23  ,M33 , 
     9                     MF11   ,MF12,MF13,MF22,MF23,MF33, 
     A                     FM12   ,FM13,FM23,OFF  ,NEL) 
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE ELBUFDEF_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT ,IREP,NPT,ISMSTR,NLAY,NEL
      INTEGER IXTG(NIXTG,*),PID(*),MAT(*),NGL(*)
C     REAL
      my_real
     .   X(3,*),OFFG(*),AREA(*),
     .   GEO(NPROPG,*),XL2(*),XL3(*),YL3(*),
     .   R11(*),R12(*),R13(*),
     .   R21(*),R22(*),R23(*),
     .   R31(*),R32(*),R33(*),
     .    K11(9,*),K12(9,*),K13(9,*),
     .    K22(9,*),K23(9,*),K33(9,*),
     .    M11(9,*),M12(9,*),M13(9,*),
     .    M22(9,*),M23(9,*),M33(9,*),
     .    MF11(9,*),MF12(9,*),MF13(9,*),
     .    MF22(9,*),MF23(9,*),MF33(9,*),
     .    FM12(9,*),FM13(9,*),FM23(9,*),OFF(*),SMSTR(*),
     .    DIR_A(NEL,*),DIR_B(NEL,*)
      TYPE(ELBUF_STRUCT_) :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NC1, NC2, NC3,I,II(3),J,K, MAT_1
C     REAL
      my_real
     .     X1(MVSIZ), Y1(MVSIZ), Z1(MVSIZ),DETA1(MVSIZ),
     .     X2(MVSIZ), Y2(MVSIZ), Z2(MVSIZ),
     .     X3(MVSIZ), Y3(MVSIZ), Z3(MVSIZ),
     .     RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ),
     .     SX(MVSIZ), SY(MVSIZ), SZ(MVSIZ)
C-----------------------------------------------
      DO I=1,3
        II(I) = NEL*(I-1)
      ENDDO
C
      MAT_1 = IXTG(1,JFT)
      DO I=JFT,JLT
       MAT(I)  = MAT_1
       NC1     = IXTG(2,I)
       NC2     = IXTG(3,I)
       NC3     = IXTG(4,I)
       PID(I)  = IXTG(5,I)
       NGL(I)  = IXTG(6,I)
C----------------------------
C     COORDONNEES
C----------------------------
       X1(I)=X(1,NC1)
       Y1(I)=X(2,NC1)
       Z1(I)=X(3,NC1)
       X2(I)=X(1,NC2)
       Y2(I)=X(2,NC2)
       Z2(I)=X(3,NC2)
       X3(I)=X(1,NC3)
       Y3(I)=X(2,NC3)
       Z3(I)=X(3,NC3)
      ENDDO
C
      DO I=JFT,JLT
        RX(I)=X2(I)-X1(I)
        SX(I)=X3(I)-X1(I)
        RY(I)=Y2(I)-Y1(I)
        SY(I)=Y3(I)-Y1(I)
        RZ(I)=Z2(I)-Z1(I)
        SZ(I)=Z3(I)-Z1(I)
      ENDDO 
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      K = 2
      CALL CLSKEW3(JFT,JLT,K,
     .   RX, RY, RZ, 
     .   SX, SY, SZ, 
     .   R11,R12,R13,R21,R22,R23,R31,R32,R33,DETA1,OFFG )
C----------------------------
C     CHANGE TO LOCAL SYSTEM
C----------------------------
      DO I=JFT,JLT
       AREA(I)=HALF*DETA1(I)
       XL2(I)=R11(I)*RX(I)+R21(I)*RY(I)+R31(I)*RZ(I)
       XL3(I)=R11(I)*SX(I)+R21(I)*SY(I)+R31(I)*SZ(I)
       YL3(I)=R12(I)*SX(I)+R22(I)*SY(I)+R32(I)*SZ(I)
      ENDDO
C----------------------------
C     SMALL STRAIN
C----------------------------
      IF(ISMSTR==1.OR.ISMSTR==2)THEN
        DO I=JFT,JLT
          IF(ABS(OFFG(I))==2.)THEN
            XL2(I)=SMSTR(II(1)+I)
            XL3(I)=SMSTR(II(2)+I)
            YL3(I)=SMSTR(II(3)+I)
          ELSE
            SMSTR(II(1)+I)=XL2(I)
            SMSTR(II(2)+I)=XL3(I)
            SMSTR(II(3)+I)=YL3(I)
          ENDIF
        ENDDO
      ENDIF
      IF(ISMSTR==1)THEN
        DO I=JFT,JLT
          IF(OFFG(I) == ONE) OFFG(I)=TWO
        ENDDO
      ENDIF
C----------------------------
C     ORTHOTROPY plus tard
C----------------------------
      IF (IREP > 0) THEN
       CALL CORTDIR3(ELBUF_STR,DIR_A,DIR_B ,JFT    ,JLT    ,
     .               NLAY   ,IREP   ,RX    ,RY     ,RZ     , 
     .               SX     ,SY     ,SZ    ,R11    ,R21    ,
     .               R31    ,R12    ,R22   ,R32    ,NEL    )
      ENDIF
c
      DO J=1,9
       DO I=JFT,JLT
          K11(J,I)=ZERO
          K12(J,I)=ZERO
          K13(J,I)=ZERO
          K22(J,I)=ZERO
          K23(J,I)=ZERO
          K33(J,I)=ZERO
          M11(J,I)=ZERO
          M12(J,I)=ZERO
          M13(J,I)=ZERO
          M22(J,I)=ZERO
          M23(J,I)=ZERO
          M33(J,I)=ZERO
       ENDDO 
      ENDDO 
      DO I=JFT,JLT
       OFF(I)=OFFG(I)
      ENDDO
C
      RETURN
      END
