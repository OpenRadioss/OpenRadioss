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
Chd|  CZCOORK3                      source/elements/shell/coquez/czcoork3.F
Chd|-- called by -----------
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        CZKEL3                        source/elements/shell/coquez/czkel3.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        CORTDIR3                      source/elements/shell/coque/cortdir3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
        SUBROUTINE CZCOORK3(JFT  ,JLT  ,X     ,IXC  ,PM    ,
     1                      OFFG ,AREA ,AREA_I,VQN  ,VQ    ,
     2                      X13  ,X24  ,Y13   ,Y24  ,MX13  ,
     3                      MX23 ,MX34 ,MY13  ,MY23 ,MY34  ,
     4                      Z1   ,GEO  ,
     5                      ELBUF_STR,SMSTR,NLAY,IREP  ,NPT   ,
     6                      ISMSTR,DIR_A ,DIR_B ,
     7                      PID  ,MAT   ,NGL  ,NPLAT,IPLAT ,
     8                      CORELV,OFF  ,THK  ,NEL)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE ELBUFDEF_MOD
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
c-----------------------------------------------
c   g l o b a l   p a r a m e t e r s
c-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER IXC(NIXC,*),JFT,JLT,IREP,NPT,ISMSTR,PID(*),MAT(*),NGL(*),NEL
      INTEGER NPLAT,NLAY,IPLAT(*)
      my_real 
     .   PM(NPROPM,*),GEO(NPROPG,*), X(3,*), 
     .   MX23(*),MY13(*),MY23(*),MY34(*),
     .   X13(*),X24(*),Y13(*),Y24(*),MX13(*),
     .   VQ(MVSIZ,3,3),AREA(*),Z1(*),MX34(*),VQN(MVSIZ,3,4),AREA_I(*)
C     .   DI(6,*),DB(3,4,*)
      my_real
     .   DIR_A(NEL,*),DIR_B(NEL,*),OFFG(*),OFF(*),
     .   CORELV(MVSIZ,2,4),THK(*)
      DOUBLE PRECISION
     .  SMSTR(*)
      TYPE(ELBUF_STRUCT_) :: ELBUF_STR
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER NNOD,I,J,K,L,M,II(6),SPLAT,JPLAT(MVSIZ),MAT_1
      PARAMETER (NNOD = 4)
      my_real 
     .   LXYZ0(3),DETA,DETA1(MVSIZ),RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ),
     .   SX(MVSIZ),SY(MVSIZ),R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),R31(MVSIZ),R32(MVSIZ),
     .   R33(MVSIZ),XL2(MVSIZ),XL3(MVSIZ),XL4(MVSIZ),YL2(MVSIZ),
     .   YL3(MVSIZ),YL4(MVSIZ),SSZ(MVSIZ),
     .   VCORE(3,NNOD),L13(MVSIZ),L24(MVSIZ),LL(MVSIZ),
     .   TOL,X13_2(MVSIZ),Y13_2(MVSIZ),X24_2(MVSIZ),Y24_2(MVSIZ),
     .   Z2(MVSIZ),A_4,SZ,SZ1,SZ2,SL,C1,C2,S1,
     .   AR(3),AD(NNOD),BTB(3),XX,YY,ZZ,XY,D(6), 
     .   ALR(3),ALD(NNOD),DBAD(3),BTB_C,J0,J1,J2
      MY_REAL 
     .   XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4
C-----------------------------------------------
      DO I=1,6
        II(I) = NEL*(I-1)
      ENDDO
C
      TOL=EM8
      MAT_1 = IXC(1,JFT)
      DO I=JFT,JLT
        MAT(I) = MAT_1
        PID(I) = IXC(6,I)
        NGL(I) = IXC(7,I)
      ENDDO
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      DO I=JFT,JLT
        RX(I)=X(1,IXC(3,I))+X(1,IXC(4,I))-X(1,IXC(2,I))-X(1,IXC(5,I))
        SX(I)=X(1,IXC(4,I))+X(1,IXC(5,I))-X(1,IXC(2,I))-X(1,IXC(3,I))
        RY(I)=X(2,IXC(3,I))+X(2,IXC(4,I))-X(2,IXC(2,I))-X(2,IXC(5,I))
        SY(I)=X(2,IXC(4,I))+X(2,IXC(5,I))-X(2,IXC(2,I))-X(2,IXC(3,I))
        RZ(I)=X(3,IXC(3,I))+X(3,IXC(4,I))-X(3,IXC(2,I))-X(3,IXC(5,I))
       SSZ(I)=X(3,IXC(4,I))+X(3,IXC(5,I))-X(3,IXC(2,I))-X(3,IXC(3,I))
      ENDDO 
      K = 0
      CALL CLSKEW3(JFT,JLT,K,
     .   RX, RY, RZ, 
     .   SX, SY, SSZ, 
     .   R11,R12,R13,R21,R22,R23,R31,R32,R33,DETA1,OFFG )
      DO I=JFT,JLT
        AREA(I)=FOURTH*DETA1(I)
        AREA_I(I)=ONE/AREA(I)
        VQ(I,1,1)=R11(I)
        VQ(I,2,1)=R21(I)
        VQ(I,3,1)=R31(I)
        VQ(I,1,2)=R12(I)
        VQ(I,2,2)=R22(I)
        VQ(I,3,2)=R32(I)
        VQ(I,1,3)=R13(I)
        VQ(I,2,3)=R23(I)
        VQ(I,3,3)=R33(I)
      ENDDO 
C--------------------------
C     TRANSMET GLOBAL-->LOCAL
C--------------------------
      DO I=JFT,JLT
        J=IXC(2,I)
        K=IXC(3,I)
        L=IXC(4,I)
        M=IXC(5,I)
        LXYZ0(1)=FOURTH*(X(1,L)+X(1,M)+X(1,J)+X(1,K))
        LXYZ0(2)=FOURTH*(X(2,L)+X(2,M)+X(2,J)+X(2,K))
        LXYZ0(3)=FOURTH*(X(3,L)+X(3,M)+X(3,J)+X(3,K))
C
        XX1=X(1,K)-X(1,J)
        YY1=X(2,K)-X(2,J)
        ZZ1=X(3,K)-X(3,J)
C
        XL2(I)=R11(I)*XX1+R21(I)*YY1+R31(I)*ZZ1
        YL2(I)=R12(I)*XX1+R22(I)*YY1+R32(I)*ZZ1
C
        XX2=X(1,J)-LXYZ0(1)
        YY2=X(2,J)-LXYZ0(2)
        ZZ2=X(3,J)-LXYZ0(3)
        Z1(I)=R13(I)*XX2+R23(I)*YY2+R33(I)*ZZ2
C
        XX3=X(1,L)-X(1,J)
        YY3=X(2,L)-X(2,J)
        ZZ3=X(3,L)-X(3,J)
        XL3(I)=R11(I)*XX3+R21(I)*YY3+R31(I)*ZZ3
        YL3(I)=R12(I)*XX3+R22(I)*YY3+R32(I)*ZZ3
C
        XX4=X(1,M)-X(1,J)
        YY4=X(2,M)-X(2,J)
        ZZ4=X(3,M)-X(3,J)
        XL4(I)=R11(I)*XX4+R21(I)*YY4+R31(I)*ZZ4
        YL4(I)=R12(I)*XX4+R22(I)*YY4+R32(I)*ZZ4
      ENDDO
C----------------------------
C     SMALL STRAIN
C----------------------------
      IF(ISMSTR==1.OR.ISMSTR==2)THEN
        DO I=JFT,JLT
          IF(ABS(OFFG(I))==TWO)THEN
            XL2(I)=SMSTR(II(1)+I)
            YL2(I)=SMSTR(II(2)+I)
            XL3(I)=SMSTR(II(3)+I)
            YL3(I)=SMSTR(II(4)+I)
            XL4(I)=SMSTR(II(5)+I)
            YL4(I)=SMSTR(II(6)+I)
            Z1(I)=ZERO
            AREA(I)=HALF*
     .              ((XL2(I)-XL4(I))*YL3(I)-XL3(I)*(YL2(I)-YL4(I)))
            AREA_I(I)=ONE/MAX(EM20,AREA(I))
          ELSE
            SMSTR(II(1)+I)=XL2(I)
            SMSTR(II(2)+I)=YL2(I)
            SMSTR(II(3)+I)=XL3(I)
            SMSTR(II(4)+I)=YL3(I)
            SMSTR(II(5)+I)=XL4(I)
            SMSTR(II(6)+I)=YL4(I)
         ENDIF
        ENDDO
      ENDIF
      IF(ISMSTR==1)THEN
        DO I=JFT,JLT
            IF(OFFG(I)==ONE)OFFG(I)=TWO
        ENDDO
      ENDIF
C----------------------------
C     ORTHOTROPY (plus tard)
C----------------------------
      IF (IREP > 0) THEN
       CALL CORTDIR3(ELBUF_STR,DIR_A,DIR_B ,JFT    ,JLT    ,
     .               NLAY   ,IREP   ,RX    ,RY     ,RZ     , 
     .               SX     ,SY     ,SSZ   ,R11    ,R21    ,
     .               R31    ,R12    ,R22   ,R32    ,NEL    )
      ENDIF

      DO I=JFT,JLT
        LXYZ0(1)=FOURTH*(XL2(I)+XL3(I)+XL4(I))
        LXYZ0(2)=FOURTH*(YL2(I)+YL3(I)+YL4(I))
        CORELV(I,1,1)=-LXYZ0(1)
        CORELV(I,1,2)=XL2(I)-LXYZ0(1)
        CORELV(I,1,3)=XL3(I)-LXYZ0(1)
        CORELV(I,1,4)=XL4(I)-LXYZ0(1)
        CORELV(I,2,1)=-LXYZ0(2)
        CORELV(I,2,2)=YL2(I)-LXYZ0(2)
        CORELV(I,2,3)=YL3(I)-LXYZ0(2)
        CORELV(I,2,4)=YL4(I)-LXYZ0(2)
        X13(I)=(CORELV(I,1,1)-CORELV(I,1,3))*HALF
        X24(I)=(CORELV(I,1,2)-CORELV(I,1,4))*HALF
        Y13(I)=(CORELV(I,2,1)-CORELV(I,2,3))*HALF
        Y24(I)=(CORELV(I,2,2)-CORELV(I,2,4))*HALF
C
        MX13(I)=(CORELV(I,1,1)+CORELV(I,1,3))*HALF
        MX23(I)=(CORELV(I,1,2)+CORELV(I,1,3))*HALF
        MX34(I)=(CORELV(I,1,3)+CORELV(I,1,4))*HALF
        MY13(I)=(CORELV(I,2,1)+CORELV(I,2,3))*HALF
        MY23(I)=(CORELV(I,2,2)+CORELV(I,2,3))*HALF
        MY34(I)=(CORELV(I,2,3)+CORELV(I,2,4))*HALF
C
        X13_2(I) =X13(I)*X13(I)
        Y13_2(I) =Y13(I)*Y13(I)
        X24_2(I) =X24(I)*X24(I)
        Y24_2(I) =Y24(I)*Y24(I)
        L13(I)=X13_2(I)+Y13_2(I)
        L24(I)=X24_2(I)+Y24_2(I)
        LL(I)=HALF*(L13(I)+L24(I))
                 S1=EM01*THK(I)*THK(I)
        LL(I)=MAX(LL(I),S1)
      ENDDO
      IF (IMP_CHK > 0) THEN
       S1 =.577350269189626
       DO I=JFT,JLT
        J1=(MX23(I)*MY13(I)-MX13(I)*MY23(I))*S1
        J2=-(MX13(I)*MY34(I)-MX34(I)*MY13(I))*S1
        J0=AREA(I)*FOURTH
        XX1=J0+J2-J1
        XX2=J0+J2+J1
        XX3=J0-J2+J1
        XX4=J0-J2-J1
        IF(OFFG(I)/=ZERO)THEN
         IF(XX1<=ZERO.OR.XX2<=ZERO.OR.
     .      XX3<=ZERO.OR.XX4<=ZERO)THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IR = IMP_IR + 1
         ENDIF 
        ENDIF 
       ENDDO
      ENDIF 
C--------------------------
      DO I=JFT,JLT
       Z2(I)=Z1(I)*Z1(I)
       IF (Z2(I)<LL(I)*TOL.OR.NPT==1) THEN
         Z1(I)=ZERO
       ELSE
C--------------------------------------------------
C WARPING SPECIAL TREATMENT
C full projection eliminer drilling rotations and rigid rotations
C--------------------------------------------------------------------------  
         A_4=AREA(I)*FOURTH
C----------  node N ----------
         SZ1=MX13(I)*Y24(I)-MY13(I)*X24(I)
         SZ2=A_4+SZ1
         SZ=Z2(I)*L24(I)
         SL=ONE/SQRT(SZ+SZ2*SZ2)
         VQN(I,1,1)=-Z1(I)*Y24(I)
         VQN(I,2,1)= Z1(I)*X24(I)
         VQN(I,3,1)= SZ2*SL
         VQN(I,1,3)=-VQN(I,1,1)
         VQN(I,2,3)=-VQN(I,2,1)
         VQN(I,1,1)= VQN(I,1,1)*SL
         VQN(I,2,1)= VQN(I,2,1)*SL
C
         SZ2=A_4-SZ1
         SL=ONE/SQRT(SZ+SZ2*SZ2)
         VQN(I,1,3)= VQN(I,1,3)*SL
         VQN(I,2,3)= VQN(I,2,3)*SL
         VQN(I,3,3)= SZ2*SL
C
         SZ1=MX13(I)*Y13(I)-MY13(I)*X13(I)
         SZ2=A_4+SZ1
         SZ=Z2(I)*L13(I)
         SL=ONE/SQRT(SZ+SZ2*SZ2)
         VQN(I,1,2)=-Z1(I)*Y13(I)
         VQN(I,2,2)= Z1(I)*X13(I)
         VQN(I,3,2)= SZ2*SL
         VQN(I,1,4)=-VQN(I,1,2)
         VQN(I,2,4)=-VQN(I,2,2)
         VQN(I,1,2)= VQN(I,1,2)*SL
         VQN(I,2,2)= VQN(I,2,2)*SL
C
         SZ2=A_4-SZ1
         SL=ONE/SQRT(SZ+SZ2*SZ2)
         VQN(I,1,4)= VQN(I,1,4)*SL
         VQN(I,2,4)= VQN(I,2,4)*SL
         VQN(I,3,4)= SZ2*SL
       ENDIF
      ENDDO
C
      NPLAT=JFT-1
      SPLAT= 0
      DO I=JFT,JLT
       IF (Z1(I)==ZERO) THEN
        NPLAT=NPLAT+1
        IPLAT(NPLAT)=I
       ELSE
        SPLAT=SPLAT+1
        JPLAT(SPLAT)=I
       ENDIF
      ENDDO 
      DO I=NPLAT+1,JLT
       IPLAT(I)=JPLAT(I-NPLAT)
      ENDDO 
C
      DO I=JFT,JLT
       OFF(I)=OFFG(I)
      ENDDO
C
      RETURN
 2001 FORMAT(/' ZERO OR NEGATIVE SHELL SUB-AREA : ELEMENT NB:',I8/)
      END
