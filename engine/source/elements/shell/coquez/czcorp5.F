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
Chd|  CZCORP5                       source/elements/shell/coquez/czcorp5.F
Chd|-- called by -----------
Chd|        CZCORC1                       source/elements/shell/coquez/czcorc.F
Chd|-- calls ---------------
Chd|        A3INVDP_V                     source/elements/shell/coquez/a3invdp_v.F
Chd|====================================================================
      SUBROUTINE CZCORP5( NUMNOD ,NEL    ,NUMELC ,VR      ,NPT    ,TOL    ,
     2                    IXC    ,PLAT   ,AREA   ,AREA_I ,V13    ,
     3                    V24    ,VHI    ,RLXYZ ,VQN    ,VQ     ,
     4                    X13    ,X24    ,Y13    ,Y24    ,MX13   ,
     6                    MY13   ,
     7                    Z1     ,DI     ,DB     ,COREL  ,RLZ    ,
     8                    LL     ,
     9                    L13    ,L24    ,IDRIL  ,DIZ    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "impl1_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NUMNOD,NEL,NUMELC,IDRIL,NPT
        LOGICAL, DIMENSION(NEL), INTENT(INOUT) :: PLAT 
        INTEGER, DIMENSION(NIXC,NUMELC), INTENT(IN) :: IXC
        my_real, DIMENSION(3,NUMNOD), INTENT(IN) :: VR 
        my_real, DIMENSION(MVSIZ,3), INTENT(INOUT) :: V13,V24,VHI,DIZ
        my_real, DIMENSION(MVSIZ,6), INTENT(INOUT) :: DI
        my_real, DIMENSION(MVSIZ,3,3), INTENT(INOUT) :: VQ
        my_real, DIMENSION(MVSIZ,3,4), INTENT(INOUT) :: VQN,DB
        my_real, DIMENSION(MVSIZ,2,4), INTENT(INOUT) :: RLXYZ,COREL
        my_real, DIMENSION(MVSIZ,4), INTENT(INOUT) :: RLZ
        my_real, DIMENSION(NEL), INTENT(INOUT) :: MX13,MY13
        my_real, DIMENSION(NEL), INTENT(INOUT) :: X13,X24,Y13,Y24
        my_real, DIMENSION(NEL), INTENT(INOUT) :: AREA,Z1,AREA_I
        my_real, DIMENSION(NEL), INTENT(INOUT) :: LL,L13,L24
        my_real, INTENT(INOUT) :: TOL
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER NNOD,I,J,K,L
        PARAMETER (NNOD = 4)
        my_real :: DETA,
     .   RRXYZ(MVSIZ,3,NNOD),
     .   Z2(MVSIZ),A_4,SZ,SZ1,SZ2,SL,C1,C2,
     .   AR(MVSIZ,3),BTB(6),XX(MVSIZ),YY(MVSIZ),ZZ(MVSIZ),XY(MVSIZ),XZ(MVSIZ),YZ(MVSIZ),
     .   ABC,XXYZ2,YYXZ2,ZZXY2,D(MVSIZ,6),DIZ1(MVSIZ,6),DIZ2(MVSIZ,6),
     .   ALR(3),ALD(NNOD),DBAD(3),BTB_C,ALRZ
        my_real, DIMENSION(MVSIZ,NNOD) :: AD
C----------------------------------
        DO I=1,NEL
            Z2(I)=Z1(I)*Z1(I)
            IF (Z2(I)<LL(I)*TOL.OR.NPT == 1) THEN
                Z1(I)=ZERO
                PLAT(I)=.TRUE.
            ELSE
                PLAT(I)=.FALSE.
C--------------------------------------------------
C WARPING SPECIAL TREATMENT
C full projection eliminer drilling rotations and rigid rotations
C--------------------------------------------------------------------------  
                A_4=AREA(I)*FOURTH
C
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
C       
                K=IXC(2,I)
                RRXYZ(I,1,1) =RLXYZ(I,1,1)
                RRXYZ(I,2,1) =RLXYZ(I,2,1)
                RRXYZ(I,3,1) =VQ(I,1,3)*VR(1,K)+VQ(I,2,3)*VR(2,K)
     1               +VQ(I,3,3)*VR(3,K)
                K=IXC(3,I)
                RRXYZ(I,1,2) =RLXYZ(I,1,2)
                RRXYZ(I,2,2) =RLXYZ(I,2,2)
                RRXYZ(I,3,2) =VQ(I,1,3)*VR(1,K)+VQ(I,2,3)*VR(2,K)
     1               +VQ(I,3,3)*VR(3,K)
                K=IXC(4,I)
                RRXYZ(I,1,3) =RLXYZ(I,1,3) 
                RRXYZ(I,2,3) =RLXYZ(I,2,3)
                RRXYZ(I,3,3) =VQ(I,1,3)*VR(1,K)+VQ(I,2,3)*VR(2,K)
     1               +VQ(I,3,3)*VR(3,K)
                K=IXC(5,I)
                RRXYZ(I,1,4) =RLXYZ(I,1,4)
                RRXYZ(I,2,4) =RLXYZ(I,2,4)
                RRXYZ(I,3,4) =VQ(I,1,3)*VR(1,K)+VQ(I,2,3)*VR(2,K)
     1               +VQ(I,3,3)*VR(3,K)
            ENDIF
       ENDDO

       IF (IMPL_S>0.AND.IKPROJ<=0) THEN
C-------------------------------------
C     DRILLING PROJECTION ONLY
C-------------------------------------
            DO I=1,NEL
                IF(.NOT.PLAT(I)) THEN
                    IF (IDRIL>0.0 ) THEN
                        RLZ(I,1)=AREA_I(I)*(VQN(I,1,1)*RRXYZ(I,1,1)+
     1                    VQN(I,2,1)*RRXYZ(I,2,1)+VQN(I,3,1)*RRXYZ(I,3,1))
                        RLZ(I,2)=AREA_I(I)*(VQN(I,1,2)*RRXYZ(I,1,2)+
     1                    VQN(I,2,2)*RRXYZ(I,2,2)+VQN(I,3,2)*RRXYZ(I,3,2))
                        RLZ(I,3)=AREA_I(I)*(VQN(I,1,3)*RRXYZ(I,1,3)+
     1                    VQN(I,2,3)*RRXYZ(I,2,3)+VQN(I,3,3)*RRXYZ(I,3,3))
                        RLZ(I,4)=AREA_I(I)*(VQN(I,1,4)*RRXYZ(I,1,4)+
     1                    VQN(I,2,4)*RRXYZ(I,2,4)+VQN(I,3,4)*RRXYZ(I,3,4))
                    END IF
                    RLXYZ(I,1,1)=(1.-VQN(I,1,1)*VQN(I,1,1))*RRXYZ(I,1,1)
     1                       -VQN(I,1,1)*VQN(I,2,1) *RRXYZ(I,2,1)
     2                       -VQN(I,1,1)*VQN(I,3,1) *RRXYZ(I,3,1)
                    RLXYZ(I,2,1)=(1.-VQN(I,2,1)*VQN(I,2,1))*RRXYZ(I,2,1)
     1                       -VQN(I,1,1)*VQN(I,2,1) *RRXYZ(I,1,1)
     2                       -VQN(I,2,1)*VQN(I,3,1) *RRXYZ(I,3,1)
C   
                    RLXYZ(I,1,2)=(1.-VQN(I,1,2)*VQN(I,1,2))*RRXYZ(I,1,2)
     1                       -VQN(I,1,2)*VQN(I,2,2) *RRXYZ(I,2,2)
     2                       -VQN(I,1,2)*VQN(I,3,2) *RRXYZ(I,3,2)
                    RLXYZ(I,2,2)=(1.-VQN(I,2,2)*VQN(I,2,2))*RRXYZ(I,2,2)
     1                       -VQN(I,1,2)*VQN(I,2,2) *RRXYZ(I,1,2)
     2                       -VQN(I,2,2)*VQN(I,3,2) *RRXYZ(I,3,2)
C   
                    RLXYZ(I,1,3)=(1.-VQN(I,1,3)*VQN(I,1,3))*RRXYZ(I,1,3)
     1                       -VQN(I,1,3)*VQN(I,2,3) *RRXYZ(I,2,3)
     2                       -VQN(I,1,3)*VQN(I,3,3) *RRXYZ(I,3,3)
                    RLXYZ(I,2,3)=(1.-VQN(I,2,3)*VQN(I,2,3))*RRXYZ(I,2,3)
     1                       -VQN(I,1,3)*VQN(I,2,3) *RRXYZ(I,1,3)
     2                       -VQN(I,2,3)*VQN(I,3,3) *RRXYZ(I,3,3)
C   
                    RLXYZ(I,1,4)=(1.-VQN(I,1,4)*VQN(I,1,4))*RRXYZ(I,1,4)
     1                       -VQN(I,1,4)*VQN(I,2,4) *RRXYZ(I,2,4)
     2                       -VQN(I,1,4)*VQN(I,3,4) *RRXYZ(I,3,4)
                    RLXYZ(I,2,4)=(1.-VQN(I,2,4)*VQN(I,2,4))*RRXYZ(I,2,4)
     1                       -VQN(I,1,4)*VQN(I,2,4) *RRXYZ(I,1,4)
     2                       -VQN(I,2,4)*VQN(I,3,4) *RRXYZ(I,3,4)
                ENDIF ! plat
            ENDDO
        ELSE 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C-----------------------full projection------------------
            DO I=1,NEL
                IF(.NOT.PLAT(I)) THEN
                    AR(I,1)=-Z1(I)*VHI(I,2)+Y13(I)*V13(I,3)+Y24(I)*V24(I,3)
     1                  +MY13(I)*VHI(I,3)
     2                  +RRXYZ(I,1,1)+RRXYZ(I,1,2)+RRXYZ(I,1,3)+RRXYZ(I,1,4)
                    AR(I,2)= Z1(I)*VHI(I,1)-X13(I)*V13(I,3)-X24(I)*V24(I,3)
     1                  -MX13(I)*VHI(I,3)
     2                  +RRXYZ(I,2,1)+RRXYZ(I,2,2)+RRXYZ(I,2,3)+RRXYZ(I,2,4)
                    AR(I,3)= X13(I)*V13(I,2)+X24(I)*V24(I,2)+MX13(I)*VHI(I,2)
     1                  -Y13(I)*V13(I,1)-Y24(I)*V24(I,1)-MY13(I)*VHI(I,1)
     2                  +RRXYZ(I,3,1)+RRXYZ(I,3,2)+RRXYZ(I,3,3)+RRXYZ(I,3,4)
                    AD(I,1)= VQN(I,1,1)*RRXYZ(I,1,1)+VQN(I,2,1)*RRXYZ(I,2,1)+
     1                  VQN(I,3,1)*RRXYZ(I,3,1)
                    AD(I,2)= VQN(I,1,2)*RRXYZ(I,1,2)+VQN(I,2,2)*RRXYZ(I,2,2)+
     1                  VQN(I,3,2)*RRXYZ(I,3,2)
                    AD(I,3)= VQN(I,1,3)*RRXYZ(I,1,3)+VQN(I,2,3)*RRXYZ(I,2,3)+
     1                  VQN(I,3,3)*RRXYZ(I,3,3)
                    AD(I,4)= VQN(I,1,4)*RRXYZ(I,1,4)+VQN(I,2,4)*RRXYZ(I,2,4)+
     1               VQN(I,3,4)*RRXYZ(I,3,4)
C      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
                    XX(I) = COREL(I,1,1)*COREL(I,1,1)+COREL(I,1,2)*COREL(I,1,2)
     1                  +COREL(I,1,3)*COREL(I,1,3)+COREL(I,1,4)*COREL(I,1,4)
                    YY(I) = COREL(I,2,1)*COREL(I,2,1)+COREL(I,2,2)*COREL(I,2,2)
     1                  +COREL(I,2,3)*COREL(I,2,3)+COREL(I,2,4)*COREL(I,2,4)
                    XY(I) = COREL(I,1,1)*COREL(I,2,1)+COREL(I,1,2)*COREL(I,2,2)
     1                  +COREL(I,1,3)*COREL(I,2,3)+COREL(I,1,4)*COREL(I,2,4)
                    XZ(I) =(COREL(I,1,1)-COREL(I,1,2)+COREL(I,1,3)-COREL(I,1,4))
     .                  *Z1(I)
                    YZ(I) =(COREL(I,2,1)-COREL(I,2,2)+COREL(I,2,3)-COREL(I,2,4))
     .                  *Z1(I)
                    ZZ(I) = FOUR*Z2(I)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
                    BTB(1)= VQN(I,1,1)*VQN(I,1,1)+VQN(I,1,2)*VQN(I,1,2)
     1                  +VQN(I,1,3)*VQN(I,1,3)+VQN(I,1,4)*VQN(I,1,4)
                    BTB(2)= VQN(I,2,1)*VQN(I,2,1)+VQN(I,2,2)*VQN(I,2,2)
     1                  +VQN(I,2,3)*VQN(I,2,3)+VQN(I,2,4)*VQN(I,2,4)
                    BTB(3)= VQN(I,3,1)*VQN(I,3,1)+VQN(I,3,2)*VQN(I,3,2)
     1                  +VQN(I,3,3)*VQN(I,3,3)+VQN(I,3,4)*VQN(I,3,4)
                    BTB(4)= VQN(I,1,1)*VQN(I,2,1)+VQN(I,1,2)*VQN(I,2,2)
     1                  +VQN(I,1,3)*VQN(I,2,3)+VQN(I,1,4)*VQN(I,2,4)
                    BTB(5)= VQN(I,1,1)*VQN(I,3,1)+VQN(I,1,2)*VQN(I,3,2)
     1                  +VQN(I,1,3)*VQN(I,3,3)+VQN(I,1,4)*VQN(I,3,4)
                    BTB(6)= VQN(I,2,1)*VQN(I,3,1)+VQN(I,2,2)*VQN(I,3,2)
     1                  +VQN(I,2,3)*VQN(I,3,3)+VQN(I,2,4)*VQN(I,3,4)
                    D(I,1)= YY(I)+ZZ(I)+FOUR-BTB(1)
                    D(I,2)= XX(I)+ZZ(I)+FOUR-BTB(2)
                    D(I,3)= XX(I)+YY(I)+FOUR-BTB(3)
                    D(I,4)= -XY(I)-BTB(4)
                    D(I,5)= -XZ(I)-BTB(5)
                    D(I,6)= -YZ(I)-BTB(6)
                ENDIF
            ENDDO
            IF(IRESP == 1)THEN
                CALL A3INVDP_V(D,DIZ2,NEL,PLAT)
                DO I=1,NEL
                    IF(.NOT.PLAT(I)) THEN
                        DI(I,1) = DIZ2(I,1)
                        DI(I,2) = DIZ2(I,2)
                        DI(I,3) = DIZ2(I,3)
                        DI(I,4) = DIZ2(I,4)
                        DI(I,5) = DIZ2(I,5)
                        DI(I,6) = DIZ2(I,6)
                    ENDIF
                ENDDO
            ELSE
                DO I=1,NEL
                    IF(.NOT.PLAT(I)) THEN
                        ABC = D(I,1)*D(I,2)*D(I,3)
                        XXYZ2 = D(I,1)*D(I,6)*D(I,6)
                        YYXZ2 = D(I,2)*D(I,5)*D(I,5)
                        ZZXY2 = D(I,3)*D(I,4)*D(I,4)
                        DETA = ABS(ABC+TWO*D(I,4)*D(I,5)*D(I,6)-XXYZ2-YYXZ2-ZZXY2)
                        DETA = ONE/MAX(DETA,EM20)
                        DI(I,1) = (ABC-XXYZ2)*DETA/MAX(D(I,1),EM20)
                        DI(I,2) = (ABC-YYXZ2)*DETA/MAX(D(I,2),EM20)
                        DI(I,3) = (ABC-ZZXY2)*DETA/MAX(D(I,3),EM20)
                        DI(I,4) = (D(I,5)*D(I,6)-D(I,4)*D(I,3))*DETA
                        DI(I,5) = (D(I,6)*D(I,4)-D(I,5)*D(I,2))*DETA
                        DI(I,6) = (D(I,4)*D(I,5)-D(I,6)*D(I,1))*DETA
                    ENDIF
                ENDDO
            END IF !(IRESP == 1)THEN
            DO J=1,NNOD
                DO I=1,NEL
                    IF(.NOT.PLAT(I)) THEN
                        DB(I,1,J)= DI(I,1)*VQN(I,1,J)+DI(I,4)*VQN(I,2,J)
     1                            +DI(I,5)*VQN(I,3,J)
                        DB(I,2,J)= DI(I,4)*VQN(I,1,J)+DI(I,2)*VQN(I,2,J)
     1                            +DI(I,6)*VQN(I,3,J)
                        DB(I,3,J)= DI(I,5)*VQN(I,1,J)+DI(I,6)*VQN(I,2,J)
     1                            +DI(I,3)*VQN(I,3,J)
                    ENDIF
                ENDDO
            ENDDO
C      
            DO I=1,NEL
                IF(.NOT.PLAT(I)) THEN
                    DBAD(1)= DB(I,1,1)*AD(I,1)+DB(I,1,2)*AD(I,2)
     1                        +DB(I,1,3)*AD(I,3)+DB(I,1,4)*AD(I,4)
                    DBAD(2)= DB(I,2,1)*AD(I,1)+DB(I,2,2)*AD(I,2)
     1                        +DB(I,2,3)*AD(I,3)+DB(I,2,4)*AD(I,4)
                    DBAD(3)= DB(I,3,1)*AD(I,1)+DB(I,3,2)*AD(I,2)
     1                        +DB(I,3,3)*AD(I,3)+DB(I,3,4)*AD(I,4)
C   
                    ALR(1) =DI(I,1)*AR(I,1)+DI(I,4)*AR(I,2)+DI(I,5)*AR(I,3)-DBAD(1)
                    ALR(2) =DI(I,4)*AR(I,1)+DI(I,2)*AR(I,2)+DI(I,6)*AR(I,3)-DBAD(2)
                    ALR(3) =DI(I,5)*AR(I,1)+DI(I,6)*AR(I,2)+DI(I,3)*AR(I,3)-DBAD(3)
C   
                    ALD(1) = AD(I,1)+VQN(I,1,1)*DBAD(1)+VQN(I,2,1)*DBAD(2)
     1                      +VQN(I,3,1)*DBAD(3)
     2                      -DB(I,1,1)*AR(I,1)-DB(I,2,1)*AR(I,2)-DB(I,3,1)*AR(I,3)
                    ALD(2) = AD(I,2)+VQN(I,1,2)*DBAD(1)+VQN(I,2,2)*DBAD(2)
     1                    +VQN(I,3,2)*DBAD(3)
     2                    -DB(I,1,2)*AR(I,1)-DB(I,2,2)*AR(I,2)-DB(I,3,2)*AR(I,3)
                    ALD(3) = AD(I,3)+VQN(I,1,3)*DBAD(1)+VQN(I,2,3)*DBAD(2)
     1                    +VQN(I,3,3)*DBAD(3)
     2                    -DB(I,1,3)*AR(I,1)-DB(I,2,3)*AR(I,2)-DB(I,3,3)*AR(I,3)
                    ALD(4) = AD(I,4)+VQN(I,1,4)*DBAD(1)+VQN(I,2,4)*DBAD(2)
     1                    +VQN(I,3,4)*DBAD(3)
     2                    -DB(I,1,4)*AR(I,1)-DB(I,2,4)*AR(I,2)-DB(I,3,4)*AR(I,3)
C
                    C1 = TWO*ALR(3)
                    V13(I,1)= V13(I,1)+C1*Y13(I)
                    V24(I,1)= V24(I,1)+C1*Y24(I)
                    VHI(I,1)= VHI(I,1)+FOUR*(ALR(3)*MY13(I)-Z1(I)*ALR(2))
                    V13(I,2)= V13(I,2)-C1*X13(I)
                    V24(I,2)= V24(I,2)-C1*X24(I)
                    VHI(I,2)= VHI(I,2)-FOUR*(ALR(3)*MX13(I)-Z1(I)*ALR(1))
                    V13(I,3)= V13(I,3)-TWO*(Y13(I)*ALR(1)-X13(I)*ALR(2))
                    V24(I,3)= V24(I,3)-TWO*(Y24(I)*ALR(1)-X24(I)*ALR(2))
                    VHI(I,3)= VHI(I,3)+FOUR*(MX13(I)*ALR(2)-MY13(I)*ALR(1))
                    RLXYZ(I,1,1)= RRXYZ(I,1,1)-ALR(1)-VQN(I,1,1)*ALD(1)
                    RLXYZ(I,1,2)= RRXYZ(I,1,2)-ALR(1)-VQN(I,1,2)*ALD(2)
                    RLXYZ(I,1,3)= RRXYZ(I,1,3)-ALR(1)-VQN(I,1,3)*ALD(3)
                    RLXYZ(I,1,4)= RRXYZ(I,1,4)-ALR(1)-VQN(I,1,4)*ALD(4)
C
                    RLXYZ(I,2,1)= RRXYZ(I,2,1)-ALR(2)-VQN(I,2,1)*ALD(1)
                    RLXYZ(I,2,2)= RRXYZ(I,2,2)-ALR(2)-VQN(I,2,2)*ALD(2)
                    RLXYZ(I,2,3)= RRXYZ(I,2,3)-ALR(2)-VQN(I,2,3)*ALD(3)
                    RLXYZ(I,2,4)= RRXYZ(I,2,4)-ALR(2)-VQN(I,2,4)*ALD(4)
                ENDIF
            ENDDO
            IF (IDRIL>0) THEN
                DO I=1,NEL
                    IF(.NOT.PLAT(I)) THEN
                        D(I,1)= YY(I)+ZZ(I)+FOUR
                        D(I,2)= XX(I)+ZZ(I)+FOUR
                        D(I,3)= XX(I)+YY(I)+FOUR
                        D(I,4)= -XY(I)
                        D(I,5)= -XZ(I)
                        D(I,6)= -YZ(I)
                    ENDIF
                ENDDO
                IF(IRESP == 1)THEN
                    CALL A3INVDP_V(D,DIZ1,NEL,PLAT)
                    DO I=1,NEL
                        IF(.NOT.PLAT(I)) THEN
                            DIZ(I,3) = DIZ1(I,3)
                            DIZ(I,1) = DIZ1(I,5)
                            DIZ(I,2) = DIZ1(I,6)
                        ENDIF
                    ENDDO
                ELSE
                    DO I=1,NEL
                        IF(.NOT.PLAT(I)) THEN
                            ABC = D(I,1)*D(I,2)*D(I,3)
                            XXYZ2 = D(I,1)*D(I,6)*D(I,6)
                            YYXZ2 = D(I,2)*D(I,5)*D(I,5)
                            ZZXY2 = D(I,3)*D(I,4)*D(I,4)
                            DETA = ABS(ABC+TWO*D(I,4)*D(I,5)*D(I,6)-XXYZ2-YYXZ2-ZZXY2)
                            DETA = ONE/MAX(DETA,EM20)
                            DIZ(I,3) = (ABC-ZZXY2)*DETA/MAX(D(I,3),EM20)
                            DIZ(I,1) = (D(I,6)*D(I,4)-D(I,5)*D(I,2))*DETA
                            DIZ(I,2) = (D(I,4)*D(I,5)-D(I,6)*D(I,1))*DETA
                        ENDIF
                    ENDDO
                END IF !(IRESP == 1)THEN
C
                DO I=1,NEL
                    IF(.NOT.PLAT(I)) THEN
                        ALRZ=AREA_I(I)*(DIZ(I,1)*AR(I,1)+DIZ(I,2)*AR(I,2)+DIZ(I,3)*AR(I,3))
                        RLZ(I,1)=RLZ(I,1)-ALRZ
                        RLZ(I,2)=RLZ(I,2)-ALRZ
                        RLZ(I,3)=RLZ(I,3)-ALRZ
                        RLZ(I,4)=RLZ(I,4)-ALRZ
                    ENDIF
                ENDDO
            END IF !IF (IDRIL>0) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
       END IF !((IMPL_S>0.AND.IKPROJ<0).OR.IDRIL>0) THEN

        RETURN
        END SUBROUTINE CZCORP5
