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
Chd|  CDKCOOR3                      source/elements/sh3n/coquedk/cdkcoor3.F
Chd|-- called by -----------
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        CORTDIR3                      source/elements/shell/coque/cortdir3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CDKCOOR3(ELBUF_STR,
     .                    JFT,JLT,MAT,PID,NGL,X,V,R,IXTG,OFFG,OFF,
     .                    R11,R12,R13,R21,R22,R23,R31,R32,R33,
     .                    XL2,YL2,XL3,YL3,SMSTR,
     .                    AREA,AREA2,CDET,VLX,VLY,VLZ,RLX,RLY,
     .                    ISMSTR,IREP,NLAY,DIR_A,DIR_B,
     .                    F11,F12,F13,F21,F22,F23,F32,F33,
     .                    M11,M12,M13,M21,M22,M23,NEL)
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
#include      "com08_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER JFT, JLT,ISMSTR,IREP,NLAY,NEL
        INTEGER IXTG(NIXTG,*),MAT(*),PID(*),NGL(*)
        my_real
     .     X(3,*),V(3,*),R(3,*), OFFG(*), OFF(*),
     .     R11(*),R12(*),R13(*),R21(*),R22(*),R23(*),
     .     R31(*),R32(*),R33(*),AREA(*),AREA2(*),CDET(*),
     .     VLX(MVSIZ,2),VLY(MVSIZ,2),VLZ(MVSIZ,2),RLX(MVSIZ,3),RLY(MVSIZ,3),
     .     XL2(*),XL3(*),YL2(*),YL3(*),
     .     F11(*), F12(*), F13(*),
     .     F21(*), F22(*), F23(*), F32(*), F33(*),
     .     M11(*), M12(*), M13(*),
     .     M21(*), M22(*), M23(*),
     .     DIR_A(NEL,*),DIR_B(NEL,*)
        DOUBLE PRECISION
     .    SMSTR(*)
        TYPE(ELBUF_STRUCT_) :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NC1, NC2, NC3,I, J,I1, I2, I3, N, NLYMAX,II(4),IBID,MAT_1
        my_real
     .     VX2(MVSIZ), VX3(MVSIZ),VY2(MVSIZ), VY3(MVSIZ),
     .     VZ2(MVSIZ), VZ3(MVSIZ),
     .     RX1(MVSIZ), RX2(MVSIZ), RX3(MVSIZ), RY1(MVSIZ),
     .     RY2(MVSIZ), RY3(MVSIZ), RZ1(MVSIZ), RZ2(MVSIZ),RZ3(MVSIZ),
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), Y1(MVSIZ),
     .     Y2(MVSIZ), Y3(MVSIZ), Z1(MVSIZ), Z2(MVSIZ),
     .     Z3(MVSIZ), RX(MVSIZ), RY(MVSIZ), RZ(MVSIZ),
     .     SX(MVSIZ), SY(MVSIZ), SZ(MVSIZ),DET(MVSIZ),
     .     VX1, VY1,VZ1,OFF_L,DT05,EXZ,EYZ,DDRX,DDRY,V21X,V31X,
     .     DDRZ1,DDRZ2
C-----------------------------------------------
        DO I=1,4
          II(I) = NEL*(I-1)
        ENDDO
C
        IBID = 0
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
C     COORDINATES
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
C----------------------------
C     VELOCITY
C----------------------------
          VX1=V(1,NC1)
          VY1=V(2,NC1)
          VZ1=V(3,NC1)
          VX2(I)=V(1,NC2)-VX1
          VY2(I)=V(2,NC2)-VY1
          VZ2(I)=V(3,NC2)-VZ1
          VX3(I)=V(1,NC3)-VX1
          VY3(I)=V(2,NC3)-VY1
          VZ3(I)=V(3,NC3)-VZ1
          RX1(I)=R(1,NC1)
          RY1(I)=R(2,NC1)
          RZ1(I)=R(3,NC1)
          RX2(I)=R(1,NC2)
          RY2(I)=R(2,NC2)
          RZ2(I)=R(3,NC2)
          RX3(I)=R(1,NC3)
          RY3(I)=R(2,NC3)
          RZ3(I)=R(3,NC3)
        ENDDO
C-----------------------------------------------
        DO I=JFT,JLT
          F12(I) =ZERO
          F13(I) =ZERO
          F22(I) =ZERO
          F23(I) =ZERO
          F32(I) =ZERO
          F33(I) =ZERO
          M11(I) =ZERO
          M12(I) =ZERO
          M13(I) =ZERO
          M21(I) =ZERO
          M22(I) =ZERO
          M23(I) =ZERO
        ENDDO
        DO I=JFT,JLT
          RX(I)=X2(I)-X1(I)
          RY(I)=Y2(I)-Y1(I)
          RZ(I)=Z2(I)-Z1(I)
          SX(I)=X3(I)-X1(I)
          SY(I)=Y3(I)-Y1(I)
          SZ(I)=Z3(I)-Z1(I)
        ENDDO
C----------------------------
C     LOCAL SYSTEM
C----------------------------
        I1 = 0
        CALL CLSKEW3(JFT,JLT,I1,
     .     RX, RY, RZ,
     .     SX, SY, SZ,
     .     R11,R12,R13,R21,R22,R23,R31,R32,R33,AREA2,OFFG)
C
        DO I=JFT,JLT
          XL2(I)=R11(I)*RX(I)+R21(I)*RY(I)+R31(I)*RZ(I)
          YL2(I)=R12(I)*RX(I)+R22(I)*RY(I)+R32(I)*RZ(I)
          XL3(I)=R11(I)*SX(I)+R21(I)*SY(I)+R31(I)*SZ(I)
          YL3(I)=R12(I)*SX(I)+R22(I)*SY(I)+R32(I)*SZ(I)
          AREA(I)=HALF*AREA2(I)
          CDET(I)=THIRD*AREA(I)
        ENDDO
        DO I=JFT,JLT
          VLX(I,1)=R11(I)*VX2(I)+R21(I)*VY2(I)+R31(I)*VZ2(I)
          VLX(I,2)=R11(I)*VX3(I)+R21(I)*VY3(I)+R31(I)*VZ3(I)
          VLY(I,1)=R12(I)*VX2(I)+R22(I)*VY2(I)+R32(I)*VZ2(I)
          VLY(I,2)=R12(I)*VX3(I)+R22(I)*VY3(I)+R32(I)*VZ3(I)
          VLZ(I,1)=R13(I)*VX2(I)+R23(I)*VY2(I)+R33(I)*VZ2(I)
          VLZ(I,2)=R13(I)*VX3(I)+R23(I)*VY3(I)+R33(I)*VZ3(I)
          RLX(I,1)=R11(I)*RX1(I)+R21(I)*RY1(I)+R31(I)*RZ1(I)
          RLX(I,2)=R11(I)*RX2(I)+R21(I)*RY2(I)+R31(I)*RZ2(I)
          RLX(I,3)=R11(I)*RX3(I)+R21(I)*RY3(I)+R31(I)*RZ3(I)
          RLY(I,1)=R12(I)*RX1(I)+R22(I)*RY1(I)+R32(I)*RZ1(I)
          RLY(I,2)=R12(I)*RX2(I)+R22(I)*RY2(I)+R32(I)*RZ2(I)
          RLY(I,3)=R12(I)*RX3(I)+R22(I)*RY3(I)+R32(I)*RZ3(I)
        ENDDO
C----------------------------
C     SMALL STRAIN
C----------------------------
        IF (ISMSTR == 1 .OR. ISMSTR == 2) THEN
          DO I=JFT,JLT
            IF (ABS(OFFG(I)) == TWO) THEN
              XL2(I)=SMSTR(II(1)+I)
              YL2(I)=SMSTR(II(2)+I)
              XL3(I)=SMSTR(II(3)+I)
              YL3(I)=SMSTR(II(4)+I)
              AREA2(I)=XL2(I)*YL3(I)-XL3(I)*YL2(I)
              AREA(I)=HALF*AREA2(I)
            ELSE
              SMSTR(II(1)+I)=XL2(I)
              SMSTR(II(2)+I)=YL2(I)
              SMSTR(II(3)+I)=XL3(I)
              SMSTR(II(4)+I)=YL3(I)
            ENDIF
          ENDDO
        ENDIF
        IF (ISMSTR ==1) THEN
          DO I=JFT,JLT
            IF (OFFG(I) == ONE) OFFG(I)=TWO
          ENDDO
        ENDIF
C----------------------------
C     ORTHOTROPY/ANISOTHROPY
C----------------------------
        CALL CORTDIR3(ELBUF_STR,DIR_A  ,DIR_B ,JFT    ,JLT    ,
     .                NLAY     ,IREP   ,RX    ,RY     ,RZ     ,
     .                SX       ,SY     ,SZ    ,R11    ,R21    ,
     .                R31      ,R12    ,R22   ,R32    ,NEL    )
C--------------------------
C-------Correction 2nd order rigid rotation due a V(t+dt/2),X(t+dt)----
C--------------------------
        DT05 = HALF*DT1
        DO I=JFT,JLT
          EXZ =  YL3(I)*VLZ(I,1)-YL2(I)*VLZ(I,2)
          EYZ = -XL3(I)*VLZ(I,1)+XL2(I)*VLZ(I,2)
          DDRY=DT05*EXZ/AREA2(I)
          DDRX=DT05*EYZ/AREA2(I)
          V21X = VLX(I,1)
          V31X = VLX(I,2)
          DDRZ1=DT05*VLY(I,1)/XL2(I)
          DDRZ2=DT05*V31X/YL3(I)
          VLX(I,1) = VLX(I,1)-DDRY*VLZ(I,1)-DDRZ1*VLY(I,1)
          VLX(I,2) = VLX(I,2)-DDRY*VLZ(I,2)-DDRZ1*VLY(I,2)
          VLY(I,1) = VLY(I,1)-DDRX*VLZ(I,1)-DDRZ2*V21X
          VLY(I,2) = VLY(I,2)-DDRX*VLZ(I,2)-DDRZ2*V31X
        ENDDO
C----------------------------
C     OFF
C----------------------------
        OFF_L = ZERO
        DO I=JFT,JLT
          OFF(I) = MIN(ONE,ABS(OFFG(I)))
          OFF_L  = MIN(OFF_L,OFFG(I))
        ENDDO
        IF(OFF_L < ZERO)THEN
          DO I=JFT,JLT
            IF(OFFG(I) < ZERO)THEN
              VLX(I,1)=ZERO
              VLX(I,2)=ZERO
              VLY(I,1)=ZERO
              VLY(I,2)=ZERO
              VLZ(I,1)=ZERO
              VLZ(I,2)=ZERO
              RLX(I,1)=ZERO
              RLX(I,2)=ZERO
              RLX(I,3)=ZERO
              RLY(I,1)=ZERO
              RLY(I,2)=ZERO
              RLY(I,3)=ZERO
            ENDIF
          ENDDO
        ENDIF
C----------------------------
        RETURN
      END
c
Chd|====================================================================
Chd|  CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|-- called by -----------
Chd|        C3COORK3                      source/elements/sh3n/coque3n/c3coork3.F
Chd|        C3EVEC3                       source/elements/sh3n/coque3n/c3evec3.F
Chd|        CBACOOR                       source/elements/shell/coqueba/cbacoor.F
Chd|        CBACOORK                      source/elements/shell/coqueba/cbacoork.F
Chd|        CBACOORT                      source/elements/shell/coqueba/cbacoor.F
Chd|        CDK6COOR3                     source/elements/sh3n/coquedk6/cdk6coor3.F
Chd|        CDKCOOR3                      source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        CZCOORK3                      source/elements/shell/coquez/czcoork3.F
Chd|        CZCORC1                       source/elements/shell/coquez/czcorc.F
Chd|        CZCORCHT                      source/elements/shell/coquez/czcorc.F
Chd|        CZCORCT                       source/elements/shell/coquez/czcorc.F
Chd|        ENRICHC_INI                   source/elements/xfem/enrichc_ini.F
Chd|        ENRICHTG_INI                  source/elements/xfem/enrichtg_ini.F
Chd|        GET_Q4L                       source/output/sta/stat_c_strafg.F
Chd|        GET_Q4LSYS                    source/output/sta/sta_c_get_q4lsys.F
Chd|        GET_T3L                       source/output/sta/stat_c_strafg.F
Chd|        GET_T3LSYS                    source/output/sta/sta_c_get_t3lsys.F
Chd|        SCOOR43                       source/elements/solid/sconnect/scoor43.F
Chd|        SHELL_LOCAL_FRAME             source/output/dynain/shell_rota.F
Chd|        SHELL_LOC_COR                 source/tools/seatbelts/shell_loc_cor.F
Chd|        SHLROTG                       source/output/anim/generate/tensor6.F
Chd|        TENS3DTO2D                    source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CLSKEW3(JFT,JLT,IREP,
     .   RX, RY, RZ, SX, SY, SZ,
     .   E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z,
     .   DET,OFF )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER JFT,JLT,IREP
        my_real
     .     RX(*) , RY(*) , RZ(*),
     .     SX(*) , SY(*) , SZ(*),
     .     E1X(*), E1Y(*), E1Z(*),
     .     E2X(*), E2Y(*), E2Z(*),
     .     E3X(*), E3Y(*), E3Z(*), DET(*), OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I
        MY_REAL C1,C2,CC,C1C1,C2C2,C1_1(MVSIZ),C2_1(MVSIZ)
        my_real :: OFF_LOC
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C IREP=0 ->QEPH IREP=1 ->Q4, IREP=2-> E1=R(ISHFRAM=1)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        DO I=JFT,JLT
C---------E3------------
          E3X(I) = RY(I) * SZ(I) - RZ(I) * SY(I)
          E3Y(I) = RZ(I) * SX(I) - RX(I) * SZ(I)
          E3Z(I) = RX(I) * SY(I) - RY(I) * SX(I)
          DET(I) = SQRT(E3X(I)*E3X(I) + E3Y(I)*E3Y(I) + E3Z(I)*E3Z(I))
          IF (DET(I) < EM20 .AND. OFF(I) /= ZERO) THEN
            OFF(I)=ZERO
            IDEL7NOK = 1
          ENDIF
          OFF_LOC = ZERO
          IF(ABS(OFF(I))/=ZERO) OFF_LOC = ONE
          DET(I)=MAX(EM20,DET(I))
          CC = OFF_LOC/DET(I)
          CC = MAX(CC,EM20)
          E3X(I) = E3X(I) * CC
          E3Y(I) = E3Y(I) * CC
          E3Z(I) = E3Z(I) * CC
        ENDDO
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        IF (IREP==2) THEN
          DO I=JFT,JLT
            E1X(I) = RX(I)
            E1Y(I) = RY(I)
            E1Z(I) = RZ(I)
          ENDDO
        ELSEIF (IREP==1) THEN
          DO I=JFT,JLT
            C2 = SQRT(SX(I)*SX(I) + SY(I)*SY(I) + SZ(I)*SZ(I))
            E1X(I) = RX(I)*C2+(SY(I)*E3Z(I)-SZ(I)*E3Y(I))
            E1Y(I) = RY(I)*C2+(SZ(I)*E3X(I)-SX(I)*E3Z(I))
            E1Z(I) = RZ(I)*C2+(SX(I)*E3Y(I)-SY(I)*E3X(I))
          ENDDO
        ELSE
          DO I=JFT,JLT
            C1C1 = RX(I)*RX(I) + RY(I)*RY(I) + RZ(I)*RZ(I)
            C2C2 = SX(I)*SX(I) + SY(I)*SY(I) + SZ(I)*SZ(I)
            IF(C1C1 /= ZERO) THEN
              C2_1(I) = SQRT(C2C2/MAX(EM20,C1C1))
              C1_1(I) = ONE
            ELSEIF(C2C2 /= ZERO)THEN
              C2_1(I) = ONE
              C1_1(I) = SQRT(C1C1/MAX(EM20,C2C2))
            END IF
          ENDDO
          DO I=JFT,JLT
            E1X(I) = RX(I)*C2_1(I)+(SY(I)*E3Z(I)-SZ(I)*E3Y(I))*C1_1(I)
            E1Y(I) = RY(I)*C2_1(I)+(SZ(I)*E3X(I)-SX(I)*E3Z(I))*C1_1(I)
            E1Z(I) = RZ(I)*C2_1(I)+(SX(I)*E3Y(I)-SY(I)*E3X(I))*C1_1(I)
          ENDDO
        ENDIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        DO I=JFT,JLT
          C1 = SQRT(E1X(I)*E1X(I) + E1Y(I)*E1Y(I) + E1Z(I)*E1Z(I))
          IF(C1 /= ZERO) C1 = ONE / MAX(EM20,C1)
          E1X(I) = E1X(I)*C1
          E1Y(I) = E1Y(I)*C1
          E1Z(I) = E1Z(I)*C1
          E2X(I) = E3Y(I) * E1Z(I) - E3Z(I) * E1Y(I)
          E2Y(I) = E3Z(I) * E1X(I) - E3X(I) * E1Z(I)
          E2Z(I) = E3X(I) * E1Y(I) - E3Y(I) * E1X(I)
        ENDDO
c-----------
        RETURN
      END
