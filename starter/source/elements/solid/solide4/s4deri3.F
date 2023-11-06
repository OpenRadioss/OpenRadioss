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
CCgw|============================================================
Chd|====================================================================
Chd|  S4DERI3                       source/elements/solid/solide4/s4deri3.F
Chd|-- called by -----------
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        MULTIFLUID_INIT3T             source/multifluid/multifluid_init3t.F
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|        S4REFSTA3                     source/elements/solid/solide4/s4refsta3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S4DERI3(VOL   ,VEUL  ,GEO   ,IGEO   ,RX     ,
     .                   RY    ,RZ    ,SX    ,SY     ,SZ     ,
     .                   TX    ,TY    ,TZ    ,
     .                   X1    ,X2    ,X3    ,X4     ,Y1     ,Y2     ,
     .                   Y3    ,Y4    ,Z1    ,Z2     ,Z3     ,Z4     ,
     .                   PX1   ,PX2   ,PX3   ,PX4    ,
     .                   PY1   ,PY2   ,PY3   ,PY4    ,
     .                   PZ1   ,PZ2   ,PZ3   ,PZ4    ,JAC_I,
     .                   DELTAX,DET   ,NGL   ,NGEO   ,MXT  ,
     .                   PM    ,VOLDP )
        USE MESSAGE_MOD
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
#include      "scr03_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IGEO(NPROPGI,*), MXT(MVSIZ)
        DOUBLE PRECISION
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),VOLDP(*)
C     REAL
        my_real
     .     VOL(*), VEUL(LVEUL,*),GEO(NPROPG,*), PM(NPROPM,*),
     .     RX(*), RY(*), RZ(*), SX(*), SY(*), SZ(*), TX(*), TY(*), TZ(*), DET(*),
     .     PX1(MVSIZ), PX2(MVSIZ), PX3(MVSIZ), PX4(MVSIZ),
     .     PY1(MVSIZ), PY2(MVSIZ), PY3(MVSIZ), PY4(MVSIZ),
     .     PZ1(MVSIZ), PZ2(MVSIZ), PZ3(MVSIZ), PZ4(MVSIZ),
     .     DELTAX(MVSIZ),JAC_I(10,MVSIZ)
        INTEGER NGL(*), NGEO(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I
        my_real
     .     B1(MVSIZ), B2(MVSIZ), B3(MVSIZ), B4(MVSIZ),
     .     C1(MVSIZ), C2(MVSIZ), C3(MVSIZ), C4(MVSIZ),
     .     D1(MVSIZ), D2(MVSIZ), D3(MVSIZ), D4(MVSIZ)
        DOUBLE PRECISION
     .     X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43,B1DP,C1DP,D1DP
        my_real
     .     D ,PXX, PYY, PZZ, PXY, PYZ, PXZ, GFAC, AA, BB, P, LD
C=======================================================================
        DO I=LFT,LLT
          X43 = X4(I) - X3(I)
          Y43 = Y4(I) - Y3(I)
          Z43 = Z4(I) - Z3(I)
          X41 = X4(I) - X1(I)
          Y41 = Y4(I) - Y1(I)
          Z41 = Z4(I) - Z1(I)
          X42 = X4(I) - X2(I)
          Y42 = Y4(I) - Y2(I)
          Z42 = Z4(I) - Z2(I)
          RX(I) = -X41
          RY(I) = -Y41
          RZ(I) = -Z41
          SX(I) = -X42
          SY(I) = -Y42
          SZ(I) = -Z42
          TX(I) = -X43
          TY(I) = -Y43
          TZ(I) = -Z43
C
          B1DP  =  Y43*Z42 - Y42*Z43
          B1(I) =  B1DP
          B2(I) =  Y41*Z43 - Y43*Z41
          B3(I) =  Y42*Z41 - Y41*Z42
          B4(I) =  -(B1(I) + B2(I) + B3(I))
C
          C1DP  =  Z43*X42 - Z42*X43
          C1(I) =  C1DP
          C2(I) =  Z41*X43 - Z43*X41
          C3(I) =  Z42*X41 - Z41*X42
          C4(I) =  -(C1(I) + C2(I) + C3(I))
C
          D1DP  =  X43*Y42 - X42*Y43
          D1(I) =  D1DP
          D2(I) =  X41*Y43 - X43*Y41
          D3(I) =  X42*Y41 - X41*Y42
          D4(I) =  -(D1(I) + D2(I) + D3(I))
C
          VOLDP(I) = (X41*B1DP + Y41*C1DP + Z41*D1DP)*ONE_OVER_6
          DET(I) = VOLDP(I)
          VOL(I) = DET(I)
        ENDDO
C
        DO I=LFT,LLT
          IF (DET(I) <= ZERO) THEN
            IF (IGEO(11,NGEO(I)) /= 0) THEN
              CALL ANCMSG(MSGID=245,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=NGL(I))
              VOL(I) = EM20    ! to prevent crash
              DET(I) = EM20
            ELSE
              CALL ANCMSG(MSGID=635,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO,
     .                    I1=NGL(I))
            ENDIF
          ENDIF
        ENDDO
C
        DO I=LFT,LLT
          D = ONE/DET(I)/SIX
          PX1(I)=-B1(I)*D
          PY1(I)=-C1(I)*D
          PZ1(I)=-D1(I)*D
          PX2(I)=-B2(I)*D
          PY2(I)=-C2(I)*D
          PZ2(I)=-D2(I)*D
          PX3(I)=-B3(I)*D
          PY3(I)=-C3(I)*D
          PZ3(I)=-D3(I)*D
          PX4(I)=-B4(I)*D
          PY4(I)=-C4(I)*D
          PZ4(I)=-D4(I)*D
        ENDDO


        IF(IDT1SOL==0)THEN

          DO I=LFT,LLT
            D = MAX(PX1(I)*PX1(I)+PY1(I)*PY1(I)+PZ1(I)*PZ1(I),
     .       PX2(I)*PX2(I)+PY2(I)*PY2(I)+PZ2(I)*PZ2(I),
     .      PX3(I)*PX3(I)+PY3(I)*PY3(I)+PZ3(I)*PZ3(I),
     .      PX4(I)*PX4(I)+PY4(I)*PY4(I)+PZ4(I)*PZ4(I))
            DELTAX(I) = ONE / SQRT(D)
          END DO

        ELSEIF(IFORMDT==0)THEN
          DO I=LFT,LLT
            D = PX1(I)*PX1(I)+PY1(I)*PY1(I)+PZ1(I)*PZ1(I)
     .        + PX2(I)*PX2(I)+PY2(I)*PY2(I)+PZ2(I)*PZ2(I)
     .        + PX3(I)*PX3(I)+PY3(I)*PY3(I)+PZ3(I)*PZ3(I)
     .        + PX4(I)*PX4(I)+PY4(I)*PY4(I)+PZ4(I)*PZ4(I)
            DELTAX(I) = ONE / SQRT(D)
          END DO

        ELSEIF(IFORMDT==1)THEN

          GFAC=PM(105,MXT(1))
          LD  =TWO*SQRT(MAX(ONE-GFAC,ZERO))+ONE
          DO I=LFT,LLT
            PXX=PX1(I)*PX1(I)+PX2(I)*PX2(I)+PX3(I)*PX3(I)+PX4(I)*PX4(I)
            PYY=PY1(I)*PY1(I)+PY2(I)*PY2(I)+PY3(I)*PY3(I)+PY4(I)*PY4(I)
            PZZ=PZ1(I)*PZ1(I)+PZ2(I)*PZ2(I)+PZ3(I)*PZ3(I)+PZ4(I)*PZ4(I)
            PXY=PX1(I)*PY1(I)+PX2(I)*PY2(I)+PX3(I)*PY3(I)+PX4(I)*PY4(I)
            PXZ=PX1(I)*PZ1(I)+PX2(I)*PZ2(I)+PX3(I)*PZ3(I)+PX4(I)*PZ4(I)
            PYZ=PY1(I)*PZ1(I)+PY2(I)*PZ2(I)+PY3(I)*PZ3(I)+PY4(I)*PZ4(I)
C
            AA = -(PXX+PYY+PZZ)
            BB =  (PXX*PYY+PXX*PZZ+PYY*PZZ-PXY**2-PXZ**2-PYZ**2)
            P  = BB-THIRD*AA*AA
            D  = TWO*SQRT(THIRD*MAX(-P,ZERO))-THIRD*AA
C
            D = LD*D
C
            DELTAX(I) = ONE / SQRT(D)
          END DO

        ELSEIF(IFORMDT==2)THEN

          GFAC=PM(105,MXT(1))
          DO I=LFT,LLT
            PXX=PX1(I)*PX1(I)+PX2(I)*PX2(I)+PX3(I)*PX3(I)+PX4(I)*PX4(I)
            PYY=PY1(I)*PY1(I)+PY2(I)*PY2(I)+PY3(I)*PY3(I)+PY4(I)*PY4(I)
            PZZ=PZ1(I)*PZ1(I)+PZ2(I)*PZ2(I)+PZ3(I)*PZ3(I)+PZ4(I)*PZ4(I)
            PXY=PX1(I)*PY1(I)+PX2(I)*PY2(I)+PX3(I)*PY3(I)+PX4(I)*PY4(I)
            PXZ=PX1(I)*PZ1(I)+PX2(I)*PZ2(I)+PX3(I)*PZ3(I)+PX4(I)*PZ4(I)
            PYZ=PY1(I)*PZ1(I)+PY2(I)*PZ2(I)+PY3(I)*PZ3(I)+PY4(I)*PZ4(I)
C
            AA = -(PXX+PYY+PZZ)
            BB =  GFAC*(PXX*PYY+PXX*PZZ+PYY*PZZ-PXY**2-PXZ**2-PYZ**2)
            P  = BB-THIRD*AA*AA
            D  = TWO*SQRT(THIRD*MAX(-P,ZERO))-THIRD*AA
C
            DELTAX(I) = ONE / SQRT(D)
          END DO

        END IF

        IF (ISMSTR==10.OR.ISMSTR==12) THEN
          DO I=LFT,LLT
            JAC_I(1,I) = PX1(I)
            JAC_I(2,I) = PX2(I)
            JAC_I(3,I) = PX3(I)
            JAC_I(4,I) = PY1(I)
            JAC_I(5,I) = PY2(I)
            JAC_I(6,I) = PY3(I)
            JAC_I(7,I) = PZ1(I)
            JAC_I(8,I) = PZ2(I)
            JAC_I(9,I) = PZ3(I)
            JAC_I(10,I) = VOL(I)
          ENDDO
        END IF
C
        IF (JEUL * (1 - IMULTI_FVM) /= 0 .AND. NXREF==0) THEN
          DO I=LFT,LLT
            VEUL(1,I) =-PX1(I)
            VEUL(2,I) = PX2(I)
            VEUL(3,I) =-PX3(I)
            VEUL(4,I) = PX4(I)
            VEUL(5,I) =-PY1(I)
            VEUL(6,I) = PY2(I)
            VEUL(7,I) =-PY3(I)
            VEUL(8,I) = PY4(I)
            VEUL(9,I) =-PZ1(I)
            VEUL(10,I)= PZ2(I)
            VEUL(11,I)=-PZ3(I)
            VEUL(12,I)= PZ4(I)
            VEUL(13,I)= DELTAX(I)
C------------------------------------------
C     CALCUL DE LA NORMALE A CHAQUE FACE
C------------------------------------------
            VEUL(14,I) = ZERO
            VEUL(15,I) = HALF*B1(I)
            VEUL(16,I) = ZERO
            VEUL(17,I) = HALF*B2(I)
            VEUL(18,I) = HALF*B3(I)
            VEUL(19,I) = HALF*B4(I)
C
            VEUL(20,I) = ZERO
            VEUL(21,I) = HALF*C1(I)
            VEUL(22,I) = ZERO
            VEUL(23,I) = HALF*C2(I)
            VEUL(24,I) = HALF*C3(I)
            VEUL(25,I) = HALF*C4(I)
C
            VEUL(26,I) = ZERO
            VEUL(27,I) = HALF*D1(I)
            VEUL(28,I) = ZERO
            VEUL(29,I) = HALF*D2(I)
            VEUL(30,I) = HALF*D3(I)
            VEUL(31,I) = HALF*D4(I)
          ENDDO
C
          IF (IGEO(11,NGEO(LFT)) == 15) THEN
            DO I=LFT,LLT
              VOL(I)=VOL(I)*GEO(1,NGEO(I))
              VOLDP(I)=VOLDP(I)*GEO(1,NGEO(I))
            ENDDO
          ENDIF
        ENDIF
C-----------
        RETURN
      END
