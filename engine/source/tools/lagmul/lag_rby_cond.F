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
Chd|  LAG_RBY_COND                  source/tools/lagmul/lag_rby_cond.F
Chd|-- called by -----------
Chd|        LAG_RBY                       source/tools/lagmul/lag_rby.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LAG_RBY_COND(NPBYL  ,LPBYL ,RBYL   ,MASS   ,INER   ,
     2                        X      ,V     ,VR     ,A      ,AR     ,
     3                        IADLL  ,LLL   ,COMNTAG,NN     ,NC     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN, NC
      INTEGER LLL(*),IADLL(*),NPBYL(NNPBY,*),LPBYL(*),COMNTAG(*)
C     REAL
      my_real
     .  RBYL(NRBY,*),X(3,*),V(3,*),VR(3,*),A(3,*),AR(3,*),
     .  MASS(*),INER(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IC,IK,N,NS,MSL,MSL2,M,IFX,IFR,NFIX,NFRE
      my_real XX,YY,ZZ,XY,YZ,XZ,IXX,IYY,IZZ,IXY,IXZ,IYZ,
     .        JXX,JYY,JZZ,JXY,JXZ,JYZ,JXY2,JXZ2,JYZ2,DET,
     .        B1,B2,B3,C1,C2,C3,VX1,VX2,VX3,WA1,WA2,WA3,MMAS,USDT,DDT,
     .        VI(3),VG(3),AG(3),XM(3),VTM(3),VRM(3),ATM(3),ARM(3),
     .        XROT(9),IPR(3)
C======================================================================|
      MSL = NPBYL(2,NN)
      MSL2= MSL*2 
      NFIX= 0
      NFRE= 0
      IFX = 0
      IFR = 0
C---  main
      NS = LPBYL(MSL)
      IF (COMNTAG(NS)==1) THEN
        NFIX = NFIX + 1
        LPBYL(MSL+NFIX)  = NS
      ELSE
        NFRE = NFRE + 1
        LPBYL(MSL2+NFRE) = NS
      ENDIF
C---  and secnds
      DO N=1,MSL-1
        NS = LPBYL(N)
        IF (COMNTAG(NS)==1) THEN
          NFIX = NFIX + 1
          LPBYL(MSL+NFIX)  = NS
        ELSE
          NFRE = NFRE + 1
          LPBYL(MSL2+NFRE) = NS
        ENDIF
      ENDDO
      IF (NFIX<=1) NFIX = 0
C--------------------------------------
      IF (NFIX/=0) THEN
        IFX  = LPBYL(MSL+1)
        JXX  = ZERO
        JYY  = ZERO
        JZZ  = ZERO
        JXY  = ZERO
        JYZ  = ZERO
        JXZ  = ZERO
        MMAS = ZERO
        DO J=1,3
          XM (J) = ZERO
          VG (J) = ZERO
          AG (J) = ZERO
          VTM(J) = ZERO
          ATM(J) = ZERO
          VRM(J) = ZERO
          ARM(J) = ZERO
        ENDDO
C---    CONDENSATION: CDG, masse, transl velocity, accel
        DO I=1,NFIX
          N = LPBYL(MSL+I)
          MMAS = MMAS + MASS(N)
          DO J=1,3
            XM (J)= XM(J) + X(J,N)*MASS(N)
            VTM(J)=VTM(J) + V(J,N)*MASS(N)
            ATM(J)=ATM(J) + A(J,N)*MASS(N)
          ENDDO
        ENDDO
        DO J=1,3
          XM(J)  =  XM(J) / MMAS
          VTM(J) = VTM(J) / MMAS
          ATM(J) = ATM(J) / MMAS
        ENDDO
        DO I=1,NFIX
          N = LPBYL(MSL+I)
          XX = X(1,N) - XM(1)
          YY = X(2,N) - XM(2)
          ZZ = X(3,N) - XM(3)
c         
c          VG(1)=VG(1) + VR(1,N)*INER(N)+MASS(N)*(YY*V(3,N)-ZZ*V(2,N))
c          VG(2)=VG(2) + VR(2,N)*INER(N)+MASS(N)*(ZZ*V(1,N)-XX*V(3,N))
c          VG(3)=VG(3) + VR(3,N)*INER(N)+MASS(N)*(XX*V(2,N)-YY*V(1,N))
c
c         sum of moments
          AG(1)=AG(1) + AR(1,N)*INER(N)+MASS(N)*(YY*A(3,N)-ZZ*A(2,N))
          AG(2)=AG(2) + AR(2,N)*INER(N)+MASS(N)*(ZZ*A(1,N)-XX*A(3,N))
          AG(3)=AG(3) + AR(3,N)*INER(N)+MASS(N)*(XX*A(2,N)-YY*A(1,N))
c---
          XY=XX*YY
          YZ=YY*ZZ
          XZ=XX*ZZ
c          
          XX=XX*XX
          YY=YY*YY
          ZZ=ZZ*ZZ
          IXX = INER(N)+(YY+ZZ)*MASS(N)
          IYY = INER(N)+(XX+ZZ)*MASS(N)
          IZZ = INER(N)+(XX+YY)*MASS(N)
          IXY = XY*MASS(N)
          IYZ = YZ*MASS(N)
          IXZ = XZ*MASS(N)
          JXX = JXX + IXX
          JYY = JYY + IYY
          JZZ = JZZ + IZZ
          JXY = JXY - IXY
          JYZ = JYZ - IYZ
          JXZ = JXZ - IXZ
        ENDDO
C-----------------------------        
        JXY2 = JXY*JXY
        JYZ2 = JYZ*JYZ
        JXZ2 = JXZ*JXZ
        DET  = JXX*JYY*JZZ-JXX*JYZ2-JYY*JXZ2-JZZ*JXY2-TWO*JXY*JYZ*JXZ
        DET  = ONE / DET
        B1   = DET*(JZZ*JYY-JYZ2)
        B2   = DET*(JXX*JZZ-JXZ2)
        B3   = DET*(JYY*JXX-JXY2)
        C1   = DET*(JXX*JYZ+JXZ*JXY)
        C2   = DET*(JYY*JXZ+JXY*JYZ)
        C3   = DET*(JZZ*JXY+JYZ*JXZ)
C-----------------------------                
        VRM(1) = VR(1,IFX)
        VRM(2) = VR(2,IFX)
        VRM(3) = VR(3,IFX)
C
        VG(1)  = VRM(1)*JXX + VRM(2)*JXY + VRM(3)*JXZ
        VG(2)  = VRM(1)*JXY + VRM(2)*JYY + VRM(3)*JYZ
        VG(3)  = VRM(1)*JXZ + VRM(2)*JYZ + VRM(3)*JZZ
C
c        VRM(1)= VG(1)*B1 + VG(2)*C3 + VG(3)*C2
c        VRM(2)= VG(1)*C3 + VG(2)*B2 + VG(3)*C1
c        VRM(3)= VG(1)*C2 + VG(2)*C1 + VG(3)*B3
        
C
        AG(1) = AG(1) - VRM(2)*VG(3) + VRM(3)*VG(2)
        AG(2) = AG(2) - VRM(3)*VG(1) + VRM(1)*VG(3)
        AG(3) = AG(3) - VRM(1)*VG(2) + VRM(2)*VG(1)
C
        ARM(1)= AG(1)*B1 + AG(2)*C3 + AG(3)*C2
        ARM(2)= AG(1)*C3 + AG(2)*B2 + AG(3)*C1
        ARM(3)= AG(1)*C2 + AG(2)*C1 + AG(3)*B3        
C-----------------------------        
        IF (NFRE==0) THEN
C---      Total condensation => direct solution + decondensation
          USDT = ONE / DT12
          DDT  = HALF * DT12
          DO J=1,3
            VRM(J) = VRM(J) + ARM(J)*DT12
          ENDDO
          DO N=1,MSL
            NS = LPBYL(N)
            DO J=1,3
              AR(J,NS) = (VRM(J)-VR(J,NS)) * USDT
            ENDDO
            XX  = X(1,NS)-XM(1)
            YY  = X(2,NS)-XM(2)
            ZZ  = X(3,NS)-XM(3)
            VX1 = VRM(2)*ZZ - VRM(3)*YY
            VX2 = VRM(3)*XX - VRM(1)*ZZ
            VX3 = VRM(1)*YY - VRM(2)*XX
            A(1,NS) = ATM(1) + USDT*
     .                (VTM(1)-V(1,NS)+VX1+DDT*(VRM(2)*VX3-VRM(3)*VX2))
            A(2,NS) = ATM(2) + USDT* 
     .                (VTM(2)-V(2,NS)+VX2+DDT*(VRM(3)*VX1-VRM(1)*VX3))
            A(3,NS) = ATM(3) + USDT*
     .                (VTM(3)-V(3,NS)+VX3+DDT*(VRM(1)*VX2-VRM(2)*VX1))

c            A(1,NS) = ATM(1) + USDT*(VTM(1) - V(1,NS) + VX1)
c            A(2,NS) = ATM(2) + USDT*(VTM(2) - V(2,NS) + VX2)
c            A(3,NS) = ATM(3) + USDT*(VTM(3) - V(3,NS) + VX3)
          ENDDO
        ELSE
C---      partial condensation : save condensed values for further treatment
          IFR = LPBYL(2*MSL+1)
          RBYL(10,NN) = MASS(IFX)
          RBYL(14,NN) = V (1,IFX)
          RBYL(15,NN) = V (2,IFX)
          RBYL(16,NN) = V (3,IFX)
          RBYL(17,NN) = VR(1,IFX)
          RBYL(18,NN) = VR(2,IFX)
          RBYL(19,NN) = VR(3,IFX)
          RBYL(20,NN) = A (1,IFX)
          RBYL(21,NN) = A (2,IFX)
          RBYL(22,NN) = A (3,IFX)
          RBYL(23,NN) = AR(1,IFX)
          RBYL(24,NN) = AR(2,IFX)
          RBYL(25,NN) = AR(3,IFX)
C
          RBYL(1,NN)  = B1
          RBYL(2,NN)  = B2
          RBYL(3,NN)  = B3
          RBYL(4,NN)  = C1
          RBYL(5,NN)  = C2
          RBYL(6,NN)  = C3
          RBYL(11,NN) = XM(1)
          RBYL(12,NN) = XM(2)
          RBYL(13,NN) = XM(3)
C
          MASS(IFX) = MMAS
          V (1,IFX) = VTM(1)
          V (2,IFX) = VTM(2)
          V (3,IFX) = VTM(3)
          VR(1,IFX) = VRM(1)
          VR(2,IFX) = VRM(2)
          VR(3,IFX) = VRM(3)
          A (1,IFX) = ATM(1)
          A (2,IFX) = ATM(2)
          A (3,IFX) = ATM(3)
          AR(1,IFX) = ARM(1)
          AR(2,IFX) = ARM(2)
          AR(3,IFX) = ARM(3)
        ENDIF
      ENDIF
      NPBYL(4,NN) = NFIX
      NPBYL(5,NN) = NFRE
      NPBYL(7,NN) = IFX
      NPBYL(8,NN) = IFR
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBY_DECOND                    source/tools/lagmul/lag_rby_cond.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RBY_DECOND(X      ,V     ,VR     ,A      ,AR     ,
     2                      IADLL  ,LLL   ,JLL    ,XLL    ,LAMBDA ,
     3                      MASS   ,INER  ,RBYL   ,NPBYL  ,LPBYL  ,
     4                      NC     ,NCR    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "lagmult.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "units_c.inc"


#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC, NCR
      INTEGER IADLL(*),LLL(*),JLL(*),NPBYL(NNPBY,*),LPBYL(*)
C     REAL
      my_real
     .  RBYL(NRBY,*),XLL(*),X(3,*),V(3,*),VR(3,*),A(3,*),AR(3,*),
     .  MASS(*),INER(*),LAMBDA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,JF,IC,IK,IR,IFX,IFR,N,NS,NFIX,NFRE,MSL,TNSL
      my_real 
     .  XX,YY,ZZ,VX1,VX2,VX3,USDT,DDT,XM(3),VTM(3),VRM(3),ATM(3),ARM(3)
C======================================================================|
      USDT = ONE / DT12
      DDT  = HALF *DT12 
      TNSL = 0
      IC = NCR
      DO IR = 1,NRBYLAG
        MSL  = NPBYL(2,IR)
        IFX  = NPBYL(7,IR)
        NFIX = NPBYL(4,IR)
        NFRE = NPBYL(5,IR)
        IF (NFIX>0.AND.NFRE>0) THEN
C---      calculate acceleration of condensed node: a = ao + [M]-1[L]t la
          DO K = 1,3
            IC = IC + 1
            DO IK=IADLL(IC),IADLL(IC+1)-1
              I = LLL(IK)
              J = JLL(IK)
              XLL(IK) = XLL(IK)*LAMBDA(IC)
              IF (J<=3) THEN
c                A(J,I)  = A(J,I)  - XLL(IK)/MASS(I)
                CALL ANCMSG(MSGID=117,ANMODE=ANINFO,
     .                    I1=I) 
                CALL ARRET(2)
              ELSEIF(I/=IFX) THEN
                J = J-3
                AR(J,I) = AR(J,I) - XLL(IK)/INER(I)
              ELSEIF (XLL(IK)/=0.) THEN
                IF(J==4) THEN
                  AR(1,IFX) = AR(1,IFX) - XLL(IK)*RBYL(1,IR)
                  AR(2,IFX) = AR(2,IFX) - XLL(IK)*RBYL(6,IR)
                  AR(3,IFX) = AR(3,IFX) - XLL(IK)*RBYL(5,IR)
                ELSEIF(J==5) THEN
                  AR(1,IFX) = AR(1,IFX) - XLL(IK)*RBYL(6,IR)
                  AR(2,IFX) = AR(2,IFX) - XLL(IK)*RBYL(2,IR)
                  AR(3,IFX) = AR(3,IFX) - XLL(IK)*RBYL(4,IR)
                ELSE
                  AR(1,IFX) = AR(1,IFX) - XLL(IK)*RBYL(5,IR)
                  AR(2,IFX) = AR(2,IFX) - XLL(IK)*RBYL(4,IR)
                  AR(3,IFX) = AR(3,IFX) - XLL(IK)*RBYL(3,IR)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
C---      decondensation
          MASS(IFX) = RBYL(10,IR)
          DO J=1,3
            VTM(J) = V (J,IFX)
            VRM(J) = VR(J,IFX)
            ATM(J) = A (J,IFX)
            ARM(J) = AR(J,IFX)
            XM (J)    = RBYL(10+J,IR)
            V (J,IFX) = RBYL(13+J,IR)
            VR(J,IFX) = RBYL(16+J,IR)
            A (J,IFX) = RBYL(19+J,IR)
            AR(J,IFX) = RBYL(22+J,IR)
          ENDDO
          K = TNSL+MSL
          DO J=1,3
            VRM(J) = VRM(J) + ARM(J)*DT12
          ENDDO
          DO N=1,NFIX
            NS = LPBYL(K+N)
            DO J=1,3
              AR(J,NS) = (VRM(J)-VR(J,NS)) * USDT
            ENDDO
            XX = X(1,NS) - XM(1)
            YY = X(2,NS) - XM(2)
            ZZ = X(3,NS) - XM(3)
c
            VX1 = VRM(2)*ZZ - VRM(3)*YY
            VX2 = VRM(3)*XX - VRM(1)*ZZ
            VX3 = VRM(1)*YY - VRM(2)*XX
c             pas de second ordre
c            A(1,NS) = ATM(1) + USDT*(VTM(1) + VX1 - V(1,NS))
c            A(2,NS) = ATM(2) + USDT*(VTM(2) + VX2 - V(2,NS))
c            A(3,NS) = ATM(3) + USDT*(VTM(3) + VX3 - V(3,NS))
c             calcul du second ordre
            A(1,NS) = ATM(1) + USDT*
     .                (VTM(1)-V(1,NS)+VX1+DDT*(VRM(2)*VX3-VRM(3)*VX2))
            A(2,NS) = ATM(2) + USDT*
     .                (VTM(2)-V(2,NS)+VX2+DDT*(VRM(3)*VX1-VRM(1)*VX3))
            A(3,NS) = ATM(3) + USDT*
     .                (VTM(3)-V(3,NS)+VX3+DDT*(VRM(1)*VX2-VRM(2)*VX1))
          ENDDO
        ENDIF
        TNSL = TNSL + 3*MSL
      ENDDO
C---
      RETURN
      END
