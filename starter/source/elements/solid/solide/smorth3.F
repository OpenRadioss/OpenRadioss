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
Chd|  SMORTH3                       source/elements/solid/solide/smorth3.F
Chd|-- called by -----------
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        INV3                          source/tools/univ/inv3.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SMORTH3(PID  ,GEO  ,IGEO ,SKEW ,IREP ,GAMA ,
     .       RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .       E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .       F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,NSIGI,SIGSP,NSIGS,
     .       SIGI ,IXS  ,X    ,JHBE ,PT   ,NEL  ,ISOLNOD)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JHBE,IREP,NSIGI,NSIGS,NEL,ISOLNOD
      INTEGER PID(*),IGEO(NPROPGI,*),IXS(NIXS,*),PT(*)
C     REAL
      my_real
     .   GEO(NPROPG,*),SKEW(LSKEW,*),GAMA(NEL,6),
     .   RX(*) ,RY(*) ,RZ(*) ,SX(*) ,SY(*) ,SZ(*) ,TX(*) ,TY(*) ,TZ(*),
     .   E1X(*),E1Y(*),E1Z(*),E2X(*),E2Y(*),E2Z(*),E3X(*),E3Y(*),E3Z(*),
     .   F1X(*),F1Y(*),F1Z(*),F2X(*),F2Y(*),F2Z(*),SIGSP(NSIGI,*),
     .   SIGI(NSIGS,*),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISK,IPNUM,IG,IIS,II,J,JJ,N,IFLAGINI,N1,N2,N4,NNOD,INIORTH(MVSIZ)
C     REAL
      my_real
     .   XL,YL,ZL,SUM,HX,HY,HZ,KX,KY,KZ,LX,LY,LZ,PHI,CP,SP,VX,VY,VZ,VN,
     .   F3X,F3Y,F3Z,
     .   G11,G22,G33,G12,G21,G23,G32,G13,G31,PTS(3)
      my_real
     .   SK(6),A(9),B(9)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------------------------------
C     Repere orthotrope
C     Stockage de Transpose(G) tq Xortho = Transpose(G) Xcvt
C     GAMA(1)= TG11 , GAMA(2) = TG12, TG13..., TG21..., TG22..., TG23...
C=======================================================================
C---- tag elm /w /INIBRI/ORTHO
      INIORTH(LFT:LLT)=0
      IF (NVSOLID3 /= 0) THEN
        IIS= NVSOLID1  + NVSOLID2 +  4 +NUSOLID 
        DO I=LFT,LLT
         JJ=PT(NFT+I)
         IF(JJ ==0 ) CYCLE
         IF(
     .   SIGSP(IIS+1,JJ) /= ZERO .OR. SIGSP(IIS+2,JJ) /=ZERO .OR.
     .   SIGSP(IIS+3,JJ) /= ZERO .OR. SIGSP(IIS+4,JJ) /=ZERO .OR.
     .   SIGSP(IIS+5,JJ) /= ZERO .OR. SIGSP(IIS+6,JJ) /=ZERO )THEN
             INIORTH(I) = 1
         ENDIF         
        ENDDO
      ENDIF
      DO I=LFT,LLT
        IF(INIORTH(I) ==1 ) CYCLE
        IG = PID(I)
        ID=IGEO(1,IG)
        CALL FRETITL2(TITR,
     .                IGEO(NPROPGI-LTITR+1,IG),LTITR)
        IPNUM = IGEO(2,IG)
        PHI = GEO(1,IG) * PI/HUNDRED80
        VX  = GEO(7,IG)
        VY  = GEO(8,IG)
        VZ  = GEO(9,IG)
        CP  = COS(PHI)
        SP  = SIN(PHI)
        IF (IPNUM > 20) THEN
         PTS(1:3) = GEO(33:35,IG)
        END IF
C
        IF (IPNUM < 0) THEN
          ISK = -IPNUM
          GAMA(I,1)=
     .      SKEW(1,ISK)*E1X(I)+SKEW(2,ISK)*E1Y(I)+SKEW(3,ISK)*E1Z(I)
          GAMA(I,2)=
     .      SKEW(1,ISK)*E2X(I)+SKEW(2,ISK)*E2Y(I)+SKEW(3,ISK)*E2Z(I)
          GAMA(I,3)=
     .      SKEW(1,ISK)*E3X(I)+SKEW(2,ISK)*E3Y(I)+SKEW(3,ISK)*E3Z(I)
          GAMA(I,4)=
     .      SKEW(4,ISK)*E1X(I)+SKEW(5,ISK)*E1Y(I)+SKEW(6,ISK)*E1Z(I)
          GAMA(I,5)=
     .      SKEW(4,ISK)*E2X(I)+SKEW(5,ISK)*E2Y(I)+SKEW(6,ISK)*E2Z(I)
          GAMA(I,6)=
     .      SKEW(4,ISK)*E3X(I)+SKEW(5,ISK)*E3Y(I)+SKEW(6,ISK)*E3Z(I)
        ELSEIF (IPNUM == 4) THEN
C         DIR1 =  projection vect V on bottom face
C         DIR2 =  orthogonal to DIR1, on bottom face
C         DIR3 =  orthogonal to bottom face
          F3X = F1Y(I)*F2Z(I) - F1Z(I)*F2Y(I)
          F3Y = F1Z(I)*F2X(I) - F1X(I)*F2Z(I)
          F3Z = F1X(I)*F2Y(I) - F1Y(I)*F2X(I)
          SUM = ONE / MAX(SQRT(F3X*F3X+F3Y*F3Y+F3Z*F3Z),EM20)
          F3X = F3X * SUM
          F3Y = F3Y * SUM
          F3Z = F3Z * SUM
C
          VX = GEO(7,IG)
          VY = GEO(8,IG)
          VZ = GEO(9,IG)
          VN = VX*F3X + VY*F3Y + VZ*F3Z
          VX = VX - VN*F3X
          VY = VY - VN*F3Y
          VZ = VZ - VN*F3Z
          SUM= SQRT(VX*VX+VY*VY+VZ*VZ)
          IF (SUM < EM20) THEN
            VX  = F1X(I)
            VY  = F1Y(I)
            VZ  = F1Z(I)
            SUM = ONE
          ELSE
            SUM = ONE / MAX(SQRT(VX*VX+VY*VY+VZ*VZ),EM20)
          ENDIF
C         repere orthogononalise facette
C         E3 = E1 x E2, E1' = V, E2' = E3 x E1'
          F1X(I) = VX * SUM
          F1Y(I) = VY * SUM
          F1Z(I) = VZ * SUM
          F2X(I) = F3Y*F1Z(I) - F3Z*F1Y(I)
          F2Y(I) = F3Z*F1X(I) - F3X*F1Z(I)
          F2Z(I) = F3X*F1Y(I) - F3Y*F1X(I)
C         dir ortho ds repere global
C         S1 = vect F1 + rot(phi) dans le plan de facette
C         S2 = vect F2 + rot(phi) dans le plan de facette
          SK(1) = CP*F1X(I) + SP*F2X(I)
          SK(2) = CP*F1Y(I) + SP*F2Y(I)
          SK(3) = CP*F1Z(I) + SP*F2Z(I)
          SK(4) =-SP*F1X(I) + CP*F2X(I)
          SK(5) =-SP*F1Y(I) + CP*F2Y(I)
          SK(6) =-SP*F1Z(I) + CP*F2Z(I)
C         dir ortho ds repere elementaire
          GAMA(I,1) = SK(1)*E1X(I) + SK(2)*E1Y(I) + SK(3)*E1Z(I)
          GAMA(I,2) = SK(1)*E2X(I) + SK(2)*E2Y(I) + SK(3)*E2Z(I)
          GAMA(I,3) = SK(1)*E3X(I) + SK(2)*E3Y(I) + SK(3)*E3Z(I)
          GAMA(I,4) = SK(4)*E1X(I) + SK(5)*E1Y(I) + SK(6)*E1Z(I)
          GAMA(I,5) = SK(4)*E2X(I) + SK(5)*E2Y(I) + SK(6)*E2Z(I)
          GAMA(I,6) = SK(4)*E3X(I) + SK(5)*E3Y(I) + SK(6)*E3Z(I)
        ELSEIF (IPNUM == 5) THEN
C         DIR1 =  orthogonal to bottom face
C         DIR2 =  projection vect V on bottom face
          F3X = F1Y(I)*F2Z(I) - F1Z(I)*F2Y(I)
          F3Y = F1Z(I)*F2X(I) - F1X(I)*F2Z(I)
          F3Z = F1X(I)*F2Y(I) - F1Y(I)*F2X(I)
          SUM = ONE / MAX(SQRT(F3X*F3X+F3Y*F3Y+F3Z*F3Z),EM20)
          F3X = F3X * SUM
          F3Y = F3Y * SUM
          F3Z = F3Z * SUM
C
          VX = GEO(7,IG)
          VY = GEO(8,IG)
          VZ = GEO(9,IG)
          VN = VX*F3X + VY*F3Y + VZ*F3Z
          VX = VX - VN*F3X
          VY = VY - VN*F3Y
          VZ = VZ - VN*F3Z
          SUM= SQRT(VX*VX+VY*VY+VZ*VZ)
          IF (SUM < EM20) THEN
            VX  = F1X(I)
            VY  = F1Y(I)
            VZ  = F1Z(I)
            SUM = ONE
          ELSE
            SUM= ONE / MAX(SQRT(VX*VX+VY*VY+VZ*VZ),EM20)
          ENDIF
C         repere orthogononalise facette
C         E3 = E1 x E2, E1' = V, E2' = E3 x E1'
          F1X(I) = VX * SUM
          F1Y(I) = VY * SUM
          F1Z(I) = VZ * SUM
          F2X(I) = F3Y*F1Z(I) - F3Z*F1Y(I)
          F2Y(I) = F3Z*F1X(I) - F3X*F1Z(I)
          F2Z(I) = F3X*F1Y(I) - F3Y*F1X(I)
C         dir ortho ds repere global
C         S1 = vect F1 + rot(phi) dans le plan de facette
C         S2 = vect F2 + rot(phi) dans le plan de facette
          SK(1) = F3X
          SK(2) = F3Y
          SK(3) = F3Z
          SK(4) = CP*F1X(I) + SP*F2X(I)
          SK(5) = CP*F1Y(I) + SP*F2Y(I)
          SK(6) = CP*F1Z(I) + SP*F2Z(I)
C         dir ortho ds repere elementaire
          GAMA(I,1) = SK(1)*E1X(I) + SK(2)*E1Y(I) + SK(3)*E1Z(I)
          GAMA(I,2) = SK(1)*E2X(I) + SK(2)*E2Y(I) + SK(3)*E2Z(I)
          GAMA(I,3) = SK(1)*E3X(I) + SK(2)*E3Y(I) + SK(3)*E3Z(I)
          GAMA(I,4) = SK(4)*E1X(I) + SK(5)*E1Y(I) + SK(6)*E1Z(I)
          GAMA(I,5) = SK(4)*E2X(I) + SK(5)*E2Y(I) + SK(6)*E2Z(I)
          GAMA(I,6) = SK(4)*E3X(I) + SK(5)*E3Y(I) + SK(6)*E3Z(I)
        ELSEIF (IPNUM == 20) THEN
C --   g1: 12 (SK(1:3)); g2: 14(SK(4:6))       
            II=NFT+I
            IF (ISOLNOD == 4 .OR. ISOLNOD == 10) THEN
              SK(1) = TX(I)
              SK(2) = TY(I)
              SK(3) = TZ(I)
              SK(4) = RX(I)
              SK(5) = RY(I)
              SK(6) = RZ(I)
            ELSE
              N1=IXS(2,II)
              N2=IXS(3,II)
              N4=IXS(5,II)
              SK(1) = X(1,N2)-X(1,N1)
              SK(2) = X(2,N2)-X(2,N1)
              SK(3) = X(3,N2)-X(3,N1)
              SK(4) = X(1,N4)-X(1,N1)
              SK(5) = X(2,N4)-X(2,N1)
              SK(6) = X(3,N4)-X(3,N1)
            END IF
            SUM = ONE / MAX(SQRT(SK(1)*SK(1)+SK(2)*SK(2)+SK(3)*SK(3)),EM20)
            SK(1) = SK(1) * SUM
            SK(2) = SK(2) * SUM
            SK(3) = SK(3) * SUM
            SUM = SK(1)*SK(4)+SK(2)*SK(5)+SK(3)*SK(6)
            SK(4) = SK(4) - SUM*SK(1)
            SK(5) = SK(5) - SUM*SK(2)
            SK(6) = SK(6) - SUM*SK(3)
            SUM = ONE / MAX(SQRT(SK(4)*SK(4)+SK(5)*SK(5)+SK(6)*SK(6)),EM20)
            SK(4) = SK(4) * SUM
            SK(5) = SK(5) * SUM
            SK(6) = SK(6) * SUM
          IF (JCVT > 0) THEN
C         dir ortho v.s. local elem
            GAMA(I,1) = SK(1)*E1X(I) + SK(2)*E1Y(I) + SK(3)*E1Z(I)
            GAMA(I,2) = SK(1)*E2X(I) + SK(2)*E2Y(I) + SK(3)*E2Z(I)
            GAMA(I,3) = SK(1)*E3X(I) + SK(2)*E3Y(I) + SK(3)*E3Z(I)
            GAMA(I,4) = SK(4)*E1X(I) + SK(5)*E1Y(I) + SK(6)*E1Z(I)
            GAMA(I,5) = SK(4)*E2X(I) + SK(5)*E2Y(I) + SK(6)*E2Z(I)
            GAMA(I,6) = SK(4)*E3X(I) + SK(5)*E3Y(I) + SK(6)*E3Z(I)
          ELSE
            GAMA(I,1:6) = SK(1:6)
          END IF
        ELSEIF (IPNUM == 21) THEN
C --   g1: o-pt(o element centrid);  g2:Z^g1      
            II=NFT+I
            NNOD = ISOLNOD
            IF (NNOD==10) NNOD=4
            LX=ZERO
            LY=ZERO
            LZ=ZERO
            DO J = 1,NNOD
             N = IXS(J+1,II)
             LX=LX+X(1,N)
             LY=LY+X(2,N)
             LZ=LZ+X(3,N)
            END DO
            LX=LX/NNOD
            LY=LY/NNOD
            LZ=LZ/NNOD
            SK(1) = LX - PTS(1)
            SK(2) = LY - PTS(2)
            SK(3) = LZ - PTS(3)
            SUM = SQRT(SK(1)*SK(1)+SK(2)*SK(2)+SK(3)*SK(3))
C----   check if SUM>0  wrong input pt position same than elem id center        
          IF (SUM < EM20) THEN
              CALL ANCMSG(MSGID=1919,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IPNUM)
          ELSE
            SK(1) = SK(1) / SUM
            SK(2) = SK(2) / SUM
            SK(3) = SK(3) / SUM
            SK(4) = - SK(2)
            SK(5) =   SK(1)
            SK(6) =   ZERO
            IF (JCVT > 0) THEN
C           dir ortho v.s. local elem
              GAMA(I,1) = SK(1)*E1X(I) + SK(2)*E1Y(I) + SK(3)*E1Z(I)
              GAMA(I,2) = SK(1)*E2X(I) + SK(2)*E2Y(I) + SK(3)*E2Z(I)
              GAMA(I,3) = SK(1)*E3X(I) + SK(2)*E3Y(I) + SK(3)*E3Z(I)
              GAMA(I,4) = SK(4)*E1X(I) + SK(5)*E1Y(I) + SK(6)*E1Z(I)
              GAMA(I,5) = SK(4)*E2X(I) + SK(5)*E2Y(I) + SK(6)*E2Z(I)
              GAMA(I,6) = SK(4)*E3X(I) + SK(5)*E3Y(I) + SK(6)*E3Z(I)
            ELSE
              GAMA(I,1:6) = SK(1:6)
            END IF
          END IF
        ELSEIF (IPNUM == 23) THEN
C --   Vj+phi       
            SUM = ONE / MAX(SQRT(SX(I)*SX(I)+SY(I)*SY(I)+SZ(I)*SZ(I)),EM20)
            F3X = SX(I) * SUM
            F3Y = SY(I) * SUM
            F3Z = SZ(I) * SUM
            VX = GEO(7,IG)
            VY = GEO(8,IG)
            VZ = GEO(9,IG)
            SUM = VX*F3X+VY*F3Y+VZ*F3Z
            SK(4) = VX - SUM*F3X
            SK(5) = VY - SUM*F3Y
            SK(6) = VZ - SUM*F3Z
            SK(1) = SK(5)*F3Z - SK(6)*F3Y
            SK(2) = SK(6)*F3X - SK(4)*F3Z
            SK(3) = SK(4)*F3Y - SK(5)*F3X
            SUM = SQRT(SK(1)*SK(1)+SK(2)*SK(2)+SK(3)*SK(3))
C----   check if SUM>0  wrong input Vj, same than s        
          IF (SUM < EM20) THEN
              CALL ANCMSG(MSGID=1920,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IPNUM)
          ELSE
            F1X(I) = SK(1) / SUM
            F1Y(I) = SK(2) / SUM
            F1Z(I) = SK(3) / SUM
            F2X(I) = F3Y*F1Z(I) - F3Z*F1Y(I)
            F2Y(I) = F3Z*F1X(I) - F3X*F1Z(I)
            F2Z(I) = F3X*F1Y(I) - F3Y*F1X(I)
C           
            SK(1) = CP*F1X(I) + SP*F2X(I)
            SK(2) = CP*F1Y(I) + SP*F2Y(I)
            SK(3) = CP*F1Z(I) + SP*F2Z(I)
            SK(4) =-SP*F1X(I) + CP*F2X(I)
            SK(5) =-SP*F1Y(I) + CP*F2Y(I)
            SK(6) =-SP*F1Z(I) + CP*F2Z(I)
            IF (JCVT > 0) THEN
C           dir ortho v.s. local elem
              GAMA(I,1) = SK(1)*E1X(I) + SK(2)*E1Y(I) + SK(3)*E1Z(I)
              GAMA(I,2) = SK(1)*E2X(I) + SK(2)*E2Y(I) + SK(3)*E2Z(I)
              GAMA(I,3) = SK(1)*E3X(I) + SK(2)*E3Y(I) + SK(3)*E3Z(I)
              GAMA(I,4) = SK(4)*E1X(I) + SK(5)*E1Y(I) + SK(6)*E1Z(I)
              GAMA(I,5) = SK(4)*E2X(I) + SK(5)*E2Y(I) + SK(6)*E2Z(I)
              GAMA(I,6) = SK(4)*E3X(I) + SK(5)*E3Y(I) + SK(6)*E3Z(I)
            ELSE
              GAMA(I,1:6) = SK(1:6)
            END IF
          END IF
        ELSEIF (IPNUM == 24) THEN
C --   Vj+Pt       
            II=NFT+I
            NNOD = ISOLNOD
            IF (NNOD==10) NNOD=4
            LX=ZERO
            LY=ZERO
            LZ=ZERO
            DO J = 1,NNOD
             N = IXS(J+1,II)
             LX=LX+X(1,N)
             LY=LY+X(2,N)
             LZ=LZ+X(3,N)
            END DO
            LX=LX/NNOD
            LY=LY/NNOD
            LZ=LZ/NNOD
            VX = GEO(7,IG)
            VY = GEO(8,IG)
            VZ = GEO(9,IG)
            SUM = ONE / MAX(SQRT(VX*VX+VY*VY+VZ*VZ),EM20)
            VX  = VX*SUM
            VY  = VY*SUM
            VZ  = VZ*SUM
            SK(1) = LX - PTS(1)
            SK(2) = LY - PTS(2)
            SK(3) = LZ - PTS(3)
            SUM = VX*SK(1)+VY*SK(2)+VZ*SK(3)
            F3X = SK(1) - SUM*VX
            F3Y = SK(2) - SUM*VY
            F3Z = SK(3) - SUM*VZ
            SK(1) = VY*F3Z - VZ*F3Y
            SK(2) = VZ*F3X - VX*F3Z
            SK(3) = VX*F3Y - VY*F3X
            SUM = SQRT(SK(1)*SK(1)+SK(2)*SK(2)+SK(3)*SK(3))
C----   check if SUM>0          
          IF (SUM < EM20) THEN
              CALL ANCMSG(MSGID=1920,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IPNUM)
          ELSE
            SK(1) = SK(1) / SUM
            SK(2) = SK(2) / SUM
            SK(3) = SK(3) / SUM
            SK(4) = VX
            SK(5) = VY
            SK(6) = VZ
            IF (JCVT > 0) THEN
C           dir ortho v.s. local elem
              GAMA(I,1) = SK(1)*E1X(I) + SK(2)*E1Y(I) + SK(3)*E1Z(I)
              GAMA(I,2) = SK(1)*E2X(I) + SK(2)*E2Y(I) + SK(3)*E2Z(I)
              GAMA(I,3) = SK(1)*E3X(I) + SK(2)*E3Y(I) + SK(3)*E3Z(I)
              GAMA(I,4) = SK(4)*E1X(I) + SK(5)*E1Y(I) + SK(6)*E1Z(I)
              GAMA(I,5) = SK(4)*E2X(I) + SK(5)*E2Y(I) + SK(6)*E2Z(I)
              GAMA(I,6) = SK(4)*E3X(I) + SK(5)*E3Y(I) + SK(6)*E3Z(I)
            ELSE
              GAMA(I,1:6) = SK(1:6)
            END IF
          END IF
        ELSEIF (JCVT == 0) THEN
          SELECT CASE (IPNUM)
          CASE (1)
            GAMA(I,1)= CP
            GAMA(I,2)= SP
            GAMA(I,3)= ZERO
            GAMA(I,4)=-SP
            GAMA(I,5)= CP
            GAMA(I,6)= ZERO
          CASE (2)
            GAMA(I,1)= ZERO
            GAMA(I,2)= CP
            GAMA(I,3)= SP
            GAMA(I,4)= ZERO
            GAMA(I,5)=-SP
            GAMA(I,6)= CP
          CASE (3)
            GAMA(I,1)= SP
            GAMA(I,2)= ZERO
            GAMA(I,3)= CP
            GAMA(I,4)= CP
            GAMA(I,5)= ZERO
            GAMA(I,6)=-SP
          CASE (11)
            VN = VX*E3X(I) + VY*E3Y(I) + VZ*E3Z(I)
            VX = VX - VN*E3X(I)
            VY = VY - VN*E3Y(I)
            VZ = VZ - VN*E3Z(I)
            SUM = SQRT(VX*VX+VY*VY+VZ*VZ)
            IF (SUM < EM10) THEN
              CALL ANCMSG(MSGID=811,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR)
              SK(1) = E1X(I)
              SK(2) = E1Y(I)
              SK(3) = E1Z(I)
            ELSE
              SUM = ONE / SUM
              SK(1) = VX * SUM
              SK(2) = VY * SUM
              SK(3) = VZ * SUM
            ENDIF
            SK(4) = E3Y(I)* SK(3) - E3Z(I)* SK(2)
            SK(5) = E3Z(I)* SK(1) - E3X(I)* SK(3)
            SK(6) = E3X(I)* SK(2) - E3Y(I)* SK(1)
            GAMA(I,1) = SK(1)*E1X(I) + SK(2)*E1Y(I) + SK(3)*E1Z(I)
            GAMA(I,2) = SK(1)*E2X(I) + SK(2)*E2Y(I) + SK(3)*E2Z(I)
            GAMA(I,3) = SK(1)*E3X(I) + SK(2)*E3Y(I) + SK(3)*E3Z(I)
            GAMA(I,4) = SK(4)*E1X(I) + SK(5)*E1Y(I) + SK(6)*E1Z(I)
            GAMA(I,5) = SK(4)*E2X(I) + SK(5)*E2Y(I) + SK(6)*E2Z(I)
            GAMA(I,6) = SK(4)*E3X(I) + SK(5)*E3Y(I) + SK(6)*E3Z(I)
          CASE (12)
            VN = VX*E1X(I) + VY*E1Y(I) + VZ*E1Z(I)
            VX = VX - VN*E1X(I)
            VY = VY - VN*E1Y(I)
            VZ = VZ - VN*E1Z(I)
            SUM = SQRT(VX*VX+VY*VY+VZ*VZ)
            IF (SUM < EM10) THEN
              CALL ANCMSG(MSGID=811,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR)
              SK(1) = E2X(I)
              SK(2) = E2Y(I)
              SK(3) = E2Z(I)
            ELSE
              SUM = ONE / SUM
              SK(1) = VX * SUM
              SK(2) = VY * SUM
              SK(3) = VZ * SUM
            ENDIF
            SK(4) = E1Y(I)* SK(3) - E1Z(I)* SK(2)
            SK(5) = E1Z(I)* SK(1) - E1X(I)* SK(3)
            SK(6) = E1X(I)* SK(2) - E1Y(I)* SK(1)
            GAMA(I,1) = SK(1)*E1X(I) + SK(2)*E1Y(I) + SK(3)*E1Z(I)
            GAMA(I,2) = SK(1)*E2X(I) + SK(2)*E2Y(I) + SK(3)*E2Z(I)
            GAMA(I,3) = SK(1)*E3X(I) + SK(2)*E3Y(I) + SK(3)*E3Z(I)
            GAMA(I,4) = SK(4)*E1X(I) + SK(5)*E1Y(I) + SK(6)*E1Z(I)
            GAMA(I,5) = SK(4)*E2X(I) + SK(5)*E2Y(I) + SK(6)*E2Z(I)
            GAMA(I,6) = SK(4)*E3X(I) + SK(5)*E3Y(I) + SK(6)*E3Z(I)
          CASE (13)
            VN = VX*E2X(I) + VY*E2Y(I) + VZ*E2Z(I)
            VX = VX - VN*E2X(I)
            VY = VY - VN*E2Y(I)
            VZ = VZ - VN*E2Z(I)
            SUM = SQRT(VX*VX+VY*VY+VZ*VZ)
            IF (SUM < EM10) THEN
              CALL ANCMSG(MSGID=811,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR)
              SK(1) = E3X(I)
              SK(2) = E3Y(I)
              SK(3) = E3Z(I)
            ELSE
              SUM= ONE / SUM
              SK(1) = VX * SUM
              SK(2) = VY * SUM
              SK(3) = VZ * SUM
            ENDIF
            SK(4) = E2Y(I)* SK(3) - E2Z(I)* SK(2)
            SK(5) = E2Z(I)* SK(1) - E2X(I)* SK(3)
            SK(6) = E2X(I)* SK(2) - E2Y(I)* SK(1)
            GAMA(I,1) = SK(1)*E1X(I) + SK(2)*E1Y(I) + SK(3)*E1Z(I)
            GAMA(I,2) = SK(1)*E2X(I) + SK(2)*E2Y(I) + SK(3)*E2Z(I)
            GAMA(I,3) = SK(1)*E3X(I) + SK(2)*E3Y(I) + SK(3)*E3Z(I)
            GAMA(I,4) = SK(4)*E1X(I) + SK(5)*E1Y(I) + SK(6)*E1Z(I)
            GAMA(I,5) = SK(4)*E2X(I) + SK(5)*E2Y(I) + SK(6)*E2Z(I)
            GAMA(I,6) = SK(4)*E3X(I) + SK(5)*E3Y(I) + SK(6)*E3Z(I)
          END SELECT
        ELSEIF (JCVT > 0) THEN
C         R'=R, T'=R^S, S'=T'^R
          SUM=SQRT(RX(I)**2+RY(I)**2+RZ(I)**2)
          IF (SUM > ZERO) SUM=ONE/SUM
          HX=RX(I)*SUM
          HY=RY(I)*SUM
          HZ=RZ(I)*SUM
          LX=HY*SZ(I)-HZ*SY(I)
          LY=HZ*SX(I)-HX*SZ(I)
          LZ=HX*SY(I)-HY*SX(I)
          SUM = SQRT(LX**2+LY**2+LZ**2)
          IF (SUM > ZERO) SUM=ONE/SUM
          LX=LX*SUM
          LY=LY*SUM
          LZ=LZ*SUM
          KX=LY*HZ-LZ*HY
          KY=LZ*HX-LX*HZ
          KZ=LX*HY-LY*HX
          SUM = SQRT(KX**2+KY**2+KZ**2)
          IF (SUM > ZERO) SUM=ONE/SUM
          KX=KX*SUM
          KY=KY*SUM
          KZ=KZ*SUM
          SELECT CASE (IPNUM)
          CASE (1)
            SK(1)= CP*HX+SP*KX
            SK(2)= CP*HY+SP*KY
            SK(3)= CP*HZ+SP*KZ
            SK(4)=-SP*HX+CP*KX
            SK(5)=-SP*HY+CP*KY
            SK(6)=-SP*HZ+CP*KZ
          CASE (2)
            SK(1)= CP*KX+SP*LX
            SK(2)= CP*KY+SP*LY
            SK(3)= CP*KZ+SP*LZ
            SK(4)=-SP*KX+CP*LX
            SK(5)=-SP*KY+CP*LY
            SK(6)=-SP*KZ+CP*LZ
          CASE (3)
            SK(1)= CP*LX+SP*HX
            SK(2)= CP*LY+SP*HY
            SK(3)= CP*LZ+SP*HZ
            SK(4)=-SP*LX+CP*HX
            SK(5)=-SP*LY+CP*HY
            SK(6)=-SP*LZ+CP*HZ
          CASE (11)
            VN = VX*LX + VY*LY + VZ*LZ
            VX = VX - VN*LX
            VY = VY - VN*LY
            VZ = VZ - VN*LZ
            SUM = SQRT(VX*VX+VY*VY+VZ*VZ)
            IF (SUM < EM10) THEN
              CALL ANCMSG(MSGID=811,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR)
              SK(1) = HX
              SK(2) = HY
              SK(3) = HZ
            ELSE
              SUM = ONE / SUM
              SK(1) = VX * SUM
              SK(2) = VY * SUM
              SK(3) = VZ * SUM
            ENDIF
            SK(4) = LY* SK(3) - LZ* SK(2)
            SK(5) = LZ* SK(1) - LX* SK(3)
            SK(6) = LX* SK(2) - LY* SK(1)
          CASE (12)
            VN = VX*HX + VY*HY + VZ*HZ
            VX = VX - VN*HX
            VY = VY - VN*HY
            VZ = VZ - VN*HZ
            SUM = SQRT(VX*VX+VY*VY+VZ*VZ)
            IF (SUM < EM10) THEN
              CALL ANCMSG(MSGID=811,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR)
              SK(1) = KX
              SK(2) = KY
              SK(3) = KZ
            ELSE
              SUM = ONE / SUM
              SK(1) = VX * SUM
              SK(2) = VY * SUM
              SK(3) = VZ * SUM
            ENDIF
            SK(4) = HY* SK(3) - HZ* SK(2)
            SK(5) = HZ* SK(1) - HX* SK(3)
            SK(6) = HX* SK(2) - HY* SK(1)
          CASE (13)
            VN = VX*KX + VY*KY + VZ*KZ
            VX = VX - VN*KX
            VY = VY - VN*KY
            VZ = VZ - VN*KZ
            SUM = SQRT(VX*VX+VY*VY+VZ*VZ)
            IF (SUM < EM10) THEN
              CALL ANCMSG(MSGID=811,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ID,
     .                    C1=TITR)
              SK(1) = LX
              SK(2) = LY
              SK(3) = LZ
            ELSE
              SUM = ONE / SUM
              SK(1) = VX * SUM
              SK(2) = VY * SUM
              SK(3) = VZ * SUM
            ENDIF
            SK(4) = KY* SK(3) - KZ* SK(2)
            SK(5) = KZ* SK(1) - KX* SK(3)
            SK(6) = KX* SK(2) - KY* SK(1)
          END SELECT
          GAMA(I,1) = SK(1)*E1X(I) + SK(2)*E1Y(I) + SK(3)*E1Z(I)
          GAMA(I,2) = SK(1)*E2X(I) + SK(2)*E2Y(I) + SK(3)*E2Z(I)
          GAMA(I,3) = SK(1)*E3X(I) + SK(2)*E3Y(I) + SK(3)*E3Z(I)
          GAMA(I,4) = SK(4)*E1X(I) + SK(5)*E1Y(I) + SK(6)*E1Z(I)
          GAMA(I,5) = SK(4)*E2X(I) + SK(5)*E2Y(I) + SK(6)*E2Z(I)
          GAMA(I,6) = SK(4)*E3X(I) + SK(5)*E3Y(I) + SK(6)*E3Z(I)
        ENDIF
      ENDDO
C-----
      IF (IREP > 0) THEN
C       dir orthotrope attache au repere isoparametrique
        DO I=LFT,LLT
          A(1) = RX(I)*E1X(I) + RY(I)*E1Y(I) + RZ(I)*E1Z(I)
          A(2) = RX(I)*E2X(I) + RY(I)*E2Y(I) + RZ(I)*E2Z(I)
          A(3) = RX(I)*E3X(I) + RY(I)*E3Y(I) + RZ(I)*E3Z(I)
          A(4) = SX(I)*E1X(I) + SY(I)*E1Y(I) + SZ(I)*E1Z(I)
          A(5) = SX(I)*E2X(I) + SY(I)*E2Y(I) + SZ(I)*E2Z(I)
          A(6) = SX(I)*E3X(I) + SY(I)*E3Y(I) + SZ(I)*E3Z(I)
          A(7) = TX(I)*E1X(I) + TY(I)*E1Y(I) + TZ(I)*E1Z(I)
          A(8) = TX(I)*E2X(I) + TY(I)*E2Y(I) + TZ(I)*E2Z(I)
          A(9) = TX(I)*E3X(I) + TY(I)*E3Y(I) + TZ(I)*E3Z(I)
          CALL INV3(A, B)
          A(1) = GAMA(I,1)
          A(2) = GAMA(I,2)
          A(3) = GAMA(I,3)
          A(4) = GAMA(I,4)
          A(5) = GAMA(I,5)
          A(6) = GAMA(I,6)
C
          GAMA(I,1) = B(1)*A(1) + B(4)*A(2) + B(7)*A(3)
          GAMA(I,2) = B(2)*A(1) + B(5)*A(2) + B(8)*A(3)
          GAMA(I,3) = B(3)*A(1) + B(6)*A(2) + B(9)*A(3)
          SUM = ONE / MAX(EM20,SQRT(GAMA(I,1)**2 + GAMA(I,2)**2 + GAMA(I,3)**2))
          GAMA(I,1) = GAMA(I,1) * SUM
          GAMA(I,2) = GAMA(I,2) * SUM
          GAMA(I,3) = GAMA(I,3) * SUM
C
          GAMA(I,4) = B(1)*A(4) + B(4)*A(5) + B(7)*A(6)
          GAMA(I,5) = B(2)*A(4) + B(5)*A(5) + B(8)*A(6)
          GAMA(I,6) = B(3)*A(4) + B(6)*A(5) + B(9)*A(6)
          SUM = ONE / MAX(EM20,SQRT(GAMA(I,4)**2 + GAMA(I,5)**2 + GAMA(I,6)**2))
          GAMA(I,4) = GAMA(I,4) * SUM
          GAMA(I,5) = GAMA(I,5) * SUM
          GAMA(I,6) = GAMA(I,6) * SUM
        ENDDO
      ENDIF
C
C---
      IF (NVSOLID3 /= 0) THEN
       IIS= NVSOLID1  + NVSOLID2 + 4 + NUSOLID
       DO I=LFT,LLT
          II=NFT+I
          JJ=PT(II)
          IFLAGINI = 1
          IF(JJ==0)IFLAGINI = 0
          IF(IFLAGINI == 1 .AND.
     .   ( SIGSP(IIS+1,JJ) /= ZERO.OR.SIGSP(IIS+2,JJ)/=ZERO.OR.
     .   SIGSP(IIS+3,JJ) /= ZERO .OR. SIGSP(IIS+4,JJ) /= ZERO .OR.
     .   SIGSP(IIS+5,JJ) /= ZERO .OR. SIGSP(IIS+6,JJ) /= ZERO) )THEN
            G11 = SIGSP(IIS+1,JJ)
            G21 = SIGSP(IIS+2,JJ)
            G31 = SIGSP(IIS+3,JJ)
            G12 = SIGSP(IIS+4,JJ)
            G22 = SIGSP(IIS+5,JJ)
            G32 = SIGSP(IIS+6,JJ)
            G13 = G21*G32-G31*G22
            G23 = G31*G12-G11*G32
            G33 = G11*G22-G21*G12
C       Change of basis : GLOBAL -> LOCAL.
            GAMA(I,1)=E1X(I)*G11+E1Y(I)*G21+E1Z(I)*G31
            GAMA(I,2)=E2X(I)*G11+E2Y(I)*G21+E2Z(I)*G31
            GAMA(I,3)=E3X(I)*G11+E3Y(I)*G21+E3Z(I)*G31
            GAMA(I,4)=E1X(I)*G12+E1Y(I)*G22+E1Z(I)*G32
            GAMA(I,5)=E2X(I)*G12+E2Y(I)*G22+E2Z(I)*G32
            GAMA(I,6)=E3X(I)*G12+E3Y(I)*G22+E3Z(I)*G32
          ENDIF
       ENDDO
      ENDIF
C
C---
      RETURN
      END SUBROUTINE SMORTH3

