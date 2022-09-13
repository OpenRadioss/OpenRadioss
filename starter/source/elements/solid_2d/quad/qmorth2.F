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
Chd|  QMORTH2                       source/elements/solid_2d/quad/qmorth2.F
Chd|-- called by -----------
Chd|        Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|        QINIT2                        source/elements/solid_2d/quad/qinit2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE QMORTH2(PID  ,GEO  ,IGEO ,GAMA, NEL,
     .                   RY   ,RZ   ,SY   ,SZ, 
     .                   E1Y  ,E1Z  ,E2Y  ,E2Z)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JHBE,NEL
      INTEGER PID(*),IGEO(NPROPGI,*)
C     REAL
      my_real
     .   GEO(NPROPG,*),GAMA(NEL,6),
     .   RY(*) ,RZ(*) ,SY(*) ,SZ(*),
     .   E1Y(*),E1Z(*),E2Y(*),E2Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IG,ID,ISK,IPNUM,IIS,II,J,JJ,N,IFLAGINI
C     REAL
      my_real
     .   XL,YL,ZL,SUM,HX,HY,HZ,KX,KY,KZ,
     .   LX,LY,LZ,PHI,CP,SP,VX,VY,VZ,VN,
     .   F3X,F3Y,F3Z,
     .   G11,G22,G33,G12,G21,G23,G32,G13,G31
      my_real
     .   SK(6)
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------------------------------
C     Repere orthotrope
C=======================================================================
      DO I=LFT,LLT
        IG = PID(I)
        ID=IGEO(1,IG)
        CALL FRETITL2(TITR,
     .                IGEO(NPROPGI-LTITR+1,IG),LTITR)
        IPNUM = IGEO(2,IG)
        VX  = GEO(7,IG)
        VY  = GEO(8,IG)
        VZ  = GEO(9,IG)
        PHI = GEO(1,IG) * PI/HUNDRED80
        CP  = COS(PHI)
        SP  = SIN(PHI)
c
        IF (JCVT == 0) THEN 
c ------------------------------
c Angle d'orthotropie defini par rapport
c a la premiere direction du repere isoparametrique
c ------------------------------
          IF(IPNUM==11) THEN
            SUM=SQRT(RY(I)**2+RZ(I)**2)
            SUM=ONE/MAX(SUM,EM20)
            HX=ZERO
            HY=RY(I)*SUM
            HZ=RZ(I)*SUM
            LX=HY*SZ(I)-HZ*SY(I)
            LY=-HX*SZ(I)
            LZ=ZERO
            SUM = SQRT(LX**2+LY**2+LZ**2)
            SUM=ONE/MAX(SUM,EM20)
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
            VN = VX*LX + VY*LY + VZ*LZ
            VX = VX - VN*LX
            VY = VY - VN*LY
            VZ = VZ - VN*LZ
            SUM = SQRT(VX**2+VY**2+VZ**2)
            IF (SUM < EM10) THEN
              CALL ANCMSG(MSGID=1620,
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
            GAMA(I,1) = SK(1)*HX + SK(2)*HY + SK(3)*HZ
            GAMA(I,2) = SK(1)*KX + SK(2)*KY + SK(3)*KZ
            GAMA(I,3) = ZERO
            GAMA(I,4) = SK(4)*HX + SK(5)*HY + SK(6)*HZ
            GAMA(I,5) = SK(4)*KX + SK(5)*KY + SK(6)*KZ
            GAMA(I,6) = ZERO
          ELSE
            GAMA(I,1)= CP
            GAMA(I,2)= SP
            GAMA(I,3)= ZERO
            GAMA(I,4)=-SP
            GAMA(I,5)= CP
            GAMA(I,6)= ZERO
          ENDIF
        ELSEIF (JCVT > 0) THEN
          IF(IPNUM==11) THEN       
            SUM=SQRT(E1Y(I)**2+E1Z(I)**2)
            IF (SUM > ZERO) SUM=ONE/SUM
            HX=ZERO
            HY=E1Y(I)*SUM
            HZ=E1Z(I)*SUM
            LX=HY*E2Z(I)-HZ*E2Y(I)
            LY=-HX*E2Z(I)
            LZ=ZERO
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
            VN = VX*LX + VY*LY + VZ*LZ
            VX = VX - VN*LX
            VY = VY - VN*LY
            VZ = VZ - VN*LZ
            SUM = SQRT(VX**2+VY**2+VZ**2)
            IF (SUM < EM10) THEN
              CALL ANCMSG(MSGID=1620,
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
          ELSE
c ------------------------------
c ATTENTION !
c Angle d'orthotropie est desormais toujours donne par l'utilisateur 
c par rapport au repere isoparametrique
c Puis transforme en interne en angle par rapport au repere corrotationnel
c (pour plus de simplicite cote utilisateur)
c (anciennement donne directement par rapport au repere corot)
c ------------------------------          
            SUM=SQRT(RY(I)**2+RZ(I)**2)
            SUM=ONE/MAX(SUM,EM20)
            HX=ZERO
            HY=RY(I)*SUM
            HZ=RZ(I)*SUM
            LX=HY*SZ(I)-HZ*SY(I)
            LY=-HX*SZ(I)
            LZ=ZERO
            SUM = SQRT(LX**2+LY**2+LZ**2)
            SUM=ONE/MAX(SUM,EM20)
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
            SK(1)= CP*HX+SP*KX
            SK(2)= CP*HY+SP*KY
            SK(3)= CP*HZ+SP*KZ
            SK(4)=-SP*HX+CP*KX
            SK(5)=-SP*HY+CP*KY
            SK(6)=-SP*HZ+CP*KZ
            HX=ZERO
            HY=E1Y(I)
            HZ=E1Z(I)
            KX=ZERO
            KY=E2Y(I)
            KZ=E2Z(I)         
          ENDIF
          GAMA(I,1) = ZERO
          GAMA(I,2) = SK(1)*HX + SK(2)*HY + SK(3)*HZ
          GAMA(I,3) = SK(1)*KX + SK(2)*KY + SK(3)*KZ
          GAMA(I,4) = ZERO
          GAMA(I,5) = SK(4)*HX + SK(5)*HY + SK(6)*HZ
          GAMA(I,6) = SK(4)*KX + SK(5)*KY + SK(6)*KZ
        ENDIF
      ENDDO
C-----

      RETURN
      END SUBROUTINE QMORTH2
      
