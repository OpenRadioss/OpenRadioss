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
Chd|  LAG_BCS                       source/tools/lagmul/lag_bcs.F 
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE LAG_BCS(IGRNOD ,IBCSLAG,SK     ,RLL    ,NGRNOD ,
     2                   IADLL  ,LLL    ,JLL    ,SLL    ,XLL    ,
     3                   COMNTAG,ICFTAG ,JCFTAG ,MASS   ,INER   ,
     4                   V      ,VR     ,A      ,AR     ,ISKIP  ,
     5                   NCF_S  ,NC     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "lagmult.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC, ISKIP,NCF_S,NGRNOD, 
     .        IBCSLAG(5,*),IADLL(*),
     .        SLL(*),LLL(*),JLL(*),COMNTAG(*),ICFTAG(*),JCFTAG(*)
      my_real
     .  XLL(*),RLL(*),SK(LSKEW,*),MASS(*),INER(*),V(3,*),VR(3,*),
     .  A(3,*),AR(3,*)
!
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IC,IG,IK,IGR,IS,NN,CT,CR
      my_real
     .   AA,VV,HH,R,SK1,SK2,SK3,DTM2
C-----------------------------------------------
C        NC : nombre de condition cinematique 
C        IC : numero de la condition cinematique (1,NC)
C        IK : 
C        I  : numero global du noeud (1,NUMNOD)
C        J  : direction 1,2,3,4,5,6
C------
C        IADLL(NC)        : IAD = IADLL(IC)
C        IK = IAD,IAD+1,IAD+2,...
C        LLL(LAG_NKF)  : I = LLL(IK)
C        JLL(LAG_NKF)  : J = JLL(IK)
C======================================================================|
      DTM2 = ONE / (DT2*DT12)
      DO I=1,NBCSLAG
        IGR = IBCSLAG(1,I)
        CT  = IBCSLAG(2,I)
        CR  = IBCSLAG(3,I)
        IS  = IBCSLAG(4,I)
        DO IG=1,IGRNOD(IGR)%NENTITY
          NN=IGRNOD(IGR)%ENTITY(IG)
         IF (COMNTAG(NN).GT.1) THEN
          IF(MASS(NN).NE.ZERO)THEN
C--- Translations
            IF(CT.EQ.1.OR.CT.EQ.3.OR.CT.EQ.5.OR.CT.EQ.7)THEN
C--- dz
              NC = NC + 1
              IC = NC - NCF_S
              ICFTAG(IC) = IC + ISKIP
              JCFTAG(IC+ISKIP) = NC
              IF(IS.EQ.1)THEN
                IADLL(NC+1)=IADLL(NC) + 1
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 3
                SLL(IK) = 0
                XLL(IK) = ONE
c                RLL(NC) = -DX(3,NN) * DTM2
c                print*,'RLL(',NC,') =',RLL(NC), DX(3,NN)
                A(3,NN) = -V(3,NN)/DT12
              ELSE
                SK1 = SK(7,IS)
                SK2 = SK(8,IS)
                SK3 = SK(9,IS)
                IADLL(NC+1)=IADLL(NC) + 3
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 1
                SLL(IK) = 0
                XLL(IK) = SK1
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 2
                SLL(IK) = 0
                XLL(IK) = SK2
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 3
                SLL(IK) = 0
                XLL(IK) = SK3
                HH = SK1*SK1 + SK2*SK2 + SK3*SK3
                R  = SK1*(V(1,NN)/DT12 + A(1,NN))
     .             + SK2*(V(2,NN)/DT12 + A(2,NN))
     .             + SK3*(V(3,NN)/DT12 + A(3,NN))
                R  = R / HH
                A(1,NN) = A(1,NN) - SK1*R
                A(2,NN) = A(2,NN) - SK2*R
                A(3,NN) = A(3,NN) - SK3*R
              ENDIF
            ENDIF
            IF(CT.EQ.2.OR.CT.EQ.3.OR.CT.EQ.6.OR.CT.EQ.7)THEN
C--- dy
              NC = NC + 1
              IC = NC - NCF_S
              ICFTAG(IC) = IC + ISKIP
              JCFTAG(IC+ISKIP) = NC
              IF(IS.EQ.1)THEN
                IADLL(NC+1)=IADLL(NC) + 1
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 2
                SLL(IK) = 0
                XLL(IK) = ONE
c                RLL(NC) = -DX(2,NN) * DTM2
c                print*,'RLL(',NC,') =',RLL(NC), DX(2,NN)
                A(2,NN) = -V(2,NN)/DT12
              ELSE
                SK1 = SK(4,IS)
                SK2 = SK(5,IS)
                SK3 = SK(6,IS)
                IADLL(NC+1)=IADLL(NC) + 3
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 1
                SLL(IK) = 0
                XLL(IK) = SK1
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 2
                SLL(IK) = 0
                XLL(IK) = SK2
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 3
                SLL(IK) = 0
                XLL(IK) = SK3
                HH = SK1*SK1 + SK2*SK2 + SK3*SK3
                R  = SK1*(V(1,NN)/DT12 + A(1,NN))
     .             + SK2*(V(2,NN)/DT12 + A(2,NN))
     .             + SK3*(V(3,NN)/DT12 + A(3,NN))
                R  = R / HH
                A(1,NN) = A(1,NN) - SK1*R
                A(2,NN) = A(2,NN) - SK2*R
                A(3,NN) = A(3,NN) - SK3*R
              ENDIF
            ENDIF
            IF(CT.EQ.4.OR.CT.EQ.5.OR.CT.EQ.6.OR.CT.EQ.7)THEN
C--- dx
              NC = NC + 1
              IC = NC - NCF_S
              ICFTAG(IC) = IC + ISKIP
              JCFTAG(IC+ISKIP) = NC
              IF(IS.EQ.1)THEN
                IADLL(NC+1)=IADLL(NC) + 1
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 1
                SLL(IK) = 0
                XLL(IK) = ONE
c                RLL(NC) = -DX(1,NN) * DTM2
c                print*,'RLL(',NC,') =',RLL(NC), DX(1,NN)
                A(1,NN) = -V(1,NN)/DT12
              ELSE
                SK1 = SK(1,IS)
                SK2 = SK(2,IS)
                SK3 = SK(3,IS)
                IADLL(NC+1)=IADLL(NC) + 3
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 1
                SLL(IK) = 0
                XLL(IK) = SK1
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 2
                SLL(IK) = 0
                XLL(IK) = SK2
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 3
                SLL(IK) = 0
                XLL(IK) = SK3
                HH = SK1*SK1 + SK2*SK2 + SK3*SK3
                R  = SK1*(V(1,NN)/DT12 + A(1,NN))
     .             + SK2*(V(2,NN)/DT12 + A(2,NN))
     .             + SK3*(V(3,NN)/DT12 + A(3,NN))
                R  = R / HH
                A(1,NN) = A(1,NN) - SK1*R
                A(2,NN) = A(2,NN) - SK2*R
                A(3,NN) = A(3,NN) - SK3*R
              ENDIF
            ENDIF
          ENDIF
          IF(INER(NN).NE.ZERO)THEN
C--- Rotations
            IF(CR.EQ.1.OR.CR.EQ.3.OR.CR.EQ.5.OR.CR.EQ.7)THEN
C--- rz
              NC = NC + 1
              IC = NC - NCF_S
              ICFTAG(IC) = IC + ISKIP
              JCFTAG(IC+ISKIP) = NC
              IF(IS.EQ.1)THEN
                IADLL(NC+1)=IADLL(NC) + 1
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 6
                SLL(IK) = 0
                XLL(IK) = ONE
c                RLL(NC) = -DR(3,NN) * DTM2
c                print*,'RLL(',NC,') =',RLL(NC), DR(3,NN)
                AR(3,NN) = -VR(3,NN)/DT12
              ELSE
                SK1 = SK(7,IS)
                SK2 = SK(8,IS)
                SK3 = SK(9,IS)
                IADLL(NC+1)=IADLL(NC) + 3
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 4
                SLL(IK) = 0
                XLL(IK) = SK1
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 5
                SLL(IK) = 0
                XLL(IK) = SK2
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 6
                SLL(IK) = 0
                XLL(IK) = SK3
                HH = SK1*SK1 + SK2*SK2 + SK3*SK3
                R  = SK1*(VR(1,NN)/DT12 + AR(1,NN))
     .             + SK2*(VR(2,NN)/DT12 + AR(2,NN))
     .             + SK3*(VR(3,NN)/DT12 + AR(3,NN))
                R  = R / HH
                AR(1,NN) = AR(1,NN) - SK1*R
                AR(2,NN) = AR(2,NN) - SK2*R
                AR(3,NN) = AR(3,NN) - SK3*R
              ENDIF
            ENDIF
            IF(CR.EQ.2.OR.CR.EQ.3.OR.CR.EQ.6.OR.CR.EQ.7)THEN
C--- ry
              NC = NC + 1
              IC = NC - NCF_S
              ICFTAG(IC) = IC + ISKIP
              JCFTAG(IC+ISKIP) = NC
              IF(IS.EQ.1)THEN
                IADLL(NC+1)=IADLL(NC) + 1
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 5
                SLL(IK) = 0
                XLL(IK) = ONE
c                RLL(NC) = -DR(2,NN) * DTM2
c                print*,'RLL(',NC,') =',RLL(NC), DR(2,NN)
                AR(2,NN) = -VR(2,NN)/DT12
              ELSE
                SK1 = SK(4,IS)
                SK2 = SK(5,IS)
                SK3 = SK(6,IS)
                IADLL(NC+1)=IADLL(NC) + 3
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 4
                SLL(IK) = 0
                XLL(IK) = SK1
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 5
                SLL(IK) = 0
                XLL(IK) = SK2
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 6
                SLL(IK) = 0
                XLL(IK) = SK3
                HH = SK1*SK1 + SK2*SK2 + SK3*SK3
                R  = SK1*(VR(1,NN)/DT12 + AR(1,NN))
     .             + SK2*(VR(2,NN)/DT12 + AR(2,NN))
     .             + SK3*(VR(3,NN)/DT12 + AR(3,NN))
                R  = R / HH
                AR(1,NN) = AR(1,NN) - SK1*R
                AR(2,NN) = AR(2,NN) - SK2*R
                AR(3,NN) = AR(3,NN) - SK3*R
              ENDIF
            ENDIF
            IF(CR.EQ.4.OR.CR.EQ.5.OR.CR.EQ.6.OR.CR.EQ.7)THEN
C--- rx
              NC = NC + 1
              IC = NC - NCF_S
              ICFTAG(IC) = IC + ISKIP
              JCFTAG(IC+ISKIP) = NC
              IF(IS.EQ.1)THEN
                IADLL(NC+1)=IADLL(NC) + 1
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 4
                SLL(IK) = 0
                XLL(IK) = ONE
c                RLL(NC) = -DR(1,NN) * DTM2
c                print*,'RLL(',NC,') =',RLL(NC), DR(1,NN)
                AR(1,NN) = -VR(1,NN)/DT12
              ELSE
                SK1 = SK(1,IS)
                SK2 = SK(2,IS)
                SK3 = SK(3,IS)
                IADLL(NC+1)=IADLL(NC) + 3
                IK = IADLL(NC)
                LLL(IK) = NN
                JLL(IK) = 4
                SLL(IK) = 0
                XLL(IK) = SK1
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 5
                SLL(IK) = 0
                XLL(IK) = SK2
                IK = IK + 1
                LLL(IK) = NN
                JLL(IK) = 6
                SLL(IK) = 0
                XLL(IK) = SK3
                HH = SK1*SK1 + SK2*SK2 + SK3*SK3
                R  = SK1*(VR(1,NN)/DT12 + AR(1,NN))
     .             + SK2*(VR(2,NN)/DT12 + AR(2,NN))
     .             + SK3*(VR(3,NN)/DT12 + AR(3,NN))
                R  = R / HH
                AR(1,NN) = AR(1,NN) - SK1*R
                AR(2,NN) = AR(2,NN) - SK2*R
                AR(3,NN) = AR(3,NN) - SK3*R
              ENDIF
            ENDIF
          ENDIF
C---      Direct solution
         ELSE
           IF(IS.EQ.1) THEN
C----           REPERE GLOBAL
             IF(CT.EQ.1)THEN
               V(3,NN)=ZERO
               A(3,NN)=ZERO
               ISKIP  = ISKIP + 1
             ELSEIF(CT.EQ.2)THEN
               V(2,NN)=ZERO
               A(2,NN)=ZERO
               ISKIP  = ISKIP + 1
             ELSEIF(CT.EQ.3)THEN
               V(2,NN)=ZERO
               V(3,NN)=ZERO
               A(2,NN)=ZERO
               A(3,NN)=ZERO
               ISKIP  = ISKIP + 2
             ELSEIF(CT.EQ.4)THEN
               V(1,NN)=ZERO
               A(1,NN)=ZERO
               ISKIP  = ISKIP + 1
             ELSEIF(CT.EQ.5)THEN
               V(1,NN)=ZERO
               V(3,NN)=ZERO
               A(1,NN)=ZERO
               A(3,NN)=ZERO
               ISKIP  = ISKIP + 2
             ELSEIF(CT.EQ.6)THEN
               V(1,NN)=ZERO
               V(2,NN)=ZERO
               A(1,NN)=ZERO
               A(2,NN)=ZERO
               ISKIP  = ISKIP + 2
             ELSEIF(CT.EQ.7)THEN
               V(1,NN)=ZERO
               V(2,NN)=ZERO
               V(3,NN)=ZERO
               A(1,NN)=ZERO
               A(2,NN)=ZERO
               A(3,NN)=ZERO
               ISKIP  = ISKIP + 3
             ENDIF
             IF(CR.EQ.1)THEN
               VR(3,NN)=ZERO
               AR(3,NN)=ZERO
               ISKIP   = ISKIP + 1
             ELSEIF(CR.EQ.2)THEN
               VR(2,NN)=ZERO
               AR(2,NN)=ZERO
               ISKIP   = ISKIP + 1
             ELSEIF(CR.EQ.3)THEN
               VR(2,NN)=ZERO
               VR(3,NN)=ZERO
               AR(2,NN)=ZERO
               AR(3,NN)=ZERO
               ISKIP   = ISKIP + 2
             ELSEIF(CR.EQ.4)THEN
               VR(1,NN)=ZERO
               AR(1,NN)=ZERO
               ISKIP   = ISKIP + 1
             ELSEIF(CR.EQ.5)THEN
               VR(1,NN)=ZERO
               VR(3,NN)=ZERO
               AR(1,NN)=ZERO
               AR(3,NN)=ZERO
               ISKIP   = ISKIP + 2
             ELSEIF(CR.EQ.6)THEN
               VR(1,NN)=ZERO
               VR(2,NN)=ZERO
               AR(1,NN)=ZERO
               AR(2,NN)=ZERO
               ISKIP   = ISKIP + 2
             ELSEIF(CR.EQ.7)THEN
               VR(1,NN)=ZERO
               VR(2,NN)=ZERO
               VR(3,NN)=ZERO
               AR(1,NN)=ZERO
               AR(2,NN)=ZERO
               AR(3,NN)=ZERO
               ISKIP   = ISKIP + 3
             ENDIF
           ELSE
C---         REPERE OBLIQUE
             IF(CT.EQ.1)THEN
               AA=SK(7,IS)*A(1,NN)+SK(8,IS)*A(2,NN)+SK(9,IS)*A(3,NN)
               VV=SK(7,IS)*V(1,NN)+SK(8,IS)*V(2,NN)+SK(9,IS)*V(3,NN)
               A(1,NN)=A(1,NN)-SK(7,IS)*AA
               A(2,NN)=A(2,NN)-SK(8,IS)*AA
               A(3,NN)=A(3,NN)-SK(9,IS)*AA
               V(1,NN)=V(1,NN)-SK(7,IS)*VV
               V(2,NN)=V(2,NN)-SK(8,IS)*VV
               V(3,NN)=V(3,NN)-SK(9,IS)*VV
             ELSEIF(CT.EQ.2)THEN
               AA=SK(4,IS)*A(1,NN)+SK(5,IS)*A(2,NN)+SK(6,IS)*A(3,NN)
               VV=SK(4,IS)*V(1,NN)+SK(5,IS)*V(2,NN)+SK(6,IS)*V(3,NN)
               A(1,NN)=A(1,NN)-SK(4,IS)*AA
               A(2,NN)=A(2,NN)-SK(5,IS)*AA
               A(3,NN)=A(3,NN)-SK(6,IS)*AA
               V(1,NN)=V(1,NN)-SK(4,IS)*VV
               V(2,NN)=V(2,NN)-SK(5,IS)*VV
               V(3,NN)=V(3,NN)-SK(6,IS)*VV
             ELSEIF(CT.EQ.3)THEN
               AA=SK(7,IS)*A(1,NN)+SK(8,IS)*A(2,NN)+SK(9,IS)*A(3,NN)
               VV=SK(7,IS)*V(1,NN)+SK(8,IS)*V(2,NN)+SK(9,IS)*V(3,NN)
               A(1,NN)=A(1,NN)-SK(7,IS)*AA
               A(2,NN)=A(2,NN)-SK(8,IS)*AA
               A(3,NN)=A(3,NN)-SK(9,IS)*AA
               V(1,NN)=V(1,NN)-SK(7,IS)*VV
               V(2,NN)=V(2,NN)-SK(8,IS)*VV
               V(3,NN)=V(3,NN)-SK(9,IS)*VV
               AA=SK(4,IS)*A(1,NN)+SK(5,IS)*A(2,NN)+SK(6,IS)*A(3,NN)
               VV=SK(4,IS)*V(1,NN)+SK(5,IS)*V(2,NN)+SK(6,IS)*V(3,NN)
               A(1,NN)=A(1,NN)-SK(4,IS)*AA
               A(2,NN)=A(2,NN)-SK(5,IS)*AA
               A(3,NN)=A(3,NN)-SK(6,IS)*AA
               V(1,NN)=V(1,NN)-SK(4,IS)*VV
               V(2,NN)=V(2,NN)-SK(5,IS)*VV
               V(3,NN)=V(3,NN)-SK(6,IS)*VV
             ELSEIF(CT.EQ.4)THEN
               AA  =SK(1,IS)*A(1,NN)+SK(2,IS)*A(2,NN)+SK(3,IS)*A(3,NN)
               VV  =SK(1,IS)*V(1,NN)+SK(2,IS)*V(2,NN)+SK(3,IS)*V(3,NN)
               A(1,NN)=A(1,NN)-SK(1,IS)*AA
               A(2,NN)=A(2,NN)-SK(2,IS)*AA
               A(3,NN)=A(3,NN)-SK(3,IS)*AA
               V(1,NN)=V(1,NN)-SK(1,IS)*VV
               V(2,NN)=V(2,NN)-SK(2,IS)*VV
               V(3,NN)=V(3,NN)-SK(3,IS)*VV
             ELSEIF(CT.EQ.5)THEN
               AA=SK(7,IS)*A(1,NN)+SK(8,IS)*A(2,NN)+SK(9,IS)*A(3,NN)
               VV=SK(7,IS)*V(1,NN)+SK(8,IS)*V(2,NN)+SK(9,IS)*V(3,NN)
               A(1,NN)=A(1,NN)-SK(7,IS)*AA
               A(2,NN)=A(2,NN)-SK(8,IS)*AA
               A(3,NN)=A(3,NN)-SK(9,IS)*AA
               V(1,NN)=V(1,NN)-SK(7,IS)*VV
               V(2,NN)=V(2,NN)-SK(8,IS)*VV
               V(3,NN)=V(3,NN)-SK(9,IS)*VV
               AA=SK(1,IS)*A(1,NN)+SK(2,IS)*A(2,NN)+SK(3,IS)*A(3,NN)
               VV=SK(1,IS)*V(1,NN)+SK(2,IS)*V(2,NN)+SK(3,IS)*V(3,NN)
               A(1,NN)=A(1,NN)-SK(1,IS)*AA
               A(2,NN)=A(2,NN)-SK(2,IS)*AA
               A(3,NN)=A(3,NN)-SK(3,IS)*AA
               V(1,NN)=V(1,NN)-SK(1,IS)*VV
               V(2,NN)=V(2,NN)-SK(2,IS)*VV
               V(3,NN)=V(3,NN)-SK(3,IS)*VV
             ELSEIF(CT.EQ.6)THEN
               AA=SK(1,IS)*A(1,NN)+SK(2,IS)*A(2,NN)+SK(3,IS)*A(3,NN)
               VV=SK(1,IS)*V(1,NN)+SK(2,IS)*V(2,NN)+SK(3,IS)*V(3,NN)
               A(1,NN)=A(1,NN)-SK(1,IS)*AA
               A(2,NN)=A(2,NN)-SK(2,IS)*AA
               A(3,NN)=A(3,NN)-SK(3,IS)*AA
               V(1,NN)=V(1,NN)-SK(1,IS)*VV
               V(2,NN)=V(2,NN)-SK(2,IS)*VV
               V(3,NN)=V(3,NN)-SK(3,IS)*VV
               AA=SK(4,IS)*A(1,NN)+SK(5,IS)*A(2,NN)+SK(6,IS)*A(3,NN)
               VV=SK(4,IS)*V(1,NN)+SK(5,IS)*V(2,NN)+SK(6,IS)*V(3,NN)
               A(1,NN)=A(1,NN)-SK(4,IS)*AA
               A(2,NN)=A(2,NN)-SK(5,IS)*AA
               A(3,NN)=A(3,NN)-SK(6,IS)*AA
               V(1,NN)=V(1,NN)-SK(4,IS)*VV
               V(2,NN)=V(2,NN)-SK(5,IS)*VV
               V(3,NN)=V(3,NN)-SK(6,IS)*VV
             ELSEIF(CT.EQ.7)THEN
               A(1,NN)=ZERO
               A(2,NN)=ZERO
               A(3,NN)=ZERO
               V(1,NN)=ZERO
               V(2,NN)=ZERO
               V(3,NN)=ZERO
             ENDIF
             IF(CT.EQ.1)THEN
               AA =SK(7,IS)*AR(1,NN)+SK(8,IS)*AR(2,NN)+SK(9,IS)*AR(3,NN)
               VV =SK(7,IS)*VR(1,NN)+SK(8,IS)*VR(2,NN)+SK(9,IS)*VR(3,NN)
               AR(1,NN)=AR(1,NN)-SK(7,IS)*AA
               AR(2,NN)=AR(2,NN)-SK(8,IS)*AA
               AR(3,NN)=AR(3,NN)-SK(9,IS)*AA
               VR(1,NN)=VR(1,NN)-SK(7,IS)*VV
               VR(2,NN)=VR(2,NN)-SK(8,IS)*VV
               VR(3,NN)=VR(3,NN)-SK(9,IS)*VV
             ELSEIF(CT.EQ.2)THEN
               AA =SK(4,IS)*AR(1,NN)+SK(5,IS)*AR(2,NN)+SK(6,IS)*AR(3,NN)
               VV =SK(4,IS)*VR(1,NN)+SK(5,IS)*VR(2,NN)+SK(6,IS)*VR(3,NN)
               AR(1,NN)=AR(1,NN)-SK(4,IS)*AA
               AR(2,NN)=AR(2,NN)-SK(5,IS)*AA
               AR(3,NN)=AR(3,NN)-SK(6,IS)*AA
               VR(1,NN)=VR(1,NN)-SK(4,IS)*VV
               VR(2,NN)=VR(2,NN)-SK(5,IS)*VV
               VR(3,NN)=VR(3,NN)-SK(6,IS)*VV
             ELSEIF(CT.EQ.3)THEN
               AA =SK(7,IS)*AR(1,NN)+SK(8,IS)*AR(2,NN)+SK(9,IS)*AR(3,NN)
               VV =SK(7,IS)*VR(1,NN)+SK(8,IS)*VR(2,NN)+SK(9,IS)*VR(3,NN)
               AR(1,NN)=AR(1,NN)-SK(7,IS)*AA
               AR(2,NN)=AR(2,NN)-SK(8,IS)*AA
               AR(3,NN)=AR(3,NN)-SK(9,IS)*AA
               VR(1,NN)=VR(1,NN)-SK(7,IS)*VV
               VR(2,NN)=VR(2,NN)-SK(8,IS)*VV
               VR(3,NN)=VR(3,NN)-SK(9,IS)*VV
               AA =SK(4,IS)*AR(1,NN)+SK(5,IS)*AR(2,NN)+SK(6,IS)*AR(3,NN)
               VV =SK(4,IS)*VR(1,NN)+SK(5,IS)*VR(2,NN)+SK(6,IS)*VR(3,NN)
               AR(1,NN)=AR(1,NN)-SK(4,IS)*AA
               AR(2,NN)=AR(2,NN)-SK(5,IS)*AA
               AR(3,NN)=AR(3,NN)-SK(6,IS)*AA
               VR(1,NN)=VR(1,NN)-SK(4,IS)*VV
               VR(2,NN)=VR(2,NN)-SK(5,IS)*VV
               VR(3,NN)=VR(3,NN)-SK(6,IS)*VV
             ELSEIF(CT.EQ.4)THEN
               AA =SK(1,IS)*AR(1,NN)+SK(2,IS)*AR(2,NN)+SK(3,IS)*AR(3,NN)
               VV =SK(1,IS)*VR(1,NN)+SK(2,IS)*VR(2,NN)+SK(3,IS)*VR(3,NN)
               AR(1,NN)=AR(1,NN)-SK(1,IS)*AA
               AR(2,NN)=AR(2,NN)-SK(2,IS)*AA
               AR(3,NN)=AR(3,NN)-SK(3,IS)*AA
               VR(1,NN)=VR(1,NN)-SK(1,IS)*VV
               VR(2,NN)=VR(2,NN)-SK(2,IS)*VV
               VR(3,NN)=VR(3,NN)-SK(3,IS)*VV
             ELSEIF(CT.EQ.5)THEN
               AA =SK(7,IS)*AR(1,NN)+SK(8,IS)*AR(2,NN)+SK(9,IS)*AR(3,NN)
               VV =SK(7,IS)*VR(1,NN)+SK(8,IS)*VR(2,NN)+SK(9,IS)*VR(3,NN)
               AR(1,NN)=AR(1,NN)-SK(7,IS)*AA
               AR(2,NN)=AR(2,NN)-SK(8,IS)*AA
               AR(3,NN)=AR(3,NN)-SK(9,IS)*AA
               VR(1,NN)=VR(1,NN)-SK(7,IS)*VV
               VR(2,NN)=VR(2,NN)-SK(8,IS)*VV
               VR(3,NN)=VR(3,NN)-SK(9,IS)*VV
               AA =SK(1,IS)*AR(1,NN)+SK(2,IS)*AR(2,NN)+SK(3,IS)*AR(3,NN)
               VV =SK(1,IS)*VR(1,NN)+SK(2,IS)*VR(2,NN)+SK(3,IS)*VR(3,NN)
               AR(1,NN)=AR(1,NN)-SK(1,IS)*AA
               AR(2,NN)=AR(2,NN)-SK(2,IS)*AA
               AR(3,NN)=AR(3,NN)-SK(3,IS)*AA
               VR(1,NN)=VR(1,NN)-SK(1,IS)*VV
               VR(2,NN)=VR(2,NN)-SK(2,IS)*VV
               VR(3,NN)=VR(3,NN)-SK(3,IS)*VV
             ELSEIF(CT.EQ.6)THEN
               AA =SK(1,IS)*AR(1,NN)+SK(2,IS)*AR(2,NN)+SK(3,IS)*AR(3,NN)
               VV =SK(1,IS)*VR(1,NN)+SK(2,IS)*VR(2,NN)+SK(3,IS)*VR(3,NN)
               AR(1,NN)=AR(1,NN)-SK(1,IS)*AA
               AR(2,NN)=AR(2,NN)-SK(2,IS)*AA
               AR(3,NN)=AR(3,NN)-SK(3,IS)*AA
               VR(1,NN)=VR(1,NN)-SK(1,IS)*VV
               VR(2,NN)=VR(2,NN)-SK(2,IS)*VV
               VR(3,NN)=VR(3,NN)-SK(3,IS)*VV
               AA =SK(4,IS)*AR(1,NN)+SK(5,IS)*AR(2,NN)+SK(6,IS)*AR(3,NN)
               VV =SK(4,IS)*VR(1,NN)+SK(5,IS)*VR(2,NN)+SK(6,IS)*VR(3,NN)
               AR(1,NN)=AR(1,NN)-SK(4,IS)*AA
               AR(2,NN)=AR(2,NN)-SK(5,IS)*AA
               AR(3,NN)=AR(3,NN)-SK(6,IS)*AA
               VR(1,NN)=VR(1,NN)-SK(4,IS)*VV
               VR(2,NN)=VR(2,NN)-SK(5,IS)*VV
               VR(3,NN)=VR(3,NN)-SK(6,IS)*VV
             ELSEIF(CT.EQ.7)THEN
               AR(1,NN)=ZERO
               AR(2,NN)=ZERO
               AR(3,NN)=ZERO
               VR(1,NN)=ZERO
               VR(2,NN)=ZERO
               VR(3,NN)=ZERO
             ENDIF
           ENDIF
         ENDIF
C---
        ENDDO
      ENDDO
C---
      RETURN
      END
