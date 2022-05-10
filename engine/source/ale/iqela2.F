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
Chd|  IQELA2                        source/ale/iqela2.F           
Chd|-- called by -----------
Chd|        INTAL2                        source/ale/intal2.F           
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        BCS2                          source/constraints/general/bcs/bcs2.F
Chd|        BCS4                          source/ale/bcs4.F             
Chd|        SHAPEH                        source/ale/shapeh.F           
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IQELA2(
     1                  X,SKEW,A,E,MSM,IRECT,LMSR,CRST,MSR,NSV,
     2                  ILOC,IRTL,LCODE,V,ISKEW,NOR,ITAB
     .                  )
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), LMSR(4,*), MSR(*), NSV(*), ILOC(*), IRTL(*),
     .   LCODE(*), ISKEW(*), ITAB(*)
      my_real
     .   X(3,*), SKEW(LSKEW,*), A(*), E(*), MSM(*), CRST(2,*), V(*),
     .   NOR(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr04_c.inc"
#include      "scr08_s_c.inc"
#include      "scr08_a_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JBC(3), NIR, I, J, I3, J3, I2, J2, I1, J1, ISK, LCOD, II,
     .   L, JJ, NN, LK, IBC
      my_real
     .   XN(3), YN(3), ZN(3), H(4), N1, N2, N3, SS, TT, AX, AY, AZ,
     .   VX, VY, VZ, AMN, VMN, AMOD, VMOD, BVZ, BAZ, BVX, BVY, BAX, BAY,
     .   A11, A12, A13, A21, A22, A23, A31, A32, A33, DET
C-----------------------------------------------
C
      NIR=2
      IF(N2D.EQ.0)NIR=4
C-------------------------------------
C     ACCELERATIONS DES NOEUDS MASTERS
C-------------------------------------
      DO I=1,NMN
        J=MSR(I)
        I3=3*I
        J3=3*J
        I2=I3-1
        J2=J3-1
        I1=I2-1
        J1=J2-1
        IF(MSM(I).GT.ZERO)THEN
         A(J1)=A(J1)+E(I1)/MSM(I)
         A(J2)=A(J2)+E(I2)/MSM(I)
         A(J3)=A(J3)+E(I3)/MSM(I)
        ENDIF
        ISK  = ISKEW(J)
        LCOD = LCODE(J)
        IF(LCOD>0)CALL BCS2(A(J1),SKEW(1,ISK),ISK,LCOD)
      ENDDO
C------------------------------------
C     ACCELERATIONS DES NOEUDS SLAVES
C------------------------------------
      DO II=1,NSN
        I=NSV(II)
        L=IRTL(II)
        DO JJ=1,NIR
          NN=IRECT(JJ,L)
          IX(JJ)=MSR(NN)
        ENDDO !NEXT JJ
        SS=CRST(1,II)
        TT=CRST(2,II)
        N1=NOR(1,II)
        N2=NOR(2,II)
        N3=NOR(3,II)
        CALL SHAPEH(H,SS,TT)
        I3=3*I
        I2=I3-1
        I1=I2-1
        AX=ZERO
        AY=ZERO
        AZ=ZERO
        VX=ZERO
        VY=ZERO
        VZ=ZERO
        DO JJ=1,NIR
          J3=3*IX(JJ)
          J2=J3-1
          J1=J2-1
          AX=AX+A(J1)*H(JJ)
          AY=AY+A(J2)*H(JJ)
          AZ=AZ+A(J3)*H(JJ)
          VX=VX+V(J1)*H(JJ)
          VY=VY+V(J2)*H(JJ)
          VZ=VZ+V(J3)*H(JJ)
        ENDDO !NEXT JJ

        AMN = N1*AX+N2*AY+N3*AZ
        VMN = N1*VX+N2*VY+N3*VZ
        AMOD= AMN-N1*A(I1)-N2*A(I2)-N3*A(I3)
        VMOD= VMN-N1*V(I1)-N2*V(I2)-N3*V(I3)

        LCOD=LCODE(I)
      
        IF(LCOD.NE.0)THEN
C--------------------------------
C     NOEUD AVEC CONDITION LIMITE
C--------------------------------
          XN(1)=N1
          YN(1)=N2
          ZN(1)=N3

          CALL BCS4(JBC,LCODE(I))
          LK=ISKEW(I)
          IBC=2

          IF(JBC(1).NE.0)THEN
           XN(IBC)=SKEW(1,LK)
           YN(IBC)=SKEW(2,LK)
           ZN(IBC)=SKEW(3,LK)
           IBC=IBC+1
          ENDIF
          IF(JBC(2).NE.0)THEN
           XN(IBC)=SKEW(4,LK)
           YN(IBC)=SKEW(5,LK)
           ZN(IBC)=SKEW(6,LK)
           IBC=IBC+1
          ENDIF
          IF(JBC(3).NE.0)THEN
           IF(IBC.EQ.4)THEN
             !C------------------------------------------------
             !C      CE TEST DEVRAIT ETRE FAIT DANS LE STARTER
             !C-----------------------------------------------------
             CALL ANCMSG(MSGID=11,ANMODE=ANINFO,
     .                   I1=ITAB(I))
             !CALL ARRET(2)
             IERR=1
             EXIT
           ELSE
             XN(IBC)=SKEW(7,LK)
             YN(IBC)=SKEW(8,LK)
             ZN(IBC)=SKEW(9,LK)
             IBC=IBC+1
           ENDIF
          ENDIF
          IF(IBC.EQ.3)THEN
           !C---------------------------
           !C     NOEUD AVEC 1 CONDITION
           !C---------------------------
           XN(3)=YN(1)*ZN(2)-ZN(1)*YN(2)
           YN(3)=ZN(1)*XN(2)-XN(1)*ZN(2)
           ZN(3)=XN(1)*YN(2)-YN(1)*XN(2)
           BVZ=V(I1)*XN(3)+V(I2)*YN(3)+V(I3)*ZN(3)
           BAZ=A(I1)*XN(3)+A(I2)*YN(3)+A(I3)*ZN(3)
          ELSE
           !C-----------------------------
           !C     NOEUD AVEC 2 CONDITIONS
           !C-----------------------------
           BVZ=ZERO
           BAZ=ZERO
          ENDIF

           BVX=VMN
           BVY=ZERO
           BAX=AMN
           BAY=ZERO

          A11=YN(2)*ZN(3)-ZN(2)*YN(3)
          A12=ZN(2)*XN(3)-XN(2)*ZN(3)
          A13=XN(2)*YN(3)-YN(2)*XN(3)
          A21=YN(3)*ZN(1)-ZN(3)*YN(1)
          A22=ZN(3)*XN(1)-XN(3)*ZN(1)
          A23=XN(3)*YN(1)-YN(3)*XN(1)
          A31=YN(1)*ZN(2)-ZN(1)*YN(2)
          A32=ZN(1)*XN(2)-XN(1)*ZN(2)
          A33=XN(1)*YN(2)-YN(1)*XN(2)
          DET=XN(1)*A11+YN(1)*A12+ZN(1)*A13

          !C-----------------------------------
          !C     CALCUL VITESSE ET ACCELERATION
          !C-----------------------------------
          !IF(DET.EQ.ZERO)GOTO 100
          IF(DET.NE.ZERO)THEN
           V(I1)=(A11*BVX+A21*BVY+A31*BVZ)/DET
           V(I2)=(A12*BVX+A22*BVY+A32*BVZ)/DET
           V(I3)=(A13*BVX+A23*BVY+A33*BVZ)/DET
           A(I1)=(A11*BAX+A21*BAY+A31*BAZ)/DET
           A(I2)=(A12*BAX+A22*BAY+A32*BAZ)/DET
           A(I3)=(A13*BAX+A23*BAY+A33*BAZ)/DET
          ENDIF
      
        ELSEIF(LCOD.EQ.0)THEN
C--------------------------------
C     NOEUD SANS CONDITION LIMITE
C--------------------------------
           A(I1)=A(I1)+AMOD*N1
           A(I2)=A(I2)+AMOD*N2
           A(I3)=A(I3)+AMOD*N3
           V(I1)=V(I1)+VMOD*N1
           V(I2)=V(I2)+VMOD*N2
           V(I3)=V(I3)+VMOD*N3
          !GO TO 100      
        ENDIF
      
      ENDDO  !100   !NEXT II
C--------------------------------  
      RETURN
      END
