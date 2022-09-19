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
Chd|  BAFIL2                        source/ale/bimat/bafil2.F     
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|-- calls ---------------
Chd|        IDP_FREE                      source/system/machine.F       
Chd|        IDP_LOCK                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE BAFIL2(PM,V,W,FILL,DFILL,IMS,X,ALPH1,ALPH2,
     .     AIRE, PY1, PY2, PZ1, PZ2, DALPH1, DALPH2,
     .     MAT, NC1, NC2, NC3, NC4)
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
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IMS(NUMNOD,*), MAT(*), NC1(*), NC2(*), NC3(*), NC4(*)
      my_real
     .   PM(NPROPM,*), V(3,*), W(3,*), FILL(NUMNOD,*),
     .   DFILL(NUMNOD,*), X(3,*), ALPH1(*), ALPH2(*),
     .   AIRE(*), PY1(*), PY2(*), PZ1(*), PZ2(*), DALPH1(*), DALPH2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N1, N2, N3, N4,
     .   NP
      my_real
!     .   GAMMA(MVSIZ),
!     .   XMS(MVSIZ),
     .   FI1(MVSIZ), FI2(MVSIZ), FI3(MVSIZ), FI4(MVSIZ),
     .   FA(MVSIZ), VDY1(MVSIZ), VDY2(MVSIZ), VDY3(MVSIZ), VDY4(MVSIZ),
     .   VDZ1(MVSIZ), VDZ2(MVSIZ), VDZ3(MVSIZ), VDZ4(MVSIZ), VDY(MVSIZ), VDZ(MVSIZ),
     .   ABF, DN, P1, P2, P3, P4, PT, PSY, PSZ, PTY,
     .   PTZ, PS, PST, PTS, DS0, DT0, DS, DT,
     .   DF1(MVSIZ), DF2(MVSIZ), DF3(MVSIZ), DF4(MVSIZ)
C-----------------------------------------------
!      EQUIVALENCE (GAMMA,BV(1,61)),(XMS,BV(1,62))
C-------------------------------
!      DO I=LFT,LLT
!        XMS(I)  =HALF/(PY1(I)*PZ2(I)-PY2(I)*PZ1(I))
!        GAMMA(I)= ONE
!      ENDDO
C-------------------------------
      DO I=LFT,LLT
        FI1(I)=FILL(NC1(I),1)
        FI2(I)=FILL(NC2(I),1)
        FI3(I)=FILL(NC3(I),1)
        FI4(I)=FILL(NC4(I),1)
        ABF=ABS(FI1(I))+ABS(FI2(I))+ABS(FI3(I))+ABS(FI4(I))
        N1=SIGN(ONE,FI1(I))
        N2=SIGN(ONE,FI2(I))
        N3=SIGN(ONE,FI3(I))
        N4=SIGN(ONE,FI4(I))
        NP=MAX(0,N1)+MAX(0,N2)+MAX(0,N3)+MAX(0,N4)
        DN=DT1*NP
        IF(DN/=ZERO)THEN
         FA(I)=-DALPH1(I)*ABF/DN
        ELSE
         FA(I)=ZERO
        ENDIF
      ENDDO
C-----------------------------------------------
C     CALCUL PAR NOEUD DE V MATIERE - V MAILLAGE
C-----------------------------------------------
      DO I=LFT,LLT
        VDY1(I)=V(2,NC1(I)) - W(2,NC1(I))
        VDZ1(I)=V(3,NC1(I)) - W(3,NC1(I))

        VDY2(I)=V(2,NC2(I)) - W(2,NC2(I))
        VDZ2(I)=V(3,NC2(I)) - W(3,NC2(I))

        VDY3(I)=V(2,NC3(I)) - W(2,NC3(I))
        VDZ3(I)=V(3,NC3(I)) - W(3,NC3(I))

        VDY4(I)=V(2,NC4(I)) - W(2,NC4(I))
        VDZ4(I)=V(3,NC4(I)) - W(3,NC4(I))
      ENDDO
C-----------------------------------------------
C     CALCUL DE (V MATIERE - V MAILLAGE) MOYEN
C-----------------------------------------------
      DO I=LFT,LLT
        P1=FI1(I) + ONE
        P2=FI2(I) + ONE
        P3=FI3(I) + ONE
        P4=FI4(I) + ONE
        PT=(P1+P2+P3+P4)
        PT=MAX(EM15,PT)
        VDY(I)=(VDY1(I)*P1+VDY2(I)*P2+VDY3(I)*P3+VDY4(I)*P4)/PT
        VDZ(I)=(VDZ1(I)*P1+VDZ2(I)*P2+VDZ3(I)*P3+VDZ4(I)*P4)/PT
      ENDDO

      DO I=LFT,LLT
        PSY=-X(2,NC1(I))+X(2,NC2(I))+X(2,NC3(I))-X(2,NC4(I))
        PSZ=-X(3,NC1(I))+X(3,NC2(I))+X(3,NC3(I))-X(3,NC4(I))
        PTY=-X(2,NC1(I))-X(2,NC2(I))+X(2,NC3(I))+X(2,NC4(I))
        PTZ=-X(3,NC1(I))-X(3,NC2(I))+X(3,NC3(I))+X(3,NC4(I))
        PS=SQRT(PSY**2+PSZ**2)
        PT=SQRT(PTY**2+PTZ**2)
        PST=PSY*PTZ-PSZ*PTY
        PTS=-PST
        DS0=-FOUR*(PTY*VDZ(I)-PTZ*VDY(I))/PTS
        DT0=-FOUR*(PSY*VDZ(I)-PSZ*VDY(I))/PST
        IF(FI1(I)>=ZERO)THEN
         DS=-FOUR*(PTY*VDZ1(I)-PTZ*VDY1(I))/PTS
         DT=-FOUR*(PSY*VDZ1(I)-PSZ*VDY1(I))/PST
        ELSE
         DS=DS0
         DT=DT0
        ENDIF
        DS=MAX(ZERO,TWO*DS)
        DT=MAX(ZERO,TWO*DT)
        DF1(I)=FOURTH*((-TWO*DS-TWO*DT+DS*DT*DT1)*FI1(I)
     .              + (         TWO*DS-DS*DT*DT1)*FI2(I)
     .              + (                 DS*DT*DT1)*FI3(I)
     .              + (         TWO*DT-DS*DT*DT1)*FI4(I) )
        IF(FI2(I)>=ZERO)THEN
         DS=-FOUR*(PTY*VDZ2(I)-PTZ*VDY2(I))/PTS
         DT=-FOUR*(PSY*VDZ2(I)-PSZ*VDY2(I))/PST
        ELSE
         DS=DS0
         DT=DT0
        ENDIF
        DS=MIN(ZERO,TWO*DS)
        DT=MAX(ZERO,TWO*DT)
           DF2(I)=FOURTH*((        -TWO*DS+DS*DT*DT1)*FI1(I)
     .                 + ( TWO*DS-TWO*DT-DS*DT*DT1)*FI2(I)
     .                 + (        +TWO*DT+DS*DT*DT1)*FI3(I)
     .                 + (                -DS*DT*DT1)*FI4(I) )
        IF(FI3(I)>=ZERO)THEN
         DS=-FOUR*(PTY*VDZ3(I)-PTZ*VDY3(I))/PTS
         DT=-FOUR*(PSY*VDZ3(I)-PSZ*VDY3(I))/PST
        ELSE
         DS=DS0
         DT=DT0
        ENDIF
        DS=MIN(ZERO,TWO*DS)
        DT=MIN(ZERO,TWO*DT)
        DF3(I)=FOURTH*((                +DS*DT*DT1)*FI1(I)
     .              + (        -TWO*DT-DS*DT*DT1)*FI2(I)
     .              + (+TWO*DS+TWO*DT+DS*DT*DT1)*FI3(I)
     .              + (-TWO*DS        -DS*DT*DT1)*FI4(I) )
        IF(FI4(I)>=ZERO)THEN
         DS=-FOUR*(PTY*VDZ4(I)-PTZ*VDY4(I))/PTS
         DT=-FOUR*(PSY*VDZ4(I)-PSZ*VDY4(I))/PST
        ELSE
         DS=DS0
         DT=DT0
        ENDIF
        DS=MAX(ZERO,TWO*DS)
        DT=MIN(ZERO,TWO*DT)
        DF4(I)=FOURTH*((        -TWO*DT+DS*DT*DT1)*FI1(I)
     .              + (                -DS*DT*DT1)*FI2(I)
     .              + (+TWO*DS        +DS*DT*DT1)*FI3(I)
     .              + (-TWO*DS+TWO*DT-DS*DT*DT1)*FI4(I) )
      ENDDO !next I

      CALL IDP_LOCK(2)

      DO I = LFT,LLT
        DFILL(NC1(I),1)=DFILL(NC1(I),1)+DF1(I)-FA(I)
        DFILL(NC2(I),1)=DFILL(NC2(I),1)+DF2(I)-FA(I)
        DFILL(NC3(I),1)=DFILL(NC3(I),1)+DF3(I)-FA(I)
        DFILL(NC4(I),1)=DFILL(NC4(I),1)+DF4(I)-FA(I)
        IMS(NC1(I),1)=IMS(NC1(I),1)+1
        IMS(NC2(I),1)=IMS(NC2(I),1)+1
        IMS(NC3(I),1)=IMS(NC3(I),1)+1
        IMS(NC4(I),1)=IMS(NC4(I),1)+1
      ENDDO

      CALL IDP_FREE(2)
C
      IF(JMULT>1)THEN
C-------------------------------
      DO I=LFT,LLT
        FI1(I)=FILL(NC1(I),2)
        FI2(I)=FILL(NC2(I),2)
        FI3(I)=FILL(NC3(I),2)
        FI4(I)=FILL(NC4(I),2)
        ABF=ABS(FI1(I))+ABS(FI2(I))+ABS(FI3(I))+ABS(FI4(I))
        N1=SIGN(ONE,FI1(I))
        N2=SIGN(ONE,FI2(I))
        N3=SIGN(ONE,FI3(I))
        N4=SIGN(ONE,FI4(I))
        NP=MAX(0,N1)+MAX(0,N2)+MAX(0,N3)+MAX(0,N4)
        DN=DT1*NP
        IF(DN/=ZERO)THEN
         FA(I)=-DALPH2(I)*ABF/DN
        ELSE
         FA(I)=ZERO
        ENDIF
      ENDDO
C-----------------------------------------------
C     CALCUL DE (V MATIERE - V MAILLAGE) MOYEN
C-----------------------------------------------
      DO I=LFT,LLT
        P1=FI1(I) + ONE
        P2=FI2(I) + ONE
        P3=FI3(I) + ONE
        P4=FI4(I) + ONE
        PT=(P1+P2+P3+P4)
        PT=MAX(EM15,PT)
        VDY(I)=(VDY1(I)*P1+VDY2(I)*P2+VDY3(I)*P3+VDY4(I)*P4)/PT
        VDZ(I)=(VDZ1(I)*P1+VDZ2(I)*P2+VDZ3(I)*P3+VDZ4(I)*P4)/PT
      ENDDO

      DO I=LFT,LLT
        PSY=-X(2,NC1(I))+X(2,NC2(I))+X(2,NC3(I))-X(2,NC4(I))
        PSZ=-X(3,NC1(I))+X(3,NC2(I))+X(3,NC3(I))-X(3,NC4(I))
        PTY=-X(2,NC1(I))-X(2,NC2(I))+X(2,NC3(I))+X(2,NC4(I))
        PTZ=-X(3,NC1(I))-X(3,NC2(I))+X(3,NC3(I))+X(3,NC4(I))
        PS=SQRT(PSY**2+PSZ**2)
        PT=SQRT(PTY**2+PTZ**2)
        PST=PSY*PTZ-PSZ*PTY
        PTS=-PST
        DS0=-FOUR*(PTY*VDZ(I)-PTZ*VDY(I))/PTS
        DT0=-FOUR*(PSY*VDZ(I)-PSZ*VDY(I))/PST
        IF(FI1(I)>=ZERO)THEN
         DS=-FOUR*(PTY*VDZ1(I)-PTZ*VDY1(I))/PTS
         DT=-FOUR*(PSY*VDZ1(I)-PSZ*VDY1(I))/PST
        ELSE
         DS=DS0
         DT=DT0
        ENDIF
        DS=MAX(ZERO,TWO*DS)
        DT=MAX(ZERO,TWO*DT)
        DF1(I)=FOURTH*((-TWO*DS-TWO*DT+DS*DT*DT1)*FI1(I)
     .              +( TWO*DS         -DS*DT*DT1)*FI2(I)
     .              +(                  DS*DT*DT1)*FI3(I)
     .              +(          TWO*DT-DS*DT*DT1)*FI4(I) )
        IF(FI2(I)>=ZERO)THEN
         DS=-FOUR*(PTY*VDZ2(I)-PTZ*VDY2(I))/PTS
         DT=-FOUR*(PSY*VDZ2(I)-PSZ*VDY2(I))/PST
        ELSE
         DS=DS0
         DT=DT0
        ENDIF
        DS=MIN(ZERO,TWO*DS)
        DT=MAX(ZERO,TWO*DT)
        DF2(I)=FOURTH*((-TWO*DS       +DS*DT*DT1)*FI1(I)
     .              +( TWO*DS-TWO*DT-DS*DT*DT1)*FI2(I)
     .              +(        +TWO*DT+DS*DT*DT1)*FI3(I)
     .              +(                -DS*DT*DT1)*FI4(I) )
        IF(FI3(I)>=ZERO)THEN
         DS=-FOUR*(PTY*VDZ3(I)-PTZ*VDY3(I))/PTS
         DT=-FOUR*(PSY*VDZ3(I)-PSZ*VDY3(I))/PST
        ELSE
         DS=DS0
         DT=DT0
        ENDIF
        DS=MIN(ZERO,TWO*DS)
        DT=MIN(ZERO,TWO*DT)
        DF3(I)=FOURTH*((               +DS*DT*DT1)*FI1(I)
     .              +(        -TWO*DT-DS*DT*DT1)*FI2(I)
     .              +(+TWO*DS+TWO*DT+DS*DT*DT1)*FI3(I)
     .              +(-TWO*DS        -DS*DT*DT1)*FI4(I) )
        IF(FI4(I)>=ZERO)THEN
         DS=-FOUR*(PTY*VDZ4(I)-PTZ*VDY4(I))/PTS
         DT=-FOUR*(PSY*VDZ4(I)-PSZ*VDY4(I))/PST
        ELSE
         DS=DS0
         DT=DT0
        ENDIF
        DS=MAX(ZERO,TWO*DS)
        DT=MIN(ZERO,TWO*DT)
           DF4(I)=FOURTH*((       -TWO*DT+DS*DT*DT1)*FI1(I)
     .                 +(                -DS*DT*DT1)*FI2(I)
     .                 +(+TWO*DS        +DS*DT*DT1)*FI3(I)
     .                 +(-TWO*DS+TWO*DT-DS*DT*DT1)*FI4(I) )
      ENDDO !next I

      CALL IDP_LOCK(2)

      DO I = LFT,LLT
        DFILL(NC1(I),2)=DFILL(NC1(I),2)+DF1(I)-FA(I)
        DFILL(NC2(I),2)=DFILL(NC2(I),2)+DF2(I)-FA(I)
        DFILL(NC3(I),2)=DFILL(NC3(I),2)+DF3(I)-FA(I)
        DFILL(NC4(I),2)=DFILL(NC4(I),2)+DF4(I)-FA(I)
        IMS(NC1(I),2)=IMS(NC1(I),2)+1
        IMS(NC2(I),2)=IMS(NC2(I),2)+1
        IMS(NC3(I),2)=IMS(NC3(I),2)+1
        IMS(NC4(I),2)=IMS(NC4(I),2)+1
      ENDDO

      CALL IDP_FREE(2)
C-------------------------------
      ENDIF
C
      RETURN
      END
