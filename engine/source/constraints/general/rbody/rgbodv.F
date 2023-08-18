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
Chd|  RGBODV                        source/constraints/general/rbody/rgbodv.F
Chd|-- called by -----------
Chd|        RBYVIT                        source/constraints/general/rbody/rbyvit.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RGBODV(V     ,VR   ,X     ,RBY ,NOD  ,
     2                  NBY   ,SKEW ,ISKEW ,FS  ,ITAB ,
     3                  WEIGHT,A    ,AR    ,MS  ,IN   ,
     4                  ISENS ,ID   ,IFAIL ,FNY ,EXPN ,
     5                  FTY   ,EXPT ,CRIT  ,NODREAC,FTHREAC ,
     6                  FREAC )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(*), NBY(*), ISKEW(*),ITAB(*), WEIGHT(*),ID, NODREAC(*)
      INTEGER, INTENT(IN) :: IFAIL
C     REAL
      my_real
     .   V(3,*), VR(3,*), X(3,*), RBY(*), SKEW(LSKEW,*), FS(*),
     .   A(3,*),AR(3,*),IN(*),MS(*),FREAC(6,*)
      my_real, INTENT(IN) :: 
     .   FNY,FTY,EXPN,EXPT
      my_real, INTENT(INOUT) :: 
     .   CRIT, FTHREAC(6,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "my_allocate.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER M, NSN, ICODR, ISK, I, N,ISENS, J, L
C     REAL
      my_real
     .   XDUM(3), VG(3), VI(3),
     .   V1X2, V2X1, V2X3, V3X2, V3X1, V1X3,USDT,DT05,
     .   VX1, VX2, VX3, MAS,AM1,AM2,AM3,VXX1,VXX2,VXX3,VY1
      my_real
     .   UX, UY, UZ, NN, FN, FT
      my_real, DIMENSION(:,:), ALLOCATABLE :: 
     .   R, RR
C-----------------------------------------------
      M    =NBY(1)
C optimisation spmd
      IF (M<0) RETURN
      NSN  =NBY(2)
C en spmd il ne faut sommer qu'une fois la vitesse du centre d'inertie
      FS(7)=FS(7)+VR(1,M)*DT2*WEIGHT(M)
      FS(8)=FS(8)+VR(2,M)*DT2*WEIGHT(M)
      FS(9)=FS(9)+VR(3,M)*DT2*WEIGHT(M)
C
      IF(DT2*DT2*(VR(1,M)**2+VR(2,M)**2+VR(3,M)**2)>1.0)THEN
         CALL ANCMSG(MSGID=110,ANMODE=ANINFO,
     .            I1=ID)
         CALL ARRET(2)
         RETURN
      ENDIF
C
      IF(N2D==0) THEN
        VG(1)=VR(1,M)+AR(1,M)*DT12
        VG(2)=VR(2,M)+AR(2,M)*DT12
        VG(3)=VR(3,M)+AR(3,M)*DT12
C
        USDT = ONE/DT12
C
        AM1 = A(1,M)
        AM2 = A(2,M)
        AM3 = A(3,M)
C   vectorisation si nsn>=10 ou si p/on active
C   sinon attention aux differences en mode vectoriel !
        IF (NSN>=10.OR.IPARIT>0) THEN
          DO I=1,NSN
            N = NOD(I)
            AR(1,N)= (VG(1)-VR(1,N)) * USDT
            AR(2,N)= (VG(2)-VR(2,N)) * USDT
            AR(3,N)= (VG(3)-VR(3,N)) * USDT
C
            V1X2=VG(1)*(X(2,N)-X(2,M))
            V2X1=VG(2)*(X(1,N)-X(1,M))
            V2X3=VG(2)*(X(3,N)-X(3,M))
            V3X2=VG(3)*(X(2,N)-X(2,M))
            V3X1=VG(3)*(X(1,N)-X(1,M))
            V1X3=VG(1)*(X(3,N)-X(3,M))
C  
            VX1=V2X3-V3X2
            VX2=V3X1-V1X3
            VX3=V1X2-V2X1

            A(1,N)= AM1 +
     .        (V(1,M)+VX1+HALF*DT2*(VG(2)*VX3-VG(3)*VX2)-V(1,N))*USDT
            A(2,N)= AM2 +
     .        (V(2,M)+VX2+HALF*DT2*(VG(3)*VX1-VG(1)*VX3)-V(2,N))*USDT
            A(3,N)= AM3 +
     .        (V(3,M)+VX3+HALF*DT2*(VG(1)*VX2-VG(2)*VX1)-V(3,N))*USDT
          ENDDO
        ELSE
          DO I=1,NSN
            N = NOD(I)
            AR(1,N)= (VG(1)-VR(1,N)) * USDT
            AR(2,N)= (VG(2)-VR(2,N)) * USDT
            AR(3,N)= (VG(3)-VR(3,N)) * USDT
C
            V1X2=VG(1)*(X(2,N)-X(2,M))
            V2X1=VG(2)*(X(1,N)-X(1,M))
            V2X3=VG(2)*(X(3,N)-X(3,M))
            V3X2=VG(3)*(X(2,N)-X(2,M))
            V3X1=VG(3)*(X(1,N)-X(1,M))
            V1X3=VG(1)*(X(3,N)-X(3,M))
C  
            VX1=V2X3-V3X2
            VX2=V3X1-V1X3
            VX3=V1X2-V2X1
            A(1,N)= AM1 +
     .        (V(1,M)+VX1+HALF*DT2*(VG(2)*VX3-VG(3)*VX2)-V(1,N))*USDT
            A(2,N)= AM2 +
     .        (V(2,M)+VX2+HALF*DT2*(VG(3)*VX1-VG(1)*VX3)-V(2,N))*USDT
            A(3,N)= AM3 +
     .        (V(3,M)+VX3+HALF*DT2*(VG(1)*VX2-VG(2)*VX1)-V(3,N))*USDT
          ENDDO
        ENDIF
      ELSEIF(N2D ==1) THEN
        VG(1)=ZERO
        VG(2)=ZERO
        VG(3)=VR(3,M)+AR(3,M)*DT12
C
        USDT = ONE/DT12
C
        AM1 = ZERO
        AM2 = A(2,M)
        AM3 = A(3,M)
C   vectorisation si nsn>=10 ou si p/on active
C   sinon attention aux differences en mode vectoriel !
        IF (NSN>=10.OR.IPARIT>0) THEN
         DO I=1,NSN
           N = NOD(I)
           AR(1,N)= ZERO
           AR(2,N)= ZERO
           AR(3,N)= (VG(3)-VR(3,N)) * USDT
C  
           VX1= VG(3)*(X(1,N)-X(1,M))
           VY1=-VG(3)*(X(2,N)-X(2,M))

           A(1,N)= AM1 + (V(1,M)+VY1-HALF*DT2*VG(3)*VX1-V(1,N))*USDT
           A(2,N)= AM2 + (V(2,M)+VX1+HALF*DT2*VG(3)*VY1-V(2,N))*USDT
           A(3,N)= AM3 + (V(3,M)-V(3,N))*USDT
         ENDDO
        ELSE
         DO I=1,NSN
           N = NOD(I)
           AR(1,N)= ZERO
           AR(2,N)= ZERO
           AR(3,N)= (VG(3)-VR(3,N)) * USDT
C  
           VX1= VG(3)*(X(1,N)-X(1,M))
           VY1=-VG(3)*(X(2,N)-X(2,M))

           A(1,N)= AM1 + (V(1,M)+VY1-HALF*DT2*VG(3)*VX1-V(1,N))*USDT
           A(2,N)= AM2 + (V(2,M)+VX1+HALF*DT2*VG(3)*VY1-V(2,N))*USDT
           A(3,N)= AM3 + (V(3,M)-V(3,N))*USDT
         ENDDO
        ENDIF
      ELSEIF(N2D ==2) THEN
        VG(1)=VR(1,M)+AR(1,M)*DT12
        VG(2)=ZERO
        VG(3)=ZERO
C
        USDT = ONE/DT12
C
        AM1 = ZERO
        AM2 = A(2,M)
        AM3 = A(3,M)
C   vectorisation si nsn>=10 ou si p/on active
C   sinon attention aux differences en mode vectoriel !
        IF (NSN>=10.OR.IPARIT>0) THEN
         DO I=1,NSN
           N = NOD(I)
           AR(1,N)= (VG(1)-VR(1,N)) * USDT
           AR(2,N)= ZERO
           AR(3,N)= ZERO
C  
           VX1=ZERO
           VX2=-VG(1)*(X(3,N)-X(3,M))
           VX3=VG(1)*(X(2,N)-X(2,M))
           VXX1=ZERO
           VXX2=-VG(1)*VG(1)*(X(2,N)-X(2,M))
           VXX3=-VG(1)*VG(1)*(X(3,N)-X(3,M))
  
           A(1,N)= ZERO
           A(2,N)= AM2 +
     .       (V(2,M)+VX2+HALF*DT2*VXX2-V(2,N))*USDT
           A(3,N)= AM3 +
     .       (V(3,M)+VX3+HALF*DT2*VXX3-V(3,N))*USDT
         ENDDO
        ELSE
         DO I=1,NSN
           N = NOD(I)
           AR(1,N)= (VG(1)-VR(1,N)) * USDT
           AR(2,N)= ZERO
           AR(3,N)= ZERO
C  
           VX1=ZERO
           VX2=-VG(1)*(X(3,N)-X(3,M))
           VX3=VG(1)*(X(2,N)-X(2,M))
           VXX1=ZERO
           VXX2=-VG(1)*VG(1)*(X(2,N)-X(2,M))
           VXX3=-VG(1)*VG(1)*(X(3,N)-X(3,M))
           A(1,N)= ZERO
           A(2,N)= AM2 +
     .       (V(2,M)+VX2+HALF*DT2*VXX2-V(2,N))*USDT
           A(3,N)= AM3 +
     .       (V(3,M)+VX3+HALF*DT2*VXX3-V(3,N))*USDT
         ENDDO
        ENDIF
      ENDIF
C-----------------------------------------------
      IF (IREAC == 0 .AND. IFAIL /= 1) RETURN
C-----------------------------------------------
C     Reaction forces and/or failure
C       Note : At this stage, Freac contains Fint+Fext+Fcont (Freac has not been finalized yet)
C-----------------------------------------------
      ALLOCATE(R (3,NSN))
      ALLOCATE(RR(3,NSN))
C
      DO I=1,NSN
        N = NOD(I)
C
C       Reaction force is also valid with zero mass or inertia
        R(1,I)  =  MS(N)*A(1,N)  - FREAC(1,N)
        R(2,I)  =  MS(N)*A(2,N)  - FREAC(2,N)
        R(3,I)  =  MS(N)*A(3,N)  - FREAC(3,N)
C
        RR(1,I) =  IN(N)*AR(1,N) - FREAC(4,N)
        RR(2,I) =  IN(N)*AR(2,N) - FREAC(5,N)
        RR(3,I) =  IN(N)*AR(3,N) - FREAC(6,N)
C
      ENDDO
C
      IF(IREAC/=0)THEN
        DO I=1,NSN
          N = NOD(I)
          L=NODREAC(N)
          IF(L/=0)THEN
            FTHREAC(1,L)=FTHREAC(1,L)+R(1,I)*DT12*WEIGHT(N) ! no other kin condition can apply to a secondary node 
            FTHREAC(2,L)=FTHREAC(2,L)+R(2,I)*DT12*WEIGHT(N)
            FTHREAC(3,L)=FTHREAC(3,L)+R(3,I)*DT12*WEIGHT(N)
            FTHREAC(4,L)=FTHREAC(4,L)+RR(1,I)*DT12*WEIGHT(N) ! no other kin condition can apply to a secondary node 
            FTHREAC(5,L)=FTHREAC(5,L)+RR(2,I)*DT12*WEIGHT(N)
            FTHREAC(6,L)=FTHREAC(6,L)+RR(3,I)*DT12*WEIGHT(N)
          END IF
        END DO
      END IF
C
      IF(IFAIL==1)THEN
        DO I=1,NSN
          N = NOD(I)
          UX    = X(1,N)-X(1,M)
          UY    = X(2,N)-X(2,M)
          UZ    = X(3,N)-X(3,M)
          NN    = ONE/MAX(EM20,SQRT(UX*UX+UY*UY+UZ*UZ))
          UX    = UX*NN
          UY    = UY*NN
          UZ    = UZ*NN
C
          FN    = R(1,I)*UX+R(2,I)*UY+R(3,I)*UZ
          R(1,I) = R(1,I)-FN*UX
          R(2,I) = R(2,I)-FN*UY
          R(3,I) = R(3,I)-FN*UZ
C     
          FN    = ABS(MIN(FN,ZERO)) ! Tension <=> Reaction force FN < 0
          FT    = SQRT(R(1,I)*R(1,I)+R(2,I)*R(2,I)+R(3,I)*R(3,I))
C
          CRIT  = MAX(CRIT,EXP(EXPN*LOG(MAX(EM20,FN/FNY)))+EXP(EXPT*LOG(MAX(EM20,FT/FTY)))) ! Max over secondary nodes and over time
        ENDDO
      END IF
C
      DEALLOCATE(R,RR)
C-----------------------------------------------
      RETURN
      END
