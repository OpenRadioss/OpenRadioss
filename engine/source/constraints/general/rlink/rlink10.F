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
Chd|  RLINK10                       source/constraints/general/rlink/rlink10.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        RLINK1                        source/constraints/general/rlink/rlink1.F
Chd|        RLINK2                        source/constraints/general/rlink/rlink2.F
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|====================================================================
      SUBROUTINE RLINK10(
     1   MS    ,IN    ,A     ,AR   ,V    ,
     2   VR    ,NLINK ,LLINK ,SKEW ,FR_RL,
     3   WEIGHT,FRL6  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLINK(*),LLINK(*),FR_RL(NSPMD+2,*),WEIGHT(*)
      my_real
     .   MS(*), IN(*), A(3,*), AR(3,*), V(3,*), VR(3,*), SKEW(LSKEW,*)
      DOUBLE PRECISION FRL6(15,6,NRLINK)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, K1, N, ISK, I, KIND(NRLINK)
C-----------------------------------------------
C
C Pre calcul index
C
          K = 1
          DO I = 1, NRLINK
            KIND(I) = K
            K = K + NLINK(4*I-3)
          ENDDO
C
          K1=1

C boucle parallele sur les threads SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
          DO N=1,NRLINK
            K1=4*N-3
            K = KIND(N)
            ISK=NLINK(K1+3)
            IF(ISK==1)THEN
              CALL RLINK1(
     1          MS         ,IN         ,A       ,AR    ,NLINK(K1) ,
     2          NLINK(K1+1),NLINK(K1+2),LLINK(K),WEIGHT,FRL6(1,1,N),
     3          1          )
            ELSE
              CALL RLINK2(
     1          MS         ,IN       ,A          ,AR         ,V       ,
     2          VR         ,NLINK(K1),NLINK(K1+1),NLINK(K1+2),LLINK(K),
     3          SKEW(1,ISK),WEIGHT   ,FRL6(1,1,N),1          )
            ENDIF
          END DO
!$OMP END DO

          IF(NSPMD > 1) THEN
!$OMP SINGLE

C routine appelee rlink par rlink a optimiser si besoin

            DO N=1,NRLINK
C routine de calcul Parith/ON de A AR MS IN rigid link
              IF(FR_RL(ISPMD+1,N)/=0)
     +          CALL SPMD_EXCH_FR6(FR_RL(1,N),FRL6(1,1,N),15*6)
            END DO

!$OMP END SINGLE
          END IF

C boucle parallele sur les threads SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
          DO N=1,NRLINK
            K1=4*N-3
            K = KIND(N)
c            DO I=1,N-1
c              K=K+NLINK(4*I-3)
c            ENDDO
            ISK=NLINK(K1+3)
            IF(ISK==1)THEN
              CALL RLINK1(
     1          MS         ,IN         ,A       ,AR    ,NLINK(K1) ,
     2          NLINK(K1+1),NLINK(K1+2),LLINK(K),WEIGHT,FRL6(1,1,N),
     3          2          )
            ELSE
              CALL RLINK2(
     1          MS         ,IN       ,A          ,AR         ,V       ,
     2          VR         ,NLINK(K1),NLINK(K1+1),NLINK(K1+2),LLINK(K),
     3          SKEW(1,ISK),WEIGHT   ,FRL6(1,1,N),2          )
            ENDIF
          END DO
!$OMP END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  RLINK11                       source/constraints/general/rlink/rlink10.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        RLINK1                        source/constraints/general/rlink/rlink1.F
Chd|        RLINK2                        source/constraints/general/rlink/rlink2.F
Chd|        RLINK3                        source/constraints/general/rlink/rlink10.F
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|====================================================================
      SUBROUTINE RLINK11(
     1   MS    ,IN    ,A     ,AR    ,V     ,
     2   VR    ,NNLINK,LLLINK,SKEW  ,FR_LL ,
     3   WEIGHT,FRL6  ,X     ,XFRAME       )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNLINK(10,*), LLLINK(*), FR_LL(NSPMD+2,*),
     .        WEIGHT(*)
      my_real
     .   MS(*), IN(*), A(3,*), AR(3,*), V(3,*), VR(3,*), SKEW(LSKEW,*),
     .   XFRAME(NXFRAME,*), X(3,*)
      DOUBLE PRECISION FRL6(15,6,NLINK)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, K1, N, ISK, I,IC,ICR, IPOL, KIND(NLINK)
C-----------------------------------------------
C
C Pre calcul index
C
          K = 1
          DO I = 1, NLINK
            KIND(I) = K
            K = K + NNLINK(1,I)
          ENDDO
C
          K1=1

C boucle parallele sur les threads SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
          DO N=1,NLINK
c            K = 1
c            DO I=1,N-1
c              K=K+NNLINK(1,I)
c            ENDDO
            ISK=NNLINK(5,N)
            IPOL=NNLINK(6,N)
            K = KIND(N)
            IPOL=NNLINK(6,N)
            IF(IPOL==1)THEN
               CALL RLINK3(
     1      MS       ,IN           ,X          ,A          ,AR         ,
     2      V        ,VR           ,NNLINK(1,N),NNLINK(3,N),NNLINK(4,N),
     3      LLLINK(K),XFRAME(1,ISK),WEIGHT     ,FRL6(1,1,N),1          )
            ELSEIF(ISK==1)THEN
               CALL RLINK1(
     1      MS         ,IN         ,A        ,AR     ,NNLINK(1,N),
     2      NNLINK(3,N),NNLINK(4,N),LLLINK(K),WEIGHT ,FRL6(1,1,N),
     3      1)
            ELSE
               CALL RLINK2(
     1      MS         ,IN         ,A          ,AR         ,V        ,
     2      VR         ,NNLINK(1,N),NNLINK(3,N),NNLINK(4,N),LLLINK(K),
     3      SKEW(1,ISK),WEIGHT     ,FRL6(1,1,N),1          )
            ENDIF
          END DO
!$OMP END DO

          IF(NSPMD > 1) THEN
!$OMP SINGLE

C routine appelee rlink par rlink a optimiser si besoin

            DO N=1,NLINK
C routine de calcul Parith/ON de A AR MS IN rigid link
              IF(FR_LL(ISPMD+1,N)/=0)
     +          CALL SPMD_EXCH_FR6(FR_LL(1,N),FRL6(1,1,N),15*6)
            END DO

!$OMP END SINGLE
          END IF

C boucle parallele sur les threads SMP
!$OMP DO SCHEDULE(DYNAMIC,1)
          DO N=1,NLINK
            ISK=NNLINK(5,N)
            IPOL=NNLINK(6,N)
            K = KIND(N)
            IPOL=NNLINK(6,N)
            IF(IPOL==1)THEN
               CALL RLINK3(
     1      MS       ,IN           ,X          ,A          ,AR         ,
     2      V        ,VR           ,NNLINK(1,N),NNLINK(3,N),NNLINK(4,N),
     3      LLLINK(K),XFRAME(1,ISK),WEIGHT     ,FRL6(1,1,N),2          )
            ELSEIF(ISK==1)THEN
              CALL RLINK1(
     1      MS         ,IN         ,A        ,AR     ,NNLINK(1,N),
     2      NNLINK(3,N),NNLINK(4,N),LLLINK(K),WEIGHT ,FRL6(1,1,N),
     3      2          )
            ELSE
               CALL RLINK2(
     1      MS         ,IN         ,A          ,AR         ,V        ,
     2      VR         ,NNLINK(1,N),NNLINK(3,N),NNLINK(4,N),LLLINK(K),
     3      SKEW(1,ISK),WEIGHT     ,FRL6(1,1,N),2          )
            ENDIF
          END DO
!$OMP END DO

C
      RETURN
      END

Chd|====================================================================
Chd|  RLINK3                        source/constraints/general/rlink/rlink10.F
Chd|-- called by -----------
Chd|        RLINK11                       source/constraints/general/rlink/rlink10.F
Chd|-- calls ---------------
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE RLINK3(MS ,IN    ,X     ,A    ,AR   ,
     2                  V  ,VR    ,NSN   ,IC   ,ICR  ,
     3                  NOD,XFRAME,WEIGHT,FRL6 ,IFLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, IC, ICR, IFLAG
      INTEGER NOD(*),WEIGHT(*)
C     REAL
      my_real
     .   MS(*), IN(*), X(3,*), A(3,*), AR(3,*), V(3,*), VR(3,*),
     .   XFRAME(*)
      DOUBLE PRECISION FRL6(15,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IC1, ICC, IC2, IC3, I, N, K
C     REAL
      my_real
     .   MASS, INER, AX, AY, AZ, VX, VY, VZ, DAX, DAY, DAZ, AAX, AAY,
     .   AAZ, DVX, DVY, DVZ, VVX, VVY, VVZ, RX, RY, RZ, SX, SY, SZ,
     .   TX, TY, TZ, OX, OY, OZ, AA, MR2,VTMR2,ATMR2,RX0, RY0, RZ0,R,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN), F5(NSN), F6(NSN), F7(NSN)
C-----------------------------------------------
C
      IF(IFLAG == 1)THEN

C Init Parith/ON
       DO K = 1, 6
         FRL6(1,K) = ZERO
         FRL6(2,K) = ZERO
         FRL6(3,K) = ZERO
         FRL6(4,K) = ZERO
         FRL6(5,K) = ZERO
         FRL6(6,K) = ZERO
         FRL6(7,K) = ZERO
         FRL6(8,K) = ZERO
         FRL6(9,K)  = ZERO
         FRL6(10,K) = ZERO
         FRL6(11,K) = ZERO
         FRL6(12,K) = ZERO
         FRL6(13,K) = ZERO
         FRL6(14,K) = ZERO
         FRL6(15,K) = ZERO
       END DO

       SX = XFRAME(1)
       SY = XFRAME(2)
       SZ = XFRAME(3)
       AA = ONE / SQRT(SX*SX + SY*SY + SZ*SZ)
       SX = SX * AA
       SY = SY * AA
       SZ = SZ * AA
       OX = XFRAME(10)
       OY = XFRAME(11)
       OZ = XFRAME(12)
c       MR2 = ZERO
c       VTMR2 = ZERO
c       ATMR2 = ZERO
C-----------------------------------------------------------------------
C     repere polaire axe S = X(frame) , rayon R , angle T
C-----------------------------------------------------------------------
       DO I=1,NSN
        N = NOD(I)
        RX0 = X(1,N) + HALF * DT2 * V(1,N) - OX
        RY0 = X(2,N) + HALF * DT2 * V(2,N) - OY
        RZ0 = X(3,N) + HALF * DT2 * V(3,N) - OZ
        TX = RY0 * SZ - RZ0 * SY
        TY = RZ0 * SX - RX0 * SZ
        TZ = RX0 * SY - RY0 * SX
        AA = ONE / SQRT(TX*TX + TY*TY + TZ*TZ)
        TX = TX * AA
        TY = TY * AA
        TZ = TZ * AA
        RX = SY * TZ - SZ * TY
        RY = SZ * TX - SX * TZ
        RZ = SX * TY - SY * TX
        R = RX * RX0 + RY * RY0 + RZ * RZ0
        R = MAX(R,EM20)
C-----------------------------------------------------------------------
C       changement de repere global => polaire
C-----------------------------------------------------------------------
        AX = A(1,N)
        AY = A(2,N)
        AZ = A(3,N)
        A(1,N) = RX * AX + RY * AY + RZ * AZ
        A(2,N) = SX * AX + SY * AY + SZ * AZ
        A(3,N) = (TX * AX + TY * AY + TZ * AZ) / R
        AX = V(1,N)
        AY = V(2,N)
        AZ = V(3,N)
        V(1,N) = RX * AX + RY * AY + RZ * AZ
        V(2,N) = SX * AX + SY * AY + SZ * AZ
        V(3,N) = (TX * AX + TY * AY + TZ * AZ) / R
        IF(IRODDL/=0)THEN
          AX = AR(1,N)
          AY = AR(2,N)
          AZ = AR(3,N)
          AR(1,N) = RX * AX + RY * AY + RZ * AZ
          AR(2,N) = SX * AX + SY * AY + SZ * AZ
          AR(3,N) = TX * AX + TY * AY + TZ * AZ
          AX = VR(1,N)
          AY = VR(2,N)
          AZ = VR(3,N)
          VR(1,N) = RX * AX + RY * AY + RZ * AZ
          VR(2,N) = SX * AX + SY * AY + SZ * AZ
          VR(3,N) = TX * AX + TY * AY + TZ * AZ
        ENDIF
        IF(WEIGHT(N)==1) THEN
          F1(I) = R*R*MS(N)
          F2(I) = R*R*MS(N)*V(3,N)
          F3(I) = R*R*MS(N)*A(3,N)
c          MR2 = MR2 + R*R*MS(N)
c          VTMR2 = VTMR2 + R*R*MS(N)*V(3,N)
c          ATMR2 = ATMR2 + R*R*MS(N)*A(3,N)
        ELSE
          F1(I) = ZERO
          F2(I) = ZERO
          F3(I) = ZERO
        ENDIF
       ENDDO
C
       IF(IC/=0)THEN
C
C Traitement Parith/ON avant echange
C
        CALL SUM_6_FLOAT(1  ,NSN  ,F1, FRL6(1,1), 15)
        CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(2,1), 15)
        CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(3,1), 15)
       
C-----------------------------------------------------------------------
C       masse quantite de mouvement
C-----------------------------------------------------------------------
        AX  =ZERO
        AY  =ZERO
        VX  =ZERO
        VY  =ZERO
        MASS=ZERO
        DO I=1,NSN
          N = NOD(I)
          IF(WEIGHT(N)==1) THEN
            F1(I)=MS(N)
            F2(I)=MS(N)*A(1,N)
            F3(I)=MS(N)*A(2,N)
            F4(I)=MS(N)*V(1,N)
            F5(I)=MS(N)*V(2,N)
          ELSE
            F1(I)=ZERO
            F2(I)=ZERO
            F3(I)=ZERO
            F4(I)=ZERO
            F5(I)=ZERO
          ENDIF
        ENDDO
C
C Traitement Parith/ON avant echange
C
        CALL SUM_6_FLOAT(1  ,NSN  ,F1, FRL6(4,1), 15)
        CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(5,1), 15)
        CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(6,1), 15)
        CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(7,1), 15)
        CALL SUM_6_FLOAT(1  ,NSN  ,F5, FRL6(8,1), 15)
       ENDIF
C-----------------------------------------------------------------------
C     DDL de rotation
C-----------------------------------------------------------------------
       IF(IRODDL/=0.AND.ICR/=0)THEN
C-----------------------------------------------------------------------
C       inertie quantite de mouvement
C-----------------------------------------------------------------------
c        AX  =ZERO
c        AY  =ZERO
c        AZ  =ZERO
c        VX  =ZERO
c        VY  =ZERO
c        VZ  =ZERO
c        INER=ZERO
C
        DO I=1,NSN
          N = NOD(I)
          IF(WEIGHT(N)==1) THEN
            F1(I)=IN(N)
            F2(I)=IN(N)*AR(1,N)
            F3(I)=IN(N)*AR(2,N)
            F4(I)=IN(N)*AR(3,N)
            F5(I)=IN(N)*VR(1,N)
            F6(I)=IN(N)*VR(2,N)
            F7(I)=IN(N)*VR(3,N)
          ENDIF
        ENDDO
C
C
C Traitement Parith/ON avant echange
C
        CALL SUM_6_FLOAT(1  ,NSN  ,F1, FRL6(9,1), 15)
        CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(10,1), 15)
        CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(11,1), 15)
        CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(12,1), 15)
        CALL SUM_6_FLOAT(1  ,NSN  ,F5, FRL6(13,1), 15)
        CALL SUM_6_FLOAT(1  ,NSN  ,F6, FRL6(14,1), 15)
        CALL SUM_6_FLOAT(1  ,NSN  ,F7, FRL6(15,1), 15)
       ENDIF
C
      ELSEIF(IFLAG == 2)THEN
       SX = XFRAME(1)
       SY = XFRAME(2)
       SZ = XFRAME(3)
       AA = ONE / SQRT(SX*SX + SY*SY + SZ*SZ)
       SX = SX * AA
       SY = SY * AA
       SZ = SZ * AA
       OX = XFRAME(10)
       OY = XFRAME(11)
       OZ = XFRAME(12)
C
       IF(IC/=0)THEN
         IC1=IC/4
         ICC=IC-4*IC1
         IC2=ICC/2
         IC3=ICC-2*IC2
C
         MR2  = FRL6(1,1)+FRL6(1,2)+FRL6(1,3)+
     +          FRL6(1,4)+FRL6(1,5)+FRL6(1,6)
         VTMR2= FRL6(2,1)+FRL6(2,2)+FRL6(2,3)+
     +          FRL6(2,4)+FRL6(2,5)+FRL6(2,6)
         ATMR2= FRL6(3,1)+FRL6(3,2)+FRL6(3,3)+
     +          FRL6(3,4)+FRL6(3,5)+FRL6(3,6)
         MASS = FRL6(4,1)+FRL6(4,2)+FRL6(4,3)+
     +          FRL6(4,4)+FRL6(4,5)+FRL6(4,6)
         AX   = FRL6(5,1)+FRL6(5,2)+FRL6(5,3)+
     +          FRL6(5,4)+FRL6(5,5)+FRL6(5,6)
         AY   = FRL6(6,1)+FRL6(6,2)+FRL6(6,3)+
     +          FRL6(6,4)+FRL6(6,5)+FRL6(6,6)
         VX   = FRL6(7,1)+FRL6(7,2)+FRL6(7,3)+
     +          FRL6(7,4)+FRL6(7,5)+FRL6(7,6)
         VY   = FRL6(8,1)+FRL6(8,2)+FRL6(8,3)+
     +          FRL6(8,4)+FRL6(8,5)+FRL6(8,6)

         AX= AX/MASS
         AY= AY/MASS
         AZ= ATMR2/MR2
         VX= VX/MASS
         VY= VY/MASS
         VZ= VTMR2/MR2
C-----------------------------------------------------------------------
C       link
C-----------------------------------------------------------------------
         DO I=1,NSN
          N = NOD(I)
          A(1,N) =A(1,N)-IC1*(A(1,N)-AX)
          A(2,N) =A(2,N)-IC2*(A(2,N)-AY)
          A(3,N) =A(3,N)-IC3*(A(3,N)-AZ)
C
          V(1,N) =V(1,N)-IC1*(V(1,N)-VX)
          V(2,N) =V(2,N)-IC2*(V(2,N)-VY)
          V(3,N) =V(3,N)-IC3*(V(3,N)-VZ)
C
         ENDDO
       END IF
C-----------------------------------------------------------------------
C     DDL de rotation
C-----------------------------------------------------------------------
       IF(IRODDL/=0.AND.ICR/=0)THEN
         IC1=ICR/4
         ICC=ICR-4*IC1
         IC2=ICC/2
         IC3=ICC-2*IC2
C
         INER= FRL6(9,1)+FRL6(9,2)+FRL6(9,3)+
     +         FRL6(9,4)+FRL6(9,5)+FRL6(9,6)
         AX  = FRL6(10,1)+FRL6(10,2)+FRL6(10,3)+
     +         FRL6(10,4)+FRL6(10,5)+FRL6(10,6)
         AY  = FRL6(11,1)+FRL6(11,2)+FRL6(11,3)+
     +         FRL6(11,4)+FRL6(11,5)+FRL6(11,6)
         AZ  = FRL6(12,1)+FRL6(12,2)+FRL6(12,3)+
     +         FRL6(12,4)+FRL6(12,5)+FRL6(12,6)
         VX  = FRL6(13,1)+FRL6(13,2)+FRL6(13,3)+
     +         FRL6(13,4)+FRL6(13,5)+FRL6(13,6)
         VY  = FRL6(14,1)+FRL6(14,2)+FRL6(14,3)+
     +         FRL6(14,4)+FRL6(14,5)+FRL6(14,6)
         VZ  = FRL6(15,1)+FRL6(15,2)+FRL6(15,3)+
     +         FRL6(15,4)+FRL6(15,5)+FRL6(15,6)
         IF(INER/=ZERO)THEN
           AX=AX/INER
           AY=AY/INER
           AZ=AZ/INER
           VX=VX/INER
           VY=VY/INER
           VZ=VZ/INER
C-----------------------------------------------------------------------
C         link
C-----------------------------------------------------------------------
           DO I=1,NSN
            N = NOD(I)
            AR(1,N) =AR(1,N)-IC1*(AR(1,N)-AX)
            AR(2,N) =AR(2,N)-IC2*(AR(2,N)-AY)
            AR(3,N) =AR(3,N)-IC3*(AR(3,N)-AZ)
C
            VR(1,N) =VR(1,N)-IC1*(VR(1,N)-VX)
            VR(2,N) =VR(2,N)-IC2*(VR(2,N)-VY)
            VR(3,N) =VR(3,N)-IC3*(VR(3,N)-VZ)
C
           ENDDO
         ENDIF
       ENDIF
C-----------------------------------------------------------------------
C     repere polaire axe S , rayon R , angle T
C-----------------------------------------------------------------------
       DO I=1,NSN
        N = NOD(I)
        RX0 = X(1,N) + HALF * DT2 * V(1,N) - OX
        RY0 = X(2,N) + HALF * DT2 * V(2,N) - OY
        RZ0 = X(3,N) + HALF * DT2 * V(3,N) - OZ
        TX = RY0 * SZ - RZ0 * SY
        TY = RZ0 * SX - RX0 * SZ
        TZ = RX0 * SY - RY0 * SX
        AA = ONE / SQRT(TX*TX + TY*TY + TZ*TZ)
        TX = TX * AA
        TY = TY * AA
        TZ = TZ * AA
        RX = SY * TZ - SZ * TY
        RY = SZ * TX - SX * TZ
        RZ = SX * TY - SY * TX
        R = RX * RX0 + RY * RY0 + RZ * RZ0
        R = MAX(R,EM20)
C-----------------------------------------------------------------------
C       changement de repere polaire => global
C-----------------------------------------------------------------------
        AX = RX * A(1,N) + SX * A(2,N) + TX * A(3,N) * R
        AY = RY * A(1,N) + SY * A(2,N) + TY * A(3,N) * R
        AZ = RZ * A(1,N) + SZ * A(2,N) + TZ * A(3,N) * R
        A(1,N) = AX
        A(2,N) = AY
        A(3,N) = AZ
        AX = RX * V(1,N) + SX * V(2,N) + TX * V(3,N) * R
        AY = RY * V(1,N) + SY * V(2,N) + TY * V(3,N) * R
        AZ = RZ * V(1,N) + SZ * V(2,N) + TZ * V(3,N) * R
        V(1,N) = AX
        V(2,N) = AY
        V(3,N) = AZ
        IF(IRODDL/=0)THEN
          AX = RX * AR(1,N) + SX * AR(2,N) + TX * AR(3,N)
          AY = RY * AR(1,N) + SY * AR(2,N) + TY * AR(3,N)
          AZ = RZ * AR(1,N) + SZ * AR(2,N) + TZ * AR(3,N)
          AR(1,N) = AX
          AR(2,N) = AY
          AR(3,N) = AZ
          AX = RX * VR(1,N) + SX * VR(2,N) + TX * VR(3,N)
          AY = RY * VR(1,N) + SY * VR(2,N) + TY * VR(3,N)
          AZ = RZ * VR(1,N) + SZ * VR(2,N) + TZ * VR(3,N)
          VR(1,N) = AX
          VR(2,N) = AY
          VR(3,N) = AZ
        ENDIF
       ENDDO
      END IF
C
      RETURN
      END
