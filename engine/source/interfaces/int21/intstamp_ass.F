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
Chd|  INTSTAMP_ASS                  source/interfaces/int21/intstamp_ass.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_GLOB_DPSUM9              source/mpi/interfaces/spmd_th.F
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|====================================================================
      SUBROUTINE INTSTAMP_ASS(
     1           INTSTAMP,MS     ,IN      ,A     ,AR    ,
     2           STIFN   ,STIFR  ,WEIGHT  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "intstamp_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER WEIGHT(*)
      my_real
     .   MS(*), IN(*), A(3,*), AR(3,*), STIFN(*), STIFR(*)
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NN, MSR, MSRR, INTDAMP, KDIR, K, IROT
C     REAL
      my_real
     .   BUFS(8,NINTSTAMP), ALPHA, VIS, MASS, INM, STF, STR,
     .   VX, VY, VZ, FVX, FVY, FVZ, DT05, DW
      DOUBLE PRECISION BUFS6(6,8,NINTSTAMP)
C-----------------------------------------------
      IF(IPARIT/=0)THEN
        DO NN=1,NINTSTAMP
         DO K=1,6
          DO KDIR=1,3
            BUFS6(K,KDIR,NN)=INTSTAMP(NN)%FC6(K,KDIR)
          END DO
          BUFS6(K,4,NN)=INTSTAMP(NN)%ST6(K)
         END DO
C
C null if irot=0
         DO K=1,6
          DO KDIR=1,3
            BUFS6(K,KDIR+4,NN)=INTSTAMP(NN)%MC6(K,KDIR)
          END DO
          BUFS6(K,8,NN)=INTSTAMP(NN)%STR6(K)
         END DO
        END DO
        IF(NSPMD>1)
     .    CALL SPMD_GLOB_DPSUM9(BUFS6,48*NINTSTAMP)
        IF(ISPMD==0)THEN
          DO NN=1,NINTSTAMP
            DO KDIR=1,8
              BUFS(KDIR,NN)=BUFS6(1,KDIR,NN)
     .                     +BUFS6(2,KDIR,NN)
     .                     +BUFS6(3,KDIR,NN)
     .                     +BUFS6(4,KDIR,NN)
     .                     +BUFS6(5,KDIR,NN)
     .                     +BUFS6(6,KDIR,NN)
            END DO
          END DO
        END IF
        IF(NSPMD>1)
     .    CALL SPMD_RBCAST(BUFS,BUFS,8*NINTSTAMP,1,0,2)
        DO NN=1,NINTSTAMP
          DO KDIR=1,3
            INTSTAMP(NN)%FC(KDIR)=BUFS(KDIR,NN)
          END DO
          INTSTAMP(NN)%STF=BUFS(4,NN)
          IROT=INTSTAMP(NN)%IROT
          IF(IROT/=0)THEN
            DO KDIR=1,3
              INTSTAMP(NN)%MC(KDIR)=BUFS(KDIR+4,NN)
            END DO
            INTSTAMP(NN)%STR=BUFS(8,NN)
          END IF
        END DO
      ELSE
        IF(NSPMD>1)THEN
          DO NN=1,NINTSTAMP
            DO KDIR=1,3
              BUFS(KDIR,NN)=INTSTAMP(NN)%FC(KDIR)
            END DO
            BUFS(4,NN)=INTSTAMP(NN)%STF
C
C null if irot=0
            DO KDIR=1,3
              BUFS(KDIR+4,NN)=INTSTAMP(NN)%MC(KDIR)
            END DO
            BUFS(8,NN)=INTSTAMP(NN)%STR
          END DO
          CALL SPMD_GLOB_DSUM9(BUFS,8*NINTSTAMP)
          CALL SPMD_RBCAST(BUFS,BUFS,8*NINTSTAMP,1,0,2)
          DO NN=1,NINTSTAMP
            DO KDIR=1,3
              INTSTAMP(NN)%FC(KDIR)=BUFS(KDIR,NN)
            END DO
            INTSTAMP(NN)%STF=BUFS(4,NN)
            IROT=INTSTAMP(NN)%IROT
            IF(IROT /= 0)THEN
C rotations
              DO KDIR=1,3
                INTSTAMP(NN)%MC(KDIR)=BUFS(KDIR+4,NN)
              END DO
              INTSTAMP(NN)%STR=BUFS(8,NN)
            END IF
          END DO
        END IF
      END IF
C---------------------
C contact force / Interface => Rbody
C---------------------
      DO NN=1,NINTSTAMP
C
       IROT=INTSTAMP(NN)%IROT
       MSR =INTSTAMP(NN)%MSR
       DO KDIR=1,3
        A(KDIR,MSR)=A(KDIR,MSR)+INTSTAMP(NN)%FC(KDIR)
       END DO
       IF(IROT/=0)THEN
         DO KDIR=1,3
          AR(KDIR,MSR)=AR(KDIR,MSR)+INTSTAMP(NN)%MC(KDIR)
         END DO
       END IF
      END DO
C---------------------
C damping
C---------------------
      DT05=HALF*DT1
      DO NN=1,NINTSTAMP
        MSR=INTSTAMP(NN)%MSR
        INTDAMP=INTSTAMP(NN)%INTDAMP
        ALPHA  =INTSTAMP(NN)%DAMP
        MASS   =MS(MSR)
        STF =INTSTAMP(NN)%STF
        VIS =ALPHA*SQRT(FOUR*MASS*STF)
        TFEXT=TFEXT + DT05*INTSTAMP(NN)%DW
        IF(INTDAMP==0)THEN
          A(1,MSR)=A(1,MSR)-VIS*INTSTAMP(NN)%V(1)
          A(2,MSR)=A(2,MSR)-VIS*INTSTAMP(NN)%V(2)
          A(3,MSR)=A(3,MSR)-VIS*INTSTAMP(NN)%V(3)
          IF(ISPMD==0)THEN
              DW=-VIS*( INTSTAMP(NN)%V(1)*INTSTAMP(NN)%V(1)
     .         +INTSTAMP(NN)%V(2)*INTSTAMP(NN)%V(2)
     .         +INTSTAMP(NN)%V(3)*INTSTAMP(NN)%V(3))
              TFEXT=TFEXT + DT05 * DW
          END IF
        ELSE
          VX=(INTSTAMP(NN)%V(1)-INTSTAMP(INTDAMP)%V(1))
          VY=(INTSTAMP(NN)%V(2)-INTSTAMP(INTDAMP)%V(2))
          VZ=(INTSTAMP(NN)%V(3)-INTSTAMP(INTDAMP)%V(3))
          FVX=VIS*VX
          FVY=VIS*VY
          FVZ=VIS*VZ
          A(1,MSR)=A(1,MSR)-FVX
          A(2,MSR)=A(2,MSR)-FVY
          A(3,MSR)=A(3,MSR)-FVZ
          MSRR=INTSTAMP(INTDAMP)%MSR
          A(1,MSRR)=A(1,MSRR)+FVX
          A(2,MSRR)=A(2,MSRR)+FVY
          A(3,MSRR)=A(3,MSRR)+FVZ
          IF(ISPMD==0)THEN
            DW=-TWO*(FVX*VX+FVY*VY+FVZ*VZ)
            TFEXT=TFEXT + DT05 * DW
          END IF
        END IF
        IROT=INTSTAMP(NN)%IROT
        IF(IROT/=0)THEN
          ALPHA=INTSTAMP(NN)%DAMPR
          INM=IN(MSR)
          STR =INTSTAMP(NN)%STR
          VIS =ALPHA*SQRT(FOUR*INM*STR)
          IF(INTDAMP==0)THEN
            AR(1,MSR)=AR(1,MSR)-VIS*INTSTAMP(NN)%VR(1)
            AR(2,MSR)=AR(2,MSR)-VIS*INTSTAMP(NN)%VR(2)
            AR(3,MSR)=AR(3,MSR)-VIS*INTSTAMP(NN)%VR(3)
            IF(ISPMD==0)THEN
                DW=-VIS*( INTSTAMP(NN)%VR(1)*INTSTAMP(NN)%VR(1)
     .           +INTSTAMP(NN)%VR(2)*INTSTAMP(NN)%VR(2)
     .           +INTSTAMP(NN)%VR(3)*INTSTAMP(NN)%VR(3))
                TFEXT=TFEXT + DT05 * DW
            END IF
          ELSE
            VX=INTSTAMP(NN)%VR(1)-INTSTAMP(INTDAMP)%VR(1)
            VY=INTSTAMP(NN)%VR(2)-INTSTAMP(INTDAMP)%VR(2)
            VZ=INTSTAMP(NN)%VR(3)-INTSTAMP(INTDAMP)%VR(3)
            FVX=VIS*VX
            FVY=VIS*VY
            FVZ=VIS*VZ
            AR(1,MSR)=AR(1,MSR)-FVX
            AR(2,MSR)=AR(2,MSR)-FVY
            AR(3,MSR)=AR(3,MSR)-FVZ
            MSRR=INTSTAMP(INTDAMP)%MSR
            AR(1,MSRR)=AR(1,MSRR)+FVX
            AR(2,MSRR)=AR(2,MSRR)+FVY
            AR(3,MSRR)=AR(3,MSRR)+FVZ
            IF(ISPMD==0)THEN
              DW=-TWO*(FVX*VX+FVY*VY+FVZ*VZ)
              TFEXT=TFEXT + DT05 * DW
            END IF
          END IF
        END IF
      ENDDO
C---------------------
      RETURN
      END
