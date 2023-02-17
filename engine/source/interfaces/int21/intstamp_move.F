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
Chd|  INTSTAMP_MOVE                 source/interfaces/int21/intstamp_move.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|====================================================================
      SUBROUTINE INTSTAMP_MOVE(
     1           INTSTAMP ,NPC    ,TF     ,SKEW    ,NODNX_SMS,
     2           V        ,VR     ,MS     ,X       ,D        ,
     3           NPBY     ,RBY    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "intstamp_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*), NODNX_SMS(*), NPBY(NNPBY,*)
C     REAL
      my_real
     .   TF(*), SKEW(LSKEW,*), V(3,*), VR(3,*), MS(*), X(3,*), D(3,*),
     .   RBY(NRBY,*)
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NN, IRB, MAIN, J, INTDAMP, MSR, MSRR, IROT
C     REAL
      my_real
     .   DW, ALPHA, MASS, VIS, STF, STR, INM, VX, VY, VZ,
     .   FVX, FVY, FVZ
C-----------------------------------------------
      DO NN=1,NINTSTAMP
C---------------------
C external work (from damping)
C---------------------
       IF(ISPMD==0)THEN
         INTDAMP=INTSTAMP(NN)%INTDAMP
         ALPHA=INTSTAMP(NN)%DAMP
         MASS=INTSTAMP(NN)%MASS
         STF =INTSTAMP(NN)%STF
         VIS =ALPHA*SQRT(FOUR*MASS*STF)
         MSR=INTSTAMP(NN)%MSR
         IF(INTDAMP==0)THEN
           FVX=VIS*INTSTAMP(NN)%V(1)
           FVY=VIS*INTSTAMP(NN)%V(2)
           FVZ=VIS*INTSTAMP(NN)%V(3)
           DW=-(FVX*V(1,MSR)+FVY*V(2,MSR)+FVZ*V(3,MSR))
         ELSE
           MSRR=INTSTAMP(NN)%MSR
           FVX=VIS*(INTSTAMP(NN)%V(1)-INTSTAMP(INTDAMP)%V(1))
           FVY=VIS*(INTSTAMP(NN)%V(2)-INTSTAMP(INTDAMP)%V(2))
           FVZ=VIS*(INTSTAMP(NN)%V(3)-INTSTAMP(INTDAMP)%V(3))
           DW=-TWO*( FVX*(V(1,MSR)-V(1,MSRR))
     .               +FVY*(V(2,MSR)-V(2,MSRR))
     .               +FVZ*(V(3,MSR)-V(3,MSRR)))
         END IF
         INTSTAMP(NN)%DW = HALF * DT2 * DW
         IROT=INTSTAMP(NN)%IROT
         IF(IROT/=0)THEN
           ALPHA=INTSTAMP(NN)%DAMPR
           INM=MIN(INTSTAMP(NN)%IN(1),
     .             INTSTAMP(NN)%IN(2),
     .             INTSTAMP(NN)%IN(3))
           STR =INTSTAMP(NN)%STR
           VIS =ALPHA*SQRT(FOUR*INM*STR)
           IF(INTDAMP==0)THEN
             FVX=VIS*INTSTAMP(NN)%VR(1)
             FVY=VIS*INTSTAMP(NN)%VR(2)
             FVZ=VIS*INTSTAMP(NN)%VR(3)
             DW=-(FVX*VR(1,MSR)+FVY*VR(2,MSR)+FVZ*VR(3,MSR))
           ELSE
             FVX=VIS*(INTSTAMP(NN)%VR(1)-INTSTAMP(INTDAMP)%VR(1))
             FVY=VIS*(INTSTAMP(NN)%VR(2)-INTSTAMP(INTDAMP)%VR(2))
             FVZ=VIS*(INTSTAMP(NN)%VR(3)-INTSTAMP(INTDAMP)%VR(3))
             DW=-TWO*( FVX*(VR(1,MSR)-VR(1,MSRR))
     .     	       +FVY*(VR(2,MSR)-VR(2,MSRR))
     .     	       +FVZ*(VR(3,MSR)-VR(3,MSRR)))
           END IF
           INTSTAMP(NN)%DW = INTSTAMP(NN)%DW + HALF * DT2 * DW
         END IF
       END IF
      ENDDO
C---------------------
C     Transfer Rbody => Interface
C---------------------
      DO NN=1,NINTSTAMP
        IRB   =INTSTAMP(NN)%IRB
        MAIN=INTSTAMP(NN)%MSR
        INTSTAMP(NN)%V(1)  =V(1,MAIN)
        INTSTAMP(NN)%V(2)  =V(2,MAIN)
        INTSTAMP(NN)%V(3)  =V(3,MAIN)
        INTSTAMP(NN)%D(1)  =D(1,MAIN)
        INTSTAMP(NN)%D(2)  =D(2,MAIN)
        INTSTAMP(NN)%D(3)  =D(3,MAIN)
        INTSTAMP(NN)%XG(1) =X(1,MAIN)
        INTSTAMP(NN)%XG(2) =X(2,MAIN)
        INTSTAMP(NN)%XG(3) =X(3,MAIN)
        IF(INTSTAMP(NN)%IROT/=0)THEN
          INTSTAMP(NN)%VR(1) =VR(1,MAIN)
          INTSTAMP(NN)%VR(2) =VR(2,MAIN)
          INTSTAMP(NN)%VR(3) =VR(3,MAIN)
          INTSTAMP(NN)%MASS  =MS(MAIN)
          DO J=1,9
            INTSTAMP(NN)%ROT(J)=RBY(J,IRB)
          END DO
        END IF
      END DO
C
      RETURN
      END
