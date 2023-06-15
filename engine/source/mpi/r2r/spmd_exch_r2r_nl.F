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
Chd|  SPMD_EXCH_R2R_NL              source/mpi/r2r/spmd_exch_r2r_nl.F
Chd|-- called by -----------
Chd|        R2R_GETDATA                   source/coupling/rad2rad/r2r_getdata.F
Chd|-- calls ---------------
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_R2R_NL(
     1    A ,AR, V, VR     ,MS  ,
     2    IN,IAD_ELEM ,FR_ELEM, SIZE,
     3    SBUF_SIZE,RBUF_SIZE,WF,WF2,DD_R2R,
     4    DD_R2R_ELEM,WEIGHT,FLAG,NLOC_DMG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "rad2r_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IAD_ELEM(2,NSPMD+1),FR_ELEM(SFR_ELEM),
     .        SIZE,DD_R2R(NSPMD+1,SDD_R2R),DD_R2R_ELEM(SDD_R2R_ELEM),
     .        FLAG,WEIGHT(NUMNOD),SBUF_SIZE,RBUF_SIZE
      my_real,INTENT(IN) :: V(3,NUMNOD),VR(3,NUMNOD)
      my_real,INTENT(INOUT) :: WF,WF2,A(3,NUMNOD),AR(3,NUMNOD),
     .        MS(NUMNOD),IN(IRODDL*NUMNOD)
      TYPE(NLOCAL_STR_), TARGET, INTENT(IN)  :: NLOC_DMG
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),OFFSET,MSGOFF,NN,
     .        OFFSET_S_NL,OFFSET_R_NL
      my_real
     .        RBUF(RBUF_SIZE),SBUF(SBUF_SIZE),
     .        DF1,DF2,DF3,DF4,DF5,DF6
c
      INTEGER, POINTER, DIMENSION(:) :: IDXI,POSI
      my_real, POINTER, DIMENSION(:) :: FNL
c     
      DATA MSGOFF/5015/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C-----------------------------------------------
      FNL  => NLOC_DMG%FNL(1:NLOC_DMG%L_NLOC,1)
      IDXI => NLOC_DMG%IDXI(1:NUMNOD)
      POSI => NLOC_DMG%POSI(1:NLOC_DMG%NNOD+1)
C-----------------------------------------------
C
      OFFSET = DD_R2R(NSPMD+1,1)-1
      OFFSET_S_NL = OFFSET + DD_R2R(NSPMD+1,2)-1
      OFFSET_R_NL = OFFSET_S_NL + DD_R2R(NSPMD+1,3)-1
C
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      
      DO I=1,NSPMD
        SIZ = SIZE*(DD_R2R(I+1,2)-DD_R2R(I,2))+DD_R2R(I+1,4)-DD_R2R(I,4)
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
      L = 1
      IAD_SEND(1) = 1
C      
      DO I=1,NSPMD
C preparation envoi  partie fixe (elem) a proc I
        IF(IRODDL/=0) THEN
#include      "vectorize.inc"
          DO J=DD_R2R(I,1),DD_R2R(I+1,1)-1
            NOD = DD_R2R_ELEM(J)
            SBUF(L  ) =  A(1,NOD)
            SBUF(L+1) =  A(2,NOD)
            SBUF(L+2) =  A(3,NOD)
            SBUF(L+3) =  AR(1,NOD)
            SBUF(L+4) =  AR(2,NOD)
            SBUF(L+5) =  AR(3,NOD)
            IF (FLAG==1) THEN
              SBUF(L+6) =  MS(NOD)
              SBUF(L+7) =  IN(NOD)              
            ENDIF
            L = L + SIZE
          ENDDO

        ELSE
#include      "vectorize.inc"
          DO J=DD_R2R(I,1),DD_R2R(I+1,1)-1
            NOD = DD_R2R_ELEM(J)
            SBUF(L  ) =  A(1,NOD)
            SBUF(L+1) =  A(2,NOD)
            SBUF(L+2) =  A(3,NOD)
            IF (FLAG==1) THEN
              SBUF(L+3) =  MS(NOD)              
            ENDIF  
            L = L + SIZE
          END DO
        ENDIF
C
#include      "vectorize.inc"
        DO J=DD_R2R(I,3),DD_R2R(I+1,3)-1
          NOD = DD_R2R_ELEM(OFFSET_S_NL + J)
          K = POSI(IDXI(NOD))
          SBUF(L) =  FNL(K) 
          L = L + 1
        ENDDO
C
        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C envoi a N+I mod P
Cel test si msg necessaire a envoyer a completer par test interface
       IF(DD_R2R(I+1,1)-DD_R2R(I,1)>0)THEN
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C decompactage
C
      DO I = 1, NSPMD
Cel test si msg necessaire a envoyer a completer par test interface
        NB_NOD = DD_R2R(I+1,2)-DD_R2R(I,2)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)

          IF(IRODDL/=0) THEN
#include        "vectorize.inc"
            DO J=DD_R2R(I,2),DD_R2R(I+1,2)-1
                NOD = DD_R2R_ELEM(OFFSET+J)
                IF(WEIGHT(NOD)==1)THEN
                  DF1 = RBUF(L)-A(1,NOD)
                  DF2 = RBUF(L+1)-A(2,NOD)
                  DF3 = RBUF(L+2)-A(3,NOD)
                  DF4 = RBUF(L+3)-AR(1,NOD)
                  DF5 = RBUF(L+4)-AR(2,NOD)
                  DF6 = RBUF(L+5)-AR(3,NOD)                  
                ENDIF                  
                A(1,NOD) = RBUF(L)
                A(2,NOD) = RBUF(L+1)
                A(3,NOD) = RBUF(L+2)
                AR(1,NOD)= RBUF(L+3)
                AR(2,NOD)= RBUF(L+4)
                AR(3,NOD)= RBUF(L+5)
                IF (FLAG==1) THEN
                  MS(NOD)= RBUF(L+6)
                  IN(NOD)= RBUF(L+7)            
                ENDIF
                L = L + SIZE                               
C calcul du travail localement                
                IF(WEIGHT(NOD)==1)THEN
                  WF = WF + (DF1*V(1,NOD)+DF2*V(2,NOD)+
     .                 DF3*V(3,NOD))/TWO
                  WF2= WF2+ (DF1*A(1,NOD)+DF2*A(2,NOD)+
     .                 DF3*A(3,NOD))/(TWO*MS(NOD))
                  WF = WF + (DF4*VR(1,NOD)+DF5*VR(2,NOD)+
     .                 DF6*VR(3,NOD))/TWO
                  WF2= WF2+ (DF4*AR(1,NOD)+DF5*AR(2,NOD)+
     .                 DF6*AR(3,NOD))/(TWO*IN(NOD))                  
                ENDIF                  
            END DO
          ELSE
#include        "vectorize.inc"
             DO J=DD_R2R(I,2),DD_R2R(I+1,2)-1
                 NOD = DD_R2R_ELEM(OFFSET+J)
                 IF(WEIGHT(NOD)==1)THEN
                   DF1 = RBUF(L)-A(1,NOD)
                   DF2 = RBUF(L+1)-A(2,NOD)
                   DF3 = RBUF(L+2)-A(3,NOD)               
                 ENDIF              
                 A(1,NOD) = RBUF(L)
                 A(2,NOD) = RBUF(L+1)
                 A(3,NOD) = RBUF(L+2)
                 IF (FLAG==1) THEN
                   MS(NOD)= RBUF(L+3)           
                 ENDIF                 
                 L = L + SIZE 
C calcul du travail localement                
                 IF(WEIGHT(NOD)==1)THEN
                   WF = WF + (DF1*V(1,NOD)+DF2*V(2,NOD)+
     .                 DF3*V(3,NOD))/TWO
                   WF2= WF2+ (DF1*A(1,NOD)+DF2*A(2,NOD)+
     .                 DF3*A(3,NOD))/(TWO*MS(NOD))                
                 ENDIF  
             END DO
          ENDIF
C
#include      "vectorize.inc"
          DO J=DD_R2R(I,4),DD_R2R(I+1,4)-1
            NOD = DD_R2R_ELEM(OFFSET_R_NL + J)
            K = POSI(IDXI(NOD))
            FNL(K) = RBUF(L)   
            L = L + 1
          ENDDO
C ---
        ENDIF
C
      END DO
C
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(DD_R2R(I+1,1)-DD_R2R(I,1)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF       
      ENDDO
C

#endif
      RETURN
      END                       
