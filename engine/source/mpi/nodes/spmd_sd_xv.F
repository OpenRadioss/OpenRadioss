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
C
Chd|====================================================================
Chd|  SPMD_SD_XV                    source/mpi/nodes/spmd_sd_xv.F 
Chd|-- called by -----------
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_SD_XV(
     1  X     ,D       ,V      ,VR    ,MS   ,
     2  IN    ,IAD_ELEM,FR_ELEM,WEIGHT,IMSCH,
     3  W     ,ISIZXV  ,ILENXV ,XDP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "rad2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IMSCH, ISIZXV, ILENXV,
     .        IAD_ELEM(2,*), FR_ELEM(*), WEIGHT(*)

      DOUBLE PRECISION
     .        XDP(3,*)
      my_real
     .        X(3,*),V(3,*),VR(3,*),D(3,*),MS(*),IN(*),W(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,INFO,I,REM_PROC,NOD,LOC_PROC,NB_NOD,DT_CST,
     .        SIZ,J,K,L, BUFSIZ, LSEND,E_LEN, IDEB,
     .        INB, II, NEW_D, IERROR, MSGOFF,MSGOFF2,
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD),
     .        L2, SIZDP, REQ_S_DP(NSPMD),IERROR1, LEN_DP

      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SRBUFDP

      DATA MSGOFF/108/
      DATA MSGOFF2/109/


      my_real
     .        VDT
      my_real, DIMENSION(:), ALLOCATABLE ::  SRBUF
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      DT_CST = 0
      IF (IDTMIN(11)==3.OR.IDTMIN(11)==8.OR.IDTMIN(10)==3) THEN
        IF(IMSCH>0) DT_CST = 1
      ENDIF
      IF ((IRAD2R==1).AND.(NCYCLE<2)) DT_CST = 1
      IF ((IALESUB/=0).OR.(IALE/=0).OR.(ISUB>0).OR.
     .    TT>=TANIM.OR.
     .    TT>=TOUTP.OR.TT>=TDES.OR.TT>=TPAT.OR.
     .    (MANIM>=1.AND.MANIM<=15)) THEN
        NEW_D = 1
        IF(IALE/=0.AND.NINTER>0) NEW_D = 2
      ELSE
        NEW_D = 0
      ENDIF
      E_LEN = 7 + IRODDL*3 + DT_CST*2 + NEW_D*3
      ALLOCATE(SRBUF(ISIZXV * MAX(ILENXV,E_LEN)))
      LEN_DP = 3

      IF(IRESP==1)THEN
        ALLOCATE(SRBUFDP(ISIZXV*LEN_DP),STAT=IERROR1)
	IF(IERROR1/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
      ELSE
         ALLOCATE(SRBUFDP(1),STAT=IERROR1)
      ENDIF

      IF(ISPMD==0) THEN
        L = 1
	L2 = 1
C
        DO I=1,NSPMD
          IF (NEW_D==0) THEN
           IF(IRODDL/=0.AND.DT_CST/=0) THEN
#include      "vectorize.inc"
             DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SRBUF(L  ) = X(1,NOD)
              SRBUF(L+1) = X(2,NOD)
              SRBUF(L+2) = X(3,NOD)
              SRBUF(L+3) = V(1,NOD)
              SRBUF(L+4) = V(2,NOD)
              SRBUF(L+5) = V(3,NOD)
              SRBUF(L+6) = VR(1,NOD)
              SRBUF(L+7) = VR(2,NOD)
              SRBUF(L+8) = VR(3,NOD)
              SRBUF(L+9) = MS(NOD)
              SRBUF(L+10)= IN(NOD)
              L = L + E_LEN
            END DO
           ELSEIF (IRODDL/=0.AND.DT_CST==0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SRBUF(L  ) = X(1,NOD)
              SRBUF(L+1) = X(2,NOD)
              SRBUF(L+2) = X(3,NOD)
              SRBUF(L+3) = V(1,NOD)
              SRBUF(L+4) = V(2,NOD)
              SRBUF(L+5) = V(3,NOD)
              SRBUF(L+6) = VR(1,NOD)
              SRBUF(L+7) = VR(2,NOD)
              SRBUF(L+8) = VR(3,NOD)
              L = L + E_LEN
            END DO
           ELSEIF (IRODDL==0.AND.DT_CST/=0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SRBUF(L  ) = X(1,NOD)
              SRBUF(L+1) = X(2,NOD)
              SRBUF(L+2) = X(3,NOD)
              SRBUF(L+3) = V(1,NOD)
              SRBUF(L+4) = V(2,NOD)
              SRBUF(L+5) = V(3,NOD)
              SRBUF(L+6) = MS(NOD)
              SRBUF(L+7) = IN(NOD)
              L = L + E_LEN
            END DO
           ELSE
#include      "vectorize.inc"
            DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SRBUF(L  ) = X(1,NOD)
              SRBUF(L+1) = X(2,NOD)
              SRBUF(L+2) = X(3,NOD)
              SRBUF(L+3) = V(1,NOD)
              SRBUF(L+4) = V(2,NOD)
              SRBUF(L+5) = V(3,NOD)
              L = L + E_LEN
            END DO
           ENDIF
Cel D est a envoyer pour sorties
          ELSEIF(NEW_D==1) THEN
           IF(IRODDL/=0.AND.DT_CST/=0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SRBUF(L  ) = X(1,NOD)
              SRBUF(L+1) = X(2,NOD)
              SRBUF(L+2) = X(3,NOD)
              SRBUF(L+3) = D(1,NOD)
              SRBUF(L+4) = D(2,NOD)
              SRBUF(L+5) = D(3,NOD)
              SRBUF(L+6) = V(1,NOD)
              SRBUF(L+7) = V(2,NOD)
              SRBUF(L+8) = V(3,NOD)
              SRBUF(L+9) = VR(1,NOD)
              SRBUF(L+10)= VR(2,NOD)
              SRBUF(L+11)= VR(3,NOD)
              SRBUF(L+12)= MS(NOD)
              SRBUF(L+13)= IN(NOD)
              L = L + E_LEN
            END DO
           ELSEIF (IRODDL/=0.AND.DT_CST==0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SRBUF(L  ) = X(1,NOD)
              SRBUF(L+1) = X(2,NOD)
              SRBUF(L+2) = X(3,NOD)
              SRBUF(L+3) = D(1,NOD)
              SRBUF(L+4) = D(2,NOD)
              SRBUF(L+5) = D(3,NOD)
              SRBUF(L+6) = V(1,NOD)
              SRBUF(L+7) = V(2,NOD)
              SRBUF(L+8) = V(3,NOD)
              SRBUF(L+9) = VR(1,NOD)
              SRBUF(L+10)= VR(2,NOD)
              SRBUF(L+11)= VR(3,NOD)
              L = L + E_LEN
            END DO
           ELSEIF (IRODDL==0.AND.DT_CST/=0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SRBUF(L  ) = X(1,NOD)
              SRBUF(L+1) = X(2,NOD)
              SRBUF(L+2) = X(3,NOD)
              SRBUF(L+3) = D(1,NOD)
              SRBUF(L+4) = D(2,NOD)
              SRBUF(L+5) = D(3,NOD)
              SRBUF(L+6) = V(1,NOD)
              SRBUF(L+7) = V(2,NOD)
              SRBUF(L+8) = V(3,NOD)
              SRBUF(L+9) = MS(NOD)
              SRBUF(L+10)= IN(NOD)
              L = L + E_LEN
            END DO
           ELSE
#include      "vectorize.inc"
            DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SRBUF(L  ) = X(1,NOD)
              SRBUF(L+1) = X(2,NOD)
              SRBUF(L+2) = X(3,NOD)
              SRBUF(L+3) = D(1,NOD)
              SRBUF(L+4) = D(2,NOD)
              SRBUF(L+5) = D(3,NOD)
              SRBUF(L+6) = V(1,NOD)
              SRBUF(L+7) = V(2,NOD)
              SRBUF(L+8) = V(3,NOD)
              L = L + E_LEN
            END DO
           ENDIF
Cel NEW_D==2 => Envoyer D et W
          ELSE
           IF(IRODDL/=0.AND.DT_CST/=0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SRBUF(L  ) = X(1,NOD)
              SRBUF(L+1) = X(2,NOD)
              SRBUF(L+2) = X(3,NOD)
              SRBUF(L+3) = D(1,NOD)
              SRBUF(L+4) = D(2,NOD)
              SRBUF(L+5) = D(3,NOD)
              SRBUF(L+6) = V(1,NOD)
              SRBUF(L+7) = V(2,NOD)
              SRBUF(L+8) = V(3,NOD)
              SRBUF(L+9) = VR(1,NOD)
              SRBUF(L+10)= VR(2,NOD)
              SRBUF(L+11)= VR(3,NOD)
              SRBUF(L+12)= MS(NOD)
              SRBUF(L+13)= IN(NOD)
              SRBUF(L+14)= W(1,NOD)
              SRBUF(L+15)= W(2,NOD)
              SRBUF(L+16)= W(3,NOD)
              L = L + E_LEN
            END DO
           ELSEIF (IRODDL/=0.AND.DT_CST==0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SRBUF(L  ) = X(1,NOD)
              SRBUF(L+1) = X(2,NOD)
              SRBUF(L+2) = X(3,NOD)
              SRBUF(L+3) = D(1,NOD)
              SRBUF(L+4) = D(2,NOD)
              SRBUF(L+5) = D(3,NOD)
              SRBUF(L+6) = V(1,NOD)
              SRBUF(L+7) = V(2,NOD)
              SRBUF(L+8) = V(3,NOD)
              SRBUF(L+9) = VR(1,NOD)
              SRBUF(L+10)= VR(2,NOD)
              SRBUF(L+11)= VR(3,NOD)
              SRBUF(L+12)= W(1,NOD)
              SRBUF(L+13)= W(2,NOD)
              SRBUF(L+14)= W(3,NOD)
              L = L + E_LEN
            END DO
           ELSEIF (IRODDL==0.AND.DT_CST/=0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SRBUF(L  ) = X(1,NOD)
              SRBUF(L+1) = X(2,NOD)
              SRBUF(L+2) = X(3,NOD)
              SRBUF(L+3) = D(1,NOD)
              SRBUF(L+4) = D(2,NOD)
              SRBUF(L+5) = D(3,NOD)
              SRBUF(L+6) = V(1,NOD)
              SRBUF(L+7) = V(2,NOD)
              SRBUF(L+8) = V(3,NOD)
              SRBUF(L+9) = MS(NOD)
              SRBUF(L+10)= IN(NOD)
              SRBUF(L+11)= W(1,NOD)
              SRBUF(L+12)= W(2,NOD)
              SRBUF(L+13)= W(3,NOD)
              L = L + E_LEN
            END DO
           ELSE
#include      "vectorize.inc"
            DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SRBUF(L  ) = X(1,NOD)
              SRBUF(L+1) = X(2,NOD)
              SRBUF(L+2) = X(3,NOD)
              SRBUF(L+3) = D(1,NOD)
              SRBUF(L+4) = D(2,NOD)
              SRBUF(L+5) = D(3,NOD)
              SRBUF(L+6) = V(1,NOD)
              SRBUF(L+7) = V(2,NOD)
              SRBUF(L+8) = V(3,NOD)
              SRBUF(L+9) = W(1,NOD)
              SRBUF(L+10)= W(2,NOD)
              SRBUF(L+11)= W(3,NOD)
              L = L + E_LEN
            END DO
           ENDIF
          ENDIF
	  IF(IRESP==1)THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(2,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)	
	      SRBUFDP(L2)  = XDP(1,NOD)
	      SRBUFDP(L2+1)= XDP(2,NOD)
	      SRBUFDP(L2+2)= XDP(3,NOD)	
	      L2= L2 + LEN_DP
	    ENDDO
	  ENDIF	
        ENDDO
C
C   Envoi messages
C
       L = 1
       L2= 1
       DO I=1,NSPMD
C----------------------------------------------------------------------------
         IF(IAD_ELEM(1,I+1)-IAD_ELEM(2,I)>0)THEN
           MSGTYP = MSGOFF
           SIZ = E_LEN*(IAD_ELEM(1,I+1)-IAD_ELEM(2,I))
           CALL MPI_ISEND(
     S       SRBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G       MPI_COMM_WORLD,REQ_S(I),IERROR)
           L = L + SIZ
	
	   IF(IRESP==1)THEN
	     MSGTYP = MSGOFF2
             SIZDP = LEN_DP*(IAD_ELEM(1,I+1)-IAD_ELEM(2,I))
             CALL MPI_ISEND(
     S         SRBUFDP(L2),SIZDP,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G         MPI_COMM_WORLD,REQ_S_DP(I),IERROR)
             L2 = L2 + SIZDP
	   ENDIF
         ENDIF
C----------------------------------------------------------------------------
       ENDDO
Cel wait terminaison isend
       DO I = 1, NSPMD
         IF(IAD_ELEM(1,I+1)-IAD_ELEM(2,I)>0)THEN
           CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
	   IF(IRESP==1)THEN
    	     CALL MPI_WAIT(REQ_S_DP(I),STATUS,IERROR)
	   ENDIF
         END IF
       END DO

      END IF
C
C decompactage
C
      IF(ISPMD/=0) THEN
        L = 1
	L2= 1
        SIZ = E_LEN*(IAD_ELEM(1,2)-IAD_ELEM(2,1))
	IF(IRESP==1)SIZDP = LEN_DP*(IAD_ELEM(1,2)-IAD_ELEM(2,1))
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(
     S      SRBUF(L),SIZ,REAL,IT_SPMD(1),MSGTYP,
     G      MPI_COMM_WORLD,STATUS,IERROR)

          IF(IRESP==1)THEN
            MSGTYP = MSGOFF2
            CALL MPI_RECV(
     S        SRBUFDP(L2),SIZDP,MPI_DOUBLE_PRECISION,IT_SPMD(1),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
          ENDIF
     	
          IF(NEW_D==0) THEN
            IF(IRODDL/=0.AND.DT_CST/=0) THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
                NOD = FR_ELEM(J)
                X(1,NOD)  =  SRBUF(L)
                X(2,NOD)  =  SRBUF(L+1)
                X(3,NOD)  =  SRBUF(L+2)
                V(1,NOD)  =  SRBUF(L+3)
                V(2,NOD)  =  SRBUF(L+4)
                V(3,NOD)  =  SRBUF(L+5)
                VR(1,NOD) =  SRBUF(L+6)
                VR(2,NOD) =  SRBUF(L+7)
                VR(3,NOD) =  SRBUF(L+8)
                MS(NOD)   =  SRBUF(L+9)
                IN(NOD)   =  SRBUF(L+10)
                L = L + E_LEN
              END DO
            ELSEIF (IRODDL/=0.AND.DT_CST==0) THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
                NOD = FR_ELEM(J)
                X(1,NOD)  =  SRBUF(L)
                X(2,NOD)  =  SRBUF(L+1)
                X(3,NOD)  =  SRBUF(L+2)
                V(1,NOD)  =  SRBUF(L+3)
                V(2,NOD)  =  SRBUF(L+4)
                V(3,NOD)  =  SRBUF(L+5)
                VR(1,NOD) =  SRBUF(L+6)
                VR(2,NOD) =  SRBUF(L+7)
                VR(3,NOD) =  SRBUF(L+8)
                L = L + E_LEN
              END DO
            ELSEIF (IRODDL==0.AND.DT_CST/=0) THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
                NOD = FR_ELEM(J)
                X(1,NOD)  =  SRBUF(L)
                X(2,NOD)  =  SRBUF(L+1)
                X(3,NOD)  =  SRBUF(L+2)
                V(1,NOD)  =  SRBUF(L+3)
                V(2,NOD)  =  SRBUF(L+4)
                V(3,NOD)  =  SRBUF(L+5)
                MS(NOD)   =  SRBUF(L+6)
                IN(NOD)   =  SRBUF(L+7)
                L = L + E_LEN
              END DO
            ELSE
#include      "vectorize.inc"
              DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
                NOD = FR_ELEM(J)
                X(1,NOD)  =  SRBUF(L)
                X(2,NOD)  =  SRBUF(L+1)
                X(3,NOD)  =  SRBUF(L+2)
                V(1,NOD)  =  SRBUF(L+3)
                V(2,NOD)  =  SRBUF(L+4)
                V(3,NOD)  =  SRBUF(L+5)
                L = L + E_LEN
              END DO
            ENDIF
Cel D a recevoir
          ELSEIF(NEW_D==1) THEN
            IF(IRODDL/=0.AND.DT_CST/=0) THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
                NOD = FR_ELEM(J)
                X(1,NOD)  =  SRBUF(L)
                X(2,NOD)  =  SRBUF(L+1)
                X(3,NOD)  =  SRBUF(L+2)
                D(1,NOD)  =  SRBUF(L+3)
                D(2,NOD)  =  SRBUF(L+4)
                D(3,NOD)  =  SRBUF(L+5)
                V(1,NOD)  =  SRBUF(L+6)
                V(2,NOD)  =  SRBUF(L+7)
                V(3,NOD)  =  SRBUF(L+8)
                VR(1,NOD) =  SRBUF(L+9)
                VR(2,NOD) =  SRBUF(L+10)
                VR(3,NOD) =  SRBUF(L+11)
                MS(NOD)   =  SRBUF(L+12)
                IN(NOD)   =  SRBUF(L+13)
                L = L + E_LEN
              END DO
            ELSEIF (IRODDL/=0.AND.DT_CST==0) THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
                NOD = FR_ELEM(J)
                X(1,NOD)  =  SRBUF(L)
                X(2,NOD)  =  SRBUF(L+1)
                X(3,NOD)  =  SRBUF(L+2)
                D(1,NOD)  =  SRBUF(L+3)
                D(2,NOD)  =  SRBUF(L+4)
                D(3,NOD)  =  SRBUF(L+5)
                V(1,NOD)  =  SRBUF(L+6)
                V(2,NOD)  =  SRBUF(L+7)
                V(3,NOD)  =  SRBUF(L+8)
                VR(1,NOD) =  SRBUF(L+9)
                VR(2,NOD) =  SRBUF(L+10)
                VR(3,NOD) =  SRBUF(L+11)
                L = L + E_LEN
              END DO
            ELSEIF (IRODDL==0.AND.DT_CST/=0) THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
                NOD = FR_ELEM(J)
                X(1,NOD)  =  SRBUF(L)
                X(2,NOD)  =  SRBUF(L+1)
                X(3,NOD)  =  SRBUF(L+2)
                D(1,NOD)  =  SRBUF(L+3)
                D(2,NOD)  =  SRBUF(L+4)
                D(3,NOD)  =  SRBUF(L+5)
                V(1,NOD)  =  SRBUF(L+6)
                V(2,NOD)  =  SRBUF(L+7)
                V(3,NOD)  =  SRBUF(L+8)
                MS(NOD)   =  SRBUF(L+9)
                IN(NOD)   =  SRBUF(L+10)
                L = L + E_LEN
              END DO
            ELSE
#include      "vectorize.inc"
              DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
                NOD = FR_ELEM(J)
                X(1,NOD)  =  SRBUF(L)
                X(2,NOD)  =  SRBUF(L+1)
                X(3,NOD)  =  SRBUF(L+2)
                D(1,NOD)  =  SRBUF(L+3)
                D(2,NOD)  =  SRBUF(L+4)
                D(3,NOD)  =  SRBUF(L+5)
                V(1,NOD)  =  SRBUF(L+6)
                V(2,NOD)  =  SRBUF(L+7)
                V(3,NOD)  =  SRBUF(L+8)
                L = L + E_LEN
              END DO
            ENDIF

CD et W a recevoir
          ELSE
            IF(IRODDL/=0.AND.DT_CST/=0) THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
                NOD = FR_ELEM(J)
                X(1,NOD)  =  SRBUF(L)
                X(2,NOD)  =  SRBUF(L+1)
                X(3,NOD)  =  SRBUF(L+2)
                D(1,NOD)  =  SRBUF(L+3)
                D(2,NOD)  =  SRBUF(L+4)
                D(3,NOD)  =  SRBUF(L+5)
                V(1,NOD)  =  SRBUF(L+6)
                V(2,NOD)  =  SRBUF(L+7)
                V(3,NOD)  =  SRBUF(L+8)
                VR(1,NOD) =  SRBUF(L+9)
                VR(2,NOD) =  SRBUF(L+10)
                VR(3,NOD) =  SRBUF(L+11)
                MS(NOD)   =  SRBUF(L+12)
                IN(NOD)   =  SRBUF(L+13)
                W(1,NOD)  =  SRBUF(L+14)
                W(2,NOD)  =  SRBUF(L+15)
                W(3,NOD)  =  SRBUF(L+16)
                L = L + E_LEN
              END DO
            ELSEIF (IRODDL/=0.AND.DT_CST==0) THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
                NOD = FR_ELEM(J)
                X(1,NOD)  =  SRBUF(L)
                X(2,NOD)  =  SRBUF(L+1)
                X(3,NOD)  =  SRBUF(L+2)
                D(1,NOD)  =  SRBUF(L+3)
                D(2,NOD)  =  SRBUF(L+4)
                D(3,NOD)  =  SRBUF(L+5)
                V(1,NOD)  =  SRBUF(L+6)
                V(2,NOD)  =  SRBUF(L+7)
                V(3,NOD)  =  SRBUF(L+8)
                VR(1,NOD) =  SRBUF(L+9)
                VR(2,NOD) =  SRBUF(L+10)
                VR(3,NOD) =  SRBUF(L+11)
                W(1,NOD)  =  SRBUF(L+12)
                W(2,NOD)  =  SRBUF(L+13)
                W(3,NOD)  =  SRBUF(L+14)
                L = L + E_LEN
              END DO
            ELSEIF (IRODDL==0.AND.DT_CST/=0) THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
                NOD = FR_ELEM(J)
                X(1,NOD)  =  SRBUF(L)
                X(2,NOD)  =  SRBUF(L+1)
                X(3,NOD)  =  SRBUF(L+2)
                D(1,NOD)  =  SRBUF(L+3)
                D(2,NOD)  =  SRBUF(L+4)
                D(3,NOD)  =  SRBUF(L+5)
                V(1,NOD)  =  SRBUF(L+6)
                V(2,NOD)  =  SRBUF(L+7)
                V(3,NOD)  =  SRBUF(L+8)
                MS(NOD)   =  SRBUF(L+9)
                IN(NOD)   =  SRBUF(L+10)
                W(1,NOD)  =  SRBUF(L+11)
                W(2,NOD)  =  SRBUF(L+12)
                W(3,NOD)  =  SRBUF(L+13)
                L = L + E_LEN
              END DO
            ELSE
#include      "vectorize.inc"
              DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
                NOD = FR_ELEM(J)
                X(1,NOD)  =  SRBUF(L)
                X(2,NOD)  =  SRBUF(L+1)
                X(3,NOD)  =  SRBUF(L+2)
                D(1,NOD)  =  SRBUF(L+3)
                D(2,NOD)  =  SRBUF(L+4)
                D(3,NOD)  =  SRBUF(L+5)
                V(1,NOD)  =  SRBUF(L+6)
                V(2,NOD)  =  SRBUF(L+7)
                V(3,NOD)  =  SRBUF(L+8)
                W(1,NOD)  =  SRBUF(L+9)
                W(2,NOD)  =  SRBUF(L+10)
                W(3,NOD)  =  SRBUF(L+11)
                L = L + E_LEN
              END DO
            ENDIF
          ENDIF
	  IF(IRESP==1)THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(2,1),IAD_ELEM(1,2)-1
	      NOD = FR_ELEM(J)
	      XDP(1,NOD)=  SRBUFDP(L2)
	      XDP(2,NOD)=  SRBUFDP(L2+1)
	      XDP(3,NOD)=  SRBUFDP(L2+2)
	      L2= L2 + LEN_DP      	
	    ENDDO
	  ENDIF	
        END IF
      ENDIF
      DEALLOCATE(SRBUF)
C
#endif
      RETURN
      END
