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
Chd|  SPMD_EXCH_A                   source/mpi/forces/spmd_exch_a.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ACCELERATION_DP_MOD           share/modules/acceleration_dp_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_A(
     1   A        ,AR     ,STIFN,STIFR ,MS  ,
     2   IAD_ELEM ,FR_ELEM,MSNF ,IFSUBM,SIZE,
     3   LENR     ,FTHE   ,MCP  ,DMSPH ,CONDN, 
     4   MS_2D,MCP_OFF,
     5   FORNEQS  ,NFACNIT,LENC  ,FCONT ,H3D_DATA )
C--------------------------------------
      USE ACCELERATION_DP_MOD
      USE H3D_MOD
C--------------------------------------
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
#include      "sphcom.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr18_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "intstamp_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), SIZE, LENR, IFSUBM,
     .        NFACNIT,LENC
      my_real
     .        AR(3,*),STIFN(*),STIFR(*),MS(*),MSNF(*),
     .        FTHE(*),MCP(*), DMSPH(*),CONDN(*)
      my_real, DIMENSION(3,*), INTENT(inout), TARGET :: A
      my_real
     .        MS_2D(*),MCP_OFF(*),
     .        FORNEQS(3,*),FCONT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,MSGOFF,
     .        SIZ,J,K,L,NB_NOD,IADMSPH,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      DATA MSGOFF/120/
      
      REAL(kind=8)
     .        RBUF(SIZE*LENR +  NFACNIT*LENR + LENC*LENR),
     .        SBUF(SIZE*LENR +  NFACNIT*LENR + LENC*LENR)
      REAL(kind=8), DIMENSION(:,:), POINTER :: ACC_POINTER
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
#ifdef MYREAL4
        ACC_POINTER=>ACC_DP(1:3,1:NUMNOD)
#else
        ACC_POINTER=>A(1:3,1:NUMNOD)
#endif    
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        SIZ = (SIZE+NFACNIT+LENC)*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(SIZ.NE.0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
C
      IF(SOL2SPH_FLAG/=0)THEN
        IADMSPH=5
        IF(IRODDL/=0)IADMSPH=IADMSPH+4
        IF(INTHEAT /= 0 .OR. ITHERM_FE /= 0 )IADMSPH=IADMSPH+1
        L = 1
        DO I=1,NSPMD
C preparation envoi  partie fixe (elem) a proc I
#include      "vectorize.inc"
         DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
           NOD = FR_ELEM(J)
           SBUF(L + IADMSPH - 1) = DMSPH(NOD)
           L = L + SIZE
         END DO
        END DO
      END IF
C
      IF(IRESP==1) THEN
        DO I = 1, NSPMD
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NOD = FR_ELEM(J)
                ACC_POINTER(1,NOD) = A(1,NOD)
                ACC_POINTER(2,NOD) = A(2,NOD)
                ACC_POINTER(3,NOD) = A(3,NOD)
            ENDDO
        ENDDO
      ENDIF


      L = 1
      IAD_SEND(1) = 1
      DO I=1,NSPMD
C preparation envoi  partie fixe (elem) a proc I
        IF(INTHEAT == 0 .AND. ITHERM_FE == 0 )THEN
         IF (N2D.EQ.0.AND.IFSUBM.EQ.0) THEN
          IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  AR(1,NOD)
              SBUF(L+4) =  AR(2,NOD)
              SBUF(L+5) =  AR(3,NOD)
              SBUF(L+6) =  STIFN(NOD)
              SBUF(L+7) =  STIFR(NOD)
              L = L + SIZE
            END DO
          ELSE
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  STIFN(NOD)
              L = L + SIZE
            END DO
          ENDIF
C cas 2D la masse est recalculee a chaque cycle, il faut la cumuler
        ELSEIF(N2D.NE.0.AND.IFSUBM.EQ.1)THEN
          IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  AR(1,NOD)
              SBUF(L+4) =  AR(2,NOD)
              SBUF(L+5) =  AR(3,NOD)
              SBUF(L+6) =  STIFN(NOD)
              SBUF(L+7) =  STIFR(NOD)
              SBUF(L+8) =  MS(NOD)
              SBUF(L+9) =  MS_2D(NOD)
              L = L + SIZE
            END DO
          ELSE
#include    "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  STIFN(NOD)
              SBUF(L+4) =  MS(NOD)
              SBUF(L+5) =  MS_2D(NOD)
              L = L + SIZE
            END DO
          ENDIF
        ELSEIF(N2D.NE.0.AND.IFSUBM.EQ.0)THEN
          IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  AR(1,NOD)
              SBUF(L+4) =  AR(2,NOD)
              SBUF(L+5) =  AR(3,NOD)
              SBUF(L+6) =  STIFN(NOD)
              SBUF(L+7) =  STIFR(NOD)
              SBUF(L+8) =  MS(NOD)
              L = L + SIZE
            END DO
          ELSE
#include    "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  STIFN(NOD)
              SBUF(L+4) =  MS(NOD)
              L = L + SIZE
            END DO
          ENDIF

        ELSEIF(N2D.EQ.0.AND.IFSUBM.EQ.1)THEN
          IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  AR(1,NOD)
              SBUF(L+4) =  AR(2,NOD)
              SBUF(L+5) =  AR(3,NOD)
              SBUF(L+6) =  STIFN(NOD)
              SBUF(L+7) =  STIFR(NOD)
              SBUF(L+8) =  MS(NOD)
              SBUF(L+9) =  MSNF(NOD)
              L = L + SIZE
            END DO
          ELSE
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  STIFN(NOD)
              SBUF(L+4) =  MS(NOD)
              SBUF(L+5) =  MSNF(NOD)
              L = L + SIZE
            END DO
          ENDIF
        ENDIF
C
C -- plus la thermique
C
      ELSE
         IF (N2D.EQ.0.AND.IFSUBM.EQ.0) THEN
          IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  AR(1,NOD)
              SBUF(L+4) =  AR(2,NOD)
              SBUF(L+5) =  AR(3,NOD)
              SBUF(L+6) =  STIFN(NOD)
              SBUF(L+7) =  STIFR(NOD)
              SBUF(L+8) =  FTHE(NOD)
              IF(ITHERM_FE == 1) SBUF(L+9) = MCP(NOD)
              IF(ITHERM_FE == 1) SBUF(L+10) = MCP_OFF(NOD)
              L = L + SIZE
            END DO
          ELSE
            IF(NODADT_THERM ==1 )THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NOD = FR_ELEM(J)
                SBUF(L  ) =  ACC_POINTER(1,NOD)
                SBUF(L+1) =  ACC_POINTER(2,NOD)
                SBUF(L+2) =  ACC_POINTER(3,NOD)
                SBUF(L+3) =  STIFN(NOD)
                SBUF(L+4) =  FTHE(NOD)
                SBUF(L+5) =  CONDN(NOD)
                IF(ITHERM_FE == 1) SBUF(L+6) = MCP(NOD)
                IF(ITHERM_FE == 1) SBUF(L+7) = MCP_OFF(NOD)
                L = L + SIZE
              END DO
            ELSE
#include      "vectorize.inc"
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NOD = FR_ELEM(J)
                SBUF(L  ) =  ACC_POINTER(1,NOD)
                SBUF(L+1) =  ACC_POINTER(2,NOD)
                SBUF(L+2) =  ACC_POINTER(3,NOD)
                SBUF(L+3) =  STIFN(NOD)
                SBUF(L+4) =  FTHE(NOD)
                IF(ITHERM_FE == 1) SBUF(L+5) = MCP(NOD)
                IF(ITHERM_FE == 1) SBUF(L+6) = MCP_OFF(NOD)
                L = L + SIZE
              END DO
            ENDIF
          ENDIF
C cas 2D la masse est recalculee a chaque cycle, il faut la cumuler
        ELSEIF(N2D.NE.0.AND.IFSUBM.EQ.1)THEN
          IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  AR(1,NOD)
              SBUF(L+4) =  AR(2,NOD)
              SBUF(L+5) =  AR(3,NOD)
              SBUF(L+6) =  STIFN(NOD)
              SBUF(L+7) =  STIFR(NOD)
              SBUF(L+8) =  MS(NOD)
              SBUF(L+9) =  FTHE(NOD)
              SBUF(L+10) = MCP(NOD)
              SBUF(L+11) =  MS_2D(NOD)
              L = L + SIZE
            END DO
          ELSE
#include    "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  STIFN(NOD)
              SBUF(L+4) =  MS(NOD)
              SBUF(L+5) =  FTHE(NOD)
              SBUF(L+6) =  MCP(NOD)
              SBUF(L+7) =  MS_2D(NOD)
              L = L + SIZE
            END DO
          ENDIF
        ELSEIF(N2D.NE.0.AND.IFSUBM.EQ.0)THEN
          IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  AR(1,NOD)
              SBUF(L+4) =  AR(2,NOD)
              SBUF(L+5) =  AR(3,NOD)
              SBUF(L+6) =  STIFN(NOD)
              SBUF(L+7) =  STIFR(NOD)
              SBUF(L+8) =  MS(NOD)
              SBUF(L+9) =  FTHE(NOD)
              SBUF(L+10) = MCP(NOD)
              L = L + SIZE
            END DO
          ELSE
#include    "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  STIFN(NOD)
              SBUF(L+4) =  MS(NOD)
              SBUF(L+5) =  FTHE(NOD)
              SBUF(L+6) =  MCP(NOD)
              L = L + SIZE
            END DO
          ENDIF
        ELSEIF(N2D.EQ.0.AND.IFSUBM.EQ.1)THEN
          IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  AR(1,NOD)
              SBUF(L+4) =  AR(2,NOD)
              SBUF(L+5) =  AR(3,NOD)
              SBUF(L+6) =  STIFN(NOD)
              SBUF(L+7) =  STIFR(NOD)
              SBUF(L+8) =  MS(NOD)
              SBUF(L+9) =  MSNF(NOD)
              SBUF(L+10) =  FTHE(NOD)
              SBUF(L+11) =  MCP(NOD)
              L = L + SIZE
            END DO
          ELSE
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  ACC_POINTER(1,NOD)
              SBUF(L+1) =  ACC_POINTER(2,NOD)
              SBUF(L+2) =  ACC_POINTER(3,NOD)
              SBUF(L+3) =  STIFN(NOD)
              SBUF(L+4) =  MS(NOD)
              SBUF(L+5) =  MSNF(NOD)
              SBUF(L+6) =  FTHE(NOD)
              SBUF(L+7) =  MCP(NOD)
              L = L + SIZE
            END DO
          ENDIF
        ENDIF
       ENDIF

c
C
C --- NITSCHE
       IF(NITSCHE > 0) THEN
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
             NOD = FR_ELEM(J)
             SBUF(L  ) =  FORNEQS(1,NOD)
             SBUF(L+1) =  FORNEQS(2,NOD)
             SBUF(L+2) =  FORNEQS(3,NOD)
             L = L + NFACNIT
          END DO
       ENDIF
C --- /CONT/MAX output
      IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX .NE.0.AND.NINTSTAMP==0) THEN
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
             NOD = FR_ELEM(J)
             SBUF(L  ) =  FCONT(1,NOD)
             SBUF(L+1) =  FCONT(2,NOD)
             SBUF(L+2) =  FCONT(3,NOD)
             L = L + 3
          END DO
       ENDIF
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
       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).GT.0)THEN
          MSGTYP = MSGOFF 
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C decompactage
C
      IF(SOL2SPH_FLAG/=0)THEN 
       IADMSPH=5
       IF(IRODDL/=0)IADMSPH=IADMSPH+4
       IF(INTHEAT /= 0 .OR. ITHERM_FE /= 0 )IADMSPH=IADMSPH+1
       DO I = 1, NSPMD
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(NB_NOD.GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)
#include        "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            DMSPH(NOD) = DMSPH(NOD) + RBUF(L+IADMSPH-1)
            L = L + SIZE
          END DO
        END IF
       END DO
      END IF
C
      DO I = 1, NSPMD
Cel test si msg necessaire a envoyer a completer par test interface
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(NB_NOD.GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)
C cas 3D
          IF(ITHERM_FE == 0 .AND. INTHEAT == 0 ) THEN
            IF (N2D.EQ.0.AND.IFSUBM.EQ.0) THEN
              IF(IRODDL.NE.0) THEN
#include        "vectorize.inc"
                DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                  NOD = FR_ELEM(J)
                  ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                  ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                  ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                  AR(1,NOD)= AR(1,NOD)+ RBUF(L+3)
                  AR(2,NOD)= AR(2,NOD)+ RBUF(L+4)
                  AR(3,NOD)= AR(3,NOD)+ RBUF(L+5)
                  STIFN(NOD)= STIFN(NOD)+ RBUF(L+6)
                  STIFR(NOD)= STIFR(NOD)+ RBUF(L+7)
                  L = L + SIZE
                END DO
              ELSE
#include        "vectorize.inc"
                DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                  NOD = FR_ELEM(J)
                  ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                  ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                  ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                  STIFN(NOD)= STIFN(NOD)+ RBUF(L+3)
                  L = L + SIZE
                END DO
              ENDIF
C cas 2D
            ELSEIF(N2D.NE.0.AND.IFSUBM.EQ.1)THEN
              IF(IRODDL.NE.0) THEN
#include        "vectorize.inc"
                DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                  NOD = FR_ELEM(J)
                  ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                  ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                  ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                  AR(1,NOD)= AR(1,NOD)+ RBUF(L+3)
                  AR(2,NOD)= AR(2,NOD)+ RBUF(L+4)
                  AR(3,NOD)= AR(3,NOD)+ RBUF(L+5)
                  STIFN(NOD)= STIFN(NOD)+ RBUF(L+6)
                  STIFR(NOD)= STIFR(NOD)+ RBUF(L+7)
                  MS(NOD) = MS(NOD)+ RBUF(L+8)
                  MS_2D(NOD) = MS_2D(NOD)+ RBUF(L+9)
                  L = L + SIZE
                END DO
              ELSE
#include        "vectorize.inc"
                DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                  NOD = FR_ELEM(J)
                  ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                  ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                  ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                  STIFN(NOD)= STIFN(NOD)+ RBUF(L+3)
                  MS(NOD) = MS(NOD)+ RBUF(L+4)
                  MS_2D(NOD) = MS_2D(NOD)+ RBUF(L+5)
                  L = L + SIZE
                END DO
              ENDIF
            ELSEIF(N2D.NE.0.AND.IFSUBM.EQ.0)THEN
              IF(IRODDL.NE.0) THEN
#include        "vectorize.inc"
                DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                  NOD = FR_ELEM(J)
                  ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                  ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                  ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                  AR(1,NOD)= AR(1,NOD)+ RBUF(L+3)
                  AR(2,NOD)= AR(2,NOD)+ RBUF(L+4)
                  AR(3,NOD)= AR(3,NOD)+ RBUF(L+5)
                  STIFN(NOD)= STIFN(NOD)+ RBUF(L+6)
                  STIFR(NOD)= STIFR(NOD)+ RBUF(L+7)
                  MS(NOD) = MS(NOD)+ RBUF(L+8)
                  L = L + SIZE
                END DO
              ELSE
#include        "vectorize.inc"
                DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                  NOD = FR_ELEM(J)
                  ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                  ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                  ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                  STIFN(NOD)= STIFN(NOD)+ RBUF(L+3)
                  MS(NOD) = MS(NOD)+ RBUF(L+4)
                  L = L + SIZE
                END DO
              ENDIF
            ELSEIF(N2D.EQ.0.AND.IFSUBM.EQ.1)THEN
              IF(IRODDL.NE.0) THEN
#include        "vectorize.inc"
                DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                  NOD = FR_ELEM(J)
                  ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                  ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                  ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                  AR(1,NOD)= AR(1,NOD)+ RBUF(L+3)
                  AR(2,NOD)= AR(2,NOD)+ RBUF(L+4)
                  AR(3,NOD)= AR(3,NOD)+ RBUF(L+5)
                  STIFN(NOD)= STIFN(NOD)+ RBUF(L+6)
                  STIFR(NOD)= STIFR(NOD)+ RBUF(L+7)
                  MS(NOD) = MS(NOD)+ RBUF(L+8)
                  MSNF(NOD) = MSNF(NOD) + RBUF(L+9)
                  L = L + SIZE
                END DO
              ELSE
#include        "vectorize.inc"
                DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                  NOD = FR_ELEM(J)
                  ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                  ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                  ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                  STIFN(NOD)= STIFN(NOD)+ RBUF(L+3)
                  MS(NOD) = MS(NOD)+ RBUF(L+4)
                  MSNF(NOD) = MSNF(NOD) + RBUF(L+5)
                  L = L + SIZE
                END DO
              ENDIF
            ENDIF
C
C ----plus la thermique
C
         ELSE
          IF (N2D.EQ.0.AND.IFSUBM.EQ.0) THEN
            IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NOD = FR_ELEM(J)
                ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                AR(1,NOD)= AR(1,NOD)+ RBUF(L+3)
                AR(2,NOD)= AR(2,NOD)+ RBUF(L+4)
                AR(3,NOD)= AR(3,NOD)+ RBUF(L+5)
                STIFN(NOD)= STIFN(NOD)+ RBUF(L+6)
                STIFR(NOD)= STIFR(NOD)+ RBUF(L+7)
                FTHE(NOD) = FTHE(NOD) + RBUF(L+8)
                IF(ITHERM_FE == 1) MCP(NOD) = MCP(NOD) + RBUF(L+9)
                IF(ITHERM_FE == 1) MCP_OFF(NOD) = MAX(MCP_OFF(NOD),RBUF(L+10))
                L = L + SIZE
              END DO
            ELSE
              IF(NODADT_THERM == 1) THEN
#include      "vectorize.inc"
                DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                  NOD = FR_ELEM(J)
                  ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                  ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                  ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                  STIFN(NOD)= STIFN(NOD)+ RBUF(L+3)
                  FTHE(NOD) = FTHE(NOD) + RBUF(L+4)
                  CONDN(NOD) = CONDN(NOD) + RBUF(L+5)
                  IF(ITHERM_FE == 1) MCP(NOD) = MCP(NOD) + RBUF(L+6)
                  IF(ITHERM_FE == 1) MCP_OFF(NOD) =MAX(MCP_OFF(NOD),RBUF(L+7))
                  L = L + SIZE
                END DO
              ELSE
#include      "vectorize.inc"
                DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                  NOD = FR_ELEM(J)
                  ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                  ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                  ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                  STIFN(NOD)= STIFN(NOD)+ RBUF(L+3)
                  FTHE(NOD) = FTHE(NOD) + RBUF(L+4)
                  IF(ITHERM_FE == 1) MCP(NOD) = MCP(NOD) + RBUF(L+5)
                  IF(ITHERM_FE == 1) MCP_OFF(NOD) =MAX(MCP_OFF(NOD),RBUF(L+6))
                  L = L + SIZE
                END DO
              ENDIF
            ENDIF
C cas 2D
          ELSEIF(N2D.NE.0.AND.IFSUBM.EQ.1)THEN
            IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NOD = FR_ELEM(J)
                ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                AR(1,NOD)= AR(1,NOD)+ RBUF(L+3)
                AR(2,NOD)= AR(2,NOD)+ RBUF(L+4)
                AR(3,NOD)= AR(3,NOD)+ RBUF(L+5)
                STIFN(NOD)= STIFN(NOD)+ RBUF(L+6)
                STIFR(NOD)= STIFR(NOD)+ RBUF(L+7)
                MS(NOD) = MS(NOD)+ RBUF(L+8)
                FTHE(NOD) = FTHE(NOD) + RBUF(L+9)
                MCP(NOD)  = MCP(NOD)  + RBUF(L+10)
                MS_2D(NOD) = MS_2D(NOD)+ RBUF(L+11)
                L = L + SIZE
              END DO
            ELSE
#include      "vectorize.inc"
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NOD = FR_ELEM(J)
                ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                STIFN(NOD)= STIFN(NOD)+ RBUF(L+3)
                MS(NOD) = MS(NOD)+ RBUF(L+4)
                FTHE(NOD) = FTHE(NOD) + RBUF(L+5)
                MCP(NOD)  = MCP(NOD)  + RBUF(L+6)
                MS_2D(NOD) = MS_2D(NOD)+ RBUF(L+7)
                L = L + SIZE
              END DO
            ENDIF
          ELSEIF(N2D.NE.0.AND.IFSUBM.EQ.0)THEN
            IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NOD = FR_ELEM(J)
                ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                AR(1,NOD)= AR(1,NOD)+ RBUF(L+3)
                AR(2,NOD)= AR(2,NOD)+ RBUF(L+4)
                AR(3,NOD)= AR(3,NOD)+ RBUF(L+5)
                STIFN(NOD)= STIFN(NOD)+ RBUF(L+6)
                STIFR(NOD)= STIFR(NOD)+ RBUF(L+7)
                MS(NOD) = MS(NOD)+ RBUF(L+8)
                FTHE(NOD) = FTHE(NOD) + RBUF(L+9)
                MCP(NOD)  = MCP(NOD)  + RBUF(L+10)
                L = L + SIZE
              END DO
            ELSE
#include      "vectorize.inc"
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NOD = FR_ELEM(J)
                ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                STIFN(NOD)= STIFN(NOD)+ RBUF(L+3)
                MS(NOD) = MS(NOD)+ RBUF(L+4)
                FTHE(NOD) = FTHE(NOD) + RBUF(L+5)
                MCP(NOD)  = MCP(NOD)  + RBUF(L+6)
                L = L + SIZE
              END DO
            ENDIF
          ELSEIF(N2D.EQ.0.AND.IFSUBM.EQ.1)THEN
            IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NOD = FR_ELEM(J)
                ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                AR(1,NOD)= AR(1,NOD)+ RBUF(L+3)
                AR(2,NOD)= AR(2,NOD)+ RBUF(L+4)
                AR(3,NOD)= AR(3,NOD)+ RBUF(L+5)
                STIFN(NOD)= STIFN(NOD)+ RBUF(L+6)
                STIFR(NOD)= STIFR(NOD)+ RBUF(L+7)
                MS(NOD) = MS(NOD)+ RBUF(L+8)
                MSNF(NOD) = MSNF(NOD) + RBUF(L+9)
                FTHE(NOD) = FTHE(NOD) + RBUF(L+10)
                MCP(NOD)  = MCP(NOD)  + RBUF(L+11)
                L = L + SIZE
              END DO
            ELSE
#include      "vectorize.inc"
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NOD = FR_ELEM(J)
                ACC_POINTER(1,NOD) = ACC_POINTER(1,NOD) + RBUF(L)
                ACC_POINTER(2,NOD) = ACC_POINTER(2,NOD) + RBUF(L+1)
                ACC_POINTER(3,NOD) = ACC_POINTER(3,NOD) + RBUF(L+2)
                STIFN(NOD)= STIFN(NOD)+ RBUF(L+3)
                MS(NOD) = MS(NOD)+ RBUF(L+4)
                MSNF(NOD) = MSNF(NOD) + RBUF(L+5)
                FTHE(NOD) = FTHE(NOD) + RBUF(L+6)
                MCP(NOD)  = MCP(NOD)  + RBUF(L+7)
                L = L + SIZE
              END DO
            ENDIF
          ENDIF
         ENDIF
C
        ENDIF
cc        IF(IPLYXFEM > 0) THEN
cc#include      "vectorize.inc"
cc            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
cc              NOD = FR_ELEM(J)
cc              NOD_PXFEM = INOD_PXFEM(NOD)
cc              IF(NOD_PXFEM > 0) THEN
cc              DO J = 1,NPLYMAX
cc                PLY(J)%ACC_POINTER(1,NOD_PXFEM)=PLY(J)%ACC_POINTER(1,NOD_PXFEM) + RBUF(L)
cc                PLY(J)%ACC_POINTER(2,NOD_PXFEM)=PLY(J)%ACC_POINTER(2,NOD_PXFEM) + RBUF(L+1)
cc                PLY(J)%ACC_POINTER(3,NOD_PXFEM)=PLY(J)%ACC_POINTER(3,NOD_PXFEM) + RBUF(L+2)
cc                PLY(J)%A(4,NOD_PXFEM)=PLY(J)%A(4,NOD_PXFEM) + RBUF(L+3)
cc                 L = L + 4
cc              END DO
cc            ENDDO
cc      ENDIF
C
C --- NITSCHE
       IF(NITSCHE > 0) THEN
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
             NOD = FR_ELEM(J)
             FORNEQS(1,NOD) = FORNEQS(1,NOD) + RBUF(L) 
             FORNEQS(2,NOD) = FORNEQS(2,NOD) + RBUF(L+1)
             FORNEQS(3,NOD) = FORNEQS(3,NOD) + RBUF(L+2)
             L = L + NFACNIT
          END DO
       ENDIF
C --- /CONT/MAX output
      IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX .NE.0.AND.NINTSTAMP==0) THEN
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
             NOD = FR_ELEM(J)
             FCONT(1,NOD) =  FCONT(1,NOD) + RBUF(L)  
             FCONT(2,NOD) =  FCONT(2,NOD) + RBUF(L+1)
             FCONT(3,NOD) =  FCONT(3,NOD) + RBUF(L+2) 
             L = L + 3
          END DO
       ENDIF
C

C
C

      END DO
C
Cel wait terminaison isend
C
      IF(IRESP==1) THEN
        DO I = 1, NSPMD
            NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
            IF(NB_NOD.GT.0)THEN
                DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                    NOD = FR_ELEM(J)
                    A(1,NOD) = ACC_POINTER(1,NOD)
                    A(2,NOD) = ACC_POINTER(2,NOD)
                    A(3,NOD) = ACC_POINTER(3,NOD)
                ENDDO
            ENDIF
        ENDDO   
      ENDIF

      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
#endif
      RETURN
      END
