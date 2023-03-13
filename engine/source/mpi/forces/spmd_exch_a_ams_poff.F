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
Chd|  SPMD_EXCH_A_AMS_POFF          source/mpi/forces/spmd_exch_a_ams_poff.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_A_AMS_POFF(
     1   A        ,AR     ,STIFN,STIFR ,MS  ,
     2   IAD_ELEM ,FR_ELEM,MSNF ,IFSUBM,SIZE,
     3   LENR     ,FTHE   ,MCP  ,FR_LOC,NB_FR,
     4   MS_2D    , MCP_OFF,FORNEQS,NFACNIT  ,
     5   LENC     ,FCONT   ,H3D_DATA,FNCONT  ,
     6   FTCONT)
C--------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
#include      "task_c.inc"
#include      "scr14_c.inc"
#include      "intstamp_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), SIZE, LENR, IFSUBM,NB_FR,FR_LOC(*),
     .        NFACNIT,LENC
      my_real
     .        A(3,*),AR(3,*),STIFN(*),STIFR(*),MS(*),MSNF(*),
     .        FTHE(*),MCP(*),MCP_OFF(*),MS_2D(*),FORNEQS(3,*)
      my_real  , INTENT(INOUT) :: FCONT(3,NUMNOD),FNCONT(3,NUMNOD),
     .        FTCONT(3,NUMNOD)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,MSGOFF,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),
     .        SHIFT
      DATA MSGOFF/121/

      my_real
     .        RBUF(SIZE*LENR + NFACNIT*LENR + LENC*LENR),
     .        SBUF(SIZE*LENR + NFACNIT*LENR + LENC*LENR)
      my_real,
     .  DIMENSION (:,:),ALLOCATABLE :: SAV_A
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ALLOCATE(SAV_A(SIZE+NFACNIT+LENC,NB_FR))
      SHIFT=0
      
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        SIZ = (SIZE + NFACNIT + LENC)*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
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

      DO I=1,NSPMD

        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          NOD = FR_ELEM(J)
          SBUF(L)   =  A(1,NOD) 
          SBUF(L+1) =  A(2,NOD)
          SBUF(L+2) =  A(3,NOD)
          SBUF(L+3) =  STIFN(NOD)

          SHIFT = L+3

          IF(IRODDL/=0) THEN
            SBUF(SHIFT+1) = AR(1,NOD)
            SBUF(SHIFT+2) = AR(2,NOD)
            SBUF(SHIFT+3) = AR(3,NOD)

            SBUF(SHIFT+4) = STIFR(NOD)
            SHIFT = SHIFT+4
          ENDIF

          IF(N2D /=0) THEN
            SBUF(SHIFT+1) = MS(NOD)
            SHIFT = SHIFT+1
          ENDIF

          IF(N2D /=0.AND.IFSUBM ==1) THEN
            SBUF(SHIFT+1) = MS_2D(NOD)
            SHIFT = SHIFT+1
          ENDIF

          IF(IFSUBM ==1)THEN
            SBUF(SHIFT+1) = MSNF(NOD)
            SHIFT = SHIFT+1
          ENDIF

          IF(INTHEAT /=0 .OR. ITHERM_FE/=0)THEN
             SBUF(SHIFT+1) =  FTHE(NOD)
             SHIFT = SHIFT+1
             IF(ITHERM_FE /= 0) THEN
              SBUF(SHIFT+1) =  MCP(NOD)
              SHIFT = SHIFT+1
              SBUF(SHIFT+1) =  MCP_OFF(NOD)
              SHIFT = SHIFT+1
             ENDIF
          ENDIF
        
          IF(NITSCHE/=0) THEN
            SBUF(SHIFT+1) = FORNEQS(1,NOD)
            SBUF(SHIFT+2) = FORNEQS(2,NOD)
            SBUF(SHIFT+3) = FORNEQS(3,NOD)
            SHIFT = SHIFT+NFACNIT
          ENDIF

C --- /CONT/MAX output
          IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX /=0.AND.NINTSTAMP==0) THEN
            SBUF(SHIFT+1) = FCONT(1,NOD)
            SBUF(SHIFT+2) = FCONT(2,NOD)
            SBUF(SHIFT+3) = FCONT(3,NOD)
            SHIFT = SHIFT+3
          ENDIF

C --- /PCONT/MAX output
          IF(ANIM_V(26)+H3D_DATA%N_VECT_PCONT_MAX /=0.AND.NINTSTAMP==0) THEN
            SBUF(SHIFT+1) = FNCONT(1,NOD)
            SBUF(SHIFT+2) = FNCONT(2,NOD)
            SBUF(SHIFT+3) = FNCONT(3,NOD)
            SBUF(SHIFT+4) = FTCONT(1,NOD)
            SBUF(SHIFT+5) = FTCONT(2,NOD)
            SBUF(SHIFT+6) = FTCONT(3,NOD)
            SHIFT = SHIFT+6
          ENDIF

          L = L + SIZE + NFACNIT +LENC
        ENDDO

        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C envoi a N+I mod P
Cel test si msg necessaire a envoyer a completer par test interface
       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO

C SAUVEGARGE VALEURS ACCEL
      DO J=1,NB_FR
         NOD=FR_LOC(J)
         
         SAV_A(1,J) =  A(1,NOD) 
         SAV_A(2,J) =  A(2,NOD)
         SAV_A(3,J) =  A(3,NOD)
         SAV_A(4,J) =  STIFN(NOD)

         A(1,NOD) = ZERO
         A(2,NOD) = ZERO
         A(3,NOD) = ZERO
         STIFN(NOD) = ZERO

         SHIFT = 4

         IF(IRODDL/=0) THEN
            SAV_A(SHIFT+1,J) = AR(1,NOD)
            SAV_A(SHIFT+2,J) = AR(2,NOD)
            SAV_A(SHIFT+3,J) = AR(3,NOD)
            SAV_A(SHIFT+4,J) = STIFR(NOD)

            AR(1,NOD) = ZERO
            AR(2,NOD) = ZERO
            AR(3,NOD) = ZERO
            STIFR(NOD) = ZERO
            SHIFT = SHIFT+4
          ENDIF

          IF(N2D /=0) THEN
            SAV_A(SHIFT+1,J) = MS(NOD)
            MS(NOD) = ZERO
            SHIFT = SHIFT+1
          ENDIF

          IF(N2D /=0.AND.IFSUBM ==1) THEN
            SAV_A(SHIFT+1,J) = MS_2D(NOD)
            MS_2D(NOD) = ZERO
            SHIFT = SHIFT+1
          ENDIF

          IF(IFSUBM ==1)THEN
            SAV_A(SHIFT+1,J) = MSNF(NOD)
            MSNF(NOD) = ZERO
            SHIFT = SHIFT+1
          ENDIF

          IF(INTHEAT /=0 .OR. ITHERM_FE/=0)THEN
             SAV_A(SHIFT+1,J) =  FTHE(NOD)
             FTHE(NOD) = ZERO
             SHIFT = SHIFT+1
             IF(ITHERM_FE /= 0 ) THEN 
               SAV_A(SHIFT+1,J) =  MCP(NOD)
               MCP(NOD) = ZERO
               SHIFT = SHIFT+1
               SAV_A(SHIFT+1,J) =  MCP_OFF(NOD)
               MCP_OFF(NOD) = ZERO
               SHIFT = SHIFT+1
             ENDIF
          ENDIF

         IF(NITSCHE/=0) THEN
            SAV_A(SHIFT+1,J) = FORNEQS(1,NOD)
            SAV_A(SHIFT+2,J) = FORNEQS(2,NOD)
            SAV_A(SHIFT+3,J) = FORNEQS(3,NOD)

            FORNEQS(1,NOD) = ZERO
            FORNEQS(2,NOD) = ZERO
            FORNEQS(3,NOD) = ZERO

            SHIFT = SHIFT+NFACNIT
          ENDIF

         IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX /=0.AND.NINTSTAMP==0) THEN
            SAV_A(SHIFT+1,J) = FCONT(1,NOD)
            SAV_A(SHIFT+2,J) = FCONT(2,NOD)
            SAV_A(SHIFT+3,J) = FCONT(3,NOD)

            FCONT(1,NOD) = ZERO
            FCONT(2,NOD) = ZERO
            FCONT(3,NOD) = ZERO

            SHIFT = SHIFT+3
          ENDIF

         IF(H3D_DATA%N_VECT_PCONT_MAX /=0.AND.NINTSTAMP==0) THEN
            SAV_A(SHIFT+1,J) = FNCONT(1,NOD)
            SAV_A(SHIFT+2,J) = FNCONT(2,NOD)
            SAV_A(SHIFT+3,J) = FNCONT(3,NOD)
            SAV_A(SHIFT+4,J) = FTCONT(1,NOD)
            SAV_A(SHIFT+5,J) = FTCONT(2,NOD)
            SAV_A(SHIFT+6,J) = FTCONT(3,NOD)

            FNCONT(1,NOD) = ZERO
            FNCONT(2,NOD) = ZERO
            FNCONT(3,NOD) = ZERO

            SHIFT = SHIFT+6
          ENDIF

        ENDDO



C
C decompactage
C
      DO I = 1, NSPMD
       IF(I/=LOC_PROC)THEN
Cel test si msg necessaire a envoyer a completer par test interface
         NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
         IF(NB_NOD>0)THEN
           CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)

            L = IAD_RECV(I)
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               NOD = FR_ELEM(J)
               A(1,NOD) = A(1,NOD) + RBUF(L)
               A(2,NOD) = A(2,NOD) + RBUF(L+1)
               A(3,NOD) = A(3,NOD) + RBUF(L+2)
               STIFN(NOD)= STIFN(NOD)+ RBUF(L+3)
  
               SHIFT = L+3

               IF(IRODDL/=0) THEN
                 AR(1,NOD)= AR(1,NOD)+ RBUF(SHIFT+1)
                 AR(2,NOD)= AR(2,NOD)+ RBUF(SHIFT+2)
                 AR(3,NOD)= AR(3,NOD)+ RBUF(SHIFT+3)
                 STIFR(NOD)= STIFR(NOD)+ RBUF(SHIFT+4)
                 SHIFT = SHIFT+4
               ENDIF

               IF(N2D /=0) THEN
                  MS(NOD) = MS(NOD)+ RBUF(SHIFT+1)
                  SHIFT = SHIFT+1
               ENDIF
   
               IF(N2D /=0.AND.IFSUBM ==1) THEN
                  MS_2D(NOD) = MS_2D(NOD)+ RBUF(SHIFT+1)
                  SHIFT = SHIFT+1
               ENDIF

               IF(IFSUBM ==1)THEN
                  MSNF(NOD) = MSNF(NOD) + RBUF(SHIFT+1)
                  SHIFT = SHIFT+1
               ENDIF

               IF(INTHEAT /=0 .OR. ITHERM_FE/=0)THEN
                  FTHE(NOD) = FTHE(NOD) + RBUF(SHIFT+1)
                  SHIFT = SHIFT+1
                  IF(ITHERM_FE /= 0) THEN
                    MCP(NOD) = MCP(NOD) + RBUF(SHIFT+1)
                    SHIFT = SHIFT+1
                    MCP_OFF(NOD) =MAX(MCP_OFF(NOD),RBUF(SHIFT+1))
                    SHIFT = SHIFT+1
                  ENDIF
               ENDIF

               IF(NITSCHE/=0) THEN
                 FORNEQS(1,NOD)= FORNEQS(1,NOD)+ RBUF(SHIFT+1)
                 FORNEQS(2,NOD)= FORNEQS(2,NOD)+ RBUF(SHIFT+2)
                 FORNEQS(3,NOD)= FORNEQS(3,NOD)+ RBUF(SHIFT+3)
                 SHIFT = SHIFT+NFACNIT
               ENDIF

              IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX /=0.AND.NINTSTAMP==0) THEN
                 FCONT(1,NOD)= FCONT(1,NOD)+ RBUF(SHIFT+1)
                 FCONT(2,NOD)= FCONT(2,NOD)+ RBUF(SHIFT+2)
                 FCONT(3,NOD)= FCONT(3,NOD)+ RBUF(SHIFT+3)
                 SHIFT = SHIFT+3
               ENDIF

               IF(H3D_DATA%N_VECT_PCONT_MAX /=0.AND.NINTSTAMP==0) THEN
                 FNCONT(1,NOD)= FNCONT(1,NOD)+ RBUF(SHIFT+1)
                 FNCONT(2,NOD)= FNCONT(2,NOD)+ RBUF(SHIFT+2)
                 FNCONT(3,NOD)= FNCONT(3,NOD)+ RBUF(SHIFT+3)
                 FTCONT(1,NOD)= FTCONT(1,NOD)+ RBUF(SHIFT+4)
                 FTCONT(2,NOD)= FTCONT(2,NOD)+ RBUF(SHIFT+5)
                 FTCONT(3,NOD)= FTCONT(3,NOD)+ RBUF(SHIFT+6)
                 SHIFT = SHIFT+6
               ENDIF

               L = L + SIZE +NFACNIT +LENC
            END DO
         ENDIF
       ELSE

         DO J=1,NB_FR
           NOD=FR_LOC(J)
           A(1,NOD) = A(1,NOD) + SAV_A(1,J)
           A(2,NOD) = A(2,NOD) + SAV_A(2,J)
           A(3,NOD) = A(3,NOD) + SAV_A(3,J)
           STIFN(NOD)= STIFN(NOD)+ SAV_A(4,J)
           SHIFT = 4

           IF(IRODDL/=0) THEN 
             AR(1,NOD) = AR(1,NOD) + SAV_A(SHIFT+1,J)
             AR(2,NOD) = AR(2,NOD) + SAV_A(SHIFT+2,J)
             AR(3,NOD) = AR(3,NOD) + SAV_A(SHIFT+3,J)
             STIFR(NOD)= STIFR(NOD)+ SAV_A(SHIFT+4,J)
             SHIFT = SHIFT+4
           ENDIF

          IF(N2D /=0) THEN
            MS(NOD) = MS(NOD) + SAV_A(SHIFT+1,J)
            SHIFT = SHIFT+1
          ENDIF

          IF(N2D /=0.AND.IFSUBM ==1) THEN
            MS_2D(NOD) = MS_2D(NOD) + SAV_A(SHIFT+1,J)
            SHIFT = SHIFT+1
          ENDIF

          IF(IFSUBM ==1)THEN
            MSNF(NOD) = MSNF(NOD) + SAV_A(SHIFT+1,J)
            SHIFT = SHIFT+1
          ENDIF

          IF(INTHEAT /=0 .OR. ITHERM_FE/=0)THEN
            FTHE(NOD) = FTHE(NOD) + SAV_A(SHIFT+1,J)
            SHIFT = SHIFT+1
            IF(ITHERM_FE /= 0) THEN
              MCP(NOD) = MCP(NOD) + SAV_A(SHIFT+1,J)
              SHIFT = SHIFT+1
              MCP_OFF(NOD) = MAX(MCP_OFF(NOD),SAV_A(SHIFT+1,J))
              SHIFT = SHIFT+1
            ENDIF
          ENDIF

           IF(NITSCHE/=0) THEN 
             FORNEQS(1,NOD) = FORNEQS(1,NOD) + SAV_A(SHIFT+1,J)
             FORNEQS(2,NOD) = FORNEQS(2,NOD) + SAV_A(SHIFT+2,J)
             FORNEQS(3,NOD) = FORNEQS(3,NOD) + SAV_A(SHIFT+3,J)
             SHIFT = SHIFT+NFACNIT
           ENDIF

C --- /CONT/MAX output
           IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX /=0.AND.NINTSTAMP==0) THEN
             FCONT(1,NOD) = FCONT(1,NOD) + SAV_A(SHIFT+1,J)
             FCONT(2,NOD) = FCONT(2,NOD) + SAV_A(SHIFT+2,J)
             FCONT(3,NOD) = FCONT(3,NOD) + SAV_A(SHIFT+3,J)
             SHIFT = SHIFT+3
           ENDIF

           IF(H3D_DATA%N_VECT_PCONT_MAX /=0.AND.NINTSTAMP==0) THEN
              FNCONT(1,NOD)= FNCONT(1,NOD)+ SAV_A(SHIFT+1,J)
              FNCONT(2,NOD)= FNCONT(2,NOD)+ SAV_A(SHIFT+2,J)
              FNCONT(3,NOD)= FNCONT(3,NOD)+ SAV_A(SHIFT+3,J)
              FTCONT(1,NOD)= FTCONT(1,NOD)+ SAV_A(SHIFT+4,J)
              FTCONT(2,NOD)= FTCONT(2,NOD)+ SAV_A(SHIFT+5,J)
              FTCONT(3,NOD)= FTCONT(3,NOD)+ SAV_A(SHIFT+6,J)
              SHIFT = SHIFT+6
            ENDIF

         END DO
       ENDIF
      END DO
C
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
      DEALLOCATE(SAV_A)
C
#endif
      RETURN
      END
