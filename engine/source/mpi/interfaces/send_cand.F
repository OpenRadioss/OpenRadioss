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
CC Message TAGS Rules
C TAGS are represented by variable integer MSGOFF
C MSGOFF is a 4 digit integer of the form
C DATA MSGOFF/ABCD/ 
C MSGTYP = MSGOFF

C TAG ID series for different message files:
C    1  ->  999  => spmd_mach.F 
C 1000  -> 1999  => send_cand.F
C 2000  -> 2999  => spmd_sph.F
C 3000  -> 3999  => spmd_cfd.F
C 4000  -> 4999  => spmd_section.F
C 5000  -> 5999  => spmd_r2r.F
C 6000  -> 6999  => spmd_int.F
C 7000  -> 7999  => spmd_anim.F
C 8000  -> 8999  => spmd_th.F
C 9000  -> 9999  => spmd_outp.F
C 10000 -> 10999 => spmd_stat.F
C 11000 -> 11999 => spmd_rest.F
C 12000 -> 12999 => spmd_lag.F
C 13000 -> 13999 => spmd_dsreso.F

C
Chd|====================================================================
Chd|  SPMD_GET_INACTI7              source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GET_INACTI7(
     .    INACTI,IPARI22,NIN,ISENDTO,IRCVFROM,
     .    INACTII)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INACTI,NIN,IPARI22,
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*),
     .        INACTII
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,K,LEN,ATID,ATAG,ALEN,IDEB,INACTI_R,
     .        MSGOFF,MSGTYP,INFO, PMAIN, LOC_PROC
      DATA MSGOFF/1000/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LOC_PROC=ISPMD+1
      IF(NSPMD==1.OR.(isendto(NIN,LOC_PROC)==0.AND.
     .   ircvfrom(NIN,LOC_PROC)==0)) THEN
        RETURN
      ENDIF
C
C   determination du proc main (celui qui fait joue le role de P0)
C
      DO K = 1, NSPMD
        IF (isendto(NIN,K)/=0.OR.
     .      ircvfrom(NIN,K)/=0) THEN
          PMAIN = K
          GOTO 110
        ENDIF
      ENDDO
 110  CONTINUE
C temporary change for exchange and cumul
      IF(INACTI < 0) INACTI=0
      IF (LOC_PROC/=PMAIN) THEN
        MSGTYP=MSGOFF        
        CALL MPI_SEND(INACTI,1,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,
     .                MPI_COMM_WORLD,ierror)
C p0 envoi la liste des noeuds pour lesquels stfn=0
        CALL MPI_RECV(INACTI,1,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,   
     .                MPI_COMM_WORLD,STATUS,ierror)
      ELSE
        DO K = 1, NSPMD
          IF (K/=LOC_PROC.AND.(isendto(NIN,K)/=0.OR.
     .   ircvfrom(NIN,K)/=0)) THEN
            MSGTYP=MSGOFF
            CALL MPI_RECV(INACTI_R,1,MPI_INTEGER,IT_SPMD(K),MSGTYP,  
     .                    MPI_COMM_WORLD,STATUS,ierror)
            INACTI = INACTI+INACTI_R
          ENDIF
        ENDDO
C
        IF (INACTI/=0) THEN
          INACTI=INACTII
        ELSE
C pour le cas inacti passe en negatif sur tous les procs
          INACTI=-ABS(INACTII)
        END IF
C
        DO K = 1, NSPMD
          IF (K/=LOC_PROC.AND.(isendto(NIN,K)/=0.OR.
     .   ircvfrom(NIN,K)/=0)) THEN
            MSGTYP=MSGOFF      
            CALL MPI_SEND(INACTI,1,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,ierror)
          ENDIF
        ENDDO
      ENDIF
C
      IPARI22 = INACTI
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_GET_STIF                 source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        IMP_ICOMCRIT                  source/implicit/imp_int_k.F   
Chd|        INTCRIT                       source/interfaces/intsort/intcrit.F
Chd|-- calls ---------------
Chd|        SPMD_SD_STFN                  source/mpi/interfaces/send_cand.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_GET_STIF(
     .    NEWFRONT,I_STOK ,CAND_N  ,STFN ,NSN ,
     .    NIN     ,ISENDTO,IRCVFROM,NSV  ,ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEWFRONT, I_STOK, NSN, NIN, CAND_N(*), NSV(*),
     .        ITAB(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*)
      my_real
     .        STFN(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,K,LEN,LENX,LENR,IDEB,P,NB,
     .        MSGOFF,MSGTYP,INFO,PMAIN, LOC_PROC,
     .        IENVOI(NSPMD)
      DATA MSGOFF/1001/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LOC_PROC=ISPMD+1
      IF(NSPMD==1.OR.(ISENDTO(NIN,LOC_PROC)==0.AND.
     .   IRCVFROM(NIN,LOC_PROC)==0)) THEN
        NEWFRONT = 0
        RETURN
      ENDIF
C
C   determination du proc main (celui qui fait joue le role de P0)
C
      DO K = 1, NSPMD
        IF (ISENDTO(NIN,K)/=0.OR.
     .      IRCVFROM(NIN,K)/=0) THEN
          PMAIN = K
          GOTO 110
        ENDIF
      ENDDO
 110  CONTINUE
C
      LEN = 0
C traitement sur tout NSN pour le cas shooting nodes
      IF(IDEL7NG>=1)THEN
        DO I = 1, NSN
          IF(STFN(I)<ZERO) THEN
            LEN = LEN + 1
          ENDIF
        ENDDO
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)
            DO I = IDEB+1, IDEB+NB
              IF(STIFI(NIN)%P(I)<ZERO) THEN
                LEN = LEN + 1
              END IF
            END DO
            IDEB = IDEB + NB
          END IF
        END DO
      ELSE
        DO I = 1, I_STOK
          IF(CAND_N(I)<=NSN) THEN
Candidat interne
            IF(STFN(CAND_N(I))<ZERO) THEN
              LEN = LEN + 1
            ENDIF
Candidat frontiere
          ELSEIF(STIFI(NIN)%P(CAND_N(I)-NSN)<ZERO) THEN
            LEN = LEN + 1
          END IF
        END DO
      END IF
      IENVOI(LOC_PROC) = LEN
C
      IF (LOC_PROC/=PMAIN) THEN
C   pack des candidats ayant stifness negative
C
        MSGTYP=MSGOFF        
        CALL MPI_SEND(LEN,1,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,
     .                MPI_COMM_WORLD,IERROR)
        CALL MPI_RECV(LENX,1,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,   
     .                MPI_COMM_WORLD,STATUS,IERROR)
      ELSE
Calcul taille totale
        LENX = LEN
        DO K = 1, NSPMD
          IENVOI(K) = 0
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .        IRCVFROM(NIN,K)/=0)) THEN            
            MSGTYP=MSGOFF
            CALL MPI_RECV(LENR,1,MPI_INTEGER,IT_SPMD(K),MSGTYP,  
     .                    MPI_COMM_WORLD,STATUS,IERROR)
            LENX = LENX + LENR
            IENVOI(K) = LENR
          ENDIF
        ENDDO
C
        DO K = 1, NSPMD
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .   IRCVFROM(NIN,K)/=0)) THEN
          MSGTYP=MSGOFF      
          CALL MPI_SEND(LENX,1,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
         ENDIF
        ENDDO
      ENDIF
C envoi des stifness
      CALL SPMD_SD_STFN(LOC_PROC,PMAIN ,LENX ,CAND_N,NSV    ,
     2                  I_STOK  ,NSN     ,STFN ,IENVOI,ISENDTO,
     3                  IRCVFROM,ITAB    ,NIN  )
C
      NEWFRONT = 0
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_GET_STIF25               source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        INTCRIT                       source/interfaces/intsort/intcrit.F
Chd|-- calls ---------------
Chd|        SPMD_SD_STFN25                source/mpi/interfaces/send_cand.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_GET_STIF25(
     .    NEWFRONT,I_STOK ,CAND_N  ,STFN ,NSN ,
     .    NIN     ,ISENDTO,IRCVFROM,NSV  ,ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEWFRONT, I_STOK, NSN, NIN, CAND_N(*), NSV(*),
     .        ITAB(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*)
      my_real
     .        STFN(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,K,LEN,LENX,LENR,IDEB,P,NB,
     .        MSGOFF,MSGTYP,INFO,PMAIN, LOC_PROC,
     .        IENVOI(NSPMD)
      DATA MSGOFF/1001/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LOC_PROC=ISPMD+1
      IF(NSPMD==1.OR.(ISENDTO(NIN,LOC_PROC)==0.AND.
     .   IRCVFROM(NIN,LOC_PROC)==0)) THEN
        NEWFRONT = 0
        RETURN
      ENDIF
C
C   determination du proc main (celui qui fait joue le role de P0)
C
      DO K = 1, NSPMD
        IF (ISENDTO(NIN,K)/=0.OR.
     .      IRCVFROM(NIN,K)/=0) THEN
          PMAIN = K
          GOTO 110
        ENDIF
      ENDDO
 110  CONTINUE
C
      LEN = 0
C traitement sur tout NSN pour les nds precedemment impactes (ne figurent pas dans I_STOK)
      DO I = 1, NSN
        IF(STFN(I)<ZERO) THEN
          LEN = LEN + 1
        ENDIF
      ENDDO
      IDEB = 0
      DO P = 1, NSPMD
        IF(P/=LOC_PROC)THEN
          NB = NSNFI(NIN)%P(P)
          DO I = IDEB+1, IDEB+NB
            IF(STIFI(NIN)%P(I)<ZERO) THEN
              LEN = LEN + 1
            END IF
          END DO
          IDEB = IDEB + NB
        END IF
      END DO
      IENVOI(LOC_PROC) = LEN
C
      IF (LOC_PROC/=PMAIN) THEN
C   pack des candidats ayant stifness negative
C
        MSGTYP=MSGOFF        
        CALL MPI_SEND(LEN,1,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,
     .                MPI_COMM_WORLD,IERROR)
        CALL MPI_RECV(LENX,1,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,   
     .                MPI_COMM_WORLD,STATUS,IERROR)
      ELSE
Calcul taille totale
        LENX = LEN
        DO K = 1, NSPMD
          IENVOI(K) = 0
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .        IRCVFROM(NIN,K)/=0)) THEN            
            MSGTYP=MSGOFF
            CALL MPI_RECV(LENR,1,MPI_INTEGER,IT_SPMD(K),MSGTYP,  
     .                    MPI_COMM_WORLD,STATUS,IERROR)
            LENX = LENX + LENR
            IENVOI(K) = LENR
          ENDIF
        ENDDO
C
        DO K = 1, NSPMD
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .   IRCVFROM(NIN,K)/=0)) THEN
          MSGTYP=MSGOFF      
          CALL MPI_SEND(LENX,1,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
         ENDIF
        ENDDO
      ENDIF
C envoi des stifness
      CALL SPMD_SD_STFN25(LOC_PROC,PMAIN ,LENX ,CAND_N,NSV    ,
     2                  I_STOK  ,NSN     ,STFN ,IENVOI,ISENDTO,
     3                  IRCVFROM,ITAB    ,NIN  )
C
      NEWFRONT = 0
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_GET_STIF20               source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        INTCRIT                       source/interfaces/intsort/intcrit.F
Chd|-- calls ---------------
Chd|        SPMD_SD_STFA20                source/mpi/interfaces/send_cand.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_GET_STIF20(
     1    NEWFRONT,I_STOK ,CAND_N  ,STFA ,NSN ,
     2    NIN     ,ISENDTO,IRCVFROM,NSV  ,ITAB,
     3    NLG     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEWFRONT, I_STOK, NSN, NIN, CAND_N(*), NSV(*),
     .        ITAB(*), NLG(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*)
      my_real
     .        STFA(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,K,LEN,LENX,LENR,IDEB,P,NB,
     .        MSGOFF,MSGTYP,INFO,PMAIN, LOC_PROC,
     .        IENVOI(NSPMD)
      DATA MSGOFF/1002/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LOC_PROC=ISPMD+1
      IF(NSPMD==1.OR.(ISENDTO(NIN,LOC_PROC)==0.AND.
     .   IRCVFROM(NIN,LOC_PROC)==0)) THEN
        NEWFRONT = 0
        RETURN
      ENDIF
C
C   determination du proc main (celui qui fait joue le role de P0)
C
      DO K = 1, NSPMD
        IF (ISENDTO(NIN,K)/=0.OR.
     .      IRCVFROM(NIN,K)/=0) THEN
          PMAIN = K
          GOTO 110
        ENDIF
      ENDDO
 110  CONTINUE
C
      LEN = 0
C traitement sur tout NSN pour le cas shooting nodes
      IF(IDEL7NG>=1)THEN
        DO I = 1, NSN
          IF(STFA(NSV(I))<ZERO) THEN
            LEN = LEN + 1
          ENDIF
        ENDDO
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)
            DO I = IDEB+1, IDEB+NB
              IF(STIFI(NIN)%P(I)<ZERO) THEN
                LEN = LEN + 1
              END IF
            END DO
            IDEB = IDEB + NB
          END IF
        END DO
      ELSE
        DO I = 1, I_STOK
          IF(CAND_N(I)<=NSN) THEN
Candidat interne
            IF(STFA(NSV(CAND_N(I)))<ZERO) THEN
              LEN = LEN + 1
            ENDIF
Candidat frontiere
          ELSEIF(STIFI(NIN)%P(CAND_N(I)-NSN)<ZERO) THEN
            LEN = LEN + 1
          END IF
        END DO
      END IF
      IENVOI(LOC_PROC) = LEN
C
      IF (LOC_PROC/=PMAIN) THEN
C   pack des candidats ayant stifness negative
C
        MSGTYP=MSGOFF        
        CALL MPI_SEND(LEN,1,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,
     .                MPI_COMM_WORLD,IERROR)
        CALL MPI_RECV(LENX,1,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,   
     .                MPI_COMM_WORLD,STATUS,IERROR)
      ELSE
Calcul taille totale
        LENX = LEN
        DO K = 1, NSPMD
          IENVOI(K) = 0
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .        IRCVFROM(NIN,K)/=0)) THEN            
            MSGTYP=MSGOFF
            CALL MPI_RECV(LENR,1,MPI_INTEGER,IT_SPMD(K),MSGTYP,  
     .                    MPI_COMM_WORLD,STATUS,IERROR)
            LENX = LENX + LENR
            IENVOI(K) = LENR
          ENDIF
        ENDDO
C
        DO K = 1, NSPMD
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .   IRCVFROM(NIN,K)/=0)) THEN
          MSGTYP=MSGOFF      
          CALL MPI_SEND(LENX,1,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
         ENDIF
        ENDDO
      ENDIF
C envoi des stifness
      CALL SPMD_SD_STFA20(LOC_PROC,PMAIN ,LENX ,CAND_N,NSV    ,
     2                    I_STOK  ,NSN     ,STFA ,IENVOI,ISENDTO,
     3                    IRCVFROM,ITAB    ,NLG  ,NIN   )
C
      NEWFRONT = 0
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_GET_STIF11               source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        IMP_ICOMCRIT                  source/implicit/imp_int_k.F   
Chd|        INTCRIT                       source/interfaces/intsort/intcrit.F
Chd|-- calls ---------------
Chd|        SPMD_SD_STFN11                source/mpi/interfaces/send_cand.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_GET_STIF11(
     .    NEWFRONT,I_STOK ,CAND_S  ,STFS  ,NRTS ,
     .    NIN     ,ISENDTO,IRCVFROM,IRECTS,ITAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEWFRONT, I_STOK, NRTS, NIN, CAND_S(*), IRECTS(2,*),
     .        ITAB(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*)
      my_real
     .        STFS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,K,LEN,LENX,LENR,IDEB,P,NB,
     .        MSGOFF,MSGTYP,INFO,PMAIN, LOC_PROC,
     .        IENVOI(NSPMD)
      DATA MSGOFF/1003/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LOC_PROC=ISPMD+1
      IF(NSPMD==1.OR.(ISENDTO(NIN,LOC_PROC)==0.AND.
     .   IRCVFROM(NIN,LOC_PROC)==0)) THEN
        NEWFRONT = 0
        RETURN
      ENDIF
C
C   determination du proc main (celui qui fait joue le role de P0)
C
      DO K = 1, NSPMD
        IF (ISENDTO(NIN,K)/=0.OR.
     .      IRCVFROM(NIN,K)/=0) THEN
          PMAIN = K
          GOTO 110
        ENDIF
      ENDDO
 110  CONTINUE
C
      LEN = 0
C traitement sur tout NRTS pour le cas shooting nodes
      IF(IDEL7NG>=1)THEN
        DO I = 1, NRTS
          IF(STFS(I)<ZERO) THEN
            LEN = LEN + 2
          ENDIF
        ENDDO
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)
            DO I = IDEB+1, IDEB+NB
              IF(STIFI(NIN)%P(I)<ZERO) THEN
                LEN = LEN + 2
              END IF
            END DO
            IDEB = IDEB + NB
          END IF
        END DO
      ELSE
        DO I = 1, I_STOK
          IF(CAND_S(I)<=NRTS) THEN
Candidat interne
            IF(STFS(CAND_S(I))<ZERO) THEN
              LEN = LEN + 2
            ENDIF
Candidat frontiere
          ELSEIF(STIFI(NIN)%P(CAND_S(I)-NRTS)<ZERO) THEN
            LEN = LEN + 2
          ENDIF
        ENDDO
      END IF
      IENVOI(LOC_PROC) = LEN
C
      IF (LOC_PROC/=PMAIN) THEN
C   pack des candidats ayant stifness negative
C
        MSGTYP=MSGOFF        
        CALL MPI_SEND(LEN,1,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,
     .                MPI_COMM_WORLD,IERROR)
        CALL MPI_RECV(LENX,1,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,   
     .                MPI_COMM_WORLD,STATUS,IERROR)
      ELSE
Calcul taille totale
        LENX = LEN
        DO K = 1, NSPMD
          IENVOI(K) = 0
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .        IRCVFROM(NIN,K)/=0)) THEN            
            MSGTYP=MSGOFF
            CALL MPI_RECV(LENR,1,MPI_INTEGER,IT_SPMD(K),MSGTYP,  
     .                    MPI_COMM_WORLD,STATUS,IERROR)
            LENX = LENX + LENR
            IENVOI(K) = LENR
          ENDIF
        ENDDO
C
        DO K = 1, NSPMD
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .   IRCVFROM(NIN,K)/=0)) THEN
          MSGTYP=MSGOFF      
          CALL MPI_SEND(LENX,1,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
         ENDIF
        ENDDO
      ENDIF
C envoi des stifness
      CALL SPMD_SD_STFN11(LOC_PROC,PMAIN ,LENX ,CAND_S,IRECTS ,
     2                    I_STOK  ,NRTS    ,STFS ,IENVOI,ISENDTO,
     3                    IRCVFROM,ITAB    ,NIN  )
C
      NEWFRONT = 0
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_GET_STIF20E              source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        INTCRIT                       source/interfaces/intsort/intcrit.F
Chd|-- calls ---------------
Chd|        SPMD_SD_STFN20E               source/mpi/interfaces/send_cand.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_GET_STIF20E(
     .    NEWFRONT,I_STOK ,CAND_S  ,STFS  ,NLINSA ,
     .    NIN     ,ISENDTO,IRCVFROM,IXLINS,ITAB   ,
     .    NLG     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEWFRONT, I_STOK, NLINSA, NIN, CAND_S(*), IXLINS(2,*),
     .        ITAB(*), NLG(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*)
      my_real
     .        STFS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,K,LEN,LENX,LENR,IDEB,P,NB,
     .        MSGOFF,MSGTYP,INFO,PMAIN, LOC_PROC,
     .        IENVOI(NSPMD)
      DATA MSGOFF/1004/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LOC_PROC=ISPMD+1
      IF(NSPMD==1.OR.(ISENDTO(NIN,LOC_PROC)==0.AND.
     .   IRCVFROM(NIN,LOC_PROC)==0)) THEN
        NEWFRONT = 0
        RETURN
      ENDIF
C
C   determination du proc main (celui qui fait joue le role de P0)
C
      DO K = 1, NSPMD
        IF (ISENDTO(NIN,K)/=0.OR.
     .      IRCVFROM(NIN,K)/=0) THEN
          PMAIN = K
          GOTO 110
        ENDIF
      ENDDO
 110  CONTINUE
C
      LEN = 0
C traitement sur tout NLINSA pour le cas shooting nodes
      IF(IDEL7NG>=1)THEN
        DO I = 1,  NLINSA
          IF(STFS(I)<ZERO) THEN
            LEN = LEN + 2
          ENDIF
        ENDDO
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFIE(NIN)%P(P)
            DO I = IDEB+1, IDEB+NB
              IF(STIFIE(NIN)%P(I)<ZERO) THEN
                LEN = LEN + 2
              END IF
            END DO
            IDEB = IDEB + NB
          END IF
        END DO
      ELSE
        DO I = 1, I_STOK
          IF(CAND_S(I)<= NLINSA) THEN
Candidat interne
            IF(STFS(CAND_S(I))<ZERO) THEN
              LEN = LEN + 2
            ENDIF
Candidat frontiere
          ELSEIF(STIFIE(NIN)%P(CAND_S(I)-NLINSA)<ZERO) THEN
            LEN = LEN + 2
          ENDIF
        ENDDO
      END IF
      IENVOI(LOC_PROC) = LEN
C
      IF (LOC_PROC/=PMAIN) THEN
C   pack des candidats ayant stifness negative
C
        MSGTYP=MSGOFF        
        CALL MPI_SEND(LEN,1,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,
     .                MPI_COMM_WORLD,IERROR)
        CALL MPI_RECV(LENX,1,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,   
     .                MPI_COMM_WORLD,STATUS,IERROR)
      ELSE
Calcul taille totale
        LENX = LEN
        DO K = 1, NSPMD
          IENVOI(K) = 0
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .        IRCVFROM(NIN,K)/=0)) THEN            
            MSGTYP=MSGOFF
            CALL MPI_RECV(LENR,1,MPI_INTEGER,IT_SPMD(K),MSGTYP,  
     .                    MPI_COMM_WORLD,STATUS,IERROR)
            LENX = LENX + LENR
            IENVOI(K) = LENR
          ENDIF
        ENDDO
C
        DO K = 1, NSPMD
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .   IRCVFROM(NIN,K)/=0)) THEN
          MSGTYP=MSGOFF      
          CALL MPI_SEND(LENX,1,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
         ENDIF
        ENDDO
      ENDIF
C envoi des stifness
      CALL SPMD_SD_STFN20E(LOC_PROC,PMAIN ,LENX ,CAND_S,IXLINS ,
     2                     I_STOK  ,NLINSA  ,STFS ,IENVOI,ISENDTO,
     3                     IRCVFROM,ITAB    ,NLG  ,NIN   )
C
      NEWFRONT = 0
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SD_STFN                  source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        SPMD_GET_STIF                 source/mpi/interfaces/send_cand.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE  SPMD_SD_STFN(
     1   LOC_PROC,PMAIN ,LENX ,CAND_N,NSV    ,
     2   I_STOK  ,NSN     ,STFN ,IENVOI,ISENDTO,
     3   IRCVFROM,ITAB    ,NIN  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LOC_PROC, PMAIN, I_STOK, NSN, LENX, NIN,
     .        IENVOI(*), CAND_N(*), NSV(*), ITAB(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*)
      my_real
     .        STFN(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,K,LEN,LENR,NUSER,IDEB,P,NB,
     .        MSGOFF,MSGTYP,
     .        IBUFFER(LENX)
      DATA MSGOFF/1005/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LEN = 0
C traitement sur tout NSN pour le cas shooting nodes
      IF(IDEL7NG>=1)THEN
        DO I = 1, NSN
          IF(STFN(I)<ZERO) THEN
            LEN = LEN + 1
            IBUFFER(LEN) = ITAB(NSV(I))
          ENDIF
        ENDDO
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)
            DO I = IDEB+1, IDEB+NB
              IF(STIFI(NIN)%P(I)<ZERO) THEN
                LEN = LEN + 1
                IBUFFER(LEN) = ITAFI(NIN)%P(I)
              END IF
            END DO
            IDEB = IDEB + NB
          END IF
        END DO
      ELSE
        DO I = 1, I_STOK
          IF(CAND_N(I)<=NSN) THEN
Candidat interne
            IF(STFN(CAND_N(I))<ZERO) THEN
              LEN = LEN + 1
              IBUFFER(LEN) = ITAB(NSV(CAND_N(I)))
            ENDIF
Candidat frontiere
          ELSEIF(STIFI(NIN)%P(CAND_N(I)-NSN)<ZERO) THEN
            LEN = LEN + 1
            IBUFFER(LEN) = ITAFI(NIN)%P(CAND_N(I)-NSN)
          ENDIF
        ENDDO
      END IF
C
      IF (LOC_PROC/=PMAIN) THEN
C   pack des candidats ayant stifness negative
C
        MSGTYP=MSGOFF        
        IF(LEN>0) THEN
          CALL MPI_SEND(IBUFFER,LEN,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
        ENDIF
        CALL MPI_RECV(IBUFFER,LENX,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,   
     .                MPI_COMM_WORLD,STATUS,IERROR)
      ELSE
Calcul taille totale
        LENR = LEN
        DO K = 1, NSPMD
          IF (IENVOI(K)/=0) THEN            
            MSGTYP=MSGOFF
            CALL MPI_RECV(
     .        IBUFFER(LENR+1),IENVOI(K),MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR    )
            LENR = LENR + IENVOI(K)
          ENDIF
        ENDDO
C
        DO K = 1, NSPMD
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .   IRCVFROM(NIN,K)/=0)) THEN
          MSGTYP=MSGOFF      
          CALL MPI_SEND(IBUFFER,LENX,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
          ENDIF
        ENDDO
C envoi des stifness
      ENDIF
C
      DO I = 1, LENX
        NUSER = IBUFFER(I)
C noeud interne
        DO K = 1, NSN
          IF(ITAB(NSV(K))==NUSER) THEN
            STFN(K) = ZERO
          ENDIF
        ENDDO
Candidat frontiere
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)
            DO K = IDEB+1, IDEB+NB
              IF(ITAFI(NIN)%P(K)==NUSER) THEN
               STIFI(NIN)%P(K) = ZERO
              END IF
            END DO
            IDEB = IDEB + NB
          END IF
        END DO
      ENDDO
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SD_STFN25                source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        SPMD_GET_STIF25               source/mpi/interfaces/send_cand.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE  SPMD_SD_STFN25(
     1   LOC_PROC,PMAIN ,LENX ,CAND_N,NSV    ,
     2   I_STOK  ,NSN     ,STFN ,IENVOI,ISENDTO,
     3   IRCVFROM,ITAB    ,NIN  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LOC_PROC, PMAIN, I_STOK, NSN, LENX, NIN,
     .        IENVOI(*), CAND_N(*), NSV(*), ITAB(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*)
      my_real
     .        STFN(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,K,LEN,LENR,NUSER,IDEB,P,NB,
     .        MSGOFF,MSGTYP,
     .        IBUFFER(LENX)
      DATA MSGOFF/1005/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LEN = 0
C traitement sur tout NSN pour les nds precedemment impactes (ne figurent pas dans I_STOK)
      DO I = 1, NSN
        IF(STFN(I)<ZERO) THEN
          LEN = LEN + 1
          IBUFFER(LEN) = ITAB(NSV(I))
        ENDIF
      ENDDO
      IDEB = 0
      DO P = 1, NSPMD
        IF(P/=LOC_PROC)THEN
          NB = NSNFI(NIN)%P(P)
          DO I = IDEB+1, IDEB+NB
            IF(STIFI(NIN)%P(I)<ZERO) THEN
              LEN = LEN + 1
              IBUFFER(LEN) = ITAFI(NIN)%P(I)
            END IF
          END DO
          IDEB = IDEB + NB
        END IF
      END DO
C
      IF (LOC_PROC/=PMAIN) THEN
C   pack des candidats ayant stifness negative
C
        MSGTYP=MSGOFF        
        IF(LEN>0) THEN
          CALL MPI_SEND(IBUFFER,LEN,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
        ENDIF
        CALL MPI_RECV(IBUFFER,LENX,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,   
     .                MPI_COMM_WORLD,STATUS,IERROR)
      ELSE
Calcul taille totale
        LENR = LEN
        DO K = 1, NSPMD
          IF (IENVOI(K)/=0) THEN            
            MSGTYP=MSGOFF
            CALL MPI_RECV(
     .        IBUFFER(LENR+1),IENVOI(K),MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR    )
            LENR = LENR + IENVOI(K)
          ENDIF
        ENDDO
C
        DO K = 1, NSPMD
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .   IRCVFROM(NIN,K)/=0)) THEN
          MSGTYP=MSGOFF      
          CALL MPI_SEND(IBUFFER,LENX,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
          ENDIF
        ENDDO
C envoi des stifness
      ENDIF
C
      DO I = 1, LENX
        NUSER = IBUFFER(I)
C noeud interne
        DO K = 1, NSN
          IF(ITAB(NSV(K))==NUSER) THEN
            STFN(K) = ZERO
          ENDIF
        ENDDO
Candidat frontiere
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)
            DO K = IDEB+1, IDEB+NB
              IF(ITAFI(NIN)%P(K)==NUSER) THEN
               STIFI(NIN)%P(K) = ZERO
              END IF
            END DO
            IDEB = IDEB + NB
          END IF
        END DO
      ENDDO
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SD_STFA20                source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        SPMD_GET_STIF20               source/mpi/interfaces/send_cand.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE  SPMD_SD_STFA20(
     1   LOC_PROC,PMAIN ,LENX ,CAND_N,NSV    ,
     2   I_STOK  ,NSN     ,STFA ,IENVOI,ISENDTO,
     3   IRCVFROM,ITAB    ,NLG  ,NIN   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LOC_PROC, PMAIN, I_STOK, NSN, LENX, NIN,
     .        IENVOI(*), CAND_N(*), NSV(*), ITAB(*), NLG(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*)
      my_real
     .        STFA(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,K,LEN,LENR,NUSER,IDEB,P,NB,
     .        MSGOFF,MSGTYP,
     .        IBUFFER(LENX)
      DATA MSGOFF/1006/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LEN = 0
C traitement sur tout NSN pour le cas shooting nodes
      IF(IDEL7NG>=1)THEN
        DO I = 1, NSN
          IF(STFA(NSV(I))<ZERO) THEN
            LEN = LEN + 1
            IBUFFER(LEN) = ITAB(NLG(NSV(I)))
          ENDIF
        ENDDO
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)
            DO I = IDEB+1, IDEB+NB
              IF(STIFI(NIN)%P(I)<ZERO) THEN
                LEN = LEN + 1
                IBUFFER(LEN) = ITAFI(NIN)%P(I)
              END IF
            END DO
            IDEB = IDEB + NB
          END IF
        END DO
      ELSE
        DO I = 1, I_STOK
          IF(CAND_N(I)<=NSN) THEN
Candidat interne
            IF(STFA(NSV(CAND_N(I)))<ZERO) THEN
              LEN = LEN + 1
              IBUFFER(LEN) = ITAB(NLG(NSV(CAND_N(I))))
            ENDIF
Candidat frontiere
          ELSEIF(STIFI(NIN)%P(CAND_N(I)-NSN)<ZERO) THEN
            LEN = LEN + 1
            IBUFFER(LEN) = ITAFI(NIN)%P(CAND_N(I)-NSN)
          ENDIF
        ENDDO
      END IF
C
      IF (LOC_PROC/=PMAIN) THEN
C   pack des candidats ayant stifness negative
C
        MSGTYP=MSGOFF        
        IF(LEN>0) THEN
          CALL MPI_SEND(IBUFFER,LEN,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
        ENDIF
        CALL MPI_RECV(IBUFFER,LENX,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,   
     .                MPI_COMM_WORLD,STATUS,IERROR)
      ELSE
Calcul taille totale
        LENR = LEN
        DO K = 1, NSPMD
          IF (IENVOI(K)/=0) THEN            
            MSGTYP=MSGOFF
            CALL MPI_RECV(
     .        IBUFFER(LENR+1),IENVOI(K),MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR    )
            LENR = LENR + IENVOI(K)
          ENDIF
        ENDDO
C
        DO K = 1, NSPMD
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .        IRCVFROM(NIN,K)/=0)) THEN
          MSGTYP=MSGOFF      
          CALL MPI_SEND(IBUFFER,LENX,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
          ENDIF
        ENDDO
C envoi des stifness
      ENDIF
C
      DO I = 1, LENX
        NUSER = IBUFFER(I)
C noeud interne
        DO K = 1, NSN
          IF(ITAB(NLG(NSV(K)))==NUSER) THEN
            STFA(NSV(K)) = ZERO
          ENDIF
        ENDDO
Candidat frontiere
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)
            DO K = IDEB+1, IDEB+NB
              IF(ITAFI(NIN)%P(K)==NUSER) THEN
               STIFI(NIN)%P(K) = ZERO
              END IF
            END DO
            IDEB = IDEB + NB
          END IF
        END DO
      ENDDO
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SD_STFN11                source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        SPMD_GET_STIF11               source/mpi/interfaces/send_cand.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE  SPMD_SD_STFN11(
     1   LOC_PROC,PMAIN ,LENX ,CAND_S,IRECTS ,
     2   I_STOK  ,NRTS    ,STFS ,IENVOI,ISENDTO,
     3   IRCVFROM,ITAB    ,NIN  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LOC_PROC, PMAIN, I_STOK, LENX, NIN, NRTS,
     .        IENVOI(*), CAND_S(*), IRECTS(2,*), ITAB(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*)
      my_real
     .        STFS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,K,LEN,LENR,NUSER1,NUSER2,N1,N2,NI,
     .        MSGOFF,MSGTYP,IDEB,P,NB,
     .        IBUFFER(LENX)
      DATA MSGOFF/1007/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LEN = 0
C traitement sur tout NSN pour le cas shooting nodes
      IF(IDEL7NG>=1)THEN
        DO I = 1, NRTS
          IF(STFS(I)<ZERO) THEN
            N1 = IRECTS(1,I)
            N2 = IRECTS(2,I)
            IBUFFER(LEN+1) = ITAB(N1)
            IBUFFER(LEN+2) = ITAB(N2)
            LEN = LEN + 2
          END IF
        END DO
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)
            DO I = IDEB+1, IDEB+NB
              IF(STIFI(NIN)%P(I)<ZERO) THEN
                N1 = 2*(I-1)+1
                N2 = 2*I
                IBUFFER(LEN+1) = ITAFI(NIN)%P(N1)
                IBUFFER(LEN+2) = ITAFI(NIN)%P(N2)
                LEN = LEN + 2
              END IF
            END DO
            IDEB = IDEB + NB
          END IF
        END DO
      ELSE
        DO I = 1, I_STOK
          IF(CAND_S(I)<=NRTS) THEN
Candidat interne
            IF(STFS(CAND_S(I))<ZERO) THEN
              N1 = IRECTS(1,CAND_S(I))
              N2 = IRECTS(2,CAND_S(I))
              IBUFFER(LEN+1) = ITAB(N1)
              IBUFFER(LEN+2) = ITAB(N2)
              LEN = LEN + 2
            ENDIF
Candidat frontiere
          ELSEIF(STIFI(NIN)%P(CAND_S(I)-NRTS)<ZERO) THEN
            NI = CAND_S(I)-NRTS
            N1 = 2*(NI-1)+1
            N2 = 2*NI
            IBUFFER(LEN+1) = ITAFI(NIN)%P(N1)
            IBUFFER(LEN+2) = ITAFI(NIN)%P(N2)
            LEN = LEN + 2
          ENDIF
        ENDDO
      ENDIF
C
      IF (LOC_PROC/=PMAIN) THEN
C   pack des candidats ayant stifness negative
C
        MSGTYP=MSGOFF        
        IF(LEN>0) THEN
          CALL MPI_SEND(IBUFFER,LEN,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
        ENDIF
        CALL MPI_RECV(IBUFFER,LENX,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,   
     .                MPI_COMM_WORLD,STATUS,IERROR)
      ELSE
Calcul taille totale
        LENR = LEN
        DO K = 1, NSPMD
          IF (IENVOI(K)/=0) THEN            
            MSGTYP=MSGOFF
            CALL MPI_RECV(
     .        IBUFFER(LENR+1),IENVOI(K),MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR    )
            LENR = LENR + IENVOI(K)
          ENDIF
        ENDDO
C
        DO K = 1, NSPMD
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .   IRCVFROM(NIN,K)/=0)) THEN
          MSGTYP=MSGOFF      
          CALL MPI_SEND(IBUFFER,LENX,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
          ENDIF
        ENDDO
C envoi des stifness
      ENDIF
C
      DO I = 1, LENX/2
        NUSER1 = IBUFFER(2*(I-1)+1)
        NUSER2 = IBUFFER(2*I)
C arete interne
        DO K = 1, NRTS
          N1 = IRECTS(1,K)
          N2 = IRECTS(2,K)
          IF(ITAB(N1)==NUSER1.AND.ITAB(N2)==NUSER2) THEN
            STFS(K) = ZERO
          END IF
        END DO
Candidat frontiere
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)
            DO K = IDEB+1, IDEB+NB
              N1 = 2*(K-1)+1
              N2 = 2*K
              IF(ITAFI(NIN)%P(N1)==NUSER1.AND.
     .           ITAFI(NIN)%P(N2)==NUSER2) THEN
                STIFI(NIN)%P(K) = ZERO
              END IF
            END DO
            IDEB = IDEB + NB
          END IF
        END DO
      END DO
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SD_STFN20E               source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        SPMD_GET_STIF20E              source/mpi/interfaces/send_cand.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE  SPMD_SD_STFN20E(
     1   LOC_PROC,PMAIN ,LENX ,CAND_S,IXLINS ,
     2   I_STOK  ,NLINSA  ,STFS ,IENVOI,ISENDTO,
     3   IRCVFROM,ITAB    ,NLG  ,NIN   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LOC_PROC, PMAIN, I_STOK, LENX, NIN, NLINSA,
     .        IENVOI(*), CAND_S(*), IXLINS(2,*), ITAB(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*), NLG(*)
      my_real
     .        STFS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,K,LEN,LENR,NUSER1,NUSER2,N1,N2,NI,
     .        MSGOFF,MSGTYP,IDEB,P,NB,N1L,N2L,
     .        IBUFFER(LENX)
      DATA MSGOFF/1008/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LEN = 0
C traitement sur tout NSN pour le cas shooting nodes
      IF(IDEL7NG>=1)THEN
        DO I = 1, NLINSA
          IF(STFS(I)<ZERO) THEN
            N1L = IXLINS(1,I)
            N2L = IXLINS(2,I)
            N1 = NLG(N1L)
            N2 = NLG(N2L)
            IBUFFER(LEN+1) = ITAB(N1)
            IBUFFER(LEN+2) = ITAB(N2)
            LEN = LEN + 2
          END IF
        END DO
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFIE(NIN)%P(P)
            DO I = IDEB+1, IDEB+NB
              IF(STIFIE(NIN)%P(I)<ZERO) THEN
                N1 = 2*(I-1)+1
                N2 = 2*I
                IBUFFER(LEN+1) = ITAFIE(NIN)%P(N1)
                IBUFFER(LEN+2) = ITAFIE(NIN)%P(N2)
                LEN = LEN + 2
              END IF
            END DO
            IDEB = IDEB + NB
          END IF
        END DO
      ELSE
        DO I = 1, I_STOK
          IF(CAND_S(I)<=NLINSA) THEN
Candidat interne
            IF(STFS(CAND_S(I))<ZERO) THEN
              N1L = IXLINS(1,CAND_S(I))
              N2L = IXLINS(2,CAND_S(I))
              N1 = NLG(N1L)
              N2 = NLG(N2L)
              IBUFFER(LEN+1) = ITAB(N1)
              IBUFFER(LEN+2) = ITAB(N2)
              LEN = LEN + 2
            ENDIF
Candidat frontiere
          ELSEIF(STIFIE(NIN)%P(CAND_S(I)-NLINSA)<ZERO) THEN
            NI = CAND_S(I)-NLINSA
            N1 = 2*(NI-1)+1
            N2 = 2*NI
            IBUFFER(LEN+1) = ITAFIE(NIN)%P(N1)
            IBUFFER(LEN+2) = ITAFIE(NIN)%P(N2)
            LEN = LEN + 2
          ENDIF
        ENDDO
      ENDIF
C
      IF (LOC_PROC/=PMAIN) THEN
C   pack des candidats ayant stifness negative
C
        MSGTYP=MSGOFF        
        IF(LEN>0) THEN
          CALL MPI_SEND(IBUFFER,LEN,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
        ENDIF
        CALL MPI_RECV(IBUFFER,LENX,MPI_INTEGER,IT_SPMD(PMAIN),MSGTYP,   
     .                MPI_COMM_WORLD,STATUS,IERROR)
      ELSE
Calcul taille totale
        LENR = LEN
        DO K = 1, NSPMD
          IF (IENVOI(K)/=0) THEN            
            MSGTYP=MSGOFF
            CALL MPI_RECV(
     .        IBUFFER(LENR+1),IENVOI(K),MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR    )
            LENR = LENR + IENVOI(K)
          ENDIF
        ENDDO
C
        DO K = 1, NSPMD
          IF (K/=LOC_PROC.AND.(ISENDTO(NIN,K)/=0.OR.
     .   IRCVFROM(NIN,K)/=0)) THEN
          MSGTYP=MSGOFF      
          CALL MPI_SEND(IBUFFER,LENX,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,IERROR)
          ENDIF
        ENDDO
C envoi des stifness
      ENDIF
C
      DO I = 1, LENX/2
        NUSER1 = IBUFFER(2*(I-1)+1)
        NUSER2 = IBUFFER(2*I)
C arete interne
        DO K = 1, NLINSA
          N1L = IXLINS(1,K)
          N2L = IXLINS(2,K)
          N1 = NLG(N1L)
          N2 = NLG(N2L)
          IF(ITAB(N1)==NUSER1.AND.ITAB(N2)==NUSER2) THEN
            STFS(K) = ZERO
          END IF
        END DO
Candidat frontiere
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFIE(NIN)%P(P)
            DO K = IDEB+1, IDEB+NB
              N1 = 2*(K-1)+1
              N2 = 2*K
              IF(ITAFIE(NIN)%P(N1)==NUSER1.AND.
     .           ITAFIE(NIN)%P(N2)==NUSER2) THEN
                STIFIE(NIN)%P(K) = ZERO
              END IF
            END DO
            IDEB = IDEB + NB
          END IF
        END DO
      END DO
C
#endif
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_IFRONT_STAMP             source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SPMD_IFRONT_STAMP(
     1   IPARI   ,NSENSOR ,INTBUF_TAB, RETRI,TEMP    ,SENSOR_TAB  ,ITAB,
     2   NBINTC21,INTLIST21)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTSTAMP_MOD
      USE INTSTAMP_GLOB_MOD
      USE INTBUFDEF_MOD 
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "intstamp_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER IPARI(NPARI,NINTER),RETRI(*),ITAB(*),
     .        NBINTC21 ,INTLIST21(*)
C     REAL
      my_real :: TEMP(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER LEN, NI,NOD,
     .        N, P, I, J, K, L, PP, NIN ,IDEB, IDEB2, IDEB3, II,KK,
     .        INTTH, LOC_PROC, MSGTYP,
     .        MSGOFF1, MSGOFF2,IERROR1,MSGOFF3,
     .        IERROR, IERROR2, REQ_S(NSPMD), IDEBUT(NINTER),IAD(NSPMD),
     .        STATUS(MPI_STATUS_SIZE),  IDEBUT2(NINTER), ISENS,INTERACT,
     .        NODFITOT,NODFI(NINTSTAMP),NIACTIF,INTERACTIF(NINTSTAMP),
     .        LENS(NSPMD),LENR(NSPMD),LENI(NSPMD),NODSI(NINTSTAMP),
     .        IADS(NSPMD),IADR(NSPMD),NODFITOTS,NODFITOTR,IFLAGLOADP
      DATA MSGOFF1/1013/
      DATA MSGOFF2/2014/
      DATA MSGOFF3/1015/
      INTEGER,DIMENSION(:), ALLOCATABLE :: IBUFS, IBUFR
      my_real,DIMENSION(:), ALLOCATABLE :: RBUFS, RBUFR
C     REAL
      my_real
     .   STARTT,STOPT,DIST,TS
C-----------------------------------------------
      IF(NSPMD==1) RETURN
C
      LOC_PROC = ISPMD+1
C
C-------------------------------------------------------------------------------
C Check inactif interfaces and end of tri ( I21buce) 
C------------------------------------------------------------------------------
      NIACTIF = 0 ! Number of actif and Tri interfaces
      NODFITOT = 0  ! Global Number of remote main node
      NODFITOTS = 0 
      NODFITOTR = 0 
      INTERACTIF = 0
      NODFI = 0

      DO KK = 1, NBINTC21 
         NI = INTLIST21(KK)
         NIN = INTSTAMP(NI)%NOINTER
         ISENS = IPARI(64,NIN)  ! IF an interface sensor is defined
         INTERACT = 0
         IF (ISENS > 0) THEN             ! Sensor ID  
           TS = SENSOR_TAB(ISENS)%TSTART
           IF (TT>=TS) INTERACT = 1
         ELSE
           STARTT=INTBUF_TAB(NIN)%VARIABLES(3)
           STOPT =INTBUF_TAB(NIN)%VARIABLES(11)
           IF (STARTT<=TT.AND.TT<=STOPT) INTERACT = 1
         ENDIF          
C
         IFLAGLOADP = 0 !IPARI(95,NIN)
         INTTH = IPARI(47,NIN)

         IF (RETRI(NIN)== 1.AND.INTERACT/=0.AND.(INTTH==2.OR.IFLAGLOADP > 0))THEN  
             NIACTIF = NIACTIF + 1
             INTERACTIF(NIACTIF) = NIN
             NODFI(NIACTIF) = 0
             DO P=1,NSPMD
                NODFI(NIACTIF) = NODFI(NIACTIF) + NMNFI(NIN)%P(P)
             ENDDO
             NODFITOT = NODFITOT + NODFI(NIACTIF)
         ENDIF
      ENDDO
C
C-------------------------------------------------------------------------------
C First COMM : COMM number of main remote node 
C------------------------------------------------------------------------------
      IF(NIACTIF /= 0) THEN
C alloc comm structure 
        ALLOCATE(IBUFS(NIACTIF*NSPMD),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
        ALLOCATE(IBUFR(NIACTIF*NSPMD),STAT=IERROR)
        IF(IERROR/=0) THEN
           CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
           CALL ARRET(2)
         ENDIF
C FILL comm structure 
        IDEB = 1
        DO P=1,NSPMD
          LENS(P) = 0
          IF (P/= LOC_PROC)THEN 
           IAD(P) = IDEB
           DO NI = 1, NIACTIF
              NIN = INTERACTIF(NI)
              IBUFS(IDEB)= NMNFI(NIN)%P(P)
              LENS(P) = LENS(P) + NMNFI(NIN)%P(P)
              IDEB = IDEB +1 
              NODFITOTS = NODFITOTS+NMNFI(NIN)%P(P)
           ENDDO
          
C SEND comm structure 
           MSGTYP = MSGOFF1 
           CALL MPI_ISEND(
     S      IBUFS(IAD(P)),NIACTIF,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(P),IERROR)

          ENDIF 
        ENDDO  
C RECEIVE comm structure 
        DO NI = 1, NIACTIF
           NODSI(NI) = 0
        ENDDO
c
        DO P=1,NSPMD
          LENR(P) = 0
          MSGTYP = MSGOFF1 
          IF (P/= LOC_PROC)THEN 
   
            CALL MPI_RECV(IBUFR(IAD(P)),NIACTIF,MPI_INTEGER,IT_SPMD(P),
     .                 MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)

            IDEB= IAD(P)
            LENR(P) = 0
            DO NI = 1, NIACTIF
              NIN = INTERACTIF(NI)         
              NMNSI(NIN)%P(P)= IBUFR(IDEB)
              LENR(P) = LENR(P) + NMNSI(NIN)%P(P)
              NODSI(NI) = NODSI(NI) + NMNSI(NIN)%P(P)
              IDEB = IDEB + 1
              NODFITOTR = NODFITOTR+NMNSI(NIN)%P(P)
            ENDDO
          ENDIF
        ENDDO
C WAITING for receiving msg
        DO P = 1, NSPMD
        IF (P/= LOC_PROC)THEN
              CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
           ENDIF
        ENDDO
C-------------------------------------------------------------------------------
C SECOND COMM : COMM main remote node 
C------------------------------------------------------------------------------
C alloc comm structure 
        DO NI = 1, NIACTIF
           NIN = INTERACTIF(NI)
           IF(ASSOCIATED( NMVSI(NIN)%P )) DEALLOCATE(NMVSI(NIN)%P)
           ALLOCATE(NMVSI(NIN)%P(NODSI(NI)),STAT=IERROR1)

           IF(ASSOCIATED( TEMPNOD(NIN)%P )) DEALLOCATE(TEMPNOD(NIN)%P)
           ALLOCATE(TEMPNOD(NIN)%P(NODSI(NI)),STAT=IERROR1)

        ENDDO

         IF(ALLOCATED(IBUFS)) DEALLOCATE(IBUFS)

         ALLOCATE(IBUFS(NSPMD*NODFITOTS),STAT=IERROR)

         IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
         ENDIF
         
         IF(ALLOCATED(IBUFR)) DEALLOCATE(IBUFR)
         ALLOCATE(IBUFR(NSPMD*NODFITOTR),STAT=IERROR)

         IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
         ENDIF
C FILL comm structure
          IADS(1) = 1
          IADR(1) = 1           
          DO P=1,NSPMD-1
             IADS(P+1) =IADS(P)+LENS(P)
             IADR(P+1) =IADR(P)+LENR(P)
          ENDDO 
          IDEB = 0
          DO NI = 1, NIACTIF
             IDEBUT2(NI) = 0
          ENDDO

          DO P = 1, NSPMD    
            IF(P/= LOC_PROC.AND.LENS(P)/=0)THEN
              DO NI = 1, NIACTIF
                NIN = INTERACTIF(NI)
                LEN = NMNFI(NIN)%P(P)

                IF(LEN /= 0) THEN 
                  IDEB2 = IDEBUT2(NI)             
                  DO I = 1,LEN
                    IBUFS(IDEB+I)= NMVFI(NIN)%P(IDEB2+I)
                  ENDDO
                  IDEB = IDEB + LEN
                  IDEBUT2(NI) = IDEBUT2(NI) + LEN
                ENDIF
              ENDDO

C SEND comm structure 
             MSGTYP = MSGOFF2 
   
             CALL MPI_ISEND(
     S       IBUFS(IADS(P)),LENS(P),MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G       MPI_COMM_WORLD,REQ_S(P),IERROR)

           ENDIF
          ENDDO

C RECEIVE comm structure 
c          IDEB = 0
          DO NI = 1, NIACTIF
             IDEBUT2(NI) = 0
          ENDDO

          DO P=1,NSPMD
             IF(P/= LOC_PROC.AND.LENR(P)/=0)THEN
               MSGTYP = MSGOFF2 

               CALL MPI_RECV(IBUFR(IADR(P)),LENR(P),MPI_INTEGER,IT_SPMD(P),
     .                 MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
              IDEB = IADR(P)-1
              DO NI = 1, NIACTIF
                NIN = INTERACTIF(NI) 
                LEN = NMNSI(NIN)%P(P)    
                IF(LEN /= 0) THEN 
                   IDEB2 = IDEBUT2(NI)   
  
                   DO I = 1,LEN         
                    NMVSI(NIN)%P(IDEB2+I)= IBUFR(IDEB+I)
                    NOD = INTBUF_TAB(NIN)%MSR_L(NMVSI(NIN)%P(IDEB2+I))
                    TEMPNOD(NIN)%P(IDEB2+I)= NOD
                   ENDDO
                   IDEB = IDEB + LEN
                   IDEB2 = IDEB2 + LEN
                   IDEBUT2(NI) = IDEBUT2(NI) + LEN
                ENDIF
               ENDDO
             ENDIF
          ENDDO
C WAITING for receiving msg
           DO P = 1, NSPMD
             IF(P/= LOC_PROC.AND.LENS(P)/=0)THEN
                CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
             ENDIF
           ENDDO

         IF(ALLOCATED(IBUFS)) DEALLOCATE(IBUFS)
         IF(ALLOCATED(IBUFR)) DEALLOCATE(IBUFR)
C-------------------------------------------------------------------------------
C THIRD COMM : COMM main temperature 
C------------------------------------------------------------------------------
C alloc comm structure 

         IF( FTEMPVAR21==1 ) THEN

          DO NI = 1, NIACTIF
           NIN = INTERACTIF(NI)

           IF(ASSOCIATED( NMTEMP(NIN)%P )) DEALLOCATE(NMTEMP(NIN)%P)
           ALLOCATE(NMTEMP(NIN)%P(NODFI(NI)),STAT=IERROR1)

          ENDDO

          ALLOCATE(RBUFS(NSPMD*NODFITOTR),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF

          ALLOCATE(RBUFR(NSPMD*NODFITOTS),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          IDEB = 0
          DO NI = 1, NIACTIF
             IDEBUT2(NI) = 0
          ENDDO
          DO P = 1, NSPMD    
             IDEB = IADR(P)-1
             LENI(P) = 0
             IF(P/= LOC_PROC.AND.LENR(P)/=0)THEN
               DO NI = 1, NIACTIF
                 NIN = INTERACTIF(NI)
                 LEN = NMNSI(NIN)%P(P)    
                 IF(LEN /= 0) THEN 
                  IDEB2 = IDEBUT2(NI)       
                   DO I = 1,LEN 
                     RBUFS(IDEB+I)= TEMP(TEMPNOD(NIN)%P(IDEB2+I))
                   ENDDO
                   IDEB = IDEB + LEN
                   IDEBUT2(NI) = IDEBUT2(NI) + LEN 
                   LENI(P) = LENI(P) + LEN    
                 ENDIF
              ENDDO  

C SEND comm structure 
             MSGTYP = MSGOFF3
             CALL MPI_ISEND(
     S        RBUFS(IADR(P)),LENR(P),REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S(P),IERROR)
          ENDIF
         ENDDO

C RECEIVE comm structure
           DO NI = 1, NIACTIF
              IDEBUT2(NI) = 0
           ENDDO
      
         DO P=1,NSPMD
           IF(P/= LOC_PROC.AND.LENS(P)/=0)THEN
            MSGTYP = MSGOFF3 
           CALL MPI_RECV(RBUFR(IADS(P)),LENS(P),REAL,IT_SPMD(P),
     .                 MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)

            IDEB = IADS(P)-1
            DO NI = 1, NIACTIF
               NIN = INTERACTIF(NI) 
               LEN = NMNFI(NIN)%P(P)    
               IF(LEN /= 0) THEN 
                 IDEB2 = IDEBUT2(NI) 
                 DO I = 1,LEN                 
                 NMTEMP(NIN)%P(IDEB2+I)= RBUFR(IDEB+I)
                ENDDO
                IDEB = IDEB + LEN
                IDEBUT2(NI) = IDEBUT2(NI) + LEN 
              ENDIF
           ENDDO
          ENDIF
         ENDDO
C WAITING for receiving msg
         DO P = 1, NSPMD
             IF(P/= LOC_PROC.AND.LENR(P)/=0)THEN
              CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
             ENDIF
         ENDDO

         IF(ALLOCATED(RBUFS)) DEALLOCATE(RBUFS)
         IF(ALLOCATED(RBUFR)) DEALLOCATE(RBUFR)

        ENDIF

      ENDIF

C
#endif
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_I21TEMPCOM               source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SPMD_I21TEMPCOM(IPARI,TEMP,INTBUF_TAB,NSENSOR,SENSOR_TAB) 
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTSTAMP_MOD
      USE INTSTAMP_GLOB_MOD
      USE INTBUFDEF_MOD 
      USE SENSOR_MOD
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "intstamp_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ,INTENT(IN) :: NSENSOR
      INTEGER  IPARI(NPARI,NINTER)
C     REAL
      my_real :: TEMP(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER LEN, NI, NIN ,IDEB, IDEB2, P,I,
     .        INTTH, LOC_PROC, MSGTYP,LENI,
     .        MSGOFF,IERROR, IERROR2, REQ_S(NSPMD), IDEBUT2(NINTER),
     .        STATUS(MPI_STATUS_SIZE),  ISENS,INTERACT,
     .        NODSITOT,NODSI(NINTSTAMP),NIACTIF,INTERACTIF(NINTSTAMP),
     .        LENS(NSPMD),LENR(NSPMD),IADS(NSPMD),IADR(NSPMD),NODFITOT,
     .        NODFI(NINTSTAMP)
      DATA MSGOFF/2016/

      my_real,DIMENSION(:), ALLOCATABLE :: RBUFS, RBUFR
C     REAL
      my_real
     .   STARTT,STOPT,DIST,TS
C------------------------------------------------------------------------
      IF(NSPMD==1) RETURN
C
      LOC_PROC = ISPMD+1
C
      NIACTIF = 0 ! Number of actif and Tri interfaces
      NODSITOT = 0  ! Global Number of remote main node
      NODFITOT = 0  ! Global Number of remote main node
C 
      LENS = 0   
      LENR = 0               
      DO NI = 1, NINTSTAMP
         NIN = INTSTAMP(NI)%NOINTER
         ISENS = IPARI(64,NIN)  ! IF an interface sensor is defined
         INTERACT = 0
         IF (ISENS > 0) THEN             ! Sensor ID  
           TS = SENSOR_TAB(ISENS)%TSTART
           IF (TT>=TS) INTERACT = 1
         ELSE
           STARTT=INTBUF_TAB(NIN)%VARIABLES(3)
           STOPT =INTBUF_TAB(NIN)%VARIABLES(11)
           IF (STARTT<=TT.AND.TT<=STOPT) INTERACT = 1
         ENDIF
C
         INTTH = IPARI(47,NIN)
C
         IF (INTERACT/=0.AND.INTTH==2)THEN  
             NIACTIF = NIACTIF + 1
             INTERACTIF(NIACTIF) = NIN
             NODSI(NIACTIF) = 0
             NODFI(NIACTIF) = 0
             DO P=1,NSPMD
                NODSI(NIACTIF) = NODSI(NIACTIF) + NMNSI(NIN)%P(P)
                NODFI(NIACTIF) = NODFI(NIACTIF) + NMNFI(NIN)%P(P)
                LENS(P) = LENS(P) + NMNSI(NIN)%P(P)
                LENR(P)= LENR(P) + NMNFI(NIN)%P(P)
             ENDDO
            NODSITOT = NODSITOT + NODSI(NIACTIF)
            NODFITOT = NODFITOT + NODFI(NIACTIF)
         ENDIF
       ENDDO

       IF(NIACTIF /= 0 ) THEN 
C alloc comm structure      
         ALLOCATE(RBUFS(NSPMD*NODSITOT),STAT=IERROR)
         IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
         ENDIF

         ALLOCATE(RBUFR(NSPMD*NODFITOT),STAT=IERROR)
         IF(IERROR/=0) THEN
           CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
           CALL ARRET(2)
         ENDIF
C FILL comm structure 
         IDEB = 0
         DO NI = 1, NIACTIF
            IDEBUT2(NI) = 0
         ENDDO
        IF(NODSITOT/= 0) THEN
         DO P = 1, NSPMD    
           IADS(P) = IDEB +1         
           IF(P/= LOC_PROC.AND.LENS(P)/= 0)THEN
             DO NI = 1, NIACTIF
                NIN = INTERACTIF(NI)
                LEN = NMNSI(NIN)%P(P) 
                LENI = NMNFI(NIN)%P(P)   
                IF(LEN /= 0) THEN 
                  IDEB2 = IDEBUT2(NI)       
                  DO I = 1,LEN 
                     RBUFS(IDEB+I)= TEMP(TEMPNOD(NIN)%P(IDEB2+I))
                  ENDDO
                  IDEB = IDEB + LEN
                  IDEBUT2(NI) = IDEBUT2(NI) + LEN 
                ENDIF
             ENDDO  
C SEND comm structure 
           
             MSGTYP = MSGOFF
             CALL MPI_ISEND(
     S        RBUFS(IADS(P)),LENS(P),REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S(P),IERROR)
            ENDIF
         ENDDO
        ENDIF
       
          IADR(1) = 1           
          DO P=1,NSPMD-1
             IADR(P+1) =IADR(P)+LENR(P)
          ENDDO 
C RECEIVE comm structure 
           DO NI = 1, NIACTIF
              IDEBUT2(NI) = 0
           ENDDO
        IF(NODFITOT /=0) THEN
         DO P=1,NSPMD
            IF(P/= LOC_PROC.AND.LENR(P)/= 0)THEN
              MSGTYP = MSGOFF 
           CALL MPI_RECV(RBUFR(IADR(P)),LENR(P),REAL,IT_SPMD(P),
     .                 MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
           IDEB= IADR(P)-1

            DO NI = 1, NIACTIF
               NIN = INTERACTIF(NI) 
               LEN = NMNFI(NIN)%P(P)    
               IF(LEN /= 0) THEN 
                 IDEB2 = IDEBUT2(NI) 
                 DO I = 1,LEN                 
                 NMTEMP(NIN)%P(IDEB2+I)= RBUFR(IDEB+I)
                ENDDO
                IDEBUT2(NI) = IDEBUT2(NI) + LEN 
              ENDIF
           ENDDO
           ENDIF
         ENDDO
        ENDIF
C WAITING for receiving msg
         DO P = 1, NSPMD
             IF(P/= LOC_PROC.AND.LENS(P)/= 0)THEN
              CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
             ENDIF
         ENDDO
         IF(ALLOCATED(RBUFS)) DEALLOCATE(RBUFS)
         IF(ALLOCATED(RBUFR)) DEALLOCATE(RBUFR)
       ENDIF
C
#endif
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_I21FTHECOM               source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        INTCONTP                      source/mpi/interfaces/spmd_i7tool.F
Chd|        SORTINT                       source/mpi/interfaces/spmd_i7tool.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_I21FTHECOM(IPARI    ,FTHE  ,INTBUF_TAB,SENSOR_TAB,NISKYFI ,
     .                           FTHESKYI ,ISKY  ,FSKYI     ,CONDNSKYI ,NSENSOR ) 
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTSTAMP_MOD
      USE INTSTAMP_GLOB_MOD
      USE INTBUFDEF_MOD 
      USE TRI7BOX
      USE SENSOR_MOD
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "intstamp_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ,INTENT(IN) :: NSENSOR
      INTEGER  IPARI(NPARI,NINTER), ISKY(*), NISKYFI(*)
C     REAL
      my_real :: FTHE(*),FTHESKYI(*),FSKYI(LSKYI,*),CONDNSKYI(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR),INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER LEN, NI, NIN ,IDEB, IDEB2, P,I,
     .        INTTH, LOC_PROC, MSGTYP,LENI,
     .        MSGOFF,IERROR, IERROR2, REQ_S(NSPMD), IDEBUT2(NINTER),IDEBUT(NINTER),
     .        STATUS(MPI_STATUS_SIZE),  ISENS,INTERACT,IFORM,NOD,N,
     .        NODSITOT,NODSI(NINTSTAMP),NIACTIF,INTERACTIF(NINTSTAMP),
     .        LENS(NSPMD),LENR(NSPMD),IADS(NSPMD),IADR(NSPMD),NODFITOT,
     .        NODFI(NINTSTAMP),L,ISIZRCV(2,NSPMD),ISIZENV(2,NSPMD),
     .        REQ_SI(NSPMD),siztemp(NSPMD),
     .        REQ_R(NSPMD),SIZ, J, K, IALLOCS, IALLOCR, MSGOFF2, NIF, NB,LENR2(NSPMD)
      DATA MSGOFF/2016/
      DATA MSGOFF2/2017/
      LOGICAL ITEST

      my_real,DIMENSION(:), ALLOCATABLE :: RBUFS, RBUFR
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX
      my_real, DIMENSION(:), ALLOCATABLE :: TEMPO
C     REAL
      my_real
     .   STARTT,STOPT,DIST,TS
C------------------------------------------------------------------------
      IF(NSPMD==1) RETURN
C
      LOC_PROC = ISPMD+1
C
      NIACTIF = 0 ! Number of actif and Tri interfaces
      NODSITOT = 0  ! Global Number of remote main node
      NODFITOT = 0  ! Global Number of remote main node
C 
      LENS(1:NSPMD) = 0   
      LENR(1:NSPMD) = 0  
      LENR2(1:NSPMD) = 0  
      IF(IPARIT==0) THEN
       DO NI = 1, NINTSTAMP
         NIN = INTSTAMP(NI)%NOINTER
         ISENS = IPARI(64,NIN)  ! IF an interface sensor is defined
         INTERACT = 0

         IF (ISENS > 0) THEN             ! Sensor ID  
           TS = SENSOR_TAB(ISENS)%TSTART
           IF (TT>=TS) INTERACT = 1
         ELSE
           STARTT=INTBUF_TAB(NIN)%VARIABLES(3)
           STOPT =INTBUF_TAB(NIN)%VARIABLES(11)
           IF (STARTT<=TT.AND.TT<=STOPT) INTERACT = 1
         ENDIF
C
         INTTH = IPARI(47,NIN)
         IFORM = IPARI(48,NIN)
C
         IF (INTERACT/=0.AND.INTTH==2.AND.IFORM/=0)THEN  
             NIACTIF = NIACTIF + 1
             INTERACTIF(NIACTIF) = NIN
             NODSI(NIACTIF) = 0
             NODFI(NIACTIF) = 0
             DO P=1,NSPMD
                NODSI(NIACTIF) = NODSI(NIACTIF) + NMNSI(NIN)%P(P)
                NODFI(NIACTIF) = NODFI(NIACTIF) + NMNFI(NIN)%P(P)
                LENS(P) = LENS(P) + NMNSI(NIN)%P(P)
                LENR(P)= LENR(P) + NMNFI(NIN)%P(P)
             ENDDO
            NODSITOT = NODSITOT + NODSI(NIACTIF)
            NODFITOT = NODFITOT + NODFI(NIACTIF)
         ENDIF
        ENDDO
        IF(NIACTIF /= 0 ) THEN 
C alloc comm structure      
         ALLOCATE(RBUFS(2*NSPMD*NODFITOT),STAT=IERROR)
         IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
         ENDIF

         ALLOCATE(RBUFR(2*NSPMD*NODSITOT),STAT=IERROR)
         IF(IERROR/=0) THEN
           CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
           CALL ARRET(2)
         ENDIF
C FILL comm structure 
         IDEB = 0
         DO NI = 1, NIACTIF
            IDEBUT2(NI) = 0
         ENDDO
         IF(NODFITOT/= 0) THEN
          DO P = 1, NSPMD    
           IADS(P) = IDEB +1         
           IF(P/= LOC_PROC.AND.LENR(P)/= 0)THEN
             DO NI = 1, NIACTIF
                NIN = INTERACTIF(NI)
                LEN = NMNFI(NIN)%P(P) 
                IF(LEN /= 0) THEN 
                  IDEB2 = IDEBUT2(NI)       
                  DO I = 1,LEN 
                     RBUFS(IDEB+1)= NMVFI(NIN)%P(IDEB2+I)
                     RBUFS(IDEB+2)= FTHEFI(NIN)%P(IDEB2+I)
                     FTHEFI(NIN)%P(IDEB2+I)= ZERO
                     IDEB = IDEB + 2
                  ENDDO
                  IDEBUT2(NI) = IDEBUT2(NI) + LEN 
                ENDIF
             ENDDO  
C SEND comm structure 
           
             MSGTYP = MSGOFF
             CALL MPI_ISEND(
     S        RBUFS(IADS(P)),2*LENR(P),REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S(P),IERROR)
            ENDIF
          ENDDO
         ENDIF
       
          IADR(1) = 1           
          DO P=1,NSPMD-1
             IADR(P+1) =IADR(P)+2*LENS(P)
          ENDDO 
C RECEIVE comm structure 
           DO NI = 1, NIACTIF
              IDEBUT2(NI) = 0
           ENDDO
         IF(NODSITOT /=0) THEN
          DO P=1,NSPMD
            IF(P/= LOC_PROC.AND.LENS(P)/= 0)THEN
              MSGTYP = MSGOFF 
           CALL MPI_RECV(RBUFR(IADR(P)),2*LENS(P),REAL,IT_SPMD(P),
     .                 MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
           IDEB= IADR(P)-1

            DO NI = 1, NIACTIF
               NIN = INTERACTIF(NI) 
               LEN = NMNSI(NIN)%P(P)    
               IF(LEN /= 0) THEN 
                 IDEB2 = IDEBUT2(NI) 
                 DO I = 1,LEN
                   N = NINT(RBUFR(IDEB+1))
                   NOD = INTBUF_TAB(NIN)%MSR_L(N)
                   FTHE(NOD) = FTHE(NOD) + RBUFR(IDEB+2)
                   IDEB = IDEB + 2	   
                ENDDO
                IDEBUT2(NI) = IDEBUT2(NI) + LEN 
              ENDIF
           ENDDO
           ENDIF
          ENDDO
         ENDIF
C WAITING for receiving msg
         DO P = 1, NSPMD
             IF(P/= LOC_PROC.AND.LENR(P)/= 0)THEN
              CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
             ENDIF
          ENDDO
          IF(ALLOCATED(RBUFS)) DEALLOCATE(RBUFS)
          IF(ALLOCATED(RBUFR)) DEALLOCATE(RBUFR)
        ENDIF
C
      ELSE    ! PARTITH   
C  
       DO NI = 1, NINTSTAMP
         NIN = INTSTAMP(NI)%NOINTER
         ISENS = IPARI(64,NIN)  ! IF an interface sensor is defined

         IF (ISENS> 0) THEN             ! Sensor ID  
           TS = SENSOR_TAB(ISENS)%TSTART
           IF (TT>=TS) INTERACT = 1
         ELSE
           STARTT=INTBUF_TAB(NIN)%VARIABLES(3)
           STOPT =INTBUF_TAB(NIN)%VARIABLES(11)
           IF (STARTT<=TT.AND.TT<=STOPT) INTERACT = 1
         ENDIF
C
         INTTH = IPARI(47,NIN)
         IFORM = IPARI(48,NIN)
C
         IF (INTERACT/=0.AND.INTTH==2.AND.IFORM/=0)THEN  
             NIACTIF = NIACTIF + 1
             INTERACTIF(NIACTIF) = NIN
             NODSI(NIACTIF) = 0
             NODFI(NIACTIF) = 0
             DO P=1,NSPMD
                NODSI(NIACTIF) = NODSI(NIACTIF) + NMNSI(NIN)%P(P)
                NODFI(NIACTIF) = NODFI(NIACTIF) + NMNFI(NIN)%P(P)
                LENS(P) = LENS(P) + NMNSI(NIN)%P(P)
                LENR(P)= LENR(P) + NMNFI(NIN)%P(P)
             ENDDO
            NODSITOT = NODSITOT + NODSI(NIACTIF)
            NODFITOT = NODFITOT + NODFI(NIACTIF)
         ENDIF
        ENDDO

C
C Init + ireceive sur taille communication
C
        SIZ = 0
        DO P = 1, NSPMD
          ISIZRCV(1,P)=0
          ISIZRCV(2,P)=0
          ISIZENV(1,P) = 0
          ISIZENV(2,P) = 0
          IF(P/=LOC_PROC)THEN
            SIZ = LENS(P)
            IF(SIZ>0)THEN
              MSGTYP = MSGOFF 
              CALL MPI_IRECV(
     .          ISIZRCV(1,P),2,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_R(P),IERROR   )
            ENDIF
          ENDIF
        ENDDO

C
C Partie 1 envoi et preparation buffer reception
C
        DO NI = 1, NIACTIF
          NIN = INTERACTIF(NI)
          NIF = NISKYFI(NIN)
          IF(NIF>0) THEN
            INTTH = IPARI(47,NIN)
     
              ALLOCATE(INDEX(NIF),STAT=IERROR)
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              END IF

              ALLOCATE( TEMPO(NIF),STAT=IERROR)
                           
              IF(IERROR/=0) THEN
                CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
                CALL ARRET(2)
              END IF
	                    
              DO J=1,NIF
                 INDEX(J)=J
                 TEMPO(J)=FTHESKYFI(NIN)%P(J)
              ENDDO

              CALL SORTINT(NIF,ISKYFI(NIN)%P(1),INDEX)
              
              DO J=1,NIF
                 K=INDEX(J)
                 FTHESKYFI(NIN)%P(J)=TEMPO(K)
              ENDDO
C precomptage du nombre de contacts par processeur+calcul nsnfi total
              CALL INTCONTP(
     +      NIF,ISKYFI(NIN)%P(1),NMNFI(NIN)%P(1),ISIZENV,LENR2,2)

             IF (NIF > 0 ) THEN
               DEALLOCATE(TEMPO,INDEX)
	     ENDIF
          ENDIF

        ENDDO
C
C alloc comm structure      
        IALLOCS = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC.AND.LENR(P)>0) THEN
            MSGTYP = MSGOFF
            CALL MPI_ISEND(
     .        ISIZENV(1,P),2,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .        MPI_COMM_WORLD,REQ_S(P),IERROR    )
            IALLOCS = IALLOCS + ISIZENV(1,P)
          ENDIF
        END DO
        IERROR=0
        IF(IALLOCS>0)
     +    ALLOCATE(RBUFS(IALLOCS+NIACTIF*NSPMD*2),STAT=IERROR) ! nbintc*NIACTIF*2 majorant place supplementaire bufs
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        END IF
C
C Send
C

        IF(NIACTIF /= 0 ) THEN 
C FILL comm structure 
         IDEB = 0
         DO NI = 1, NIACTIF
            IDEBUT(NI) = 0
            IDEBUT2(NI) = 1
         ENDDO
         L=0
         SIZ = 0 
         DO P = 1, NSPMD    
           IADS(P) = L +1         
           IF(P/= LOC_PROC.AND.ISIZENV(1,P)/= 0)THEN
             DO NI = 1, NIACTIF
                NIN = INTERACTIF(NI)
                LEN = NMNFI(NIN)%P(P) 
                IF(LEN /= 0) THEN 
                  IDEB2 = IDEBUT2(NI)  
                  IDEB  = IDEBUT(NI)  
                  L = L + 1     
                  DO I = 1,LEN 
C noeud generant une force
                      NOD = NMVFI(NIN)%P(IDEB+I)
                      IF(IDEB2<=NISKYFI(NIN)) THEN
                        ITEST = ISKYFI(NIN)%P(IDEB2)==IDEB+I
                      ELSE
                        ITEST = .FALSE.
                      ENDIF
                      DO WHILE(ITEST)
                         RBUFS(L+1)= NOD
                         RBUFS(L+2)= FTHESKYFI(NIN)%P(IDEB2)
                         L = L + 2
                         IDEB2= IDEB2 +1
                        IF(IDEB2<=NISKYFI(NIN)) THEN
                          ITEST = ISKYFI(NIN)%P(IDEB2)==IDEB+I
                        ELSE
                          ITEST = .FALSE.
                        ENDIF
                      ENDDO
                  ENDDO
                  RBUFS(IADS(P)) = (L-IADS(P))/2
                  IDEBUT2(NI) = IDEB2
                ENDIF
             ENDDO  
C SEND comm structure 
              SIZ = L+1-IADS(P)  
             siztemp(P) = SIZ         
             MSGTYP = MSGOFF2
             CALL MPI_ISEND(
     S        RBUFS(IADS(P)),SIZ,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,REQ_SI(P),IERROR)
            ENDIF
            DO NI = 1, NIACTIF
                NIN = INTERACTIF(NI)
                LEN = NMNFI(NIN)%P(P) 
               IDEBUT(NI) = IDEBUT(NI) + LEN
            ENDDO
          ENDDO
        DO NI = 1, NIACTIF
          NIN = INTERACTIF(NI)
          NISKYFI(NIN)  = 0
        ENDDO
       
C
C Receive 1er message : taille communication
C
        IALLOCR = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC.AND.LENS(P)>0)THEN
            CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
            IALLOCR = MAX(IALLOCR,ISIZRCV(1,P))   ! pour comm bloquantes
c           IALLOCR = IALLOCR + ISIZRCV(P)     ! pour comm non bloquantes
          END IF
        END DO
C
        IERROR=0
        IF(IALLOCR>0)
     .    ALLOCATE(RBUFR(IALLOCR+NIACTIF*2),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
C

          DO P=1,NSPMD
            IF(P/= LOC_PROC.AND.ISIZRCV(1,P)/= 0)THEN
              MSGTYP = MSGOFF2 
            L = 1           ! envoi bloquant + opti alloc memoire sur max des comm
            CALL MPI_RECV(
     .        RBUFR(L),ISIZRCV(1,P)+NIACTIF,REAL  ,IT_SPMD(P),MSGTYP,
     .        MPI_COMM_WORLD            ,STATUS,IERROR    )

            DO NI = 1, NIACTIF
               NIN = INTERACTIF(NI) 
               LEN = NMNSI(NIN)%P(P)    
               IF(LEN /= 0) THEN               
                 NB = NINT(RBUFR(L))
                 L = L + 1
                 IF(NB /= 0) THEN 
                   IDEB = 1
                   DO I = 1,NB
                      N = NINT(RBUFR(IDEB+1))
                      NOD = INTBUF_TAB(NIN)%MSR_L(N)
                      NISKY = NISKY + 1
                      FSKYI(NISKY,1)=ZERO
                      FSKYI(NISKY,2)=ZERO
                      FSKYI(NISKY,3)=ZERO
                      FSKYI(NISKY,4)=ZERO
                      IF(NODADT_THERM == 1 )  CONDNSKYI(NISKY)=ZERO
                      FTHESKYI(NISKY)=RBUFR(IDEB+2)	
                      ISKY(NISKY) = NOD  
                      IDEB = IDEB + 2
                   ENDDO
                   L = L + 2*NB
                ENDIF
              ENDIF
           ENDDO
           ENDIF
          ENDDO

C WAITING for receiving msg
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            IF(LENR(P)>0) THEN
              CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
            END IF
            IF(ISIZENV(1,P)>0)THEN
              CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
            END IF
          END IF
        END DO
         IF(ALLOCATED(RBUFS)) DEALLOCATE(RBUFS)
         IF(ALLOCATED(RBUFR)) DEALLOCATE(RBUFR)
        ENDIF
C
       ENDIF
#endif
      RETURN
      END

C
Chd|====================================================================
Chd|  SPMD_GET_PENIS                source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        I11BUCE_CRIT                  source/interfaces/intsort/i11buce_crit.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_GET_PENIS(NRTS,PENIS,NIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTS, NIN
      my_real
     .        PENIS(2,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, N, P, NOD, NB, IDEB, MSGTYP, LOC_PROC, IERROR,
     .        IERROR1, LENS, LENR, LENSR, MSGOFF, MSGOFF2
      INTEGER STATUS(MPI_STATUS_SIZE),REQ_SI(NSPMD)
      my_real       ,DIMENSION(:), ALLOCATABLE :: BUFS, BUFR
      DATA MSGOFF/2017/
      DATA MSGOFF2/2018/
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      IF(NSPMD==1) RETURN
        LENS = 0
        LENR = 0
        DO P = 1, NSPMD
          LENS = LENS + NSNFI(NIN)%P(P)
          LENR = LENR + NSNSI(NIN)%P(P)
        END DO
        LENSR = MAX(LENS,LENR)
        IF(LENSR>0)THEN
          IERROR=0
          ALLOCATE(BUFS(LENSR),STAT=IERROR)
          IERROR1=0
          ALLOCATE(BUFR(LENSR),STAT=IERROR1)
          IF(IERROR+IERROR1/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          END IF
        END IF
        LOC_PROC = ISPMD+1
C
C Envoi PENIS(2)
C
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)
            DO N = 1, NB
              BUFS(IDEB+N) = PENFI(NIN)%P(2,IDEB+N)
            END DO
            IF(NB>0)THEN
              MSGTYP = MSGOFF
              CALL MPI_ISEND(
     .          BUFS(IDEB+1),NB,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_SI(P),IERROR    )
              IDEB = IDEB + NB
            END IF      
          END IF
        END DO
C
C Recep PENIS(2) remote et maj PENIS
C
        IDEB = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNSI(NIN)%P(P)
            IF(NB>0)THEN
              MSGTYP = MSGOFF 
              CALL MPI_RECV(
     .          BUFR,NB,REAL  ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,STATUS,IERROR    )
              DO N = 1, NB
                NOD = NSVSI(NIN)%P(IDEB+N)
                PENIS(2,NOD) = MAX(PENIS(2,NOD),BUFR(N))
              END DO
              IDEB = IDEB + NB              
            END IF
          END IF      
        END DO
C
C Attente reception 1er envoi
C
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)
            IF(NB>0)THEN
              CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
            ENDIF      
          END IF
        END DO

C
C Renvoi PENIS(2) update
C
        IDEB =0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNSI(NIN)%P(P)
            DO N = 1, NB
              NOD = NSVSI(NIN)%P(IDEB+N)
              BUFS(IDEB+N) = PENIS(2,NOD)
            END DO
            IF(NB>0)THEN
              MSGTYP = MSGOFF2
              CALL MPI_ISEND(
     .          BUFS(IDEB+1),NB,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_SI(P),IERROR    )
              IDEB = IDEB + NB
            END IF      
          END IF
        END DO
C
C Reception PENIS(2) update dans PENFI et maj PENFI (cf i11buce_crit)
C
        IDEB =0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)
            IF(NB>0)THEN
              MSGTYP = MSGOFF2
              CALL MPI_RECV(
     .          BUFR,NB,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,STATUS,IERROR    )
              DO N = 1, NB
                PENFI(NIN)%P(1,IDEB+N) = MIN(PENFI(NIN)%P(1,IDEB+N),
     .                                       BUFR(N))
                PENFI(NIN)%P(2,IDEB+N) = ZERO
              END DO
              IDEB = IDEB + NB
            END IF
          END IF      
        END DO
C
C Attente reception 2eme envoi
C
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNSI(NIN)%P(P)
            IF(NB>0)THEN
              CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
            ENDIF      
          END IF
        END DO
C
        IF(LENSR>0)THEN
          DEALLOCATE(BUFR)
          DEALLOCATE(BUFS)
        END IF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_GET_PENIS20              source/mpi/interfaces/send_cand.F
Chd|-- called by -----------
Chd|        I20BUCE_CRIT                  source/interfaces/intsort/i20buce_crit.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_GET_PENIS20(NSV,IXLINS,PENIS,PENISE,PENIA,NIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NSV(*), IXLINS(2,*)
      my_real
     .        PENIS(2,*), PENISE(2,*), PENIA(5,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, N, P, NOD, NB, IDEB, MSGTYP, LOC_PROC, IERROR,
     .        IERROR1, LENS, LENR, LENSR, NB1, NB2, IDEB1, IDEB2,
     .        II, IL, IL1, IL2, MSGOFF, MSGOFF2
      INTEGER STATUS(MPI_STATUS_SIZE),REQ_SI(NSPMD)
      my_real
     .       ,DIMENSION(:), ALLOCATABLE :: BUFS, BUFR
      DATA MSGOFF/2019/
      DATA MSGOFF2/2020/
C-----------------------------------------------
C   F u n c t i o n s
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      IF(NSPMD==1) RETURN
        LENS = 0
        LENR = 0
        DO P = 1, NSPMD
          LENS = LENS + 2*NSNFI(NIN)%P(P)+ 3*NSNFIE(NIN)%P(P)
          LENR = LENR + 2*NSNSI(NIN)%P(P)+ 3*NSNSIE(NIN)%P(P)
        END DO
        LENSR = MAX(LENS,LENR)
        IF(LENSR>0)THEN
          IERROR=0
          ALLOCATE(BUFS(LENSR),STAT=IERROR)
          IERROR1=0
          ALLOCATE(BUFR(LENSR),STAT=IERROR1)
          IF(IERROR+IERROR1/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          END IF
        END IF
        LOC_PROC = ISPMD+1
C
C Envoi PENIS(2)
C
        IDEB = 0
        IDEB1= 0
        IDEB2= 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB1 = NSNFI(NIN)%P(P)
            DO N = 1, NB1
              BUFS(IDEB+N)    = PENFI(NIN)%P(2,IDEB1+N)
              BUFS(IDEB+NB1+N)= PENFIA(NIN)%P(5,IDEB1+N)
            END DO
            IDEB1 = IDEB1 + NB1
            NB2 = NSNFIE(NIN)%P(P)
            DO N = 1, NB2
              BUFS(IDEB+2*NB1+N)       = PENFIE(NIN)%P(2,IDEB2+N)
              BUFS(IDEB+2*NB1+NB2+2*(N-1)+1)= 
     +                             PENFIAE(NIN)%P(5,2*IDEB2+2*(N-1)+1)
              BUFS(IDEB+2*NB1+NB2+2*N) = PENFIAE(NIN)%P(5,2*IDEB2+2*N)
            END DO
            IDEB2 = IDEB2 + NB2
            NB=2*NB1+3*NB2
            IF(NB>0)THEN
              MSGTYP = MSGOFF 
              CALL MPI_ISEND(
     .          BUFS(IDEB+1),NB,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_SI(P),IERROR    )
              IDEB = IDEB + NB
            END IF      
          END IF
        END DO
C
C Recep PENIS(2) remote et maj PENIS
C
        IDEB1 = 0
        IDEB2 = 0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB1 = NSNSI(NIN)%P(P)
            NB2 = NSNSIE(NIN)%P(P)
            NB=2*NB1+3*NB2
            IF(NB>0)THEN
              MSGTYP = MSGOFF
              CALL MPI_RECV(
     .          BUFR,NB,REAL  ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,STATUS,IERROR    )
              DO N = 1, NB1
                II = NSVSI(NIN)%P(IDEB1+N)
                IL = NSV(II)                      
                PENIS(2,II) = MAX(PENIS(2,II),BUFR(N))
                PENIA(5,IL) = MAX(PENIA(5,IL),BUFR(NB1+N))
              END DO
              IDEB1 = IDEB1+NB1
              DO N = 1, NB2
                II = NSVSIE(NIN)%P(IDEB2+N)
                IL1=IXLINS(1,II)
                IL2=IXLINS(2,II)
                PENISE(2,II)=MAX(PENISE(2,II),BUFR(2*NB1+N))
                PENIA(5,IL1)=MAX(PENIA(5,IL1),BUFR(2*NB1+NB2+2*(N-1)+1))
                PENIA(5,IL2)=MAX(PENIA(5,IL2),BUFR(2*NB1+NB2+2*N))
              END DO
              IDEB2 = IDEB2 + NB2              
            END IF
          END IF      
        END DO
C
C Attente reception 1er envoi
C
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNFI(NIN)%P(P)+NSNFIE(NIN)%P(P)
            IF(NB>0)THEN
              CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
            ENDIF      
          END IF
        END DO

C
C Renvoi PENIS(2) update
C
        IDEB  =0
        IDEB1 =0
        IDEB2 =0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB1 = NSNSI(NIN)%P(P)
            DO N = 1, NB1
              II = NSVSI(NIN)%P(IDEB1+N)
              IL = NSV(II)                      
              BUFS(IDEB+N) = PENIS(2,II)
              BUFS(IDEB+NB1+N) = PENIA(5,IL)
            END DO
            IDEB1 = IDEB1 + NB1
            NB2 = NSNSIE(NIN)%P(P)
            DO N = 1, NB2
              II = NSVSIE(NIN)%P(IDEB2+N)
              IL1=IXLINS(1,II)
              IL2=IXLINS(2,II)
              BUFS(IDEB+2*NB1+N) = PENISE(2,II)
              BUFS(IDEB+2*NB1+NB2+2*(N-1)+1) = PENIA(5,IL1)
              BUFS(IDEB+2*NB1+NB2+2*N) = PENIA(5,IL2)
            END DO
            IDEB2 = IDEB2 + NB2
            NB=2*NB1+3*NB2            
            IF(NB>0)THEN
              MSGTYP = MSGOFF2
              CALL MPI_ISEND(
     .          BUFS(IDEB+1),NB,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_SI(P),IERROR    )
              IDEB = IDEB + NB
            END IF      
          END IF
        END DO
C
C Reception PENIS(2) update dans PENFI et maj PENFI (cf i11buce_crit)
C
        IDEB1 =0
        IDEB2 =0
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB1 = NSNFI(NIN)%P(P)
            NB2 = NSNFIE(NIN)%P(P)
            NB=2*NB1+3*NB2            
            IF(NB>0)THEN
              MSGTYP = MSGOFF2 
              CALL MPI_RECV(
     .          BUFR,NB,REAL     ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,STATUS,IERROR    )
              DO N = 1, NB1
                PENFI(NIN)%P(1,IDEB1+N) = MIN(PENFI(NIN)%P(1,IDEB1+N),
     .                                        BUFR(N))
                PENFI(NIN)%P(2,IDEB1+N) = ZERO
                PENFIA(NIN)%P(4,IDEB1+N)= MIN(PENFIA(NIN)%P(4,IDEB1+N),
     .                                        BUFR(NB1+N))
                PENFIA(NIN)%P(5,IDEB1+N) = ZERO
              END DO
              IDEB1 = IDEB1 + NB1
              DO N = 1, NB2
                PENFIE(NIN)%P(1,IDEB2+N) = MIN(PENFIE(NIN)%P(1,IDEB2+N),
     .                                         BUFR(2*NB1+N))
                PENFIE(NIN)%P(2,IDEB2+N) = ZERO
                PENFIAE(NIN)%P(4,2*IDEB2+2*(N-1)+1) = 
     .            MIN(PENFIAE(NIN)%P(4,2*IDEB2+2*(N-1)+1),
     .                BUFR(2*NB1+NB2+2*(N-1)+1))
                PENFIAE(NIN)%P(5,2*IDEB2+2*(N-1)+1) = ZERO
                PENFIAE(NIN)%P(4,2*IDEB2+2*N) = 
     .            MIN(PENFIAE(NIN)%P(4,2*IDEB2+2*N),
     .                BUFR(2*NB1+NB2+2*N))
                PENFIAE(NIN)%P(5,2*IDEB2+2*N) = ZERO
              END DO
              IDEB2 = IDEB2 + NB2
            END IF
          END IF      
        END DO
C
C Attente reception 2eme envoi
C
        DO P = 1, NSPMD
          IF(P/=LOC_PROC)THEN
            NB = NSNSI(NIN)%P(P)+NSNSIE(NIN)%P(P)
            IF(NB>0)THEN
              CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
            ENDIF      
          END IF
        END DO
C
        IF(LENSR>0)THEN
          DEALLOCATE(BUFR)
          DEALLOCATE(BUFS)
        END IF
C
#endif
      RETURN
      END

