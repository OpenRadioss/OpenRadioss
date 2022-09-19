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
Chd|  FR_RLINK1                     source/mpi/kinematic_conditions/fr_rlink1.F
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SYSFUS2                       source/system/sysfus.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FR_RLINK1(NOD,ITABM1,FR_RL,NSN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NOD(*),ITABM1(*), FR_RL(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NSN_L, P, PMAIN, IMAX,
     .        NODTMP(NSN),NODU(NSN)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER SYSFUS2
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C Recherche du no de noeud interne
C
      DO I=1,NSN
        NODU(I) = NOD(I)
C SYSFUS2 retourne 0 si noeud non trouve
        NODTMP(I) = SYSFUS2(NOD(I),ITABM1,NUMNOD)
      END DO
C Denombrement nb de noeuds locaux (ie nb de noeuds N<>0
      NSN_L=0
      DO I=1,NSN
        IF(NODTMP(I)/=0) THEN
          NSN_L = NSN_L+1
          NOD(NSN_L) = NODTMP(I)
        END IF
      END DO
      FR_RL(ISPMD+1)=NSN_L
C sauvegarde nb de noeuds totaux
      FR_RL(NSPMD+1)=NSN
C Verification des ID user avec comm globale
      IF(NSPMD > 1) CALL SPMD_GLOB_ISUM9(NODTMP,NSN)
      IF(ISPMD==0) THEN
       DO I = 1, NSN
        IF(NODTMP(I)==0) THEN
          CALL ANCMSG(MSGID=186,ANMODE=ANINFO_BLIND,
     .                I1=NODU(I),C1='RIGID LINK')
          IERR=IERR+1
        END IF
       END DO
      ENDIF
C affectation du nb de noeud local
      NSN = NSN_L
C echange valeur fr_rl
      IF(NSPMD > 1) THEN
        DO P = 1, NSPMD
          CALL SPMD_IBCAST(FR_RL(P),FR_RL(P),1,1,IT_SPMD(P),0)
        ENDDO
      END IF
C determination du pmain
      IMAX = 0
      PMAIN = 1
      DO P = 1, NSPMD
        IF(FR_RL(P)>IMAX)THEN
          PMAIN = P
          IMAX = FR_RL(P)
        END IF
      END DO
      FR_RL(NSPMD+2) = PMAIN
C        
      RETURN
      END
C
Chd|====================================================================
Chd|  FR_RLALE                      source/mpi/kinematic_conditions/fr_rlink1.F
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SYSFUS2                       source/system/sysfus.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FR_RLALE(M1,M2,NOD,ITABM1,ITAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER M1, M2,  NOD(*),ITABM1(*),ITAG
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N,NSN,
     .        NODTMP(ABS(ITAG)+2),NODU(ABS(ITAG)+2)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER SYSFUS2
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C  NOD(1:NSN) is the array containing user ids from
C  /VEL/ALE card (ale links) if the node is present on
C local domain then it is replaced by its internal id
C otherwise its sign is changed.
C                     / internal_id, if present in local domain
C OUTPUT:  NOD(id) = -
C                     \ -user id, otherwise
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C Recherche du no de noeuds internes
C
      IF(ITAG>0)THEN !ALE LINK DEFINED BY A NODE LIST
        NSN=ITAG
        NODU(NSN+1)=M1
        NODU(NSN+2)=M2
        NODTMP(NSN+1) = SYSFUS2(M1,ITABM1,NUMNOD)
        NODTMP(NSN+2) = SYSFUS2(M2,ITABM1,NUMNOD)
        DO I=1,NSN
          NODU(I) = NOD(I)
          ! SYSFUS2 retourne 0 si noeud non trouve, local id otherwise
          NODTMP(I) = SYSFUS2(NOD(I),ITABM1,NUMNOD)
        END DO
        !Denombrement nb de noeuds locaux (ie nb de noeuds N<>0)
        DO I=1,NSN
          IF(NODTMP(I)/=0) THEN
            NOD(I) = NODTMP(I)
          ELSE
            NOD(I) = -NODU(I)
          END IF
        END DO
        ! si noeuds non present alors tag en - pour M1 et M2
        IF(NODTMP(NSN+1)==0)THEN
          M1=-M1
        ELSE
          M1=NODTMP(NSN+1)
        END IF
        IF(NODTMP(NSN+2)==0)THEN
          M2=-M2
        ELSE
          M2=NODTMP(NSN+2)
        END IF
        ! Verification des ID user avec comm globale
        IF(NSPMD > 1) CALL SPMD_GLOB_ISUM9(NODTMP,NSN+2)
        IF(ISPMD==0) THEN
         !stop if main nodes not found
         DO I = NSN+1, NSN+2
          IF(NODTMP(I)==0) THEN
            CALL ANCMSG(MSGID=186,ANMODE=ANINFO_BLIND,
     .                  I1=NODU(I),C1='ALE LINK')
            IERR=IERR+1
            RETURN
          END IF
         END DO
         !stop if a SECONDARY node is not found
         DO I = 1, NSN
          IF(NODTMP(I)==0) THEN
            CALL ANCMSG(MSGID=186,ANMODE=ANINFO_BLIND,
     .                  I1=NODU(I),C1='ALE LINK')
            IERR=IERR+1
            RETURN
          END IF
         END DO
        ENDIF

      ELSE ! ALE LINK DEFINED FROM A GRNOD
        NSN=0
        NODU(NSN+1)=M1
        NODU(NSN+2)=M2
        NODTMP(NSN+1) = SYSFUS2(M1,ITABM1,NUMNOD)
        NODTMP(NSN+2) = SYSFUS2(M2,ITABM1,NUMNOD)
        ! si noeuds non present alors tag en - pour M1 et M2
        IF(NODTMP(NSN+1)==0)THEN
          M1=-M1
        ELSE
          M1=NODTMP(NSN+1)
        END IF
        IF(NODTMP(NSN+2)==0)THEN
          M2=-M2
        ELSE
          M2=NODTMP(NSN+2)
        END IF
        ! Verification des ID user avec comm globale
        IF(NSPMD > 1) CALL SPMD_GLOB_ISUM9(NODTMP,NSN+2)
        IF(ISPMD==0) THEN
         !stop if main nodes not found
         DO I = NSN+1, NSN+2
          IF(NODTMP(I)==0) THEN
            CALL ANCMSG(MSGID=186,ANMODE=ANINFO_BLIND,
     .                  I1=NODU(I),C1='ALE LINK')
            IERR=IERR+1
            RETURN
          END IF
         END DO
        ENDIF

        !CHECK EXISTENS OF GRNOD_ID

      ENDIF !(ITAG>0)

      RETURN
      END

