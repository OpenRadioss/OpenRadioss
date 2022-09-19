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
Chd|  DSACTI1                       source/implicit/dsolve/dsacti.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE DSACTI1(GRAPHE, IKC, NDDLQ, NDDL0)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKC(*), NDDLQ, NDDL0
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, DSNDDL, NDDLACTI, ITAG(NDDL0), NDI0, NDF0, NDI, NDF,
     .        K, KK, IAD, NSDMAX, L
C-----------------------------------------------------------------------
C REPORT DES BLOCAGES PRESENTS DANS LA MATRICE DE RIGIDITE SUR LE GRAPHE
C-----------------------------------------------------------------------
      DO I=1,NSPROC
         DSNDDL=GRAPHE(I)%NDDL_INI
         NDDLACTI=0
         DO J=1,DSNDDL
            IF (IKC(J)<1) THEN
               NDDLACTI=NDDLACTI+1
               ITAG(J)=NDDLACTI
               DO K=1,2
                  GRAPHE(I)%LSDDL(K,NDDLACTI)=GRAPHE(I)%LSDDL_INI(K,J)
               ENDDO
               GRAPHE(I)%LSDDL_GLOB(NDDLACTI)
     .                              =GRAPHE(I)%LSDDL_GLOB_INI(J)
            ENDIF
         ENDDO
         GRAPHE(I)%NDDL=NDDLACTI
         IF (NDDLACTI/=NDDLQ) THEN
            CALL ANCMSG(MSGID=36,ANMODE=ANINFO,
     .            C1='NDDLQ NON EQUAL TO NDDLACTI')
            CALL ARRET(2)
         ENDIF
C Elimination des ddls bloques dans tous les superelements
         DO J=1,GRAPHE(I)%NSUPEL
            NDI0=GRAPHE(I)%DGRAPH(J)%NDDL_I_INI
            NDF0=GRAPHE(I)%DGRAPH(J)%NDDL_F_INI
*
*            WRITE(10,*) 'I ',I,' J ',J
*            WRITE(10,*) 'DDLS_INI - NDF0 ',NDF0,' NDI0 ',NDI0
*            WRITE(10,'(10I8)')
*     .           (GRAPHE(I)%DGRAPH(J)%DDLS_INI(K),K=1,NDF0+NDI0)
*
            NSDMAX=GRAPHE(I)%DGRAPH(J)%NSDMAX
            NDI=0
            NDF=0
            IAD=0
            DO K=1,NDF0
               KK=GRAPHE(I)%DGRAPH(J)%DDLS_INI(IAD+K)
               IF (IKC(KK)<1) THEN
                  NDF=NDF+1
                  GRAPHE(I)%DGRAPH(J)%DDLS(NDF)=ITAG(KK)
                  DO L=1,NSDMAX+1
                     GRAPHE(I)%DGRAPH(J)%IFAC(L,NDF)=
     .                    GRAPHE(I)%DGRAPH(J)%IFAC_INI(L,IAD+K)
                  ENDDO
                  GRAPHE(I)%DGRAPH(J)%IFACM(NDF)=
     .                    GRAPHE(I)%DGRAPH(J)%IFACM_INI(IAD+K)
               ENDIF
            ENDDO
            IAD=IAD+NDF0
            DO K=1,NDI0
               KK=GRAPHE(I)%DGRAPH(J)%DDLS_INI(IAD+K)
               IF (IKC(KK)<1) THEN
                  NDI=NDI+1
                  GRAPHE(I)%DGRAPH(J)%DDLS(NDF+NDI)=ITAG(KK)
               ENDIF
            ENDDO
            GRAPHE(I)%DGRAPH(J)%NDDL_I=NDI
            GRAPHE(I)%DGRAPH(J)%NDDL_F=NDF
*
*            WRITE(10,*) 'DDLS - NDF ',NDF,' NDI ',NDI
*            WRITE(10,'(10I8)')
*     .           (GRAPHE(I)%DGRAPH(J)%DDLS(K),K=1,NDF+NDI)
*
         ENDDO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSACTI2                       source/implicit/dsolve/dsacti.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSACTI2(GRAPHE, LBLOC, NDDL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LBLOC(*), NDDL
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, KK, NDI0, NDF0, NDI, NDF, IAD, DDLT(NDDL),
     .        NSDMAX, L
      INTEGER, DIMENSION(:), ALLOCATABLE :: IFACMT
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IFACT
C-----------------------------------------------------------------------
C REPORT DES BLOCAGES POUR CALCUL MODAL SUR LE GRAPHE
C Ces blocages sont absents de la matrice de rigidite
C-----------------------------------------------------------------------
      DO I=1,NSPROC
C
         DO J=1,GRAPHE(I)%NSUPEL
            NDI0=GRAPHE(I)%DGRAPH(J)%NDDL_I
            NDF0=GRAPHE(I)%DGRAPH(J)%NDDL_F
*
*            WRITE(10,*) 'I ',I,' J ',J
*            WRITE(10,*) 'DDLS_INI DSACTI2- NDF0 ',NDF0,' NDI0 ',NDI0
*            WRITE(10,'(10I8)')
*     .           (GRAPHE(I)%DGRAPH(J)%DDLS(K),K=1,NDF0+NDI0)
*
            NSDMAX=GRAPHE(I)%DGRAPH(J)%NSDMAX
            ALLOCATE(IFACT(NSDMAX+1,NDF0), IFACMT(NDF0))
            NDI=0
            NDF=0
            IAD=0
            DO K=1,NDF0
               KK=GRAPHE(I)%DGRAPH(J)%DDLS(IAD+K)
               KK=LBLOC(KK)
               IF (KK==0) THEN
                  NDF=NDF+1
                  DDLT(NDF)=GRAPHE(I)%DGRAPH(J)%DDLS(IAD+K)
                  DO L=1,NSDMAX+1
                     IFACT(L,NDF)=GRAPHE(I)%DGRAPH(J)%IFAC(L,IAD+K)
                  ENDDO
                  IFACMT(NDF)=GRAPHE(I)%DGRAPH(J)%IFACM(IAD+K)
               ENDIF
            ENDDO
            IAD=IAD+NDF0
            DO K=1,NDI0
               KK=GRAPHE(I)%DGRAPH(J)%DDLS(IAD+K)
               KK=LBLOC(KK)
               IF (KK==0) THEN
                  NDI=NDI+1
                  DDLT(NDF+NDI)=GRAPHE(I)%DGRAPH(J)%DDLS(IAD+K)
               ENDIF
            ENDDO
            GRAPHE(I)%DGRAPH(J)%NDDL_I=NDI
            GRAPHE(I)%DGRAPH(J)%NDDL_F=NDF
            DO K=1,NDI+NDF
               GRAPHE(I)%DGRAPH(J)%DDLS(K)=DDLT(K)
            ENDDO
            DO K=1,NDF
               DO L=1,NSDMAX+1
                  GRAPHE(I)%DGRAPH(J)%IFAC(L,K)=IFACT(L,K)
               ENDDO
               GRAPHE(I)%DGRAPH(J)%IFACM(K)=IFACMT(K)
            ENDDO
            DEALLOCATE(IFACT, IFACMT)
*
*            WRITE(10,*) 'DDLS DSACTI2- NDF ',NDF,' NDI ',NDI
*            WRITE(10,'(10I8)')
*     .           (GRAPHE(I)%DGRAPH(J)%DDLS(K),K=1,NDF+NDI)
*
         ENDDO
C
      ENDDO
C
      RETURN
      END

