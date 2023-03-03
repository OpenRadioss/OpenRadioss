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
Chd|  PRESEARCHIGEO3D               source/elements/ige3d/searchigeo3d.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MYQSORT3D                     source/elements/ige3d/searchigeo3d.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE PRESEARCHIGEO3D(IGRSURF,xigetmp, permige)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PERMIGE(*)
C     REAL
      my_real 
     .  xigetmp(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IGS,J,NBTABIGE,IADTABIGE
C-----------------------------------------------
c      NBTABIGE=0
      IADTABIGE = 0
c
      DO IGS=1,NSURF
        IF(IGRSURF(IGS)%NSEG_IGE>0) THEN !! on est sur une surface o   on a des elements isogeometriques
c          NBTABIGE=NBTABIGE+16*IGRSURF(IGS)%NSEG_IGE ! nombre de points non tries de cette surface
          NBTABIGE=16*IGRSURF(IGS)%NSEG_IGE/9 ! nombre de points non tries de cette surface

          CALL MYQSORT3D(NBTABIGE,XIGETMP(3*(IADTABIGE)+1),PERMIGE(IADTABIGE+1))

          IADTABIGE = IADTABIGE + NBTABIGE

        ENDIF
      ENDDO

c      CALL MYQSORT3D(NBTABIGE,XIGETMP,PERMIGE)
c
      RETURN
      END
      
Chd|====================================================================
Chd|  SEARCHIGEO3D                  source/elements/ige3d/searchigeo3d.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SEARCHIGEO3D(IGRSURF,IADTABIGEINI, PERM, 
     .                  NIGETMP, NIGE, RIGETMP, RIGE,
     .                  XIGETMP, XIGE, VIGETMP, VIGE,
     .                  NDOUBLONSTOT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADTABIGEINI,NDOUBLONSTOT,PERM(*), NIGETMP(*),NIGE(*)
c      INTEGER N,NDOUBLONSTOT,NSEGIGE,PERM(*), NIGETMP(*),NIGE(*),
c     .        IGBUFSSGTMP(*), IGBUFSSG(*)
C     REAL
      my_real 
     .  RIGETMP(3,*), XIGETMP(3,*), VIGETMP(3,*),
     .  RIGE(3,*)   , XIGE(3,*)   , VIGE(3,*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NVALEURS, PERMUTE(IADTABIGEINI),NDOUBLONS,NBTABIGE
      INTEGER I,J,K,ITSURF,DECALSURF,IADTABIGE
C     REAL
      my_real
     .     TOL
C-----------------------------------------------
      TOL=EM06
c
      ITSURF=1
      I=1
      NDOUBLONSTOT=0
      IADTABIGE=0      
      DO ITSURF=1,NSURF
      
        IF(IGRSURF(ITSURF)%NSEG_IGE>0) THEN !! on est sur une surface o   on a des elements isogeometriques
       
          IGRSURF(ITSURF)%IAD_IGE = I-1
          DECALSURF=I+NDOUBLONSTOT-1
          NBTABIGE=16*IGRSURF(ITSURF)%NSEG_IGE/9 ! nombre de points non tries de cette surface
          IADTABIGE = IADTABIGE + NBTABIGE
          NDOUBLONS=0

          DO WHILE(I+NDOUBLONSTOT+NDOUBLONS<=IADTABIGE)
            NVALEURS = 0
            NIGE(I)   = NIGETMP(PERM(I+NDOUBLONSTOT+NDOUBLONS)+DECALSURF) ! + decalage dans le perm
            RIGE(:,I) = RIGETMP(:,PERM(I+NDOUBLONSTOT+NDOUBLONS)+DECALSURF)
            XIGE(:,I) = XIGETMP(:,I+NDOUBLONSTOT+NDOUBLONS)
            VIGE(:,I) = VIGETMP(:,PERM(I+NDOUBLONSTOT+NDOUBLONS)+DECALSURF)
            PERMUTE(PERM(I+NDOUBLONSTOT+NDOUBLONS)+DECALSURF) = I - IGRSURF(ITSURF)%IAD_IGE
            DO WHILE (((I+NDOUBLONSTOT+NDOUBLONS+NVALEURS+1)<=DECALSURF+NBTABIGE)!-1)
     .         .AND. (ABS(XIGETMP(1,I+NDOUBLONSTOT+NDOUBLONS)-
     .                    XIGETMP(1,I+NDOUBLONSTOT+NDOUBLONS+NVALEURS+1)) <= TOL)
     .         .AND. (ABS(XIGETMP(2,I+NDOUBLONSTOT+NDOUBLONS)-
     .                    XIGETMP(2,I+NDOUBLONSTOT+NDOUBLONS+NVALEURS+1)) <= TOL)
     .         .AND. (ABS(XIGETMP(3,I+NDOUBLONSTOT+NDOUBLONS)-
     .                    XIGETMP(3,I+NDOUBLONSTOT+NDOUBLONS+NVALEURS+1)) <= TOL))
              NVALEURS = NVALEURS + 1
              PERMUTE(PERM(I+NDOUBLONSTOT+NDOUBLONS+NVALEURS)+DECALSURF) = I - IGRSURF(ITSURF)%IAD_IGE
            ENDDO
            NDOUBLONS=NDOUBLONS+NVALEURS
            I=I+1
          ENDDO
          NDOUBLONSTOT=NDOUBLONSTOT+NDOUBLONS
          
          DO J=1,IGRSURF(ITSURF)%NSEG_IGE!NSEGIGE
           DO K=1,4
            IGRSURF(ITSURF)%NODES_IGE(J,K)=PERMUTE(IGRSURF(ITSURF)%NODES_IGE(J,K)-NUMNOD)+NUMNOD
           ENDDO
          ENDDO
        ENDIF

      ENDDO
c
      NUMFAKENODIGEO=NUMFAKENODIGEO-NDOUBLONSTOT 
      
      RETURN
      END
      
Chd|====================================================================
Chd|  SEARCHIGEO3DOLD               source/elements/ige3d/searchigeo3d.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SEARCHIGEO3DOLD(IGRSURF,IADTABIGEINI, PERM, 
     .                  NIGETMP, NIGE, RIGETMP, RIGE,
     .                  XIGETMP, XIGE, VIGETMP, VIGE,
     .                  NDOUBLONSTOT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADTABIGEINI,NDOUBLONSTOT,PERM(*), NIGETMP(*),NIGE(*)
c      INTEGER N,NDOUBLONSTOT,NSEGIGE,PERM(*), NIGETMP(*),NIGE(*),
c     .        IGBUFSSGTMP(*), IGBUFSSG(*)
C     REAL
      my_real 
     .  RIGETMP(3,*), XIGETMP(3,*), VIGETMP(3,*),
     .  RIGE(3,*)   , XIGE(3,*)   , VIGE(3,*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NVALEURS, PERMUTE(IADTABIGEINI),NDOUBLONS,NBTABIGE
      INTEGER I,J,ITSURF,DECALSURF,IADTABIGE
C     REAL
      my_real
     .     TOL
C-----------------------------------------------
      TOL=EM06
c
      ITSURF=1
      I=1
      NDOUBLONSTOT=0
      DO ITSURF=1,NSURF

        DECALSURF=I+NDOUBLONSTOT-1

        IF(IGRSURF(ITSURF)%NSEG_IGE>0) THEN !! on est sur une surface o   on a des elements isogeometriques
          NBTABIGE=16*IGRSURF(ITSURF)%NSEG_IGE/9! nombre de points non tries de cette surface
          NDOUBLONS=0

          DO WHILE(I+NDOUBLONSTOT+NDOUBLONS<=DECALSURF+NBTABIGE)
            NVALEURS = 0
            NIGE(I)   = NIGETMP(PERM(I+NDOUBLONSTOT+NDOUBLONS)+DECALSURF) ! + decalage dans le perm
            RIGE(:,I) = RIGETMP(:,PERM(I+NDOUBLONSTOT+NDOUBLONS)+DECALSURF)
            XIGE(:,I) = XIGETMP(:,I+NDOUBLONSTOT+NDOUBLONS)
            VIGE(:,I) = VIGETMP(:,PERM(I+NDOUBLONSTOT+NDOUBLONS)+DECALSURF)
            PERMUTE(PERM(I+NDOUBLONSTOT+NDOUBLONS)+DECALSURF) = I!+DECALSURF
            DO WHILE (((I+NDOUBLONSTOT+NDOUBLONS+NVALEURS+1)<=DECALSURF+NBTABIGE)!-1)
     .         .AND. (ABS(XIGETMP(1,I+NDOUBLONSTOT+NDOUBLONS)-
     .                    XIGETMP(1,I+NDOUBLONSTOT+NDOUBLONS+NVALEURS+1)) <= TOL)
     .         .AND. (ABS(XIGETMP(2,I+NDOUBLONSTOT+NDOUBLONS)-
     .                    XIGETMP(2,I+NDOUBLONSTOT+NDOUBLONS+NVALEURS+1)) <= TOL)
     .         .AND. (ABS(XIGETMP(3,I+NDOUBLONSTOT+NDOUBLONS)-
     .                    XIGETMP(3,I+NDOUBLONSTOT+NDOUBLONS+NVALEURS+1)) <= TOL))
              NVALEURS = NVALEURS + 1
              PERMUTE(PERM(I+NDOUBLONSTOT+NDOUBLONS+NVALEURS)+DECALSURF) = I!+DECALSURF
            ENDDO
            NDOUBLONS=NDOUBLONS+NVALEURS
            I=I+1
          ENDDO
          NDOUBLONSTOT=NDOUBLONSTOT+NDOUBLONS
        ENDIF

      ENDDO
c
      IADTABIGE=0
      DO ITSURF=1,NSURF
        IF(IGRSURF(ITSURF)%NSEG_IGE>0) THEN !! on est sur une surface o   on a des elements isogeometriques
          DO I=1,IGRSURF(ITSURF)%NSEG_IGE!NSEGIGE
           DO J=1,4
            IGRSURF(ITSURF)%NODES_IGE(I,J)=PERMUTE(IGRSURF(ITSURF)%NODES_IGE(I,J)-NUMNOD)+NUMNOD
           ENDDO
          ENDDO
          NBTABIGE=16*IGRSURF(ITSURF)%NSEG_IGE/9 ! nombre de points non tries de cette surface
          IADTABIGE = IADTABIGE + NBTABIGE
        ENDIF
        
      ENDDO
c
      NUMFAKENODIGEO=NUMFAKENODIGEO-NDOUBLONSTOT 
      
      RETURN
      END

Chd|====================================================================
Chd|  SEARCHIGEO3D3                 source/elements/ige3d/searchigeo3d.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SEARCHIGEO3D3(IGRSURF,N, PERM, 
c      SUBROUTINE SEARCHIGEO3D(IGRSURF,N, NSEGIGE ,PERM, 
     .                        NIGETMP, NIGE, RIGETMP, RIGE,
     .                        XIGETMP, XIGE, VIGETMP, VIGE,
     .                        NDOUBLONSTOT)
c     .                        IGBUFSSGTMP, IGBUFSSG, NDOUBLONSTOT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,NDOUBLONSTOT,PERM(*), NIGETMP(*),NIGE(*)
c      INTEGER N,NDOUBLONSTOT,NSEGIGE,PERM(*), NIGETMP(*),NIGE(*),
c     .        IGBUFSSGTMP(*), IGBUFSSG(*)
C     REAL
      my_real 
     .  RIGETMP(3,*), XIGETMP(3,*), VIGETMP(3,*),
     .  RIGE(3,*)   , XIGE(3,*)   , VIGE(3,*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NVALEURS, PERMUTE(N-1),NDOUBLONS,NBTABIGE,IADTABIGE,
     .        IADTABIGEINI
      INTEGER I,J,ITSURF,DECALSURF,DECAL
C     REAL
      my_real
     .     TOL
C-----------------------------------------------
      TOL=EM06
c      IADTABIGEINI=N-1

      IADTABIGEINI=N
c
      ITSURF=1
      NDOUBLONSTOT=0
      I=1
      DECALSURF=0
      DECAL=0
      NBTABIGE=0
      
      DO ITSURF=1,NSURF
        DECALSURF=DECALSURF+I-1 !+NDOUBLONS-1
        DECAL = DECAL+NBTABIGE
        I=1

c on attaque ici les groupes de surface mais on a aussi les tableaux NIGE RIGE ET XIGE qui sont globaux

        IF(IGRSURF(ITSURF)%NSEG_IGE>0) THEN !! on est sur une surface o   on a des elements isogeometriques
          NBTABIGE=16*IGRSURF(ITSURF)%NSEG_IGE/9! nombre de points non tries de cette surface

c          NBTABIGE=16*IGRSURF(ITSURF)%NSEG_IGE ! nombre de points non tries de cette surface
c          IADTABIGE=ISURF(13,ITSURF) ! adresse des points non tries de cette surface dans NIGE
c          ISURF(13,ITSURF)=I ! nouvelle adresse des points tries de cette surface dans NIGE
          NDOUBLONS=0

          DO WHILE(I+NDOUBLONS<=NBTABIGE) !   a devrait etre tout les points, pas qu'une seule surface
c          DO WHILE(I+NDOUBLONSTOT+NDOUBLONS<=IADTABIGE+NBTABIGE-1)
            NVALEURS = 0
            NIGE(I+DECALSURF)   = NIGETMP(PERM(I+NDOUBLONS+DECAL)) ! + decalage dans le perm
            RIGE(:,I+DECALSURF) = RIGETMP(:,PERM(I+NDOUBLONS+DECAL))
            XIGE(:,I+DECALSURF) = XIGETMP(:,I+DECALSURF+NDOUBLONS)
            VIGE(:,I+DECALSURF) = VIGETMP(:,PERM(I+NDOUBLONS+DECAL))
            PERMUTE(PERM(I+NDOUBLONS+DECAL)) = I+DECALSURF
c            PERMUTE(PERM(I+NDOUBLONSTOT+NDOUBLONS)+DECALSURF) = I-ISURF(13,ITSURF)+1
            DO WHILE (((I+NDOUBLONS+NVALEURS+1)<=NBTABIGE)
c            DO WHILE (((I+NDOUBLONSTOT+NDOUBLONS+NVALEURS+1)<=IADTABIGE+NBTABIGE-1)
     .         .AND. (ABS(XIGETMP(1,I+DECALSURF+NDOUBLONS)-
     .                    XIGETMP(1,I+DECALSURF+NDOUBLONS+NVALEURS+1)) <= TOL)
     .         .AND. (ABS(XIGETMP(2,I+DECALSURF+NDOUBLONS)-
     .                    XIGETMP(2,I+DECALSURF+NDOUBLONS+NVALEURS+1)) <= TOL)
     .         .AND. (ABS(XIGETMP(3,I+DECALSURF+NDOUBLONS)-
     .                    XIGETMP(3,I+DECALSURF+NDOUBLONS+NVALEURS+1)) <= TOL))
              NVALEURS = NVALEURS + 1
              PERMUTE(PERM(I+NDOUBLONS+NVALEURS+DECAL)) = I+DECALSURF
c              PERMUTE(PERM(I+NDOUBLONSTOT+NDOUBLONS+NVALEURS)+DECALSURF) = I-ISURF(13,ITSURF)+1
            ENDDO
            NDOUBLONS=NDOUBLONS+NVALEURS
            I=I+1
          ENDDO
          NDOUBLONSTOT=NDOUBLONSTOT+NDOUBLONS
        ENDIF

      ENDDO
c
      DO ITSURF=1,NSURF
        IF(IGRSURF(ITSURF)%NSEG_IGE>0) THEN !! on est sur une surface o   on a des elements isogeometriques
          DO I=1,IGRSURF(ITSURF)%NSEG_IGE!NSEGIGE
           DO J=1,4
            IGRSURF(ITSURF)%NODES_IGE(I,J)=PERMUTE(IGRSURF(ITSURF)%NODES_IGE(I,J)-NUMNOD)+NUMNOD
c            IGBUFSSG(6*(I-1)+J) = PERMUTE(IGBUFSSGTMP(6*(I-1)+J)-NUMNOD)+NUMNOD
           ENDDO
          ENDDO
        ENDIF
      ENDDO
c
      NUMFAKENODIGEO=NUMFAKENODIGEO-NDOUBLONSTOT
c      NUMFAKENODIGEO=IADTABIGEINI-NDOUBLONSTOT
c      N=NUMFAKENODIGEO
c     
      RETURN
      END
c

Chd|====================================================================
Chd|  SEARCHIGEO3D2                 source/elements/ige3d/searchigeo3d.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SEARCHIGEO3D2(IGRSURF,IADTABIGE, PERM, 
     .                        NIGETMP, NIGE, RIGETMP, RIGE,
     .                        XIGETMP, XIGE, VIGETMP, VIGE,
     .                        NDOUBLONS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDOUBLONS,PERM(*), NIGETMP(*),NIGE(*),
     .        IADTABIGE
C     REAL
      my_real 
     .  RIGETMP(3,*), XIGETMP(3,*), VIGETMP(3,*),
     .  RIGE(3,*)   , XIGE(3,*)   , VIGE(3,*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NVALEURS, PERMUTE(IADTABIGE)
      INTEGER I,J,ITSURF
C     REAL
      my_real
     .     TOL
C-----------------------------------------------
      TOL=EM06
c
      ITSURF=1
      I=1
      NDOUBLONS=0

      DO WHILE(I+NDOUBLONS<=IADTABIGE)
        NVALEURS = 0
        NIGE(I)   = NIGETMP(PERM(I+NDOUBLONS)) ! + decalage dans le perm
        RIGE(:,I) = RIGETMP(:,PERM(I+NDOUBLONS))
        XIGE(:,I) = XIGETMP(:,I+NDOUBLONS)
        VIGE(:,I) = VIGETMP(:,PERM(I+NDOUBLONS))
        PERMUTE(PERM(I+NDOUBLONS)) = I!+1
        DO WHILE (((I+NDOUBLONS+NVALEURS+1)<=IADTABIGE))
          IF(ABS(XIGETMP(3,I+NDOUBLONS)-XIGETMP(3,I+NDOUBLONS+NVALEURS+1)) > TOL) EXIT
          IF(ABS(XIGETMP(2,I+NDOUBLONS)-XIGETMP(2,I+NDOUBLONS+NVALEURS+1)) > TOL) EXIT
          IF(ABS(XIGETMP(1,I+NDOUBLONS)-XIGETMP(1,I+NDOUBLONS+NVALEURS+1)) > TOL) EXIT
c     .     .AND. (ABS(XIGETMP(1,I+NDOUBLONS)-
c     .                XIGETMP(1,I+NDOUBLONS+NVALEURS+1)) <= TOL)
c     .     .AND. (ABS(XIGETMP(2,I+NDOUBLONS)-
c     .                XIGETMP(2,I+NDOUBLONS+NVALEURS+1)) <= TOL)
c     .     .AND. (ABS(XIGETMP(3,I+NDOUBLONS)-
c     .                XIGETMP(3,I+NDOUBLONS+NVALEURS+1)) <= TOL))
           NVALEURS = NVALEURS + 1
           PERMUTE(PERM(I+NDOUBLONS+NVALEURS)) = I!+1
        ENDDO
        NDOUBLONS=NDOUBLONS+NVALEURS
        I=I+1
      ENDDO

      DO ITSURF=1,NSURF
        IF(IGRSURF(ITSURF)%NSEG_IGE>0) THEN !! on est sur une surface o   on a des elements isogeometriques
          DO I=1,IGRSURF(ITSURF)%NSEG_IGE
           DO J=1,4
            IGRSURF(ITSURF)%NODES_IGE(I,J)=PERMUTE(IGRSURF(ITSURF)%NODES_IGE(I,J)-NUMNOD)+NUMNOD
           ENDDO
          ENDDO
        ENDIF
      ENDDO
c
      NUMFAKENODIGEO=IADTABIGE-NDOUBLONS
      IADTABIGE=NUMFAKENODIGEO
c     
      RETURN
      END
c
Chd|====================================================================
Chd|  MYQSORT3D                     source/elements/ige3d/searchigeo3d.F
Chd|-- called by -----------
Chd|        PRESEARCHIGEO3D               source/elements/ige3d/searchigeo3d.F
Chd|        REORDER_IG3D                  source/elements/ige3d/reorder_ig3d.F
Chd|-- calls ---------------
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|====================================================================
      SUBROUTINE MYQSORT3D(n, X, perm)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER n,perm (n)
      my_real 
     .  X (3,n)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real, DIMENSION(:,:), ALLOCATABLE::  X_COPY
      my_real, DIMENSION(:), ALLOCATABLE :: RKEY
      my_real :: MAXI
      INTEGER :: I,error

C-----------------------------------------------
       ALLOCATE(X_copy(3,n))
       ALLOCATE(RKEY(n))


       X_copy(1:3,1:n) = X(1:3,1:n) 
       MAXI = 0
       DO i = 1,n
          MAXI = max(abs(X(1,i)),MAXI)
          MAXI = max(abs(X(2,i)),MAXI)
          MAXI = max(abs(X(3,i)),MAXI)
       ENDDO                                
       DO I = 1,N
        RKEY(I) = (X(1,I)+MAXI) * MAXI * MAXI + (X(2,I)+MAXI) * MAXI + (X(3,I)+MAXI)
       ENDDO
       CALL MYQSORT(3*n,RKEY,PERM,error)
       DO I = 1, n
         X(1,I) = X_COPY(1,PERM(I))
         X(2,I) = X_COPY(2,PERM(I))
         X(3,I) = X_COPY(3,PERM(I))
       ENDDO
       DEALLOCATE(X_COPY)
       DEALLOCATE(RKEY)
      RETURN
c
      end
c
