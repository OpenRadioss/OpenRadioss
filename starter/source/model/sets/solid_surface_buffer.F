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
Chd|  SOLID_SURFACE_BUFFER          source/model/sets/solid_surface_buffer.F
Chd|-- called by -----------
Chd|        SURFACE_BUFFER                source/model/sets/surface_buffer.F
Chd|-- calls ---------------
Chd|        SURF_SEGMENT                  source/model/sets/solid_surface_buffer.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE SOLID_SURFACE_BUFFER(
     .                        IXS      ,IXS10    ,IXC      ,IXTG      ,CLAUSE   ,
     .                        KNOD2ELS ,NOD2ELS  ,KNOD2ELC ,NOD2ELC   ,KNOD2ELTG,
     .                        NOD2ELTG ,NSEG     ,IEXT     ,BUFTMPSURF,IPARTS   ,
     .                        IPARTC   ,IPARTG   ,IAD_SURF ,KEYSET)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEXT,NSEG,IAD_SURF
      INTEGER IXS(NIXS,*),IXS10(6,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        KNOD2ELS(*),NOD2ELS(*),KNOD2ELC(*),NOD2ELC(*),
     .        KNOD2ELTG(*),NOD2ELTG(*),BUFTMPSURF(*),IPARTS(*),
     .        IPARTC(*),IPARTG(*)
      CHARACTER KEYSET*ncharfield
!
      TYPE (SET_) ::  CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,J,K,JS,KS,II,JJ,K1,K2,LL,FACE(4),FC10(3),NN,KK,I1,
     .        NI(4),NS(4),MI(4),MS(4),NMIN,MMIN,NF,MF,IPERM,N1,N2,I
      INTEGER NODTAG(NUMNOD),FASTAG(NUMELS),FACES(4,6),PWR(7),
     .        FACES10(3,6),NNS,ISHEL,ISEG,NB_SOLID,IND
      INTEGER, DIMENSION(:), ALLOCATABLE:: SOLID_TAG,PART_TAG

!
      DATA FACES/4,3,2,1,
     .           5,6,7,8,
     .           1,2,6,5,
     .           3,4,8,7,
     .           2,3,7,6,
     .           1,5,8,4/
      DATA FACES10/0,0,0,
     .             0,0,0,
     .             3,6,4,
     .             5,6,2,
     .             1,2,3,
     .             4,5,1/
      DATA PWR/1,2,4,8,16,32,64/
C=======================================================================
      ALLOCATE(SOLID_TAG(NUMELS))
      ALLOCATE(PART_TAG(NPART))
      SOLID_TAG(1:NUMELS)=0
      PART_TAG(1:NPART)=0

      IF(KEYSET == 'PART' )THEN
        DO I=1, CLAUSE%NB_PART
            PART_TAG(CLAUSE%PART(I))=1
        ENDDO
      ENDIF
      IF(KEYSET == 'SOLID' )THEN
        DO I=1, CLAUSE%NB_SOLID
            SOLID_TAG(CLAUSE%SOLID(I))=1
        ENDDO
      ENDIF

      FASTAG=0
C
      IF (IEXT == 1) THEN
C
        NB_SOLID = CLAUSE%NB_SOLID
        DO IND=1,NB_SOLID
          JS = CLAUSE%SOLID(IND)
          DO JJ=1,6
            DO II=1,4
              NS(II)=IXS(FACES(II,JJ)+1,JS)
            END DO
C
C           keep only 1 occurence of each node (triangles, degenerated cases...)
C
            DO K1=1,3
              DO K2=K1+1,4
                IF(NS(K2)==NS(K1))NS(K2)=0
              END DO
            END DO
            NF=0
            DO K1=1,4
              N1=NS(K1)
              IF(N1/=0)THEN
                    NF=NF+1
                    NS(NF)=N1
              END IF
            END DO
            IF (NF < 3)CYCLE
C
C           permute
C
            NMIN=NS(1)
            DO II=2,NF
              NMIN=MIN(NMIN,NS(II))
            END DO
            DO IPERM=1,NF
              IF(NMIN==NS(IPERM).AND.
     .           NS(MOD(IPERM,NF)+1)/=NS(IPERM))THEN
                DO II=1,NF
                  NI(II)=NS(MOD(II+IPERM-2,NF)+1)
                END DO
                EXIT
              END IF
            END DO
C
C           looks for an elt sharing the face.
C
            DO K=KNOD2ELS(NI(1))+1,KNOD2ELS(NI(1)+1)
              KS=NOD2ELS(K)
              IF (KS==JS .OR. KS > NUMELS8+NUMELS10) CYCLE
!!              IF (CLAUSE%NB_SOLID > 0 .AND. CLAUSE%SOLID(KS)==0) CYCLE
!!              IF (CLAUSE%NB_PART > 0  .AND. CLAUSE%PART(IPARTS(KS))==0) CYCLE
              IF (KEYSET == 'SOLID' .AND. SOLID_TAG(KS)==0) CYCLE
              IF (KEYSET == 'PART'  .AND. PART_TAG(IPARTS(KS))==0) CYCLE
!
              DO II=1,NF
                NODTAG(NI(II))=0
              END DO
              DO II=1,8
                NODTAG(IXS(II+1,KS))=1
              END DO
              NN=0
              DO II=1,NF
                NN=NN+NODTAG(NI(II))
              END DO
              IF(NN==NF)THEN
                DO KK=1,6
                  DO II=1,4
                    MS(II)=IXS(FACES(II,KK)+1,KS)
                  END DO
C
C                 keep only 1 occurence of each node (triangles, degenerated cases...)
C
                  DO K1=1,3
                    DO K2=K1+1,4
                      IF(MS(K2)==MS(K1))MS(K2)=0
                    END DO
                  END DO
                  MF=0
                  DO K1=1,4
                    N1=MS(K1)
                    IF(N1/=0)THEN
                      MF=MF+1
                      MS(MF)=N1
                    END IF
                  END DO
                  IF(MF /= NF)CYCLE
C
C                 permute
C
                      MMIN=MS(1)
                      DO II=2,MF
                        MMIN=MIN(MMIN,MS(II))
                      END DO
                  DO IPERM=1,MF
                    IF(MMIN==MS(IPERM).AND.
     .                 MS(MOD(IPERM,MF)+1)/=MS(IPERM))THEN
                      DO II=1,MF
                            MI(II)=MS(MOD(II+IPERM-2,MF)+1)
                      END DO
                      EXIT
                    END IF
                  END DO
                  IF(MI(1)==NI(1).AND.MI(NF)==NI(2))THEN
C                    FACTAG(JS) moins face jj
                     FASTAG(JS)=FASTAG(JS)+PWR(JJ)
                     GO TO 300
                  END IF
                END DO
              END IF
            END DO
 300        CONTINUE
          END DO
        END DO ! DO IND=1,NB_SOLID
      END IF ! IF(IEXT==1)THEN
C-----------



!-------
!       HEXA8
!-------



        NB_SOLID = CLAUSE%NB_SOLID
        DO IND=1,NB_SOLID
          JS = CLAUSE%SOLID(IND)
!
          IF (JS > NUMELS8) CYCLE   ! HEXA8 ONLY
!
          LL=FASTAG(JS)
          DO JJ=1,6
            IF(MOD(LL,PWR(JJ+1))/PWR(JJ)/=0)CYCLE
C
C           still needs to filter degenerated faces
            DO K1=1,4
              I1      =FACES(K1,JJ)+1
              FACE(K1)=IXS(I1,JS)
            END DO
            DO K1=1,4
              N1=FACE(K1)
              DO K2=1,4
                IF(K2/=K1)THEN
                  N2=FACE(K2)
                  IF(N2==N1)FACE(K2)=0
                END IF
              END DO
            END DO
            NN=0
            DO K1=1,4
              N1=FACE(K1)
              IF(N1/=0)THEN
                    NN=NN+1
                    FACE(NN)=N1
              END IF
            END DO
C---   find shells SURF/PART/EXT
            IF(NN==3)THEN
              KS = 0
              ISHEL = 0
              DO K=KNOD2ELTG(FACE(1))+1,KNOD2ELTG(FACE(1)+1)
                KS=NOD2ELTG(K)
                ISHEL = 0
                DO I=1,3
                  DO J=1,3
                    IF(FACE(I) == IXTG(J+1,KS)) ISHEL = ISHEL + 1
                  ENDDO
                ENDDO
                IF (ISHEL == 3)EXIT
                KS = 0
              ENDDO
              IF(KS == 0)THEN
                NSEG = NSEG + 1
                CALL SURF_SEGMENT(FACE(1)    ,FACE(2) ,FACE(3) ,FACE(3) ,JS  ,
     .                         BUFTMPSURF ,IAD_SURF ,1)
!!              ELSEIF (CLAUSE%NB_PART > 0 .AND.
              ELSEIF (KEYSET == 'PART' .AND.
     .                IABS(PART_TAG(IPARTG(KS))) /= 1)THEN
                NSEG = NSEG + 1
                CALL SURF_SEGMENT(FACE(1)    ,FACE(2) ,FACE(3) ,FACE(3) ,JS  ,
     .                         BUFTMPSURF ,IAD_SURF ,1)
              ENDIF
            ELSEIF(NN==4)THEN
              KS = 0
              ISHEL = 0
              DO K=KNOD2ELC(FACE(1))+1,KNOD2ELC(FACE(1)+1)
                KS=NOD2ELC(K)
                ISHEL = 0
                DO I=1,4
                  DO J=1,4
                    IF(FACE(I) == IXC(J+1,KS)) ISHEL = ISHEL + 1
                  ENDDO
                ENDDO
                IF (ISHEL == 4)EXIT
                KS = 0
              ENDDO
              IF(KS == 0)THEN
               NSEG = NSEG + 1
                CALL SURF_SEGMENT(FACE(1)    ,FACE(2) ,FACE(3) ,FACE(4) ,JS  ,
     .                         BUFTMPSURF ,IAD_SURF ,1)
!!              ELSEIF (CLAUSE%NB_PART > 0 .AND.
              ELSEIF (KEYSET == 'PART' .AND.
     .                IABS(PART_TAG(IPARTC(KS))) /= 1 ) THEN
               NSEG = NSEG + 1
                CALL SURF_SEGMENT(FACE(1)    ,FACE(2) ,FACE(3) ,FACE(4) ,JS  ,
     .                         BUFTMPSURF ,IAD_SURF ,1)
              ENDIF
            END IF
C---
          END DO ! DO JJ=1,6
        END DO ! DO IND=1,NB_SOLID




!-------
!       TETRA10
!-------



        NB_SOLID = CLAUSE%NB_SOLID
        DO IND=1,NB_SOLID
          JS = CLAUSE%SOLID(IND)
!
          J = JS - NUMELS8  ! TETRA10 ONLY
          IF (J <= 0) CYCLE  ! TETRA10 ONLY
!
          LL=FASTAG(JS)
          DO JJ=3,6
            IF(MOD(LL,PWR(JJ+1))/PWR(JJ) /= 0)CYCLE
C
C           still needs to filter degenerated faces
C
            DO K1=1,4
              FACE(K1)=IXS(FACES(K1,JJ)+1,JS)
            END DO
            DO K1=1,3
              DO K2=K1+1,4
                IF(FACE(K2) == FACE(K1)) FACE(K2)=0
              END DO
            END DO
            NN=0
            DO K1=1,4
              IF(FACE(K1) /= 0)THEN
                NN=NN+1
                FACE(NN)=FACE(K1)
              END IF
            END DO
C---
            IF(NN == 3)THEN
              NNS=1
              FC10(1)=IXS10(FACES10(1,JJ),J)
              FC10(2)=IXS10(FACES10(2,JJ),J)
              FC10(3)=IXS10(FACES10(3,JJ),J)
              IF(FC10(1) /= 0)NNS=NNS+1
              IF(FC10(2) /= 0)NNS=NNS+1
              IF(FC10(3) /= 0)NNS=NNS+1
              IF(NNS == 3)NNS=2
              NSEG=NSEG+NNS
              IF (NNS == 4) THEN
c               4 triangles
                  CALL SURF_SEGMENT(FACE(1)    ,FC10(1) ,FC10(3) ,FC10(3) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                  CALL SURF_SEGMENT(FACE(2)    ,FC10(2) ,FC10(1) ,FC10(1) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                  CALL SURF_SEGMENT(FACE(3)    ,FC10(3) ,FC10(2) ,FC10(2) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                  CALL SURF_SEGMENT(FC10(1)    ,FC10(2) ,FC10(3) ,FC10(3) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
              ELSEIF (NNS == 3) THEN
c               1 quadrangle, 1 triangle
                IF(FC10(1) == 0)THEN
                  CALL SURF_SEGMENT(FACE(1)    ,FACE(2) ,FC10(2) ,FC10(3) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                  CALL SURF_SEGMENT(FACE(3)    ,FC10(3) ,FC10(2) ,FC10(2) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                ELSEIF(FC10(2) == 0)THEN
                  CALL SURF_SEGMENT(FACE(2)    ,FACE(3) ,FC10(3) ,FC10(1) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                  CALL SURF_SEGMENT(FACE(1)    ,FC10(1) ,FC10(3) ,FC10(3) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                ELSEIF(FC10(3) == 0)THEN
                  CALL SURF_SEGMENT(FACE(3)    ,FACE(1) ,FC10(1) ,FC10(2) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                  CALL SURF_SEGMENT(FACE(2)    ,FC10(2) ,FC10(1) ,FC10(1) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                ENDIF
              ELSEIF (NNS == 2) THEN
c               2 triangles
                IF(FC10(1) /= 0)THEN
                  CALL SURF_SEGMENT(FACE(3)    ,FACE(1) ,FC10(1) ,FC10(1) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                  CALL SURF_SEGMENT(FACE(2)    ,FACE(3) ,FC10(1) ,FC10(1) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                ELSEIF(FC10(2) /= 0)THEN
                  CALL SURF_SEGMENT(FACE(1)    ,FACE(2) ,FC10(2) ,FC10(2) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                  CALL SURF_SEGMENT(FACE(3)    ,FACE(1) ,FC10(2) ,FC10(2) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                ELSEIF(FC10(3) /= 0)THEN
                  CALL SURF_SEGMENT(FACE(2)    ,FACE(3) ,FC10(3) ,FC10(3) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                  CALL SURF_SEGMENT(FACE(1)    ,FACE(2) ,FC10(3) ,FC10(3) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
                ENDIF
              ELSEIF (NNS == 1) THEN
c                 1 triangle
                  CALL SURF_SEGMENT(FACE(1)    ,FACE(2) ,FACE(3) ,FACE(3) ,JS  ,
     .                           BUFTMPSURF ,IAD_SURF ,1)
              END IF
            END IF
C---
          END DO ! DO JJ=3,6
        END DO ! DO IND=1,NB_SOLID
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SURF_SEGMENT                  source/model/sets/solid_surface_buffer.F
Chd|-- called by -----------
Chd|        QUAD_SURFACE_BUFFER           source/model/sets/quad_surface_buffer.F
Chd|        SOLID_SURFACE_BUFFER          source/model/sets/solid_surface_buffer.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SURF_SEGMENT(N1         ,N2       ,N3      ,N4      ,ELEM,
     .                        BUFTMPSURF ,IAD_SURF ,ELTYP  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
      INTEGER N1,N2,N3,N4,ELEM,BUFTMPSURF(*),IAD_SURF,ELTYP
C-----------------------------------------------
!---
!       seg to add
        BUFTMPSURF(IAD_SURF) = N1
        IAD_SURF=IAD_SURF+1
        BUFTMPSURF(IAD_SURF) = N2
        IAD_SURF=IAD_SURF+1
        BUFTMPSURF(IAD_SURF) = N3
        IAD_SURF=IAD_SURF+1
        BUFTMPSURF(IAD_SURF) = N4
        IAD_SURF=IAD_SURF+1
        BUFTMPSURF(IAD_SURF) = ELTYP
        IAD_SURF=IAD_SURF+1
        BUFTMPSURF(IAD_SURF) = ELEM ! ELEM
        IAD_SURF=IAD_SURF+1
!---
      RETURN
      END
