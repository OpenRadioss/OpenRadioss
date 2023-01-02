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
Chd|  TSURFTAG                      source/groups/tsurftag.F      
Chd|-- called by -----------
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        SEGSURF                       source/groups/tsurftag.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE TSURFTAG(IXS     ,IXS10  ,IGRSURF,FLAG ,NSEG  ,
     2                    KNOD2ELS,NOD2ELS,N1     ,N2   ,N3    ,
     3                    NSEG0   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXS10(6,*),
     .        KNOD2ELS(*), NOD2ELS(*)
      INTEGER FLAG, NSEG,N1,N2,N3
!
      TYPE (SURF_) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IE,IE10,NNS,ELEM,FACET,IORD,FC,ELEM8,
     .        MIDNOD(3,4),FACES(6,4),FACES10(3,16),FACE(3),
     .        FCMID10(3),FC10(3),ISEG,NSEG0

      DATA MIDNOD/1,5,4,
     .            3,2,1,
     .            3,6,4,
     .            5,6,2/
      DATA FACES/2,4,6,2,4,6,
     .           2,7,4,2,7,4,
     .           2,7,6,2,7,6,
     .           4,6,7,4,6,7/
C-------------------------------------------------------------------------

      IORD = 0
      ELEM = 0
      ELEM8 = 0
C-----------Search Element Tetra 10 containing the facet N1 N2 N3---------------

      DO I=KNOD2ELS(N1)+1,KNOD2ELS(N1+1)     
         IE = NOD2ELS(I)    
         IF(NUMELS8 < IE .AND. IE <= NUMELS8+NUMELS10)THEN 
           DO J=1,4
             DO k=1,3 
               IF(IXS(FACES(K,J),IE)==N1.AND.IXS(FACES(K+1,J),IE)==N2.AND.
     .                IXS(FACES(K+2,J),IE)==N3) THEN
                  IORD = 1   ! same order as defined in Faces
                  ELEM = IE
                  FACET = J
                  IE10=IE-NUMELS8
               ELSEIF(IXS(FACES(K,J),IE)==N1.AND.IXS(FACES(K+1,J),IE)==N3.AND.
     .                IXS(FACES(K+2,J),IE)==N2 ) THEN
                  IORD = -1  ! opposit order than defined in Faces
                  ELEM = IE
                  FACET = J
                  IE10=IE-NUMELS8
               ENDIF
             ENDDO
           ENDDO
         ENDIF
       ENDDO

C----------Facet divided into 4 facets if not degenrated---------------
c
      IF (ELEM /= 0) THEN       
C           still need to filter degenerated faces
        NNS=1
        DO J=1,3
          FCMID10(J) = IXS10(MIDNOD(J,FACET),IE10) 
          IF (FCMID10(J) /= 0) NNS=NNS+1 
        ENDDO 
C
        NSEG=NSEG+NNS
!
        IF (FLAG == 1) THEN
          DO K=1,3
            FACE(K)=IXS(FACES(K,FACET),ELEM)
          END DO
C
          IF (NNS==4) THEN  
c           4 triangles 
            IF (IORD == 1) THEN 
              ISEG = NSEG-NNS + 1
              CALL SEGSURF(FACE(1),FCMID10(1),FCMID10(3),FCMID10(3),NSEG0,
     .                     ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ISEG = NSEG-NNS + 2
              CALL SEGSURF(FCMID10(1),FACE(2),FCMID10(2),FCMID10(2),NSEG0,
     .                     ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ISEG = NSEG-NNS + 3
              CALL SEGSURF(FCMID10(1),FCMID10(2),FCMID10(3),FCMID10(3),NSEG0,
     .                     ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ISEG = NSEG-NNS + 4
              CALL SEGSURF(FCMID10(2),FACE(3),FCMID10(3),FCMID10(3),NSEG0,
     .                     ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
            ELSE
              ISEG = NSEG-NNS + 1
              CALL SEGSURF(FACE(1),FCMID10(3),FCMID10(1),FCMID10(1),NSEG0,
     .                     ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ISEG = NSEG-NNS + 2
              CALL SEGSURF(FCMID10(1),FCMID10(2),FACE(2),FACE(2),NSEG0,
     .                     ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ISEG = NSEG-NNS + 3
              CALL SEGSURF(FCMID10(1),FCMID10(3),FCMID10(2),FCMID10(2),NSEG0,
     .                     ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ISEG = NSEG-NNS + 4
              CALL SEGSURF(FCMID10(2),FCMID10(3),FACE(3),FACE(3),NSEG0,
     .                     ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
            ENDIF
          ELSEIF (NNS==3) THEN 
c               3 triangles 
            IF (IORD == 1 ) THEN        
              IF (FCMID10(1) == 0) THEN
                ISEG = NSEG-NNS + 1
                CALL SEGSURF(FACE(1),FACE(2),FCMID10(2),FCMID10(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 2
                CALL SEGSURF(FACE(1),FCMID10(2),FCMID10(3),FCMID10(3),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 3
                CALL SEGSURF(FCMID10(3),FCMID10(2),FACE(3),FACE(3),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ELSEIF (FCMID10(2) == 0) THEN
                ISEG = NSEG-NNS + 1
                CALL SEGSURF(FACE(1),FCMID10(1),FCMID10(3),FCMID10(3),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 2
                CALL SEGSURF(FCMID10(1),FACE(2),FCMID10(3),FCMID10(3),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 3
                CALL SEGSURF(FACE(2),FACE(3),FCMID10(3),FCMID10(3),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ELSEIF (FCMID10(3) == 0) THEN
                ISEG = NSEG-NNS + 1
                CALL SEGSURF(FCMID10(1),FACE(2),FCMID10(2),FCMID10(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 2
                CALL SEGSURF(FCMID10(1),FCMID10(2),FACE(1),FACE(1),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 3
                CALL SEGSURF(FACE(1),FCMID10(2),FACE(3),FACE(3),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ENDIF
            ELSE
              IF (FCMID10(1) == 0) THEN
                ISEG = NSEG-NNS + 1
                CALL SEGSURF(FACE(1),FCMID10(2),FACE(2),FACE(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 2
                CALL SEGSURF(FACE(1),FCMID10(3),FCMID10(2),FCMID10(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 3
                CALL SEGSURF(FCMID10(3),FACE(3),FCMID10(2),FCMID10(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ELSEIF (FCMID10(2) == 0) THEN
                ISEG = NSEG-NNS + 1
                CALL SEGSURF(FACE(1),FCMID10(3),FCMID10(1),FCMID10(1),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 2
                CALL SEGSURF(FCMID10(1),FCMID10(3),FACE(2),FACE(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 3
                CALL SEGSURF(FACE(2),FCMID10(3),FACE(3),FACE(3),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ELSEIF (FCMID10(3) == 0) THEN
                ISEG = NSEG-NNS + 1
                CALL SEGSURF(FCMID10(1),FCMID10(2),FACE(2),FACE(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 2
                CALL SEGSURF(FCMID10(1),FACE(1),FCMID10(2),FCMID10(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 3
                CALL SEGSURF(FACE(1),FACE(3),FCMID10(2),FCMID10(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ENDIF
            ENDIF            
          ELSEIF (NNS==2) THEN 
c               2 triangles 
            IF (IORD == 1 ) THEN        
              IF (FCMID10(1) /= 0) THEN
                ISEG = NSEG-NNS + 1
                CALL SEGSURF(FACE(1),FCMID10(1),FACE(3),FACE(3),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 2
                CALL SEGSURF(FCMID10(1),FACE(2),FACE(3),FACE(3),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ELSEIF (FCMID10(2) /= 0) THEN
                ISEG = NSEG-NNS + 1
                CALL SEGSURF(FACE(1),FACE(2),FCMID10(2),FCMID10(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 2
                CALL SEGSURF(FACE(1),FCMID10(2),FACE(3),FACE(3),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ELSEIF (FCMID10(3) /= 0) THEN
                ISEG = NSEG-NNS + 1
                CALL SEGSURF(FACE(1),FACE(2),FCMID10(3),FCMID10(3),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 2
                CALL SEGSURF(FCMID10(3),FACE(2),FACE(3),FACE(3),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ENDIF
            ELSE
              IF (FCMID10(1) /= 0) THEN
                ISEG = NSEG-NNS + 1
                CALL SEGSURF(FACE(1),FACE(3),FCMID10(1),FCMID10(1),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 2
                CALL SEGSURF(FCMID10(1),FACE(3),FACE(2),FACE(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ELSEIF (FCMID10(2) /= 0) THEN
                ISEG = NSEG-NNS + 1
                CALL SEGSURF(FACE(1),FCMID10(2),FACE(2),FACE(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 2
                CALL SEGSURF(FACE(1),FACE(3),FCMID10(2),FCMID10(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ELSEIF (FCMID10(3) /= 0) THEN
                ISEG = NSEG-NNS + 1
                CALL SEGSURF(FACE(1),FCMID10(3),FACE(2),FACE(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
                ISEG = NSEG-NNS + 2
                CALL SEGSURF(FCMID10(3),FACE(3),FACE(2),FACE(2),NSEG0,
     .                       ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
              ENDIF
            ENDIF 
          ELSEIF (NNS==1) THEN  
c               1 triangle 
            ISEG = NSEG-NNS + 1
            CALL SEGSURF(N1  ,N2     ,N3     ,N3,NSEG0,
     .                   ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,ELEM,1)
          ENDIF 
        ENDIF
      ELSE
        NNS = 1
        NSEG = NSEG + 1
        IF (FLAG==1) THEN
          ISEG = NSEG
C-------ELEM Hexa containing segment
          CALL SEGSURF(N1    ,N2     ,N3     ,N3,NSEG0,
     .                 ISEG   ,IGRSURF%NODES,IGRSURF%ELTYP,IGRSURF%ELEM,0,0)
        ENDIF
      ENDIF
C
      RETURN
      END

Chd|====================================================================
Chd|  SEGSURF                       source/groups/tsurftag.F      
Chd|-- called by -----------
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|        TSURFTAG                      source/groups/tsurftag.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SEGSURF(N1    ,N2        ,N3        ,N4       ,NSEG0,
     .                   ISEG  ,SURF_NODES,SURF_ELTYP,SURF_ELEM,ELEM,ELTY)
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
      INTEGER N1,N2,N3,N4,ISEG,NSEG0,ELEM,ELTY,
     .  SURF_NODES(NSEG0,4),SURF_ELTYP(NSEG0),SURF_ELEM(NSEG0)
C-----------------------------------------------
      SURF_NODES(ISEG,1) = N1
      SURF_NODES(ISEG,2) = N2
      SURF_NODES(ISEG,3) = N3
      SURF_NODES(ISEG,4) = N4
!
      SURF_ELTYP(ISEG) = ELTY
      SURF_ELEM(ISEG)  = ELEM
!---
      RETURN
      END

