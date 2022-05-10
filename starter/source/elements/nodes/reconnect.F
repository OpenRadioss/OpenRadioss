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
Chd|  RECONNECT                     source/elements/nodes/reconnect.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE RECONNECT(
     .           IXS      ,IXS10    ,IXS20    ,IXS16    ,IXQ      ,
     .           IXC      ,IXT      ,IXP      ,IXR      ,IXTG     ,
     .           IXUR     ,IGRNOD   ,IGRSURF  ,IGRSLIN  ,
     .           ISKN     ,IMERGE   ,NMERGE_TOT)
C--------------------------------------------------------
C     RENITIALISE ELEMENT CONNECTIVITIES AFTER NODE MERGE
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
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "rnur_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXS10(6,*),IXS16(8,*),IXS20(12,*),IXQ(NIXQ,*),
     .   IXC(NIXC,*),IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IXTG(NIXTG,*),
     .   IXUR(6,*),ISKN(LISKN,*),IMERGE(*),NMERGE_TOT
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,IAD,IPT,ITYP,NN,TAGNO(NUMNOD),NM
C=======================================================================
C-----------------------------------
C     ELEMENTS
C-----------------------------------
C
C---  Solids
C
      DO I = 1, NUMELS
        DO J=2,9
          DO K = 1,NMERGED
            IF (IXS(J,I) == IMERGE(K)) IXS(J,I) = IMERGE(NMERGE_TOT+K)
          ENDDO
        ENDDO
      ENDDO
C
      DO I = 1, NUMELS10
        DO J=1,6
          IF(IXS10(J,I).NE.0)THEN
            DO K = 1,NMERGED
              IF(IXS10(J,I) == IMERGE(K)) IXS10(J,I) = IMERGE(NMERGE_TOT+K)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
      DO I = 1, NUMELS20
        DO J=1,12
          IF(IXS20(J,I).NE.0)THEN
            DO K = 1,NMERGED
              IF(IXS20(J,I) == IMERGE(K)) IXS20(J,I) = IMERGE(NMERGE_TOT+K)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
      DO I = 1, NUMELS16
        DO J=1,8
          IF(IXS16(J,I).NE.0)THEN
            DO K = 1,NMERGED
              IF(IXS16(J,I) == IMERGE(K)) IXS16(J,I) = IMERGE(NMERGE_TOT+K)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
C---  Quads
C
C
      DO I = 1, NUMELQ
        DO J=2,5
          DO K = 1,NMERGED
            IF (IXQ(J,I) == IMERGE(K)) IXQ(J,I) = IMERGE(NMERGE_TOT+K)
          ENDDO
        ENDDO
      ENDDO
C
C---  Shells
C
      DO I = 1, NUMELC
        DO J=2,5
          DO K = 1,NMERGED
            IF (IXC(J,I) == IMERGE(K)) IXC(J,I) = IMERGE(NMERGE_TOT+K)
          ENDDO
        ENDDO
      ENDDO
C
C---  Truss
C
      DO I = 1, NUMELT
        DO J=2,3
          DO K = 1,NMERGED
            IF (IXT(J,I) == IMERGE(K)) IXT(J,I) = IMERGE(NMERGE_TOT+K)
          ENDDO
        ENDDO
      ENDDO
C
C---  Beams
C
      DO I = 1, NUMELP
        DO J=2,4
          DO K = 1,NMERGED
            IF (IXP(J,I) == IMERGE(K)) IXP(J,I) = IMERGE(NMERGE_TOT+K)
          ENDDO
        ENDDO
      ENDDO
C
C---  Springs
C
      DO I = 1, NUMELR
        DO J=2,4
          IF (IXR(J,I) /= 0) THEN
            DO K = 1,NMERGED
              IF (IXR(J,I) == IMERGE(K)) IXR(J,I) = IMERGE(NMERGE_TOT+K)
            ENDDO
          END IF
        ENDDO
      ENDDO
C
C---  Triangle Shells
C
      DO I = 1, NUMELTG
        DO J=2,4
          DO K = 1,NMERGED
            IF (IXTG(J,I) == IMERGE(K)) IXTG(J,I) = IMERGE(NMERGE_TOT+K)
          ENDDO
        ENDDO
      ENDDO
C
C---  RNUR elements
C
      DO I = 1, NUMELUR
        DO J=2,5
          DO K = 1,NMERGED
            IF (IXUR(J,I) == IMERGE(K)) IXUR(J,I) = IMERGE(NMERGE_TOT+K)
          ENDDO
        ENDDO
      ENDDO
C-----------------------------------
C     GROUPS
C-----------------------------------
C
C---  Surfaces
C
      DO I=1,NSURF
        NN   = IGRSURF(I)%NSEG
        DO J=1,NN
          DO L=1,4
            DO K = 1,NMERGED
              IF (IGRSURF(I)%NODES(J,L) == IMERGE(K))
     .            IGRSURF(I)%NODES(J,L) =  IMERGE(NMERGE_TOT+K)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C---  Lines
C
      DO I=1,NSLIN
        NN  = IGRSLIN(I)%NSEG
        DO J=1,NN
          DO L=1,2
            DO K = 1,NMERGED
              IF (IGRSLIN(I)%NODES(J,L) == IMERGE(K))
     .            IGRSLIN(I)%NODES(J,L) =  IMERGE(NMERGE_TOT+K)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C---  Node groups
C
      DO I=1,NGRNOD
        ITYP = IGRNOD(I)%GRTYPE
        TAGNO(1:NUMNOD)=0
        IF (ITYP == 0) THEN
          NN  = IGRNOD(I)%NENTITY
C--------- Pass 1        
          DO J=1,NN
            TAGNO(IGRNOD(I)%ENTITY(J)) = 1           
          ENDDO
C--------- Pass 2
          NM = 0        
          DO J=1,NN
            DO K = 1,NMERGED
              IF (IGRNOD(I)%ENTITY(J) == IMERGE(K)) THEN
                IF (TAGNO(IMERGE(NMERGE_TOT+K)) == 0) THEN
C--               node is replaced
                  IGRNOD(I)%ENTITY(J) =  IMERGE(NMERGE_TOT+K)
                  TAGNO(IMERGE(NMERGE_TOT+K)) = 1
                ELSE
C--               node is removed
                  NM = NM + 1
                  TAGNO(IGRNOD(I)%ENTITY(J)) = -1 
                ENDIF
              ENDIF
            ENDDO
          ENDDO
C--------- Pass 3  
          IF (NM > 0) THEN
            L = 0      
            DO J=1,NN
              IF (TAGNO(IGRNOD(I)%ENTITY(J)) /= -1) THEN
                L = L + 1
                IGRNOD(I)%ENTITY(L) = IGRNOD(I)%ENTITY(J)
              ENDIF         
            ENDDO
            IGRNOD(I)%NENTITY = IGRNOD(I)%NENTITY - NM
          ENDIF
C--
        ENDIF
      ENDDO
C-----------------------------------
C     KINEMATIC CONDITIONS
C-----------------------------------
C
C---  Skews
C
      DO I=2,NUMSKW
        DO J=1,3
          DO K = 1,NMERGED
            IF (ISKN(J,I) == IMERGE(K)) ISKN(J,I) = IMERGE(NMERGE_TOT+K)
          ENDDO
        ENDDO
      ENDDO
C-----------------------------------
      RETURN
      END SUBROUTINE RECONNECT
