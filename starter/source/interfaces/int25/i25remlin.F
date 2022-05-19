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
Chd|  I25REMLINE                    source/interfaces/int25/i25remlin.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        I11PENE_LIN                   source/interfaces/inter3d1/i11remlin.F
Chd|        ORIGIN                        source/model/remesh/build_admesh.F
Chd|====================================================================
      SUBROUTINE I25REMLINE(
     1         X    ,NEDGE ,LEDGE ,NUMNOD ,GAP_E    ,GAP_E_L,
     2         IGAP0,IGAP  ,DRAD  ,BGAPEMX,BGAPEMX_L,KREMNODE, 
     3         REMNODE,NREMNODE,I_START,I_MEM_REM,INOD2LIN,
     4         TAGSLAV,NOD2LIN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEDGE, NUMNOD,IGAP0,IGAP, NREMNODE
      INTEGER LEDGE(NLEDGE,*),KREMNODE(*),REMNODE(*),I_START,I_MEM_REM
      INTEGER INOD2LIN(NUMNOD+1),TAGSLAV(NUMNOD),NOD2LIN(2*NEDGE)
      my_real
     .        X(3,*),GAP_E(*),GAP_E_L(*),DRAD,BGAPEMX,BGAPEMX_L
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,LIN,ILIN,LEVEL,CPT,NBLIN,LIN1,L,CPT1,N,NBLIN_MAX,CPT_TOTAL
      INTEGER ITAG(NEDGE),
     .        LISTLIN(NEDGE),LISTLINTMP(NEDGE),LISTLINTOTAL(NEDGE),
     .        IM1,IM2
      INTEGER, DIMENSION(:),ALLOCATABLE ::
     .        KNOD2LIN,TAGNOD,ORIGIN
      my_real
     .        DMAX,NEW_DIST,PENE,I11PENE_LIN,XL,GAPV,GAP
      my_real, DIMENSION(:),ALLOCATABLE ::
     .        DIST1
C-----------------------------------------------
c Build inverse connectivity for segments - only at first pass (I_START=1)
C-----------------------------------------------
C
      IF (I_START ==1) THEN
C
        ALLOCATE(KNOD2LIN(NUMNOD+1))
C
        KREMNODE(1) = 1
        NOD2LIN(1:2*NEDGE)    = 0
        KNOD2LIN(1:NUMNOD+1) = 0
        INOD2LIN(1:NUMNOD+1) = 0
        TAGSLAV(1:NUMNOD)    = 0
        CPT = NEDGE
C
        DO I=1,NEDGE
          TAGSLAV(LEDGE(5,I)) = 1
          TAGSLAV(LEDGE(6,I)) = 1
        ENDDO


C-----------------------------------------------
C  Definition of node to segment connections
C-----------------------------------------------
C
         DO I=1,NEDGE
            N = LEDGE(5,I)
            KNOD2LIN(N) = KNOD2LIN(N) + 1
            N = LEDGE(6,I)
            KNOD2LIN(N) = KNOD2LIN(N) + 1
          END DO
C
          INOD2LIN(1) = 1
          DO I=1,NUMNOD
            INOD2LIN(I+1) = INOD2LIN(I) + KNOD2LIN(I)
          END DO
          KNOD2LIN(1:NUMNOD+1) = INOD2LIN(1:NUMNOD+1)
C
          DO I=1,NEDGE
            N = LEDGE(5,I)
            NOD2LIN(KNOD2LIN(N)) = I
            KNOD2LIN(N) = KNOD2LIN(N) + 1
            N = LEDGE(6,I)
            NOD2LIN(KNOD2LIN(N)) = I
            KNOD2LIN(N) = KNOD2LIN(N) + 1
          END DO
C
          DEALLOCATE(KNOD2LIN)
C
      ENDIF
C
C
      IF (I_START>=1) THEN
C
C-----------------------------------------------
C  Searching Algorith Connected nodes : D < SQRT(2.) * GAP
C-----------------------------------------------
C
        ALLOCATE(TAGNOD(NUMNOD),ORIGIN(NUMNOD),DIST1(NUMNOD))
        TAGNOD(1:NUMNOD) = 0
        ORIGIN(1:NUMNOD)     = 0
        DIST1(1:NUMNOD) = EP30
        ITAG(1:NEDGE) = 0
        LISTLIN(1:NEDGE) = 0
        LISTLINTMP(1:NEDGE)=0
        LISTLINTOTAL(1:NEDGE) = 0
        CPT_TOTAL = 0

C
        DO I=I_START,NEDGE

          LEVEL = 1
          LIN = I

          ITAG(LIN) = LEVEL
          LISTLIN(1)=LIN
          NBLIN=1
          NBLIN_MAX=1
          CPT = 0
          CPT_TOTAL = 0
          XL = (X(1,LEDGE(5,I))-X(1,LEDGE(6,I)))**2+(X(2,LEDGE(5,I))-X(2,LEDGE(6,I)))**2+(X(3,LEDGE(5,I))-X(3,LEDGE(6,I)))**2
          XL = SQRT(XL)
C

          IF(IGAP0 == 0) THEN
            GAP = BGAPEMX+DEUX*GAP_E(I)
          ELSE
             GAP = DEUX*(BGAPEMX+GAP_E(I))
          ENDIF
          IF(IGAP==3) GAP = MIN(GAP,GAP_E_L(I)+BGAPEMX_L)
 
          DMAX = SQRT(DEUX) * MAX(GAP,DRAD)
C
          TAGNOD(LEDGE(5,LIN)) = 1
          DIST1(LEDGE(5,LIN)) = ZERO

          TAGNOD(LEDGE(6,LIN)) = 1
          DIST1(LEDGE(6,LIN)) = ZERO
C
          DO WHILE (NBLIN/=0)
C
            LEVEL = LEVEL+1
            CPT = 0
            DO ILIN=1,NBLIN
              LIN=LISTLIN(ILIN)

              TAGNOD(LEDGE(5,LIN)) = 2
              TAGNOD(LEDGE(6,LIN)) = 2
C
              PENE = ZERO
              IF ((DIST1(LEDGE(5,LIN)) > DMAX).AND.(DIST1(LEDGE(6,LIN)) > DMAX).AND.(LEVEL>2)) THEN
                PENE = I11PENE_LIN(X,LEDGE(5,LIN),LEDGE(5,LIN),LEDGE(5,I),LEDGE(6,I),DMAX,DRAD)
              ENDIF
C
              IF ((LEVEL <= 2).OR.(DIST1(LEDGE(5,LIN)) <= DMAX).OR.(DIST1(LEDGE(6,LIN)) <= DMAX).OR.(PENE > ZERO)) THEN
              DO J=5,6
                DO K=INOD2LIN(LEDGE(J,LIN)),INOD2LIN(LEDGE(J,LIN)+1)-1
                  LIN1 = NOD2LIN(K)
                  IF( (ITAG(LIN1) == 0  .OR. ITAG(LIN1) == LEVEL)) THEN
                    IF(ITAG(LIN1) == 0)THEN
                      CPT = CPT + 1
                      LISTLINTMP(CPT)=LIN1
                    ENDIF
                    ITAG(LIN1)=LEVEL
                    DO L=5,6

                      IF ((TAGSLAV(LEDGE(L,LIN1))== 1).AND.(ORIGIN(LEDGE(L,LIN1)) /= LEDGE(J,LIN))
     .                     .AND.((LEDGE(L,LIN1)) /= LEDGE(J,LIN)).AND.(TAGNOD(LEDGE(L,LIN1)) /= 2)) THEN
C
                        NEW_DIST=DIST1(LEDGE(J,LIN))+
     .                  SQRT((X(1,LEDGE(L,LIN1))-X(1,LEDGE(J,LIN)))**2 + 
     .                       (X(2,LEDGE(L,LIN1)) - X(2,LEDGE(J,LIN)))**2 +
     .                       (X(3,LEDGE(L,LIN1)) - X(3,LEDGE(J,LIN)))**2 )
C
                        IF (NEW_DIST < DIST1(LEDGE(L,LIN1))) THEN
                          DIST1(LEDGE(L,LIN1)) = NEW_DIST
                        ENDIF
C
                        IF(TAGNOD(LEDGE(L,LIN1))==0) THEN
                          TAGNOD(LEDGE(L,LIN1)) = 1
                        ENDIF                     
C
                      ENDIF
                    ENDDO 
                  ENDIF                  
                ENDDO   
              ENDDO
              ENDIF
C
              TAGNOD(LEDGE(5:6,LIN))=1
            ENDDO
C
            NBLIN = CPT
C 
            NBLIN_MAX = MAX(NBLIN_MAX,NBLIN)
            IF(NBLIN ==0)EXIT
            DO J=1,CPT
              LISTLIN(J)=LISTLINTMP(J)
              LISTLINTMP(J) = 0
              LISTLINTOTAL(J+CPT_TOTAL) = LISTLIN(J)
            ENDDO
            CPT_TOTAL = CPT_TOTAL + CPT
C
C----------------
          ENDDO
C
CC END DO WHILE
C
C-- Check memory for data storage
C
          I_START = I
          IF (KREMNODE(I)+CPT_TOTAL > NREMNODE) THEN
C--         Not enough memory - upgrade_remnode
            I_MEM_REM = 1
            EXIT
          ENDIF
C
          CPT1 = 0
          IM1 = LEDGE(5,I)
          IM2 = LEDGE(6,I)
C

            DO L=1,CPT_TOTAL
              LIN = LISTLINTOTAL(L)
              IF ((IM1 /= LEDGE(5,LIN)).AND.(IM1 /= LEDGE(6,LIN))
     .           .AND.(IM2 /= LEDGE(5,LIN)).AND.(IM2 /= LEDGE(6,LIN))) THEN
C--- lines with common nodes with master lines are aready removed - no need to store them in remnode
                IF(IGAP0 == 0) THEN
                   GAPV = GAP_E(LIN)+DEUX*GAP_E(I)
                ELSE
                   GAPV = DEUX*(GAP_E(LIN)+GAP_E(I))
                ENDIF
                IF(IGAP==3) GAPV = MIN(GAPV,GAP_E_L(LIN)+GAP_E_L(I))

                GAPV = SQRT(DEUX)*MAX(DRAD,GAPV)
                IF ((DIST1(LEDGE(5,LIN)) <= GAPV).OR.(DIST1(LEDGE(6,LIN)) <= GAPV)) THEN
                  REMNODE(KREMNODE(I)+CPT1) = LIN
                  CPT1 = CPT1 + 1
                ELSE
                  PENE = I11PENE_LIN(X,LEDGE(5,LIN),LEDGE(6,LIN),LEDGE(5,I),LEDGE(6,I),GAPV,DRAD)
                  IF (PENE > 0) THEN
                    REMNODE(KREMNODE(I)+CPT1) = LIN
                    CPT1 = CPT1 + 1
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
            KREMNODE(I+1) = KREMNODE(I) + CPT1

C
C-----------------------------------------------
C  Clean of used arrays
C-----------------------------------------------
C
          DIST1(LEDGE(5,I)) = EP30
          DIST1(LEDGE(6,I)) = EP30
          ORIGIN(LEDGE(5,I)) = 0
          ORIGIN(LEDGE(6,I)) = 0
          TAGNOD(LEDGE(5,I)) = 0
          TAGNOD(LEDGE(6,I)) = 0
          ITAG(I) = 0
C
          DO L=1,CPT_TOTAL
            LIN = LISTLINTOTAL(L)
            ITAG(LIN) = 0
            LISTLINTOTAL(L) = 0
            TAGNOD(LEDGE(5,LIN)) = 0
            TAGNOD(LEDGE(6,LIN)) = 0
            DIST1(LEDGE(5,LIN)) = EP30
            DIST1(LEDGE(6,LIN)) = EP30
            ORIGIN(LEDGE(5,LIN)) = 0
            ORIGIN(LEDGE(6,LIN)) = 0
          ENDDO
          LISTLINTMP(1:NBLIN_MAX)=0
          LISTLIN(1:NBLIN_MAX)=0
C
        ENDDO
CC END DO NEDGE
C
      ENDIF
C
      DEALLOCATE(DIST1,TAGNOD,ORIGIN)
C
      RETURN
      END
