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
Chd|  I11REMLINE                    source/interfaces/inter3d1/i11remlin.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        I11PENE_LIN                   source/interfaces/inter3d1/i11remlin.F
Chd|        ORIGIN                        source/model/remesh/build_admesh.F
Chd|====================================================================
      SUBROUTINE I11REMLINE(
     1         X,NRTM,IRECTM,NRTS,IRECTS,
     2         NUMNOD,GAP_S ,GAP_M, GAPMIN,IGAP,
     3         KREMNODE,REMNODE,GAP,DRAD,NREMNODE,
     4         I_START,I_MEM_REM,INOD2LIN,TAGSECND,NOD2LIN,
     5         DGAPLOAD)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,NRTS, NUMNOD,IGAP, NREMNODE
      INTEGER IRECTM(2,*),IRECTS(2,*),KREMNODE(*),REMNODE(*),I_START,I_MEM_REM
      INTEGER INOD2LIN(NUMNOD+1),TAGSECND(NUMNOD),NOD2LIN(2*NRTM)
      my_real
     .        X(3,*),GAP_S(*),GAP_M(*),GAP,DRAD,GAPMIN
      my_real , INTENT(IN) :: DGAPLOAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,LIN,ILIN,LEVEL,CPT,NBLIN,LIN1,L,CPT1,N,NBLIN_MAX,CPT_TOTAL
      INTEGER ITAG(NRTM),
     .        LISTLIN(NRTM),LISTLINTMP(NRTM),LISTLINTOTAL(NRTM),
     .        IM1,IM2
      INTEGER, DIMENSION(:),ALLOCATABLE ::
     .        KNOD2LIN,TAGNOD,ORIGIN
      my_real
     .        DMAX,NEW_DIST,PENE,I11PENE_LIN,XL,GAPV
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
        NOD2LIN(1:2*NRTM)    = 0
        KNOD2LIN(1:NUMNOD+1) = 0
        INOD2LIN(1:NUMNOD+1) = 0
        TAGSECND(1:NUMNOD)    = 0
        CPT = 0
C
        DO I=1,NRTS
          TAGSECND(IRECTS(1,I)) = 1
          TAGSECND(IRECTS(2,I)) = 1
        ENDDO
C
        DO I=1,NRTM
          DO J=1,2
            IF( TAGSECND(IRECTM(J,I)) == 1 ) CPT = CPT + 1
          ENDDO
        ENDDO
C
        IF (CPT == 0) THEN
C--
          DO I=1,NRTM
            KREMNODE(I+1) = 0
          ENDDO
C
        ELSE
C-----------------------------------------------
C  Definition of node to segment connections
C-----------------------------------------------
C
          DO I=1,NRTM
            CPT = 0
            DO K=1,2
              IF(TAGSECND(IRECTM(K,I)) == 1) CPT = CPT + 1
            END DO
            IF (CPT /= 0 ) THEN
              DO K=1,2
                N = IRECTM(K,I)
                KNOD2LIN(N) = KNOD2LIN(N) + 1
              END DO
            ENDIF
          END DO
C
          INOD2LIN(1) = 1
          DO I=1,NUMNOD
            INOD2LIN(I+1) = INOD2LIN(I) + KNOD2LIN(I)
          END DO
          KNOD2LIN(1:NUMNOD+1) = INOD2LIN(1:NUMNOD+1)
C
          DO I=1,NRTM
            CPT = 0
            DO K=1,2
              IF(TAGSECND(IRECTM(K,I)) == 1) CPT = CPT + 1
            END DO
            IF (CPT /= 0) THEN
              DO K=1,2
                N = IRECTM(K,I)
                NOD2LIN(KNOD2LIN(N)) = I
                KNOD2LIN(N) = KNOD2LIN(N) + 1
              END DO
            ENDIF
          END DO
C
          DEALLOCATE(KNOD2LIN)
C
        ENDIF
C
      ENDIF
C
      IF (((I_START==1).AND.(CPT > 0)).OR.(I_START>1)) THEN
C
C-----------------------------------------------
C  Searching Algorithm Connected nodes : D < SQRT(2.) * GAP
C-----------------------------------------------
C
        ALLOCATE(TAGNOD(NUMNOD),ORIGIN(NUMNOD),DIST1(NUMNOD))
        TAGNOD(1:NUMNOD) = 0
        ORIGIN(1:NUMNOD)     = 0
        DIST1(1:NUMNOD) = EP30
        ITAG(1:NRTM) = 0
        LISTLIN(1:NRTM) = 0
        LISTLINTMP(1:NRTM)=0
        LISTLINTOTAL(1:NRTM) = 0
        CPT_TOTAL = 0
C
        DMAX = SQRT(TWO) * MAX(GAP+DGAPLOAD,DRAD)
C
        DO I=I_START,NRTM

          LEVEL = 1
          LIN = I
C          IF ((ITAB(IRECTM(1,LIN))/=30151).OR.(ITAB(IRECTM(2,LIN))/=30197)) CYCLE
          ITAG(LIN) = LEVEL
          LISTLIN(1)=LIN
          NBLIN=1
          NBLIN_MAX=1
          CPT = 0
          CPT_TOTAL = 0
          XL = (X(1,IRECTM(1,I))-X(1,IRECTM(2,I)))**2+(X(2,IRECTM(1,I))-X(2,IRECTM(2,I)))**2+(X(3,IRECTM(1,I))-X(3,IRECTM(2,I)))**2
          XL = SQRT(XL)
C
          DO J=1,2
            TAGNOD(IRECTM(J,LIN)) = 1
            DIST1(IRECTM(J,LIN)) = ZERO
          ENDDO
C
          DO WHILE (NBLIN/=0)
C
            LEVEL = LEVEL+1
            CPT = 0
            DO ILIN=1,NBLIN
              LIN=LISTLIN(ILIN)
              TAGNOD(IRECTM(1:2,LIN))=2
C
C              ESTA = (DIST1(IRECTM(2,LIN))*DIST1(IRECTM(2,LIN))-DIST1(IRECTM(1,LIN))*DIST1(IRECTM(1,LIN))-XL*XL)
C     .               /(TWO*XL*DIST1(IRECTM(1,LIN)))
C              DIST_AXIS = DIST1(IRECTM(1,LIN))*SQRT(ONE-ESTA*ESTA)
C
              PENE = ZERO
              IF ((DIST1(IRECTM(1,LIN)) > DMAX).AND.(DIST1(IRECTM(2,LIN)) > DMAX).AND.(LEVEL>2)) THEN
                PENE = I11PENE_LIN(X,IRECTM(1,LIN),IRECTM(2,LIN),IRECTM(1,I),IRECTM(2,I),DMAX)
              ENDIF
C
              IF ((LEVEL <= 2).OR.(DIST1(IRECTM(1,LIN)) <= DMAX).OR.(DIST1(IRECTM(2,LIN)) <= DMAX).OR.(PENE > ZERO)) THEN
              DO J=1,2
                DO K=INOD2LIN(IRECTM(J,LIN)),INOD2LIN(IRECTM(J,LIN)+1)-1
                  LIN1 = NOD2LIN(K)
                  IF( (ITAG(LIN1) == 0  .OR. ITAG(LIN1) == LEVEL)) THEN
                    IF(ITAG(LIN1) == 0)THEN
                      CPT = CPT + 1
                      LISTLINTMP(CPT)=LIN1
                    ENDIF
                    ITAG(LIN1)=LEVEL
                    DO L=1,2

                      IF ((TAGSECND(IRECTM(L,LIN1))== 1).AND.(ORIGIN(IRECTM(L,LIN1)) /= IRECTM(J,LIN))
     .                     .AND.((IRECTM(L,LIN1)) /= IRECTM(J,LIN)).AND.(TAGNOD(IRECTM(L,LIN1)) /= 2)) THEN
C
                        NEW_DIST=DIST1(IRECTM(J,LIN))+
     .                  SQRT((X(1,IRECTM(L,LIN1))-X(1,IRECTM(J,LIN)))**2 + 
     .                       (X(2,IRECTM(L,LIN1)) - X(2,IRECTM(J,LIN)))**2 +
     .                       (X(3,IRECTM(L,LIN1)) - X(3,IRECTM(J,LIN)))**2 )
C
                        IF (NEW_DIST < DIST1(IRECTM(L,LIN1))) THEN
                          DIST1(IRECTM(L,LIN1)) = NEW_DIST
                        ENDIF
C
                        IF(TAGNOD(IRECTM(L,LIN1))==0) THEN
                          TAGNOD(IRECTM(L,LIN1)) = 1
                        ENDIF                     
C
                      ENDIF
                    ENDDO 
                  ENDIF                  
                ENDDO   
              ENDDO
              ENDIF
C
              TAGNOD(IRECTM(1:2,LIN))=1
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
          IM1 = IRECTM(1,I)
          IM2 = IRECTM(2,I)
C
          IF (IGAP == 0) THEN 
            DO L=1,CPT_TOTAL
              LIN = LISTLINTOTAL(L)
              IF ((IM1 /= IRECTM(1,LIN)).AND.(IM1 /= IRECTM(2,LIN))
     .           .AND.(IM2 /= IRECTM(1,LIN)).AND.(IM2 /= IRECTM(2,LIN))) THEN
C--- lines with common nodes with main lines are already removed - no need to store them in remnode
                IF ((DIST1(IRECTM(1,LIN)) <= DMAX).OR.(DIST1(IRECTM(2,LIN)) <= DMAX)) THEN
                  REMNODE(KREMNODE(I)+CPT1) = LIN
                  CPT1 = CPT1 + 1
                ELSE
                  PENE = I11PENE_LIN(X,IRECTM(1,LIN),IRECTM(2,LIN),IRECTM(1,I),IRECTM(2,I),SQRT(TWO)*MAX(GAP+DGAPLOAD,DRAD))
                  IF (PENE > 0) THEN
                    REMNODE(KREMNODE(I)+CPT1) = LIN
                    CPT1 = CPT1 + 1
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
            KREMNODE(I+1) = KREMNODE(I) + CPT1
          ELSEIF (IGAP == 1) THEN 
            DO L=1,CPT_TOTAL
              LIN = LISTLINTOTAL(L)
              IF ((IM1 /= IRECTM(1,LIN)).AND.(IM1 /= IRECTM(2,LIN))
     .           .AND.(IM2 /= IRECTM(1,LIN)).AND.(IM2 /= IRECTM(2,LIN))) THEN
C--- lines with common nodes with main lines are already removed - no need to store them in remnode
                GAPV = GAP_S(LIN)+GAP_M(I)
                GAPV = SQRT(TWO)*MAX(DRAD,GAPMIN,GAPV+DGAPLOAD)
                IF ((DIST1(IRECTM(1,LIN)) <= GAPV).OR.(DIST1(IRECTM(2,LIN)) <= GAPV)) THEN
                  REMNODE(KREMNODE(I)+CPT1) = LIN
                  CPT1 = CPT1 + 1
                ELSE
                  PENE = I11PENE_LIN(X,IRECTM(1,LIN),IRECTM(2,LIN),IRECTM(1,I),IRECTM(2,I),GAPV)
                  IF (PENE > 0) THEN
                    REMNODE(KREMNODE(I)+CPT1) = LIN
                    CPT1 = CPT1 + 1
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
            KREMNODE(I+1) = KREMNODE(I) + CPT1
          ENDIF
C
C-----------------------------------------------
C  Clean of used arrays
C-----------------------------------------------
C
          DIST1(IRECTM(1,I)) = EP30
          DIST1(IRECTM(2,I)) = EP30
          ORIGIN(IRECTM(1,I)) = 0
          ORIGIN(IRECTM(2,I)) = 0
          TAGNOD(IRECTM(1,I)) = 0
          TAGNOD(IRECTM(2,I)) = 0
          ITAG(I) = 0
C
          DO L=1,CPT_TOTAL
            LIN = LISTLINTOTAL(L)
            ITAG(LIN) = 0
            LISTLINTOTAL(L) = 0
            TAGNOD(IRECTM(1,LIN)) = 0
            TAGNOD(IRECTM(2,LIN)) = 0
            DIST1(IRECTM(1,LIN)) = EP30
            DIST1(IRECTM(2,LIN)) = EP30
            ORIGIN(IRECTM(1,LIN)) = 0
            ORIGIN(IRECTM(2,LIN)) = 0
          ENDDO
          LISTLINTMP(1:NBLIN_MAX)=0
          LISTLIN(1:NBLIN_MAX)=0
C
        ENDDO
CC END DO NRTM
C
      ENDIF
C
      DEALLOCATE(DIST1,TAGNOD,ORIGIN)
C
      RETURN
      END
C
C=======================================================================
Chd|====================================================================
Chd|  I11PENE_LIN                   source/interfaces/inter3d1/i11remlin.F
Chd|-- called by -----------
Chd|        I11REMLINE                    source/interfaces/inter3d1/i11remlin.F
Chd|        I25REMLINE                    source/interfaces/int25/i25remlin.F
Chd|-- calls ---------------
Chd|====================================================================
      my_real FUNCTION I11PENE_LIN(X,N1,N2,M1,M2,GAP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,N2,M1,M2
      my_real
     .     X(3,*),GAP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .     XS12,YS12,ZS12,XM12,YM12,ZM12,XA,XB,
     .     XS2,XM2,XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .     XX,YY,ZZ,ALS,ALM,DET,H1S,H2S,H1M,H2M,NX,NY,NZ,GAP2
C-----------------------------------------------
C
C      COMPTUTATION OF PENE*PENE FOR PENETRATION CHECK
C
       GAP2 = GAP**2
C
       XS12 = X(1,N2)-X(1,N1)
       YS12 = X(2,N2)-X(2,N1)
       ZS12 = X(3,N2)-X(3,N1)
       XS2 = XS12*XS12 + YS12*YS12 + ZS12*ZS12
       XM12 = X(1,M2)-X(1,M1)
       YM12 = X(2,M2)-X(2,M1)
       ZM12 = X(3,M2)-X(3,M1)
       XM2 = XM12*XM12 + YM12*YM12 + ZM12*ZM12
       XSM = - (XS12*XM12 + YS12*YM12 + ZS12*ZM12)
       XS2M2 = X(1,M2)-X(1,N2)
       YS2M2 = X(2,M2)-X(2,N2)
       ZS2M2 = X(3,M2)-X(3,N2)
       XA =  XS12*XS2M2 + YS12*YS2M2 + ZS12*ZS2M2
       XB = -XM12*XS2M2 - YM12*YS2M2 - ZM12*ZS2M2 
       DET = XM2*XS2 - XSM*XSM
       DET = MAX(EM20,DET)
C
       H1M = (XA*XSM-XB*XS2) / DET
C
       XS2 = MAX(XS2,EM20)
       XM2 = MAX(XM2,EM20)
       H1M=MIN(ONE,MAX(ZERO,H1M))
       H1S = -(XA + H1M*XSM) / XS2
       H1S=MIN(ONE,MAX(ZERO,H1S))
       H1M = -(XB + H1S*XSM) / XM2
       H1M=MIN(ONE,MAX(ZERO,H1M))
C
       H2S = ONE - H1S
       H2M = ONE - H1M
C !!!!!!!!!!!!!!!!!!!!!!!
C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL
C!!!!!!!!!!!!!!!!!!!!!!!!
       NX =  H1S*X(1,N1) + H2S*X(1,N2)
     .      - H1M*X(1,M1) - H2M*X(1,M2)
       NY =  H1S*X(2,N1) + H2S*X(2,N2)
     .      - H1M*X(2,M1) - H2M*X(2,M2)
       NZ =  H1S*X(3,N1) + H2S*X(3,N2)
     .      - H1M*X(3,M1) - H2M*X(3,M2)
       I11PENE_LIN = GAP2 - NX*NX - NY*NY - NZ*NZ
       I11PENE_LIN = MAX(ZERO,I11PENE_LIN)
C
       RETURN
       END
