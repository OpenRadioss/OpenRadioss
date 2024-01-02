Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
     4         TAGSECND,NOD2LIN,DGAPLOAD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEDGE, NUMNOD,IGAP0,IGAP, NREMNODE
      INTEGER LEDGE(NLEDGE,*),KREMNODE(*),REMNODE(*),I_START,I_MEM_REM
      INTEGER INOD2LIN(NUMNOD+1),TAGSECND(NUMNOD),NOD2LIN(2*NEDGE)
      my_real
     .        X(3,*),GAP_E(*),GAP_E_L(*),DRAD,BGAPEMX,BGAPEMX_L
      my_real , INTENT(IN) :: DGAPLOAD
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
     .        DMAX,NEW_DIST,PENE,I11PENE_LIN,XL,GAPV,GAP,DRAD2
      my_real, DIMENSION(:),ALLOCATABLE ::
     .        DIST1
C-----------------------------------------------
c Build inverse connectivity for segments - only at first pass (I_START=1)
C-----------------------------------------------
C
       DRAD2 = ZERO !no thermal exchange for E2E 
      IF (I_START ==1) THEN
C
        ALLOCATE(KNOD2LIN(NUMNOD+1))
C
        KREMNODE(1) = 1
        NOD2LIN(1:2*NEDGE)    = 0
        KNOD2LIN(1:NUMNOD+1) = 0
        INOD2LIN(1:NUMNOD+1) = 0
        TAGSECND(1:NUMNOD)    = 0
        CPT = NEDGE
C
        DO I=1,NEDGE
          TAGSECND(LEDGE(5,I)) = 1
          TAGSECND(LEDGE(6,I)) = 1
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
C  Searching Algorithm Connected nodes : D < SQRT(2.) * GAP
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
            GAP = BGAPEMX+TWO*GAP_E(I)
          ELSE
             GAP = TWO*(BGAPEMX+GAP_E(I))
          ENDIF
          IF(IGAP==3) GAP = MIN(GAP,GAP_E_L(I)+BGAPEMX_L)
 
          DMAX = SQRT(TWO) * MAX(GAP+DGAPLOAD,DRAD2)
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
                PENE = I11PENE_LIN(X,LEDGE(5,LIN),LEDGE(5,LIN),LEDGE(5,I),LEDGE(6,I),DMAX)
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

                      IF ((TAGSECND(LEDGE(L,LIN1))== 1).AND.(ORIGIN(LEDGE(L,LIN1)) /= LEDGE(J,LIN))
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
C--- lines with common nodes with main lines are already removed - no need to store them in remnode
                IF(IGAP0 == 0) THEN
                   GAPV = GAP_E(LIN)+TWO*GAP_E(I)
                ELSE
                   GAPV = TWO*(GAP_E(LIN)+GAP_E(I))
                ENDIF
                IF(IGAP==3) GAPV = MIN(GAPV,GAP_E_L(LIN)+GAP_E_L(I))

                GAPV = SQRT(TWO)*MAX(DRAD2,GAPV+DGAPLOAD)

                IF ((DIST1(LEDGE(5,LIN)) <= GAPV).OR.(DIST1(LEDGE(6,LIN)) <= GAPV)) THEN
                  REMNODE(KREMNODE(I)+CPT1) = LIN
                  CPT1 = CPT1 + 1
                ELSE
                  PENE = I11PENE_LIN(X,LEDGE(5,LIN),LEDGE(6,LIN),LEDGE(5,I),LEDGE(6,I),GAPV)
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


Chd|====================================================================
Chd|  REMN_I2OP_EDG25               source/interfaces/int25/i25remlin.F
Chd|-- called by -----------
Chd|        REMN_I2OP                     source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        UPGRADE_REMNODE_E2S           source/interfaces/interf1/upgrade_remnode.F
Chd|        UPGRADE_REMNODE_EDG2          source/interfaces/interf1/upgrade_remnode.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE REMN_I2OP_EDG25(N         ,FLAGREMNODE, IPARI   ,INTBUF_TAB   ,I2NODE  ,
     .                          POINTS_I2N ,I2NODE_SIZE, NOM_OPT ,ITAB,FLAG_OUTPUT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: N , FLAGREMNODE, I2NODE_SIZE
      INTEGER , INTENT(INOUT) :: IPARI(NPARI,NINTER)
      INTEGER , INTENT(IN) :: I2NODE(I2NODE_SIZE,3),POINTS_I2N(NUMNOD,2)
      INTEGER , INTENT(IN) :: NOM_OPT(LNOPT1,NINTER),ITAB(NUMNOD)
      INTEGER, INTENT(in) :: FLAG_OUTPUT !< flag to print the message/warning

      TYPE(INTBUF_STRUCT_) , INTENT(INOUT) :: INTBUF_TAB(NINTER)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,J,K,IE,NN, NM,N2,ND,NES,NM2,M,
     .       NN2,NNOD,NNREM_EDG,KI,KL,JJ,IEDG,IEDGS,ES,
     .       COMPTEUR,I,L,L1,IS,IIS,NS,IADA,III,JJJ,NNOD_2,
     .       FIRST,LAST,NNREM_EDG_SAVE,
     .        OFFSET, NBR_INTRA,NBR_EXTRA,TOTAL_INSERTED,
     .        OLDSIZE,MAX_INSERTED_EDG,
     .        NREMOV_EDG,NEDGE,MAX_INSERTED_I2,ND_TAG,
     .        SOL_EDGE,SH_EDGE,IEDGE,NRTM
       INTEGER(8) :: SIZE_INSERTED_EDG
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGD_EDG
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
        INTEGER, DIMENSION(:), ALLOCATABLE :: NBR_INSERT_II,ADRESS_II
        INTEGER, DIMENSION(:), ALLOCATABLE :: KREMNODE_EDG_SAVE,INSERTED_EDG,REMNODE_EDG,TMP_EDG
        INTEGER, DIMENSION(:),ALLOCATABLE :: INOD2LIN,NOD2LIN,KNOD2LIN
        INTEGER, DIMENSION(:),ALLOCATABLE :: TAG_ND,IDX_ND,TAG_NDE
!       -------------------------------
!       FIRST : integer , first block of inserted edges
!       LAST : integer , last block of inserted edges
!       NNREM_edg_SAVE : integer , internal counter
!       OFFSET : integer , internal offset for the REMNODE_EDG array
!       NBR_INTRA : integer , number of old EDGES between 2 blocks
!       NBR_EXTRA : integer , number of old remaining edges
!       TOTAL_INSERTED : integer , total number of inserted edgees
!       NBR_INSERT_II : integer, dimension = NEDGE , number of inserted edges for each II edge
!       ADRESS_II : integer, dimension = NEDGE , adress of the first inserted edges for each II segment
!       KREMNODE_EDG_SAVE : integer, dimension = NEDGE+1 , list of old edges
!       SIZE_INSERTED_EDG : integer, size of the INSERTED_EDG array ; SIZE_INSERTED_EDG is an upper bound, 
!                            can be optimized!
!       INSERTED_EDG : integer, dimension = SIZE_INSERTED_EDG, list inserted edges
!       REMNODE_EDG : integer, dimension = NEDGE + TOTAL_INSERTED, new array with old and inserted edges
!       -------------------------------

      INTEGER :: LIMIT
C-----------------------------------------------

        ID=NOM_OPT(1,N)
        CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,N),LTITR)

        NEDGE =IPARI(68,N)
        IEDGE = IPARI(58,N)
        NRTM = IPARI(4,N)
C
        ALLOCATE(INOD2LIN(NUMNOD+1),NOD2LIN(2*NEDGE))
        ALLOCATE(KNOD2LIN(NUMNOD+1))
C
        NOD2LIN(1:2*NEDGE)    = 0
        KNOD2LIN(1:NUMNOD+1) = 0
        INOD2LIN(1:NUMNOD+1) = 0
C-----------------------------------------------
C  Definition of node to line connections
C-----------------------------------------------
C
        DO I=1,NEDGE
           NN = INTBUF_TAB(N)%LEDGE(5+(I-1)*NLEDGE)
           KNOD2LIN(NN) = KNOD2LIN(NN) + 1
           NN = INTBUF_TAB(N)%LEDGE(6+(I-1)*NLEDGE)
           KNOD2LIN(NN) = KNOD2LIN(NN) + 1
        END DO
C
        INOD2LIN(1) = 1
        DO I=1,NUMNOD
           INOD2LIN(I+1) = INOD2LIN(I) + KNOD2LIN(I)
         END DO
         KNOD2LIN(1:NUMNOD+1) = INOD2LIN(1:NUMNOD+1)
C
        DO I=1,NEDGE
           NN = INTBUF_TAB(N)%LEDGE(5+(I-1)*NLEDGE)
           NOD2LIN(KNOD2LIN(NN)) = I
           KNOD2LIN(NN) = KNOD2LIN(NN) + 1
           NN = INTBUF_TAB(N)%LEDGE(6+(I-1)*NLEDGE)
           NOD2LIN(KNOD2LIN(NN)) = I
           KNOD2LIN(NN) = KNOD2LIN(NN) + 1
        END DO

        ALLOCATE(TAGD_EDG(NEDGE))

        SOL_EDGE =IEDGE/10 ! solids
        SH_EDGE  =IEDGE-10*SOL_EDGE ! shells

C---------------------------------------------------------------------
C  Main solids : build tables for deleted edges for each NRTM
C     Use tabs of N2S : edge is deactivated if 1 node is deactivated
C--------------------------------------------------------------------
        IF(SOL_EDGE > 0 .AND. IPARI(63,N) == 2) THEN

           MAX_INSERTED_EDG = 0
           DO I=1,NUMNOD
                MAX_INSERTED_EDG = MAX( MAX_INSERTED_EDG,(INOD2LIN(I+1)-INOD2LIN(I)) ) 
           ENDDO

           ALLOCATE( NBR_INSERT_II(NRTM) )
           ALLOCATE( KREMNODE_EDG_SAVE(NRTM+1) )
           SIZE_INSERTED_EDG = MAX_INSERTED_EDG*IPARI(62,N)
           CALL MY_ALLOC(INSERTED_EDG,SIZE_INSERTED_EDG)
           TAGD_EDG(1:NEDGE)=0
           KREMNODE_EDG_SAVE(1:NRTM+1) = 0
           NBR_INSERT_II(1:NRTM) = 0
           JJJ = 0
           NNREM_EDG = 0
           DO II=1,NRTM
              K = INTBUF_TAB(N)%KREMNODE(II)+1
              L = INTBUF_TAB(N)%KREMNODE(II+1)               
              DO M=K,L
                 NN = INTBUF_TAB(N)%REMNODE(M)
                 IF ((INOD2LIN(NN+1)-INOD2LIN(NN))/=0) THEN
                   DO IE=INOD2LIN(NN),INOD2LIN(NN+1)-1 
                    IEDGS = NOD2LIN(IE)
                    IF (TAGD_EDG(IEDGS)==0) THEN
                       NNREM_EDG = NNREM_EDG + 1  
                       TAGD_EDG(IEDGS)=1  
                       JJJ = JJJ + 1
                       INSERTED_EDG(JJJ) = IEDGS  
                       NBR_INSERT_II(II) = NBR_INSERT_II(II) +1
                    ENDIF
                   ENDDO 
                 ENDIF
              ENDDO
              KREMNODE_EDG_SAVE(II+1) = KREMNODE_EDG_SAVE(II)+NBR_INSERT_II(II)
              DO M=K,L
                 NN = INTBUF_TAB(N)%REMNODE(M)
                 IF ((INOD2LIN(NN+1)-INOD2LIN(NN))/=0) THEN
                   DO IE=INOD2LIN(NN),INOD2LIN(NN+1)-1 
                    IEDGS = NOD2LIN(IE)
                    IF (TAGD_EDG(IEDGS)==1) TAGD_EDG(IEDGS)=0 
                   ENDDO 
                 ENDIF
              ENDDO
           ENDDO

           CALL UPGRADE_REMNODE_E2S(IPARI(1,N),NNREM_EDG,INTBUF_TAB(N))

           INTBUF_TAB(N)%REMNODE_E2S(1:NNREM_EDG) = INSERTED_EDG(1:NNREM_EDG)
           INTBUF_TAB(N)%KREMNODE_E2S(1:NRTM+1) = KREMNODE_EDG_SAVE(1:NRTM+1) 
           INTBUF_TAB(N)%KREMNODE_E2S(1)=0
           DO II=1,NRTM+1
              INTBUF_TAB(N)%KREMNODE_E2S(II) =INTBUF_TAB(N)%KREMNODE_E2S(II)+1
           ENDDO

           DEALLOCATE(NBR_INSERT_II,KREMNODE_EDG_SAVE,INSERTED_EDG)

           ! ---------
           IF(FLAG_OUTPUT>0) THEN
            ! Output message 
            CALL ANCMSG(MSGID=2067,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=NNREM_EDG)
           ENDIF
           ! ---------
        ENDIF

C---------------------------------------------------------------------
C  Main shells : same algorithm  like N2S
C--------------------------------------------------------------------

        IF(SH_EDGE > 0) THEN


          ALLOCATE(TAG_ND(NUMNOD))
          ALLOCATE(IDX_ND(NUMNOD))
          ALLOCATE(TAG_NDE(NUMNOD))

          ALLOCATE( NBR_INSERT_II(NEDGE) )
          ALLOCATE( ADRESS_II(NEDGE) )
          ALLOCATE( KREMNODE_EDG_SAVE(NEDGE+1) )
          NBR_INSERT_II(1:NEDGE) = 0
          ADRESS_II(1:NEDGE) = 0
          KREMNODE_EDG_SAVE(1:NEDGE+1) = 0

C---------
          JJJ = 0
          NNREM_EDG = 0   
   
          TAGD_EDG(1:NEDGE)=0
          TAG_ND(1:NUMNOD) = 0
          IDX_ND(1:NUMNOD) = 0 
          TAG_NDE(1:NUMNOD) = 0
          NREMOV_EDG = IPARI(94,N)
          IADA= 1
          IF(NREMOV_EDG>0) KREMNODE_EDG_SAVE(1:NEDGE+1) = INTBUF_TAB(N)%KREMNODE_EDG(1:NEDGE+1)

          SIZE_INSERTED_EDG = 1
          MAX_INSERTED_EDG = 1
          MAX_INSERTED_I2 = 1
          DO II=1,NEDGE           
            DO J=5,6
              NM = INTBUF_TAB(N)%LEDGE(J+(II-1)*NLEDGE)
              IF (POINTS_I2N(NM,1)/=0) THEN
                MAX_INSERTED_I2 = MAX( MAX_INSERTED_I2,POINTS_I2N(NM,2)-POINTS_I2N(NM,1) )
                DO I=POINTS_I2N(NM,1),POINTS_I2N(NM,2)
                   MAX_INSERTED_EDG = MAX( MAX_INSERTED_EDG,(INOD2LIN(NM+1)-INOD2LIN(NM)) ) 
                ENDDO 
              ENDIF                                  
            ENDDO
          ENDDO

          ! We need an overstimation of the size of INSERTED_EDG, the following code 
          ! overestimate the required size, avoiding integer overflow
          LIMIT = HUGE(NEDGE) / 8
          IF( NEDGE > LIMIT ) THEN ! NEDGE cannot be multiplied by 8
            SIZE_INSERTED_EDG =  HUGE(NEDGE)
          ELSE IF ( MAX_INSERTED_EDG > LIMIT / (NEDGE)) THEN ! MAX_INSERTED_EDG cannot be multiplied by 8*NEDGE
            SIZE_INSERTED_EDG = HUGE(NEDGE)
          ELSE IF (MAX_INSERTED_I2 > LIMIT / (NEDGE*MAX_INSERTED_EDG)) THEN ! MAX_INSERTED_I2, cannot be multiplied by 8*NEDGE*MAX_INSERTED_EDG
            SIZE_INSERTED_EDG = HUGE(NEDGE)                             
          ELSE ! no integer overflow
            SIZE_INSERTED_EDG =  8 * NEDGE *MAX_INSERTED_EDG *MAX_INSERTED_I2
          ENDIF

          CALL MY_ALLOC(INSERTED_EDG,SIZE_INSERTED_EDG) 

          DO II=1,NEDGE
            NNREM_EDG_SAVE = NNREM_EDG
C 
C          Do not add nodes already stored w/IREM_GAP
            IF(FLAGREMNODE==2)THEN
              KI = INTBUF_TAB(N)%KREMNODE_EDG(II)
              KL = INTBUF_TAB(N)%KREMNODE_EDG(II+1) -1
              DO J=KI,KL
                 ES = INTBUF_TAB(N)%REMNODE_EDG(J)
                 TAGD_EDG(ES)=1
              END DO
            ENDIF
C  
                               
            IF(JJJ + MAX_INSERTED_EDG*MAX_INSERTED_I2  > SIZE_INSERTED_EDG) THEN
C extend INSERTED_EDG if needed
               OLDSIZE = SIZE_INSERTED_EDG
               SIZE_INSERTED_EDG = SIZE_INSERTED_EDG  + MAX(NEDGE,MAX_INSERTED_EDG*MAX_INSERTED_I2)
               CALL MY_ALLOC(TMP_EDG,SIZE_INSERTED_EDG)
               TMP_EDG(1:OLDSIZE) = INSERTED_EDG(1:OLDSIZE)
!              move_alloc deallocates TMP
               CALL MOVE_ALLOC(TMP_EDG,INSERTED_EDG)
            ENDIF

            ND_TAG = 0
            DO J=5,6
               NM = INTBUF_TAB(N)%LEDGE(J+(II-1)*NLEDGE) !first node of the edge
!       Sort the I2NODE array :
!               | NSM(1) | Inter(1) | SECONDARY(1)
!               | NSM(1) | Inter(1) | SECONDARY(20)
!               | NSM(1) | Inter(1) | SECONDARY(3)
!               | NSM(1) | Inter(2) | SECONDARY(1)
!               | NSM(2) | Inter(4) | SECONDARY(14)
!               | NSM(2) | Inter(5) | SECONDARY(18)
!               | NSM(3) | Inter(1) | SECONDARY(1)
!               |  ...   |   ...    |   ...      
!       Compute the pointer array POINT_I2NODE :
!               | 0 | 0 | if 0,0 --> node not in type2 interface
!               | 1 | 3 | 
!               | 4 | 5 | 
!               | 0 | 0 | 

               IF (POINTS_I2N(NM,1)/=0) THEN
                DO I=POINTS_I2N(NM,1),POINTS_I2N(NM,2) ! size of the loop: number of type2 interface that have this node 
                  N2 = I2NODE(I,2) !interface id
                  IS = I2NODE(I,3) !node id 
                  IF (IS >0) THEN ! secondary node?                                
                     NS = INTBUF_TAB(N2)%NSV(IS) !secondary node of the type2 interface
                     IF (((INOD2LIN(NS+1)-INOD2LIN(NS))/=0).AND.(TAG_NDE(NS)==0)) THEN ! What is NOD2LIN? 
                       TAG_NDE(NS)=1 !thag the node for the first time it found (in what?) Nodes of type2 interface?
                       DO IE=INOD2LIN(NS),INOD2LIN(NS+1)-1  !for all edges in what?
                         IEDGS = NOD2LIN(IE) ! IEDGES = all the edges that have NS as node?
                         IF (TAGD_EDG(IEDGS)==0) THEN  ! if not already tagged
                             NNREM_EDG = NNREM_EDG + 1  
                             TAGD_EDG(IEDGS)=1  
                             JJJ = JJJ + 1
                             INSERTED_EDG(JJJ) = IEDGS  ! IEDG inserted 
                             NES = INTBUF_TAB(N)%LEDGE(5+(IEDGS-1)*NLEDGE)
                             TAG_ND(NES) = TAG_ND(NES) +1 !Nodes of type25 Edge
                             ND_TAG = ND_TAG + 1
                             IDX_ND( ND_TAG)=NES
                             NES = INTBUF_TAB(N)%LEDGE(6+(IEDGS-1)*NLEDGE)
                             TAG_ND(NES) = TAG_ND(NES) +1
                             ND_TAG = ND_TAG +1
                             IDX_ND( ND_TAG)=NES
                         END IF
                       ENDDO
                     ENDIF
                  ELSEIF (IS <0) THEN ! node is main?
                      IIS = -IS
                      L = INTBUF_TAB(N2)%IRTLM(IIS)
                      NNOD_2 = 4
                      ! If triangle, NNOD_2 = 3, if quand NNOD_2 = 4
                      IF( INTBUF_TAB(N2)%IRECTM(4*(L-1)+4)==INTBUF_TAB(N2)%IRECTM(4*(L-1)+3) ) NNOD_2 = 3
                      DO III = 1,NNOD_2 ! for all nodes of the surface
                         NM2 = INTBUF_TAB(N2)%IRECTM(4*(L-1)+III)  ! NM2 is the id of the IIIth node of the surface
                         IF (((INOD2LIN(NM2+1)-INOD2LIN(NM2))/=0).AND.(TAG_NDE(NM2)==0)) THEN
                           TAG_NDE(NM2)=1
                           DO IE=INOD2LIN(NM2),INOD2LIN(NM2+1)-1  
                            IEDGS = NOD2LIN(IE)  ! edge id to remove 
                            IF (TAGD_EDG(IEDGS)==0) THEN !if not already removed
                                NNREM_EDG = NNREM_EDG + 1  
                                TAGD_EDG(IEDGS)=1          !remove
                                JJJ = JJJ + 1              !increment the counter
                                INSERTED_EDG(JJJ) = IEDGS     
                                NES = INTBUF_TAB(N)%LEDGE(5+(IEDGS-1)*NLEDGE) ! Node 1 of the edge IEDGS
                                TAG_ND(NES) = TAG_ND(NES) +1 !TAG_ND(NES) = number of times that the node NES belongs to a removed edge?
                                ND_TAG = ND_TAG +1 !nb entity to insert to node NM2
                                IDX_ND( ND_TAG)=NES 
                                NES = INTBUF_TAB(N)%LEDGE(6+(IEDGS-1)*NLEDGE) !Node 2 of the edge IEDGS
                                TAG_ND(NES) = TAG_ND(NES) +1  
                                ND_TAG = ND_TAG +1
                                IDX_ND( ND_TAG)=NES 
                             END IF
                           ENDDO 
                         ENDIF
                      ENDDO
                  END IF
                ENDDO
               ENDIF
            END DO ! 
            
           !   -------------------
           !   Adding edges where 2 nodes already tagged 
           !  Two nodes of what? type25 or type2, which tag?
          
            DO ND = 1,ND_TAG
              NES = IDX_ND( ND)
              IF(TAG_ND(NES) ==1)THEN ! to double check, why ==1 and not > 0 ? if the nodes belongs exactly to one removed edge?
                 IF ((INOD2LIN(NES+1)-INOD2LIN(NES))/=0) THEN
                  DO IE=INOD2LIN(NES),INOD2LIN(NES+1)-1 ! all the edges to remove that have node NES? 
                     IEDGS = NOD2LIN(IE) 
                     DO J=5,6
                        NM =INTBUF_TAB(N)%LEDGE(J+(IEDGS-1)*NLEDGE)
                        !to double check, why ==1 and not > 0 ?
                        IF(TAG_ND(NM)==1.AND.TAGD_EDG(IEDGS) ==0) THEN ! if the node NM, but the edge was not taged 
                           NNREM_EDG = NNREM_EDG + 1  
                           TAGD_EDG(IEDGS)=1  
                           JJJ = JJJ + 1
                           INSERTED_EDG(JJJ) = IEDGS  
                        ENDIF
                     ENDDO
                  ENDDO
                 ENDIF
              ENDIF
            ENDDO

            
           !   -------------------
           !   number of inserted edges
            NBR_INSERT_II(II) = NNREM_EDG - NNREM_EDG_SAVE
            KREMNODE_EDG_SAVE(II) = KREMNODE_EDG_SAVE(II+1) - KREMNODE_EDG_SAVE(II)
            IADA = IADA + KREMNODE_EDG_SAVE(II)
           !   adress of the first inserted node
            ADRESS_II(II) = IADA
            KREMNODE_EDG_SAVE(II) = IADA + NBR_INSERT_II(II) - 1
            IADA = IADA + NBR_INSERT_II(II)
                                !   -------------------
                                
C-----reset    TAGD_EDG=0     

            DO ND = 1,ND_TAG
              NES = IDX_ND( ND)
              IF(TAG_ND(NES) ==1)THEN
                 IF ((INOD2LIN(NES+1)-INOD2LIN(NES))/=0) THEN
                  DO IE=INOD2LIN(NES),INOD2LIN(NES+1)-1  
                     IEDGS = NOD2LIN(IE) 
                      DO J=5,6
                         NM =INTBUF_TAB(N)%LEDGE(J+(IEDGS-1)*NLEDGE)
                         IF(TAGD_EDG(IEDGS) ==1) TAGD_EDG(IEDGS)=0
                       ENDDO
                   ENDDO
                  ENDIF
              ENDIF
            ENDDO  
            DO J=5,6
              NM = INTBUF_TAB(N)%LEDGE(J+(II-1)*NLEDGE)
              IF (POINTS_I2N(NM,1)/=0) THEN
               DO I=POINTS_I2N(NM,1),POINTS_I2N(NM,2)
                 N2 = I2NODE(I,2)
                 IS = I2NODE(I,3)
                 IF (IS >0) THEN
                     NS = INTBUF_TAB(N2)%NSV(IS)
                     IF (((INOD2LIN(NS+1)-INOD2LIN(NS))/=0).AND.(TAG_NDE(NS)==1)) THEN
                       TAG_NDE(NS)=0
                       DO IE=INOD2LIN(NS),INOD2LIN(NS+1)-1  
                        IEDGS = NOD2LIN(IE)    
                        IF (TAGD_EDG(IEDGS)==1) THEN
                            TAGD_EDG(IEDGS)=0
                            NES = INTBUF_TAB(N)%LEDGE(5+(IEDGS-1)*NLEDGE)
                            TAG_ND(NES) = 0
                            NES = INTBUF_TAB(N)%LEDGE(6+(IEDGS-1)*NLEDGE)
                            TAG_ND(NES) = 0
                        ENDIF
                       ENDDO
                     ENDIF
                 ELSEIF (IS <0) THEN
                     IIS = -IS
                     L = INTBUF_TAB(N2)%IRTLM(IIS)
                     NNOD_2 = 4
                     IF( INTBUF_TAB(N2)%IRECTM(4*(L-1)+4)==INTBUF_TAB(N2)%IRECTM(4*(L-1)+3) ) NNOD_2 = 3
                     DO III = 1,NNOD_2
                        NM2 = INTBUF_TAB(N2)%IRECTM(4*(L-1)+III)
                        IF ((INOD2LIN(NM2+1)-INOD2LIN(NM2))/=0.AND.(TAG_NDE(NM2)==1)) THEN
                          TAG_NDE(NM2)=0
                          DO IE=INOD2LIN(NM2),INOD2LIN(NM2+1)-1  
                           IEDGS = NOD2LIN(IE)                                     
                           IF (TAGD_EDG(IEDGS)==1) THEN
                               TAGD_EDG(IEDGS)=0
                               NES = INTBUF_TAB(N)%LEDGE(5+(IEDGS-1)*NLEDGE)
                               TAG_ND(NES) = 0
                               NES = INTBUF_TAB(N)%LEDGE(6+(IEDGS-1)*NLEDGE)
                               TAG_ND(NES) = 0
                           ENDIF
                          ENDDO 
                        ENDIF
                     ENDDO
                 END IF
               END DO
              ENDIF
            ENDDO

            IF(FLAGREMNODE==2)THEN
              DO IE=KI,KL
                 IEDGS = INTBUF_TAB(N)%REMNODE_EDG(IE)
                 TAGD_EDG(IEDGS)=0
              END DO
            END IF
C
          END DO !II=1,NEDGE

          IF(NNREM_EDG>0) THEN
                              
           ! get the first and the last inserted node
            FIRST = 0
            LAST = 0
            DO II = 1,NEDGE
               IF(FIRST==0) THEN
                 IF( NBR_INSERT_II(II)/=0 ) FIRST = II
               ENDIF
               IF(LAST==0) THEN
                 IF( NBR_INSERT_II(NEDGE+1-II)/=0 ) LAST = NEDGE+1-II
               ENDIF
            ENDDO
           !       count the total number of inserted edges
            TOTAL_INSERTED = 0
            DO II=1,NEDGE
               TOTAL_INSERTED = TOTAL_INSERTED + NBR_INSERT_II(II)
            ENDDO
          !       allocate the buffer array
            ALLOCATE( REMNODE_EDG(NREMOV_EDG+TOTAL_INSERTED) )
       
            J = 0
            I = 0
            OFFSET = 0
            IF( FIRST>0 ) THEN
             !   insertion of the first chunk of edge : if ADRESS_II(FIRST) > 1
             !   --> need to copy the old edges 
              IF( ADRESS_II(FIRST)>1 ) THEN
                REMNODE_EDG(1:ADRESS_II(FIRST)-1) = INTBUF_TAB(N)%REMNODE_EDG(1:ADRESS_II(FIRST)-1)
                OFFSET = OFFSET + ADRESS_II(FIRST)-1
                I = I + ADRESS_II(FIRST)-1
              ENDIF
        
              DO II=FIRST,LAST
               !       insertion of the edges
                IF( NBR_INSERT_II(II)>0 ) THEN
                   DO JJ = 1,NBR_INSERT_II(II)
                      J = J + 1
                      REMNODE_EDG(OFFSET+NBR_INSERT_II(II)+1-JJ) = INSERTED_EDG(J)
                   ENDDO
                   OFFSET = OFFSET + NBR_INSERT_II(II)
                ENDIF
                IF(II<LAST.AND.NREMOV_EDG>0) THEN
                 ! copy of the old edges
                   NBR_INTRA = ADRESS_II(II+1) - ADRESS_II(II)-NBR_INSERT_II(II)
                   IF( NBR_INTRA>0 )THEN
                      DO JJ = 1,NBR_INTRA
                         I = I + 1
                         REMNODE_EDG(JJ+OFFSET) = INTBUF_TAB(N)%REMNODE_EDG(I)
                      ENDDO
                      OFFSET = OFFSET + NBR_INTRA
                   ENDIF
                ENDIF
              ENDDO
            ENDIF

         !       copy of the old edges for the LAST chunk
            IF( I<NREMOV_EDG ) THEN
               NBR_EXTRA = NREMOV_EDG - I
               REMNODE_EDG(OFFSET+1:OFFSET+NBR_EXTRA) = INTBUF_TAB(N)%REMNODE_EDG(I+1:NREMOV_EDG)
            ENDIF
          !       update of NNREM_edg and deallocation / allocation of the new array
            NNREM_EDG = NNREM_EDG + NREMOV_EDG
            CALL UPGRADE_REMNODE_EDG2(IPARI(1,N),NNREM_EDG,INTBUF_TAB(N))
            INTBUF_TAB(N)%REMNODE_EDG(1:NNREM_EDG) = REMNODE_EDG(1:NNREM_EDG)
            INTBUF_TAB(N)%KREMNODE_EDG(2:NEDGE+1) = KREMNODE_EDG_SAVE(1:NEDGE) 
            INTBUF_TAB(N)%KREMNODE_EDG(1)=0
            DO II=1,NEDGE+1
              INTBUF_TAB(N)%KREMNODE_EDG(II) =INTBUF_TAB(N)%KREMNODE_EDG(II)+1
            ENDDO
             ! ---------
            IF(FLAG_OUTPUT>0) THEN
              ! Output message 
              CALL ANCMSG(MSGID=2067,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=NNREM_EDG)
            ENDIF
            ! ---------
C----------used for Iedge=1     
            NREMOV_EDG = NNREM_EDG
          END IF !IF (NNREM_EDG>0) THEN
          IF(ALLOCATED(REMNODE_EDG)) DEALLOCATE( REMNODE_EDG )  
          IF(ALLOCATED(INSERTED_EDG)) DEALLOCATE( INSERTED_EDG )  
 
        !   ------------------------------------------------

          DEALLOCATE( NBR_INSERT_II )
          DEALLOCATE( ADRESS_II )
          DEALLOCATE( KREMNODE_EDG_SAVE )     

          DEALLOCATE(TAGD_EDG,TAG_ND,IDX_ND,TAG_NDE)

        ENDIF ! SHELL_EDG

        DEALLOCATE(INOD2LIN,NOD2LIN)
C----
        RETURN
        END
