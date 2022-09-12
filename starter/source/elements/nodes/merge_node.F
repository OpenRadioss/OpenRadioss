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
Chd|  MERGE_NODE                    source/elements/nodes/merge_node.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MERGE_BUCKET_SEARCH           source/elements/nodes/merge_bucket_search.F
Chd|        USRTOS                        source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NOD2EL_MOD                    share/modules1/nod2el_mod.F   
Chd|====================================================================
      SUBROUTINE MERGE_NODE(X,ITAB,ITABM1 ,IMERGE,IMERGE2,
     .                      IADMERGE2,NMERGE_TOT,MERGE_NODE_TAB,MERGE_NODE_TOL,
     .                      NMERGE_NODE_CAND,NMERGE_NODE_DEST,IXS,IXS10,IXS20,
     .                      IXS16,IXQ,IXC,IXT,IXP,
     .                      IXR,IXTG,IXUR,EANI,IGRNOD)   
      USE MESSAGE_MOD
      USE NOD2EL_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "scr06_c.inc"
#include      "scr03_c.inc"
#include      "titr_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(NUMNOD), ITABM1(2*NUMNOD),IMERGE(*),
     .        IMERGE2(NUMNOD+1),IADMERGE2(NUMNOD+1),MERGE_NODE_TAB(4,*),
     .        NMERGE_NODE_CAND,NMERGE_NODE_DEST,NMERGE_TOT
      INTEGER IXS(NIXS,*),IXS10(6,*),IXS16(8,*),IXS20(12,*),IXQ(NIXQ,*),
     .        IXC(NIXC,*),IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IXTG(NIXTG,*),
     .        IXUR(6,*),EANI(*)
      TARGET  ITAB
      my_real
     .        X(3,NUMNOD),MERGE_NODE_TOL(*)
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,M,N,NUMNOD1,NM,FLAG,N_DEST,N_DEST_DEST,NN1,NN2,CUR_ID,GR_IDS,ALL_VS_ALL,
     .        NM_L
C
      INTEGER, DIMENSION(:),ALLOCATABLE :: IMERGE0,LBUF,IADMERGE2TMP,LIST1,LIST2,LIST1_INV,LIST2_INV
      INTEGER, DIMENSION(:),ALLOCATABLE :: LIST1_IDMERGE,LIST2_IDMERGE,LIST1_NBMERGE,LIST2_NBMERGE
      my_real, DIMENSION(:),ALLOCATABLE :: DIST
C 
      my_real
     .        DBUC
C-----------------------------------------------
       INTEGER 
     .         USRTOS,USRTOSC
      EXTERNAL USRTOS,USRTOSC
C
C=======================================================================
C - MERGING ROUTINE for /MERGE/NODE
C======================================================================= 
C     
C-------    dbuc------------------------------
C-- Cnodes not taken into account
C
      NUMNOD1 = NUMNOD0-NUMCNOD
      DBUC = ZERO 
      ALL_VS_ALL = 0
      DO I=1,NB_MERGE_NODE 
        DBUC = MAX(DBUC,MERGE_NODE_TOL(I))
        GR_IDS = MERGE_NODE_TAB(2,I)
        IF (GR_IDS == 0) ALL_VS_ALL = 1
      ENDDO   
C
C----------------------------------------------------------------------------------- 
C     BUCKET SEARCH - search of node to merge for each candidate - stored in IMERGE0                           
C-----------------------------------------------------------------------------------
C
      NN1 = NMERGE_NODE_CAND
      NN2 = NMERGE_NODE_DEST
C
      ALLOCATE(IMERGE0(NN1),DIST(NN1),LIST1(NMERGE_NODE_CAND),LIST2(NMERGE_NODE_DEST))
      ALLOCATE(LIST1_INV(NUMNOD),LIST2_INV(NUMNOD),LBUF(NUMNOD),IADMERGE2TMP(NUMNOD+1))
      ALLOCATE(LIST1_IDMERGE(NMERGE_NODE_CAND),LIST2_IDMERGE(NMERGE_NODE_DEST))
      ALLOCATE(LIST1_NBMERGE(NMERGE_NODE_CAND),LIST2_NBMERGE(NMERGE_NODE_DEST))
      IMERGE0 = 0
      DIST = ZERO
      LIST1 = 0
      LIST1_IDMERGE = 0
      LIST1_NBMERGE = 0
      LIST1_INV = 0
      LIST2 = 0
      LIST2_IDMERGE = 0
      LIST2_NBMERGE = 0
      LIST2_INV = 0
      LBUF = 0
      IADMERGE2TMP = 0
C
      FLAG = 2
C
C---- bluid list of candidates
      IF (ALL_VS_ALL == 1) THEN
C--     all nodes are candidates - only one merge
        DO I=1,NUMNOD
          LIST1(I) = I
          LIST1_INV(I) = I
          LIST1_NBMERGE(I) = 1
          LIST1_IDMERGE(I) = 1
        ENDDO
      ELSE
C--     list filled merge by merge
        NM = 0
        DO I=1,NB_MERGE_NODE 
          GR_IDS = MERGE_NODE_TAB(2,I)
          DO J=1,IGRNOD(GR_IDS)%NENTITY
            IF (LIST1_INV(IGRNOD(GR_IDS)%ENTITY(J)) == 0) THEN
C--           new point in list
              NM = NM + 1
              LIST1(NM) = IGRNOD(GR_IDS)%ENTITY(J)
              LIST1_INV(IGRNOD(GR_IDS)%ENTITY(J)) = NM
              LIST1_NBMERGE(NM) = 1
              LIST1_IDMERGE(NM) = I
            ELSE
C--           point already in list
              NM_L = LIST1_INV(IGRNOD(GR_IDS)%ENTITY(J))
C--           coding of idmerge in case of several merge
              LIST1_NBMERGE(NM_L) = LIST1_NBMERGE(NM_L) + 1
              LIST1_IDMERGE(NM_L) = LIST1_IDMERGE(NM_L) + I*(NB_MERGE_NODE**(LIST1_NBMERGE(NM_L)-1))   
            ENDIF            
          ENDDO
        ENDDO   
      ENDIF
C
C---- bluid list of destination - filled merge by merge
      IF (ALL_VS_ALL == 1) THEN
C--   all nodes are destination 
        DO I=1,NUMNOD
          LIST2(I) = I
          LIST2_INV(I) = I
          LIST2_NBMERGE(I) = 1
          LIST2_IDMERGE(I) = 1
        ENDDO
      ELSE
C--     list filled merge by merge
        NM = 0
        DO I=1,NB_MERGE_NODE 
          GR_IDS = MERGE_NODE_TAB(2,I)
          IF (MERGE_NODE_TAB(1,I) == 1) THEN
C--         destination defined by grnod
            DO J=1,IGRNOD(GR_IDS)%NENTITY
              IF (LIST2_INV(IGRNOD(GR_IDS)%ENTITY(J)) == 0) THEN
C--             new point in list
                NM = NM + 1
                LIST2(NM) = IGRNOD(GR_IDS)%ENTITY(J)
                LIST2_INV(IGRNOD(GR_IDS)%ENTITY(J)) = NM
                LIST2_NBMERGE(NM) = 1
                LIST2_IDMERGE(NM) = I
              ELSE
C--             point already in list
                NM_L = LIST2_INV(IGRNOD(GR_IDS)%ENTITY(J))
C--             coding of idmerge in case of several merge
                LIST2_NBMERGE(NM_L) = LIST2_NBMERGE(NM_L) + 1
                LIST2_IDMERGE(NM_L) = LIST2_IDMERGE(NM_L) + I*(NB_MERGE_NODE**(LIST2_NBMERGE(NM_L)-1))        
              ENDIF      
            ENDDO
          ELSE
C--         all nodes are destination
            DO J=1,NUMNOD
              IF (LIST2_INV(J) == 0) THEN
C--             new point in list
                NM = NM + 1
                LIST2(NM) = J
                LIST2_INV(J) = NM
                LIST2_NBMERGE(NM) = 1
                LIST2_IDMERGE(NM) = I
              ELSE
C--             point already in list
                NM_L = LIST2_INV(J)
C--             coding of idmerge in case of several merge
                LIST2_NBMERGE(NM_L) = LIST2_NBMERGE(NM_L) + 1
                LIST2_IDMERGE(NM_L) = LIST2_IDMERGE(NM_L) + I*(NB_MERGE_NODE**(LIST2_NBMERGE(NM_L)-1))               
              ENDIF      
            ENDDO     
          ENDIF
        ENDDO   
      ENDIF
C
      CALL MERGE_BUCKET_SEARCH(X,ITAB,ITABM1,IMERGE0,MERGE_NODE_TOL,
     .                         DBUC,NN1,NN2,LIST1,LIST2,
     .                         DIST,FLAG,LIST1_IDMERGE,LIST1_NBMERGE,LIST2_IDMERGE,
     .                         LIST2_NBMERGE)
C
C-------------------------------------------------------------------------- 
C     Remove merge of nodes of one element                        
C--------------------------------------------------------------------------
C
      DO I= 1,NN1 
        IF (IMERGE0(I) > 0) THEN
          N = LIST1(I)
          N_DEST = USRTOS(IMERGE0(I),ITABM1)
C
C---      Check of elements
	  FLAG = 0 	
C------------------------>   cas des coques  <-----------------------C		    
          DO J = KNOD2ELC(N)+1,KNOD2ELC(N+1)
            DO K=2,5	           
              IF (IXC(K,NOD2ELC(J)) == N_DEST) FLAG = 1   	   
            ENDDO
          ENDDO
C------------------------>   cas des coques triagnles <--------------C		    
          DO J = KNOD2ELTG(N)+1,KNOD2ELTG(N+1)
            DO K=2,4            
              IF (IXTG(K,NOD2ELTG(J)) == N_DEST) FLAG = 1   	   
            ENDDO
          ENDDO
C------------------------>   cas des solides <-----------------------C	
          DO J = KNOD2ELS(N)+1,KNOD2ELS(N+1)     
            CUR_ID = NOD2ELS(J)
            DO K=2,9
	      IF(IXS(K,NOD2ELS(J)) == N_DEST) FLAG = 1
            ENDDO
	    IF (EANI(CUR_ID)==10) THEN
              DO K=1,6
	        IF(IXS10(K,CUR_ID-NUMELS8) == N_DEST) FLAG = 1
              ENDDO
            ELSEIF (EANI(CUR_ID)==20) THEN		 	       
              DO K=1,12
	        IF(IXS20(K,CUR_ID-NUMELS8-NUMELS10) == N_DEST) FLAG = 1
              ENDDO
	    ELSEIF (EANI(CUR_ID)==16) THEN      
              DO K=1,8
	        IF(IXS16(K,CUR_ID-NUMELS8-NUMELS10-NUMELS20) == N) FLAG = 1
              ENDDO			 		 	       
	    ENDIF	       	           	    
	  END DO
C------------------------>   cas des elements 1D <-------------------C
          DO J = KNOD2EL1D(N)+1,KNOD2EL1D(N+1)
            CUR_ID = NOD2EL1D(J)
	    IF (CUR_ID <= NUMELT) THEN
              DO K=2,3
                IF (IXT(K,NOD2EL1D(J)) == N_DEST) FLAG = 1   	   
              ENDDO
            ELSEIF (CUR_ID <= NUMELT + NUMELP) THEN
              DO K=2,4
                IF (IXP(K,CUR_ID-NUMELT) == N_DEST) FLAG = 1   	   
              ENDDO
            ELSE
              DO K=2,4
                IF (IXR(K,CUR_ID-NUMELT-NUMELP) == N_DEST) FLAG = 1   	   
              ENDDO
            ENDIF
          ENDDO
C------------------------>   cas des elements 1D inverse (pour 3e noeud )<---C
          DO J = KNOD2EL1D(N_DEST)+1,KNOD2EL1D(N_DEST+1)
            CUR_ID = NOD2EL1D(J)
	    IF (CUR_ID <= NUMELT) THEN
              DO K=2,3
                IF (IXT(K,NOD2EL1D(J)) == N) FLAG = 1   	   
              ENDDO
            ELSEIF (CUR_ID <= NUMELT + NUMELP) THEN
              DO K=2,4
                IF (IXP(K,CUR_ID-NUMELT) == N) FLAG = 1   	   
              ENDDO
            ELSE
              DO K=2,4
                IF (IXR(K,CUR_ID-NUMELT-NUMELP) == N) FLAG = 1   	   
              ENDDO
            ENDIF
          ENDDO
C------------------------>   cas des quad  <-----------------------C		    
          DO J = KNOD2ELQ(N)+1,KNOD2ELQ(N+1)
            DO K=2,5	           
              IF (IXQ(K,NOD2ELQ(J)) == N_DEST) FLAG = 1   	   
            ENDDO
          ENDDO	
C
          IF (FLAG == 1) THEN
C--         connection removed
            IMERGE0(I) = 0
            CALL ANCMSG(MSGID=2039,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ITAB(N),I2=ITAB(N_DEST),
     .                  R1=DIST(I),
     .                  PRMOD=MSG_CUMU)
          ENDIF 
C
        ENDIF                                 
      ENDDO
C
      CALL ANCMSG(MSGID=2039,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                PRMOD=MSG_PRINT )
C
C-------------------------------------------------------------------------- 
C     Ignore merge in case of hierarchy                         
C--------------------------------------------------------------------------
      DO I= 1,NN1 
        IF (IMERGE0(I) > 0) THEN
          N = LIST1(I)
          N_DEST = USRTOS(IMERGE0(I),ITABM1)
          IF (LIST1_INV(N_DEST) > 0) THEN
           IF (IMERGE0(LIST1_INV(N_DEST)) > 0) THEN
            N_DEST_DEST = USRTOS(IMERGE0(LIST1_INV(N_DEST)),ITABM1)
C--         hierarchy detected - the longest merge is removed
            IF (DIST(LIST1_INV(N_DEST)) > DIST(I)) THEN
              IMERGE0(LIST1_INV(N_DEST)) = 0
              CALL ANCMSG(MSGID=2038,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(N_DEST),I2=ITAB(N_DEST_DEST),
     .                R1=DIST(LIST1_INV(N_DEST)),
     .                PRMOD=MSG_CUMU)
C
            ELSE
              IMERGE0(I) = 0
              CALL ANCMSG(MSGID=2038,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(N),I2=ITAB(N_DEST),
     .                R1=DIST(I),
     .                PRMOD=MSG_CUMU)
            ENDIF
           ENDIF
          ENDIF
        ENDIF                                 
      ENDDO
C
      CALL ANCMSG(MSGID=2038,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                PRMOD=MSG_PRINT )
C
C-------------------------------------------------------------------------- 
C     COMPACT IMERGE -> No systeme                           
C-------------------------------------------------------------------------- 
      NM = 0                                  
      DO I= 1,NN1 
        IF (IMERGE0(I) > 0) THEN
          N = LIST1(I)
          NM = NM+1                          
          IMERGE(NMERGE_TOT+NUMCNOD+NM) = USRTOS(IMERGE0(I),ITABM1)
          IMERGE(NUMCNOD+NM) = N
        ENDIF                                 
      ENDDO                                   
      NMERGED = NMERGED + NM
                           
C--------------------------------------------------
C     TAB ID_NODE systeme  -> ID_CNODE  systeme (done from scratch even with cnodes)              
C--------------------------------------------------
      IF (NMERGED > 0) THEN
        LBUF = 0
        DO I = 1,NMERGE_TOT
          IF (IMERGE(NMERGE_TOT+I) > 0) THEN
            N = IMERGE(NMERGE_TOT+I)
            LBUF(N) = LBUF(N) + 1
          ENDIF
        ENDDO
        IADMERGE2(1) = 1
        IADMERGE2TMP(1) = 1
        DO I = 2,NUMNOD+1
          IADMERGE2(I) = IADMERGE2(I-1) + LBUF(I-1)
          IADMERGE2TMP(I) = IADMERGE2TMP(I-1) + LBUF(I-1)
        ENDDO
        DO I = 1,NMERGE_TOT
          IF (IMERGE(NMERGE_TOT+I) > 0) THEN
            N = IMERGE(NMERGE_TOT+I)
            IMERGE2(IADMERGE2TMP(N)) = IMERGE(I)
            IADMERGE2TMP(N)=IADMERGE2TMP(N)+1
          ENDIF  
        ENDDO
      ENDIF

C--------------------------------------------------
      IF (NUMCNOD == 0) WRITE(IOUT,1000)
      WRITE(IOUT,1001)
C    
      J=0                                                            
      DO  N=1,NMERGED,50                                             
        J=J+50                                                       
        J=MIN(J,NMERGED)
        DO  I=N,J
          WRITE(IOUT,'(5X,I10,8X,I10)') 
     .          ITAB(IMERGE(NUMCNOD+I)),ITAB(IMERGE(NUMCNOD+NMERGE_TOT+I))
        ENDDO                                                       
      ENDDO
C--------
      DEALLOCATE(IMERGE0)
C--------
      RETURN
C
1000  FORMAT(/
     . '               MERGE NODES '/
     . '   --------------------------------------')
1001  FORMAT(/
     . '          NODE     MERGED TO     NODE '/)
C
      END
