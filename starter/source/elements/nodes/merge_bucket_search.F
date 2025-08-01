Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    merge_bucket_search   ../starter/source/elements/nodes/merge_bucket_search.F
!||--- called by ------------------------------------------------------
!||    merge_node            ../starter/source/elements/nodes/merge_node.F
!||--- calls      -----------------------------------------------------
!||    decode_merge          ../starter/source/elements/nodes/merge_bucket_search.F
!||    usrtos                ../starter/source/system/sysfus.F
!||    usrtosc               ../starter/source/model/submodel/merge.F
!||--- uses       -----------------------------------------------------
!||    message_mod           ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE MERGE_BUCKET_SEARCH(X,ITAB,ITABM1,IMERGE0,CMERGE,
     .                               DBUC,NN1,NN2,LIST1,LIST2,
     .                               DDD,FLAG,LIST1_IDMERGE,LIST1_NBMERGE,LIST2_IDMERGE,
     .                               LIST2_NBMERGE)
C
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(NUMNOD), ITABM1(2*NUMNOD), IMERGE0(*), NN1, NN2,LIST1(*),
     .        LIST2(*),FLAG,LIST1_IDMERGE(*),LIST2_IDMERGE(*),LIST1_NBMERGE(*),
     .        LIST2_NBMERGE(*)
      TARGET  ITAB
      my_real
     .        X(3,NUMNOD),CMERGE(*),DDD(*),DBUC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,IB,IG,JG,KP,NC,NS,NN,
     .      IBZ,IBY,IBX,KS,
     .      NBOX,NBOY,NBOZ,NBX,NBY,NBZ,NBAND,IBOITE
C
    ! Declaration of integer scalar variables
      INTEGER :: NUMNOD1, TAG
      INTEGER, ALLOCATABLE, DIMENSION(:) :: NOBX, NOBY, NOBZ
      INTEGER, ALLOCATABLE, DIMENSION(:) :: NOBCX, NOBCY, NOBCZ
      INTEGER, ALLOCATABLE, DIMENSION(:) :: TABS, TABC
C 
      INTEGER, DIMENSION(:),POINTER     :: ITABC
      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .    NPX,IPX,NPY,IPY,NPZ,IPZ,NPCX,IPCX,NPCY,IPCY,NPCZ,IPCZ
C 
      my_real
     .        XI, YI, ZI, XJ, YJ, ZJ,
     .        DIST2,DVOIS,EPS,XMIN,XMAX,YMIN,YMAX,ZMIN,
     .        ZMAX,DMX,DMY,DMZ,DMERGE
C-----------------------------------------------
       INTEGER 
     .         USRTOS,USRTOSC
      EXTERNAL USRTOS,USRTOSC
C
C=======================================================================
C - BUCKET SEARCH FOR MERGING OF NODES
C - --> FLAG = 1 : merging of nodes with cnodes
C - --> FLAG = 2 : merging of nodes with modes (/MERGE/NODE)
C
C - --> LIST1 / NN1 : list of nodes candidate for merging
C - --> LIST2 / NN2 : list of destinations
C=======================================================================
C 
      ALLOCATE(NOBX(NUMNOD))
      ALLOCATE(NOBY(NUMNOD))
      ALLOCATE(NOBZ(NUMNOD))
      ALLOCATE(NOBCX(NN2))
      ALLOCATE(NOBCY(NN2))
      ALLOCATE(NOBCZ(NN2))
      ALLOCATE(TABS(NB_MERGE_NODE))
      ALLOCATE(TABC(NB_MERGE_NODE))
      DBUC = TWO*DBUC
      EPS=EM3*DBUC 
      XMIN=EP30
      XMAX=-EP30
      YMIN=EP30
      YMAX=-EP30
      ZMIN=EP30
      ZMAX=-EP30    
C
      DO I=1,NN1
        N = LIST1(I)
        NN  = USRTOS(ITAB(N),ITABM1)
        XMIN= MIN(XMIN,X(1,NN))
        YMIN= MIN(YMIN,X(2,NN))
        ZMIN= MIN(ZMIN,X(3,NN))
        XMAX= MAX(XMAX,X(1,NN))
        YMAX= MAX(YMAX,X(2,NN))
        ZMAX= MAX(ZMAX,X(3,NN))
      ENDDO
C
      XMIN=XMIN-EPS
      YMIN=YMIN-EPS
      ZMIN=ZMIN-EPS
      XMAX=XMAX+EPS
      YMAX=YMAX+EPS
      ZMAX=ZMAX+EPS
C
      DMX=XMAX-XMIN
      DMY=YMAX-YMIN
      DMZ=ZMAX-ZMIN
C    
      NBX =MAX(1,INT(DMX/DBUC))
      NBY =MAX(1,INT(DMY/DBUC))
      NBZ =MAX(1,INT(DMZ/DBUC))
C              
      DO I=1,NN1   
        N = LIST1(I)
        NN  = USRTOS(ITAB(N),ITABM1)
        NOBX(I) = INT( (X(1,NN)-XMIN)/DBUC)
        NOBY(I) = INT( (X(2,NN)-YMIN)/DBUC)
        NOBZ(I) = INT( (X(3,NN)-ZMIN)/DBUC)
      ENDDO
C  
      IF (FLAG == 1) THEN
C--     destinations are cnodes (NN2 = NUMCNOD)
        NUMNOD1 = NUMNOD0-NUMCNOD
        ITABC => ITAB(NUMNOD1+1:NUMNOD0)
        DO I=1,NN2
          N = LIST2(I)   
          NN  = USRTOSC(ITABC(N),ITABM1)
          NOBCX(N) =INT( (X(1,NN)-XMIN)/DBUC)
          NOBCY(N) =INT( (X(2,NN)-YMIN)/DBUC)
          NOBCZ(N) =INT( (X(3,NN)-ZMIN)/DBUC)
        ENDDO
      ELSE
        ITABC => NULL()
C--     destinations are nodes
        DO I=1,NN2 
          N = LIST2(I)       
          NN  = USRTOS(ITAB(N),ITABM1)
          NOBCX(I) = INT( (X(1,NN)-XMIN)/DBUC)
          NOBCY(I) = INT( (X(2,NN)-YMIN)/DBUC)
          NOBCZ(I) = INT( (X(3,NN)-ZMIN)/DBUC)
        ENDDO
      ENDIF 
C  
      NBAND = MAX(NBX, NBY,NBZ) + 1
C      
      ALLOCATE( NPX(0:NN1+NBAND  ) , NPY(0:3*(NN1+NBAND)), 
     .         NPZ(0:9*(NN1+NBAND)) , IPX(NN1+NBAND)   ,
     .         IPY(NN1+NBAND)     , IPZ(NN1+NBAND),
     .         NPCX(0:NN2+NBAND) , NPCY(0:NN2+NBAND) ,
     .         NPCZ(0:NN2+NBAND)       , IPCX(NN2+NBAND)   , 
     .         IPCY(NN2+NBAND)   , IPCZ(NN2+NBAND))

C--------------------------------------------------
C     CLASSEMENT DES BUCKETS X
C--------------------------------------------------
C 
C---      bande NBX uniquement. 
C 
      DO IB=0,NBX+1
       NPX(IB)=0
      ENDDO
      DO N=1,NN1       
         NBOX=NOBX(N)+1
         IF(NBOX >= 1.AND.NBOX <= NBX+1)THEN
           NPX(NBOX)=NPX(NBOX)+1
         ENDIF
      ENDDO
      DO IB=1,NBX+1
         NPX(IB)=NPX(IB)+NPX(IB-1)
      ENDDO
      DO IB=NBX+1,1,-1
          NPX(IB)=NPX(IB-1)
      ENDDO
      DO N=1,NN1
        NBOX=NOBX(N)+1
C      bande NBX uniquement.
        IF(NBOX >= 1.AND.NBOX <= NBX+1)THEN
           NPX(NBOX)=NPX(NBOX)+1
           IPX(NPX(NBOX))=N       
        ENDIF
      ENDDO  
C
C    Cnode bande nbx 
C  
      DO IB=0,NBX+1
         NPCX(IB)=0
      ENDDO
      DO N=1,NN2      
          NBOX=NOBCX(N)+1
          IF(NBOX >= 1.AND.NBOX <= NBX+1)THEN
           NPCX(NBOX)=NPCX(NBOX)+1
          ENDIF
      ENDDO
      DO IB=1,NBX+1
          NPCX(IB)=NPCX(IB)+NPCX(IB-1)
      ENDDO
      DO IB=NBX+1,1,-1
         NPCX(IB)=NPCX(IB-1)
      ENDDO
      DO N=1,NN2
         NBOX=NOBCX(N)+1
C      bande NBX uniquement.
         IF(NBOX >= 1.AND.NBOX <= NBX+1)THEN
           NPCX(NBOX)=NPCX(NBOX)+1
           IPCX(NPCX(NBOX))=N               
        ENDIF
      ENDDO   
C-----
      DO IBX=1,NBX+1
         IBOITE = 0
         DO KP= NPCX(IBX-1)+1,NPCX(IBX)
             IF(IPCX(KP)> 0)IBOITE =1  
         ENDDO 
C
         IF(IBOITE > 0) THEN   
              DO IBY=0,NBY+1
                 NPY(IBY)=0
              ENDDO  
              DO KS=NPX(MAX(IBX-2,0))+1,NPX(MIN(IBX+1,NBX+1))
                 N  =IPX(KS)
                 NBOY=NOBY(N)+1           
C       bande NBY uniquement.
                 IF(NBOY >= 1 .AND. NBOY <= NBY+1)THEN
                     NPY(NBOY)=NPY(NBOY)+1
                 ENDIF
             ENDDO
             DO IBY=1,NBY+1
                 NPY(IBY)=NPY(IBY)+NPY(IBY-1)
             ENDDO 
             DO IBY=NBY+1,1,-1
                 NPY(IBY)=NPY(IBY-1)
             ENDDO               
             DO KS=NPX(MAX(IBX-2,0))+1,NPX(MIN(IBX+1,NBX+1))
                  N  =IPX(KS)      
                  NBOY=NOBY(N)+1
C       bande NBY uniquement.
                  IF(NBOY >= 1 .AND. NBOY <= NBY+1)THEN
                       NPY(NBOY)=NPY(NBOY)+1
                       IPY(NPY(NBOY))=N 
                  ENDIF
             ENDDO  
C 
C  Cnode Bande Y
C
             DO IBY=0,NBY+1
                 NPCY(IBY)=0
             ENDDO
             DO KS=NPCX(IBX-1)+1,NPCX(IBX)
                 N  =IPCX(KS)      
                 NBOY=NOBCY(N)+1
C       bande NBY uniquement.
                 IF(NBOY >= 1.AND.NBOY <= NBY+1)THEN
                     NPCY(NBOY)=NPCY(NBOY)+1
                 ENDIF
             ENDDO
C             
             DO IBY=1,NBY+1
                  NPCY(IBY)=NPCY(IBY)+NPCY(IBY-1)
             ENDDO
C
             DO IBY=NBY+1,1,-1
                  NPCY(IBY)=NPCY(IBY-1)
             ENDDO
             DO KS=NPCX(IBX-1)+1,NPCX(IBX)
                  N  =IPCX(KS)      
                  NBOY=NOBCY(N)+1
C       bande NBY uniquement.
                  IF(NBOY >= 1.AND. NBOY <= NBY+1)THEN
                     NPCY(NBOY)=NPCY(NBOY)+1
                     IPCY(NPCY(NBOY))=N         
                  ENDIF
             ENDDO
C        
C -- les boites suivantes z
C 
             DO IBY=1,NBY+1   
                IBOITE = 0   
                DO KP= NPCY(IBY-1)+1,NPCY(IBY)
                    IF(IPCY(KP) > 0)IBOITE = 1
                ENDDO
C                    
                IF(IBOITE > 0) THEN            
                  DO IBZ=0,NBZ+1
                      NPZ(IBZ)=0
                  ENDDO
                  DO KS=NPY(MAX(IBY-2,0))+1,NPY(MIN(IBY+1, NBY+1))
                      N  =IPY(KS) 
                      NBOZ=NOBZ(N)+1
C        bande NBZ uniquement.
                      IF(NBOZ >= 1.AND.NBOZ <= NBZ+1)THEN
                           NPZ(NBOZ)=NPZ(NBOZ)+1
                      ENDIF
                  ENDDO
                  DO IBZ=1,NBZ+1
                      NPZ(IBZ)=NPZ(IBZ)+NPZ(IBZ-1)
                  ENDDO
                  DO IBZ=NBZ+1,1,-1
                      NPZ(IBZ)=NPZ(IBZ-1)
                  ENDDO
                  DO KS=NPY(MAX(IBY-2,0))+1,NPY(MIN(IBY+1, NBY+1))
                      N  =IPY(KS)
                      NBOZ=NOBZ(N)+1
C        bande NBZ uniquement.
                      IF(NBOZ >= 1 .AND. NBOZ <= NBZ+1)THEN
                           NPZ(NBOZ)=NPZ(NBOZ)+1
                           IPZ(NPZ(NBOZ))=N   
                      ENDIF  
                  ENDDO
C
C  Cnode Bande Z
C
                  DO IBZ=0,NBZ+1
                     NPCZ(IBZ)=0
                  ENDDO
                  DO KS=NPCY(IBY-1)+1,NPCY(IBY)
                     N  =IPCY(KS) 
                     NBOZ=NOBCZ(N)+1
                     IF(NBOZ >= 1.AND.NBOZ <= NBZ+1)THEN
                          NPCZ(NBOZ)=NPCZ(NBOZ)+1
                     ENDIF
                  ENDDO
                  DO IBZ=1,NBZ+1
                       NPCZ(IBZ)=NPCZ(IBZ)+NPCZ(IBZ-1)
                  ENDDO
                  DO IBZ=NBZ+1,1,-1
                       NPCZ(IBZ)=NPCZ(IBZ-1)
                  ENDDO
                  DO KS=NPCY(IBY-1)+1,NPCY(IBY)
                      N  =IPCY(KS)      
                      NBOZ=NOBCZ(N)+1
                      IF(NBOZ >= 1.AND. NBOZ <= NBZ+1)THEN
                           NPCZ(NBOZ)=NPCZ(NBOZ)+1
                           IPCZ(NPCZ(NBOZ))=N
                      ENDIF  
                  ENDDO
C
C ---recherche cnode par boite tt d'abord
C
                  DVOIS = HUGE(DVOIS)
                  DO IBZ=1,NBZ+1
                     DO KP= NPCZ(IBZ-1)+1,NPCZ(IBZ) 
                       IF(IPCZ(KP) > 0) THEN
                         DO KS=NPZ(MAX(IBZ-2,0))+1,NPZ(MIN(IBZ+1,NBZ+1))                   
                            IF (FLAG == 1) THEN
C----------------------------------------------------------------------------------------
C--                           Merging with cnodes - cnode is destination for closest node
C----------------------------------------------------------------------------------------
                              NC =IPCZ(KP) 
                              NS =IPZ(KS)       
                              IG = USRTOSC(ITABC(NC),ITABM1)
                              XI =X(1,IG)
                              YI =X(2,IG)
                              ZI =X(3,IG)
                              DMERGE = CMERGE(NC)*CMERGE(NC)  
                              JG=USRTOS(ITAB(NS),ITABM1)
                              XJ =(X(1,JG)-XI)
                              YJ =(X(2,JG)-YI)
                              ZJ =(X(3,JG)-ZI)               
                              DIST2=XJ**2 + YJ**2 + ZJ**2
                              IF(ITABC(NC)/=ITAB(NS).AND.DIST2<=DMERGE)THEN
                                IF(IMERGE0(NC) == 0) THEN
                                   IMERGE0(NC) = ITAB(NS)
                                   DVOIS = DIST2
                                ELSEIF(DIST2 < DVOIS)THEN
                                   IMERGE0(NC) = ITAB(NS)
                                   DVOIS = DIST2
                                ENDIF
                              ENDIF
                            ELSE
C---------------------------------------------------------------------------------------------
C--                           Merging with nodes - node with lowest user id is the destination
C---------------------------------------------------------------------------------------------
                              NC = LIST2(IPCZ(KP))
                              NS = LIST1(IPZ(KS))  
                              IG=USRTOS(ITAB(NC),ITABM1)
                              XI =X(1,IG)
                              YI =X(2,IG)
                              ZI =X(3,IG)
                              JG=USRTOS(ITAB(NS),ITABM1)
                              XJ =(X(1,JG)-XI)
                              YJ =(X(2,JG)-YI)
                              ZJ =(X(3,JG)-ZI)               
                              DIST2=XJ**2 + YJ**2 + ZJ**2
C
                              CALL DECODE_MERGE(LIST2_IDMERGE(IPCZ(KP)),LIST2_NBMERGE(IPCZ(KP)),TABC,NB_MERGE_NODE)
                              CALL DECODE_MERGE(LIST1_IDMERGE(IPZ(KS)),LIST1_NBMERGE(IPZ(KS)),TABS,NB_MERGE_NODE)
C
                              TAG = 0
                              DMERGE = ZERO
                              DO J=1,LIST2_NBMERGE(IPCZ(KP))
                                DO K=1,LIST1_NBMERGE(IPZ(KS))
                                  IF (ABS(TABC(J)) == TABS(K)) THEN
C--                                 pair is retained only if referred by same merge/node
C--                                 tabc < 0 if merge_type = 2 - criteria itab(ns) < itab(nc) not used
                                    DMERGE = CMERGE(TABS(K))*CMERGE(TABS(K))
                                    IF ((ITAB(NS)>ITAB(NC)).OR.((TABC(J)<0).AND.(NS/=NC))) THEN
                                      IF (DIST2<=DMERGE) THEN
                                        IF(IMERGE0(IPZ(KS)) == 0) THEN
                                          IMERGE0(IPZ(KS)) = ITAB(NC)
                                          DDD(IPZ(KS)) = DIST2
                                        ELSEIF(IMERGE0(IPZ(KS)) > ITAB(NC))THEN
                                          IMERGE0(IPZ(KS)) = ITAB(NC)
                                          DDD(IPZ(KS)) = DIST2
                                        ENDIF
                                      ENDIF  
                                    ENDIF
                                  ENDIF
                                ENDDO
                              ENDDO                             
C                             
                            ENDIF
                          ENDDO   
                        ENDIF           
                      ENDDO 
                    ENDDO
                ENDIF          
              ENDDO
            ENDIF 
          ENDDO
       DEALLOCATE(NOBX)
       DEALLOCATE(NOBY)
       DEALLOCATE(NOBZ)
       DEALLOCATE(NOBCX)
       DEALLOCATE(NOBCY)
       DEALLOCATE(NOBCZ)
       DEALLOCATE(TABS)
       DEALLOCATE(TABC)
C--------
      RETURN
      END

!||====================================================================
!||    decode_merge          ../starter/source/elements/nodes/merge_bucket_search.F
!||--- called by ------------------------------------------------------
!||    merge_bucket_search   ../starter/source/elements/nodes/merge_bucket_search.F
!||====================================================================
      SUBROUTINE DECODE_MERGE(CODE,NVAL,TAB,NB_MERGE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CODE,NVAL,TAB(*),NB_MERGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,CODE_TEMP,BASE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C
C--   decode id merge
C
      BASE = 2*NB_MERGE
C
      TAB(1:NB_MERGE) = 0 
C   
      CODE_TEMP = CODE
      DO I=1,NVAL
        TAB(I) = CODE_TEMP / (BASE**(NVAL-I))
C        TAB(I) = FLOOR(DIV)
        CODE_TEMP = CODE_TEMP - (BASE**(NVAL-I))*TAB(I)
        IF (TAB(I) > NB_MERGE) TAB(I) = -(TAB(I)-NB_MERGE)
      ENDDO                   
C
      END SUBROUTINE DECODE_MERGE
