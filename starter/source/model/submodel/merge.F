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
Chd|  MERGE                         source/model/submodel/merge.F 
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MERGE_CNOD_CNOD               source/model/submodel/merge_cnod_cnod.F
Chd|        USRTOS                        source/system/sysfus.F        
Chd|        USRTOSC                       source/model/submodel/merge.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MERGE(X      ,ITAB   ,ITABM1 ,CMERGE ,IMERGE,
     .                 IMERGE2,IADMERGE2,NMERGE_TOT)
      USE MESSAGE_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(NUMNOD), ITABM1(2*NUMNOD),IMERGE(*),
     .        IMERGE2(NUMNOD+1),IADMERGE2(NUMNOD+1),NMERGE_TOT
      TARGET  ITAB
      my_real
     .        X(3,NUMNOD),CMERGE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,M,N,I1,IB,IG,JG,J1,JK,KK,KP,N1,N2,NC,NS,NN,NM,
     .      IBZ1,IBZ,IBY1,IBY,IBX1,IBX,KS,NUMNOD1,NUMCNOD1,
     .      NBOX,NBOY,NBOZ,NBX,NBY,NBZ,NBAND,IBOITE
C
      INTEGER 
     .    NOBX(NUMNOD),NOBY(NUMNOD),NOBZ(NUMNOD),
     .    NOBCX(NUMCNOD),NOBCY(NUMCNOD),NOBCZ(NUMCNOD),
     .    LBUF(NUMNOD),IADMERGE2TMP(NUMNOD+1)
C 
      INTEGER, DIMENSION(:),POINTER     :: ITABC
      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .    NPX,IPX,NPY,IPY,NPZ,IPZ,NPCX,IPCX,NPCY,IPCY,NPCZ,IPCZ,IMERGE0
C 
      my_real
     .        XI, YI, ZI, XJ, YJ, ZJ, DK,
     .        DIST2,DVOIS,DBUC,EPS,XMIN,XMAX,YMIN,YMAX,ZMIN,
     .        ZMAX,DMX,DMY,DMZ,DMERGE,DDD(NUMCNOD)
C-----------------------------------------------
       INTEGER 
     .         USRTOS,USRTOSC
      EXTERNAL USRTOS,USRTOSC
C=======================================================================

C-------    dbuc------------------------------
      DO  N=1,2*NUMCNOD
        IMERGE(N) = 0
      END DO 
      DO  N=1,NUMNOD+1
        IMERGE2(N) = 0
        IADMERGE2(N) = 0
      END DO 
C     
      NUMNOD1 = NUMNOD0-NUMCNOD
      ITABC => ITAB(NUMNOD1+1:NUMNOD0)
      DBUC = ZERO 
      DO N=1,NUMCNOD       
        NN  = USRTOSC(ITABC(N),ITABM1)
        DBUC = MAX(DBUC,CMERGE(N))  
      ENDDO    
C 
      DBUC = TWO*DBUC
      EPS=EM3*DBUC 
      XMIN=EP30
      XMAX=-EP30
      YMIN=EP30
      YMAX=-EP30
      ZMIN=EP30
      ZMAX=-EP30    
C
      DO N=1,NUMNOD0
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
      DO N=1,NUMNOD1   
        NN  = USRTOS(ITAB(N),ITABM1)
        NOBX(N) = (X(1,NN)-XMIN)/DBUC
        NOBY(N) = (X(2,NN)-YMIN)/DBUC
        NOBZ(N) = (X(3,NN)-ZMIN)/DBUC
       ENDDO
C  
      DO N=1,NUMCNOD       
        NN  = USRTOSC(ITABC(N),ITABM1)
        NOBCX(N) = (X(1,NN)-XMIN)/DBUC
        NOBCY(N) = (X(2,NN)-YMIN)/DBUC
        NOBCZ(N) = (X(3,NN)-ZMIN)/DBUC
      ENDDO 
C  
      NBAND = MAX(NBX, NBY,NBZ) + 1
C      
      ALLOCATE( NPX(0:NUMNOD1+NBAND  ) , NPY(0:3*(NUMNOD1+NBAND)), 
     .         NPZ(0:9*(NUMNOD1+NBAND)) , IPX(NUMNOD1+NBAND)   ,
     .         IPY(NUMNOD1+NBAND)     , IPZ(NUMNOD1+NBAND),
     .         NPCX(0:NUMCNOD+NBAND) , NPCY(0:NUMCNOD+NBAND) ,
     .         NPCZ(0:NUMCNOD+NBAND)       , IPCX(NUMCNOD+NBAND)   , 
     .         IPCY(NUMCNOD+NBAND)   , IPCZ(NUMCNOD+NBAND),
     .         IMERGE0(NUMCNOD))
      IMERGE0(1:NUMCNOD) = 0
C--------------------------------------------------
C     CLASSEMENT DES BUCKETS X
C--------------------------------------------------
C 
C---      bande NBX uniquement. 
C 
      DO IB=0,NBX+1
       NPX(IB)=0
      ENDDO
      DO N=1,NUMNOD1       
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
      DO N=1,NUMNOD1
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
      DO N=1,NUMCNOD       
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
      DO N=1,NUMCNOD
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
                  DO IBZ=1,NBZ+1
                     DO KP= NPCZ(IBZ-1)+1,NPCZ(IBZ) 
                       NC =IPCZ(KP) 
                       IF(NC > 0) THEN
                         DO KS=NPZ(MAX(IBZ-2,0))+1,NPZ(MIN(IBZ+1,NBZ+1)) 
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
                            DDD(NC) = DIST2
                           IF(ITABC(NC)/=ITAB(NS).AND.DIST2<=DMERGE)THEN
                              IF(IMERGE0(NC) == 0) THEN
                                  IMERGE0(NC) = ITAB(NS)
                                  DVOIS = DIST2
                                  DDD(NC) = DIST2
                              ELSEIF(DIST2 < DVOIS)THEN
                                  IMERGE0(NC) = ITAB(NS)
                                  DVOIS = DIST2
                                  DDD(NC) = DIST2
                              ENDIF
                            ENDIF         
                          ENDDO   
                        ENDIF           
                      ENDDO 
                      
                    ENDDO
                   
                ENDIF          
              ENDDO
            ENDIF 
          ENDDO
C-------------------------------------------------- 
C     COMPACT IMERGE -> No systeme                           
C-------------------------------------------------- 
      NM = 0                                  
      DO I= 1,NUMCNOD 
        IF (IMERGE0(I) > 0) THEN
          NM = NM+1                          
          IMERGE(NMERGE_TOT+NM) = USRTOSC(IMERGE0(I),ITABM1)
          IMERGE(NM)         = USRTOSC(ITABC(I) ,ITABM1)
!        ELSE
!          CALL ANCMSG(MSGID=739,
!     .                MSGTYPE=MSGWARNING,
!     .                ANMODE=ANINFO_BLIND_1,
!     .                I1=ITABC(I))
        ENDIF                                 
      ENDDO                                   
      NMERGED = NM                           
C--------------------------------------------------
C     TAB ID_NODE systeme  -> ID_CNODE  systeme               
C--------------------------------------------------
      IF (NM > 0) THEN
        LBUF = 0
        DO I = 1,NUMCNOD
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
        DO I = 1,NUMCNOD
          IF (IMERGE(NMERGE_TOT+I) > 0) THEN
            N = IMERGE(NMERGE_TOT+I)
            IMERGE2(IADMERGE2TMP(N)) = IMERGE(I)
            IADMERGE2TMP(N)=IADMERGE2TMP(N)+1
          ENDIF  
        ENDDO
      ENDIF
C--------------------------------------------------
      WRITE(IOUT,'(//A/A//A/)')TITRE(114),TITRE(115),TITRE(116)    
      J=0                                                            
      DO  N=1,NMERGED,50                                             
        J=J+50                                                       
        J=MIN(J,NMERGED)
        DO  I=N,J
          WRITE(IOUT,'(5X,I10,8X,I10)') 
     .          ITAB(IMERGE(I)),ITAB(IMERGE(NMERGE_TOT+I))
        ENDDO                                                       
      ENDDO    
C-------- 
       DEALLOCATE(NPX   ,NPY   ,NPZ   ,IPX   ,IPY   ,IPZ  ,
     .            NPCX  ,NPCY  ,NPCZ  ,IPCX  ,IPCY  ,IPCZ )
C--------
C   Merging CNODE to CNODE
C--------
      CALL MERGE_CNOD_CNOD(X      ,ITAB     ,ITABM1 ,CMERGE ,IMERGE,
     .                     IMERGE2,IADMERGE2,IMERGE0,NMERGE_TOT)
      DEALLOCATE(IMERGE0)
C--------
      RETURN
      END
C------------------------------------------------------------
Chd|====================================================================
Chd|  USRTOSC                       source/model/submodel/merge.F 
Chd|-- called by -----------
Chd|        MERGE                         source/model/submodel/merge.F 
Chd|        MERGE_BUCKET_SEARCH           source/elements/nodes/merge_bucket_search.F
Chd|        MERGE_CNOD_CNOD               source/model/submodel/merge_cnod_cnod.F
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION USRTOSC(IU,ITABM1)
C      IDENTIQUE A USR2SYS, SANS GENERER D'ERREUR
C      FONCTION DONNE N0 SYSTEME DU NOEUD USER IU
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IU
      INTEGER ITABM1(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JINF, JSUP, J
      JINF=1
      JSUP=NUMNOD0
      J=MAX(1,NUMCNOD/2)
   10 IF(JSUP<=JINF.AND.(IU-ITABM1(J))/=0) THEN
        USRTOSC=0
        RETURN
      ENDIF
      IF((IU-ITABM1(J))==0)THEN
C     >CAS IU=TABM FIN DE LA RECHERCHE
         USRTOSC=ITABM1(J+NUMNOD0)
         RETURN
      ELSE IF (IU-ITABM1(J)<0) THEN
C     >CAS IU<TABM
         JSUP=J-1
      ELSE
C     >CAS IU>TABM
         JINF=J+1
      ENDIF
      J=(JSUP+JINF)/2
      GO TO 10
      END
