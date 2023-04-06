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
Chd|  HM_READ_CLUSTER               source/output/cluster/hm_read_cluster.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        ITRIMHPSORT                   source/output/cluster/itrimhpsort.F
Chd|        CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_CLUSTER(CLUSTERS,UNITAB ,ISKN    ,IGRBRIC,IGRSPRING,
     .                           IXS     ,IXR    ,NOM_OPT ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE CLUSTER_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
#include      "scr17_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ISKN(LISKN,*),
     .   IXS(NIXS,NUMELS),IXR(NIXR,NUMELR),NOM_OPT(LNOPT1,*)
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTERS
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(NSUBMOD)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,II,JJ,KK,N,CLID,UID,ICLUS,IAD,IG,IGR,ISK,IFAIL,
     .   IEL,NEL,NNOD,IFLAGUNIT,VAL,NNOD0,ITY,NELMAX
      INTEGER NOD1(2000),NOD2(2000),SUB_INDEX,ISK_L
      my_real
     .   BID(1),AX(4),NX(4),FMAX(2),MMAX(2)
      CHARACTER TITR*nchartitle
      CHARACTER KEY *ncharfield
      PARAMETER (NELMAX = 500)
      LOGICAL :: IS_AVAILABLE,FOUND
C=======================================================================
      IS_AVAILABLE = .FALSE.
      
C--------------------------------------------------
C START BROWSING MODEL CLUSTER
C--------------------------------------------------
c
      CALL HM_OPTION_START('/CLUSTER')
c
C--------------------------------------------------
C EXTRACT DATAS
C--------------------------------------------------
      DO I=1,NCLUSTER
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = CLID,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_INDEX = SUB_INDEX,
     .                       KEYWORD2  = KEY,
     .                       OPTION_TITR = TITR)
c
        NOM_OPT(1,I)=CLID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
c
        IFLAGUNIT = 0                             
        IF (UID > 0) THEN                         
          DO J=1,NUNITS                           
            IF (UNITAB%UNIT_ID(J) == UID) THEN    
              IFLAGUNIT = 1                       
              EXIT                                
            ENDIF                                 
          ENDDO                                   
        ENDIF
        IF (UID / = 0 .and. IFLAGUNIT == 0) THEN                        
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,      
     .                I1=CLID,I2=UID,C1='CLUSTER',                  
     .                C2='CLUSTER',                               
     .                C3=TITR)                                       
        ENDIF                                                        
c----------------------------
Card1
        CALL HM_GET_INTV  ('group_ID' ,IGR      ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV  ('skew_ID'  ,ISK      ,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV  ('ifail'    ,IFAIL    ,IS_AVAILABLE, LSUBMODEL)

Card2   Failure normal traction force
        
        CALL HM_GET_FLOATV('fn_fail1'      ,FMAX(1)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('scalefactor_a1',AX(1)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('scalefactor_b1',NX(1)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        
Card3   Failure tangent shear force
        
        CALL HM_GET_FLOATV('fs_fail'       ,FMAX(2)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('scalefactor_a2',AX(2)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('scalefactor_b2',NX(2)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        
Card4   Failure torsion moment
        
        CALL HM_GET_FLOATV('mt_fail'       ,MMAX(1)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('scalefactor_a3',AX(3)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('scalefactor_b3',NX(3)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        
Card5   Failure bending moment
        
        CALL HM_GET_FLOATV('mb_fail'       ,MMAX(2)  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('scalefactor_a4',AX(4)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('scalefactor_b4',NX(4)    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        
c----------------------------
        ISK_L = ISK
        IF (ISK > 0) THEN
          FOUND        = .FALSE.
          DO J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
            IF (ISK == ISKN(4,J+1)) THEN
              ISK = J+1
              FOUND = .TRUE.
              EXIT
            ENDIF
          ENDDO
          IF (.NOT.FOUND) THEN
            CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,     
     .              C1='CLUSTER',
     .              C2='CLUSTER',
     .              I2=ISK,
     .              I1=CLID,
     .              C3=TITR)       
          ENDIF
        ENDIF
        NOD1 = 0
        NOD2 = 0
        NNOD = 0
        NEL  = 0                                         
c----------------------------
c       BRICK CLUSTER
c----------------------------
        KK = NGRNOD+1
        IF (KEY(1:5) == 'BRICK') THEN     
          DO IG = 1,NGRBRIC                                         
            II = IG
            IF (IGR == IGRBRIC(IG)%ID .and. IGRBRIC(IG)%GRTYPE == 1) THEN
              NEL = IGRBRIC(IG)%NENTITY
              IF (NEL > NELMAX) THEN
                CALL ANCMSG(MSGID=1055,       
     .                      ANMODE=ANINFO,    
     .                      MSGTYPE=MSGERROR, 
     .                      I1=CLID,          
     .                      I2=NEL)  
                EXIT
              ENDIF
              CLUSTERS(I)%ID = CLID                                          
              CLUSTERS(I)%IGR = II                                           
              CLUSTERS(I)%TYPE = 1                                           
              CLUSTERS(I)%SKEW = ISK                                         
              CLUSTERS(I)%NEL  = NEL                                         
              CLUSTERS(I)%IFAIL= IFAIL                                       
              CLUSTERS(I)%OFF  = 1                                       
              CLUSTERS(I)%FAIL = ONE                                         
c
              ALLOCATE (CLUSTERS(I)%NG(NEL) )
              ALLOCATE (CLUSTERS(I)%ELEM(NEL) )                              
              DO IEL = 1,NEL
                JJ = IGRBRIC(IG)%ENTITY(IEL)
                CLUSTERS(I)%ELEM(IEL) = JJ            ! Sys number
c               CLUSTERS(I)%ELEM(IEL-IAD+1) = IXS(11,JJ)     ! Elem ID
                DO K=2,5                                                     
                  NNOD = NNOD+1                                              
                  NOD1(NNOD) = IXS(K  ,JJ)                                   
                  NOD2(NNOD) = IXS(K+4,JJ)                                   
                ENDDO                                                        
              ENDDO                                                          
c
              NNOD0=NNOD                                                     
              CALL ITRIMHPSORT(NOD1,NNOD)                                    
              NNOD = NNOD0                                                   
              CALL ITRIMHPSORT(NOD2,NNOD)                                    
              CLUSTERS(I)%NNOD = NNOD                                        
c
              ALLOCATE (CLUSTERS(I)%NOD1(NNOD))       
              ALLOCATE (CLUSTERS(I)%NOD2(NNOD))  
              DO K=1,NNOD                                                    
                CLUSTERS(I)%NOD1(K) = NOD1(K)                                
                CLUSTERS(I)%NOD2(K) = NOD2(K)                                
              END DO                                                         
c
              WRITE(IOUT,1001) CLID                                          
c
              EXIT  !  group found => exit loop                              
            ENDIF                                                            
          ENDDO     !  IG = 1,NGRBRIC                                        
c
        ELSEIF (KEY(1:6) == 'SPRING') THEN                                                 
c----------------------------
c         SPRING CLUSTER
c----------------------------
          KK = NGRNOD+NGRBRIC+NGRQUAD+NGRSHEL+NGRTRUS+NGRBEAM + 1
          DO IG = 1,NGRSPRI                                                 
            II = KK+IG-1                                                     
            IF (IGR == IGRSPRING(IG)%ID .and. IGRSPRING(IG)%GRTYPE == 6) THEN
              NEL = IGRSPRING(IG)%NENTITY
              IF (NEL > NELMAX) THEN
                CALL ANCMSG(MSGID=1055,       
     .                      ANMODE=ANINFO,    
     .                      MSGTYPE=MSGERROR, 
     .                      I1=CLID,          
     .                      I2=NEL)  
                EXIT
              ENDIF
              CLUSTERS(I)%ID = CLID                                          
              CLUSTERS(I)%IGR = II
              CLUSTERS(I)%TYPE = 2                                      
              CLUSTERS(I)%SKEW = ISK                                         
              CLUSTERS(I)%NEL  = NEL                                         
              CLUSTERS(I)%IFAIL= IFAIL                                         
              CLUSTERS(I)%OFF  = 1                                       
              CLUSTERS(I)%FAIL = ONE                                         
c
              ALLOCATE (CLUSTERS(I)%NG(NEL) )                                
              ALLOCATE (CLUSTERS(I)%ELEM(NEL) )                              
              DO IEL = 1,NEL
                JJ = IGRSPRING(IG)%ENTITY(IEL)
                CLUSTERS(I)%ELEM(IEL) = JJ !IXR(6,JJ)  ! Elem ID
                NNOD = NNOD+1                                                
                NOD1(NNOD) = IXR(2,JJ)                                       
                NOD2(NNOD) = IXR(3,JJ)                                       
              ENDDO                                                          
c
              NNOD0=NNOD                                                     
              CALL ITRIMHPSORT(NOD1,NNOD)                                    
              NNOD = NNOD0                                                   
              CALL ITRIMHPSORT(NOD2,NNOD)                                    
              CLUSTERS(I)%NNOD = NNOD                                        
c
              ALLOCATE (CLUSTERS(I)%NOD1(NNOD) )                             
              ALLOCATE (CLUSTERS(I)%NOD2(NNOD) )                             
              DO K=1,NNOD                                                    
                CLUSTERS(I)%NOD1(K) = NOD1(K)                                
                CLUSTERS(I)%NOD2(K) = NOD2(K)                                
              END DO                                                         
c
              WRITE(IOUT,1002) CLID                                          
c
              EXIT  !  group found => exit loop                              
            ENDIF                                                           
          ENDDO     !  IG = 1,NGRSPRI                                       
        ENDIF
c--------------------------
        IF (NEL == 0)  THEN
          CALL ANCMSG(MSGID=1054,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                I1=CLID,
     .                I2=IGR)
          CYCLE
        ENDIF
c--------------------------
        IF (IFAIL > 0) THEN                                               
          IF (FMAX(1) == ZERO) FMAX(1) = INFINITY
          IF (FMAX(2) == ZERO) FMAX(2) = INFINITY
          IF (MMAX(1) == ZERO) MMAX(1) = INFINITY
          IF (MMAX(2) == ZERO) MMAX(2) = INFINITY
          ALLOCATE (CLUSTERS(I)%FMAX(2) )                                  
          ALLOCATE (CLUSTERS(I)%MMAX(2) )                                  
          CLUSTERS(I)%FMAX(1) = FMAX(1)                                         
          CLUSTERS(I)%FMAX(2) = FMAX(2)                                         
          CLUSTERS(I)%MMAX(1) = MMAX(1)                                         
          CLUSTERS(I)%MMAX(2) = MMAX(2)                                         
        ELSE                                      
          ALLOCATE (CLUSTERS(I)%FMAX(0) )                                  
          ALLOCATE (CLUSTERS(I)%MMAX(0) )                                  
        ENDIF                                                              
        IF (IFAIL == 3) THEN                                                  
          ALLOCATE (CLUSTERS(I)%AX(4) )                                    
          ALLOCATE (CLUSTERS(I)%NX(4) )             
          IF (AX(1) == ZERO) AX(1) = ONE           
          IF (AX(2) == ZERO) AX(2) = ONE           
          IF (AX(3) == ZERO) AX(3) = ONE           
          IF (AX(4) == ZERO) AX(4) = ONE           
          IF (NX(1) == ZERO) NX(1) = ONE           
          IF (NX(2) == ZERO) NX(2) = ONE           
          IF (NX(3) == ZERO) NX(3) = ONE           
          IF (NX(4) == ZERO) NX(4) = ONE           
          CLUSTERS(I)%AX(1) = AX(1)                                         
          CLUSTERS(I)%AX(2) = AX(2)                                         
          CLUSTERS(I)%AX(3) = AX(3)                                         
          CLUSTERS(I)%AX(4) = AX(4)                                         
          CLUSTERS(I)%NX(1) = NX(1)                                         
          CLUSTERS(I)%NX(2) = NX(2)                                         
          CLUSTERS(I)%NX(3) = NX(3)                                         
          CLUSTERS(I)%NX(4) = NX(4)                                         
        ELSE                                        
          ALLOCATE (CLUSTERS(I)%AX(0) )                                    
          ALLOCATE (CLUSTERS(I)%NX(0) )             
        ENDIF                                                                
c-----
        WRITE(IOUT,2000) IGR, ISK_L, NEL, NNOD*2, IFAIL            
        IF (IFAIL > 0)  WRITE(IOUT,2001) FMAX(1),FMAX(2),MMAX(1),MMAX(2)
        IF (IFAIL == 3) WRITE(IOUT,2002) AX(1),AX(2),AX(3),AX(4),
     .                                   NX(1),NX(2),NX(3),NX(4) 
       
        WRITE(IOUT,9000)        
c-----
      ENDDO  !  I=1,NCLUSTER
C-------------------------------------
 1001 FORMAT(/
     & 5X,'SPOTWELD CLUSTER OF BRICK ELEMENTS,  ID=',I10)
 1002 FORMAT(/
     & 5X,'SPOTWELD CLUSTER OF SPRING ELEMENTS, ID=',I10)
 2000 FORMAT(
     & 10X,'ELEMENT GROUP ID. . . . . . . . . . . . .=',I10/,
     & 10X,'SKEW ID . . . . . . . . . . . . . . . . .=',I10/,
     & 10X,'NUMBER OF ELEMENTS. . . . . . . . . . . .=',I10/,
     & 10X,'NUMBER OF NODES . . . . . . . . . . . . .=',I10/,
     & 10X,'FAILURE FLAG  . . . . . . . . . . . . . .=',I10)
 2001 FORMAT(
     & 10X,'MAX NORMAL FORCE. . . . . . . . . . . . .=',1PG20.13/,
     & 10X,'MAX TANGENT FORCE . . . . . . . . . . . .=',1PG20.13/,
     & 10X,'MAX TORSION MOMENT. . . . . . . . . . . .=',1PG20.13/,
     & 10X,'MAX BENDING MOMENT. . . . . . . . . . . .=',1PG20.13)
 2002 FORMAT(
     & 10X,'FAILURE COEFFICIENT A1. . . . . . . . . .=',1PG20.13/,
     & 10X,'FAILURE COEFFICIENT A2. . . . . . . . . .=',1PG20.13/,
     & 10X,'FAILURE COEFFICIENT A3. . . . . . . . . .=',1PG20.13/,
     & 10X,'FAILURE COEFFICIENT A4. . . . . . . . . .=',1PG20.13/,
     & 10X,'FAILURE EXPONENT N1 . . . . . . . . . . .=',1PG20.13/,
     & 10X,'FAILURE EXPONENT N2 . . . . . . . . . . .=',1PG20.13/,
     & 10X,'FAILURE EXPONENT N3 . . . . . . . . . . .=',1PG20.13/,
     & 10X,'FAILURE EXPONENT N4 . . . . . . . . . . .=',1PG20.13)
 9000 FORMAT(10X/)
C-----------
      RETURN
  998 CALL ANCMSG(MSGID=55,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=KEY0(KCUR),
     .            C2=KLINE,
     .            C3=LINE)
C-----------
      RETURN
      END
