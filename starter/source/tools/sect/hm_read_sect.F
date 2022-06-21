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
Chd|  LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        SECSTRI                       source/tools/sect/hm_read_sect.F
Chd|        SEC_NODES                     source/tools/sect/hm_read_sect.F
Chd|        SEC_NODES_SOL                 source/tools/sect/hm_read_sect.F
Chd|        SEC_TRI                       source/tools/sect/hm_read_sect.F
Chd|        SUBROTPOINT                   source/model/submodel/subrot.F
Chd|        SUBROTVECT                    source/model/submodel/subrot.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        ZERORE                        source/system/zerore.F        
Chd|        ELEGROR                       source/groups/elegror.F       
Chd|        NODGRNR5                      source/starter/freform.F      
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LECSEC42(IXS   ,IXQ   ,IXC    ,IXT    ,IXP    ,IXR    ,
     2                    IXTG  ,X0    ,NSTRF  ,ITAB   ,ITABM1 ,
     3                    IGRNOD,SECBUF ,
     4                    IPARI ,IXS10 ,IXS20  ,IXS16  ,UNITAB ,
     5                    ISKN  ,XFRAME,ISOLNOD,NOM_SECT,RTRANS,
     6                    LSUBMODEL,NOM_OPT,IGRBRIC,IGRQUAD,IGRSH4N,
     7                    IGRTRUSS,IGRBEAM,IGRSPRING,IGRSH3N,SEATBELT_SHELL_TO_SPRING,
     8                    NB_SEATBELT_SHELLS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE R2R_MOD
      USE SUBMODEL_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "sysunit.inc"
#include      "sphcom.inc"
#include      "r2r_c.inc"
#include      "submod_c.inc"
#include      "tabsiz_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IXC(NIXC,*), IXTG(NIXTG,*), NSTRF(*), ITAB(*),
     .        ITABM1(*),IXS(NIXS,*), IXQ(NIXQ,*), IXT(NIXT,*),
     .        IXP(NIXP,*), IXR(NIXR,*), IPARI(NPARI,*),
     .        IXS10(6,*),IXS20(12,*),IXS16(8,*),ISKN(LISKN,*),
     .        ISOLNOD(*),NOM_SECT(*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      my_real
     .   X0(3,*),SECBUF(*),XFRAME(NXFRAME,*),
     .   RTRANS(NTRANSF,*)
      INTEGER,INTENT(IN):: NB_SEATBELT_SHELLS
      INTEGER,INTENT(IN)::SEATBELT_SHELL_TO_SPRING(NUMELC,2)
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRNOD)   :: IGRNOD
      TYPE (GROUP_)  ,DIMENSION(NGRBRIC)  :: IGRBRIC
      TYPE (GROUP_)  ,DIMENSION(NGRQUAD)  :: IGRQUAD
      TYPE (GROUP_)  ,DIMENSION(NGRSHEL)  :: IGRSH4N
      TYPE (GROUP_)  ,DIMENSION(NGRTRUS)  :: IGRTRUSS
      TYPE (GROUP_)  ,DIMENSION(NGRBEAM)  :: IGRBEAM
      TYPE (GROUP_)  ,DIMENSION(NGRSPRI)  :: IGRSPRING
      TYPE (GROUP_)  ,DIMENSION(NGRSH3N)  :: IGRSH3N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ,DIMENSION(NSECT) :: SECTIDS
      INTEGER K1, I, J, L, KK, K2, K,LREC,
     .   NNOD, NBINTER,K0,K3,K4,K5,K6,K7,K8,K9,KR0,
     .   NSEGQ,NSEGS,NSEGC,NSEGT,NSEGP,NSEGR,NSEGTG,I0,ID,
     .   IGU,IGS,IGUS,IGUQ,IGUC,IGUT,IGUP,IGUR,IGUTG,IFRAM,
     .   NNSK1,NNSK2,NNSK3,UID,IFLAGUNIT,IE,IADV,
     .   TAGNODES(NUMNOD*2+NPART),ISU,NFRAM,JJ,IUN,
     .   TAGELEM1,TAGELEM2,TAGELEM3,
     .   TAGNDOUBL(NUMNOD),N1,CPT,
     .   TAGELEMS(1+NUMELC+NUMELS+NUMELT+NUMELQ+NUMELP+
     .   NUMELR+NUMELTG),NG,NOPRINT
      INTEGER L0,ISTYP,SUB_ID,NODTAG(NUMNOD),ILEN
      my_real
     .   DELTAT,ALPHA,FAC_T,A,B,C,D,E,F,POS,R,MAXDT
      CHARACTER MESS*40,TITR*nchartitle,CHAR8*ncharline
      CHARACTER KEY2*ncharfield
      my_real
     .   BID, XM, YM, ZM, X1, Y1, Z1, X2, Y2, Z2, NORM,
     .   X3, Y3, Z3, N3, PNOR1, PNOR2, PNORM1, DET, DET1, DET2, DET3
      LOGICAL :: IS_AVAILABLE
      INTEGER :: NUMSECT,SNSTRF1
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,NODGRNR5,ELEGROR,ELEGROR_SEATBELT
      EXTERNAL USR2SYS,NODGRNR5,ELEGROR,ELEGROR_SEATBELT
C
C
      DATA MESS/'SECTION DEFINITION                      '/
      DATA IUN/1/
C=======================================================================
      SNSTRF1 = 0
      NOPRINT = 0
      NFRAM = 0
      NODTAG = 0
      TAGELEMS  = 0
C GENERIC WRITE FLAG
      NSTRF(1)=0
C GENERIC READ FLAG
      NSTRF(2)=0
C file flip/flop
      NSTRF(3)=0
C file run number
      NSTRF(4)=1
C file next run number
      NSTRF(5)=2
C file rec length
      LREC=0
C file record flip/flop

      NSTRF(7)=0
      K0 = 31
      KR0= 11
      NSTRF(25)=K0
      NSTRF(26)=KR0
      L0 = 7
      NG = 0


      CALL HM_OPTION_START('/SECT')
               
      DO I=1,NSECT
C
       ISTYP = 0
       NG=NG+1
       IGU=0
       NFRAM=0
       XM=ZERO
       YM=ZERO
       ZM=ZERO
       X1=ZERO
       Y1=ZERO
       X2=ZERO
       Y2=ZERO
       Z2=ZERO
       A=ZERO
       B=ZERO
       C=ZERO
       R=ZERO
       IGUS=0
       IGUQ=0
       IGUC=0
       IGUT=0
       IGUP=0
       IGUR=0
       IGUTG=0
       NBINTER=0
       IFRAM=0
C----------Multidomaines --> on ignore les sections non tagees----
       IF(NSUBDOM > 0) THEN
          IF((TAGSEC(NG) == 0))CALL HM_SZ_R2R(TAGSEC,NG,LSUBMODEL)
       ENDIF
C----------------------------------------------------------------- 
C
       LREC = LREC+3
       K1 = K0+30
        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR, UNIT_ID=UID, SUBMODEL_ID=SUB_ID, KEYWORD2=KEY2)

        NOM_OPT(1,I)=ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1, I),LTITR)
      
        CALL HM_GET_INTV('Axis_Origin_Node_N1', NSTRF(K0+3), IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('Axis_Node_N2', NSTRF(K0+4), IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('Axis_Node_N3', NSTRF(K0+5), IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('ISAVE', NSTRF(K0), IS_AVAILABLE, LSUBMODEL)  
C 
        IF (SUB_ID > 0) THEN
C--       Warning for use with submodels
          IF ((NSTRF(K0) == 1).OR.(NSTRF(K0) == 2)) THEN
            CALL ANCMSG(MSGID=1743,                                                                                         
     .               MSGTYPE=MSGWARNING,                                                                                
     .               ANMODE=ANINFO_BLIND_1,                                                                             
     .               I1=ID,                                                                                             
     .               C1=TITR)
          ELSEIF ((NSTRF(K0) == 100).OR.(NSTRF(K0) == 101)) THEN
            CALL ANCMSG(MSGID=1744,                                                                                         
     .               MSGTYPE=MSGWARNING,                                                                                
     .               ANMODE=ANINFO_BLIND_1,                                                                             
     .               I1=ID,                                                                                             
     .               C1=TITR)
          ENDIF
        ENDIF
C
        CALL HM_GET_STRING('file_name', CHAR8, ncharline, IS_AVAILABLE)
        ILEN=LEN_TRIM(CHAR8)
        ILEN=MAX(0,ILEN)
        IF(ILEN >= 0 .AND. ILEN < ncharline)THEN
          DO K=ILEN+1,ncharline
            CHAR8(K:K)=' '
          ENDDO       
        ENDIF

        IF(KEY2(1:5) == 'PARAL') THEN
          ISTYP = 1
        ELSEIF(KEY2(1:6) == 'CIRCLE') THEN
          ISTYP = 2
        ELSE
          ISTYP = 0
          CALL HM_GET_INTV('Grnod_ID', IGU, IS_AVAILABLE, LSUBMODEL)  
          CALL HM_GET_INTV('System_Id', NFRAM, IS_AVAILABLE, LSUBMODEL)  
        ENDIF 

        IFLAGUNIT = 0                                            
        DO J=1,NUNITS        
            IF (UNITAB%UNIT_ID(J) == UID) THEN    
              FAC_T = UNITAB%FAC_T(J)          
              IFLAGUNIT = 1                                        
              EXIT                                                 
           ENDIF                                                  
        ENDDO                                                    
        IF (UID /= 0.AND.IFLAGUNIT == 0) THEN                    
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,  
     .                I2=UID,I1=ID,C1='SECTION',                 
     .                C2='SECTION',                              
     .                C3=TITR)                                   
        ENDIF                                                    

       SECTIDS(I)=ID                                                                                                    

       CALL HM_GET_FLOATV('detltaT', DELTAT, IS_AVAILABLE, LSUBMODEL, UNITAB)                                           
       CALL HM_GET_FLOATV('alpha', ALPHA, IS_AVAILABLE, LSUBMODEL, UNITAB)                                              

       IF(IGU == 0 .AND. NFRAM == 0 .AND. ISTYP == 0) THEN                                                              
         CALL ANCMSG(MSGID=507,                                                                                         
     .               MSGTYPE=MSGWARNING,                                                                                
     .               ANMODE=ANINFO_BLIND_1,                                                                             
     .               I1=ID,                                                                                             
     .               C1=TITR)                                                                                           
       ENDIF                                                                                                            

       DO J=1,ncharline                                                                                                 
         NOM_SECT((I-1)*ncharline+J) = ICHAR(CHAR8(J:J))                                                                
       ENDDO                                                                                                            
                                                                                                                        
       IGUQ = 0                                                                                                         
       CALL HM_GET_INTV('grbrick_id', IGUS, IS_AVAILABLE, LSUBMODEL)                                                    
       CALL HM_GET_INTV('grshel_id', IGUC, IS_AVAILABLE, LSUBMODEL)                                                     
       CALL HM_GET_INTV('grtrus_id', IGUT, IS_AVAILABLE, LSUBMODEL)                                                     
       CALL HM_GET_INTV('grbeam_id', IGUP, IS_AVAILABLE, LSUBMODEL)                                                     
       CALL HM_GET_INTV('grsprg_id', IGUR, IS_AVAILABLE, LSUBMODEL)                                                     
       CALL HM_GET_INTV('grtria_id', IGUTG, IS_AVAILABLE, LSUBMODEL)                                                    
       CALL HM_GET_INTV('Niter', NBINTER, IS_AVAILABLE, LSUBMODEL)                                                      
       CALL HM_GET_INTV('Iframe', IFRAM, IS_AVAILABLE, LSUBMODEL)                                                       

        IF (NBINTER < 0 .OR. NBINTER > 10) THEN                    
          CALL ANCMSG(MSGID=124,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=ID,C1=TITR) 
        ENDIF
                  
       IF((IGUS == 0).AND.(IGUQ == 0).AND.(IGUC == 0).AND.(IGUT == 0).                                                  
     .    AND.(IGUP == 0).AND.(IGUR == 0).AND.(IGUTG == 0).AND.                                                         
     .   (NBINTER == 0))THEN                                                                                            
         CALL ANCMSG(MSGID=600,                                                                                         
     .               MSGTYPE=MSGWARNING,                                                                                
     .               ANMODE=ANINFO_BLIND_1,                                                                             
     .               I1=ID,                                                                                             
     .               C1=TITR)                                                                                           
       END IF                                                                                                           
                                                                                                                        
       DO J=1,NBINTER                                                                                                   
         CALL HM_GET_INT_ARRAY_INDEX('int_id' ,NSTRF(K1-1+J) ,J ,IS_AVAILABLE, LSUBMODEL)                               
       ENDDO                                                                                                            

       IF (ISTYP == 1) THEN                                                                                             
         CALL HM_GET_FLOATV('XTail', XM, IS_AVAILABLE, LSUBMODEL, UNITAB)                                               
         CALL HM_GET_FLOATV('YTail', YM, IS_AVAILABLE, LSUBMODEL, UNITAB)                                               
         CALL HM_GET_FLOATV('ZTail', ZM, IS_AVAILABLE, LSUBMODEL, UNITAB)                                                   
         IF(SUB_ID  /=  0)CALL SUBROTPOINT(XM,YM,ZM,RTRANS,SUB_ID,LSUBMODEL)                                            

         CALL HM_GET_FLOATV('cnode1_x', X1, IS_AVAILABLE, LSUBMODEL, UNITAB)                                            
         CALL HM_GET_FLOATV('cnode1_y', Y1, IS_AVAILABLE, LSUBMODEL, UNITAB)                                            
         CALL HM_GET_FLOATV('cnode1_z', Z1, IS_AVAILABLE, LSUBMODEL, UNITAB)                                                    
         IF(SUB_ID  /=  0)CALL SUBROTPOINT(X1,Y1,Z1,RTRANS,SUB_ID,LSUBMODEL)                                            

         CALL HM_GET_FLOATV('cnode2_x', X2, IS_AVAILABLE, LSUBMODEL, UNITAB)                                            
         CALL HM_GET_FLOATV('cnode2_y', Y2, IS_AVAILABLE, LSUBMODEL, UNITAB)                                            
         CALL HM_GET_FLOATV('cnode2_z', Z2, IS_AVAILABLE, LSUBMODEL, UNITAB)                                                     
         IF(SUB_ID  /=  0)CALL SUBROTPOINT(X2,Y2,Z2,RTRANS,SUB_ID,LSUBMODEL)                                            
         D = XM                                                                                                         
         E = YM                                                                                                         
         F = ZM                                                                                                         
         A = ((Y1-YM)*(Z2-ZM))-((Y2-YM)*(Z1-ZM))                                                                        
         B = ((X2-XM)*(Z1-ZM))-((X1-XM)*(Z2-ZM))                                                                        
         C = ((X1-XM)*(Y2-YM))-((X2-XM)*(Y1-YM))                                                                        
         NORM = A*A+B*B+C*C                                                                                             
         A = A/SQRT(NORM)                                                                                               
         B = B/SQRT(NORM)                                                                                               
         C = C/SQRT(NORM)                                                                                               
       ELSEIF (ISTYP == 2) THEN                                                                                         
         CALL HM_GET_FLOATV('XTail', XM, IS_AVAILABLE, LSUBMODEL, UNITAB)                                               
         CALL HM_GET_FLOATV('YTail', YM, IS_AVAILABLE, LSUBMODEL, UNITAB)                                               
         CALL HM_GET_FLOATV('ZTail', ZM, IS_AVAILABLE, LSUBMODEL, UNITAB)                                                   
         IF(SUB_ID  /=  0)CALL SUBROTPOINT(XM,YM,ZM,RTRANS,SUB_ID,LSUBMODEL)                                            
                                                                                                                        
         CALL HM_GET_FLOATV('Normal_x', A, IS_AVAILABLE, LSUBMODEL, UNITAB)                                             
         CALL HM_GET_FLOATV('Normal_y', B, IS_AVAILABLE, LSUBMODEL, UNITAB)                                             
         CALL HM_GET_FLOATV('Normal_z', C, IS_AVAILABLE, LSUBMODEL, UNITAB)                                                   
         IF(SUB_ID  /=  0)CALL SUBROTVECT(A,B,C,RTRANS,SUB_ID,LSUBMODEL)                                                

         CALL HM_GET_FLOATV('Radius', R, IS_AVAILABLE, LSUBMODEL, UNITAB)                                               

         D = XM                                                                                                         
         E = YM                                                                                                         
         F = ZM                                                                                                         
         NORM = A*A+B*B+C*C                                                                                             
         A = A/SQRT(NORM)                                                                                               
         B = B/SQRT(NORM)                                                                                               
         C = C/SQRT(NORM)                                                                                               
       ENDIF                                                                                                            

       WRITE (IOUT,2900)I,ID,TRIM(TITR),NSTRF(K0),CHAR8(1:ILEN),DELTAT,ALPHA,IFRAM,NBINTER
       WRITE (IOUT,'(10I10)')(NSTRF(K1-1+J),J=1,MAX(0,MIN(10,NBINTER))) 
       DO J=1,NBINTER                                                                                                   
        DO L=1,NINTER                                                                                                   
         IF(NSTRF(K1-1+J) == IPARI(15,L))THEN                                                                           
           IPARI(28,L) = IPARI(28,L) + 1                                                                                
C numero interne      NSTRF(K1-1+J) = L                                                                                 o 
         ENDIF                                                                                                          
        ENDDO                                                                                                           
       ENDDO                                                                                                            
C
C Noeuds concernes par la section (si NFRAM /= 0)
C Noeuds concerns :
C au dessus du plan N2N3 du frame (suivant la direction +Z)
C & dans le plan N2N3 du frame
C
       IF (ISTYP >= 1 .OR. NFRAM > 0) THEN                             
         IF(ISTYP == 0) THEN                                           
          DO K=1,NUMFRAM                                               
            J=K+1                                                      
            JJ=(NUMSKW+1)+NSUBMOD+MIN(IUN,NSPCOND)*NUMSPH+K+1          
            IF(NFRAM == ISKN(4,JJ)) THEN                               
              A = XFRAME(7,J)                                         
              B = XFRAME(8,J)                                         
              C = XFRAME(9,J)                                         
              D = XFRAME(10,J)                                         
              E = XFRAME(11,J)                                         
              F = XFRAME(12,J)                                         
              N1 = ISKN(1,JJ)                                         
              IF (NSTRF(K0+3) == 0 ) THEN                              
                IF (ISKN(1,JJ)  /=  0) THEN                            
                  NSTRF(K0+3) = ITAB(ISKN(1,JJ))                       
                ELSE                                                   
                  CALL ANCMSG(MSGID=742,                               
     .                        MSGTYPE=MSGERROR,                        
     .                        ANMODE=ANINFO,                           
     .                        I1=ID,                                   
     .                        C1=TITR,                                 
     .                        C2='N1',                                 
     .                        I2=NFRAM)                                
                ENDIF                                                  
              ENDIF                                                    
              IF (NSTRF(K0+4) == 0 ) THEN                              
                IF (ISKN(2,JJ)  /=  0) THEN                            
                  NSTRF(K0+4) = ITAB(ISKN(2,JJ))                       
                ELSE                                                   
                  CALL ANCMSG(MSGID=742,                               
     .                        MSGTYPE=MSGERROR,                        
     .                        ANMODE=ANINFO,                           
     .                        I1=ID,                                   
     .                        C1=TITR,                                 
     .                        C2='N2',                                 
     .                        I2=NFRAM)                                
                ENDIF                                                  
              ENDIF                                                    
              IF (NSTRF(K0+5) == 0 ) THEN                              
                IF (ISKN(3,JJ)  /=  0) THEN                            
                  NSTRF(K0+5) = ITAB(ISKN(3,JJ))                       
                ELSE                                                   
                  CALL ANCMSG(MSGID=742,                               
     .                        MSGTYPE=MSGERROR,                        
     .                        ANMODE=ANINFO,                           
     .                        I1=ID,                                   
     .                        C1=TITR,                                 
     .                        C2='N3',                                 
     .                        I2=NFRAM)                                
                ENDIF                                                  
              ENDIF                                                    
            ENDIF                                                      
          ENDDO                                                        
         ENDIF                                                         
         KK=1+NGRNOD                                                   
         NNOD = 0                                                     
         CPT = 1                                                      
         CALL SEC_NODES_SOL(IGUS,ISTYP,NGRBRIC,IGRBRIC,X0,A,          
     2                 B,C,D,E,F,ITAB,IXS,IXS10,IXS16,IXS20,           
     3                 NIXS,KK,NNOD,NSTRF,NBINTER,N1 ,K1,              
     4                 CPT,NODTAG,ISOLNOD,TAGELEMS,                    
     5                 X1,Y1,Z1,X2,Y2,Z2,R)                            

         KK=KK+NGRBRIC                                                 
         CALL SEC_NODES(IGUQ,ISTYP,NGRQUAD,IGRQUAD,X0,A,              
     2                 B,C,D,E,F,ITAB,IXQ,NIXQ,KK,NNOD,NSTRF,          
     3                 NBINTER,N1,K1,4,CPT,NODTAG,TAGELEMS(1+NUMELS),  
     4                 X1,Y1,Z1,X2,Y2,Z2,R)                            
         KK=KK+NGRQUAD                                                 
         CALL SEC_NODES(IGUC,ISTYP,NGRSHEL,IGRSH4N,X0,A,              
     2                 B,C,D,E,F,ITAB,IXC,NIXC,KK,NNOD,NSTRF,          
     3                 NBINTER,N1,K1,4,CPT,NODTAG,TAGELEMS(1+NUMELS    
     .               +NUMELQ),                                         
     4               X1,Y1,Z1,X2,Y2,Z2,R)                              
         KK=KK+NGRSHEL                                                 
         CALL SEC_NODES(IGUT,ISTYP,NGRTRUS,IGRTRUSS,X0,A,             
     2                 B,C,D,E,F,ITAB,IXT,NIXT,KK,NNOD,NSTRF,          
     3                 NBINTER,N1,K1,2,CPT,NODTAG,TAGELEMS(1+NUMELS    
     .              +NUMELQ+NUMELC),                                   
     4               X1,Y1,Z1,X2,Y2,Z2,R)                              
         KK=KK+NGRTRUS                                                 
         CALL SEC_NODES(IGUP,ISTYP,NGRBEAM,IGRBEAM,X0,A,              
     2                 B,C,D,E,F,ITAB,IXP,NIXP,KK,NNOD,NSTRF,          
     3                 NBINTER,N1,K1,2,CPT,NODTAG,TAGELEMS(1+NUMELS    
     .              +NUMELQ+NUMELC+NUMELT),                            
     4               X1,Y1,Z1,X2,Y2,Z2,R)                              
         KK=KK+NGRBEAM                                                 
         CALL SEC_NODES(IGUR,ISTYP,NGRSPRI,IGRSPRING,X0,A,            
     2                 B,C,D,E,F,ITAB,IXR,NIXR,KK,NNOD,NSTRF,          
     3                 NBINTER,N1,K1,2,CPT,NODTAG,TAGELEMS(1+NUMELS    
     .              +NUMELQ+NUMELC+NUMELT+NUMELP),                     
     4               X1,Y1,Z1,X2,Y2,Z2,R)                              
         KK=KK+NGRSPRI                                                 
         CALL SEC_NODES(IGUTG,ISTYP,NGRSH3N,IGRSH3N,X0,A,             
     2                B,C,D,E,F,ITAB,IXTG,NIXTG,KK,NNOD,NSTRF,         
     3                 NBINTER,N1,K1,3,CPT,NODTAG,TAGELEMS(1+NUMELS    
     .              +NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR),              
     4               X1,Y1,Z1,X2,Y2,Z2,R)                              
       ENDIF                                                           
C
       K2=K1+NBINTER                                                   
C
       IF (NFRAM == 0 .AND. ISTYP == 0) THEN                           
          NNOD=NODGRNR5(IGU,IGS,NSTRF(K2),IGRNOD,ITABM1,MESS) 
       ENDIF                                                           
C
       WRITE (IOUT,3000)NNOD                                           
       WRITE (IOUT,'(10I10)')(ITAB(NSTRF(K2+J-1)),J=1,NNOD)            
       IF (NNOD == 0)                                                  
     .   CALL ANCMSG(MSGID=1113,                                       
     .              MSGTYPE=MSGWARNING,                                
     .              ANMODE=ANINFO_BLIND_1,                             
     .              I1=ID,                                             
     .              C1=TITR)                                           
C
       K3=K2+NNOD                                                      
       NSEGS=ELEGROR(IGUS,IGRBRIC,NGRBRIC,'BRIC',                      
     .               NSTRF(K3),2,MESS,NFRAM,TAGELEMS,ISTYP,            
     .               ID,TITR)                                          
       K4=K3+2*NSEGS                                                   
       NSEGQ=ELEGROR(IGUQ,IGRQUAD,NGRQUAD,'QUAD',                      
     .               NSTRF(K4),2,MESS,NFRAM,TAGELEMS(1+NUMELS),ISTYP,  
     .               ID,TITR)                                          
       K5=K4+2*NSEGQ                                                   
       NSEGC=ELEGROR(IGUC,IGRSH4N,NGRSHEL,'SHEL',                      
     .               NSTRF(K5),2,MESS,NFRAM,TAGELEMS(1+NUMELS          
     .              +NUMELQ),ISTYP,                                    
     .               ID,TITR)                                          
       K6=K5+2*NSEGC                                                   
       NSEGT=ELEGROR(IGUT,IGRTRUSS,NGRTRUS,'TRUS',                     
     .               NSTRF(K6),2,MESS,NFRAM,TAGELEMS(1+NUMELS          
     .              +NUMELQ+NUMELC),ISTYP,                             
     .               ID,TITR)                                          
       K7=K6+2*NSEGT                                                   
       NSEGP=ELEGROR(IGUP,IGRBEAM,NGRBEAM,'BEAM',                      
     .               NSTRF(K7),2,MESS,NFRAM,TAGELEMS(1+NUMELS          
     .              +NUMELQ+NUMELC+NUMELT),ISTYP,                      
     .               ID,TITR)                                          
       K8=K7+2*NSEGP                                                   
       NSEGR=ELEGROR(IGUR,IGRSPRING,NGRSPRI,'SPRI',                    
     .               NSTRF(K8),2,MESS,NFRAM,TAGELEMS(1+NUMELS          
     .              +NUMELQ+NUMELC+NUMELT+NUMELP),ISTYP,               
     .               ID,TITR)                                               
       
       IF (NB_SEATBELT_SHELLS .NE.0) THEN
         SNSTRF1 = (SNSTRF-K8)/2
         NSEGR=NSEGR+ELEGROR_SEATBELT(IGUC,IGRSH4N,NGRSHEL,                    
     .               NSTRF(K8),2,SNSTRF1,NFRAM,TAGELEMS(1+NUMELS          
     .              +NUMELQ),ISTYP,               
     .               SEATBELT_SHELL_TO_SPRING)   
       ENDIF
                                        
       K9=K8+2*NSEGR                                                   
       NSEGTG=ELEGROR(IGUTG,IGRSH3N,NGRSH3N,'SH3N',                    
     .                NSTRF(K9),2,MESS,NFRAM,TAGELEMS(1+NUMELS         
     .              +NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR),ISTYP,        
     .               ID,TITR)                                          
C
       IF(NSEGS+NSEGQ+NSEGC+NSEGT+NSEGP+NSEGR+NSEGTG==0)THEN
         CALL ANCMSG(MSGID=1813,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO,
     .               I1= ID,
     .               C1= TITR)
       END IF
C
C-------------------------------------------------------------------------
C
C
       NSTRF(K0+14)=NBINTER
       NSTRF(K0+6)=NNOD
       NSTRF(K0+7)=NSEGS
       NSTRF(K0+8)=NSEGQ
       NSTRF(K0+9)=NSEGC
       NSTRF(K0+10)=NSEGT
       NSTRF(K0+11)=NSEGP
       NSTRF(K0+12)=NSEGR
       NSTRF(K0+13) = NSEGTG
       NSTRF(K0+26) = IFRAM
         DO L=K0+3,K0+5
           IF (NSTRF(L) /= 0) THEN
             NSTRF(L)=USR2SYS(NSTRF(L),ITABM1,MESS,ID)
             CALL ANODSET(NSTRF(L), CHECK_USED)
           ENDIF
         ENDDO
       NNSK1=ITAB(NSTRF(K0+3))
       NNSK2=ITAB(NSTRF(K0+4))
       NNSK3=ITAB(NSTRF(K0+5))
       X1=X0(1,NSTRF(K0+4))-X0(1,NSTRF(K0+3))
       Y1=X0(2,NSTRF(K0+4))-X0(2,NSTRF(K0+3))
       Z1=X0(3,NSTRF(K0+4))-X0(3,NSTRF(K0+3))
       X2=X0(1,NSTRF(K0+5))-X0(1,NSTRF(K0+4))
       Y2=X0(2,NSTRF(K0+5))-X0(2,NSTRF(K0+4))
       Z2=X0(3,NSTRF(K0+5))-X0(3,NSTRF(K0+4))
       X3=Y1*Z2-Z1*Y2
       Y3=Z1*X2-Z2*X1
       Z3=X1*Y2-X2*Y1
       N3=X3*X3+Y3*Y3+Z3*Z3
c
       PNOR1=SQRT(X1*X1+Y1*Y1+Z1*Z1)
       IF (PNOR1 < EM20) THEN
          CALL ANCMSG(MSGID=508,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)
       ELSE
         PNOR2=SQRT(N3)
         IF (PNOR2 > EM20) THEN
           PNORM1=ONE/(PNOR1*PNOR2)
           DET1=ABS((Y3*Z1-Z3*Y1)*PNORM1)
           DET2=ABS((Z3*X1-X3*Z1)*PNORM1)
           DET3=ABS((X3*Y1-Y3*X1)*PNORM1)
           DET= MAX(DET1,DET2,DET3)
         ELSE
           DET=ZERO
         ENDIF
         IF (DET < EM5) THEN
           CALL ANCMSG(MSGID=508,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)
         ENDIF
       ENDIF
C
C-------------------------------------------------------------------------
C      SOLIDES
C--------------------------------------------------------------
       WRITE (IOUT,3300) NSEGS
       CALL SECSTRI(NSEGS,NSTRF(K3),IXS,IXS10,IXS16,IXS20,
     2              NSTRF(K2),NNOD,ITAB,I,NOPRINT)
C-------------------------------------------------------------
C      QUADS
C--------------------------------------------------------------
       WRITE (IOUT,3400) NSEGQ
       CALL SEC_TRI(NSEGQ,NSTRF(K4),IXQ,NIXQ,4,NSTRF(K2),
     2              NNOD,ITAB,NUMELQ,I,' QUADS')
C-------------------------------------------------------------
C      COQUES
C--------------------------------------------------------------
       WRITE (IOUT,3100) NSEGC
       CALL SEC_TRI(NSEGC,NSTRF(K5),IXC,NIXC,4,NSTRF(K2),
     2              NNOD,ITAB,NUMELC,I,' SHELLS')
C-------------------------------------------------------------
C      BARRES
C--------------------------------------------------------------
       WRITE (IOUT,3500) NSEGT
       CALL SEC_TRI(NSEGT,NSTRF(K6),IXT,NIXT,2,NSTRF(K2),
     2              NNOD,ITAB,NUMELT,I,' TRUSSES')
C-------------------------------------------------------------
C      POUTRES
C--------------------------------------------------------------
       WRITE (IOUT,3600) NSEGP
       CALL SEC_TRI(NSEGP,NSTRF(K7),IXP,NIXP,2,NSTRF(K2),
     2              NNOD,ITAB,NUMELP,I,' BEAMS')
C-------------------------------------------------------------
C      RESSORTS
C--------------------------------------------------------------
       WRITE (IOUT,3700) NSEGR
       CALL SEC_TRI(NSEGR,NSTRF(K8),IXR,NIXR,2,NSTRF(K2),
     2              NNOD,ITAB,NUMELR,I,' SPRINGS')
C-------------------------------------------------------------
C      COQUES 3N
C--------------------------------------------------------------
       WRITE (IOUT,3200) NSEGTG
       CALL SEC_TRI(NSEGTG,NSTRF(K9),IXTG,NIXTG,3,NSTRF(K2),
     2              NNOD,ITAB,NUMELTG,I,' 3 NODES SHELLS')
                   
C-------------------------------------------------------------
C
       IF(NSTRF(K0) >= 102)THEN
         CALL ZERORE(1,10+30*NNOD,SECBUF(KR0))
       ELSEIF(NSTRF(K0) >= 101)THEN
         CALL ZERORE(1,10+24*NNOD,SECBUF(KR0))
       ELSEIF(NSTRF(K0) >= 100)THEN
         CALL ZERORE(1,10+12*NNOD,SECBUF(KR0))
       ELSE
         CALL ZERORE(1,10,SECBUF(KR0))
       ENDIF
       SECBUF(KR0) = DELTAT
       SECBUF(KR0+1) = ZERO
       SECBUF(KR0+2) = ALPHA
       SECBUF(KR0+3) = ZERO
C
       IF(NSTRF(K0) == 1.OR.NSTRF(K0) == 2)THEN
         IF(SECBUF(1) == ZERO)THEN
           SECBUF(1) = DELTAT
         ELSE
           MAXDT=MAX(SECBUF(1),DELTAT)          
           IF(ABS((SECBUF(1)-DELTAT)/SECBUF(1)) > EM06 )THEN
            CALL ANCMSG(MSGID=356,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR)
          ENDIF
         ENDIF
       ENDIF
C
       IF(NSTRF(K0) >= 1.AND.NSTRF(K0) <= 10)THEN
         NSTRF(1)=NSTRF(1)+1
c       ELSEIF(NSTRF(K0) >= 101.AND.NSTRF(K0) <= 200)THEN
       ELSEIF(NSTRF(K0) >= 100.AND.NSTRF(K0) <= 200)THEN
         NSTRF(2)=NSTRF(2)+1
         DO J=1,8
           NSTRF(15+J)=NSTRF(K0+14+J)
         ENDDO
       ENDIF
       IF(NSTRF(K0) == 1)THEN
         LREC = LREC+6*NNOD
       ELSEIF(NSTRF(K0) == 2)THEN
         LREC = LREC+12*NNOD
       ENDIF
C
       NSTRF(K0+23) = ID
       NSTRF(K0+24) = K9+2*NSEGTG
       NSTRF(K0+25) = KR0+10
       IF(NSTRF(K0) >= 100)NSTRF(K0+25) = NSTRF(K0+25)+12*NNOD
       IF(NSTRF(K0) >= 101)NSTRF(K0+25) = NSTRF(K0+25)+12*NNOD
       IF(NSTRF(K0) >= 102)NSTRF(K0+25) = NSTRF(K0+25)+6*NNOD
C
       KR0 = NSTRF(K0+25)
       K0  = NSTRF(K0+24)
C-------------------------------------------------------------
       NODTAG = 0
       TAGELEMS  = 0
      ENDDO
      CALL UDOUBLE(SECTIDS,1,NSECT,MESS,0,BID)

C file rec length
      NSTRF(6)=LREC*4
      
C-------------------------------------------------------------
 2900 FORMAT(/' SECTION',I10,' ID',I10/
     +        ' ---------------'/
     + ,A/,
     + '        TYPE . . . . . . . . . . . . . . .',I10/
     + '        FILENAME . . . . . . . . . . . . .',A/
     + '        DELTAT . . . . . . . . . . . . . .',1PG20.13/
     + '        ALPHA. . . . . . . . . . . . . . .',1PG20.13/
     + '        FRAME TYPE . . . . . . . . . . . .',I10/
     + '        NUMBER OF INTERFACES . . . . . . .',I10/
     + '        INTERFACES:')
 2901 FORMAT(/' SECTION',I10,' ID',I10/
     +        ' ---------------'/
     + '        TYPE . . . . . . . . . . . . . . .',I8/
     + '        FRAME TYPE . . . . . . . . . . . .',I8/)
 3000 FORMAT(/
     + '        NUMBER OF NODES. . . . . . . . . .',I10/
     + '        NODES:')
 3100 FORMAT(/
     + '        NUMBER OF SHELL ELEMENTS . . . . .',I10/
     + '   SHELL      N1      N2      N3      N4')
 3200 FORMAT(/
     + '        NUMBER OF 3 NODES SHELL ELEMENTS .',I10/
     + '   SHELL      N1      N2      N3')
 3300 FORMAT(/
     + '        NUMBER OF BRICK ELEMENTS . . . . .',I10/
     + '   BRICK      N1      N2      N3      N4',
     + '     N5      N6      N7      N8')
 3400 FORMAT(/
     + '        NUMBER OF QUAD ELEMENTS . . . . .',I10/
     + '   QUAD      N1      N2      N3      N4')
 3500 FORMAT(/
     + '        NUMBER OF TRUSS ELEMENTS . . . . .',I10/
     + '   TRUSS      N1      N2')
 3600 FORMAT(/
     + '        NUMBER OF BEAM ELEMENTS . . . . .',I10/
     + '   BEAM      N1      N2')
 3700 FORMAT(/
     + '        NUMBER OF SPRING ELEMENTS . . . . .',I8/
     + '   SPRING      N1      N2')
C
      RETURN
 999  CALL FREERR(3)
      END
C=========================================================================
Chd|====================================================================
Chd|  SECSTRI                       source/tools/sect/hm_read_sect.F
Chd|-- called by -----------
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SECSTRI(NSEG,ISECBUF,IXS,IXS10,IXS16,IXS20,
     2                   NOD ,NNOD ,ITAB ,ISEC, NOPRINT )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C
      INTEGER NSEG,ISECBUF(2,*),IXS(NIXS,*),
     .        IXS10(6,*),IXS16(8,*),IXS20(12,*),
     .        NOD(*),NNOD,ITAB(*),ISEC,NOPRINT
      INTEGER I,J,JJ,K,N,NN,L,POWER2(20)
C
      DATA POWER2/1,2,4,8,16,32,64,128,256,512,
     .            1024,2048,4096,8192,16384,
     .            32768,65536,131072,262144,524288/
C
C-------------------------------------------------------------
C      TRI SUR LES NUMEROS INTERNES
C--------------------------------------------------------------
       DO J=1,NSEG-1
         N = ISECBUF(1,J)
         DO JJ=J,NSEG
           NN = ISECBUF(1,JJ)
           IF(NN < N)THEN
               ISECBUF(1,J)  = NN
               ISECBUF(1,JJ) = N
               N = ISECBUF(1,J)
           ENDIF
         ENDDO
       ENDDO
C-------------------------------------------------------------
C      RECHERCHE DES NOEUDS DES ELEMENTS DE LA SECTION
C--------------------------------------------------------------
       DO J=1,NSEG
         N = ISECBUF(1,J)
         ISECBUF(2,J) = 0
         DO L=1,8
           DO NN=1,NNOD
             IF(IXS(L+1,N) == NOD(NN))THEN
               ISECBUF(2,J) = ISECBUF(2,J) + POWER2(L)
               GOTO 70
             ENDIF
           ENDDO
 70        CONTINUE
         ENDDO
         IF(N > NUMELS8)THEN
          N=N-NUMELS8
          IF(N <= NUMELS10)THEN
           DO L=1,6
            DO NN=1,NNOD
              IF(IXS10(L,N) == NOD(NN))THEN
                ISECBUF(2,J) = ISECBUF(2,J) + POWER2(L+8)
                GOTO 80
              ENDIF
            ENDDO
 80         CONTINUE
           ENDDO
          ELSE
           N=N-NUMELS10
           IF(N <= NUMELS20)THEN
            DO L=1,12
             DO NN=1,NNOD
               IF(IXS20(L,N) == NOD(NN))THEN
                 ISECBUF(2,J) = ISECBUF(2,J) + POWER2(L+8)
                 GOTO 90
               ENDIF
             ENDDO
 90          CONTINUE
            ENDDO
           ELSE
            DO L=1,8
             DO NN=1,NNOD
               IF(IXS16(L,N) == NOD(NN))THEN
                 ISECBUF(2,J) = ISECBUF(2,J) + POWER2(L+8)
                 GOTO 100
               ENDIF
             ENDDO
 100         CONTINUE
            ENDDO
           END IF
          END IF
         END IF
       ENDDO
c
       IF (NOPRINT == 1) RETURN
c
       DO J=1,NSEG
         N = ISECBUF(1,J)
         IF(N <= NUMELS8)THEN
           WRITE (IOUT,'(9I10)')IXS(NIXS,ISECBUF(1,J)),
     .         (MOD(ISECBUF(2,J)/POWER2(K),2),K=1,8)
         ELSE
           N=ISECBUF(1,J)-NUMELS8
           IF(N <= NUMELS10)THEN
             WRITE (IOUT,'(5I10,/,10X,6I10)')
     .         IXS(NIXS,ISECBUF(1,J)),
     .         MOD(ISECBUF(2,J)/POWER2(1),2),
     .         MOD(ISECBUF(2,J)/POWER2(3),2),
     .         MOD(ISECBUF(2,J)/POWER2(6),2),
     .         MOD(ISECBUF(2,J)/POWER2(5),2),
     .         (MOD(ISECBUF(2,J)/POWER2(K),2),K=9,14)
           ELSE
             N=ISECBUF(1,J)-NUMELS8-NUMELS10
             IF(N <= NUMELS20)THEN
              WRITE (IOUT,'(9I10,/,10X,12I10)')
     .         IXS(NIXS,ISECBUF(1,J)),
     .         (MOD(ISECBUF(2,J)/POWER2(K),2),K=1,20)
             ELSE
              WRITE (IOUT,'(9I10,/,10X,8I10)')
     .         IXS(NIXS,ISECBUF(1,J)),
     .         (MOD(ISECBUF(2,J)/POWER2(K),2),K=1,16)
             END IF
           END IF
         END IF
       ENDDO
C
       RETURN
       END

C=========================================================================
Chd|====================================================================
Chd|  SEC_TRI                       source/tools/sect/hm_read_sect.F
Chd|-- called by -----------
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SEC_TRI(NSEG,ISECBUF,IX,NIX,NNE,NOD,
     2                   NNOD,ITAB   ,NUMEL,ISEC,TXT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C
      INTEGER NIX,NSEG,ISECBUF(2,*),IX(NIX,*),NNE,NOD(*),NNOD,
     .        ITAB(*),NUMEL,ISEC
      INTEGER I,J,JJ,K,N,NN,L,POWER2(10),UNPACK(0:1023,10),IFIRST
      CHARACTER*(*) TXT
C
      DATA POWER2/1,2,4,8,16,32,64,128,256,512/
      DATA IFIRST/0/
      SAVE IFIRST,UNPACK
C
      IF(IFIRST == 0)THEN
       IFIRST=1
       DO I=1,10
        DO J=0,1023
          UNPACK(J,I)=MOD(J/POWER2(I),2)
        ENDDO
       ENDDO
      ENDIF

C-------------------------------------------------------------
C      TRI SUR LES NUMEROS INTERNES
C--------------------------------------------------------------
       DO J=1,NSEG-1
         N = ISECBUF(1,J)
         DO JJ=J,NSEG
           NN = ISECBUF(1,JJ)
           IF(NN < N)THEN
               ISECBUF(1,J)  = NN
               ISECBUF(1,JJ) = N
               N = ISECBUF(1,J)
           ENDIF
         ENDDO
       ENDDO
C-------------------------------------------------------------
C      RECHERCHE DES NOEUDS DES ELEMENTS DE LA SECTION
C--------------------------------------------------------------
       DO J=1,NSEG
         N = ISECBUF(1,J)
         ISECBUF(2,J) = 0
         DO L=1,NNE
           DO NN=1,NNOD
             IF(IX(L+1,N) == NOD(NN))THEN
               ISECBUF(2,J) = ISECBUF(2,J) + POWER2(L)
               GOTO 70
             ENDIF
           ENDDO
 70        CONTINUE
         ENDDO
       ENDDO
       DO J=1,NSEG
         WRITE (IOUT,'(11I10)')IX(NIX,ISECBUF(1,J)),
     .       (UNPACK(ISECBUF(2,J),K),K=1,NNE)
       ENDDO
C
       RETURN
       END
Chd|====================================================================
Chd|  LECSEC0                       source/tools/sect/hm_read_sect.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LECSEC0(LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, TYPE, NBINTER
      LOGICAL IS_AVAILABLE
      INTEGER ID
      CHARACTER*ncharline TITR
C-----------------------------------------------
C   S o u r c e   C o d e
C-----------------------------------------------

      ISECUT=0

      IF(NSECT /= 0)CALL HM_OPTION_START('/SECT')
      DO I=1,NSECT
       CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID = ID,OPTION_TITR = TITR)
       CALL HM_GET_INTV('ISAVE', TYPE, IS_AVAILABLE, LSUBMODEL)  
       CALL HM_GET_INTV('Niter', NBINTER, IS_AVAILABLE, LSUBMODEL)  
       IF(TYPE > 0)ISECUT=1
       IF(NBINTER > 0)ISECUT=1
      ENDDO

      RETURN
 999  CALL FREERR(3)
      END
C
Chd|====================================================================
Chd|  SEC_NODES                     source/tools/sect/hm_read_sect.F
Chd|-- called by -----------
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SEC_NODES(IGU1,ISTYP,NGRELE,IGRELE,X0,A,
     2                  B,C,D,E,F,ITAB,IX,NIX,KK,NNOD,NSTRF,
     3                  NBINTER,N1 ,K1,NBNODES,J,NODTAG,TAGELEMS,
     4                  X1,Y1,Z1,X2,Y2,Z2,R)
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGU1,ISTYP,NGRELE,NIX,KK,NNOD,NBINTER,N1,K1,NBNODES,J
      INTEGER ITAB(*),IX(NIX,*), NSTRF(*),
     .        NODTAG(NUMNOD),TAGELEMS(*)
      my_real
     .   X0(3,*),A,B,C,D,E,F,X1,Y1,Z1,X2,Y2,Z2,R
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRELE)   :: IGRELE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,L,ISU,IADV,IE,TAGELEM1,TAGELEM2,TAGELEM3,
     .   TAGNDOUBL(NUMNOD),TAGNODES(NUMNOD*2+NPART),NBPROJOK
      my_real
     .        POS,PROJX,PROJY,PROJZ,P1,P2
C=======================================================================
C
C Determine les noeuds concernes par la section si on utilise un
C frame pour definir la section
C
        TAGNDOUBL = 0
	TAGNODES  = 0
	IF (IGU1 /= 0) THEN
	  DO L=1,NGRELE
	    IF ( IGRELE(L)%ID == IGU1 )THEN
	      ISU = L
	    ENDIF
	  ENDDO
          DO L=1,IGRELE(ISU)%NENTITY
            IE=IGRELE(ISU)%ENTITY(L)
	    IF (IE  /=  0) THEN
	      TAGELEM1=0
	      TAGELEM2=0
              TAGELEM3=0
              NBPROJOK = 0
              DO K=2,NBNODES+1
C             tag les noeuds connectes a l'element
                POS = (X0(1,IX(K,IE))-D)*A +
     .  	      (X0(2,IX(K,IE))-E)*B +
     .  	      (X0(3,IX(K,IE))-F)*C
                IF (ISTYP == 1) THEN
                  PROJX = X0(1,IX(K,IE))-POS*A
                  PROJY = X0(2,IX(K,IE))-POS*B
                  PROJZ = X0(3,IX(K,IE))-POS*C
                  PROJX = PROJX-D
                  PROJY = PROJY-E
                  PROJZ = PROJZ-F
c
                  IF ( (X2-D) /=  ZERO .AND.
     .              (Y1-E)-(X1-D)*(Y2-E) /=  ZERO)THEN
                    P1 = (PROJY-PROJX*(Y2-E)/(X2-D))/
     .                ((Y1-E)-(X1-D)*(Y2-E)/(X2-D))
                  ELSEIF( (Y2-E) /=  ZERO .AND.
     .              (Z1-F)-(Y1-E)*(Z2-F) /=  ZERO)THEN
                    P1 = (PROJZ-PROJY*(Z2-F)/(Y2-E))/
     .                ((Z1-F)-(Y1-E)*(Z2-F)/(Y2-E))
                  ELSEIF( (Z2-F) /=  ZERO .AND.
     .              (X1-D)-(Y1-E)*(X2-D) /=  ZERO)THEN
                    P1 = (PROJX-PROJZ*(X2-D)/(Z2-F))/
     .                ((X1-D)-(Y1-E)*(X2-D)/(Z2-F))
                  ENDIF
                  IF ( (X1-D) /=  ZERO .AND.
     .              (Y2-E)-(X2-D)*(Y1-E)  /=  ZERO)THEN
                    P2 = (PROJY-PROJX*(Y1-E)/(X1-D))/
     .                ((Y2-E)-(X2-D)*(Y1-E)/(X1-D))
                  ELSEIF ( (Y1-E) /=  ZERO .AND.
     .              (Z2-F)-(Y2-E)*(Z1-F)  /=  ZERO)THEN
                    P2 = (PROJZ-PROJY*(Z1-F)/(Y1-E))/
     .                ((Z2-F)-(Y2-E)*(Z1-F)/(Y1-E))
                  ELSEIF ( (Z1-F) /=  ZERO .AND.
     .              (X2-D)-(X1-D)*(Y2-E)  /=  ZERO)THEN
                    P2 = (PROJX-PROJZ*(X1-D)/(Z1-F))/
     .                ((X2-D)-(Y2-E)*(X1-D)/(Z1-F))
                  ENDIF
c
                  IF((X2-D)== ZERO .AND. (X1-D)/= ZERO) 
     .                     P1 = PROJX / (X1-D) 
                  IF((X1-D)== ZERO .AND. (X2-D)/= ZERO)  
     .                     P2 = PROJX / (X2-D)
                  IF((Y2-E)== ZERO .AND. (Y1-E)/= ZERO)  
     .                     P1 = PROJY / (Y1-E)
                  IF((Y1-E)== ZERO .AND. (Y2-E)/= ZERO)  
     .                     P2 = PROJY / (Y2-E)
                  IF((Z2-F)== ZERO .AND. (Z1-F)/= ZERO)  
     .                     P1 = PROJZ / (Z1-F)
                  IF((Z1-F)== ZERO .AND. (Z2-F)/= ZERO)  
     .                     P2 = PROJZ / (Z2-F)
C
                  IF( P1 <= 1 .AND. P1 >= 0 .AND. P2 <= 1 .AND. P2 >= 0)
     .                 NBPROJOK = NBPROJOK + 1
                ELSEIF (ISTYP == 2) THEN
                  PROJX = X0(1,IX(K,IE))-POS*A
                  PROJY = X0(2,IX(K,IE))-POS*B
                  PROJZ = X0(3,IX(K,IE))-POS*C
                  PROJX = PROJX-D
                  PROJY = PROJY-E
                  PROJZ = PROJZ-F
                  P1 = SQRT(PROJX**2+PROJY**2+PROJZ**2)
                  IF( P1 <= R)   NBPROJOK = NBPROJOK + 1
                ENDIF

                IF ( POS < ZERO) THEN
		  TAGNODES(IX(K,IE))= -1
	          TAGNDOUBL(IX(K,IE)) = TAGNDOUBL(IX(K,IE)) + 1
	          TAGELEM1 = 1
	        ELSEIF ( POS == ZERO) THEN
		  TAGNODES(IX(K,IE))= 0
	          TAGNDOUBL(IX(K,IE)) = TAGNDOUBL(IX(K,IE)) + 1
	          TAGELEM2 = 1
	        ELSE
		  TAGNODES(IX(K,IE))= 1
	          TAGNDOUBL(IX(K,IE)) = TAGNDOUBL(IX(K,IE)) + 1
	          TAGELEM3 = 1
	        ENDIF
              ENDDO
	      IF ( (ISTYP == 0 .OR. NBPROJOK >= 1) .AND.
     .             ( TAGELEM1+TAGELEM3 /= 1
     .     	   .OR. ( TAGELEM2 == 1 .AND. TAGELEM3 == 1 )))
     .       	      TAGELEMS(IE) = 1
	    ENDIF
	  ENDDO
          DO L=1,IGRELE(ISU)%NENTITY
            IE=IGRELE(ISU)%ENTITY(L)
	    IF (IE  /=  0) THEN
	      IF (TAGELEMS(IE) == 1) THEN
	        DO K=2,NBNODES+1
	     	  IF ( TAGNDOUBL(IX(K,IE)) >= 1
     .     	       .AND. TAGNODES(IX(K,IE)) > 0
     .     	       .AND. NODTAG(IX(K,IE)) == 0) THEN
	   	    NSTRF(K1+NBINTER+J-1) = IX(K,IE)
	   	    TAGNDOUBL(IX(K,IE)) = 0
	   	    NODTAG(IX(K,IE)) = 1
	   	    J = J + 1
	   	    NNOD = NNOD + 1
	     	  ENDIF
	        ENDDO
	      ENDIF
	    ENDIF
	  ENDDO
	ENDIF
      RETURN
      END
C
Chd|====================================================================
Chd|  SEC_NODES_SOL                 source/tools/sect/hm_read_sect.F
Chd|-- called by -----------
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SEC_NODES_SOL(IGU1,ISTYP,NGR,IGRBRIC,X0,A,
     2                   B,C,D,E,F,ITAB,IXS,IXS10,IXS16,IXS20,
     3                   NIX,KK,NNOD,NSTRF,NBINTER,N1 ,K1,
     4                   J,NODTAG,ISOLNOD,TAGELEMS,
     5                   X1,Y1,Z1,X2,Y2,Z2,R)
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGU1,ISTYP,NGR,NIX,KK,NNOD,NBINTER,N1,K1,J
      INTEGER ITAB(*),IXS(NIX,*), NSTRF(*),
     .        NODTAG(NUMNOD), IXS10(6,*),IXS16(8,*),IXS20(12,*),
     .        ISOLNOD(*),TAGELEMS(*)
      my_real
     .   X0(3,*),A,B,C,D,E,F,X1,Y1,Z1,X2,Y2,Z2,R
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRBRIC)  :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,L,ISU,IADV,IE,TAGELEM1,TAGELEM2,TAGELEM3,
     .   NBNODES,NBPROJOK,OFFSET
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGNDOUBL,TAGNODES
      my_real
     .        POS,PROJX,PROJY,PROJZ,P1,P2
C=======================================================================
C
C Determine les noeuds concernes par la section si on utilise un
C frame pour dfinir la section
C
        ALLOCATE( TAGNDOUBL(NUMNOD),TAGNODES(NUMNOD*2+NPART) )
        TAGNDOUBL = 0
	TAGNODES  = 0
	IF (IGU1 /= 0) THEN
	  DO L=1,NGRBRIC
	    IF ( IGRBRIC(L)%ID == IGU1 )THEN
	      ISU = L
	    ENDIF
	  ENDDO
          DO L=1,IGRBRIC(ISU)%NENTITY
            IE=IGRBRIC(ISU)%ENTITY(L)
	    IF (IE  /=  0) THEN
	      NBNODES = ISOLNOD(IE)
	      IF (NBNODES == 4) NBNODES = 8
	      TAGELEM1=0
	      TAGELEM2=0
              TAGELEM3=0
              NBPROJOK = 0
              DO K=2,NBNODES+1
C            	tag les noeuds connectes a l'element
                IF (NBNODES == 10 .AND. K > 5) THEN
                  OFFSET = NUMELS8
             	  POS = (X0(1,IXS10(K-5,IE-OFFSET))-D)*A +
     .     	  	(X0(2,IXS10(K-5,IE-OFFSET))-E)*B +
     .     	  	(X0(3,IXS10(K-5,IE-OFFSET))-F)*C
                  IF (ISTYP >= 1) THEN
                    PROJX = X0(1,IXS10(K-5,IE-OFFSET))-POS*A
                    PROJY = X0(2,IXS10(K-5,IE-OFFSET))-POS*B
                    PROJZ = X0(3,IXS10(K-5,IE-OFFSET))-POS*C
                  ENDIF
             	ELSEIF (NBNODES == 16 .AND. K > 9) THEN
                  OFFSET = NUMELS8+NUMELS10+NUMELS20
             	  POS = (X0(1,IXS16(K-9,IE-OFFSET))-D)*A +
     .     	  	(X0(2,IXS16(K-9,IE-OFFSET))-E)*B +
     .     	  	(X0(3,IXS16(K-9,IE-OFFSET))-F)*C
                  IF (ISTYP >= 1) THEN
                    PROJX = X0(1,IXS16(K-9,IE-OFFSET))-POS*A
                    PROJY = X0(2,IXS16(K-9,IE-OFFSET))-POS*B
                    PROJZ = X0(3,IXS16(K-9,IE-OFFSET))-POS*C
                  ENDIF
             	ELSEIF (NBNODES == 20 .AND. K > 9) THEN
                  OFFSET = NUMELS8+NUMELS10
             	  POS = (X0(1,IXS20(K-9,IE-OFFSET))-D)*A +
     .     	  	(X0(2,IXS20(K-9,IE-OFFSET))-E)*B +
     .     	  	(X0(3,IXS20(K-9,IE-OFFSET))-F)*C
                  IF (ISTYP >= 1) THEN
                    PROJX = X0(1,IXS20(K-9,IE-OFFSET))-POS*A
                    PROJY = X0(2,IXS20(K-9,IE-OFFSET))-POS*B
                    PROJZ = X0(3,IXS20(K-9,IE-OFFSET))-POS*C
                  ENDIF
             	ELSE 
             	  POS = (X0(1,IXS(K,IE))-D)*A +
     .     	  	(X0(2,IXS(K,IE))-E)*B +
     .     	  	(X0(3,IXS(K,IE))-F)*C
                  IF (ISTYP >= 1) THEN
                    PROJX = X0(1,IXS(K,IE))-POS*A
                    PROJY = X0(2,IXS(K,IE))-POS*B
                    PROJZ = X0(3,IXS(K,IE))-POS*C
                  ENDIF
             	ENDIF
                IF (ISTYP == 1) THEN
                  PROJX = PROJX - D
                  PROJY = PROJY - E
                  PROJZ = PROJZ - F
c
                  IF ( (X2-D) /=  ZERO .AND.
     .              (Y1-E)-(X1-D)*(Y2-E) /=  ZERO)THEN
                    P1 = (PROJY-PROJX*(Y2-E)/(X2-D))/
     .                ((Y1-E)-(X1-D)*(Y2-E)/(X2-D))
                  ELSEIF( (Y2-E) /=  ZERO .AND.
     .              (Z1-F)-(Y1-E)*(Z2-F) /=  ZERO)THEN
                    P1 = (PROJZ-PROJY*(Z2-F)/(Y2-E))/
     .                ((Z1-F)-(Y1-E)*(Z2-F)/(Y2-E))
                  ELSEIF( (Z2-F) /=  ZERO .AND.
     .              (X1-D)-(Y1-E)*(X2-D) /=  ZERO)THEN
                    P1 = (PROJX-PROJZ*(X2-D)/(Z2-F))/
     .                ((X1-D)-(Y1-E)*(X2-D)/(Z2-F))
                  ENDIF
                  IF ( (X1-D) /=  ZERO .AND.
     .              (Y2-E)-(X2-D)*(Y1-E)  /=  ZERO)THEN
                    P2 = (PROJY-PROJX*(Y1-E)/(X1-D))/
     .                ((Y2-E)-(X2-D)*(Y1-E)/(X1-D))
                  ELSEIF ( (Y1-E) /=  ZERO .AND.
     .              (Z2-F)-(Y2-E)*(Z1-F)  /=  ZERO)THEN
                    P2 = (PROJZ-PROJY*(Z1-F)/(Y1-E))/
     .                ((Z2-F)-(Y2-E)*(Z1-F)/(Y1-E))
                  ELSEIF ( (Z1-F) /=  ZERO .AND.
     .              (X2-D)-(X1-D)*(Y2-E)  /=  ZERO)THEN
                    P2 = (PROJX-PROJZ*(X1-D)/(Z1-F))/
     .                ((X2-D)-(Y2-E)*(X1-D)/(Z1-F))
                  ENDIF
c
                  IF((X2-D)== ZERO .AND. (X1-D)/= ZERO) 
     .                     P1 = PROJX / (X1-D) 
                  IF((X1-D)== ZERO .AND. (X2-D)/= ZERO)  
     .                     P2 = PROJX / (X2-D)
                  IF((Y2-E)== ZERO .AND. (Y1-E)/= ZERO)  
     .                     P1 = PROJY / (Y1-E)
                  IF((Y1-E)== ZERO .AND. (Y2-E)/= ZERO)  
     .                     P2 = PROJY / (Y2-E)
                  IF((Z2-F)== ZERO .AND. (Z1-F)/= ZERO)  
     .                     P1 = PROJZ / (Z1-F)
                  IF((Z1-F)== ZERO .AND. (Z2-F)/= ZERO)  
     .                     P2 = PROJZ / (Z2-F)
C

c
                  IF( P1 <= 1 .AND. P1 >= 0 .AND. P2 <= 1 .AND. P2 >= 0)
     .                 NBPROJOK = NBPROJOK + 1
                ELSEIF (ISTYP == 2) THEN
                  PROJX = PROJX - D
                  PROJY = PROJY - E
                  PROJZ = PROJZ - F
                  P1 = SQRT(PROJX**2+PROJY**2+PROJZ**2)
                  IF( P1 <= R)   NBPROJOK = NBPROJOK + 1
                ENDIF
             	IF ( POS < ZERO) THEN
             	  IF (NBNODES == 10 .AND. K>5) THEN
                    OFFSET = NUMELS8
	   	    TAGNODES(IXS10(K-5,IE-OFFSET))= -1
	     	    TAGNDOUBL(IXS10(K-5,IE-OFFSET)) = TAGNDOUBL(IXS10(K-5,IE-OFFSET)) + 1
             	  ELSEIF (NBNODES == 16 .AND. K>9) THEN
                    OFFSET = NUMELS8+NUMELS10+NUMELS20
	   	    TAGNODES(IXS16(K-9,IE-OFFSET))= -1
	     	    TAGNDOUBL(IXS16(K-9,IE-OFFSET)) = TAGNDOUBL(IXS16(K-9,IE-OFFSET)) + 1
             	  ELSEIF (NBNODES == 20 .AND. K>9) THEN
                    OFFSET = NUMELS8+NUMELS10
	   	    TAGNODES(IXS20(K-9,IE-OFFSET))= -1
	     	    TAGNDOUBL(IXS20(K-9,IE-OFFSET)) = TAGNDOUBL(IXS20(K-9,IE-OFFSET)) + 1
             	  ELSE 
	   	    TAGNODES(IXS(K,IE))= -1
	     	    TAGNDOUBL(IXS(K,IE)) = TAGNDOUBL(IXS(K,IE)) + 1
	   	  ENDIF
	     	  TAGELEM1 = 1
	     	ELSEIF ( POS == ZERO) THEN
             	  IF(NBNODES == 10 .AND. K>5) THEN
                    OFFSET = NUMELS8
	   	    TAGNODES(IXS10(K-5,IE-OFFSET))= 0
	     	    TAGNDOUBL(IXS10(K-5,IE-OFFSET)) = TAGNDOUBL(IXS10(K-5,IE-OFFSET)) + 1
             	  ELSEIF (NBNODES == 16 .AND. K>9) THEN
                    OFFSET = NUMELS8+NUMELS10+NUMELS20
	   	    TAGNODES(IXS16(K-9,IE-OFFSET))= 0
	     	    TAGNDOUBL(IXS16(K-9,IE-OFFSET)) = TAGNDOUBL(IXS16(K-9,IE-OFFSET)) + 1
             	  ELSEIF (NBNODES == 20 .AND. K>9) THEN
                    OFFSET = NUMELS8+NUMELS10
	   	    TAGNODES(IXS20(K-9,IE-OFFSET))= 0
	     	    TAGNDOUBL(IXS20(K-9,IE-OFFSET)) = TAGNDOUBL(IXS20(K-9,IE-OFFSET)) + 1
             	  ELSE
	   	    TAGNODES(IXS(K,IE))= 0
	     	    TAGNDOUBL(IXS(K,IE)) = TAGNDOUBL(IXS(K,IE)) + 1
	   	  ENDIF
	     	  TAGELEM2 = 1
	     	ELSE
             	  IF (NBNODES == 10 .AND. K>5) THEN
                    OFFSET = NUMELS8
	   	    TAGNODES(IXS10(K-5,IE-OFFSET))= 1
	     	    TAGNDOUBL(IXS10(K-5,IE-OFFSET)) = TAGNDOUBL(IXS10(K-5,IE-OFFSET)) + 1
             	  ELSEIF (NBNODES == 16 .AND. K>9) THEN
                    OFFSET = NUMELS8+NUMELS10+NUMELS20
	   	    TAGNODES(IXS16(K-9,IE-OFFSET))= 1
	     	    TAGNDOUBL(IXS16(K-9,IE-OFFSET)) = TAGNDOUBL(IXS16(K-9,IE-OFFSET)) + 1
             	  ELSEIF (NBNODES == 20 .AND. K>9) THEN
                    OFFSET = NUMELS8+NUMELS10
	   	    TAGNODES(IXS20(K-9,IE-OFFSET))= 1
	     	    TAGNDOUBL(IXS20(K-9,IE-OFFSET)) = TAGNDOUBL(IXS20(K-9,IE-OFFSET)) + 1
             	  ELSE
	   	    TAGNODES(IXS(K,IE))= 1
	     	    TAGNDOUBL(IXS(K,IE)) = TAGNDOUBL(IXS(K,IE)) + 1
	   	  ENDIF
	     	  TAGELEM3 = 1
	     	ENDIF
              ENDDO
	      IF ( (ISTYP == 0 .OR. NBPROJOK >= 1) .AND.
     .             ( TAGELEM1+TAGELEM3 /= 1
     .     	   .OR. ( TAGELEM2 == 1 .AND. TAGELEM3 == 1 )))
     .       	      TAGELEMS(IE) = 1
	    ENDIF
	  ENDDO
!
          DO L=1,IGRBRIC(ISU)%NENTITY
            IE=IGRBRIC(ISU)%ENTITY(L)
	    IF (IE  /=  0) THEN
	      NBNODES = ISOLNOD(IE)
	      IF (NBNODES == 4) NBNODES = 8
	      IF (TAGELEMS(IE) == 1) THEN
	        DO K=2,NBNODES+1
                  IF (NBNODES == 10 .AND. K > 5) THEN
                    OFFSET = NUMELS8
	            IF ( TAGNDOUBL(IXS10(K-5,IE-OFFSET)) >= 1
     .          	 .AND. TAGNODES(IXS10(K-5,IE-OFFSET)) > 0
     .          	 .AND. NODTAG(IXS10(K-5,IE-OFFSET)) == 0) THEN
	              NSTRF(K1+NBINTER+J-1) = IXS10(K-5,IE-OFFSET)
	              TAGNDOUBL(IXS10(K-5,IE-OFFSET)) = 0
	              NODTAG(IXS10(K-5,IE-OFFSET)) = 1
	              J = J + 1
	              NNOD = NNOD + 1
	            ENDIF
                  ELSEIF (NBNODES == 16 .AND. K > 9) THEN
                    OFFSET = NUMELS8+NUMELS10+NUMELS20
	            IF ( TAGNDOUBL(IXS16(K-9,IE-OFFSET)) >= 1
     .          	 .AND. TAGNODES(IXS16(K-9,IE-OFFSET)) > 0
     .          	 .AND. NODTAG(IXS16(K-9,IE-OFFSET)) == 0) THEN
	              NSTRF(K1+NBINTER+J-1) = IXS16(K-9,IE-OFFSET)
	              TAGNDOUBL(IXS16(K-9,IE-OFFSET)) = 0
	              NODTAG(IXS16(K-9,IE-OFFSET)) = 1
	              J = J + 1
	              NNOD = NNOD + 1
	            ENDIF
                  ELSEIF (NBNODES == 20 .AND. K > 9) THEN
                    OFFSET = NUMELS8+NUMELS10
	            IF ( TAGNDOUBL(IXS20(K-9,IE-OFFSET)) >= 1
     .          	 .AND. TAGNODES(IXS20(K-9,IE-OFFSET)) > 0
     .          	 .AND. NODTAG(IXS20(K-9,IE-OFFSET)) == 0) THEN
	              NSTRF(K1+NBINTER+J-1) = IXS20(K-9,IE-OFFSET)
	              TAGNDOUBL(IXS20(K-9,IE-OFFSET)) = 0
	              NODTAG(IXS20(K-9,IE-OFFSET)) = 1
	              J = J + 1
	              NNOD = NNOD + 1
	            ENDIF
                  ELSE
	            IF ( TAGNDOUBL(IXS(K,IE)) >= 1
     .          	 .AND. TAGNODES(IXS(K,IE)) > 0
     .          	 .AND. NODTAG(IXS(K,IE)) == 0) THEN
	              NSTRF(K1+NBINTER+J-1) = IXS(K,IE)
	              TAGNDOUBL(IXS(K,IE)) = 0
	              NODTAG(IXS(K,IE)) = 1
	              J = J + 1
	              NNOD = NNOD + 1
	            ENDIF
	          ENDIF
	        ENDDO
	      ENDIF
	    ENDIF
	  ENDDO
	ENDIF

        DEALLOCATE( TAGNDOUBL,TAGNODES )
      RETURN
      END
