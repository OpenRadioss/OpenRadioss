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
Chd|  LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        SECSTRI                       source/tools/sect/hm_read_sect.F
Chd|        SEC_NODES_SOL                 source/tools/sect/hm_read_sect.F
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
      SUBROUTINE LECSEC4BOLT(IXS   ,IXQ   ,IXC    ,IXT    ,IXP    ,IXR    ,
     2                       IXTG  ,X0    ,NSTRF  ,ITAB   ,ITABM1 ,
     3                       IGRNOD,SECBUF ,
     4                       IPARI ,IXS10 ,IXS20  ,IXS16  ,UNITAB ,
     5                       ISKN  ,XFRAME,ISOLNOD,NOM_SECT,RTRANS,
     6                       LSUBMODEL,NOM_OPT,IGRBRIC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE R2R_MOD
      USE SUBMODEL_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
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
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "sysunit.inc"
#include      "sphcom.inc"
#include      "submod_c.inc"
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
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      my_real
     .   X0(3,*),SECBUF(*),XFRAME(NXFRAME,*),
     .   RTRANS(NTRANSF,*)
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRNOD)   :: IGRNOD
      TYPE (GROUP_)  ,DIMENSION(NGRBRIC)  :: IGRBRIC
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
     .   NUMELR+NUMELTG),NG,NOPRINT,ILEN
      INTEGER FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,L0,ISTYP,SUB_ID,
     .   NODTAG(NUMNOD)
      my_real
     .   DELTAT,ALPHA,FAC_T,A,B,C,D,E,F,POS,R
      CHARACTER MESS*40,TITR*nchartitle,CHAR8*ncharline
      CHARACTER KEY2*ncharfield
      my_real
     .   BID, XM, YM, ZM, X1, Y1, Z1, X2, Y2, Z2, NORM,
     .   X3, Y3, Z3, N3, PNOR1, PNOR2, PNORM1, DET, DET1, DET2, DET3
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,NODGRNR5,ELEGROR
      EXTERNAL USR2SYS,NODGRNR5,ELEGROR
C
C
      DATA MESS/'SECTION DEFINITION                      '/
      DATA IUN/1/
C=======================================================================
      NOPRINT = 1
      FLAG_FMT = 0
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
C
      K0 = 31
      KR0= 11
      NSTRF(25)=K0
      NSTRF(26)=KR0
      L0 = 7
      NG = 0
      IGUQ=0

      CALL HM_OPTION_START('/SECT')
      
      DO I=1,NSECT
        ISTYP = 0
        NG=NG+1
        LREC = LREC+3
        K1 = K0+30
        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR, UNIT_ID=UID, SUBMODEL_ID=SUB_ID, KEYWORD2=KEY2)

        CALL HM_GET_INTV('Axis_Origin_Node_N1', NSTRF(K0+3), IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('Axis_Node_N2', NSTRF(K0+4), IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('Axis_Node_N3', NSTRF(K0+5), IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('ISAVE', NSTRF(K0), IS_AVAILABLE, LSUBMODEL)  

        CALL HM_GET_STRING('file_name', CHAR8, ncharline, IS_AVAILABLE)
        ILEN=LEN(CHAR8)
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
          CALL HM_GET_INTV('Grnod_ID', IGU, IS_AVAILABLE, LSUBMODEL)  
          CALL HM_GET_INTV('System_Id', NFRAM, IS_AVAILABLE, LSUBMODEL)  
          ISTYP = 0
        ENDIF        

        CALL HM_GET_FLOATV('detltaT', DELTAT, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('alpha', ALPHA, IS_AVAILABLE, LSUBMODEL, UNITAB)
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
                
        IFLAGUNIT = 0                                            
        DO J=1,NUNITS  
            IF (UNITAB%UNIT_ID(J) == UID) THEN    
              FAC_T = UNITAB%FAC_T(J)
              IFLAGUNIT = 1                                        
              EXIT                                                 
            ENDIF                                                  
        ENDDO                                                    
        IF (UID/=0.AND.IFLAGUNIT==0) THEN                    
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,I2=UID,I1=ID,C1='SECTION',C2='SECTION',C3=TITR) 
        ENDIF                                                    

        SECTIDS(I)=ID
        
        IF(IGU == 0 .AND. NFRAM == 0 .AND. ISTYP == 0) THEN
          CALL ANCMSG(MSGID=507,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)
        ENDIF

        DO J=1,ncharline
          NOM_SECT((I-1)*ncharline+J) = ICHAR(CHAR8(J:J))
        ENDDO



        IF((IGUS==0))THEN                                                                                
          CALL ANCMSG(MSGID=600,                                                                           
     .                MSGTYPE=MSGWARNING,                                                                  
     .                ANMODE=ANINFO_BLIND_1,                                                               
     .                I1=ID,                                                                               
     .                C1=TITR)                                                                             
        END IF     
        
        DO J=1,NBINTER
          CALL HM_GET_INT_ARRAY_INDEX('int_id' ,NSTRF(K1-1+J) ,J ,IS_AVAILABLE, LSUBMODEL) 
        ENDDO  
                                                                        
        IF (ISTYP == 1) THEN                                                                               
          CALL HM_GET_FLOATV('XTail', XM, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('YTail', YM, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('ZTail', ZM, IS_AVAILABLE, LSUBMODEL, UNITAB)                                                 
          IF(SUB_ID /= 0)CALL SUBROTPOINT(XM,YM,ZM,RTRANS,SUB_ID,LSUBMODEL)                                          

          CALL HM_GET_FLOATV('cnode1_x', X1, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('cnode1_y', Y1, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('cnode1_z', Z1, IS_AVAILABLE, LSUBMODEL, UNITAB)                                                  
          IF(SUB_ID /= 0) CALL SUBROTPOINT(X1,Y1,Z1,RTRANS,SUB_ID,LSUBMODEL)                                          

          CALL HM_GET_FLOATV('cnode2_x', X2, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('cnode2_y', Y2, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('cnode2_z', Z2, IS_AVAILABLE, LSUBMODEL, UNITAB)                                                   
          IF(SUB_ID /= 0)CALL SUBROTPOINT(X2,Y2,Z2,RTRANS,SUB_ID,LSUBMODEL)                                          
                                                                                    
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
          IF(SUB_ID /= 0)CALL SUBROTPOINT(XM,YM,ZM,RTRANS,SUB_ID,LSUBMODEL)
          
          CALL HM_GET_FLOATV('Normal_x', A, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('Normal_y', B, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('Normal_z', C, IS_AVAILABLE, LSUBMODEL, UNITAB)                                                 
          IF(SUB_ID /= 0)CALL SUBROTVECT(A,B,C,RTRANS,SUB_ID,LSUBMODEL)

          CALL HM_GET_FLOATV('Radius', R, IS_AVAILABLE, LSUBMODEL, UNITAB)
                                                                                      
          D = XM                                                                                           
          E = YM                                                                                           
          F = ZM                                                                                           
          NORM = A*A+B*B+C*C                                                                               
          A = A/SQRT(NORM)                                                                                 
          B = B/SQRT(NORM)                                                                                 
          C = C/SQRT(NORM)                                                                                 
        ENDIF                                                                                              

        DO J=1,NBINTER                                                                                     
         DO L=1,NINTER                                                                                     
          IF(NSTRF(K1-1+J)==IPARI(15,L))THEN                                                             
            IPARI(28,L) = IPARI(28,L) + 1                                                                  
C numero interne      NSTRF(K1-1+J) = L
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
             IF(NFRAM==ISKN(4,JJ)) THEN                                
	       A = XFRAME(7,J)                                           
	       B = XFRAME(8,J)                                           
	       C = XFRAME(9,J)                                           
               D = XFRAME(10,J)                                          
               E = XFRAME(11,J)                                          
               F = XFRAME(12,J)                                          
	       N1 = ISKN(1,JJ)                                           
               IF (NSTRF(K0+3) == 0 ) THEN                               
                 IF (ISKN(1,JJ) /= 0) THEN                             
                   NSTRF(K0+3) = ITAB(ISKN(1,JJ))                        
                 ELSE                                                    
                   CALL ANCMSG(MSGID=742,                                
     .                         MSGTYPE=MSGERROR,                         
     .                         ANMODE=ANINFO,                            
     .                         I1=ID,                                    
     .                         C1=TITR,                                  
     .                         C2='N1',                                  
     .                         I2=NFRAM)                                 
                 ENDIF                                                   
               ENDIF                                                     
               IF (NSTRF(K0+4) == 0 ) THEN                               
                 IF (ISKN(2,JJ) /= 0) THEN                             
                   NSTRF(K0+4) = ITAB(ISKN(2,JJ))                        
                 ELSE                                                    
                   CALL ANCMSG(MSGID=742,                                
     .                         MSGTYPE=MSGERROR,                         
     .                         ANMODE=ANINFO,                            
     .                         I1=ID,                                    
     .                         C1=TITR,                                  
     .                         C2='N2',                                  
     .                         I2=NFRAM)                                 
                 ENDIF                                                   
               ENDIF                                                     
               IF (NSTRF(K0+5) == 0 ) THEN                               
                 IF (ISKN(3,JJ) /= 0) THEN                             
                   NSTRF(K0+5) = ITAB(ISKN(3,JJ))                        
                 ELSE                                                    
                   CALL ANCMSG(MSGID=742,                                
     .                         MSGTYPE=MSGERROR,                         
     .                         ANMODE=ANINFO,                            
     .                         I1=ID,                                    
     .                         C1=TITR,                                  
     .                         C2='N3',                                  
     .                         I2=NFRAM)                                 
                 ENDIF                                                   
               ENDIF                                                     
             ENDIF                                                       
           ENDDO                                                         
          ENDIF                                                          
          KK=1+NGRNOD                                                    
	  NNOD = 0                                                       
	  CPT = 1                                                        
	  CALL SEC_NODES_SOL(IGUS,ISTYP,NGRBRIC,IGRBRIC,X0,A,            
     2                  B,C,D,E,F,ITAB,IXS,IXS10,IXS16,IXS20,            
     3                  NIXS,KK,NNOD,NSTRF,NBINTER,N1 ,K1,               
     4                  CPT,NODTAG,ISOLNOD,TAGELEMS,                     
     5                  X1,Y1,Z1,X2,Y2,Z2,R)                             

          KK=KK+NGRBRIC                                                  
        ENDIF                                                            

        K2=K1+NBINTER                               

        IF (NFRAM == 0 .AND. ISTYP == 0) THEN       
          NNOD=NODGRNR5(IGU,IGS,NSTRF(K2),IGRNOD,ITABM1,MESS)                                 
        ENDIF                                       

        K3=K2+NNOD                                                                               
        NSEGS=ELEGROR(IGUS,IGRBRIC,NGRBRIC,'BRIC',NSTRF(K3),2,MESS,NFRAM,TAGELEMS,ISTYP,ID,TITR) 
        K4=K3+2*NSEGS                                                                            
        NSEGQ=0                                                                                  
        K5=K4+2*NSEGQ                                                                            
        NSEGC=0                                                                                  
        K6=K5+2*NSEGC                                                                            
        NSEGT=0                                                                                  
        K7=K6+2*NSEGT                                                                            
        NSEGP=0                                                                                  
        K8=K7+2*NSEGP                                                                            
        NSEGR=0                                                                                  
        K9=K8+2*NSEGR                                                                            
        NSEGTG=0                                                                                 
C
        IF(NSEGS+NSEGQ+NSEGC+NSEGT+NSEGP+NSEGR+NSEGTG==0)THEN
          CALL ANCMSG(MSGID=1813,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                I1= ID,
     .                C1= TITR)
        END IF
C
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
            IF (NSTRF(L)/=0) THEN
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

        PNOR1=SQRT(X1*X1+Y1*Y1+Z1*Z1)
        IF (PNOR1 < EM20) THEN
           CALL ANCMSG(MSGID=508,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR)
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
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR)
          ENDIF
        ENDIF
C
C-------------------------------------------------------------------------
C      SOLIDES
C--------------------------------------------------------------
       CALL SECSTRI(NSEGS,NSTRF(K3),IXS,IXS10,IXS16,IXS20,NSTRF(K2),NNOD,ITAB,I,NOPRINT)
       IF(NSTRF(K0)>=102)THEN
         CALL ZERORE(1,10+30*NNOD,SECBUF(KR0))
       ELSEIF(NSTRF(K0)>=101)THEN
         CALL ZERORE(1,10+24*NNOD,SECBUF(KR0))
       ELSEIF(NSTRF(K0)>=100)THEN
         CALL ZERORE(1,10+12*NNOD,SECBUF(KR0))
       ELSE
         CALL ZERORE(1,10,SECBUF(KR0))
       ENDIF
       SECBUF(KR0) = DELTAT
       SECBUF(KR0+1) = ZERO
       SECBUF(KR0+2) = ALPHA
       SECBUF(KR0+3) = ZERO

       IF(NSTRF(K0)==1.OR.NSTRF(K0)==2)THEN
         IF(SECBUF(1)==ZERO)THEN
           SECBUF(1) = DELTAT
          ELSEIF(ABS((SECBUF(1)-DELTAT)/SECBUF(1)) > EM06 )THEN
            CALL ANCMSG(MSGID=356,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR)
         ENDIF
       ENDIF

       IF(NSTRF(K0)>=1.AND.NSTRF(K0)<=10)THEN
         NSTRF(1)=NSTRF(1)+1
       ELSEIF(NSTRF(K0)>=100.AND.NSTRF(K0)<=200)THEN
         NSTRF(2)=NSTRF(2)+1
         DO J=1,8
           NSTRF(15+J)=NSTRF(K0+14+J)
         ENDDO
       ENDIF
       IF(NSTRF(K0)==1)THEN
         LREC = LREC+6*NNOD
       ELSEIF(NSTRF(K0)==2)THEN
         LREC = LREC+12*NNOD
       ENDIF

       NSTRF(K0+23) = ID
       NSTRF(K0+24) = K9+2*NSEGTG
       NSTRF(K0+25) = KR0+10
       IF(NSTRF(K0)>=100)NSTRF(K0+25) = NSTRF(K0+25)+12*NNOD
       IF(NSTRF(K0)>=101)NSTRF(K0+25) = NSTRF(K0+25)+12*NNOD
       IF(NSTRF(K0)>=102)NSTRF(K0+25) = NSTRF(K0+25)+6*NNOD

       KR0 = NSTRF(K0+25)
       K0  = NSTRF(K0+24)
C-------------------------------------------------------------
        !IF (FLAG_FMT /= 0) THEN
        !  CALL FMT_DEF(INVERS_INIT,IFIX_INIT)
        !ENDIF
        NODTAG = 0
        TAGELEMS  = 0
      ENDDO
      CALL UDOUBLE(SECTIDS,1,NSECT,MESS,0,BID)

C file rec length
      NSTRF(6)=LREC*4
      
C-------------------------------------------------------------
 2900 FORMAT(/' SECTION',I10,' ID',I10/
     +        ' ---------------'/
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
      END
