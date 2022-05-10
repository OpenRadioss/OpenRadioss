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
Chd|  HM_READ_THGRNE                source/output/th/hm_read_thgrne.F
Chd|-- called by -----------
Chd|        HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL                       source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|        HORD3                         source/output/th/hord3.F      
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|        HM_THVARC                     source/output/th/hm_read_thvarc.F
Chd|        R2R_NIN                       source/coupling/rad2rad/routines_r2r.F
Chd|        R2R_SYS                       source/coupling/rad2rad/routines_r2r.F
Chd|        SET_USRTOS                    source/model/sets/ipartm1.F   
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_THGRNE(
     1      ITYP   ,KEY    ,ITAB  ,ITABM1 ,IX     ,
     3      NIX    ,IAD    ,IFI   ,ITHGRP ,ITHBUF ,
     4      NV     ,VARE   ,NUMEL ,VARG   ,NVG    ,
     5      IVARG  ,NSNE   ,NV0   ,IMERGE ,ITHVAR ,
     6      FLAGABF,NVARABF,RFI   ,LSUBMODEL,MAP, MAPSIZE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,ITYP,ITABM1(*),IX(NIX,*),
     .        ITAB(*),ITHGRP(NITHGR),ITHBUF(*),
     .        IFI,IAD,NV,NUMEL,NVG,IVARG(18,*),NSNE,
     .        NV0,IMERGE(*),ITHVAR(*),FLAGABF,NVARABF,RFI
      CHARACTER*10 VARE(NV),KEY,VARG(NVG),KEY1
      INTEGER, INTENT(in) :: MAPSIZE
      INTEGER, DIMENSION(MAPSIZE,2), INTENT(in) :: MAP
      TYPE(SUBMODEL_DATA) :: LSUBMODEL(NSUBMOD)      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,JJ, I,ISU,ID,NNE,NOSYS,J10(10),NTOT,KK,IER,
     .        OK,IGS,IGRS,NSU,K,L,CONT,IAD0,IADV,NTRI,NL,
     .        IFITMP,IADFIN,NVAR,M,N,IAD1,IAD2,ISK,IPROC,JC,
     .        IDSMAX,SIZ
      INTEGER :: ID_LOCAL
      CHARACTER TITR*nchartitle,MESS*40,DIRMSG*3,
     .          TITR1*nchartitle
      LOGICAL IS_AVAILABLE
      my_real BID       
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,ULIST2S,LISTCNT,NINTRN,HM_THVARC
      INTEGER R2R_SYS,R2R_NIN
      INTEGER SET_USRTOS
      EXTERNAL SET_USRTOS
      DATA MESS/'TH GROUP                      '/
C=======================================================================
      IS_AVAILABLE = .FALSE.
      ID=ITHGRP(1)
      CALL FRETITL2(TITR1,ITHGRP(NITHGR-LTITR+1),LTITR)
      ITHGRP(2)=ITYP
      ITHGRP(3)=0                                                
      IFITMP=IFI+1000                                            
      ! Number of variables indicated by the user
      CALL HM_GET_INTV('Number_Of_Variables',NVAR,IS_AVAILABLE,LSUBMODEL) 

      ! Number of stored variables and reading the variables
      IF (NVAR>0) NVAR = HM_THVARC(VARE,NV,ITHBUF(IAD),VARG,NVG,IVARG,NV0,ID,TITR1 ,LSUBMODEL) 
c
      IF(NVAR == 0) CALL ANCMSG(MSGID=1109,
     .   MSGTYPE=MSGERROR,
     .   ANMODE=ANINFO_BLIND_1,
     .   I1=ID,
     .   C1=TITR1 )
c
      ITHGRP(6)=NVAR                                             
      ITHGRP(7)=IAD                                              
      IAD=IAD+NVAR                                               
      IFI=IFI+NVAR
      RFI=0	                                              	                                              
C
      CONT=1
      NNE=0                                                      
C------------------------
      ! Number of Objects IDs
      CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)  

      ! Loop over Objects IDs                                                             
      DO K = 1,IDSMAX                                                                     
        CALL HM_GET_INT_ARRAY_INDEX('ids',N,K,IS_AVAILABLE,LSUBMODEL) 
        IF (NSUBDOM /= 0) THEN                                                            
C         Multidomains-> on ignore les noeuds et elements supprimes----                                                         
          IF (ITYP.EQ.0) THEN                                                             
             IF(R2R_SYS(N,ITABM1,MESS) /= 0) NNE=NNE+1                                    
          ELSE                                                                            
            IF(R2R_NIN(N,IX,NIX,NUMEL) /= 0) NNE=NNE+1                                    
          ENDIF                                                                           
        ELSE                                                                              
          NNE = NNE+1                                                                     
        ENDIF                                                                             
      ENDDO	      
C------------------------
      ITHGRP(4)=NNE                                        
      ITHGRP(5)=IAD                                        
      IAD2=IAD+LVARITHB*NNE                                       
      ITHGRP(8)=IAD2                                       
      IFI=IFI+LVARITHB*NNE+40*NNE                                 
      CALL ZEROIN(IAD,IAD+(40+LVARITHB)*NNE-1,ITHBUF)
      RFI=0                 
C------------------------
      DO K=1,NNE    
        CALL HM_GET_INT_ARRAY_INDEX('ids',N,K,IS_AVAILABLE,LSUBMODEL) 
        CALL HM_GET_INT_ARRAY_INDEX('SKEW_ARRAY',ISK,K,IS_AVAILABLE,LSUBMODEL) 
        CALL HM_GET_STRING_INDEX('NAME_ARRAY',TITR,K,40,IS_AVAILABLE) 
        SIZ=LEN_TRIM(TITR)
        TITR(SIZ+1:SIZ+1)=' '     
        IF (ITYP == 0) THEN                               
C         Multidomains-> on ignore les noeuds supprimes----		    	 		  		  
	  IF(NSUBDOM.NE.0) THEN
              IF (R2R_SYS(N,ITABM1,MESS).EQ.0) CYCLE
	  ENDIF  
          ITHBUF(IAD)=USR2SYS(N,ITABM1,MESS,ID)                          
          ITHBUF(IAD+NNE)=ISK                                         
          DO JC = 1,NMERGED                                           
            IF (ITHBUF(IAD) == IMERGE(JC))                            
     .          ITHBUF(IAD)  = IMERGE(NUMCNOD+JC)                     
          ENDDO                                                         
        ELSE                                                                           
C         Multidomains-> on ignore les elements supprimes----		    	 		  		  
	  IF(NSUBDOM.NE.0) THEN                                   
	      IF (R2R_NIN(N,IX,NIX,NUMEL).EQ.0) CYCLE             
	  ENDIF  

          ID_LOCAL = SET_USRTOS(N,MAP,MAPSIZE)
          IF(ID_LOCAL == 0) THEN
                CALL ANCMSG(MSGID=69,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ITHGRP(1),
     .                      C1=TITR1,
     .                      I2=N)
                ITHBUF(IAD)=0
          ELSE
                ITHBUF(IAD)=MAP(ID_LOCAL,2)
          ENDIF                         
          IPROC=0                      
          ITHBUF(IAD+NNE)=IPROC    
          ITHBUF(IAD+3*NNE)=ISK 
          IF(ISK/=0) THEN
             RFI=RFI+2             
          ENDIF                                        
        ENDIF                                                        
        CALL FRETITL(TITR,ITHBUF(IAD2),40)                           
        IAD=IAD+1                                                    
        IAD2=IAD2+40                                                 
      ENDDO                                                          
C------------------------
C
      IAD = ITHGRP(5)                                                
      IAD2= ITHGRP(8)   
      CALL HORD3(ITHBUF(IAD),NNE,ITHBUF(IAD+NNE),ITHBUF(IAD2),40) 
C
      IF(ITYP.EQ.0) THEN                                             
       IAD0=ITHGRP(7)                                                
       DO I=IAD0,IAD0+NVAR-1
         IF((IREAC == 0) .AND. (ITHBUF(I) == 620 .OR.                
     .       ITHBUF(I) == 621 .OR. ITHBUF(I) == 622 .OR.        
     .        ITHBUF(I) == 623 .OR. ITHBUF(I) == 624 .OR.        
     .        ITHBUF(I) == 625)) IREAC = 1                       
         IF((ITHBUF(I) == 626 .OR. ITHBUF(I) == 627 .OR.        
     .       ITHBUF(I) == 628) .AND.                            
     .      ((ISUB == 0.AND.ISECUT == 0.AND.IISROT == 0.AND.   
     .       MSHSUB == 0.AND.IMPOSE_DR == 0 .AND. IDROT == 0) .OR.   
     .       IRODDL == 0)) THEN                           
               IF (ITHBUF(I) == 626)DIRMSG='DRX'           
               IF (ITHBUF(I) == 627)DIRMSG='DRY'               
               IF (ITHBUF(I) == 628)DIRMSG='DRZ'            
               CALL FRETITL2(TITR,ITHGRP(NITHGR-LTITR+1),LTITR)
               CALL ANCMSG(MSGID=774,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=ITHGRP(1),
     .                     C1=TITR,
     .                     I2=ITHGRP(1),
     .                     C2=DIRMSG)
         ENDIF   
         IF(ITHBUF(I) == 19 .AND. ITHERM_FE == 0 ) THEN
               CALL FRETITL2(TITR,ITHGRP(NITHGR-LTITR+1),LTITR)
               CALL ANCMSG(MSGID=1087,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=ITHGRP(1),
     .                     C1=TITR,
     .                     I2=ITHGRP(1),
     .                     C2='TEMP')
         ENDIF                                                            
       ENDDO                                                         
       DO I=1,NNE                                                    
         K = ITHBUF(IAD)                                             
         ITHBUF(IAD+2*NNE)=ITAB(K)                                   
         IAD=IAD+1                                                   
       ENDDO                                                         
       IAD=ITHGRP(5)                                                 
       CALL UDOUBLE(ITHBUF(IAD+2*nne),1,NNE,MESS,0,BID)              
      ELSE   ! Not used in the program. Now is used for saving skew number when it's defined                                                        
       DO I=1,NNE                                                    
         K = ITHBUF(IAD)                                             
         ITHBUF(IAD+2*NNE)=IX(NIX,K)                                 
         IAD=IAD+1                                                   
       ENDDO                                                         
       IAD=ITHGRP(5)                                                 
       CALL UDOUBLE(ITHBUF(IAD+2*nne),1,NNE,MESS,0,BID)              
      ENDIF                                                          
C
      IAD=IAD2+40*NNE                                                
C
      NSNE=NSNE+NNE                                                  
C===============================
C ABF FILES
C===============================
        NVAR=ITHGRP(6)
        IAD0=ITHGRP(7)
        ITHGRP(9)=NVARABF
        DO J=IAD0,IAD0+NVAR-1
          DO K=1,10
            ITHVAR((ITHGRP(9)+(J-IAD0)-1)*10+K)=
     .            ICHAR(VARE(ITHBUF(J))(K:K))
          ENDDO
        ENDDO
        NVARABF = NVARABF + NVAR
C===============================
C PRINTOUT
C===============================
      IF (IPRI < 1) RETURN
C
        N=ITHGRP(4)
        IAD1=ITHGRP(5)
        NVAR=ITHGRP(6)
        IAD0=ITHGRP(7)
        IAD2=ITHGRP(8)
        WRITE(IOUT,'(//)')
        IF ( KEY == 'NODE' ) THEN
          IF ( N >= 2 ) THEN
            KEY1 = 'NODES'
          ELSE
            KEY1 = 'NODE'  
          ENDIF
        ELSE 
          KEY1 = KEY  
        ENDIF  
        CALL FRETITL2(TITR,ITHGRP(NITHGR-LTITR+1),LTITR)
        WRITE(IOUT,'(A,I10,3A,I3,A,I5,A,2A)')'TH GROUP:',ITHGRP(1),',',TRIM(TITR),',',NVAR,' VAR,',N,' ',KEY1,':'
        WRITE(IOUT,'(A)')' -------------------'
        IF(ITYP.EQ.0)THEN
          WRITE(IOUT,'(10A10)')(VARE(ITHBUF(J)),J=IAD0,IAD0+NVAR-1)
          WRITE(IOUT,'(A)')'    NODE  SKEW(OR FRAME)     NAME '
          DO K=IAD1,IAD1+N-1
            CALL FRETITL2(TITR,ITHBUF(IAD2),40)
            IAD2=IAD2+40
            WRITE(IOUT,'(2I10,8X,2A)')ITAB(ITHBUF(K)),ITHBUF(K+N),' ',TITR(1:40)
          ENDDO
        ELSE
          WRITE(IOUT,'(10A10)')(VARE(ITHBUF(J)),J=IAD0,IAD0+NVAR-1)
          WRITE(IOUT,'(2A)')KEY,'  P_SPMD       NAME '
          DO K=IAD1,IAD1+N-1
            CALL FRETITL2(TITR,ITHBUF(IAD2),40)
            IAD2=IAD2+40
            WRITE(IOUT,'(2I10,2A)')IX(NIX,ITHBUF(K)),ITHBUF(K+N),' ',TITR(1:40)
          ENDDO
        ENDIF
C---------------------------------------------------------
      RETURN
 999  CALL FREERR(1)
      RETURN
      END
