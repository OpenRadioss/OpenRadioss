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
Chd|  HM_READ_THGRNS                source/output/th/hm_read_thgrns.F
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
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|        HM_THVARC                     source/output/th/hm_read_thvarc.F
Chd|        NINTRN                        source/system/nintrn.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_THGRNS(
     1      ITYP  ,KEY    ,ITAB   ,ITABM1,KXX    ,
     3      IXX   ,IAD    ,IFI    ,ITHGRP,ITHBUF ,
     4      NV    ,VARE   ,VARG   ,NVG   ,IVARG  ,
     5      NSNE  ,IVNS2R, NV0    ,ID    ,TITR   ,
     6      ITHVAR,FLAGABF,NVARABF, LSUBMODEL)
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
#include      "scr23_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,ITYP,ITABM1(*),KXX(NIXX,*),IXX(*),
     .        ITAB(*),ITHGRP(NITHGR),ITHBUF(*),
     .        IFI,IAD,NV,NVG,IVARG(18,*),NSNE,
     .        IVNS2R(18,*),NV0,ITHVAR(*),FLAGABF,NVARABF
      CHARACTER*10 VARE(NV),KEY,VARG(NVG)
      CHARACTER TITR*nchartitle
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,JJ, I,ISU,ID,NNE,NOSYS,J10(10),NTOT,KK,IER,
     .        OK,IGS,IGRS,NSU,K,L,JREC,CONT,IAD0,IADV,NTRI,NL,
     .        IFITMP,IADFIN,NVAR,M,N,IAD1,IAD2,ISK,IPROC,
     .        IDNS, INS, IUN, IST, NST, IDST
      CHARACTER TITLE*nchartitle,MESS*40,CSTRAND1*9,CSTRAND2*13
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)      
      LOGICAL IS_AVAILABLE
      INTEGER LENTRIM
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NINTRN,THVARC,HM_THVARC
      DATA MESS/'TH GROUP DEFINITION                     '/
      DATA IUN/1/,
     .     CSTRAND1/'STRAND_ID'/,CSTRAND2/'STRAND_NUMBER'/
C-----------------------------------------------
      ID=ITHGRP(1)
      CALL FRETITL2(TITR,ITHGRP(NITHGR-LTITR+1),LTITR)
      ITHGRP(2)=ITYP
      ITHGRP(3)=0                                                                                                   
      IFITMP=IFI+1000                                                                                               
      ! Number of variables indicated by the user                                                                   
      CALL HM_GET_INTV('Number_Of_Variables',NVAR,IS_AVAILABLE,LSUBMODEL)                                           

      ! Number of stored variables and reading the variables                                                        
      IF (NVAR>0) NVAR = HM_THVARC(VARE,NV,ITHBUF(IAD),VARG,NVG,IVARG,NV0,ID,TITR ,LSUBMODEL)                       
      IF(NVAR == 0) CALL ANCMSG(MSGID=1109,                                                                         
     .   MSGTYPE=MSGERROR,                                                                                          
     .   ANMODE=ANINFO_BLIND_1,                                                                                     
     .   I1=ID,                                                                                                     
     .   C1=TITR )                                                                                                  
c
      ITHGRP(6)=NVAR                                                                                                
      ITHGRP(7)=IAD                                                                                                 
      IAD=IAD+NVAR                                                                                                  
      IFI=IFI+NVAR                                                                                                  
C
       !JREC=IREC+1                                                                                                 
       !READ(IIN,REC=JREC,ERR=999,FMT='(A)')LINE                                                                    
       !READ(LINE,ERR=999,FMT=FMT_I)IDNS                                                                            
       !CALL FRETITL2(TITR,ITHGRP(NITHGR-LTITR+1),LTITR)                                                            
       !INS =NINTRN(IDNS,KXX,NIXX,NUMELX,ITHGRP(1),TITR)                                                            
       !NNE =KXX(3,INS)-1                                                                                           
       CALL HM_GET_INTV('ids',IDNS,IS_AVAILABLE,LSUBMODEL)                                                          
       INS =NINTRN(IDNS,KXX,NIXX,NUMELX,ITHGRP(1),TITR)                                                             
       NNE =KXX(3,INS)-1                                                                                            
C
C
       !NST=0                                                                                                       
       !DOWHILE(LINE(1:1)/='/')                                                                                   
       ! READ(LINE,ERR=999,FMT=FMT_THGR)IST,IDST,TITR                                                               
       ! IF (IST>NNE) THEN                                                                                       
       !    CALL FRETITL2(TITR,ITHGRP(NITHGR-LTITR+1),LTITR)                                                        
       !    CALL ANCMSG(MSGID=361,                                                                                  
     . !                MSGTYPE=MSGERROR,                                                                           
     . !                ANMODE=ANINFO_BLIND_1,                                                                      
     . !                I1=ITHGRP(1),                                                                               
     . !                C1=TITR,                                                                                    
     . !                I2=IST)                                                                                     
       !    GOTO 999                                                                                                
       ! ENDIF                                                                                                      
       ! NST=NST+1                                                                                                  
       ! JREC=JREC+1                                                                                                
       ! READ(IIN,REC=JREC,ERR=999,FMT='(A)')LINE                                                                   
       !ENDDO                                                                                                       

       CALL HM_GET_INTV('Num_Cards',NST,IS_AVAILABLE,LSUBMODEL)                                                     

C
      ITHGRP(4)=NST                                                                                                 
      ITHGRP(5)=IAD                                                                                                 
      IAD2=IAD+4*NST                                                                                                
      ITHGRP(8)=IAD2                                                                                                
      IFI=IFI+4*NST+40*NST                                                                                          
      CALL ZEROIN(IAD,IAD+44*NST-1,ITHBUF)                                                                          
C
      DO K=1,NST                                                                                                    
        !IREC=IREC+1                                                                                                
        ITHBUF(IAD)=INS                                                                                             
        !READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE                                                                   
        CALL HM_GET_INT_ARRAY_INDEX('XELEM_NUM',IST,K,IS_AVAILABLE,LSUBMODEL)                                       
        CALL HM_GET_INT_ARRAY_INDEX('XELEM_USER',IDST,K,IS_AVAILABLE,LSUBMODEL)                                     
        CALL HM_GET_STRING_INDEX('NAME_ARRAY',TITLE,K,80,IS_AVAILABLE)  
        LENTRIM = LEN_TRIM(TITLE)
        TITLE = TITLE(1:LENTRIM)                                 

        IF (IST > NNE) THEN                                                                                         
           CALL FRETITL2(TITR,ITHGRP(NITHGR-LTITR+1),LTITR)                                                         
           CALL ANCMSG(MSGID=361,                                                                                   
     .                 MSGTYPE=MSGERROR,                                                                            
     .                 ANMODE=ANINFO_BLIND_1,                                                                       
     .                 I1=ITHGRP(1),                                                                                
     .                 C1=TITR,                                                                                     
     .                 I2=IST)                                                                                      
           GOTO 999                                                                                                 
        ENDIF                                                                                                       
                                                                                                                    
        !READ(LINE,ERR=999,FMT=FMT_THGR)IST,IDST,TITLE                                                              
        ITHBUF(IAD+2*NST)=IDST                                                                                      
        ITHBUF(IAD+3*NST)=IST                                                                                       
        IPROC=0                                                                                                     
        ITHBUF(IAD+NST)=IPROC                                                                                       
        CALL FRETITL(TITLE,ITHBUF(IAD2),40)                                                                         
        IAD=IAD+1                                                                                                   
        IAD2=IAD2+40                                                                                                
      ENDDO                                                                                                         
C
      IAD = ITHGRP(5)                                                                                               
      IAD2= ITHGRP(8)                                                                                               
      IAD=IAD2+40*NST                                                                                               
C
      NSNE=NSNE+NST
      
C=======================================================================
C ABF FILES
C=======================================================================
      NVAR = ITHGRP(6)
      IAD0 = ITHGRP(7)
      ITHGRP(9) = NVARABF
      DO J = IAD0,IAD0+NVAR-1
        DO K = 1,10
          ITHVAR((ITHGRP(9)+(J-IAD0)-1)*10+K) = ICHAR(VARE(ITHBUF(J))(K:K))
        ENDDO
      ENDDO
      NVARABF = NVARABF + NVAR
                                                                                                             
C=======================================================================
C PRINTOUT
C=======================================================================
      IF(IPRI >= 1) THEN
        N=ITHGRP(4)
        IAD1=ITHGRP(5)
        NVAR=ITHGRP(6)
        IAD0=ITHGRP(7)
        IAD2=ITHGRP(8)
        WRITE(IOUT,'(//)')
        CALL FRETITL2(TITR,ITHGRP(NITHGR-LTITR+1),LTITR)
        WRITE(IOUT,'(A,I10,3A,I3,A,I2,2A,I10)')' TH GROUP:',ITHGRP(1),',',TRIM(TITR),',',NVAR,' VAR',IUN,KEY,':',IDNS
        WRITE(IOUT,'(A)')' -------------------'
        WRITE(IOUT,'(10A10)')(VARE(ITHBUF(J)),J=IAD0,IAD0+NVAR-1)
        WRITE(IOUT,'(4A)')CSTRAND1,' ',CSTRAND2,' P_SPMD       NAME '
        DO K=IAD1,IAD1+N-1
         CALL FRETITL2(TITR,ITHBUF(IAD2),40)
         WRITE(IOUT,'(3I10,2A)')ITHBUF(K+2*N),ITHBUF(K+3*N),ITHBUF(K+N),' ',TITR(1:40)
         IAD2=IAD2+40
        ENDDO
      ENDIF

      IAD1=ITHGRP(7)
      DO I=1,NVAR
       ITHBUF(IAD1+I-1)=IVNS2R(ITHBUF(IAD1+I-1),1)
      ENDDO
      
      RETURN
 999  CALL FREERR(1)
      RETURN
      END
