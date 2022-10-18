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
Chd|  HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|-- called by -----------
Chd|        HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|        HM_THVARVENT                  source/output/th/hm_thvarent.F
Chd|        HORD                          source/output/th/hord.F       
Chd|        NAME_FVBAG                    source/output/th/hm_thgrki_vent.F
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|        HM_THVARC                     source/output/th/hm_read_thvarc.F
Chd|        R2R_EXIST                     source/coupling/rad2rad/routines_r2r.F
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_THGRKI_VENT(
     1      ITYP  ,KEY    ,INOPT1,
     3      IAD   ,IFI    ,ITHGRP,ITHBUF ,
     4      NV    ,VARE  ,NUM    ,VARG  ,NVG    ,
     5      IVARG ,NSNE,NV0,ITHVAR,FLAGABF,NVARABF,
     6      NOM_OPT,IGS,T_MONVOL,NVARMVENT,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
      USE MESSAGE_MOD
      USE MONVOL_STRUCT_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
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
      INTEGER ITYP,INOPT1,
     .        ITHGRP(NITHGR),ITHBUF(*),
     .        IFI,IAD,NV,NUM,NVG,NSNE  ,IVARG(18,*),
     .        NV0,ITHVAR(*),FLAGABF,NVARABF,ID_VENT(10)
      CHARACTER*10 VARE(NV),KEY,VARG(NVG)
      INTEGER NOM_OPT(LNOPT1,*),NVARMVENT
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
      TYPE(SUBMODEL_DATA) :: LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,JJ, I,ISU,ID,NNE,NOSYS,NTOT,KK,IER,
     .        OK,IGS,IGRS,NSU,K,L,CONT,IAD0,IADV,NTRI,
     .        IFITMP,IADFIN,NVAR,M,N,IAD1,IAD2,ISK,IPROC,VARVENT(NVARMVENT),
     .        NBMONVOL,NBVENT,NVAR_TMP,ITYP_MONV,
     .        NVENT(NVOLU),NBVENT_MAX,N_BAK,IDSMAX,
     .        K1,K2,KIBJET,KIBHOL
      CHARACTER TITR*nchartitle,MESS*40,TITR1*nchartitle,TMP_CHAR*40
      CHARACTER*20 VENT_NAME(10,NVOLU)
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,ULIST2S,LISTCNT,NINTRN,THVARC
      INTEGER,EXTERNAL :: HM_THVARC
      INTEGER R2R_LISTCNT,R2R_EXIST
      DATA MESS/'TH GROUP DEFINITION                     '/
C-----------------------------------------------
C
C
C-----------------------------------------------
      VENT_NAME(1:10,1:NVOLU) = ''

      NVENT(1:NVOLU) = 0
      VARVENT(1:NVARMVENT) = 0
      ID=ITHGRP(1)
      ID_VENT(1:10) = 0
      NBVENT_MAX = 0
      CALL FRETITL2(TITR1,ITHGRP(NITHGR-LTITR+1),LTITR)
      ITHGRP(2)=ITYP
      ITHGRP(3)=0
      IFITMP=IFI+1000

      CALL HM_GET_INTV('idsmax',NBMONVOL,IS_AVAILABLE,LSUBMODEL)       
      CALL HM_GET_INTV('Number_Of_Variables',NVAR,IS_AVAILABLE,LSUBMODEL)       
      IF(NVAR>0)NVAR=HM_THVARC(VARE,NV,ITHBUF(IAD),VARG,NVG,IVARG,NV0,ID,TITR1,LSUBMODEL)

      K1=1
      K2=1+NIMV*NVOLU
      KIBJET=K2+LICBAG
      KIBHOL=KIBJET+LIBAGJET
      DO N=1,NVOLU                                                       
        ITYP_MONV=T_MONVOL(N)%TYPE                                       
        NVENT(N)=T_MONVOL(N)%NVENT                                       
        IF (NVENT(N) /= 0) THEN                                        
           CALL NAME_FVBAG(T_MONVOL(N)%IBAGHOL,VENT_NAME(1,N),NVENT(N))  
        ENDIF                                                            
      ENDDO                                                              

      CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL) 
        
      ! Loop over Objects IDs                                                             
      DO K = 1,IDSMAX                                                                     
        CALL HM_GET_INT_ARRAY_INDEX('ids',N,K,IS_AVAILABLE,LSUBMODEL) 
        CALL HM_GET_INT_ARRAY_INDEX('SKEW_ARRAY',ISK,K,IS_AVAILABLE,LSUBMODEL) 
        CALL HM_GET_STRING_INDEX('NAME_ARRAY',TITR,K,40,IS_AVAILABLE) 
        IF (NSUBDOM>0) THEN
C----------->Multidomaines - on saute si l'entite n'existe plus-------- 
          IF(R2R_EXIST(ITYP,N)==0) CYCLE
C---------------------------------------------------------------------- 
        ENDIF 
        N_BAK=N
        N=0        
        DO J=1,NUM                                                
          IF(N_BAK==NOM_OPT(1,INOPT1+J))THEN
            N=J                         
            EXIT
          ENDIF
        ENDDO                                                     
        IF(N==0)THEN                                            
           CALL FRETITL2(TITR1,ITHGRP(NITHGR-LTITR+1),LTITR)      
           CALL ANCMSG(MSGID=257,                                 
     .                 MSGTYPE=MSGERROR,                          
     .                 ANMODE=ANINFO_BLIND_1,                     
     .                 I1=ITHGRP(1),                              
     .                 C1=TITR1,                                  
     .                 C2=KEY,                                    
     .                 I2=N_BAK)                                 
        ELSE                                                      
          NBVENT_MAX = MAX(NBVENT_MAX,NVENT(N))                   
        ENDIF                                                     
      ENDDO
c
      CALL HM_THVARVENT(VARE,NV,ITHBUF(IAD),VARG,NVG,IVARG,NV0,ID,TITR1,VARVENT,NBVENT_MAX,LSUBMODEL)
c  
      NBVENT = 0
      DO I=1,10
        DO J=1,5
          IF (VARVENT( 5*(I-1) + J ) == 1) THEN 
            NBVENT = NBVENT + 1
            ID_VENT(NBVENT) = I
            EXIT
          ENDIF
        ENDDO
      ENDDO
c
      IF (NBVENT == 0 .OR. NBVENT_MAX == 0) THEN
        IGS = IGS - 1
        ITHGRP(1:NITHGR)=0
      ELSE
c
        NNE = NBVENT * NBMONVOL
c
        NVAR = 0
        DO I=1,10
          NVAR_TMP = 0
          DO J=1,5
            IF (VARVENT((I-1)*5+J) == 1) THEN
              NVAR_TMP = NVAR_TMP + 1
            ENDIF
          ENDDO
          NVAR = MAX(NVAR,NVAR_TMP)
        ENDDO
c
        IF(NVAR == 0) CALL ANCMSG(MSGID=1109,
     .     MSGTYPE=MSGERROR,
     .     ANMODE=ANINFO_BLIND_1,
     .     I1=ID,
     .     C1=TITR1 )
c
        ITHGRP(6)=NVAR
        ITHGRP(7)=IAD
        IAD=IAD+NVAR
        IFI=IFI+NVAR
        ITHGRP(4)=NNE
        ITHGRP(5)=IAD
        IAD2=IAD+3*NNE
        ITHGRP(8)=IAD2
        IFI=IFI+3*NNE+40*NNE
        CALL ZEROIN(IAD,IAD+43*NNE-1,ITHBUF)
        
C
        DO KK = 1,IDSMAX                                                                     
          CALL HM_GET_INT_ARRAY_INDEX('ids',N,KK,IS_AVAILABLE,LSUBMODEL) 
          CALL HM_GET_INT_ARRAY_INDEX('SKEW_ARRAY',ISK,KK,IS_AVAILABLE,LSUBMODEL) 
          CALL HM_GET_STRING_INDEX('NAME_ARRAY',TITR,KK,40,IS_AVAILABLE) 
          IF(N/=0)THEN
               IF (NSUBDOM>0) THEN
C----------->Multidomaines - on saute si l'entite n'existe plus-------- 
                 IF(R2R_EXIST(ITYP,N)==0) CYCLE
               ENDIF
C---------------------------------------------------------------------- 
          ENDIF          
          N_BAK = N  
          N=0                                                  
          DO J=1,NUM                                              
            IF(N_BAK==NOM_OPT(1,INOPT1+J))THEN
              N=J
              EXIT
            ENDIF                       
          ENDDO                                                   
          IF(N==0)THEN                                          
             CALL FRETITL2(TITR1,ITHGRP(NITHGR-LTITR+1),LTITR)    
             CALL ANCMSG(MSGID=257,                               
     .                   MSGTYPE=MSGERROR,                        
     .                   ANMODE=ANINFO_BLIND_1,                   
     .                   I1=ITHGRP(1),                            
     .                   C1=TITR1,                                
     .                   C2=KEY,                                  
     .                   I2=N_BAK)                                
          ENDIF                                                   
          DO J=1,NBVENT                                           
            NSNE=NSNE+1                                           
            ITHBUF(IAD)=N                                         
            IAD=IAD+1                                             
          ENDDO                                                   
        ENDDO
C
        IAD = ITHGRP(5)
        CALL HORD(ITHBUF(IAD),NNE)
C
        N=ITHBUF(IAD)
        DO K=1,NBMONVOL
         DO I=1,NBVENT
          N=ITHBUF(IAD)
          ITHBUF(IAD+2*NNE)=ID_TH_AUTO
          ID_TH_AUTO = ID_TH_AUTO + 1

          DO J=1,20
            ITHBUF(IAD2+J-1)=NOM_OPT(J+LNOPT1-LTITR,INOPT1+N)
          ENDDO
          CALL FRETITL2(TITR1,ITHBUF(IAD2),40)

          IF (I <= NVENT(K)) THEN
            WRITE(TMP_CHAR,FMT='(I2,A)')  ID_VENT(I),VENT_NAME(I,K) 
          ELSE
            WRITE(TMP_CHAR,FMT='(I2,A)')  ID_VENT(I),''
          ENDIF
          TITR1(21:40) = TMP_CHAR(1:20) 
          CALL FRETITL(TITR1,ITHBUF(IAD2),40)

          IAD=IAD+1
          IAD2=IAD2+40
         ENDDO
        ENDDO
C
      IAD=IAD2
C
C=======================================================================
C ABF FILES
C=======================================================================
          NVAR=ITHGRP(6)
          IAD0=ITHGRP(7)
          ITHGRP(9)=NVARABF
          DO J=IAD0,IAD0+NVAR-1
            DO K=1,10
              ITHVAR((ITHGRP(9)+(J-IAD0)-1)*10+K)=
     .              ICHAR(VARE(ITHBUF(J))(K:K))
            ENDDO
          ENDDO
          NVARABF = NVARABF + NVAR
C=======================================================================
C PRINTOUT
C=======================================================================
        IF(IPRI<1)RETURN
C
        N=ITHGRP(4)
        IAD1=ITHGRP(5)
        NVAR=ITHGRP(6)
        IAD0=ITHGRP(7)
        IAD2=ITHGRP(8)
        WRITE(IOUT,'(//)')
        CALL FRETITL2(TITR1,ITHGRP(NITHGR-LTITR+1),LTITR)
        WRITE(IOUT,'(A,I10,3A,I3,A,I5,2A)')' TH GROUP:',ITHGRP(1),
     .         ',',TITR1,',',NVAR,' VAR',N, KEY,':'
        WRITE(IOUT,'(A)')' -------------------'
        WRITE(IOUT,'(10A10)')(VARE(ITHBUF(J)),J=IAD0,IAD0+NVAR-1)
        WRITE(IOUT,'(3A)')'    ',KEY,'        NAME '
        DO K=IAD1,IAD1+N-1
           CALL FRETITL2(TITR1,ITHBUF(IAD2),40)
           IAD2=IAD2+40
           WRITE(IOUT,FMT=FMW_I_A)NOM_OPT(1,INOPT1+ITHBUF(K)),
     .             TITR1(1:40)
        ENDDO
      ENDIF
      RETURN
      END
C
Chd|====================================================================
Chd|  NAME_FVBAG                    source/output/th/hm_thgrki_vent.F
Chd|-- called by -----------
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|====================================================================
      SUBROUTINE NAME_FVBAG(IBAGHOL, VENT_NAME, NVENT )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBAGHOL(NIBHOL,*),NVENT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,IVENT,TITREVENT(20)
      CHARACTER*20 VENTTITLE,VENT_NAME(*)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      DO IVENT=1,NVENT
         DO K=1,20
           TITREVENT(K)=IBAGHOL(14+K,IVENT)
           VENTTITLE(K:K) = ACHAR(TITREVENT(K))
         ENDDO
         VENT_NAME(IVENT)=VENTTITLE
      ENDDO
C
      RETURN
      END
