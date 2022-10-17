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
Chd|  HM_READ_THGRKI_RBODY          source/output/th/hm_read_thgrki_rbody.F
Chd|-- called by -----------
Chd|        HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HORD                          source/output/th/hord.F       
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|        HM_THVARC                     source/output/th/hm_read_thvarc.F
Chd|        R2R_EXIST                     source/coupling/rad2rad/routines_r2r.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_THGRKI_RBODY(
     1      ITYP   ,KEY    ,INOPT1,
     3      IAD    ,IFI    ,ITHGRP, ITHBUF     ,
     4      NV     ,VARE   ,NUM   , VARG      ,NVG    ,
     5      IVARG  ,NSNE   ,NV0   , ITHVAR    ,FLAGABF, NVARABF,
     6      NOM_OPT,IGS    ,NPBY  , LSUBMODEL)
C
        USE MESSAGE_MOD
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
#include      "submod_c.inc"
#include      "com04_c.inc"
#include      "lagmult.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ITYP,INOPT1,
     .          ITHGRP(NITHGR),ITHBUF(*),
     .          IFI,IAD,NV,NUM,NVG,NSNE  ,IVARG(18,*),
     .          NV0,ITHVAR(*),FLAGABF,NVARABF,IGS
        CHARACTER*10 VARE(NV),KEY,VARG(NVG)
        INTEGER NOM_OPT(LNOPT1,*)
        TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      INTEGER NPBY(NNPBY,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER J,JJ, I,ISU,ID,NNE,NOSYS,J10(10),NTOT,KK,IER,
     .          OK,IGRS,NSU,K,L,JREC,CONT,IAD0,IADV,NTRI,
     .          IFITMP,IADFIN,NVAR,M,N,IAD1,IAD2,ISK,IPROC,
     .          IDSMAX,IDS,
     .        TAG(NUM),IRB
        CHARACTER, DIMENSION(10) :: VAR
        LOGICAL :: IS_AVAILABLE
        CHARACTER TITR*nchartitle
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER,EXTERNAL :: HM_THVARC,R2R_EXIST
C-----------------------------------------------
        IS_AVAILABLE = .FALSE.
        NSNE = 0
        ! ID of the /TH
        ID = ITHGRP(1)
        ! Title of the /TH
        CALL FRETITL2(TITR,ITHGRP(NITHGR-LTITR+1),LTITR)
        ! Type of the /TH
        ITHGRP(2)=ITYP
        ITHGRP(3)=0
        IFITMP=IFI+1000
c
        ! Number of variables indicated by the user
        CALL HM_GET_INTV('Number_Of_Variables',NVAR,IS_AVAILABLE,LSUBMODEL) 
c
        ! Number of stored variables and reading the variables
        IF (NVAR>0) NVAR = HM_THVARC(VARE,NV,ITHBUF(IAD),VARG,NVG,IVARG,NV0,ID,TITR,LSUBMODEL) 
c
        ! Filling the tables
        IF (NVAR == 0) THEN
          IF (ITYP /= 107)
     .      CALL ANCMSG(MSGID=1109,
     .           MSGTYPE=MSGERROR ,
     .           ANMODE=ANINFO_BLIND_1,
     .           I1=ID,
     .           C1=TITR )
          IGS = IGS - 1
          ITHGRP(1:NITHGR) = 0
        ELSE
c
          ! Number of Objects IDs
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)  
c
          ! Filling tables
          ITHGRP(6) = NVAR
          ITHGRP(7) = IAD
          IAD       = IAD+NVAR
          IFI       = IFI+NVAR
          NNE       = IDSMAX
          !ITHGRP(4) = NNE
          ITHGRP(5) = IAD
          IAD2      = IAD+3*NNE
          ITHGRP(8) = IAD2
          CALL ZEROIN(IAD,IAD+43*NNE-1,ITHBUF)
          NNE = 0  
          TAG(:)=0  
C
          ! Loop over Objects IDs
          DO K = 1,IDSMAX
            CALL HM_GET_INT_ARRAY_INDEX('ids',IDS,K,IS_AVAILABLE,LSUBMODEL) 
            IF (NSUBDOM > 0) THEN 
              IF (R2R_EXIST(ITYP,IDS) == 0) CYCLE
            ENDIF
            IF(IDS==0)CYCLE !skip empty object
            !check then if object_id does exist
            N = 0
            DO J = 1,NUM
              IF (IDS == NOM_OPT(1,INOPT1+J)) THEN
                N   = J
                EXIT
              ENDIF
            ENDDO
            IF (N == 0) THEN
              CALL FRETITL2(TITR,ITHGRP(NITHGR-LTITR+1),LTITR)
              CALL ANCMSG(MSGID=257,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ITHGRP(1),
     .                    C1=TITR,
     .                    C2=KEY,
     .                    I2=IDS)
            ELSE
              IF(NPBY(12,N) /= 0) THEN 
                IRB=NPBY(13,N)
                IF(TAG(IRB) == 0) THEN
                  NNE=NNE+1
                  NSNE=NSNE+1
                  ITHBUF(IAD)=IRB
                  IAD=IAD+1
                  TAG(IRB)=1
                ENDIF
              ELSEIF(TAG(N) == 0) THEN
                NNE=NNE+1
                NSNE=NSNE+1
                ITHBUF(IAD)=N
                IAD=IAD+1
                TAG(N)=1
              ENDIF
            ENDIF           
          ENDDO
C
          IAD       = ITHGRP(5)
          ITHGRP(4) = NNE
          IAD2      = IAD+3*NNE
          ITHGRP(8) = IAD2
          IFI       = IFI+3*NNE+40*NNE   
cc
          CALL HORD(ITHBUF(IAD),NNE)
C
          DO I = 1,NNE
            N = ITHBUF(IAD)
            ITHBUF(IAD+2*NNE) = NOM_OPT(1,INOPT1+N)
            DO J = 1,40
              ITHBUF(IAD2+J-1)=NOM_OPT(J+LNOPT1-LTITR,INOPT1+N)
            ENDDO
            IAD=IAD+1
            IAD2=IAD2+40
          ENDDO    
C
          IAD = IAD2
C
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
          IF(IPRI<1) RETURN
C
          N = ITHGRP(4)
          IAD1 = ITHGRP(5)
          NVAR=ITHGRP(6)
          IAD0=ITHGRP(7)
          IAD2=ITHGRP(8)
          WRITE(IOUT,'(//)')
          CALL FRETITL2(TITR,ITHGRP(NITHGR-LTITR+1),LTITR)
          WRITE(IOUT,'(A,I10,3A,I3,A,I5,2A)')' TH GROUP:',ITHGRP(1),',',TRIM(TITR),',',NVAR,' VAR',N, KEY,':'
          WRITE(IOUT,'(A)')' -------------------'
          WRITE(IOUT,'(10A10)')(VARE(ITHBUF(J)),J=IAD0,IAD0+NVAR-1)
          WRITE(IOUT,'(3A)')'    ',KEY,'        NAME '
          DO K=IAD1,IAD1+N-1
             CALL FRETITL2(TITR,ITHBUF(IAD2),40)
             IAD2=IAD2+40
             WRITE(IOUT,FMT=FMW_I_A)NOM_OPT(1,INOPT1+ITHBUF(K)),
     .                              TITR(1:40)
          ENDDO
C
        ENDIF
        RETURN
      END
