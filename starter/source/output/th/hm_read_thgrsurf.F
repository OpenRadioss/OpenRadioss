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
!||    hm_read_thgrsurf         ../starter/source/output/th/hm_read_thgrsurf.F
!||--- called by ------------------------------------------------------
!||    hm_read_thgrou           ../starter/source/output/th/hm_read_thgrou.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    fretitl                  ../starter/source/starter/freform.F
!||    fretitl2                 ../starter/source/starter/freform.F
!||    hm_get_int_array_index   ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_thvarc                ../starter/source/output/th/hm_read_thvarc.F
!||    hord                     ../starter/source/output/th/hord.F
!||    nintrn                   ../starter/source/system/nintrn.F
!||    r2r_exist                ../starter/source/coupling/rad2rad/routines_r2r.F
!||    r2r_listcnt              ../starter/source/coupling/rad2rad/routines_r2r.F
!||    ulist2s                  ../starter/source/system/sysfus.F
!||    usr2sys                  ../starter/source/system/sysfus.F
!||    zeroin                   ../starter/source/system/zeroin.F
!||--- uses       -----------------------------------------------------
!||    format_mod               ../starter/share/modules1/format_mod.F90
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_THGRSURF(
     1      ITYP   ,KEY    ,
     3      IAD    ,IFI    ,ITHGRP   ,ITHBUF ,
     4      NV     ,VARE   ,NUM      ,VARG   ,NVG     ,
     5      IVARG  ,NSNE   ,NV0      ,ITHVAR ,FLAGABF ,NVARABF,
     6      IGRSURF,IGS    ,LSUBMODEL)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Starter Reader for option /TH/SURF
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD , only : SURF_
      USE MESSAGE_MOD , only : ANCMSG, aninfo_blind_1, MSGERROR, MSGWARNING
      USE SUBMODEL_MOD , only : SUBMODEL_DATA, NSUBMOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      USE FORMAT_MOD , ONLY : FMW_I_A
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: ITYP,NV,NUM,NVG,IVARG(18,*),NV0,FLAGABF
      INTEGER,INTENT(INOUT) :: ITHGRP(NITHGR), IGS, IFI, IAD, NSNE, NVARABF, ITHVAR(*), ITHBUF(*)
      CHARACTER*10,INTENT(IN) :: VARE(NV),KEY,VARG(NVG)
      TYPE (SURF_),INTENT(INOUT), DIMENSION(NSURF) :: IGRSURF
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,JJ, I,ISU,ID,NNE,NOSYS,NTOT,KK,IER,
     .        OK,IGRS,NSU,K,L,JREC,CONT,IAD0,IADV,NTRI,
     .        IFITMP,IADFIN,NVAR,M,N,IAD1,IAD2,ISK,IPROC,ITITLE(LTITR),
     .        IDS,IDSMAX,
     .        NUM_FOUND
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER MESS*40
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,ULIST2S,LISTCNT,NINTRN,THVARC,HM_THVARC
      INTEGER R2R_LISTCNT,R2R_EXIST
      DATA MESS/'TH GROUP DEFINITION                     '/
C-----------------------------------------------
C   S o u r c e   L i n e s
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

      IF(NVAR == 0) THEN
        IF(ITYP /= 116)THEN
         CALL ANCMSG(MSGID=1109,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,I1=ID,C1=TITR )
        ENDIF
        IGS = IGS - 1
        ITHGRP(1:NITHGR)=0
      ELSE
        CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)  
        ITHGRP(6)=NVAR
        ITHGRP(7)=IAD
        IAD=IAD+NVAR
        IFI=IFI+NVAR
        NNE=IDSMAX
        ITHGRP(4)=NNE
        ITHGRP(5)=IAD
        IAD2=IAD+3*NNE
        ITHGRP(8)=IAD2
        IFI=IFI+3*NNE+40*NNE
        CALL ZEROIN(IAD,IAD+43*NNE-1,ITHBUF)      
C
        NUM_FOUND=0
        DO K = 1,IDSMAX
          CALL HM_GET_INT_ARRAY_INDEX('ids',IDS,K,IS_AVAILABLE,LSUBMODEL) 
          N=0
          IF(IDS/=0)THEN
            IF (NSUBDOM>0) THEN
              ! Multidomain : skip if entity does not exist---------------
              IF(R2R_EXIST(ITYP,IDS)==0) CYCLE !next K
              ENDIF      
            DO J=1,NUM
              IF(IDS == IGRSURF(J)%ID)THEN
                 N=J
                 IGRSURF(J)%TH_SURF=1          
                 NUM_FOUND=NUM_FOUND+1
                 EXIT
              ENDIF
            ENDDO
            NSNE=NSNE+1
            ITHBUF(IAD)=N
            IAD=IAD+1
          ENDIF
          IF(N == 0)THEN
            CALL FRETITL2(TITR,ITHGRP(NITHGR-LTITR+1),LTITR)
            CALL ANCMSG(MSGID=257, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1, I1=ITHGRP(1), I2=IDS, C1=TITR, C2=KEY)
          ENDIF             
        ENDDO !next K
C
        IAD = ITHGRP(5)
        CALL HORD(ITHBUF(IAD),NUM_FOUND)
C
        DO I=1,NNE
          N=ITHBUF(IAD)
          IF(N > 0)THEN
            ITHBUF(IAD+2*NNE)=IGRSURF(N)%ID
            DO J=1,40
              CALL FRETITL(IGRSURF(N)%TITLE,ITITLE,LTITR)
              ITHBUF(IAD2+J-1)=ITITLE(J)
            ENDDO
            IAD=IAD+1
            IAD2=IAD2+40
          ENDIF
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
              ITHVAR((ITHGRP(9)+(J-IAD0)-1)*10+K)=ICHAR(VARE(ITHBUF(J))(K:K))
            ENDDO
          ENDDO
          NVARABF = NVARABF + NVAR
C=======================================================================
C PRINTOUT
C=======================================================================
          IF(IPRI<1)RETURN
          N=ITHGRP(4)
          IAD1=ITHGRP(5)
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
             WRITE(IOUT,FMT=FMW_I_A)IGRSURF(ITHBUF(K))%ID,TITR(1:40)
          ENDDO
C
        ENDIF
      RETURN
      END
