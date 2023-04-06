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
Chd|  HM_READ_RBODY_LAGMUL          source/constraints/general/rbody/hm_read_rbody_lagmul.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        NODGRNR6                      source/starter/freform.F      
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_RBODY_LAGMUL(RBYL    ,NPBYL   ,LPBYL   ,IGRNOD   ,LSUBMODEL ,
     .                                ITAB    ,ITABM1  ,IKINE   ,IKINE1LAG,NOM_OPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE R2R_MOD
      USE SUBMODEL_MOD
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
#include      "units_c.inc"
#include      "param_c.inc"
#include      "lagmult.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBYL(NNPBY,*),LPBYL(*), 
     .        ITAB(*), ITABM1(*),IKINE(*),IKINE1LAG(*)
C     REAL
      my_real
     .   RBYL(NRBY,*)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,K,L,N,NR,MM,ID,IGU,IGS,
     .        NSKEW,NSL,MSL,SUB_INDEX,NRB,NRB_R2R
      CHARACTER MESS*40,TITR*nchartitle,KEY*ncharkey
      LOGICAL IS_AVAILABLE
      DATA MESS/'RIGID BODY DEFINITIONS'/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  USR2SYS,NODGRNR6
      EXTERNAL USR2SYS,NODGRNR6
C-----------------------------------
C   NPBYL(1,N) = MAIN NODE
C   NPBYL(2,N) = NUMBER OF SECONDARY NODES + MAIN
C   NPBYL(3,N) = 
C   NPBYL(4,N) = 
C   NPBYL(5,N) = 
C   NPBYL(6,N) = IDENTIFICATEUR
C   NPBYL(7,N) = 
C   NPBYL(8,N) = 
C======================================================================|
      WRITE(IOUT,1000)
C---
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/RBODY')
C---
      K   = 0
      NRB = 0
      NRB_R2R = 0
C
      DO NR=1,NRBODY
C
C--------------------------------------------------
C EXTRACT DATAS OF /RBODY/... LINE
C--------------------------------------------------
C
       NRB_R2R = NRB_R2R + 1     
       IF (NSUBDOM > 0) THEN
         IF(TAGRBY(NRB_R2R) == 0) CALL HM_SZ_R2R(TAGRBY,NRB_R2R,LSUBMODEL)
       ENDIF
C
       KEY=''
       CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY,
     .                       SUBMODEL_INDEX = SUB_INDEX)
       IF(KEY(1:6)=='LAGMUL')THEN     
        NRB = NRB + 1
        IF (NSUBDOM > 0) THEN ! TAGRBY is allocated only if NSUBDOM>0
          IF(TAGRBY(NRB) == 0) CALL HM_SZ_R2R(TAGRBY,NRB,LSUBMODEL)
        ENDIF
C---
        NOM_OPT(1,NRB)=ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,NRB),LTITR)
C---
        CALL HM_GET_INTV('node_ID',MM,IS_AVAILABLE,LSUBMODEL)
        MM  = USR2SYS(MM,ITABM1,MESS,ID)
C---
        CALL HM_GET_INTV('grnd_ID',IGU,IS_AVAILABLE,LSUBMODEL)
        NSL = NODGRNR6(MM,IGU,IGS,LPBYL(K+1),IGRNOD,ITABM1,MESS,ID)
        MSL = NSL+1
C---
        LPBYL(K+MSL) = MM
C---
        IF (NSL == 0) THEN
          CALL ANCMSG(MSGID=352,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR)
        ENDIF
        CALL ANODSET(MM, CHECK_RB_M)
        DO J=1, NSL
          CALL ANODSET(LPBYL(J+K), CHECK_RB_S)
        ENDDO
C---
        DO J=1,MSL
          CALL KINSET(512,ITAB(LPBYL(J+K)),IKINE(LPBYL(J+K)),7,0,
     .                IKINE1LAG(LPBYL(J+K)))
        ENDDO
C---
        NPBYL(1,NRB) = MM
        NPBYL(2,NRB) = MSL
        NPBYL(6,NRB) = ID
        LAG_NCL = LAG_NCL + NSL*6
        LAG_NKL = LAG_NKL + NSL*21
C---
        WRITE(IOUT,1100)ID,TRIM(TITR),ITAB(MM),MSL
        WRITE(IOUT,1101)
        WRITE(IOUT,1102) (ITAB(LPBYL(J+K)),J=1,NSL)
        K = K + 3*MSL
       END IF ! IF(KEY(1:6)=='LAGMUL')THEN  
      ENDDO
C---
      RETURN
C------------------------------
1000  FORMAT(
     . /'      RIGID BODY DEFINITIONS (LAGRANGE MULTIPLIERS)'
     . /'      -------------------------------------------- '/)
1100  FORMAT( /5X,'RIGID BODY ID ',I10,1X,A,
     .       /10X,'PRIMARY NODE                            ',I10
     .       /10X,'NUMBER OF NODES                         ',I10)
1101  FORMAT( 10X,'SECONDARY NODES ')
1102  FORMAT(  9X,10I10)
      END
