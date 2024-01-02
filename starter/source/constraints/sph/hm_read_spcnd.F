Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  HM_READ_SPCND                 source/constraints/sph/hm_read_spcnd.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_SPCND(ISPCOND ,ISKEW  ,ITAB   ,ITABM1 ,IKINE  ,
     .                         IGRNOD  ,NOD2SP ,IFRAME ,NOM_OPT,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE R2R_MOD
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
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISPCOND(NISPCOND,*), ISKEW(*), ITAB(*), ITABM1(*),
     .        IKINE(*),NOD2SP(*),IFRAME(LISKN,*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID, IC, N, IS, IC1, IC2, IC3, IC4,
     .        NOSYS, J,IGR,IGRS,K,
     .        NCELL,
     .        ILEV, NY
      CHARACTER MESS*40,TITR*nchartitle,KEY*ncharkey,DIR*ncharfield
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C-----------------------------------------------
      DATA MESS/'SPECIFIC TO SPH SYMMETRY CONDITIONS      '/
C-----------------------------------------------
      WRITE(IOUT,1000)
      NY = 0
      CALL HM_OPTION_START('/SPHBCS')

        DO I=1,NSPCOND
          NY=NY+1
C----------Multidomaines --> on ignore les BCS SPH non tagees-----------
          IF(NSUBDOM>0)THEN
              IF(TAGSPHBCS(NY)==0)CALL HM_SZ_R2R(TAGSPHBCS,NY,LSUBMODEL)
          END IF
C--------------------------------------------------
C EXTRACT DATAS OF /SPHBCS/... LINE
C--------------------------------------------------
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                         OPTION_ID = ID,
     .                         OPTION_TITR = TITR,
     .                         KEYWORD2 = KEY)
          NOM_OPT(1,I)=ID
          CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
          IF (KEY(1:5)=='SLIDE')THEN
           ISPCOND(5,I)=1
          ELSEIF (KEY(1:4)=='TIED')THEN
           ISPCOND(5,I)=0
          ELSE
             CALL ANCMSG(MSGID=398,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   C1=KEY)
          ENDIF
          ISPCOND(NISPCOND,I)=ID
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
          CALL HM_GET_INTV('inputsystem',IS,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('entityid',IGR,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('rad_sphbcs_ilev',ILEV,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (STRING)
C--------------------------------------------------
          CALL HM_GET_STRING('rad_dir',DIR,ncharfield,IS_AVAILABLE)
C
          IF(DIR(1:1)=='X')THEN
            IC=1
          ELSEIF(DIR(1:1)=='Y')THEN
            IC=2
          ELSEIF(DIR(1:1)=='Z')THEN
            IC=3
          ENDIF
          IF(ILEV/=0.AND.ILEV/=1)THEN
             CALL ANCMSG(MSGID=399,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ILEV)
          ENDIF
          ISPCOND(1,I)=ILEV
          ISPCOND(2,I)=IC
          IF (IS==0)THEN
           ISPCOND(3,I)=1
           GOTO 111
          ELSE
           DO J=1,NUMFRAM
            IF (IFRAME(4,J+1)==IS)THEN
             ISPCOND(3,I)=J+1
             GOTO 111
            ENDIF
           ENDDO
          ENDIF
             CALL ANCMSG(MSGID=400,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IS)
 111      CONTINUE
C
          INGR2USR => IGRNOD(1:NGRNOD)%ID
          IGRS=NGR2USR(IGR,INGR2USR,NGRNOD)
          ISPCOND(4,I)=IGRS
C
          DO NOSYS=1,NUMNOD
              NCELL=NOD2SP(NOSYS)
              IF (NCELL==0) THEN
              ELSE
              ENDIF
          ENDDO
          WRITE(IOUT,1100)ID,TRIM(TITR),DIR(1:1),IS,IGR,ILEV
        ENDDO
C-------------------------------------
      RETURN
C
1000  FORMAT(
     . '      SPECIFIC TO SPH SYMMETRY CONDITIONS '/
     . '      ---------------------------------- '/)
1100  FORMAT(/5X,'CONDITION ID                               ',I10,1X,A
     .       /10X,'NORMAL DIRECTION TO SYMMETRY PLANE         ',A10,
     .       /10X,'REFERENCE FRAME ID                        ',I10,
     .       /10X,'NODES GROUP ID FOR KINEMATIC CONDITIONS   ',I10,
     .       /10X,'FORMULATION LEVEL                         ',I10)
      END
