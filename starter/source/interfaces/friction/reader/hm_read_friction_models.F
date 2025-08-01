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
!||    hm_read_friction_models   ../starter/source/interfaces/friction/reader/hm_read_friction_models.F
!||--- called by ------------------------------------------------------
!||    lectur                    ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                    ../starter/source/output/message/message.F
!||    hm_option_read_key        ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start           ../starter/source/devtools/hm_reader/hm_option_start.F
!||    hm_read_friction          ../starter/source/interfaces/friction/reader/hm_read_friction.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod        ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod               ../starter/share/message_module/message_mod.F
!||    r2r_mod                   ../starter/share/modules1/r2r_mod.F
!||    reader_old_mod            ../starter/share/modules1/reader_old_mod.F90
!||    submodel_mod              ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FRICTION_MODELS(
     1        NOM_OPT                ,UNITAB          ,IGRPART          ,IPART         ,TAGPRT_FRIC,
     2        TABCOUPLEPARTS_FRIC_TMP,TABCOEF_FRIC_TMP,INTBUF_FRIC_TAB  ,
     .        NSETFRICTOT            ,
     3        IFLAG                  ,COEFSLEN        ,IORTHFRICMAX     ,IFRICORTH_TMP ,NGRPF      ,
     4        LENGRPF                ,LENG            ,NSETMAX          ,LSUBMODEL )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE INTBUF_FRIC_MOD        
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      USE READER_OLD_MOD , ONLY : IREC
C-----------------------------------------------      
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER IFLAG ,NSETFRICTOT ,COEFSLEN ,IORTHFRICMAX ,NGRPF ,LENG ,NSETMAX
      INTEGER IPART(LIPART1,*) ,TAGPRT_FRIC(*),
     .        TABCOUPLEPARTS_FRIC_TMP(NINTERFRIC,*),IFRICORTH_TMP(NINTERFRIC,*),
     .        LENGRPF

      my_real TABCOEF_FRIC_TMP(NINTERFRIC,*)

      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
      TYPE (SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRPART) :: IGRPART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIF ,NIN , NSET ,NGRPF0 ,NOINTF ,SUB_ID ,UID ,IFLAGUNIT ,J
      CHARACTER(LEN=NCHARTITLE) :: TITR
C
C=======================================================================
C     READING FRICTION Model /FRICTION
C=======================================================================
C
      NIF=0
C--------------------------------------------------
C WRITE TITLE IN OUT FILE
C--------------------------------------------------
      IF(IFLAG==1)    WRITE(IOUT,1000)
C

C--------------------------------------------------
C START BROWSING MODEL FRICTIONS
C--------------------------------------------------
      CALL HM_OPTION_START('/FRICTION')
C--------------------------------------------------
C BROWSING FRICTION MODELS 1->NINTERFRIC
C--------------------------------------------------
      DO NIN=1,NINTERFRIC
c
C--------------------------------------------------
C EXTRACT DATAS OF /FRICTION 
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = NOINTF,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_ID = SUB_ID,
     .                       OPTION_TITR = TITR)
C--------------------------------------------------
C CHECK IF UID IS EXISTING
C--------------------------------------------------
        IFLAGUNIT = 0
        DO J=1,UNITAB%NUNITS
          IF (UNITAB%UNIT_ID(J) == UID) THEN    
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UID/=0.AND.IFLAGUNIT==0) THEN
           CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .           I2=UID,I1=NOINTF,C1='FRITION',
     .           C2='FRICTION',
     .                 C3=TITR)
        ENDIF

C--------------------------------
C        Friction model defining
C-------------------------

         NIF=NIF+1
c        
         NSET = 0
         NGRPF0 = NGRPF
C   
         CALL HM_READ_FRICTION(
     1        NIF      ,NOM_OPT    ,TITR         ,UNITAB                 ,IGRPART         ,
     2        IPART    ,NSET       ,TAGPRT_FRIC  ,TABCOUPLEPARTS_FRIC_TMP,TABCOEF_FRIC_TMP,
     3        INTBUF_FRIC_TAB(NIF)%FRICMOD,INTBUF_FRIC_TAB(NIF)%IFFILTER ,
     .             INTBUF_FRIC_TAB(NIF)%XFILTR_FRIC,INTBUF_FRIC_TAB(NIF)%FRICFORM         ,
     4        IFLAG    ,INTBUF_FRIC_TAB(NIF)%IORTHFRIC,IFRICORTH_TMP     ,NGRPF          ,
     5        LENGRPF  ,LENG         ,NOINTF      , LSUBMODEL )  

         IF(IFLAG == 0) THEN
           NSET = NGRPF - NGRPF0 
           NSET = NSET * NSET
           NSETFRICTOT = NSETFRICTOT + NSET
           NSETMAX =MAX(NSETMAX,NSET)
           INTBUF_FRIC_TAB(NIF)%NSETPRTS = NSET
           IORTHFRICMAX = MAX(IORTHFRICMAX,INTBUF_FRIC_TAB(NIF)%IORTHFRIC)
         ELSE
           NSETFRICTOT = NSETFRICTOT + NSET
           INTBUF_FRIC_TAB(NIF)%NSETPRTS = NSET
           NSETMAX =MAX(NSETMAX,NSET)
           IORTHFRICMAX = MAX(IORTHFRICMAX,INTBUF_FRIC_TAB(NIF)%IORTHFRIC) 
         ENDIF 
         IREC=IREC+1

      ENDDO
C
C=======================================================================
      RETURN
C-----
 1000 FORMAT(    /1X,'   FRICTION INTERFACES      ' /
     .            1X,'   --------------------           '// )

      END SUBROUTINE HM_READ_FRICTION_MODELS
