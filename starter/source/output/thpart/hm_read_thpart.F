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
Chd|  HM_READ_THPART                source/output/thpart/hm_read_thpart.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        NINTRIGR                      source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_THPART(IPART    ,IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N,
     .                     IGRTRUSS ,IGRBEAM ,IGRSPRING, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*)
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER PID,MID,SID,ID,I,IMID,IPID,ISID,K,ITH,IGTYP,N,
     .   GR,IGR
      CHARACTER TITR*nchartitle,MESS*40,TYP*6
      INTEGER IDS, CNT, FLAG_FMT, FLAG_FMT_TMP, IFIX_TMP,NGROU,ITYP
      CHARACTER*nchartitle,TITR1
      CHARACTER*ncharkey KEY
      LOGICAL IS_ENCRYPTED,IS_AVAILABLE,IS_FOUND_SURF
      my_real BID
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NINTRI,NINTRIGR
      DATA MESS/' THPART DEFINITION                      '/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      WRITE(IOUT,'(//A)')'     THPARTS' 
      WRITE(IOUT,'(A//)')'       -----' 

      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IS_FOUND_SURF = .FALSE.

      IGRELEM = 0
      IF(NTHPART>0) IGRELEM = 1
      CALL HM_OPTION_START('/THPART')
      
      DO I=1,NTHPART

        TITR = ''
        TYP  = ''            
        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID = ID,OPTION_TITR = TITR ,KEYWORD2 = KEY   )
        CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

        TYP(1:6)=KEY(1:6)
        TITR1=TITR
        CALL FRETITL(TITR,IPART(LIPART1-LTITR+1,NPART+I),LTITR)
         CALL HM_GET_INTV('grelem_ID',  GR  ,IS_AVAILABLE,LSUBMODEL)        
        
C ITYP : 1 BRIC, 
C        2 QUAD, 
C        3 SHELL, 
C        4 TRUSS, 
C        5 BEAM, 
C        6 SPRINGS,
C        7 SHELL_3N

        IF (TYP(1:6) == 'GRBRIC') THEN
          ITYP = 1
          IGR = NINTRIGR(GR,IGRBRIC,NGRBRIC)
          IF (ITYP == IGRBRIC(IGR)%GRTYPE) IS_FOUND_SURF = .TRUE.

        ELSEIF (TYP(1:6) == 'GRQUAD') THEN
          ITYP = 2
          IGR = NINTRIGR(GR,IGRQUAD,NGRQUAD)
          IF (ITYP == IGRQUAD(IGR)%GRTYPE) IS_FOUND_SURF = .TRUE.

        ELSEIF (TYP(1:6) == 'GRSHEL') THEN
          ITYP = 3
          IGR = NINTRIGR(GR,IGRSH4N,NGRSHEL)
          IF (ITYP == IGRSH4N(IGR)%GRTYPE) IS_FOUND_SURF = .TRUE.
          
        ELSEIF (TYP(1:6) == 'GRTRUS') THEN
          ITYP = 4
          IGR = NINTRIGR(GR,IGRTRUSS,NGRTRUS)
          IF (ITYP == IGRTRUSS(IGR)%GRTYPE) IS_FOUND_SURF = .TRUE.
          
        ELSEIF (TYP(1:6) == 'GRBEAM') THEN
          ITYP = 5
          IGR = NINTRIGR(GR,IGRBEAM,NGRBEAM)
          IF (ITYP == IGRBEAM(IGR)%GRTYPE) IS_FOUND_SURF = .TRUE.
          
        ELSEIF (TYP(1:6) == 'GRSPRI') THEN
          ITYP = 6
          IGR = NINTRIGR(GR,IGRSPRING,NGRSPRI)
          IF (ITYP == IGRSPRING(IGR)%GRTYPE) IS_FOUND_SURF = .TRUE.
          
        ELSEIF (TYP(1:6) == 'GRSH3N' .OR. TYP(1:6) == 'GRTRIA') THEN
          ITYP = 7
          IGR = NINTRIGR(GR,IGRSH3N,NGRSH3N)
          IF (ITYP == IGRSH3N(IGR)%GRTYPE) IS_FOUND_SURF = .TRUE.
        ENDIF
        
        IF(.NOT. IS_FOUND_SURF)THEN
            CALL ANCMSG(MSGID=763,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR,I2=GR,C2=TYP(1:6))
        ENDIF        
        
        WRITE(IOUT,'(/A,I10,2A)')'THPART:',ID,',',TRIM(TITR)
        WRITE(IOUT,'(A)')       '----'
        WRITE(IOUT,'(A,A)')'TYPE OF ELEMENT GROUP : ',TYP(1:6)
        WRITE(IOUT,'(A,I10)')'ELEMENT GROUP ID : ',GR

        IPART(1,NPART+I)=IGR
        IPART(2,NPART+I)=ITYP
        IPART(4,NPART+I)=ID

        IF(IPART(4,NPART+I) == 0) THEN
          CALL ANCMSG(MSGID=493,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,C1=TITR1)
        ENDIF
        
      ENDDO

      !-------------------------------------
      ! Recherche des ID doubles
      !-------------------------------------
      CALL UDOUBLE(IPART(4,1),LIPART1,NPART+NTHPART,MESS,0,BID)

      RETURN
C
      END
