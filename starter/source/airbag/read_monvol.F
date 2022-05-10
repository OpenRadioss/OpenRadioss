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
Chd|  READ_MONVOL                   source/airbag/read_monvol.F   
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_READ_MONVOL_TYPE1          source/airbag/hm_read_monvol_type1.F
Chd|        HM_READ_MONVOL_TYPE10         source/airbag/hm_read_monvol_type10.F
Chd|        HM_READ_MONVOL_TYPE11         source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE2          source/airbag/hm_read_monvol_type2.F
Chd|        HM_READ_MONVOL_TYPE3          source/airbag/hm_read_monvol_type3.F
Chd|        HM_READ_MONVOL_TYPE4          source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE5          source/airbag/hm_read_monvol_type5.F
Chd|        HM_READ_MONVOL_TYPE6          source/airbag/hm_read_monvol_type6.F
Chd|        HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          source/airbag/hm_read_monvol_type9.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_MONVOL(T_MONVOL, T_MONVOL_METADATA, ITAB, ITABM1, IPM, IGEO, 
     .     X, PM, GEO, IXC, IXTG, LSENSOR,SENSOR,
     .     UNITAB, NPC, NPT, PLD, IGRSURF, IGRBRIC, NOM_OPT,IFRAME, XFRAME, 
     .     LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE MONVOL_STRUCT_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C     NVOLU
#include      "com04_c.inc"
C     KMONVO, IREC
#include      "scr17_c.inc"
C     NSUBDOM
#include      "r2r_c.inc"
C     NIMV, NRVOLU
#include      "param_c.inc"
C     LUNIT, NUNITS
#include      "sysunit.inc"
C     IOUT
#include      "units_c.inc"
C     NSUBMOD
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU + NMONVOL), INTENT(INOUT) :: T_MONVOL
      TYPE(MONVOL_METADATA_), INTENT(INOUT) :: T_MONVOL_METADATA
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
      my_real, INTENT(IN) :: X(3, *), GEO(NPROPG, *), PM(NPROPM, *), PLD(2, *), 
     .     SENSOR(NSENSR, *),XFRAME(NXFRAME,*)
      INTEGER, INTENT(IN) :: NPC(*), NPT(*), ITAB(*), ITABM1(*), 
     .     IXC(NIXC, *), IXTG(NIXTG, *), LSENSOR(NSENSI, *), IPM(NPROPMI, *), 
     .     IGEO(NPROPGI, *),IFRAME(LISKN,*)
      INTEGER, INTENT(INOUT) :: NOM_OPT(LNOPT1, *)
      TYPE (SURF_), INTENT(INOUT), DIMENSION(NSURF) :: IGRSURF
      TYPE (GROUP_), DIMENSION(NGRBRIC), INTENT(IN) :: IGRBRIC
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, JJ
      INTEGER :: NVB, ITYPE, ID, UID, LOCAL_UID
      CHARACTER :: KEY*ncharkey, TITR*nchartitle
      LOGICAL :: FOUND
C-----------------------------------------------
C     B e g i n n i n g   o f   s o u r c e
C-----------------------------------------------
C     Beginning of MONVOL cards in the IIN file
      WRITE(IOUT, 1000)
      T_MONVOL_METADATA%LCA = 0
      NVB = 0
C
!     **************************    !
!     MONVOL read with hm reader    !
!     **************************    !
      CALL HM_OPTION_START('/MONVOL')
      DO II = 1, NMONVOL
         NVB = NVB + 1
C     Multidomain -> skip untagged monvols
         IF (NSUBDOM > 0) THEN
            IF( TAGMON(NVB) == 0) THEN
            CALL HM_SZ_R2R(TAGMON, NVB, LSUBMODEL)
         ENDIF
         ENDIF
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, UNIT_ID = UID, OPTION_TITR = TITR,
     .        KEYWORD2 = KEY)
         NOM_OPT(1, II) = ID
         CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,II),LTITR)
C     Check units
         FOUND = .FALSE.
         DO JJ = 1, NUNITS
            IF (UNITAB%UNIT_ID(JJ) == UID) THEN
               FOUND = .TRUE.
               LOCAL_UID = JJ
               EXIT
            ENDIF
         ENDDO
         IF (.NOT. FOUND) THEN
            CALL ANCMSG(MSGID = 659, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .           I2 = UID, I1 = ID, C1 = 'MONITORED VOLUME', C2 = 'MONITORED VOLUME', C3=TITR)
         ENDIF
         IF (KEY(1:4) == 'AREA') THEN
            ITYPE = 1
         ELSEIF (KEY(1:4) == 'PRES') THEN
            ITYPE = 2
         ELSEIF (KEY(1:3) == 'GAS') THEN
            ITYPE = 3
         ELSEIF (KEY(1:7) == 'AIRBAG1') THEN
            ITYPE = 7
         ELSEIF (KEY(1:6) == 'AIRBAG') THEN
            ITYPE = 4
         ELSEIF (KEY(1:6) == 'COMMU1') THEN
            ITYPE = 9
         ELSEIF (KEY(1:5) == 'COMMU') THEN
            ITYPE = 5
         ELSEIF (KEY(1:7) == 'FVMBAG2') THEN
            ITYPE = 11
         ELSEIF (KEY(1:7) == 'FVMBAG1') THEN
            ITYPE = 8
         ELSEIF (KEY(1:6) == 'FVMBAG') THEN
            ITYPE = 6
         ELSEIF (KEY(1:6) == 'LFLUID') THEN
            ITYPE = 10
         ELSE
            CALL ANCMSG(MSGID=7,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .           I1=ID,C1=TITR)
         ENDIF

         T_MONVOL(II)%ID = ID
         T_MONVOL(II)%IVOLU(1) = ID
         T_MONVOL(II)%TYPE = ITYPE
         T_MONVOL(II)%IVOLU(2) = ITYPE
         T_MONVOL(II)%TITLE = TRIM(TITR)
         
         WRITE(IOUT,1001) ID, T_MONVOL(II)%TITLE, KEY(1:LEN_TRIM(KEY))

         SELECT CASE(ITYPE)
         CASE(1)
!     ************     !
!     /MONVOL/AREA     !
!     ************     !
            CALL HM_READ_MONVOL_TYPE1(T_MONVOL(II), IPM, IGEO, ITABM1, 
     .           LSENSOR, SENSOR, NPT, PLD, 
     .           UNITAB, LOCAL_UID, NPC, IGRSURF, ITAB, X, PM, GEO, IXC, IXTG, 
     .           LSUBMODEL)
         CASE(2)
!     ************     !
!     /MONVOL/PRES     !
!     ************     !
            CALL HM_READ_MONVOL_TYPE2(T_MONVOL(II), IPM, IGEO, ITABM1, 
     .           LSENSOR, SENSOR, NPT, PLD, 
     .           UNITAB, LOCAL_UID, NPC, IGRSURF, ITAB, X, PM, GEO, IXC, IXTG, 
     .           LSUBMODEL)
         CASE(3)
!     ***********     !
!     /MONVOL/GAS     !
!     ***********     !
            CALL HM_READ_MONVOL_TYPE3(T_MONVOL(II), IPM, IGEO, ITABM1,   
     .           LSENSOR, SENSOR, NPT, PLD, 
     .           UNITAB, LOCAL_UID, NPC, IGRSURF, ITAB, X, PM, GEO, IXC, IXTG, 
     .           LSUBMODEL)
         CASE(4)
!     **************     !
!     /MONVOL/AIRBAG     !
!     **************     !
            CALL HM_READ_MONVOL_TYPE4(T_MONVOL(II), IPM, IGEO, ITABM1, 
     .           LSENSOR, SENSOR, NPT, PLD, 
     .           UNITAB, LOCAL_UID, NPC, IGRSURF, ITAB, X, PM, GEO, IXC, IXTG,
     .           LSUBMODEL)
         CASE(5)
!     *************     !
!     /MONVOL/COMMU     !
!     *************     !
            CALL HM_READ_MONVOL_TYPE5(T_MONVOL(II), T_MONVOL_METADATA, IPM, IGEO, ITABM1, 
     .           LSENSOR, SENSOR, NPT, PLD, 
     .           UNITAB, LOCAL_UID, NPC, IGRSURF, ITAB, X, PM, GEO, IXC, IXTG,
     .           LSUBMODEL)
         CASE(6)
!     **************     !
!     /MONVOL/FVMBAG     !
!     **************     !
            CALL HM_READ_MONVOL_TYPE6(T_MONVOL(II), IPM, IGEO, ITABM1, 
     .           LSENSOR, SENSOR, NPT, PLD, IGRBRIC,
     .           UNITAB, LOCAL_UID, NPC, IGRSURF, ITAB, X, PM, GEO, IXC, IXTG,
     .           LSUBMODEL)
         CASE(7)
!     ***************     !
!     /MONVOL/AIRBAG1     !
!     ***************     !
            CALL HM_READ_MONVOL_TYPE7(T_MONVOL(II), IPM, IGEO, ITABM1,   
     .           LSENSOR, SENSOR, NPT, PLD, 
     .           UNITAB, LOCAL_UID, NPC, IGRSURF, ITAB, X, PM, GEO, IXC, IXTG,
     .           LSUBMODEL)
         CASE(8)
!     ***************     !
!     /MONVOL/FVMBAG1     !
!     ***************     !
           CALL HM_READ_MONVOL_TYPE8(T_MONVOL(II), IPM, IGEO, ITABM1,   
     .           LSENSOR, SENSOR, NPT, PLD, IFRAME, XFRAME, IGRBRIC,
     .           UNITAB, LOCAL_UID, NPC, IGRSURF, ITAB, X, PM, GEO, IXC, IXTG, 
     .           LSUBMODEL)           

         CASE(9)
!     **************     !
!     /MONVOL/COMMU1     !
!     **************     !
            CALL HM_READ_MONVOL_TYPE9(T_MONVOL(II), T_MONVOL_METADATA, IPM, IGEO, ITABM1,   
     .           LSENSOR, SENSOR, NPT, PLD, 
     .           UNITAB, LOCAL_UID, NPC, IGRSURF, ITAB, X, PM, GEO, IXC, IXTG,
     .           LSUBMODEL)
         CASE(10)
!     **************     !
!     /MONVOL/LFLUID     !
!     **************     !
            CALL HM_READ_MONVOL_TYPE10(T_MONVOL(II), IPM, IGEO, ITABM1,   
     .           LSENSOR, SENSOR, NPT, PLD, 
     .           UNITAB, LOCAL_UID, NPC, IGRSURF, ITAB, X, PM, GEO, IXC, IXTG, 
     .           LSUBMODEL)
         CASE(11)
!     ***************     !
!     /MONVOL/FVMBAG2     !
!     ***************     !
            CALL HM_READ_MONVOL_TYPE11(T_MONVOL(II), IPM, IGEO, ITABM1,   
     .           LSENSOR, SENSOR, NPT, PLD, IFRAME, XFRAME, IGRBRIC,
     .           UNITAB, LOCAL_UID, NPC, IGRSURF, ITAB, X, PM, GEO, IXC, IXTG, 
     .           LSUBMODEL)
         END SELECT
      ENDDO

      NVOLU = NVOLU + NMONVOL

      RETURN
 1000 FORMAT(
     . //,'     MONITORED VOLUME DEFINITION      '/
     .     '     ----------------------------     ')
 1001 FORMAT(//5X,'VOLUME NUMBER ',I10,
     .     / 5X,'------------------------',
     .     / 5X,'TITLE: ',A,
     .     / 5X,'TYPE OF MONITORED VOLUME. . . . . . . .=',A10)
      END SUBROUTINE READ_MONVOL
