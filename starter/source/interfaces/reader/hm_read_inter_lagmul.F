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
!||    hm_read_inter_lagmul          ../starter/source/interfaces/reader/hm_read_inter_lagmul.F
!||--- called by ------------------------------------------------------
!||    hm_read_interfaces            ../starter/source/interfaces/reader/hm_read_interfaces.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                        ../starter/source/output/message/message.F
!||    fretitl                       ../starter/source/starter/freform.F
!||    hm_read_inter_lagmul_type02   ../starter/source/interfaces/int02/hm_read_inter_lagmul_type02.F
!||    hm_read_inter_lagmul_type07   ../starter/source/interfaces/int07/hm_read_inter_lagmul_type07.F
!||    hm_read_inter_lagmul_type16   ../starter/source/interfaces/int16/hm_read_inter_lagmul_type16.F
!||    hm_read_inter_lagmul_type17   ../starter/source/interfaces/int17/hm_read_inter_lagmul_type17.F
!||--- uses       -----------------------------------------------------
!||    message_mod                   ../starter/share/message_module/message_mod.F
!||    submodel_mod                  ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_INTER_LAGMUL(
     1        IPARI    ,STFAC      ,FRIGAP    ,IGRNOD      ,IGRSURF    ,
     2        ILAGM    ,NI         ,NOM_OPT   ,TITR        ,DEF_INTER  ,
     3        IGRBRIC  ,UNITAB     ,LSUBMODEL ,NOINT       ,KEY        ,
     4        NPARI    ,NPARIR)
C============================================================================
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD , ONLY : SUBMODEL_DATA, NSUBMOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NPARI, NPARIR !< array sizes IPARI and FRIGAP
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER ISU1,ISU2,ILAGM,NI,NOINT
      INTEGER IPARI(NPARI),DEF_INTER(100)
      my_real STFAC
      my_real FRIGAP(NPARIR)
      CHARACTER(LEN=NCHARTITLE)::TITR
      CHARACTER(LEN=NCHARKEY)::KEY
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER UID,NTYP,USR_TYP
C=======================================================================
C     READING LAGRANGE MULTIPLIER INTERFACES (/INTER/LAGMUL/TYPE2, 
C         /INTER/LAGMUL/TYPE7, /INTER/LAGMUL/TYPE16,/INTER/LAGMUL/TYPE17)
C========================================================================
C
      ILAGM = 1
      IPARI(33) = ILAGM
C
      NOM_OPT(1,NI)=NOINT
C
      WRITE(IOUT,1000) NOINT,TRIM(TITR)
C---------------------------------------------------------------
      CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,NI),LTITR)
C---------------------------------------------------------------
C LAGMUL INTERFACES READING ROUTINES
C-----------------------------
      NTYP = 0
      SELECT CASE(KEY(1:LEN_TRIM(KEY)))
c------------------------------
         CASE ('TYPE2')
c------------------------------
           NTYP = 2
           CALL HM_READ_INTER_LAGMUL_TYPE02(
     1        IPARI      ,FRIGAP     ,NOINT      ,
     2        IGRNOD     ,IGRSURF    ,DEF_INTER  ,TITR      ,UNITAB     ,
     3        LSUBMODEL  ,NPARI      ,NPARIR)
c------------------------------
         CASE ('TYPE7')
c------------------------------
           NTYP = 7
           CALL HM_READ_INTER_LAGMUL_TYPE07(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,
     2        IGRNOD     ,IGRSURF    , TITR       ,UNITAB     ,LSUBMODEL  )
c------------------------------
         CASE ('TYPE16')
c------------------------------
           NTYP = 16 
           CALL HM_READ_INTER_LAGMUL_TYPE16(
     .          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,
     .          IGRNOD     ,IGRBRIC    ,LSUBMODEL )
c------------------------------
         CASE ('TYPE17')
c------------------------------
           NTYP = 17
           CALL HM_READ_INTER_LAGMUL_TYPE17(
     .          IPARI      ,STFAC      ,FRIGAP    ,NOINT     ,
     .          IGRBRIC    ,LSUBMODEL )
C---------------------------------------------------------------
      END SELECT
C---------------------------------------------------------------
      USR_TYP = NTYP
      IF (USR_TYP/=2.AND.USR_TYP/=7.AND.USR_TYP/=16.AND.USR_TYP/=17) THEN
         CALL ANCMSG(MSGID=486,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .               I1=NOINT,
     .               C1=TITR,
     .               I2=NTYP)
      ENDIF
C
      IF (STFAC == ZERO ) STFAC = ONE_FIFTH
C  
C--------------------------------------------------------------
 1000 FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
C------------
      RETURN
      END
