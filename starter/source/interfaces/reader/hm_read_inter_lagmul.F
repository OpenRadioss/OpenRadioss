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
Chd|  HM_READ_INTER_LAGMUL          source/interfaces/reader/hm_read_inter_lagmul.F
Chd|-- called by -----------
Chd|        HM_READ_INTERFACES            source/interfaces/reader/hm_read_interfaces.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_READ_INTER_LAGMUL_TYPE02   source/interfaces/int02/hm_read_inter_lagmul_type02.F
Chd|        HM_READ_INTER_LAGMUL_TYPE07   source/interfaces/int07/hm_read_inter_lagmul_type07.F
Chd|        HM_READ_INTER_LAGMUL_TYPE16   source/interfaces/int16/hm_read_inter_lagmul_type16.F
Chd|        HM_READ_INTER_LAGMUL_TYPE17   source/interfaces/int17/hm_read_inter_lagmul_type17.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_LAGMUL(
     1        IPARI    ,STFAC      ,FRIGAP    ,IGRNOD      ,IGRSURF    ,
     2        ILAGM    ,NI         ,NOM_OPT   ,TITR        ,DEF_INTER  ,
     3        IGRBRIC  ,UNITAB     ,LSUBMODEL,NOINT        ,KEY        )
C============================================================================
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
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
#include      "scr17_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER ISU1,ISU2,ILAGM,NI,NOINT
      INTEGER IPARI(*),DEF_INTER(*)
      my_real STFAC
      my_real FRIGAP(*)
      CHARACTER TITR*nchartitle
      CHARACTER KEY*ncharkey
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr12_c.inc"
#include      "sysunit.inc"
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
      SELECT CASE(KEY(1:LEN_TRIM(KEY)))
c------------------------------
         CASE ('TYPE2')
c------------------------------
           NTYP = 2
           CALL HM_READ_INTER_LAGMUL_TYPE02(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI         ,    
     2        IGRNOD     ,IGRSURF    ,DEF_INTER  ,TITR      ,UNITAB     ,
     3        LSUBMODEL  )
c------------------------------
         CASE ('TYPE7')
c------------------------------
           NTYP = 7
           CALL HM_READ_INTER_LAGMUL_TYPE07(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI         ,    
     2        IGRNOD     ,IGRSURF    , TITR       ,UNITAB     ,LSUBMODEL  )
c------------------------------
         CASE ('TYPE16')
c------------------------------
           NTYP = 16 
           CALL HM_READ_INTER_LAGMUL_TYPE16(
     .          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     .          IGRNOD     ,IGRBRIC    ,UNITAB     ,LSUBMODEL )
c------------------------------
         CASE ('TYPE17')
c------------------------------
           NTYP = 17
           CALL HM_READ_INTER_LAGMUL_TYPE17(
     .          IPARI      ,STFAC      ,FRIGAP    ,NOINT     ,NI       ,
     .          IGRBRIC    ,UNITAB     ,LSUBMODEL )
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
