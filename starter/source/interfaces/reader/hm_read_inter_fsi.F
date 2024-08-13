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
      !||====================================================================
      !||    hm_read_inter_fsi      ../starter/source/interfaces/reader/hm_read_inter_fsi.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_interfaces     ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||--- calls      -----------------------------------------------------
      !||    fretitl                ../starter/source/starter/freform.F
      !||    hm_read_inter_type01   ../starter/source/interfaces/int01/hm_read_inter_type01.F
      !||    hm_read_inter_type09   ../starter/source/interfaces/int09/hm_read_inter_type09.F
      !||    hm_read_inter_type12   ../starter/source/interfaces/int12/hm_read_inter_type12.F
      !||    hm_read_inter_type18   ../starter/source/interfaces/int18/hm_read_inter_type18.F
      !||    hm_read_inter_type22   ../starter/source/interfaces/int22/hm_read_inter_type22.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    submodel_mod           ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_INTER_FSI(
     1        IPARI     ,STFAC   ,FRIGAP    ,XFILTR    , FRIC_P   ,
     2        IGRNOD    ,IGRSURF ,ILAGM     ,UNITAB    , NI       ,
     3        NOM_OPT   ,TITR    ,IGRBRIC   ,IGRSH3N   , IGRTRUSS ,
     4        NOINT     ,KEY     ,LSUBMODEL , ITAB     ,
     5        ITABM1    ,ISKN    ,MULTI_FVM ,NPARI     , NPARIR   ,
     6        NGRNOD    ,NGRBRIC ,NGRSH3N   ,NGRTRUS   , NSURF    ,
     7        LNOPT1    ,SITABM1 ,SISKWN    ,LISKN     ,
     8        LTITR     ,SITAB)
C-----------------------------------------------
C   D e s r i p t i o n
C-----------------------------------------------
C  FSI INTERFACES : READER SUBROUTINES
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD , ONLY : UNIT_TYPE_
      USE MESSAGE_MOD
      USE GROUPDEF_MOD , ONLY : GROUP_, SURF_
      USE SUBMODEL_MOD , ONLY : NSUBMOD, SUBMODEL_DATA
      USE MULTI_FVM_MOD , ONLY : MULTI_FVM_STRUCT
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: LISKN, SISKWN !< array size (ISKN)
      INTEGER,INTENT(IN) :: SITAB, SITABM1 !< array size (ITAB and ITABM1)
      INTEGER,INTENT(IN) :: LNOPT1,LTITR !< array size NOM_OPT
      INTEGER,INTENT(IN) :: NPARI, NPARIR  !< array size IPARI and FRIGAP
      INTEGER,INTENT(IN) :: NGRNOD,NGRBRIC,NGRSH3N,NGRTRUS,NSURF  !< array size (elem data structure)
      TYPE(UNIT_TYPE_),INTENT(IN) :: UNITAB
      INTEGER,INTENT(INOUT) :: NOM_OPT(LNOPT1,*)
      INTEGER,INTENT(INOUT) :: ILAGM,NI,NOINT
      INTEGER,INTENT(INOUT) :: IPARI(NPARI)
      INTEGER,INTENT(IN) :: ITAB(SITAB),ITABM1(SITABM1),ISKN(SISKWN)
      my_real STFAC,XFILTR
      my_real,INTENT(INOUT) :: FRIGAP(NPARIR),FRIC_P(10)
      CHARACTER(len=nchartitle),INTENT(IN) :: TITR
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE (GROUP_),TARGET,DIMENSION(NGRNOD) :: IGRNOD
      TYPE (GROUP_),TARGET,DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_),DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_),DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (SURF_),TARGET,DIMENSION(NSURF) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L,NTYP,UID,IFLAGUNIT,FLAG_FMT,FLAG_FMT_TMP,IERR1,SUB_ID,IFIX_TMP
      CHARACTER(len=ncharkey) :: KEY
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NTYP = 0
      ILAGM = 0
      NOM_OPT(1,NI)=NOINT
      WRITE(IOUT,1000) NOINT,TRIM(TITR)
      CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,NI),LTITR)

      SELECT CASE(KEY(1:LEN_TRIM(KEY)))

          CASE ('TYPE22')
              NTYP=22
              CALL HM_READ_INTER_TYPE22(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,
     2          IGRNOD     ,IGRSURF    ,IGRBRIC    ,IGRSH3N   ,IGRTRUSS ,
     3          FRIC_P     ,TITR       ,LSUBMODEL  ,NPARI     ,NPARIR)
              

          CASE ('TYPE18')
             NTYP=18
             CALL HM_READ_INTER_TYPE18(
     1            IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,
     2            IGRNOD     ,IGRSURF    ,IGRBRIC    ,XFILTR    ,FRIC_P   ,
     3            TITR       ,UNITAB     ,LSUBMODEL  ,MULTI_FVM ,NPARI    ,
     4            NPARIR)

         CASE ('TYPE12')
              NTYP=12
              CALL HM_READ_INTER_TYPE12(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,
     2          IGRSURF    ,ITAB       ,ITABM1     ,ISKN      ,
     3          LSUBMODEL  ,UNITAB     ,SITAB      ,SITABM1   ,
     4          NPARI      ,NPARIR     ,SISKWN     ,LISKN)
     
          CASE ('TYPE9')
              NTYP=9
              CALL  HM_READ_INTER_TYPE09(
     1           IPARI     ,STFAC      ,FRIGAP     ,NOINT     ,
     2           IGRSURF   ,UNITAB     ,LSUBMODEL  ,NPARI     ,
     3           NPARIR)

          CASE ('TYPE1')
              NTYP=1
              CALL  HM_READ_INTER_TYPE01(IPARI, STFAC, FRIGAP, IGRSURF, LSUBMODEL, NSURF, NOINT, NPARI, NPARIR)

          CASE ('KINE')
              NTYP=18
              ILAGM = -2

      END SELECT ! SELECT CASE(NTYP)
C--------------------------------------------------------------
 1000 FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
C--------------------------------------------------------------
      RETURN
      END
