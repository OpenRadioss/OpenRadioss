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
Chd|  HM_READ_INTER_FSI             source/interfaces/reader/hm_read_inter_fsi.F
Chd|-- called by -----------
Chd|        HM_READ_INTERFACES            source/interfaces/reader/hm_read_interfaces.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_READ_INTER_TYPE01          source/interfaces/int01/hm_read_inter_type01.F
Chd|        HM_READ_INTER_TYPE09          source/interfaces/int09/hm_read_inter_type09.F
Chd|        HM_READ_INTER_TYPE12          source/interfaces/int12/hm_read_inter_type12.F
Chd|        HM_READ_INTER_TYPE18          source/interfaces/int18/hm_read_inter_type18.F
Chd|        HM_READ_INTER_TYPE22          source/interfaces/int22/hm_read_inter_type22.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_FSI(
     1        IPARI       ,STFAC      ,FRIGAP         ,XFILTR   , FRIC_P   ,
     2        IGRNOD      ,IGRSURF    ,ILAGM          ,UNITAB   , NI       ,
     3        NOM_OPT     ,TITR       ,IGRBRIC        ,IGRSH3N  , IGRTRUSS ,      
     4        IDDLEVEL    ,NOINT      ,KEY            ,LSUBMODEL, ITAB     ,
     5        ITABM1      ,ISKN       ,MULTI_FVM)
C============================================================================

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE MULTI_FVM_MOD
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
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "sphcom.inc"
#include      "submod_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::IDDLEVEL
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER ILAGM,NI,NOINT
      INTEGER IPARI(*)
      INTEGER,INTENT(IN) :: ITAB(NUMNOD),ITABM1(*),ISKN(*)
      my_real
     .   STFAC,XFILTR
      my_real
     .   FRIGAP(*),FRIC_P(*)
      CHARACTER TITR*nchartitle
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM      
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com09_c.inc"
#include      "intstamp_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "fluid_scr_c.inc"
#include      "scr12_c.inc"
#include      "remesh_c.inc"
#include      "sysunit.inc"
#include      "thknod_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   I,J,L,NTYP,UID,IFLAGUNIT,FLAG_FMT,FLAG_FMT_TMP,IERR1,
     .   SUB_ID,IFIX_TMP
      my_real
     .   FAC_L,FAC_T,FAC_M,FAC_C
      CHARACTER KEY*ncharkey
!

C=======================================================================
C     FSI INTERFACES READING ROUTINES
C=======================================================================
      NTYP = 0
C
      ILAGM = 0

C
      NOM_OPT(1,NI)=NOINT
C
      WRITE(IOUT,1000) NOINT,TRIM(TITR)
      CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,NI),LTITR)
C---------------------------------------------------------------
C  INTERFACES TYPE : READING ROUTINES
C---------------------------------------------------------------

      SELECT CASE(KEY(1:LEN_TRIM(KEY)))

          CASE ('TYPE22')
C--------------------------------------------------
C            HM READING INTER TYPE22 
C--------------------------------------------------
              NTYP=22
              CALL HM_READ_INTER_TYPE22(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2          IGRNOD     ,IGRSURF    ,IGRBRIC    ,IGRSH3N   ,IGRTRUSS ,
     3          FRIC_P     ,TITR       ,LSUBMODEL)
              

          CASE ('TYPE18')
C--------------------------------------------------
C            HM READING INTER TYPE18 
C--------------------------------------------------
             NTYP=18
             CALL HM_READ_INTER_TYPE18(
     1            IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2            IGRNOD     ,IGRSURF    ,IGRBRIC    ,XFILTR    ,FRIC_P   ,
     3            IDDLEVEL   ,TITR, UNITAB, LSUBMODEL,MULTI_FVM)  

         CASE ('TYPE12')
C--------------------------------------------------
C            HM READING INTER TYPE12 
C--------------------------------------------------
              NTYP=12
              CALL HM_READ_INTER_TYPE12(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI      ,
     2          IGRSURF    ,ITAB       ,ITABM1     ,ISKN      ,TITR    ,
     3          LSUBMODEL  ,UNITAB)
     
          CASE ('TYPE9')
C--------------------------------------------------
C            HM READING INTER TYPE9 
C--------------------------------------------------
              NTYP=9
              CALL  HM_READ_INTER_TYPE09(
     1           IPARI     ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2           IGRSURF   ,TITR       ,UNITAB     ,LSUBMODEL )

          CASE ('TYPE1')
C--------------------------------------------------
C            HM READING INTER TYPE1 
C--------------------------------------------------
              NTYP=1
              CALL  HM_READ_INTER_TYPE01(
     1           IPARI     ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2           IGRSURF   ,TITR       ,LSUBMODEL)

          CASE ('KINE')
C--------------------------------------------------
C            HM READING INTER KINE 
C--------------------------------------------------
              NTYP=18
              ILAGM = -2

      END SELECT ! SELECT CASE(NTYP)
C=======================================================================
 1000 FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
C--------------------------------------------------------------
      RETURN
 999  CALL FREERR(3)
      RETURN


      END
