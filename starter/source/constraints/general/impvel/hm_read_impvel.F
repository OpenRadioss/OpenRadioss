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
Chd|  HM_READ_IMPVEL                source/constraints/general/impvel/hm_read_impvel.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        READ_IMPDISP                  source/constraints/general/impvel/read_impdisp.F
Chd|        READ_IMPDISP_FGEO             source/constraints/general/impvel/read_impdisp_fgeo.F
Chd|        READ_IMPVEL                   source/constraints/general/impvel/read_impvel.F
Chd|        READ_IMPVEL_FGEO              source/constraints/general/impvel/read_impvel_fgeo.F
Chd|        READ_IMPVEL_LAGMUL            source/constraints/general/impvel/read_impvel_lagmul.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_IMPVEL(
     .           FBFVEL   ,IBFVEL   ,IKINE    ,IKINE1LAG,
     .           ITAB     ,ITABM1   ,IGRNOD   ,X0       ,IXR      ,
     .           IPART    ,IPARTR   ,ISKN     ,NOM_OPT  ,
     .           NIMPDISP ,NIMPVEL  ,UNITAB   ,LSUBMODEL)
C============================================================================
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
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NIMPDISP,NIMPVEL
      INTEGER ,DIMENSION(*)  :: ITAB,ITABM1,IKINE,IKINE1LAG,IPARTR
      INTEGER ,DIMENSION(LISKN,*)           ,INTENT(IN)  :: ISKN
      INTEGER ,DIMENSION(LIPART1,*)         ,INTENT(IN)  :: IPART
      INTEGER ,DIMENSION(NIXR,*)            ,INTENT(IN)  :: IXR
      INTEGER ,DIMENSION(LNOPT1,*)          ,INTENT(OUT) :: NOM_OPT
      INTEGER ,DIMENSION(NIFV,NFXVEL)       ,INTENT(OUT) :: IBFVEL
      my_real ,DIMENSION(LFXVELR,NFXVEL)    ,INTENT(OUT) :: FBFVEL
      my_real ,DIMENSION(LUNIT,*)           ,INTENT(IN)  :: UNITAB
      my_real ,DIMENSION(3,NUMNOD)          ,INTENT(IN)  :: X0
      TYPE (GROUP_)      ,DIMENSION(NGRNOD) ,INTENT(IN)  :: IGRNOD
      TYPE(SUBMODEL_DATA),DIMENSION(*)      ,INTENT(IN)  :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IOPT,INUM,NOPT,NFDISP,NFVEL,FGEOD,FGEOV,NIMPDISP_0,LAGMULV
      INTEGER ,DIMENSION(:), ALLOCATABLE  :: OPTID
      CHARACTER(nchartitle)  :: MESS
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA MESS/'IMPOSED VELOCITY DEFINITION  '/
C======================================================================|
      INUM = 0  ! init index of IBFVEL,FBFVEL (imposed node counter)
      IOPT = 0  ! init counter of impdisp, impvel and impacc options
c--------------------------------------------------
c     READ /IMPDISP
c--------------------------------------------------
c           
      CALL HM_OPTION_COUNT('/IMPDISP'     ,NIMPDISP )
      NIMPDISP_0 = NIMPDISP
      CALL HM_OPTION_COUNT('/IMPDISP/FGEO',FGEOD)
c
      CALL HM_OPTION_COUNT('/IMPVEL'       ,NIMPVEL )
      CALL HM_OPTION_COUNT('/IMPVEL/FGEO'  ,FGEOV)
      CALL HM_OPTION_COUNT('/IMPVEL/LAGMUL',LAGMULV)
c
      NFDISP = NIMPDISP - FGEOD
      NFVEL  = NIMPVEL  - FGEOV - LAGMULV
      NOPT   = NFDISP + NFVEL
c    
c--------------------------------------------------
      IF (NIMPDISP > 0) THEN
      
        IF (NFDISP > 0) THEN 
          CALL READ_IMPDISP(
     .         NIMPDISP ,INUM     ,IOPT     ,FBFVEL   ,IBFVEL   ,
     .         ITAB     ,ITABM1   ,IKINE    ,IGRNOD   ,NOM_OPT  ,
     .         ISKN     ,UNITAB   ,LSUBMODEL)
        ENDIF
c
        IF (FGEOD > 0) THEN 
          CALL READ_IMPDISP_FGEO(
     .         FGEOD    ,INUM     ,IOPT     ,FBFVEL   ,IBFVEL   ,
     .         ITAB     ,ITABM1   ,IGRNOD   ,NOM_OPT  ,X0       ,
     .         IXR      ,IPART    ,IPARTR   ,UNITAB   ,LSUBMODEL)
        ENDIF
c
c       TEST DOUBLE IDs of IMPDISP
c
        ALLOCATE( OPTID(NIMPDISP) )
        OPTID(1:NIMPDISP) = NOM_OPT(1,1:NIMPDISP)
        CALL UDOUBLE(OPTID,1,NIMPDISP,MESS,0,ZERO)
        DEALLOCATE( OPTID )
c
      END IF  ! NIMPDISP > 0
      NIMPDISP = INUM
c
c--------------------------------------------------
c     READ /IMPVEL
c--------------------------------------------------
      IF (NIMPVEL > 0) THEN
      
c
        IF (NFVEL > 0) THEN 
          CALL READ_IMPVEL(
     .              NIMPVEL  ,INUM     ,IOPT     ,FBFVEL   ,IBFVEL   ,
     .              ITAB     ,ITABM1   ,IKINE    ,IKINE1LAG,NOM_OPT  ,
     .              IGRNOD   ,ISKN     ,UNITAB   ,LSUBMODEL)
        END IF
c
c       READ /IMPVEL/FGEO
c
        IF (FGEOV > 0) THEN 
          CALL READ_IMPVEL_FGEO(
     .              FGEOV    ,INUM     ,IOPT     ,FBFVEL   ,IBFVEL   ,
     .              ITAB     ,ITABM1   ,IGRNOD   ,NOM_OPT  ,X0       ,
     .              IXR      ,IPART    ,IPARTR   ,UNITAB   ,LSUBMODEL)
        END IF
c
c       READ /IMPVEL/LAGMUL
c
        IF (LAGMULV > 0) THEN 
          CALL READ_IMPVEL_LAGMUL(
     .              LAGMULV  ,INUM     ,IOPT     ,FBFVEL   ,IBFVEL   ,
     .              ITAB     ,ITABM1   ,IGRNOD   ,NOM_OPT  ,X0       ,
     .              IXR      ,IPART    ,IPARTR   ,ISKN     ,IKINE    ,
     .              UNITAB   ,LSUBMODEL)
        END IF
c
c       TEST DOUBLE IDs of IMPVEL
c
        ALLOCATE( OPTID(NIMPVEL) )
        OPTID(1:NIMPVEL) = NOM_OPT(1,NIMPDISP_0+1:NIMPVEL+NIMPDISP_0)
        CALL UDOUBLE(OPTID,1,NIMPVEL,MESS,0,ZERO)
        DEALLOCATE( OPTID )
c
      END IF  ! NIMPVEL > 0
c-----------      
      NIMPVEL = INUM - NIMPDISP
      NFXVEL  = INUM
c--------------------------------------------------
      RETURN
      END
