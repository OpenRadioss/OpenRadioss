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
Chd|  LECGGROUP                     source/groups/lecggroup.F     
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_GROGRO                     source/groups/hm_grogro.F     
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LECGGROUP(
     .           FLAGG ,
     .           ICOUNT   ,ITER  ,IGRBRIC  ,IGRQUAD  ,IGRSH4N,
     .           IGRSH3N,IGRTRUSS,IGRBEAM  ,IGRSPRING,IGRPART,
     .           LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FLAGG,ICOUNT,ITER
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE (GROUP_)  , DIMENSION(NGRPART) :: IGRPART
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IBRIC,IQUAD,ISHEL,ISH3N,ITRUS,IBEAM,ISPRI,IPRT,IFLAG
      SAVE  IBRIC,IQUAD,ISHEL,ISH3N,ITRUS,IBEAM,ISPRI,IPRT,IFLAG
C-----------------------------------------------
C     LECTURE DES GROUPES DE GROUPES
C=======================================================================
      IFLAG = MOD(FLAGG,2)
C-----------------------------------------------
C     BRICK
C-----------------------------------------------
      IF (FLAGG == 0) IBRIC = 0
      IF (NGRBRIC > 0) THEN
        CALL HM_OPTION_START('/GRBRIC')
        CALL HM_GROGRO(NUMELS ,NGRBRIC,IGRBRIC,IBRIC, IFLAG  ,ITER   ,'BRIC' ,LSUBMODEL)
      ENDIF
C-----------------------------------------------
C     QUAD
C-----------------------------------------------
      IF (FLAGG == 0) IQUAD = 0
      IF (NGRQUAD > 0) THEN
        CALL HM_OPTION_START('/GRQUAD')
        CALL HM_GROGRO(NUMELQ ,NGRQUAD,IGRQUAD,IQUAD, IFLAG  ,ITER   ,'QUAD' ,LSUBMODEL)
      ENDIF
C-----------------------------------------------
C     SHELL
C-----------------------------------------------
      IF (FLAGG == 0) ISHEL = 0
      IF (NGRSHEL > 0) THEN
        CALL HM_OPTION_START('/GRSHEL')
        CALL HM_GROGRO(NUMELC ,NGRSHEL,IGRSH4N,ISHEL,IFLAG  ,ITER   ,'SHEL' ,LSUBMODEL)
      ENDIF
C-----------------------------------------------
C     TRUS
C-----------------------------------------------
      IF (FLAGG == 0) ITRUS = 0
      IF (NGRTRUS > 0) THEN
        CALL HM_OPTION_START('/GRTRUS')
        CALL HM_GROGRO(NUMELT ,NGRTRUS,IGRTRUSS,ITRUS, IFLAG  ,ITER   ,'TRUS',LSUBMODEL  )
      ENDIF
C-----------------------------------------------
C     BEAM
C-----------------------------------------------
      IF (FLAGG == 0) IBEAM = 0
      IF (NGRBEAM > 0) THEN
        CALL HM_OPTION_START('/GRBEAM')
        CALL HM_GROGRO(NUMELP ,NGRBEAM,IGRBEAM,IBEAM,IFLAG  ,ITER   ,'BEAM' ,LSUBMODEL)
      ENDIF
C-----------------------------------------------
C     SPRI GROUP
C-----------------------------------------------
      IF (FLAGG == 0) ISPRI = 0
      IF (NGRSPRI > 0) THEN
        CALL HM_OPTION_START('/GRSPRI')
        CALL HM_GROGRO(NUMELR ,NGRSPRI,IGRSPRING,ISPRI,IFLAG  ,ITER   ,'SPRI'   ,LSUBMODEL)
      ENDIF
C-----------------------------------------------
C     SH_3N GROUP
C-----------------------------------------------
      IF (FLAGG == 0) ISH3N = 0
      IF (NGRSH3N > 0 .AND. N2D==0)THEN
        CALL HM_OPTION_START('/GRSH3N')
        CALL HM_GROGRO(NUMELTG ,NGRSH3N,IGRSH3N,ISH3N, IFLAG   ,ITER   ,'SH3N' ,LSUBMODEL)
      ENDIF
C-----------------------------------------------
C     TRIA GROUP
C-----------------------------------------------
      IF (FLAGG == 0) ISH3N = 0
      IF (NGRSH3N > 0 .AND. N2D/=0)THEN
        CALL HM_OPTION_START('/GRTRIA')
        CALL HM_GROGRO(NUMELTG,NGRSH3N,IGRSH3N,ISH3N,IFLAG,ITER,'TRIA' ,LSUBMODEL)
      ENDIF
C-----------------------------------------------
C     PART GROUP
C-----------------------------------------------
      IF (FLAGG == 0) IPRT = 0
      IF (NGRPART > 0)THEN
        CALL HM_OPTION_START('/GRPART')
        CALL HM_GROGRO(NPART ,NGRPART,IGRPART,IPRT,IFLAG ,ITER   ,'PART' ,LSUBMODEL)
      ENDIF
C-----------
      ICOUNT  = IBRIC+IQUAD+ISHEL+ISH3N+ITRUS+IBEAM+ISPRI+IPRT
C-----------
      RETURN
      END
