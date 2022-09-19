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
Chd|  LECGROUP                      source/groups/lecgroup.F      
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_LECGRE                     source/groups/hm_lecgre.F     
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LECGROUP(
     1      ITAB     ,ITABM1             ,ISUBMOD  ,
     2      X        ,IXS      ,IXQ      ,IXC      ,IXT      ,IXP      ,
     3      IXR      ,IXTG     ,                              IPART    ,
     4      IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP   ,IPARTR   ,
     5      IPARTG   ,FLAGG    ,SH4TREE  ,SH3TREE  ,
     6      SKEW     ,ISKN     ,UNITAB   ,IBOX     ,
     7      IXS10    ,IXS16    ,IXS20    ,RTRANS   ,LSUBMODEL,
     8      IXS_S    ,IXS_S_IND, IXQ_S  ,IXQ_S_IND ,IXC_S    ,IXC_S_IND,
     9      IXT_S    ,IXT_S_IND, IXP_S  ,IXP_S_IND ,IXR_S    ,IXR_S_IND,
     A      IXTG_S  ,IXTG_S_IND,IADBOXMAX,SUBSET,IGRBRIC,IGRQUAD,
     B      IGRSH4N,IGRSH3N,IGRTRUSS,IGRBEAM,IGRSPRING)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE GROUPDEF_MOD
      USE OPTIONDEF_MOD
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
#include      "scr03_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "sphcom.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER ITAB(*),ITABM1(*),
     .        ISUBMOD(*),IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IXTG(NIXTG,*),
     .        IXS10(6,*),IXS16(8,*),IXS20(12,*),IPART(LIPART1,*),
     .        IPARTQ(*),IPARTC(*),IPARTT(*),IPARTP(*),IPARTR(*),
     .        IPARTG(*), SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*),
     .        ISKN(LISKN,*),IPARTS(*),
     .        IXS_S(*), IXS_S_IND(*), IXQ_S(*), IXQ_S_IND(*),
     .        IXC_S(*), IXC_S_IND(*), IXT_S(*), IXT_S_IND(*),
     .        IXP_S(*), IXP_S_IND(*), IXR_S(*), IXR_S_IND(*),
     .        IXTG_S(*), IXTG_S_IND(*)
      INTEGER FLAGG,IADBOXMAX
      MY_REAL
     .        X(3,*),SKEW(LSKEW,*),RTRANS(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
!
C-----------------------------------------------
      TYPE (SUBSET_) , DIMENSION(NSUBS)   :: SUBSET
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE (BOX_)    , DIMENSION(NBBOX)   :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KK,K
      INTEGER ADMBID
      DATA ADMBID/0/
C-----------------------------------------------
C    IGRELEM(IGS)%ID : GROUP identifier
C    IGRELEM(IGS)%TITLE : GROUP title
C    IGRELEM(IGS)%NENTITY : Entities (elements) number of the GROUP
C    IGRELEM(IGS)%GRTYPE : TYPE ( 0-NOEUDS, 1-BRIC, 2-QUAD,   3-SHELL_4N,
!                                 4-TRUSS,  5-BEAM, 6-SPRINGS,7-SHELL_3N)
!!                   GRTYPE --- > OBSOLETTE
C    IGRELEM(IGS)%SORTED  : FLAG for sorted/unsorted elements
!                    = 0 -> sorted
!                    = 1 -> unsorted
C    IGRELEM(IGS)%GRPGRP : TYPE of element GROUP
!                                 = 1 ELEM
!                                 = 2 GRELEM
!                                 = 3 SUBSET,PART,MAT,PROP
!                                 = 4 BOX,GENE
!                                 = 5 SUBMOD
C    IGRELEM(IGS)%LEVEL  : Hierachy level
!                             (FLAG 'SUBLEVEL DONE' FOR GROUPS OF GROUPS)
!                            = 0 ---> not yet initialized
!                            = 1 ---> done
C-----------------------------------------------
C     LECTURE DES GROUPES D'ELEMENTS
C=======================================================================
C-----------------------------------------------
C     BRICK
C-----------------------------------------------
      IF (NGRBRIC > 0)THEN
        CALL HM_LECGRE(
     1      IGRBRIC,IXS   ,
     2      8      ,NIXS   ,NUMELS,NGRBRIC,    1,
     3      IPART ,IPARTS ,X      ,'BRIC',
     4      ISUBMOD  ,FLAGG ,ADMBID,ADMBID,ADMBID,
     5      ADMBID   ,ADMBID,SKEW ,ISKN   ,UNITAB,
     6      ITABM1   ,IBOX  ,RTRANS ,LSUBMODEL,
     7      IXS_S    ,IXS_S_IND,IADBOXMAX,SUBSET,'/GRBRIC')
      ENDIF
C-----------------------------------------------
C     QUAD
C-----------------------------------------------
      IF (NGRQUAD > 0)THEN
        CALL HM_LECGRE(
     1      IGRQUAD,IXQ   ,
     2      4      ,NIXQ   ,NUMELQ,NGRQUAD,    2,
     3      IPART ,IPARTQ ,X      ,'QUAD',
     4      ISUBMOD  ,FLAGG ,ADMBID,ADMBID,ADMBID,
     5      ADMBID   ,ADMBID,SKEW ,ISKN   ,UNITAB,
     6      ITABM1  ,IBOX   ,RTRANS ,LSUBMODEL,
     7      IXQ_S    ,IXQ_S_IND,IADBOXMAX,SUBSET,'/GRQUAD')
      ENDIF
C-----------------------------------------------
C     SHELL
C-----------------------------------------------
      IF (NGRSHEL > 0)THEN
        CALL HM_LECGRE(
     1      IGRSH4N,IXC   ,
     2      4      ,NIXC   ,NUMELC,NGRSHEL,    3,
     3      IPART ,IPARTC ,X      ,'SHEL',
     4      ISUBMOD,FLAGG ,KSH4TREE,SH4TREE,2    ,
     5      4      ,3     ,SKEW   ,ISKN   ,UNITAB,
     6      ITABM1 ,IBOX  ,RTRANS ,LSUBMODEL,
     7      IXC_S    ,IXC_S_IND,IADBOXMAX,SUBSET,'/GRSHEL')
      ENDIF
C-----------------------------------------------
C     TRUS
C-----------------------------------------------
      IF (NGRTRUS > 0)THEN
        CALL HM_LECGRE(
     1      IGRTRUSS,IXT   ,
     2      2      ,NIXT   ,NUMELT,NGRTRUS,    4,
     3      IPART ,IPARTT ,X      ,'TRUS',
     4      ISUBMOD  ,FLAGG ,ADMBID,ADMBID,ADMBID,
     5      ADMBID   ,ADMBID,SKEW ,ISKN   ,UNITAB,
     6      ITABM1   ,IBOX  ,RTRANS ,LSUBMODEL,
     7      IXT_S    ,IXT_S_IND,IADBOXMAX,SUBSET,'/GRTRUS')
      ENDIF
C-----------------------------------------------
C     BEAM
C-----------------------------------------------
      IF (NGRBEAM > 0)THEN
        CALL HM_LECGRE(
     1      IGRBEAM,IXP   ,
     2      2      ,NIXP   ,NUMELP,NGRBEAM,    5,
     3      IPART ,IPARTP ,X      ,'BEAM',
     4      ISUBMOD  ,FLAGG ,ADMBID,ADMBID,ADMBID,
     5      ADMBID   ,ADMBID,SKEW ,ISKN   ,UNITAB,
     6      ITABM1   ,IBOX  ,RTRANS ,LSUBMODEL,
     7      IXP_S    ,IXP_S_IND,IADBOXMAX,SUBSET,'/GRBEAM')
      ENDIF
C-----------------------------------------------
C     SPRI GROUP
C-----------------------------------------------
      IF (NGRSPRI > 0)THEN
        CALL HM_LECGRE(
     1      IGRSPRING,IXR   ,
     2      2      ,NIXR   ,NUMELR,NGRSPRI,    6,
     3      IPART ,IPARTR ,X      ,'SPRI',
     4      ISUBMOD  ,FLAGG ,ADMBID,ADMBID,ADMBID,
     5      ADMBID   ,ADMBID,SKEW ,ISKN   ,UNITAB,
     6      ITABM1   ,IBOX  ,RTRANS ,LSUBMODEL,
     7      IXR_S    ,IXR_S_IND,IADBOXMAX,SUBSET,'/GRSPRI')
      ENDIF
C-----------------------------------------------
C     SH_3N GROUP
C-----------------------------------------------
      IF (NGRSH3N > 0 .AND. N2D==0)THEN
        CALL HM_LECGRE(
     1      IGRSH3N   ,IXTG      ,
     2      3         ,NIXTG     ,NUMELTG   ,NGRSH3N       ,7       ,
     3      IPART     ,IPARTG    ,X         ,'SH3N'       ,
     4      ISUBMOD   ,FLAGG     ,KSH3TREE  ,SH3TREE      ,2        ,
     5      4         ,3         ,SKEW      ,ISKN         ,UNITAB   ,
     6      ITABM1    ,IBOX      ,RTRANS    ,LSUBMODEL    ,
     7      IXTG_S    ,IXTG_S_IND,IADBOXMAX,SUBSET       ,'/GRSH3N')
      ENDIF
C-----------------------------------------------
C     TRIA GROUP
C-----------------------------------------------
      IF (NGRSH3N > 0 .AND. N2D/=0)THEN
        CALL HM_LECGRE(
     1      IGRSH3N   ,IXTG      ,
     2      3         ,NIXTG     ,NUMELTG   ,NGRSH3N      ,7        ,
     3      IPART     ,IPARTG    ,X         ,'TRIA'       ,
     4      ISUBMOD   ,FLAGG     ,KSH3TREE  ,SH3TREE      ,2        ,
     5      4         ,3         ,SKEW      ,ISKN         ,UNITAB   ,
     6      ITABM1    ,IBOX      ,RTRANS    ,LSUBMODEL    ,
     7      IXTG_S    ,IXTG_S_IND,IADBOXMAX,SUBSET        ,'/GRTRIA')
      ENDIF
C-----------
      RETURN
      END
