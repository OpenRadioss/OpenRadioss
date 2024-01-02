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
Chd|====================================================================
Chd|  H3D_NODAL_SCALAR              source/output/h3d/h3d_results/h3d_nodal_scalar.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        ANIMBALE                      source/output/anim/generate/genani.F
Chd|        H3D_WRITE_SCALAR              source/output/h3d/h3d_results/h3d_write_scalar.F
Chd|        NODALD                        source/output/anim/generate/nodald.F
Chd|        NODALDT                       source/output/anim/generate/nodaldt.F
Chd|        NODALP                        source/output/anim/generate/nodalp.F
Chd|        NODALSSP                      source/output/anim/generate/nodalssp.F
Chd|        NODALT                        source/output/anim/generate/nodalt.F
Chd|        NODALVOL                      source/output/anim/generate/nodalvol.F
Chd|        NODAL_SCHLIEREN               source/output/anim/generate/nodal_schlieren.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|====================================================================
      SUBROUTINE H3D_NODAL_SCALAR(
     .                  ELBUF_TAB          ,NODAL_SCALAR ,IFUNC           ,IPARG                  ,GEO     ,
     .                  MASS               ,PM           ,ANIN            ,ITAB                   ,NODE_ID ,
     .                  INFO1              ,INFO2        ,IS_WRITTEN_NODE ,H3D_PART               ,IPARTC  ,
     .                  IPARTTG            ,IXC          ,IXTG            ,TEMP                   ,IFLOW   ,
     .                  RFLOW              ,IXS          ,IXQ             ,NV46                   ,MONVOL  ,
     .                  VOLMON             ,ALE_CONNECT  ,DIAG_SMS        ,MS                     ,PDAMA2  ,
     .                  X                  ,STIFR        ,STIFN           ,KEYWORD                ,H3D_DATA,
     .                  NPBY               ,RBY          ,INTERSKID       ,NINTERSKID             ,PSKIDS ,
     .                  NODGLOB            ,ITYSKID      ,IPARTSP         ,IPARTR                 ,IPARTP  ,
     .                  IPARTT             ,IPARTS       ,IPARTQ          ,KXSP                   ,IXR     ,
     .                  IXP                ,IXT          ,N_H3D_PART_LIST ,INTERFRIC              ,CSEFRIC ,
     .                  CSEFRICG           ,CSEFRIC_STAMP,CSEFRICG_STAMP  ,NODAL_SCALAR_FVM       ,AIRBAGS_TOTAL_FVM_IN_H3D,
     .                  IS_WRITTEN_NODE_FVM,ISPMD        ,FVDATA_P        ,AIRBAGS_NODE_ID_SHIFT) 
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is storing relevant scalar values requested
C for H3D export by /H3D/NODA/... keywords
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
      USE H3D_MOD 
      USE ALE_CONNECTIVITY_MOD
      USE OUTPUTS_MOD
      USE GROUPDEF_MOD , only: GROUP_
      USE FVBAG_MOD , only : FVBAG_DATA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr14_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "sms_c.inc"
#include      "intstamp_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: AIRBAGS_TOTAL_FVM_IN_H3D,ISPMD, AIRBAGS_NODE_ID_SHIFT
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      my_real
     .   NODAL_SCALAR(NUMNOD),MASS(*),GEO(NPROPG,NUMGEO),
     .   PM(NPROPM,NUMMAT),ANIN(*),TEMP(*),RFLOW(*),VOLMON(*), DIAG_SMS(*),MS(*),
     .   PDAMA2(2,*),X(*),STIFR(*),STIFN(*),RBY(NRBY,*),PSKIDS(NINTERSKID,*)
      my_real,INTENT(INOUT) :: NODAL_SCALAR_FVM(AIRBAGS_TOTAL_FVM_IN_H3D)
      INTEGER IPARG(NPARG,*),IFUNC,NODE_ID(*),
     .   INFO1,INFO2,IS_WRITTEN_NODE(NUMNOD),H3D_PART(*),ITAB(NUMNOD),
     .   IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),IPARTC(*),IPARTTG(*),IFLOW(*),
     .   IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),NV46,MONVOL(*),NPBY(NNPBY,*),
     .   NODGLOB(*),
     .   INTERSKID,NINTERSKID,ITYSKID
      INTEGER ,INTENT(IN) :: IPARTSP(NUMSPH),IPARTR(NUMELR),IPARTP(NUMELP),
     .                       IPARTT(NUMELT),IPARTS(NUMELS),IPARTQ(NUMELQ)
      INTEGER ,INTENT(IN) :: KXSP(NISP,NUMSPH),IXR(NIXR,NUMELR),IXP(NIXP,NUMELP),
     .                       IXT(NIXT,NUMELT)
      CHARACTER*ncharline KEYWORD
      TYPE (H3D_DATABASE) :: H3D_DATA
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      INTEGER ,INTENT(IN) :: N_H3D_PART_LIST
      INTEGER , INTENT(IN) :: INTERFRIC
      my_real  , INTENT(IN) :: CSEFRIC(NINEFRIC,S_EFRICINT) ,CSEFRICG(S_EFRIC),
     .                         CSEFRIC_STAMP(NINEFRIC_STAMP,S_EFRICINTG) ,CSEFRICG_STAMP(S_EFRICG)
      INTEGER,INTENT(INOUT) :: IS_WRITTEN_NODE_FVM(AIRBAGS_TOTAL_FVM_IN_H3D)
      TYPE(FVBAG_DATA), INTENT(IN) :: FVDATA_P
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,M,N
      
      ! H3D export for /NODE entities. Number : NUMNOD
      INTEGER IOK_PART(NUMNOD)
      INTEGER IS_WRITTEN_VALUE(NUMNOD)
      my_real VALUE(NUMNOD)
      my_real VALUE_NUMNOD(NUMNOD)
      REAL VALUE_NUMNOD_REAL(NUMNOD)
      
      ! H3D export for FVMBAG (virtual nodes located at polyhedra centroids). Number : AIRBAGS_TOTAL_FVM_IN_H3D
      INTEGER IOK_PART_FVM(AIRBAGS_TOTAL_FVM_IN_H3D)
      my_real VALUE_FVM(AIRBAGS_TOTAL_FVM_IN_H3D)      
      my_real VALUE_NUMNOD_FVM(AIRBAGS_TOTAL_FVM_IN_H3D)
      REAL VALUE_NUMNOD_REAL_FVM(AIRBAGS_TOTAL_FVM_IN_H3D)
C-----------------------------------------------
C
      VALUE(1:NUMNOD) = ZERO
      VALUE_NUMNOD_REAL(1:NUMNOD) = ZERO
      VALUE_NUMNOD(1:NUMNOD) = ZERO
c
      VALUE_FVM(1:AIRBAGS_TOTAL_FVM_IN_H3D) = ZERO
      VALUE_NUMNOD_REAL_FVM(1:AIRBAGS_TOTAL_FVM_IN_H3D) = ZERO
      VALUE_NUMNOD_FVM(1:AIRBAGS_TOTAL_FVM_IN_H3D) = ZERO
      IOK_PART_FVM(1:AIRBAGS_TOTAL_FVM_IN_H3D) = 1  
c
      DO  I=1,NUMNOD 
        NODE_ID(I) = ITAB(I)
        IOK_PART(I) = 0
        IS_WRITTEN_NODE(I) = 0
      ENDDO
C
      IF(N_H3D_PART_LIST .NE. 0)THEN
        DO I=1,NUMSPH
          IF ( H3D_PART(IPARTSP(I)) == 1) THEN
            IF(KXSP(2,I) > 0 )IOK_PART(KXSP(2,I)) = 1
          ENDIF
        ENDDO
c      
        DO I=1,NUMELR
          IF ( H3D_PART(IPARTR(I)) == 1) THEN
            DO J=2,4
              IF(IXR(J,I) > 0 )IOK_PART(IXR(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO
c      
        DO I=1,NUMELP
          IF ( H3D_PART(IPARTP(I)) == 1) THEN
            DO J=2,4
              IF(IXP(J,I) > 0 )IOK_PART(IXP(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO
c      
        DO I=1,NUMELT
          IF ( H3D_PART(IPARTT(I)) == 1) THEN
            DO J=2,4
              IF(IXT(J,I) > 0 )IOK_PART(IXT(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO
c      
        DO I=1,NUMELC
          IF ( H3D_PART(IPARTC(I)) == 1) THEN
            DO J=2,5
              IF(IXC(J,I) > 0 )IOK_PART(IXC(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO
c      
        DO I=1,NUMELTG
          IF ( H3D_PART(IPARTTG(I)) == 1) THEN
            DO J=2,4
              IF(IXTG(J,I) > 0 )IOK_PART(IXTG(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO
c      
        DO I=1,NUMELS
          IF ( H3D_PART(IPARTS(I)) == 1) THEN
            DO J=2,9
              IF(IXS(J,I) > 0 )IOK_PART(IXS(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO
c      
        DO I=1,NUMELQ
          IF ( H3D_PART(IPARTQ(I)) == 1) THEN
            DO J=2,5
              IF(IXQ(J,I) > 0 )IOK_PART(IXQ(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO
      ELSE
        IOK_PART(1:NUMNOD) = 1
      ENDIF
C
C--------------------------------------------------
      IF(KEYWORD == 'DMASS' .OR. KEYWORD == 'DINER')THEN 
C--------------------------------------------------
        !IF(KEYWORD == 'DT') K = 0
        IF(KEYWORD == 'DMASS') K = MIN(1,ANIM_N(1)+H3D_DATA%N_SCAL_DT) * NUMNOD
        IF(KEYWORD == 'DINER') K =(MIN(1,ANIM_N(1)+H3D_DATA%N_SCAL_DT) +
     .                            MIN(1,ANIM_N(2)+H3D_DATA%N_SCAL_DMAS) ) * NUMNOD
        DO I=1,NUMNOD
         VALUE(I) = ANIN(I+K)       
         IS_WRITTEN_VALUE(I) = 1
        ENDDO
      ELSEIF(KEYWORD == 'MASS')THEN      ! IFUNC<3.OR.IFUNC==12)THEN
        DO I=1,NUMNOD
         VALUE(I) = MS(I)       
         IS_WRITTEN_VALUE(I) = 1
        ENDDO
        DO N=1,NRBYKIN
          M=NPBY(1,N)
          IF (M>0) THEN
            VALUE(M) = VALUE(M) +(RBY(15,N)-MS(M))
          ENDIF
        ENDDO
C--------------------------------------------------
      ELSEIF( KEYWORD == 'P'     .OR. KEYWORD == 'DENS'  .OR. KEYWORD == 'ENER'  .OR.
     .        KEYWORD == 'TEMP'  .OR. KEYWORD == 'SSP'   .OR. KEYWORD == 'DT'    .OR.
     .        KEYWORD == 'NVAR1' .OR. KEYWORD == 'NVAR2' .OR. KEYWORD == 'NVAR3' .OR. 
     .        KEYWORD == 'NVAR4' .OR. KEYWORD == 'NVAR5' .OR. KEYWORD == 'VOL') THEN  
C--------------------------------------------------
       IF(KEYWORD == 'TEMP' .AND. (ITHERM_FE  > 0  )) THEN
         DO I=1,NUMNOD
           VALUE(I)=TEMP(I) 
           IS_WRITTEN_VALUE(I) = 1
         ENDDO
       ELSE
c
         IF(KEYWORD == 'P') IFUNC = 3
         IF(KEYWORD == 'DENS') IFUNC = 4
         IF(KEYWORD == 'ENER') IFUNC = 5
         IF(KEYWORD == 'TEMP') IFUNC = 6
         IF(KEYWORD == 'NVAR1') IFUNC = 7
         IF(KEYWORD == 'NVAR2') IFUNC = 8
         IF(KEYWORD == 'NVAR3') IFUNC = 9
         IF(KEYWORD == 'NVAR4') IFUNC = 10
         IF(KEYWORD == 'NVAR5') IFUNC = 11
         IF(KEYWORD == 'SSP') IFUNC = 30
         IF(KEYWORD == 'VOL') IFUNC = 0 !useless         
         IF(KEYWORD == 'DT') THEN
           IFUNC = 0 !not 
           K = 0    
           DO I=1,NUMNOD
             VALUE(I) = ANIN(I+K)       
             IS_WRITTEN_VALUE(I) = 1
           ENDDO                  
         ENDIF
         !Nodal Pressure
         IF (KEYWORD == 'P') THEN
           IF(N2D==0)CALL NODALP(IFUNC,VALUE_NUMNOD_REAL,VALUE_NUMNOD_REAL_FVM,IFLOW  ,RFLOW,
     .                           IPARG,ELBUF_TAB        ,IXS                  ,NIXS   ,NUMELS,
     .                           ITAB ,NV46             ,MONVOL               ,VOLMON ,AIRBAGS_TOTAL_FVM_IN_H3D,
     .                           IS_WRITTEN_NODE        ,IS_WRITTEN_NODE_FVM  ,ISPMD, FVDATA_P, 
     .                           NUMNOD                 ,AIRBAGS_NODE_ID_SHIFT)
           IF(N2D/=0)CALL NODALP(IFUNC,VALUE_NUMNOD_REAL,VALUE_NUMNOD_REAL_FVM,IFLOW  ,RFLOW ,
     .                           IPARG,ELBUF_TAB        ,IXQ                  ,NIXQ   ,NUMELQ,
     .                           ITAB ,NV46             ,MONVOL               ,VOLMON ,AIRBAGS_TOTAL_FVM_IN_H3D,
     .                           IS_WRITTEN_NODE        ,IS_WRITTEN_NODE_FVM  ,ISPMD, FVDATA_P,
     .                           NUMNOD                 ,AIRBAGS_NODE_ID_SHIFT)
         ENDIF
         !Nodal Density
         IF (KEYWORD == 'DENS') THEN
           IF(N2D==0)CALL NODALD(IFUNC,VALUE_NUMNOD_REAL,VALUE_NUMNOD_REAL_FVM,IFLOW  ,RFLOW,
     .                           IPARG,ELBUF_TAB        ,IXS                  ,NIXS   ,NUMELS,
     .                           ITAB ,NV46             ,MONVOL               ,VOLMON ,AIRBAGS_TOTAL_FVM_IN_H3D,
     .                           IS_WRITTEN_NODE        ,IS_WRITTEN_NODE_FVM  ,ISPMD, FVDATA_P, 
     .                           NUMNOD                 ,AIRBAGS_NODE_ID_SHIFT) 
           IF(N2D/=0)CALL NODALD(IFUNC,VALUE_NUMNOD_REAL,VALUE_NUMNOD_REAL_FVM,IFLOW  ,RFLOW,
     .                           IPARG,ELBUF_TAB        ,IXQ                  ,NIXQ   ,NUMELQ,
     .                           ITAB ,NV46             ,MONVOL               ,VOLMON ,AIRBAGS_TOTAL_FVM_IN_H3D,
     .                           IS_WRITTEN_NODE        ,IS_WRITTEN_NODE_FVM  ,ISPMD, FVDATA_P,
     .                           NUMNOD                 ,AIRBAGS_NODE_ID_SHIFT) 
         ENDIF
         !Nodal Temperature
         IF (KEYWORD == 'TEMP') THEN
           IF(N2D==0)CALL NODALT(IFUNC,VALUE_NUMNOD_REAL,VALUE_NUMNOD_REAL_FVM,IFLOW  ,RFLOW,
     .                           IPARG,ELBUF_TAB        ,IXS                  ,NIXS   ,NUMELS,
     .                           ITAB ,NV46             ,MONVOL               ,VOLMON ,AIRBAGS_TOTAL_FVM_IN_H3D,
     .                           IS_WRITTEN_NODE        ,IS_WRITTEN_NODE_FVM  ,ISPMD, FVDATA_P,
     .                           NUMNOD                 ,AIRBAGS_NODE_ID_SHIFT) 
           IF(N2D/=0)CALL NODALT(IFUNC,VALUE_NUMNOD_REAL,VALUE_NUMNOD_REAL_FVM,IFLOW  ,RFLOW ,
     .                           IPARG,ELBUF_TAB        ,IXQ                  ,NIXQ   ,NUMELQ,
     .                           ITAB ,NV46             ,MONVOL               ,VOLMON ,AIRBAGS_TOTAL_FVM_IN_H3D,
     .                           IS_WRITTEN_NODE        ,IS_WRITTEN_NODE_FVM  ,ISPMD, FVDATA_P,
     .                           NUMNOD                 ,AIRBAGS_NODE_ID_SHIFT) 
         ENDIF
         !Nodal Sound Speed
         IF (KEYWORD == 'SSP') THEN
           IF(N2D==0)CALL NODALSSP(IFUNC,VALUE_NUMNOD_REAL,VALUE_NUMNOD_REAL_FVM,IFLOW  ,RFLOW,
     .                             IPARG,ELBUF_TAB        ,IXS                  ,NIXS   ,NUMELS,
     .                             ITAB ,NV46             ,MONVOL               ,VOLMON ,AIRBAGS_TOTAL_FVM_IN_H3D,
     .                             IS_WRITTEN_NODE        ,IS_WRITTEN_NODE_FVM  ,ISPMD, FVDATA_P,
     .                           NUMNOD                 ,AIRBAGS_NODE_ID_SHIFT) 
           IF(N2D/=0)CALL NODALSSP(IFUNC,VALUE_NUMNOD_REAL,VALUE_NUMNOD_REAL_FVM,IFLOW  ,RFLOW ,
     .                             IPARG,ELBUF_TAB        ,IXQ                  ,NIXQ   ,NUMELQ,
     .                             ITAB ,NV46             ,MONVOL               ,VOLMON ,AIRBAGS_TOTAL_FVM_IN_H3D,
     .                             IS_WRITTEN_NODE        ,IS_WRITTEN_NODE_FVM  ,ISPMD, FVDATA_P,
     .                           NUMNOD                 ,AIRBAGS_NODE_ID_SHIFT) 
         ENDIF   
         !Nodal Sound Speed
         IF (KEYWORD == 'DT') THEN
           IF(N2D==0)CALL NODALDT(IFUNC,VALUE_NUMNOD_REAL,VALUE_NUMNOD_REAL_FVM,IFLOW  ,RFLOW,
     .                            IPARG,ELBUF_TAB        ,IXS                  ,NIXS   ,NUMELS,
     .                            ITAB ,NV46             ,MONVOL               ,VOLMON ,AIRBAGS_TOTAL_FVM_IN_H3D,
     .                            IS_WRITTEN_NODE        ,IS_WRITTEN_NODE_FVM  ,ISPMD, FVDATA_P,
     .                           NUMNOD                 ,AIRBAGS_NODE_ID_SHIFT) 
           IF(N2D/=0)CALL NODALDT(IFUNC,VALUE_NUMNOD_REAL,VALUE_NUMNOD_REAL_FVM,IFLOW  ,RFLOW ,
     .                            IPARG,ELBUF_TAB        ,IXQ                  ,NIXQ   ,NUMELQ,
     .                            ITAB ,NV46             ,MONVOL               ,VOLMON ,AIRBAGS_TOTAL_FVM_IN_H3D,
     .                            IS_WRITTEN_NODE        ,IS_WRITTEN_NODE_FVM  ,ISPMD, FVDATA_P,
     .                           NUMNOD                 ,AIRBAGS_NODE_ID_SHIFT) 
         ENDIF  
         !Nodal Sound Speed
         IF (KEYWORD == 'VOL') THEN
           IF(N2D==0)CALL NODALVOL(IFUNC,VALUE_NUMNOD_REAL,VALUE_NUMNOD_REAL_FVM,IFLOW  ,RFLOW,
     .                            IPARG,ELBUF_TAB        ,IXS                  ,NIXS   ,NUMELS,
     .                            ITAB ,NV46             ,MONVOL               ,VOLMON ,AIRBAGS_TOTAL_FVM_IN_H3D,
     .                            IS_WRITTEN_NODE        ,IS_WRITTEN_NODE_FVM  ,ISPMD, FVDATA_P,
     .                           NUMNOD                 ,AIRBAGS_NODE_ID_SHIFT) 
           IF(N2D/=0)CALL NODALVOL(IFUNC,VALUE_NUMNOD_REAL,VALUE_NUMNOD_REAL_FVM,IFLOW  ,RFLOW ,
     .                            IPARG,ELBUF_TAB        ,IXQ                  ,NIXQ   ,NUMELQ,
     .                            ITAB ,NV46             ,MONVOL               ,VOLMON ,AIRBAGS_TOTAL_FVM_IN_H3D,
     .                            IS_WRITTEN_NODE        ,IS_WRITTEN_NODE_FVM  ,ISPMD, FVDATA_P,
     .                           NUMNOD                 ,AIRBAGS_NODE_ID_SHIFT) 
         ENDIF                        
         !Then Monitored Volumes (Pressure,Density,Temperature,Sound Speed)
         CALL ANIMBALE(IFUNC, VALUE_NUMNOD_REAL, MONVOL, VOLMON, 1)
         DO I=1,NUMNOD
           VALUE(I)=VALUE_NUMNOD_REAL(I) 
           IS_WRITTEN_VALUE(I) = IS_WRITTEN_NODE(I)
         ENDDO
c
       ENDIF
C--------------------------------------------------
      ELSEIF (KEYWORD == 'FPOT') THEN
C--------------------------------------------------
        CALL NODALP(13                 , NODAL_SCALAR         , VALUE_NUMNOD_REAL_FVM, IFLOW    , RFLOW,
     .              IPARG              , ELBUF_TAB            , IXS                  , NIXS     , NUMELS,
     .              ITAB               , NV46                 , MONVOL               , VOLMON   , AIRBAGS_TOTAL_FVM_IN_H3D,
     .              IS_WRITTEN_NODE    , IS_WRITTEN_NODE_FVM  , ISPMD                , FVDATA_P ,
     .              NUMNOD             , AIRBAGS_NODE_ID_SHIFT) 
        DO I=1,NUMNOD 
          IS_WRITTEN_VALUE(I) = 1
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'NDMASS')THEN
C--------------------------------------------------
        IF(IDTMINS /= 0)THEN
          DO I=1,NUMNOD
            VALUE(I)=MAX(ZERO,DIAG_SMS(I)/MAX(EM20,MS(I))-ONE)   
            IS_WRITTEN_VALUE(I) = 1
          ENDDO
        ENDIF
C--------------------------------------------------
      ELSEIF(KEYWORD == 'DAMA2/NORMAL')THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(I)=PDAMA2(1,I)   
          IS_WRITTEN_VALUE(I) = 1
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'DAMA2/TANGENT')THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(I)=PDAMA2(2,I)   
          IS_WRITTEN_VALUE(I) = 1
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'SCHLI')THEN
C--------------------------------------------------
         CALL NODAL_SCHLIEREN(VALUE,X,IXS,IXQ,ITAB,IPARG,0,ELBUF_TAB,ALE_CONNECT)  
         DO I=1,NUMNOD
           IS_WRITTEN_VALUE(I) = 1
         ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'STIFR')THEN
C--------------------------------------------------
        IF(IRODDL/=0)THEN
          DO I=1,NUMNOD
            VALUE(I)=STIFR(I)     
            IS_WRITTEN_VALUE(I) = 1
          ENDDO
        ENDIF   
C-------------------------------------------------- 
      ELSEIF(KEYWORD == 'STIF')THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(I)=STIFN(I)     
          IS_WRITTEN_VALUE(I) = 1
        ENDDO
C-------------------------------------------------- 
      ELSEIF(KEYWORD == 'SKID_LINE')THEN
C--------------------------------------------------
        IF(ITYSKID == 21 ) THEN
           DO I=1,NUMNOD
              K=NODGLOB(I)
              VALUE(I)=PSKIDS(INTERSKID,K)     
              IS_WRITTEN_VALUE(I) = 1
           ENDDO
        ELSE
           DO I=1,NUMNOD
              VALUE(I)=PSKIDS(INTERSKID,I)     
              IS_WRITTEN_VALUE(I) = 1
           ENDDO
        ENDIF
C--------------------------------------------------
      ELSEIF(KEYWORD == 'INTERNAL.ID')THEN 
C-------------------------------------------------- 
        DO I=1,NUMNOD
          VALUE(I) = I
          IS_WRITTEN_VALUE(I) = 1  
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'CSE_FRIC')THEN
C--------------------------------------------------
        IF(INTERFRIC > 0) THEN
          IF(INTERFRIC <= NINEFRIC ) THEN
             DO I=1,NUMNOD
                VALUE(I)=CSEFRIC(INTERFRIC,I)     
                IS_WRITTEN_VALUE(I) = 1
             ENDDO
          ELSE
             DO I=1,NUMNOD
                K=NODGLOB(I)
                VALUE(I)=CSEFRIC_STAMP(INTERFRIC-NINEFRIC,K)     
                IS_WRITTEN_VALUE(I) = 1
             ENDDO
          ENDIF 
        ENDIF
C--------------------------------------------------
      ELSEIF(KEYWORD == 'CSE_FRICG')THEN
C--------------------------------------------------
        IF(NINTSTAMP==0) THEN
           DO I=1,NUMNOD
              VALUE(I)=CSEFRICG(I)  
              IS_WRITTEN_VALUE(I) = 1
           ENDDO
        ELSE
           IF(NSPMD > 1 ) THEN
              DO I=1,NUMNOD
                 VALUE(I)=CSEFRICG(I)  
                 IS_WRITTEN_VALUE(I) = 1
              ENDDO
           ELSE
              DO I=1,NUMNOD
                 K=NODGLOB(I)
                 VALUE(I)=CSEFRICG(I) + CSEFRICG_STAMP(K)     
                 IS_WRITTEN_VALUE(I) = 1
              ENDDO
           ENDIF
        ENDIF
      ENDIF
c   
      CALL H3D_WRITE_SCALAR(IOK_PART,IS_WRITTEN_NODE,NODAL_SCALAR,NUMNOD,0,0,VALUE,IS_WRITTEN_VALUE)

      IF(AIRBAGS_TOTAL_FVM_IN_H3D > 0)THEN
        DO I=1,AIRBAGS_TOTAL_FVM_IN_H3D
          VALUE_FVM(I)=VALUE_NUMNOD_REAL_FVM(I) 
        ENDDO
        CALL H3D_WRITE_SCALAR(IOK_PART_FVM, IS_WRITTEN_NODE_FVM, NODAL_SCALAR_FVM, AIRBAGS_TOTAL_FVM_IN_H3D,0, 0,
     .                        VALUE_FVM   , IS_WRITTEN_NODE_FVM)
      ENDIF


      RETURN
      END
