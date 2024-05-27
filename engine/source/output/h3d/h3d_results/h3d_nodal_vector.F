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
Chd|  H3D_NODAL_VECTOR              source/output/h3d/h3d_results/h3d_nodal_vector.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        ANIM_NODAL_VECTOR_FVMBAGS     source/output/anim/generate/anim_nodal_vector_fvmbags.F
Chd|        H3D_VELVECC22                 source/output/h3d/h3d_results/h3d_velvecc22.F
Chd|        H3D_VELVECZ22                 source/output/h3d/h3d_results/h3d_velvecz22.F
Chd|        H3D_WRITE_VECTOR              source/output/h3d/h3d_results/h3d_write_vector.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        OUTMAX_MOD                    ../common_source/modules/outmax_mod.F
Chd|====================================================================
      SUBROUTINE H3D_NODAL_VECTOR(
     .                  ELBUF_TAB          ,NODAL_VECTOR            ,IFUNC          ,IPARG           ,GEO             , 
     .                  MASS               ,PM                      ,ANIN           ,ITAB            ,NODE_ID         , 
     .                  INFO1              ,INFO2                   ,IS_WRITTEN_NODE,H3D_PART        ,IPARTC          , 
     .                  IPARTTG            ,IXC                     ,IXTG           ,TEMP            ,IFLOW           , 
     .                  RFLOW              ,IXS                     ,IXQ            ,NV46            ,MONVOL          , 
     .                  DIAG_SMS           ,MS                      ,PDAMA2         ,X               ,VOLMON          , 
     .                  STIFR              ,STIFN                   ,A              ,D               ,V               , 
     .                  CONT               ,FCONTG                  ,FINT           ,FEXT            ,KEYWORD         , 
     .                  FNCONT             ,FNCONTG                 ,FTCONT         ,FTCONTG         ,FNCONT2         , 
     .                  DR                 ,DXANCG                  ,FANREAC        ,FCLUSTER        ,MCLUSTER        , 
     .                  VR                 ,FOPT                    ,NPBY           ,VGAZ            ,                  
     .                  IPARI              ,IGRNOD                  ,WEIGHT         ,NODGLOB         ,FCONT_MAX       , 
     .                  FNCONTP2           ,FTCONTP2                ,AR             ,IPARTSP         ,IPARTR          , 
     .                  IPARTP             ,IPARTT                  ,IPARTS         ,IPARTQ          ,KXSP            , 
     .                  IXR                ,IXP                     ,IXT            ,N_H3D_PART_LIST ,NODAL_VECTOR_FVM, 
     .                  IS_WRITTEN_NODE_FVM,AIRBAGS_TOTAL_FVM_IN_H3D,SMONVOL        ,SVOLMON         ,ISPMD           ,
     .                  FVDATA_P           ,AIRBAGS_NODE_ID_SHIFT   ,W              ,SW              ,X_C )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is storing relevant vector values requested
C for H3D export by /H3D/NODA/... keywords
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE GROUPDEF_MOD
      USE OUTMAX_MOD
      USE FVBAG_MOD , only : FVBAG_DATA, NFVBAG
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "intstamp_c.inc"
#include      "scr03_c.inc"
#include      "scr16_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: SMONVOL, SVOLMON, ISPMD,AIRBAGS_NODE_ID_SHIFT, AIRBAGS_TOTAL_FVM_IN_H3D
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER,INTENT(IN) :: SW
      my_real
     .   NODAL_VECTOR(3*NUMNOD),MASS(*),GEO(NPROPG,NUMGEO),
     .   PM(NPROPM,NUMMAT),ANIN(*),TEMP(*),RFLOW(*),VOLMON(SVOLMON), DIAG_SMS(*),MS(NUMNOD),
     .   PDAMA2(2,*),X(3,NUMNOD),STIFR(*),STIFN(NUMNOD),A(3,NUMNOD),D(3,NUMNOD),V(3,NUMNOD), CONT(3,*),
     .   FCONTG(3,*), FINT(3,NUMNOD), FEXT(3,NUMNOD),FNCONT(3,*),FNCONTG(3,*),
     .   FTCONT(3,*),FTCONTG(3,*),FNCONT2(3,*), DR(3,NUMNOD),DXANCG(3,*),
     .   FANREAC(6,*),FCLUSTER(3,*),MCLUSTER(3,*),VR(3,NUMNOD),FOPT(6,*),VGAZ(3,*),
     .   FCONT_MAX(3,*),FNCONTP2(3,*),FTCONTP2(3,*)
      my_real,INTENT(IN) :: W(3,SW/3)
      INTEGER IPARG(NPARG,*),IFUNC,NODE_ID(*),
     .   INFO1,INFO2,IS_WRITTEN_NODE(NUMNOD),H3D_PART(*),ITAB(NUMNOD),
     .   IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),IPARTC(*),IPARTTG(*),IFLOW(*),
     .   IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),NV46,MONVOL(SMONVOL),NPBY(NNPBY,*),
     .   IPARI(NPARI,NINTER),WEIGHT(*),NODGLOB(*)
      CHARACTER(LEN=NCHARLINE100):: KEYWORD
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
      my_real , INTENT(IN) :: AR(3,NUMNOD)
      my_real , INTENT(IN) , DIMENSION(3,NUMNOD) :: X_C
      INTEGER ,INTENT(IN) :: IPARTSP(NUMSPH),IPARTR(NUMELR),IPARTP(NUMELP),
     .                       IPARTT(NUMELT),IPARTS(NUMELS),IPARTQ(NUMELQ)
      INTEGER ,INTENT(IN) :: KXSP(NISP,NUMSPH),IXR(NIXR,NUMELR),IXP(NIXP,NUMELP),
     .                       IXT(NIXT,NUMELT)
      INTEGER ,INTENT(IN) :: N_H3D_PART_LIST
      my_real, INTENT(INOUT) :: NODAL_VECTOR_FVM(3,AIRBAGS_TOTAL_FVM_IN_H3D)
      INTEGER, INTENT(INOUT) :: IS_WRITTEN_NODE_FVM(AIRBAGS_TOTAL_FVM_IN_H3D)   
      TYPE(FVBAG_DATA),INTENT(IN) ::  FVDATA_P  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,IOK_PART(NUMNOD)
      my_real VALUE(3)
C-----------------------------------------------

      VALUE = ZERO

      DO  I=1,NUMNOD 
        NODE_ID(I) = ITAB(I)
        IOK_PART(I) = 0
        IS_WRITTEN_NODE(I) = 0
      ENDDO

      IF(N_H3D_PART_LIST /= 0)THEN
        DO I=1,NUMSPH
          IF ( H3D_PART(IPARTSP(I)) == 1) THEN
            IF(KXSP(2,I) > 0 )IOK_PART(KXSP(2,I)) = 1
          ENDIF
        ENDDO

        DO I=1,NUMELR
          IF ( H3D_PART(IPARTR(I)) == 1) THEN
            DO J=2,4
              IF(IXR(J,I) > 0 )IOK_PART(IXR(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO

        DO I=1,NUMELP
          IF ( H3D_PART(IPARTP(I)) == 1) THEN
            DO J=2,4
              IF(IXP(J,I) > 0 )IOK_PART(IXP(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO

        DO I=1,NUMELT
          IF ( H3D_PART(IPARTT(I)) == 1) THEN
            DO J=2,4
              IF(IXT(J,I) > 0 )IOK_PART(IXT(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO

        DO I=1,NUMELC
          IF ( H3D_PART(IPARTC(I)) == 1) THEN
            DO J=2,5
              IF(IXC(J,I) > 0 )IOK_PART(IXC(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO

        DO I=1,NUMELTG
          IF ( H3D_PART(IPARTTG(I)) == 1) THEN
            DO J=2,4
              IF(IXTG(J,I) > 0 )IOK_PART(IXTG(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO

        DO I=1,NUMELS
          IF ( H3D_PART(IPARTS(I)) == 1) THEN
            DO J=2,9
              IF(IXS(J,I) > 0 )IOK_PART(IXS(J,I)) = 1
            ENDDO
          ENDIF
        ENDDO

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
      IF(KEYWORD == 'VEL') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = V(1,I)
          VALUE(2) = V(2,I)
          VALUE(3) = V(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
        
        !------------------------------------------
        !  /MONVOL/FVMBAG*
        !     set velocity from polyhedra centroids
        !------------------------------------------
        IF(AIRBAGS_TOTAL_FVM_IN_H3D > 0)THEN
          IS_WRITTEN_NODE_FVM(1:AIRBAGS_TOTAL_FVM_IN_H3D)=0
          IF(NFVBAG>0 .AND. ISPMD == 0 .AND. AIRBAGS_TOTAL_FVM_IN_H3D > 0)THEN
           CALL ANIM_NODAL_VECTOR_FVMBAGS('VEL ' , NODAL_VECTOR_FVM,MONVOL,VOLMON                  ,FVDATA_P, NFVBAG   ,
     .                                    SMONVOL, SVOLMON                ,AIRBAGS_TOTAL_FVM_IN_H3D,IS_WRITTEN_NODE_FVM,
     .                                    AIRBAGS_NODE_ID_SHIFT )
          ENDIF
        ENDIF
        
C--------------------------------------------------
      ELSEIF(KEYWORD == 'DIS') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = D(1,I)
          VALUE(2) = D(2,I)
          VALUE(3) = D(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'ACC') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = A(1,I)
          VALUE(2) = A(2,I)
          VALUE(3) = A(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'CONT'.AND.KEYWORD /= 'CONT/TMAX')THEN
C--------------------------------------------------
       IF(NINTSTAMP==0)THEN
         DO I=1,NUMNOD
           VALUE(1) = CONT(1,I)
           VALUE(2) = CONT(2,I)
           VALUE(3) = CONT(3,I)
           CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
         ENDDO
       ELSE
         DO I=1,NUMNOD
           K=NODGLOB(I)
           VALUE(1) = CONT(1,I) + FCONTG(1,K)
           VALUE(2) = CONT(2,I) + FCONTG(2,K)
           VALUE(3) = CONT(3,I) + FCONTG(3,K)
           CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
         ENDDO
       END IF
C------------------------------------------------------------------------------
      ELSEIF(KEYWORD == 'CONT/TMAX')THEN
C------------------------------------------------------------------------------
       IF(NINTSTAMP==0.OR.NSPMD==1)THEN
         DO I=1,NUMNOD
            VALUE(1) = FCONT_MAX(1,I)
            VALUE(2) = FCONT_MAX(2,I)
            VALUE(3) = FCONT_MAX(3,I)
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
         ENDDO
       ELSE
         DO I=1,NUMNOD
            VALUE(1) = ZERO
            VALUE(2) = ZERO
            VALUE(3) = ZERO
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
         ENDDO
       ENDIF
C--------------------------------------------------
      ELSEIF(KEYWORD == 'FINT') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = FINT(1,I)
          VALUE(2) = FINT(2,I)
          VALUE(3) = FINT(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'FEXT') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = FEXT(1,I)
          VALUE(2) = FEXT(2,I)
          VALUE(3) = FEXT(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'FOPT/FORCE') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          IS_WRITTEN_NODE(I) = 0
        ENDDO
        DO N=1,NRBODY
          I = NPBY(1,N)
          IF (I>0) THEN
            IF (WEIGHT(I)==1) THEN
              VALUE(1) = FOPT(1,NSECT+N)
              VALUE(2) = FOPT(2,NSECT+N)
              VALUE(3) = FOPT(3,NSECT+N)
              CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
            ENDIF
          ENDIF
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'FOPT/MOMENT') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          IS_WRITTEN_NODE(I) = 0
        ENDDO
        DO N=1,NRBODY
          I = NPBY(1,N)
          IF (I>0) THEN
            IF (WEIGHT(I)==1) THEN
              VALUE(1) = FOPT(4,NSECT+N)
              VALUE(2) = FOPT(5,NSECT+N)
              VALUE(3) = FOPT(6,NSECT+N)
              CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
            ENDIF
          ENDIF
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'VROT') THEN
C--------------------------------------------------
        IF(IRODDL/=0) THEN
          DO I=1,NUMNOD
            VALUE(1) = VR(1,I)
            VALUE(2) = VR(2,I)
            VALUE(3) = VR(3,I)
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
          ENDDO
        ENDIF 
C--------------------------------------------------
      ELSEIF(KEYWORD == 'FVEL') THEN
C--------------------------------------------------
C--------------------------------------------------
      ELSEIF(KEYWORD == 'FRES') THEN
C--------------------------------------------------
C--------------------------------------------------
      ELSEIF(KEYWORD == 'PCONT/NORMAL') THEN
C--------------------------------------------------
        IF(NINTSTAMP==0)THEN
          DO I=1,NUMNOD
            VALUE(1) = FNCONT(1,I)
            VALUE(2) = FNCONT(2,I)
            VALUE(3) = FNCONT(3,I)
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
          ENDDO  
        ELSE
          DO I=1,NUMNOD
            K=NODGLOB(I)
            VALUE(1) = FNCONT(1,I)+FNCONTG(1,K)
            VALUE(2) = FNCONT(2,I)+FNCONTG(2,K)
            VALUE(3) = FNCONT(3,I)+FNCONTG(3,K)
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
          ENDDO  
        ENDIF
C------------------------------------------------------------------------------
      ELSEIF(KEYWORD == 'MAXPCONT/NORMAL')THEN
C------------------------------------------------------------------------------
       IF(NINTSTAMP==0.OR.NSPMD==1)THEN
         DO I=1,NUMNOD
            VALUE(1) = FNCONT_MAX(3*(I-1)+1)
            VALUE(2) = FNCONT_MAX(3*(I-1)+2)
            VALUE(3) = FNCONT_MAX(3*(I-1)+3)
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
         ENDDO
       ELSE
         DO I=1,NUMNOD
            VALUE(1) = ZERO
            VALUE(2) = ZERO
            VALUE(3) = ZERO
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
         ENDDO
       ENDIF
C--------------------------------------------------
      ELSEIF(KEYWORD == 'PCONT/TANGENT') THEN
C--------------------------------------------------
        IF(NINTSTAMP==0)THEN
          DO I=1,NUMNOD
            VALUE(1) = FTCONT(1,I)
            VALUE(2) = FTCONT(2,I)
            VALUE(3) = FTCONT(3,I)
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
          ENDDO  
        ELSE
          DO I=1,NUMNOD
            K=NODGLOB(I)
            VALUE(1) = FTCONT(1,I)+FTCONTG(1,K)
            VALUE(2) = FTCONT(2,I)+FTCONTG(2,K)
            VALUE(3) = FTCONT(3,I)+FTCONTG(3,K)
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
          ENDDO  
        ENDIF
C------------------------------------------------------------------------------
      ELSEIF(KEYWORD == 'MAXPCONT/TANGENT')THEN
C------------------------------------------------------------------------------
       IF(NINTSTAMP==0.OR.NSPMD==1)THEN
         DO I=1,NUMNOD
            VALUE(1) = FTCONT_MAX(3*(I-1)+1)
            VALUE(2) = FTCONT_MAX(3*(I-1)+2)
            VALUE(3) = FTCONT_MAX(3*(I-1)+3)
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
         ENDDO
       ELSE
         DO I=1,NUMNOD
            VALUE(1) = ZERO
            VALUE(2) = ZERO
            VALUE(3) = ZERO
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
         ENDDO
       ENDIF
C--------------------------------------------------
      ELSEIF(KEYWORD == 'CONT2') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = FNCONT2(1,I)
          VALUE(2) = FNCONT2(2,I)
          VALUE(3) = FNCONT2(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'CONT2/MOMENT') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = MCONT2(1,I)
          VALUE(2) = MCONT2(2,I)
          VALUE(3) = MCONT2(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO        
C--------------------------------------------------
      ELSEIF(KEYWORD == 'PCONT2/NORMAL') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = FNCONTP2(1,I)
          VALUE(2) = FNCONTP2(2,I)
          VALUE(3) = FNCONTP2(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'PCONT2/TANGENT') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = FTCONTP2(1,I)
          VALUE(2) = FTCONTP2(2,I)
          VALUE(3) = FTCONTP2(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'CONT2/TMAX') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = FCONT2_MAX(3*(I-1)+1)
          VALUE(2) = FCONT2_MAX(3*(I-1)+2)
          VALUE(3) = FCONT2_MAX(3*(I-1)+3)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'CONT2/TMIN') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = FCONT2_MIN(3*(I-1)+1)
          VALUE(2) = FCONT2_MIN(3*(I-1)+2)
          VALUE(3) = FCONT2_MIN(3*(I-1)+3)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'MAXPCONT2/NORMAL') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = FNCONT2_MAX(3*(I-1)+1)
          VALUE(2) = FNCONT2_MAX(3*(I-1)+2)
          VALUE(3) = FNCONT2_MAX(3*(I-1)+3)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'MAXPCONT2/TANGENT') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = FTCONT2_MAX(3*(I-1)+1)
          VALUE(2) = FTCONT2_MAX(3*(I-1)+2)
          VALUE(3) = FTCONT2_MAX(3*(I-1)+3)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'MINPCONT2/NORMAL') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = FNCONT2_MIN(3*(I-1)+1)
          VALUE(2) = FNCONT2_MIN(3*(I-1)+2)
          VALUE(3) = FNCONT2_MIN(3*(I-1)+3)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'MINPCONT2/TANGENT') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = FTCONT2_MIN(3*(I-1)+1)
          VALUE(2) = FTCONT2_MIN(3*(I-1)+2)
          VALUE(3) = FTCONT2_MIN(3*(I-1)+3)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'DROT')THEN
C--------------------------------------------------
        IF( (IDROT == 1 .OR. ISECUT>0 .OR. IISROT>0 .OR. IMPOSE_DR>0) .AND. IRODDL/=0) THEN
          DO I=1,NUMNOD
            VALUE(1) = DR(1,I)
            VALUE(2) = DR(2,I)
            VALUE(3) = DR(3,I)
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
          ENDDO
        ENDIF
C--------------------------------------------------
      ELSEIF (KEYWORD == 'DXANC') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1) = DXANCG(1,I)
          VALUE(2) = DXANCG(2,I)
          VALUE(3) = DXANCG(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF (KEYWORD == 'GVEL') THEN
C--------------------------------------------------
        IF(IALELAG  > 0 ) THEN
          DO I=1,NUMNOD
            VALUE(1) = VGAZ(1,I)
            VALUE(2) = VGAZ(2,I)
            VALUE(3) = VGAZ(3,I)
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
          ENDDO
        ENDIF
C--------------------------------------------------
      ELSEIF(KEYWORD == 'FREAC') THEN
C--------------------------------------------------
              DO I=1,NUMNOD
                VALUE(1)=FANREAC(1,I)
                VALUE(2)=FANREAC(2,I)
                VALUE(3)=FANREAC(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
              ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'MREAC') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1)=FANREAC(4,I)
          VALUE(2)=FANREAC(5,I)
          VALUE(3)=FANREAC(6,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'CLUSTER/FORCE') THEN
C--------------------------------------------------
              DO I=1,NUMNOD
                VALUE(1)=FCLUSTER(1,I)
                VALUE(2)=FCLUSTER(2,I)
                VALUE(3)=FCLUSTER(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
              ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'CLUSTER/MOMENT') THEN
C--------------------------------------------------
              DO I=1,NUMNOD
                VALUE(1)=MCLUSTER(1,I)
                VALUE(2)=MCLUSTER(2,I)
                VALUE(3)=MCLUSTER(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
              ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'ZVEL') THEN
        CALL H3D_VELVECC22(ELBUF_TAB,IPARG,1,IXS,IXQ,ITAB,IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR)
C--------------------------------------------------
C--------------------------------------------------
      ELSEIF(KEYWORD == 'ZFVEL') THEN
        CALL H3D_VELVECZ22(ELBUF_TAB,IPARG,IPARI,IGRNOD,X,IXS,IXQ,ITAB,1,IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR)
C--------------------------------------------------
C--------------------------------------------------
      ELSEIF(KEYWORD == 'ZMOM') THEN
        CALL H3D_VELVECC22(ELBUF_TAB,IPARG,2,IXS,IXQ,ITAB,IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR)
C--------------------------------------------------
C--------------------------------------------------
      ELSEIF(KEYWORD == 'ZFP') THEN
        CALL H3D_VELVECZ22(ELBUF_TAB,IPARG,IPARI,IGRNOD,X,IXS,IXQ,ITAB,2,IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR)
C--------------------------------------------------
C--------------------------------------------------
      ELSEIF(KEYWORD == 'ZFINT') THEN
        CALL H3D_VELVECC22(ELBUF_TAB,IPARG,3,IXS,IXQ,ITAB,IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR)
C--------------------------------------------------
      ELSEIF(KEYWORD == 'VEL/TMAX') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          J = 3*(I-1)+1
          VALUE(1) = TM_VEL(J)
          VALUE(2) = TM_VEL(J+1)
          VALUE(3) = TM_VEL(J+2)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'DIS/TMAX') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          J = 3*(I-1)+1
          VALUE(1) = TM_DIS(J)
          VALUE(2) = TM_DIS(J+1)
          VALUE(3) = TM_DIS(J+2)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ELSEIF(KEYWORD == 'AROT') THEN
C--------------------------------------------------
        IF(IRODDL/=0) THEN
          DO I=1,NUMNOD
            VALUE(1) = AR(1,I)
            VALUE(2) = AR(2,I)
            VALUE(3) = AR(3,I)
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
          ENDDO
        ENDIF
C--------------------------------------------------
      ELSEIF(KEYWORD == 'VEL/GRID') THEN
C--------------------------------------------------
        IF(SW > 0) THEN
          DO I=1,NUMNOD
            VALUE(1) = W(1,I)
            VALUE(2) = W(2,I)
            VALUE(3) = W(3,I)
            CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
          ENDDO
        ENDIF
C--------------------------------------------------
      ELSEIF(KEYWORD == 'SH_OFFSET') THEN
C--------------------------------------------------
        DO I=1,NUMNOD
          VALUE(1:3) = X_C(1:3,I) - X(1:3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,VALUE)
        ENDDO
C--------------------------------------------------
      ENDIF
C--------------------------------------------------
      RETURN
      END SUBROUTINE H3D_NODAL_VECTOR
