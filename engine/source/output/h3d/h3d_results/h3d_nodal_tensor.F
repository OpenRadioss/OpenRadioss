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
Chd|  H3D_NODAL_TENSOR              source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        GET_TM_GPS                    source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|        H3D_WRITE_TENSOR              source/output/h3d/h3d_results/h3d_write_tensor.F
Chd|        SPMD_EXCH_NODAREA2            source/mpi/anim/spmd_exch_nodarea2.F
Chd|        SPMD_EXCH_NODAREAI            source/mpi/anim/spmd_exch_nodareai.F
Chd|        TENSGPS1                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS2                      source/output/anim/generate/tensor6.F
Chd|        TENSGPS3                      source/output/anim/generate/tensor6.F
Chd|        TENSGPSTRAIN                  source/output/anim/generate/tensgpstrain.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        OUTMAX_MOD                    ../common_source/modules/outmax_mod.F
Chd|====================================================================
      SUBROUTINE H3D_NODAL_TENSOR(
     .                  ELBUF_TAB, NODAL_TENSOR, IFUNC          , IPARG      , GEO      ,
     .                  MASS     , PM          , ANIN           , ITAB       , NODE_ID  ,
     .                  INFO1    , INFO2       , IS_WRITTEN_NODE, H3D_PART   , IPARTC   ,
     .                  IPARTTG  , IXC         , IXTG           , TEMP       , IFLOW    ,
     .                  RFLOW    , IXS         , IXQ            , NV46,MONVOL, VOLMON   ,
     .                  DIAG_SMS    , MS             , PDAMA2     , X        , 
     .                  STIFR    , STIFN       , A              , D          , V        , 
     .                  CONT     , FCONTG      , FINT           , FEXT       , KEYWORD  ,
     .                  BUFMAT   , IXS10       , IXS16          , IXS20      , IXT      ,
     .                  IXP      , IXR         , IAD_ELEM       , FR_ELEM    , WEIGHT   , 
     .                  IPARTSP  , IPARTR      , IPARTP         , IPARTT     , IPARTS   ,
     .                  IPARTQ   , KXSP        , N_H3D_PART_LIST)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
      USE OUTMAX_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      my_real
     .   NODAL_TENSOR(*),MASS(*),GEO(NPROPG,*),
     .   PM(NPROPM,*),ANIN(*),TEMP(*),RFLOW(*),VOLMON(*), DIAG_SMS(*),MS(*),
     .   PDAMA2(2,*),X(*),STIFR(*),STIFN(*),A(3,*),D(3,*),V(3,*), CONT(3,*),
     .   FCONTG(3,*), FINT(3,*), FEXT(3,*),BUFMAT(*)
      INTEGER IPARG(NPARG,*),IFUNC,NODE_ID(*),
     .   INFO1,INFO2,IS_WRITTEN_NODE(*),H3D_PART(*),ITAB(*),
     .   IXC(NIXC,*),IXTG(NIXTG,*),IPARTC(*),IPARTTG(*),IFLOW(*),
     .   IXS(NIXS,*),IXQ(NIXQ,*),NV46,MONVOL(*),
     .   IXS10(6,*) ,IXS16(8,*) ,IXS20(12,*) ,
     .   IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IAD_ELEM(2,*),FR_ELEM(*),WEIGHT(*)
      CHARACTER*ncharline KEYWORD
      INTEGER ,INTENT(IN) :: IPARTSP(NUMSPH),IPARTR(NUMELR),IPARTP(NUMELP),
     .                       IPARTT(NUMELT),IPARTS(NUMELS),IPARTQ(NUMELQ)
      INTEGER ,INTENT(IN) :: KXSP(NISP,NUMSPH)
      INTEGER ,INTENT(IN) :: N_H3D_PART_LIST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,IOK_PART(NUMNOD),LENR
      my_real
     .   VALUE(6,NUMNOD)
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGPS    
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: AFLU, VFLU
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: VGPS
C-----------------------------------------------
C
      ALLOCATE(AFLU(3,NUMNOD))
      ALLOCATE(VFLU(3,NUMNOD))
      ALLOCATE(ITAGPS(NUMNOD))
      ALLOCATE(VGPS(NUMNOD))


      VALUE(1:6,1:NUMNOD) = ZERO
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
C-----------------------------------------------
      IF(KEYWORD == 'GPS') THEN
C-----------------------------------------------
         DO N=1,NUMNOD
          ITAGPS(N) = 0
         ENDDO
         DO J=1,3
          DO N=1,NUMNOD
           VFLU(J,N) = ZERO
           AFLU(J,N) = ZERO
          ENDDO
         ENDDO
         CALL TENSGPS3(ELBUF_TAB,VFLU	 ,AFLU    ,IPARG   ,GEO     ,
     .  	       IXS	,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .  	       IXC	,IXTG	 ,IXT	  ,IXP     ,IXR     ,
     .  	       X	,ITAGPS  ,PM	  )

         IF(NSPMD > 1)THEN
           LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
           CALL SPMD_EXCH_NODAREAI(ITAGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
           DO J=1,3
            CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
            CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
           ENDDO
         ENDIF
         DO J=1,3
           DO N=1,NUMNOD
             IF (ITAGPS(N)>0) VALUE(J,N)=VFLU(J,N)/ITAGPS(N)
           ENDDO
         ENDDO
         DO J=4,6
           DO N=1,NUMNOD
             IF (ITAGPS(N)>0) VALUE(J,N)=AFLU(J-3,N)/ITAGPS(N)
           ENDDO
         ENDDO
C-----------------------------------------------
      ELSEIF(KEYWORD == 'GPS1') THEN
C-----------------------------------------------
        DO N=1,NUMNOD
         ITAGPS(N) = 0
        ENDDO
        DO J=1,3
         DO N=1,NUMNOD
          VFLU(J,N) = ZERO
          AFLU(J,N) = ZERO
         ENDDO
        ENDDO
        CALL TENSGPS1(VFLU    ,AFLU    ,IPARG	,GEO	 ,
     .  	      IXS   ,IXS10   ,IXS16   ,IXS20   ,IXQ	,
     .  	      IXC   ,IXTG    ,IXT     ,IXP     ,IXR	,
     .  	      X     ,ITAGPS  ,ELBUF_TAB)

         IF(NSPMD > 1)THEN
           LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
           CALL SPMD_EXCH_NODAREAI(ITAGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
           DO J=1,3
            CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
            CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
           ENDDO
         ENDIF

        DO J=1,3
          DO N=1,NUMNOD
            IF (ITAGPS(N)>0) VALUE(J,N)=VFLU(J,N)/ITAGPS(N)
          ENDDO
        ENDDO
        DO J=4,6
          DO N=1,NUMNOD
            IF (ITAGPS(N)>0) VALUE(J,N)=AFLU(J-3,N)/ITAGPS(N)
          ENDDO
        ENDDO
C-----------------------------------------------
      ELSEIF(KEYWORD == 'GPS2') THEN
C-----------------------------------------------
        DO N=1,NUMNOD
         VGPS(N) = ZERO
        ENDDO
        DO J=1,3
         DO N=1,NUMNOD
          VFLU(J,N) = ZERO
          AFLU(J,N) = ZERO
         ENDDO
        ENDDO
        CALL TENSGPS2(VFLU    ,AFLU    ,IPARG	,GEO	 ,
     .  	      IXS   ,IXS10   ,IXS16   ,IXS20   ,IXQ	,
     .  	      IXC   ,IXTG    ,IXT     ,IXP     ,IXR	,
     .  	      X     ,VGPS   ,ELBUF_TAB )

         IF(NSPMD > 1)THEN
           LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
           CALL SPMD_EXCH_NODAREAI(ITAGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
           DO J=1,3
            CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
            CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
           ENDDO
         ENDIF

        DO J=1,3
          DO N=1,NUMNOD
            IF (VGPS(N)>ZERO) VALUE(J,N)=VFLU(J,N)/VGPS(N)
          ENDDO
        ENDDO
        DO J=4,6
          DO N=1,NUMNOD
            IF (VGPS(N)>ZERO) VALUE(J,N)=AFLU(J-3,N)/VGPS(N)
          ENDDO
        ENDDO
C-----------------------------------------------
      ELSEIF(KEYWORD == 'GPSTRAIN') THEN
C-----------------------------------------------
         DO N=1,NUMNOD
          ITAGPS(N) = 0
         ENDDO
         DO J=1,3
          DO N=1,NUMNOD
           VFLU(J,N) = ZERO
           AFLU(J,N) = ZERO
          ENDDO
         ENDDO
         CALL TENSGPSTRAIN(ELBUF_TAB,VFLU	 ,AFLU    ,IPARG   ,GEO     ,
     .  	       IXS	,IXS10   ,IXS16   ,IXS20   ,IXQ     ,
     .  	       IXC	,IXTG	 ,IXT	  ,IXP     ,IXR     ,
     .  	       X	,ITAGPS  ,PM	  )
     
         IF(NSPMD > 1)THEN
           LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
           CALL SPMD_EXCH_NODAREAI(ITAGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
           DO J=1,3
            CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
            CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
           ENDDO
         ENDIF

         DO J=1,3
           DO N=1,NUMNOD
             IF (ITAGPS(N)>0) VALUE(J,N)=VFLU(J,N)/ITAGPS(N)
           ENDDO
         ENDDO
         DO J=4,6
           DO N=1,NUMNOD
             IF (ITAGPS(N)>0) VALUE(J,N)=AFLU(J-3,N)/ITAGPS(N)
           ENDDO
         ENDDO
C-----------------------------------------------
      ELSEIF(KEYWORD == 'GPS/TMAX') THEN
C-----------------------------------------------
         CALL GET_TM_GPS(VALUE,TM_NSIG1,NUMNOD,IOK_PART,IGPSTAG)
C-----------------------------------------------
      ELSEIF(KEYWORD == 'GPS/TMIN') THEN
C-----------------------------------------------
         CALL GET_TM_GPS(VALUE,TM_NSIG3,NUMNOD,IOK_PART,IGPSTAG)
C-----------------------------------------------
      ELSEIF(KEYWORD == 'GPSTRAIN/TMAX') THEN
C-----------------------------------------------
         CALL GET_TM_GPS(VALUE,TM_NSTRA1,NUMNOD,IOK_PART,IGPSTRATAG)
C-----------------------------------------------
      ELSEIF(KEYWORD == 'GPSTRAIN/TMIN') THEN
C-----------------------------------------------
         CALL GET_TM_GPS(VALUE,TM_NSTRA3,NUMNOD,IOK_PART,IGPSTRATAG)
C
C-----------------------------------------------
C
      ENDIF



      CALL H3D_WRITE_TENSOR(IOK_PART,IS_WRITTEN_NODE,NODAL_TENSOR,NUMNOD,0,0,
     .                                    VALUE,IOK_PART)

      DEALLOCATE(AFLU)
      DEALLOCATE(VFLU)
      DEALLOCATE(VGPS)
      DEALLOCATE(ITAGPS)

      RETURN
      END
Chd|====================================================================
Chd|  GET_TM_GPS                    source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|-- called by -----------
Chd|        H3D_NODAL_TENSOR              source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GET_TM_GPS(VALUE,TM_NSIG,NNOD,IOK_PART,IGPSTAG)
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NNOD
      my_real, DIMENSION(6,NNOD) ,INTENT(OUT):: VALUE
      my_real, DIMENSION(NNOD,6) ,INTENT(INOUT):: TM_NSIG
      INTEGER, DIMENSION(NNOD) ,INTENT(IN):: IGPSTAG
      INTEGER, DIMENSION(NNOD) ,INTENT(INOUT):: IOK_PART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J
      my_real
     .   S(3),NORM2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      DO  I=1,NNOD 
        IOK_PART(I) = MIN(IOK_PART(I),IGPSTAG(I))
        IF (IGPSTAG(I)==0) CYCLE
        VALUE(1:6,I) = TM_NSIG(I,1:6)
      ENDDO
C      
      RETURN
      END
