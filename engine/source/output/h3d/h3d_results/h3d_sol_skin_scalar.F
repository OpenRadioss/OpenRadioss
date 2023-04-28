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
Chd|  H3D_SOL_SKIN_SCALAR           source/output/h3d/h3d_results/h3d_sol_skin_scalar.F
Chd|-- called by -----------
Chd|        H3D_SKIN_SCALAR               source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|-- calls ---------------
Chd|        GPSSTRAIN_SKIN                source/output/anim/generate/tensgpstrain.F
Chd|        H3D_SOL_SKIN_SCALAR1          source/output/h3d/h3d_results/h3d_sol_skin_scalar1.F
Chd|        SPMD_EXCH_NODAREA2            source/mpi/anim/spmd_exch_nodarea2.F
Chd|        SPMD_EXCH_NODAREAI            source/mpi/anim/spmd_exch_nodareai.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE H3D_SOL_SKIN_SCALAR(
     .                   ELBUF_TAB,SKIN_SCALAR, IPARG   ,IXS     ,X     ,PM  ,
     4                   IPARTS  ,IPM     ,IGEO    ,IXS10 ,IXS16 , IXS20  ,
     5                   IS_WRITTEN_SKIN  ,H3D_PART,INFO1   ,KEYWORD ,NSKIN ,
     6                   IAD_ELEM        ,FR_ELEM     , WEIGHT  , TAG_SKINS6,
     7                   NPF  ,TF   ,BUFMAT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
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
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   SKIN_SCALAR(*),PM(NPROPM,*), X(3,*),TF(*)  ,BUFMAT(*)
      INTEGER IPARG(NPARG,*), 
     .   IXS(NIXS,*),IPM(NPROPMI,*),IPARTS(*),
     .   IXS10(6,*) ,IXS16(8,*) ,IXS20(12,*) ,
     .   IGEO(NPROPGI,*),IS_WRITTEN_SKIN(*),NPF(*),
     .   H3D_PART(*),INFO1,NSKIN,TAG_SKINS6(*),IAD_ELEM(2,*),FR_ELEM(*),WEIGHT(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      CHARACTER*ncharline KEYWORD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I,I1,II,J,LENR,NEL,NFT,N

      INTEGER JJ,N1,N2
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGPS,TAG_SKIN_ND    
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: AFLU, VFLU,T6GPS
      INTEGER FACES(4,6),NS,K1,PWR(7),LL
      DATA PWR/1,2,4,8,16,32,64/
      DATA FACES/4,3,2,1,
     .           5,6,7,8,
     .           1,2,6,5,
     .           3,4,8,7,
     .           2,3,7,6,
     .           1,5,8,4/
C-----------------------------------------------
C
      ALLOCATE(AFLU(3,NUMNOD),VFLU(3,NUMNOD),T6GPS(6,NUMNOD))
      ALLOCATE(ITAGPS(NUMNOD),TAG_SKIN_ND(NUMNOD))
      AFLU  = ZERO
      VFLU  = ZERO
      T6GPS = ZERO
      ITAGPS = 0
C------TAG_SKIN_ND only the big seg(mid-node of S10 not include)
      TAG_SKIN_ND = 0 
        DO I=1,NUMELS
            LL=TAG_SKINS6(I)
            DO JJ=1,6
              IF(MOD(LL,PWR(JJ+1))/PWR(JJ) /= 0)CYCLE
              DO K1=1,4
                NS=IXS(FACES(K1,JJ)+1,I)
                TAG_SKIN_ND(NS) = 1 
              END DO
            END DO
        END DO
       IF (KEYWORD == 'FLDZ/OUTER' .OR. KEYWORD == 'FLDF/OUTER') THEN
         CALL GPSSTRAIN_SKIN(ELBUF_TAB,VFLU ,AFLU    ,IPARG   ,
     .                    IXS      ,IXS10   ,IXS16   ,IXS20   ,X        ,
     .                    ITAGPS  ,PM    ,TAG_SKIN_ND )
         IF(NSPMD > 1)THEN
           LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
           CALL SPMD_EXCH_NODAREAI(ITAGPS,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
           DO J=1,3
            CALL SPMD_EXCH_NODAREA2(VFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
            CALL SPMD_EXCH_NODAREA2(AFLU,IAD_ELEM,FR_ELEM,LENR,WEIGHT,J)
           ENDDO
         ENDIF
         DO N=1,NUMNOD
          IF (ITAGPS(N)>0) T6GPS(1:3,N)=VFLU(1:3,N)/ITAGPS(N)
         ENDDO
C------------change shear to eij         
         DO N=1,NUMNOD
           IF (ITAGPS(N)>0) T6GPS(4:6,N)=HALF*AFLU(1:3,N)/ITAGPS(N)
         ENDDO
       END IF
C       
      CALL H3D_SOL_SKIN_SCALAR1(ELBUF_TAB,IPARG,IPARTS,IXS,IXS10,
     .                          SKIN_SCALAR,TAG_SKINS6,T6GPS,X  ,
     .                          NPF,TF,BUFMAT,H3D_PART,IS_WRITTEN_SKIN,
     .                          IPM,KEYWORD,NSKIN )
C
       DEALLOCATE(AFLU,VFLU,T6GPS,ITAGPS,TAG_SKIN_ND)
C-----------
      RETURN
      END 
