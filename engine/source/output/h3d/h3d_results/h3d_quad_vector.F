Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    h3d_quad_vector        ../engine/source/output/h3d/h3d_results/h3d_quad_vector.F
!||--- called by ------------------------------------------------------
!||    genh3d                 ../engine/source/output/h3d/h3d_results/genh3d.F
!||--- calls      -----------------------------------------------------
!||    h3d_write_vectors      ../engine/source/output/h3d/h3d_results/h3d_write_vectors.F
!||    initbuf                ../engine/share/resol/initbuf.F
!||    qrota_vect             ../engine/source/output/anim/generate/qrota_vect.F
!||--- uses       -----------------------------------------------------
!||    alefvm_mod             ../common_source/modules/ale/alefvm_mod.F
!||    elbufdef_mod           ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod            ../common_source/modules/elements/element_mod.F90
!||    initbuf_mod            ../engine/share/resol/initbuf.F
!||    multi_fvm_mod          ../common_source/modules/ale/multi_fvm_mod.F90
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||    schlieren_mod          ../engine/share/modules/schlieren_mod.F
!||    stack_mod              ../engine/share/modules/stack_mod.F
!||====================================================================
      SUBROUTINE H3D_QUAD_VECTOR(
     .                  ELBUF_TAB   ,QUAD_VECTOR,IFUNC   ,IPARG       ,GEO        ,
     .                  IXQ         ,IXC       ,IXTG      ,PM         ,
     .                  EL2FA       ,NBF       ,IADP        ,
     .                  NBF_L       ,EHOUR     ,ANIM      ,NBPART      ,IADG       ,
     .                  IPM         ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                  INVERT      ,X         ,V         ,W           ,
     .                  NV46        ,NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK       ,ID_ELEM   ,INFO1       ,INFO2      , 
     .                  IS_WRITTEN_QUAD,IPARTQ,IPARTTG   ,LAYER_INPUT ,IPT_INPUT  ,
     .                  PLY_INPUT   ,GAUSS_INPUT,IUVAR_INPUT,H3D_PART  ,KEYWORD   ,
     .                  BUFMAT      ,MULTI_FVM ,IR_INPUT  ,IS_INPUT    ,IT_INPUT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD 
      USE MULTI_FVM_MOD              
      USE ALEFVM_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
      use element_mod , only : nixq,nixc,nixtg
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   QUAD_VECTOR(3,*),X(3,NUMNOD),V(3,NUMNOD),W(3,NUMNOD),THKE(*),EHOUR(*),GEO(NPROPG,NUMGEO),
     .   ANIM(*),PM(NPROPM,*),ERR_THK_SH4(*), ERR_THK_SH3(*)
      INTEGER IPARG(NPARG,NGROUP),IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),EL2FA(*),
     .   IXQ(NIXQ,NUMELQ),IFUNC,NBF,
     .   IADP(*),NBF_L, NBPART,IADG(NSPMD,*),IPM(NPROPMI,NUMMAT),
     .   IGEO(NPROPGI,NUMGEO),INVERT(*),  NV46,ID_ELEM(*),
     .   INFO1,INFO2,IS_WRITTEN_QUAD(*),IPARTQ(*),IPARTTG(*),H3D_PART(*),
     .   LAYER_INPUT ,IPT_INPUT,GAUSS_INPUT,PLY_INPUT,IUVAR_INPUT,
     .   IR_INPUT,IS_INPUT,IT_INPUT,IS_WRITTEN_VECTOR(MVSIZ)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      CHARACTER(LEN=NCHARLINE100) :: KEYWORD
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   FAIL(MVSIZ),
     .   VALUE(3)
      my_real
     .   BUFMAT(*)
      my_real
     .   EVAR(3,MVSIZ)
      INTEGER I, NG, NEL, NPTR, NPTS, NPTT, NLAY, ILAY, 
     .        IR,IS,IT,MLW, NUVAR,
     .        JTURB,
     .        IHBE,
     .        ISUBSTACK,NERCVOIS(*),NESDVOIS(*),
     .        LERCVOIS(*),LESDVOIS(*),
     .        IVISC
      INTEGER 
     .        IOK_PART(MVSIZ),JJ(6),IUVAR,
     .        IS_WRITTEN_VALUE(MVSIZ)
 
 
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
 
 
      my_real,
     .  DIMENSION(:), POINTER  :: UVAR
 
 
 
      TARGET :: BUFMAT
C-----------------------------------------------
      ILAY = LAYER_INPUT
      IUVAR = IUVAR_INPUT
      IR = IR_INPUT
      IS = IS_INPUT
      IT = IT_INPUT
C
      DO I=1,NUMELQ
         IS_WRITTEN_QUAD(I) = 0
      ENDDO
C-----------
      DO 900 NG=1,NGROUP
       CALL INITBUF(IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTURB   ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
       IF(MLW /= 13) THEN
          NFT   =IPARG(3,NG)
          IAD   =IPARG(4,NG)
          ISUBSTACK = IPARG(71,NG)
          IVISC = IPARG(61,NG)
          IOK_PART(1:NEL) = 0 
          LFT=1
          LLT=NEL
!
          DO I=1,6
            JJ(I) = NEL*(I-1)
          ENDDO  
c
          VALUE(1:3) = ZERO
          DO I=1,NEL
            IS_WRITTEN_VALUE(I) = 0
          ENDDO       
          EVAR(1:3,1:NEL) = ZERO
          IS_WRITTEN_VECTOR(1:NEL) = 0
C-----------------------------------------------
C           QUAD
C-----------------------------------------------
         IF (ITY == 2) THEN

            GBUF => ELBUF_TAB(NG)%GBUF
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
            UVAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR
            JALE=(IPARG(7,NG)+IPARG(11,NG))
            JTURB=IPARG(12,NG)*JALE
            NPTR  = ELBUF_TAB(NG)%NPTR
            NPTS  = ELBUF_TAB(NG)%NPTS
            NPTT  = ELBUF_TAB(NG)%NPTT
            NLAY  = ELBUF_TAB(NG)%NLAY
            NUVAR = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT
c
            DO  I=1,NEL 
              ID_ELEM(NFT+I) = IXQ(NIXQ,NFT+I)
              IF( H3D_PART(IPARTQ(NFT+I)) == 1) IOK_PART(I) = 1
            ENDDO
C---------------------    
            DO I=1,NEL
              QUAD_VECTOR(1:3,NFT+I) = ZERO   ! Default = zero in all cases !
            ENDDO
c
            IUVAR = IUVAR_INPUT
C--------------------------------------------------
            IF (KEYWORD == 'VECT/VEL') THEN 
C--------------------------------------------------
               IF (MLW == 151) THEN
                  DO I = 1, NEL
                     EVAR(1,I) = MULTI_FVM%VEL(1, I + NFT)
                     EVAR(2,I) = MULTI_FVM%VEL(2, I + NFT)
                     EVAR(3,I) = MULTI_FVM%VEL(3, I + NFT)
                     IS_WRITTEN_VECTOR(I) = 1
                  ENDDO
               ELSE
                  DO I=1,NEL
                     IF(GBUF%G_MOM>0 .AND. ALEFVM_Param%IEnabled > 0)THEN
                        EVAR(1,I) = GBUF%MOM(JJ(1) + I) / GBUF%RHO(I) 
                        EVAR(2,I) = GBUF%MOM(JJ(2) + I) / GBUF%RHO(I)
                        EVAR(3,I) = GBUF%MOM(JJ(3) + I) / GBUF%RHO(I)
                        IS_WRITTEN_VECTOR(I) = 1
                     ENDIF
                  ENDDO
               ENDIF
               IF (JCVT == 0 .OR. ISORTH /= 0) THEN
C                OUTPUT TENSOR STORED IN GLOBAL SYSTEM TO BE TRANSFERRED IN THE ELEMENT LOCAL ONE
                 CALL QROTA_VECT(X,IXQ(1,NFT+1),JCVT,EVAR,GBUF%GAMA,NEL)
               ENDIF
C--------------------------------------------------
            ELSEIF (KEYWORD == 'VECT/ACC') THEN
C--------------------------------------------------
               IF (MLW == 151 .AND. ALLOCATED(MULTI_FVM%ACC)) THEN
                  DO I = 1, NEL
                     EVAR(1,I) = MULTI_FVM%ACC(1, I + NFT)
                     EVAR(2,I) = MULTI_FVM%ACC(2, I + NFT)
                     EVAR(3,I) = MULTI_FVM%ACC(3, I + NFT)
                     IS_WRITTEN_VECTOR(I) = 1
                  ENDDO
               ENDIF
C--------------------------------------------------
            ENDIF  ! KEYWORD
C-----------------------------------------------
          CALL H3D_WRITE_VECTORS(IOK_PART,IS_WRITTEN_QUAD,QUAD_VECTOR,NEL,0,NFT,EVAR,IS_WRITTEN_VECTOR)
C-----------------------------------------------
          ENDIF  ! ITY

C-----------------------------------------------
       ENDIF     ! MLW /= 13
 900  CONTINUE   ! NG 
C-----------------------------------------------
      RETURN
      END
