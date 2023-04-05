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
Chd|  H3D_SOLID_VECTOR              source/output/h3d/h3d_results/h3d_solid_vector.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_VECTOR              source/output/h3d/h3d_results/h3d_write_vector.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_SOLID_VECTOR(
     .                  ELBUF_TAB       ,SOLID_VECTOR ,IFUNC     ,IPARG       ,GEO         ,
     .                  IXQ             ,IXS          ,IXTG      ,PM          ,
     .                  EL2FA           ,NBF          ,IADP        ,
     .                  NBF_L           ,EHOUR        ,ANIM      ,NBPART      ,IADG        ,
     .                  IPM             ,IGEO         ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3 ,
     .                  INVERT          ,X            ,V         ,W           ,
     .                  NV46            ,NERCVOIS     ,NESDVOIS  ,LERCVOIS    ,LESDVOIS    ,
     .                  STACK           ,ID_ELEM      ,ITY_ELEM  ,IPARTS      ,LAYER_INPUT ,
     .                  IR_INPUT        ,IS_INPUT     ,IT_INPUT  ,IUVAR_INPUT ,H3D_PART    ,
     .                  IS_WRITTEN_SOLID,INFO1        ,KEYWORD   ,FANI_CELL   ,SFANI_CELL  ,
     .                  H3D_DATA        ,MULTI_FVM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD       
      USE H3D_MOD  
      USE MULTI_FVM_MOD
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
#include      "scr14_c.inc"
#include      "param_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SOLID_VECTOR(3,*),X(3,NUMNOD),V(3,NUMNOD),W(3,NUMNOD),THKE(*),EHOUR(*),GEO(NPROPG,NUMGEO),
     .   ANIM(*),PM(NPROPM,NUMMAT),ERR_THK_SH4(*), ERR_THK_SH3(*)
      INTEGER IPARG(NPARG,NGROUP),IXS(NIXS,NUMELS),IXTG(NIXTG,NUMELTG),EL2FA(*),
     .   IXQ(NIXQ,NUMELQ),IFUNC,NBF,
     .   IADP(*),NBF_L, NBPART,IADG(NSPMD,*),IPM(NPROPMI,NUMMAT),
     .   IGEO(NPROPGI,NUMGEO),INVERT(*), NV46,ID_ELEM(*),ITY_ELEM(*),IPARTS(*),
     .   H3D_PART(*),IS_WRITTEN_SOLID(*),INFO1,LAYER_INPUT,IR_INPUT,IS_INPUT,IT_INPUT,
     .   IUVAR_INPUT,SFANI_CELL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      CHARACTER*ncharline KEYWORD
      my_real, INTENT(IN) :: FANI_CELL(SFANI_CELL)
      TYPE (H3D_DATABASE) :: H3D_DATA
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real VALUE(3)
      INTEGER I,NG,NEL,NPTR,NPTS,NPTT,NLAY,ILAY,IR,IS,IT,MLW,OFFSET,
     .        NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),IUVAR,IDX,ILEN,IPOS,
     .        ISOLNOD,IVISC,NPTG,TSHELL,TSH_ORT,IOK_PART(MVSIZ),JJ(6),IS_WRITTEN_VALUE(MVSIZ)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF  
      TYPE(BUF_MAT_)  ,POINTER :: MBUF      
C-----------------------------------------------
      DO I=1,NUMELS
         IS_WRITTEN_SOLID(I) = 0
      ENDDO
C
c
      DO NG=1,NGROUP

        CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
       IF (MLW /= 13) THEN
          NFT = IPARG(3,NG)
          ISOLNOD = IPARG(28,NG)
          IVISC = IPARG(61,NG)
          IOK_PART(1:NEL) = 0 
          LFT=1
          LLT=NEL
c
          DO I=1,6
            JJ(I) = NEL*(I-1)
          ENDDO  
c
          VALUE(1:3) = ZERO
          DO I=1,NEL
            IS_WRITTEN_VALUE(I) = 0
          ENDDO	     
C-----------------------------------------------
          IF (ITY == 1) THEN
c           SOLID ELEMENTS
            IF (JCVT==1.AND.ISORTH/=0) JCVT=2
C-----------------------------------------------
            GBUF => ELBUF_TAB(NG)%GBUF
            MBUF => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)       
            NLAY = ELBUF_TAB(NG)%NLAY                    
            NPTR = ELBUF_TAB(NG)%NPTR                     
            NPTS = ELBUF_TAB(NG)%NPTS                     
            NPTT = ELBUF_TAB(NG)%NPTT                     
            NPTG = NPTT*NPTS*NPTR*NLAY
            TSHELL  = 0                                             
            TSH_ORT = 0  
            IF (IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22) TSHELL = 1 
            IF (IGTYP==21 .OR. IGTYP==22) TSH_ORT = 1               
            IF (ITY == 1) OFFSET = 0
c
            DO  I=1,NEL 
              IF (ITY == 1) THEN
                ID_ELEM(OFFSET+NFT+I) = IXS(NIXS,NFT+I)
                ITY_ELEM(OFFSET+NFT+I) = 1
                IF( H3D_PART(IPARTS(NFT+I)) == 1) IOK_PART(I) = 1
              ENDIF
            ENDDO  
c
            ILAY = LAYER_INPUT
            IUVAR = IUVAR_INPUT
            IR = IR_INPUT
            IS = IS_INPUT
            IT = IT_INPUT
            IF (ILAY == -2) ILAY = 1
            IF (ILAY == -3) ILAY = NLAY
C--------------------------------------------------
            IF (KEYWORD == 'VECT/VEL') THEN
C--------------------------------------------------
               IF (MLW == 151) THEN
                  DO I = 1, NEL
                     VALUE(1) = MULTI_FVM%VEL(1, I + NFT)
                     VALUE(2) = MULTI_FVM%VEL(2, I + NFT)
                     VALUE(3) = MULTI_FVM%VEL(3, I + NFT)
                     CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_SOLID,SOLID_VECTOR,I,OFFSET,NFT,VALUE)
                  ENDDO
               ELSE
                  DO I=1,NEL
                     IF(GBUF%G_MOM>0 )THEN
                        VALUE(1) = GBUF%MOM(JJ(1) + I) / GBUF%RHO(I) 
                        VALUE(2) = GBUF%MOM(JJ(2) + I) / GBUF%RHO(I)
                        VALUE(3) = GBUF%MOM(JJ(3) + I) / GBUF%RHO(I)
                        CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_SOLID,SOLID_VECTOR,I,OFFSET,NFT,VALUE)
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
C--------------------------------------------------
            IF (KEYWORD == 'VECT/CONT') THEN
C--------------------------------------------------
               IF (MLW == 151) THEN
                  IDX=NUMELS*(ANIM_SE(10)+ANIM_SE(4960)+ANIM_SE(4961)) !skip vorticity data
                  ILEN=NUMELS+NUMELQ           
                  DO I = 1, NEL
                     IPOS = IDX+3*(NFT+I-1)
                     VALUE(1) = FANI_CELL(IPOS+1)
                     VALUE(2) = FANI_CELL(IPOS+2)
                     VALUE(3) = FANI_CELL(IPOS+3)
                     CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_SOLID,SOLID_VECTOR,I,OFFSET,NFT,VALUE)
                  ENDDO
               ENDIF
            ENDIF
C--------------------------------------------------
         ENDIF
       ENDIF

      ENDDO!next NG
C-----------------------------------------------
      RETURN
      END
