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
Chd|  H3D_SHELL_VECTOR_1            source/output/h3d/h3d_results/h3d_shell_vector_1.F
Chd|-- called by -----------
Chd|        H3D_SHELL_VECTOR              source/output/h3d/h3d_results/h3d_shell_vector.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_VECTOR              source/output/h3d/h3d_results/h3d_write_vector.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_SHELL_VECTOR_1(
     .                  ELBUF_TAB   ,SHELL_VECTOR,IFUNC   ,IPARG       ,GEO        ,
     .                  IXQ         ,IXC       ,IXTG      ,PM         ,
     .                  EL2FA       ,NBF       ,IADP        ,
     .                  NBF_L       ,EHOUR     ,ANIM      ,NBPART      ,IADG       ,
     .                  IPM         ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                  INVERT      ,X         ,V         ,W           ,
     .                  NV46        ,NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK       ,ID_ELEM   ,ITY_ELEM  ,INFO1       ,INFO2      , 
     .                  IS_WRITTEN_SHELL,IPARTC,IPARTTG   ,LAYER_INPUT ,IPT_INPUT  ,
     .                  PLY_INPUT   ,GAUSS_INPUT,IUVAR_INPUT,H3D_PART  ,KEYWORD    ,
     .                  D           ,NG        ,MULTI_FVM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD 
      USE MULTI_FVM_MOD              
      USE ALEFVM_MOD , only:ALEFVM_Param
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
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SHELL_VECTOR(3,*),X(3,NUMNOD),V(3,NUMNOD),W(3,NUMNOD),D(3,NUMNOD),THKE(*),EHOUR(*),GEO(NPROPG,NUMGEO),
     .   ANIM(*),PM(NPROPM,NUMMAT),ERR_THK_SH4(*), ERR_THK_SH3(*)
      INTEGER IPARG(NPARG,NGROUP),IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),EL2FA(*),
     .   IXQ(NIXQ,NUMELQ),IFUNC,NBF,
     .   IADP(*),NBF_L, NBPART,IADG(NSPMD,*),IPM(NPROPMI,NUMMAT),
     .   IGEO(NPROPGI,NUMGEO),INVERT(*), NV46,ID_ELEM(*),ITY_ELEM(*),
     .   INFO1,INFO2,IS_WRITTEN_SHELL(*),IPARTC(*),IPARTTG(*),H3D_PART(*),
     .   LAYER_INPUT ,IPT_INPUT,GAUSS_INPUT,PLY_INPUT,IUVAR_INPUT,NG
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      CHARACTER*ncharline KEYWORD
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real VALUE(3),P
      INTEGER I,NEL,NPTR,NPTS,NPTT,NLAY,ILAY,MLW,JTURB,NVARF,
     .        OFFSET,IHBE,NPG,MPT,IPT,ISUBSTACK,ITHK,NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*)
      INTEGER NPT_ALL,IPLY,
     .        IOK_PART(MVSIZ),JJ(5),IUVAR,
     .        IS_WRITTEN_VALUE(MVSIZ)
      REAL R4
      TYPE(G_BUFEL_)  ,POINTER :: GBUF    
      TYPE(L_BUFEL_)  ,POINTER :: LBUF
      my_real,DIMENSION(:), POINTER  :: UVAR
C----------------------------------------------- 

       CALL INITBUF (IPARG   ,NG      ,                    
     2               MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3               NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTURB   ,  
     4               JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5               NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6               IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7               ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )

       IF(MLW /= 13) THEN 
  
          NFT = IPARG(3,NG)
          IAD = IPARG(4,NG)  
          ISUBSTACK = IPARG(71,NG)

          IOK_PART(1:NEL) = 0  
!
          DO I=1,5
            JJ(I) = NEL*(I-1)
          ENDDO  
c
          DO I=1,NEL
            IS_WRITTEN_VALUE(I) = 0
          ENDDO	     
C----------------------------------------------- 
C           COQUES 3 N 4 N
C-----------------------------------------------
         IF (ITY == 3.OR.ITY == 7) THEN

            GBUF => ELBUF_TAB(NG)%GBUF
            NPT   = IPARG(6,NG)
            IHBE  = IPARG(23,NG)
            IREP  = IPARG(35,NG)
            IGTYP = IPARG(38,NG)
            ITHK  = IPARG(28,NG)
            MPT   = IABS(NPT)
            NPTR  = ELBUF_TAB(NG)%NPTR
            NPTS  = ELBUF_TAB(NG)%NPTS
            NPTT  = ELBUF_TAB(NG)%NPTT
            NLAY  = ELBUF_TAB(NG)%NLAY
            NPG   = NPTR*NPTS
c
            IF (ITY == 3) OFFSET = 0
            IF (ITY == 7) OFFSET = NUMELC
c
            DO  I=1,NEL 
              IF (ITY == 3) THEN
                ID_ELEM(OFFSET+NFT+I) = IXC(NIXC,NFT+I)
                ITY_ELEM(OFFSET+NFT+I) = 3
                IF( H3D_PART(IPARTC(NFT+I)) == 1) IOK_PART(I) = 1
              ELSEIF (ITY == 7) THEN 
                ID_ELEM(OFFSET+NFT+I) = IXTG(NIXTG,NFT+I)
                ITY_ELEM(OFFSET+NFT+I) = 7
                IF( H3D_PART(IPARTTG(NFT+I)) == 1) IOK_PART(I) = 1
              ENDIF
            ENDDO
C
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPT_ALL = 0
              DO IPT=1,NLAY
                NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IPT)%NPTT
              ENDDO
              IF (NLAY == 1) MPT  = MAX(1,NPT_ALL)
            ENDIF
c
            ILAY = LAYER_INPUT
            IPT = IPT_INPUT
            IPLY = PLY_INPUT
c            IG = IGAUSS_INPUT
            IUVAR = IUVAR_INPUT
            IF (ILAY == -2) ILAY = 1
            IF (ILAY == -3) ILAY = NLAY
            IF (IPT == -2) IPT = 1
            IF (IPT == -3) IPT = NPT
            VALUE(1:3) = ZERO
C---------------------    
            DO I=1,NEL
              SHELL_VECTOR(1:3,OFFSET+NFT+I) = ZERO   ! Default = zero in all cases !
            ENDDO
c
C--------------------------------------------------
            IF (KEYWORD == 'VECT/VEL') THEN 
C--------------------------------------------------
               IF (MLW == 151) THEN
                  DO I = 1, NEL
                     VALUE(1) = MULTI_FVM%VEL(1, I + NFT)
                     VALUE(2) = MULTI_FVM%VEL(2, I + NFT)
                     VALUE(3) = MULTI_FVM%VEL(3, I + NFT)
                     CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_SHELL,SHELL_VECTOR,I,OFFSET,NFT,
     .                    VALUE)
                  ENDDO
               ELSE
                  DO I=1,NEL
                     IF(GBUF%G_MOM>0  .AND. ALEFVM_Param%IEnabled > 0)THEN
                        VALUE(1) = GBUF%MOM(JJ(1) + I) / GBUF%RHO(I) 
                        VALUE(2) = GBUF%MOM(JJ(2) + I) / GBUF%RHO(I)
                        VALUE(3) = GBUF%MOM(JJ(3) + I) / GBUF%RHO(I)
                        CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_SHELL,SHELL_VECTOR,I,OFFSET,NFT,
     .                       VALUE)
                     ENDIF
                  ENDDO
               ENDIF
c 
            ENDIF  ! KEYWORD
          ENDIF  ! ITY 
c
C-----------------------------------------------
       ENDIF     ! MLW /= 13  
C-----------------------------------------------
      RETURN
      END

