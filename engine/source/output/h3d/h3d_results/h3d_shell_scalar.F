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
Chd|  H3D_SHELL_SCALAR              source/output/h3d/h3d_results/h3d_shell_scalar.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_SHELL_SCALAR_1            source/output/h3d/h3d_results/h3d_shell_scalar_1.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_SHELL_SCALAR(
     .                  ELBUF_TAB   ,SHELL_SCALAR,IFUNC   ,IPARG       ,GEO        ,
     .                  IXQ         ,IXC       ,IXTG      ,PM          ,BUFMAT     ,
     .                  EL2FA       ,NBF       ,IXUR      ,IADP        ,
     .                  NBF_L       ,EHOUR     ,ANIM      ,NBPART      ,IADG       ,
     .                  IPM         ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                  INVERT      ,X         ,V         ,W           ,ALE_CONNECT,
     .                  NV46        ,NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK       ,ID_ELEM   ,ITY_ELEM  ,INFO1       ,INFO2      , 
     .                  IS_WRITTEN_SHELL,IPARTC,IPARTTG   ,LAYER_INPUT ,IPT_INPUT  ,
     .                  PLY_INPUT   ,GAUSS_INPUT,IUVAR_INPUT,H3D_PART  ,KEYWORD    ,
     .                  D           , MULTI_FVM ,IDMDS    ,IMDSVAR     ,MDS_MATID  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD    
      USE SCHLIEREN_MOD 
      USE STACK_MOD      
      USE MULTI_FVM_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr25_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SHELL_SCALAR(*),X(3,NUMNOD),V(3,NUMNOD),W(3,NUMNOD),D(3,NUMNOD),THKE(*),EHOUR(*),GEO(NPROPG,NUMGEO),
     .   ANIM(*),PM(NPROPM,NUMMAT),ERR_THK_SH4(NUMELC), ERR_THK_SH3(NUMELTG)
      INTEGER IPARG(NPARG,NGROUP),IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),EL2FA(*),
     .   IXQ(NIXQ,NUMELQ),IXUR(NIXUR,NUMELUR),IFUNC,NBF,
     .   IADP(*),NBF_L, NBPART,IADG(NSPMD,NPART),IPM(NPROPMI,NUMMAT),
     .   IGEO(NPROPGI,NUMGEO),INVERT(*), NV46,ID_ELEM(*),ITY_ELEM(*),
     .   INFO1,INFO2,IS_WRITTEN_SHELL(*),IPARTC(NUMELC),IPARTTG(NUMELTG),H3D_PART(*),
     .   LAYER_INPUT ,IPT_INPUT,GAUSS_INPUT,PLY_INPUT,IUVAR_INPUT,IDMDS,
     .   MDS_MATID(*),IMDSVAR,NERCVOIS(SNERCVOIS),NESDVOIS(SNESDVOIS),LERCVOIS(SLERCVOIS),LESDVOIS(SLESDVOIS)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      CHARACTER*ncharline KEYWORD
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      my_real,INTENT(IN) :: BUFMAT(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,NG,OFFSET,SWA_L,LENCOM,IOFF,IALEL,MLW,NEL           
      TYPE(G_BUFEL_)  ,POINTER :: GBUF       
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO II=1,NUMELC+NUMELTG
         IS_WRITTEN_SHELL(II) = 0
      ENDDO
C-----------------------------------------------
      
      !-------------------------------------------------------!
      !     SCHLIEREN INITIALIZATION (IF DEFINED)             !
      !       DENSITY FOR ALL TRIA ARE STORED IN WA_L         !
      !-------------------------------------------------------!    
      ! /TRIA are 2d solid elements (new entity type derived from SH3N, it is currently managed from h3d_shell_* subroutines. It will change in the future.  
      IF(KEYWORD == 'SCHLIEREN' .AND. N2D > 0)THEN
        SWA_L = MAX(IALE,ITHERM,IEULER,IALELAG)*( NUMELTG + NTGVOIS + NSEGFLU )
        IF(.NOT.ALLOCATED(WA_L))ALLOCATE(WA_L(SWA_L)) ! work array for ANIM/ELEM/SCHLIEREN or /H3D/ELEM/SCHLIEREN (density)
        DO NG=1,NGROUP
          CALL INITBUF (IPARG	 ,NG	  ,		       
     2        MLW     ,NEL     ,NFT	,IAD	 ,ITY	  ,  
     3        NPT     ,JALE    ,ISMSTR  ,JEUL	 ,JTUR    ,  
     4        JTHE    ,JLAG    ,JMULT	,JHBE	 ,JIVF    ,  
     5        NVAUX   ,JPOR    ,JCVT	,JCLOSE  ,JPLASOL ,  
     6        IREP    ,IINT    ,IGTYP	,ISRAT   ,ISROT   ,  
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
          IF (MLW /= 13) THEN	  
            DO OFFSET = 0,NEL-1,NVSIZ
              NFT = IPARG(3,NG) + OFFSET
              LFT=1
              LLT=MIN(NVSIZ,NEL-OFFSET)
              IALEL  = IPARG(7,NG) + IPARG(11,NG)
              IF (IALEL==0)CYCLE
              IF (ITY == 7) THEN !TRIA
               !SOLID ELEMENTS
               IF (MLW == 151) THEN
                WA_L(NFT+LFT:NFT+LLT) =  MULTI_FVM%RHO(NFT+LFT:NFT+LLT)
               ELSE
        	GBUF => ELBUF_TAB(NG)%GBUF
        	WA_L(NFT+LFT:NFT+LLT) = GBUF%RHO(LFT:LLT)   
               ENDIF
              ENDIF
            ENDDO
          ENDIF!(MLW /= 13)	     
        ENDDO!next NG
          !--------------------
        CALL MY_BARRIER    
          !--------------------
          !    SPMD EXCHANGE OF ADJACENT ELEMS 
          !--------------------
        IF(IALE+IEULER+ITHERM /= 0)THEN	     
          LENCOM = NERCVOIS(NSPMD+1)+NESDVOIS(NSPMD+1)
          IF (NSPMD>1) THEN
            CALL SPMD_E1VOIS(WA_L     ,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,LENCOM  )
          END IF         
          !In Case of Inter12, additional surface adjacent elems must be taken into account (3D only)
          IOFF = 0  
        ENDIF      
      ENDIF
C-----------------------------------------------


      !-------------------------------------------------------!
      !     LOOP OVER ELEM GROUPS & OUTPUT DATA               !
      !-------------------------------------------------------!
      DO NG=1,NGROUP
       CALL H3D_SHELL_SCALAR_1(
     .                  ELBUF_TAB   ,SHELL_SCALAR,IFUNC   ,IPARG       ,GEO        ,
     .                  IXQ         ,IXC       ,IXTG      ,PM          ,BUFMAT     ,
     .                  EL2FA       ,NBF       ,IXUR      ,IADP        ,
     .                  NBF_L       ,EHOUR     ,ANIM      ,NBPART      ,IADG       ,
     .                  IPM         ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3,
     .                  INVERT      ,X         ,V         ,W           ,ALE_CONNECT      ,
     .                  NV46        ,NERCVOIS  ,NESDVOIS  ,LERCVOIS    ,LESDVOIS,
     .                  STACK       ,ID_ELEM   ,ITY_ELEM  ,INFO1       ,INFO2      , 
     .                  IS_WRITTEN_SHELL,IPARTC,IPARTTG   ,LAYER_INPUT ,IPT_INPUT  ,
     .                  PLY_INPUT   ,GAUSS_INPUT,IUVAR_INPUT,H3D_PART  ,KEYWORD    ,
     .                  D           ,NG         ,MULTI_FVM,IDMDS       ,IMDSVAR    ,
     .                  MDS_MATID   )
      ENDDO
C-----------------------------------------------


      RETURN
      END
