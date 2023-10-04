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
Chd|  SCHLIEREN_BUFFER_GATHERING    source/output/anim/generate/schlieren_buffer_gathering.F
Chd|-- called by -----------
Chd|        DFUNC0                        source/output/anim/generate/dfunc0.F
Chd|        DFUNCC                        source/output/anim/generate/dfuncc.F
Chd|        DFUNCS                        source/output/anim/generate/dfunc6.F
Chd|        H3D_QUAD_SCALAR               source/output/h3d/h3d_results/h3d_quad_scalar.F
Chd|        H3D_SHELL_SCALAR              source/output/h3d/h3d_results/h3d_shell_scalar.F
Chd|        H3D_SOLID_SCALAR              source/output/h3d/h3d_results/h3d_solid_scalar.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_E1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|====================================================================
      SUBROUTINE SCHLIEREN_BUFFER_GATHERING(
     1                     NERCVOIS , NESDVOIS  ,LERCVOIS   ,LESDVOIS ,
     2                     IPARG    , ELBUF_TAB , MULTI_FVM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     This subroutine outputs is gathering density
C     from all SPMDdomains to calculate later 
C     numerical schlieren from schlieren subroutine
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C     ALE/EULER element groups only.
C       this can be checked with IALEL =IPARG(7,NG)+IPARG(11,NG) > 0
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD  
      USE MULTI_FVM_MOD       
      USE SCHLIEREN_MOD       
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: NERCVOIS(SNERCVOIS),NESDVOIS(SNESDVOIS),LERCVOIS(SLERCVOIS),LESDVOIS(SLESDVOIS)
      INTEGER,INTENT(IN) :: IPARG(NPARG,NGROUP)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG,MLW,IALEL,LENCOM,II,NEL,OFFSET,SWA_L
      TYPE(G_BUFEL_)  ,POINTER :: GBUF           
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      
      II=0
      IF(N2D/=0)II=1
      SWA_L = MAX(IALE,ITHERM,IEULER,IALELAG)*( NUMELS + NUMELQ + II*NUMELTG + NSVOIS+ NQVOIS + II*NTGVOIS + NSEGFLU )        
      IF(.NOT.ALLOCATED(WA_L))ALLOCATE(WA_L(SWA_L)) ! work array for /ANIM/ELEM/SCHLIEREN or /H3D/ELEM/SCHLIEREN (DENSITY GATHERING)            
      DO NG=1,NGROUP                                                                                                    
        CALL INITBUF(                                                                                                  
     1          IPARG   ,NG      ,                                                                                      
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,                                                           
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,                                                           
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,                                                           
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,                                                           
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,                                                           
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )                                                           
       IF (MLW /= 13) THEN                                                                                              
        DO OFFSET = 0,NEL-1,NVSIZ                                                                                       
          NFT = IPARG(3,NG) + OFFSET                                                                                    
          LFT=1                                                                                                         
          LLT=MIN(NVSIZ,NEL-OFFSET)                                                                                     
          IALEL = IPARG(7,NG) + IPARG(11,NG)                                                                            
          IF (IALEL==0)CYCLE                                                                                            
          IF (ITY==1 .OR. ITY==2 .OR. (ITY==7 .AND. N2D/=0)) THEN                                                       
             IF (MLW == 151) THEN                                                                                       
              !-- COLLOCATED SCHEME                                                                                     
              WA_L(NFT+LFT:NFT+LLT) =  MULTI_FVM%RHO(NFT+LFT:NFT+LLT)                                                   
             ELSE                                                                                                       
              !-- STAGGERED SCHEME                                                                                      
              GBUF => ELBUF_TAB(NG)%GBUF                                                                                
              WA_L(NFT+LFT:NFT+LLT) = GBUF%RHO(LFT:LLT)                                                                 
             ENDIF                                                                                                      
          ENDIF                                                                                                         
        ENDDO                                                                                                           
       ENDIF!(MLW /= 13)                                                                                                
      ENDDO!next NG                                                                                               
      !--------------------                                                                                             
      !    SPMD EXCHANGE                                                                                                
      !--------------------    
      IF(IALE+IEULER+ITHERM /= 0)THEN
        LENCOM = NERCVOIS(NSPMD+1)+NESDVOIS(NSPMD+1)                                                                      
        IF (NSPMD>1) THEN                                                                                                                                                                                     
          CALL SPMD_E1VOIS( WA_L,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,LENCOM )                                                                                                                          
        END IF                                                                                                            
      ENDIF                                                                                                             

      END SUBROUTINE SCHLIEREN_BUFFER_GATHERING
        
