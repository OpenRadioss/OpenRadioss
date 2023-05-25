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
Chd|  W_ELBUF_STR                   source/elements/elbuf/w_elbuf_str.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_DPDB                    source/output/tools/write_db.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE W_ELBUF_STR(IPARG,ELBUF_TAB,FLAG_XFEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP),FLAG_XFEM
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP)  :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,K,IL,IR,IS,IT,NG,NEL,NPT,L_REL,BUFLEN,NUVAR,
     .   ITY,IGTYP,ISORTH,ISRATE,ISROT,IREP,ERR,IEXPAN,ISTRA,IFAIL,   
     .   NFAIL,IEOS,IXFEM,IVISC,IXEL,NPG,NPTT,NPTTOT,INLOC,NONL,IDFAIL
      INTEGER ! global variables
     .   G_GAMA,G_SIG,G_OFF,G_NOFF,G_EINT,G_EINS,G_TEMP,
     .   G_RHO,G_PLA,G_VOL,G_EPSD,G_QVIS,G_DELTAX,G_TB,G_RK,G_RE,
     .   G_SMSTR,G_HOURG,G_BFRAC,G_THK,G_FOR,G_MOM,G_TAG22,G_STRA,G_SIGI,
     .   G_DMG,G_FORPG,G_MOMPG,G_STRPG,G_UELR,G_UELR1,G_EPSQ,G_IERR,
     .   G_DAMDL,G_FORTH,G_EINTTH,G_FILL,G_SEQ,G_STRW,G_STRWPG,G_THK_I,
     .   G_JAC_I,G_AREA,G_SKEW,G_LENGTH,G_TOTDEPL,G_TOTROT,G_FOREP,G_MOMEP,
     .   G_DEP_IN_TENS,G_DEP_IN_COMP,G_ROT_IN_TENS,G_ROT_IN_COMP,
     .   G_POSX,G_POSY,G_POSZ,G_POSXX,G_POSYY,G_POSZZ,G_YIELD,G_LENGTH_ERR,
     .   G_DV,G_DFS,G_SKEW_ERR,G_E6,G_RUPTCRIT,G_MASS,G_V_REPCVT,
     .   G_VR_REPCVT,G_NUVAR,G_NUVARN,G_INIFRIC,G_DT,G_ISMS,G_STRHG,G_ETOTSH,
     .   G_BPRELD,G_ABURN,G_MU,G_PLANL,G_EPSDNL,G_DT_PITER,G_TEMPG,G_COR_NF,
     .   G_COR_FR,G_COR_XR,G_DEFINI,G_FORINI,G_MAXFRAC,G_MAXEPS,G_IDT_TSH,
     .   G_FORPGPINCH,G_MOMPGPINCH,G_EPGPINCHXZ,G_EPGPINCHYZ,G_EPGPINCHZZ,
     .   G_SKEW_ID,G_GAMA_R,G_SLIPRING_ID,G_SLIPRING_STRAND,G_RETRACTOR_ID,G_RINGSLIP,
     .   G_ADD_NODE,G_UPDATE,G_INTVAR,G_BETAORTH,G_AMU,G_SLIPRING_FRAM_ID,G_FRAM_FACTOR,
     .   G_TM_YIELD,G_TM_SEQ,G_TM_EINT,G_TM_DMG,G_TM_SIG,G_TM_STRA,G_TSAIWU
      INTEGER  ! layer variables
     .   LY_DMG,LY_GAMA,LY_DIRA,LY_DIRB,LY_CRKDIR,LY_PLAPT,LY_SIGPT,LY_SMSTR,
     .   LY_HOURG,LY_UELR,LY_UELR1,LY_OFFPG,LY_OFF,LEN_PLAPT,LEN_SIGPT
      INTEGER  ! interlayer variables
     .   LY_EINT,LY_COUNT
      INTEGER  ! local variables
     .   L_OFF,L_EINT,L_EINS,L_RHO,L_DP_DRHO,L_VOL,L_PLA,L_TB,L_TEMP,
     .   L_RK,L_RE,L_VK,L_ROB,L_SIG,L_SIGV,L_SIGL,L_SIGA,L_SIGD,L_SIGB,
     .   L_SIGC,L_SIGF,L_STRA,L_EPSA,L_EPSD,L_EPSQ,L_EPSF,L_CRAK,
     .   L_ANG,L_SF,L_QVIS,L_DELTAX,L_GAMA,
     .   L_DAM,L_DSUM,L_DGLO,L_EPE,L_EPC,L_XST,L_SSP,L_Z,L_FRAC,L_VISC,
     .   L_THK,L_FOR,L_MOM,L_EPS,L_SMSTR,L_BFRAC,L_DMG,L_FORTH,L_EINTTH,
     .   L_SEQ,L_SIGPLY,L_JAC_I,L_FAC_YLD,L_ABURN,L_MU,L_PLANL,L_EPSDNL,
     .   L_DMGSCL,L_TSAIWU,L_PIJ,L_VOL0DP,L_DPR
      INTEGER  ! failure variables
     .   LF_DAM,LF_DAMMX,LF_DAMINI,LF_TDEL,LF_INDX,LF_OFF
      INTEGER sum
      my_real,
     .   ALLOCATABLE, DIMENSION (:)  :: RBUF_L

      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DP_RBUF
      INTEGER SDP_RBUF

      TYPE(ELBUF_STRUCT_), POINTER :: ELBUF
      TYPE(G_BUFEL_)     , POINTER :: GBUF
      TYPE(L_BUFEL_)     , POINTER :: LBUF
      TYPE(BUF_INTLOC_)  , POINTER :: ILBUF
      TYPE(BUF_LAY_)     , POINTER :: BUFLY
      TYPE(BUF_INTLAY_)  , POINTER :: INTLAY
      TYPE(FAIL_LOC_)    , POINTER :: FLOC
      TYPE(BUF_NLOC_)    , POINTER :: BUFNL
      TYPE(BUF_NLOCTS_)  , POINTER :: BUFNLTS
      TYPE(L_BUFEL_DIR_) , POINTER :: LBUF_DIR
C=======================================================================
      DO NG=1,NGROUP
c----
          ITY    = IPARG(5,NG)
          INLOC  = IPARG(78,NG) ! Non-local regularization flag
          IF (ITY == 1 .OR. ITY == 2 .OR. ITY == 51 .OR.
     .        ITY == 3 .OR. ITY == 7 .OR. ITY == 101.OR.
     .        ITY == 4 .OR. ITY == 5 .OR. ITY == 6 .OR. ITY == 100) THEN
          ELBUF => ELBUF_TAB(NG)
c
          IXFEM = IPARG(54,NG)  ! flag XFEM                                                  
          IF (IXFEM == 0 .AND. FLAG_XFEM == 1) CYCLE ! no "XFEM_TAB" buffer for non xfem PARTS
c
          IGTYP = ELBUF%IGTYP
          IXFEM = ELBUF%IXFEM                                  
          NEL   = ELBUF%NEL
cc          NPT   = IPARG(6,NG)                                                   
C
          NPT  = ELBUF%NLAY*ELBUF%NPTR*ELBUF%NPTS*ELBUF%NPTT
C
c---------------------------------------------------    
          IF (ITY == 1 .OR. ITY == 2 .OR. ITY == 51 .OR.
     .        ITY == 101) THEN
            NPG = ELBUF%NPTR*ELBUF%NPTS*ELBUF%NPTT
            NPTTOT = NPT
          ELSEIF (ITY == 3 .OR. ITY == 7) THEN
            NPG = ELBUF%NPTR*ELBUF%NPTS
            NPTTOT  = 0
            DO IL=1,ELBUF_TAB(NG)%NLAY
              NPTTOT = NPTTOT + NPG*ELBUF_TAB(NG)%BUFLY(IL)%NPTT
            ENDDO
            IF (NPT == 0) NPTTOT = NPT
          ELSEIF (ITY == 4 .OR. ITY == 5 .OR. ITY == 6 .OR. ITY == 100) THEN
            NPG = 0
            NPTTOT = 0
          ENDIF
c---------------------------------------------------          
c         allocate and fill the transfer table for restart
c---------------------------------------------------          
          BUFLEN = IPARG(66,NG)                                         
c          print*,'restart : buflen, NG,igtyp,=',buflen,NG,igtyp,ITY
          ALLOCATE (RBUF_L(BUFLEN), STAT=ERR)

C
          RBUF_L(1:BUFLEN) = ZERO  
          L_REL = 0
c---------  write top structure parameters: NPAR_TAB = 9    
          RBUF_L(L_REL+1) = ELBUF%IGTYP            
                 L_REL = L_REL+1                                      
          RBUF_L(L_REL+1) = ELBUF%NEL          
                 L_REL = L_REL+1                                      
          RBUF_L(L_REL+1) = ELBUF%NLAY          
                 L_REL = L_REL+1                                      
          RBUF_L(L_REL+1) = ELBUF%NINTLAY          
                 L_REL = L_REL+1                                      
          RBUF_L(L_REL+1) = ELBUF%NPTR          
                 L_REL = L_REL+1                                      
          RBUF_L(L_REL+1) = ELBUF%NPTS          
                 L_REL = L_REL+1                                      
          RBUF_L(L_REL+1) = ELBUF%NPTT          
                 L_REL = L_REL+1                                      
          RBUF_L(L_REL+1) = ELBUF%IXFEM          
                 L_REL = L_REL+1                          
          RBUF_L(L_REL+1) = ELBUF%NXEL       
                 L_REL = L_REL+1  
          RBUF_L(L_REL+1) = ELBUF%IDRAPE       
                 L_REL = L_REL+1                                 
c---------------------------------------------------          
c         LAYERS : write buffer at layer level 
c---------------------------------------------------
          sum = L_REL
     
          SDP_RBUF = 0
          DO IL = 1,ELBUF%NLAY                                             
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
c
c---------    write layer parameters: NPAR_LAY = 13
c
              RBUF_L(L_REL+1) = BUFLY%ILAW           
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = BUFLY%IMAT          
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = BUFLY%IEOS           
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = BUFLY%IVISC          
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = BUFLY%IPORO
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = BUFLY%NFAIL           
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = BUFLY%NVAR_MAT           
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = BUFLY%NVARTMP           
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = BUFLY%NVAR_EOS           
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = BUFLY%NVAR_VISC           
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = BUFLY%NVAR_LOC        
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = BUFLY%NVAR_LAY      
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = BUFLY%NPTT      
                   L_REL = L_REL+1
c
c-------      write layer variable lengths (BUFLY%NVAR_LAY = 7
c             
              LY_DMG   = BUFLY%LY_DMG   
              LY_GAMA  = BUFLY%LY_GAMA  
              LY_DIRA  = BUFLY%LY_DIRA   
              LY_DIRB  = BUFLY%LY_DIRB 
              LY_CRKDIR= BUFLY%LY_CRKDIR
              LY_PLAPT = BUFLY%LY_PLAPT 
              LY_SIGPT = BUFLY%LY_SIGPT 
              LY_HOURG = BUFLY%LY_HOURG
              LY_UELR  = BUFLY%LY_UELR
              LY_UELR1 = BUFLY%LY_UELR1
              LY_OFFPG = BUFLY%LY_OFFPG
              LY_OFF   = BUFLY%LY_OFF 
c
              RBUF_L(L_REL+1) = LY_DMG     
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = LY_GAMA     
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = LY_DIRA     
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = LY_DIRB     
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = LY_CRKDIR
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = LY_PLAPT     
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = LY_SIGPT     
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = LY_HOURG
                   L_REL = L_REL+1
              RBUF_L(L_REL+1) = LY_UELR
                   L_REL = L_REL+1                                    
              RBUF_L(L_REL+1) = LY_UELR1
                   L_REL = L_REL+1
              RBUF_L(L_REL+1) = LY_OFFPG
                   L_REL = L_REL+1
              RBUF_L(L_REL+1) = LY_OFF
                   L_REL = L_REL+1
c
c          print*,'    layer var lengths: l_rel=',l_rel
c-------      write local variable lengths (BUFLY%NVAR_LOC = 48)
c
              L_OFF    = BUFLY%L_OFF    
              L_GAMA   = BUFLY%L_GAMA   
              L_STRA   = BUFLY%L_STRA   
              L_FRAC   = BUFLY%L_FRAC
              L_BFRAC  = BUFLY%L_BFRAC 	         
              L_EINT   = BUFLY%L_EINT   
              L_EINS   = BUFLY%L_EINS   
              L_RHO    = BUFLY%L_RHO    
              L_DP_DRHO= BUFLY%L_DP_DRHO
              L_QVIS   = BUFLY%L_QVIS   
              L_DELTAX = BUFLY%L_DELTAX 
              L_VOL    = BUFLY%L_VOL    
              L_EPSA   = BUFLY%L_EPSA   
              L_EPSD   = BUFLY%L_EPSD   
              L_EPSQ   = BUFLY%L_EPSQ   
              L_EPSF   = BUFLY%L_EPSF   
              L_PLA    = BUFLY%L_PLA    
              L_TEMP   = BUFLY%L_TEMP   
              L_TB     = BUFLY%L_TB     
              L_RK     = BUFLY%L_RK     
              L_RE     = BUFLY%L_RE     
              L_VK     = BUFLY%L_VK     
              L_SF     = BUFLY%L_SF     
              L_ROB    = BUFLY%L_ROB    
              L_DAM    = BUFLY%L_DAM    
              L_DSUM   = BUFLY%L_DSUM   
              L_DGLO   = BUFLY%L_DGLO   
              L_CRAK   = BUFLY%L_CRAK   
              L_ANG    = BUFLY%L_ANG    
              L_EPE    = BUFLY%L_EPE    
              L_EPC    = BUFLY%L_EPC    
              L_XST    = BUFLY%L_XST    
              L_SSP    = BUFLY%L_SSP    
              L_Z      = BUFLY%L_Z      
              L_VISC   = BUFLY%L_VISC   
              L_SIGL   = BUFLY%L_SIGL   
              L_SIGV   = BUFLY%L_SIGV   
              L_SIGA   = BUFLY%L_SIGA   
              L_SIGB   = BUFLY%L_SIGB   
              L_SIGC   = BUFLY%L_SIGC   
              L_SIGD   = BUFLY%L_SIGD   
              L_SIGF   = BUFLY%L_SIGF   
              L_SIG    = BUFLY%L_SIG    
              L_SIGPLY = BUFLY%L_SIGPLY    
              L_FOR    = BUFLY%L_FOR    
              L_MOM    = BUFLY%L_MOM
              L_THK    = BUFLY%L_THK    
              L_SMSTR  = BUFLY%L_SMSTR    
              L_DMG    = BUFLY%L_DMG    
              L_FORTH  = BUFLY%L_FORTH    
              L_EINTTH = BUFLY%L_EINTTH    
              L_SEQ    = BUFLY%L_SEQ
              L_JAC_I  = BUFLY%L_JAC_I
              L_FAC_YLD= BUFLY%L_FAC_YLD
              L_ABURN  = BUFLY%L_ABURN
              L_MU     = BUFLY%L_MU
              L_PLANL  = BUFLY%L_PLANL
              L_EPSDNL = BUFLY%L_EPSDNL
              L_DMGSCL = BUFLY%L_DMGSCL
              L_TSAIWU = BUFLY%L_TSAIWU
              L_PIJ    = BUFLY%L_PIJ
              L_VOL0DP = BUFLY%L_VOL0DP
c---
              RBUF_L(L_REL+1) = L_OFF   
                L_REL = L_REL + 1                           
              RBUF_L(L_REL+1) = L_GAMA   
                L_REL = L_REL + 1                           
              RBUF_L(L_REL+1) = L_STRA       
                L_REL = L_REL + 1                          
              RBUF_L(L_REL+1) = L_FRAC   
                L_REL = L_REL + 1                                    
              RBUF_L(L_REL+1) = L_BFRAC       
                L_REL = L_REL + 1 
              RBUF_L(L_REL+1) = L_EINT       
                L_REL = L_REL + 1                        
              RBUF_L(L_REL+1) = L_EINS       
                L_REL = L_REL + 1                          
              RBUF_L(L_REL+1) = L_RHO        
                L_REL = L_REL + 1                       
              RBUF_L(L_REL+1) = L_DP_DRHO    
                L_REL = L_REL + 1                         
              RBUF_L(L_REL+1) = L_QVIS
                L_REL = L_REL + 1                           
              RBUF_L(L_REL+1) = L_DELTAX     
                L_REL = L_REL + 1                          
              RBUF_L(L_REL+1) = L_VOL 
                L_REL = L_REL + 1                           
              RBUF_L(L_REL+1) = L_EPSA       
                L_REL = L_REL + 1                          
              RBUF_L(L_REL+1) = L_EPSD       
                L_REL = L_REL + 1                          
              RBUF_L(L_REL+1) = L_EPSQ       
                L_REL = L_REL + 1                           
              RBUF_L(L_REL+1) = L_EPSF       
                L_REL = L_REL + 1                            
              RBUF_L(L_REL+1) = L_PLA        
                L_REL = L_REL + 1                        
              RBUF_L(L_REL+1) = L_TEMP       
                L_REL = L_REL + 1                            
              RBUF_L(L_REL+1) = L_TB         
                L_REL = L_REL + 1                           
              RBUF_L(L_REL+1) = L_RK         
                L_REL = L_REL + 1                          
              RBUF_L(L_REL+1) = L_RE        
                L_REL = L_REL + 1                          
              RBUF_L(L_REL+1) = L_VK         
                L_REL = L_REL + 1                               
              RBUF_L(L_REL+1) = L_SF         
                L_REL = L_REL + 1                            
              RBUF_L(L_REL+1) = L_ROB        
                L_REL = L_REL + 1                  
              RBUF_L(L_REL+1) = L_DAM        
                L_REL = L_REL + 1                                       
              RBUF_L(L_REL+1) = L_DSUM       
                L_REL = L_REL + 1                             
              RBUF_L(L_REL+1) = L_DGLO       
                L_REL = L_REL + 1                          
              RBUF_L(L_REL+1) = L_CRAK       
                L_REL = L_REL + 1                                    
              RBUF_L(L_REL+1) = L_ANG        
                L_REL = L_REL + 1                              
              RBUF_L(L_REL+1) = L_EPE        
                L_REL = L_REL + 1                                
              RBUF_L(L_REL+1) = L_EPC      
                L_REL = L_REL + 1                                         
              RBUF_L(L_REL+1) = L_XST   
                L_REL = L_REL + 1                            
              RBUF_L(L_REL+1) = L_SSP    
                L_REL = L_REL + 1                             
              RBUF_L(L_REL+1) = L_Z      
                L_REL = L_REL + 1                            
              RBUF_L(L_REL+1) = L_VISC       
                L_REL = L_REL + 1                          
              RBUF_L(L_REL+1) = L_SIGL       
                L_REL = L_REL + 1                           
              RBUF_L(L_REL+1) = L_SIGV       
                L_REL = L_REL + 1                            
              RBUF_L(L_REL+1) = L_SIGA       
                L_REL = L_REL + 1                           
              RBUF_L(L_REL+1) = L_SIGB       
                L_REL = L_REL + 1                         
              RBUF_L(L_REL+1) = L_SIGC       
                L_REL = L_REL + 1                          
              RBUF_L(L_REL+1) = L_SIGD       
                L_REL = L_REL + 1                       
              RBUF_L(L_REL+1) = L_SIGF       
                L_REL = L_REL + 1                         
              RBUF_L(L_REL+1) = L_SIG           
                L_REL = L_REL + 1                           
              RBUF_L(L_REL+1) = L_SIGPLY           
                L_REL = L_REL + 1                           
              RBUF_L(L_REL+1) = L_FOR   
                L_REL = L_REL + 1                           
              RBUF_L(L_REL+1) = L_MOM   
                L_REL = L_REL + 1
              RBUF_L(L_REL+1) = L_THK  
                L_REL = L_REL + 1                            
              RBUF_L(L_REL+1) = L_SMSTR  
                L_REL = L_REL + 1                            
              RBUF_L(L_REL+1) = L_DMG  
                L_REL = L_REL + 1 
              RBUF_L(L_REL+1) = L_FORTH   
                L_REL = L_REL + 1   
              RBUF_L(L_REL+1) = L_EINTTH   
                L_REL = L_REL + 1                              
              RBUF_L(L_REL+1) = L_SEQ
                L_REL = L_REL + 1
              RBUF_L(L_REL+1) = L_JAC_I
                L_REL = L_REL + 1
              RBUF_L(L_REL+1) = L_FAC_YLD
                L_REL = L_REL + 1
              RBUF_L(L_REL+1) = L_ABURN
                L_REL = L_REL + 1
              RBUF_L(L_REL+1) = L_MU
                L_REL = L_REL + 1
              RBUF_L(L_REL+1) = L_PLANL
                L_REL = L_REL + 1
              RBUF_L(L_REL+1) = L_EPSDNL
                L_REL = L_REL + 1
              RBUF_L(L_REL+1) = L_DMGSCL
                L_REL = L_REL + 1
              RBUF_L(L_REL+1) = L_TSAIWU
                L_REL = L_REL + 1                
              RBUF_L(L_REL+1) = L_PIJ
                L_REL = L_REL + 1
              RBUF_L(L_REL+1) = L_VOL0DP
                L_REL = L_REL + 1
c---------
c             ! write layer variables for each element
c
              RBUF_L(L_REL+1:L_REL+NEL*LY_DMG) = BUFLY%DMG(1:NEL*LY_DMG)        
                L_REL = L_REL+NEL*LY_DMG                                     
              RBUF_L(L_REL+1:L_REL+NEL*LY_GAMA) = BUFLY%GAMA(1:NEL*LY_GAMA)        
                L_REL = L_REL+NEL*LY_GAMA                                     
              IF(ELBUF%IDRAPE == 0 .OR.( ELBUF%IDRAPE > 0 .AND. IGTYP == 17)) THEN
                RBUF_L(L_REL+1:L_REL+NEL*LY_DIRA) = BUFLY%DIRA(1:NEL*LY_DIRA)        
                  L_REL = L_REL+NEL*LY_DIRA                                     
                RBUF_L(L_REL+1:L_REL+NEL*LY_DIRB) = BUFLY%DIRB(1:NEL*LY_DIRB)        
                  L_REL = L_REL+NEL*LY_DIRB   
              ENDIF                                     
              RBUF_L(L_REL+1:L_REL+NEL*LY_CRKDIR) = BUFLY%CRKDIR(1:NEL*LY_CRKDIR)        
                L_REL = L_REL+NEL*LY_CRKDIR                                 
c          print*,'    layer a : ,npt,npg,l_rel=',npt,npg,l_rel

            IF (NPG > 1) THEN
              IF (ELBUF%NLAY > 1) THEN
                LEN_PLAPT = NEL*LY_PLAPT
                LEN_SIGPT = NEL*LY_SIGPT
              ELSE
                IF (IGTYP /= 51 .AND. IGTYP /= 52) THEN
                  LEN_PLAPT = NEL*LY_PLAPT*NPT
                  LEN_SIGPT = NEL*LY_SIGPT*NPT
                ELSE
                  LEN_PLAPT = NEL*LY_PLAPT*BUFLY%NPTT
                  LEN_SIGPT = NEL*LY_SIGPT*BUFLY%NPTT
                ENDIF
              ENDIF
              RBUF_L(L_REL+1:L_REL+LEN_PLAPT) = BUFLY%PLAPT(1:LEN_PLAPT)
                L_REL = L_REL+LEN_PLAPT
              RBUF_L(L_REL+1:L_REL+LEN_SIGPT) = BUFLY%SIGPT(1:LEN_SIGPT)
                L_REL = L_REL+LEN_SIGPT
            ELSEIF (NPG == 1) THEN  !  nothing to write
C
C             must point after  => LBUF%PLA and LBUF%SIG
C
            ENDIF
c
            RBUF_L(L_REL+1:L_REL+NEL*LY_HOURG) = BUFLY%HOURG(1:NEL*LY_HOURG)        
              L_REL = L_REL+NEL*LY_HOURG
            RBUF_L(L_REL+1:L_REL+NEL*LY_UELR)  = BUFLY%UELR(1:NEL*LY_UELR)        
              L_REL = L_REL+NEL*LY_UELR
            RBUF_L(L_REL+1:L_REL+NEL*LY_UELR1) = BUFLY%UELR1(1:NEL*LY_UELR1)        
              L_REL = L_REL+NEL*LY_UELR1
            RBUF_L(L_REL+1:L_REL+NEL*LY_OFFPG) = BUFLY%OFFPG(1:NEL*LY_OFFPG)        
              L_REL = L_REL+NEL*LY_OFFPG
            RBUF_L(L_REL+1:L_REL+NEL*LY_OFF)   = BUFLY%OFF(1:NEL*LY_OFF)        
              L_REL = L_REL+NEL*LY_OFF
c
c          print*,'    layer vars : l_rel=',l_rel
c---------
c
c          ! write local layer variables for each element
c
           sum = l_rel

           IF (IGTYP == 51 .OR. IGTYP == 52) THEN
             NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
           ELSE
             NPTT = ELBUF_TAB(NG)%NPTT
           ENDIF
c           
           DO I = 1,ELBUF%NPTR                                               
            DO J = 1,ELBUF%NPTS                                             
c             DO K = 1,ELBUF%NPTT                                            
             DO K = 1,NPTT
              LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(I,J,K)   
c              
              RBUF_L(L_REL+1) = LBUF%MLAW
                L_REL = L_REL + 1                           
              RBUF_L(L_REL+1) = LBUF%LawID
                L_REL = L_REL + 1                                         
c                                    
              RBUF_L(L_REL+1:L_REL+NEL*L_OFF) = LBUF%OFF(1:NEL*L_OFF)        
                L_REL = L_REL+NEL*L_OFF                                      
              RBUF_L(L_REL+1:L_REL+NEL*L_GAMA) =LBUF%GAMA(1:NEL*L_GAMA)       
                L_REL = L_REL+NEL *L_GAMA                               
              RBUF_L(L_REL+1:L_REL+NEL*L_STRA) = LBUF%STRA(1:NEL*L_STRA)     
                L_REL = L_REL+NEL*L_STRA                                         
              RBUF_L(L_REL+1:L_REL+NEL*L_FRAC) = LBUF%FRAC(1:NEL*L_FRAC)
                L_REL = L_REL+NEL*L_FRAC
              RBUF_L(L_REL+1:L_REL+NEL*L_BFRAC)= LBUF%BFRAC(1:NEL*L_BFRAC)        
                L_REL = L_REL+NEL*L_BFRAC  		
              RBUF_L(L_REL+1:L_REL+NEL*L_EINT)= LBUF%EINT(1:NEL*L_EINT)        
                L_REL = L_REL+NEL*L_EINT                                     
              RBUF_L(L_REL+1:L_REL+NEL*L_EINS)= LBUF%EINS(1:NEL*L_EINS)        
                L_REL = L_REL+NEL*L_EINS                                     
              RBUF_L(L_REL+1:L_REL+NEL*L_RHO) = LBUF%RHO(1:NEL*L_RHO)         
                L_REL = L_REL+NEL*L_RHO                                      
              RBUF_L(L_REL+1:L_REL+NEL*L_DP_DRHO)=
     .                             LBUF%DP_DRHO(1:NEL*L_DP_DRHO)             
                L_REL = L_REL+NEL*L_DP_DRHO                                  
              RBUF_L(L_REL+1:L_REL+NEL*L_QVIS)= LBUF%QVIS(1:NEL*L_QVIS)      
                L_REL = L_REL+NEL*L_QVIS                                     
              RBUF_L(L_REL+1:L_REL+NEL*L_DELTAX)=                               
     .                              LBUF%DELTAX(1:NEL*L_DELTAX)                  
                L_REL = L_REL+NEL*L_DELTAX                                   
              RBUF_L(L_REL+1:L_REL+NEL*L_VOL)  = LBUF%VOL(1:NEL*L_VOL)       
                L_REL = L_REL+NEL*L_VOL                                      
              RBUF_L(L_REL+1:L_REL+NEL*L_EPSA) = LBUF%EPSA(1:NEL*L_EPSA)     
                L_REL = L_REL+NEL*L_EPSA                                         
              RBUF_L(L_REL+1:L_REL+NEL*L_EPSD) = LBUF%EPSD(1:NEL*L_EPSD)     
                L_REL = L_REL+NEL*L_EPSD
              RBUF_L(L_REL+1:L_REL+NEL*L_EPSQ) = LBUF%EPSQ(1:NEL*L_EPSQ)     
                L_REL = L_REL+NEL*L_EPSQ
              RBUF_L(L_REL+1:L_REL+NEL*L_EPSF) = LBUF%EPSF(1:NEL*L_EPSF)     
                L_REL = L_REL+NEL*L_EPSF
              RBUF_L(L_REL+1:L_REL+NEL*L_PLA)  = LBUF%PLA(1:NEL*L_PLA)       
                L_REL = L_REL+NEL*L_PLA              
              RBUF_L(L_REL+1:L_REL+NEL*L_TEMP) = LBUF%TEMP(1:NEL*L_TEMP)     
                L_REL = L_REL+NEL*L_TEMP                                        
              RBUF_L(L_REL+1:L_REL+NEL*L_TB)   = LBUF%TB(1:NEL*L_TB)         
                L_REL = L_REL+NEL*L_TB                                           
              RBUF_L(L_REL+1:L_REL+NEL*L_RK)   = LBUF%RK(1:NEL*L_RK)         
                L_REL = L_REL+NEL*L_RK                                           
              RBUF_L(L_REL+1:L_REL+NEL*L_RE)   = LBUF%RE(1:NEL*L_RE)         
                L_REL = L_REL+NEL*L_RE
              RBUF_L(L_REL+1:L_REL+NEL*L_VK)   = LBUF%VK(1:NEL*L_VK)         
                L_REL = L_REL+NEL*L_VK
              RBUF_L(L_REL+1:L_REL+NEL*L_SF)   = LBUF%SF(1:NEL*L_SF)         
                L_REL = L_REL+NEL*L_SF
              RBUF_L(L_REL+1:L_REL+NEL*L_ROB)  = LBUF%ROB(1:NEL*L_ROB)       
                L_REL = L_REL+NEL*L_ROB
              RBUF_L(L_REL+1:L_REL+NEL*L_DAM)  = LBUF%DAM(1:NEL*L_DAM)
                L_REL = L_REL+NEL*L_DAM
              RBUF_L(L_REL+1:L_REL+NEL*L_DSUM) = LBUF%DSUM(1:NEL*L_DSUM)
                L_REL = L_REL+NEL*L_DSUM
              RBUF_L(L_REL+1:L_REL+NEL*L_DGLO) = LBUF%DGLO(1:NEL*L_DGLO)
                L_REL = L_REL+NEL*L_DGLO
              RBUF_L(L_REL+1:L_REL+NEL*L_CRAK) = LBUF%CRAK(1:NEL*L_CRAK)
                L_REL = L_REL+NEL*L_CRAK                                   
              RBUF_L(L_REL+1:L_REL+NEL*L_ANG)  = LBUF%ANG(1:NEL*L_ANG)       
                L_REL = L_REL+NEL*L_ANG
              RBUF_L(L_REL+1:L_REL+NEL*L_EPE)  = LBUF%EPE(1:NEL*L_EPE)
                L_REL = L_REL+NEL*L_EPE
              RBUF_L(L_REL+1:L_REL+NEL*L_EPC)  = LBUF%EPC(1:NEL*L_EPC)
                L_REL = L_REL+NEL*L_EPC
              RBUF_L(L_REL+1:L_REL+NEL*L_XST)  = LBUF%XST(1:NEL*L_XST)
                L_REL = L_REL+NEL*L_XST
              RBUF_L(L_REL+1:L_REL+NEL*L_SSP)  = LBUF%SSP(1:NEL*L_SSP)
                L_REL = L_REL+NEL*L_SSP                                         
              RBUF_L(L_REL+1:L_REL+NEL*L_Z)    = LBUF%Z(1:NEL*L_Z)
                L_REL = L_REL+NEL*L_Z                                          
              RBUF_L(L_REL+1:L_REL+NEL*L_VISC) = LBUF%VISC(1:NEL*L_VISC)
                L_REL = L_REL+NEL*L_VISC
              RBUF_L(L_REL+1:L_REL+NEL*L_SIGL) = LBUF%SIGL(1:NEL*L_SIGL)
                L_REL = L_REL+NEL*L_SIGL
              RBUF_L(L_REL+1:L_REL+NEL*L_SIGV) = LBUF%SIGV(1:NEL*L_SIGV)
                L_REL = L_REL+NEL*L_SIGV
              RBUF_L(L_REL+1:L_REL+NEL*L_SIGA) = LBUF%SIGA(1:NEL*L_SIGA)
                L_REL = L_REL+NEL*L_SIGA
              RBUF_L(L_REL+1:L_REL+NEL*L_SIGB) = LBUF%SIGB(1:NEL*L_SIGB)
                L_REL = L_REL+NEL*L_SIGB
              RBUF_L(L_REL+1:L_REL+NEL*L_SIGC) = LBUF%SIGC(1:NEL*L_SIGC)
                L_REL = L_REL+NEL*L_SIGC
              RBUF_L(L_REL+1:L_REL+NEL*L_SIGD) = LBUF%SIGD(1:NEL*L_SIGD)
                L_REL = L_REL+NEL*L_SIGD                                         
              RBUF_L(L_REL+1:L_REL+NEL*L_SIGF) = LBUF%SIGF(1:NEL*L_SIGF)
                L_REL = L_REL+NEL*L_SIGF
              RBUF_L(L_REL+1:L_REL+NEL*L_SIG) = LBUF%SIG(1:NEL*L_SIG)       
                L_REL = L_REL+NEL*L_SIG                                     
              RBUF_L(L_REL+1:L_REL+NEL*L_SIGPLY) = LBUF%SIGPLY(1:NEL*L_SIGPLY)       
                L_REL = L_REL+NEL*L_SIGPLY                                     
              RBUF_L(L_REL+1:L_REL+NEL*L_FOR) = LBUF%FOR(1:NEL*L_FOR)
                L_REL = L_REL+NEL*L_FOR
              RBUF_L(L_REL+1:L_REL+NEL*L_MOM) = LBUF%MOM(1:NEL*L_MOM)
                L_REL = L_REL+NEL*L_MOM
              RBUF_L(L_REL+1:L_REL+NEL*L_THK) = LBUF%THK(1:NEL*L_THK)
                L_REL = L_REL+NEL*L_THK
              RBUF_L(L_REL+1:L_REL+NEL*L_SMSTR) = LBUF%SMSTR(1:NEL*L_SMSTR)
                L_REL = L_REL+NEL*L_SMSTR
              RBUF_L(L_REL+1:L_REL+NEL*L_DMG) = LBUF%DMG(1:NEL*L_DMG)
                L_REL = L_REL+NEL*L_DMG
              RBUF_L(L_REL+1:L_REL+NEL*L_FORTH) = LBUF%FORTH(1:NEL*L_FORTH)
                L_REL = L_REL+NEL*L_FORTH
              RBUF_L(L_REL+1:L_REL+NEL*L_EINTTH) = LBUF%EINTTH(1:NEL*L_EINTTH)
                L_REL = L_REL+NEL*L_EINTTH
              RBUF_L(L_REL+1:L_REL+NEL*L_SEQ) = LBUF%SEQ(1:NEL*L_SEQ)
                L_REL = L_REL+NEL*L_SEQ
              RBUF_L(L_REL+1:L_REL+NEL*L_JAC_I) = LBUF%JAC_I(1:NEL*L_JAC_I)
                L_REL = L_REL+NEL*L_JAC_I
              RBUF_L(L_REL+1:L_REL+NEL*L_FAC_YLD) = LBUF%FAC_YLD(1:NEL*L_FAC_YLD)
                L_REL = L_REL+NEL*L_FAC_YLD
              RBUF_L(L_REL+1:L_REL+NEL*L_ABURN) = LBUF%ABURN(1:NEL*L_ABURN)
                L_REL = L_REL+NEL*L_ABURN
              RBUF_L(L_REL+1:L_REL+NEL*L_MU) = LBUF%MU(1:NEL*L_MU)
                L_REL = L_REL+NEL*L_MU
              RBUF_L(L_REL+1:L_REL+NEL*L_PLANL) = LBUF%PLANL(1:NEL*L_PLANL)
                L_REL = L_REL+NEL*L_PLANL
              RBUF_L(L_REL+1:L_REL+NEL*L_EPSDNL) = LBUF%EPSDNL(1:NEL*L_EPSDNL)
                L_REL = L_REL+NEL*L_EPSDNL
              RBUF_L(L_REL+1:L_REL+NEL*L_DMGSCL) = LBUF%DMGSCL(1:NEL*L_DMGSCL)
                L_REL = L_REL+NEL*L_DMGSCL
              RBUF_L(L_REL+1:L_REL+NEL*L_TSAIWU) = LBUF%TSAIWU(1:NEL*L_TSAIWU)
                L_REL = L_REL+NEL*L_TSAIWU
              RBUF_L(L_REL+1:L_REL+NEL*L_PIJ) = LBUF%PIJ(1:NEL*L_PIJ)
                L_REL = L_REL+NEL*L_PIJ
C------ DP of VOL0DP                
                SDP_RBUF = SDP_RBUF+NEL*L_VOL0DP
             ENDDO                                                          
            ENDDO                                                           
           ENDDO  
            IF(ELBUF%IDRAPE > 0 .AND.( IGTYP == 51  .OR. IGTYP == 52)) THEN
              DO K = 1,NPTT
                 LBUF_DIR => ELBUF_TAB(NG)%BUFLY(IL)%LBUF_DIR(K)
                 RBUF_L(L_REL+1:L_REL+NEL*LY_DIRA) = LBUF_DIR%DIRA(1:NEL*LY_DIRA)        
                 L_REL = L_REL + NEL*LY_DIRA                                     
                 RBUF_L(L_REL+1:L_REL+NEL*LY_DIRB) = LBUF_DIR%DIRB(1:NEL*LY_DIRB)        
                 L_REL = L_REL + NEL*LY_DIRB
              ENDDO
            ENDIF                                                           
          ENDDO   ! NALY                                                           
c          print*,'    local=',l_rel
c          print*,'    local=',l_rel-sum
c
          !-----------------------------------------------------                                                                 
          ! Layer Material buffer - UVAR                                                             
          DO IL = 1,ELBUF%NLAY                                          
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)              
            NUVAR  = BUFLY%NVAR_MAT                                     
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
            ELSE
              NPTT = ELBUF_TAB(NG)%NPTT
            ENDIF 
            DO IR = 1,ELBUF%NPTR                                             
              DO IS = 1,ELBUF%NPTS                                           
                DO IT = 1,NPTT
                  RBUF_L(L_REL+1:L_REL+NEL*NUVAR)= BUFLY%MAT(IR,IS,IT)%VAR(1:NEL*NUVAR)
                  L_REL = L_REL+NEL*NUVAR
                ENDDO                                                       
              ENDDO                                                         
            ENDDO                                                           
          ENDDO                                                       
c          print*,'    mat =',l_rel

c         !-----------------------------------------------------                                                                 
          ! Layer Fail buffer - UVAR 
          DO IL = 1,ELBUF%NLAY                                          
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)             
            NFAIL = BUFLY%NFAIL  
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
            ELSE
              NPTT = ELBUF_TAB(NG)%NPTT
            ENDIF
            DO IR = 1,ELBUF%NPTR                                             
              DO IS = 1,ELBUF%NPTS                                           
                DO IT = 1,NPTT
                  DO K = 1,NFAIL   
                    FLOC=>ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(K)
                    IFAIL = FLOC%ILAWF
                    IDFAIL = FLOC%IDFAIL
                    NUVAR = FLOC%NVAR
                    LF_DAM  = FLOC%LF_DAM
                    LF_DAMMX= FLOC%LF_DAMMX
                    LF_DAMINI = FLOC%LF_DAMINI
                    LF_TDEL = FLOC%LF_TDEL
                    LF_INDX = FLOC%LF_INDX
                    LF_OFF  = FLOC%LF_OFF
c
                    RBUF_L(L_REL+1)=IFAIL
                      L_REL = L_REL+1
                    RBUF_L(L_REL+1)=IDFAIL
                      L_REL = L_REL+1
                    RBUF_L(L_REL+1)=NUVAR
                      L_REL = L_REL+1
                    RBUF_L(L_REL+1)=LF_DAM
                      L_REL = L_REL+1
                    RBUF_L(L_REL+1)=LF_DAMMX
                      L_REL = L_REL+1
                    RBUF_L(L_REL+1)=LF_DAMINI
                      L_REL = L_REL+1
                    RBUF_L(L_REL+1)=LF_TDEL
                      L_REL = L_REL+1
                    RBUF_L(L_REL+1)=LF_INDX
                      L_REL = L_REL+1
                    RBUF_L(L_REL+1)=LF_OFF
                      L_REL = L_REL+1
c
                    RBUF_L(L_REL+1:L_REL+NEL*NUVAR) = FLOC%VAR(1:NEL*NUVAR)
                      L_REL = L_REL + NEL*NUVAR
                    RBUF_L(L_REL+1:L_REL+NEL*LF_DAM) = FLOC%DAM(1:NEL*LF_DAM)
                      L_REL = L_REL + NEL*LF_DAM
                    RBUF_L(L_REL+1:L_REL+NEL*LF_DAMMX) = FLOC%DAMMX(1:NEL*LF_DAMMX)
                      L_REL = L_REL+NEL*LF_DAMMX
                    RBUF_L(L_REL+1:L_REL+NEL*LF_DAMINI) = FLOC%DAMINI(1:NEL*LF_DAMINI)
                      L_REL = L_REL+NEL*LF_DAMINI
                    RBUF_L(L_REL+1:L_REL+NEL*LF_TDEL) = FLOC%TDEL(1:NEL*LF_TDEL)
                      L_REL = L_REL + NEL*LF_TDEL
                    RBUF_L(L_REL+1:L_REL+NEL*LF_INDX) = FLOC%INDX(1:NEL*LF_INDX)
                      L_REL = L_REL + NEL*LF_INDX
                    RBUF_L(L_REL+1:L_REL+NEL*LF_OFF) = FLOC%OFF(1:NEL*LF_OFF)
                      L_REL = L_REL + NEL*LF_OFF
                ENDDO                                                       
              ENDDO                                                       
             ENDDO                                                         
            ENDDO                                                           
          ENDDO                                                       

          !-----------------------------------------------------                                                                 
          ! Layer Visc buffer - UVAR                                                             
          DO IL = 1,ELBUF%NLAY                                          
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)              
            NUVAR  = BUFLY%NVAR_VISC                                    
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
            ELSE
              NPTT = ELBUF_TAB(NG)%NPTT
            ENDIF
            DO IR = 1,ELBUF%NPTR                                             
              DO IS = 1,ELBUF%NPTS                                           
                DO IT = 1,NPTT
                  RBUF_L(L_REL+1:L_REL+NEL*NUVAR)=  BUFLY%VISC(IR,IS,IT)%VAR(1:NEL*NUVAR)
                  L_REL = L_REL+NEL*NUVAR
                ENDDO                                                       
              ENDDO                                                         
            ENDDO                                                           
          ENDDO    

          !-----------------------------------------------------                                                                 
          ! Non-local regularization in the shell thickness             
          IF ((ITY==3).OR.(ITY==7)) THEN                         
            IF ((INLOC>0).AND.(ELBUF%NPTT>1)) THEN               
              ! Number of non-local nodes in the thickness       
              IF (ELBUF%NPTT>2) THEN                             
                NONL = ELBUF%NPTT+1                              
              ELSE                                               
                NONL = ELBUF%NPTT                                
              ENDIF                                              
              ! Copying the buffer                               
              DO IR = 1,ELBUF%NPTR                                              
                DO IS = 1,ELBUF%NPTS                             
                  ! Pointing the non-local buffer                
                  BUFNL => ELBUF_TAB(NG)%NLOC(IR,IS)             
                  DO I = 1,NEL                                   
                    ! Nodal masses                               
                    RBUF_L(L_REL+1:L_REL+NONL) = BUFNL%MASSTH(I,1:NONL)                    
                    L_REL = L_REL + NONL                         
                    ! Non-local in-thickness cumulated variables 
                    RBUF_L(L_REL+1:L_REL+NONL) = BUFNL%UNLTH(I,1:NONL)                     
                    L_REL = L_REL + NONL                         
                    ! Non-local in-thickness velocities          
                    RBUF_L(L_REL+1:L_REL+NONL) = BUFNL%VNLTH(I,1:NONL)                     
                    L_REL = L_REL + NONL                         
                    ! Non-local in-thickness forces              
                    RBUF_L(L_REL+1:L_REL+NONL) = BUFNL%FNLTH(I,1:NONL)                     
                    L_REL = L_REL + NONL                         
                  ENDDO                                          
                ENDDO                                            
              ENDDO                                              
            ENDIF   
          !-----------------------------------------------------                                                                 
          ! Non-local regularization in the thickshell thickness             
          ELSEIF (ITY==1) THEN                         
            IF ((INLOC>0).AND.(ELBUF%NLAY>1)) THEN               
              ! Number of non-local nodes in the thickness                            
              NONL = ELBUF%NLAY+1                                                                           
              ! Copying the buffer                               
              DO IR = 1,ELBUF%NPTR                                              
                DO IS = 1,ELBUF%NPTS                             
                  ! Pointing the non-local buffer                
                  BUFNLTS => ELBUF_TAB(NG)%NLOCTS(IR,IS)             
                  DO I = 1,NEL                                   
                    ! Nodal masses                               
                    RBUF_L(L_REL+1:L_REL+NONL) = BUFNLTS%MASSTH(I,1:NONL)                    
                    L_REL = L_REL + NONL                         
                    ! Non-local in-thickness cumulated variables 
                    RBUF_L(L_REL+1:L_REL+NONL) = BUFNLTS%UNLTH(I,1:NONL)                     
                    L_REL = L_REL + NONL                         
                    ! Non-local in-thickness velocities          
                    RBUF_L(L_REL+1:L_REL+NONL) = BUFNLTS%VNLTH(I,1:NONL)                     
                    L_REL = L_REL + NONL                         
                    ! Non-local in-thickness forces              
                    RBUF_L(L_REL+1:L_REL+NONL) = BUFNLTS%FNLTH(I,1:NONL)                     
                    L_REL = L_REL + NONL                         
                  ENDDO                                          
                ENDDO                                            
              ENDDO                                              
            ENDIF               
          ENDIF                                                  
           
          !-----------------------------------------------------                                                                 
          ! EoS         
          DO IL = 1,ELBUF%NLAY                                          
            NUVAR  = BUFLY%NVAR_EOS
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)                                                 
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
            ELSE
              NPTT = ELBUF_TAB(NG)%NPTT
            ENDIF
            DO IR = 1,ELBUF%NPTR                                             
              DO IS = 1,ELBUF%NPTS                                           
                DO IT = 1,NPTT
                  RBUF_L(L_REL+1:L_REL+NEL*NUVAR)= BUFLY%EOS(IR,IS,IT)%VAR(1:NEL*NUVAR)
                  L_REL = L_REL+NEL*NUVAR
                ENDDO                                                       
              ENDDO                                                         
            ENDDO                                                           
          ENDDO              
          !-----------------------------------------------------                                                                 
c          
          sum = l_rel                                                      

c          print*,'    before interlayers=',l_rel
c
c---------------------------------------------------          
c         INTER LAYERS (Ply Xfem only)
c---------------------------------------------------          
          L_EPS  = 3  
          L_SIG  = 3  
          LY_EINT = 1  
          LY_COUNT= 1  
c
          DO IL = 1,ELBUF%NINTLAY                                             
            INTLAY => ELBUF_TAB(NG)%INTLAY(IL)
c         
c---------  write inter layer parameters: = 4
c
            RBUF_L(L_REL+1) = INTLAY%ILAW             
                 L_REL = L_REL+1                                      
            RBUF_L(L_REL+1) = INTLAY%IMAT            
                 L_REL = L_REL+1                                      
            RBUF_L(L_REL+1) = INTLAY%NFAIL             
                 L_REL = L_REL+1                                      
            RBUF_L(L_REL+1) = INTLAY%NVAR_MAT             
                 L_REL = L_REL+1                                      
            RBUF_L(L_REL+1) = INTLAY%NVARTMP             
                 L_REL = L_REL+1                                      
c
c          ! write inter layer variables for each layer
           RBUF_L(L_REL+1:L_REL+NEL*LY_EINT)  = INTLAY%EINT(1:NEL*LY_EINT)          
             L_REL = L_REL+NEL*LY_EINT                                       
           RBUF_L(L_REL+1:L_REL+NEL*LY_COUNT) = INTLAY%COUNT(1:NEL*LY_COUNT)          
             L_REL = L_REL+NEL*LY_COUNT                                       

c          ! write local inter layer variables for each element
            DO I = 1,ELBUF%NPTR                                              
              DO J = 1,ELBUF%NPTS                                           
               ILBUF => ELBUF_TAB(NG)%INTLAY(IL)%ILBUF(I,J)   
c                                     
               RBUF_L(L_REL+1:L_REL+NEL*L_EPS) =ILBUF%EPS(1:NEL*L_EPS)      
                 L_REL = L_REL+NEL*L_EPS                                   
               RBUF_L(L_REL+1:L_REL+NEL*L_SIG) =ILBUF%SIG(1:NEL*L_SIG)      
                 L_REL = L_REL+NEL*L_SIG                                   
              ENDDO                                                         
            ENDDO                                                            
          ENDDO                                                              
c
c-----    Inter Layer Material buffer - UVAR                                                             
c
          DO IL = 1,ELBUF%NINTLAY                                          
            INTLAY => ELBUF_TAB(NG)%INTLAY(IL)              
            NUVAR  = INTLAY%NVAR_MAT                                     
            DO IR = 1,ELBUF%NPTR                                             
              DO IS = 1,ELBUF%NPTS                                           
                  RBUF_L(L_REL+1:L_REL+NEL*NUVAR)=
     .                   INTLAY%MAT(IR,IS)%VAR(1:NEL*NUVAR)
                  L_REL = L_REL+NEL*NUVAR
              ENDDO                                                         
            ENDDO                                                           
          ENDDO                                                       
c
c-----    Inter Layer Fail buffer - UVAR                                                             
c
          DO IL = 1,ELBUF%NINTLAY                                          
            INTLAY => ELBUF_TAB(NG)%INTLAY(IL)             
            NFAIL = INTLAY%NFAIL  
            DO IR = 1,ELBUF%NPTR                                             
              DO IS = 1,ELBUF%NPTS                                           
                DO K = 1,NFAIL                                           
                  FLOC=>ELBUF_TAB(NG)%INTLAY(IL)%FAIL(IR,IS)%FLOC(K)  
                  IFAIL = FLOC%ILAWF    
                  IDFAIL = FLOC%IDFAIL                                 
                  NUVAR = FLOC%NVAR                                      
                  RBUF_L(L_REL+1)=IFAIL                                  
                    L_REL = L_REL+1 
                  RBUF_L(L_REL+1)=IDFAIL                                 
                    L_REL = L_REL+1                     
                  RBUF_L(L_REL+1)=NUVAR                                  
                    L_REL = L_REL+1                                      
                  RBUF_L(L_REL+1:L_REL+NEL*NUVAR)=                       
     .                      FLOC%VAR(1:NEL*NUVAR)                        
                    L_REL = L_REL+NEL*NUVAR                              
                ENDDO                                                       
             ENDDO                                                         
            ENDDO                                                           
          ENDDO                                                       
c          print*,'    start global=',l_rel
c         
c-----------------------------------------------------------------------
c         GLOBAL BUFFER     
c-----------------------------------------------------------------------
          GBUF => ELBUF_TAB(NG)%GBUF
c
          G_NOFF  = GBUF%G_NOFF  
          G_IERR  = GBUF%G_IERR  
          G_OFF   = GBUF%G_OFF   
          G_GAMA  = GBUF%G_GAMA  
          G_SMSTR = GBUF%G_SMSTR 
          G_HOURG = GBUF%G_HOURG 
          G_BFRAC = GBUF%G_BFRAC 
          G_EINT  = GBUF%G_EINT  
          G_EINS  = GBUF%G_EINS  
          G_RHO   = GBUF%G_RHO   
          G_QVIS  = GBUF%G_QVIS  
          G_DELTAX= GBUF%G_DELTAX
          G_VOL   = GBUF%G_VOL   
          G_EPSD  = GBUF%G_EPSD  
          G_EPSQ  = GBUF%G_EPSQ
          G_PLA   = GBUF%G_PLA   
          G_TEMP  = GBUF%G_TEMP  
          G_TB    = GBUF%G_TB    
          G_RK    = GBUF%G_RK    
          G_RE    = GBUF%G_RE    
          G_SIG   = GBUF%G_SIG   
          G_FOR   = GBUF%G_FOR   
          G_MOM   = GBUF%G_MOM   
          G_THK   = GBUF%G_THK   
          G_TAG22 = GBUF%G_TAG22 
          G_STRA  = GBUF%G_STRA 
          G_SIGI  = GBUF%G_SIGI 
          G_DMG   = GBUF%G_DMG
          G_FORPG = GBUF%G_FORPG
          G_MOMPG = GBUF%G_MOMPG
          G_GAMA_R= GBUF%G_GAMA_R  
C
          G_FORPGPINCH = GBUF%G_FORPGPINCH
          G_MOMPGPINCH = GBUF%G_MOMPGPINCH
          G_EPGPINCHXZ = GBUF%G_EPGPINCHXZ
          G_EPGPINCHYZ = GBUF%G_EPGPINCHYZ
          G_EPGPINCHZZ = GBUF%G_EPGPINCHZZ
C
          G_STRPG = GBUF%G_STRPG
          G_UELR  = GBUF%G_UELR
          G_UELR1 = GBUF%G_UELR1
          G_DAMDL = GBUF%G_DAMDL
          G_FORTH = GBUF%G_FORTH   
          G_EINTTH= GBUF%G_EINTTH   
          G_FILL  = GBUF%G_FILL
          G_SEQ   = GBUF%G_SEQ
          G_STRW  = GBUF%G_STRW 
          G_STRWPG= GBUF%G_STRWPG 
          G_THK_I = GBUF%G_THK_I 
          G_JAC_I = GBUF%G_JAC_I
          G_DT    = GBUF%G_DT
          G_ISMS  = GBUF%G_ISMS 
          G_BPRELD= GBUF%G_BPRELD
          G_ABURN = GBUF%G_ABURN
          G_MU    = GBUF%G_MU
          G_PLANL = GBUF%G_PLANL
          G_EPSDNL = GBUF%G_EPSDNL
          G_TSAIWU = GBUF%G_TSAIWU
          G_TEMPG  = GBUF%G_TEMPG
          G_COR_NF = GBUF%G_COR_NF
          G_COR_FR = GBUF%G_COR_FR
          G_COR_XR = GBUF%G_COR_XR
          G_MAXFRAC= GBUF%G_MAXFRAC
          G_MAXEPS = GBUF%G_MAXEPS
          G_BETAORTH  = GBUF%G_BETAORTH
          G_AMU    = GBUF%G_AMU
          
c---
c 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
c---
          G_AREA        = GBUF%G_AREA
          G_SKEW        = GBUF%G_SKEW
          G_LENGTH      = GBUF%G_LENGTH
          G_TOTDEPL     = GBUF%G_TOTDEPL
          G_TOTROT      = GBUF%G_TOTROT
          G_FOREP       = GBUF%G_FOREP
          G_MOMEP       = GBUF%G_MOMEP
          G_DEP_IN_TENS = GBUF%G_DEP_IN_TENS
          G_DEP_IN_COMP = GBUF%G_DEP_IN_COMP
          G_ROT_IN_TENS = GBUF%G_ROT_IN_TENS
          G_ROT_IN_COMP = GBUF%G_ROT_IN_COMP
          G_POSX        = GBUF%G_POSX
          G_POSY        = GBUF%G_POSY
          G_POSZ        = GBUF%G_POSZ
          G_POSXX       = GBUF%G_POSXX
          G_POSYY       = GBUF%G_POSYY
          G_POSZZ       = GBUF%G_POSZZ
          G_YIELD       = GBUF%G_YIELD
          G_LENGTH_ERR  = GBUF%G_LENGTH_ERR
          G_DV          = GBUF%G_DV
          G_DFS         = GBUF%G_DFS
          G_SKEW_ERR    = GBUF%G_SKEW_ERR
          G_E6          = GBUF%G_E6
          G_RUPTCRIT    = GBUF%G_RUPTCRIT
          G_MASS        = GBUF%G_MASS
          G_V_REPCVT    = GBUF%G_V_REPCVT
          G_VR_REPCVT   = GBUF%G_VR_REPCVT
          G_NUVAR       = GBUF%G_NUVAR
          G_NUVARN      = GBUF%G_NUVARN
          G_DEFINI      = GBUF%G_DEFINI
          G_FORINI      = GBUF%G_FORINI
          G_INIFRIC     = GBUF%G_INIFRIC
          G_STRHG       = GBUF%G_STRHG
          G_ETOTSH      = GBUF%G_ETOTSH
          G_DT_PITER    = GBUF%G_DT_PITER
          G_SKEW_ID     = GBUF%G_SKEW_ID
c---
c seatbelt elements
c---
          G_SLIPRING_ID = GBUF%G_SLIPRING_ID
          G_SLIPRING_FRAM_ID = GBUF%G_SLIPRING_FRAM_ID
          G_SLIPRING_STRAND = GBUF%G_SLIPRING_STRAND
          G_RETRACTOR_ID= GBUF%G_RETRACTOR_ID
          G_RINGSLIP    = GBUF%G_RINGSLIP
          G_ADD_NODE    = GBUF%G_ADD_NODE
          G_UPDATE      = GBUF%G_UPDATE
          G_FRAM_FACTOR = GBUF%G_FRAM_FACTOR
          G_INTVAR      = GBUF%G_INTVAR
          G_IDT_TSH     = GBUF%G_IDT_TSH
C ---H3D/TMAX in BUFLEN         
          G_TM_YIELD = GBUF%G_TM_YIELD
          G_TM_SEQ   = GBUF%G_TM_SEQ  
          G_TM_EINT  = GBUF%G_TM_EINT 
          G_TM_DMG   = GBUF%G_TM_DMG  
          G_TM_SIG   = GBUF%G_TM_SIG  
          G_TM_STRA  = GBUF%G_TM_STRA 
c
          sum = l_rel
c
          RBUF_L(L_REL+1) =  GBUF%NVAR_GLOB      
            L_REL = L_REL + 1                                
c
c         ! write global variable lengths (GBUF%NVAR_GLOB = 34)
c
           RBUF_L(L_REL+1) =  G_NOFF        
             L_REL = L_REL + 1                               
           RBUF_L(L_REL+1) =  G_IERR        
             L_REL = L_REL + 1                               
           RBUF_L(L_REL+1) =  G_OFF
             L_REL = L_REL + 1                                  
           RBUF_L(L_REL+1) =  G_GAMA       
             L_REL = L_REL + 1                                
           RBUF_L(L_REL+1) =  G_SMSTR        
             L_REL = L_REL + 1                                 
           RBUF_L(L_REL+1) =  G_HOURG          
             L_REL = L_REL + 1                                
           RBUF_L(L_REL+1) =  G_BFRAC        
             L_REL = L_REL + 1                                  
           RBUF_L(L_REL+1) =  G_EINT        
             L_REL = L_REL + 1                               
           RBUF_L(L_REL+1) =  G_EINS        
             L_REL = L_REL + 1                                  
           RBUF_L(L_REL+1) =  G_RHO         
             L_REL = L_REL + 1                                
           RBUF_L(L_REL+1) =  G_QVIS      
             L_REL = L_REL + 1                            
           RBUF_L(L_REL+1) =  G_DELTAX      
             L_REL = L_REL + 1                              
           RBUF_L(L_REL+1) =  G_VOL        
             L_REL = L_REL + 1                                 
           RBUF_L(L_REL+1) =  G_EPSD        
             L_REL = L_REL + 1                                
           RBUF_L(L_REL+1) =  G_EPSQ
             L_REL = L_REL + 1                                  
           RBUF_L(L_REL+1) =  G_PLA         
             L_REL = L_REL + 1                               
           RBUF_L(L_REL+1) =  G_TEMP        
             L_REL = L_REL + 1                             
           RBUF_L(L_REL+1) =  G_TB          
             L_REL = L_REL + 1                             
           RBUF_L(L_REL+1) =  G_RK          
             L_REL = L_REL + 1                                
           RBUF_L(L_REL+1) =  G_RE            
             L_REL = L_REL + 1                               
           RBUF_L(L_REL+1) =  G_SIG         
             L_REL = L_REL + 1                                
           RBUF_L(L_REL+1) =  G_FOR           
             L_REL = L_REL + 1                               
           RBUF_L(L_REL+1) =  G_MOM        
             L_REL = L_REL + 1                               
           RBUF_L(L_REL+1) =  G_THK        
             L_REL = L_REL + 1                               
           RBUF_L(L_REL+1) =  G_TAG22      
             L_REL = L_REL + 1                               
           RBUF_L(L_REL+1) =  G_STRA      
             L_REL = L_REL + 1                              
           RBUF_L(L_REL+1) =  G_SIGI      
             L_REL = L_REL + 1                                    
           RBUF_L(L_REL+1) =  G_DMG      
             L_REL = L_REL + 1                                
           RBUF_L(L_REL+1) =  G_FORPG      
             L_REL = L_REL + 1                                  
           RBUF_L(L_REL+1) =  G_MOMPG      
             L_REL = L_REL + 1
C
           RBUF_L(L_REL+1) =  G_FORPGPINCH      
             L_REL = L_REL + 1                                  
           RBUF_L(L_REL+1) =  G_MOMPGPINCH      
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_EPGPINCHXZ      
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_EPGPINCHYZ      
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_EPGPINCHZZ      
             L_REL = L_REL + 1
C                                    
           RBUF_L(L_REL+1) =  G_STRPG      
             L_REL = L_REL + 1                                 
           RBUF_L(L_REL+1) =  G_UELR
             L_REL = L_REL + 1                                
           RBUF_L(L_REL+1) =  G_UELR1
             L_REL = L_REL + 1                                
           RBUF_L(L_REL+1) =  G_DAMDL
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_FORTH           
             L_REL = L_REL + 1 
           RBUF_L(L_REL+1) =  G_EINTTH           
             L_REL = L_REL + 1                                 
           RBUF_L(L_REL+1) =  G_FILL       
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_SEQ
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_STRW
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_STRWPG
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_THK_I
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_JAC_I
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_DT
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_ISMS
             L_REL = L_REL + 1              
           RBUF_L(L_REL+1) =  G_BPRELD
             L_REL = L_REL + 1  
           RBUF_L(L_REL+1) =  G_ABURN       
             L_REL = L_REL + 1                                  
           RBUF_L(L_REL+1) =  G_MU       
             L_REL = L_REL + 1    
           RBUF_L(L_REL+1) =  G_PLANL    
             L_REL = L_REL + 1    
           RBUF_L(L_REL+1) =  G_EPSDNL  
             L_REL = L_REL + 1    
           RBUF_L(L_REL+1) =  G_TSAIWU
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_TEMPG      
             L_REL = L_REL + 1                                     
           RBUF_L(L_REL+1) =  G_COR_NF       
             L_REL = L_REL + 1                                  
           RBUF_L(L_REL+1) =  G_COR_FR     
             L_REL = L_REL + 1                                  
           RBUF_L(L_REL+1) =  G_COR_XR     
             L_REL = L_REL + 1  
            RBUF_L(L_REL+1) =  G_MAXFRAC     
             L_REL = L_REL + 1                                
           RBUF_L(L_REL+1) =  G_MAXEPS     
             L_REL = L_REL + 1 
           RBUF_L(L_REL+1) =  G_BETAORTH
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_AMU    
             L_REL = L_REL + 1 
c---
c 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
c---
           RBUF_L(L_REL+1) =  G_AREA
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_SKEW
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_LENGTH
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_TOTDEPL
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_TOTROT
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_FOREP
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_MOMEP
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_DEP_IN_TENS
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_DEP_IN_COMP
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_ROT_IN_TENS
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_ROT_IN_COMP
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_POSX
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_POSY
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_POSZ
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_POSXX
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_POSYY
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_POSZZ
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_YIELD
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_LENGTH_ERR
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_DV
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_DFS
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_SKEW_ERR
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_E6
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_RUPTCRIT
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_MASS
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_V_REPCVT
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_VR_REPCVT
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_NUVAR
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_NUVARN
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_DEFINI
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_FORINI
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_INIFRIC
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_STRHG
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_ETOTSH
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_DT_PITER
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_SKEW_ID
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_GAMA_R      
             L_REL = L_REL + 1
c---
c seatbelt elements
c---
           RBUF_L(L_REL+1) =  G_SLIPRING_ID
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_SLIPRING_FRAM_ID
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_SLIPRING_STRAND
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_RETRACTOR_ID
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_RINGSLIP
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_ADD_NODE
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_UPDATE
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_FRAM_FACTOR
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_INTVAR
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_IDT_TSH
             L_REL = L_REL + 1
c---
c H3D/MAX
c---
           RBUF_L(L_REL+1) =  G_TM_YIELD
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_TM_SEQ
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_TM_EINT
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_TM_DMG
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_TM_SIG
             L_REL = L_REL + 1
           RBUF_L(L_REL+1) =  G_TM_STRA
             L_REL = L_REL + 1
c
c          print*,'    global lengths=',l_rel
          sum = l_rel
c
c         ! write global variables for each element
c
          RBUF_L(L_REL+1:L_REL+NEL*G_NOFF)  = GBUF%NOFF(1:NEL*G_NOFF)
            L_REL = L_REL + NEL*G_NOFF                             
          RBUF_L(L_REL+1:L_REL+NEL*G_IERR)  = GBUF%IERR(1:NEL*G_IERR)
            L_REL = L_REL + NEL*G_IERR                              
          RBUF_L(L_REL+1:L_REL+NEL*G_GAMA)  = GBUF%GAMA(1:NEL*G_GAMA)  
            L_REL = L_REL + NEL*G_GAMA                               
C           RBUF_L(L_REL+1:L_REL+NEL*G_SMSTR) = GBUF%SMSTR(1:NEL*G_SMSTR)                
C            L_REL = L_REL + NEL*G_SMSTR                                          
          RBUF_L(L_REL+1:L_REL+NEL*G_HOURG) = GBUF%HOURG(1:NEL*G_HOURG)                
            L_REL = L_REL + NEL*G_HOURG                                          
          RBUF_L(L_REL+1:L_REL+NEL*G_TAG22) = GBUF%TAG22(1:NEL*G_TAG22) 
            L_REL = L_REL+NEL*G_TAG22 
          RBUF_L(L_REL+1:L_REL+NEL*G_STRA)  = GBUF%STRA(1:NEL*G_STRA) 
            L_REL = L_REL+NEL*G_STRA 
          RBUF_L(L_REL+1:L_REL+NEL*G_SIGI)  = GBUF%SIGI(1:NEL*G_SIGI) 
            L_REL = L_REL+NEL*G_SIGI 
          RBUF_L(L_REL+1:L_REL+NEL*G_DMG)   = GBUF%DMG(1:NEL*G_DMG) 
            L_REL = L_REL+NEL*G_DMG 
          RBUF_L(L_REL+1:L_REL+NEL*G_UELR)  = GBUF%UELR(1:NEL*G_UELR)
            L_REL = L_REL+NEL*G_UELR
          RBUF_L(L_REL+1:L_REL+NEL*G_UELR1) = GBUF%UELR1(1:NEL*G_UELR1)
            L_REL = L_REL+NEL*G_UELR1
          RBUF_L(L_REL+1:L_REL+NEL*G_DAMDL) = GBUF%DAMDL(1:NEL*G_DAMDL)
            L_REL = L_REL+NEL*G_DAMDL
          RBUF_L(L_REL+1:L_REL+NEL*G_FOR)   = GBUF%FOR(1:NEL*G_FOR) 
            L_REL = L_REL+NEL*G_FOR 
          RBUF_L(L_REL+1:L_REL+NEL*G_MOM)   = GBUF%MOM(1:NEL*G_MOM) 
            L_REL = L_REL+NEL*G_MOM 
          RBUF_L(L_REL+1:L_REL+NEL*G_THK)   = GBUF%THK(1:NEL*G_THK) 
            L_REL = L_REL+NEL*G_THK 
          RBUF_L(L_REL+1:L_REL+NEL*G_STRW)  = GBUF%STRW(1:NEL*G_STRW) 
            L_REL = L_REL+NEL*G_STRW 
          RBUF_L(L_REL+1:L_REL+NEL*G_THK_I) = GBUF%THK_I(1:NEL*G_THK_I) 
            L_REL = L_REL+NEL*G_THK_I 
          RBUF_L(L_REL+1:L_REL+NEL*G_JAC_I) = GBUF%JAC_I(1:NEL*G_JAC_I) 
            L_REL = L_REL+NEL*G_JAC_I 
          RBUF_L(L_REL+1:L_REL+NEL*G_DT)    = GBUF%DT(1:NEL*G_DT) 
            L_REL = L_REL+NEL*G_DT
          RBUF_L(L_REL+1:L_REL+NEL*G_ISMS)  = GBUF%ISMS(1:NEL*G_ISMS) 
            L_REL = L_REL+NEL*G_ISMS
          RBUF_L(L_REL+1:L_REL+NEL*G_BPRELD)= GBUF%BPRELD(1:NEL*G_BPRELD) 
            L_REL = L_REL+NEL*G_BPRELD
          RBUF_L(L_REL+1:L_REL+NEL*G_COR_NF)= GBUF%COR_NF(1:NEL*G_COR_NF) 
            L_REL = L_REL+NEL*G_COR_NF
          RBUF_L(L_REL+1:L_REL+NEL*G_COR_FR)= GBUF%COR_FR(1:NEL*G_COR_FR) 
            L_REL = L_REL+NEL*G_COR_FR
          RBUF_L(L_REL+1:L_REL+NEL*G_COR_XR)= GBUF%COR_XR(1:NEL*G_COR_XR) 
            L_REL = L_REL+NEL*G_COR_XR
          RBUF_L(L_REL+1:L_REL+NEL*G_MAXFRAC)= GBUF%MAXFRAC(1:NEL*G_MAXFRAC) 
            L_REL = L_REL+NEL*G_MAXFRAC
          RBUF_L(L_REL+1:L_REL+NEL*G_MAXEPS)= GBUF%MAXEPS(1:NEL*G_MAXEPS) 
            L_REL = L_REL+NEL*G_MAXEPS
          RBUF_L(L_REL+1:L_REL+NEL*G_BETAORTH)= GBUF%BETAORTH(1:NEL*G_BETAORTH)
          L_REL = L_REL+NEL*G_BETAORTH
          RBUF_L(L_REL+1:L_REL+NEL*G_AMU)= GBUF%AMU(1:NEL*G_AMU)
            L_REL = L_REL+NEL*G_AMU
c-------
c
c        IF (NPG == 1) THEN  ! nothing to write (pointers only)                         
c
         IF (NPG == 1) THEN
           IF (G_STRPG>G_STRA) THEN
             RBUF_L(L_REL+1:L_REL+NEL*G_STRPG) = GBUF%STRPG(1:NEL*G_STRPG) 
             L_REL = L_REL+NEL*G_STRPG                                   
           END IF
         ELSEIF (NPG > 1) THEN                                           
           RBUF_L(L_REL+1:L_REL+NEL*G_FORPG) = GBUF%FORPG(1:NEL*G_FORPG) 
             L_REL = L_REL+NEL*G_FORPG                                   
           RBUF_L(L_REL+1:L_REL+NEL*G_MOMPG) = GBUF%MOMPG(1:NEL*G_MOMPG) 
             L_REL = L_REL+NEL*G_MOMPG
C
           RBUF_L(L_REL+1:L_REL+NEL*G_FORPGPINCH) = GBUF%FORPGPINCH(1:NEL*G_FORPGPINCH) 
             L_REL = L_REL+NEL*G_FORPGPINCH                                   
           RBUF_L(L_REL+1:L_REL+NEL*G_MOMPGPINCH) = GBUF%MOMPGPINCH(1:NEL*G_MOMPGPINCH) 
             L_REL = L_REL+NEL*G_MOMPGPINCH
           RBUF_L(L_REL+1:L_REL+NEL*G_EPGPINCHXZ) = GBUF%EPGPINCHXZ(1:NEL*G_EPGPINCHXZ) 
             L_REL = L_REL+NEL*G_EPGPINCHXZ
           RBUF_L(L_REL+1:L_REL+NEL*G_EPGPINCHYZ) = GBUF%EPGPINCHYZ(1:NEL*G_EPGPINCHYZ) 
             L_REL = L_REL+NEL*G_EPGPINCHYZ
           RBUF_L(L_REL+1:L_REL+NEL*G_EPGPINCHZZ) = GBUF%EPGPINCHZZ(1:NEL*G_EPGPINCHZZ) 
             L_REL = L_REL+NEL*G_EPGPINCHZZ 
C                                   
           RBUF_L(L_REL+1:L_REL+NEL*G_STRPG) = GBUF%STRPG(1:NEL*G_STRPG) 
             L_REL = L_REL+NEL*G_STRPG                                   
           RBUF_L(L_REL+1:L_REL+NEL*G_STRWPG)= GBUF%STRWPG(1:NEL*G_STRWPG) 
            L_REL = L_REL+NEL*G_STRWPG 
         ENDIF                                                           
c          print*,'    global 2 =',l_rel
c-------
         IF (NPTTOT /= 1) THEN
          RBUF_L(L_REL+1:L_REL+NEL*G_BFRAC)= GBUF%BFRAC(1:NEL*G_BFRAC)                
            L_REL = L_REL + NEL*G_BFRAC                                          
          RBUF_L(L_REL+1:L_REL+NEL*G_OFF)  = GBUF%OFF(1:NEL*G_OFF)
            L_REL = L_REL+NEL*G_OFF
          RBUF_L(L_REL+1:L_REL+NEL*G_EINT) = GBUF%EINT(1:NEL*G_EINT)
            L_REL = L_REL+NEL*G_EINT
          RBUF_L(L_REL+1:L_REL+NEL*G_EINS) = GBUF%EINS(1:NEL*G_EINS)
            L_REL = L_REL+NEL*G_EINS
          RBUF_L(L_REL+1:L_REL+NEL*G_RHO)  = GBUF%RHO(1:NEL*G_RHO)
            L_REL = L_REL+NEL*G_RHO
          RBUF_L(L_REL+1:L_REL+NEL*G_QVIS) = GBUF%QVIS(1:NEL*G_QVIS)
            L_REL = L_REL+NEL*G_QVIS
          RBUF_L(L_REL+1:L_REL+NEL*G_DELTAX)=GBUF%DELTAX(1:NEL*G_DELTAX)
            L_REL = L_REL+NEL*G_DELTAX
          RBUF_L(L_REL+1:L_REL+NEL*G_VOL)  = GBUF%VOL(1:NEL*G_VOL)
            L_REL = L_REL+NEL*G_VOL
          RBUF_L(L_REL+1:L_REL+NEL*G_EPSD) = GBUF%EPSD(1:NEL*G_EPSD)                
            L_REL = L_REL+NEL*G_EPSD                                         
          RBUF_L(L_REL+1:L_REL+NEL*G_EPSQ) = GBUF%EPSQ(1:NEL*G_EPSQ)
            L_REL = L_REL+NEL*G_EPSQ
          RBUF_L(L_REL+1:L_REL+NEL*G_PLA)  = GBUF%PLA(1:NEL*G_PLA)
            L_REL = L_REL+NEL*G_PLA
          RBUF_L(L_REL+1:L_REL+NEL*G_TEMP) = GBUF%TEMP(1:NEL*G_TEMP)
            L_REL = L_REL+NEL*G_TEMP
          RBUF_L(L_REL+1:L_REL+NEL*G_TB)   = GBUF%TB(1:NEL*G_TB)
            L_REL = L_REL+NEL*G_TB
          RBUF_L(L_REL+1:L_REL+NEL*G_RK)   = GBUF%RK(1:NEL*G_RK)
            L_REL = L_REL+NEL*G_RK
          RBUF_L(L_REL+1:L_REL+NEL*G_RE)   = GBUF%RE(1:NEL*G_RE)
            L_REL = L_REL+NEL*G_RE
          RBUF_L(L_REL+1:L_REL+NEL*G_SIG)  = GBUF%SIG(1:NEL*G_SIG) 
            L_REL = L_REL+NEL*G_SIG 
          RBUF_L(L_REL+1:L_REL+NEL*G_FORTH)  = GBUF%FORTH(1:NEL*G_FORTH) 
            L_REL = L_REL+NEL*G_FORTH
          RBUF_L(L_REL+1:L_REL+NEL*G_EINTTH)  = GBUF%EINTTH(1:NEL*G_EINTTH) 
            L_REL = L_REL+NEL*G_EINTTH 
          RBUF_L(L_REL+1:L_REL+NEL*G_SEQ)  = GBUF%SEQ(1:NEL*G_SEQ)
            L_REL = L_REL+NEL*G_SEQ
          RBUF_L(L_REL+1:L_REL+NEL*G_ABURN)= GBUF%ABURN(1:NEL*G_ABURN)                
            L_REL = L_REL + NEL*G_ABURN                                        
          RBUF_L(L_REL+1:L_REL+NEL*G_MU)   = GBUF%MU(1:NEL*G_MU)                
            L_REL = L_REL + NEL*G_MU                                        
         ENDIF                             
C---------
         RBUF_L(L_REL+1:L_REL+NEL*G_FILL) = GBUF%FILL(1:NEL*G_FILL) 
           L_REL = L_REL+NEL*G_FILL
c          print*,'     global vars=',l_rel-sum
c          print*,'      final length : buflen=',l_rel,ITY
c---
c 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
c---
          RBUF_L(L_REL+1:L_REL+NEL*G_AREA)        = GBUF%AREA(1:NEL*G_AREA)
            L_REL = L_REL +NEL*G_AREA
          RBUF_L(L_REL+1:L_REL+NEL*G_SKEW)        = GBUF%SKEW(1:NEL*G_SKEW)
            L_REL = L_REL +NEL*G_SKEW
          RBUF_L(L_REL+1:L_REL+NEL*G_LENGTH)      = GBUF%LENGTH(1:NEL*G_LENGTH)
            L_REL = L_REL +NEL*G_LENGTH
          RBUF_L(L_REL+1:L_REL+NEL*G_TOTDEPL)     = GBUF%TOTDEPL(1:NEL*G_TOTDEPL)
            L_REL = L_REL +NEL*G_TOTDEPL
          RBUF_L(L_REL+1:L_REL+NEL*G_TOTROT)      = GBUF%TOTROT(1:NEL*G_TOTROT)
            L_REL = L_REL +NEL*G_TOTROT
          RBUF_L(L_REL+1:L_REL+NEL*G_FOREP)       = GBUF%FOREP(1:NEL*G_FOREP)
            L_REL = L_REL +NEL*G_FOREP
          RBUF_L(L_REL+1:L_REL+NEL*G_MOMEP)       = GBUF%MOMEP(1:NEL*G_MOMEP)
            L_REL = L_REL +NEL*G_MOMEP
          RBUF_L(L_REL+1:L_REL+NEL*G_DEP_IN_TENS) = GBUF%DEP_IN_TENS(1:NEL*G_DEP_IN_TENS)
            L_REL = L_REL + NEL*G_DEP_IN_TENS
          RBUF_L(L_REL+1:L_REL+NEL*G_DEP_IN_COMP) = GBUF%DEP_IN_COMP(1:NEL*G_DEP_IN_COMP)
            L_REL = L_REL +NEL*G_DEP_IN_COMP
          RBUF_L(L_REL+1:L_REL+NEL*G_ROT_IN_TENS) = GBUF%ROT_IN_TENS(1:NEL*G_ROT_IN_TENS)
            L_REL = L_REL +NEL*G_ROT_IN_TENS
          RBUF_L(L_REL+1:L_REL+NEL*G_ROT_IN_COMP) = GBUF%ROT_IN_COMP(1:NEL*G_ROT_IN_COMP)
            L_REL = L_REL +NEL*G_ROT_IN_COMP
          RBUF_L(L_REL+1:L_REL+NEL*G_POSX)        = GBUF%POSX(1:NEL*G_POSX)
            L_REL = L_REL +NEL*G_POSX
          RBUF_L(L_REL+1:L_REL+NEL*G_POSY)        = GBUF%POSY(1:NEL*G_POSY)
            L_REL = L_REL +NEL*G_POSY
          RBUF_L(L_REL+1:L_REL+NEL*G_POSZ)        = GBUF%POSZ(1:NEL*G_POSZ)
            L_REL = L_REL +NEL*G_POSZ
          RBUF_L(L_REL+1:L_REL+NEL*G_POSXX)       = GBUF%POSXX(1:NEL*G_POSXX)
            L_REL = L_REL +NEL*G_POSXX
          RBUF_L(L_REL+1:L_REL+NEL*G_POSYY)       = GBUF%POSYY(1:NEL*G_POSYY)
            L_REL = L_REL +NEL*G_POSYY
          RBUF_L(L_REL+1:L_REL+NEL*G_POSZZ)       = GBUF%POSZZ(1:NEL*G_POSZZ)
            L_REL = L_REL +NEL*G_POSZZ
          RBUF_L(L_REL+1:L_REL+NEL*G_YIELD)       = GBUF%YIELD(1:NEL*G_YIELD)
            L_REL = L_REL +NEL*G_YIELD
          RBUF_L(L_REL+1:L_REL+NEL*G_LENGTH_ERR)  = GBUF%LENGTH_ERR(1:NEL*G_LENGTH_ERR)
            L_REL = L_REL +NEL*G_LENGTH_ERR
          RBUF_L(L_REL+1:L_REL+NEL*G_DV)          = GBUF%DV(1:NEL*G_DV)
            L_REL = L_REL +NEL*G_DV
          RBUF_L(L_REL+1:L_REL+NEL*G_DFS)         = GBUF%DFS(1:NEL*G_DFS)
            L_REL = L_REL +NEL*G_DFS
          RBUF_L(L_REL+1:L_REL+NEL*G_SKEW_ERR)    = GBUF%SKEW_ERR(1:NEL*G_SKEW_ERR)
            L_REL = L_REL +NEL*G_SKEW_ERR
          RBUF_L(L_REL+1:L_REL+NEL*G_E6)          = GBUF%E6(1:NEL*G_E6)
            L_REL = L_REL +NEL*G_E6
          RBUF_L(L_REL+1:L_REL+NEL*G_RUPTCRIT)    = GBUF%RUPTCRIT(1:NEL*G_RUPTCRIT)
            L_REL = L_REL +NEL*G_RUPTCRIT
          RBUF_L(L_REL+1:L_REL+NEL*G_MASS)        = GBUF%MASS(1:NEL*G_MASS)
            L_REL = L_REL +NEL*G_MASS
          RBUF_L(L_REL+1:L_REL+NEL*G_V_REPCVT)    = GBUF%V_REPCVT(1:NEL*G_V_REPCVT)
            L_REL = L_REL +NEL*G_V_REPCVT
          RBUF_L(L_REL+1:L_REL+NEL*G_VR_REPCVT)   = GBUF%VR_REPCVT(1:NEL*G_VR_REPCVT)
            L_REL = L_REL +NEL*G_VR_REPCVT
          RBUF_L(L_REL+1:L_REL+NEL*G_NUVAR)       = GBUF%VAR(1:NEL*G_NUVAR)
            L_REL = L_REL +NEL*G_NUVAR
          RBUF_L(L_REL+1:L_REL+NEL*G_NUVARN)      = GBUF%VARN(1:NEL*G_NUVARN)
            L_REL = L_REL +NEL*G_NUVARN
          RBUF_L(L_REL+1:L_REL+NEL*G_DEFINI)      = GBUF%DEFINI(1:NEL*G_DEFINI)
            L_REL = L_REL +NEL*G_DEFINI
          RBUF_L(L_REL+1:L_REL+NEL*G_FORINI)      = GBUF%FORINI(1:NEL*G_FORINI)
            L_REL = L_REL +NEL*G_FORINI
          RBUF_L(L_REL+1:L_REL+NEL*G_INIFRIC)     = GBUF%INIFRIC(1:NEL*G_INIFRIC)
            L_REL = L_REL +NEL*G_INIFRIC
          RBUF_L(L_REL+1:L_REL+NEL*G_STRHG)       = GBUF%STRHG(1:NEL*G_STRHG)
            L_REL = L_REL +NEL*G_STRHG
          RBUF_L(L_REL+1:L_REL+NEL*G_DT_PITER)    = GBUF%DT_PITER(1:NEL*G_DT_PITER)
            L_REL = L_REL +NEL*G_DT_PITER
          RBUF_L(L_REL+1:L_REL+NEL*G_SKEW_ID)     = GBUF%SKEW_ID(1:NEL*G_SKEW_ID)
            L_REL = L_REL +NEL*G_SKEW_ID
c---
c seatbelt elements
c---
          RBUF_L(L_REL+1:L_REL+NEL*G_SLIPRING_ID) = GBUF%SLIPRING_ID(1:NEL*G_SLIPRING_ID)
            L_REL = L_REL +NEL*G_SLIPRING_ID
          RBUF_L(L_REL+1:L_REL+NEL*G_SLIPRING_FRAM_ID) = GBUF%SLIPRING_FRAM_ID(1:NEL*G_SLIPRING_FRAM_ID)
            L_REL = L_REL +NEL*G_SLIPRING_FRAM_ID
          RBUF_L(L_REL+1:L_REL+NEL*G_SLIPRING_STRAND) = GBUF%SLIPRING_STRAND(1:NEL*G_SLIPRING_STRAND)
            L_REL = L_REL +NEL*G_SLIPRING_STRAND
          RBUF_L(L_REL+1:L_REL+NEL*G_RETRACTOR_ID)= GBUF%RETRACTOR_ID(1:NEL*G_RETRACTOR_ID)
            L_REL = L_REL +NEL*G_RETRACTOR_ID
          RBUF_L(L_REL+1:L_REL+NEL*G_RINGSLIP)= GBUF%RINGSLIP(1:NEL*G_RINGSLIP)
            L_REL = L_REL +NEL*G_RINGSLIP
          RBUF_L(L_REL+1:L_REL+NEL*G_ADD_NODE)= GBUF%ADD_NODE(1:NEL*G_ADD_NODE)
            L_REL = L_REL +NEL*G_ADD_NODE
          RBUF_L(L_REL+1:L_REL+NEL*G_UPDATE)= GBUF%UPDATE(1:NEL*G_UPDATE)
            L_REL = L_REL +NEL*G_UPDATE
          RBUF_L(L_REL+1:L_REL+NEL*G_FRAM_FACTOR)= GBUF%FRAM_FACTOR(1:NEL*G_FRAM_FACTOR)
            L_REL = L_REL +NEL*G_FRAM_FACTOR
          RBUF_L(L_REL+1:L_REL+NEL*G_INTVAR)= GBUF%INTVAR(1:NEL*G_INTVAR)
            L_REL = L_REL +NEL*G_INTVAR
          RBUF_L(L_REL+1:L_REL+NEL*G_IDT_TSH)= GBUF%IDT_TSH(1:NEL*G_IDT_TSH)
            L_REL = L_REL +NEL*G_IDT_TSH
C---------
          IF (NPG > 1) THEN            
            RBUF_L(L_REL+1:L_REL + NEL*GBUF%G_TEMPG)= GBUF%TEMPG(1:NEL*GBUF%G_TEMPG) 
            L_REL = L_REL + NEL*GBUF%G_TEMPG
          ENDIF
c---
c TMAX(H3D)
c---
          RBUF_L(L_REL+1:L_REL+NEL*G_TM_YIELD) = GBUF%TM_YIELD(1:NEL*G_TM_YIELD)
            L_REL = L_REL +NEL*G_TM_YIELD
          RBUF_L(L_REL+1:L_REL+NEL*G_TM_SEQ) = GBUF%TM_SEQ(1:NEL*G_TM_SEQ)
            L_REL = L_REL +NEL*G_TM_SEQ
          RBUF_L(L_REL+1:L_REL+NEL*G_TM_EINT) = GBUF%TM_EINT(1:NEL*G_TM_EINT)
            L_REL = L_REL +NEL*G_TM_EINT
          RBUF_L(L_REL+1:L_REL+NEL*G_TM_DMG) = GBUF%TM_DMG(1:NEL*G_TM_DMG)
            L_REL = L_REL +NEL*G_TM_DMG
          RBUF_L(L_REL+1:L_REL+NEL*G_TM_SIG) = GBUF%TM_SIG1(1:NEL*G_TM_SIG)
            L_REL = L_REL +NEL*G_TM_SIG
          RBUF_L(L_REL+1:L_REL+NEL*G_TM_SIG) = GBUF%TM_SIG3(1:NEL*G_TM_SIG)
            L_REL = L_REL +NEL*G_TM_SIG
          RBUF_L(L_REL+1:L_REL+NEL*G_TM_STRA) = GBUF%TM_STRA1(1:NEL*G_TM_STRA)
            L_REL = L_REL +NEL*G_TM_STRA
          RBUF_L(L_REL+1:L_REL+NEL*G_TM_STRA) = GBUF%TM_STRA3(1:NEL*G_TM_STRA)
            L_REL = L_REL +NEL*G_TM_STRA
C ------------          
          CALL WRITE_DB (RBUF_L,L_REL)
C ------------Part of DP          
C
C SMSTR must be written in separate Buffer (Forced in double precision)
C
          SDP_RBUF = SDP_RBUF + G_SMSTR *NEL
          IF(SDP_RBUF > 0)THEN
            ALLOCATE(DP_RBUF(SDP_RBUF))
          L_DPR = 0
C--------local buf first          
           DO IL = 1,ELBUF%NLAY                                          
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)              
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
            ELSE
              NPTT = ELBUF_TAB(NG)%NPTT
            ENDIF 
            DO IR = 1,ELBUF%NPTR                                             
              DO IS = 1,ELBUF%NPTS                                           
                DO IT = 1,NPTT
                 LBUF => ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)   
c              
                 DP_RBUF(L_DPR+1:L_DPR+NEL*L_VOL0DP) = LBUF%VOL0DP(1:NEL*L_VOL0DP)
                 L_DPR = L_DPR+NEL*L_VOL0DP
                ENDDO                                                       
              ENDDO                                                         
            ENDDO                                                           
           ENDDO                                                       
           DP_RBUF(L_DPR+1:L_DPR+NEL*G_SMSTR)= GBUF%SMSTR(1:NEL*G_SMSTR)
            CALL WRITE_DPDB(DP_RBUF,SDP_RBUF)
            DEALLOCATE(DP_RBUF)
          ENDIF
C---------
          DEALLOCATE (RBUF_L)
c---------
        ENDIF   ! ity
      ENDDO      ! NG=1,NGROUP
C-----------
      RETURN
      END
