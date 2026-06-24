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
!||    allocbuf_auto    ../engine/source/elements/elbuf/allocbuf_auto.F
!||--- called by ------------------------------------------------------
!||    elbuf_ini        ../engine/source/elements/elbuf/elbuf_ini.F
!||--- calls      -----------------------------------------------------
!||    read_db          ../common_source/tools/input_output/read_db.F
!||    read_dpdb        ../common_source/tools/input_output/read_db.F
!||--- uses       -----------------------------------------------------
!||    h3d_inc_mod      ../engine/share/modules/h3d_inc_mod.F
!||    mat_elem_mod     ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    my_alloc_mod     ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
      SUBROUTINE ALLOCBUF_AUTO(ELBUF_STR,MAT_ELEM,BUFLEN,NPT,ITY,ISNOD,NEWLEN,INLOC,
     .                         IDAMP_FREQ_RANGE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD    
      USE H3D_INC_MOD          
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_alloc_mod
      use my_dealloc_mod, only : my_dealloc
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER BUFLEN,NPT,ITY,ISNOD,NEWLEN,INLOC
      INTEGER, INTENT(IN) :: IDAMP_FREQ_RANGE
      TYPE(ELBUF_STRUCT_) , TARGET   :: ELBUF_STR
      TYPE(MAT_ELEM_) , TARGET   :: MAT_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,IL,IR,IS,IT,IAD,ERR,NUVAR,NVARTMP,NVARTMP_EOS,IGTYP,IXFEM,NLAY,
     .  NINTLAY,NPTR,NPTS,NPTT,NEL,NPG,IPT,LEN1,LEN2,ILAW,IMAT,IEOS,NONL,
     .  IVISC,IPORO,NFAIL,NVAR_MAT,NVAR_EOS,NVAR_VISC,NVAR_LOC,NVAR_LAY,NPTTOT,IADP,
     .  IDRAPE
      INTEGER ! global variables
     .   G_GAMA,G_SIG,G_OFF,G_NOFF,G_EINT,G_EINS,G_TEMP,  
     .   G_RHO,G_PLA,G_VOL,G_EPSD,G_QVIS,G_DELTAX,G_TB,G_RK,G_RE,G_WPLA, 
     .   G_SMSTR,G_HOURG,G_BFRAC,G_THK,G_FOR,G_MOM,G_TAG22,G_STRA,G_SIGI,
     .   G_DMG,G_FORPG,G_MOMPG,G_STRPG,G_UELR,G_UELR1,G_EPSQ,G_IERR,
     .   G_DAMDL,G_FORTH,G_EINTTH,G_FILL,G_SEQ,G_STRW,G_STRWPG,G_THK_I,
     .   G_JAC_I,G_AREA,G_SKEW,G_LENGTH,G_TOTDEPL,G_TOTROT,G_FOREP,G_MOMEP,
     .   G_DEP_IN_TENS,G_DEP_IN_COMP,G_ROT_IN_TENS,G_ROT_IN_COMP,
     .   G_POSX,G_POSY,G_POSZ,G_POSXX,G_POSYY,G_POSZZ,G_YIELD,G_LENGTH_ERR,
     .   G_DV,G_DFS,G_SKEW_ERR,G_E6,G_RUPTCRIT,G_MASS,G_V_REPCVT,
     .   G_VR_REPCVT,G_NUVAR,G_NUVARN,G_INIFRIC,G_DT,G_ISMS,G_STRHG,G_ETOTSH,
     .   G_BPRELD,G_ABURN,G_MU,G_PLANL,G_EPSDNL,G_DT_PITER_OLD,G_DT_PITER,G_TEMPG,
     .   G_COR_NF,G_COR_FR,G_COR_XR,G_DEFINI,G_FORINI,G_IDT_TSH,
     .   G_FORPGPINCH,G_MOMPGPINCH,G_EPGPINCHXZ,G_EPGPINCHYZ,G_EPGPINCHZZ,
     .   G_SKEW_ID,G_MAXFRAC,G_MAXEPS,G_GAMA_R,G_SLIPRING_ID,G_SLIPRING_STRAND,G_RETRACTOR_ID,
     .   G_RINGSLIP,G_ADD_NODE,G_UPDATE,G_INTVAR,G_BETAORTH,G_AMU,G_SLIPRING_FRAM_ID,G_FRAM_FACTOR,
     .   G_TM_YIELD,G_TM_SEQ,G_TM_EINT,G_TM_DMG,G_TM_SIG,G_TM_STRA,G_TSAIWU,G_DMGSCL,
     .   G_SH_IOFFSET,G_EINT_DISTOR,G_FOR_G,G_FORPG_G,G_YIELD_IN_COMP,G_XXOLD_IN_COMP,
     .   G_RBODY_NODE
      INTEGER  ! layer variables
     .   LY_DMG,LY_GAMA,LY_DIRA,LY_DIRB,LY_CRKDIR,LY_PLAPT,LY_SIGPT,
     .   LY_HOURG,LY_UELR,LY_UELR1,LY_OFFPG,LY_OFF,LEN_PLAPT,LEN_SIGPT
      INTEGER  ! interlayer variables
     .   LY_EINT,LY_COUNT
      INTEGER  ! local variables
     .   L_OFF,L_EINT,L_EINS,L_RHO,L_DP_DRHO,L_VOL,L_PLA,L_TB,L_TEMP,
     .   L_RK,L_RE,L_VK,L_ROB,L_SIG,L_SIGV,L_SIGL,L_SIGA,L_SIGD,L_SIGB,
     .   L_SIGC,L_SIGF,L_STRA,L_EPSA,L_EPSD,L_EPSQ,L_EPSF,L_CRAK,L_WPLA,
     .   L_ANG,L_SF,L_QVIS,L_DELTAX,L_GAMA,
     .   L_DAM,L_DSUM,L_DGLO,L_EPE,L_EPC,L_XST,L_SSP,L_Z,L_FRAC,L_VISC,
     .   L_THK,L_FOR,L_MOM,L_EPS,L_SMSTR,L_BFRAC,L_DMG,L_FORTH,L_EINTTH,
     .   L_SEQ,L_SIGPLY,L_JAC_I,L_FAC_YLD,L_ABURN,L_MU,L_PLANL,L_EPSDNL,
     .   L_DMGSCL,L_PIJ,L_VOL0DP,L_TSAIWU
       INTEGER  ! failure variables
     .   LF_DAM,LF_DAMMX,LF_DAMINI,LF_TDEL,LF_INDX,LF_OFF
      my_real, 
     .  ALLOCATABLE, DIMENSION(:)  :: RBUF

      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DP_RBUF
      INTEGER SDP_RBUF
      
      
      TYPE(BUF_MAT_)     , POINTER :: MATBUF
      TYPE(BUF_LAY_)     , POINTER :: BUFLY
      TYPE(BUF_INTLAY_)  , POINTER :: INTLAY
      TYPE(BUF_VISC_)    , POINTER :: VISCBUF
      TYPE(G_BUFEL_)     , POINTER :: GBUF
      TYPE(L_BUFEL_)     , POINTER :: LBUF
      TYPE(BUF_INTLOC_)  , POINTER :: ILBUF
      TYPE(FAIL_LOC_)    , POINTER :: FLOC
      TYPE(BUF_NLOC_)    , POINTER :: BUFNL
      TYPE(BUF_NLOCTS_)  , POINTER :: BUFNLTS
      TYPE(L_BUFEL_DIR_) , POINTER :: LBUF_DIR


        LY_SIGPT = 0
        L_VOL0DP = 0
        NPG = 0
        LY_PLAPT = 0
        NPTTOT = 0
c-------------------------------------------------
C       allocation of sub-structures of the element buffer
C=======================================================================
c------  Allocate and read element data from restart
        call my_alloc(RBUF, BUFLEN, "RBUF")
        CALL READ_DB(RBUF, BUFLEN)
c-------------------------------------------------
C       read element buffer
c-------------------------------------------------
      IAD = 0                                                         
C     part of DP in elbuf
      SDP_RBUF = 0
c-----
      IGTYP = NINT(RBUF(IAD+1))                        
        IAD = IAD+1                                      
      NEL   = NINT(RBUF(IAD+1))                                          
        IAD = IAD+1                                      
      NLAY  = NINT(RBUF(IAD+1))                        
        IAD = IAD+1                                      
      NINTLAY= NINT(RBUF(IAD+1))                        
        IAD  = IAD+1                                     
      NPTR  = NINT(RBUF(IAD+1))                          
        IAD = IAD+1                                      
      NPTS  = NINT(RBUF(IAD+1))                          
        IAD = IAD+1                                      
      NPTT  = NINT(RBUF(IAD+1))                          
        IAD = IAD+1                                      
      IXFEM = NINT(RBUF(IAD+1))                        
        IAD = IAD+1                                      
      NXEL  = NINT(RBUF(IAD+1))                        
        IAD = IAD+1                     
      IDRAPE  = NINT(RBUF(IAD+1))                        
        IAD = IAD+1                           
c-------------------------------------------------
      ELBUF_STR%IGTYP  = IGTYP
      ELBUF_STR%NEL    = NEL  
      ELBUF_STR%NLAY   = NLAY 
      ELBUF_STR%NINTLAY= NINTLAY 
      ELBUF_STR%NPTR   = NPTR 
      ELBUF_STR%NPTS   = NPTS 
      ELBUF_STR%NPTT   = NPTT 
      ELBUF_STR%IXFEM  = IXFEM 
      ELBUF_STR%NXEL   = NXEL 
      ELBUF_STR%IDRAPE   = IDRAPE
      NPT  = NLAY*NPTR*NPTS*NPTT
c-------------------------------------------------
      IF (ITY == 1 .OR. ITY == 2 .OR. ITY == 51 .OR. ITY == 101) THEN
        NPG = NPTR*NPTS*NPTT
        NPTTOT = NPT
      ELSEIF (ITY == 3 .OR. ITY == 7) THEN
        NPG = NPTR*NPTS
        NPTTOT = 0  !  filled below
      ELSEIF (ITY == 4 .OR. ITY == 5 .OR. ITY == 6 .OR. ITY == 100) THEN
        NPG = 0
        NPTTOT = 0
      ENDIF
c---------------------------------------------------          
C         Damping parameters for frequency range 
c--------------------------------------------------- 
      IF  (IDAMP_FREQ_RANGE > 0) THEN
        call my_alloc(ELBUF_STR%DAMP_RANGE%ALPHA, 3, "ELBUF_STR%DAMP_RANGE%ALPHA")
        call my_alloc(ELBUF_STR%DAMP_RANGE%TAU, 3, "ELBUF_STR%DAMP_RANGE%TAU")
        ELBUF_STR%DAMP_RANGE%ALPHA(1:3) = RBUF(IAD+1:IAD+3)
        ELBUF_STR%DAMP_RANGE%TAU(1:3) = RBUF(IAD+4:IAD+6)
        IAD = IAD + 6
      ENDIF         
c-------------------------------------------------
      call my_alloc(ELBUF_STR%BUFLY, NLAY, "ELBUF_STR%BUFLY")
C
C   moved below because of various NPTT through each layer for IGTYP = 51
C
C      DO IL = 1, NLAY                                                 
C        ALLOCATE(ELBUF_STR%BUFLY(IL)%LBUF(NPTR,NPTS,NPTT),STAT=ERR)   
C        ALLOCATE(ELBUF_STR%BUFLY(IL)%MAT (NPTR,NPTS,NPTT),STAT=ERR)       
C        ALLOCATE(ELBUF_STR%BUFLY(IL)%FAIL(NPTR,NPTS,NPTT),STAT=ERR)   
C        ALLOCATE(ELBUF_STR%BUFLY(IL)%PROP(NPTR,NPTS,NPTT),STAT=ERR)   
C        ALLOCATE(ELBUF_STR%BUFLY(IL)%EOS (NPTR,NPTS,NPTT),STAT=ERR)   
C        ALLOCATE(ELBUF_STR%BUFLY(IL)%VISC(NPTR,NPTS,NPTT),STAT=ERR)   
C        ALLOCATE(ELBUF_STR%BUFLY(IL)%PORO(NPTR,NPTS,NPTT),STAT=ERR)   
C      ENDDO                                                 
c-------------------------------------------------
      call my_alloc(ELBUF_STR%INTLAY, NINTLAY, "ELBUF_STR%INTLAY")
      DO IL = 1, NINTLAY                                                 
        ALLOCATE(ELBUF_STR%INTLAY(IL)%ILBUF(NPTR,NPTS),STAT=ERR)
        ALLOCATE(ELBUF_STR%INTLAY(IL)%MAT  (NPTR,NPTS),STAT=ERR)       
        ALLOCATE(ELBUF_STR%INTLAY(IL)%FAIL (NPTR,NPTS),STAT=ERR)   
      ENDDO                                                 
c-------------------------------------------------
C     LAYER VARIABLES
c-------------------------------------------------
      DO IL = 1,ELBUF_STR%NLAY                                         
C       Layer parameters : NPAR_LAY = 13 
c
        BUFLY => ELBUF_STR%BUFLY(IL)
c
        ILAW  = NINT(RBUF(IAD+1))                         
          IAD = IAD+1                                                  
        IMAT  = NINT(RBUF(IAD+1))                         
          IAD = IAD+1                                                  
        IEOS  = NINT(RBUF(IAD+1))                         
          IAD = IAD+1                                                  
        IVISC = NINT(RBUF(IAD+1))                         
          IAD = IAD+1                                                  
        IPORO = NINT(RBUF(IAD+1))                        
          IAD = IAD+1                                                  
        NFAIL = NINT(RBUF(IAD+1))                        
          IAD = IAD+1                                                  
        NVAR_MAT = NINT(RBUF(IAD+1))               
          IAD = IAD+1   
        NVAR_EOS = NINT(RBUF(IAD+1))               
          IAD = IAD+1                                                          
        NVARTMP = NINT(RBUF(IAD+1))               
          IAD = IAD+1                                                  
        NVARTMP_EOS = NINT(RBUF(IAD+1))               
          IAD = IAD+1                                     
        NVAR_VISC= NINT(RBUF(IAD+1))                        
          IAD = IAD+1                                                  
        NVAR_LOC = NINT(RBUF(IAD+1))                        
          IAD = IAD+1                                                  
        NVAR_LAY = NINT(RBUF(IAD+1))                        
          IAD = IAD+1                                                  
        NPTT  = NINT(RBUF(IAD+1))                        
          IAD = IAD+1
c
c---    layer variable lengths (BUFLY%NVAR_LAY = 6
C             
        LY_DMG = NINT(RBUF(IAD+1))                        
          IAD = IAD+1                                                  
        LY_GAMA = NINT(RBUF(IAD+1))                        
          IAD = IAD+1                                                  
        LY_DIRA = NINT(RBUF(IAD+1))                        
          IAD = IAD+1                                                  
        LY_DIRB = NINT(RBUF(IAD+1))                        
          IAD = IAD+1                                                  
        LY_CRKDIR= NINT(RBUF(IAD+1))                        
          IAD = IAD+1                                                  
        LY_PLAPT = NINT(RBUF(IAD+1))                        
          IAD = IAD+1                                                  
        LY_SIGPT = NINT(RBUF(IAD+1))                        
          IAD = IAD+1                                                  
        LY_HOURG = NINT(RBUF(IAD+1))                        
          IAD = IAD+1
        LY_UELR = NINT(RBUF(IAD+1))                        
          IAD = IAD+1
        LY_UELR1 = NINT(RBUF(IAD+1))                        
          IAD = IAD+1
        LY_OFFPG = NINT(RBUF(IAD+1))                        
          IAD = IAD+1
        LY_OFF   = NINT(RBUF(IAD+1))                        
          IAD = IAD+1
c
C       local variable lengths : NVAR_LOC = 48
c
        L_OFF    = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_GAMA   = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_STRA   = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_FRAC   = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_BFRAC   = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                         
        L_EINT   = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_EINS   = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_RHO    = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_DP_DRHO= NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_QVIS   = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_DELTAX = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_VOL    = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_EPSA   = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_EPSD   = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_EPSQ   = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_EPSF   = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_PLA    = NINT(RBUF(IAD+1))  
          IAD = IAD+1    
        L_WPLA    = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                                  
        L_TEMP   = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_TB     = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                         
        L_RK     = NINT(RBUF(IAD+1))  
          IAD = IAD+1                                                  
        L_RE     = NINT(RBUF(IAD+1))  
          IAD = IAD+1                               
        L_VK     = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_SF     = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_ROB    = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_DAM    = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_DSUM   = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_DGLO   = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_CRAK   = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_ANG    = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_EPE    = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_EPC    = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_XST    = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_SSP    = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_Z      = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_VISC   = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_SIGL   = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_SIGV   = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_SIGA   = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_SIGB   = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_SIGC   = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_SIGD   = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_SIGF   = NINT(RBUF(IAD+1)) 
          IAD = IAD+1                                                  
        L_SIG    = NINT(RBUF(IAD+1))
          IAD = IAD+1                                                  
        L_SIGPLY = NINT(RBUF(IAD+1))
          IAD = IAD+1                                                  
        L_FOR    = NINT(RBUF(IAD+1))
          IAD = IAD+1                                                  
        L_MOM    = NINT(RBUF(IAD+1))
          IAD = IAD+1
        L_THK    = NINT(RBUF(IAD+1))
          IAD = IAD+1                                                  
        L_SMSTR = NINT(RBUF(IAD+1))
          IAD = IAD+1                                                  
        L_DMG = NINT(RBUF(IAD+1))
          IAD = IAD+1 
        L_FORTH  = NINT(RBUF(IAD+1))
          IAD = IAD+1    
        L_EINTTH    = NINT(RBUF(IAD+1))
          IAD = IAD+1
        L_SEQ    = NINT(RBUF(IAD+1))
          IAD = IAD+1
        L_JAC_I  = NINT(RBUF(IAD+1))
          IAD = IAD+1
        L_FAC_YLD    = NINT(RBUF(IAD+1))
          IAD = IAD+1
        L_ABURN      = NINT(RBUF(IAD+1)) 
          IAD = IAD+1      
        L_MU  = NINT(RBUF(IAD+1)) 
          IAD = IAD+1  
        L_PLANL = NINT(RBUF(IAD+1)) 
          IAD = IAD+1    
        L_EPSDNL = NINT(RBUF(IAD+1)) 
          IAD = IAD+1   
        L_DMGSCL = NINT(RBUF(IAD+1)) 
          IAD = IAD+1       
        L_TSAIWU = NINT(RBUF(IAD+1)) 
          IAD = IAD+1               
        L_PIJ    = NINT(RBUF(IAD+1)) 
          IAD = IAD+1 
        L_VOL0DP = NINT(RBUF(IAD+1)) 
          IAD = IAD+1
c-------
c
        BUFLY%ILAW      = ILAW             
        BUFLY%IMAT      = IMAT             
        BUFLY%IEOS      = IEOS             
        BUFLY%IVISC     = IVISC            
        BUFLY%IPORO     = IPORO            
        BUFLY%NFAIL     = NFAIL            
        BUFLY%NVAR_MAT  = NVAR_MAT  
        BUFLY%NVAR_EOS  = NVAR_EOS
        BUFLY%NVARTMP   = NVARTMP
        BUFLY%NVARTMP_EOS = NVARTMP_EOS
        BUFLY%NVAR_VISC = NVAR_VISC        
        BUFLY%NVAR_LOC  = NVAR_LOC         
        BUFLY%NVAR_LAY  = NVAR_LAY         
        BUFLY%NPTT      = NPTT
c
        BUFLY%LY_DMG    = LY_DMG                  
        BUFLY%LY_GAMA   = LY_GAMA                 
        BUFLY%LY_DIRA   = LY_DIRA                 
        BUFLY%LY_DIRB   = LY_DIRB                 
        BUFLY%LY_CRKDIR = LY_CRKDIR              
        BUFLY%LY_PLAPT  = LY_PLAPT                 
        BUFLY%LY_SIGPT  = LY_SIGPT                 
        BUFLY%LY_HOURG  = LY_HOURG
        BUFLY%LY_UELR   = LY_UELR
        BUFLY%LY_UELR1  = LY_UELR1
        BUFLY%LY_OFFPG  = LY_OFFPG
        BUFLY%LY_OFF    = LY_OFF 
c
        BUFLY%L_OFF     = L_OFF    
        BUFLY%L_GAMA    = L_GAMA   
        BUFLY%L_STRA    = L_STRA   
        BUFLY%L_FRAC    = L_FRAC   
        BUFLY%L_BFRAC   = L_BFRAC   
        BUFLY%L_EINT    = L_EINT   
        BUFLY%L_EINS    = L_EINS   
        BUFLY%L_RHO     = L_RHO         
        BUFLY%L_DP_DRHO = L_DP_DRHO     
        BUFLY%L_QVIS    = L_QVIS   
        BUFLY%L_DELTAX  = L_DELTAX 
        BUFLY%L_VOL     = L_VOL    
        BUFLY%L_EPSA    = L_EPSA   
        BUFLY%L_EPSD    = L_EPSD   
        BUFLY%L_EPSQ    = L_EPSQ   
        BUFLY%L_EPSF    = L_EPSF   
        BUFLY%L_PLA     = L_PLA    
        BUFLY%L_WPLA    = L_WPLA           
        BUFLY%L_TEMP    = L_TEMP        
        BUFLY%L_TB      = L_TB     
        BUFLY%L_RK      = L_RK     
        BUFLY%L_RE      = L_RE     
        BUFLY%L_VK      = L_VK     
        BUFLY%L_SF      = L_SF     
        BUFLY%L_ROB     = L_ROB    
        BUFLY%L_DAM     = L_DAM    
        BUFLY%L_DSUM    = L_DSUM   
        BUFLY%L_DGLO    = L_DGLO     
        BUFLY%L_CRAK    = L_CRAK   
        BUFLY%L_ANG     = L_ANG      
        BUFLY%L_EPE     = L_EPE    
        BUFLY%L_EPC     = L_EPC    
        BUFLY%L_XST     = L_XST    
        BUFLY%L_SSP     = L_SSP    
        BUFLY%L_Z       = L_Z      
        BUFLY%L_VISC    = L_VISC   
        BUFLY%L_SIGL    = L_SIGL   
        BUFLY%L_SIGV    = L_SIGV   
        BUFLY%L_SIGA    = L_SIGA   
        BUFLY%L_SIGB    = L_SIGB   
        BUFLY%L_SIGC    = L_SIGC        
        BUFLY%L_SIGD    = L_SIGD        
        BUFLY%L_SIGF    = L_SIGF   
        BUFLY%L_SIG     = L_SIG    
        BUFLY%L_SIGPLY  = L_SIGPLY    
        BUFLY%L_FOR     = L_FOR    
        BUFLY%L_MOM     = L_MOM
        BUFLY%L_THK     = L_THK    
        BUFLY%L_SMSTR   = L_SMSTR    
        BUFLY%L_DMG     = L_DMG  
        BUFLY%L_FORTH   = L_FORTH    
        BUFLY%L_EINTTH  = L_EINTTH
        BUFLY%L_SEQ     = L_SEQ
        BUFLY%L_JAC_I   = L_JAC_I
        BUFLY%L_FAC_YLD = L_FAC_YLD
        BUFLY%L_ABURN   = L_ABURN 
        BUFLY%L_MU      = L_MU 
        BUFLY%L_PLANL   = L_PLANL
        BUFLY%L_EPSDNL  = L_EPSDNL
        BUFLY%L_DMGSCL  = L_DMGSCL
        BUFLY%L_TSAIWU  = L_TSAIWU
        BUFLY%L_PIJ     = L_PIJ 
        BUFLY%L_VOL0DP  = L_VOL0DP 
c-------------------------------------------------
        IF (IGTYP == 51 .OR. IGTYP == 52) THEN
          NPTT = ELBUF_STR%BUFLY(IL)%NPTT
        ELSE
          NPTT = ELBUF_STR%NPTT
        ENDIF
        ALLOCATE(ELBUF_STR%BUFLY(IL)%LBUF(NPTR,NPTS,NPTT),STAT=ERR)   
        ALLOCATE(ELBUF_STR%BUFLY(IL)%MAT (NPTR,NPTS,NPTT),STAT=ERR)       
        ALLOCATE(ELBUF_STR%BUFLY(IL)%FAIL(NPTR,NPTS,NPTT),STAT=ERR)   
        ALLOCATE(ELBUF_STR%BUFLY(IL)%PROP(NPTR,NPTS,NPTT),STAT=ERR)   
        ALLOCATE(ELBUF_STR%BUFLY(IL)%EOS (NPTR,NPTS,NPTT),STAT=ERR)   
        ALLOCATE(ELBUF_STR%BUFLY(IL)%VISC(NPTR,NPTS,NPTT),STAT=ERR)   
        ALLOCATE(ELBUF_STR%BUFLY(IL)%PORO(NPTR,NPTS,NPTT),STAT=ERR)   
        IF(IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP ==52))
     .   ALLOCATE(ELBUF_STR%BUFLY(IL)%LBUF_DIR(NPTT),STAT=ERR)
c-------------------------------------------------
c-------------------------------------------------
C       allocation & read of layer variables
c-------------------------------------------------
        call my_alloc(BUFLY%DMG, NEL*LY_DMG, "BUFLY%DMG")
        BUFLY%DMG(1:NEL*LY_DMG)  = RBUF(IAD+1:IAD+NEL*LY_DMG)                 
            IAD = IAD+NEL*LY_DMG                                                
        call my_alloc(BUFLY%GAMA, NEL*LY_GAMA, "BUFLY%GAMA")
          BUFLY%GAMA(1:NEL*LY_GAMA) = RBUF(IAD+1:IAD+NEL*LY_GAMA)            
            IAD = IAD+NEL*LY_GAMA                                                
        IF(IDRAPE == 0 .OR. (IDRAPE > 0 .AND. IGTYP == 17)) THEN
          call my_alloc(BUFLY%DIRA, NEL*LY_DIRA, "BUFLY%DIRA")
          BUFLY%DIRA(1:NEL*LY_DIRA) = RBUF(IAD+1:IAD+NEL*LY_DIRA)                 
            IAD = IAD+NEL*LY_DIRA                                                
          call my_alloc(BUFLY%DIRB, NEL*LY_DIRB, "BUFLY%DIRB")
          BUFLY%DIRB(1:NEL*LY_DIRB) = RBUF(IAD+1:IAD+NEL*LY_DIRB)                 
            IAD = IAD+NEL*LY_DIRB    
        ENDIF                                                
        call my_alloc(BUFLY%CRKDIR, NEL*LY_CRKDIR, "BUFLY%CRKDIR")
          BUFLY%CRKDIR(1:NEL*LY_CRKDIR) = RBUF(IAD+1:IAD+NEL*LY_CRKDIR)                 
            IAD = IAD+NEL*LY_CRKDIR                                               

        IF (NPG > 1) THEN
          IF (NLAY > 1) THEN
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
          call my_alloc(BUFLY%PLAPT, LEN_PLAPT, "BUFLY%PLAPT")
            BUFLY%PLAPT(1:LEN_PLAPT) = RBUF(IAD+1:IAD+LEN_PLAPT)
              IAD = IAD + LEN_PLAPT
          call my_alloc(BUFLY%SIGPT, LEN_SIGPT, "BUFLY%SIGPT")
            BUFLY%SIGPT(1:LEN_SIGPT) = RBUF(IAD+1:IAD+LEN_SIGPT)
              IAD = IAD+LEN_SIGPT
        ELSEIF (NPG == 1) THEN
C
C         must point after  => LBUF%PLA and LBUF%SIG
C
        ENDIF
        call my_alloc(BUFLY%HOURG, NEL*LY_HOURG, "BUFLY%HOURG")
          BUFLY%HOURG(1:NEL*LY_HOURG)  = RBUF(IAD+1:IAD+NEL*LY_HOURG)                 
            IAD = IAD+NEL*LY_HOURG
        call my_alloc(BUFLY%UELR, NEL*LY_UELR, "BUFLY%UELR")
          BUFLY%UELR(1:NEL*LY_UELR)  = RBUF(IAD+1:IAD+NEL*LY_UELR)                 
            IAD = IAD+NEL*LY_UELR
        call my_alloc(BUFLY%UELR1, NEL*LY_UELR1, "BUFLY%UELR1")
          BUFLY%UELR1(1:NEL*LY_UELR1)  = RBUF(IAD+1:IAD+NEL*LY_UELR1)                 
            IAD = IAD+NEL*LY_UELR1
        call my_alloc(BUFLY%OFFPG, NEL*LY_OFFPG, "BUFLY%OFFPG")
          BUFLY%OFFPG(1:NEL*LY_OFFPG)  = RBUF(IAD+1:IAD+NEL*LY_OFFPG)                 
            IAD = IAD+NEL*LY_OFFPG
        call my_alloc(BUFLY%OFF, NEL*LY_OFF, "BUFLY%OFF")
          BUFLY%OFF(1:NEL*LY_OFF)  = RBUF(IAD+1:IAD+NEL*LY_OFF)                 
            IAD = IAD+NEL*LY_OFF
c-------------------------------------------------
C       allocation & read of local buffer variables
c-------------------------------------------------
       DO IR = 1,NPTR                                                            
        DO IS = 1,NPTS                                                            
         DO IT = 1,NPTT                                                           
          LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)                             
c--------
          LBUF%MLAW   = NINT(RBUF(IAD+1))
            IAD = IAD+1                                                  
          LBUF%lawID  = NINT(RBUF(IAD+1))
            IAD = IAD+1                                                  
c--------
          call my_alloc(LBUF%OFF, NEL*L_OFF, "LBUF%OFF")
            LBUF%OFF(1:NEL*L_OFF)    = RBUF(IAD+1:IAD+NEL*L_OFF)                 
              IAD = IAD+NEL*L_OFF                                                
          call my_alloc(LBUF%GAMA, NEL*L_GAMA, "LBUF%GAMA")
            LBUF%GAMA(1:NEL*L_GAMA)  = RBUF(IAD+1:IAD+NEL*L_GAMA)                
             IAD = IAD+NEL*L_GAMA                                                
          call my_alloc(LBUF%STRA, NEL*L_STRA, "LBUF%STRA")
            LBUF%STRA(1:NEL*L_STRA) = RBUF(IAD+1:IAD+NEL*L_STRA)                 
              IAD = IAD+NEL*L_STRA                                               
          call my_alloc(LBUF%FRAC, NEL*L_FRAC, "LBUF%FRAC")
            LBUF%FRAC(1:NEL*L_FRAC)  = RBUF(IAD+1:IAD+NEL*L_FRAC)                
              IAD = IAD+NEL*L_FRAC
          call my_alloc(LBUF%BFRAC, NEL*L_BFRAC, "LBUF%BFRAC")
            LBUF%BFRAC(1:NEL*L_BFRAC)   = RBUF(IAD+1:IAD+NEL*L_BFRAC)               
              IAD = IAD+NEL*L_BFRAC                                               
          call my_alloc(LBUF%EINT, NEL*L_EINT, "LBUF%EINT")
            LBUF%EINT(1:NEL*L_EINT)   = RBUF(IAD+1:IAD+NEL*L_EINT)               
              IAD = IAD+NEL*L_EINT                                               
          call my_alloc(LBUF%EINS, NEL*L_EINS, "LBUF%EINS")
            LBUF%EINS(1:NEL*L_EINS)   = RBUF(IAD+1:IAD+NEL*L_EINS)               
              IAD = IAD+NEL*L_EINS                                               
          call my_alloc(LBUF%RHO, NEL*L_RHO, "LBUF%RHO")
            LBUF%RHO(1:NEL*L_RHO)    = RBUF(IAD+1:IAD+NEL*L_RHO)                 
              IAD = IAD+NEL*L_RHO    
          call my_alloc(LBUF%DP_DRHO, NEL*L_DP_DRHO, "LBUF%DP_DRHO")
           LBUF%DP_DRHO(1:NEL*L_DP_DRHO)=RBUF(IAD+1:IAD+NEL*L_DP_DRHO)          
              IAD = IAD+NEL*L_DP_DRHO                                            
          call my_alloc(LBUF%QVIS, NEL*L_QVIS, "LBUF%QVIS")
            LBUF%QVIS(1:NEL*L_QVIS)   = RBUF(IAD+1:IAD+NEL*L_QVIS)               
              IAD = IAD+NEL*L_QVIS                                               
          call my_alloc(LBUF%DELTAX, NEL*L_DELTAX, "LBUF%DELTAX")
            LBUF%DELTAX(1:NEL*L_DELTAX)=RBUF(IAD+1:IAD+NEL*L_DELTAX)             
              IAD = IAD+NEL*L_DELTAX      
          call my_alloc(LBUF%VOL, NEL*L_VOL, "LBUF%VOL")
            LBUF%VOL(1:NEL*L_VOL)    = RBUF(IAD+1:IAD+NEL*L_VOL)                 
              IAD = IAD+NEL*L_VOL                                                
          call my_alloc(LBUF%EPSA, NEL*L_EPSA, "LBUF%EPSA")
            LBUF%EPSA(1:NEL*L_EPSA)  = RBUF(IAD+1:IAD+NEL*L_EPSA)                
              IAD = IAD+NEL*L_EPSA                                               
          call my_alloc(LBUF%EPSD, NEL*L_EPSD, "LBUF%EPSD")
            LBUF%EPSD(1:NEL*L_EPSD)   = RBUF(IAD+1:IAD+NEL*L_EPSD)               
              IAD = IAD+NEL*L_EPSD                                               
          call my_alloc(LBUF%EPSQ, NEL*L_EPSQ, "LBUF%EPSQ")
            LBUF%EPSQ(1:NEL*L_EPSQ)   = RBUF(IAD+1:IAD+NEL*L_EPSQ)               
              IAD = IAD+NEL*L_EPSQ                                               
          call my_alloc(LBUF%EPSF, NEL*L_EPSF, "LBUF%EPSF")
            LBUF%EPSF(1:NEL*L_EPSF)  = RBUF(IAD+1:IAD+NEL*L_EPSF)                
              IAD = IAD+NEL*L_EPSF                                               
          call my_alloc(LBUF%PLA, NEL*L_PLA, "LBUF%PLA")
            LBUF%PLA(1:NEL*L_PLA)    = RBUF(IAD+1:IAD+NEL*L_PLA)                 
              IAD = IAD+NEL*L_PLA    
          call my_alloc(LBUF%WPLA, NEL*L_WPLA, "LBUF%WPLA")
            LBUF%WPLA(1:NEL*L_WPLA)    = RBUF(IAD+1:IAD+NEL*L_WPLA)                 
              IAD = IAD+NEL*L_WPLA                                                            
          call my_alloc(LBUF%TEMP, NEL*L_TEMP, "LBUF%TEMP")
            LBUF%TEMP(1:NEL*L_TEMP)   = RBUF(IAD+1:IAD+NEL*L_TEMP)               
              IAD = IAD+NEL*L_TEMP                                               
          call my_alloc(LBUF%TB, NEL*L_TB, "LBUF%TB")
            LBUF%TB(1:NEL*L_TB)   = RBUF(IAD+1:IAD+NEL*L_TB)                     
              IAD = IAD+NEL*L_TB                                                 
          call my_alloc(LBUF%RK, NEL*L_RK, "LBUF%RK")
            LBUF%RK(1:NEL*L_RK)   = RBUF(IAD+1:IAD+NEL*L_RK)                     
              IAD = IAD+NEL*L_RK                                                 
          call my_alloc(LBUF%RE, NEL*L_RE, "LBUF%RE")
            LBUF%RE(1:NEL*L_RE)   = RBUF(IAD+1:IAD+NEL*L_RE)                     
              IAD = IAD+NEL*L_RE                                                 
          call my_alloc(LBUF%VK, NEL*L_VK, "LBUF%VK")
            LBUF%VK(1:NEL*L_VK)   = RBUF(IAD+1:IAD+NEL*L_VK)                     
              IAD = IAD+NEL*L_VK                                                 
          call my_alloc(LBUF%SF, NEL*L_SF, "LBUF%SF")
            LBUF%SF(1:NEL*L_SF)   = RBUF(IAD+1:IAD+NEL*L_SF)                     
              IAD = IAD+NEL*L_SF                                                 
          call my_alloc(LBUF%ROB, NEL*L_ROB, "LBUF%ROB")
            LBUF%ROB(1:NEL*L_ROB)    = RBUF(IAD+1:IAD+NEL*L_ROB)                 
              IAD = IAD+NEL*L_ROB                                                
          call my_alloc(LBUF%DAM, NEL*L_DAM, "LBUF%DAM")
            LBUF%DAM(1:NEL*L_DAM)  = RBUF(IAD+1:IAD+NEL*L_DAM)                   
              IAD = IAD+NEL*L_DAM                                                
          call my_alloc(LBUF%DSUM, NEL*L_DSUM, "LBUF%DSUM")
            LBUF%DSUM(1:NEL*L_DSUM)  = RBUF(IAD+1:IAD+NEL*L_DSUM)                
              IAD = IAD+NEL*L_DSUM                                               
          call my_alloc(LBUF%DGLO, NEL*L_DGLO, "LBUF%DGLO")
            LBUF%DGLO(1:NEL*L_DGLO)  = RBUF(IAD+1:IAD+NEL*L_DGLO)                
              IAD = IAD+NEL*L_DGLO                                               
          call my_alloc(LBUF%CRAK, NEL*L_CRAK, "LBUF%CRAK")
            LBUF%CRAK(1:NEL*L_CRAK)  = RBUF(IAD+1:IAD+NEL*L_CRAK)                
              IAD = IAD+NEL*L_CRAK                                               
          call my_alloc(LBUF%ANG, NEL*L_ANG, "LBUF%ANG")
            LBUF%ANG(1:NEL*L_ANG)  = RBUF(IAD+1:IAD+NEL*L_ANG)                   
              IAD = IAD+NEL*L_ANG                                                
          call my_alloc(LBUF%EPE, NEL*L_EPE, "LBUF%EPE")
            LBUF%EPE(1:NEL*L_EPE)  = RBUF(IAD+1:IAD+NEL*L_EPE)                   
              IAD = IAD+NEL*L_EPE                                                
          call my_alloc(LBUF%EPC, NEL*L_EPC, "LBUF%EPC")
            LBUF%EPC(1:NEL*L_EPC)  = RBUF(IAD+1:IAD+NEL*L_EPC)                   
              IAD = IAD+NEL*L_EPC                                                
          call my_alloc(LBUF%XST, NEL*L_XST, "LBUF%XST")
            LBUF%XST(1:NEL*L_XST)  = RBUF(IAD+1:IAD+NEL*L_XST)                   
              IAD = IAD+NEL*L_XST                                                
          call my_alloc(LBUF%SSP, NEL*L_SSP, "LBUF%SSP")
            LBUF%SSP(1:NEL*L_SSP)  = RBUF(IAD+1:IAD+NEL*L_SSP)                   
              IAD = IAD+NEL*L_SSP                                                
          call my_alloc(LBUF%Z, NEL*L_Z, "LBUF%Z")
            LBUF%Z(1:NEL*L_Z)  = RBUF(IAD+1:IAD+NEL*L_Z)                         
              IAD = IAD+NEL*L_Z                                                  
          call my_alloc(LBUF%VISC, NEL*L_VISC, "LBUF%VISC")
            LBUF%VISC(1:NEL*L_VISC)  = RBUF(IAD+1:IAD+NEL*L_VISC)                
              IAD = IAD+NEL*L_VISC                                               
          call my_alloc(LBUF%SIGL, NEL*L_SIGL, "LBUF%SIGL")
            LBUF%SIGL(1:NEL*L_SIGL)  = RBUF(IAD+1:IAD+NEL*L_SIGL)                
              IAD = IAD+NEL*L_SIGL                                               
          call my_alloc(LBUF%SIGV, NEL*L_SIGV, "LBUF%SIGV")
            LBUF%SIGV(1:NEL*L_SIGV)  = RBUF(IAD+1:IAD+NEL*L_SIGV)                
              IAD = IAD+NEL*L_SIGV                                               
          call my_alloc(LBUF%SIGA, NEL*L_SIGA, "LBUF%SIGA")
            LBUF%SIGA(1:NEL*L_SIGA) = RBUF(IAD+1:IAD+NEL*L_SIGA)                 
              IAD = IAD+NEL*L_SIGA                                               
          call my_alloc(LBUF%SIGB, NEL*L_SIGB, "LBUF%SIGB")
            LBUF%SIGB(1:NEL*L_SIGB) = RBUF(IAD+1:IAD+NEL*L_SIGB)                 
              IAD = IAD+NEL*L_SIGB                                               
          call my_alloc(LBUF%SIGC, NEL*L_SIGC, "LBUF%SIGC")
            LBUF%SIGC(1:NEL*L_SIGC) = RBUF(IAD+1:IAD+NEL*L_SIGC)                 
              IAD = IAD+NEL*L_SIGC                                               
          call my_alloc(LBUF%SIGD, NEL*L_SIGD, "LBUF%SIGD")
            LBUF%SIGD(1:NEL*L_SIGD) = RBUF(IAD+1:IAD+NEL*L_SIGD)                 
              IAD = IAD+NEL*L_SIGD                                               
          call my_alloc(LBUF%SIGF, NEL*L_SIGF, "LBUF%SIGF")
            LBUF%SIGF(1:NEL*L_SIGF)  = RBUF(IAD+1:IAD+NEL*L_SIGF)                
              IAD = IAD+NEL*L_SIGF                                               
          call my_alloc(LBUF%SIG, NEL*L_SIG, "LBUF%SIG")
            LBUF%SIG(1:NEL*L_SIG)  = RBUF(IAD+1:IAD+NEL*L_SIG)                   
              IAD = IAD+NEL*L_SIG                                                
          call my_alloc(LBUF%SIGPLY, NEL*L_SIGPLY, "LBUF%SIGPLY")
            LBUF%SIGPLY(1:NEL*L_SIGPLY)  = RBUF(IAD+1:IAD+NEL*L_SIGPLY)                   
              IAD = IAD+NEL*L_SIGPLY                                                
          call my_alloc(LBUF%FOR, NEL*L_FOR, "LBUF%FOR")
            LBUF%FOR(1:NEL*L_FOR)  = RBUF(IAD+1:IAD+NEL*L_FOR)                
              IAD = IAD+NEL*L_FOR
          call my_alloc(LBUF%MOM, NEL*L_MOM, "LBUF%MOM")
            LBUF%MOM(1:NEL*L_MOM)  = RBUF(IAD+1:IAD+NEL*L_MOM)                
              IAD = IAD+NEL*L_MOM
          call my_alloc(LBUF%THK, NEL*L_THK, "LBUF%THK")
            LBUF%THK(1:NEL*L_THK)  = RBUF(IAD+1:IAD+NEL*L_THK)                
              IAD = IAD+NEL*L_THK
          call my_alloc(LBUF%SMSTR, NEL*L_SMSTR, "LBUF%SMSTR")
            LBUF%SMSTR(1:NEL*L_SMSTR)  = RBUF(IAD+1:IAD+NEL*L_SMSTR)                
              IAD = IAD+NEL*L_SMSTR
          call my_alloc(LBUF%DMG, NEL*L_DMG, "LBUF%DMG")
            LBUF%DMG(1:NEL*L_DMG)  = RBUF(IAD+1:IAD+NEL*L_DMG)                
              IAD = IAD+NEL*L_DMG
          call my_alloc(LBUF%FORTH, NEL*L_FORTH, "LBUF%FORTH")
            LBUF%FORTH(1:NEL*L_FORTH)  = RBUF(IAD+1:IAD+NEL*L_FORTH)                
              IAD = IAD+NEL*L_FORTH
          call my_alloc(LBUF%EINTTH, NEL*L_EINTTH, "LBUF%EINTTH")
            LBUF%EINTTH(1:NEL*L_EINTTH)  = RBUF(IAD+1:IAD+NEL*L_EINTTH)                
              IAD = IAD+NEL*L_EINTTH
          call my_alloc(LBUF%SEQ, NEL*L_SEQ, "LBUF%SEQ")
            LBUF%SEQ(1:NEL*L_SEQ)  = RBUF(IAD+1:IAD+NEL*L_SEQ)
              IAD = IAD+NEL*L_SEQ
          call my_alloc(LBUF%JAC_I, NEL*L_JAC_I, "LBUF%JAC_I")
            LBUF%JAC_I(1:NEL*L_JAC_I)  = RBUF(IAD+1:IAD+NEL*L_JAC_I)
              IAD = IAD+NEL*L_JAC_I
          call my_alloc(LBUF%FAC_YLD, NEL*L_FAC_YLD, "LBUF%FAC_YLD")
            LBUF%FAC_YLD(1:NEL*L_FAC_YLD)  = RBUF(IAD+1:IAD+NEL*L_FAC_YLD)
              IAD = IAD+NEL*L_FAC_YLD
          call my_alloc(LBUF%ABURN, NEL*L_ABURN, "LBUF%ABURN")
            LBUF%ABURN(1:NEL*L_ABURN)   = RBUF(IAD+1:IAD+NEL*L_ABURN)               
              IAD = IAD+NEL*L_ABURN                                              
          call my_alloc(LBUF%MU, NEL*L_MU, "LBUF%MU")
            LBUF%MU(1:NEL*L_MU)   = RBUF(IAD+1:IAD+NEL*L_MU)               
              IAD = IAD+NEL*L_MU    
          call my_alloc(LBUF%PLANL, NEL*L_PLANL, "LBUF%PLANL")
            LBUF%PLANL(1:NEL*L_PLANL)  = RBUF(IAD+1:IAD+NEL*L_PLANL)               
              IAD = IAD+NEL*L_PLANL            
          call my_alloc(LBUF%EPSDNL, NEL*L_EPSDNL, "LBUF%EPSDNL")
            LBUF%EPSDNL(1:NEL*L_EPSDNL) = RBUF(IAD+1:IAD+NEL*L_EPSDNL)               
              IAD = IAD+NEL*L_EPSDNL   
          call my_alloc(LBUF%DMGSCL, NEL*L_DMGSCL, "LBUF%DMGSCL")
            LBUF%DMGSCL(1:NEL*L_DMGSCL) = RBUF(IAD+1:IAD+NEL*L_DMGSCL)               
              IAD = IAD+NEL*L_DMGSCL  
          call my_alloc(LBUF%TSAIWU, NEL*L_TSAIWU, "LBUF%TSAIWU")
            LBUF%TSAIWU(1:NEL*L_TSAIWU) = RBUF(IAD+1:IAD+NEL*L_TSAIWU)               
              IAD = IAD+NEL*L_TSAIWU 
          call my_alloc(LBUF%PIJ, NEL*L_PIJ, "LBUF%PIJ")
            LBUF%PIJ(1:NEL*L_PIJ)   = RBUF(IAD+1:IAD+NEL*L_PIJ)               
              IAD = IAD+NEL*L_PIJ 
C---------- size of DP ---             
          call my_alloc(LBUF%VOL0DP, NEL*L_VOL0DP, "LBUF%VOL0DP")
              SDP_RBUF = SDP_RBUF + NEL*L_VOL0DP
!----------------------------------------------------------
         ENDDO !next IT
        ENDDO !next IS
       ENDDO !next IR
       IF(IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
         DO IT = 1, NPTT                
          LBUF_DIR => ELBUF_STR%BUFLY(IL)%LBUF_DIR(IT)
          call my_alloc(LBUF_DIR%DIRA, NEL*LY_DIRA, "LBUF_DIR%DIRA")
          LBUF_DIR%DIRA(1:NEL*LY_DIRA) = RBUF(IAD+1:IAD+NEL*LY_DIRA)         
          IAD = IAD + NEL*LY_DIRA                               
          call my_alloc(LBUF_DIR%DIRB, NEL*LY_DIRB, "LBUF_DIR%DIRB")
          LBUF_DIR%DIRB(1:NEL*LY_DIRB) = RBUF(IAD+1:IAD+NEL*LY_DIRB)         
          IAD = IAD + NEL*LY_DIRB  
        ENDDO !next IT
      ENDIF 
      ENDDO !next IL 
C
      IF (ITY == 3 .OR. ITY == 7) THEN
cc        NPTTOT  = 0
        DO IL=1,ELBUF_STR%NLAY
          NPTTOT = NPTTOT + NPG*ELBUF_STR%BUFLY(IL)%NPTT
        ENDDO
        IF (NPT == 0) NPTTOT = NPT
      ENDIF
C
C layer mean variable (NPG = 1)
C
      IF (NPG == 1) THEN
        IF (NLAY > 1) THEN
          DO IL = 1,NLAY
            BUFLY => ELBUF_STR%BUFLY(IL)
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPTT = BUFLY%NPTT
              DO IT=1,NPTT
                LBUF  => BUFLY%LBUF(1,1,IT)
                BUFLY%PLAPT(1:NEL*LY_PLAPT) => LBUF%PLA(1:NEL*L_PLA)
                BUFLY%SIGPT(1:NEL*LY_SIGPT) => LBUF%SIG(1:NEL*L_SIG)
              ENDDO
            ELSE
              LBUF  => BUFLY%LBUF(1,1,1)
              BUFLY%PLAPT(1:NEL*LY_PLAPT) => LBUF%PLA(1:NEL*L_PLA)
              BUFLY%SIGPT(1:NEL*LY_SIGPT) => LBUF%SIG(1:NEL*L_SIG)
            ENDIF
          ENDDO
        ELSE    ! this part seems to have pointer issues
          BUFLY => ELBUF_STR%BUFLY(1)
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN
            NPTT = BUFLY%NPTT
            DO IPT = 1,NPTT
              LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)
              LEN1 = 1+(IPT-1)*NEL*LY_PLAPT
              LEN2 = IPT*NEL*LY_PLAPT
              BUFLY%PLAPT(LEN1:LEN2) => LBUF%PLA(1:NEL*L_PLA)
              LEN1 = 1+(IPT-1)*NEL*LY_SIGPT
              LEN2 = IPT*NEL*LY_SIGPT
              BUFLY%SIGPT(LEN1:LEN2) => LBUF%SIG(1:NEL*L_SIG)
            ENDDO
          ELSE
            DO IPT = 1,NPT
              LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)
              LEN1 = 1+(IPT-1)*NEL*LY_PLAPT
              LEN2 = IPT*NEL*LY_PLAPT
              BUFLY%PLAPT(LEN1:LEN2) => LBUF%PLA(1:NEL*L_PLA)
              LEN1 = 1+(IPT-1)*NEL*LY_SIGPT
              LEN2 = IPT*NEL*LY_SIGPT
              BUFLY%SIGPT(LEN1:LEN2) => LBUF%SIG(1:NEL*L_SIG)
            ENDDO
         ENDIF
        ENDIF
      ENDIF
c
c-----    Material buffer - per integration point                                                                  
c
      DO IL = 1, NLAY                                                     
        NUVAR   = ELBUF_STR%BUFLY(IL)%NVAR_MAT                              
        NVARTMP = ELBUF_STR%BUFLY(IL)%NVARTMP
        IF (IGTYP == 51 .OR. IGTYP == 52) THEN
          NPTT = ELBUF_STR%BUFLY(IL)%NPTT
        ELSE
          NPTT = ELBUF_STR%NPTT
        ENDIF
        DO IR = 1,NPTR                                                    
          DO IS = 1,NPTS                                                  
            DO IT = 1,NPTT                                                
              MATBUF => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)                 
              call my_alloc(MATBUF%VAR, NEL*NUVAR, "MATBUF%VAR")
              MATBUF%VAR(1:NEL*NUVAR) = RBUF(IAD+1:IAD+NEL*NUVAR)                          
              IAD = IAD+NEL*NUVAR                                         
              call my_alloc(MATBUF%VARTMP, NEL*NVARTMP, "MATBUF%VARTMP")
              MATBUF%VARTMP(1:NEL*NVARTMP) = 0
c-----                                                                           
            ENDDO                                                             
          ENDDO                                                              
        ENDDO                                                             
      ENDDO                                                               
c-------------------------------
C         failure model variables                            
c-------------------------------
      DO IL = 1, NLAY  
        IMAT  = ELBUF_STR%BUFLY(IL)%IMAT                                                    
        NFAIL = ELBUF_STR%BUFLY(IL)%NFAIL                                     
        IF (IGTYP == 51 .OR. IGTYP == 52) THEN
          NPTT = ELBUF_STR%BUFLY(IL)%NPTT
        ELSE
          NPTT = ELBUF_STR%NPTT
        ENDIF
        DO IR = 1,NPTR                                                     
          DO IS = 1,NPTS                                                   
            DO IT = 1,NPTT                                                 
              ALLOCATE (ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(NFAIL),
     .                                                       STAT=ERR)        
              DO K = 1,NFAIL                                               
                FLOC=>ELBUF_STR%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(K)  
         
                FLOC%ILAWF = RBUF(IAD+1)                                   
                IAD = IAD+1    

                FLOC%IDFAIL = RBUF(IAD+1)
                IAD = IAD+1

                NUVAR = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%NVAR = NUVAR
   
                LF_DAM = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%LF_DAM = LF_DAM 

                LF_DAMMX = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%LF_DAMMX = LF_DAMMX     

                LF_DAMINI = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%LF_DAMINI = LF_DAMINI

                LF_TDEL = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%LF_TDEL = LF_TDEL                                           

                LF_INDX = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%LF_INDX = LF_INDX                                           

                LF_OFF = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%LF_OFF = LF_OFF                                           
                                       
                call my_alloc(FLOC%VAR, NEL*NUVAR, "FLOC%VAR")
                FLOC%VAR(1:NEL*NUVAR) = RBUF(IAD+1:IAD+NEL*NUVAR)                        
                IAD = IAD + NEL*NUVAR                                        
                                       
                NVARTMP = MAT_ELEM%MAT_PARAM(IMAT)%FAIL(K)%NVARTMP                                      
                call my_alloc(FLOC%VARTMP, NEL*NVARTMP, "FLOC%VARTMP")
                FLOC%VARTMP(1:NEL*NVARTMP) = 1                 

                call my_alloc(FLOC%DAM, NEL*LF_DAM, "FLOC%DAM")
                FLOC%DAM(1:NEL*LF_DAM) = RBUF(IAD+1:IAD+NEL*LF_DAM)                        
                IAD = IAD + NEL*LF_DAM    

                call my_alloc(FLOC%DAMMX, NEL*LF_DAMMX, "FLOC%DAMMX")
                FLOC%DAMMX(1:NEL*LF_DAMMX) = RBUF(IAD+1:IAD+NEL*LF_DAMMX)                        
                IAD = IAD + NEL*LF_DAMMX     

                call my_alloc(FLOC%DAMINI, NEL*LF_DAMINI, "FLOC%DAMINI")
                FLOC%DAMINI(1:NEL*LF_DAMINI) = RBUF(IAD+1:IAD+NEL*LF_DAMINI)                        
                IAD = IAD + NEL*LF_DAMINI 

                call my_alloc(FLOC%TDEL, NEL*LF_TDEL, "FLOC%TDEL")
                FLOC%TDEL(1:NEL*LF_TDEL) = RBUF(IAD+1:IAD+NEL*LF_TDEL)                        
                IAD = IAD + NEL*LF_TDEL                                    

                call my_alloc(FLOC%INDX, NEL*LF_INDX, "FLOC%INDX")
                FLOC%INDX(1:NEL*LF_INDX) = RBUF(IAD+1:IAD+NEL*LF_INDX)                        
                IAD = IAD + NEL*LF_INDX                                    

                call my_alloc(FLOC%OFF, NEL*LF_OFF, "FLOC%OFF")
                FLOC%OFF(1:NEL*LF_OFF) = RBUF(IAD+1:IAD+NEL*LF_OFF)                        
                IAD = IAD + NEL*LF_OFF                                    
              ENDDO                                                         
            ENDDO                                                          
          ENDDO                                                            
        ENDDO                                                              
      ENDDO                                                                
c-------------------------------
C         Viscosity model variables                            
c-------------------------------
      DO IL = 1, NLAY                                                      
        NUVAR = ELBUF_STR% BUFLY(IL)%NVAR_VISC                           
        IF (IGTYP == 51 .OR. IGTYP == 52) THEN
          NPTT = ELBUF_STR%BUFLY(IL)%NPTT
        ELSE
          NPTT = ELBUF_STR%NPTT
        ENDIF
        DO IR = 1,NPTR                                                    
          DO IS = 1,NPTS                                                  
            DO IT = 1,NPTT                                                
              VISCBUF => ELBUF_STR%BUFLY(IL)%VISC(IR,IS,IT)                 
              call my_alloc(VISCBUF%VAR, NEL*NUVAR, "VISCBUF%VAR")
              VISCBUF%VAR(1:NEL*NUVAR) = RBUF(IAD+1:IAD+NEL*NUVAR)                          
              IAD = IAD+NEL*NUVAR                                         
c-----                                                                           
            ENDDO                                                             
          ENDDO                                                              
        ENDDO                                                             
      ENDDO
c-------------------------------
C         Non-local regularization in the shell thickness                
c-------------------------------
      IF ((ITY==3).OR.(ITY==7)) THEN
        ! Number of integration points in the shell thickness
        NPTT = ELBUF_STR%NPTT
        IF ((INLOC>0).AND.(NPTT>1)) THEN
          ! Allocation of the non-local buffer
          ALLOCATE(ELBUF_STR%NLOC(NPTR,NPTS))
          ! Number of non-local nodes in the thickness
          IF (NPTT>2) THEN
            NONL = NPTT+1
          ELSE
            NONL = NPTT
          ENDIF
          ! Loop over the elements
          DO IR = 1, NPTR
            DO IS = 1, NPTS
              ! Pointing the non-local buffer
              BUFNL => ELBUF_STR%NLOC(IR,IS)
              ! Non-local in-thickness masses
              call my_alloc(BUFNL%MASSTH, NEL, NONL, "BUFNL%MASSTH")
              ! Non-local in-thickness cumulated variables
              call my_alloc(BUFNL%UNLTH, NEL, NONL, "BUFNL%UNLTH")
              ! Non-local in-thickness velocities
              call my_alloc(BUFNL%VNLTH, NEL, NONL, "BUFNL%VNLTH")
              ! Non-local in-thickness forces
              call my_alloc(BUFNL%FNLTH, NEL, NONL, "BUFNL%FNLTH")
              ! Copying the buffer
              DO I = 1,NEL
                ! Nodal masses
                BUFNL%MASSTH(I,1:NONL) = RBUF(IAD+1:IAD+NONL)
                IAD = IAD + NONL
                ! Non-local in-thickness cumulated variables
                BUFNL%UNLTH(I,1:NONL)  = RBUF(IAD+1:IAD+NONL)
                IAD = IAD + NONL
                ! Non-local in-thickness velocities
                BUFNL%VNLTH(I,1:NONL)  = RBUF(IAD+1:IAD+NONL)
                IAD = IAD + NONL
                ! Non-local in-thickness forces
                BUFNL%FNLTH(I,1:NONL)  = RBUF(IAD+1:IAD+NONL)
                IAD = IAD + NONL
              ENDDO
            ENDDO
          ENDDO
        ENDIF
c-------------------------------
C         Non-local regularization in the thickshell thickness                
c-------------------------------
      ELSEIF (ITY==1) THEN
        ! Number of integration points in the shell thickness
        NPTT = ELBUF_STR%NLAY
        IF ((INLOC>0).AND.(NPTT>1)) THEN
          ! Allocation of the non-local buffer
          ALLOCATE(ELBUF_STR%NLOCTS(NPTR,NPTS), STAT=ERR)
          ! Number of non-local nodes in the thickness
          NONL = NPTT+1
          ! Loop over the elements
          DO IR = 1, NPTR
            DO IS = 1, NPTS
              ! Pointing the non-local buffer
              BUFNLTS => ELBUF_STR%NLOCTS(IR,IS)
              ! Non-local in-thickness masses
              call my_alloc(BUFNLTS%MASSTH, NEL, NONL, "BUFNLTS%MASSTH")
              ! Non-local in-thickness cumulated variables
              call my_alloc(BUFNLTS%UNLTH, NEL, NONL, "BUFNLTS%UNLTH")
              ! Non-local in-thickness velocities
              call my_alloc(BUFNLTS%VNLTH, NEL, NONL, "BUFNLTS%VNLTH")
              ! Non-local in-thickness forces
              call my_alloc(BUFNLTS%FNLTH, NEL, NONL, "BUFNLTS%FNLTH")
              ! Copying the buffer
              DO I = 1,NEL
                ! Nodal masses
                BUFNLTS%MASSTH(I,1:NONL) = RBUF(IAD+1:IAD+NONL)
                IAD = IAD + NONL
                ! Non-local in-thickness cumulated variables
                BUFNLTS%UNLTH(I,1:NONL)  = RBUF(IAD+1:IAD+NONL)
                IAD = IAD + NONL
                ! Non-local in-thickness velocities
                BUFNLTS%VNLTH(I,1:NONL)  = RBUF(IAD+1:IAD+NONL)
                IAD = IAD + NONL
                ! Non-local in-thickness forces
                BUFNLTS%FNLTH(I,1:NONL)  = RBUF(IAD+1:IAD+NONL)
                IAD = IAD + NONL
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
c-------------------------------
C         EoS buffer
c-------------------------------
      DO IL = 1, NLAY 
        NVAR_EOS = ELBUF_STR%BUFLY(IL)%NVAR_EOS
        NVARTMP_EOS = ELBUF_STR%BUFLY(IL)%NVARTMP_EOS
        IF (IGTYP == 51 .OR. IGTYP == 52) THEN
          NPTT = ELBUF_STR%BUFLY(IL)%NPTT
        ELSE
          NPTT = ELBUF_STR%NPTT
        ENDIF
        DO IR = 1,NPTR                                                    
          DO IS = 1,NPTS                                                  
            DO IT = 1,NPTT                                                           
              ALLOCATE( ELBUF_STR%BUFLY(IL)%EOS(IR,IS,IT)%VAR(NEL*NVAR_EOS), STAT=ERR)
              ALLOCATE( ELBUF_STR%BUFLY(IL)%EOS(IR,IS,IT)%VARTMP(NEL*NVARTMP_EOS), STAT=ERR)
              ELBUF_STR%BUFLY(IL)%EOS(IR,IS,IT)%VAR(1:NEL*NVAR_EOS) = RBUF(IAD+1:IAD+NEL*NVAR_EOS)                          
              ELBUF_STR%BUFLY(IL)%EOS(IR,IS,IT)%VARTMP(1:NEL*NVARTMP_EOS) = 0
              IAD = IAD+NEL*NVAR_EOS
            ENDDO                                                             
          ENDDO                                                              
        ENDDO                                                             
      ENDDO     
            
c-------------------------------------------------
C     INTER LAYER VARIABLES
c-------------------------------------------------
      L_EPS   = 3
      L_SIG   = 3
      LY_EINT = 1  
      LY_COUNT= 1  
c
      DO IL = 1,ELBUF_STR%NINTLAY                                         
        INTLAY => ELBUF_STR%INTLAY(IL)
c
C       Interlayer parameters : 4
        INTLAY%ILAW  = NINT(RBUF(IAD+1))                         
          IAD = IAD+1                                                  
        INTLAY%IMAT  = NINT(RBUF(IAD+1))                         
          IAD = IAD+1                                                  
        INTLAY%NFAIL = NINT(RBUF(IAD+1))                        
          IAD = IAD+1                                                  
        INTLAY%NVAR_MAT = NINT(RBUF(IAD+1))               
          IAD = IAD+1                                                  
        INTLAY%NVARTMP = NINT(RBUF(IAD+1))               
          IAD = IAD+1                                                  
c-------------------------------------------------
C     Interlayer :  allocation & read of local buffer variables
c-------------------------------------------------
        call my_alloc(INTLAY%EINT, NEL*LY_EINT, "INTLAY%EINT")
          INTLAY%EINT(1:NEL*LY_EINT) = RBUF(IAD+1:IAD+NEL*LY_EINT)
            IAD = IAD+NEL*LY_EINT                                              
        call my_alloc(INTLAY%COUNT, NEL*LY_COUNT, "INTLAY%COUNT")
          INTLAY%COUNT(1:NEL*LY_COUNT) = RBUF(IAD+1:IAD+NEL*LY_COUNT)
            IAD = IAD+NEL*LY_COUNT                                              
c
        DO IR = 1,NPTR                                                            
          DO IS = 1,NPTS                                                            
            ILBUF => ELBUF_STR%INTLAY(IL)%ILBUF(IR,IS)   
c--------
            call my_alloc(ILBUF%EPS, NEL*L_EPS, "ILBUF%EPS")
              ILBUF%EPS(1:NEL*L_EPS)    = RBUF(IAD+1:IAD+NEL*L_EPS)                 
                IAD = IAD+NEL*L_EPS                                                
            call my_alloc(ILBUF%SIG, NEL*L_SIG, "ILBUF%SIG")
              ILBUF%SIG(1:NEL*L_SIG)    = RBUF(IAD+1:IAD+NEL*L_SIG)               
                IAD = IAD+NEL*L_SIG                                              
!----------------------------------------------------------
          ENDDO !next IS
        ENDDO !next IR
      ENDDO !next IL                                                                     
c
c---  Interlayer Material buffer - per integration point                                                                  
c
      DO IL = 1, NINTLAY                                                     
        NUVAR = ELBUF_STR%INTLAY(IL)%NVAR_MAT                              
        NVARTMP = ELBUF_STR%BUFLY(IL)%NVARTMP                              
        DO IR = 1,NPTR                                                    
          DO IS = 1,NPTS                                                  
              MATBUF => ELBUF_STR%INTLAY(IL)%MAT(IR,IS)                 
              call my_alloc(MATBUF%VAR, NEL*NUVAR, "MATBUF%VAR")
              MATBUF%VAR(1:NEL*NUVAR) = RBUF(IAD+1:IAD+NEL*NUVAR)                          
              IAD = IAD+NEL*NUVAR                                         
              call my_alloc(MATBUF%VARTMP, NEL*NVARTMP, "MATBUF%VARTMP")
              MATBUF%VARTMP(1:NEL*NVARTMP) = 0                    
          ENDDO                                                              
        ENDDO                                                             
      ENDDO                                                               
c-------------------------------
C     Interlayer failure model variables                            
c-------------------------------
      DO IL = 1, NINTLAY
        IMAT  = ELBUF_STR%INTLAY(IL)%IMAT                                                     
        NFAIL = ELBUF_STR%INTLAY(IL)%NFAIL                                     
        DO IR = 1,NPTR                                                     
          DO IS = 1,NPTS                                                   
              ALLOCATE (ELBUF_STR%INTLAY(IL)%FAIL(IR,IS)%FLOC(NFAIL),                                                    
     .                                                       STAT=ERR)        
              DO K = 1,NFAIL                                               
                FLOC=>ELBUF_STR%INTLAY(IL)%FAIL(IR,IS)%FLOC(K)           
                FLOC%ILAWF = RBUF(IAD+1)                                   
                IAD = IAD+1       
                FLOC%IDFAIL = RBUF(IAD+1)
                IAD = IAD+1                                         
                NUVAR = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%NVAR = NUVAR                                          
                call my_alloc(FLOC%VAR, NEL*NUVAR, "FLOC%VAR")
                FLOC%VAR(1:NEL*NUVAR) = RBUF(IAD+1:IAD+NEL*NUVAR)                        
                IAD = IAD+NEL*NUVAR                                        
                NVARTMP =  MAT_ELEM%MAT_PARAM(IMAT)%FAIL(K)%NVARTMP                                      
                call my_alloc(FLOC%VARTMP, NEL*NVARTMP, "FLOC%VARTMP")
                FLOC%VARTMP(1:NEL*NVARTMP)=1                
              ENDDO                                                         
          ENDDO                                                            
        ENDDO                                                              
      ENDDO                                                                
c-------------------------------
C     Interlayer Viscosity model variables                            
c-------------------------------
!      DO IL = 1, NINTLAY                                                      
!        NUVAR = ELBUF_STR%INTLAY(IL)%NVAR_VISC                                
!        DO IR = 1,NPTR                                                    
!          DO IS = 1,NPTS                                                  
!              VISCBUF => ELBUF_STR%INTLAY(IL)%VISC(IR,IS)                 
!              ALLOCATE (VISCBUF%VAR(NEL*NUVAR), STAT=ERR)                        
!              VISCBUF%VAR(1:NEL*NUVAR) = RBUF(IAD+1:IAD+NEL*NUVAR)                          
!              IAD = IAD+NEL*NUVAR                                         
!c-----                                                                           
!          ENDDO                                                              
!        ENDDO                                                             
!      ENDDO                                                                
c-------------------------------------------------
C       allocation of global buffer variables
c-------------------------------------------------
      GBUF  => ELBUF_STR%GBUF     
C      
      GBUF%NVAR_GLOB = NINT(RBUF(IAD+1))         
        IAD = IAD + 1                            
c
      G_NOFF =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                            
      G_IERR =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                            
      G_OFF  =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                               
      G_GAMA =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                             
      G_SMSTR=  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                              
      G_HOURG=  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                             
      G_BFRAC=  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                               
      G_EINT =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                            
      G_EINS =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                          
      G_RHO  =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                          
      G_QVIS =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                             
      G_DELTAX=  NINT(RBUF(IAD+1))        
        IAD = IAD + 1                            
      G_VOL  =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                             
      G_EPSD =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                            
      G_EPSQ =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                            
      G_PLA  =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1         
      G_WPLA =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                           
      G_TEMP =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                            
      G_TB   =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                           
      G_RK   =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                                 
      G_RE   =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                             
      G_SIG  =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1  
      G_FOR  =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                                
      G_MOM  =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                              
      G_THK  =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                             
      G_TAG22 =  NINT(RBUF(IAD+1))         
       IAD = IAD + 1                                
      G_STRA =  NINT(RBUF(IAD+1))         
       IAD = IAD + 1                                
      G_SIGI =  NINT(RBUF(IAD+1))         
       IAD = IAD + 1                                
      G_DMG =  NINT(RBUF(IAD+1))         
       IAD = IAD + 1                                
      G_FORPG =  NINT(RBUF(IAD+1))         
       IAD = IAD + 1                                
      G_MOMPG =  NINT(RBUF(IAD+1))         
       IAD = IAD + 1
C
      G_FORPGPINCH =  NINT(RBUF(IAD+1))         
       IAD = IAD + 1 
      G_MOMPGPINCH =  NINT(RBUF(IAD+1))         
       IAD = IAD + 1   
      G_EPGPINCHXZ =  NINT(RBUF(IAD+1))         
       IAD = IAD + 1
      G_EPGPINCHYZ =  NINT(RBUF(IAD+1))         
       IAD = IAD + 1
      G_EPGPINCHZZ =  NINT(RBUF(IAD+1))         
       IAD = IAD + 1
C                                 
      G_STRPG =  NINT(RBUF(IAD+1))         
       IAD = IAD + 1                                
      G_UELR =  NINT(RBUF(IAD+1))
       IAD = IAD + 1                   
      G_UELR1 =  NINT(RBUF(IAD+1))
       IAD = IAD + 1
      G_DAMDL =  NINT(RBUF(IAD+1))
       IAD = IAD + 1
      G_FORTH  =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1   
      G_EINTTH  =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1   
      G_FILL   =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1
      G_SEQ  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_STRW  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_STRWPG  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_THK_I  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_JAC_I  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_DT     =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_ISMS   =  NINT(RBUF(IAD+1))
        IAD = IAD + 1        
      G_BPRELD =  NINT(RBUF(IAD+1))
        IAD = IAD + 1   
      G_ABURN=  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                               
      G_MU=  NINT(RBUF(IAD+1))         
        IAD = IAD + 1  
      G_PLANL=  NINT(RBUF(IAD+1))         
        IAD = IAD + 1  
      G_EPSDNL= NINT(RBUF(IAD+1))         
        IAD = IAD + 1    
      G_DMGSCL= NINT(RBUF(IAD+1))         
        IAD = IAD + 1   
      G_TSAIWU= NINT(RBUF(IAD+1))         
        IAD = IAD + 1   
      G_TEMPG =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1       
      G_COR_NF  =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                               
      G_COR_FR  =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                               
      G_COR_XR  =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1 
      G_MAXFRAC=  NINT(RBUF(IAD+1))         
        IAD = IAD + 1  
      G_MAXEPS=  NINT(RBUF(IAD+1))         
        IAD = IAD + 1                              
      G_BETAORTH   =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_AMU=  NINT(RBUF(IAD+1))         
        IAD = IAD + 1
      G_SH_IOFFSET=  NINT(RBUF(IAD+1))         
        IAD = IAD + 1
      G_EINT_DISTOR=  NINT(RBUF(IAD+1))         
        IAD = IAD + 1
      G_FOR_G=  NINT(RBUF(IAD+1))         
        IAD = IAD + 1
      G_FORPG_G=  NINT(RBUF(IAD+1))         
        IAD = IAD + 1
c---
C 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
c---
      G_AREA  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_SKEW  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_LENGTH  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_TOTDEPL  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_TOTROT  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_FOREP  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_MOMEP  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_DEP_IN_TENS  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_DEP_IN_COMP  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_ROT_IN_TENS  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_ROT_IN_COMP  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_POSX  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_POSY  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_POSZ  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_POSXX  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_POSYY  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_POSZZ  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_YIELD  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_LENGTH_ERR  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_DV  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_DFS  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_SKEW_ERR  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_E6  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_RUPTCRIT  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_MASS  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_V_REPCVT  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_VR_REPCVT  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_NUVAR  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_NUVARN =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_DEFINI =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_FORINI =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_INIFRIC =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_STRHG   =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_ETOTSH  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
c---
      G_DT_PITER_OLD  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1

      G_DT_PITER=0
      IF(ISNOD==10)THEN
        IF(G_DT_PITER_OLD/=0)THEN
          G_DT_PITER=G_DT_PITER_OLD
        ELSEIF(IDT1TET10 > 1)THEN
          G_DT_PITER=30 ! New dimensioning and allocation of DT_PITER due to /DT1TET10/N for THIS run
          NEWLEN    =NEWLEN+30*NEL
        END IF
      END IF
C
      G_SKEW_ID  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_GAMA_R =  NINT(RBUF(IAD+1))         
        IAD = IAD + 1
      G_YIELD_IN_COMP = NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_XXOLD_IN_COMP = NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_RBODY_NODE  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
c---
C seatbelt elements
c---
      G_SLIPRING_ID  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_SLIPRING_FRAM_ID  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_SLIPRING_STRAND  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_RETRACTOR_ID  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_RINGSLIP  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_ADD_NODE  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_UPDATE    =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_FRAM_FACTOR   =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_INTVAR   =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_IDT_TSH  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
C ---H3D/TMAX in BUFLEN         
      G_TM_YIELD  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_TM_SEQ  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_TM_EINT  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_TM_DMG  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_TM_SIG  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
      G_TM_STRA  =  NINT(RBUF(IAD+1))
        IAD = IAD + 1
c---
      GBUF%G_NOFF   = G_NOFF     
      GBUF%G_IERR   = G_IERR     
      GBUF%G_OFF    = G_OFF      
      GBUF%G_GAMA   = G_GAMA     
      GBUF%G_SMSTR  = G_SMSTR    
      GBUF%G_HOURG  = G_HOURG    
      GBUF%G_BFRAC  = G_BFRAC    
      GBUF%G_EINT   = G_EINT     
      GBUF%G_EINS   = G_EINS     
      GBUF%G_RHO    = G_RHO      
      GBUF%G_QVIS   = G_QVIS     
      GBUF%G_DELTAX = G_DELTAX   
      GBUF%G_VOL    = G_VOL      
      GBUF%G_EPSD   = G_EPSD     
      GBUF%G_EPSQ   = G_EPSQ
      GBUF%G_PLA    = G_PLA  
      GBUF%G_WPLA   = G_WPLA    
      GBUF%G_TEMP   = G_TEMP     
      GBUF%G_TB     = G_TB       
      GBUF%G_RK     = G_RK       
      GBUF%G_RE     = G_RE       
      GBUF%G_SIG    = G_SIG      
      GBUF%G_FOR    = G_FOR      
      GBUF%G_MOM    = G_MOM      
      GBUF%G_THK    = G_THK      
      GBUF%G_TAG22  = G_TAG22     
      GBUF%G_STRA   = G_STRA     
      GBUF%G_SIGI   = G_SIGI     
      GBUF%G_DMG    = G_DMG     
      GBUF%G_FORPG  = G_FORPG     
      GBUF%G_MOMPG  = G_MOMPG
C
      GBUF%G_FORPGPINCH  = G_FORPGPINCH     
      GBUF%G_MOMPGPINCH  = G_MOMPGPINCH 
      GBUF%G_EPGPINCHXZ  = G_EPGPINCHXZ  
      GBUF%G_EPGPINCHYZ  = G_EPGPINCHYZ 
      GBUF%G_EPGPINCHZZ  = G_EPGPINCHZZ 
C      
      GBUF%G_STRPG  = G_STRPG
      GBUF%G_UELR   = G_UELR
      GBUF%G_UELR1  = G_UELR1
      GBUF%G_DAMDL  = G_DAMDL
      GBUF%G_FORTH  = G_FORTH      
      GBUF%G_EINTTH = G_EINTTH      
      GBUF%G_FILL   = G_FILL
      GBUF%G_SEQ    = G_SEQ
      GBUF%G_STRW   = G_STRW    
      GBUF%G_STRWPG = G_STRWPG    
      GBUF%G_THK_I  = G_THK_I      
      GBUF%G_JAC_I  = G_JAC_I    
      GBUF%G_DT     = G_DT
      GBUF%G_ISMS   = G_ISMS        
      GBUF%G_BPRELD = G_BPRELD 
      GBUF%G_ABURN  = G_ABURN  
      GBUF%G_MU     = G_MU   
      GBUF%G_PLANL  = G_PLANL
      GBUF%G_EPSDNL = G_EPSDNL 
      GBUF%G_DMGSCL = G_DMGSCL
      GBUF%G_TSAIWU = G_TSAIWU      
      GBUF%G_TEMPG  = G_TEMPG     
      GBUF%G_COR_NF = G_COR_NF
      GBUF%G_COR_FR = G_COR_FR
      GBUF%G_COR_XR = G_COR_XR
      GBUF%G_MAXFRAC= G_MAXFRAC
      GBUF%G_MAXEPS = G_MAXEPS
      GBUF%G_BETAORTH  = G_BETAORTH
      GBUF%G_AMU    = G_AMU
      GBUF%G_SH_IOFFSET  = G_SH_IOFFSET
      GBUF%G_EINT_DISTOR = G_EINT_DISTOR
      GBUF%G_FOR_G = G_FOR_G
      GBUF%G_FORPG_G = G_FORPG_G
c---
C 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
c---
      GBUF%G_AREA        = G_AREA
      GBUF%G_SKEW        = G_SKEW
      GBUF%G_LENGTH      = G_LENGTH
      GBUF%G_TOTDEPL     = G_TOTDEPL
      GBUF%G_TOTROT      = G_TOTROT
      GBUF%G_FOREP       = G_FOREP
      GBUF%G_MOMEP       = G_MOMEP
      GBUF%G_DEP_IN_TENS = G_DEP_IN_TENS
      GBUF%G_DEP_IN_COMP = G_DEP_IN_COMP
      GBUF%G_ROT_IN_TENS = G_ROT_IN_TENS
      GBUF%G_ROT_IN_COMP = G_ROT_IN_COMP
      GBUF%G_POSX        = G_POSX
      GBUF%G_POSY        = G_POSY
      GBUF%G_POSZ        = G_POSZ
      GBUF%G_POSXX       = G_POSXX
      GBUF%G_POSYY       = G_POSYY
      GBUF%G_POSZZ       = G_POSZZ
      GBUF%G_YIELD       = G_YIELD
      GBUF%G_LENGTH_ERR  = G_LENGTH_ERR
      GBUF%G_DV          = G_DV
      GBUF%G_DFS         = G_DFS
      GBUF%G_SKEW_ERR    = G_SKEW_ERR
      GBUF%G_E6          = G_E6
      GBUF%G_RUPTCRIT    = G_RUPTCRIT
      GBUF%G_MASS        = G_MASS
      GBUF%G_V_REPCVT    = G_V_REPCVT
      GBUF%G_VR_REPCVT   = G_VR_REPCVT
      GBUF%G_NUVAR       = G_NUVAR
      GBUF%G_NUVARN      = G_NUVARN
      GBUF%G_DEFINI      = G_DEFINI
      GBUF%G_FORINI      = G_FORINI
      GBUF%G_INIFRIC     = G_INIFRIC
      GBUF%G_STRHG       = G_STRHG
      GBUF%G_ETOTSH      = G_ETOTSH
      GBUF%G_DT_PITER    = G_DT_PITER
      GBUF%G_SKEW_ID     = G_SKEW_ID
      GBUF%G_GAMA_R      = G_GAMA_R
      GBUF%G_YIELD_IN_COMP = G_YIELD_IN_COMP
      GBUF%G_XXOLD_IN_COMP = G_XXOLD_IN_COMP
      GBUF%G_RBODY_NODE  = G_RBODY_NODE
c---
C seatbelt elements
c---
      GBUF%G_SLIPRING_ID = G_SLIPRING_ID
      GBUF%G_SLIPRING_FRAM_ID = G_SLIPRING_FRAM_ID
      GBUF%G_SLIPRING_STRAND = G_SLIPRING_STRAND
      GBUF%G_RETRACTOR_ID= G_RETRACTOR_ID
      GBUF%G_RINGSLIP = G_RINGSLIP
      GBUF%G_ADD_NODE = G_ADD_NODE
      GBUF%G_UPDATE   = G_UPDATE
      GBUF%G_FRAM_FACTOR   = G_FRAM_FACTOR
      GBUF%G_INTVAR   = G_INTVAR
      GBUF%G_IDT_TSH  = G_IDT_TSH
C ---H3D/TMAX in BUFLEN         
      GBUF%G_TM_YIELD = G_TM_YIELD  
      GBUF%G_TM_SEQ   = G_TM_SEQ    
      GBUF%G_TM_EINT  = G_TM_EINT   
      GBUF%G_TM_DMG   = G_TM_DMG    
      GBUF%G_TM_SIG   = G_TM_SIG    
      GBUF%G_TM_STRA  = G_TM_STRA
     
C------------  ETOTSH Work array used for free shear locking of S17
      call my_alloc(ELBUF_STR%GBUF%ETOTSH, NEL*G_ETOTSH, "ELBUF_STR%GBUF%ETOTSH")
      GBUF%ETOTSH = ZERO  
C------------  Work array GAMA_R : co-rotational local sys
      call my_alloc(ELBUF_STR%GBUF%GAMA_R, NEL*G_GAMA_R, "ELBUF_STR%GBUF%GAMA_R")
      GBUF%GAMA_R = ZERO  
c---
      call my_alloc(ELBUF_STR%GBUF%NOFF, NEL*G_NOFF, "ELBUF_STR%GBUF%NOFF")
        GBUF%NOFF(1:NEL*G_NOFF)  = NINT(RBUF(IAD+1:IAD+NEL*G_NOFF))                            
          IAD = IAD+NEL*G_NOFF                                                          
      call my_alloc(ELBUF_STR%GBUF%IERR, NEL*G_IERR, "ELBUF_STR%GBUF%IERR")
        GBUF%IERR(1:NEL*G_IERR)  = NINT(RBUF(IAD+1:IAD+NEL*G_IERR))                            
          IAD = IAD+NEL*G_IERR                                                          
      call my_alloc(ELBUF_STR%GBUF%GAMA, NEL*G_GAMA, "ELBUF_STR%GBUF%GAMA")
        GBUF%GAMA(1:NEL*G_GAMA)  = RBUF(IAD+1:IAD+NEL*G_GAMA)                    
          IAD = IAD+NEL*G_GAMA
C      ALLOCATE (ELBUF_STR%GBUF%SMSTR(NEL*G_SMSTR) ,STAT=ERR)                     
C        GBUF%SMSTR(1:NEL*G_SMSTR) = RBUF(IAD+1:IAD+NEL*G_SMSTR)                  
C          IAD = IAD+NEL*G_SMSTR
      call my_alloc(ELBUF_STR%GBUF%HOURG, NEL*G_HOURG, "ELBUF_STR%GBUF%HOURG")
        GBUF%HOURG(1:NEL*G_HOURG) = RBUF(IAD+1:IAD+NEL*G_HOURG)                  
          IAD = IAD+NEL*G_HOURG                                                  
      call my_alloc(ELBUF_STR%GBUF%TAG22, NEL*G_TAG22, "ELBUF_STR%GBUF%TAG22")
        GBUF%TAG22(1:NEL*G_TAG22) = RBUF(IAD+1:IAD+NEL*G_TAG22)                  
          IAD = IAD+NEL*G_TAG22                 
      call my_alloc(ELBUF_STR%GBUF%STRA, NEL*G_STRA, "ELBUF_STR%GBUF%STRA")
        GBUF%STRA(1:NEL*G_STRA) = RBUF(IAD+1:IAD+NEL*G_STRA)                  
          IAD = IAD+NEL*G_STRA                 
      call my_alloc(ELBUF_STR%GBUF%SIGI, NEL*G_SIGI, "ELBUF_STR%GBUF%SIGI")
        GBUF%SIGI(1:NEL*G_SIGI) = RBUF(IAD+1:IAD+NEL*G_SIGI)                  
          IAD = IAD+NEL*G_SIGI                 
      call my_alloc(ELBUF_STR%GBUF%DMG, NEL*G_DMG, "ELBUF_STR%GBUF%DMG")
        GBUF%DMG(1:NEL*G_DMG) = RBUF(IAD+1:IAD+NEL*G_DMG)                  
          IAD = IAD+NEL*G_DMG                 
      call my_alloc(ELBUF_STR%GBUF%UELR, NEL*G_UELR, "ELBUF_STR%GBUF%UELR")
        GBUF%UELR(1:NEL*G_UELR) = RBUF(IAD+1:IAD+NEL*G_UELR)
          IAD = IAD+NEL*G_UELR
      call my_alloc(ELBUF_STR%GBUF%UELR1, NEL*G_UELR1, "ELBUF_STR%GBUF%UELR1")
        GBUF%UELR1(1:NEL*G_UELR1) = RBUF(IAD+1:IAD+NEL*G_UELR1)
          IAD = IAD+NEL*G_UELR1
      call my_alloc(ELBUF_STR%GBUF%DAMDL, NEL*G_DAMDL, "ELBUF_STR%GBUF%DAMDL")
        GBUF%DAMDL(1:NEL*G_DAMDL) = RBUF(IAD+1:IAD+NEL*G_DAMDL)
          IAD = IAD+NEL*G_DAMDL
      call my_alloc(ELBUF_STR%GBUF%FOR, NEL*G_FOR, "ELBUF_STR%GBUF%FOR")
        GBUF%FOR(1:NEL*G_FOR)   = RBUF(IAD+1:IAD+NEL*G_FOR)                           
          IAD = IAD+NEL*G_FOR                                                         
      call my_alloc(ELBUF_STR%GBUF%MOM, NEL*G_MOM, "ELBUF_STR%GBUF%MOM")
        GBUF%MOM(1:NEL*G_MOM)   = RBUF(IAD+1:IAD+NEL*G_MOM)                           
          IAD = IAD+NEL*G_MOM                                                         
      call my_alloc(ELBUF_STR%GBUF%THK, NEL*G_THK, "ELBUF_STR%GBUF%THK")
        GBUF%THK(1:NEL*G_THK)   = RBUF(IAD+1:IAD+NEL*G_THK)                           
          IAD = IAD+NEL*G_THK 
      call my_alloc(ELBUF_STR%GBUF%STRW, NEL*G_STRW, "ELBUF_STR%GBUF%STRW")
        GBUF%STRW(1:NEL*G_STRW)=RBUF(IAD+1:IAD+NEL*G_STRW)                   
          IAD = IAD+NEL*G_STRW                                  
      call my_alloc(ELBUF_STR%GBUF%THK_I, NEL*G_THK_I, "ELBUF_STR%GBUF%THK_I")
        GBUF%THK_I(1:NEL*G_THK_I)=RBUF(IAD+1:IAD+NEL*G_THK_I)                           
          IAD = IAD+NEL*G_THK_I                                  
      call my_alloc(ELBUF_STR%GBUF%JAC_I, NEL*G_JAC_I, "ELBUF_STR%GBUF%JAC_I")
        GBUF%JAC_I(1:NEL*G_JAC_I)=RBUF(IAD+1:IAD+NEL*G_JAC_I)                           
          IAD = IAD+NEL*G_JAC_I                                  
      call my_alloc(ELBUF_STR%GBUF%DT, NEL*G_DT, "ELBUF_STR%GBUF%DT")
        GBUF%DT(1:NEL*G_DT)=RBUF(IAD+1:IAD+NEL*G_DT)                           
          IAD = IAD+NEL*G_DT
      call my_alloc(ELBUF_STR%GBUF%ISMS, NEL*G_ISMS, "ELBUF_STR%GBUF%ISMS")
        GBUF%ISMS(1:NEL*G_ISMS)=RBUF(IAD+1:IAD+NEL*G_ISMS)                           
          IAD = IAD+NEL*G_ISMS    
      call my_alloc(ELBUF_STR%GBUF%BPRELD, NEL*G_BPRELD, "ELBUF_STR%GBUF%BPRELD")
        GBUF%BPRELD(1:NEL*G_BPRELD)=RBUF(IAD+1:IAD+NEL*G_BPRELD)                           
          IAD = IAD+NEL*G_BPRELD    
      call my_alloc(ELBUF_STR%GBUF%COR_NF, NEL*G_COR_NF, "ELBUF_STR%GBUF%COR_NF")
        GBUF%COR_NF(1:NEL*G_COR_NF)=RBUF(IAD+1:IAD+NEL*G_COR_NF)                           
          IAD = IAD+NEL*G_COR_NF    
      call my_alloc(ELBUF_STR%GBUF%COR_FR, NEL*G_COR_FR, "ELBUF_STR%GBUF%COR_FR")
        GBUF%COR_FR(1:NEL*G_COR_FR)=RBUF(IAD+1:IAD+NEL*G_COR_FR)                           
          IAD = IAD+NEL*G_COR_FR    
      call my_alloc(ELBUF_STR%GBUF%COR_XR, NEL*G_COR_XR, "ELBUF_STR%GBUF%COR_XR")
        GBUF%COR_XR(1:NEL*G_COR_XR)=RBUF(IAD+1:IAD+NEL*G_COR_XR)                           
          IAD = IAD+NEL*G_COR_XR 
      call my_alloc(ELBUF_STR%GBUF%MAXFRAC, NEL*G_MAXFRAC, "ELBUF_STR%GBUF%MAXFRAC")
        GBUF%MAXFRAC(1:NEL*G_MAXFRAC)=RBUF(IAD+1:IAD+NEL*G_MAXFRAC)                           
          IAD = IAD+NEL*G_MAXFRAC   
      call my_alloc(ELBUF_STR%GBUF%MAXEPS, NEL*G_MAXEPS, "ELBUF_STR%GBUF%MAXEPS")
        GBUF%MAXEPS(1:NEL*G_MAXEPS)=RBUF(IAD+1:IAD+NEL*G_MAXEPS)                           
          IAD = IAD+NEL*G_MAXEPS
      call my_alloc(ELBUF_STR%GBUF%BETAORTH, NEL*G_BETAORTH, "ELBUF_STR%GBUF%BETAORTH")
        GBUF%BETAORTH(1:NEL*G_BETAORTH)   = RBUF(IAD+1:IAD+NEL*G_BETAORTH)                         
          IAD = IAD+NEL*G_BETAORTH
      call my_alloc(ELBUF_STR%GBUF%AMU, NEL*G_AMU, "ELBUF_STR%GBUF%AMU")
        GBUF%AMU(1:NEL*G_AMU)=RBUF(IAD+1:IAD+NEL*G_AMU)                           
          IAD = IAD+NEL*G_AMU
      call my_alloc(ELBUF_STR%GBUF%SH_IOFFSET, NEL*G_SH_IOFFSET, "ELBUF_STR%GBUF%SH_IOFFSET")
        GBUF%SH_IOFFSET(1:NEL*G_SH_IOFFSET)=RBUF(IAD+1:IAD+NEL*G_SH_IOFFSET)                           
          IAD = IAD+NEL*G_SH_IOFFSET
      call my_alloc(ELBUF_STR%GBUF%EINT_DISTOR, NEL*G_EINT_DISTOR, "ELBUF_STR%GBUF%EINT_DISTOR")
        GBUF%EINT_DISTOR(1:NEL*G_EINT_DISTOR)=RBUF(IAD+1:IAD+NEL*G_EINT_DISTOR)                           
          IAD = IAD+NEL*G_EINT_DISTOR
      call my_alloc(ELBUF_STR%GBUF%FOR_G, NEL*G_FOR_G, "ELBUF_STR%GBUF%FOR_G")
        GBUF%FOR_G(1:NEL*G_FOR_G)=RBUF(IAD+1:IAD+NEL*G_FOR_G)                           
          IAD = IAD+NEL*G_FOR_G
      call my_alloc(ELBUF_STR%GBUF%WPLA, NEL*G_WPLA, "ELBUF_STR%GBUF%WPLA")
        GBUF%WPLA(1:NEL*G_WPLA)  = RBUF(IAD+1:IAD+NEL*G_WPLA)                         
          IAD = IAD+NEL*G_WPLA    
c-------
      IF (NPG == 1) THEN
        ELBUF_STR%GBUF%FORPG => GBUF%FOR
        ELBUF_STR%GBUF%MOMPG => GBUF%MOM
C
C        ELBUF_STR%GBUF%FORPGPINCH => GBUF%FORPINCH ! [PM] add later
C        ELBUF_STR%GBUF%MOMPGPINCH => GBUF%MOMPINCH ! [PM] add later
C        ELBUF_STR%GBUF%EPGPINCHXZ => GBUF%EPGPINCHXZ ! [PM] add later
C        ELBUF_STR%GBUF%EPGPINCHYZ => GBUF%EPGPINCHYZ ! [PM] add later
C        ELBUF_STR%GBUF%EPGPINCHZZ => GBUF%EPGPINCHZZ ! [PM] add later
C
        IF (G_STRPG<=G_STRA) THEN
          ELBUF_STR%GBUF%STRPG => GBUF%STRA
        ELSE
          call my_alloc(ELBUF_STR%GBUF%STRPG, NEL*G_STRPG, "ELBUF_STR%GBUF%STRPG")
          GBUF%STRPG(1:NEL*G_STRPG) = RBUF(IAD+1:IAD+NEL*G_STRPG)                  
          IAD = IAD+NEL*G_STRPG 
        END IF
        ELBUF_STR%GBUF%FORPG_G => GBUF%FOR_G
      ELSEIF (NPG > 1) THEN
        call my_alloc(ELBUF_STR%GBUF%FORPG, NEL*G_FORPG, "ELBUF_STR%GBUF%FORPG")
          GBUF%FORPG(1:NEL*G_FORPG) = RBUF(IAD+1:IAD+NEL*G_FORPG)                  
            IAD = IAD+NEL*G_FORPG                 
        call my_alloc(ELBUF_STR%GBUF%MOMPG, NEL*G_MOMPG, "ELBUF_STR%GBUF%MOMPG")
          GBUF%MOMPG(1:NEL*G_MOMPG) = RBUF(IAD+1:IAD+NEL*G_MOMPG)                  
            IAD = IAD+NEL*G_MOMPG
C
        call my_alloc(ELBUF_STR%GBUF%FORPGPINCH, NEL*G_FORPGPINCH, "ELBUF_STR%GBUF%FORPGPINCH")
          GBUF%FORPGPINCH(1:NEL*G_FORPGPINCH) = RBUF(IAD+1:IAD+NEL*G_FORPGPINCH)                  
            IAD = IAD+NEL*G_FORPGPINCH                 
        call my_alloc(ELBUF_STR%GBUF%MOMPGPINCH, NEL*G_MOMPGPINCH, "ELBUF_STR%GBUF%MOMPGPINCH")
          GBUF%MOMPG(1:NEL*G_MOMPGPINCH) = RBUF(IAD+1:IAD+NEL*G_MOMPGPINCH)                  
            IAD = IAD+NEL*G_MOMPGPINCH   
        call my_alloc(ELBUF_STR%GBUF%EPGPINCHXZ, NEL*G_EPGPINCHXZ, "ELBUF_STR%GBUF%EPGPINCHXZ")
          GBUF%EPGPINCHXZ(1:NEL*G_EPGPINCHXZ) = RBUF(IAD+1:IAD+NEL*G_EPGPINCHXZ)                  
            IAD = IAD+NEL*G_EPGPINCHXZ 
        call my_alloc(ELBUF_STR%GBUF%EPGPINCHYZ, NEL*G_EPGPINCHYZ, "ELBUF_STR%GBUF%EPGPINCHYZ")
          GBUF%EPGPINCHYZ(1:NEL*G_EPGPINCHYZ) = RBUF(IAD+1:IAD+NEL*G_EPGPINCHYZ)                  
            IAD = IAD+NEL*G_EPGPINCHYZ 
        call my_alloc(ELBUF_STR%GBUF%EPGPINCHZZ, NEL*G_EPGPINCHZZ, "ELBUF_STR%GBUF%EPGPINCHZZ")
          GBUF%EPGPINCHZZ(1:NEL*G_EPGPINCHZZ) = RBUF(IAD+1:IAD+NEL*G_EPGPINCHZZ)                  
            IAD = IAD+NEL*G_EPGPINCHZZ 
C                  
        call my_alloc(ELBUF_STR%GBUF%STRPG, NEL*G_STRPG, "ELBUF_STR%GBUF%STRPG")
          GBUF%STRPG(1:NEL*G_STRPG) = RBUF(IAD+1:IAD+NEL*G_STRPG)                  
            IAD = IAD+NEL*G_STRPG  
        call my_alloc(ELBUF_STR%GBUF%STRWPG, NEL*G_STRWPG, "ELBUF_STR%GBUF%STRWPG")
        GBUF%STRWPG(1:NEL*G_STRWPG)=RBUF(IAD+1:IAD+NEL*G_STRWPG)                   
          IAD = IAD+NEL*G_STRWPG                                  
        call my_alloc(ELBUF_STR%GBUF%FORPG_G, NEL*G_FORPG_G, "ELBUF_STR%GBUF%FORPG_G")
        GBUF%FORPG_G(1:NEL*G_FORPG_G)=RBUF(IAD+1:IAD+NEL*G_FORPG_G)                   
          IAD = IAD+NEL*G_FORPG_G                                  
      ENDIF
c------------------------------------------------
      IF (NPTTOT == 1) THEN  ! NPTTOT = 1 ! global variables are pointing to local ones     
        LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)                                        
        ELBUF_STR%GBUF%BFRAC  => LBUF%BFRAC
        ELBUF_STR%GBUF%OFF    => LBUF%OFF                                            
        ELBUF_STR%GBUF%EINT   => LBUF%EINT                                            
        ELBUF_STR%GBUF%EINS   => LBUF%EINS                                            
        ELBUF_STR%GBUF%RHO    => LBUF%RHO                                             
        ELBUF_STR%GBUF%QVIS   => LBUF%QVIS                                            
        ELBUF_STR%GBUF%DELTAX => LBUF%DELTAX                                          
        ELBUF_STR%GBUF%VOL    => LBUF%VOL                                             
        ELBUF_STR%GBUF%EPSD   => LBUF%EPSD                                            
        ELBUF_STR%GBUF%EPSQ   => LBUF%EPSQ
        ELBUF_STR%GBUF%PLA    => LBUF%PLA                  
        ELBUF_STR%GBUF%WPLA   => LBUF%WPLA                                                
        ELBUF_STR%GBUF%TEMP   => LBUF%TEMP                                            
        ELBUF_STR%GBUF%TB     => LBUF%TB                                              
        ELBUF_STR%GBUF%RK     => LBUF%RK                                              
        ELBUF_STR%GBUF%RE     => LBUF%RE                                              
        ELBUF_STR%GBUF%SIG    => LBUF%SIG                                             
        ELBUF_STR%GBUF%FORTH  => LBUF%FORTH
        ELBUF_STR%GBUF%EINTTH => LBUF%EINTTH                                                                                       
        ELBUF_STR%GBUF%SEQ    => LBUF%SEQ
        ELBUF_STR%GBUF%ABURN  => LBUF%ABURN
        ELBUF_STR%GBUF%MU     => LBUF%MU

      ELSE  ! NPTTOT  /= 1                                                                
        call my_alloc(ELBUF_STR%GBUF%BFRAC, NEL*G_BFRAC, "ELBUF_STR%GBUF%BFRAC")
          GBUF%BFRAC(1:NEL*G_BFRAC) = RBUF(IAD+1:IAD+NEL*G_BFRAC)                        
            IAD = IAD+NEL*G_BFRAC                                                      
        call my_alloc(ELBUF_STR%GBUF%OFF, NEL*G_OFF, "ELBUF_STR%GBUF%OFF")
          GBUF%OFF(1:NEL*G_OFF)   = RBUF(IAD+1:IAD+NEL*G_OFF)                    
            IAD = IAD+NEL*G_OFF                                                  
        call my_alloc(ELBUF_STR%GBUF%EINT, NEL*G_EINT, "ELBUF_STR%GBUF%EINT")
          GBUF%EINT(1:NEL*G_EINT) = RBUF(IAD+1:IAD+NEL*G_EINT)                        
            IAD = IAD+NEL*G_EINT                                                      
        call my_alloc(ELBUF_STR%GBUF%EINS, NEL*G_EINS, "ELBUF_STR%GBUF%EINS")
          GBUF%EINS(1:NEL*G_EINS) = RBUF(IAD+1:IAD+NEL*G_EINS)                        
            IAD = IAD+NEL*G_EINS                                                      
        call my_alloc(ELBUF_STR%GBUF%RHO, NEL*G_RHO, "ELBUF_STR%GBUF%RHO")
          GBUF%RHO(1:NEL*G_RHO)   = RBUF(IAD+1:IAD+NEL*G_RHO)                         
            IAD = IAD+NEL*G_RHO                                                       
        call my_alloc(ELBUF_STR%GBUF%QVIS, NEL*G_QVIS, "ELBUF_STR%GBUF%QVIS")
          GBUF%QVIS(1:NEL*G_QVIS) = RBUF(IAD+1:IAD+NEL*G_QVIS)                        
            IAD = IAD+NEL*G_QVIS                                                      
        call my_alloc(ELBUF_STR%GBUF%DELTAX, NEL*G_DELTAX, "ELBUF_STR%GBUF%DELTAX")
          GBUF%DELTAX(1:NEL*G_DELTAX) = RBUF(IAD+1:IAD+NEL*G_DELTAX)                  
            IAD = IAD+NEL*G_DELTAX                                                    
        call my_alloc(ELBUF_STR%GBUF%VOL, NEL*G_VOL, "ELBUF_STR%GBUF%VOL")
          GBUF%VOL(1:NEL*G_VOL)   = RBUF(IAD+1:IAD+NEL*G_VOL)                         
            IAD = IAD+NEL*G_VOL                                                       
        call my_alloc(ELBUF_STR%GBUF%EPSD, NEL*G_EPSD, "ELBUF_STR%GBUF%EPSD")
          GBUF%EPSD(1:NEL*G_EPSD)   = RBUF(IAD+1:IAD+NEL*G_EPSD)                      
            IAD = IAD+NEL*G_EPSD                                                      
        call my_alloc(ELBUF_STR%GBUF%EPSQ, NEL*G_EPSQ, "ELBUF_STR%GBUF%EPSQ")
          GBUF%EPSQ(1:NEL*G_EPSQ)   = RBUF(IAD+1:IAD+NEL*G_EPSQ)                         
            IAD = IAD+NEL*G_EPSQ                                                       
        call my_alloc(ELBUF_STR%GBUF%PLA, NEL*G_PLA, "ELBUF_STR%GBUF%PLA")
          GBUF%PLA(1:NEL*G_PLA)   = RBUF(IAD+1:IAD+NEL*G_PLA)                         
            IAD = IAD+NEL*G_PLA 
        call my_alloc(ELBUF_STR%GBUF%TEMP, NEL*G_TEMP, "ELBUF_STR%GBUF%TEMP")
          GBUF%TEMP(1:NEL*G_TEMP) = RBUF(IAD+1:IAD+NEL*G_TEMP)                        
            IAD = IAD+NEL*G_TEMP                                                      
        call my_alloc(ELBUF_STR%GBUF%TB, NEL*G_TB, "ELBUF_STR%GBUF%TB")
          GBUF%TB(1:NEL*G_TB) = RBUF(IAD+1:IAD+NEL*G_TB)                              
            IAD = IAD+NEL*G_TB                                                        
        call my_alloc(ELBUF_STR%GBUF%RK, NEL*G_RK, "ELBUF_STR%GBUF%RK")
          GBUF%RK(1:NEL*G_RK) = RBUF(IAD+1:IAD+NEL*G_RK)                              
            IAD = IAD+NEL*G_RK                                                        
        call my_alloc(ELBUF_STR%GBUF%RE, NEL*G_RE, "ELBUF_STR%GBUF%RE")
          GBUF%RE(1:NEL*G_RE) = RBUF(IAD+1:IAD+NEL*G_RE)                              
            IAD = IAD+NEL*G_RE                                                        
        call my_alloc(ELBUF_STR%GBUF%SIG, NEL*G_SIG, "ELBUF_STR%GBUF%SIG")
          GBUF%SIG(1:NEL*G_SIG)   = RBUF(IAD+1:IAD+NEL*G_SIG)                         
            IAD = IAD+NEL*G_SIG                                                       
        call my_alloc(ELBUF_STR%GBUF%FORTH, NEL*G_FORTH, "ELBUF_STR%GBUF%FORTH")
          GBUF%FORTH(1:NEL*G_FORTH)   = RBUF(IAD+1:IAD+NEL*G_FORTH)                         
            IAD = IAD+NEL*G_FORTH 
        call my_alloc(ELBUF_STR%GBUF%EINTTH, NEL*G_EINTTH, "ELBUF_STR%GBUF%EINTTH")
          GBUF%EINTTH(1:NEL*G_EINTTH)   = RBUF(IAD+1:IAD+NEL*G_EINTTH)                         
            IAD = IAD+NEL*G_EINTTH
        call my_alloc(ELBUF_STR%GBUF%SEQ, NEL*G_SEQ, "ELBUF_STR%GBUF%SEQ")
          GBUF%SEQ(1:NEL*G_SEQ)   = RBUF(IAD+1:IAD+NEL*G_SEQ)                         
            IAD = IAD+NEL*G_SEQ
        call my_alloc(ELBUF_STR%GBUF%ABURN, NEL*G_ABURN, "ELBUF_STR%GBUF%ABURN")
          GBUF%ABURN(1:NEL*G_ABURN) = RBUF(IAD+1:IAD+NEL*G_ABURN)                        
            IAD = IAD+NEL*G_ABURN                                                      
        call my_alloc(ELBUF_STR%GBUF%MU, NEL*G_MU, "ELBUF_STR%GBUF%MU")
          GBUF%MU(1:NEL*G_MU) = RBUF(IAD+1:IAD+NEL*G_MU)                        
            IAD = IAD+NEL*G_MU                                                      

      ENDIF  ! NPT   
c
      call my_alloc(ELBUF_STR%GBUF%FILL, NEL*G_FILL, "ELBUF_STR%GBUF%FILL")
        GBUF%FILL(1:NEL*G_FILL)   = RBUF(IAD+1:IAD+NEL*G_FILL)                         
          IAD = IAD+NEL*G_FILL                                                     
c
c---
C 1-D elements (TRUSS, BEAM, SPRING)
c---
      IF(IGTYP == 3 .and. ITY == 5) THEN 
        call my_alloc(ELBUF_STR%GBUF%DMGSCL, NEL*G_DMGSCL, "ELBUF_STR%GBUF%DMGSCL")
          GBUF%DMGSCL(1:NEL*G_DMGSCL)   = RBUF(IAD+1:IAD+NEL*G_DMGSCL)                    
            IAD = IAD+NEL*G_DMGSCL         
      ENDIF
       call my_alloc(ELBUF_STR%GBUF%AREA, NEL*G_AREA, "ELBUF_STR%GBUF%AREA")
        GBUF%AREA(1:NEL*G_AREA) = RBUF(IAD+1:IAD+NEL*G_AREA)
          IAD = IAD+NEL*G_AREA
      call my_alloc(ELBUF_STR%GBUF%SKEW, NEL*G_SKEW, "ELBUF_STR%GBUF%SKEW")
        GBUF%SKEW(1:NEL*G_SKEW) = RBUF(IAD+1:IAD+NEL*G_SKEW)
          IAD = IAD+NEL*G_SKEW
      call my_alloc(ELBUF_STR%GBUF%LENGTH, NEL*G_LENGTH, "ELBUF_STR%GBUF%LENGTH")
        GBUF%LENGTH(1:NEL*G_LENGTH) = RBUF(IAD+1:IAD+NEL*G_LENGTH)
          IAD = IAD+NEL*G_LENGTH
      call my_alloc(ELBUF_STR%GBUF%TOTDEPL, NEL*G_TOTDEPL, "ELBUF_STR%GBUF%TOTDEPL")
        GBUF%TOTDEPL(1:NEL*G_TOTDEPL) = RBUF(IAD+1:IAD+NEL*G_TOTDEPL)
          IAD = IAD+NEL*G_TOTDEPL
      call my_alloc(ELBUF_STR%GBUF%TOTROT, NEL*G_TOTROT, "ELBUF_STR%GBUF%TOTROT")
        GBUF%TOTROT(1:NEL*G_TOTROT) = RBUF(IAD+1:IAD+NEL*G_TOTROT) 
          IAD = IAD+NEL*G_TOTROT
      call my_alloc(ELBUF_STR%GBUF%FOREP, NEL*G_FOREP, "ELBUF_STR%GBUF%FOREP")
        GBUF%FOREP(1:NEL*G_FOREP) = RBUF(IAD+1:IAD+NEL*G_FOREP)
          IAD = IAD+NEL*G_FOREP
      call my_alloc(ELBUF_STR%GBUF%MOMEP, NEL*G_MOMEP, "ELBUF_STR%GBUF%MOMEP")
        GBUF%MOMEP(1:NEL*G_MOMEP) = RBUF(IAD+1:IAD+NEL*G_MOMEP)
          IAD = IAD+NEL*G_MOMEP
      call my_alloc(ELBUF_STR%GBUF%DEP_IN_TENS, NEL*G_DEP_IN_TENS, "ELBUF_STR%GBUF%DEP_IN_TENS")
        GBUF%DEP_IN_TENS(1:NEL*G_DEP_IN_TENS) = RBUF(IAD+1:IAD+NEL*G_DEP_IN_TENS)
          IAD = IAD+NEL*G_DEP_IN_TENS
      call my_alloc(ELBUF_STR%GBUF%DEP_IN_COMP, NEL*G_DEP_IN_COMP, "ELBUF_STR%GBUF%DEP_IN_COMP")
        GBUF%DEP_IN_COMP(1:NEL*G_DEP_IN_COMP) = RBUF(IAD+1:IAD+NEL*G_DEP_IN_COMP)
          IAD = IAD+NEL*G_DEP_IN_COMP
      call my_alloc(ELBUF_STR%GBUF%ROT_IN_TENS, NEL*G_ROT_IN_TENS, "ELBUF_STR%GBUF%ROT_IN_TENS")
        GBUF%ROT_IN_TENS(1:NEL*G_ROT_IN_TENS) = RBUF(IAD+1:IAD+NEL*G_ROT_IN_TENS)
          IAD = IAD+NEL*G_ROT_IN_TENS
      call my_alloc(ELBUF_STR%GBUF%ROT_IN_COMP, NEL*G_ROT_IN_COMP, "ELBUF_STR%GBUF%ROT_IN_COMP")
        GBUF%ROT_IN_COMP(1:NEL*G_ROT_IN_COMP) = RBUF(IAD+1:IAD+NEL*G_ROT_IN_COMP)
          IAD = IAD+NEL*G_ROT_IN_COMP
      call my_alloc(ELBUF_STR%GBUF%POSX, NEL*G_POSX, "ELBUF_STR%GBUF%POSX")
        GBUF%POSX(1:NEL*G_POSX) = RBUF(IAD+1:IAD+NEL*G_POSX)
          IAD = IAD+NEL*G_POSX
      call my_alloc(ELBUF_STR%GBUF%POSY, NEL*G_POSY, "ELBUF_STR%GBUF%POSY")
        GBUF%POSY(1:NEL*G_POSY) = RBUF(IAD+1:IAD+NEL*G_POSY)
          IAD = IAD+NEL*G_POSY
      call my_alloc(ELBUF_STR%GBUF%POSZ, NEL*G_POSZ, "ELBUF_STR%GBUF%POSZ")
        GBUF%POSZ(1:NEL*G_POSZ) = RBUF(IAD+1:IAD+NEL*G_POSZ)
          IAD = IAD+NEL*G_POSZ
      call my_alloc(ELBUF_STR%GBUF%POSXX, NEL*G_POSXX, "ELBUF_STR%GBUF%POSXX")
        GBUF%POSXX(1:NEL*G_POSXX) = RBUF(IAD+1:IAD+NEL*G_POSXX)
          IAD = IAD+NEL*G_POSXX
      call my_alloc(ELBUF_STR%GBUF%POSYY, NEL*G_POSYY, "ELBUF_STR%GBUF%POSYY")
        GBUF%POSYY(1:NEL*G_POSYY) = RBUF(IAD+1:IAD+NEL*G_POSYY)
          IAD = IAD+NEL*G_POSYY
      call my_alloc(ELBUF_STR%GBUF%POSZZ, NEL*G_POSZZ, "ELBUF_STR%GBUF%POSZZ")
        GBUF%POSZZ(1:NEL*G_POSZZ) = RBUF(IAD+1:IAD+NEL*G_POSZZ)
          IAD = IAD+NEL*G_POSZZ
      call my_alloc(ELBUF_STR%GBUF%YIELD, NEL*G_YIELD, "ELBUF_STR%GBUF%YIELD")
        GBUF%YIELD(1:NEL*G_YIELD) = RBUF(IAD+1:IAD+NEL*G_YIELD)
          IAD = IAD+NEL*G_YIELD
      call my_alloc(ELBUF_STR%GBUF%LENGTH_ERR, NEL*G_LENGTH_ERR, "ELBUF_STR%GBUF%LENGTH_ERR")
        GBUF%LENGTH_ERR(1:NEL*G_LENGTH_ERR) = RBUF(IAD+1:IAD+NEL*G_LENGTH_ERR)
          IAD = IAD+NEL*G_LENGTH_ERR
      call my_alloc(ELBUF_STR%GBUF%DV, NEL*G_DV, "ELBUF_STR%GBUF%DV")
        GBUF%DV(1:NEL*G_DV) = RBUF(IAD+1:IAD+NEL*G_DV)
          IAD = IAD+NEL*G_DV
      call my_alloc(ELBUF_STR%GBUF%DFS, NEL*G_DFS, "ELBUF_STR%GBUF%DFS")
        GBUF%DFS(1:NEL*G_DFS) = RBUF(IAD+1:IAD+NEL*G_DFS)
          IAD = IAD+NEL*G_DFS
      call my_alloc(ELBUF_STR%GBUF%SKEW_ERR, NEL*G_SKEW_ERR, "ELBUF_STR%GBUF%SKEW_ERR")
        GBUF%SKEW_ERR(1:NEL*G_SKEW_ERR) = RBUF(IAD+1:IAD+NEL*G_SKEW_ERR)
          IAD = IAD+NEL*G_SKEW_ERR
      call my_alloc(ELBUF_STR%GBUF%E6, NEL*G_E6, "ELBUF_STR%GBUF%E6")
        GBUF%E6(1:NEL*G_E6) = RBUF(IAD+1:IAD+NEL*G_E6)
          IAD = IAD+NEL*G_E6
      call my_alloc(ELBUF_STR%GBUF%RUPTCRIT, NEL*G_RUPTCRIT, "ELBUF_STR%GBUF%RUPTCRIT")
        GBUF%RUPTCRIT(1:NEL*G_RUPTCRIT) = RBUF(IAD+1:IAD+NEL*G_RUPTCRIT)
          IAD = IAD+NEL*G_RUPTCRIT
      call my_alloc(ELBUF_STR%GBUF%MASS, NEL*G_MASS, "ELBUF_STR%GBUF%MASS")
        GBUF%MASS(1:NEL*G_MASS) = RBUF(IAD+1:IAD+NEL*G_MASS)
          IAD = IAD+NEL*G_MASS
      call my_alloc(ELBUF_STR%GBUF%V_REPCVT, NEL*G_V_REPCVT, "ELBUF_STR%GBUF%V_REPCVT")
        GBUF%V_REPCVT(1:NEL*G_V_REPCVT) = RBUF(IAD+1:IAD+NEL*G_V_REPCVT)
          IAD = IAD+NEL*G_V_REPCVT
      call my_alloc(ELBUF_STR%GBUF%VR_REPCVT, NEL*G_VR_REPCVT, "ELBUF_STR%GBUF%VR_REPCVT")
        GBUF%VR_REPCVT(1:NEL*G_VR_REPCVT) = RBUF(IAD+1:IAD+NEL*G_VR_REPCVT)
          IAD = IAD+NEL*G_VR_REPCVT
      call my_alloc(ELBUF_STR%GBUF%VAR, NEL*G_NUVAR, "ELBUF_STR%GBUF%VAR")
        GBUF%VAR(1:NEL*G_NUVAR) = RBUF(IAD+1:IAD+NEL*G_NUVAR)
          IAD = IAD+NEL*G_NUVAR
      call my_alloc(ELBUF_STR%GBUF%VARN, NEL*G_NUVARN, "ELBUF_STR%GBUF%VARN")
        GBUF%VARN(1:NEL*G_NUVARN) = RBUF(IAD+1:IAD+NEL*G_NUVARN)
          IAD = IAD+NEL*G_NUVARN
      call my_alloc(ELBUF_STR%GBUF%DEFINI, NEL*G_DEFINI, "ELBUF_STR%GBUF%DEFINI")
        GBUF%DEFINI(1:NEL*G_DEFINI) = RBUF(IAD+1:IAD+NEL*G_DEFINI)
          IAD = IAD+NEL*G_DEFINI
      call my_alloc(ELBUF_STR%GBUF%FORINI, NEL*G_FORINI, "ELBUF_STR%GBUF%FORINI")
        GBUF%FORINI(1:NEL*G_FORINI) = RBUF(IAD+1:IAD+NEL*G_FORINI)
          IAD = IAD+NEL*G_FORINI
      call my_alloc(ELBUF_STR%GBUF%INIFRIC, NEL*G_INIFRIC, "ELBUF_STR%GBUF%INIFRIC")
        GBUF%INIFRIC(1:NEL*G_INIFRIC) = RBUF(IAD+1:IAD+NEL*G_INIFRIC)
          IAD = IAD+NEL*G_INIFRIC
      call my_alloc(ELBUF_STR%GBUF%STRHG, NEL*G_STRHG, "ELBUF_STR%GBUF%STRHG")
        GBUF%STRHG(1:NEL*G_STRHG) = RBUF(IAD+1:IAD+NEL*G_STRHG)
          IAD = IAD+NEL*G_STRHG
      call my_alloc(ELBUF_STR%GBUF%DT_PITER, NEL*G_DT_PITER, "ELBUF_STR%GBUF%DT_PITER")
      IF(ISNOD==10)THEN
        IF(IDT1TET10 > 1)THEN
          IF(G_DT_PITER_OLD==0)THEN
            GBUF%DT_PITER(1:NEL*G_DT_PITER) = ZERO
          ELSE
            GBUF%DT_PITER(1:NEL*G_DT_PITER) = RBUF(IAD+1:IAD+NEL*G_DT_PITER) ! G_DT_PITER==G_DT_PITER_OLD
          END IF
        END IF
      END IF
      IAD = IAD+NEL*G_DT_PITER_OLD
C needed for batoz shell and /heat/mat  
      IF (NPG == 1) THEN
        ELBUF_STR%GBUF%TEMPG => GBUF%TEMP
      ENDIF
C
      call my_alloc(ELBUF_STR%GBUF%SKEW_ID, NEL*G_SKEW_ID, "ELBUF_STR%GBUF%SKEW_ID")
        GBUF%SKEW_ID(1:NEL*G_SKEW_ID) = NINT(RBUF(IAD+1:IAD+NEL*G_SKEW_ID))
          IAD = IAD+NEL*G_SKEW_ID
      call my_alloc(ELBUF_STR%GBUF%YIELD_IN_COMP, NEL*G_YIELD_IN_COMP, "ELBUF_STR%GBUF%YIELD_IN_COMP")
        GBUF%YIELD_IN_COMP(1:NEL*G_YIELD_IN_COMP) = NINT(RBUF(IAD+1:IAD+NEL*G_YIELD_IN_COMP))
          IAD = IAD+NEL*G_YIELD_IN_COMP
      call my_alloc(ELBUF_STR%GBUF%XXOLD_IN_COMP, NEL*G_XXOLD_IN_COMP, "ELBUF_STR%GBUF%XXOLD_IN_COMP")
        GBUF%XXOLD_IN_COMP(1:NEL*G_XXOLD_IN_COMP) = NINT(RBUF(IAD+1:IAD+NEL*G_XXOLD_IN_COMP))
          IAD = IAD+NEL*G_XXOLD_IN_COMP
      call my_alloc(ELBUF_STR%GBUF%RBODY_NODE, NEL*G_RBODY_NODE, "ELBUF_STR%GBUF%RBODY_NODE")
        GBUF%RBODY_NODE(1:NEL*G_RBODY_NODE) = RBUF(IAD+1:IAD+NEL*G_RBODY_NODE)
          IAD = IAD+NEL*G_RBODY_NODE
C            
c---
C         failure model for beams type 3
c---
      IF (IGTYP == 3 .and. G_NOFF > 0) THEN
        call my_alloc(ELBUF_STR%GBUF%FAIL, 1, "ELBUF_STR%GBUF%FAIL")
          GBUF%FAIL(1)%ILAWF     = NINT(RBUF(IAD+1))
            IAD = IAD + 1
          GBUF%FAIL(1)%IDFAIL    = NINT(RBUF(IAD+1))
            IAD = IAD + 1
          GBUF%FAIL(1)%NVAR      = NINT(RBUF(IAD+1))
            IAD = IAD + 1
          GBUF%FAIL(1)%LF_DAM    = NINT(RBUF(IAD+1))
            IAD = IAD + 1
          GBUF%FAIL(1)%LF_DAMMX  = NINT(RBUF(IAD+1))
            IAD = IAD + 1
          GBUF%FAIL(1)%LF_DAMINI = NINT(RBUF(IAD+1))
            IAD = IAD + 1
          GBUF%FAIL(1)%LF_TDEL   = NINT(RBUF(IAD+1))
            IAD = IAD + 1
          GBUF%FAIL(1)%LF_INDX   = NINT(RBUF(IAD+1))
            IAD = IAD + 1
          GBUF%FAIL(1)%LF_OFF    = NINT(RBUF(IAD+1))
            IAD = IAD + 1

          ALLOCATE(ELBUF_STR%GBUF%FAIL(1)%VAR(GBUF%FAIL(1)%NVAR*NEL) ,STAT=ERR)
            GBUF%FAIL(1)%VAR(:) = RBUF(IAD+1:IAD+NEL*GBUF%FAIL(1)%NVAR)
              IAD = IAD + NEL*GBUF%FAIL(1)%NVAR
          ALLOCATE(ELBUF_STR%GBUF%FAIL(1)%DAM(GBUF%FAIL(1)%LF_DAM*NEL) ,STAT=ERR)
            GBUF%FAIL(1)%DAM(:) = RBUF(IAD+1:IAD+NEL*GBUF%FAIL(1)%LF_DAM)
              IAD = IAD + NEL*GBUF%FAIL(1)%LF_DAM
          ALLOCATE(ELBUF_STR%GBUF%FAIL(1)%DAMMX(GBUF%FAIL(1)%LF_DAMMX*NEL) ,STAT=ERR)
            GBUF%FAIL(1)%DAMMX(:) = RBUF(IAD+1:IAD+NEL*GBUF%FAIL(1)%LF_DAMMX)
              IAD = IAD + NEL*GBUF%FAIL(1)%LF_DAMMX
          ALLOCATE(ELBUF_STR%GBUF%FAIL(1)%DAMINI(GBUF%FAIL(1)%LF_DAMINI*NEL) ,STAT=ERR)
            GBUF%FAIL(1)%DAMINI(:) = RBUF(IAD+1:IAD+NEL*GBUF%FAIL(1)%LF_DAMINI)
              IAD = IAD + NEL*GBUF%FAIL(1)%LF_DAMINI
          ALLOCATE(ELBUF_STR%GBUF%FAIL(1)%TDEL(GBUF%FAIL(1)%LF_TDEL*NEL) ,STAT=ERR)
            GBUF%FAIL(1)%TDEL(:) = RBUF(IAD+1:IAD+NEL*GBUF%FAIL(1)%LF_TDEL)
              IAD = IAD + NEL*GBUF%FAIL(1)%LF_TDEL
          ALLOCATE(ELBUF_STR%GBUF%FAIL(1)%INDX(GBUF%FAIL(1)%LF_INDX*NEL) ,STAT=ERR)
            GBUF%FAIL(1)%INDX(:) = RBUF(IAD+1:IAD+NEL*GBUF%FAIL(1)%LF_INDX)
              IAD = IAD + NEL*GBUF%FAIL(1)%LF_INDX
          ALLOCATE(ELBUF_STR%GBUF%FAIL(1)%OFF(GBUF%FAIL(1)%LF_OFF*NEL) ,STAT=ERR)
            GBUF%FAIL(1)%OFF(:) = RBUF(IAD+1:IAD+NEL*GBUF%FAIL(1)%LF_OFF)
              IAD = IAD + NEL*GBUF%FAIL(1)%LF_OFF
      ENDIF
c---
C seatbelt elements
c---
      call my_alloc(ELBUF_STR%GBUF%SLIPRING_ID, NEL*G_SLIPRING_ID, "ELBUF_STR%GBUF%SLIPRING_ID")
        GBUF%SLIPRING_ID(1:NEL*G_SLIPRING_ID) = NINT(RBUF(IAD+1:IAD+NEL*G_SLIPRING_ID))
          IAD = IAD+NEL*G_SLIPRING_ID
      call my_alloc(ELBUF_STR%GBUF%SLIPRING_FRAM_ID, NEL*G_SLIPRING_FRAM_ID, "ELBUF_STR%GBUF%SLIPRING_FRAM_ID")
        GBUF%SLIPRING_FRAM_ID(1:NEL*G_SLIPRING_FRAM_ID) = NINT(RBUF(IAD+1:IAD+NEL*G_SLIPRING_FRAM_ID))
          IAD = IAD+NEL*G_SLIPRING_FRAM_ID
      call my_alloc(ELBUF_STR%GBUF%SLIPRING_STRAND, NEL*G_SLIPRING_STRAND, "ELBUF_STR%GBUF%SLIPRING_STRAND")
        GBUF%SLIPRING_STRAND(1:NEL*G_SLIPRING_STRAND) = NINT(RBUF(IAD+1:IAD+NEL*G_SLIPRING_STRAND))
          IAD = IAD+NEL*G_SLIPRING_STRAND
      call my_alloc(ELBUF_STR%GBUF%RETRACTOR_ID, NEL*G_RETRACTOR_ID, "ELBUF_STR%GBUF%RETRACTOR_ID")
        GBUF%RETRACTOR_ID(1:NEL*G_RETRACTOR_ID) = NINT(RBUF(IAD+1:IAD+NEL*G_RETRACTOR_ID))
          IAD = IAD+NEL*G_RETRACTOR_ID
      call my_alloc(ELBUF_STR%GBUF%RINGSLIP, NEL*G_RINGSLIP, "ELBUF_STR%GBUF%RINGSLIP")
        GBUF%RINGSLIP(1:NEL*G_RINGSLIP) = RBUF(IAD+1:IAD+NEL*G_RINGSLIP)
          IAD = IAD+NEL*G_RINGSLIP
      call my_alloc(ELBUF_STR%GBUF%ADD_NODE, NEL*G_ADD_NODE, "ELBUF_STR%GBUF%ADD_NODE")
        GBUF%ADD_NODE(1:NEL*G_ADD_NODE) = NINT(RBUF(IAD+1:IAD+NEL*G_ADD_NODE))
          IAD = IAD+NEL*G_ADD_NODE
      call my_alloc(ELBUF_STR%GBUF%UPDATE, NEL*G_UPDATE, "ELBUF_STR%GBUF%UPDATE")
        GBUF%UPDATE(1:NEL*G_UPDATE) = NINT(RBUF(IAD+1:IAD+NEL*G_UPDATE))
          IAD = IAD+NEL*G_UPDATE     
      call my_alloc(ELBUF_STR%GBUF%FRAM_FACTOR, NEL*G_FRAM_FACTOR, "ELBUF_STR%GBUF%FRAM_FACTOR")
        GBUF%FRAM_FACTOR(1:NEL*G_FRAM_FACTOR) = RBUF(IAD+1:IAD+NEL*G_FRAM_FACTOR)
          IAD = IAD+NEL*G_FRAM_FACTOR    
      call my_alloc(ELBUF_STR%GBUF%INTVAR, NEL*G_INTVAR, "ELBUF_STR%GBUF%INTVAR")
        GBUF%INTVAR(1:NEL*G_INTVAR) = RBUF(IAD+1:IAD+NEL*G_INTVAR)
          IAD = IAD+NEL*G_INTVAR
      call my_alloc(ELBUF_STR%GBUF%IDT_TSH, NEL*G_IDT_TSH, "ELBUF_STR%GBUF%IDT_TSH")
        GBUF%IDT_TSH(1:NEL*G_IDT_TSH) = NINT(RBUF(IAD+1:IAD+NEL*G_IDT_TSH))
          IAD = IAD+NEL*G_IDT_TSH
      IF (NPG > 1) THEN
        call my_alloc(ELBUF_STR%GBUF%TEMPG, NEL*GBUF%G_TEMPG, "ELBUF_STR%GBUF%TEMPG")
        GBUF%TEMPG(1:NEL*GBUF%G_TEMPG)=RBUF(IAD+1:IAD+NEL*GBUF%G_TEMPG) 
        IAD = IAD + NEL*GBUF%G_TEMPG
      ENDIF
c---
C TMAX(H3D)
c---
      call my_alloc(ELBUF_STR%GBUF%TM_YIELD, NEL*G_TM_YIELD, "ELBUF_STR%GBUF%TM_YIELD")
        GBUF%TM_YIELD(1:NEL*G_TM_YIELD) = RBUF(IAD+1:IAD+NEL*G_TM_YIELD)
          IAD = IAD+NEL*G_TM_YIELD
      call my_alloc(ELBUF_STR%GBUF%TM_SEQ, NEL*G_TM_SEQ, "ELBUF_STR%GBUF%TM_SEQ")
        GBUF%TM_SEQ(1:NEL*G_TM_SEQ) = RBUF(IAD+1:IAD+NEL*G_TM_SEQ)
          IAD = IAD+NEL*G_TM_SEQ
      call my_alloc(ELBUF_STR%GBUF%TM_EINT, NEL*G_TM_EINT, "ELBUF_STR%GBUF%TM_EINT")
        GBUF%TM_EINT(1:NEL*G_TM_EINT) = RBUF(IAD+1:IAD+NEL*G_TM_EINT)
          IAD = IAD+NEL*G_TM_EINT
      call my_alloc(ELBUF_STR%GBUF%TM_DMG, NEL*G_TM_DMG, "ELBUF_STR%GBUF%TM_DMG")
        GBUF%TM_DMG(1:NEL*G_TM_DMG) = RBUF(IAD+1:IAD+NEL*G_TM_DMG)
          IAD = IAD+NEL*G_TM_DMG
      call my_alloc(ELBUF_STR%GBUF%TM_SIG1, NEL*G_TM_SIG, "ELBUF_STR%GBUF%TM_SIG1")
        GBUF%TM_SIG1(1:NEL*G_TM_SIG) = RBUF(IAD+1:IAD+NEL*G_TM_SIG)
          IAD = IAD+NEL*G_TM_SIG
      call my_alloc(ELBUF_STR%GBUF%TM_SIG3, NEL*G_TM_SIG, "ELBUF_STR%GBUF%TM_SIG3")
        GBUF%TM_SIG3(1:NEL*G_TM_SIG) = RBUF(IAD+1:IAD+NEL*G_TM_SIG)
          IAD = IAD+NEL*G_TM_SIG
      call my_alloc(ELBUF_STR%GBUF%TM_STRA1, NEL*G_TM_STRA, "ELBUF_STR%GBUF%TM_STRA1")
        GBUF%TM_STRA1(1:NEL*G_TM_STRA) = RBUF(IAD+1:IAD+NEL*G_TM_STRA)
          IAD = IAD+NEL*G_TM_STRA
      call my_alloc(ELBUF_STR%GBUF%TM_STRA3, NEL*G_TM_STRA, "ELBUF_STR%GBUF%TM_STRA3")
        GBUF%TM_STRA3(1:NEL*G_TM_STRA) = RBUF(IAD+1:IAD+NEL*G_TM_STRA)
          IAD = IAD+NEL*G_TM_STRA
c--- work array
      IF (G_TM_SIG>0) ALLOCATE (ELBUF_STR%GBUF%TM_PSIG(NEL*2),STAT=ERR)
      IF (G_TM_STRA>0) ALLOCATE (ELBUF_STR%GBUF%TM_PSTRA(NEL*2),STAT=ERR)
c
C-----
      IF (ALLOCATED(RBUF)) call my_dealloc(RBUF)
      SDP_RBUF = SDP_RBUF + GBUF%G_SMSTR * NEL
      call my_alloc(ELBUF_STR%GBUF%SMSTR, NEL*G_SMSTR, "ELBUF_STR%GBUF%SMSTR")
      IF (SDP_RBUF > 0)THEN
        call my_alloc(DP_RBUF, SDP_RBUF, "DP_RBUF")
        CALL READ_DPDB(DP_RBUF,SDP_RBUF)
c-------------------------------------------------
C       local buffer variables first
c-------------------------------------------------
       IADP = 0
      IF (L_VOL0DP > 0)THEN
       DO IL = 1,ELBUF_STR%NLAY                                         
c
        BUFLY => ELBUF_STR%BUFLY(IL)
        IF (IGTYP == 51 .OR. IGTYP == 52) THEN
          NPTT = ELBUF_STR%BUFLY(IL)%NPTT
        ELSE
          NPTT = ELBUF_STR%NPTT
        ENDIF
       DO IR = 1,NPTR                                                            
        DO IS = 1,NPTS                                                            
         DO IT = 1,NPTT                                                           
          LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)                             
           LBUF%VOL0DP(1:NEL*L_VOL0DP)   = DP_RBUF(IADP+1:IADP+NEL*L_VOL0DP)               
           IADP = IADP+NEL*L_VOL0DP 
         ENDDO !next IT
        ENDDO !next IS
       ENDDO !next IR
       ENDDO !next IL  
      END IF !(L_VOL0DP > 0)THEN
C      
       GBUF%SMSTR(1:NEL*G_SMSTR) = DP_RBUF(IADP+1:IADP+NEL*G_SMSTR)                  
       call my_dealloc(DP_RBUF)
      ENDIF

C-----------------------------------------------
      END SUBROUTINE ALLOCBUF_AUTO
