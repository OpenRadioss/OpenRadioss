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
Chd|  ALLOCBUF_AUTO                 source/elements/elbuf/allocbuf_auto.F
Chd|-- called by -----------
Chd|        ELBUF_INI                     source/elements/elbuf/elbuf_ini.F
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_DPDB                     source/output/tools/read_db.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ALLOCBUF_AUTO(ELBUF_STR,BUFLEN,NPT,ITY,ISNOD,NEWLEN,INLOC)
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
#include      "com_xfem1.inc"
#include      "inter22.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER BUFLEN,NPT,ITY,ISNOD,NEWLEN,INLOC
      TYPE(ELBUF_STRUCT_) , TARGET   :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,IL,IR,IS,IT,IAD,ERR,NUVAR,NVARTMP,IGTYP,IXFEM,NLAY,
     .  NINTLAY,NPTR,NPTS,NPTT,NEL,NPG,IPT,LEN1,LEN2,ILAW,IMAT,IEOS,NONL,
     .  IVISC,IPORO,NFAIL,NVAR_MAT,NVAR_VISC,NVAR_LOC,NVAR_LAY,NPTTOT,IADP
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
     .   G_BPRELD,G_ABURN,G_MU,G_DT_PITER_OLD,G_DT_PITER,G_TEMPG,
     .   G_COR_NF,G_COR_FR,G_COR_XR,G_DEFINI,G_FORINI,G_IDT_TSH,
     .   G_FORPGPINCH,G_MOMPGPINCH,G_EPGPINCHXZ,G_EPGPINCHYZ,G_EPGPINCHZZ,
     .   G_SKEW_ID,G_MAXFRAC,G_MAXEPS,G_GAMA_R,G_SLIPRING_ID,G_SLIPRING_STRAND,G_RETRACTOR_ID,
     .   G_RINGSLIP,G_ADD_NODE,G_UPDATE,G_INTVAR,G_BETAORTH,G_AMU,G_SLIPRING_FRAM_ID,G_FRAM_FACTOR,
     .   G_TM_YIELD,G_TM_SEQ,G_TM_EINT,G_TM_DMG,G_TM_SIG,G_TM_STRA 
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
     .   L_SEQ,L_SIGPLY,L_JAC_I,L_FAC_YLD,L_ABURN,L_MU,L_PIJ,L_VOL0DP
       INTEGER  ! failure variables
     .   LF_DAM,LF_DAMMX,LF_TDEL,LF_INDX,LF_OFF
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
c-------------------------------------------------
c       allocation of sub-structures of the element buffer
C=======================================================================
c------  Allocate and read element data from restart
        ALLOCATE (RBUF(BUFLEN) ,STAT=ERR)
        CALL READ_DB(RBUF, BUFLEN)
c-------------------------------------------------
c       read element buffer
c-------------------------------------------------
      IAD = 0                                                         
c     part of DP in elbuf
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
c-------------------------------------------------
      ALLOCATE (ELBUF_STR%BUFLY(NLAY) ,STAT=ERR)                      
C
C   moved below because of various NPTT through each layer for IGTYP = 51
C
c      DO IL = 1, NLAY                                                 
c        ALLOCATE(ELBUF_STR%BUFLY(IL)%LBUF(NPTR,NPTS,NPTT),STAT=ERR)   
c        ALLOCATE(ELBUF_STR%BUFLY(IL)%MAT (NPTR,NPTS,NPTT),STAT=ERR)       
c        ALLOCATE(ELBUF_STR%BUFLY(IL)%FAIL(NPTR,NPTS,NPTT),STAT=ERR)   
c        ALLOCATE(ELBUF_STR%BUFLY(IL)%PROP(NPTR,NPTS,NPTT),STAT=ERR)   
c        ALLOCATE(ELBUF_STR%BUFLY(IL)%EOS (NPTR,NPTS,NPTT),STAT=ERR)   
c        ALLOCATE(ELBUF_STR%BUFLY(IL)%VISC(NPTR,NPTS,NPTT),STAT=ERR)   
c        ALLOCATE(ELBUF_STR%BUFLY(IL)%PORO(NPTR,NPTS,NPTT),STAT=ERR)   
c      ENDDO                                                 
c-------------------------------------------------
      ALLOCATE (ELBUF_STR%INTLAY(NINTLAY) ,STAT=ERR)                      
      DO IL = 1, NINTLAY                                                 
        ALLOCATE(ELBUF_STR%INTLAY(IL)%ILBUF(NPTR,NPTS),STAT=ERR)
        ALLOCATE(ELBUF_STR%INTLAY(IL)%MAT  (NPTR,NPTS),STAT=ERR)       
        ALLOCATE(ELBUF_STR%INTLAY(IL)%FAIL (NPTR,NPTS),STAT=ERR)   
      ENDDO                                                 
c-------------------------------------------------
c     LAYER VARIABLES
c-------------------------------------------------
      DO IL = 1,ELBUF_STR%NLAY                                         
c       Layer parameters : NPAR_LAY = 12 
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
        NVARTMP = NINT(RBUF(IAD+1))               
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
c             
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
c       local variable lengths : NVAR_LOC = 48
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
        L_PIJ    = NINT(RBUF(IAD+1)) 
          IAD = IAD+1 
        L_VOL0DP    = NINT(RBUF(IAD+1)) 
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
        BUFLY%NVARTMP   = NVARTMP         
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
c-------------------------------------------------
c-------------------------------------------------
c       allocation & read of layer variables
c-------------------------------------------------
        ALLOCATE(BUFLY%DMG (NEL*LY_DMG)   ,STAT=ERR)                             
          BUFLY%DMG(1:NEL*LY_DMG)  = RBUF(IAD+1:IAD+NEL*LY_DMG)                 
            IAD = IAD+NEL*LY_DMG                                                
        ALLOCATE(BUFLY%GAMA (NEL*LY_GAMA)   ,STAT=ERR)                             
          BUFLY%GAMA(1:NEL*LY_GAMA) = RBUF(IAD+1:IAD+NEL*LY_GAMA)            
            IAD = IAD+NEL*LY_GAMA                                                
        ALLOCATE(BUFLY%DIRA (NEL*LY_DIRA)   ,STAT=ERR)                             
          BUFLY%DIRA(1:NEL*LY_DIRA) = RBUF(IAD+1:IAD+NEL*LY_DIRA)                 
            IAD = IAD+NEL*LY_DIRA                                                
        ALLOCATE(BUFLY%DIRB (NEL*LY_DIRB)   ,STAT=ERR)                             
          BUFLY%DIRB(1:NEL*LY_DIRB) = RBUF(IAD+1:IAD+NEL*LY_DIRB)                 
            IAD = IAD+NEL*LY_DIRB                                                
        ALLOCATE(BUFLY%CRKDIR(NEL*LY_CRKDIR)   ,STAT=ERR)                             
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
          ALLOCATE(BUFLY%PLAPT(LEN_PLAPT)   ,STAT=ERR)
            BUFLY%PLAPT(1:LEN_PLAPT) = RBUF(IAD+1:IAD+LEN_PLAPT)
              IAD = IAD + LEN_PLAPT
          ALLOCATE(BUFLY%SIGPT (LEN_SIGPT)   ,STAT=ERR)
            BUFLY%SIGPT(1:LEN_SIGPT) = RBUF(IAD+1:IAD+LEN_SIGPT)
              IAD = IAD+LEN_SIGPT
        ELSEIF (NPG == 1) THEN
C
C         must point after  => LBUF%PLA and LBUF%SIG
C
        ENDIF
        ALLOCATE(BUFLY%HOURG(NEL*LY_HOURG)   ,STAT=ERR)                             
          BUFLY%HOURG(1:NEL*LY_HOURG)  = RBUF(IAD+1:IAD+NEL*LY_HOURG)                 
            IAD = IAD+NEL*LY_HOURG
        ALLOCATE(BUFLY%UELR(NEL*LY_UELR)   ,STAT=ERR)                             
          BUFLY%UELR(1:NEL*LY_UELR)  = RBUF(IAD+1:IAD+NEL*LY_UELR)                 
            IAD = IAD+NEL*LY_UELR
        ALLOCATE(BUFLY%UELR1(NEL*LY_UELR1)   ,STAT=ERR)                             
          BUFLY%UELR1(1:NEL*LY_UELR1)  = RBUF(IAD+1:IAD+NEL*LY_UELR1)                 
            IAD = IAD+NEL*LY_UELR1
        ALLOCATE(BUFLY%OFFPG(NEL*LY_OFFPG)   ,STAT=ERR)                             
          BUFLY%OFFPG(1:NEL*LY_OFFPG)  = RBUF(IAD+1:IAD+NEL*LY_OFFPG)                 
            IAD = IAD+NEL*LY_OFFPG
        ALLOCATE(BUFLY%OFF(NEL*LY_OFF)   ,STAT=ERR)                             
          BUFLY%OFF(1:NEL*LY_OFF)  = RBUF(IAD+1:IAD+NEL*LY_OFF)                 
            IAD = IAD+NEL*LY_OFF
c-------------------------------------------------
c       allocation & read of local buffer variables
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
          ALLOCATE(LBUF%OFF (NEL*L_OFF)   ,STAT=ERR)                             
            LBUF%OFF(1:NEL*L_OFF)    = RBUF(IAD+1:IAD+NEL*L_OFF)                 
              IAD = IAD+NEL*L_OFF                                                
          ALLOCATE (LBUF%GAMA(NEL*L_GAMA)  ,STAT=ERR)                            
            LBUF%GAMA(1:NEL*L_GAMA)  = RBUF(IAD+1:IAD+NEL*L_GAMA)                
             IAD = IAD+NEL*L_GAMA                                                
          ALLOCATE(LBUF%STRA  (NEL*L_STRA)  ,STAT=ERR)                           
            LBUF%STRA(1:NEL*L_STRA) = RBUF(IAD+1:IAD+NEL*L_STRA)                 
              IAD = IAD+NEL*L_STRA                                               
          ALLOCATE(LBUF%FRAC (NEL*L_FRAC)  ,STAT=ERR)                            
            LBUF%FRAC(1:NEL*L_FRAC)  = RBUF(IAD+1:IAD+NEL*L_FRAC)                
              IAD = IAD+NEL*L_FRAC
          ALLOCATE(LBUF%BFRAC(NEL*L_BFRAC)  ,STAT=ERR)              
            LBUF%BFRAC(1:NEL*L_BFRAC)   = RBUF(IAD+1:IAD+NEL*L_BFRAC)               
              IAD = IAD+NEL*L_BFRAC                                               
          ALLOCATE(LBUF%EINT(NEL*L_EINT)  ,STAT=ERR)                             
            LBUF%EINT(1:NEL*L_EINT)   = RBUF(IAD+1:IAD+NEL*L_EINT)               
              IAD = IAD+NEL*L_EINT                                               
          ALLOCATE(LBUF%EINS(NEL*L_EINS)  ,STAT=ERR)                             
            LBUF%EINS(1:NEL*L_EINS)   = RBUF(IAD+1:IAD+NEL*L_EINS)               
              IAD = IAD+NEL*L_EINS                                               
          ALLOCATE(LBUF%RHO(NEL*L_RHO)   ,STAT=ERR)                              
            LBUF%RHO(1:NEL*L_RHO)    = RBUF(IAD+1:IAD+NEL*L_RHO)                 
              IAD = IAD+NEL*L_RHO    
          ALLOCATE(LBUF%DP_DRHO(NEL*L_DP_DRHO)   ,STAT=ERR)                      
           LBUF%DP_DRHO(1:NEL*L_DP_DRHO)=RBUF(IAD+1:IAD+NEL*L_DP_DRHO)          
              IAD = IAD+NEL*L_DP_DRHO                                            
          ALLOCATE(LBUF%QVIS(NEL*L_QVIS)  ,STAT=ERR)                             
            LBUF%QVIS(1:NEL*L_QVIS)   = RBUF(IAD+1:IAD+NEL*L_QVIS)               
              IAD = IAD+NEL*L_QVIS                                               
          ALLOCATE(LBUF%DELTAX(NEL*L_DELTAX),STAT=ERR)                           
            LBUF%DELTAX(1:NEL*L_DELTAX)=RBUF(IAD+1:IAD+NEL*L_DELTAX)             
              IAD = IAD+NEL*L_DELTAX      
          ALLOCATE(LBUF%VOL   (NEL*L_VOL)   ,STAT=ERR)                           
            LBUF%VOL(1:NEL*L_VOL)    = RBUF(IAD+1:IAD+NEL*L_VOL)                 
              IAD = IAD+NEL*L_VOL                                                
          ALLOCATE(LBUF%EPSA  (NEL*L_EPSA)  ,STAT=ERR)                           
            LBUF%EPSA(1:NEL*L_EPSA)  = RBUF(IAD+1:IAD+NEL*L_EPSA)                
              IAD = IAD+NEL*L_EPSA                                               
          ALLOCATE(LBUF%EPSD  (NEL*L_EPSD)  ,STAT=ERR)                           
            LBUF%EPSD(1:NEL*L_EPSD)   = RBUF(IAD+1:IAD+NEL*L_EPSD)               
              IAD = IAD+NEL*L_EPSD                                               
          ALLOCATE(LBUF%EPSQ  (NEL*L_EPSQ)  ,STAT=ERR)                           
            LBUF%EPSQ(1:NEL*L_EPSQ)   = RBUF(IAD+1:IAD+NEL*L_EPSQ)               
              IAD = IAD+NEL*L_EPSQ                                               
          ALLOCATE(LBUF%EPSF  (NEL*L_EPSF)  ,STAT=ERR)                           
            LBUF%EPSF(1:NEL*L_EPSF)  = RBUF(IAD+1:IAD+NEL*L_EPSF)                
              IAD = IAD+NEL*L_EPSF                                               
          ALLOCATE(LBUF%PLA   (NEL*L_PLA)   ,STAT=ERR)                           
            LBUF%PLA(1:NEL*L_PLA)    = RBUF(IAD+1:IAD+NEL*L_PLA)                 
              IAD = IAD+NEL*L_PLA                                                
          ALLOCATE(LBUF%TEMP(NEL*L_TEMP), STAT=ERR)                              
            LBUF%TEMP(1:NEL*L_TEMP)   = RBUF(IAD+1:IAD+NEL*L_TEMP)               
              IAD = IAD+NEL*L_TEMP                                               
          ALLOCATE(LBUF%TB(NEL*L_TB), STAT=ERR)                                  
            LBUF%TB(1:NEL*L_TB)   = RBUF(IAD+1:IAD+NEL*L_TB)                     
              IAD = IAD+NEL*L_TB                                                 
          ALLOCATE(LBUF%RK(NEL*L_RK), STAT=ERR)    
            LBUF%RK(1:NEL*L_RK)   = RBUF(IAD+1:IAD+NEL*L_RK)                     
              IAD = IAD+NEL*L_RK                                                 
          ALLOCATE(LBUF%RE(NEL*L_RE), STAT=ERR)                                  
            LBUF%RE(1:NEL*L_RE)   = RBUF(IAD+1:IAD+NEL*L_RE)                     
              IAD = IAD+NEL*L_RE                                                 
          ALLOCATE(LBUF%VK(NEL*L_VK), STAT=ERR)                                  
            LBUF%VK(1:NEL*L_VK)   = RBUF(IAD+1:IAD+NEL*L_VK)                     
              IAD = IAD+NEL*L_VK                                                 
          ALLOCATE(LBUF%SF(NEL*L_SF), STAT=ERR)                                  
            LBUF%SF(1:NEL*L_SF)   = RBUF(IAD+1:IAD+NEL*L_SF)                     
              IAD = IAD+NEL*L_SF                                                 
          ALLOCATE(LBUF%ROB(NEL*L_ROB), STAT=ERR)                                
            LBUF%ROB(1:NEL*L_ROB)    = RBUF(IAD+1:IAD+NEL*L_ROB)                 
              IAD = IAD+NEL*L_ROB                                                
          ALLOCATE(LBUF%DAM  (NEL*L_DAM)  ,STAT=ERR)                             
            LBUF%DAM(1:NEL*L_DAM)  = RBUF(IAD+1:IAD+NEL*L_DAM)                   
              IAD = IAD+NEL*L_DAM                                                
          ALLOCATE(LBUF%DSUM  (NEL*L_DSUM)  ,STAT=ERR)                           
            LBUF%DSUM(1:NEL*L_DSUM)  = RBUF(IAD+1:IAD+NEL*L_DSUM)                
              IAD = IAD+NEL*L_DSUM                                               
          ALLOCATE(LBUF%DGLO  (NEL*L_DGLO)  ,STAT=ERR)                           
            LBUF%DGLO(1:NEL*L_DGLO)  = RBUF(IAD+1:IAD+NEL*L_DGLO)                
              IAD = IAD+NEL*L_DGLO                                               
          ALLOCATE(LBUF%CRAK  (NEL*L_CRAK)  ,STAT=ERR)                           
            LBUF%CRAK(1:NEL*L_CRAK)  = RBUF(IAD+1:IAD+NEL*L_CRAK)                
              IAD = IAD+NEL*L_CRAK                                               
          ALLOCATE(LBUF%ANG  (NEL*L_ANG)  ,STAT=ERR)                             
            LBUF%ANG(1:NEL*L_ANG)  = RBUF(IAD+1:IAD+NEL*L_ANG)                   
              IAD = IAD+NEL*L_ANG                                                
          ALLOCATE(LBUF%EPE  (NEL*L_EPE)  ,STAT=ERR)                             
            LBUF%EPE(1:NEL*L_EPE)  = RBUF(IAD+1:IAD+NEL*L_EPE)                   
              IAD = IAD+NEL*L_EPE                                                
          ALLOCATE(LBUF%EPC  (NEL*L_EPC)  ,STAT=ERR)                             
            LBUF%EPC(1:NEL*L_EPC)  = RBUF(IAD+1:IAD+NEL*L_EPC)                   
              IAD = IAD+NEL*L_EPC                                                
          ALLOCATE(LBUF%XST  (NEL*L_XST)  ,STAT=ERR)                             
            LBUF%XST(1:NEL*L_XST)  = RBUF(IAD+1:IAD+NEL*L_XST)                   
              IAD = IAD+NEL*L_XST                                                
          ALLOCATE(LBUF%SSP  (NEL*L_SSP)  ,STAT=ERR)                             
            LBUF%SSP(1:NEL*L_SSP)  = RBUF(IAD+1:IAD+NEL*L_SSP)                   
              IAD = IAD+NEL*L_SSP                                                
          ALLOCATE(LBUF%Z  (NEL*L_Z)  ,STAT=ERR)                                 
            LBUF%Z(1:NEL*L_Z)  = RBUF(IAD+1:IAD+NEL*L_Z)                         
              IAD = IAD+NEL*L_Z                                                  
          ALLOCATE(LBUF%VISC  (NEL*L_VISC)  ,STAT=ERR)                           
            LBUF%VISC(1:NEL*L_VISC)  = RBUF(IAD+1:IAD+NEL*L_VISC)                
              IAD = IAD+NEL*L_VISC                                               
          ALLOCATE(LBUF%SIGL  (NEL*L_SIGL)  ,STAT=ERR)                           
            LBUF%SIGL(1:NEL*L_SIGL)  = RBUF(IAD+1:IAD+NEL*L_SIGL)                
              IAD = IAD+NEL*L_SIGL                                               
          ALLOCATE(LBUF%SIGV  (NEL*L_SIGV)  ,STAT=ERR)                           
            LBUF%SIGV(1:NEL*L_SIGV)  = RBUF(IAD+1:IAD+NEL*L_SIGV)                
              IAD = IAD+NEL*L_SIGV                                               
          ALLOCATE(LBUF%SIGA  (NEL*L_SIGA)  ,STAT=ERR)                           
            LBUF%SIGA(1:NEL*L_SIGA) = RBUF(IAD+1:IAD+NEL*L_SIGA)                 
              IAD = IAD+NEL*L_SIGA                                               
          ALLOCATE(LBUF%SIGB  (NEL*L_SIGB)  ,STAT=ERR)                           
            LBUF%SIGB(1:NEL*L_SIGB) = RBUF(IAD+1:IAD+NEL*L_SIGB)                 
              IAD = IAD+NEL*L_SIGB                                               
          ALLOCATE(LBUF%SIGC  (NEL*L_SIGC)  ,STAT=ERR)                           
            LBUF%SIGC(1:NEL*L_SIGC) = RBUF(IAD+1:IAD+NEL*L_SIGC)                 
              IAD = IAD+NEL*L_SIGC                                               
          ALLOCATE(LBUF%SIGD  (NEL*L_SIGD)  ,STAT=ERR)                           
            LBUF%SIGD(1:NEL*L_SIGD) = RBUF(IAD+1:IAD+NEL*L_SIGD)                 
              IAD = IAD+NEL*L_SIGD                                               
          ALLOCATE(LBUF%SIGF  (NEL*L_SIGF)  ,STAT=ERR)                           
            LBUF%SIGF(1:NEL*L_SIGF)  = RBUF(IAD+1:IAD+NEL*L_SIGF)                
              IAD = IAD+NEL*L_SIGF                                               
          ALLOCATE(LBUF%SIG(NEL*L_SIG)   ,STAT=ERR)                              
            LBUF%SIG(1:NEL*L_SIG)  = RBUF(IAD+1:IAD+NEL*L_SIG)                   
              IAD = IAD+NEL*L_SIG                                                
          ALLOCATE(LBUF%SIGPLY(NEL*L_SIGPLY)   ,STAT=ERR)                              
            LBUF%SIGPLY(1:NEL*L_SIGPLY)  = RBUF(IAD+1:IAD+NEL*L_SIGPLY)                   
              IAD = IAD+NEL*L_SIGPLY                                                
          ALLOCATE(LBUF%FOR (NEL*L_FOR)  ,STAT=ERR)                            
            LBUF%FOR(1:NEL*L_FOR)  = RBUF(IAD+1:IAD+NEL*L_FOR)                
              IAD = IAD+NEL*L_FOR
          ALLOCATE(LBUF%MOM (NEL*L_MOM)  ,STAT=ERR)                            
            LBUF%MOM(1:NEL*L_MOM)  = RBUF(IAD+1:IAD+NEL*L_MOM)                
              IAD = IAD+NEL*L_MOM
          ALLOCATE(LBUF%THK (NEL*L_THK)  ,STAT=ERR)                            
            LBUF%THK(1:NEL*L_THK)  = RBUF(IAD+1:IAD+NEL*L_THK)                
              IAD = IAD+NEL*L_THK
          ALLOCATE(LBUF%SMSTR (NEL*L_SMSTR)  ,STAT=ERR)                            
            LBUF%SMSTR(1:NEL*L_SMSTR)  = RBUF(IAD+1:IAD+NEL*L_SMSTR)                
              IAD = IAD+NEL*L_SMSTR
          ALLOCATE(LBUF%DMG (NEL*L_DMG)  ,STAT=ERR)                            
            LBUF%DMG(1:NEL*L_DMG)  = RBUF(IAD+1:IAD+NEL*L_DMG)                
              IAD = IAD+NEL*L_DMG
          ALLOCATE(LBUF%FORTH (NEL*L_FORTH)  ,STAT=ERR)                            
            LBUF%FORTH(1:NEL*L_FORTH)  = RBUF(IAD+1:IAD+NEL*L_FORTH)                
              IAD = IAD+NEL*L_FORTH
          ALLOCATE(LBUF%EINTTH (NEL*L_EINTTH)  ,STAT=ERR)                            
            LBUF%EINTTH(1:NEL*L_EINTTH)  = RBUF(IAD+1:IAD+NEL*L_EINTTH)                
              IAD = IAD+NEL*L_EINTTH
          ALLOCATE(LBUF%SEQ (NEL*L_SEQ)  ,STAT=ERR)
            LBUF%SEQ(1:NEL*L_SEQ)  = RBUF(IAD+1:IAD+NEL*L_SEQ)
              IAD = IAD+NEL*L_SEQ
          ALLOCATE(LBUF%JAC_I (NEL*L_JAC_I)  ,STAT=ERR)
            LBUF%JAC_I(1:NEL*L_JAC_I)  = RBUF(IAD+1:IAD+NEL*L_JAC_I)
              IAD = IAD+NEL*L_JAC_I
          ALLOCATE(LBUF%FAC_YLD (NEL*L_FAC_YLD)  ,STAT=ERR)
            LBUF%FAC_YLD(1:NEL*L_FAC_YLD)  = RBUF(IAD+1:IAD+NEL*L_FAC_YLD)
              IAD = IAD+NEL*L_FAC_YLD
          ALLOCATE(LBUF%ABURN(NEL*L_ABURN)  ,STAT=ERR)              
            LBUF%ABURN(1:NEL*L_ABURN)   = RBUF(IAD+1:IAD+NEL*L_ABURN)               
              IAD = IAD+NEL*L_ABURN                                              
          ALLOCATE(LBUF%MU(NEL*L_MU)  ,STAT=ERR)              
            LBUF%MU(1:NEL*L_MU)   = RBUF(IAD+1:IAD+NEL*L_MU)               
              IAD = IAD+NEL*L_MU                                              
          ALLOCATE(LBUF%PIJ(NEL*L_PIJ)  ,STAT=ERR)              
            LBUF%PIJ(1:NEL*L_PIJ)   = RBUF(IAD+1:IAD+NEL*L_PIJ)               
              IAD = IAD+NEL*L_PIJ 
C---------- size of DP ---             
          ALLOCATE(LBUF%VOL0DP(NEL*L_VOL0DP)  ,STAT=ERR)              
              SDP_RBUF = SDP_RBUF + NEL*L_VOL0DP
!----------------------------------------------------------
         ENDDO !next IT
        ENDDO !next IS
       ENDDO !next IR
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
        ELSE    ! cette partie semble avoir des problemes de pointeur
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
              ALLOCATE (MATBUF%VAR(NEL*NUVAR), STAT=ERR)                        
              MATBUF%VAR(1:NEL*NUVAR) = RBUF(IAD+1:IAD+NEL*NUVAR)                          
              IAD = IAD+NEL*NUVAR                                         
              ALLOCATE (MATBUF%VARTMP(NEL*NVARTMP), STAT=ERR)                        
              MATBUF%VARTMP(1:NEL*NVARTMP) = 0
c-----                                                                           
            ENDDO                                                             
          ENDDO                                                              
        ENDDO                                                             
      ENDDO                                                               
c-------------------------------
c         failure model variables                            
c-------------------------------
      DO IL = 1, NLAY                                                      
        NFAIL = ELBUF_STR% BUFLY(IL)%NFAIL                                     
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
                NUVAR = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%NVAR = NUVAR
   
                LF_DAM = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%LF_DAM = LF_DAM 

                LF_DAMMX = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%LF_DAMMX = LF_DAMMX       

                LF_TDEL = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%LF_TDEL = LF_TDEL                                           

                LF_INDX = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%LF_INDX = LF_INDX                                           

                LF_OFF = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%LF_OFF = LF_OFF                                           
                                       
                ALLOCATE(FLOC%VAR(NEL*NUVAR), STAT=ERR)                    
                FLOC%VAR(1:NEL*NUVAR) = RBUF(IAD+1:IAD+NEL*NUVAR)                        
                IAD = IAD + NEL*NUVAR                                        

                ALLOCATE(FLOC%DAM(NEL*LF_DAM), STAT=ERR)                    
                FLOC%DAM(1:NEL*LF_DAM) = RBUF(IAD+1:IAD+NEL*LF_DAM)                        
                IAD = IAD + NEL*LF_DAM    

                ALLOCATE(FLOC%DAMMX(NEL*LF_DAMMX), STAT=ERR)                    
                FLOC%DAMMX(1:NEL*LF_DAMMX) = RBUF(IAD+1:IAD+NEL*LF_DAMMX)                        
                IAD = IAD + NEL*LF_DAMMX     

                ALLOCATE(FLOC%TDEL(NEL*LF_TDEL), STAT=ERR)                    
                FLOC%TDEL(1:NEL*LF_TDEL) = RBUF(IAD+1:IAD+NEL*LF_TDEL)                        
                IAD = IAD + NEL*LF_TDEL                                    

                ALLOCATE(FLOC%INDX(NEL*LF_INDX), STAT=ERR)                    
                FLOC%INDX(1:NEL*LF_INDX) = RBUF(IAD+1:IAD+NEL*LF_INDX)                        
                IAD = IAD + NEL*LF_INDX                                    

                ALLOCATE(FLOC%OFF(NEL*LF_OFF), STAT=ERR)                    
                FLOC%OFF(1:NEL*LF_OFF) = RBUF(IAD+1:IAD+NEL*LF_OFF)                        
                IAD = IAD + NEL*LF_OFF                                    
              ENDDO                                                         
            ENDDO                                                          
          ENDDO                                                            
        ENDDO                                                              
      ENDDO                                                                
c-------------------------------
c         Viscosity model variables                            
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
              ALLOCATE (VISCBUF%VAR(NEL*NUVAR), STAT=ERR)                        
              VISCBUF%VAR(1:NEL*NUVAR) = RBUF(IAD+1:IAD+NEL*NUVAR)                          
              IAD = IAD+NEL*NUVAR                                         
c-----                                                                           
            ENDDO                                                             
          ENDDO                                                              
        ENDDO                                                             
      ENDDO
c-------------------------------
c         Non-local regularization in the shell thickness                
c-------------------------------
      IF ((ITY==3).OR.(ITY==7)) THEN
        ! Number of integration points in the shell thickness
        NPTT = ELBUF_STR%NPTT
        IF ((INLOC>0).AND.(NPTT>1)) THEN
          ! Allocation of the non-local buffer
          ALLOCATE(ELBUF_STR%NLOC(NPTR,NPTS), STAT=ERR)
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
              ALLOCATE(BUFNL%MASSTH(NEL,NONL), STAT=ERR) 
              ! Non-local in-thickness cumulated variables
              ALLOCATE(BUFNL%UNLTH(NEL,NONL) , STAT=ERR) 
              ! Non-local in-thickness velocities
              ALLOCATE(BUFNL%VNLTH(NEL,NONL) , STAT=ERR) 
              ! Non-local in-thickness forces
              ALLOCATE(BUFNL%FNLTH(NEL,NONL) , STAT=ERR) 
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
      ENDIF
c-------------------------------------------------
c     INTER LAYER VARIABLES
c-------------------------------------------------
      L_EPS   = 3
      L_SIG   = 3
      LY_EINT = 1  
      LY_COUNT= 1  
c
      DO IL = 1,ELBUF_STR%NINTLAY                                         
        INTLAY => ELBUF_STR%INTLAY(IL)
c
c       Interlayer parameters : 4
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
c     Interlayer :  allocation & read of local buffer variables
c-------------------------------------------------
        ALLOCATE(INTLAY%EINT (NEL*LY_EINT)  ,STAT=ERR)                           
          INTLAY%EINT(1:NEL*LY_EINT) = RBUF(IAD+1:IAD+NEL*LY_EINT)
            IAD = IAD+NEL*LY_EINT                                              
        ALLOCATE(INTLAY%COUNT (NEL*LY_COUNT)  ,STAT=ERR)                           
          INTLAY%COUNT(1:NEL*LY_COUNT) = RBUF(IAD+1:IAD+NEL*LY_COUNT)
            IAD = IAD+NEL*LY_COUNT                                              
c
        DO IR = 1,NPTR                                                            
          DO IS = 1,NPTS                                                            
            ILBUF => ELBUF_STR%INTLAY(IL)%ILBUF(IR,IS)   
c--------
            ALLOCATE(ILBUF%EPS (NEL*L_EPS)   ,STAT=ERR)                             
              ILBUF%EPS(1:NEL*L_EPS)    = RBUF(IAD+1:IAD+NEL*L_EPS)                 
                IAD = IAD+NEL*L_EPS                                                
            ALLOCATE(ILBUF%SIG (NEL*L_SIG)   ,STAT=ERR)                           
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
              ALLOCATE (MATBUF%VAR(NEL*NUVAR), STAT=ERR)                        
              MATBUF%VAR(1:NEL*NUVAR) = RBUF(IAD+1:IAD+NEL*NUVAR)                          
              IAD = IAD+NEL*NUVAR                                         
              ALLOCATE (MATBUF%VARTMP(NEL*NVARTMP), STAT=ERR)                        
              MATBUF%VARTMP(1:NEL*NVARTMP) = 0                    
          ENDDO                                                              
        ENDDO                                                             
      ENDDO                                                               
c-------------------------------
c     Interlayer failure model variables                            
c-------------------------------
      DO IL = 1, NINTLAY                                                      
        NFAIL = ELBUF_STR% INTLAY(IL)%NFAIL                                     
        DO IR = 1,NPTR                                                     
          DO IS = 1,NPTS                                                   
              ALLOCATE (ELBUF_STR%INTLAY(IL)%FAIL(IR,IS)%FLOC(NFAIL),                                                    
     .                                                       STAT=ERR)        
              DO K = 1,NFAIL                                               
                FLOC=>ELBUF_STR%INTLAY(IL)%FAIL(IR,IS)%FLOC(K)           
                FLOC%ILAWF = RBUF(IAD+1)                                   
                IAD = IAD+1                                                
                NUVAR = RBUF(IAD+1)                                        
                IAD = IAD+1                                                
                FLOC%NVAR = NUVAR                                          
                ALLOCATE(FLOC%VAR(NEL*NUVAR), STAT=ERR)                    
                FLOC%VAR(1:NEL*NUVAR) = RBUF(IAD+1:IAD+NEL*NUVAR)                        
                IAD = IAD+NEL*NUVAR                                        
              ENDDO                                                         
          ENDDO                                                            
        ENDDO                                                              
      ENDDO                                                                
c-------------------------------
c     Interlayer Viscosity model variables                            
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
c       allocation of global buffer variables
c-------------------------------------------------
      GBUF  => ELBUF_STR%GBUF     
c      
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
c---
c 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
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
c---
c seatbelt elements
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
      GBUF%G_TEMPG  = G_TEMPG     
      GBUF%G_COR_NF = G_COR_NF
      GBUF%G_COR_FR = G_COR_FR
      GBUF%G_COR_XR = G_COR_XR
      GBUF%G_MAXFRAC= G_MAXFRAC
      GBUF%G_MAXEPS = G_MAXEPS
      GBUF%G_BETAORTH  = G_BETAORTH
      GBUF%G_AMU    = G_AMU
c---
c 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
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
c---
c seatbelt elements
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
      ALLOCATE(ELBUF_STR%GBUF%ETOTSH(NEL*G_ETOTSH)         ,STAT=ERR)                         
      GBUF%ETOTSH = ZERO  
C------------  Work array GAMA_R : co-rotational local sys
      ALLOCATE(ELBUF_STR%GBUF%GAMA_R(NEL*G_GAMA_R)         ,STAT=ERR)                         
      GBUF%GAMA_R = ZERO  
c---
      ALLOCATE (ELBUF_STR%GBUF%NOFF(NEL*G_NOFF)  ,STAT=ERR)                             
        GBUF%NOFF(1:NEL*G_NOFF)  = NINT(RBUF(IAD+1:IAD+NEL*G_NOFF))                            
          IAD = IAD+NEL*G_NOFF                                                          
      ALLOCATE (ELBUF_STR%GBUF%IERR(NEL*G_IERR)  ,STAT=ERR)                             
        GBUF%IERR(1:NEL*G_IERR)  = NINT(RBUF(IAD+1:IAD+NEL*G_IERR))                            
          IAD = IAD+NEL*G_IERR                                                          
      ALLOCATE (ELBUF_STR%GBUF%GAMA(NEL*G_GAMA)  ,STAT=ERR)
        GBUF%GAMA(1:NEL*G_GAMA)  = RBUF(IAD+1:IAD+NEL*G_GAMA)                    
          IAD = IAD+NEL*G_GAMA
c      ALLOCATE (ELBUF_STR%GBUF%SMSTR(NEL*G_SMSTR) ,STAT=ERR)                     
c        GBUF%SMSTR(1:NEL*G_SMSTR) = RBUF(IAD+1:IAD+NEL*G_SMSTR)                  
c          IAD = IAD+NEL*G_SMSTR
      ALLOCATE (ELBUF_STR%GBUF%HOURG(NEL*G_HOURG) ,STAT=ERR)                     
        GBUF%HOURG(1:NEL*G_HOURG) = RBUF(IAD+1:IAD+NEL*G_HOURG)                  
          IAD = IAD+NEL*G_HOURG                                                  
      ALLOCATE (ELBUF_STR%GBUF%TAG22(NEL*G_TAG22) ,STAT=ERR)                     
        GBUF%TAG22(1:NEL*G_TAG22) = RBUF(IAD+1:IAD+NEL*G_TAG22)                  
          IAD = IAD+NEL*G_TAG22                 
      ALLOCATE (ELBUF_STR%GBUF%STRA(NEL*G_STRA) ,STAT=ERR)                     
        GBUF%STRA(1:NEL*G_STRA) = RBUF(IAD+1:IAD+NEL*G_STRA)                  
          IAD = IAD+NEL*G_STRA                 
      ALLOCATE (ELBUF_STR%GBUF%SIGI(NEL*G_SIGI) ,STAT=ERR)                     
        GBUF%SIGI(1:NEL*G_SIGI) = RBUF(IAD+1:IAD+NEL*G_SIGI)                  
          IAD = IAD+NEL*G_SIGI                 
      ALLOCATE (ELBUF_STR%GBUF%DMG(NEL*G_DMG) ,STAT=ERR)                     
        GBUF%DMG(1:NEL*G_DMG) = RBUF(IAD+1:IAD+NEL*G_DMG)                  
          IAD = IAD+NEL*G_DMG                 
      ALLOCATE (ELBUF_STR%GBUF%UELR(NEL*G_UELR) ,STAT=ERR)
        GBUF%UELR(1:NEL*G_UELR) = RBUF(IAD+1:IAD+NEL*G_UELR)
          IAD = IAD+NEL*G_UELR
      ALLOCATE (ELBUF_STR%GBUF%UELR1(NEL*G_UELR1) ,STAT=ERR)
        GBUF%UELR1(1:NEL*G_UELR1) = RBUF(IAD+1:IAD+NEL*G_UELR1)
          IAD = IAD+NEL*G_UELR1
      ALLOCATE (ELBUF_STR%GBUF%DAMDL(NEL*G_DAMDL) ,STAT=ERR)
        GBUF%DAMDL(1:NEL*G_DAMDL) = RBUF(IAD+1:IAD+NEL*G_DAMDL)
          IAD = IAD+NEL*G_DAMDL
      ALLOCATE (ELBUF_STR%GBUF%FOR (NEL*G_FOR)   ,STAT=ERR)                           
        GBUF%FOR(1:NEL*G_FOR)   = RBUF(IAD+1:IAD+NEL*G_FOR)                           
          IAD = IAD+NEL*G_FOR                                                         
      ALLOCATE (ELBUF_STR%GBUF%MOM (NEL*G_MOM)   ,STAT=ERR)                           
        GBUF%MOM(1:NEL*G_MOM)   = RBUF(IAD+1:IAD+NEL*G_MOM)                           
          IAD = IAD+NEL*G_MOM                                                         
      ALLOCATE (ELBUF_STR%GBUF%THK (NEL*G_THK)   ,STAT=ERR)                           
        GBUF%THK(1:NEL*G_THK)   = RBUF(IAD+1:IAD+NEL*G_THK)                           
          IAD = IAD+NEL*G_THK 
      ALLOCATE (ELBUF_STR%GBUF%STRW(NEL*G_STRW),STAT=ERR)                           
        GBUF%STRW(1:NEL*G_STRW)=RBUF(IAD+1:IAD+NEL*G_STRW)                   
          IAD = IAD+NEL*G_STRW                                  
      ALLOCATE (ELBUF_STR%GBUF%THK_I(NEL*G_THK_I),STAT=ERR)                           
        GBUF%THK_I(1:NEL*G_THK_I)=RBUF(IAD+1:IAD+NEL*G_THK_I)                           
          IAD = IAD+NEL*G_THK_I                                  
      ALLOCATE (ELBUF_STR%GBUF%JAC_I(NEL*G_JAC_I),STAT=ERR)                           
        GBUF%JAC_I(1:NEL*G_JAC_I)=RBUF(IAD+1:IAD+NEL*G_JAC_I)                           
          IAD = IAD+NEL*G_JAC_I                                  
      ALLOCATE (ELBUF_STR%GBUF%DT(NEL*G_DT),STAT=ERR)                           
        GBUF%DT(1:NEL*G_DT)=RBUF(IAD+1:IAD+NEL*G_DT)                           
          IAD = IAD+NEL*G_DT
      ALLOCATE (ELBUF_STR%GBUF%ISMS(NEL*G_ISMS),STAT=ERR)                           
        GBUF%ISMS(1:NEL*G_ISMS)=RBUF(IAD+1:IAD+NEL*G_ISMS)                           
          IAD = IAD+NEL*G_ISMS    
      ALLOCATE (ELBUF_STR%GBUF%BPRELD(NEL*G_BPRELD),STAT=ERR)                           
        GBUF%BPRELD(1:NEL*G_BPRELD)=RBUF(IAD+1:IAD+NEL*G_BPRELD)                           
          IAD = IAD+NEL*G_BPRELD    
      ALLOCATE (ELBUF_STR%GBUF%COR_NF(NEL*G_COR_NF),STAT=ERR)                           
        GBUF%COR_NF(1:NEL*G_COR_NF)=RBUF(IAD+1:IAD+NEL*G_COR_NF)                           
          IAD = IAD+NEL*G_COR_NF    
      ALLOCATE (ELBUF_STR%GBUF%COR_FR(NEL*G_COR_FR),STAT=ERR)                           
        GBUF%COR_FR(1:NEL*G_COR_FR)=RBUF(IAD+1:IAD+NEL*G_COR_FR)                           
          IAD = IAD+NEL*G_COR_FR    
      ALLOCATE (ELBUF_STR%GBUF%COR_XR(NEL*G_COR_XR),STAT=ERR)                           
        GBUF%COR_XR(1:NEL*G_COR_XR)=RBUF(IAD+1:IAD+NEL*G_COR_XR)                           
          IAD = IAD+NEL*G_COR_XR 
      ALLOCATE (ELBUF_STR%GBUF%MAXFRAC(NEL*G_MAXFRAC),STAT=ERR)                           
        GBUF%MAXFRAC(1:NEL*G_MAXFRAC)=RBUF(IAD+1:IAD+NEL*G_MAXFRAC)                           
          IAD = IAD+NEL*G_MAXFRAC   
      ALLOCATE (ELBUF_STR%GBUF%MAXEPS(NEL*G_MAXEPS),STAT=ERR)                           
        GBUF%MAXEPS(1:NEL*G_MAXEPS)=RBUF(IAD+1:IAD+NEL*G_MAXEPS)                           
          IAD = IAD+NEL*G_MAXEPS
      ALLOCATE (ELBUF_STR%GBUF%BETAORTH (NEL*G_BETAORTH)   ,STAT=ERR)                         
        GBUF%BETAORTH(1:NEL*G_BETAORTH)   = RBUF(IAD+1:IAD+NEL*G_BETAORTH)                         
          IAD = IAD+NEL*G_BETAORTH
      ALLOCATE (ELBUF_STR%GBUF%AMU(NEL*G_AMU),STAT=ERR)                           
        GBUF%AMU(1:NEL*G_AMU)=RBUF(IAD+1:IAD+NEL*G_AMU)                           
          IAD = IAD+NEL*G_AMU
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
        ELBUF_STR%GBUF%STRPG => GBUF%STRA
      ELSEIF (NPG > 1) THEN
        ALLOCATE (ELBUF_STR%GBUF%FORPG(NEL*G_FORPG) ,STAT=ERR)                     
          GBUF%FORPG(1:NEL*G_FORPG) = RBUF(IAD+1:IAD+NEL*G_FORPG)                  
            IAD = IAD+NEL*G_FORPG                 
        ALLOCATE (ELBUF_STR%GBUF%MOMPG(NEL*G_MOMPG) ,STAT=ERR)                     
          GBUF%MOMPG(1:NEL*G_MOMPG) = RBUF(IAD+1:IAD+NEL*G_MOMPG)                  
            IAD = IAD+NEL*G_MOMPG
C
        ALLOCATE (ELBUF_STR%GBUF%FORPGPINCH(NEL*G_FORPGPINCH) ,STAT=ERR)                     
          GBUF%FORPGPINCH(1:NEL*G_FORPGPINCH) = RBUF(IAD+1:IAD+NEL*G_FORPGPINCH)                  
            IAD = IAD+NEL*G_FORPGPINCH                 
        ALLOCATE (ELBUF_STR%GBUF%MOMPGPINCH(NEL*G_MOMPGPINCH) ,STAT=ERR)                     
          GBUF%MOMPG(1:NEL*G_MOMPGPINCH) = RBUF(IAD+1:IAD+NEL*G_MOMPGPINCH)                  
            IAD = IAD+NEL*G_MOMPGPINCH   
        ALLOCATE (ELBUF_STR%GBUF%EPGPINCHXZ(NEL*G_EPGPINCHXZ) ,STAT=ERR)                     
          GBUF%EPGPINCHXZ(1:NEL*G_EPGPINCHXZ) = RBUF(IAD+1:IAD+NEL*G_EPGPINCHXZ)                  
            IAD = IAD+NEL*G_EPGPINCHXZ 
        ALLOCATE (ELBUF_STR%GBUF%EPGPINCHYZ(NEL*G_EPGPINCHYZ) ,STAT=ERR)                     
          GBUF%EPGPINCHYZ(1:NEL*G_EPGPINCHYZ) = RBUF(IAD+1:IAD+NEL*G_EPGPINCHYZ)                  
            IAD = IAD+NEL*G_EPGPINCHYZ 
        ALLOCATE (ELBUF_STR%GBUF%EPGPINCHZZ(NEL*G_EPGPINCHZZ) ,STAT=ERR)                     
          GBUF%EPGPINCHZZ(1:NEL*G_EPGPINCHZZ) = RBUF(IAD+1:IAD+NEL*G_EPGPINCHZZ)                  
            IAD = IAD+NEL*G_EPGPINCHZZ 
C                  
        ALLOCATE (ELBUF_STR%GBUF%STRPG(NEL*G_STRPG) ,STAT=ERR)                     
          GBUF%STRPG(1:NEL*G_STRPG) = RBUF(IAD+1:IAD+NEL*G_STRPG)                  
            IAD = IAD+NEL*G_STRPG  
        ALLOCATE (ELBUF_STR%GBUF%STRWPG(NEL*G_STRWPG),STAT=ERR)                           
        GBUF%STRWPG(1:NEL*G_STRWPG)=RBUF(IAD+1:IAD+NEL*G_STRWPG)                   
          IAD = IAD+NEL*G_STRWPG                                  
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
        ALLOCATE (ELBUF_STR%GBUF%BFRAC(NEL*G_BFRAC)  ,STAT=ERR)
          GBUF%BFRAC(1:NEL*G_BFRAC) = RBUF(IAD+1:IAD+NEL*G_BFRAC)                        
            IAD = IAD+NEL*G_BFRAC                                                      
        ALLOCATE (ELBUF_STR%GBUF%OFF (NEL*G_OFF)   ,STAT=ERR)                    
          GBUF%OFF(1:NEL*G_OFF)   = RBUF(IAD+1:IAD+NEL*G_OFF)                    
            IAD = IAD+NEL*G_OFF                                                  
        ALLOCATE (ELBUF_STR%GBUF%EINT(NEL*G_EINT)  ,STAT=ERR)                         
          GBUF%EINT(1:NEL*G_EINT) = RBUF(IAD+1:IAD+NEL*G_EINT)                        
            IAD = IAD+NEL*G_EINT                                                      
        ALLOCATE (ELBUF_STR%GBUF%EINS(NEL*G_EINS)  ,STAT=ERR)                         
          GBUF%EINS(1:NEL*G_EINS) = RBUF(IAD+1:IAD+NEL*G_EINS)                        
            IAD = IAD+NEL*G_EINS                                                      
        ALLOCATE (ELBUF_STR%GBUF%RHO (NEL*G_RHO)   ,STAT=ERR)                         
          GBUF%RHO(1:NEL*G_RHO)   = RBUF(IAD+1:IAD+NEL*G_RHO)                         
            IAD = IAD+NEL*G_RHO                                                       
        ALLOCATE (ELBUF_STR%GBUF%QVIS(NEL*G_QVIS)  ,STAT=ERR)                         
          GBUF%QVIS(1:NEL*G_QVIS) = RBUF(IAD+1:IAD+NEL*G_QVIS)                        
            IAD = IAD+NEL*G_QVIS                                                      
        ALLOCATE (ELBUF_STR%GBUF%DELTAX(NEL*G_DELTAX),STAT=ERR)                       
          GBUF%DELTAX(1:NEL*G_DELTAX) = RBUF(IAD+1:IAD+NEL*G_DELTAX)                  
            IAD = IAD+NEL*G_DELTAX                                                    
        ALLOCATE (ELBUF_STR%GBUF%VOL (NEL*G_VOL)   ,STAT=ERR)                         
          GBUF%VOL(1:NEL*G_VOL)   = RBUF(IAD+1:IAD+NEL*G_VOL)                         
            IAD = IAD+NEL*G_VOL                                                       
        ALLOCATE (ELBUF_STR%GBUF%EPSD(NEL*G_EPSD)  ,STAT=ERR)                         
          GBUF%EPSD(1:NEL*G_EPSD)   = RBUF(IAD+1:IAD+NEL*G_EPSD)                      
            IAD = IAD+NEL*G_EPSD                                                      
        ALLOCATE (ELBUF_STR%GBUF%EPSQ(NEL*G_EPSQ)   ,STAT=ERR)                         
          GBUF%EPSQ(1:NEL*G_EPSQ)   = RBUF(IAD+1:IAD+NEL*G_EPSQ)                         
            IAD = IAD+NEL*G_EPSQ                                                       
        ALLOCATE (ELBUF_STR%GBUF%PLA (NEL*G_PLA)   ,STAT=ERR)                         
          GBUF%PLA(1:NEL*G_PLA)   = RBUF(IAD+1:IAD+NEL*G_PLA)                         
            IAD = IAD+NEL*G_PLA                                                       
        ALLOCATE (ELBUF_STR%GBUF%TEMP(NEL*G_TEMP)  ,STAT=ERR)                         
          GBUF%TEMP(1:NEL*G_TEMP) = RBUF(IAD+1:IAD+NEL*G_TEMP)                        
            IAD = IAD+NEL*G_TEMP                                                      
        ALLOCATE (ELBUF_STR%GBUF%TB(NEL*G_TB)  ,STAT=ERR)                             
          GBUF%TB(1:NEL*G_TB) = RBUF(IAD+1:IAD+NEL*G_TB)                              
            IAD = IAD+NEL*G_TB                                                        
        ALLOCATE (ELBUF_STR%GBUF%RK(NEL*G_RK)  ,STAT=ERR)                             
          GBUF%RK(1:NEL*G_RK) = RBUF(IAD+1:IAD+NEL*G_RK)                              
            IAD = IAD+NEL*G_RK                                                        
        ALLOCATE (ELBUF_STR%GBUF%RE(NEL*G_RE)  ,STAT=ERR)                             
          GBUF%RE(1:NEL*G_RE) = RBUF(IAD+1:IAD+NEL*G_RE)                              
            IAD = IAD+NEL*G_RE                                                        
        ALLOCATE (ELBUF_STR%GBUF%SIG (NEL*G_SIG)   ,STAT=ERR)                         
          GBUF%SIG(1:NEL*G_SIG)   = RBUF(IAD+1:IAD+NEL*G_SIG)                         
            IAD = IAD+NEL*G_SIG                                                       
        ALLOCATE (ELBUF_STR%GBUF%FORTH (NEL*G_FORTH)   ,STAT=ERR)                         
          GBUF%FORTH(1:NEL*G_FORTH)   = RBUF(IAD+1:IAD+NEL*G_FORTH)                         
            IAD = IAD+NEL*G_FORTH 
        ALLOCATE (ELBUF_STR%GBUF%EINTTH (NEL*G_EINTTH)   ,STAT=ERR)                         
          GBUF%EINTTH(1:NEL*G_EINTTH)   = RBUF(IAD+1:IAD+NEL*G_EINTTH)                         
            IAD = IAD+NEL*G_EINTTH
        ALLOCATE (ELBUF_STR%GBUF%SEQ (NEL*G_SEQ)   ,STAT=ERR)                         
          GBUF%SEQ(1:NEL*G_SEQ)   = RBUF(IAD+1:IAD+NEL*G_SEQ)                         
            IAD = IAD+NEL*G_SEQ
        ALLOCATE (ELBUF_STR%GBUF%ABURN(NEL*G_ABURN)  ,STAT=ERR)
          GBUF%ABURN(1:NEL*G_ABURN) = RBUF(IAD+1:IAD+NEL*G_ABURN)                        
            IAD = IAD+NEL*G_ABURN                                                      
        ALLOCATE (ELBUF_STR%GBUF%MU(NEL*G_MU)  ,STAT=ERR)
          GBUF%MU(1:NEL*G_MU) = RBUF(IAD+1:IAD+NEL*G_MU)                        
            IAD = IAD+NEL*G_MU                                                      

      ENDIF  ! NPT   
c
      ALLOCATE (ELBUF_STR%GBUF%FILL (NEL*G_FILL)   ,STAT=ERR)                         
        GBUF%FILL(1:NEL*G_FILL)   = RBUF(IAD+1:IAD+NEL*G_FILL)                         
          IAD = IAD+NEL*G_FILL                                                     
c
c---
c 1-D elements (TRUSS, BEAM, SPRING)
c---
      ALLOCATE (ELBUF_STR%GBUF%AREA(NEL*G_AREA)               ,STAT=ERR)
        GBUF%AREA(1:NEL*G_AREA) = RBUF(IAD+1:IAD+NEL*G_AREA)
          IAD = IAD+NEL*G_AREA
      ALLOCATE (ELBUF_STR%GBUF%SKEW(NEL*G_SKEW)               ,STAT=ERR)
        GBUF%SKEW(1:NEL*G_SKEW) = RBUF(IAD+1:IAD+NEL*G_SKEW)
          IAD = IAD+NEL*G_SKEW
      ALLOCATE (ELBUF_STR%GBUF%LENGTH(NEL*G_LENGTH)           ,STAT=ERR)
        GBUF%LENGTH(1:NEL*G_LENGTH) = RBUF(IAD+1:IAD+NEL*G_LENGTH)
          IAD = IAD+NEL*G_LENGTH
      ALLOCATE (ELBUF_STR%GBUF%TOTDEPL(NEL*G_TOTDEPL)         ,STAT=ERR)
        GBUF%TOTDEPL(1:NEL*G_TOTDEPL) = RBUF(IAD+1:IAD+NEL*G_TOTDEPL)
          IAD = IAD+NEL*G_TOTDEPL
      ALLOCATE (ELBUF_STR%GBUF%TOTROT(NEL*G_TOTROT)           ,STAT=ERR)
        GBUF%TOTROT(1:NEL*G_TOTROT) = RBUF(IAD+1:IAD+NEL*G_TOTROT) 
          IAD = IAD+NEL*G_TOTROT
      ALLOCATE (ELBUF_STR%GBUF%FOREP(NEL*G_FOREP)             ,STAT=ERR)
        GBUF%FOREP(1:NEL*G_FOREP) = RBUF(IAD+1:IAD+NEL*G_FOREP)
          IAD = IAD+NEL*G_FOREP
      ALLOCATE (ELBUF_STR%GBUF%MOMEP(NEL*G_MOMEP)             ,STAT=ERR)
        GBUF%MOMEP(1:NEL*G_MOMEP) = RBUF(IAD+1:IAD+NEL*G_MOMEP)
          IAD = IAD+NEL*G_MOMEP
      ALLOCATE (ELBUF_STR%GBUF%DEP_IN_TENS(NEL*G_DEP_IN_TENS) ,STAT=ERR)
        GBUF%DEP_IN_TENS(1:NEL*G_DEP_IN_TENS) = RBUF(IAD+1:IAD+NEL*G_DEP_IN_TENS)
          IAD = IAD+NEL*G_DEP_IN_TENS
      ALLOCATE (ELBUF_STR%GBUF%DEP_IN_COMP(NEL*G_DEP_IN_COMP) ,STAT=ERR)
        GBUF%DEP_IN_COMP(1:NEL*G_DEP_IN_COMP) = RBUF(IAD+1:IAD+NEL*G_DEP_IN_COMP)
          IAD = IAD+NEL*G_DEP_IN_COMP
      ALLOCATE (ELBUF_STR%GBUF%ROT_IN_TENS(NEL*G_ROT_IN_TENS) ,STAT=ERR)
        GBUF%ROT_IN_TENS(1:NEL*G_ROT_IN_TENS) = RBUF(IAD+1:IAD+NEL*G_ROT_IN_TENS)
          IAD = IAD+NEL*G_ROT_IN_TENS
      ALLOCATE (ELBUF_STR%GBUF%ROT_IN_COMP(NEL*G_ROT_IN_COMP) ,STAT=ERR)
        GBUF%ROT_IN_COMP(1:NEL*G_ROT_IN_COMP) = RBUF(IAD+1:IAD+NEL*G_ROT_IN_COMP)
          IAD = IAD+NEL*G_ROT_IN_COMP
      ALLOCATE (ELBUF_STR%GBUF%POSX(NEL*G_POSX)               ,STAT=ERR)
        GBUF%POSX(1:NEL*G_POSX) = RBUF(IAD+1:IAD+NEL*G_POSX)
          IAD = IAD+NEL*G_POSX
      ALLOCATE (ELBUF_STR%GBUF%POSY(NEL*G_POSY)               ,STAT=ERR)
        GBUF%POSY(1:NEL*G_POSY) = RBUF(IAD+1:IAD+NEL*G_POSY)
          IAD = IAD+NEL*G_POSY
      ALLOCATE (ELBUF_STR%GBUF%POSZ(NEL*G_POSZ)               ,STAT=ERR)
        GBUF%POSZ(1:NEL*G_POSZ) = RBUF(IAD+1:IAD+NEL*G_POSZ)
          IAD = IAD+NEL*G_POSZ
      ALLOCATE (ELBUF_STR%GBUF%POSXX(NEL*G_POSXX)             ,STAT=ERR)
        GBUF%POSXX(1:NEL*G_POSXX) = RBUF(IAD+1:IAD+NEL*G_POSXX)
          IAD = IAD+NEL*G_POSXX
      ALLOCATE (ELBUF_STR%GBUF%POSYY(NEL*G_POSYY)             ,STAT=ERR)
        GBUF%POSYY(1:NEL*G_POSYY) = RBUF(IAD+1:IAD+NEL*G_POSYY)
          IAD = IAD+NEL*G_POSYY
      ALLOCATE (ELBUF_STR%GBUF%POSZZ(NEL*G_POSZZ)             ,STAT=ERR)
        GBUF%POSZZ(1:NEL*G_POSZZ) = RBUF(IAD+1:IAD+NEL*G_POSZZ)
          IAD = IAD+NEL*G_POSZZ
      ALLOCATE (ELBUF_STR%GBUF%YIELD(NEL*G_YIELD)             ,STAT=ERR)
        GBUF%YIELD(1:NEL*G_YIELD) = RBUF(IAD+1:IAD+NEL*G_YIELD)
          IAD = IAD+NEL*G_YIELD
      ALLOCATE (ELBUF_STR%GBUF%LENGTH_ERR(NEL*G_LENGTH_ERR)   ,STAT=ERR)
        GBUF%LENGTH_ERR(1:NEL*G_LENGTH_ERR) = RBUF(IAD+1:IAD+NEL*G_LENGTH_ERR)
          IAD = IAD+NEL*G_LENGTH_ERR
      ALLOCATE (ELBUF_STR%GBUF%DV(NEL*G_DV)                   ,STAT=ERR)
        GBUF%DV(1:NEL*G_DV) = RBUF(IAD+1:IAD+NEL*G_DV)
          IAD = IAD+NEL*G_DV
      ALLOCATE (ELBUF_STR%GBUF%DFS(NEL*G_DFS)                 ,STAT=ERR)
        GBUF%DFS(1:NEL*G_DFS) = RBUF(IAD+1:IAD+NEL*G_DFS)
          IAD = IAD+NEL*G_DFS
      ALLOCATE (ELBUF_STR%GBUF%SKEW_ERR(NEL*G_SKEW_ERR)       ,STAT=ERR)
        GBUF%SKEW_ERR(1:NEL*G_SKEW_ERR) = RBUF(IAD+1:IAD+NEL*G_SKEW_ERR)
          IAD = IAD+NEL*G_SKEW_ERR
      ALLOCATE (ELBUF_STR%GBUF%E6(NEL*G_E6)                   ,STAT=ERR)
        GBUF%E6(1:NEL*G_E6) = RBUF(IAD+1:IAD+NEL*G_E6)
          IAD = IAD+NEL*G_E6
      ALLOCATE (ELBUF_STR%GBUF%RUPTCRIT(NEL*G_RUPTCRIT)       ,STAT=ERR)
        GBUF%RUPTCRIT(1:NEL*G_RUPTCRIT) = RBUF(IAD+1:IAD+NEL*G_RUPTCRIT)
          IAD = IAD+NEL*G_RUPTCRIT
      ALLOCATE (ELBUF_STR%GBUF%MASS(NEL*G_MASS)               ,STAT=ERR)
        GBUF%MASS(1:NEL*G_MASS) = RBUF(IAD+1:IAD+NEL*G_MASS)
          IAD = IAD+NEL*G_MASS
      ALLOCATE (ELBUF_STR%GBUF%V_REPCVT(NEL*G_V_REPCVT)       ,STAT=ERR)
        GBUF%V_REPCVT(1:NEL*G_V_REPCVT) = RBUF(IAD+1:IAD+NEL*G_V_REPCVT)
          IAD = IAD+NEL*G_V_REPCVT
      ALLOCATE (ELBUF_STR%GBUF%VR_REPCVT(NEL*G_VR_REPCVT)     ,STAT=ERR)
        GBUF%VR_REPCVT(1:NEL*G_VR_REPCVT) = RBUF(IAD+1:IAD+NEL*G_VR_REPCVT)
          IAD = IAD+NEL*G_VR_REPCVT
      ALLOCATE (ELBUF_STR%GBUF%VAR(NEL*G_NUVAR)               ,STAT=ERR)
        GBUF%VAR(1:NEL*G_NUVAR) = RBUF(IAD+1:IAD+NEL*G_NUVAR)
          IAD = IAD+NEL*G_NUVAR
      ALLOCATE (ELBUF_STR%GBUF%VARN(NEL*G_NUVARN)             ,STAT=ERR)
        GBUF%VARN(1:NEL*G_NUVARN) = RBUF(IAD+1:IAD+NEL*G_NUVARN)
          IAD = IAD+NEL*G_NUVARN
      ALLOCATE (ELBUF_STR%GBUF%DEFINI(NEL*G_DEFINI)             ,STAT=ERR)
        GBUF%DEFINI(1:NEL*G_DEFINI) = RBUF(IAD+1:IAD+NEL*G_DEFINI)
          IAD = IAD+NEL*G_DEFINI
      ALLOCATE (ELBUF_STR%GBUF%FORINI(NEL*G_FORINI)             ,STAT=ERR)
        GBUF%FORINI(1:NEL*G_FORINI) = RBUF(IAD+1:IAD+NEL*G_FORINI)
          IAD = IAD+NEL*G_FORINI
      ALLOCATE (ELBUF_STR%GBUF%INIFRIC(NEL*G_INIFRIC)         ,STAT=ERR)
        GBUF%INIFRIC(1:NEL*G_INIFRIC) = RBUF(IAD+1:IAD+NEL*G_INIFRIC)
          IAD = IAD+NEL*G_INIFRIC
      ALLOCATE (ELBUF_STR%GBUF%STRHG(NEL*G_STRHG)         ,STAT=ERR)
        GBUF%STRHG(1:NEL*G_STRHG) = RBUF(IAD+1:IAD+NEL*G_STRHG)
          IAD = IAD+NEL*G_STRHG
      ALLOCATE (ELBUF_STR%GBUF%DT_PITER(NEL*G_DT_PITER)         ,STAT=ERR)
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
      ALLOCATE (ELBUF_STR%GBUF%SKEW_ID(NEL*G_SKEW_ID)         ,STAT=ERR)
        GBUF%SKEW_ID(1:NEL*G_SKEW_ID) = NINT(RBUF(IAD+1:IAD+NEL*G_SKEW_ID))
          IAD = IAD+NEL*G_SKEW_ID
c---
c seatbelt elements
c---
      ALLOCATE (ELBUF_STR%GBUF%SLIPRING_ID(NEL*G_SLIPRING_ID) ,STAT=ERR)
        GBUF%SLIPRING_ID(1:NEL*G_SLIPRING_ID) = NINT(RBUF(IAD+1:IAD+NEL*G_SLIPRING_ID))
          IAD = IAD+NEL*G_SLIPRING_ID
      ALLOCATE (ELBUF_STR%GBUF%SLIPRING_FRAM_ID(NEL*G_SLIPRING_FRAM_ID) ,STAT=ERR)
        GBUF%SLIPRING_FRAM_ID(1:NEL*G_SLIPRING_FRAM_ID) = NINT(RBUF(IAD+1:IAD+NEL*G_SLIPRING_FRAM_ID))
          IAD = IAD+NEL*G_SLIPRING_FRAM_ID
      ALLOCATE (ELBUF_STR%GBUF%SLIPRING_STRAND(NEL*G_SLIPRING_STRAND) ,STAT=ERR)
        GBUF%SLIPRING_STRAND(1:NEL*G_SLIPRING_STRAND) = NINT(RBUF(IAD+1:IAD+NEL*G_SLIPRING_STRAND))
          IAD = IAD+NEL*G_SLIPRING_STRAND
      ALLOCATE (ELBUF_STR%GBUF%RETRACTOR_ID(NEL*G_RETRACTOR_ID) ,STAT=ERR)
        GBUF%RETRACTOR_ID(1:NEL*G_RETRACTOR_ID) = NINT(RBUF(IAD+1:IAD+NEL*G_RETRACTOR_ID))
          IAD = IAD+NEL*G_RETRACTOR_ID
      ALLOCATE (ELBUF_STR%GBUF%RINGSLIP(NEL*G_RINGSLIP) ,STAT=ERR)
        GBUF%RINGSLIP(1:NEL*G_RINGSLIP) = RBUF(IAD+1:IAD+NEL*G_RINGSLIP)
          IAD = IAD+NEL*G_RINGSLIP
      ALLOCATE (ELBUF_STR%GBUF%ADD_NODE(NEL*G_ADD_NODE) ,STAT=ERR)
        GBUF%ADD_NODE(1:NEL*G_ADD_NODE) = NINT(RBUF(IAD+1:IAD+NEL*G_ADD_NODE))
          IAD = IAD+NEL*G_ADD_NODE
      ALLOCATE (ELBUF_STR%GBUF%UPDATE(NEL*G_UPDATE) ,STAT=ERR)
        GBUF%UPDATE(1:NEL*G_UPDATE) = NINT(RBUF(IAD+1:IAD+NEL*G_UPDATE))
          IAD = IAD+NEL*G_UPDATE     
      ALLOCATE (ELBUF_STR%GBUF%FRAM_FACTOR(NEL*G_FRAM_FACTOR) ,STAT=ERR)
        GBUF%FRAM_FACTOR(1:NEL*G_FRAM_FACTOR) = RBUF(IAD+1:IAD+NEL*G_FRAM_FACTOR)
          IAD = IAD+NEL*G_FRAM_FACTOR    
      ALLOCATE (ELBUF_STR%GBUF%INTVAR(NEL*G_INTVAR) ,STAT=ERR)
        GBUF%INTVAR(1:NEL*G_INTVAR) = RBUF(IAD+1:IAD+NEL*G_INTVAR)
          IAD = IAD+NEL*G_INTVAR
      ALLOCATE (ELBUF_STR%GBUF%IDT_TSH(NEL*G_IDT_TSH) ,STAT=ERR)
        GBUF%IDT_TSH(1:NEL*G_IDT_TSH) = NINT(RBUF(IAD+1:IAD+NEL*G_IDT_TSH))
          IAD = IAD+NEL*G_IDT_TSH
      IF (NPG > 1) THEN
        ALLOCATE (ELBUF_STR%GBUF%TEMPG(NEL*GBUF%G_TEMPG),STAT=ERR) 
        GBUF%TEMPG(1:NEL*GBUF%G_TEMPG)=RBUF(IAD+1:IAD+NEL*GBUF%G_TEMPG) 
        IAD = IAD + NEL*GBUF%G_TEMPG
      ENDIF
c---
c TMAX(H3D)
c---
      ALLOCATE (ELBUF_STR%GBUF%TM_YIELD(NEL*G_TM_YIELD) ,STAT=ERR)
        GBUF%TM_YIELD(1:NEL*G_TM_YIELD) = RBUF(IAD+1:IAD+NEL*G_TM_YIELD)
          IAD = IAD+NEL*G_TM_YIELD
      ALLOCATE (ELBUF_STR%GBUF%TM_SEQ(NEL*G_TM_SEQ) ,STAT=ERR)
        GBUF%TM_SEQ(1:NEL*G_TM_SEQ) = RBUF(IAD+1:IAD+NEL*G_TM_SEQ)
          IAD = IAD+NEL*G_TM_SEQ
      ALLOCATE (ELBUF_STR%GBUF%TM_EINT(NEL*G_TM_EINT) ,STAT=ERR)
        GBUF%TM_EINT(1:NEL*G_TM_EINT) = RBUF(IAD+1:IAD+NEL*G_TM_EINT)
          IAD = IAD+NEL*G_TM_EINT
      ALLOCATE (ELBUF_STR%GBUF%TM_DMG(NEL*G_TM_DMG) ,STAT=ERR)
        GBUF%TM_DMG(1:NEL*G_TM_DMG) = RBUF(IAD+1:IAD+NEL*G_TM_DMG)
          IAD = IAD+NEL*G_TM_DMG
      ALLOCATE (ELBUF_STR%GBUF%TM_SIG1(NEL*G_TM_SIG) ,STAT=ERR)
        GBUF%TM_SIG1(1:NEL*G_TM_SIG) = RBUF(IAD+1:IAD+NEL*G_TM_SIG)
          IAD = IAD+NEL*G_TM_SIG
      ALLOCATE (ELBUF_STR%GBUF%TM_SIG3(NEL*G_TM_SIG) ,STAT=ERR)
        GBUF%TM_SIG3(1:NEL*G_TM_SIG) = RBUF(IAD+1:IAD+NEL*G_TM_SIG)
          IAD = IAD+NEL*G_TM_SIG
      ALLOCATE (ELBUF_STR%GBUF%TM_STRA1(NEL*G_TM_STRA) ,STAT=ERR)
        GBUF%TM_STRA1(1:NEL*G_TM_STRA) = RBUF(IAD+1:IAD+NEL*G_TM_STRA)
          IAD = IAD+NEL*G_TM_STRA
      ALLOCATE (ELBUF_STR%GBUF%TM_STRA3(NEL*G_TM_STRA) ,STAT=ERR)
        GBUF%TM_STRA3(1:NEL*G_TM_STRA) = RBUF(IAD+1:IAD+NEL*G_TM_STRA)
          IAD = IAD+NEL*G_TM_STRA
c--- work array
      IF (G_TM_SIG>0) ALLOCATE (ELBUF_STR%GBUF%TM_PSIG(NEL*2),STAT=ERR)
      IF (G_TM_STRA>0) ALLOCATE (ELBUF_STR%GBUF%TM_PSTRA(NEL*2),STAT=ERR)
c
C-----
      IF (ALLOCATED(RBUF)) DEALLOCATE (RBUF)
      SDP_RBUF = SDP_RBUF + GBUF%G_SMSTR * NEL
      ALLOCATE (ELBUF_STR%GBUF%SMSTR(NEL*G_SMSTR) ,STAT=ERR)  
      IF (SDP_RBUF > 0)THEN
        ALLOCATE (DP_RBUF(SDP_RBUF))
        CALL READ_DPDB(DP_RBUF,SDP_RBUF)
c-------------------------------------------------
c       local buffer variables first
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
       DEALLOCATE(DP_RBUF)
      ENDIF

C-----------------------------------------------
      END SUBROUTINE ALLOCBUF_AUTO
