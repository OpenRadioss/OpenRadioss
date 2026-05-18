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
!||    alloc_elbuf_imp   ../engine/source/elements/elbuf/alloc_elbuf_imp.F
!||--- called by ------------------------------------------------------
!||    resol             ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod      ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      SUBROUTINE ALLOC_ELBUF_IMP(ELBUF_TAB,ELBUF_IMP,NGROUP,IPARG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_alloc_mod
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGROUP
      INTEGER IPARG(NPARG,NGROUP)
      TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP),TARGET ::ELBUF_TAB,ELBUF_IMP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG,ITY,ERR,BUFLEN,NEL,NPT,NPG,NLAY,NPTR,NPTS,NPTT,NUVAR,NVARTMP,
     .   NFAIL,K,IL,IR,IS,IT,IGTYP,NPTTOT,I,NONL,INLOC,NVAR_EOS,NVARTMP_EOS
      INTEGER ! global variables 
     .   G_GAMA,G_SIG,G_OFF,G_NOFF,G_EINT,G_EINS,G_TEMP,
     .   G_RHO,G_PLA,G_VOL,G_EPSD,G_QVIS,G_DELTAX,G_TB,G_RK,G_RE,G_WPLA
     .   G_SMSTR,G_HOURG,G_BFRAC,G_THK,G_FOR,G_MOM,G_TAG22,G_STRA,G_SIGI,
     .   G_DMG,G_FORPG,G_MOMPG,G_STRPG,G_UELR,G_UELR1,G_EPSQ,G_IERR,
     .   G_DAMDL,G_FORTH,G_EINTTH,G_SEQ,
     .   G_AREA,G_SKEW,G_LENGTH,G_TOTDEPL,G_TOTROT,G_FOREP,G_MOMEP,
     .   G_DEP_IN_TENS,G_DEP_IN_COMP,G_ROT_IN_TENS,G_ROT_IN_COMP,
     .   G_POSX,G_POSY,G_POSZ,G_POSXX,G_POSYY,G_POSZZ,G_YIELD,G_LENGTH_ERR,
     .   G_DV,G_DFS,G_SKEW_ERR,G_E6,G_RUPTCRIT,G_MASS,G_V_REPCVT,
     .   G_VR_REPCVT,G_NUVAR,G_NUVARN,G_INIFRIC,G_STRHG,G_ABURN,G_MU,
     .   G_BPRELD,G_COR_NF,G_COR_FR,G_COR_XR,G_DEFINI,G_FORINI,
     .   G_FORPGPINCH,G_MOMPGPINCH,G_EPGPINCHXZ,G_EPGPINCHYZ,G_EPGPINCHZZ,
     .   G_SKEW_ID,G_MAXFRAC,G_MAXEPS,G_SLIPRING_ID,G_SLIPRING_STRAND,G_RETRACTOR_ID,
     .   G_RINGSLIP,G_ADD_NODE,G_UPDATE,G_INTVAR,G_AMU,G_SLIPRING_FRAM_ID,G_FRAM_FACTOR,
     .   G_YIELD_IN_COMP,G_XXOLD_IN_COMP
      INTEGER  ! layer variables
     .   LY_DMG,LY_GAMA,LY_DIRA,LY_DIRB,LY_CRKDIR,LY_PLAPT,LY_SIGPT,LY_HOURG,
     .   LY_UELR,LY_UELR1,LY_OFFPG,LY_OFF,LEN_PLAPT,LEN_SIGPT
      INTEGER  ! local variables
     .   L_OFF,L_EINT,L_EINS,L_RHO,L_DP_DRHO,L_VOL,L_PLA,L_TB,L_TEMP,
     .   L_RK,L_RE,L_VK,L_ROB,L_SIG,L_SIGV,L_SIGL,L_SIGA,L_SIGD,L_SIGB,
     .   L_SIGC,L_SIGF,L_STRA,L_EPSA,L_EPSD,L_EPSQ,L_EPSF,L_CRAK,L_WPLA,
     .   L_ANG,L_SF,L_QVIS,L_DELTAX,L_GAMA,
     .   L_DAM,L_DSUM,L_DGLO,L_EPE,L_EPC,L_XST,L_SSP,L_Z,L_FRAC,L_VISC,
     .   L_THK,L_FOR,L_MOM,L_SMSTR,L_BFRAC,L_DMG,L_FORTH,L_EINTTH,L_SEQ,
     .   L_SIGPLY,L_FAC_YLD,L_ABURN,L_MU
      INTEGER  ! failure variables
     .   LF_DAM,LF_DAMMX,LF_DAMINI,LF_TDEL,LF_INDX,LF_OFF
      TYPE(BUF_LAY_)  , POINTER :: BUFLY,BUFLY_IMP
      TYPE(G_BUFEL_)  , POINTER :: GBUF
      TYPE(L_BUFEL_)  , POINTER :: LBUF
      TYPE(FAIL_LOC_) , POINTER :: FLOC
      TYPE(BUF_NLOC_) , POINTER :: BUFNL
      TYPE(BUF_NLOCTS_),POINTER :: BUFNLTS
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NPG = 0
      NPTTOT = 0
      DO NG = 1,NGROUP                                                 
       ITY = IPARG(5,NG)    
       INLOC = IPARG(78,NG) ! Non-local regularization flag                                                        
        IF (ITY == 1 .OR. ITY == 2 .OR. ITY == 51  .OR.
     .      ITY == 3 .OR. ITY == 7 .OR. ITY == 101 .OR.
     .      ITY == 4 .OR. ITY == 5 .OR. ITY == 6   .OR. ITY == 100) THEN
        IGTYP = ELBUF_TAB(NG)%IGTYP
        NEL   = ELBUF_TAB(NG)%NEL                                                     
        NLAY  = ELBUF_TAB(NG)%NLAY                                                    
        NPTR  = ELBUF_TAB(NG)%NPTR                                                    
        NPTS  = ELBUF_TAB(NG)%NPTS                                                    
        NPTT  = ELBUF_TAB(NG)%NPTT                                                    
        NPT   = NLAY*NPTR*NPTS*NPTT                                                   
        IF (ITY == 1 .OR. ITY == 2 .OR. ITY == 51 .OR.
     .      ITY == 101) THEN
          NPG = NPTR*NPTS*NPTT
          NPTTOT = NPT
        ELSEIF (ITY == 3 .OR. ITY == 7) THEN
          NPG = NPTR*NPTS
          NPTTOT  = 0
          DO IL=1,ELBUF_TAB(NG)%NLAY
            NPTTOT = NPTTOT + NPG*ELBUF_TAB(NG)%BUFLY(IL)%NPTT
          ENDDO
          IF (NPT == 0) NPTTOT = NPT
        ELSEIF (ITY == 4 .OR. ITY == 5 .OR. ITY == 6 .OR. ITY == 100) THEN
          NPG = 0
          NPTTOT = 0
        ELSE
          NPG = 0
          NPTTOT = 0
        ENDIF
c
        ALLOCATE (ELBUF_IMP(NG)%BUFLY(NLAY) ,STAT=ERR)                                
c
        DO IL = 1, NLAY                                                               
         IF (IGTYP == 51 .OR. IGTYP == 52) NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
         ALLOCATE(ELBUF_IMP(NG)%BUFLY(IL)%LBUF(NPTR,NPTS,NPTT),STAT=ERR)              
         ALLOCATE(ELBUF_IMP(NG)%BUFLY(IL)%MAT (NPTR,NPTS,NPTT),STAT=ERR)              
         ALLOCATE(ELBUF_IMP(NG)%BUFLY(IL)%FAIL(NPTR,NPTS,NPTT),STAT=ERR)              
         ALLOCATE(ELBUF_IMP(NG)%BUFLY(IL)%PROP(NPTR,NPTS,NPTT),STAT=ERR)              
         ALLOCATE(ELBUF_IMP(NG)%BUFLY(IL)%EOS (NPTR,NPTS,NPTT),STAT=ERR)              
         ALLOCATE(ELBUF_IMP(NG)%BUFLY(IL)%VISC(NPTR,NPTS,NPTT),STAT=ERR)              
         ALLOCATE(ELBUF_IMP(NG)%BUFLY(IL)%PORO(NPTR,NPTS,NPTT),STAT=ERR)              
        ENDDO                                                           
c-------------------------------------------------
C       skip interlayer buffer for PLY_XFEM
c-------------------------------------------------
C       allocation & read of local buffer variables
c-------------------------------------------------
        DO IL = 1, NLAY                                                            
         BUFLY => ELBUF_TAB(NG)%BUFLY(IL)                         
         BUFLY_IMP => ELBUF_IMP(NG)%BUFLY(IL)                         
c
         BUFLY_IMP%ILAW      = BUFLY%ILAW   
         BUFLY_IMP%IMAT      = BUFLY%IMAT   
         BUFLY_IMP%IEOS      = BUFLY%IEOS   
         BUFLY_IMP%IVISC     = BUFLY%IVISC    
         BUFLY_IMP%NFAIL     = BUFLY%NFAIL    
         BUFLY_IMP%NVAR_MAT  = BUFLY%NVAR_MAT    
         BUFLY_IMP%NVAR_EOS  = BUFLY%NVAR_EOS         
         BUFLY_IMP%NVARTMP   = BUFLY%NVARTMP    
         BUFLY_IMP%NVARTMP_EOS = BUFLY%NVARTMP_EOS         
         BUFLY_IMP%NVAR_VISC = BUFLY%NVAR_VISC    
         BUFLY_IMP%NVAR_LOC  = BUFLY%NVAR_LOC    
         BUFLY_IMP%NVAR_LAY  = BUFLY%NVAR_LAY   
         BUFLY_IMP%NPTT      = BUFLY%NPTT
c
         BUFLY_IMP%LY_DMG    = BUFLY%LY_DMG    
         BUFLY_IMP%LY_GAMA   = BUFLY%LY_GAMA   
         BUFLY_IMP%LY_DIRA   = BUFLY%LY_DIRA    
         BUFLY_IMP%LY_DIRB   = BUFLY%LY_DIRB   
         BUFLY_IMP%LY_CRKDIR = BUFLY%LY_CRKDIR
         BUFLY_IMP%LY_PLAPT  = BUFLY%LY_PLAPT   
         BUFLY_IMP%LY_SIGPT  = BUFLY%LY_SIGPT   
         BUFLY_IMP%LY_HOURG  = BUFLY%LY_HOURG
         BUFLY_IMP%LY_UELR   = BUFLY%LY_UELR
         BUFLY_IMP%LY_UELR1  = BUFLY%LY_UELR1
         BUFLY_IMP%LY_OFFPG  = BUFLY%LY_OFFPG
         BUFLY_IMP%LY_OFF    = BUFLY%LY_OFF
c
         call my_alloc(BUFLY_IMP%DMG, NEL*BUFLY%LY_DMG, "BUFLY_IMP%DMG", stat=ERR)
         call my_alloc(BUFLY_IMP%GAMA, NEL*BUFLY%LY_GAMA, "BUFLY_IMP%GAMA", stat=ERR)
         call my_alloc(BUFLY_IMP%DIRA, NEL*BUFLY%LY_DIRA, "BUFLY_IMP%DIRA", stat=ERR)
         call my_alloc(BUFLY_IMP%DIRB, NEL*BUFLY%LY_DIRB, "BUFLY_IMP%DIRB", stat=ERR)
         call my_alloc(BUFLY_IMP%CRKDIR, NEL*BUFLY%LY_CRKDIR, "BUFLY_IMP%CRKDIR", stat=ERR)
         IF (NPG > 1) THEN
           IF (NLAY > 1) THEN
             LEN_PLAPT = NEL*BUFLY%LY_PLAPT
             LEN_SIGPT = NEL*BUFLY%LY_SIGPT
           ELSE
             IF (IGTYP /= 51 .AND. IGTYP /= 52) THEN
               LEN_PLAPT = NEL*BUFLY%LY_PLAPT*NPT
               LEN_SIGPT = NEL*BUFLY%LY_SIGPT*NPT
             ELSE
               LEN_PLAPT = NEL*BUFLY%LY_PLAPT*BUFLY%NPTT
               LEN_SIGPT = NEL*BUFLY%LY_SIGPT*BUFLY%NPTT
             ENDIF
           ENDIF
           call my_alloc(BUFLY_IMP%PLAPT, NEL*LEN_PLAPT, "BUFLY_IMP%PLAPT", stat=ERR)
           call my_alloc(BUFLY_IMP%SIGPT, NEL*LEN_SIGPT, "BUFLY_IMP%SIGPT", stat=ERR)
cc           ALLOCATE(BUFLY_IMP%PLAPT(NEL*BUFLY%LY_PLAPT) ,STAT=ERR)                             
cc           ALLOCATE(BUFLY_IMP%SIGPT(NEL*BUFLY%LY_SIGPT) ,STAT=ERR)
         ENDIF                             
         call my_alloc(BUFLY_IMP%HOURG, NEL*BUFLY%LY_HOURG, "BUFLY_IMP%HOURG", stat=ERR)
         call my_alloc(BUFLY_IMP%UELR, NEL*BUFLY%LY_UELR, "BUFLY_IMP%UELR", stat=ERR)
         call my_alloc(BUFLY_IMP%UELR1, NEL*BUFLY%LY_UELR1, "BUFLY_IMP%UELR1", stat=ERR)
         call my_alloc(BUFLY_IMP%OFFPG, NEL*BUFLY%LY_OFFPG, "BUFLY_IMP%OFFPG", stat=ERR)
         call my_alloc(BUFLY_IMP%OFF, NEL*BUFLY%LY_OFF, "BUFLY_IMP%OFF", stat=ERR)
c
         BUFLY_IMP%L_OFF     = BUFLY%L_OFF      
         BUFLY_IMP%L_GAMA    = BUFLY%L_GAMA   
         BUFLY_IMP%L_STRA    = BUFLY%L_STRA   
         BUFLY_IMP%L_FRAC    = BUFLY%L_FRAC   
         BUFLY_IMP%L_BFRAC   = BUFLY%L_BFRAC   
         BUFLY_IMP%L_EINT    = BUFLY%L_EINT   
         BUFLY_IMP%L_EINS    = BUFLY%L_EINS   
         BUFLY_IMP%L_RHO     = BUFLY%L_RHO         
         BUFLY_IMP%L_DP_DRHO = BUFLY%L_DP_DRHO     
         BUFLY_IMP%L_QVIS    = BUFLY%L_QVIS   
         BUFLY_IMP%L_DELTAX  = BUFLY%L_DELTAX 
         BUFLY_IMP%L_VOL     = BUFLY%L_VOL    
         BUFLY_IMP%L_EPSA    = BUFLY%L_EPSA   
         BUFLY_IMP%L_EPSD    = BUFLY%L_EPSD   
         BUFLY_IMP%L_EPSQ    = BUFLY%L_EPSQ   
         BUFLY_IMP%L_EPSF    = BUFLY%L_EPSF   
         BUFLY_IMP%L_PLA     = BUFLY%L_PLA    
         BUFLY_IMP%L_WPLA    = BUFLY%L_WPLA    
         BUFLY_IMP%L_TEMP    = BUFLY%L_TEMP        
         BUFLY_IMP%L_TB      = BUFLY%L_TB     
         BUFLY_IMP%L_RK      = BUFLY%L_RK     
         BUFLY_IMP%L_RE      = BUFLY%L_RE     
         BUFLY_IMP%L_VK      = BUFLY%L_VK     
         BUFLY_IMP%L_SF      = BUFLY%L_SF     
         BUFLY_IMP%L_ROB     = BUFLY%L_ROB    
         BUFLY_IMP%L_DAM     = BUFLY%L_DAM    
         BUFLY_IMP%L_DSUM    = BUFLY%L_DSUM   
         BUFLY_IMP%L_DGLO    = BUFLY%L_DGLO     
         BUFLY_IMP%L_CRAK    = BUFLY%L_CRAK   
         BUFLY_IMP%L_ANG     = BUFLY%L_ANG      
         BUFLY_IMP%L_EPE     = BUFLY%L_EPE    
         BUFLY_IMP%L_EPC     = BUFLY%L_EPC    
         BUFLY_IMP%L_XST     = BUFLY%L_XST    
         BUFLY_IMP%L_SSP     = BUFLY%L_SSP    
         BUFLY_IMP%L_Z       = BUFLY%L_Z      
         BUFLY_IMP%L_VISC    = BUFLY%L_VISC   
         BUFLY_IMP%L_SIGL    = BUFLY%L_SIGL   
         BUFLY_IMP%L_SIGV    = BUFLY%L_SIGV   
         BUFLY_IMP%L_SIGA    = BUFLY%L_SIGA   
         BUFLY_IMP%L_SIGB    = BUFLY%L_SIGB   
         BUFLY_IMP%L_SIGC    = BUFLY%L_SIGC        
         BUFLY_IMP%L_SIGD    = BUFLY%L_SIGD        
         BUFLY_IMP%L_SIGF    = BUFLY%L_SIGF   
         BUFLY_IMP%L_SIG     = BUFLY%L_SIG    
         BUFLY_IMP%L_SIGPLY  = BUFLY%L_SIGPLY    
         BUFLY_IMP%L_FOR     = BUFLY%L_FOR    
         BUFLY_IMP%L_MOM     = BUFLY%L_MOM
         BUFLY_IMP%L_THK     = BUFLY%L_THK    
         BUFLY_IMP%L_SMSTR   = BUFLY%L_SMSTR    
         BUFLY_IMP%L_DMG     = BUFLY%L_DMG    
         BUFLY_IMP%L_FORTH   = BUFLY%L_FORTH
         BUFLY_IMP%L_EINTTH  = BUFLY%L_EINTTH
         BUFLY_IMP%L_SEQ     = BUFLY%L_SEQ
         BUFLY_IMP%L_FAC_YLD = BUFLY%L_FAC_YLD
         BUFLY_IMP%L_ABURN   = BUFLY%L_ABURN
         BUFLY_IMP%L_MU      = BUFLY%L_MU
         BUFLY_IMP%L_PLANL   = BUFLY%L_PLANL
         BUFLY_IMP%L_EPSDNL  = BUFLY%L_EPSDNL
         BUFLY_IMP%L_DMGSCL  = BUFLY%L_DMGSCL
         BUFLY_IMP%L_TSAIWU  = BUFLY%L_TSAIWU
c
         IF (IGTYP == 51 .OR. IGTYP == 52) THEN
           NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
         ELSE
           NPTT = ELBUF_TAB(NG)%NPTT
         ENDIF
         DO IR = 1,NPTR                                                            
          DO IS = 1,NPTS                                                            
           DO IT = 1,NPTT                                                           
            LBUF => ELBUF_IMP(NG)%BUFLY(IL)%LBUF(IR,IS,IT)                             
c
            call my_alloc(LBUF%OFF, NEL*BUFLY%L_OFF, "LBUF%OFF", stat=ERR)
            call my_alloc(LBUF%GAMA, NEL*BUFLY%L_GAMA, "LBUF%GAMA", stat=ERR)
            call my_alloc(LBUF%STRA, NEL*BUFLY%L_STRA, "LBUF%STRA", stat=ERR)
            call my_alloc(LBUF%FRAC, NEL*BUFLY%L_FRAC, "LBUF%FRAC", stat=ERR)
            call my_alloc(LBUF%BFRAC, NEL*BUFLY%L_BFRAC, "LBUF%BFRAC", stat=ERR)
            call my_alloc(LBUF%EINT, NEL*BUFLY%L_EINT, "LBUF%EINT", stat=ERR)
            call my_alloc(LBUF%EINS, NEL*BUFLY%L_EINS, "LBUF%EINS", stat=ERR)
            call my_alloc(LBUF%RHO, NEL*BUFLY%L_RHO, "LBUF%RHO", stat=ERR)
            call my_alloc(LBUF%DP_DRHO, NEL*BUFLY%L_DP_DRHO, "LBUF%DP_DRHO", stat=ERR)
            call my_alloc(LBUF%QVIS, NEL*BUFLY%L_QVIS, "LBUF%QVIS", stat=ERR)
            call my_alloc(LBUF%DELTAX, NEL*BUFLY%L_DELTAX, "LBUF%DELTAX", stat=ERR)
            call my_alloc(LBUF%VOL, NEL*BUFLY%L_VOL, "LBUF%VOL", stat=ERR)
            call my_alloc(LBUF%EPSA, NEL*BUFLY%L_EPSA, "LBUF%EPSA", stat=ERR)
            call my_alloc(LBUF%EPSD, NEL*BUFLY%L_EPSD, "LBUF%EPSD", stat=ERR)
            call my_alloc(LBUF%EPSQ, NEL*BUFLY%L_EPSQ, "LBUF%EPSQ", stat=ERR)
            call my_alloc(LBUF%EPSF, NEL*BUFLY%L_EPSF, "LBUF%EPSF", stat=ERR)
            call my_alloc(LBUF%PLA, NEL*BUFLY%L_PLA, "LBUF%PLA", stat=ERR)
            call my_alloc(LBUF%WPLA, NEL*BUFLY%L_WPLA, "LBUF%WPLA", stat=ERR)
            call my_alloc(LBUF%TEMP, NEL*BUFLY%L_TEMP, "LBUF%TEMP", stat=ERR)
            call my_alloc(LBUF%TB, NEL*BUFLY%L_TB, "LBUF%TB", stat=ERR)
            call my_alloc(LBUF%RK, NEL*BUFLY%L_RK, "LBUF%RK", stat=ERR)
            call my_alloc(LBUF%RE, NEL*BUFLY%L_RE, "LBUF%RE", stat=ERR)
            call my_alloc(LBUF%VK, NEL*BUFLY%L_VK, "LBUF%VK", stat=ERR)
            call my_alloc(LBUF%SF, NEL*BUFLY%L_SF, "LBUF%SF", stat=ERR)
            call my_alloc(LBUF%ROB, NEL*BUFLY%L_ROB, "LBUF%ROB", stat=ERR)
            call my_alloc(LBUF%DAM, NEL*BUFLY%L_DAM, "LBUF%DAM", stat=ERR)
            call my_alloc(LBUF%DSUM, NEL*BUFLY%L_DSUM, "LBUF%DSUM", stat=ERR)
            call my_alloc(LBUF%DGLO, NEL*BUFLY%L_DGLO, "LBUF%DGLO", stat=ERR)
            call my_alloc(LBUF%CRAK, NEL*BUFLY%L_CRAK, "LBUF%CRAK", stat=ERR)
            call my_alloc(LBUF%ANG, NEL*BUFLY%L_ANG, "LBUF%ANG", stat=ERR)
            call my_alloc(LBUF%EPE, NEL*BUFLY%L_EPE, "LBUF%EPE", stat=ERR)
            call my_alloc(LBUF%EPC, NEL*BUFLY%L_EPC, "LBUF%EPC", stat=ERR)
            call my_alloc(LBUF%XST, NEL*BUFLY%L_XST, "LBUF%XST", stat=ERR)
            call my_alloc(LBUF%SSP, NEL*BUFLY%L_SSP, "LBUF%SSP", stat=ERR)
            call my_alloc(LBUF%Z, NEL*BUFLY%L_Z, "LBUF%Z", stat=ERR)
            call my_alloc(LBUF%VISC, NEL*BUFLY%L_VISC, "LBUF%VISC", stat=ERR)
            call my_alloc(LBUF%SIGL, NEL*BUFLY%L_SIGL, "LBUF%SIGL", stat=ERR)
            call my_alloc(LBUF%SIGV, NEL*BUFLY%L_SIGV, "LBUF%SIGV", stat=ERR)
            call my_alloc(LBUF%SIGA, NEL*BUFLY%L_SIGA, "LBUF%SIGA", stat=ERR)
            call my_alloc(LBUF%SIGB, NEL*BUFLY%L_SIGB, "LBUF%SIGB", stat=ERR)
            call my_alloc(LBUF%SIGC, NEL*BUFLY%L_SIGC, "LBUF%SIGC", stat=ERR)
            call my_alloc(LBUF%SIGD, NEL*BUFLY%L_SIGD, "LBUF%SIGD", stat=ERR)
            call my_alloc(LBUF%SIGF, NEL*BUFLY%L_SIGF, "LBUF%SIGF", stat=ERR)
            call my_alloc(LBUF%SIG, NEL*BUFLY%L_SIG, "LBUF%SIG", stat=ERR)
            call my_alloc(LBUF%SIGPLY, NEL*BUFLY%L_SIGPLY, "LBUF%SIGPLY", stat=ERR)
            call my_alloc(LBUF%FOR, NEL*BUFLY%L_FOR, "LBUF%FOR", stat=ERR)
            call my_alloc(LBUF%MOM, NEL*BUFLY%L_MOM, "LBUF%MOM", stat=ERR)
            call my_alloc(LBUF%THK, NEL*BUFLY%L_THK, "LBUF%THK", stat=ERR)
            call my_alloc(LBUF%SMSTR, NEL*BUFLY%L_SMSTR, "LBUF%SMSTR", stat=ERR)
            call my_alloc(LBUF%DMG, NEL*BUFLY%L_DMG, "LBUF%DMG", stat=ERR)
            call my_alloc(LBUF%FORTH, NEL*BUFLY%L_FORTH, "LBUF%FORTH", stat=ERR)
            call my_alloc(LBUF%EINTTH, NEL*BUFLY%L_EINTTH, "LBUF%EINTTH", stat=ERR)
            call my_alloc(LBUF%SEQ, NEL*BUFLY%L_SEQ, "LBUF%SEQ", stat=ERR)
            call my_alloc(LBUF%FAC_YLD, NEL*BUFLY%L_FAC_YLD, "LBUF%FAC_YLD", stat=ERR)
            call my_alloc(LBUF%BFRAC, NEL*BUFLY%L_ABURN, "LBUF%BFRAC", stat=ERR)
            call my_alloc(LBUF%BFRAC, NEL*BUFLY%L_MU, "LBUF%BFRAC", stat=ERR)
            call my_alloc(LBUF%PLANL, NEL*BUFLY%L_PLANL, "LBUF%PLANL", stat=ERR)
            call my_alloc(LBUF%EPSDNL, NEL*BUFLY%L_EPSDNL, "LBUF%EPSDNL", stat=ERR)
            call my_alloc(LBUF%DMGSCL, NEL*BUFLY%L_DMGSCL, "LBUF%DMGSCL", stat=ERR)
            call my_alloc(LBUF%TSAIWU, NEL*BUFLY%L_TSAIWU, "LBUF%TSAIWU", stat=ERR)
           ENDDO                                                                    
          ENDDO                                                                     
         ENDDO                                                                      
        ENDDO       
C        
c-----    Material buffer - per integration point                                                                  
c
        DO IL = 1, NLAY                                                     
          NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT                              
          NVARTMP = ELBUF_TAB(NG)%BUFLY(IL)%NVARTMP                              
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN
            NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
          ELSE
            NPTT = ELBUF_TAB(NG)%NPTT
          ENDIF
          DO IR = 1,NPTR                                                    
            DO IS = 1,NPTS                                                  
              DO IT = 1,NPTT                                                
                ALLOCATE (ELBUF_IMP(NG)%BUFLY(IL)%MAT(IR,IS,IT)
     .                                 %VAR(NEL*NUVAR), STAT=ERR)
                ALLOCATE (ELBUF_IMP(NG)%BUFLY(IL)%MAT(IR,IS,IT)
     .                                 %VARTMP(NEL*NVARTMP), STAT=ERR)
              ENDDO                                                             
            ENDDO                                                              
          ENDDO                                                             
        ENDDO                                                               
c-------------------------------
C         failure model variables                            
c-------------------------------
        DO IL = 1, NLAY                                                      
          NFAIL = ELBUF_TAB(NG)% BUFLY(IL)%NFAIL                                   
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN
            NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
          ELSE
            NPTT = ELBUF_TAB(NG)%NPTT
          ENDIF
          DO IR = 1,NPTR                                                     
            DO IS = 1,NPTS                                                   
              DO IT = 1,NPTT                                                 
                ALLOCATE (ELBUF_IMP(NG)%BUFLY(IL)%FAIL(IR,IS,IT)                                                  
     .                                 %FLOC(NFAIL),   STAT=ERR)        
                DO K = 1,NFAIL                                               
                 NUVAR=
     .            ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(K)%NVAR 
                 LF_DAM=
     .            ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(K)%LF_DAM 
                 LF_DAMMX=
     .            ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(K)%LF_DAMMX 
                 LF_DAMINI=
     .            ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(K)%LF_DAMINI
                 LF_TDEL=
     .            ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(K)%LF_TDEL 
                 LF_INDX=
     .            ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(K)%LF_INDX
                 LF_OFF=
     .            ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(K)%LF_OFF
                                   
                 FLOC=>ELBUF_IMP(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(K)                   
                 call my_alloc(FLOC%VAR, NEL*NUVAR, "FLOC%VAR", stat=ERR)
                 call my_alloc(FLOC%DAM, NEL*LF_DAM, "FLOC%DAM", stat=ERR)
                 call my_alloc(FLOC%DAMMX, NEL*LF_DAMMX, "FLOC%DAMMX", stat=ERR)
                 call my_alloc(FLOC%DAMINI, NEL*LF_DAMINI, "FLOC%DAMINI", stat=ERR)
                 call my_alloc(FLOC%TDEL, NEL*LF_TDEL, "FLOC%TDEL", stat=ERR)
                 call my_alloc(FLOC%INDX, NEL*LF_INDX, "FLOC%INDX", stat=ERR)
                 call my_alloc(FLOC%OFF, NEL*LF_OFF, "FLOC%OFF", stat=ERR)
                              
                ENDDO                                                         
              ENDDO                                                          
            ENDDO                                                            
          ENDDO                                                              
        ENDDO                                                                
c-------------------------------
C         Viscosity model variables                            
c-------------------------------
        DO IL = 1, NLAY                                                    
          NUVAR = ELBUF_TAB(NG)% BUFLY(IL)%NVAR_VISC                                   
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN
            NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
          ELSE
            NPTT = ELBUF_TAB(NG)%NPTT
          ENDIF
          DO IR = 1,NPTR                                                  
            DO IS = 1,NPTS                                                
              DO IT = 1,NPTT                                              
                ALLOCATE (ELBUF_IMP(NG)%BUFLY(IL)%VISC(IR,IS,IT)%VAR(NEL*NUVAR), STAT=ERR)
              ENDDO                                                           
            ENDDO                                                            
          ENDDO                                                           
        ENDDO     
c-------------------------------
C         Non-local regularization in the shell thickness                 
c-------------------------------
        IF ((ITY==3).OR.(ITY==7)) THEN
          ! Number of integration points in the thickness
          NPTT = ELBUF_TAB(NG)%NPTT
          IF ((INLOC>0).AND.(NPTT>1)) THEN
            ! Allocation of the non-local buffer structure
            ALLOCATE(ELBUF_TAB(NG)%NLOC(NPTR,NPTS), STAT=ERR)
            ! Number of non-local nodes in the thickness
            IF (NPTT>2) THEN
              NONL = NPTT+1
            ELSE
              NONL = NPTT
            ENDIF
            ! Loop over the integration points in the plane of the shell
            DO IR = 1,NPTR
              DO IS = 1,NPTS
                ! Pointing the non-local buffer
                BUFNL => ELBUF_TAB(NG)%NLOC(IR,IS)
                ! Non-local in-thickness masses
                call my_alloc(BUFNL%MASSTH, NEL, NONL, "BUFNL%MASSTH", stat=ERR)
                ! Non-local in-thickness cumulated variables
                call my_alloc(BUFNL%UNLTH, NEL, NONL, "BUFNL%UNLTH", stat=ERR)
                ! Non-local in-thickness velocities
                call my_alloc(BUFNL%VNLTH, NEL, NONL, "BUFNL%VNLTH", stat=ERR)
                ! Non-local in-thickness forces
                call my_alloc(BUFNL%FNLTH, NEL, NONL, "BUFNL%FNLTH", stat=ERR)
              ENDDO
            ENDDO  
          ENDIF
c-------------------------------
C         Non-local regularization in the thickshell thickness                 
c-------------------------------
        ELSEIF (ITY==1) THEN
          ! Number of integration points in the thickness
          NPTT = ELBUF_TAB(NG)%NLAY
          IF ((INLOC>0).AND.(NPTT>1)) THEN
            ! Allocation of the non-local buffer structure
            ALLOCATE(ELBUF_TAB(NG)%NLOCTS(NPTR,NPTS), STAT=ERR)
            ! Number of non-local nodes in the thickness
            NONL = NPTT+1
            ! Loop over the integration points in the plane of the shell
            DO IR = 1,NPTR
              DO IS = 1,NPTS
                ! Pointing the non-local buffer
                BUFNLTS => ELBUF_TAB(NG)%NLOCTS(IR,IS)
                ! Non-local in-thickness masses
                call my_alloc(BUFNLTS%MASSTH, NEL, NONL, "BUFNLTS%MASSTH", stat=ERR)
                ! Non-local in-thickness cumulated variables
                call my_alloc(BUFNLTS%UNLTH, NEL, NONL, "BUFNLTS%UNLTH", stat=ERR)
                ! Non-local in-thickness velocities
                call my_alloc(BUFNLTS%VNLTH, NEL, NONL, "BUFNLTS%VNLTH", stat=ERR)
                ! Non-local in-thickness forces
                call my_alloc(BUFNLTS%FNLTH, NEL, NONL, "BUFNLTS%FNLTH", stat=ERR)
              ENDDO
            ENDDO  
          ENDIF
        ENDIF
        
c-------------------------------
C         EoS buffer
c-------------------------------
      DO IL = 1, NLAY 
        NVAR_EOS = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_EOS 
        NVARTMP_EOS = ELBUF_TAB(NG)%BUFLY(IL)%NVARTMP_EOS
        IF (IGTYP == 51 .OR. IGTYP == 52) THEN
          NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
        ELSE
          NPTT = ELBUF_TAB(NG)%NPTT
        ENDIF
        DO IR = 1,NPTR                                                    
          DO IS = 1,NPTS                                                  
            DO IT = 1,NPTT                                                           
              ALLOCATE( ELBUF_TAB(NG)%BUFLY(IL)%EOS(IR,IS,IT)%VAR(NEL*NVAR_EOS), STAT=ERR)  
              ALLOCATE( ELBUF_TAB(NG)%BUFLY(IL)%EOS(IR,IS,IT)%VARTMP(NEL*NVARTMP_EOS), STAT=ERR)                                                                                                                                                    
            ENDDO                                                             
          ENDDO                                                              
        ENDDO                                                             
      ENDDO       
c-------------------------------------------------
C       allocation of global buffer variables
c-------------------------------------------------
        GBUF  => ELBUF_TAB(NG)%GBUF                                                  
        ELBUF_IMP(NG)%GBUF%NVAR_GLOB = GBUF%NVAR_GLOB
C  
        ELBUF_IMP(NG)%GBUF%G_NOFF   = GBUF%G_NOFF     
        ELBUF_IMP(NG)%GBUF%G_IERR   = GBUF%G_IERR     
        ELBUF_IMP(NG)%GBUF%G_OFF    = GBUF%G_OFF      
        ELBUF_IMP(NG)%GBUF%G_GAMA   = GBUF%G_GAMA     
        ELBUF_IMP(NG)%GBUF%G_SMSTR  = GBUF%G_SMSTR    
        ELBUF_IMP(NG)%GBUF%G_HOURG  = GBUF%G_HOURG    
        ELBUF_IMP(NG)%GBUF%G_BFRAC  = GBUF%G_BFRAC    
        ELBUF_IMP(NG)%GBUF%G_EINT   = GBUF%G_EINT     
        ELBUF_IMP(NG)%GBUF%G_EINS   = GBUF%G_EINS     
        ELBUF_IMP(NG)%GBUF%G_RHO    = GBUF%G_RHO         
        ELBUF_IMP(NG)%GBUF%G_QVIS   = GBUF%G_QVIS     
        ELBUF_IMP(NG)%GBUF%G_DELTAX = GBUF%G_DELTAX   
        ELBUF_IMP(NG)%GBUF%G_VOL    = GBUF%G_VOL      
        ELBUF_IMP(NG)%GBUF%G_EPSD   = GBUF%G_EPSD     
        ELBUF_IMP(NG)%GBUF%G_EPSQ   = GBUF%G_EPSQ
        ELBUF_IMP(NG)%GBUF%G_PLA    = GBUF%G_PLA      
        ELBUF_IMP(NG)%GBUF%G_WPLA   = GBUF%G_WPLA             
        ELBUF_IMP(NG)%GBUF%G_TEMP   = GBUF%G_TEMP     
        ELBUF_IMP(NG)%GBUF%G_TB     = GBUF%G_TB       
        ELBUF_IMP(NG)%GBUF%G_RK     = GBUF%G_RK       
        ELBUF_IMP(NG)%GBUF%G_RE     = GBUF%G_RE       
        ELBUF_IMP(NG)%GBUF%G_SIG    = GBUF%G_SIG      
        ELBUF_IMP(NG)%GBUF%G_FOR    = GBUF%G_FOR      
        ELBUF_IMP(NG)%GBUF%G_MOM    = GBUF%G_MOM      
        ELBUF_IMP(NG)%GBUF%G_THK    = GBUF%G_THK      
        ELBUF_IMP(NG)%GBUF%G_TAG22  = GBUF%G_TAG22     
        ELBUF_IMP(NG)%GBUF%G_STRA   = GBUF%G_STRA    
        ELBUF_IMP(NG)%GBUF%G_SIGI   = GBUF%G_SIGI    
        ELBUF_IMP(NG)%GBUF%G_DMG    = GBUF%G_DMG   
        ELBUF_IMP(NG)%GBUF%G_FORPG  = GBUF%G_FORPG   
        ELBUF_IMP(NG)%GBUF%G_MOMPG  = GBUF%G_MOMPG
C
        ELBUF_IMP(NG)%GBUF%G_FORPGPINCH  = GBUF%G_FORPGPINCH   
        ELBUF_IMP(NG)%GBUF%G_MOMPGPINCH  = GBUF%G_MOMPGPINCH  
        ELBUF_IMP(NG)%GBUF%G_EPGPINCHXZ  = GBUF%G_EPGPINCHXZ 
        ELBUF_IMP(NG)%GBUF%G_EPGPINCHYZ  = GBUF%G_EPGPINCHYZ 
        ELBUF_IMP(NG)%GBUF%G_EPGPINCHZZ  = GBUF%G_EPGPINCHZZ
C    
        ELBUF_IMP(NG)%GBUF%G_STRPG  = GBUF%G_STRPG   
        ELBUF_IMP(NG)%GBUF%G_UELR   = GBUF%G_UELR   
        ELBUF_IMP(NG)%GBUF%G_UELR1  = GBUF%G_UELR1   
        ELBUF_IMP(NG)%GBUF%G_DAMDL  = GBUF%G_DAMDL  
        ELBUF_IMP(NG)%GBUF%G_FORTH  = GBUF%G_FORTH
        ELBUF_IMP(NG)%GBUF%G_EINTTH = GBUF%G_EINTTH
        ELBUF_IMP(NG)%GBUF%G_SEQ    = GBUF%G_SEQ
        ELBUF_IMP(NG)%GBUF%G_STRW   = GBUF%G_STRW    
        ELBUF_IMP(NG)%GBUF%G_STRWPG = GBUF%G_STRWPG 
        ELBUF_IMP(NG)%GBUF%G_ABURN  = GBUF%G_ABURN    
        ELBUF_IMP(NG)%GBUF%G_MU     = GBUF%G_MU    
        ELBUF_IMP(NG)%GBUF%G_PLANL  = GBUF%G_PLANL  
        ELBUF_IMP(NG)%GBUF%G_EPSDNL = GBUF%G_EPSDNL
        ELBUF_IMP(NG)%GBUF%G_DMGSCL = GBUF%G_DMGSCL
        ELBUF_IMP(NG)%GBUF%G_TSAIWU = GBUF%G_TSAIWU
        ELBUF_IMP(NG)%GBUF%G_BPRELD  = GBUF%G_BPRELD    
        ELBUF_IMP(NG)%GBUF%G_COR_NF  = GBUF%G_COR_NF   
        ELBUF_IMP(NG)%GBUF%G_COR_FR = GBUF%G_COR_FR  
        ELBUF_IMP(NG)%GBUF%G_COR_XR = GBUF%G_COR_XR  
        ELBUF_IMP(NG)%GBUF%G_MAXFRAC= GBUF%G_MAXFRAC    
        ELBUF_IMP(NG)%GBUF%G_MAXEPS = GBUF%G_MAXEPS    
        ELBUF_IMP(NG)%GBUF%G_BETAORTh = GBUF%G_BETAORTh    
        ELBUF_IMP(NG)%GBUF%G_AMU    = GBUF%G_AMU
        ELBUF_IMP(NG)%GBUF%G_FOR_G  = GBUF%G_FOR_G
        ELBUF_IMP(NG)%GBUF%G_FORPG_G= GBUF%G_FORPG_G
c---
C 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
c---
        ELBUF_IMP(NG)%GBUF%G_AREA   = GBUF%G_AREA
        ELBUF_IMP(NG)%GBUF%G_SKEW   = GBUF%G_SKEW
        ELBUF_IMP(NG)%GBUF%G_LENGTH   = GBUF%G_LENGTH
        ELBUF_IMP(NG)%GBUF%G_TOTDEPL   = GBUF%G_TOTDEPL
        ELBUF_IMP(NG)%GBUF%G_TOTROT   = GBUF%G_TOTROT
        ELBUF_IMP(NG)%GBUF%G_FOREP   = GBUF%G_FOREP
        ELBUF_IMP(NG)%GBUF%G_MOMEP   = GBUF%G_MOMEP
        ELBUF_IMP(NG)%GBUF%G_DEP_IN_TENS = GBUF%G_DEP_IN_TENS
        ELBUF_IMP(NG)%GBUF%G_DEP_IN_COMP = GBUF%G_DEP_IN_COMP
        ELBUF_IMP(NG)%GBUF%G_ROT_IN_TENS = GBUF%G_ROT_IN_TENS
        ELBUF_IMP(NG)%GBUF%G_ROT_IN_COMP = GBUF%G_ROT_IN_COMP
        ELBUF_IMP(NG)%GBUF%G_POSX   = GBUF%G_POSX
        ELBUF_IMP(NG)%GBUF%G_POSY   = GBUF%G_POSY
        ELBUF_IMP(NG)%GBUF%G_POSZ   = GBUF%G_POSZ
        ELBUF_IMP(NG)%GBUF%G_POSXX   = GBUF%G_POSXX
        ELBUF_IMP(NG)%GBUF%G_POSYY   = GBUF%G_POSYY
        ELBUF_IMP(NG)%GBUF%G_POSZZ   = GBUF%G_POSZZ
        ELBUF_IMP(NG)%GBUF%G_YIELD   = GBUF%G_YIELD
        ELBUF_IMP(NG)%GBUF%G_LENGTH_ERR  = GBUF%G_LENGTH_ERR
        ELBUF_IMP(NG)%GBUF%G_DV    = GBUF%G_DV
        ELBUF_IMP(NG)%GBUF%G_DFS   = GBUF%G_DFS
        ELBUF_IMP(NG)%GBUF%G_SKEW_ERR   = GBUF%G_SKEW_ERR
        ELBUF_IMP(NG)%GBUF%G_E6    = GBUF%G_E6
        ELBUF_IMP(NG)%GBUF%G_RUPTCRIT   = GBUF%G_RUPTCRIT
        ELBUF_IMP(NG)%GBUF%G_MASS   = GBUF%G_MASS
        ELBUF_IMP(NG)%GBUF%G_V_REPCVT   = GBUF%G_V_REPCVT
        ELBUF_IMP(NG)%GBUF%G_VR_REPCVT   = GBUF%G_VR_REPCVT
        ELBUF_IMP(NG)%GBUF%G_NUVAR       = GBUF%G_NUVAR
        ELBUF_IMP(NG)%GBUF%G_NUVARN      = GBUF%G_NUVARN
        ELBUF_IMP(NG)%GBUF%G_DEFINI      = GBUF%G_DEFINI
        ELBUF_IMP(NG)%GBUF%G_FORINI      = GBUF%G_FORINI
        ELBUF_IMP(NG)%GBUF%G_INIFRIC     = GBUF%G_INIFRIC
        ELBUF_IMP(NG)%GBUF%G_STRHG       = GBUF%G_STRHG
        ELBUF_IMP(NG)%GBUF%G_SKEW_ID   = GBUF%G_SKEW_ID
        ELBUF_IMP(NG)%GBUF%G_YIELD_IN_COMP = GBUF%G_YIELD_IN_COMP
        ELBUF_IMP(NG)%GBUF%G_XXOLD_IN_COMP = GBUF%G_XXOLD_IN_COMP
c---
C seatbelt elements
c---
        ELBUF_IMP(NG)%GBUF%G_SLIPRING_ID = GBUF%G_SLIPRING_ID
        ELBUF_IMP(NG)%GBUF%G_SLIPRING_FRAM_ID = GBUF%G_SLIPRING_FRAM_ID
        ELBUF_IMP(NG)%GBUF%G_SLIPRING_STRAND = GBUF%G_SLIPRING_STRAND
        ELBUF_IMP(NG)%GBUF%G_RETRACTOR_ID = GBUF%G_RETRACTOR_ID
        ELBUF_IMP(NG)%GBUF%G_RINGSLIP    = GBUF%G_RINGSLIP
        ELBUF_IMP(NG)%GBUF%G_ADD_NODE    = GBUF%G_ADD_NODE
        ELBUF_IMP(NG)%GBUF%G_UPDATE      = GBUF%G_UPDATE
        ELBUF_IMP(NG)%GBUF%G_FRAM_FACTOR = GBUF%G_FRAM_FACTOR
        ELBUF_IMP(NG)%GBUF%G_INTVAR      = GBUF%G_INTVAR
C       
        ALLOCATE (ELBUF_IMP(NG)%GBUF%NOFF(NEL*GBUF%G_NOFF)       ,STAT=ERR)                         
        ALLOCATE (ELBUF_IMP(NG)%GBUF%IERR(NEL*GBUF%G_IERR)       ,STAT=ERR)                         
        ALLOCATE (ELBUF_IMP(NG)%GBUF%GAMA(NEL*GBUF%G_GAMA)       ,STAT=ERR)                  
        ALLOCATE (ELBUF_IMP(NG)%GBUF%SMSTR(NEL*GBUF%G_SMSTR)     ,STAT=ERR)                  
        ALLOCATE (ELBUF_IMP(NG)%GBUF%HOURG(NEL*GBUF%G_HOURG)     ,STAT=ERR)                  
        ALLOCATE (ELBUF_IMP(NG)%GBUF%TAG22(NEL*GBUF%G_TAG22)     ,STAT=ERR)                  
        ALLOCATE (ELBUF_IMP(NG)%GBUF%STRA(NEL*GBUF%G_STRA)       ,STAT=ERR)                  
        ALLOCATE (ELBUF_IMP(NG)%GBUF%SIGI(NEL*GBUF%G_SIGI)       ,STAT=ERR)                  
        ALLOCATE (ELBUF_IMP(NG)%GBUF%DMG(NEL*GBUF%G_DMG)         ,STAT=ERR)                  
        ALLOCATE (ELBUF_IMP(NG)%GBUF%UELR(NEL*GBUF%G_UELR)       ,STAT=ERR)                  
        ALLOCATE (ELBUF_IMP(NG)%GBUF%UELR1(NEL*GBUF%G_UELR1)     ,STAT=ERR)                  
        ALLOCATE (ELBUF_IMP(NG)%GBUF%DAMDL(NEL*GBUF%G_DAMDL)     ,STAT=ERR)                  
        ALLOCATE (ELBUF_IMP(NG)%GBUF%FOR (NEL*GBUF%G_FOR)        ,STAT=ERR)                       
        ALLOCATE (ELBUF_IMP(NG)%GBUF%MOM (NEL*GBUF%G_MOM)        ,STAT=ERR)                       
        ALLOCATE (ELBUF_IMP(NG)%GBUF%THK (NEL*GBUF%G_THK)        ,STAT=ERR)          
        ALLOCATE (ELBUF_IMP(NG)%GBUF%STRW(NEL*GBUF%G_STRW)       ,STAT=ERR)                 
        ALLOCATE (ELBUF_IMP(NG)%GBUF%BPRELD(NEL*GBUF%G_BPRELD)   ,STAT=ERR)                 
        ALLOCATE (ELBUF_IMP(NG)%GBUF%COR_NF(NEL*GBUF%G_COR_NF)   ,STAT=ERR)                 
        ALLOCATE (ELBUF_IMP(NG)%GBUF%COR_FR(NEL*GBUF%G_COR_FR)   ,STAT=ERR)                 
        ALLOCATE (ELBUF_IMP(NG)%GBUF%COR_XR(NEL*GBUF%G_COR_XR)   ,STAT=ERR)                 
        ALLOCATE (ELBUF_IMP(NG)%GBUF%MAXFRAC(NEL*GBUF%G_MAXFRAC) ,STAT=ERR)   
        ALLOCATE (ELBUF_IMP(NG)%GBUF%MAXEPS(NEL*GBUF%G_MAXEPS)   ,STAT=ERR)      
        ALLOCATE (ELBUF_IMP(NG)%GBUF%BETAORTH(NEL*GBUF%G_BETAORTH) ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%AMU(NEL*GBUF%G_AMU)         ,STAT=ERR)                          
        ALLOCATE (ELBUF_IMP(NG)%GBUF%FOR_G(NEL*GBUF%G_FOR_G)     ,STAT=ERR)  
        ALLOCATE (ELBUF_IMP(NG)%GBUF%WPLA(NEL*GBUF%G_WPLA)       ,STAT=ERR)                                           
c
        IF (NPTTOT /= 1) THEN
          ALLOCATE (ELBUF_IMP(NG)%GBUF%BFRAC(NEL*GBUF%G_BFRAC)   ,STAT=ERR)                  
          ALLOCATE (ELBUF_IMP(NG)%GBUF%OFF (NEL*GBUF%G_OFF)      ,STAT=ERR)                  
          ALLOCATE (ELBUF_IMP(NG)%GBUF%EINT(NEL*GBUF%G_EINT)     ,STAT=ERR)                       
          ALLOCATE (ELBUF_IMP(NG)%GBUF%EINS(NEL*GBUF%G_EINS)     ,STAT=ERR)                       
          ALLOCATE (ELBUF_IMP(NG)%GBUF%RHO (NEL*GBUF%G_RHO)      ,STAT=ERR)                       
          ALLOCATE (ELBUF_IMP(NG)%GBUF%QVIS(NEL*GBUF%G_QVIS)     ,STAT=ERR)                       
          ALLOCATE (ELBUF_IMP(NG)%GBUF%DELTAX(NEL*GBUF%G_DELTAX) ,STAT=ERR)                    
          ALLOCATE (ELBUF_IMP(NG)%GBUF%VOL (NEL*GBUF%G_VOL)      ,STAT=ERR)                       
          ALLOCATE (ELBUF_IMP(NG)%GBUF%EPSD(NEL*GBUF%G_EPSD)     ,STAT=ERR)                       
          ALLOCATE (ELBUF_IMP(NG)%GBUF%EPSQ(NEL*GBUF%G_EPSQ)     ,STAT=ERR) 
          ALLOCATE (ELBUF_IMP(NG)%GBUF%PLA (NEL*GBUF%G_PLA)      ,STAT=ERR)   
          ALLOCATE (ELBUF_IMP(NG)%GBUF%TEMP(NEL*GBUF%G_TEMP)     ,STAT=ERR)                       
          ALLOCATE (ELBUF_IMP(NG)%GBUF%TB(NEL*GBUF%G_TB)         ,STAT=ERR)                           
          ALLOCATE (ELBUF_IMP(NG)%GBUF%RK(NEL*GBUF%G_RK)         ,STAT=ERR)                           
          ALLOCATE (ELBUF_IMP(NG)%GBUF%RE(NEL*GBUF%G_RE)         ,STAT=ERR)                           
          ALLOCATE (ELBUF_IMP(NG)%GBUF%SIG (NEL*GBUF%G_SIG)      ,STAT=ERR)                       
          ALLOCATE (ELBUF_IMP(NG)%GBUF%FORTH (NEL*GBUF%G_FORTH)  ,STAT=ERR)                       
          ALLOCATE (ELBUF_IMP(NG)%GBUF%EINTTH (NEL*GBUF%G_EINTTH),STAT=ERR)
          ALLOCATE (ELBUF_IMP(NG)%GBUF%SEQ (NEL*GBUF%G_SEQ)      ,STAT=ERR)
          ALLOCATE (ELBUF_IMP(NG)%GBUF%ABURN(NEL*GBUF%G_ABURN)   ,STAT=ERR)                  
          ALLOCATE (ELBUF_IMP(NG)%GBUF%MU(NEL*GBUF%G_MU)         ,STAT=ERR)                  
        ENDIF
c
        IF (NPG == 1) THEN
          IF (GBUF%G_STRPG > GBUF%G_STRA) 
     .      ALLOCATE(ELBUF_IMP(NG)%GBUF%STRPG(NEL*GBUF%G_STRPG),STAT=ERR)                  
        ELSEIF (NPG > 1) THEN
          ALLOCATE(ELBUF_IMP(NG)%GBUF%FORPG(NEL*GBUF%G_FORPG),STAT=ERR)                  
          ALLOCATE(ELBUF_IMP(NG)%GBUF%MOMPG(NEL*GBUF%G_MOMPG),STAT=ERR)
C
          ALLOCATE(ELBUF_IMP(NG)%GBUF%FORPGPINCH(NEL*GBUF%G_FORPGPINCH),STAT=ERR)                  
          ALLOCATE(ELBUF_IMP(NG)%GBUF%MOMPGPINCH(NEL*GBUF%G_MOMPGPINCH),STAT=ERR) 
          ALLOCATE(ELBUF_IMP(NG)%GBUF%EPGPINCHXZ(NEL*GBUF%G_EPGPINCHXZ),STAT=ERR)
          ALLOCATE(ELBUF_IMP(NG)%GBUF%EPGPINCHYZ(NEL*GBUF%G_EPGPINCHYZ),STAT=ERR)   
          ALLOCATE(ELBUF_IMP(NG)%GBUF%EPGPINCHZZ(NEL*GBUF%G_EPGPINCHZZ),STAT=ERR)
C                   
          ALLOCATE(ELBUF_IMP(NG)%GBUF%STRPG(NEL*GBUF%G_STRPG),STAT=ERR)                  
          ALLOCATE(ELBUF_IMP(NG)%GBUF%STRWPG(NEL*GBUF%G_STRWPG),STAT=ERR)                 
          ALLOCATE(ELBUF_IMP(NG)%GBUF%FORPG_G(NEL*GBUF%G_FORPG_G),STAT=ERR)                           
        ENDIF
C
c---
C 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
c---
        IF(IGTYP == 3 .and. ITY == 5) THEN 
          ALLOCATE (ELBUF_IMP(NG)%GBUF%DMGSCL(NEL*GBUF%G_DMGSCL)          ,STAT=ERR)
        ENDIF
        ALLOCATE (ELBUF_IMP(NG)%GBUF%AREA(NEL*GBUF%G_AREA)              ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%SKEW(NEL*GBUF%G_SKEW)              ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%LENGTH(NEL*GBUF%G_LENGTH)          ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%TOTDEPL(NEL*GBUF%G_TOTDEPL)        ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%TOTROT(NEL*GBUF%G_TOTROT)          ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%FOREP(NEL*GBUF%G_FOREP)            ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%MOMEP(NEL*GBUF%G_MOMEP)            ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%DEP_IN_TENS(NEL*GBUF%G_DEP_IN_TENS),STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%DEP_IN_COMP(NEL*GBUF%G_DEP_IN_COMP),STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%ROT_IN_TENS(NEL*GBUF%G_ROT_IN_TENS),STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%ROT_IN_COMP(NEL*GBUF%G_ROT_IN_COMP),STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%POSX(NEL*GBUF%G_POSX)              ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%POSY(NEL*GBUF%G_POSY)              ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%POSZ(NEL*GBUF%G_POSZ)              ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%POSXX(NEL*GBUF%G_POSXX)            ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%POSYY(NEL*GBUF%G_POSYY)            ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%POSZZ(NEL*GBUF%G_POSZZ)            ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%YIELD(NEL*GBUF%G_YIELD)            ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%LENGTH_ERR(NEL*GBUF%G_LENGTH_ERR)  ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%DV(NEL*GBUF%G_DV)                  ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%DFS(NEL*GBUF%G_DFS)                ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%SKEW_ERR(NEL*GBUF%G_SKEW_ERR)      ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%E6(NEL*GBUF%G_E6)                  ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%RUPTCRIT(NEL*GBUF%G_RUPTCRIT)      ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%MASS(NEL*GBUF%G_MASS)              ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%V_REPCVT(NEL*GBUF%G_V_REPCVT)      ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%VR_REPCVT(NEL*GBUF%G_VR_REPCVT)    ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%VAR(NEL*GBUF%G_NUVAR)              ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%VARN(NEL*GBUF%G_NUVARN)            ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%DEFINI(NEL*GBUF%G_DEFINI)          ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%FORINI(NEL*GBUF%G_FORINI)          ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%INIFRIC(NEL*GBUF%G_INIFRIC)        ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%STRHG(NEL*GBUF%G_STRHG)            ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%SKEW_ID(NEL*GBUF%G_SKEW_ID)        ,STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%YIELD_IN_COMP(NEL*GBUF%G_YIELD_IN_COMP),STAT=ERR)
        ALLOCATE (ELBUF_IMP(NG)%GBUF%XXOLD_IN_COMP(NEL*GBUF%G_XXOLD_IN_COMP),STAT=ERR)
c---
C seatbelt elements
c---
        ALLOCATE (ELBUF_IMP(NG)%GBUF%SLIPRING_ID(NEL*GBUF%G_SLIPRING_ID))
        ALLOCATE (ELBUF_IMP(NG)%GBUF%SLIPRING_FRAM_ID(NEL*GBUF%G_SLIPRING_FRAM_ID))
        ALLOCATE (ELBUF_IMP(NG)%GBUF%SLIPRING_STRAND(NEL*GBUF%G_SLIPRING_STRAND)) 
        ALLOCATE (ELBUF_IMP(NG)%GBUF%RETRACTOR_ID(NEL*GBUF%G_RETRACTOR_ID))
        ALLOCATE (ELBUF_IMP(NG)%GBUF%RINGSLIP(NEL*GBUF%G_RINGSLIP))
        ALLOCATE (ELBUF_IMP(NG)%GBUF%ADD_NODE(NEL*GBUF%G_ADD_NODE))
        ALLOCATE (ELBUF_IMP(NG)%GBUF%UPDATE(NEL*GBUF%G_UPDATE))
        ALLOCATE (ELBUF_IMP(NG)%GBUF%FRAM_FACTOR(NEL*GBUF%G_FRAM_FACTOR))
        ALLOCATE (ELBUF_IMP(NG)%GBUF%INTVAR(NEL*GBUF%G_INTVAR))

C-----------
       ENDIF
      ENDDO   ! NG = 1,NGROUP
C-----------
      RETURN
      END
