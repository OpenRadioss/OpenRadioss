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
Chd|  COPY_ELBUF_1                  source/elements/elbuf/copy_elbuf_1.F
Chd|-- called by -----------
Chd|        COPY_ELBUF                    source/elements/elbuf/copy_elbuf.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE COPY_ELBUF_1(ELBUF_SRC,ELBUF_TGT,IPARG,NGROUP,NG)
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
#include      "tabsiz_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGROUP,NG
      INTEGER IPARG(NPARG,NGROUP)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_SRC,ELBUF_TGT
      TARGET :: ELBUF_SRC,ELBUF_TGT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IL,IR,IS,IT,ITY,K,NUVAR,NVARTMP,NFAIL,NLAY,NPTR,NPTS,NPTT,NPT,
     .   NPG,NEL,IGTYP,LEN1,LEN2,NPTTOT,I,NONL,INLOC
      INTEGER ! global variables 
     .   G_GAMA,G_SIG,G_OFF,G_NOFF,G_EINT,G_EINS,G_TEMP,
     .   G_RHO,G_PLA,G_VOL,G_EPSD,G_QVIS,G_DELTAX,G_TB,G_RK,G_RE,
     .   G_SMSTR,G_HOURG,G_BFRAC,G_THK,G_FOR,G_MOM,G_TAG22,G_STRA,G_SIGI,
     .   G_DMG,G_FORPG,G_MOMPG,G_STRPG,G_EPSQ,G_UELR,G_UELR1,G_IERR,
     .   G_DAMDL,G_FORTH,G_EINTTH,G_SEQ,G_STRW,G_STRWPG,
     .   G_AREA,G_SKEW,G_LENGTH,G_TOTDEPL,G_TOTROT,G_FOREP,G_MOMEP,
     .   G_DEP_IN_TENS,G_DEP_IN_COMP,G_ROT_IN_TENS,G_ROT_IN_COMP,
     .   G_POSX,G_POSY,G_POSZ,G_POSXX,G_POSYY,G_POSZZ,G_YIELD,G_LENGTH_ERR,
     .   G_DV,G_DFS,G_SKEW_ERR,G_E6,G_RUPTCRIT,G_MASS,G_V_REPCVT,
     .   G_VR_REPCVT,G_NUVAR,G_NUVARN,G_INIFRIC,G_DT,G_ISMS,G_STRHG,G_ABURN,G_MU,
     .   G_PLANL,G_EPSDNL,G_BPRELD,G_COR_NF,G_COR_FR,G_COR_XR,G_DEFINI,G_FORINI,
     .   G_FORPGPINCH,G_MOMPGPINCH,G_EPGPINCHXZ,G_EPGPINCHYZ,G_EPGPINCHZZ,
     .   G_SKEW_ID,G_MAXFRAC,G_MAXEPS,G_SLIPRING_ID,G_SLIPRING_STRAND,G_RETRACTOR_ID,
     .   G_RINGSLIP,G_ADD_NODE,G_UPDATE,G_INTVAR,G_BETAORTH,G_AMU,G_SLIPRING_FRAM_ID,
     .   G_FRAM_FACTOR,G_TSAIWU
      INTEGER  ! layer variables
     .   LY_DMG,LY_GAMA,LY_DIRA,LY_DIRB,LY_CRKDIR,LY_PLAPT,LY_SIGPT,LY_HOURG,
     .   LY_UELR,LY_UELR1,LY_OFFPG,LY_OFF,LEN_PLAPT,LEN_SIGPT
      INTEGER  ! local variables
     .   L_OFF,L_EINT,L_EINS,L_RHO,L_DP_DRHO,L_VOL,L_PLA,L_TB,L_TEMP,
     .   L_RK,L_RE,L_VK,L_ROB,L_SIG,L_SIGV,L_SIGL,L_SIGA,L_SIGD,L_SIGB,
     .   L_SIGC,L_SIGF,L_STRA,L_EPSA,L_EPSD,L_EPSQ,L_EPSF,L_CRAK,
     .   L_ANG,L_SF,L_QVIS,L_DELTAX,L_GAMA,
     .   L_DAM,L_DSUM,L_DGLO,L_EPE,L_EPC,L_XST,L_SSP,L_Z,L_FRAC,L_VISC,
     .   L_THK,L_FOR,L_MOM,L_SMSTR,L_BFRAC,L_DMG,LF_DAM,LF_DAMMX,LF_TDEL,
     .   LF_INDX,LF_OFF,L_FORTH,L_EINTTH,L_SEQ,L_SIGPLY,L_FAC_YLD,L_ABURN,
     .   L_MU,L_PLANL,L_EPSDNL,L_DMGSCL,L_TSAIWU,LF_DAMINI
      TYPE(BUF_LAY_)  , POINTER :: BUFLY
      TYPE(L_BUFEL_)  , POINTER :: LBUF
      TYPE(G_BUFEL_)  , POINTER :: GBUFS,GBUFT
      TYPE(L_BUFEL_)  , POINTER :: LBUFS,LBUFT
      TYPE(FAIL_LOC_) , POINTER :: FLOCS,FLOCT
      TYPE(BUF_VISC_) , POINTER :: VISCS,VISCT
      TYPE(BUF_NLOC_) , POINTER :: BUFNLS,BUFNLT
c-------------------------------------------------
c      copy the element buffer : ELBUF_SRC -> ELBUF_TGT
c      target must be allocated
C=======================================================================                                                
        ITY = IPARG(5,NG)
        INLOC = IPARG(78,NG)
        IF (ITY == 1 .OR. ITY == 2 .OR. ITY == 51 .OR.
     .      ITY == 3 .OR. ITY == 7 .OR.
     .      ITY == 4 .OR. ITY == 5 .OR. ITY == 6 .OR. ITY == 100) THEN
c-------------------------------------------------
          IGTYP  = ELBUF_SRC(NG)%IGTYP
          NEL    = ELBUF_SRC(NG)%NEL  
          NLAY   = ELBUF_SRC(NG)%NLAY 
          NPTR   = ELBUF_SRC(NG)%NPTR 
          NPTS   = ELBUF_SRC(NG)%NPTS 
          NPTT   = ELBUF_SRC(NG)%NPTT
          NPT    = NLAY*NPTR*NPTS*NPTT
          IF (ITY == 1 .OR. ITY == 2 .OR. ITY == 51) THEN
            NPG = NPTR*NPTS*NPTT
            NPTTOT = NPT
          ELSEIF (ITY == 3 .OR. ITY == 7) THEN
            NPG = NPTR*NPTS
            NPTTOT  = 0
            DO IL=1,NLAY
              NPTTOT = NPTTOT + NPG*ELBUF_SRC(NG)%BUFLY(IL)%NPTT
            ENDDO
            IF (NPT == 0) NPTTOT = NPT
          ELSEIF (ITY == 4 .OR. ITY == 5 .OR. ITY == 6 .OR. ITY == 100) THEN
            NPG = 0
            NPTTOT = 0
          ENDIF

          ELBUF_TGT(NG)%IGTYP  = IGTYP
          ELBUF_TGT(NG)%NEL    = NEL  
          ELBUF_TGT(NG)%NLAY   = NLAY 
          ELBUF_TGT(NG)%NPTR   = NPTR 
          ELBUF_TGT(NG)%NPTS   = NPTS 
          ELBUF_TGT(NG)%NPTT   = NPTT 
c
          DO IL = 1,NLAY                                         
            ELBUF_TGT(NG)%BUFLY(IL)%ILAW  =ELBUF_SRC(NG)%BUFLY(IL)%ILAW                          
            ELBUF_TGT(NG)%BUFLY(IL)%IMAT  =ELBUF_SRC(NG)%BUFLY(IL)%IMAT                          
            ELBUF_TGT(NG)%BUFLY(IL)%IEOS  =ELBUF_SRC(NG)%BUFLY(IL)%IEOS                           
            ELBUF_TGT(NG)%BUFLY(IL)%IVISC =ELBUF_SRC(NG)%BUFLY(IL)%IVISC                          
            ELBUF_TGT(NG)%BUFLY(IL)%IPORO =ELBUF_SRC(NG)%BUFLY(IL)%IPORO                         
            ELBUF_TGT(NG)%BUFLY(IL)%NFAIL =ELBUF_SRC(NG)%BUFLY(IL)%NFAIL                         
            ELBUF_TGT(NG)%BUFLY(IL)%NVAR_MAT =
     .                                  ELBUF_SRC(NG)%BUFLY(IL)%NVAR_MAT                           
            ELBUF_TGT(NG)%BUFLY(IL)%NVARTMP =
     .                                  ELBUF_SRC(NG)%BUFLY(IL)%NVARTMP                           
            ELBUF_TGT(NG)%BUFLY(IL)%NVAR_VISC=
     .                                 ELBUF_SRC(NG)%BUFLY(IL)%NVAR_VISC                        
            ELBUF_TGT(NG)%BUFLY(IL)%NVAR_LOC=
     .                                  ELBUF_SRC(NG)%BUFLY(IL)%NVAR_LOC                        
            ELBUF_TGT(NG)%BUFLY(IL)%NVAR_LAY=
     .                                  ELBUF_SRC(NG)%BUFLY(IL)%NVAR_LAY                       
            ELBUF_TGT(NG)%BUFLY(IL)%NPTT=
     .                                  ELBUF_SRC(NG)%BUFLY(IL)%NPTT
          ENDDO                                                            
c
          DO IL = 1, NLAY                                                         
           BUFLY => ELBUF_SRC(NG)%BUFLY(IL)
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
           L_FAC_YLD = BUFLY%L_FAC_YLD
           L_ABURN  = BUFLY%L_ABURN
           L_MU     = BUFLY%L_MU
           L_PLANL  = BUFLY%L_PLANL
           L_EPSDNL = BUFLY%L_EPSDNL
           L_DMGSCL = BUFLY%L_DMGSCL
           L_TSAIWU = BUFLY%L_TSAIWU

c
           ELBUF_TGT(NG)%BUFLY(IL)%DMG(1:NEL*LY_DMG) =
     .                       BUFLY%DMG(1:NEL*LY_DMG)               
           ELBUF_TGT(NG)%BUFLY(IL)%GAMA(1:NEL*LY_GAMA) =
     .                       BUFLY%GAMA(1:NEL*LY_GAMA)               
           ELBUF_TGT(NG)%BUFLY(IL)%DIRA(1:NEL*LY_DIRA) =
     .                       BUFLY%DIRA(1:NEL*LY_DIRA)               
           ELBUF_TGT(NG)%BUFLY(IL)%DIRB(1:NEL*LY_DIRB) =
     .                       BUFLY%DIRB(1:NEL*LY_DIRB) 
           ELBUF_TGT(NG)%BUFLY(IL)%CRKDIR(1:NEL*LY_CRKDIR) =
     .                       BUFLY%CRKDIR(1:NEL*LY_CRKDIR) 
           ELBUF_TGT(NG)%BUFLY(IL)%HOURG(1:NEL*LY_HOURG) =
     .                       BUFLY%HOURG(1:NEL*LY_HOURG)
           ELBUF_TGT(NG)%BUFLY(IL)%UELR(1:NEL*LY_UELR) =
     .                       BUFLY%UELR(1:NEL*LY_UELR)
           ELBUF_TGT(NG)%BUFLY(IL)%UELR1(1:NEL*LY_UELR1) =
     .                       BUFLY%UELR1(1:NEL*LY_UELR1)
           ELBUF_TGT(NG)%BUFLY(IL)%OFFPG(1:NEL*LY_OFFPG) =
     .                       BUFLY%OFFPG(1:NEL*LY_OFFPG)
           ELBUF_TGT(NG)%BUFLY(IL)%OFF(1:NEL*LY_OFF) =
     .                       BUFLY%OFF(1:NEL*LY_OFF)
c
           IF (IGTYP == 51 .OR. IGTYP == 52) THEN
             NPTT = ELBUF_SRC(NG)%BUFLY(IL)%NPTT
           ELSE
             NPTT = ELBUF_SRC(NG)%NPTT
           ENDIF
c
           DO IR = 1,NPTR                                                         
            DO IS = 1,NPTS                                                        
             DO IT = 1,NPTT                                                       
               LBUFS => ELBUF_SRC(NG)%BUFLY(IL)%LBUF(IR,IS,IT)                         
               LBUFT => ELBUF_TGT(NG)%BUFLY(IL)%LBUF(IR,IS,IT)                         
c
               LBUFT%OFF(1:NEL*L_OFF)        =LBUFS%OFF(1:NEL*L_OFF)              
               LBUFT%GAMA(1:NEL*L_GAMA)      =LBUFS%GAMA(1:NEL*L_GAMA)            
               LBUFT%STRA(1:NEL*L_STRA)      =LBUFS%STRA(1:NEL*L_STRA)
               LBUFT%FRAC(1:NEL*L_FRAC)      =LBUFS%FRAC(1:NEL*L_FRAC)
               LBUFT%BFRAC(1:NEL*L_BFRAC)    =LBUFS%BFRAC(1:NEL*L_BFRAC)
               LBUFT%EINT(1:NEL*L_EINT)      =LBUFS%EINT(1:NEL*L_EINT)            
               LBUFT%EINS(1:NEL*L_EINS)      =LBUFS%EINS(1:NEL*L_EINS)            
               LBUFT%RHO(1:NEL*L_RHO)        =LBUFS%RHO(1:NEL*L_RHO)              
               LBUFT%DP_DRHO(1:NEL*L_DP_DRHO)=LBUFS%DP_DRHO(1:NEL*L_DP_DRHO)
               LBUFT%QVIS(1:NEL*L_QVIS)      =LBUFS%QVIS(1:NEL*L_QVIS)           
               LBUFT%DELTAX(1:NEL*L_DELTAX)  =LBUFS%DELTAX(1:NEL*L_DELTAX)      
               LBUFT%VOL(1:NEL*L_VOL)        =LBUFS%VOL(1:NEL*L_VOL)  
               LBUFT%EPSA(1:NEL*L_EPSA)      =LBUFS%EPSA(1:NEL*L_EPSA)
               LBUFT%EPSD(1:NEL*L_EPSD)      =LBUFS%EPSD(1:NEL*L_EPSD)
               LBUFT%EPSQ(1:NEL*L_EPSQ)      =LBUFS%EPSQ(1:NEL*L_EPSQ)
               LBUFT%EPSF(1:NEL*L_EPSF)      =LBUFS%EPSF(1:NEL*L_EPSF)
               LBUFT%PLA(1:NEL*L_PLA)        =LBUFS%PLA(1:NEL*L_PLA)  
               LBUFT%TEMP(1:NEL*L_TEMP)      =LBUFS%TEMP(1:NEL*L_TEMP)
               LBUFT%TB(1:NEL*L_TB)          =LBUFS%TB(1:NEL*L_TB)    
               LBUFT%RK(1:NEL*L_RK)          =LBUFS%RK(1:NEL*L_RK)    
               LBUFT%RE(1:NEL*L_RE)          =LBUFS%RE(1:NEL*L_RE)    
               LBUFT%VK(1:NEL*L_VK)          =LBUFS%VK(1:NEL*L_VK)    
               LBUFT%SF(1:NEL*L_SF)          =LBUFS%SF(1:NEL*L_SF)    
               LBUFT%ROB(1:NEL*L_ROB)        =LBUFS%ROB(1:NEL*L_ROB)  
               LBUFT%DAM(1:NEL*L_DAM)        =LBUFS%DAM(1:NEL*L_DAM)  
               LBUFT%DSUM(1:NEL*L_DSUM)      =LBUFS%DSUM(1:NEL*L_DSUM)
               LBUFT%DGLO(1:NEL*L_DGLO)      =LBUFS%DGLO(1:NEL*L_DGLO)
               LBUFT%CRAK(1:NEL*L_CRAK)      =LBUFS%CRAK(1:NEL*L_CRAK)
               LBUFT%ANG(1:NEL*L_ANG)        =LBUFS%ANG(1:NEL*L_ANG)  
               LBUFT%EPE(1:NEL*L_EPE)        =LBUFS%EPE(1:NEL*L_EPE)  
               LBUFT%EPC(1:NEL*L_EPC)        =LBUFS%EPC(1:NEL*L_EPC)  
               LBUFT%XST(1:NEL*L_XST)        =LBUFS%XST(1:NEL*L_XST)  
               LBUFT%SSP(1:NEL*L_SSP)        =LBUFS%SSP(1:NEL*L_SSP)  
               LBUFT%Z(1:NEL*L_Z)            =LBUFS%Z(1:NEL*L_Z)      
               LBUFT%VISC(1:NEL*L_VISC)      =LBUFS%VISC(1:NEL*L_VISC)
               LBUFT%SIGL(1:NEL*L_SIGL)      =LBUFS%SIGL(1:NEL*L_SIGL)
               LBUFT%SIGV(1:NEL*L_SIGV)      =LBUFS%SIGV(1:NEL*L_SIGV)
               LBUFT%SIGA(1:NEL*L_SIGA)      =LBUFS%SIGA(1:NEL*L_SIGA)
               LBUFT%SIGB(1:NEL*L_SIGB)      =LBUFS%SIGB(1:NEL*L_SIGB)
               LBUFT%SIGC(1:NEL*L_SIGC)      =LBUFS%SIGC(1:NEL*L_SIGC)
               LBUFT%SIGD(1:NEL*L_SIGD)      =LBUFS%SIGD(1:NEL*L_SIGD)
               LBUFT%SIGF(1:NEL*L_SIGF)      =LBUFS%SIGF(1:NEL*L_SIGF)
               LBUFT%SIG(1:NEL*L_SIG)        =LBUFS%SIG(1:NEL*L_SIG)              
               LBUFT%SIGPLY(1:NEL*L_SIGPLY)  =LBUFS%SIGPLY(1:NEL*L_SIGPLY)              
               LBUFT%FOR(1:NEL*L_FOR)        =LBUFS%FOR(1:NEL*L_FOR)
               LBUFT%MOM(1:NEL*L_MOM)        =LBUFS%MOM(1:NEL*L_MOM)
               LBUFT%THK(1:NEL*L_THK)        =LBUFS%THK(1:NEL*L_THK)
               LBUFT%SMSTR(1:NEL*L_SMSTR)    =LBUFS%SMSTR(1:NEL*L_SMSTR)
               LBUFT%DMG(1:NEL*L_DMG)        =LBUFS%DMG(1:NEL*L_DMG)
               LBUFT%FORTH(1:NEL*L_FORTH)    =LBUFS%FORTH(1:NEL*L_FORTH)
               LBUFT%EINTTH(1:NEL*L_EINTTH)  =LBUFS%EINTTH(1:NEL*L_EINTTH)
               LBUFT%SEQ(1:NEL*L_SEQ)        =LBUFS%SEQ(1:NEL*L_SEQ)
               LBUFT%FAC_YLD(1:NEL*L_FAC_YLD)=LBUFS%FAC_YLD(1:NEL*L_FAC_YLD)
               LBUFT%ABURN(1:NEL*L_ABURN)    =LBUFS%ABURN(1:NEL*L_ABURN)
               LBUFT%MU(1:NEL*L_MU)          =LBUFS%MU(1:NEL*L_MU)
               LBUFT%PLANL(1:NEL*L_PLANL)    =LBUFS%PLANL(1:NEL*L_PLANL)
               LBUFT%EPSDNL(1:NEL*L_EPSDNL)  =LBUFS%EPSDNL(1:NEL*L_EPSDNL)
               LBUFT%DMGSCL(1:NEL*L_DMGSCL)  =LBUFS%DMGSCL(1:NEL*L_DMGSCL)
               LBUFT%TSAIWU(1:NEL*L_TSAIWU)  =LBUFS%TSAIWU(1:NEL*L_TSAIWU)
             ENDDO                     
            ENDDO                                                                 
           ENDDO                                                                  
          ENDDO  ! IL = 1, NLAY                                                                
c------
          IF (NPG > 1) THEN                                       
            DO IL = 1, NLAY                                                 
              BUFLY => ELBUF_SRC(NG)%BUFLY(IL)
              IF (NLAY > 1) THEN
                LEN_PLAPT = NEL*LY_PLAPT
                LEN_SIGPT = NEL*LY_SIGPT
              ELSE
                IF (IGTYP /= 51 .AND. IGTYP /= 52 ) THEN
                  LEN_PLAPT = NEL*LY_PLAPT*NPT
                  LEN_SIGPT = NEL*LY_SIGPT*NPT
                ELSE
                  LEN_PLAPT = NEL*LY_PLAPT*BUFLY%NPTT
                  LEN_SIGPT = NEL*LY_SIGPT*BUFLY%NPTT
                ENDIF
              ENDIF
              ELBUF_TGT(NG)%BUFLY(IL)%PLAPT(1:LEN_PLAPT) =       
     .                      BUFLY%PLAPT(1:LEN_PLAPT)             
              ELBUF_TGT(NG)%BUFLY(IL)%SIGPT(1:LEN_SIGPT) =       
     .                      BUFLY%SIGPT(1:LEN_SIGPT)             
            ENDDO  ! IL = 1, NLAY                                                                
          ELSE                                                    
            IF (NLAY > 1) THEN                                    
              DO IL = 1, NLAY                                                
                IF (IGTYP == 51 .OR. IGTYP == 52) THEN
                  NPTT = ELBUF_SRC(NG)%BUFLY(IL)%NPTT
                  DO IT=1,NPTT
                    LBUF => ELBUF_SRC(NG)%BUFLY(IL)%LBUF(1,1,IT)
                    ELBUF_TGT(NG)%BUFLY(IL)%PLAPT(1:NEL*LY_PLAPT)            
     .               => LBUF%PLA(1:NEL*L_PLA)               
                    ELBUF_TGT(NG)%BUFLY(IL)%SIGPT(1:NEL*LY_SIGPT)            
     .               => LBUF%SIG(1:NEL*L_SIG)
                  ENDDO
                ELSE
                  LBUF => ELBUF_SRC(NG)%BUFLY(IL)%LBUF(1,1,1)
                  ELBUF_TGT(NG)%BUFLY(IL)%PLAPT(1:NEL*LY_PLAPT)            
     .             => LBUF%PLA(1:NEL*L_PLA)               
                  ELBUF_TGT(NG)%BUFLY(IL)%SIGPT(1:NEL*LY_SIGPT)            
     .             => LBUF%SIG(1:NEL*L_SIG)               
                ENDIF
              ENDDO  ! IL = 1, NLAY                                                              
            ELSE  ! NLAY = 1, NPG = 1                                                
              BUFLY => ELBUF_SRC(NG)%BUFLY(1)
              IF (IGTYP == 51 .OR. IGTYP == 52) THEN
                NPTT = BUFLY%NPTT
                DO IT = 1,NPTT
                  LEN1 = 1+(IT-1)*NEL*LY_PLAPT
                  LEN2 = IT*NEL*LY_PLAPT
                  ELBUF_TGT(NG)%BUFLY(1)%PLAPT(LEN1:LEN2)
     .             => BUFLY%LBUF(1,1,IT)%PLA(1:NEL*L_PLA)
                  LEN1 = 1+(IT-1)*NEL*LY_SIGPT
                  LEN2 = IT*NEL*LY_SIGPT
                ELBUF_TGT(NG)%BUFLY(1)%SIGPT(LEN1:LEN2)
     .            => BUFLY%LBUF(1,1,IT)%SIG(1:NEL*L_SIG)
                ENDDO
              ELSE
                DO IT = 1,NPT
                  LEN1 = 1+(IT-1)*NEL*LY_PLAPT
                  LEN2 = IT*NEL*LY_PLAPT
                  ELBUF_TGT(NG)%BUFLY(1)%PLAPT(LEN1:LEN2)
     .             => BUFLY%LBUF(1,1,IT)%PLA(1:NEL*L_PLA)
                  LEN1 = 1+(IT-1)*NEL*LY_SIGPT
                  LEN2 = IT*NEL*LY_SIGPT
                ELBUF_TGT(NG)%BUFLY(1)%SIGPT(LEN1:LEN2)
     .            => BUFLY%LBUF(1,1,IT)%SIG(1:NEL*L_SIG)
                ENDDO
              ENDIF  ! IF (IGTYP == 51) THEN
cc              DO IT = 1,NPT
cc                ELBUF_TGT(NG)%BUFLY(1)%PLAPT(1:NEL*LY_PLAPT)            
cc     .           => BUFLY%LBUF(1,1,IT)%PLA(1:NEL*L_PLA)               
cc                ELBUF_TGT(NG)%BUFLY(1)%SIGPT(1:NEL*LY_SIGPT)            
cc     .           => BUFLY%LBUF(1,1,IT)%SIG(1:NEL*L_SIG)               
cc              ENDDO  ! IT = 1,NPT                                                                                              
            ENDIF
C
C           SIGPLY (PLYXFEM) not available w/implicit                                                 
          ENDIF                                                   
c
c-----    Material buffer - per integration point                                                                  
c
          DO IL = 1, NLAY                                                 
            NUVAR = ELBUF_SRC(NG)%BUFLY(IL)%NVAR_MAT                          
            NVARTMP = ELBUF_SRC(NG)%BUFLY(IL)%NVARTMP                          
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPTT = ELBUF_SRC(NG)%BUFLY(IL)%NPTT
            ELSE
              NPTT = ELBUF_SRC(NG)%NPTT
            ENDIF
            DO IR = 1,NPTR                                                
              DO IS = 1,NPTS                                              
                DO IT = 1,NPTT                                            
                 ELBUF_TGT(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR(1:NEL*NUVAR)=
     .           ELBUF_SRC(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR(1:NEL*NUVAR)             
                 ELBUF_TGT(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VARTMP(1:NEL*NVARTMP)=
     .           ELBUF_SRC(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VARTMP(1:NEL*NVARTMP)             
                ENDDO                                                         
              ENDDO                                                          
            ENDDO                                                         
          ENDDO                                                           
c-------------------------------
c         failure model variables                            
c-------------------------------
          DO IL = 1, NLAY                                                   
            NFAIL = ELBUF_SRC(NG)%BUFLY(IL)%NFAIL                                 
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPTT = ELBUF_SRC(NG)%BUFLY(IL)%NPTT
            ELSE
              NPTT = ELBUF_SRC(NG)%NPTT
            ENDIF
            DO IR = 1,NPTR                                                  
              DO IS = 1,NPTS                                                
                DO IT = 1,NPTT                                              
                  DO K = 1,NFAIL                                            
                   FLOCS=>ELBUF_SRC(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(K)      
                   FLOCT=>ELBUF_TGT(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(K)
                   NUVAR = FLOCS%NVAR        
                   FLOCT%NVAR = NUVAR        
                   FLOCT%VAR(1:NEL*NUVAR) = FLOCS%VAR(1:NEL*NUVAR) 
c
                   LF_DAM = FLOCS%LF_DAM 
                   FLOCT%LF_DAM = LF_DAM 
                   FLOCT%DAM(1:NEL*LF_DAM) = FLOCS%DAM(1:NEL*LF_DAM) 
c
                   LF_DAMMX = FLOCS%LF_DAMMX 
                   FLOCT%LF_DAMMX = LF_DAMMX   
                   FLOCT%DAMMX(1:NEL*LF_DAMMX) = FLOCS%DAMMX(1:NEL*LF_DAMMX) 
c
                   LF_DAMINI = FLOCS%LF_DAMINI 
                   FLOCT%LF_DAMINI = LF_DAMINI
                   FLOCT%DAMINI(1:NEL*LF_DAMINI) = FLOCS%DAMINI(1:NEL*LF_DAMINI) 
c
                   LF_TDEL = FLOCS%LF_TDEL 
                   FLOCT%LF_TDEL = LF_TDEL  
                   FLOCT%TDEL(1:NEL*LF_TDEL) = FLOCS%TDEL(1:NEL*LF_TDEL)
c
                   LF_INDX = FLOCS%LF_INDX 
                   FLOCT%LF_INDX = LF_INDX
                   FLOCT%INDX(1:NEL*LF_INDX) = FLOCS%INDX(1:NEL*LF_INDX)
c
                   LF_OFF = FLOCS%LF_OFF 
                   FLOCT%LF_OFF = LF_OFF
                   FLOCT%OFF(1:NEL*LF_OFF) = FLOCS%OFF(1:NEL*LF_OFF)
              
                  ENDDO                                                     
                ENDDO                                                       
              ENDDO                                                         
            ENDDO                                                           
          ENDDO                                                             
c-------------------------------
c         Viscosity model variables                            
c-------------------------------
          DO IL = 1, NLAY                                                  
            NUVAR = ELBUF_SRC(NG)%BUFLY(IL)%NVAR_VISC                                 
            IF (IGTYP == 51 .OR. IGTYP == 52) THEN
              NPTT = ELBUF_SRC(NG)%BUFLY(IL)%NPTT
            ELSE
              NPTT = ELBUF_SRC(NG)%NPTT
            ENDIF
            DO IR = 1,NPTR                                                
              DO IS = 1,NPTS                                              
                DO IT = 1,NPTT                                            
                  VISCS => ELBUF_SRC(NG)%BUFLY(IL)%VISC(IR,IS,IT)             
                  VISCT => ELBUF_TGT(NG)%BUFLY(IL)%VISC(IR,IS,IT)             
                  VISCT%VAR(1:NEL*NUVAR) = VISCS%VAR(1:NEL*NUVAR)                     
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
            NPTT = ELBUF_SRC(NG)%NPTT
            IF ((INLOC>0).AND.(NPTT>1)) THEN
              ! Number of non-local nodes in the thickness
              IF (NPTT>2) THEN
                NONL = NPTT+1
              ELSE
                NONL = NPTT
              ENDIF
              ! Loop over integration points in the plane of the shell
              DO IR = 1,NPTR
                DO IS = 1,NPTS
                  ! Pointing the non-local buffer of the source
                  BUFNLS => ELBUF_SRC(NG)%NLOC(IR,IS)
                  ! Pointing the non-local buffer of the target
                  BUFNLT => ELBUF_TGT(NG)%NLOC(IR,IS)
                  ! Copying the buffer
                  DO I = 1,NEL
                    ! Nodal masses
                    BUFNLT%MASSTH(I,1:NONL) = BUFNLS%MASSTH(I,1:NONL) 
                    ! Non-local in-thickness cumulated variables
                    BUFNLT%UNLTH(I,1:NONL)  = BUFNLS%UNLTH(I,1:NONL) 
                    ! Non-local in-thickness velocities
                    BUFNLT%VNLTH(I,1:NONL)  = BUFNLS%VNLTH(I,1:NONL) 
                    ! Non-local in-thickness forces
                    BUFNLT%FNLTH(I,1:NONL)  = BUFNLS%FNLTH(I,1:NONL) 
                  ENDDO
                ENDDO
              ENDDO
            ENDIF
          ENDIF
c-------------------------------------------------
c         Global buffer variables
c-------------------------------------------------
          GBUFS => ELBUF_SRC(NG)%GBUF                              
          GBUFT => ELBUF_TGT(NG)%GBUF                              
c
          GBUFT%NVAR_GLOB  = GBUFS%NVAR_GLOB                       
c
          G_NOFF  = GBUFS%G_NOFF      
          G_IERR  = GBUFS%G_IERR      
          G_OFF   = GBUFS%G_OFF       
          G_GAMA  = GBUFS%G_GAMA      
          G_SMSTR = GBUFS%G_SMSTR     
          G_HOURG = GBUFS%G_HOURG     
          G_BFRAC = GBUFS%G_BFRAC     
          G_EINT  = GBUFS%G_EINT      
          G_EINS  = GBUFS%G_EINS      
          G_RHO   = GBUFS%G_RHO       
          G_QVIS  = GBUFS%G_QVIS      
          G_DELTAX= GBUFS%G_DELTAX    
          G_VOL   = GBUFS%G_VOL       
          G_EPSD  = GBUFS%G_EPSD      
          G_EPSQ  = GBUFS%G_EPSQ
          G_PLA   = GBUFS%G_PLA       
          G_TEMP  = GBUFS%G_TEMP      
          G_TB    = GBUFS%G_TB        
          G_RK    = GBUFS%G_RK        
          G_RE    = GBUFS%G_RE        
          G_SIG   = GBUFS%G_SIG       
          G_FOR   = GBUFS%G_FOR       
          G_MOM   = GBUFS%G_MOM       
          G_THK   = GBUFS%G_THK       
          G_TAG22 = GBUFS%G_TAG22     
          G_STRA  = GBUFS%G_STRA
          G_SIGI  = GBUFS%G_SIGI
          G_DMG   = GBUFS%G_DMG      
          G_FORPG = GBUFS%G_FORPG
          G_MOMPG = GBUFS%G_MOMPG
C
          G_FORPGPINCH = GBUFS%G_FORPGPINCH
          G_MOMPGPINCH = GBUFS%G_MOMPGPINCH
          G_EPGPINCHXZ = GBUFS%G_EPGPINCHXZ  
          G_EPGPINCHYZ = GBUFS%G_EPGPINCHYZ 
          G_EPGPINCHZZ = GBUFS%G_EPGPINCHZZ 
C   
          G_STRPG = GBUFS%G_STRPG  
          G_UELR  = GBUFS%G_UELR  
          G_UELR1 = GBUFS%G_UELR1  
          G_DAMDL = GBUFS%G_DAMDL 
          G_FORTH = GBUFS%G_FORTH       
          G_EINTTH= GBUFS%G_EINTTH
          G_SEQ   = GBUFS%G_SEQ
          G_STRW  = GBUFS%G_STRW     
          G_STRWPG= GBUFS%G_STRWPG
          G_DT    = GBUFS%G_DT
          G_ISMS  = GBUFS%G_ISMS
          G_ABURN = GBUFS%G_ABURN    
          G_MU    = GBUFS%G_MU    
          G_PLANL = GBUFS%G_PLANL
          G_EPSDNL = GBUFS%G_EPSDNL
          G_TSAIWU = GBUFS%G_TSAIWU
          G_BPRELD= GBUFS%G_BPRELD
          G_COR_NF = GBUFS%G_COR_NF
          G_COR_FR = GBUFS%G_COR_FR
          G_COR_XR = GBUFS%G_COR_XR
          G_MAXFRAC= GBUFS%G_MAXFRAC
          G_MAXEPS = GBUFS%G_MAXEPS
          G_BETAORTH  = GBUFS%G_BETAORTH
          G_AMU    = GBUFS%G_AMU
c---
c 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
c---
          G_AREA        = GBUFS%G_AREA
          G_SKEW        = GBUFS%G_SKEW
          G_LENGTH      = GBUFS%G_LENGTH
          G_TOTDEPL     = GBUFS%G_TOTDEPL
          G_TOTROT      = GBUFS%G_TOTROT
          G_FOREP       = GBUFS%G_FOREP
          G_MOMEP       = GBUFS%G_MOMEP
          G_DEP_IN_TENS = GBUFS%G_DEP_IN_TENS
          G_DEP_IN_COMP = GBUFS%G_DEP_IN_COMP
          G_ROT_IN_TENS = GBUFS%G_ROT_IN_TENS
          G_ROT_IN_COMP = GBUFS%G_ROT_IN_COMP
          G_POSX        = GBUFS%G_POSX
          G_POSY        = GBUFS%G_POSY
          G_POSZ        = GBUFS%G_POSZ
          G_POSXX       = GBUFS%G_POSXX
          G_POSYY       = GBUFS%G_POSYY
          G_POSZZ       = GBUFS%G_POSZZ
          G_YIELD       = GBUFS%G_YIELD
          G_LENGTH_ERR  = GBUFS%G_LENGTH_ERR
          G_DV          = GBUFS%G_DV
          G_DFS         = GBUFS%G_DFS
          G_SKEW_ERR    = GBUFS%G_SKEW_ERR
          G_E6          = GBUFS%G_E6
          G_RUPTCRIT    = GBUFS%G_RUPTCRIT
          G_MASS        = GBUFS%G_MASS
          G_V_REPCVT    = GBUFS%G_V_REPCVT
          G_VR_REPCVT   = GBUFS%G_VR_REPCVT
          G_NUVAR       = GBUFS%G_NUVAR
          G_NUVARN      = GBUFS%G_NUVARN
          G_DEFINI      = GBUFS%G_DEFINI
          G_FORINI      = GBUFS%G_FORINI
          G_INIFRIC     = GBUFS%G_INIFRIC
          G_STRHG       = GBUFS%G_STRHG
          G_SKEW_ID     = GBUFS%G_SKEW_ID
c---
c seatbelt elements
c---
          G_SLIPRING_ID = GBUFS%G_SLIPRING_ID
          G_SLIPRING_FRAM_ID = GBUFS%G_SLIPRING_FRAM_ID
          G_SLIPRING_STRAND = GBUFS%G_SLIPRING_STRAND
          G_RETRACTOR_ID = GBUFS%G_RETRACTOR_ID
          G_RINGSLIP    = GBUFS%G_RINGSLIP
          G_ADD_NODE    = GBUFS%G_ADD_NODE
          G_UPDATE      = GBUFS%G_UPDATE
          G_FRAM_FACTOR = GBUFS%G_FRAM_FACTOR
          G_INTVAR      = GBUFS%G_INTVAR
c
          GBUFT%NOFF(1:NEL*G_NOFF)   = GBUFS%NOFF(1:NEL*G_NOFF)                       
          GBUFT%IERR(1:NEL*G_IERR)   = GBUFS%IERR(1:NEL*G_IERR)                       
          GBUFT%GAMA(1:NEL*G_GAMA)   = GBUFS%GAMA(1:NEL*G_GAMA)          
          GBUFT%SMSTR(1:NEL*G_SMSTR) = GBUFS%SMSTR(1:NEL*G_SMSTR)        
          GBUFT%HOURG(1:NEL*G_HOURG) = GBUFS%HOURG(1:NEL*G_HOURG)        
          GBUFT%BFRAC(1:NEL*G_BFRAC) = GBUFS%BFRAC(1:NEL*G_BFRAC)        
          GBUFT%TAG22(1:NEL*G_TAG22) = GBUFS%TAG22(1:NEL*G_TAG22)           
          GBUFT%STRA(1:NEL*G_STRA)   = GBUFS%STRA(1:NEL*G_STRA)          
          GBUFT%SIGI(1:NEL*G_SIGI)   = GBUFS%SIGI(1:NEL*G_SIGI)          
          GBUFT%DMG(1:NEL*G_DMG)     = GBUFS%DMG(1:NEL*G_DMG)          
          GBUFT%UELR(1:NEL*G_UELR)   = GBUFS%UELR(1:NEL*G_UELR)          
          GBUFT%UELR1(1:NEL*G_UELR1) = GBUFS%UELR1(1:NEL*G_UELR1)          
          GBUFT%DAMDL(1:NEL*G_DAMDL) = GBUFS%DAMDL(1:NEL*G_DAMDL)          
          GBUFT%FOR(1:NEL*G_FOR)     = GBUFS%FOR(1:NEL*G_FOR)
          GBUFT%MOM(1:NEL*G_MOM)     = GBUFS%MOM(1:NEL*G_MOM)
          GBUFT%THK(1:NEL*G_THK)     = GBUFS%THK(1:NEL*G_THK)
          GBUFT%STRW(1:NEL*G_STRW)   = GBUFS%STRW(1:NEL*G_STRW)  
          GBUFT%ABURN(1:NEL*G_ABURN) = GBUFS%ABURN(1:NEL*G_ABURN)        
          GBUFT%MU(1:NEL*G_MU)       = GBUFS%MU(1:NEL*G_MU)        
          GBUFT%BPRELD(1:NEL*G_BPRELD) = GBUFS%BPRELD(1:NEL*G_BPRELD)        
          GBUFT%COR_NF(1:NEL*G_COR_NF) = GBUFS%COR_NF(1:NEL*G_COR_NF)        
          GBUFT%COR_FR(1:NEL*G_COR_FR) = GBUFS%COR_FR(1:NEL*G_COR_FR)        
          GBUFT%COR_XR(1:NEL*G_COR_XR) = GBUFS%COR_XR(1:NEL*G_COR_XR) 
          GBUFT%MAXFRAC(1:NEL*G_MAXFRAC) = GBUFS%MAXFRAC(1:NEL*G_MAXFRAC) 
          GBUFT%MAXEPS(1:NEL*G_MAXEPS) = GBUFS%MAXEPS(1:NEL*G_MAXEPS)      
          GBUFT%BETAORTH(1:NEL*G_BETAORTH) = GBUFS%BETAORTH(1:NEL*G_BETAORTH)
          GBUFT%AMU(1:NEL*G_AMU)       = GBUFS%AMU(1:NEL*G_AMU)   
c
          IF (NPTTOT == 1) THEN  ! global variables are pointing to local ones
            LBUFT => ELBUF_TGT(NG)%BUFLY(1)%LBUF(1,1,1)
            ELBUF_TGT(NG)%GBUF%BFRAC  => LBUFT%BFRAC
            ELBUF_TGT(NG)%GBUF%OFF    => LBUFT%OFF
            ELBUF_TGT(NG)%GBUF%EINT   => LBUFT%EINT
            ELBUF_TGT(NG)%GBUF%EINS   => LBUFT%EINS
            ELBUF_TGT(NG)%GBUF%RHO    => LBUFT%RHO
            ELBUF_TGT(NG)%GBUF%QVIS   => LBUFT%QVIS
            ELBUF_TGT(NG)%GBUF%DELTAX => LBUFT%DELTAX
            ELBUF_TGT(NG)%GBUF%VOL    => LBUFT%VOL
            ELBUF_TGT(NG)%GBUF%EPSD   => LBUFT%EPSD
            ELBUF_TGT(NG)%GBUF%EPSQ   => LBUFT%EPSQ	    
            ELBUF_TGT(NG)%GBUF%PLA    => LBUFT%PLA
            ELBUF_TGT(NG)%GBUF%TEMP   => LBUFT%TEMP
            ELBUF_TGT(NG)%GBUF%TB     => LBUFT%TB
            ELBUF_TGT(NG)%GBUF%RK     => LBUFT%RK
            ELBUF_TGT(NG)%GBUF%RE     => LBUFT%RE
            ELBUF_TGT(NG)%GBUF%SIG    => LBUFT%SIG
            ELBUF_TGT(NG)%GBUF%FORTH  => LBUFT%FORTH
            ELBUF_TGT(NG)%GBUF%EINTTH => LBUFT%EINTTH
            ELBUF_TGT(NG)%GBUF%SEQ    => LBUFT%SEQ
            ELBUF_TGT(NG)%GBUF%ABURN  => LBUFT%ABURN
            ELBUF_TGT(NG)%GBUF%MU     => LBUFT%MU
          ELSE  ! NPTTOT /= 1
            GBUFT%BFRAC(1:NEL*G_BFRAC)  =  GBUFS%BFRAC(1:NEL*G_BFRAC)
            GBUFT%OFF(1:NEL*G_OFF)      =  GBUFS%OFF(1:NEL*G_OFF)             
            GBUFT%EINT(1:NEL*G_EINT)    =  GBUFS%EINT(1:NEL*G_EINT)
            GBUFT%EINS(1:NEL*G_EINS)    =  GBUFS%EINS(1:NEL*G_EINS)
            GBUFT%RHO(1:NEL*G_RHO)      =  GBUFS%RHO(1:NEL*G_RHO)
            GBUFT%QVIS(1:NEL*G_QVIS)    =  GBUFS%QVIS(1:NEL*G_QVIS)
            GBUFT%DELTAX(1:NEL*G_DELTAX)=  GBUFS%DELTAX(1:NEL*G_DELTAX)
            GBUFT%VOL(1:NEL*G_VOL)      =  GBUFS%VOL(1:NEL*G_VOL)
            GBUFT%EPSD(1:NEL*G_EPSD)    =  GBUFS%EPSD(1:NEL*G_EPSD)
            GBUFT%EPSQ(1:NEL*G_EPSQ)    =  GBUFS%EPSQ(1:NEL*G_EPSQ)
            GBUFT%PLA(1:NEL*G_PLA)      =  GBUFS%PLA(1:NEL*G_PLA)
            GBUFT%TEMP(1:NEL*G_TEMP)    =  GBUFS%TEMP(1:NEL*G_TEMP)
            GBUFT%TB(1:NEL*G_TB)        =  GBUFS%TB(1:NEL*G_TB)
            GBUFT%RK(1:NEL*G_RK)        =  GBUFS%RK(1:NEL*G_RK)
            GBUFT%RE(1:NEL*G_RE)        =  GBUFS%RE(1:NEL*G_RE)
            GBUFT%SIG(1:NEL*G_SIG)      =  GBUFS%SIG(1:NEL*G_SIG)
            GBUFT%FORTH(1:NEL*G_FORTH)  =  GBUFS%FORTH(1:NEL*G_FORTH)
            GBUFT%EINTTH(1:NEL*G_EINTTH)=  GBUFS%EINTTH(1:NEL*G_EINTTH)
            GBUFT%SEQ(1:NEL*G_SEQ)      =  GBUFS%SEQ(1:NEL*G_SEQ)
            GBUFT%ABURN(1:NEL*G_ABURN)  =  GBUFS%ABURN(1:NEL*G_ABURN)
            GBUFT%MU(1:NEL*G_MU)        =  GBUFS%MU(1:NEL*G_MU)
          ENDIF  ! NPTTOT
c---
c 1-D elements (TRUSS, BEAM, SPRING, NSTRAND)
c---
          GBUFT%AREA(1:NEL*G_AREA)=  GBUFS%AREA(1:NEL*G_AREA)
          GBUFT%SKEW(1:NEL*G_SKEW)=  GBUFS%SKEW(1:NEL*G_SKEW)
          GBUFT%LENGTH(1:NEL*G_LENGTH)=  GBUFS%LENGTH(1:NEL*G_LENGTH)
          GBUFT%TOTDEPL(1:NEL*G_TOTDEPL)=  GBUFS%TOTDEPL(1:NEL*G_TOTDEPL)
          GBUFT%TOTROT(1:NEL*G_TOTROT)=  GBUFS%TOTROT(1:NEL*G_TOTROT)
          GBUFT%FOREP(1:NEL*G_FOREP)=  GBUFS%FOREP(1:NEL*G_FOREP)
          GBUFT%MOMEP(1:NEL*G_MOMEP)=  GBUFS%MOMEP(1:NEL*G_MOMEP)
          GBUFT%DEP_IN_TENS(1:NEL*G_DEP_IN_TENS)=  GBUFS%DEP_IN_TENS(1:NEL*G_DEP_IN_TENS)
          GBUFT%DEP_IN_COMP(1:NEL*G_DEP_IN_COMP)=  GBUFS%DEP_IN_COMP(1:NEL*G_DEP_IN_COMP)
          GBUFT%ROT_IN_TENS(1:NEL*G_ROT_IN_TENS)=  GBUFS%ROT_IN_TENS(1:NEL*G_ROT_IN_TENS)
          GBUFT%ROT_IN_COMP(1:NEL*G_ROT_IN_COMP)=  GBUFS%ROT_IN_COMP(1:NEL*G_ROT_IN_COMP)
          GBUFT%POSX(1:NEL*G_POSX)=  GBUFS%POSX(1:NEL*G_POSX)
          GBUFT%POSY(1:NEL*G_POSY)=  GBUFS%POSY(1:NEL*G_POSY)
          GBUFT%POSZ(1:NEL*G_POSZ)=  GBUFS%POSZ(1:NEL*G_POSZ)
          GBUFT%POSXX(1:NEL*G_POSXX)=  GBUFS%POSXX(1:NEL*G_POSXX)
          GBUFT%POSYY(1:NEL*G_POSYY)=  GBUFS%POSYY(1:NEL*G_POSYY)
          GBUFT%POSZZ(1:NEL*G_POSZZ)=  GBUFS%POSZZ(1:NEL*G_POSZZ)
          GBUFT%YIELD(1:NEL*G_YIELD)=  GBUFS%YIELD(1:NEL*G_YIELD)
          GBUFT%LENGTH_ERR(1:NEL*G_LENGTH_ERR)=  GBUFS%LENGTH_ERR(1:NEL*G_LENGTH_ERR)
          GBUFT%DV(1:NEL*G_DV)=  GBUFS%DV(1:NEL*G_DV)
          GBUFT%DFS(1:NEL*G_DFS)=  GBUFS%DFS(1:NEL*G_DFS)
          GBUFT%SKEW_ERR(1:NEL*G_SKEW_ERR)=  GBUFS%SKEW_ERR(1:NEL*G_SKEW_ERR)
          GBUFT%E6(1:NEL*G_E6)=  GBUFS%E6(1:NEL*G_E6)
          GBUFT%RUPTCRIT(1:NEL*G_RUPTCRIT)=  GBUFS%RUPTCRIT(1:NEL*G_RUPTCRIT)
          GBUFT%MASS(1:NEL*G_MASS)=  GBUFS%MASS(1:NEL*G_MASS)
          GBUFT%V_REPCVT(1:NEL*G_V_REPCVT)=  GBUFS%V_REPCVT(1:NEL*G_V_REPCVT)
          GBUFT%VR_REPCVT(1:NEL*G_VR_REPCVT)=  GBUFS%VR_REPCVT(1:NEL*G_VR_REPCVT)
          GBUFT%VAR(1:NEL*G_NUVAR)=  GBUFS%VAR(1:NEL*G_NUVAR)
          GBUFT%VARN(1:NEL*G_NUVARN)=  GBUFS%VARN(1:NEL*G_NUVARN)
          GBUFT%DEFINI(1:NEL*G_DEFINI)=  GBUFS%DEFINI(1:NEL*G_DEFINI)
          GBUFT%FORINI(1:NEL*G_FORINI)=  GBUFS%FORINI(1:NEL*G_FORINI)
          GBUFT%INIFRIC(1:NEL*G_INIFRIC)=  GBUFS%INIFRIC(1:NEL*G_INIFRIC)
          GBUFT%STRHG(1:NEL*G_STRHG) = GBUFS%STRHG(1:NEL*G_STRHG)
          GBUFT%SKEW_ID(1:NEL*G_SKEW_ID)=  GBUFS%SKEW_ID(1:NEL*G_SKEW_ID)
c---
c seatbelt elements
c---
          GBUFT%SLIPRING_ID(1:NEL*G_SLIPRING_ID)=GBUFS%SLIPRING_ID(1:NEL*G_SLIPRING_ID)
          GBUFT%SLIPRING_FRAM_ID(1:NEL*G_SLIPRING_FRAM_ID)=GBUFS%SLIPRING_FRAM_ID(1:NEL*G_SLIPRING_FRAM_ID)
          GBUFT%SLIPRING_STRAND(1:NEL*G_SLIPRING_STRAND)= GBUFS%SLIPRING_STRAND(1:NEL*G_SLIPRING_STRAND)
          GBUFT%RETRACTOR_ID(1:NEL*G_RETRACTOR_ID)=GBUFS%RETRACTOR_ID(1:NEL*G_RETRACTOR_ID)
          GBUFT%RINGSLIP(1:NEL*G_RINGSLIP)=GBUFS%RINGSLIP(1:NEL*G_RINGSLIP)
          GBUFT%ADD_NODE(1:NEL*G_ADD_NODE)=GBUFS%ADD_NODE(1:NEL*G_ADD_NODE)
          GBUFT%UPDATE(1:NEL*G_UPDATE)=GBUFS%UPDATE(1:NEL*G_UPDATE)       
          GBUFT%FRAM_FACTOR(1:NEL*G_FRAM_FACTOR)=GBUFS%FRAM_FACTOR(1:NEL*G_FRAM_FACTOR)  
          GBUFT%INTVAR(1:NEL*G_INTVAR)=GBUFS%INTVAR(1:NEL*G_INTVAR) 
c
          IF (NPG == 1) THEN
            IF (NPTTOT == 1) THEN
              LBUFT => ELBUF_TGT(NG)%BUFLY(1)%LBUF(1,1,1)
              ELBUF_TGT(NG)%GBUF%FORPG => LBUFT%FOR
              ELBUF_TGT(NG)%GBUF%MOMPG => LBUFT%MOM
C
C              ELBUF_TGT(NG)%GBUF%FORPGPINCH => LBUFT%FORPINCH ! [PM] add later
C              ELBUF_TGT(NG)%GBUF%MOMPGPINCH => LBUFT%MOMPINCH ! [PM] add later
C              ELBUF_TGT(NG)%GBUF%EPGPINCHXZ => LBUFT%EPGPINCHXZ ! [PM] add later
C              ELBUF_TGT(NG)%GBUF%EPGPINCHYZ => LBUFT%EPGPINCHYZ ! [PM] add later
C              ELBUF_TGT(NG)%GBUF%EPGPINCHZZ => LBUFT%EPGPINCHZZ ! [PM] add later
C
              ELBUF_TGT(NG)%GBUF%STRPG => LBUFT%STRA
            ELSE
              ELBUF_TGT(NG)%GBUF%FORPG => ELBUF_TGT(NG)%GBUF%FOR
              ELBUF_TGT(NG)%GBUF%MOMPG => ELBUF_TGT(NG)%GBUF%MOM
C
C              ELBUF_TGT(NG)%GBUF%FORPGPINCH => ELBUF_TGT(NG)%GBUF%FORPINCH ! [PM] add later
C              ELBUF_TGT(NG)%GBUF%MOMPGPINCH => ELBUF_TGT(NG)%GBUF%MOMPINCH ! [PM] add later
C              ELBUF_TGT(NG)%GBUF%EPGPINCHXZ => ELBUF_TGT(NG)%GBUF%EPGPINCHXZ ! [PM] add later
C              ELBUF_TGT(NG)%GBUF%EPGPINCHYZ => ELBUF_TGT(NG)%GBUF%EPGPINCHYZ ! [PM] add later
C              ELBUF_TGT(NG)%GBUF%EPGPINCHZZ => ELBUF_TGT(NG)%GBUF%EPGPINCHZZ ! [PM] add later
C
              ELBUF_TGT(NG)%GBUF%STRPG => ELBUF_TGT(NG)%GBUF%STRA
            ENDIF
            ELBUF_TGT(NG)%GBUF%STRWPG=> ELBUF_TGT(NG)%GBUF%STRW
          ELSEIF (NPG > 1) THEN
            GBUFT%FORPG(1:NEL*G_FORPG) =  GBUFS%FORPG(1:NEL*G_FORPG)
            GBUFT%MOMPG(1:NEL*G_MOMPG) =  GBUFS%MOMPG(1:NEL*G_MOMPG)
C
            GBUFT%FORPGPINCH(1:NEL*G_FORPGPINCH) =  GBUFS%FORPGPINCH(1:NEL*G_FORPGPINCH)
            GBUFT%MOMPGPINCH(1:NEL*G_MOMPGPINCH) =  GBUFS%MOMPGPINCH(1:NEL*G_MOMPGPINCH)
            GBUFT%EPGPINCHXZ(1:NEL*G_EPGPINCHXZ) =  GBUFS%EPGPINCHXZ(1:NEL*G_EPGPINCHXZ)
            GBUFT%EPGPINCHYZ(1:NEL*G_EPGPINCHYZ) =  GBUFS%EPGPINCHYZ(1:NEL*G_EPGPINCHYZ)
            GBUFT%EPGPINCHZZ(1:NEL*G_EPGPINCHZZ) =  GBUFS%EPGPINCHZZ(1:NEL*G_EPGPINCHZZ)
C
            GBUFT%STRPG(1:NEL*G_STRPG) =  GBUFS%STRPG(1:NEL*G_STRPG)
            GBUFT%STRWPG(1:NEL*G_STRWPG)= GBUFS%STRWPG(1:NEL*G_STRWPG)
          ENDIF
c
        ENDIF  ! ITY
C-----------------------------------------------
      RETURN
      END SUBROUTINE COPY_ELBUF_1

