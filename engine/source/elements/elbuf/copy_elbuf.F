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
Chd|  COPY_ELBUF                    source/elements/elbuf/copy_elbuf.F
Chd|-- called by -----------
Chd|        CP_IMPBUF                     source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        COPY_ELBUF_1                  source/elements/elbuf/copy_elbuf_1.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE COPY_ELBUF(ELBUF_SRC,ELBUF_TGT,IPARG,NGROUP)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGROUP
      INTEGER IPARG(NPARG,NGROUP)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_SRC,ELBUF_TGT
      TARGET :: ELBUF_SRC,ELBUF_TGT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IL,IR,IS,IT,ITY,K,NG,NUVAR,NVARTMP,NFAIL,NLAY,NPTR,NPTS,NPTT,NPT,
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
     .   G_BPRELD,G_COR_NF,G_COR_FR,G_COR_XR,G_DEFINI,G_FORINI,G_MAXFRAC,G_MAXEPS,
     .   G_FORPGPINCH,G_MOMPGPINCH,G_EPGPINCHXZ,G_EPGPINCHYZ,G_EPGPINCHZZ
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
     .   LF_INDX,LF_OFF,L_FORTH,L_EINTTH,L_SEQ,L_SIGPLY,L_FAC_YLD,L_ABURN,L_MU
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
      DO NG = 1,NGROUP      
        CALL COPY_ELBUF_1(ELBUF_SRC,ELBUF_TGT,IPARG,NGROUP,NG)
      ENDDO   ! NG = 1,NGROUP
C-----------------------------------------------
      RETURN
      END      

