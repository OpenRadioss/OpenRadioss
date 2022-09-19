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
Chd|  LASER2                        source/loads/laser/laser2.F   
Chd|-- called by -----------
Chd|        LASER1                        source/loads/laser/laser1.F   
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        INTERP                        source/tools/curve/interp.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE LASER2(NL ,N1   ,N2   ,IFUNC    ,IAFUNC ,
     .                 LAS ,XLAS ,X    ,ELBUF_TAB,PM     ,
     .                 WA  ,IPARG,IXQ  ,TF       ,NPF    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr11_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NL, N1, N2, IFUNC, IAFUNC
      INTEGER LAS(2,*), IPARG(NPARG,*), IXQ(7,*), NPF(*)
      my_real
     .   XLAS(*),X(3,*),WA(3,*),TF(*),PM(NPROPM,*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IL, NG, I, NEL, NFT, II, M3,
     .   M4, M11, M12, NP1, NPOINT, IC, NELC, NB1C, NFTC, M4C, M13C,
     .   M2, M6, MX, M11C
      INTEGER  MTN,IAD,ITY,NPT,JALE,ISMSTR,
     .   JEUL,JTUR,JTHE,JLAG,NVAUX,JMULT,JHBE,JIVF,JPOR,JPLA,JCLOSE,
     .   IREP,IINT,IGTYP,JCVT,ISROT,ISRAT,ISORTH,ISORTHG,ICSEN,IFAILURE,
     .   JSMS
      my_real
     .   CHALEUR, FI, ALPHA, Z1, Z2, Z3, Z4, ZZ, T, DDFI,
     .   DFI, DE, AF, BID, RHOC, C0, ZM, ZMC, Y1, Y2, Y3, Y4, A1, A2,
     .   AIRE, D, VM, SZ, DAR, FI0, VOL, XKZZ, RHOA2, RHO0,
     .   TC, DFI1, SY, SX, P, DP, XK0,HNUK,XK,ATOM,RHO,Z,TE,TSCAL,FIFUN
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      TYPE(L_BUFEL_)  ,POINTER :: LBUF
C-----------------------------------------------
       CHALEUR = XLAS(1)
       FI      = XLAS(2)
       ALPHA   = XLAS(3)
       XK0     = XLAS(4)
       HNUK    = XLAS(5)
       DAR     = XLAS(6)
       TSCAL   = XLAS(7)
C
       IF(IFUNC > 0) THEN
         TSCAL  = TSCAL*TT
         NPOINT = (NPF(IFUNC+1)-NPF(IFUNC))/2
         CALL INTERP(TF(NPF(IFUNC)),TSCAL,NPOINT,FIFUN,BID)
         FI     = FI * FIFUN
       ENDIF
       FI0 = FI
C
       JSMS=0
C------------------------------------------------------------
       DO 200 IL=1,NL
         NG   =  LAS(1,IL)
         I    =  LAS(2,IL)
         GBUF => ELBUF_TAB(NG)%GBUF
         LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)

         CALL INITBUF (
     1     IPARG   ,NG      ,
     2     MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,
     3     NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4     JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5     NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLA    ,
     6     IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7     ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )

        II = I + NFT
        Z1 = X(3,IXQ(2,II))
        Z2 = X(3,IXQ(3,II))
        Z3 = X(3,IXQ(4,II))
        Z4 = X(3,IXQ(5,II))
        ZZ = HALF * (ABS(Z1 - Z3) + ABS(Z2 - Z4))
C-------------------------------------------------------------
C ABSORPTION DE LA LUMIERE LASER DANS LE PLASMA  BREMSTRALUNG  
C-------------------------------------------------------------
        RHO      = GBUF%RHO(I)
        VOL      = GBUF%VOL(I)
        TE       = GBUF%TEMP(I)
        Z        = LBUF%Z(I)
        MX       = IXQ(1,II)
        ATOM     = PM(37,MX)
        RHO0     = PM(1,MX)
C
        RHOA2    = (RHO/ATOM)**2
        XK       = XK0  * RHOA2 * (Z / HNUK)**3 / SQRT(TE)
        XKZZ     = XK * (ONE - EXP(-HNUK/TE)) * ZZ
        RHOA2    = ((RHO-RHO0)/ATOM)**2
        IF(TE<=EP04) XKZZ = XKZZ + DAR * RHOA2 * ZZ
C
        DDFI     = (ONE - EXP(-XKZZ))
        DFI      = FI * DDFI
        FI       = FI - DFI
        DE       = DFI * DT2 / ZZ
C
        GBUF%EINT(I) = GBUF%EINT(I) + DE
        TFEXT    = TFEXT + DE * VOL
C
        WA(1,IL) = ZZ
        WA(2,IL) = DDFI
C
  200  CONTINUE
C
C-------------------------------------------------------------
C ABSORPTION DE LA LUMIERE LASER DANS LA CIBLE => VAPORISATION
C-------------------------------------------------------------
C
       IF(IAFUNC>0)THEN
        NP1    = NPF(IAFUNC)
        NPOINT =(NPF(IAFUNC+1)-NP1)/2
        T      = TE*TSCAL
        CALL INTERP(TF(NP1),T,NPOINT,AF,BID)
        ALPHA  = ALPHA * AF
       ENDIF
       DFI     = ALPHA * FI
       FI      = FI - DFI
       DE      = DFI * DT2 / ZZ
C
       GBUF%EINT(I) = GBUF%EINT(I) + DE
       TFEXT   = TFEXT + DE * VOL
C-------------------------
C VITESSE DE VAPORISATION  
C-------------------------
       IL       = NL+1
       NG       = LAS(1,IL)
       IC       = LAS(2,IL)
c
       GBUF     => ELBUF_TAB(NG)%GBUF
c
       NELC     = IPARG(2,NG)
       NB1C     = IPARG(4,NG)
       NFTC     = IPARG(3,NG)
       II       = IC + NFTC
       M4C      = NB1C+8*NELC+IC-1
       M11C     = NB1C+12*NELC+IC-1
       M13C     = NB1C+14*NELC+IC-1

       RHOC     = GBUF%RHO(IC)
       C0       = GBUF%EPSD(IC)

       ZM       = Z1 + Z2 + Z3 + Z4
       Z1       = X(3,IXQ(2,II))
       Z2       = X(3,IXQ(3,II))
       Z3       = X(3,IXQ(4,II))
       Z4       = X(3,IXQ(5,II))
       ZMC      = Z1 + Z2 + Z3 + Z4
       Y1       = X(2,IXQ(2,II))
       Y2       = X(2,IXQ(3,II))
       Y3       = X(2,IXQ(4,II))
       Y4       = X(2,IXQ(5,II))
       A1       = Y2*(Z3-Z4)+Y3*(Z4-Z2)+Y4*(Z2-Z3)
       A2       = Y2*(Z4-Z1)+Y4*(Z1-Z2)+Y1*(Z2-Z4)
       AIRE     = (A1+A2)/TWO

       TC       = GBUF%TEMP(IC)
       DFI1     = PM(75,MX) * (TWO*(TE - TC)/ZZ) / DT2

       D        = (DFI+DFI1) / (RHOC*CHALEUR)

       VM       = D * AIRE * RHOC * FOURTH
       IF(ZMC>ZM) VM = -VM
       WA(3,N1) = WA(3,N1) + VM
       WA(3,N2) = WA(3,N2) + VM
C-------------------------
C PRESSION DE CIBLE  
C-------------------------
c       NB2 = NB1+NEL
c       M2  = NB2+6*I-6
c       SY  = BUFEL(M2)
c       SZ  = BUFEL(M2+1)
c       SX  = BUFEL(M2+2)
c       P   = -(SX+SY+SZ) * THIRD
c       DP  = RHOC*C0*D
c       DP  = MIN(DP,P)
c       DP  = MAX(DP,ZERO)
c       SX  = -P + DP * HALF
c       SY  = -P + DP * HALF
c       SZ  = -P - DP
C     
C-------------------------
C REFLEXION DU RAYON LASER  
C-------------------------
C
      DO IL=NL,1,-1
C-------------------------------------------------------------
C ABSORPTION DE LA LUMIERE LASER DANS LE PLASMA  BREMSTRALUNG  
C-------------------------------------------------------------
        NG           = LAS(1,IL)
        I            = LAS(2,IL)
        GBUF         => ELBUF_TAB(NG)%GBUF
        ZZ           = WA(1,IL)
        DDFI         = WA(2,IL)
        DFI          = FI * DDFI
        FI           = FI - DFI
        DE           = DFI * DT2 / ZZ
        GBUF%EINT(I) = GBUF%EINT(I) + DE
        VOL          = GBUF%VOL(I)
        TFEXT        = TFEXT + DE * VOL
      ENDDO
C-------------------------
      RETURN
      END
C
Chd|====================================================================
Chd|  LASER3                        source/loads/laser/laser2.F   
Chd|-- called by -----------
Chd|        LASER1                        source/loads/laser/laser1.F   
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        INTERP                        source/tools/curve/interp.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE LASER3(NL ,N1   ,N2   ,IFUNC ,IAFUNC ,
     .                 LAS ,XLAS ,X    ,ELBUF_TAB ,PM     ,
     .                 WA  ,IPARG,IXQ  ,TF    ,NPF    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr11_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NL, N1, N2, IFUNC, IAFUNC
      INTEGER LAS(2,*), IPARG(NPARG,*), IXQ(7,*), NPF(*)
      my_real
     .   XLAS(*),X(3,*),WA(3,*),TF(*),PM(NPROPM,*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IL, NG, I, NEL, NFT, II, M3,
     .   M4, M11, M12, NP1, NPOINT, IC, NELC, NB1C, NFTC, M4C, M13C,
     .   M2, M6, MX, M11C
      INTEGER  MTN,IAD,ITY,NPT,JALE,ISMSTR,
     .   JEUL,JTUR,JTHE,JLAG,NVAUX,JMULT,JHBE,JIVF,JPOR,JPLA,JCLOSE,
     .   IREP,IINT,IGTYP,JCVT,ISROT,ISRAT,ISORTH,ISORTHG,ICSEN,IFAILURE,
     .   JSMS
      my_real
     .   FI, Z1, Z2, Z3, Z4, ZZ, T, DDFI,
     .   DFI, DE, AF, BID, RHOC, C0, ZM, ZMC, Y1, Y2, Y3, Y4, A1, A2,
     .   AIRE, D, VM, SZ, VOL, RHOA2, RHO0,
     .   TC, DFI1, SY, SX, P, DP, TSCAL,FIFUN,
     .   XK0,HNUK,XK,ATOM,RHO,Z,TE,ENERLIM,ENER
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C=======================================================================
      ENERLIM   = XLAS(1)
      FI        = XLAS(2)
      TSCAL     = XLAS(7)
C
       IF(IFUNC > 0) THEN
         TSCAL = TSCAL*TT
         NPOINT=(NPF(IFUNC+1)-NPF(IFUNC))/2
         CALL INTERP(TF(NPF(IFUNC)),TSCAL,NPOINT,FIFUN,BID)
         FI = FI * FIFUN
       ENDIF
C
      JSMS = 0
      DFI  = ZERO
C-------------------------------------------------------------
      DO IL=1,NL
        NG   = LAS(1,IL)
        I    = LAS(2,IL)
        GBUF => ELBUF_TAB(NG)%GBUF
        CALL INITBUF (IPARG    ,NG      ,
     2     MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,
     3     NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4     JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5     NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLA    ,
     6     IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7     ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        II   = I + NFT
        MX   = IXQ(1,II)
        ENER = GBUF%EINT(I)
        RHO  = GBUF%RHO(I)
        VOL  = GBUF%VOL(I)
        TE   = GBUF%TEMP(I)
        RHO0 = PM(1,MX)
        IF(ENER<ENERLIM)THEN
          Z1           = X(3,IXQ(2,II))
          Z2           = X(3,IXQ(3,II))
          Z3           = X(3,IXQ(4,II))
          Z4           = X(3,IXQ(5,II))
          Y1           = X(2,IXQ(2,II))
          Y2           = X(2,IXQ(3,II))
          Y3           = X(2,IXQ(4,II))
          Y4           = X(2,IXQ(5,II))
          ZZ           = HALF * (ABS(Z1 - Z3) + ABS(Z2 - Z4))
          DE           = DFI * DT2 / ZZ      !waring DFI is zero : need to be validated, may be wrong
          GBUF%EINT(I) = GBUF%EINT(I) + DE
          TFEXT        = TFEXT + DE * VOL
          EXIT !and return
        ENDIF
      ENDDO
C-----------
      RETURN
      END

