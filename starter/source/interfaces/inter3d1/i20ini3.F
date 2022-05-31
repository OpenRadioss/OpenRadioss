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
Chd|  I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        I11BUC1                       source/interfaces/inter3d1/i11buc1.F
Chd|        I20DST3                       source/interfaces/inter3d1/i20dst3.F
Chd|        I20DST3E                      source/interfaces/inter3d1/i20dst3.F
Chd|        I20GAP1                       source/interfaces/inter3d1/i20dst3.F
Chd|        I20NLG                        source/interfaces/inter3d1/i20sti3.F
Chd|        I20NORM                       source/interfaces/inter3d1/i20dst3.F
Chd|        I20PWR3                       source/interfaces/inter3d1/i20pwr3.F
Chd|        I20PWR3A                      source/interfaces/inter3d1/i20pwr3.F
Chd|        I20PWR3AE                     source/interfaces/inter3d1/i20pwr3.F
Chd|        I20PWR3E                      source/interfaces/inter3d1/i20pwr3.F
Chd|        I20STI3                       source/interfaces/inter3d1/i20sti3.F
Chd|        I20STI3E                      source/interfaces/inter3d1/i20sti3.F
Chd|        I20WCONTDD                    source/spmd/domain_decomposition/grid2mat.F
Chd|        I7BUC1                        source/interfaces/inter3d1/i7buc1.F
Chd|        I7COR3                        source/interfaces/inter3d1/i7cor3.F
Chd|        I7ERR3                        source/interfaces/inter3d1/i7err3.F
Chd|        I7NINTDD                      source/spmd/domain_decomposition/grid2mat.F
Chd|        I7PEN3                        source/interfaces/inter3d1/i7pen3.F
Chd|        UPGRADE_IXINT                 source/interfaces/interf1/upgrade_ixint.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        IXINTMOD                      share/modules1/restart_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I20INI3(X       ,IXS    ,IXC     ,
     2                  PM      ,GEO     ,IPARI   ,NUMINT ,ITAB    ,
     3                  MS      ,MWA     ,RWA     ,IXTG   ,IWRN    ,
     4                  IKINE   ,IXT     ,IXP     ,IXR    ,NELEMINT,
     5                  IDDLEVEL,IFIEND  ,NSNET   ,
     6                  NMNET   ,IWCONT ,NSNT    ,
     7                  NMNT    ,KNOD2ELS,KNOD2ELC,KNOD2ELTG,NOD2ELS,
     8                  NOD2ELC ,NOD2ELTG,IGRSURF ,IKINE1 ,IPART   ,
     9                  IPARTC  ,IPARTTG ,THK     ,THK_PART,INPENE ,
     A                  IWPENTOT,IXS10 ,I_MEM    ,
     B                  SIXINT  ,IXS16   ,IXS20   ,ID    ,TITR     ,
     C                  KXX     ,IXX     ,IGEO  ,NOD2EL1D,KNOD2EL1D, 
     D                  LELX    ,INTBUF_TAB ,PM_STACK, IWORKSH )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE IXINTMOD
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "units_c.inc"
#include      "vect07_c.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMINT, IWRN, NSNT, NMNT,SIXINT,
     .   NSNET  ,NMNET, INPENE,IWPENTOT
      INTEGER IXS(NIXS,*), IXC(NIXC,*),
     .   IPARI(*), IXT(NIXT,*) ,IXP(NIXP,*) ,IXR(NIXR,*),
     .   ITAB(*), MWA(*), IXTG(NIXTG,*), IKINE(*),
     .   NELEMINT, IDDLEVEL,IFIEND,
     .   IWCONT(*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), 
     .   NOD2ELS(*), NOD2ELC(*), NOD2ELTG(*),
     .   IPART(*),IPARTC(*), IPARTTG(*),IXS10(*),I_MEM, 
     .   IXS16(*), IXS20(*),KXX(*),IXX(*), IGEO(NPROPGI,*),
     .   NOD2EL1D(*), KNOD2EL1D(*),IWORKSH(3,*)
      INTEGER IKINE1(*)
C     REAL ou REAL*8
      my_real
     .   X(*), PM(*), GEO(*), MS(*),RWA(6,*),
     .   THK(*),THK_PART(*),LELX(*),PM_STACK(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB

      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NSN, NMN, NMN0, NTY, NST, MST, IBUC, NOINT,
     .   NSNE, NMNE,NLINS,NLINM,NLN,IWPENE,IWPENEDGE,
     .   I, I_STOK,I_STOK_E,IRS,IRM,ILEV,IDEL2,
     .   NSEG, NGROUS, NG, INACTI,
     .   JLT_NEW,IGAP,MULTIMP,ISEARCH,ITIED,
     .   IGN,IGE,NME,NMES,NAD,EAD,ISU1,ISU2,
     .   INTTH,NLINSA,NLINMA,ISS2,IFS2,ISYM
      INTEGER
     .   N1(MVSIZ),N2(MVSIZ),M1(MVSIZ),M2(MVSIZ)
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG
C     REAL ou REAL*8
      my_real
     .   MAXBOX,MINBOX,GAP0,BID,TZINF,GAPINF,GAP_TRI,GAPSHMAX,GAPMAX0,
     .   GAPINFS,GAPINFM,GAPE,GAPINPUT,FPENMAX,DRAD
      my_real
     .   NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ),GAPV(MVSIZ),XANEW(3,NUMNOD)
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: SOLIDN_NORMAL

      INTEGER, DIMENSION(MVSIZ) :: IX1,IX2,IX3,IX4
      INTEGER, DIMENSION(MVSIZ) :: PROV_N,PROV_E,NSVG
      my_real, DIMENSION(MVSIZ) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ) :: N11,N21,N31
      my_real, DIMENSION(MVSIZ) :: XI,YI,ZI
      my_real, DIMENSION(MVSIZ) :: X0,Y0,Z0
      my_real, DIMENSION(MVSIZ) :: XX1,YY1,ZZ1
      my_real, DIMENSION(MVSIZ) :: XX2,YY2,ZZ2
      my_real, DIMENSION(MVSIZ) :: XX3,YY3,ZZ3
      my_real, DIMENSION(MVSIZ) :: XX4,YY4,ZZ4
      my_real, DIMENSION(MVSIZ) :: XN1,YN1,ZN1
      my_real, DIMENSION(MVSIZ) :: XN2,YN2,ZN2
      my_real, DIMENSION(MVSIZ) :: XN3,YN3,ZN3
      my_real, DIMENSION(MVSIZ) :: XN4,YN4,ZN4
      my_real, DIMENSION(MVSIZ) :: PENE
      my_real, DIMENSION(MVSIZ) :: P1,P2,P3,P4
      my_real, DIMENSION(MVSIZ) :: LB1,LB2,LB3,LB4
      my_real, DIMENSION(MVSIZ) :: LC1,LC2,LC3,LC4,STIF
C=======================================================================

      BID = ZERO
      IWPENE=0
      IWPENEDGE=0
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NMN0  =NMN 
      NTY   =IPARI(7)
      NST   =IPARI(8)
      MST   =IPARI(9)
      IBUC  =IPARI(12)
      ISEARCH=IPARI(12)
      NOINT =IPARI(15)
      IGAP  =IPARI(21)
      INACTI=IPARI(22)
      MULTIMP=IPARI(23)
      IRM   =IPARI(24)
      IRS   =IPARI(25)
      IDEL2 =IPARI(17)
      ILEV  =IPARI(20)
      ITIED =0
      ISU1  =IPARI(45)
      ISU2  =IPARI(46)
C
      NLN   = IPARI(35)
      ISYM  = IPARI(43)
      DRAD = ZERO

      ALLOCATE(TAG(NUMNOD))
      TAG(1:NUMNOD)=0

      CALL I7ERR3(
     1 X         ,NRTM     ,INTBUF_TAB%IRECTM  ,NOINT     ,ITAB,ID,TITR,
     2         IX1      ,IX2    ,IX3    ,IX4    ,X1     ,
     3         X2       ,X3     ,X4     ,Y1     ,Y2     ,
     4         Y3       ,Y4     ,Z1     ,Z2     ,Z3     ,
     5         Z4       ,N11    ,N21    ,N31    ,X0     ,
     6         Y0       ,Z0     ,XN1    ,YN1    ,ZN1    ,
     7         XN2      ,YN2    ,ZN2    ,XN3    ,YN3    ,
     8         ZN3    ,XN4      ,YN4    ,ZN4      )
C
C     CALCUL DES RIGIDITES ELEMENTTAIRES ET NODALES 
C
      IF(ISU2 /= 0 .and. ISYM == 1)THEN
        IFS2 = 1
        ISS2 = ISU2
      ELSE
        IFS2 = 0
        ISS2 = 1
      ENDIF
      GAPINPUT = INTBUF_TAB%VARIABLES(2)
      CALL I20STI3(
     1 PM             ,GEO            ,X              ,MS             ,
     2 IXS            ,IXC            ,IXTG           ,IXT            ,
     3 IXP            ,RWA            ,NUMINT         ,NTY           ,
     4 NOINT          ,NRTM           ,NSN            ,INTBUF_TAB%IRECTM  ,
     5 INTBUF_TAB%NSV ,INACTI         ,INTBUF_TAB%VARIABLES(2),IGAP       ,
     6 INTBUF_TAB%GAP_S  ,INTBUF_TAB%GAP_M ,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(6),
     7 INTBUF_TAB%VARIABLES(16),INTBUF_TAB%STFAC(1) ,INTBUF_TAB%STFM  ,INTBUF_TAB%STFA  ,                 
     8 KNOD2ELS       ,KNOD2ELC       ,KNOD2ELTG      ,NOD2ELS        ,
     9 NOD2ELC        ,NOD2ELTG       ,IGRSURF(ISU1)  ,IFS2           ,
     A IGRSURF(ISS2)  ,IPARI(47)      ,INTBUF_TAB%IELES  ,
     B INTBUF_TAB%IELEC  ,INTBUF_TAB%AREAS  ,IPARTC           ,IPARTTG        ,
     C THK            ,THK_PART       ,INTBUF_TAB%GAP_SH  ,XANEW              ,
     D GAPSHMAX       ,INTBUF_TAB%NBINFLG  ,INTBUF_TAB%MBINFLG  ,NLN          ,
     E INTBUF_TAB%NLG ,INTBUF_TAB%VARIABLES(29),IXS10         ,IXS16          ,
     F IXS20          ,ID,TITR,IGEO, PM_STACK , IWORKSH )
      IPARI(21) = IGAP
C
C     IL FAUT ENCORE FAIRE ONE BUCKET SORT DANS LE STARTER
C
      MAXBOX  = INTBUF_TAB%VARIABLES(9)
      MINBOX  = INTBUF_TAB%VARIABLES(12)
      GAPMAX0 = INTBUF_TAB%VARIABLES(16) + GAPSHMAX
      CALL I7BUC1(
     1  X           ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,INTBUF_TAB%VARIABLES(4),NSEG ,
     2  NMN         ,NRTM        ,MWA       ,NSN        ,INTBUF_TAB%CAND_E,
     3  INTBUF_TAB%CAND_N,INTBUF_TAB%VARIABLES(2),RWA   ,NOINT   ,I_STOK      ,
     4  INTBUF_TAB%VARIABLES(5),INTBUF_TAB%VARIABLES(8),MAXBOX,MINBOX   ,INTBUF_TAB%MSR,
     5  INTBUF_TAB%STFM,INTBUF_TAB%STFA  ,MULTIMP       ,1       ,IDDLEVEL    ,
     6  ITAB        ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,IGAP,INTBUF_TAB%VARIABLES(13),
     7  GAPMAX0     ,INACTI     ,BID        ,BID,I_MEM,ID,TITR, 0,PROV_N,PROV_E,
     9   NSVG,IX1 ,IX2 ,IX3   ,IX4   ,
     1   N11 ,N21 ,N31 ,PENE  ,X1    ,
     2   X2  ,X3  ,X4  ,Y1    ,Y2    ,
     3   Y3  ,Y4  ,Z1  ,Z2    ,Z3    ,
     4   Z4  ,XI  ,YI  ,ZI    ,X0    ,
     5   Y0  ,Z0  ,XN1 ,YN1   ,ZN1   ,
     6   XN2 ,YN2 ,ZN2 ,XN3   ,YN3   ,
     7   ZN3 ,XN4 ,YN4 ,ZN4   ,P1    ,
     8   P2  ,P3  ,P4  ,LB1   ,LB2   ,
     9   LB3 ,LB4 ,LC1 ,LC2   ,LC3   ,
     1   LC4,STIF)
      if (I_MEM == 2)RETURN

      INTBUF_TAB%VARIABLES(9) = MAXBOX
      INTBUF_TAB%VARIABLES(12) = MINBOX
      IF (IMACH.EQ.3) THEN
        IF (IDDLEVEL.EQ.0)THEN
          IF ( (NELEMINT+I_STOK)*6 >  SIXINT)THEN
            CALL UPGRADE_IXINT(SIXINT,NELEMINT,I_STOK)
          ENDIF
          CALL I7NINTDD(
     1     NELEMINT         ,IXINT(6*NELEMINT+1)    ,INTBUF_TAB%IRECTM,
     2     INTBUF_TAB%NSV   ,INTBUF_TAB%CAND_E      ,INTBUF_TAB%CAND_N,I_STOK,IFIEND,
     3     X                ,INTBUF_TAB%VARIABLES(2),DRAD, IGAP,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,
     4     INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML      ,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16))
        ENDIF
        IF((IDDLEVEL.EQ.0.OR.NDSOLV.EQ.1).AND.
     .     (DECTYP.GE.3.AND.DECTYP.LE.6))THEN
C Appe routine poids noeuds interfaces
          CALL I20WCONTDD(INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN,NMN,IWCONT,
     .                  NSNT,NMNT)
        END IF
      ELSE
        IF (NDSOLV.EQ.1)THEN 
           IF ( (NELEMINT+I_STOK)*6 >  SIXINT)THEN
             CALL UPGRADE_IXINT(SIXINT,NELEMINT,I_STOK)
           ENDIF
           CALL I7NINTDD(
     1       NELEMINT ,IXINT(6*NELEMINT+1),INTBUF_TAB%IRECTM,
     2       INTBUF_TAB%NSV,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,I_STOK,IFIEND,
     3       X,INTBUF_TAB%VARIABLES(2),DRAD,IGAP,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,
     4     INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML      ,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16))
        ENDIF
        IF(DECTYP.GE.3.AND.DECTYP.LE.6)THEN
C Appe routine poids noeuds interfaces
          CALL I20WCONTDD(INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN,NMN,IWCONT,
     .                  NSNT,NMNT)
        END IF
      END IF

c----------------------------------------------------
c   Calcul des normales nodales
c   Igap/=0 pour solides (GAP=0)
c----------------------------------------------------
c      IF(ICURV==3.or.IGAP/=0)THEN
      IF(IGAP/=0)THEN
        ALLOCATE(SOLIDN_NORMAL (3,NUMNOD))
        CALL I20NORM(IPARI(4),INTBUF_TAB%IRECTM,NUMNOD,X,SOLIDN_NORMAL,
     .         IPARI(6),INTBUF_TAB%MSR,NLN,INTBUF_TAB%NLG,INTBUF_TAB%GAP_SH)
      ENDIF
C-----EDGES -------

      NLINS  =IPARI(51)
      NLINM  =IPARI(52)
      NLINSA =IPARI(53)
      NLINMA =IPARI(54)
      NSNE   =IPARI(55)
      NMNE   =IPARI(56)
      
      IF(NLINS + NLINM /= 0)THEN  
C       CALCUL DES RIGIDITES ELEMENTTAIRES 
C
        GAP0 = GAPINPUT
        GAPE = GAPINPUT
        GAPINFS = EP30
        GAPINFM = EP30
        CALL I20STI3E(
     1X         ,INTBUF_TAB%IXLINM ,INTBUF_TAB%STF,IXS      ,PM           ,
     2GEO       ,NLINM         ,IXC         ,NUMINT    ,INTBUF_TAB%STFAC(1),
     3NTY       ,GAPE          ,NOINT       ,INTBUF_TAB%GAP_ME,        
     4MS        ,IXTG          ,IXT         ,IXP       ,IXR          ,
     5IGAP      ,INTBUF_TAB%VARIABLES(13),GAP0      ,GAPINFS   ,NSNE         ,
     6IPARTC    ,IPARTTG       ,THK         ,THK_PART  ,IXS10        ,
     7ID        ,TITR          ,KXX         ,IXX       ,IGEO         ,
     8 NOD2EL1D  ,KNOD2EL1D     ,KNOD2ELS    ,KNOD2ELC  ,KNOD2ELTG    ,
     9 NOD2ELS   ,NOD2ELC       ,NOD2ELTG    ,LELX , PM_STACK , IWORKSH )
C      
        CALL I20STI3E(
     1X         ,INTBUF_TAB%IXLINS,INTBUF_TAB%STFS,IXS      ,PM           ,
     2GEO       ,NLINS        ,IXC         ,-NUMINT   ,INTBUF_TAB%STFAC(1),
     3NTY       ,GAPE         ,NOINT       ,INTBUF_TAB%GAP_SE,       
     4MS        ,IXTG         ,IXT         ,IXP       ,IXR          ,
     5IGAP      ,INTBUF_TAB%VARIABLES(13),GAP0     ,GAPINFM   ,NSNE         ,
     6IPARTC      ,IPARTTG    ,THK         ,THK_PART  ,IXS10        ,
     7ID        ,TITR         ,KXX         ,IXX       ,IGEO         ,
     7 NOD2EL1D  ,KNOD2EL1D    ,KNOD2ELS    ,KNOD2ELC  ,KNOD2ELTG    ,
     8 NOD2ELS   ,NOD2ELC      ,NOD2ELTG    ,LELX , PM_STACK , IWORKSH)
 
        INTBUF_TAB%VARIABLES(2) = MAX(INTBUF_TAB%VARIABLES(2),GAPE)
        GAPINF=GAPINFS+GAPINFM
        GAPINF=MIN(GAPINF,INTBUF_TAB%VARIABLES(6))
        INTBUF_TAB%VARIABLES(6)=MAX(GAPINF,INTBUF_TAB%VARIABLES(13))
C
C       IL FAUT ENCORE FAIRE ONE BUCKET SORT DANS LE STARTER
C
        MAXBOX  = INTBUF_TAB%VARIABLES(9)
        MINBOX  = INTBUF_TAB%VARIABLES(12)
        GAP_TRI = INTBUF_TAB%VARIABLES(2)
c       majoration temporaire du gap pour tri (gap shift)
        IF(IGAP/=0)GAP_TRI=TWO*GAP_TRI
        CALL I11BUC1(
     1X         ,INTBUF_TAB%IXLINM,INTBUF_TAB%IXLINS,INTBUF_TAB%VARIABLES(4),NLINSA,
     2NMNE      ,NLINMA       ,MWA       ,NSNE        ,INTBUF_TAB%LCAND_N,
     3INTBUF_TAB%LCAND_S,GAP_TRI   ,RWA  ,NOINT    ,I_STOK_E  ,
     4INTBUF_TAB%VARIABLES(5),INTBUF_TAB%VARIABLES(8),MAXBOX  ,MINBOX ,INTBUF_TAB%MSRL,
     5INTBUF_TAB%NSVL,MULTIMP   ,INTBUF_TAB%ADCCM20,INTBUF_TAB%CHAIN20,I_MEM,
     6ID,TITR,IDDLEVEL,DRAD, 0)

      if (I_MEM == 2)RETURN
        INTBUF_TAB%VARIABLES(9)  = MAXBOX
        INTBUF_TAB%VARIABLES(12) = MINBOX
C------------------------------------------------------
C     CALCUL LES PENETRATIONS INITIALES 
C     CORRECTION DE LA POSITION DES POINTS D'ANCRAGE 
C     1-EDGES
C------------------------------------------------------
 
        NGROUS=1+(I_STOK_E-1)/NVSIZ
C
        IF(IPRI.GE.1) WRITE(IOUT,2011)
C
        DO NG=1,NGROUS
         NFT = (NG-1) * NVSIZ
         LFT = 1
         LLT = MIN0( NVSIZ, I_STOK_E - NFT )
         JLT_NEW = 0
         CALL I20DST3E(
     1  LLT    ,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%LCAND_S(1+NFT) ,INTBUF_TAB%LCAND_N(1+NFT),
     .  INTBUF_TAB%IXLINS,
     2  INTBUF_TAB%IXLINM,NX      ,NY         ,NZ        ,
     4  N1        ,N2          ,M1         ,M2        ,JLT_NEW   ,
     5  X         ,IGAP        ,INTBUF_TAB%GAP_SE ,INTBUF_TAB%GAP_ME,GAPV,
     6  NLN       ,INTBUF_TAB%NLG,SOLIDN_NORMAL)
C
         FPENMAX   = INTBUF_TAB%VARIABLES(27)
         LLT = JLT_NEW
         CALL I20PWR3AE(ITAB   ,INACTI,INTBUF_TAB%LCAND_N(1+NFT),INTBUF_TAB%LCAND_S(1+NFT),
     2 INTBUF_TAB%STFS,INTBUF_TAB%STF,XANEW     ,INTBUF_TAB%NSVL,IWPENEDGE,
     3 N1        ,N2        ,M1     ,M2        ,NX        ,
     4 NY        ,NZ        ,GAPV   ,INTBUF_TAB%GAP_SE,INTBUF_TAB%GAP_ME,
     5 IGAP      ,X         ,FPENMAX )
         IF(IWPENEDGE.NE.0.AND.INACTI.EQ.3.OR.INACTI.EQ.4) IWRN = 1
        ENDDO
        IF(((IMACH.EQ.3.AND.IDDLEVEL.EQ.0).OR.NDSOLV.EQ.1).AND.
     +  (DECTYP.GE.3.AND.DECTYP.LE.6))THEN
C Appel routine poids noeuds interfaces
          CALL I20WCONTDD(INTBUF_TAB%NSVL,INTBUF_TAB%MSRL,NSNE,NMNE,IWCONT,
     .                NSNET,NMNET)
        END IF
      END IF
C------------------------------------------------------
C     2-NOEUDS FACETTES
C------------------------------------------------------
      IF(IGAP /= 0)CALL I20GAP1(
     1 NRTM         ,NSN          ,NLN,  INTBUF_TAB%GAP_M,INTBUF_TAB%GAP_SH,
     2 INTBUF_TAB%GAP_S,INTBUF_TAB%NBINFLG,INTBUF_TAB%NSV,INTBUF_TAB%NLG,TAG)

      NGROUS=1+(I_STOK-1)/NVSIZ
C
      DO NG=1,NGROUS
       IF(IPRI.GE.1) WRITE(IOUT,2007)
       NFT = (NG-1) * NVSIZ
       LFT = 1
       LLT = MIN0( NVSIZ, I_STOK - NFT )
       CALL I7COR3(
     1  X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT),
     2  INTBUF_TAB%STFM,INTBUF_TAB%STFA,GAPV    ,IGAP    ,INTBUF_TAB%VARIABLES(2) ,
     3  INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,1,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16),
     4  BID         ,BID       ,DRAD,IX1     ,IX2   ,
     5  IX3    ,IX4    ,NSVG,X1      ,X2    ,
     6  X3     ,X4     ,Y1  ,Y2      ,Y3    ,
     7  Y4     ,Z1     ,Z2  ,Z3      ,Z4    ,
     8  XI     ,YI     ,ZI  ,STIF)

       CALL I20DST3(IGAP,INTBUF_TAB%GAP_SH,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT),GAPV ,
     2    INTBUF_TAB%VARIABLES(2),INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,INTBUF_TAB%VARIABLES(16),
     .                                                INTBUF_TAB%VARIABLES(13),
     3    INTBUF_TAB%IRECTM,NLN ,INTBUF_TAB%NLG,SOLIDN_NORMAL,INTBUF_TAB%NSV,
     4    INTBUF_TAB%NBINFLG,TAG,IX3  ,IX4          ,X1 ,
     5    X2,  X3, X4 ,Y1 ,Y2 ,
     6    Y3,  Y4, Z1 ,Z2 ,Z3 ,
     7    Z4,  XI, YI ,ZI ,X0 ,
     8    Y0,  Z0, XN1,YN1,ZN1,
     9    XN2,YN2, ZN2,XN3,YN3,
     1    ZN3,XN4, YN4,ZN4,P1 ,
     2    P2 ,P3 ,P4  ,LB1,LB2,
     3    LB3,LB4,LC1 ,LC2,LC3,
     4    LC4)
       CALL I7PEN3(ZERO,GAPV,N11 ,N21 ,N31 ,
     1             PENE ,XN1 ,YN1,ZN1,XN2,
     2             YN2  ,ZN2 ,XN3,YN3,ZN3,
     3             XN4  ,YN4 ,ZN4,P1 ,P2 ,
     4             P3   ,P4)

       FPENMAX   = INTBUF_TAB%VARIABLES(27)
       CALL I20PWR3A(ITAB  ,INACTI,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT),
     .     INTBUF_TAB%STFA ,
     1     INTBUF_TAB%STFM,XANEW,INTBUF_TAB%NSV,IWPENE    ,IWRN         ,
     2     INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,MWA   ,NOINT     ,GAPV         ,
     3     NTY          ,ITIED , FPENMAX ,ID,TITR ,
     4     IX1,IX2,IX3,IX4,X1,
     5     X2 ,X3 ,X4 ,Y1 ,Y2,
     6     Y3 ,Y4 ,Z1 ,Z2 ,Z3,
     7     Z4 ,XI ,YI ,ZI ,N11,
     8     N21,N31,PENE,NSVG)
      ENDDO

C------------------------------------------------------
C     RE-CALCUL LES PENETRATIONS INITIALES 
C     APRES CORRECTION DE LA POSITION DES POINTS D'ANCRAGE 
C     1-NOEUDS FACETTES
C------------------------------------------------------
 
      NGROUS=1+(I_STOK-1)/NVSIZ
      IWPENE   =0
      IWPENEDGE=0
C
      DO NG=1,NGROUS
       IF(IPRI.GE.1) WRITE(IOUT,2007)
       NFT = (NG-1) * NVSIZ
       LFT = 1
       LLT = MIN0( NVSIZ, I_STOK - NFT )
       CALL I7COR3(
     1  XANEW ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT),
     2  INTBUF_TAB%STFM,INTBUF_TAB%STFA,GAPV    ,IGAP    ,INTBUF_TAB%VARIABLES(2) ,
     3  INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,1,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16),
     4  BID         ,BID     ,DRAD,IX1     ,IX2   ,
     5  IX3    ,IX4    ,NSVG,X1      ,X2    ,
     6  X3     ,X4     ,Y1  ,Y2      ,Y3    ,
     7  Y4     ,Z1     ,Z2  ,Z3      ,Z4    ,
     8  XI     ,YI     ,ZI  ,STIF)

       CALL I20DST3(IGAP,INTBUF_TAB%GAP_SH,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT),GAPV ,
     2     INTBUF_TAB%VARIABLES(2),INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,INTBUF_TAB%VARIABLES(16),
     .                                                 INTBUF_TAB%VARIABLES(13),
     3     INTBUF_TAB%IRECTM,NLN ,INTBUF_TAB%NLG,SOLIDN_NORMAL,INTBUF_TAB%NSV,
     4     INTBUF_TAB%NBINFLG,TAG,IX3  ,IX4          ,X1 ,
     5     X2,  X3, X4 ,Y1 ,Y2 ,
     6     Y3,  Y4, Z1 ,Z2 ,Z3 ,
     7     Z4,  XI, YI ,ZI ,X0 ,
     8     Y0,  Z0, XN1,YN1,ZN1,
     9     XN2,YN2, ZN2,XN3,YN3,
     1     ZN3,XN4, YN4,ZN4,P1 ,
     2     P2 ,P3 ,P4  ,LB1,LB2,
     3     LB3,LB4,LC1 ,LC2,LC3,
     4     LC4)

       CALL I7PEN3(ZERO,GAPV,N11 ,N21 ,N31 ,
     1             PENE,XN1 ,YN1 ,ZN1 ,XN2,
     2             YN2 ,ZN2 ,XN3 ,YN3 ,ZN3,
     3             XN4 ,YN4 ,ZN4 ,P1  ,P2 ,
     4             P3  ,P4)

       CALL I20PWR3(ITAB  ,INACTI,INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%CAND_N(1+NFT),INTBUF_TAB%STFA,
     1     INTBUF_TAB%STFM,XANEW,INTBUF_TAB%NSV,IWPENE    ,IWRN        ,
     2     INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,MWA   ,NOINT    ,GAPV           ,
     3     NTY        ,ITIED ,INTBUF_TAB%PENIS,INTBUF_TAB%PENIM,INTBUF_TAB%GAP_S,
     4     IGAP       ,ID ,TITR,IX1,IX2,
     5     IX3        ,IX4,N11 ,N21,N31,
     6     PENE,NSVG)
      ENDDO
      INTBUF_TAB%I_STOK(1)=IWPENE

C------------------------------------------------------
C     2-EDGES
C------------------------------------------------------
      IF(NLINS /= 0)THEN  
 
        NGROUS=1+(I_STOK_E-1)/NVSIZ
C
        IF(IPRI.GE.1) WRITE(IOUT,2011)
C
        DO NG=1,NGROUS
         NFT = (NG-1) * NVSIZ
         LFT = 1
         LLT = MIN0( NVSIZ, I_STOK_E - NFT )
         JLT_NEW = 0
         CALL I20DST3E(
     1  LLT    ,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%LCAND_S(1+NFT) ,INTBUF_TAB%LCAND_N(1+NFT),
     .  INTBUF_TAB%IXLINS,
     2  INTBUF_TAB%IXLINM,NX      ,NY         ,NZ        ,
     4  N1        ,N2          ,M1         ,M2        ,JLT_NEW   ,
     5  XANEW     ,IGAP        ,INTBUF_TAB%GAP_SE ,INTBUF_TAB%GAP_ME,GAPV,
     6  NLN       ,INTBUF_TAB%NLG,SOLIDN_NORMAL)
         LLT = JLT_NEW
         CALL I20PWR3E(ITAB   ,INACTI,INTBUF_TAB%LCAND_S(1+NFT),INTBUF_TAB%LCAND_N(1+NFT),
     2 INTBUF_TAB%STFS,INTBUF_TAB%STF,XANEW  ,INTBUF_TAB%NSVL,IWPENEDGE,
     3 N1        ,N2        ,M1     ,M2        ,NX        ,
     4 NY        ,NZ        ,GAPV   ,INTBUF_TAB%GAP_SE,INTBUF_TAB%GAP_ME,
     5 INTBUF_TAB%PENISE,INTBUF_TAB%PENIME,IGAP   )
         IF(IWPENEDGE.NE.0.AND.INACTI.EQ.3.OR.INACTI.EQ.4) IWRN = 1
        ENDDO
        IF(((IMACH.EQ.3.AND.IDDLEVEL.EQ.0).OR.NDSOLV.EQ.1).AND.
     +  (DECTYP.GE.3.AND.DECTYP.LE.6))THEN
C Appel routine poids noeuds interfaces
          CALL I20WCONTDD(INTBUF_TAB%NSVL,INTBUF_TAB%MSRL,NSNE,NMNE,IWCONT,
     .                NSNET,NMNET)
        END IF
      END IF
C-----------
c replace global node by local node in NSV,IRECT,NSVE,LINE...
      CALL I20NLG(NLN,NRTM,NSN          ,NLINS        ,NLINM        ,
     2 INTBUF_TAB%NLG,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,INTBUF_TAB%IXLINS,
     2                                                 INTBUF_TAB%IXLINM,
     3 NMN          ,NSNE         ,NMNE   ,INTBUF_TAB%MSR,INTBUF_TAB%NSVL,
     4 INTBUF_TAB%MSRL,INTBUF_TAB%STFA,INTBUF_TAB%AVX_ANCR,XANEW  ,X          ,
     5 INTBUF_TAB%PENIA,INTBUF_TAB%ALPHAK)

c      IF(ICURV==3.OR.IADM/=.OR.IGAP/=0)THEN
      IF(IGAP/=0)THEN
          DEALLOCATE(SOLIDN_NORMAL)
      END IF

      IWPENTOT = IWPENE + IWPENEDGE
      DEALLOCATE(TAG)
C-----------
      RETURN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
 2007 FORMAT(//'    IMPACT CANDIDATES',/,
     +'  MAIN        SECONDARY  NODES '/
     +'   NODE ')
 2011 FORMAT(//'    IMPACT CANDIDATES',/,
     +'  MAIN   NODES     SECONDARY  NODES ')
      END
