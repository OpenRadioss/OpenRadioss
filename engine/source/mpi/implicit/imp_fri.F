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
Chd|  IMP_FRI                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_INTFR                     source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRKI                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRSL                      source/mpi/implicit/imp_fri.F 
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_FRI(
     1    NUM_IMP   ,NS_IMP    ,NE_IMP    ,IPARI     ,INTBUF_TAB,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,
     3    IRBYAC    ,NINT2     ,IINT2     ,IDDL      ,IKC       ,
     4    NDOF      ,INLOC     ,NSREM     ,NSL       ,NBINTC    ,
     5    INTLIST   ,X         ,IBFV      ,LJ        ,SKEW      ,
     6    XFRAME    ,ISKEW     ,ICODT     ,A         ,UD        ,
     7    LB        ,IFDIS     ,NDDL      ,URD       ,IDDLI     ,
     8    IRBE3     ,LRBE3     ,FRBE3     ,IRBE2     ,LRBE2     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),NUM_IMP(*),NS_IMP(*),
     .        NE_IMP(*),NSREM  ,NSL,NBINTC,INTLIST(*),
     .        IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*)
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IDDL(*),IKC(*),NDOF(*),INLOC(*),
     .        IBFV(*),LJ(*),ISKEW(*),ICODT(*),IFDIS,NDDL,IDDLI(*)
C     REAL ou REAL*8
      my_real
     .       X(3,*),SKEW(*) ,XFRAME(*),
     .       A(3,*),UD(3,*),LB(*),URD(3,*),FRBE3(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IKPAT0
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: LB0
C-----------------------------------------------
        CALL IMP_FRSL(NBINTC,NSREM ,NSL   )
C-----------------------------------------------
       IF (INTP_C.GT.0) THEN
        ALLOCATE(LB0(NDDL))
            LB0=ZERO
       ENDIF
       IF ((NSREM+NSL).GT.0) THEN
        IF (INTP_C.LE.0) THEN
         CALL IMP_FRKI(
     1      IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     2      NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3      NINT2     ,IINT2     ,IDDL      ,IKC       ,NDOF      ,
     4      INLOC     ,NSREM     ,NSL       ,NBINTC    ,INTLIST   ,
     5      X         ,IBFV      ,LJ        ,SKEW      ,
     6      XFRAME    ,ISKEW     ,ICODT     ,IRBE3     ,LRBE3     ,
     7      FRBE3     ,IRBE2     ,LRBE2     )
        ELSE
         IKPAT0 = IKPAT
         IKPAT = 1
         CALL IMP_FRKD(
     1      NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2      IPARI     ,INTBUF_TAB,NINT2     ,IINT2     ,IDDL      ,
     3      IKC       ,NDOF      ,INLOC     ,NSREM     ,NSL       ,
     4      NBINTC    ,INTLIST   ,X         ,IBFV      ,
     5      LJ        ,SKEW      ,XFRAME    ,ISKEW     ,ICODT     ,
     6      A         ,UD        ,LB0       ,IFDIS     ,URD       ,
     7      IDDLI     ,IRBE3     ,LRBE3     ,FRBE3     ,IRBE2     ,
     8      LRBE2     )
         IKPAT = IKPAT0
        ENDIF
       ENDIF
       IF (INTP_C.GT.0) THEN
        CALL SPMD_SUMF_V(LB0)
        DO I=1,NDDL
         LB(I) = LB(I)+LB0(I)
        ENDDO
        DEALLOCATE(LB0)
       ENDIF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FRFV                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_INTFR                     source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|        INI_DD0                       source/mpi/implicit/imp_fri.F 
Chd|        INI_DDFV                      source/mpi/implicit/imp_fri.F 
Chd|        PRODUT_VMHP                   source/implicit/produt_v.F    
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_FRFV(
     1    NUM_IMP   ,NS_IMP    ,NE_IMP    ,IPARI     ,INTBUF_TAB,
     2    IDDL      ,IKC       ,NDOF      ,NSREM     ,
     3    NSL       ,D_IMP     ,DD        ,DR_IMP    ,DDR       ,
     4    A         ,AR        ,MS        ,V         ,X         ,
     5    LB        ,NDDL      ,IBFV      ,SKEW      ,XFRAME    ,
     6    IRBE3     ,LRBE3     ,IRBE2     ,LRBE2     ,DE        ,
     7    NDDL0     ,W_DDL     )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),NUM_IMP(*),NS_IMP(*),
     .        NE_IMP(*),NSREM  ,NSL,NDDL,IRBE3(*),LRBE3(*)
      INTEGER IDDL(*),IKC(*),NDOF(*),IBFV(*),IRBE2(*),LRBE2(*),
     .        NDDL0 ,W_DDL(*)
C     REAL ou REAL*8
      my_real
     .       D_IMP(3,*) ,DD(3,*),DR_IMP(3,*) ,DDR(3,*),LB(*),
     .       A(3,*) ,AR(3,*),MS(*) ,V(3,*),X(3,*),
     .       SKEW(*),XFRAME(*),DE
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L
      my_real
     .       LX(NDDL)
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: DD_TMP,DDR_TMP
C-----------------------------------------------
       IF (ILINE.GT.0) THEN
          DO I =1,NDDL
           LX(I)=ZERO
          ENDDO
          CALL FR_MATV(    A     ,V     ,D_IMP ,MS     ,X       ,
     1                     DR_IMP,AR    ,IPARI ,INTBUF_TAB      ,
     2                     NDOF  ,NUM_IMP,NS_IMP,NE_IMP,LX      ,
     3                     NSREM ,NSL    ,IBFV ,SKEW   ,XFRAME  ,
     4                     LX    ,IRBE3  ,LRBE3,IRBE2  ,LRBE2   )
          CALL SPMD_SUMF_V(LX)
          DO I =1,NDDL
           LB(I) = LB(I)-LX(I)
          ENDDO
         IF ((NSREM+NSL).GT.0)
     .   CALL INI_DD0(IDDL ,IKC   ,NDOF  ,IPARI ,INTBUF_TAB,
     .                D_IMP,DR_IMP,NSL   ,IRBE3,LRBE3 ,
     .                IRBE2,LRBE2 )
       ELSE
        L=3*NUMNOD
        ALLOCATE(DD_TMP(L))
        DD_TMP=ZERO
        IF (IRODDL.NE.0) THEN
         ALLOCATE(DDR_TMP(L))
         DDR_TMP=ZERO
        END IF
        IF ((NSREM+NSL).GT.0)
     .   CALL INI_DDFV(IDDL ,IKC   ,NDOF  ,IPARI ,INTBUF_TAB,
     .                 D_IMP,DR_IMP,DD_TMP,DDR_TMP,NSL   ,
     .                 IRBE3,LRBE3 ,IRBE2 ,LRBE2 )
          DO I =1,NDDL
           LX(I)=ZERO
          ENDDO
          CALL FR_MATV(    A     ,V     ,DD_TMP    ,MS     ,X       ,
     1                     DDR_TMP   ,AR    ,IPARI ,INTBUF_TAB      ,
     2                     NDOF  ,NUM_IMP,NS_IMP,NE_IMP,LX      ,
     3                     NSREM ,NSL    ,IBFV ,SKEW   ,XFRAME  ,
     4                     LX    ,IRBE3  ,LRBE3 ,IRBE2 ,LRBE2   )
          DEALLOCATE(DD_TMP)
          IF (IRODDL.NE.0) DEALLOCATE(DDR_TMP)
          CALL SPMD_SUMF_V(LX)
          DO I =1,NDDL
           LB(I) = LB(I)-LX(I)
          ENDDO
C------calcul DE for L_search as DD,DDR will be updated           
          CALL PRODUT_VMHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                     DD    ,DDR   ,LB    ,DE    ,W_DDL )
        IF ((NSREM+NSL).GT.0)
     .   CALL INI_DD0(IDDL ,IKC   ,NDOF  ,IPARI ,INTBUF_TAB ,
     .                DD    ,DDR  ,NSL   ,IRBE3,LRBE3  ,
     .                IRBE2 ,LRBE2)
       ENDIF !(ILINE.GT.0) THEN
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FRKI                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRI                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|        DIM_KINEFR                    source/mpi/implicit/imp_fri.F 
Chd|        IDDL_INT                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRSN                      source/mpi/implicit/imp_fri.F 
Chd|        IND_KINEFR                    source/mpi/implicit/imp_fri.F 
Chd|        INI_INTM                      source/mpi/implicit/imp_fri.F 
Chd|        SPMD_IFC1                     source/mpi/implicit/imp_spmd.F
Chd|        TAG_INTM                      source/mpi/implicit/imp_fri.F 
Chd|        TAG_INTM11                    source/mpi/implicit/imp_fri.F 
Chd|        TAG_INTS                      source/mpi/implicit/imp_fri.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_FRKI(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    NINT2     ,IINT2     ,IDDL      ,IKC       ,NDOF      ,
     4    INLOC     ,NSREM     ,NSL       ,NBINTC    ,INTLIST   ,
     5    X         ,IBFV      ,LJ        ,SKEW      ,
     6    XFRAME    ,ISKEW     ,ICODT     ,IRBE3     ,LRBE3     ,
     7    FRBE3     ,IRBE2     ,LRBE2     )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),NUM_IMP(*),NS_IMP(*),
     .        NE_IMP(*),NSREM  ,NSL,NBINTC,INTLIST(*)
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IDDL(*),IKC(*),NDOF(*),INLOC(*),
     .        IBFV(*),LJ(*),ISKEW(*),ICODT(*),IRBE3(*),LRBE3(*),
     .        IRBE2(*),LRBE2(*)
C     REAL ou REAL*8
      my_real
     .       X(3,*),SKEW(*),XFRAME(*),FRBE3(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIN,NTY,ILOCP(NUMNOD),IDDLM(NUMNOD),LNS,LNS2,N_KINE
      INTEGER I,J,K,L,NDOFI,N,IAD,NRTS,
     .        NSN,NKC,J1,ND,N_KINE_M,LNSS ,LNSS2,LNS3,LNSS3,
     .        LRS2,LRSS2
      my_real
     .       KSS(6,NSL)
C------com. tableau de noeuds secnds remotes (a proc origine) et diag_ss
      CALL SPMD_IFC1(NSREM  ,NSL ,KSS )
      IF (NSL.GT.0) CALL IMP_FRSN(IPARI ,INTBUF_TAB  ,NBINTC,INTLIST)
C      IF ((NSREM+NSL).EQ.0) RETURN
      DO N =1,NUMNOD
       ILOCP(N)=0
      ENDDO
C--------tag noeuds supp eventuellement dependants----
      N_KINE=0
C
      CALL TAG_INTS(NSL    ,ILOCP     ,N_KINE)
      N_KINE_M=N_KINE
      IF (NSREM.GT.0) THEN
       IAD=1
       DO NIN=1,NINTER
        NTY   =IPARI(7,NIN)
        IF(NTY.EQ.5) IAD=IAD+NUM_IMP(NIN)
       ENDDO
       DO NIN=1,NINTER
        NSN   =IPARI(5,NIN)
        NTY   =IPARI(7,NIN)
        IF(NTY.EQ.3)THEN
        ELSEIF(NTY.EQ.4)THEN
        ELSEIF(NTY.EQ.5)THEN
        ELSEIF(NTY.EQ.6)THEN
        ELSEIF(NTY.EQ.7.OR.NTY.EQ.10.OR.NTY.EQ.24)THEN
C
         CALL TAG_INTM(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                 INTBUF_TAB(NIN)%IRECTM,INTBUF_TAB(NIN)%NSV , ILOCP    ,
     .                 N_KINE  ,NSN   )
         IAD=IAD+NUM_IMP(NIN)
C
        ELSEIF(NTY.EQ.11)THEN
         NRTS   =IPARI(3,NIN)
         CALL TAG_INTM11(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                 INTBUF_TAB(NIN)%IRECTS,INTBUF_TAB(NIN)%IRECTM, ILOCP   ,
     .                 N_KINE  ,NRTS  )
         IAD=IAD+NUM_IMP(NIN)
C
        ENDIF
       ENDDO
      ENDIF
C-------init iml--isl ----
      CALL INI_INTM(ILOCP  ,N_KINE_M, N_KINE    )
C-------iddlm-----------------
      NKC=0
      DO N =1,NUMNOD
       I=INLOC(N)
       IDDLM(I)=IDDL(I)-NKC
       DO J=1,NDOF(I)
        ND = IDDL(I)+J
        IF (IKC(ND).NE.0) NKC = NKC + 1
       ENDDO
      ENDDO
C-----couplage avec cond. kine ------
      CALL DIM_KINEFR(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,ILOCP,
     3    LNS       ,LNS2      ,LNSS      ,LNSS2     ,N_KINE_M  ,
     4    IRBE3     ,LNS3      ,LNSS3     ,IRBE2     ,LRBE2     ,
     5    LRS2      ,LRSS2     )
      CALL IND_KINEFR(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,ILOCP,
     3    LNS       ,LNS2      ,LNSS      ,LNSS2     ,N_KINE_M  ,
     4    IBFV      ,LJ        ,ISKEW     ,ICODT     ,IRBE3     ,
     5    LNS3      ,LNSS3     ,IRBE2     ,LRBE2     ,LRS2      ,
     6    LRSS2     )
      CALL IDDL_INT(NSL    ,IDDL    ,IKC    ,NDOF     ,IDDLM   ,
     .              IPARI  ,INTBUF_TAB ,IRBE3  ,LRBE3    ,FRBE3   ,
     .              X      ,SKEW    ,IRBE2  ,LRBE2    )
      IF (NSL.GT.0)
     . CALL DIAG_INT(NSL    ,NDOF    ,IPARI  ,INTBUF_TAB,
     .               KSS    ,X       ,IBFV   ,SKEW    ,XFRAME  ,
     .               IRBE3  ,LRBE3   ,IRBE2  ,LRBE2   )
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Cqz------retouve ISL en num global(par chaque proc)---
Chd|====================================================================
Chd|  IMP_FRSN                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRKI                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_FRSN(IPARI ,INTBUF_TAB  ,NBINTC,INTLIST)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IPARI(NPARI,*), NBINTC,INTLIST(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,N,NN,IAD,NSN,NIN,NUM,NTY,II,NB
      INTEGER N0,N1,N2
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
        L = 0
C -----tri par proc---independant interface structure------
        DO I = 1, NSPMD
          IF(IAD_SL(I+1).GT.IAD_SL(I))THEN
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              NB = INBSL(II,I)
              IF(NB.GT.0)THEN
               NTY  = IPARI(7,NIN)
               IF(NTY.EQ.7.OR.NTY.EQ.10.OR.NTY.EQ.24) THEN
                  DO J = 1, NB
                   L = L + 1
                   N0 = ISL(L)
                   N = INTBUF_TAB(NIN)%NSV(N0)
                   ISL(L) = N
                  ENDDO
C
               ELSEIF(NTY.EQ.11) THEN
                  DO J = 1, NB/2
                   L = L + 1
                   N0 = ISL(L)
                   N1 = INTBUF_TAB(NIN)%IRECTS(2*(N0-1)+1)
                   N2 = INTBUF_TAB(NIN)%IRECTS(2*(N0-1)+2)
                   ISL(L) = N1
                   L = L + 1
                   ISL(L) = N2
                  ENDDO
C
               END IF
              END IF
            ENDDO
          ENDIF
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FRII                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE IMP_FRII(NINTER)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NINTER
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      ALLOCATE(IND_INT(NINTER))
      DO I=1,NINTER
         NULLIFY(IND_INT(I)%p)
      ENDDO
      ALLOCATE(SHF_INT(NINTER))
C
      INTP_D = MAX(0,INTP_C)
      NDDL_SI = 0
      NDDL_SL = 0
      NZ_SI = 0
      NZ_SL = 0
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FR7I                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE IMP_FR7I(IPARI ,INTBUF_TAB,NUM_IMP ,NS_IMP ,NSREM ,
     1                    NBINTC,INTLIST)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE IMP_INTM
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NUM_IMP(*),NS_IMP(*),IPARI(NPARI,*),
     .         NSREM,NBINTC,INTLIST(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,N,NN,IAD,NSN,NIN,NUM,NTY,II,NB
      INTEGER IERROR1,IERROR2,NRTS,NSREM11,NCONT,
     .        IERROR3,IERROR4,IERROR5,IERROR6,IERROR7
      INTEGER DEBUT(NINTER),DEBUTI(NINTER),L1,
     .        IERROR8,IERROR9,IDEB,IDEBI,LOC_PROC
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C----init
        DO II = 1, NBINTC
          NIN = INTLIST(II)
          DEBUTI(NIN) = 0
        ENDDO
        DO I = 1, NSPMD
          IF(I.NE.LOC_PROC)THEN
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              NB = NSNFI(NIN)%P(I)
              NTY  = IPARI(7,NIN)
              IF(NTY.EQ.7.OR.NTY.EQ.10.OR.NTY.EQ.11.OR.NTY.EQ.24) THEN
               IF(NB.GT.0) THEN
                   DEBUTI(NIN) = DEBUTI(NIN) + NB
               END IF
              END IF
            ENDDO
          ENDIF
        ENDDO
C
C
       NSREM11 = 0
       IAD=0
       L = 0
       DO NIN=1,NINTER
        NTY   =IPARI(7,NIN)
        IF(NTY.EQ.5) IAD=IAD+NUM_IMP(NIN)
       ENDDO
       DO NIN=1,NINTER
        NUM=0
        NTY   =IPARI(7,NIN)
        NSN   =IPARI(5,NIN)
        SHF_INT(NIN) = L
        IF(NTY.EQ.7.OR.NTY.EQ.10.OR.NTY.EQ.24) THEN
          DO I=1,NUM_IMP(NIN)
           IF (NS_IMP(I+IAD).GT.NSN) THEN
            NN=NS_IMP(I+IAD)-NSN
C -------tag impact nodes-------
            NSVFI(NIN)%P(NN)=-ABS(NSVFI(NIN)%P(NN))
            NUM=NUM+1
           ENDIF
          ENDDO
          IAD=IAD+NUM_IMP(NIN)
        ELSEIF(NTY.EQ.11)THEN
C
         NRTS   =IPARI(3,NIN)
          DO I=1,NUM_IMP(NIN)
           IF (NS_IMP(I+IAD).GT.NRTS) THEN
            NN=NS_IMP(I+IAD)-NRTS
C -------tag impact nodes-------
            NSVFI(NIN)%P(NN)=-ABS(NSVFI(NIN)%P(NN))
            NUM=NUM+1
            NSREM11 = NSREM11 + 1
           ENDIF
          ENDDO
          IAD=IAD+NUM_IMP(NIN)
C
        ELSE
        ENDIF
        L = L + NUM
        IF(ASSOCIATED(IND_INT(NIN)%P)) DEALLOCATE(IND_INT(NIN)%P)
        L1=DEBUTI(NIN)
        IF (L1.GT.0) ALLOCATE(IND_INT(NIN)%P(L1),STAT=IERROR1)
       ENDDO
C---------
C
C Allocation des tableaux de frontieres interfaces pour implicit
C
C
        NCONT = NSREM-NSREM11
      IF (INTP_D.EQ.0) THEN
        IF(ALLOCATED(STIFS)) DEALLOCATE(STIFS)
        ALLOCATE(STIFS(NCONT),STAT=IERROR3)
        IF(ALLOCATED(H_E)) DEALLOCATE(H_E)
        ALLOCATE(H_E(4,NCONT),STAT=IERROR4)
        IF(ALLOCATED(N_E)) DEALLOCATE(N_E)
        ALLOCATE(N_E(3,NCONT),STAT=IERROR5)
C
      ENDIF
C
        IF(ALLOCATED(FR_SREM)) DEALLOCATE(FR_SREM)
        ALLOCATE(FR_SREM(NSREM),STAT=IERROR2)
        IF(ALLOCATED(DFI)) DEALLOCATE(DFI)
        ALLOCATE(DFI(3,NSREM),STAT=IERROR6)
        IF(ALLOCATED(FFI)) DEALLOCATE(FFI)
        ALLOCATE(FFI(3,NSREM),STAT=IERROR7)
C
      IF(ALLOCATED(IAD_SREM)) DEALLOCATE(IAD_SREM)
      ALLOCATE(IAD_SREM(NSPMD+1),STAT=IERROR8)
      IF(ALLOCATED(INBSL)) DEALLOCATE(INBSL)
      ALLOCATE(INBSL(NBINTC,NSPMD),STAT=IERROR9)
C -----tri par proc---independant interface structure------
        DO II = 1, NBINTC
          NIN = INTLIST(II)
          DEBUT(NIN) = 0
          DEBUTI(NIN) = 0
        ENDDO
       L = 1
C
       IAD_SREM(1) = L
        DO I = 1, NSPMD
          IF(I.NE.LOC_PROC)THEN
            DO II = 1, NBINTC
              NIN = INTLIST(II)
              IDEB = DEBUT(NIN)
              IDEBI = DEBUTI(NIN)
              NB = NSNFI(NIN)%P(I)
              NTY  = IPARI(7,NIN)
              NSN  = IPARI(5,NIN)
              L1 = L
              IF(NTY.EQ.7.OR.NTY.EQ.10.OR.NTY.EQ.11.OR.NTY.EQ.24) THEN
               IF(NB.GT.0) THEN
                IF(NTY.EQ.7.OR.NTY.EQ.10.OR.NTY.EQ.24) THEN
                  DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      FR_SREM(L) = NSVFI(NIN)%P(IDEB+N)
                      IND_INT(NIN)%P(IDEBI+N)=L
                      L = L + 1
                    ENDIF
                  ENDDO
                  DEBUTI(NIN) = DEBUTI(NIN) + NB
C
                ELSEIF(NTY.EQ.11) THEN
                  DO N = 1, NB
                    IF(NSVFI(NIN)%P(IDEB+N).LT.0)THEN
                      NSVFI(NIN)%P(IDEB+N)=-NSVFI(NIN)%P(IDEB+N)
                      FR_SREM(L) = NSVFI(NIN)%P(IDEB+N)
                      IND_INT(NIN)%P(IDEBI+N)=L
                      L = L + 1
                      FR_SREM(L) = 0
                      L = L + 1
                    ENDIF
                  ENDDO
                  DEBUTI(NIN) = DEBUTI(NIN) + NB
C
                ENDIF
                DEBUT(NIN) = DEBUT(NIN) + NB
               END IF
              END IF
              INBSL(II,I)= L - L1
            ENDDO
          ENDIF
          IAD_SREM(I+1)=L
        ENDDO
        IF(IAD_SREM(NSPMD+1).GT.1)NSREM=IAD_SREM(NSPMD+1)-1
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FRSL                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRI                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        SPMD_INIS                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_INISL                    source/mpi/implicit/imp_spmd.F
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE IMP_FRSL(NBINTC,NSREM ,NSL   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NBINTC,NSL,NSREM
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IERROR1,IERROR2,IERROR3,IERROR4,IERROR5,
     .        II,IAD
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      IF(ALLOCATED(IAD_SL)) DEALLOCATE(IAD_SL)
      ALLOCATE(IAD_SL(NSPMD+1),STAT=IERROR1)
      IF (NSREM.EQ.0) THEN
        IF(ALLOCATED(IAD_SREM)) DEALLOCATE(IAD_SREM)
        ALLOCATE(IAD_SREM(NSPMD+1),STAT=IERROR2)
        DO I = 1, NSPMD+1
         IAD_SREM(I)=1
        ENDDO
      ENDIF
C
       CALL SPMD_INIS(IAD_SREM,IAD_SL)
       NSL=IAD_SL(NSPMD+1)-1
       IF (NSREM.EQ.0) THEN
         IF(ALLOCATED(INBSL)) DEALLOCATE(INBSL)
         ALLOCATE(INBSL(NBINTC,NSPMD),STAT=IERROR5)
         DO I = 1, NSPMD
          DO II = 1, NBINTC
           INBSL(II,I)=0
          ENDDO
         ENDDO
       ENDIF
       CALL SPMD_INISL(NBINTC,INBSL)
       IF (NSL.GT.0) THEN
        IF(ALLOCATED(ISL)) DEALLOCATE(ISL)
        ALLOCATE(ISL(NSL),STAT=IERROR3)
C
        IF (INTP_D.EQ.0) THEN
        IF(ALLOCATED(DIAG_S)) DEALLOCATE(DIAG_S)
        ALLOCATE(DIAG_S(3,NSL),STAT=IERROR4)
        IF(ALLOCATED(ISLM)) DEALLOCATE(ISLM)
        ALLOCATE(ISLM(NSL),STAT=IERROR5)
C
        ENDIF
       ENDIF

C
      RETURN
      END
Cqz --------- ----
Chd|====================================================================
Chd|  TAG_INTM                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKI                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TAG_INTM(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT    ,NSV       ,
     2    ILOC      ,N_IMPN    ,NSN         )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NSV(*),
     .        ILOC(*),N_IMPN,NSN
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG
C-----------------------------------------------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG.GT.NSN) THEN
        NE=NE_IMP(I)
        DO J=1,3
         N=IRECT(J,NE)
         IF (ILOC(N).EQ.0) THEN
          N_IMPN=N_IMPN+1
          ILOC(N)=N_IMPN
         ENDIF
        ENDDO
        IF (IRECT(3,NE).NE.IRECT(4,NE)) THEN
         N=IRECT(4,NE)
         IF (ILOC(N).EQ.0) THEN
          N_IMPN=N_IMPN+1
          ILOC(N)=N_IMPN
         ENDIF
        ENDIF
       ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Cqz --------- ----
Chd|====================================================================
Chd|  TAG_INTS                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKI                      source/mpi/implicit/imp_fri.F 
Chd|        KIN_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE TAG_INTS(NSL       ,ILOC      ,N_IMPN     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,ILOC(*),N_IMPN
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG
C-----------------------------------------------
      IF (INTP_D.GT.0) THEN
       DO I = 1, NSL
C--------secnd node--------------
        N = ISL(I)
        IF (ILOC(N).EQ.0) ILOC(N)=I
       ENDDO
      ELSE

       DO I = 1, NSL
C--------secnd node-----
        N = ISL(I)
        IF (ILOC(N).EQ.0) THEN
         N_IMPN=N_IMPN+1
         ILOC(N)=N_IMPN
C
         ISLM(I) = N
        ELSE
         ISLM(I) = 0
C
        ENDIF
       ENDDO
C
      ENDIF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  INI_DDFV                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRFV                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INI_DDFV(IDDL ,IKC  ,NDOF  ,IPARI ,INTBUF_TAB ,
     .                    D    ,DR   ,DD    ,DDR   ,NSL   ,
     .                    IRBE3,LRBE3,IRBE2 ,LRBE2 )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDDL(*),IKC(*),NDOF(*),NSL
      INTEGER  IPARI(NPARI,*),IRBE3(NRBE3L,*),LRBE3(*) ,
     .         IRBE2(NRBE2L,*),LRBE2(*)
C     REAL ou REAL*8
      my_real
     .    D(3,*),DR(3,*),DD(3,*),DDR(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ND,N,J,IAD
      INTEGER M,NSN,JI,L,NNOD,NJ,NL,NI
C-----------------------------------------------
       DO I = 1, NSL
C--------local secnd node-----
        N = ISL(I)
         DO J = 1, MIN(3,NDOF(N))
          ND = IDDL(N)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DD(J,N)=D(J,N)
          ENDIF
         ENDDO
         DO J = 3, NDOF(N)
          ND = IDDL(N)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DDR(J-3,N)=DR(J-3,N)
          ENDIF
         ENDDO
       ENDDO
C
       DO I = 1, NML
C--------local main node-----
        N = IML(I)
         DO J = 1, MIN(3,NDOF(N))
          ND = IDDL(N)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DD(J,N)=D(J,N)
          ENDIF
         ENDDO
         DO J = 3, NDOF(N)
          ND = IDDL(N)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DDR(J-3,N)=DR(J-3,N)
          ENDIF
         ENDDO
       ENDDO
C------Rigid bodies-----
       DO I=1,NRB_FR
        M=IFRSR(1,I)
        DO J=1,MIN(3,NDOF(M))
         ND = IDDL(M)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DD(J,M)=D(J,M)
          ENDIF
        ENDDO
        DO J=3,NDOF(M)
         ND = IDDL(M)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DDR(J-3,M)=DR(J-3,M)
          ENDIF
        ENDDO
       ENDDO
C------int2-------
       DO I=1,NI2_FR
         N=IFRS2(1,I)
         NI=IFRS2(2,I)
         JI=IPARI(1,N)
         NSN=IPARI(5,N)
         L=INTBUF_TAB(N)%IRTLM(NI)
         NL=4*(L-1)
         IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
C-------si noeud main est dependant aussi-----
         DO M=1,NNOD
          NJ=INTBUF_TAB(N)%IRECTM(NL+M)
          DO J = 1 , MIN(3,NDOF(NJ))
               ND = IDDL(NJ) + J
           IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
            DD(J,NJ)=D(J,NJ)
           ENDIF
          ENDDO
          DO J = 3 , NDOF(NJ)
               ND = IDDL(NJ) + J
           IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
            DDR(J-3,NJ)=DR(J-3,NJ)
           ENDIF
          ENDDO
         ENDDO
       ENDDO
C------RBE3-------
       DO I=1,NRBE3_FR
         N=IFRS3(I)
         NNOD=IRBE3(5,N)
         IAD=IRBE3(1,N)
C-------si noeud main est dependant aussi-----
         DO M=1,NNOD
          NJ=LRBE3(IAD+M)
          DO J = 1 , MIN(3,NDOF(NJ))
               ND = IDDL(NJ) + J
           IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
            DD(J,NJ)=D(J,NJ)
           ENDIF
          ENDDO
          DO J = 3 , NDOF(NJ)
               ND = IDDL(NJ) + J
           IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
            DDR(J-3,NJ)=DR(J-3,NJ)
           ENDIF
          ENDDO
         ENDDO
       ENDDO
C------RBE2-------
       DO I=1,NRBE2_FR
         N=IFRS4(1,I)
         M=IRBE2(3,N)
        DO J=1,MIN(3,NDOF(M))
         ND = IDDL(M)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DD(J,M)=D(J,M)
          ENDIF
        ENDDO
        DO J=3,NDOF(M)
         ND = IDDL(M)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DDR(J-3,M)=DR(J-3,M)
          ENDIF
        ENDDO
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  INI_DD0                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRFV                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INI_DD0(IDDL ,IKC  ,NDOF  ,IPARI ,INTBUF_TAB ,
     .                   DD   ,DDR  ,NSL   ,IRBE3 ,LRBE3 ,
     .                   IRBE2,LRBE2 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDDL(*),IKC(*),NDOF(*),NSL
      INTEGER  IPARI(NPARI,*),IRBE3(NRBE3L,*),LRBE3(*),
     .         IRBE2(NRBE2L,*),LRBE2(*)
C     REAL ou REAL*8
      my_real
     .    DD(3,*),DDR(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ND,N,J,IAD,JI
      INTEGER M,NSN,L,NNOD,NJ,NL,NI
C-----------------------------------------------
       DO I = 1, NSL
C--------local secnd node-----
        N = ISL(I)
         DO J = 1, MIN(3,NDOF(N))
          ND = IDDL(N)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DD(J,N)=ZERO
          ENDIF
         ENDDO
         DO J = 3, NDOF(N)
          ND = IDDL(N)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DDR(J-3,N)=ZERO
          ENDIF
         ENDDO
       ENDDO
C
       DO I = 1, NML
C--------local main node-----
        N = IML(I)
         DO J = 1, MIN(3,NDOF(N))
          ND = IDDL(N)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DD(J,N)=ZERO
          ENDIF
         ENDDO
         DO J = 3, NDOF(N)
          ND = IDDL(N)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DDR(J-3,N)=ZERO
          ENDIF
         ENDDO
       ENDDO
C------Rigid bodies-------
       DO I=1,NRB_FR
        M=IFRSR(1,I)
        DO J=1,MIN(3,NDOF(M))
         ND = IDDL(M)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DD(J,M)=ZERO
          ENDIF
        ENDDO
         DO J = 3, NDOF(M)
          ND = IDDL(M)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DDR(J-3,M)=ZERO
          ENDIF
         ENDDO
       ENDDO
C------int2-------
       DO I=1,NI2_FR
         N=IFRS2(1,I)
         NI=IFRS2(2,I)
         JI=IPARI(1,N)
         NSN=IPARI(5,N)
         L=INTBUF_TAB(N)%IRTLM(NI)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3).EQ.INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
C-------si noeud main est dependant aussi-----
         DO M=1,NNOD
          NJ=INTBUF_TAB(N)%IRECTM(NL+M)
          DO J = 1 , MIN(3,NDOF(NJ))
               ND = IDDL(NJ) + J
           IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
            DD(J,NJ)=ZERO
           ENDIF
          ENDDO
          DO J = 3 , NDOF(NJ)
               ND = IDDL(NJ) + J
           IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
            DDR(J-3,NJ)=ZERO
           ENDIF
          ENDDO
         ENDDO
       ENDDO
C------RBE3-------
       DO I=1,NRBE3_FR
         N=IFRS3(I)
         NNOD=IRBE3(5,N)
         IAD=IRBE3(1,N)
C-------si noeud main est dependant aussi-----
         DO M=1,NNOD
          NJ=LRBE3(IAD+M)
          DO J = 1 , MIN(3,NDOF(NJ))
               ND = IDDL(NJ) + J
           IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
            DD(J,NJ)=ZERO
           ENDIF
          ENDDO
          DO J = 3 , NDOF(NJ)
               ND = IDDL(NJ) + J
           IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
            DDR(J-3,NJ)=ZERO
           ENDIF
          ENDDO
         ENDDO
       ENDDO
C------RBE2-------
       DO I=1,NRBE2_FR
        N=IFRS4(1,I)
        M=IRBE2(3,N)
        DO J=1,MIN(3,NDOF(M))
         ND = IDDL(M)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DD(J,M)=ZERO
          ENDIF
        ENDDO
         DO J = 3, NDOF(M)
          ND = IDDL(M)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           DDR(J-3,M)=ZERO
          ENDIF
         ENDDO
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Cqz --------- ----
Chd|====================================================================
Chd|  INI_INTM                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKI                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE INI_INTM(ILOC  ,N_IMPS,N_IMPN )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ILOC(*),N_IMPN,N_IMPS
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,IERROR1
C-----------------------------------------------
C
      NML=N_IMPN-N_IMPS
      IF (NML.GT.0) THEN
       IF(ALLOCATED(IML)) DEALLOCATE(IML)
       ALLOCATE(IML(NML),STAT=IERROR1)
       DO N = 1, NUMNOD
        IF (ILOC(N).GT.N_IMPS) THEN
         I=ILOC(N)-N_IMPS
         IML(I)=N
        ENDIF
       ENDDO
      ENDIF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  DIM_KINEFR                    source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKI                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_KINEFR(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,INLOC     ,
     3    LNS       ,LNS2      ,LNSS      ,LNSS2     ,NK_M      ,
     4    IRBE3     ,LNS3      ,LNSS3     ,IRBE2     ,LRBE2     ,
     5    LNR2      ,LNRS2     )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C      INTEGER NNMAX,NKMAX
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IPARI(NPARI,*)
      INTEGER
     .   INLOC(*),LNS  ,LNS2,LNSS ,LNSS2,NK_M,
     .   IRBE3(NRBE3L,*),LNS3 ,LNSS3 ,IRBE2(NRBE2L,*),LRBE2(*),
     .   LNR2      ,LNRS2
C     REAL ou REAL*8
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .        I,J,K,N,L,NL,NJ,NI,J1,M,NSN,N1,N2,NK,ID,
     .        JI,KFI,NS,IAD
C----------------------------
      LNS2=0
      LNSS2=0
      DO J=1,NINT2
       N=IINT2(J)
       NSN = IPARI(5,N)
       JI=IPARI(1,N)
       DO I=1,NSN
        NI=INTBUF_TAB(N)%NSV(I)
        IF (INLOC(NI).GT.0) THEN
         LNS2=LNS2+1
         IF (INLOC(NI).LE.NK_M) LNSS2=LNSS2+1
        ENDIF
       ENDDO
      ENDDO
C--------RBE3--------------------
      LNS3=0
      LNSS3=0
      DO N=1,NRBE3
       NI = IRBE3(3,N)
       IF (NI.EQ.0) CYCLE
       IF (INLOC(NI).GT.0) THEN
         LNS3=LNS3+1
         IF (INLOC(NI).LE.NK_M) LNSS3=LNSS3+1
       ENDIF
      ENDDO
CC-----active rigid body main nodes------
      LNS=0
      LNSS=0
      DO J=1,NRBYAC
       N=IRBYAC(J)
       K=IRBYAC(J+NRBYKIN)
       M  =NPBY(1,N)
       NSN  =NPBY(2,N)
        DO I=1,NSN
         ID = I+K
         NI=LPBY(ID)
         IF (INLOC(NI).GT.0) THEN
          LNS=LNS+1
          IF (INLOC(NI).LE.NK_M) LNSS=LNSS+1
          IF (INLOC(M).EQ.0) INLOC(M) = 1
         ENDIF
       ENDDO
      ENDDO
C--------RBE2--------------------
      LNR2=0
      LNRS2=0
      DO N=1,NRBE2
       IAD = IRBE2(1,N)
       M  = IRBE2(3,N)
       NSN  =IRBE2(5,N)
        DO I=1,NSN
         ID = IAD+I
         NI=LRBE2(ID)
         IF (INLOC(NI).GT.0) THEN
          LNR2=LNR2+1
          IF (INLOC(NI).LE.NK_M) LNRS2=LNRS2+1
          IF (INLOC(M).EQ.0) INLOC(M) = 2
         ENDIF
       ENDDO
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IND_KINEFR                    source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKI                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IND_KINEFR(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,INLOC     ,
     3    NSS       ,NSS2      ,NSS_S     ,NSS2_S    ,KN_M      ,
     4    IBFV      ,LJ        ,ISKEW     ,ICODT     ,IRBE3     ,
     5    NSS3      ,NSS3_S    ,IRBE2     ,LRBE2     ,NSR2      ,
     6    NRS2_S    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE IMP_RWL
      USE IMP_ASPC
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C      INTEGER NNMAX,NKMAX
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IPARI(NPARI,*),ISKEW(*),ICODT(*)
      INTEGER
     .   INLOC(*),NSS,NSS2,NSS_S ,NSS2_S,KN_M,IBFV(NIFV,*),LJ(*),
     .   IRBE3(NRBE3L,*),NSS3 ,NSS3_S  ,IRBE2(NRBE2L,*),LRBE2(*),
     .   NSR2      ,NRS2_S
C     REAL ou REAL*8
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .        I,J,K,N,L,NL,NJ,NI,J1,M,NSN,N1,N2,NK,ID,IAD,JI,
     .        IERROR1,IERROR2,IERROR3,IERROR4,IERROR5,IERROR6,IERROR7
c----------------------
      NI2_FR=0
      NI2_FRS=0
      IF (NSS2.GT.0) THEN
       IF(ALLOCATED(IFRS2)) DEALLOCATE(IFRS2)
       ALLOCATE(IFRS2(2,NSS2),STAT=IERROR1)
       IF (NSS2_S.GT.0) THEN
        IF(ALLOCATED(IFRS2_S)) DEALLOCATE(IFRS2_S)
        ALLOCATE(IFRS2_S(NSS2_S),STAT=IERROR3)
       ENDIF
       DO J=1,NINT2
        N=IINT2(J)
        NSN = IPARI(5,N)
        JI=IPARI(1,N)
        DO I=1,NSN
         NI=INTBUF_TAB(N)%NSV(I)
         IF (INLOC(NI).GT.0) THEN
          NI2_FR=NI2_FR+1
          IFRS2(1,NI2_FR)=N
          IFRS2(2,NI2_FR)=I
          IF (INLOC(NI).LE.KN_M)THEN
           NI2_FRS=NI2_FRS+1
           IFRS2_S(NI2_FRS)=NI2_FR
          ENDIF
         ENDIF
        ENDDO
       ENDDO
       IF (NI2_FR.NE.NSS2.OR.NI2_FRS.NE.NSS2_S)
     .        WRITE(*,*)'pb cal NI2_FR'
      ENDIF
c-------RBE3---------------
      NRBE3_FR=0
      NRBE3_FRS=0
      IF (NSS3.GT.0) THEN
       IF(ALLOCATED(IFRS3)) DEALLOCATE(IFRS3)
       ALLOCATE(IFRS3(NSS3),STAT=IERROR1)
       IF (NSS3_S.GT.0) THEN
        IF(ALLOCATED(IFRS3_S)) DEALLOCATE(IFRS3_S)
        ALLOCATE(IFRS3_S(NSS3_S),STAT=IERROR3)
       ENDIF
       DO N=1,NRBE3
         NI=IRBE3(3,N)
         IF (NI.EQ.0) CYCLE
         IF (INLOC(NI).GT.0) THEN
          NRBE3_FR=NRBE3_FR+1
          IFRS3(NRBE3_FR)=N
          IF (INLOC(NI).LE.KN_M)THEN
           NRBE3_FRS=NRBE3_FRS+1
           IFRS3_S(NRBE3_FRS)=NRBE3_FR
          ENDIF
         ENDIF
       ENDDO
       IF (NRBE3_FR.NE.NSS3.OR.NRBE3_FRS.NE.NSS3_S)
     .        WRITE(*,*)'pb cal NRBE3_FR'
      ENDIF
C-----active rigid body main nodes------
      NRB_FR=0
      NRB_FRS=0
      IF (NSS.GT.0) THEN
       IF(ALLOCATED(IFRSR)) DEALLOCATE(IFRSR)
       ALLOCATE(IFRSR(2,NSS),STAT=IERROR2)
       IF (NSS_S.GT.0) THEN
        IF(ALLOCATED(IFRSR_S)) DEALLOCATE(IFRSR_S)
        ALLOCATE(IFRSR_S(NSS_S),STAT=IERROR4)
       ENDIF
       DO J=1,NRBYAC
        N=IRBYAC(J)
        K=IRBYAC(J+NRBYKIN)
        M  =NPBY(1,N)
        IF (INLOC(M).GT.0) THEN
         NSN  =NPBY(2,N)
         DO I=1,NSN
          ID = I+K
          NI=LPBY(ID)
          IF (INLOC(NI).GT.0) THEN
           NRB_FR=NRB_FR+1
           IFRSR(1,NRB_FR)=M
           IFRSR(2,NRB_FR)=NI
           IF (INLOC(NI).LE.KN_M) THEN
            NRB_FRS=NRB_FRS+1
            IFRSR_S(NRB_FRS)=NRB_FR
           ENDIF
          ENDIF
         ENDDO
        ENDIF
       ENDDO
      ENDIF
C
      NBC_FR = 0
      DO N=1,NUMNOD
        IF (ISKEW(N).GT.1.AND.ICODT(N).NE.7) THEN
         IF (INLOC(N).GT.0.AND.INLOC(N).LE.KN_M)NBC_FR = NBC_FR + 1
        ENDIF
      ENDDO
      IF (NBC_FR.GT.0) THEN
       IF(ALLOCATED(IBC_FR)) DEALLOCATE(IBC_FR)
       ALLOCATE(IBC_FR(3,NBC_FR),STAT=IERROR5)
       NBC_FR = 0
       DO N=1,NUMNOD
        IF (ISKEW(N).GT.1.AND.ICODT(N).NE.7) THEN
         IF (INLOC(N).GT.0.AND.INLOC(N).LE.KN_M) THEN
           NBC_FR = NBC_FR + 1
           IBC_FR(1,NBC_FR) = N
           IBC_FR(2,NBC_FR) = ISKEW(N)
           IBC_FR(3,NBC_FR) = ICODT(N)
         ENDIF
        ENDIF
       ENDDO
      ENDIF
C-------AUTOSPC
      NSPC_FR = 0
      DO I=1,NSPCL
        N = IN_SPC(I)
        IF (INLOC(N).GT.0.AND.INLOC(N).LE.KN_M.AND.IC_SPC(I).LE.3)
     .    NSPC_FR = NSPC_FR + 1
      ENDDO
      IF (NSPC_FR.GT.0) THEN
       IF(ALLOCATED(ISPC_FR)) DEALLOCATE(ISPC_FR)
       ALLOCATE(ISPC_FR(NSPC_FR),STAT=IERROR5)
       NSPC_FR = 0
       DO I=1,NSPCL
        N = IN_SPC(I)
        IF (INLOC(N).GT.0.AND.INLOC(N).LE.KN_M.AND.IC_SPC(I).LE.3) THEN
             NSPC_FR = NSPC_FR + 1
             ISPC_FR(NSPC_FR)=I
            ENDIF
       ENDDO
      ENDIF
C---
      NFX_FR = 0
      DO J=1,NFXVEL
        IF (LJ(J).GT.0.AND.LJ(J).LE.3) THEN
         N=IABS(IBFV(1,J))
         IF (INLOC(N).GT.0.AND.INLOC(N).LE.KN_M)NFX_FR = NFX_FR + 1
        ENDIF
      ENDDO
      IF (NFX_FR.GT.0) THEN
       IF(ALLOCATED(IFX_FR)) DEALLOCATE(IFX_FR)
       ALLOCATE(IFX_FR(2,NFX_FR),STAT=IERROR6)
       NFX_FR = 0
       DO J=1,NFXVEL
        IF (LJ(J).GT.0.AND.LJ(J).LE.3) THEN
         N=IABS(IBFV(1,J))
         IF (INLOC(N).GT.0.AND.INLOC(N).LE.KN_M) THEN
           NFX_FR = NFX_FR + 1
           IFX_FR(1,NFX_FR) = J
           IFX_FR(2,NFX_FR) = LJ(J)
         ENDIF
        ENDIF
       ENDDO
      ENDIF
C
      NRW_FR = 0
      DO J=1,N_RWL
       N=IN_RWL(J)
       IF (INLOC(N).GT.0) NRW_FR = NRW_FR + 1
      ENDDO
      IF (NRW_FR.GT.0) THEN
       IF(ALLOCATED(IRW_FR)) DEALLOCATE(IRW_FR)
       ALLOCATE(IRW_FR(NRW_FR),STAT=IERROR7)
       NRW_FR = 0
       DO J=1,N_RWL
         N=IN_RWL(J)
         IF (INLOC(N).GT.0) THEN
           NRW_FR = NRW_FR + 1
           IRW_FR(NRW_FR) = J
         ENDIF
       ENDDO
      ENDIF
C
C-----RBE2------
      NRBE2_FR=0
      NRBE2_FRS=0
      IF (NSR2.GT.0) THEN
       IF(ALLOCATED(IFRS4)) DEALLOCATE(IFRS4)
       ALLOCATE(IFRS4(2,NSR2),STAT=IERROR2)
       IF (NRS2_S.GT.0) THEN
        IF(ALLOCATED(IFRS4_S)) DEALLOCATE(IFRS4_S)
        ALLOCATE(IFRS4_S(NRS2_S),STAT=IERROR4)
       ENDIF
       DO N=1,NRBE2
        M  =IRBE2(3,N)
        IF (INLOC(M).GT.0) THEN
         IAD = IRBE2(1,N)
         NSN  =IRBE2(5,N)
         DO I=1,NSN
          ID = IAD+I
          NI=LRBE2(ID)
          IF (INLOC(NI).GT.0) THEN
           NRBE2_FR=NRBE2_FR+1
           IFRS4(1,NRBE2_FR)=N
           IFRS4(2,NRBE2_FR)=NI
           IF (INLOC(NI).LE.KN_M) THEN
            NRBE2_FRS=NRBE2_FRS+1
            IFRS4_S(NRBE2_FRS)=NRBE2_FR
           ENDIF
          ENDIF
         ENDDO
        ENDIF
       ENDDO
      ENDIF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IDDL_INT                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKI                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IDDL_INT(NSL    ,IDDL    ,IKC     ,NDOF     ,IDDLM   ,
     .                    IPARI  ,INTBUF_TAB,IRBE3   ,LRBE3    ,FRBE3   ,
     .                     X     ,SKEW    ,IRBE2   ,LRBE2    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NSL,IDDL(*) ,IKC(*) ,NDOF(*) ,IDDLM(*)
      INTEGER  IPARI(NPARI,*),IRBE3(NRBE3L,*),LRBE3(*),
     .         IRBE2(NRBE2L,*),LRBE2(*)
      my_real
     .  FRBE3(*),X(*),SKEW(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID,N,J,NDD,I1,IAD,NMT,IROTG,IADS
      INTEGER M,NSN,L,NNOD,NJ,NL,NI,JI
      INTEGER IERROR1,IERROR2,IERROR3,IERROR4,IERROR5
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
       IF (NML.GT.0) THEN
        IF(ALLOCATED(IDDML)) DEALLOCATE(IDDML)
        ALLOCATE(IDDML(3,NML),STAT=IERROR1)
C--------initialization for ndof=0---
        IDDML = -7
        DO I = 1, NML
         N = IML(I)
         NDD = 0
         DO J = 1 , MIN(3,NDOF(N))
              ID = IDDL(N) + J
          IF (IKC(ID).LT.1) THEN
           NDD = NDD + 1
           IDDML(J,I) = IDDLM(N) + NDD
          ELSE
           IDDML(J,I) = -IKC(ID)
          ENDIF
         ENDDO
        ENDDO
       ENDIF
C
       IF (NSL.GT.0) THEN
        IF(ALLOCATED(IDDSL)) DEALLOCATE(IDDSL)
        ALLOCATE(IDDSL(3,NSL),STAT=IERROR2)
        IDDSL = -7
        DO I = 1, NSL
         N = ISL(I)
         NDD = 0
         DO J = 1 , MIN(3,NDOF(N))
              ID = IDDL(N) + J
          IF (IKC(ID).LT.1) THEN
           NDD = NDD + 1
           IDDSL(J,I) = IDDLM(N) + NDD
          ELSE
           IDDSL(J,I) = -IKC(ID)
          ENDIF
         ENDDO
        ENDDO
       ENDIF
C
       IF (NRB_FR.GT.0) THEN
        IF(ALLOCATED(IDDMR)) DEALLOCATE(IDDMR)
        ALLOCATE(IDDMR(6,NRB_FR),STAT=IERROR3)
        DO I = 1, NRB_FR
         N = IFRSR(1,I)
         NDD = 0
         DO J = 1 , NDOF(N)
              ID = IDDL(N) + J
          IF (IKC(ID).LT.1) THEN
           NDD = NDD + 1
           IDDMR(J,I) = IDDLM(N) + NDD
          ELSE
           IDDMR(J,I) = -IKC(ID)
          ENDIF
         ENDDO
        ENDDO
       ENDIF
C
       IF (NI2_FR.GT.0) THEN
        IF(ALLOCATED(IDDMI2)) DEALLOCATE(IDDMI2)
        ALLOCATE(IDDMI2(6,4,NI2_FR),STAT=IERROR4)
        DO I=1,NI2_FR
         N=IFRS2(1,I)
         NI=IFRS2(2,I)
         JI=IPARI(1,N)
         NSN=IPARI(5,N)
         L=INTBUF_TAB(N)%IRTLM(NI)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3).EQ.INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
C-------si noeud main est dependant aussi-----
         DO M=1,NNOD
          NJ=INTBUF_TAB(N)%IRECTM(NL+M)
          NDD = 0
          DO J = 1 , NDOF(NJ)
               ID = IDDL(NJ) + J
           IF (IKC(ID).LT.1) THEN
            NDD = NDD + 1
            IDDMI2(J,M,I) = IDDLM(NJ) + NDD
           ELSE
            IDDMI2(J,M,I) = -IKC(ID)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDIF
C-------RBE3-----------
       IF (NRBE3_FR.GT.0) THEN
        IF(ALLOCATED(IDDMI3)) DEALLOCATE(IDDMI3)
        NDD = 0
        DO I=1,NRBE3_FR
         N=IFRS3(I)
         NDD = MAX(NDD ,IRBE3(5,N))
        ENDDO
        ALLOCATE(IDDMI3(6,NDD,NRBE3_FR),STAT=IERROR5)
        IROTG=0
        NMT = 0
        DO I=1,NRBE3_FR
         N=IFRS3(I)
         NI=IRBE3(3,N)
         NNOD=IRBE3(5,N)
         IAD=IRBE3(1,N)
             IROTG =MAX(IROTG,IRBE3(6,N))
C-------
         DO M=1,NNOD
          NJ=LRBE3(IAD+M)
          NDD = 0
          DO J = 1 , NDOF(NJ)
               ID = IDDL(NJ) + J
           IF (IKC(ID).LT.1) THEN
            NDD = NDD + 1
            IDDMI3(J,M,I) = IDDLM(NJ) + NDD
           ELSE
            IDDMI3(J,M,I) = -IKC(ID)
           ENDIF
          ENDDO
         ENDDO
         NMT = NMT + NNOD
        ENDDO
        IF(ALLOCATED(FRCDI)) DEALLOCATE(FRCDI)
        ALLOCATE(FRCDI(18*NMT))
        IF (IROTG.GT.0) THEN
         IF(ALLOCATED(MRCDI)) DEALLOCATE(MRCDI)
         ALLOCATE(MRCDI(18*NMT))
        ENDIF
C------- init FRCDI,MRCDI
        NMT = SLRBE3/2
        IADS =1
        DO I=1,NRBE3_FR
         N=IFRS3(I)
         NI=IRBE3(3,N)
         NNOD=IRBE3(5,N)
         IAD=IRBE3(1,N)
             IROTG =IRBE3(6,N)
         CALL RBE3CL(LRBE3(IAD+1),LRBE3(NMT+IAD+1),NI     ,X    ,
     .              FRBE3(6*IAD+1),SKEW   ,NNOD    ,IROTG  ,FRCDI(IADS),
     .              MRCDI(IADS) ,IRBE3(2,N))
C-------
         IADS = IADS + NNOD
        ENDDO
       ENDIF
C-----------Rbe2-------------------
       IF (NRBE2_FR.GT.0) THEN
        IF(ALLOCATED(IDDMR2)) DEALLOCATE(IDDMR2)
        ALLOCATE(IDDMR2(6,NRBE2_FR),STAT=IERROR3)
        DO I = 1, NRBE2_FR
         N = IFRS4(1,I)
         M = IRBE2(3,N)
         NDD = 0
         DO J = 1 , NDOF(M)
              ID = IDDL(M) + J
          IF (IKC(ID).LT.1) THEN
           NDD = NDD + 1
           IDDMR2(J,I) = IDDLM(M) + NDD
          ELSE
           IDDMR2(J,I) = -IKC(ID)
          ENDIF
         ENDDO
        ENDDO
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKI                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        BCL_IMPKD                     source/constraints/general/bcs/bc_imp0.F
Chd|        FV_UPDKD                      source/constraints/general/impvel/fv_imp0.F
Chd|        FV_UPDKD2                     source/constraints/general/bcs/bc_imp0.F
Chd|        I2_FRUP0                      source/interfaces/interf/i2_imp1.F
Chd|        I2_FRUP1                      source/interfaces/interf/i2_imp1.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        PRERBE2FR                     source/constraints/general/rbe2/rbe2f.F
Chd|        PRERBE3FR                     source/constraints/general/rbe3/rbe3f.F
Chd|        RBE2_IMPKD                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_FRUPD                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        UPDFR_RB                      source/constraints/general/rbody/rby_imp0.F
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIAG_INT(NSL    ,NDOF    ,IPARI  ,INTBUF_TAB,
     .                    KSS    ,X       ,IBFV   ,SKEW    ,XFRAME,
     .                    IRBE3  ,LRBE3   ,IRBE2  ,LRBE2   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE IMP_RWL
      USE IMP_ASPC
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER   NSL  , NDOF(*) ,IBFV(NIFV,*)
      INTEGER  IPARI(NPARI,*),IRBE3(NRBE3L,*),LRBE3(*),
     .         IRBE2(NRBE2L,*),LRBE2(*)
      my_real
     .  KSS(6,*),X(3,*),SKEW(LSKEW,*),XFRAME(NXFRAME,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID,N,J,NDD,I1,IS,NS,ILEV,JT(3),JR(3)
      INTEGER M,NSN,JI,L,NNOD,NJ,NL,NI,IROT,IRAD
      INTEGER IERROR1,IERROR2,J1,ISK,IFM,K1,K2,K3,ICT,NN,IAD,IADS,K
      my_real
     .  XS,YS,ZS,KII(3,3),KJJ(6),EJ(3),S,KDD(6,6),KMM(6)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      IF (NRB_FRS.GT.0) THEN
       IF(ALLOCATED(DIAG_MR)) DEALLOCATE(DIAG_MR)
       ALLOCATE(DIAG_MR(6,NRB_FRS),STAT=IERROR1)
       DO I1 = 1, NRB_FRS
        DO J = 1, 6
          DIAG_MR(J,I1)=ZERO
        ENDDO
       ENDDO
      ENDIF
      IF (NI2_FRS.GT.0) THEN
       IF(ALLOCATED(DIAG_M2)) DEALLOCATE(DIAG_M2)
       ALLOCATE(DIAG_M2(6,4,NI2_FRS),STAT=IERROR2)
       DO I1=1,NI2_FRS
        DO J = 1, 6
        DO M = 1, 4
         DIAG_M2(J,M,I1)=ZERO
        ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF (NRBE3_FRS.GT.0) THEN
C------RBE3---
        IADS=0
       DO I1=1,NRBE3_FRS
         I = IFRS3_S(I1)
         N=IFRS3(I)
         IADS=MAX(IADS,IRBE3(5,N))
        ENDDO
       IF(ALLOCATED(DIAG_M3)) DEALLOCATE(DIAG_M3)
       ALLOCATE(DIAG_M3(6,IADS,NRBE3_FRS),STAT=IERROR1)
         DIAG_M3=ZERO
      ENDIF
C-------------RBE2----------------------
      IF (NRBE2_FRS.GT.0) THEN
       IF(ALLOCATED(DIAG_MR2)) DEALLOCATE(DIAG_MR2)
       ALLOCATE(DIAG_MR2(6,NRBE2_FRS),STAT=IERROR1)
       DIAG_MR2=ZERO
      ENDIF
C--------local secnd node-,d'abord independant----
       DO I = 1, NSL
        N = ISL(I)
        DO J = 1, MIN(3,NDOF(N))
         ID = IDDSL(J,I)
         IF (ID.GT.0) DIAG_S(J,I)=KSS(J,I)
        ENDDO
       ENDDO
C
        DO I1=1,NI2_FRS
         I = IFRS2_S(I1)
         N=IFRS2(1,I)
         NI=IFRS2(2,I)
         JI=IPARI(1,N)
         NSN=IPARI(5,N)
         NS=INTBUF_TAB(N)%NSV(NI)
         DO IS = 1, NSL
         IF (ISL(IS).EQ.NS) THEN
          ILEV  =IPARI(20,N)
          IF (ILEV.EQ.1) THEN
           CALL I2_FRUP1(X   ,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%DPARA ,INTBUF_TAB(N)%NSV ,
     1               INTBUF_TAB(N)%IRTLM ,NS  ,KSS(1,IS),DIAG_M2(1,1,I1))
          ELSE
           CALL I2_FRUP0(X   ,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%CSTS ,INTBUF_TAB(N)%NSV ,
     1              INTBUF_TAB(N)%IRTLM,NS,NDOF,KSS(1,IS),DIAG_M2(1,1,I1))
          ENDIF
         ENDIF
         ENDDO
        ENDDO
C------------RBE2--------------
        DO I1 = 1, NRBE2_FRS
         I = IFRS4_S(I1)
         N = IFRS4(1,I)
         M = IRBE2(3,N)
         NS = IFRS4(2,I)
         ICT = IRBE2(4,N)
         ISK = IRBE2(7,N)
             IRAD =IRBE2(11,N)
C--------remove ICR---
         ICT =(ICT/512)*512
         CALL PRERBE2FR(ICT    ,JT  ,JR   )
         DO K=1,6
          DO J=1,6
           KDD(K,J) = ZERO
          ENDDO
              KMM(K)=  ZERO
         ENDDO
         DO IS = 1, NSL
         IF (ISL(IS).EQ.NS) THEN
          DO J=1,3
           KDD(J,J) = KSS(J,IS)
          ENDDO
          KDD(1,2)=KSS(4,IS)
          KDD(1,3)=KSS(5,IS)
          KDD(2,3)=KSS(6,IS)
          KDD(2,1)=KDD(1,2)
          KDD(3,1)=KDD(1,3)
          KDD(3,2)=KDD(2,3)
          CALL RBE2_IMPKD(M     ,NS    ,X     ,ISK     ,JT     ,
     2                  JR     ,NDOF  ,SKEW(1,ISK),KDD    ,KMM ,
     3                  DIAG_S(1,IS),IRAD  )
          DO J = 1 , NDOF(M)
           DIAG_MR2(J,I1) = DIAG_MR2(J,I1) + KMM(J)
          END DO
         END IF
         END DO
        END DO
C------RBE3---
        IADS=1
        DO I1=1,NRBE3_FRS
         I = IFRS3_S(I1)
         N=IFRS3(I)
         NI=IRBE3(3,N)
         NNOD=IRBE3(5,N)
         IROT=IRBE3(6,N)
         IAD=IRBE3(1,N)
         DO IS = 1, NSL
         IF (ISL(IS).EQ.NI) THEN
C-------
           CALL PRERBE3FR(IRBE3 ,N    ,JT  ,JR   )
           CALL RBE3_FRUPD(NNOD   ,LRBE3(IAD+1) ,FRCDI(IADS),
     1                     MRCDI(IADS),NDOF  ,JT   ,IROT  ,
     2                     KSS(1,IS),DIAG_M3(1,1,I1))
         ENDIF
         ENDDO
         IADS=IADS+NNOD
        ENDDO
C------RBODY---
        DO I1 = 1, NRB_FRS
         I = IFRSR_S(I1)
         M = IFRSR(1,I)
         N = IFRSR(2,I)
         DO IS = 1, NSL
         IF (ISL(IS).EQ.N) THEN
          XS=X(1,N)-X(1,M)
          YS=X(2,N)-X(2,M)
          ZS=X(3,N)-X(3,M)
          CALL UPDFR_RB(XS,YS,ZS,KSS(1,IS),DIAG_MR(1,I1))
         ENDIF
         ENDDO
        ENDDO
      DO I1 = 1,NBC_FR
        N = IBC_FR(1,I1)
        ISK= IBC_FR(2,I1)
        ICT= IBC_FR(3,I1)
         DO IS = 1, NSL
         IF (ISL(IS).EQ.N) THEN
          KII(1,1)=KSS(1,IS)
          KII(2,2)=KSS(2,IS)
          KII(3,3)=KSS(3,IS)
          KII(1,2)=KSS(4,IS)
          KII(1,3)=KSS(5,IS)
          KII(2,3)=KSS(6,IS)
          KII(2,1)=KII(1,2)
          KII(3,1)=KII(1,3)
          KII(3,2)=KII(2,3)
          CALL BCL_IMPKD(ICT  ,ISK   ,SKEW  ,KII   ,DIAG_S(1,IS))
         ENDIF
         ENDDO
      ENDDO
C-------AUTOSPC
      DO I1=1,NSPC_FR
        N = ISPC_FR(I1)
        I = IN_SPC(N)
            IAD = 6*(N-1)+1
            NN = IC_SPC(N)
         DO IS = 1, NSL
         IF (ISL(IS).EQ.I) THEN
          KII(1,1)=KSS(1,IS)
          KII(2,2)=KSS(2,IS)
          KII(3,3)=KSS(3,IS)
          KII(1,2)=KSS(4,IS)
          KII(1,3)=KSS(5,IS)
          KII(2,3)=KSS(6,IS)
          KII(2,1)=KII(1,2)
          KII(3,1)=KII(1,3)
          KII(3,2)=KII(2,3)
          IF (NN.EQ.1) THEN
           EJ(1)=SKEW_SPC(IAD)
           EJ(2)=SKEW_SPC(IAD+1)
           EJ(3)=SKEW_SPC(IAD+2)
           CALL L_DIR(EJ,J)
           CALL FV_UPDKD(EJ    ,J    ,KII   ,DIAG_S(1,IS))
          ELSEIF (NN.EQ.2) THEN
           CALL FV_UPDKD2(SKEW_SPC(IAD),SKEW_SPC(IAD+3),KII,
     .                    DIAG_S(1,IS))
          END IF
         ENDIF
         ENDDO
      ENDDO
      DO I1 = 1,NFX_FR
        N = IFX_FR(1,I1)
        J1= IFX_FR(2,I1)
          I=IABS(IBFV(1,N))
          ISK=IBFV(2,N)/10
          IFM = IBFV(9,N)
          J=IBFV(2,N)
          IF (IFM.LE.1) J=J-10*ISK
          K1=3*J-2
          K2=3*J-1
          K3=3*J
            IF (ISK.GT.1) THEN
              EJ(1)=SKEW(K1,ISK)
              EJ(2)=SKEW(K2,ISK)
              EJ(3)=SKEW(K3,ISK)
            ELSE
              EJ(1)=XFRAME(K1,IFM)
              EJ(2)=XFRAME(K2,IFM)
              EJ(3)=XFRAME(K3,IFM)
            ENDIF
         DO IS = 1, NSL
         IF (ISL(IS).EQ.I) THEN
          KII(1,1)=KSS(1,IS)
          KII(2,2)=KSS(2,IS)
          KII(3,3)=KSS(3,IS)
          KII(1,2)=KSS(4,IS)
          KII(1,3)=KSS(5,IS)
          KII(2,3)=KSS(6,IS)
          KII(2,1)=KII(1,2)
          KII(3,1)=KII(1,3)
          KII(3,2)=KII(2,3)
          S = ONE/EJ(J1)
          DO NN =1,3
           EJ(NN) = EJ(NN)*S
          ENDDO
          CALL FV_UPDKD(EJ    ,J1    ,KII   ,DIAG_S(1,IS))
         ENDIF
         ENDDO
      ENDDO
      DO I1 = 1,NRW_FR
        I = IRW_FR(I1)
        N = IN_RWL(I)
         DO IS = 1, NSL
         IF (ISL(IS).EQ.N) THEN
          KII(1,1)=KSS(1,IS)
          KII(2,2)=KSS(2,IS)
          KII(3,3)=KSS(3,IS)
          KII(1,2)=KSS(4,IS)
          KII(1,3)=KSS(5,IS)
          KII(2,3)=KSS(6,IS)
          KII(2,1)=KII(1,2)
          KII(3,1)=KII(1,3)
          KII(3,2)=KII(2,3)
          EJ(1)=NOR_RWL(1,I)
          EJ(2)=NOR_RWL(2,I)
          EJ(3)=NOR_RWL(3,I)
          CALL L_DIR(EJ,J1)
          CALL FV_UPDKD(EJ    ,J1    ,KII   ,DIAG_S(1,IS))
         ENDIF
         ENDDO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  INTABFR                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        GET_IAD                       source/mpi/implicit/imp_fri.F 
Chd|        IMP_FVKM                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FVKSS                     source/mpi/implicit/imp_fri.F 
Chd|        INI_FRUD                      source/mpi/implicit/imp_fri.F 
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE INTABFR(NIC,IC,N,INTAB)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,NIC,IC(*),INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C----6-----IC est deja en ordre croissante---------------------------7---------8
      INTAB=0
       DO I =1,NIC
        IF (N.EQ.IC(I)) THEN
         INTAB=I
         RETURN
        ENDIF
       ENDDO
C
      RETURN
      END
Cqz --------- ----
Chd|====================================================================
Chd|  IMP_DIAGS                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_DIAGS(DIAG_K,NDOF,NSL,IPARI,INTBUF_TAB,IRBE3,LRBE3,
     .                     IRBE2 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDOF(*),NSL
      INTEGER IPARI(NPARI,*),IRBE3(NRBE3L,*),LRBE3(*),
     .         IRBE2(NRBE2L,*)
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,ID,I1,IAD
      INTEGER M,NSN,L,NNOD,NJ,NL,NI,JI
C-----------------------------------------------
      IF (INTP_D.GT.0) THEN
       DO I = 1, NDDL_SL
        ID = IDDL_SL(I)
        DIAG_K(ID)=DIAG_K(ID)+DIAG_SL(I)
       ENDDO
      ELSE
       DO I = 1, NSL
C--------local secnd node-----
        N = ISL(I)
         DO J = 1, MIN(3,NDOF(N))
          ID = IDDSL(J,I)
          IF (ID.GT.0) DIAG_K(ID)=DIAG_K(ID)+DIAG_S(J,I)
         ENDDO
       ENDDO
C--------si il y a des noeuds independant-----
       DO I1=1,NI2_FRS
         I=IFRS2_S(I1)
         N=IFRS2(1,I)
         NI=IFRS2(2,I)
         JI=IPARI(1,N)
         NSN=IPARI(5,N)
         L=INTBUF_TAB(N)%IRTLM(NI)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3).EQ.INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
         DO M=1,NNOD
          NJ=INTBUF_TAB(N)%IRECTM(NL+M)
          DO J = 1 , NDOF(NJ)
               ID = IDDMI2(J,M,I)
           IF (ID.GT.0) DIAG_K(ID)=DIAG_K(ID)+DIAG_M2(J,M,I1)
          ENDDO
         ENDDO
       ENDDO
C--------RBE3-----
       DO I1=1,NRBE3_FRS
         I=IFRS3_S(I1)
         N=IFRS3(I)
         IAD=IRBE3(1,N)
         NNOD=IRBE3(5,N)
         DO M=1,NNOD
          NJ=LRBE3(IAD+M)
          DO J = 1 , NDOF(NJ)
               ID = IDDMI3(J,M,I)
           IF (ID.GT.0) DIAG_K(ID)=DIAG_K(ID)+DIAG_M3(J,M,I1)
          ENDDO
         ENDDO
       ENDDO
       DO I1 = 1, NRB_FRS
         I=IFRSR_S(I1)
         N = IFRSR(1,I)
         DO J = 1 , NDOF(N)
              ID = IDDMR(J,I)
          IF (ID.GT.0) DIAG_K(ID)=DIAG_K(ID)+DIAG_MR(J,I1)
         ENDDO
       ENDDO
C--------RBE2-----
       DO I1 = 1, NRBE2_FRS
         I=IFRS4_S(I1)
         N = IFRS4(1,I)
         M = IRBE2(3,N)
         DO J = 1 , NDOF(M)
              ID = IDDMR2(J,I)
          IF (ID.GT.0) DIAG_K(ID)=DIAG_K(ID)+DIAG_MR2(J,I1)
         ENDDO
       ENDDO
C
      ENDIF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Cqz --------- ----
Chd|====================================================================
Chd|  IMP_DIAGSN                    source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_DIAGSN(DIAG_K,NDOF,NSL,IPARI,INTBUF_TAB,IRBE3,LRBE3,
     .                      IRBE2 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDOF(*),NSL
      INTEGER IPARI(NPARI,*),IRBE3(NRBE3L,*),LRBE3(*),
     .         IRBE2(NRBE2L,*)
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,ID,I1,IAD
      INTEGER M,NSN,L,NNOD,NJ,NL,NI,JI
C-----------------------------------------------
      IF (INTP_D.GT.0) THEN
       DO I = 1, NDDL_SL
        ID = IDDL_SL(I)
        DIAG_K(ID)=DIAG_K(ID)-DIAG_SL(I)
       ENDDO
      ELSE
       DO I = 1, NSL
C--------local secnd node-----
        N = ISL(I)
         DO J = 1, MIN(3,NDOF(N))
          ID = IDDSL(J,I)
          IF (ID.GT.0) DIAG_K(ID)=DIAG_K(ID)-DIAG_S(J,I)
         ENDDO
       ENDDO
C--------si il y a des noeuds independant-----
       DO I1=1,NI2_FRS
         I=IFRS2_S(I1)
         N=IFRS2(1,I)
         NI=IFRS2(2,I)
         JI=IPARI(1,N)
         NSN=IPARI(5,N)
         L=INTBUF_TAB(N)%IRTLM(NI)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3).EQ.INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
         DO M=1,NNOD
          NJ=INTBUF_TAB(N)%IRECTM(NL+M)
          DO J = 1 , NDOF(NJ)
           ID = IDDMI2(J,M,I)
           IF (ID.GT.0) DIAG_K(ID)=DIAG_K(ID)-DIAG_M2(J,M,I1)
          ENDDO
         ENDDO
       ENDDO
C--------RBE3-----
       DO I1=1,NRBE3_FRS
         I=IFRS3_S(I1)
         N=IFRS3(I)
         IAD=IRBE3(1,N)
         NNOD=IRBE3(5,N)
         DO M=1,NNOD
          NJ=LRBE3(IAD+M)
          DO J = 1 , NDOF(NJ)
               ID = IDDMI3(J,M,I)
           IF (ID.GT.0) DIAG_K(ID)=DIAG_K(ID)-DIAG_M3(J,M,I1)
          ENDDO
         ENDDO
       ENDDO
       DO I1 = 1, NRB_FRS
         I=IFRSR_S(I1)
         N = IFRSR(1,I)
         DO J = 1 , NDOF(N)
              ID = IDDMR(J,I)
          IF (ID.GT.0) DIAG_K(ID)=DIAG_K(ID)-DIAG_MR(J,I1)
         ENDDO
       ENDDO
C--------RBE2-----
       DO I1 = 1, NRBE2_FRS
         I=IFRS4_S(I1)
         N = IFRS4(1,I)
         M = IRBE2(3,N)
         DO J = 1 , NDOF(M)
              ID = IDDMR2(J,I)
          IF (ID.GT.0) DIAG_K(ID)=DIAG_K(ID)-DIAG_MR2(J,I1)
         ENDDO
       ENDDO
C
      ENDIF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRFV                      source/mpi/implicit/imp_fri.F 
Chd|        MAV_LTH                       source/implicit/produt_v.F    
Chd|        MAV_LTH0                      source/implicit/produt_v.F    
Chd|        MAV_LTP                       source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        FR_A2B                        source/mpi/implicit/imp_fri.F 
Chd|        FR_A2BD                       source/mpi/implicit/imp_fri.F 
Chd|        FR_U2D                        source/mpi/implicit/imp_fri.F 
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        I7MAINFR                      source/interfaces/int07/i7ke3.F
Chd|        MAV_LTFR                      source/mpi/implicit/imp_fri.F 
Chd|        SPMD_IFCD                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_IFCF                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_IFRF                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_IFRU                     source/mpi/implicit/imp_spmd.F
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE FR_MATV(  A     ,V     ,D      ,MS     ,X      ,
     1                     DR    ,AR    ,IPARI  ,INTBUF_TAB     ,
     2                     NDOF  ,NUM_IMP,NS_IMP,NE_IMP,LX      ,
     3                     NSREM ,NSL    ,IBFV  ,SKEW   ,XFRAME ,
     4                     F     ,IRBE3  ,LRBE3 ,IRBE2  ,LRBE2 )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), NDOF(*),IBFV(*) ,
     .         NUM_IMP(*),NS_IMP(*),NE_IMP(*),NSREM,NSL,
     .         IRBE3(*) ,LRBE3(*),IRBE2(*),LRBE2(*)
      my_real
     .  A(3,*),AR(3,*),V(3,*),D(3,*),DR(3,*),X(3,*) ,
     .  MS(*),SKEW(*) ,XFRAME(*)
      my_real
     .  LX(*),F(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDF
C-----------------------------------------------
      IF ((NSREM+NSL).EQ.0) RETURN
C
      IF (INTP_C.GT.0) THEN
C-----renvoie U(NDDL_SL) et receive U(NDDL_SI)-----
       CALL SPMD_IFRU(LX )
       CALL MAV_LTFR(LX  ,F   )
C-----renvoie F (NDDL_SI) et receive F (NDDL_SL)-----
       CALL SPMD_IFRF(F )
      ELSE
C
C-----initialise D,A noeuds independant d'abord-----
      CALL FR_U2D(NDOF  ,LX    ,D     ,A     ,NSREM, NSL )
C-----noeuds kin -----
      CALL FR_U2DD(D    ,DR    ,X     ,IPARI  ,INTBUF_TAB,
     1             NDOF  ,A     ,AR     ,LX     ,
     2             IBFV ,SKEW  ,XFRAME,IRBE3,LRBE3    ,
     3             IRBE2,LRBE2 )
C-----renvoie D (NSL) et receive DFI (NSREM)-----
      CALL SPMD_IFCD(D  ,NSL, NSREM)
C-----calcul  force de contact ->A ,FFI-----
      IF (NSREM.GT.0)
     . CALL I7MAINFR(A     ,V     ,D      ,X      ,MS    ,
     1               IPARI ,INTBUF_TAB  ,NUM_IMP,NS_IMP,
     2               NE_IMP)
C-----renvoie FFI (NSREM) et receive A (NSL)-----
      CALL SPMD_IFCF(A, NSREM  ,NSL)
C-----Calcul A avec noeuds dependant---------
      CALL UPD_FR(A     ,AR     ,X     ,IPARI  ,INTBUF_TAB,
     1            NDOF   ,IBFV  ,SKEW   ,XFRAME ,
     2            IRBE3 ,LRBE3  ,IRBE2 ,LRBE2  )
C
       CALL FR_A2B(NDOF   ,F     ,A      ,NSL  )
       CALL FR_A2BD(NDOF  ,IPARI ,INTBUF_TAB,F     ,A     ,
     .              AR    ,IRBE3 ,LRBE3  ,IRBE2 ,LRBE2 )
      END IF !(INTP_C.GT.0) THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  FR_MATV_GPU                   source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MAV_LTFR_GPU                  source/mpi/implicit/imp_fri.F 
Chd|        SPMD_IFRF_GPU                 source/mpi/implicit/imp_spmd.F
Chd|        SPMD_IFRU_GPU                 source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE FR_MATV_GPU(NSREM, NSL, LX ,F ,NINDEX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSREM, NSL, NINDEX(*) 
      my_real
     .        LX(*), F(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      IF ((NSREM+NSL).EQ.0) RETURN
C
      IF (INTP_C.GT.0) THEN
C-----renvoie U(NDDL_SL) et receive U(NDDL_SI)-----
       CALL SPMD_IFRU_GPU(LX,NINDEX)
       CALL MAV_LTFR_GPU(LX  ,F, NINDEX)
C-----renvoie F (NDDL_SI) et receive F (NDDL_SL)-----
       CALL SPMD_IFRF_GPU(F,NINDEX)
      ELSE
        PRINT*,'**ERROR OPTION NOT COMPATIBLE WITH GPU'
        STOP 1234
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  FR_U2D                        source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE FR_U2D(NDOF  ,LX    ,D     ,A    ,NSREM ,NSL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDOF(*),NSL,NSREM
      my_real
     .  D(3,*),A(3,*),LX(*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ID,ND,M,N
C----noeuds independant d'abord-----
      DO I = 1,NSREM
       FFI(1,I)=ZERO
       FFI(2,I)=ZERO
       FFI(3,I)=ZERO
      ENDDO
      DO I = 1,NSL
       N=ISL(I)
        DO J=1,MIN(3,NDOF(N))
         ND = IDDSL(J,I)
         IF (ND.GT.0) D(J,N)=LX(ND)
C--------pour com avec FFI----
         A(J,N)=ZERO
        ENDDO
      ENDDO
      DO I = 1,NML
       N=IML(I)
        DO J=1,MIN(3,NDOF(N))
         ND = IDDML(J,I)
         IF (ND.GT.0) D(J,N)=LX(ND)
         A(J,N)=ZERO
        ENDDO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        BCL_IMPD                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPD2D                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDD                       source/constraints/general/bcs/bc_imp0.F
Chd|        FV_IMPD                       source/constraints/general/impvel/fv_imp0.F
Chd|        I2_FRRD0                      source/interfaces/interf/i2_imp2.F
Chd|        I2_FRRD1                      source/interfaces/interf/i2_imp2.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        PRERBE2FR                     source/constraints/general/rbe2/rbe2f.F
Chd|        PRERBE3FR                     source/constraints/general/rbe3/rbe3f.F
Chd|        RBE2_FRD                      source/constraints/general/rbe2/rbe2v.F
Chd|        RBE3_FRD                      source/constraints/general/rbe3/rbe3v.F
Chd|        RBY_IMP3                      source/constraints/general/rbody/rby_impd.F
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE FR_U2DD( D     ,DR    ,X     ,IPARI  ,INTBUF_TAB,
     .                    NDOF  ,A     ,AR     ,LX     ,
     .                    IBFV  ,SKEW  ,XFRAME,IRBE3  ,LRBE3  ,
     .                    IRBE2 ,LRBE2 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE IMP_RWL
      USE IMP_ASPC
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),IRBE2(NRBE2L,*),LRBE2(*),
     .        IPARI(NPARI,*), NDOF(*),IBFV(NIFV,*)
      my_real
     .  D(3,*),DR(3,*),A(3,*),AR(3,*),X(3,*) ,LX(*),
     .  SKEW(LSKEW,*)  ,XFRAME(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,M,NS,NI,NSN,ILEV,IADS,IROT,IAD,JI,
     .        L,NNOD,NJ,ND,NL,ISK,IFM,LJFR(NFXVEL),
     .        JT(3),JR(3),IRAD,NN,IC
      my_real
     .  EJ(3)
       DO L=NBC_FR,1,-1
          I = IBC_FR(1,L)
          ISK =IBC_FR(2,L)
          IFM =IBC_FR(3,L)
          CALL BCL_IMPD(IFM  ,ISK   ,SKEW  ,I     ,D     )
          A(1,I)=ZERO
          A(2,I)=ZERO
          A(3,I)=ZERO
       ENDDO
       DO L=NSPC_FR,1,-1
          N = ISPC_FR(L)
          I = IN_SPC(N)
              IAD = 6*(N-1)+1
              NN = IC_SPC(N)
            IF (NN.EQ.1) THEN
             EJ(1)=SKEW_SPC(IAD)
             EJ(2)=SKEW_SPC(IAD+1)
             EJ(3)=SKEW_SPC(IAD+2)
          CALL L_DIR(EJ,J)
          D(J,I) = ZERO
          CALL BC_UPDD(I     ,EJ   ,J     ,D     )
            ELSEIF (NN.EQ.2) THEN
          CALL BC_UPD2D(I     ,SKEW_SPC(IAD),SKEW_SPC(IAD+3),D     )
            END IF
          A(1,I)=ZERO
          A(2,I)=ZERO
          A(3,I)=ZERO
       ENDDO
C
       IF (NFX_FR.GT.0) THEN
        DO N=1,NFXVEL
         LJFR(N)=0
        ENDDO
        DO L=1,NFX_FR
         I = IFX_FR(1,L)
         LJFR(I) = IFX_FR(2,L)
        ENDDO
        CALL FV_IMPD(IBFV  ,LJFR  ,SKEW  ,XFRAME,D     ,
     1               DR    )
        DO L=NFX_FR,1,-1
          J = IFX_FR(2,L)
          A(J,N)=ZERO
        ENDDO
       ENDIF
C
      DO L = 1,NRW_FR
        I = IRW_FR(L)
        N=IN_RWL(I)
        EJ(1)=NOR_RWL(1,I)
        EJ(2)=NOR_RWL(2,I)
        EJ(3)=NOR_RWL(3,I)
        CALL L_DIR(EJ,J)
        CALL BC_UPDD(N     ,EJ   ,J     ,D     )
        A(J,N)=ZERO
      ENDDO
C
C------Rigid bodies-------
       DO I=NRB_FR,1,-1
        M=IFRSR(1,I)
         DO J = 1 , NDOF(M)
              ND = IDDMR(J,I)
          IF (J.LE.3.AND.ND.GT.0) THEN
            D(J,M)=LX(ND)
          ELSEIF (ND.GT.0) THEN
            DR(J-3,M)=LX(ND)
          ENDIF
         ENDDO
        NS=IFRSR(2,I)
        CALL RBY_IMP3(X    ,M      ,NS     ,D     ,DR     ,
     .                A    ,AR   )
       ENDDO
C--------RBE3-----
       IADS=1
       DO I=1,NRBE3_FR
         N=IFRS3(I)
         IAD=IRBE3(1,N)
         NS=IRBE3(3,N)
         NNOD=IRBE3(5,N)
         IROT=IRBE3(6,N)
         DO M=1,NNOD
          NJ=LRBE3(IAD+M)
           DO J=1,NDOF(NJ)
            ND = IDDMI3(J,M,I)
            IF (J.LE.3.AND.ND.GT.0) THEN
             D(J,NJ)=LX(ND)
             A(J,NJ)=ZERO
            ELSEIF(ND.GT.0) THEN
             DR(J-3,NJ)=LX(ND)
             AR(J-3,NJ)=ZERO
            ENDIF
           ENDDO
         ENDDO
         CALL PRERBE3FR(IRBE3 ,N    ,JT  ,JR   )
         CALL RBE3_FRD(NNOD  ,LRBE3(IAD+1),NS    ,D     ,DR    ,
     1                FRCDI(IADS),MRCDI(IADS),JT  ,JR   ,IROT  )
        IADS=IADS+NNOD
       ENDDO
C------Rbe2------
       DO I=NRBE2_FR,1,-1
        N=IFRS4(1,I)
        M=IRBE2(3,N)
         DO J = 1 , NDOF(M)
              ND = IDDMR2(J,I)
          IF (J.LE.3.AND.ND.GT.0) THEN
            D(J,M)=LX(ND)
            A(J,M)=ZERO
          ELSEIF (ND.GT.0) THEN
            DR(J-3,M)=LX(ND)
            AR(J-3,M)=ZERO
          ENDIF
         ENDDO
        NS=IFRS4(2,I)
        ISK = IRBE2(7,N)
            IRAD =IRBE2(11,N)
         IC = IRBE2(4,N)
C--------remove ICR---
         IC =(IC/512)*512
         CALL PRERBE2FR(IC    ,JT  ,JR   )
         CALL RBE2_FRD(NS    ,M     ,X     ,D     ,DR    ,
     1                JT    ,JR     ,SKEW(1,ISK),ISK   ,IRAD  )
         A(1,NS)=ZERO
         A(2,NS)=ZERO
         A(3,NS)=ZERO
       ENDDO
C------int2-------
       DO I=NI2_FR,1,-1
        N=IFRS2(1,I)
        NI=IFRS2(2,I)
        JI=IPARI(1,N)
        NSN=IPARI(5,N)
         L=INTBUF_TAB(N)%IRTLM(NI)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3).EQ.INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
C-------si noeud main est dependant aussi-----
         DO M=1,NNOD
          NJ=INTBUF_TAB(N)%IRECTM(NL+M)
           DO J=1,NDOF(NJ)
            ND = IDDMI2(J,M,I)
            IF (J.LE.3.AND.ND.GT.0) THEN
             D(J,NJ)=LX(ND)
             A(J,NJ)=ZERO
            ELSEIF(ND.GT.0) THEN
             DR(J-3,NJ)=LX(ND)
             AR(J-3,NJ)=ZERO
            ENDIF
           ENDDO
         ENDDO
        ILEV  =IPARI(20,N)
        IF (ILEV.EQ.1) THEN
         CALL I2_FRRD1(X   ,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%DPARA ,INTBUF_TAB(N)%NSV ,
     1                INTBUF_TAB(N)%IRTLM ,D   ,NI    )
        ELSE
         CALL I2_FRRD0(X   ,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%CSTS ,INTBUF_TAB(N)%NSV ,
     1                INTBUF_TAB(N)%IRTLM ,D    ,DR    ,NI  ,NDOF      )
        ENDIF
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        BC_FI                         source/constraints/general/bcs/bc_imp0.F
Chd|        BC_FI2                        source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDF                       source/constraints/general/bcs/bc_imp0.F
Chd|        FV_UPDF                       source/constraints/general/impvel/fv_imp0.F
Chd|        I2_FRFM0                      source/interfaces/interf/i2_imp1.F
Chd|        I2_FRFM1                      source/interfaces/interf/i2_imp1.F
Chd|        KIN_UPDF                      source/constraints/general/impvel/fv_imp0.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        PRERBE2FR                     source/constraints/general/rbe2/rbe2f.F
Chd|        PRERBE3FR                     source/constraints/general/rbe3/rbe3f.F
Chd|        RBE2FRF                       source/constraints/general/rbe2/rbe2f.F
Chd|        RBE3FRF                       source/constraints/general/rbe3/rbe3f.F
Chd|        RBY_IMPF                      source/constraints/general/rbody/rby_imp0.F
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPD_FR(  A     ,AR     ,X     ,IPARI  ,INTBUF_TAB,
     1                   NDOF   ,IBFV  ,SKEW   ,XFRAME ,
     2                   IRBE3  ,LRBE3  ,IRBE2 ,LRBE2  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE IMP_RWL
      USE IMP_ASPC
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),IRBE2(NRBE2L,*),LRBE2(*),
     .        IPARI(NPARI,*), NDOF(*),IBFV(NIFV,*)
      my_real
     .  A(3,*),AR(3,*),X(3,*) ,SKEW(LSKEW,*),XFRAME(NXFRAME,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,M,NS,NI,NSN,ILEV,I1,IADS,IROT,IAD,NNOD,ISK,IRAD,
     .        JI,JT(3),JR(3),NN,IC
      my_real
     .  EJ(3)
C------int2-------
       DO I=1,NI2_FR
        N=IFRS2(1,I)
        JI=IPARI(1,N)
        NSN=IPARI(5,N)
        NI=IFRS2(2,I)
        ILEV  =IPARI(20,N)
        IF (ILEV.EQ.1) THEN
         CALL I2_FRFM1(X   ,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%DPARA ,INTBUF_TAB(N)%NSV ,
     1                INTBUF_TAB(N)%IRTLM ,A   ,NI   )
        ELSE
         CALL I2_FRFM0(X   ,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%CSTS ,INTBUF_TAB(N)%NSV ,
     1                INTBUF_TAB(N)%IRTLM ,A   ,AR    ,NI  ,NDOF    )
        ENDIF
       ENDDO
C------RBE2-------
       DO I=1,NRBE2_FR
        N=IFRS4(1,I)
        M=IRBE2(3,N)
        NS=IFRS4(2,I)
        ISK = IRBE2(7,N)
            IRAD =IRBE2(11,N)
         IC = IRBE2(4,N)
C--------remove ICR---
         IC =(IC/512)*512
         CALL PRERBE2FR(IC    ,JT  ,JR   )
         CALL RBE2FRF(NS    ,M     ,A     ,AR    ,JT    ,
     1                JR    ,X     ,ISK   ,SKEW(1,ISK),IRAD  )
       ENDDO
C--------RBE3-----
       IADS=1
       DO I=1,NRBE3_FR
         N=IFRS3(I)
         IAD=IRBE3(1,N)
         NS=IRBE3(3,N)
         NNOD=IRBE3(5,N)
         IROT=IRBE3(6,N)
         CALL PRERBE3FR(IRBE3 ,N    ,JT  ,JR   )
         CALL RBE3FRF(NNOD  ,LRBE3(IAD+1),NS    ,A     ,AR    ,
     1                FRCDI(IADS),MRCDI(IADS),JT    ,JR    ,IROT  )
        IADS=IADS+NNOD
       ENDDO
C------Rigid bodies-------
       DO I=1,NRB_FR
        M=IFRSR(1,I)
        NS=IFRSR(2,I)
        CALL RBY_IMPF(X    ,M      ,NS     ,NDOF   ,A     ,
     .               AR    )
       ENDDO
       IF (NBC_FR.GT.0) THEN
        CALL BC_UPDF(NBC_FR ,IBC_FR ,SKEW  ,A      )
       ENDIF
       DO I1=1,NSPC_FR
          N = ISPC_FR(I1)
          I = IN_SPC(N)
              IAD = 6*(N-1)+1
              NN = IC_SPC(N)
            IF (NN.EQ.1) THEN
             EJ(1)=SKEW_SPC(IAD)
             EJ(2)=SKEW_SPC(IAD+1)
             EJ(3)=SKEW_SPC(IAD+2)
          CALL L_DIR(EJ,J)
          CALL BC_FI(I    ,SKEW_SPC(IAD),J    ,A     )
            ELSEIF (NN.EQ.2) THEN
          CALL BC_FI2(I   ,SKEW_SPC(IAD),SKEW_SPC(IAD+3),A    )
            END IF
       ENDDO
C
       IF (NFX_FR.GT.0) THEN
        CALL FV_UPDF(NFX_FR ,IFX_FR ,IBFV  ,SKEW  ,XFRAME,
     1               A      )
       ENDIF
       DO I1 = 1,NRW_FR
        I = IRW_FR(I1)
        N = IN_RWL(I)
        EJ(1)=NOR_RWL(1,I)
        EJ(2)=NOR_RWL(2,I)
        EJ(3)=NOR_RWL(3,I)
        CALL L_DIR(EJ,J)
        CALL KIN_UPDF(N    ,EJ    ,J    ,A     )
       ENDDO
C
      RETURN
      END
Cqz ----  nouvelle routine---------
Chd|====================================================================
Chd|  FR_A2B                        source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE FR_A2B(NDOF  ,LB    ,A     ,NSL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDOF(*),NSL
      my_real
     .  A(3,*),LB(*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ID,ND,M,N
C---------
      DO I = 1,NSL
       N=ISLM(I)
       IF (N.GT.0) THEN
        DO J=1,MIN(3,NDOF(N))
         ND = IDDSL(J,I)
         IF (ND.GT.0) LB(ND)=LB(ND)+A(J,N)
        ENDDO
       ENDIF
      ENDDO
      DO I = 1,NML
       N=IML(I)
        DO J=1,MIN(3,NDOF(N))
         ND = IDDML(J,I)
         IF (ND.GT.0) LB(ND)=LB(ND)+A(J,N)
        ENDDO
      ENDDO
C
      RETURN
      END
Cqz ----  nouvelle routine---------
Chd|====================================================================
Chd|  FR_A2BD                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE FR_A2BD(NDOF  ,IPARI ,INTBUF_TAB  ,LB    ,A     ,
     .                   AR    ,IRBE3 ,LRBE3  ,IRBE2 ,LRBE2  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDOF(*),IPARI(NPARI,*),
     .        IRBE3(NRBE3L,*),LRBE3(*) ,IRBE2(NRBE2L,*),LRBE2(*)
      my_real
     .  A(3,*),AR(3,*),LB(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,M,NS,NI,NSN,
     .        L,NNOD,NJ,ND,NL,IAD,JI
C------------------------------------
C------Rigid bodies-------
       DO I=1,NRB_FR
        M=IFRSR(1,I)
         DO J=1,NDOF(M)
          ND = IDDMR(J,I)
          IF (J.LE.3.AND.ND.GT.0) THEN
           LB(ND)=LB(ND)+A(J,M)
           A(J,M)=ZERO
          ELSEIF(ND.GT.0) THEN
           LB(ND)=LB(ND)+AR(J-3,M)
           AR(J-3,M)=ZERO
          ENDIF
         ENDDO
       ENDDO
C------int2-------
       DO I=1,NI2_FR
        N=IFRS2(1,I)
        NI=IFRS2(2,I)
        JI=IPARI(1,N)
        NSN=IPARI(5,N)
         L=INTBUF_TAB(N)%IRTLM(NI)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3).EQ.INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
C-------si noeud main est dependant aussi-----
         DO M=1,NNOD
          NJ=INTBUF_TAB(N)%IRECTM(NL+M)
           DO J=1,NDOF(NJ)
            ND = IDDMI2(J,M,I)
            IF (J.LE.3.AND.ND.GT.0) THEN
             LB(ND)=LB(ND)+A(J,NJ)
             A(J,NJ)=ZERO
            ELSEIF(ND.GT.0) THEN
             LB(ND)=LB(ND)+AR(J-3,NJ)
             AR(J-3,NJ)=ZERO
            ENDIF
           ENDDO
         ENDDO
       ENDDO
C--------RBE3-----
       DO I=1,NRBE3_FR
         N=IFRS3(I)
         IAD=IRBE3(1,N)
         NNOD=IRBE3(5,N)
         DO M=1,NNOD
          NJ=LRBE3(IAD+M)
           DO J=1,NDOF(NJ)
            ND = IDDMI3(J,M,I)
            IF (J.LE.3.AND.ND.GT.0) THEN
             LB(ND)=LB(ND)+A(J,NJ)
             A(J,NJ)=ZERO
            ELSEIF(ND.GT.0) THEN
             LB(ND)=LB(ND)+AR(J-3,NJ)
             AR(J-3,NJ)=ZERO
            ENDIF
           ENDDO
         ENDDO
       ENDDO
C------RBE2-------
       DO I=1,NRBE2_FR
        N=IFRS4(1,I)
        M=IRBE2(3,N)
         DO J=1,NDOF(M)
          ND = IDDMR2(J,I)
          IF (J.LE.3.AND.ND.GT.0) THEN
           LB(ND)=LB(ND)+A(J,M)
           A(J,M)=ZERO
          ELSEIF(ND.GT.0) THEN
           LB(ND)=LB(ND)+AR(J-3,M)
           AR(J-3,M)=ZERO
          ENDIF
         ENDDO
       ENDDO
C
      RETURN
      END
Cqz ----  nouvelle routine---------
Chd|====================================================================
Chd|  DIM_NRMAX                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        DIM_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DIM_NRMAX(NROW  ,FR_ELEM ,IAD_ELEM,NNMAX   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NROW(*),IAD_ELEM(2,*),FR_ELEM(*),NNMAX
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NR
C------------------------------------
      IF (N_FRNN.EQ.0) RETURN
C
       DO I =1,NSPMD
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          NR=IAD_RL(J+1)-IAD_RL(J)
          IF (NR.GT.0)THEN
           N = FR_ELEM(J)
           NROW(N)=NROW(N)+NR
           NNMAX = MAX(NNMAX,NROW(N))
          ENDIF
        ENDDO
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  KIN_NRMAX0                    source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        DIM_KINMAX                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE KIN_NRMAX0(
     1    NNMAX     ,NKMAX     ,NROWK     ,ICOK      ,ICOKM     ,
     2    ILOC      ,INK       ,FR_ELEM   ,IAD_ELEM  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,NKMAX,NROWK(*),ILOC(*),INK
      INTEGER ICOK(NNMAX,*),ICOKM(NKMAX,*),IAD_ELEM(2,*),FR_ELEM(*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NL,NR,K,NN
C------------------------------------
      IF (N_FRNN.EQ.0) RETURN
C
       DO I =1,NSPMD
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
         N = FR_ELEM(J)
         NN = ILOC(N)
         IF (NN.GT.INK)THEN
          DO K=IAD_RL(J),IAD_RL(J+1)-1
           NL = FR_ICOL(K)+IAD_ELEM(1,I)-1
           CALL REORDER_A(NROWK(NN),ICOK(1,NN),FR_ELEM(NL))
          ENDDO
         ELSEIF (NN.GT.0)THEN
          DO K=IAD_RL(J),IAD_RL(J+1)-1
           NL = FR_ICOL(K)+IAD_ELEM(1,I)-1
           CALL REORDER_A(NROWK(NN),ICOKM(1,NN),FR_ELEM(NL))
          ENDDO
         ENDIF
        ENDDO
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  KIN_NRMAX                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        DIM_KINMAX                    source/implicit/ind_glob_k.F  
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE KIN_NRMAX(
     1    NNMAX     ,NKMAX     ,NROWK     ,ICOK      ,ICOKM     ,
     2    ILOC      ,INK       ,FR_ELEM   ,IAD_ELEM  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,NKMAX,NROWK(*),ILOC(*),INK
      INTEGER ICOK(NNMAX,*),ICOKM(NKMAX,*),IAD_ELEM(2,*),FR_ELEM(*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NL,NR,K,NK,NN
C------------------------------------
      IF (N_FRNN.EQ.0) RETURN
C
       DO I =1,NSPMD
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
         N = FR_ELEM(J)
         NN = ILOC(N)
         IF (NN.GT.INK)THEN
          NK=NN-INK
          DO K=IAD_RL(J),IAD_RL(J+1)-1
           NL = FR_ICOL(K)+IAD_ELEM(1,I)-1
           CALL REORDER_A(NROWK(NN),ICOKM(1,NK),FR_ELEM(NL))
          ENDDO
         ELSEIF (NN.GT.0)THEN
          DO K=IAD_RL(J),IAD_RL(J+1)-1
           NL = FR_ICOL(K)+IAD_ELEM(1,I)-1
           CALL REORDER_A(NROWK(NN),ICOK(1,NN),FR_ELEM(NL))
          ENDDO
         ENDIF
        ENDDO
       ENDDO
C
      RETURN
      END
Cqz ----  nouvelle routine---------
Chd|====================================================================
Chd|  IND_NRMAX                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_NRFR                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE IND_NRMAX(
     1    NFT       ,NEL       ,NPN       ,NPPS      ,NNMAX     ,
     2    NROW      ,ICOL      ,IAD_RL    ,FR_ICOL   ,N_FRNN    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,IAD_RL(*),FR_ICOL(*),N_FRNN
      INTEGER NFT,NEL,NPN,NPPS,NROW(*),ICOL(NNMAX,*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,M,JLT,JLT1,NK,NFT1,ISH,L,NPP
C------------------------------------
       IF (N_FRNN.EQ.0) RETURN
       JLT=NEL+NFT
       NPP=NUMNOD-NPPS
       IF (NPN.GT.NFT) THEN
        JLT1 = MIN( NEL, NPN - NFT )
        DO NK=1,JLT1
         J=NK+NFT
         DO I=IAD_RL(J),IAD_RL(J+1)-1
          CALL REORDER_A(NROW(NK),ICOL(1,NK),FR_ICOL(I))
         ENDDO
        ENDDO
       ENDIF
       IF (NPP.LT.JLT) THEN
        NFT1 = MAX(NFT,NPP)+1
        ISH= NPP-NPN
        DO N=NFT1,JLT
         NK=N-NFT
         J=N-ISH
         DO I=IAD_RL(J),IAD_RL(J+1)-1
          CALL REORDER_A(NROW(NK),ICOL(1,NK),FR_ICOL(I))
         ENDDO
        ENDDO
       ENDIF
C
      RETURN
      END

Chd|====================================================================
Chd|  DIM_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        DIM_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        DIM_ELEMSP                    source/implicit/ind_glob_k.F  
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        SPMD_ICOL                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_NROW                     source/mpi/implicit/imp_spmd.F
Chd|        INTAB                         source/implicit/ind_glob_k.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DIM_FR_K(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NDOF      ,
     4    NNMAX     ,INLOC     ,FR_ELEM   ,IAD_ELEM  ,N_FR      ,
     5    IGEO      ,FR_I2M    ,IAD_I2M   ,ELBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),NNMAX,N_FR ,NDOF(*),IGEO(*),
     .        FR_I2M(*),IAD_I2M(*)
      INTEGER
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*),IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXUR(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IXTG1(4,*),INLOC(*),FR_ELEM(*),IAD_ELEM(2,*)
C     REAL ou REAL*8
      my_real
     .   ELBUF(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NK,ICOL(NNMAX,N_FR),NROW(N_FR),IP,JLT,NFT,
     .        FR_NROW(N_FR),L,IAD_S(NSPMD+1),IAD_R(NSPMD+1),NF1,
     .        IERROR1,IERROR2,IERROR3,LS,LR,SSIZE,RSIZE,J0,NN,NR,NL,NRN
Cqz ----  creer NROW,ICOL aux front.---------
      IF (N_FR <=0 .OR. NNMAX <=0 ) THEN
       NDDLFR=0
       NDDLFRB=0
       NDFRMAX=0
       N_FRNN=0
c      RETURN
      ENDIF
      DO N =1,NUMNOD
       INLOC(N)=0
      ENDDO
      DO N =1,N_FR
       NROW(N)=0
      ENDDO
      DO IP =1,NSPMD
       JLT=IAD_ELEM(1,IP+1)-IAD_ELEM(1,IP)
       NFT=IAD_ELEM(1,IP)-1
       NF1=MIN(N_FR,NFT+1)
       DO J=1,JLT
        NK=J+NFT
        N=FR_ELEM(NK)
        INLOC(N) = J
       ENDDO
       CALL DIM_ELEMSP(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROW(NF1),
     4    INLOC     ,NNMAX     ,ICOL(1,NF1),IGEO   ,ELBUF_TAB )
       DO J=1,JLT
        NK=J+NFT
        N=FR_ELEM(NK)
        INLOC(N) = 0
       ENDDO
      ENDDO
       CALL SPMD_NROW(NROW,FR_NROW,IAD_ELEM,N_FR)
C------prepare comm.-----
       LS=1
       LR=1
       IAD_S(1)=LS
       IAD_R(1)=LR
       DO IP =1,NSPMD
        DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         LS=LS+NROW(NK)
         LR=LR+FR_NROW(NK)
        ENDDO
        IAD_S(IP+1)=LS
        IAD_R(IP+1)=LR
       ENDDO
       IF ((LS+LR).GT.2) THEN
        SSIZE=IAD_S(NSPMD+1)-1
        RSIZE=IAD_R(NSPMD+1)-1
C
        CALL SPMD_ICOL(
     1   IAD_S    ,IAD_R    ,NNMAX    ,ICOL     ,NROW     ,
     2   FR_NROW  ,IAD_ELEM ,FR_ELEM  ,SSIZE    ,RSIZE    )
       ENDIF
C
       DO IP =1,NSPMD
        NR=IAD_I2M(IP+1)-IAD_I2M(IP)
        IF (NR.GT.0) THEN
         JLT=IAD_ELEM(1,IP+1)-IAD_ELEM(1,IP)
         NFT=IAD_ELEM(1,IP)-1
         DO J=1,JLT
          NK=J+NFT
          N=FR_ELEM(NK)
          INLOC(N) = J
         ENDDO
         DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
          N=FR_ELEM(NK)
          IF (INTAB(NR,FR_I2M(IAD_I2M(IP)),N)) THEN
           DO L=IAD_I2M(IP),IAD_I2M(IP+1)-1
            NN=FR_I2M(L)
            NL=INLOC(NN)
            IF (NL.GT.0.AND.NN.NE.N) THEN
                 NRN = MAX(NROW(NK),FR_NROW(NK))
                 CALL REORDER_A(NRN,ICOL(1,NK),NL)
                  IF (NRN.GT.MAX(NROW(NK),FR_NROW(NK))) FR_NROW(NK)=NRN
            ENDIF
           ENDDO
          ENDIF
         ENDDO
C
         DO J=1,JLT
          NK=J+NFT
          N=FR_ELEM(NK)
          INLOC(N) = 0
         ENDDO
        ENDIF !IF (NR.GT.0)
       ENDDO !IP =1,NSPMD
       NRMAX = 0
       IF(ALLOCATED(IAD_RL)) DEALLOCATE(IAD_RL)
       ALLOCATE(IAD_RL(N_FR+1),STAT=IERROR3)
       LR=1
       IAD_RL(1)=LR
       DO IP =1,NSPMD
        DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         IF (NROW(NK).LT.FR_NROW(NK)) THEN
          NR=FR_NROW(NK)-NROW(NK)
          LR=LR+NR
          NRMAX = MAX(NRMAX,NR)
         ENDIF
         IAD_RL(NK+1)=LR
        ENDDO
       ENDDO
       N_FRNN=LR-1
C-------d'abord c'est locale------------
       IF (N_FRNN.GT.0) THEN
        IF(ALLOCATED(FR_ICOL)) DEALLOCATE(FR_ICOL)
        ALLOCATE(FR_ICOL(LR),STAT=IERROR2)
        DO IP =1,NSPMD
         DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
           J0=NROW(NK)-IAD_RL(NK)+1
           DO L=IAD_RL(NK),IAD_RL(NK+1)-1
            J=L+J0
            FR_ICOL(L)=ICOL(J,NK)
           ENDDO
         ENDDO
        ENDDO
       ENDIF
C------definit IFRLOC (global)pour ind_glob_k---
       IF(ALLOCATED(IFRLOC)) DEALLOCATE(IFRLOC)
       ALLOCATE(IFRLOC(N_FR),STAT=IERROR1)
       NN=0
       DO N =1,NUMNOD
        INLOC(N)=0
       ENDDO
       DO IP =1,NSPMD
        DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         N=FR_ELEM(NK)
         IF (INLOC(N).EQ.0) THEN
          NN = NN +1
          INLOC(N)=NN
          IFRLOC(NK)=NN
          ICOL(1,NN)=NK
         ELSE
          IFRLOC(NK)=-ICOL(1,INLOC(N))
          IF (NROW(NK).LT.FR_NROW(NK))NRMAX=NRMAX+FR_NROW(NK)-NROW(NK)
         ENDIF
        ENDDO
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  IND_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        DIM_ELEMSP                    source/implicit/ind_glob_k.F  
Chd|        IND_FR_K0                     source/mpi/implicit/imp_fri.F 
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IND_FR_K(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NDOF      ,
     4    NNMAX     ,INLOC     ,FR_ELEM   ,IAD_ELEM  ,N_FR      ,
     5    IGEO      ,ELBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),NNMAX,N_FR ,NDOF(*),IGEO(*)
      INTEGER
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*),IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXUR(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IXTG1(4,*),INLOC(*),FR_ELEM(*),IAD_ELEM(2,*)
C     REAL ou REAL*8
      my_real
     .   ELBUF(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NK,ICOL(NNMAX,N_FR),NROW(N_FR),IP,JLT,NFT,
     .        IERROR1,IERROR2,IERROR3,LS,LR,SSIZE,RSIZE,J0,L,NF1
Cqz ----  creer NROW,ICOL aux front.---------
      IF (N_FR.GT.0) THEN
      DO N =1,NUMNOD
       INLOC(N)=0
      ENDDO
      DO N =1,N_FR
       NROW(N)=0
      ENDDO
      DO IP =1,NSPMD
       JLT=IAD_ELEM(1,IP+1)-IAD_ELEM(1,IP)
       NFT=IAD_ELEM(1,IP)-1
       NF1=MIN(N_FR,NFT+1)
       DO J=1,JLT
        NK=J+NFT
        N=FR_ELEM(NK)
        INLOC(N) = J
       ENDDO
       CALL DIM_ELEMSP(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROW(NF1),
     4    INLOC     ,NNMAX     ,ICOL(1,NF1),IGEO   ,ELBUF_TAB )
       DO J=1,JLT
        NK=J+NFT
        N=FR_ELEM(NK)
        INLOC(N) = 0
       ENDDO
      ENDDO
C--------recupere fr_nrow --
       IF (N_FRNN.GT.0) THEN
        DO IP =1,NSPMD
         DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
           DO L=IAD_RL(NK),IAD_RL(NK+1)-1
            CALL REORDER_A(NROW(NK),ICOL(1,NK),FR_ICOL(L))
           ENDDO
         ENDDO
        ENDDO
       ENDIF
      ENDIF  ! (N_FR.GT.0)
C------ind_ [k] de FR.------
       CALL IND_FR_K0(
     1    NDOF      ,NROW      ,NNMAX     ,ICOL      ,FR_ELEM   ,
     2    IAD_ELEM  ,N_FR      )
C
      RETURN
      END
Chd|====================================================================
Chd|  IND_NRFR                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        IND_NRMAX                     source/mpi/implicit/imp_fri.F 
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IND_NRFR(
     1    NFT       ,NEL       ,NPN       ,NPP       ,NNMAX     ,
     2    NROW      ,ICOL      ,FR_ELEM   ,IAD_ELEM  ,N_FR      ,
     3    ICOK      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,NFT,NEL,NPN,NPP,NROW(*),ICOL(NNMAX,*)
      INTEGER
     .   FR_ELEM(*),IAD_ELEM(2,*),N_FR,ICOK(NRMAX,N_FR)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NK,NROWK(N_FR),IP,
     .        L,IAD_R(N_FR+1),FR_ICOL1(N_FRNN+1),LR,J0,NN,N_FRN
C--------recupere fr_nrow et passe en global (par rap. a PROC) --
      IF (NNMAX <=0 ) RETURN
       DO IP =1,NSPMD
        DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         NROWK(NK)=IAD_RL(NK+1)-IAD_RL(NK)
         J0 = IAD_ELEM(1,IP)-1
         DO L=IAD_RL(NK),IAD_RL(NK+1)-1
          J=L-IAD_RL(NK)+1
              N= FR_ICOL(L)+J0
              ICOK(J,NK)=FR_ELEM(N)
         ENDDO
        ENDDO
       ENDDO
C----cumule au cas ou les noeuds partient plusieurs frontieres--
       DO IP =1,NSPMD
        DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         N = -IFRLOC(NK)
             IF (N.GT.0) THEN
           DO J=1,NROWK(NK)
            CALL REORDER_A(NROWK(N),ICOK(1,N),ICOK(J,NK))
           ENDDO
         ENDIF
        ENDDO
       ENDDO
C------dim de IAD_R->NN+1--
       LR=1
       IAD_R(1)=LR
       DO IP =1,NSPMD
        DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         N = IFRLOC(NK)
         IF (N.GT.0) THEN
          LR=LR+NROWK(NK)
          IAD_R(N+1)=LR
         ENDIF
        ENDDO
       ENDDO
       N_FRN=LR-1
       if (N_FRN.GT.N_FRNN) print *,'PROBLEM IN IND_NRFR'
       IF (N_FRN.GT.0) THEN
        DO IP =1,NSPMD
         DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
          N = IFRLOC(NK)
          IF (N.GT.0) THEN
           DO J=1,NROWK(NK)
            L=IAD_R(N)+J-1
            FR_ICOL1(L)=ICOK(J,NK)
           ENDDO
          ENDIF
         ENDDO
        ENDDO
       ENDIF
C
       CALL IND_NRMAX(
     1    NFT       ,NEL       ,NPN       ,NPP       ,NNMAX     ,
     2    NROW      ,ICOL      ,IAD_R     ,FR_ICOL1  ,N_FRNN    )

C
      RETURN
      END
Chd|====================================================================
Chd|  IND_FR_K0                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        REORDER_L                     source/implicit/ind_glob_k.F  
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IND_FR_K0(
     1    NDOF      ,NROW      ,NNMAX     ,ICOL      ,FR_ELEM   ,
     2    IAD_ELEM  ,N_FR      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,N_FR ,NDOF(*),NROW(*)
      INTEGER
     .   FR_ELEM(*),IAD_ELEM(2,*),ICOL(NNMAX,*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NK,IP,L,NDDL0,NZZK0,NN,IAD,NB,
     .        NNZ(NSPMD),NDOFJ,K,NDDL,NZZK,NJ,NK0,NJN,NL,NZZ,
     .        IERROR0,IERROR1,IERROR2,IERROR3,IERROR4,IERROR5
      INTEGER, DIMENSION(:),ALLOCATABLE :: ITAG
      my_real
     .        S1,STMP
C------dim [k] pour chaque FR.-----
      IF (IRODDL.EQ.0) THEN
       NDOFJ=3
      ELSE
       NDOFJ=6
      ENDIF
      NDDLFR=0
      NDDLFRB=0
      NDFRMAX=0
      NZZK=0
      IF(ALLOCATED(ND_FR)) DEALLOCATE(ND_FR)
      ALLOCATE(ND_FR(NSPMD),STAT=IERROR0)
      DO IP =1,NSPMD
       ND_FR(IP)=0
       NNZ(IP)=0
       DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
        N=FR_ELEM(NK)
        ND_FR(IP)=ND_FR(IP)+NDOF(N)
        DO K=1,NDOF(N)
C-------termes knn-------
         DO J=1,NDOF(N)
          IF (J.NE.K) NNZ(IP) = NNZ(IP)+1
         ENDDO
C-------termes kn,nj-------
         DO J=1,NROW(NK)
          DO L=1,NDOFJ
           NNZ(IP) = NNZ(IP)+1
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       NDDLFR = NDDLFR + ND_FR(IP)
       NDFRMAX=MAX(NDFRMAX,ND_FR(IP))
       NZZK = NZZK + NNZ(IP)
      ENDDO
      NZZK = NZZK/2+1
      STMP = ZERO
      IF (NDDLFR.GT.0) THEN
       ALLOCATE(ITAG(N_FR))
       ITAG = 2
       DO NK=1,N_FR
        NB=-IFRLOC(NK)
        IF (NB.GT.0) ITAG(NB) = ITAG(NB) + 1
       ENDDO
       DO NK=1,N_FR
        NB=-IFRLOC(NK)
        IF (NB.LT.0) NB = NK
        S1 = ONE/ITAG(NB)
        STMP = STMP + S1*NDOF(FR_ELEM(NB))
       ENDDO
       DEALLOCATE(ITAG)
      ENDIF
      CALL SPMD_SUM_S(STMP)
      NDDLFRB = INT(STMP)
      IF (NDDLFR.EQ.0) RETURN
C
      NDDL = NDDLFR + NSPMD
      IF(ALLOCATED(IADFR)) DEALLOCATE(IADFR)
      ALLOCATE(IADFR(NDDL),STAT=IERROR1)
      IF(ALLOCATED(JDIFR)) DEALLOCATE(JDIFR)
      ALLOCATE(JDIFR(NZZK),STAT=IERROR2)
      IF(ALLOCATED(IDDLFR)) DEALLOCATE(IDDLFR)
      ALLOCATE(IDDLFR(N_FR),STAT=IERROR3)
      IF(ALLOCATED(IFR2K)) DEALLOCATE(IFR2K)
      ALLOCATE(IFR2K(NDDLFR),STAT=IERROR4)
      IF (IPREC.GT.2.OR.IAUTSPC.GT.0) THEN
       IF(ALLOCATED(JFR2K)) DEALLOCATE(JFR2K)
       ALLOCATE(JFR2K(NZZK),STAT=IERROR5)
      ENDIF
      DO IP =1,NSPMD
       NDDL0=0
       DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
        N=FR_ELEM(NK)
        IDDLFR(NK) = NDDL0
        NDDL0=NDDL0+NDOF(N)
       ENDDO
      ENDDO
C------ind_ [k] pour chaque FR.-----
      NDDL0 = NDDL
      NZZK0 = NZZK
      NZZK=0
      IAD=0
      DO IP =1,NSPMD
       NK0=IAD_ELEM(1,IP)-1
       NZZ = 0
       NDDL=1
       IADFR(NDDL+IAD)=1
       DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
        N=FR_ELEM(NK)
        CALL REORDER_L(NROW(NK),ICOL(1,NK),NK-NK0,
     .                 IDDLFR(IAD_ELEM(1,IP)))
        IF (NDOF(N).GT.0) THEN
          DO K=1,NDOF(N)
C-------termes knj,n-------
            DO J=1,NROW(NK)
             NJ = ICOL(J,NK)+NK0
             NN=FR_ELEM(NJ)
             DO L=1,NDOF(NN)
              NZZ = NZZ+1
              JDIFR(NZZ+NZZK) = IDDLFR(NJ)+L
             ENDDO
            ENDDO
            DO J=1,K-1
             NZZ = NZZ+1
             JDIFR(NZZ+NZZK) = IDDLFR(NK)+J
            ENDDO
           NDDL = NDDL +1
           IADFR(NDDL+IAD) = NZZ+1
          ENDDO
        ENDIF
       ENDDO
       NZZK = NZZK +NZZ
       IAD = IAD + NDDL
      ENDDO
      IF (NZZK.GT.NZZK0.OR.IAD.NE.NDDL0)
     .    WRITE(*,*)'--MEMERY PROBLEM 3--:',NZZK,NZZK0,IAD,NDDL0
      NZKFR = NZZK
C
      RETURN
      END
Chd|====================================================================
Chd|  INI_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        DIM_ELEMSP                    source/implicit/ind_glob_k.F  
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        SPMD_ICOL                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_NROW                     source/mpi/implicit/imp_spmd.F
Chd|        INTAB                         source/implicit/ind_glob_k.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE INI_FR_K(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NDOF      ,
     4    NNMAX     ,INLOC     ,FR_ELEM   ,IAD_ELEM  ,N_FR      ,
     5    IGEO      ,FR_I2M    ,IAD_I2M   ,ELBUF_TAB ,NNRMAX    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),NNMAX,N_FR ,NDOF(*),IGEO(*),
     .        FR_I2M(*),IAD_I2M(*),NNRMAX
      INTEGER
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*),IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXUR(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IXTG1(4,*),INLOC(*),FR_ELEM(*),IAD_ELEM(2,*)
C     REAL ou REAL*8
      my_real
     .   ELBUF(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NK,ICOL(NNMAX,N_FR),NROW(N_FR),IP,JLT,NFT,
     .        FR_NROW(N_FR),L,IAD_S(NSPMD+1),IAD_R(NSPMD+1),
     .        IERROR1,IERROR2,IERROR3,LS,LR,SSIZE,RSIZE,J0,NN,NR,NL,NRN,
     .        NR2,NF1
Cqz ----  suite pb avec elem delete.---------
C     IF (N_FR <=0 .OR. NNMAX <=0) RETURN
      DO N =1,NUMNOD
       INLOC(N)=0
      ENDDO
      DO N =1,N_FR
       NROW(N)=0
      ENDDO
      DO IP =1,NSPMD
       JLT=IAD_ELEM(1,IP+1)-IAD_ELEM(1,IP)
       NFT=IAD_ELEM(1,IP)-1
       NF1=MIN(N_FR,NFT+1)
       DO J=1,JLT
        NK=J+NFT
        N=FR_ELEM(NK)
        INLOC(N) = J
       ENDDO
       CALL DIM_ELEMSP(
     1    ELBUF     ,IPARG     ,IXS       ,IXQ       ,IXC       ,
     2    IXT       ,IXP       ,IXR       ,IXTG      ,IXTG1     ,
     3    IXS10     ,IXS20     ,IXS16     ,IXUR      ,NROW(NF1),
     4    INLOC     ,NNMAX     ,ICOL(1,NF1),IGEO   ,ELBUF_TAB )
       DO J=1,JLT
        NK=J+NFT
        N=FR_ELEM(NK)
        INLOC(N) = 0
       ENDDO
      ENDDO
       CALL SPMD_NROW(NROW,FR_NROW,IAD_ELEM,N_FR)
C------prepare comm.-----
       LS=1
       LR=1
       IAD_S(1)=LS
       IAD_R(1)=LR
       DO IP =1,NSPMD
        DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         LS=LS+NROW(NK)
         LR=LR+FR_NROW(NK)
        ENDDO
        IAD_S(IP+1)=LS
        IAD_R(IP+1)=LR
       ENDDO
       IF ((LS+LR).GT.2) THEN
        SSIZE=IAD_S(NSPMD+1)-1
        RSIZE=IAD_R(NSPMD+1)-1
C
        CALL SPMD_ICOL(
     1   IAD_S    ,IAD_R    ,NNMAX    ,ICOL     ,NROW     ,
     2   FR_NROW  ,IAD_ELEM ,FR_ELEM  ,SSIZE    ,RSIZE    )
       ENDIF
C
       NR2=IAD_I2M(NSPMD+1)-IAD_I2M(1)
      IF (NR2.GT.0) THEN
       DO IP =1,NSPMD
        NR=IAD_I2M(IP+1)-IAD_I2M(IP)
        IF (NR.GT.0) THEN
         JLT=IAD_ELEM(1,IP+1)-IAD_ELEM(1,IP)
         NFT=IAD_ELEM(1,IP)-1
         DO J=1,JLT
          NK=J+NFT
          N=FR_ELEM(NK)
          INLOC(N) = J
         ENDDO
         DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
          N=FR_ELEM(NK)
          IF (INTAB(NR,FR_I2M(IAD_I2M(IP)),N)) THEN
           DO L=IAD_I2M(IP),IAD_I2M(IP+1)-1
            NN=FR_I2M(L)
            NL=INLOC(NN)
            IF (NL.GT.0.AND.NN.NE.N) THEN
             NRN = MAX(NROW(NK),FR_NROW(NK))
             CALL REORDER_A(NRN,ICOL(1,NK),NL)
             IF (NRN.GT.MAX(NROW(NK),FR_NROW(NK))) FR_NROW(NK)=NRN
            ENDIF
           ENDDO
          ENDIF
         ENDDO
C
         DO J=1,JLT
          NK=J+NFT
          N=FR_ELEM(NK)
          INLOC(N) = 0
         ENDDO
        ENDIF
       ENDDO
      END IF !(NR2.GT.0) THEN
C
       IF(ALLOCATED(IAD_RL)) DEALLOCATE(IAD_RL)
       ALLOCATE(IAD_RL(N_FR+1),STAT=IERROR3)
       LR=1
       IAD_RL(1)=LR
       NRMAX = 0
       DO IP =1,NSPMD
        DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         IF (NROW(NK).LT.FR_NROW(NK)) THEN
          NR=FR_NROW(NK)-NROW(NK)
          LR=LR+NR
          NRMAX = MAX(NRMAX,NR)
          N=FR_ELEM(NK)
          IF (INLOC(N).EQ.0) THEN
           INLOC(N)=1
          ELSE
           NRMAX=NRMAX+NR
          ENDIF
         ENDIF
         IAD_RL(NK+1)=LR
        ENDDO
       ENDDO
       NNRMAX = NRMAX
C
       DO IP =1,NSPMD
        DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         IF (NROW(NK).LT.FR_NROW(NK)) THEN
          N=FR_ELEM(NK)
          INLOC(N) = 0
         ENDIF
        ENDDO
       ENDDO
C
       N_FRNN=LR-1
C-------d'abord c'est locale------------
       IF (N_FRNN.GT.0) THEN
        IF(ALLOCATED(FR_ICOL)) DEALLOCATE(FR_ICOL)
        ALLOCATE(FR_ICOL(LR),STAT=IERROR2)
        DO IP =1,NSPMD
         DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
           J0=NROW(NK)-IAD_RL(NK)+1
           DO L=IAD_RL(NK),IAD_RL(NK+1)-1
            J=L+J0
            FR_ICOL(L)=ICOL(J,NK)
           ENDDO
         ENDDO
        ENDDO
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  UPD_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_K_EIG                     stub/imp_k_eig.F              
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        CONDENS_IND                   source/implicit/upd_glob_k.F  
Chd|        SPMD_MAX_IV                   source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|        INTAB0                        source/implicit/imp_fsa_inv.F 
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE UPD_FR_K(
     1    IADK     ,JDIK     ,NDOF      ,IKC      ,IDDL     ,
     2    INLOC    ,FR_ELEM  ,IAD_ELEM  ,NDDL     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "impl1_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*) ,JDIK(*),IDDL(*),INLOC(*),NDOF(*)
      INTEGER
     .   IKC(*), FR_ELEM(*),IAD_ELEM(2,*),NDDL
C     REAL ou REAL*8
C-----------------------------------------------
C   External function
C-----------------------------------------------
      INTEGER INTAB0
      EXTERNAL INTAB0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NK,IP,L,IFIX,II,IAD2,IJ,IND,N_FR,NB,
     .        K,ND,NZZK,NJ,NZZ,IAD,JAD,ID,JD,IDK,NC,IDF,IKCFR(NDDLFR)
      INTEGER IIC(NDFRMAX),IDDLM(NUMNOD),NDN(NSPMD),NZN(NSPMD)
      INTEGER, DIMENSION(:),ALLOCATABLE :: ITAG
      my_real
     .        S1,STMP
C
      DDLP0=0
      DDLP1=NDDL
C
      STMP = ZERO
      IF (NDDLFR.GT.0) THEN
       N_FR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
       ALLOCATE(ITAG(N_FR))
       ITAG = 2
       DO NK=1,N_FR
        NB=-IFRLOC(NK)
        IF (NB.GT.0) ITAG(NB) = ITAG(NB) + 1
       ENDDO
      ENDIF
C
      IF (NDDLFR.GT.0) THEN
C-------if not the same IKC (e.g. RBE3 Ns)      
c       IKCFR (before condensation) for comm IKC
       IAD2 = 0
       DO IP =1,NSPMD
        DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         N=FR_ELEM(NK)
         ID=IDDLFR(NK)+IAD2
         IDK=IDDL(N)
         DO J=1,NDOF(N)
          IKCFR(ID+J)=IKC(IDK+J)
         ENDDO
        ENDDO
        IAD2 = IAD2 + ND_FR(IP)
       ENDDO
       CALL SPMD_MAX_IV(IKCFR)
       IAD2 = 0
      IAD = 0
      JAD = 0
      II = 0
      IJ = 0
      DO IP =1,NSPMD
       IFIX = 0
       DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
        N=FR_ELEM(NK)
        ID=IDDLFR(NK)
        IDK=IDDL(N)
        IND = IFIX
        DO J=1,NDOF(N)
         IIC(ID+J)=IKCFR(ID+IAD2+J)
         IF (IIC(ID+J).NE.0) IFIX=IFIX+1
        ENDDO
C
        NB=-IFRLOC(NK)
        IF (NB.LT.0) NB = NK
        S1 = ONE/ITAG(NB)
        STMP = STMP + S1*(IFIX-IND)
C
       ENDDO
C -----update---condense-IKC>=1------------------------------------
       ND = ND_FR(IP)
       NZZ=IADFR(IAD+ND_FR(IP)+1)-IADFR(IAD+1)
       IAD2 = IAD2 + ND_FR(IP)
       IF (IFIX.GT.0) THEN
        NZZK=NZZ
        CALL CONDENS_IND(ND_FR(IP),NZZK,IADFR(IAD+1),JDIFR(JAD+1),IIC)
        II = II + IFIX
        IJ = IJ + NZZ-NZZK
       ENDIF
       IAD = IAD + ND + 1
       JAD = JAD + NZZ
       NDN(IP)=II
       NZN(IP)=IJ
      ENDDO
      DEALLOCATE(ITAG)
      ENDIF  ! (NDDLFR.GT.0)
      CALL SPMD_SUM_S(STMP)
      NDDLFRB1 = NDDLFRB-INT(STMP)
      IF (NDDLFR.LE.0) RETURN
C-------s'il y a condensation-------
      IF (NDN(NSPMD).GT.0) THEN
       IAD = ND_FR(1) + 1
       DO IP =2,NSPMD
        II = NDN(IP-1)
        IF (II.GT.0) THEN
         II = II+IAD
         DO I=1,ND_FR(IP)+1
          IADFR(IAD+I)=IADFR(II+I)
         ENDDO
        ENDIF
        IAD = IAD + ND_FR(IP) + 1
       ENDDO
       IAD = ND_FR(1) + 1
       JAD = IADFR(IAD)-IADFR(1)
       DO IP =2,NSPMD
        IJ = NZN(IP-1)
        IF (IJ.GT.0) THEN
         IJ = IJ+JAD
         DO J=IADFR(IAD+1),IADFR(IAD+1+ND_FR(IP))-1
          JDIFR(J+JAD)= JDIFR(J+IJ)
         ENDDO
        ENDIF
        NZZK=IADFR(IAD+ND_FR(IP)+1)-IADFR(IAD+1)
        IAD = IAD + ND_FR(IP) + 1
        JAD = JAD + NZZK
       ENDDO
      ENDIF
C
        IFIX=0
       DO N = 1,NUMNOD
         I=INLOC(N)
         IDDLM(I)=IDDL(I)-IFIX
         DO J=1,NDOF(I)
          ND = IDDL(I)+J
          IF (IKC(ND).NE.0) IFIX=IFIX+1
         ENDDO
       ENDDO
C
C-------FR2K-------
      IAD2 = 0
      DO IP =1,NSPMD
       IFIX=0
       DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
        N=FR_ELEM(NK)
        ID=IDDLFR(NK)+IAD2-IFIX
        IDK=IDDL(N)
        IND=0
        DO J=1,NDOF(N)
         IF (IKC(IDK+J).LT.1) THEN
          IND=IND+1
          IFR2K(ID+IND)=IDDLM(N)+IND
         ELSE
          IFIX=IFIX+1
         ENDIF
        ENDDO
       ENDDO
       IAD2 = IAD2 + ND_FR(IP)
      ENDDO
      LEN_V = IAD2
C
       LEN_K = 0
      IF (IPREC.GT.2) THEN
       IAD = 0
       IAD2 = 0
       JAD = 0
       DO IP =1,NSPMD
        DO I=1,ND_FR(IP)
         ID = I+IAD
         II = IFR2K(I+IAD2)
         DO J=IADFR(ID),IADFR(ID+1)-1
          JD = J + JAD
          K=JDIFR(JD)
          IJ = IFR2K(K+IAD2)
          IF (II.GT.IJ) THEN
           NC = IADK(II+1)-IADK(II)
           N=INTAB0(NC,JDIK(IADK(II)),IJ)
           IF (N.GT.0) THEN
            JFR2K(JD)=N+IADK(II)-1
           ELSE
            write(*,*)'index error in UPD_FR_K I>J',IJ,ip,nc
c           write(*,*)'i,j,ik,iad=',i,k,ii,iad
           ENDIF
          ELSE
           NC = IADK(IJ+1)-IADK(IJ)
           N=INTAB0(NC,JDIK(IADK(IJ)),II)
           IF (N.GT.0) THEN
            JFR2K(JD)=N+IADK(IJ)-1
           ELSE
            write(*,*)'index error in UPD_FR_K J>I',II,ip,nc
c           write(*,*)'i,j,ik,iad=',i,k,ij,iad
           ENDIF
          ENDIF
         ENDDO
        ENDDO
        NZZK=IADFR(IAD+ND_FR(IP)+1)-IADFR(IAD+1)
        IAD = IAD + ND_FR(IP) +1
        IAD2 = IAD2 + ND_FR(IP)
        JAD = JAD + NZZK
       ENDDO
       LEN_K = IAD2+JAD
       IAD2 = 0
       DO IP =1,ISPMD
        DO J =1,ND_FR(IP)
         DDLP0 = MAX(DDLP0,IFR2K(IAD2+J))
        ENDDO
        IAD2=IAD2+ND_FR(IP)
       ENDDO
C
       DO IP =ISPMD+2,NSPMD
        IFIX=0
        DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         N=FR_ELEM(NK)
         ID=IDDLFR(NK)+IAD2-IFIX
         IDK=IDDL(N)
         DO J=1,NDOF(N)
          IF (IKC(IDK+J).GT.0) IFIX=IFIX+1
         ENDDO
         IF(IFRLOC(NK).GT.0) THEN
          IND=0
          DO J=1,NDOF(N)
           IF (IKC(IDK+J).LT.1) THEN
            IND=IND+1
            DDLP1=IFR2K(ID+IND)-1
            GOTO 100
           ENDIF
          ENDDO
         ENDIF
        ENDDO
        IAD2 = IAD2 + ND_FR(IP)
       ENDDO
      ENDIF
c      write(*,*)'LEN_V,LEN_K,ISPMD=',LEN_V,LEN_K,ISPMD
c      write(*,*)'ND_FR=',(ND_FR(J),J=1,NSPMD),ISPMD
 100  CONTINUE
C
      RETURN
      END
Chd|====================================================================
Chd|  FR_DLFT                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_CHECM0                    source/implicit/imp_solv.F    
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE FR_DLFT(NDDL,IDLFT0,IDLFT1)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDLFT0,IDLFT1,NDDL
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L,IP,J
C
C
       IDLFT0 = DDLP0
       IDLFT1 = DDLP1
C
      RETURN
      END
CCgw|     DIM_GLOB_K                       /implicit/ind_glob_k.F
Chd|====================================================================
Chd|  SET_IKIN2G                    source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        DIM_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SET_IKIN2G(NKINE,INLOC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NKINE,INLOC(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,IERROR1
C
      IF (NKINE.LE.0) RETURN
      IF(ALLOCATED(IKIN2G)) DEALLOCATE(IKIN2G)
      ALLOCATE(IKIN2G(NKINE),STAT=IERROR1)
      DO I =1,NUMNOD
       N=INLOC(I)
       IF (N.GT.0) IKIN2G(N)=I
      ENDDO
C
      RETURN
      END
CCgw|     IND_GLOB_K                       /implicit/ind_glob_k.F
Chd|====================================================================
Chd|  GET_IKIN2G                    source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE GET_IKIN2G(NKINE,INK,ILOC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NKINE,ILOC(*),INK
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NRB
C
      NRB=NKINE-INK
      DO N =1,NRB
       I=IKIN2G(N)
       ILOC(I)=N+INK
      ENDDO
      DO N =NRB+1,NKINE
       I=IKIN2G(N)
       ILOC(I)=N-NRB
      ENDDO
C
      RETURN
      END
CCgw|     IND_GLOB_K                       /implicit/ind_glob_k.F
Chd|====================================================================
Chd|  ZERO_IKIN2G                   source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE ZERO_IKIN2G(NKINE,ILOC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NKINE,ILOC(*),INK
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
C
      DO N =1,NKINE
       I=IKIN2G(N)
       ILOC(I)=0
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  IND_KINE_KP                   source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        REORDER_J                     source/implicit/ind_glob_k.F  
Chd|        REORDER_L                     source/implicit/ind_glob_k.F  
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IND_KINE_KP(
     1    NROWK     ,ICOK      ,ICOKM     ,NNMAX     ,NKMAX     ,
     2    NKINE     ,INK       ,IKPAT     ,IDDL      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX,NKMAX,NROWK(*),NKINE,INK,IKPAT
      INTEGER ICOK(NNMAX,*),ICOKM(NKMAX,*),IDDL(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,K,NL,NK,NJ,NRB
c----------------------
      NRB=NKINE-INK
      IF (IKPAT.EQ.0) THEN
       DO NK =1,NRB
        J=IKIN2G(NK)
        CALL REORDER_J(NROWK(NK+INK),ICOKM(1,NK),J,IDDL)
       ENDDO
       DO NK =NRB+1,NKINE
        J=IKIN2G(NK)
        NJ=NK-NRB
        CALL REORDER_J(NROWK(NJ),ICOK(1,NJ),J,IDDL)
       ENDDO
      ELSE
       DO NK =1,NRB
        J=IKIN2G(NK)
        CALL REORDER_L(NROWK(NK+INK),ICOKM(1,NK),J,IDDL)
       ENDDO
       DO NK =NRB+1,NKINE
        J=IKIN2G(NK)
        NJ=NK-NRB
        CALL REORDER_L(NROWK(NJ),ICOK(1,NJ),J,IDDL)
       ENDDO
      ENDIF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  TAG_INTM11                    source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKI                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TAG_INTM11(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECTS   ,IRECTM   ,
     2    ILOC      ,N_IMPN    ,NSN         )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECTS(2,*),IRECTM(2,*),
     .        ILOC(*),N_IMPN,NSN
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,N1,N2,NE,IG,M1,M2
C-----------------------------------------------
       DO I = 1, JLT
C--------secnd node-----
        IG = NS_IMP(I)
       IF (IG.GT.NSN) THEN
        NE=NE_IMP(I)
         M1=IRECTM(1,NE)
         M2=IRECTM(2,NE)
         IF (ILOC(M1).EQ.0) THEN
          N_IMPN=N_IMPN+1
          ILOC(M1)=N_IMPN
         ENDIF
         IF (ILOC(M2).EQ.0) THEN
          N_IMPN=N_IMPN+1
          ILOC(M2)=N_IMPN
         ENDIF
       ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  MAV_LTFR                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE MAV_LTFR(V     ,W     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .  W(*), V(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ID
      my_real
     .   L_K
C-----------------------------
      DO I=1,NDDL_SI
       FSI(I) = ZERO
      ENDDO
      DO I=1,NDDL_SI
       DO J =IAD_SI(I),IAD_SI(I+1)-1
        K =JDI_SI(J)
        L_K = LT_SI(J)
        FSI(I) = FSI(I) + L_K*V(K)
        W(K) = W(K) + L_K*USI(I)
      ENDDO
      ENDDO
C
      DO I=1,NDDL_SL
       ID = IDDL_SL(I)
       W(ID) = W(ID) + DIAG_SL(I)*V(ID)
       DO J =IAD_SS(I),IAD_SS(I+1)-1
        K =JDI_SL(J)
        L_K = LT_SL(J)
        W(ID) = W(ID) + L_K*V(K)
        W(K) = W(K) + L_K*V(ID)
       ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  MAV_LTFR_GPU                  source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        FR_MATV_GPU                   source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE MAV_LTFR_GPU(V     ,W     ,NINDEX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDEX(*)
C     REAL ou REAL*8
      my_real
     .  W(*), V(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ID,KK,II
      my_real
     .   L_K
C-----------------------------
      DO I=1,NDDL_SI
       FSI(I) = ZERO
      ENDDO
      DO I=1,NDDL_SI
       DO J =IAD_SI(I),IAD_SI(I+1)-1
        K =JDI_SI(J)
        KK=NINDEX(K)
        L_K = LT_SI(J)
        FSI(I) = FSI(I) + L_K*V(KK)
        W(KK) = W(KK) + L_K*USI(I)
       ENDDO
      ENDDO
C
      DO I=1,NDDL_SL
       ID = IDDL_SL(I)
       II = NINDEX(ID)
       W(II) = W(II) + DIAG_SL(I)*V(II)
       DO J =IAD_SS(I),IAD_SS(I+1)-1
        K =JDI_SL(J)
        KK=NINDEX(K)
        L_K = LT_SL(J)
        W(II) = W(II) + L_K*V(KK)
        W(KK) = W(KK) + L_K*V(II)
       ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  IND_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        REORDER_FR                    source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_DIM                     source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_DIM11                   source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_DIM24                   source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_IND                     source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_IND11                   source/mpi/implicit/imp_fri.F 
Chd|        ROWFR_IND24                   source/mpi/implicit/imp_fri.F 
Chd|        SET_IND_FR                    source/mpi/implicit/imp_fri.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IND_FRKD(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     2    IDDL      ,IKC       ,NDOF      ,NSREM     ,IND_IMP   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),NUM_IMP(*),NS_IMP(*),
     .        NE_IMP(*),NSREM,IND_IMP(*)
      INTEGER IDDL(*),IKC(*),NDOF(*)
C     REAL ou REAL*8
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIN,NTY,NROW(NSREM)
      INTEGER I,J,K,L,NDOFI,N,IAD,INSV11,NRTS,
     .        INSV,NSN,NKC,J1,ND,IER1,NNMAX
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ICOL
C------------------------------------------------------------
C--------dim Nrow----
       DO N =1,NSREM
        NROW(N)=0
       ENDDO
C
       IAD=1
       DO NIN=1,NINTER
        NTY   =IPARI(7,NIN)
        IF(NTY.EQ.5) IAD=IAD+NUM_IMP(NIN)
       ENDDO
       DO NIN=1,NINTER
        NSN   =IPARI(5,NIN)
        NTY   =IPARI(7,NIN)
        IF(NTY.EQ.3)THEN
        ELSEIF(NTY.EQ.4)THEN
        ELSEIF(NTY.EQ.5)THEN
        ELSEIF(NTY.EQ.6)THEN
        ELSEIF(NTY.EQ.7.OR.NTY.EQ.10)THEN
C
         CALL ROWFR_DIM(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                  INTBUF_TAB(NIN)%IRECTM,NROW   ,NSN   ,NIN     )
         IAD=IAD+NUM_IMP(NIN)
        ELSEIF(NTY.EQ.24)THEN
C
         CALL ROWFR_DIM24(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                  INTBUF_TAB(NIN)%IRECTM,NROW   ,NSN   ,NIN     ,
     .                   IND_IMP  ,INTBUF_TAB(NIN)%NVOISIN)
         IAD=IAD+NUM_IMP(NIN)
        ELSEIF(NTY.EQ.11)THEN
         NRTS   =IPARI(3,NIN)
         CALL ROWFR_DIM11(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                    INTBUF_TAB(NIN)%IRECTM, NROW ,NRTS ,NIN      )
         IAD=IAD+NUM_IMP(NIN)
        ENDIF
       ENDDO
C
       NNMAX=0
       DO N =1,NSREM
        NNMAX=MAX(NNMAX,NROW(N))
        NROW(N)=0
       ENDDO
       ALLOCATE(ICOL(NNMAX,NSREM),STAT=IER1)
C--------ind Nrow----
       IAD=1
       DO NIN=1,NINTER
        NTY   =IPARI(7,NIN)
        IF(NTY.EQ.5) IAD=IAD+NUM_IMP(NIN)
       ENDDO
       DO NIN=1,NINTER
        NSN   =IPARI(5,NIN)
        NTY   =IPARI(7,NIN)
        IF(NTY.EQ.3)THEN
        ELSEIF(NTY.EQ.4)THEN
        ELSEIF(NTY.EQ.5)THEN
        ELSEIF(NTY.EQ.6)THEN
        ELSEIF(NTY.EQ.7.OR.NTY.EQ.10)THEN
C
         CALL ROWFR_IND(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                  INTBUF_TAB(NIN)%IRECTM,NROW   ,ICOL  ,NNMAX   ,
     .                  NSN     ,NIN       )
         IAD=IAD+NUM_IMP(NIN)
        ELSEIF(NTY.EQ.24)THEN
C
         CALL ROWFR_IND24(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                  INTBUF_TAB(NIN)%IRECTM,NROW   ,ICOL  ,NNMAX   ,
     .                  NSN     ,NIN  ,IND_IMP ,INTBUF_TAB(NIN)%NVOISIN)
         IAD=IAD+NUM_IMP(NIN)
        ELSEIF(NTY.EQ.11)THEN
         NRTS   =IPARI(3,NIN)
         CALL ROWFR_IND11(NUM_IMP(NIN),NS_IMP(IAD),NE_IMP(IAD),
     .                    INTBUF_TAB(NIN)%IRECTM, NROW ,ICOL  ,NNMAX   ,
     .                    NRTS   ,NIN      )
         IAD=IAD+NUM_IMP(NIN)
        ENDIF
       ENDDO
C
        DO N =1,NSREM
         CALL REORDER_FR(NROW(N),ICOL(1,N),IDDL)
        ENDDO
        CALL SET_IND_FR(
     1    NSREM     ,IDDL      ,NDOF      ,NROW      ,ICOL      ,
     2    NNMAX     )
C
        DEALLOCATE(ICOL)
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ROWFR_DIM                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE ROWFR_DIM(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT     ,NROW      ,
     2    NSN       ,NIN         )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NROW(*),NSN,NIN
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NE,IG,NS
C-----------------------------------------------
       DO I = 1, JLT
        IG = NS_IMP(I)-NSN
        IF (IG.GT.0) THEN
             NS=IND_INT(NIN)%P(IG)
         NE=NE_IMP(I)
         IF (IRECT(3,NE).EQ.IRECT(4,NE)) THEN
          NROW(NS)=NROW(NS)+3
         ELSE
          NROW(NS)=NROW(NS)+4
         ENDIF
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ROWFR_DIM11                   source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE ROWFR_DIM11(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECTM    ,NROW      ,
     2    NSN       ,NIN         )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECTM(2,*),NROW(*),NSN,NIN
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NE,IG,NS1,NS2
C-----------------------------------------------
       DO I = 1, JLT
        IG = NS_IMP(I)-NSN
        IF (IG.GT.0) THEN
             NS1=IND_INT(NIN)%P(IG)
             NS2=NS1+1
         NE=NE_IMP(I)
         NROW(NS1)=NROW(NS1)+2
         NROW(NS2)=NROW(NS2)+2
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ROWFR_IND                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE ROWFR_IND(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT     ,NROW      ,
     2    ICOL      ,NNMAX     ,NSN         ,NIN       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NROW(*),
     .        ICOL(NNMAX,*),NSN,NIN
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NE,IG,NS,NM
C-----------------------------------------------
       DO I = 1, JLT
        IG = NS_IMP(I)-NSN
        IF (IG.GT.0) THEN
             NS=IND_INT(NIN)%P(IG)
         NE=NE_IMP(I)
         DO J = 1,4
          NM = IRECT(J,NE)
          CALL REORDER_A(NROW(NS),ICOL(1,NS),NM)
         ENDDO
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ROWFR_IND11                   source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE ROWFR_IND11(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECTM    ,NROW      ,
     2    ICOL      ,NNMAX     ,NSN       ,NIN         )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECTM(2,*),NROW(*),
     .        ICOL(NNMAX,*),NSN,NIN
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NE,IG,NS1,NS2,NM1,NM2
C-----------------------------------------------
       DO I = 1, JLT
        IG = NS_IMP(I)-NSN
        IF (IG.GT.0) THEN
             NS1=IND_INT(NIN)%P(IG)
             NS2=NS1+1
         NE=NE_IMP(I)
         NM1=IRECTM(1,NE)
         NM2=IRECTM(2,NE)
         CALL REORDER_A(NROW(NS1),ICOL(1,NS1),NM1)
         CALL REORDER_A(NROW(NS1),ICOL(1,NS1),NM2)
         CALL REORDER_A(NROW(NS2),ICOL(1,NS2),NM1)
         CALL REORDER_A(NROW(NS2),ICOL(1,NS2),NM2)
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ROWFR_DIM24                   source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        I24MSEGV                      source/implicit/ind_glob_k.F  
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE ROWFR_DIM24(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT     ,NROW      ,
     2    NSN       ,NIN       ,SUBTRIA     ,NVOISIN   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NROW(*),NSN,NIN,
     +        SUBTRIA(*),NVOISIN(8,*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NE,IG,NS,IRTLM(4),NEI
C-----------------------------------------------
       DO I = 1, JLT
        IG = NS_IMP(I)-NSN
        IF (IG.GT.0) THEN
             NS=IND_INT(NIN)%P(IG)
         NE=NE_IMP(I)
        IF (NE<0) THEN
         NEI=-NE
         CALL I24MSEGV(NE,IRTLM ,SUBTRIA(I),IRECT(1,NEI),NVOISIN(1,NEI))
        ELSE
         IRTLM(1:4) = IRECT(1:4,NE)
        END IF
         IF (IRTLM(3).EQ.IRTLM(4)) THEN
          NROW(NS)=NROW(NS)+3
         ELSE
          NROW(NS)=NROW(NS)+4
         ENDIF
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ROWFR_IND24                   source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        I24MSEGV                      source/implicit/ind_glob_k.F  
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE ROWFR_IND24(
     1    JLT       ,NS_IMP    ,NE_IMP      ,IRECT     ,NROW      ,
     2    ICOL      ,NNMAX     ,NSN         ,NIN       ,SUBTRIA     ,
     3    NVOISIN   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX
      INTEGER JLT,NS_IMP(*),NE_IMP(*),IRECT(4,*),NROW(*),
     .        ICOL(NNMAX,*),NSN,NIN,SUBTRIA(*),NVOISIN(8,*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NE,IG,NS,NM,IRTLM(4),NEI
C-----------------------------------------------
       DO I = 1, JLT
        IG = NS_IMP(I)-NSN
        IF (IG.GT.0) THEN
             NS=IND_INT(NIN)%P(IG)
         NE=NE_IMP(I)
         IF (NE<0) THEN
          NEI=-NE
         CALL I24MSEGV(NE,IRTLM ,SUBTRIA(I),IRECT(1,NEI),NVOISIN(1,NEI))
         ELSE
          IRTLM(1:4) = IRECT(1:4,NE)
         END IF
         DO J = 1,4
          NM = IRTLM(J)
          CALL REORDER_A(NROW(NS),ICOL(1,NS),NM)
         ENDDO
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  SET_IND_FR                    source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        ZERO1                         source/system/zero.F          
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE SET_IND_FR(
     1    NSREM     ,IDDL      ,NDOF      ,NROW      ,ICOL      ,
     2    NNMAX     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX
      INTEGER
     .   NSREM,IDDL(*),NDOF(*),NROW(*) ,ICOL(NNMAX,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,N,NL,NJ,NDOFI,NZ
      INTEGER IER1,IER2,IER3,IER4,IER5,IER6
c----------
       NDOFI = 3
       IF(ALLOCATED(IDDL_SI)) DEALLOCATE(IDDL_SI)
       ALLOCATE(IDDL_SI(NSREM),STAT=IER1)
       NDDL_SI =0
       DO N=1,NSREM
        IDDL_SI(N)=NDDL_SI
        NDDL_SI = NDDL_SI + NDOFI
       ENDDO
       NZ = 0
       DO N=1,NSREM
        DO K=1,NDOFI
C-------termes kn,nj-------
         DO J=1,NROW(N)
          DO L=1,NDOFI
           NZ = NZ+1
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C
       IF(ALLOCATED(IAD_SI)) DEALLOCATE(IAD_SI)
       ALLOCATE(IAD_SI(NDDL_SI+1),STAT=IER2)
       IF(ALLOCATED(JDI_SI)) DEALLOCATE(JDI_SI)
       ALLOCATE(JDI_SI(NZ),STAT=IER3)
       IF(ALLOCATED(LT_SI)) DEALLOCATE(LT_SI)
       ALLOCATE(LT_SI(NZ),STAT=IER4)
       CALL ZERO1(LT_SI,NZ)
C
       NL = 1
       NZ = 0
       IAD_SI(NL) = NZ+1
       DO N=1,NSREM
        DO K=1,NDOFI
C-------termes knj,n-------
          DO J=1,NROW(N)
           NJ = ICOL(J,N)
           DO L=1,NDOFI
            NZ = NZ+1
            JDI_SI(NZ) = IDDL(NJ)+L
           ENDDO
          ENDDO
          NL = NL +1
          IAD_SI(NL) = NZ+1
        ENDDO
       ENDDO
       NZ_SI = NZ
C-------save ICOL en forme compress-------
       NZ = 0
       DO N=1,NSREM
        NZ = NZ+NROW(N)
       ENDDO
       IF(ALLOCATED(IAD_SINR)) DEALLOCATE(IAD_SINR)
       ALLOCATE(IAD_SINR(NSREM+1),STAT=IER5)
       IF(ALLOCATED(JDI_SINR)) DEALLOCATE(JDI_SINR)
       ALLOCATE(JDI_SINR(NZ),STAT=IER6)
       NZ = 0
       IAD_SINR(1) = NZ+1
       DO N=1,NSREM
          DO J=1,NROW(N)
           NJ = ICOL(J,N)
           NZ = NZ+1
           JDI_SINR(NZ) = NJ
          ENDDO
          IAD_SINR(N+1) = NZ+1
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  REORDER_FR                    source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REORDER_FR(N,IC,IDDL)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N ,IC(*),IDDL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,IT,IIC,IMIN
C
C-----en ordre iddl croisante-----
      DO I =1,N
       IMIN=IDDL(IC(I))
       II=I
       DO J =I+1,N
        IIC = IDDL(IC(J))
        IF (IIC.LT.IMIN) THEN
         IMIN=IIC
         II=J
        ENDIF
       ENDDO
       IF (II.NE.I) THEN
        IT=IC(I)
        IC(I)=IC(II)
        IC(II)=IT
       ENDIF
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  JDIFRTOK                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE JDIFRTOK(ITOK  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITOK(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NJ
C
C-----pass IDDLI to IDDLJ  croisante-----
      DO I =1,NDDL_SI
       DO J =IAD_SI(I),IAD_SI(I+1)-1
        NJ = JDI_SI(J)
        JDI_SI(J) = ITOK(NJ)
       ENDDO
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRI                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        CP_IADD                       source/mpi/implicit/imp_fri.F 
Chd|        DIM_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|        DIM_FRKM1                     source/mpi/implicit/imp_fri.F 
Chd|        DIM_FVN                       source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRKS                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRSN                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FVKM                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FVKSL                     source/mpi/implicit/imp_fri.F 
Chd|        IMP_FVKSS                     source/mpi/implicit/imp_fri.F 
Chd|        IND_FVN                       source/mpi/implicit/imp_fri.F 
Chd|        IND_SLD                       source/mpi/implicit/imp_fri.F 
Chd|        INI_FRFD                      source/mpi/implicit/imp_fri.F 
Chd|        INI_FRKC                      source/mpi/implicit/imp_fri.F 
Chd|        INI_FRUD                      source/mpi/implicit/imp_fri.F 
Chd|        INI_KSI                       source/mpi/implicit/imp_fri.F 
Chd|        KIN_KML                       source/mpi/implicit/imp_fri.F 
Chd|        KIN_KSL                       source/mpi/implicit/imp_fri.F 
Chd|        SCOM_FRK                      source/mpi/implicit/imp_fri.F 
Chd|        SCOM_FRK1                     source/mpi/implicit/imp_fri.F 
Chd|        SCOM_FRUD                     source/mpi/implicit/imp_fri.F 
Chd|        SPMD_IFC1                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_IFRF                     source/mpi/implicit/imp_spmd.F
Chd|        TAG_INTML                     source/mpi/implicit/imp_fri.F 
Chd|        TRA_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_FRKD(
     1      NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2      IPARI     ,INTBUF_TAB,NINT2     ,IINT2     ,IDDL      ,
     3      IKC       ,NDOF      ,INLOC     ,NSREM     ,NSL       ,
     4      NBINTC    ,INTLIST   ,X         ,IBFV      ,
     5      LJ        ,SKEW      ,XFRAME    ,ISKEW     ,ICODT     ,
     6      A         ,UD        ,LB        ,IFDIS     ,URD       ,
     7      IDDLI     ,IRBE3     ,LRBE3     ,FRBE3     ,IRBE2     ,
     8      LRBE2    )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),NSREM  ,NSL,NBINTC,INTLIST(*)
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IDDL(*),IKC(*),NDOF(*),INLOC(*),
     .        IBFV(*),LJ(*),ISKEW(*),ICODT(*),IFDIS,IDDLI(*),
     .        IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*)
C     REAL ou REAL*8
      my_real
     .       X(3,*),UD(3,*),A(3,*),SKEW(*),XFRAME(*),
     .       LB(*),URD(3,*),FRBE3(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LSI,LSL,IDDLM(NUMNOD),ILOCP(NUMNOD)
      INTEGER I,J,K,L,N,NKC,J1,ND,LSG,
     .        NF_SL(NSL),NF_SI(NSREM),N_KINE,
     .        IAD_SLD(NSPMD+1),IAD_MLD(NSPMD+1),
     .        ISI ,ISL,NFV,LVSI,LVSL,LFSI,LFSL,NZ,
     .        IER1,IER2,IER3,IER4,IER5,IER6,IER7,LSI0
      my_real
     .       KSS(6,NSL)
      INTEGER, DIMENSION(:),ALLOCATABLE :: IAD_M,IKCSI,IKCSL,
     .                                      IFVSI,IFVSL
      my_real, DIMENSION(:),ALLOCATABLE :: KSI_FR,KSL_FR,
     .                                     UDSI,UDSL,FDSI
C------------------------------------------------------------
C---IKCSI(NSREM),IKCSL(NSL): initialised by NDD=sum(IKC(id+1,3)) or 21 for NDOF=0
C---they are updated just after by DIM_FRKM :nb of [K}3x3-> is sent in SL
      CALL SPMD_IFC1(NSREM  ,NSL ,KSS)
C-------iddlm-----------------
      NKC=0
      ISI =0
      LFSI = 0
      DO N =1,NUMNOD
       I=INLOC(N)
       IDDLM(I)=IDDL(I)-NKC
       DO J=1,NDOF(I)
        ND = IDDL(I)+J
        IF (IKC(ND).NE.0) NKC = NKC + 1
       ENDDO
      ENDDO
      IF (NSL.GT.0) THEN
       CALL IMP_FRSN(IPARI ,INTBUF_TAB  ,NBINTC,INTLIST)
       ALLOCATE(IAD_M(NSL+1))
      ENDIF
      DO I=1,NSREM
        NF_SI(I) = 0
      ENDDO
C------s'il y a des neouds kin---IKC_SL(NSL)>0->IKC_SI----
      CALL INI_FRKC(NSREM  ,NSL ,IKC ,NDOF ,IDDL)
C------com. [Ksm] secnds remotes dependant--s.KSI;r.KSL-IKC_SI,SL->nb [Ksm]--
      CALL DIM_FRKM(NSREM  ,NSL  ,LSI   ,LSL  )
      LSI0 = LSI
      LSG = LSI+LSL
C      due to if (NSREM+NSL>0) outside CALL SPMD_MAX_I(LSG)
      IF (LSG >= 0) THEN
       IF (IFDIS.GT.0) NFV = 0
       IF (LSI.GT.0) THEN
        ALLOCATE(KSI_FR(9*LSI),STAT=IER1)
        CALL INI_KSI(NSREM  ,KSI_FR ,IDDLI )
       ENDIF
       IF (LSL.GT.0) ALLOCATE(KSL_FR(9*LSL),STAT=IER2)
       CALL SCOM_FRK(KSI_FR,KSL_FR,LSI   ,LSL)
C--------condense KSL_FR------
       IF (LSL.GT.0) THEN
        CALL KIN_KSL(
     1    IPARI     ,INTBUF_TAB,NINT2     ,IINT2     ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    X         ,IBFV      ,LJ        ,SKEW      ,XFRAME    ,
     4    ISKEW     ,ICODT     ,NDOF      ,ILOCP     ,NSL       ,
     5    IAD_M     ,IRBE3     ,LRBE3     ,IRBE2     ,LRBE2     )
        LSI = IAD_M(NSL+1) -IAD_M(1)
        CALL IND_SLD(NSL  ,NDOF  ,KSS  )
C-------utilise tempo KSI_FR pour K_SL modifie-(neouds mains)----------------
        IF(ALLOCATED(KSI_FR)) DEALLOCATE(KSI_FR)
        ALLOCATE(KSI_FR(9*LSI),STAT=IER3)
        CALL UPD_KSL(
     1    IPARI     ,INTBUF_TAB,NINT2     ,IINT2     ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    X         ,IBFV      ,LJ        ,SKEW      ,XFRAME    ,
     4    ISKEW     ,ICODT     ,ILOCP     ,NSL       ,IAD_M     ,
     5    IDDL      ,IKC       ,NDOF      ,IDDLM     ,UD        ,
     6    A         ,LB        ,KSS       ,KSL_FR    ,KSI_FR    ,
     7    IRBE3     ,LRBE3     ,FRBE3     ,IRBE2     ,LRBE2     )
C-------s'il y a imposed Ud(secnd local), il faut renvoyer UDSL->UDSI---------
C-------NFV: nb of /IMPVEL node in SL, use ISLM(NFV) to save node num.
        IF (IFDIS.GT.0) THEN
         CALL DIM_FVN(
     1    NDOF      ,IDDL      ,IKC       ,ILOCP     ,NFV      )
         IF (NFV .GT. 0) THEN
          CALL IND_FVN(
     1    NDOF      ,IDDL      ,IKC       ,ILOCP     ,NFV     )
          ALLOCATE(UDSL(3*NFV))
C-------modif LB due au KSS et ini UDSL---put KSS dans K_SL-------
          CALL IMP_FVKSS(
     1    KSS      ,IDDL      ,IDDLM     ,IKC       ,NSL      ,
     2    UD       ,LB        ,NFV       ,UDSL      ,ILOCP    ,
     3    NDOF     )
         ENDIF
        ENDIF     ! IF (IFDIS.GT.0)
       ENDIF      ! IF (LSL.GT.0)
C-------dim pour re-copy KSI_FR (K_SL modifie) a K_SL (ne reste que IKC=0---------
       CALL DIM_FRKM1(NSREM,NSL  ,IDDL ,IKC ,NDOF  ,
     .                NF_SI,NF_SL,LSI  ,LSL ,ISI ,ISL )
C
       IF (LSL.GT.0) THEN
        IF(ALLOCATED(KSL_FR)) DEALLOCATE(KSL_FR)
        ALLOCATE(KSL_FR(9*LSL),IKCSL(3*ISL),STAT=IER5)
        IF (ISL>0) IKCSL=1
        CALL TRA_FRKM(NSL  ,IDDL ,IKC ,NDOF  ,IAD_M ,
     .               KSI_FR,KSL_FR,IKCSL)
        IF(ALLOCATED(KSI_FR)) DEALLOCATE(KSI_FR)
        IF (IFDIS.GT.0) ALLOCATE(IFVSL(ISL))
       ENDIF
       IF (LSI.GT.0) THEN
        IF(ALLOCATED(KSI_FR)) DEALLOCATE(KSI_FR)
        ALLOCATE(KSI_FR(9*LSI),IKCSI(3*ISI),STAT=IER6)
        IF (IFDIS.GT.0) ALLOCATE(IFVSI(ISI))
       ENDIF
C-------s: KSL_FR, r:KSI_FR :[Ksm] condense-----
       LSG = LSI+LSL
C       CALL SPMD_MAX_I(LSG)
       IF (LSG >= 0)
     .  CALL SCOM_FRK1(KSL_FR,KSI_FR,NF_SL,NF_SI,IKCSL,IKCSI)
       IF(ALLOCATED(KSL_FR)) DEALLOCATE(KSL_FR)
       IF (IFDIS.GT.0) THEN
C-------ini LVSI pour recevoire Ud(lvsi)-----
        CALL INI_FRUD(NSREM ,NSL   ,NFV   ,IFVSI ,IFVSL ,
     .                NF_SI ,NF_SL ,LVSI  )
        IF (LVSI.GT.0) ALLOCATE(UDSI(3*LVSI))
        CALL SCOM_FRUD(UDSL,UDSI,NF_SL,NF_SI,IFVSL,IFVSI)
C-------modifie LB(mi) du au [Ksm]^t ,Ud(si)-----
        IF (LVSI.GT.0) CALL IMP_FVKSL(
     1    IDDL      ,IDDLM     ,IKC       ,IFVSI      ,NF_SI     ,
     2    KSI_FR    ,LB        ,NSREM     ,UDSI       )
        IF(ALLOCATED(UDSI).AND.LVSI.GT.0) DEALLOCATE(UDSI)
       ENDIF
        IF(ALLOCATED(UDSL).AND.NFV.GT.0) DEALLOCATE(UDSL)
        IF(ALLOCATED(IFVSI).AND.ISI.GT.0) DEALLOCATE(IFVSI)
        IF(ALLOCATED(IFVSL).AND.ISL.GT.0) DEALLOCATE(IFVSL)
        IF(ALLOCATED(IKCSL).AND.ISL.GT.0) DEALLOCATE(IKCSL)
C
      ENDIF ! (LSG.GT.0)
      IF (NSL.GT.0) THEN
       DEALLOCATE(IAD_M)
       CALL IMP_FRKS(NSL  ,IDDL ,IKC ,NDOF  ,IDDLM,KSS  ,IAD_SLD )
      ENDIF
C
      IF (IFDIS.GT.0) NFV = 0
      IF (NSREM.GT.0) THEN
       DO N =1,NUMNOD
        ILOCP(N)=0
       ENDDO
C--------N_KINE: NML---LSI nb. noeuds kin-
       N_KINE=0
       CALL TAG_INTML(NSREM  ,ILOCP  ,N_KINE ,IDDL ,IKC ,NDOF ,LSI)
       IF (LSI.GT.0) THEN
        ALLOCATE(IAD_M(N_KINE+1))
        CALL KIN_KML(
     1    IPARI     ,INTBUF_TAB,NINT2     ,IINT2     ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    X         ,IBFV      ,LJ        ,SKEW      ,XFRAME    ,
     4    ISKEW     ,ICODT     ,NDOF      ,ILOCP     ,NSREM     ,
     5    IDDL      ,IKC       ,IAD_M     ,N_KINE    ,IRBE3     ,
     6    LRBE3     ,IRBE2     ,LRBE2     )
C-------utilise tempo KSL_FR pour KML_FR modifie-----------------
        LSL = IAD_M(N_KINE+1) -IAD_M(1)
        ALLOCATE(KSL_FR(9*LSL),STAT=IER7)
        CALL UPD_KML(
     1    IPARI     ,INTBUF_TAB,NINT2     ,IINT2     ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    X         ,IBFV      ,LJ        ,SKEW      ,XFRAME    ,
     4    ISKEW     ,ICODT     ,ILOCP     ,N_KINE    ,IAD_M     ,
     5    IDDL      ,IKC       ,NDOF      ,IDDLM     ,UD        ,
     6    A         ,KSL_FR    ,KSI_FR    ,NSREM     ,NF_SI     ,
     7    IDDLI     ,IRBE3     ,LRBE3     ,FRBE3     ,IRBE2     ,
     8    LRBE2     )
        IF (IFDIS.GT.0) THEN
         CALL DIM_FVN(
     1    NDOF      ,IDDL      ,IKC       ,ILOCP     ,NFV     )
         IF (NFV .GT. 0) THEN
          CALL IND_FVN(
     1    NDOF      ,IDDL      ,IKC       ,ILOCP     ,NFV     )
          DO I=1,NSREM
           DO J =1, MAX(1,NF_SI(I))
            LFSI = LFSI + 1
           ENDDO
          ENDDO
          ALLOCATE(FDSI(3*LFSI))
C-------modifie FDSI du au [Ksm] ,Ud(mi)-----
          CALL IMP_FVKM(
     1    KSI_FR   ,KSL_FR   ,IDDL      ,NDOF      ,IKC       ,
     2    ILOCP    ,IAD_M    ,NSREM     ,N_KINE    ,UD        ,
     3    FDSI     ,NF_SI    ,NFV       ,LFSI      ,IDDLI     )
         ENDIF
        ENDIF
       ENDIF
       LSI = LSI + ISI + LSI0
C-------actualise [Ksm] et stock dans LT_SI----
       CALL IMP_FRKM(
     1    NSREM     ,LSI       ,IKCSI   ,IKC     ,NDOF    ,
     2    IDDL      ,IDDLM     ,ILOCP   ,IAD_M   ,KSI_FR  ,
     3    KSL_FR    ,NF_SI     ,IAD_MLD ,IDDLI   )
      ENDIF
        IF(ALLOCATED(IAD_M)) DEALLOCATE(IAD_M)
        IF(ALLOCATED(KSI_FR)) DEALLOCATE(KSI_FR)
        IF(ALLOCATED(KSL_FR)) DEALLOCATE(KSL_FR)
       IF (IFDIS.GT.0) CALL INI_FRFD(NSREM ,NFV ,IKCSI,NF_SI ,FDSI )
       CALL CP_IADD(NSL ,NSREM,IAD_SLD  ,IAD_MLD)
       IF (IFDIS.GT.0) CALL SPMD_IFRF(LB )
C ------passe IAD_SL,IAD_SREM en ddl-----
        IF(ALLOCATED(IKCSI).AND.ISI.GT.0) DEALLOCATE(IKCSI)
        IF(ALLOCATED(FDSI).AND.LFSI.GT.0) DEALLOCATE(FDSI)
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  INI_FRKC                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        SPMD_ISR                      source/mpi/implicit/imp_spmd.F
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE INI_FRKC(NSREM  ,NSL    ,IKC    ,NDOF   ,IDDL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSREM  ,NSL    ,IKC(*)    ,NDOF(*)   ,IDDL(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,ID,NDD,IERROR1,IERROR2
C--------------------------------------------
        IF(ALLOCATED(IKC_SL)) DEALLOCATE(IKC_SL)
        ALLOCATE(IKC_SL(NSL),STAT=IERROR1)
        IF(ALLOCATED(IKC_SI)) DEALLOCATE(IKC_SI)
        ALLOCATE(IKC_SI(NSREM),STAT=IERROR2)
        DO I = 1, NSL
         N = ISL(I)
             ID = IDDL(N)
         NDD = 0
         DO J = 1 , MIN(3,NDOF(N))
           NDD = NDD + IKC(ID+J)
         ENDDO
         IF (NDOF(N).EQ.0) NDD = 21
         IKC_SL(I) = NDD
        ENDDO
C
        CALL SPMD_ISR(IAD_SL,IAD_SREM,IKC_SL,IKC_SI,NSL  ,NSREM  )
C
        IF (NSREM.EQ.0) NDDL_SI=0
        NDDL_SL=0
C
      RETURN
      END
Chd|====================================================================
Chd|  DIM_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        SPMD_ISR                      source/mpi/implicit/imp_spmd.F
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE DIM_FRKM(NSREM  ,NSL    ,SSIZE,RSIZE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSREM  ,NSL ,SSIZE,RSIZE
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID
C--------------------------------------------
C   -----renvoie Ksl,Ksm et recevoie Kss,Kslm a condenser
        SSIZE = 0
        DO I = 1, NSREM
         IF (IKC_SI(I).GT.0) THEN
C   -----nb de [K}3x3--------
          IKC_SI(I) = IAD_SINR(I+1)-IAD_SINR(I)
          SSIZE = SSIZE + IKC_SI(I)
         ENDIF
        ENDDO
        CALL SPMD_ISR(IAD_SREM,IAD_SL,IKC_SI,IKC_SL,NSREM,NSL  )
C
        RSIZE = 0
        DO I = 1, NSL
         IF (IKC_SL(I).GT.0) RSIZE = RSIZE + IKC_SL(I)
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  INI_KSI                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        GETFR_KIJ                     source/mpi/implicit/imp_fri.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE INI_KSI(NSREM  ,KSI  ,IDDL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSREM  ,IDDL(*)
C     REAL ou REAL*8
      my_real
     .  KSI(9,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,JD,NM,ND,IAD
C--------------------------------------------
        ND = 3
        IAD = 0
        DO I = 1, NSREM
         IF (IKC_SI(I).GT.0) THEN
          ID = IDDL_SI(I)
          DO J = IAD_SINR(I), IAD_SINR(I+1)-1
           NM = JDI_SINR(J)
           JD = IDDL(NM)
           IAD = IAD + 1
           CALL GETFR_KIJ( ID    ,JD   ,IAD_SI ,JDI_SI,LT_SI  ,
     1                     KSI(1,IAD),ND   ,ND    )
          ENDDO
         ENDIF
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        BCL_FRK                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDFR                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDFR2                     source/constraints/general/bcs/bc_imp0.F
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        FVL_FRK                       source/constraints/general/impvel/fv_imp0.F
Chd|        FV_UPDFR                      source/constraints/general/impvel/fv_imp0.F
Chd|        GET_IAD                       source/mpi/implicit/imp_fri.F 
Chd|        I2_FRK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2_FRK1                       source/interfaces/interf/i2_imp1.F
Chd|        INTABFR                       source/mpi/implicit/imp_fri.F 
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        PRERBE2FR                     source/constraints/general/rbe2/rbe2f.F
Chd|        PRERBE3FR                     source/constraints/general/rbe3/rbe3f.F
Chd|        RBE2_FRK                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_FR0                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_FRK                       source/constraints/general/rbody/rby_imp0.F
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPD_KSL(
     1    IPARI     ,INTBUF_TAB,NINT2     ,IINT2     ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    X         ,IBFV      ,LJ        ,SKEW      ,XFRAME    ,
     4    ISKEW     ,ICODT     ,INLOC     ,NSL       ,IAD_M     ,
     5    IDDL      ,IKC       ,NDOF      ,IDDLM     ,UD        ,
     6    A         ,B         ,KSS       ,KSL_FR    ,KSI_FR    ,
     7    IRBE3     ,LRBE3     ,FRBE3     ,IRBE2     ,LRBE2     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE IMP_RWL
      USE IMP_ASPC
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),IPARI(NPARI,*),NSL,IAD_M(*)
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IDDL(*),IKC(*),NDOF(*),INLOC(*),
     .        LJ(*),ISKEW(*),ICODT(*),IDDLM(*),IRBE3(NRBE3L,*),
     .        LRBE3(*),IRBE2(NRBE2L,*),LRBE2(*)
C     REAL ou REAL*8
      my_real
     .       X(3,*),SKEW(LSKEW,*),XFRAME(*),FRBE3(*)
      my_real
     .   UD(3,*),A(3,*),B(*) ,KSS(6,*),KSL_FR(9,*) ,KSI_FR(9,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,K,M,NS,NI,NSN,ILEV,IAD_M1(NSL+1),IS,
     .        JI,L,NNOD,NJ,ND,NL,ISK,IFM,J1,NM,ID,IAD0,IAD,IADS,
     .        I1,ICT,IFSS,IFSM,IDM(4),NR,JT(3),JR(3),IROT,
     .        IDLM(SLRBE3/2),IADR,NN,IRAD,IC,EID
      my_real
     .  EJ(3),KSM(9),KNM(9,4),KRM(9,4)
C----4 no more sufficient for RBE3: alloc dynamic only for IFSM=1
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: KNM3,KRM3
C----------Kss d'abord------------------------------
       IFSS =1
       IFSM=0
       IAD_M1(1) = 1
       DO N=1,NSL
        NL =0
        IF (IKC_SL(N).GT.0) THEN
         NR = IAD_SLNR(N+1)-IAD_SLNR(N)
         NL = MAX(1,NR)
        ENDIF
        IAD_M1(N+1) = IAD_M1(N)+NL
       ENDDO
C------int2----------------------------------
       DO I=NI2_FR,1,-1
        N=IFRS2(1,I)
        NI=IFRS2(2,I)
         JI=IPARI(1,N)
         NSN=IPARI(5,N)
         L=INTBUF_TAB(N)%IRTLM(NI)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3).EQ.INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
         ILEV  =IPARI(20,N)
C
        DO IS = 1,NSL
         IF (ISL(IS).EQ.NS) THEN
          NR = IAD_SLNR(IS+1)-IAD_SLNR(IS)
          DO K =1,NNOD
           NJ=INTBUF_TAB(N)%IRECTM(NL+K)
           CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(IS)),NJ,J)
           IF (J.GT.0) THEN
            IDM(K) = IDDL_SL(IAD_M1(IS)+J-1)
           ELSE
           ENDIF
          ENDDO
          IF (ILEV.EQ.1) THEN
           CALL I2_FRK1(INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%DPARA,X  ,ITAB ,
     .                  INTBUF_TAB(N)%NSV,
     1                  INTBUF_TAB(N)%IRTLM,IKC  ,NDOF  ,IDDL ,IDDLM,
     2                  IAD_SS ,JDI_SL ,DIAG_SL,LT_SL ,B    ,A      ,
     3                  KSS(1,IS),KSM  ,KNM   ,KRM  ,NI    ,
     4                  IDM    ,IFSS   ,IFSM   )
          ELSE
           CALL I2_FRK0(INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%CSTS,X    ,ITAB ,
     .                  INTBUF_TAB(N)%NSV,
     1                  INTBUF_TAB(N)%IRTLM,IKC  ,NDOF  ,IDDL ,IDDLM,
     2                  IAD_SS ,JDI_SL ,DIAG_SL,LT_SL ,B    ,A      ,
     3                  KSS(1,IS),KSM  ,KNM   ,KRM  ,NI    ,
     4                  IDM    ,IFSS   ,IFSM   )
          ENDIF
         ENDIF
        ENDDO
       ENDDO
C------RBE2-------
       DO I=NRBE2_FR,1,-1
        N=IFRS4(1,I)
        M=IRBE2(3,N)
        NS=IFRS4(2,I)
        ISK = IRBE2(7,N)
            IRAD =IRBE2(11,N)
        IC = IRBE2(4,N)
C--------remove ICR---
        IC =(IC/512)*512
        DO IS = 1,NSL
         IF (ISL(IS).EQ.NS) THEN
          NR = IAD_SLNR(IS+1)-IAD_SLNR(IS)
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(IS)),M ,J)
           IF (J.GT.0) THEN
            IDM(1) = IDDL_SL(IAD_M1(IS)+J-1)
           ELSE
           ENDIF
          CALL PRERBE2FR(IC    ,JT  ,JR   )
          CALL RBE2_FRK(NS    ,M    ,X     ,ISK  ,SKEW(1,ISK) ,
     1                  IRAD  ,NDOF  ,IDDL ,JT   ,JR      ,
     2                  IAD_SS,JDI_SL,DIAG_SL,LT_SL ,B    ,
     3                  A    ,KSS(1,IS),KSM   ,KNM   ,KRM  ,
     4                  IDM(1),IFSS,IFSM )
         ENDIF
        ENDDO
       ENDDO
C------RBE3----------------------------------
       DO I=1,NRBE3_FR
        N=IFRS3(I)
        IAD=IRBE3(1,N)
        NS=IRBE3(3,N)
        NNOD=IRBE3(5,N)
        IROT =IRBE3(6,N)
        EID=IRBE3(2,N)
        IADS = SLRBE3/2+IAD
C
        CALL PRERBE3FR(IRBE3 ,N    ,JT  ,JR   )
        DO IS = 1,NSL
         IF (ISL(IS).EQ.NS) THEN
          NR = IAD_SLNR(IS+1)-IAD_SLNR(IS)
          DO K =1,NNOD
           NJ = LRBE3(IAD+K)
           CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(IS)),NJ,J)
           IF (J.GT.0) THEN
            IDLM(K) = IDDL_SL(IAD_M1(IS)+J-1)
           ELSE
           ENDIF
          ENDDO
          CALL RBE3_FR0(NS    ,NNOD  ,LRBE3(IAD+1)  ,X     ,IROT   ,
     2                  JT    ,JR    ,FRBE3(6*IAD+1)  ,SKEW  ,IKC   ,
     3                  NDOF  ,IAD_SS ,JDI_SL ,DIAG_SL,LT_SL ,
     2                  KSS(1,IS),KSM    ,KNM  ,KRM    ,IDLM  ,
     3                  IFSS   ,IFSM    ,ITAB  ,LRBE3(IADS+1),EID)
         ENDIF
        ENDDO
       ENDDO
C------Rigid bodies-------
       DO I=NRB_FR,1,-1
        M=IFRSR(1,I)
        NS=IFRSR(2,I)
        DO IS = 1,NSL
         IF (ISL(IS).EQ.NS) THEN
          NR = IAD_SLNR(IS+1)-IAD_SLNR(IS)
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(IS)),M ,J)
           IF (J.GT.0) THEN
            IDM(1) = IDDL_SL(IAD_M1(IS)+J-1)
           ELSE
           ENDIF
          CALL RBY_FRK(NS   ,M    ,X     ,ITAB ,IKC  ,
     1                NDOF  ,IDDL ,IDDLM,IAD_SS,JDI_SL,
     2                DIAG_SL,LT_SL ,B    ,A    ,KSS(1,IS),
     3                KSM   ,KNM   ,KRM  ,IDM(1),IFSS,IFSM )
         ENDIF
        ENDDO
       ENDDO
C
      DO I1 = 1,NBC_FR
        N = IBC_FR(1,I1)
        ISK= IBC_FR(2,I1)
        ICT= IBC_FR(3,I1)
        DO IS = 1,NSL
         IF (ISL(IS).EQ.N) THEN
          NR = IAD_SLNR(IS+1)-IAD_SLNR(IS)
          IF (NR.GT.0) THEN
           CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(IS)),N ,J)
            IDM(1) = IDDL_SL(IAD_M1(IS)+J-1)
          ELSE
            IDM(1) = IDDL_SL(IAD_M1(IS))
          ENDIF
          CALL BCL_FRK(N      ,IDDL  ,IDDLM  ,ICT  ,ISK   ,
     1                 SKEW   ,IKC   ,IAD_SS,JDI_SL,DIAG_SL,
     2                 LT_SL  ,B     ,A     ,KSS(1,IS),KSM   ,
     3                 IDM(1) ,IFSS  ,IFSM )
        ENDIF
       ENDDO
      ENDDO
C
      DO I1 = 1,NSPC_FR
        N = ISPC_FR(I1)
        I = IN_SPC(N)
            IAD = 6*(N-1)+1
            NN = IC_SPC(N)
        DO IS = 1,NSL
         IF (ISL(IS).EQ.I) THEN
          NR = IAD_SLNR(IS+1)-IAD_SLNR(IS)
          IF (NR.GT.0) THEN
           CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(IS)),I ,J)
            IDM(1) = IDDL_SL(IAD_M1(IS)+J-1)
          ELSE
            IDM(1) = IDDL_SL(IAD_M1(IS))
          ENDIF
              IF (NN.EQ.1) THEN
               EJ(1)=SKEW_SPC(IAD)
               EJ(2)=SKEW_SPC(IAD+1)
               EJ(3)=SKEW_SPC(IAD+2)
           CALL L_DIR(EJ,J)
           CALL BC_UPDFR(I     ,IDDL  ,EJ     ,J     ,IDDLM  ,
     1                   IKC   ,IAD_SS,JDI_SL,DIAG_SL,LT_SL ,
     2                   B     ,A     ,KSS(1,IS),KSM   ,IDM(1),
     3                   IFSS  ,IFSM  )
          ELSE
           CALL BC_UPDFR2(I     ,IDDL  ,SKEW_SPC(IAD),SKEW_SPC(IAD+3),
     1                   IDDLM  ,IKC   ,IAD_SS,JDI_SL,DIAG_SL,LT_SL ,
     2                   B     ,A     ,KSS(1,IS),KSM   ,IDM(1),
     3                   IFSS  ,IFSM  )
              END IF
        ENDIF
       ENDDO
      ENDDO
C
      IF (NFX_FR.GT.0) THEN
       DO L=1,NFX_FR
        I = IFX_FR(1,L)
        J1 = IFX_FR(2,L)
        N=IABS(IBFV(1,I))
        DO IS = 1,NSL
         IF (ISL(IS).EQ.N) THEN
          NR = IAD_SLNR(IS+1)-IAD_SLNR(IS)
          IF (NR.GT.0) THEN
           CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(IS)),N ,J)
            IDM(1) = IDDL_SL(IAD_M1(IS)+J-1)
          ELSE
            IDM(1) = IDDL_SL(IAD_M1(IS))
          ENDIF
          CALL FVL_FRK(J1     ,I      ,IBFV  ,SKEW    ,XFRAME,
     1                 IDDL    ,IDDLM  ,IKC   ,IAD_SS  ,JDI_SL,
     2                 DIAG_SL ,LT_SL ,UD    ,B      ,A     ,
     3                 KSS(1,IS),KSM   ,IDM(1) ,IFSS  ,IFSM  )
         ENDIF
        ENDDO
       ENDDO
      ENDIF
C
      DO L = 1,NRW_FR
        I = IRW_FR(L)
        N=IN_RWL(I)
        EJ(1)=NOR_RWL(1,I)
        EJ(2)=NOR_RWL(2,I)
        EJ(3)=NOR_RWL(3,I)
        CALL L_DIR(EJ,J1)
        DO IS = 1,NSL
         IF (ISL(IS).EQ.N) THEN
          NR = IAD_SLNR(IS+1)-IAD_SLNR(IS)
          IF (NR.GT.0) THEN
           CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(IS)),N ,J)
            IDM(1) = IDDL_SL(IAD_M1(IS)+J-1)
          ELSE
            IDM(1) = IDDL_SL(IAD_M1(IS))
          ENDIF
          CALL FV_UPDFR(N     ,EJ    ,J1    ,IDDL  ,IDDLM    ,
     1                 IKC    ,IAD_SS,JDI_SL,DIAG_SL,LT_SL   ,
     2                 UD     ,B     ,A     ,KSS(1,IS),KSM   ,
     3                 IDM(1) ,IFSS  ,IFSM)
        ENDIF
        ENDDO
      ENDDO
C----------Ksm ------------------------------
       IFSS = 0
       IFSM = 1
       IAD_M1(1) = 0
       DO N=1,NSL
        IAD_M1(N+1) = IAD_M1(N)+IKC_SL(N)
       ENDDO
C------cp KSL_FR dans KSI_FR-(pour des kin sans modif-----
       DO I = 1,NSL
         NS = ISL(I)
         DO J = 1,IKC_SL(I)
           IAD = IAD_M1(I)+J
           CALL CP_REAL(9,KSL_FR(1,IAD),KSM)
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,NS   ,I  ,
     1                  J     ,IKC_SL(I),NDOF ,IAD   )
           IF (IAD.GT.0) CALL CP_REAL(9,KSM,KSI_FR(1,IAD))
         ENDDO
       ENDDO
C
       DO I=NI2_FR,1,-1
        N=IFRS2(1,I)
        NI=IFRS2(2,I)
         JI=IPARI(1,N)
         NSN=IPARI(5,N)
         L=INTBUF_TAB(N)%IRTLM(NI)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3).EQ.INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
         ILEV  =IPARI(20,N)
C
        DO IS = 1,NSL
         IF (ISL(IS).EQ.NS) THEN
         DO J = 1,IKC_SL(IS)
C----------------int2 herarch-------
          IF (INLOC(NS).GT.NSL) THEN
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,NS   ,IS  ,
     1                   J     ,IKC_SL(IS),NDOF ,IAD   )
           CALL CP_REAL(9,KSI_FR(1,IAD),KSM)
          ELSE
           IAD = IAD_M1(IS)+J
           CALL CP_REAL(9,KSL_FR(1,IAD),KSM)
          ENDIF
          IF (ILEV.EQ.1) THEN
           CALL I2_FRK1(INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%DPARA,X  ,ITAB ,
     .                  INTBUF_TAB(N)%NSV,
     1                  INTBUF_TAB(N)%IRTLM,IKC  ,NDOF  ,IDDL ,IDDLM,
     2                  IAD_SS ,JDI_SL ,DIAG_SL,LT_SL ,B    ,A      ,
     3                  KSS(1,IS),KSM  ,KNM   ,KRM  ,NI    ,
     4                  IDM    ,IFSS   ,IFSM   )
          ELSE
           CALL I2_FRK0(INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%CSTS,X    ,ITAB ,
     .                  INTBUF_TAB(N)%NSV,
     1                  INTBUF_TAB(N)%IRTLM,IKC  ,NDOF  ,IDDL ,IDDLM,
     2                  IAD_SS ,JDI_SL ,DIAG_SL,LT_SL ,B    ,A      ,
     3                  KSS(1,IS),KSM  ,KNM   ,KRM  ,NI    ,
     4                  IDM    ,IFSS   ,IFSM   )
          ENDIF
          DO K =1,NNOD
           NJ=INTBUF_TAB(N)%IRECTM(NL+K)
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,NJ   ,IS  ,
     1                   J     ,IKC_SL(IS),NDOF ,IAD   )
           CALL CP_REAL(9,KNM(1,K),KSI_FR(1,IAD))
           IF (NDOF(NJ).EQ.6)CALL CP_REAL(9,KRM(1,K),KSI_FR(1,IAD+1))
          ENDDO
         ENDDO
         ENDIF
        ENDDO
       ENDDO
C------RBE2-------
       DO I=NRBE2_FR,1,-1
        N=IFRS4(1,I)
        M=IRBE2(3,N)
        NS=IFRS4(2,I)
        ISK = IRBE2(7,N)
            IRAD =IRBE2(11,N)
            IC =IRBE2(4,N)
C--------remove ICR---
         IC =(IC/512)*512
        DO IS = 1,NSL
         IF (ISL(IS).EQ.NS) THEN
         DO J = 1,IKC_SL(IS)
C----------------ns main int2 -------
          IF (INLOC(NS).GT.NSL) THEN
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,NS   ,IS  ,
     1                  J     ,IKC_SL(IS),NDOF ,IAD   )
           CALL CP_REAL(9,KSI_FR(1,IAD),KSM)
          ELSE
           IAD = IAD_M1(IS)+J
           CALL CP_REAL(9,KSL_FR(1,IAD),KSM)
          ENDIF
          CALL PRERBE2FR(IC    ,JT  ,JR   )
          CALL RBE2_FRK(NS    ,M    ,X     ,ISK  ,SKEW(1,ISK) ,
     1                  IRAD  ,NDOF  ,IDDL ,JT   ,JR      ,
     2                  IAD_SS,JDI_SL,DIAG_SL,LT_SL ,B    ,
     3                  A    ,KSS(1,IS),KSM   ,KNM   ,KRM  ,
     4                  IDM(1),IFSS,IFSM )
          CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,M    ,IS  ,
     1                 J     ,IKC_SL(IS),NDOF ,IAD   )
           CALL CP_REAL(9,KNM,KSI_FR(1,IAD))
           CALL CP_REAL(9,KRM,KSI_FR(1,IAD+1))
         ENDDO
         ENDIF
        ENDDO
       ENDDO
C ------RBE3
       DO I=1,NRBE3_FR
        N=IFRS3(I)
        IADR=IRBE3(1,N)
        NS=IRBE3(3,N)
        NNOD=IRBE3(5,N)
        IROT =IRBE3(6,N)
        EID=IRBE3(2,N)
        IADS = SLRBE3/2+IADR
        CALL PRERBE3FR(IRBE3 ,N    ,JT  ,JR   )
        ALLOCATE(KNM3(9*NNOD))
        IF (IROT>0) ALLOCATE(KRM3(9*NNOD))
C
        DO IS = 1,NSL
         IF (ISL(IS).EQ.NS) THEN
         DO J = 1,IKC_SL(IS)
C----------------int2 herarch-------
          IF (INLOC(NS).GT.NSL) THEN
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,NS   ,IS  ,
     1                   J     ,IKC_SL(IS),NDOF ,IAD   )
           CALL CP_REAL(9,KSI_FR(1,IAD),KSM)
          ELSE
           IAD = IAD_M1(IS)+J
           CALL CP_REAL(9,KSL_FR(1,IAD),KSM)
          ENDIF
          CALL RBE3_FR0(NS    ,NNOD  ,LRBE3(IADR+1)  ,X     ,IROT   ,
     2                  JT    ,JR    ,FRBE3(6*IADR+1)  ,SKEW  ,IKC   ,
     3                  NDOF  ,IAD_SS ,JDI_SL ,DIAG_SL,LT_SL ,
     2                  KSS(1,IS),KSM    ,KNM3 ,KRM3   ,IDLM  ,
     3                  IFSS   ,IFSM    ,ITAB  ,LRBE3(IADS+1),EID)
          DO K =1,NNOD
           NJ = LRBE3(IADR+K)
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,NJ   ,IS  ,
     1                   J     ,IKC_SL(IS),NDOF ,IAD   )
           ID = 9*(K-1) +1
           CALL CP_REAL(9,KNM3(ID),KSI_FR(1,IAD))
           IF (IROT>0.AND.NDOF(NJ).EQ.6)
     +           CALL CP_REAL(9,KRM3(ID),KSI_FR(1,IAD+1))
          ENDDO
         ENDDO
         ENDIF
        ENDDO
         DEALLOCATE(KNM3)
         IF (IROT>0) DEALLOCATE(KRM3)
       ENDDO
C------Rigid bodies-------
       DO I=NRB_FR,1,-1
        M=IFRSR(1,I)
        NS=IFRSR(2,I)
        DO IS = 1,NSL
         IF (ISL(IS).EQ.NS) THEN
         DO J = 1,IKC_SL(IS)
C----------------ns main int2 -------
          IF (INLOC(NS).GT.NSL) THEN
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,NS   ,IS  ,
     1                  J     ,IKC_SL(IS),NDOF ,IAD   )
           CALL CP_REAL(9,KSI_FR(1,IAD),KSM)
          ELSE
           IAD = IAD_M1(IS)+J
           CALL CP_REAL(9,KSL_FR(1,IAD),KSM)
          ENDIF
          CALL RBY_FRK(NS   ,M    ,X     ,ITAB ,IKC  ,
     1                NDOF  ,IDDL ,IDDLM,IAD_SS,JDI_SL,
     2                DIAG_SL,LT_SL ,B    ,A    ,KSS(1,IS),
     3                KSM   ,KNM   ,KRM  ,IDM(1),IFSS,IFSM )
          CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,M    ,IS  ,
     1                 J     ,IKC_SL(IS),NDOF ,IAD   )
           CALL CP_REAL(9,KNM,KSI_FR(1,IAD))
           CALL CP_REAL(9,KRM,KSI_FR(1,IAD+1))
         ENDDO
         ENDIF
        ENDDO
       ENDDO
C
      DO I1 = 1,NBC_FR
        N = IBC_FR(1,I1)
        ISK= IBC_FR(2,I1)
        ICT= IBC_FR(3,I1)
        DO IS = 1,NSL
         IF (ISL(IS).EQ.N) THEN
         DO J = 1,IKC_SL(IS)
          IF (INLOC(N).GT.NSL) THEN
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,N    ,IS  ,
     1                  J     ,IKC_SL(IS),NDOF ,IAD   )
           CALL CP_REAL(9,KSI_FR(1,IAD),KSM)
          ELSE
           IAD = IAD_M1(IS)+J
           CALL CP_REAL(9,KSL_FR(1,IAD),KSM)
          ENDIF
          CALL BCL_FRK(N      ,IDDL  ,IDDLM  ,ICT  ,ISK   ,
     1                 SKEW   ,IKC   ,IAD_SS,JDI_SL,DIAG_SL,
     2                 LT_SL  ,B     ,A     ,KSS(1,IS),KSM   ,
     3                 IDM(1) ,IFSS  ,IFSM )
          IF (INLOC(N).LE.NSL) THEN
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,N    ,IS  ,
     1                  J     ,IKC_SL(IS),NDOF ,IAD   )
          ENDIF
          CALL CP_REAL(9,KSM,KSI_FR(1,IAD))
        ENDDO
        ENDIF
       ENDDO
      ENDDO
C
      DO I1 = 1,NSPC_FR
        N = ISPC_FR(I1)
        I = IN_SPC(N)
            IADR = 6*(N-1)+1
            NN = IC_SPC(N)
        DO IS = 1,NSL
         IF (ISL(IS).EQ.I) THEN
         DO J = 1,IKC_SL(IS)
          IF (INLOC(I).GT.NSL) THEN
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,I    ,IS  ,
     1                  J     ,IKC_SL(IS),NDOF ,IAD   )
           CALL CP_REAL(9,KSI_FR(1,IAD),KSM)
          ELSE
           IAD = IAD_M1(IS)+J
           CALL CP_REAL(9,KSL_FR(1,IAD),KSM)
          ENDIF
              IF (NN.EQ.1) THEN
               EJ(1)=SKEW_SPC(IADR)
               EJ(2)=SKEW_SPC(IADR+1)
               EJ(3)=SKEW_SPC(IADR+2)
           CALL L_DIR(EJ,JI)
           CALL BC_UPDFR(I     ,IDDL  ,EJ     ,JI    ,IDDLM  ,
     1                   IKC   ,IAD_SS,JDI_SL,DIAG_SL,LT_SL ,
     2                   B     ,A     ,KSS(1,IS),KSM   ,IDM(1),
     3                   IFSS  ,IFSM  )
          ELSE
           CALL BC_UPDFR2(I     ,IDDL  ,SKEW_SPC(IADR),SKEW_SPC(IADR+3),
     1                   IDDLM  ,IKC   ,IAD_SS,JDI_SL,DIAG_SL,LT_SL ,
     2                   B     ,A     ,KSS(1,IS),KSM   ,IDM(1),
     3                   IFSS  ,IFSM  )
              END IF
          IF (INLOC(I).LE.NSL) THEN
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,I    ,IS  ,
     1                  J     ,IKC_SL(IS),NDOF ,IAD   )
          ENDIF
          CALL CP_REAL(9,KSM,KSI_FR(1,IAD))
        ENDDO
        ENDIF
       ENDDO
      ENDDO
C
      IF (NFX_FR.GT.0) THEN
       DO L=1,NFX_FR
        I = IFX_FR(1,L)
        J1 = IFX_FR(2,L)
        N=IABS(IBFV(1,I))
        DO IS = 1,NSL
         IF (ISL(IS).EQ.N) THEN
         DO J = 1,IKC_SL(IS)
          IF (INLOC(N).GT.NSL) THEN
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,N    ,IS  ,
     1                  J     ,IKC_SL(IS),NDOF ,IAD   )
           CALL CP_REAL(9,KSI_FR(1,IAD),KSM)
          ELSE
           IAD = IAD_M1(IS)+J
           CALL CP_REAL(9,KSL_FR(1,IAD),KSM)
          ENDIF
          CALL FVL_FRK(J1     ,I      ,IBFV  ,SKEW    ,XFRAME,
     1                 IDDL    ,IDDLM  ,IKC   ,IAD_SS  ,JDI_SL,
     2                 DIAG_SL ,LT_SL ,UD    ,B      ,A     ,
     3                 KSS(1,IS),KSM   ,IDM  ,IFSS  ,IFSM  )
          IF (INLOC(N).LE.NSL) THEN
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,N    ,IS  ,
     1                  J     ,IKC_SL(IS),NDOF ,IAD   )
          ENDIF
          CALL CP_REAL(9,KSM,KSI_FR(1,IAD))
         ENDDO
         ENDIF
        ENDDO
       ENDDO
      ENDIF
C
      DO L = 1,NRW_FR
        I = IRW_FR(L)
        N=IN_RWL(I)
        EJ(1)=NOR_RWL(1,I)
        EJ(2)=NOR_RWL(2,I)
        EJ(3)=NOR_RWL(3,I)
        CALL L_DIR(EJ,J1)
        DO IS = 1,NSL
         IF (ISL(IS).EQ.N) THEN
         DO J = 1,IKC_SL(IS)
          IF (INLOC(N).GT.NSL) THEN
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,N    ,IS  ,
     1                  J     ,IKC_SL(IS),NDOF ,IAD   )
           CALL CP_REAL(9,KSI_FR(1,IAD),KSM)
          ELSE
           IAD = IAD_M1(IS)+J
           CALL CP_REAL(9,KSL_FR(1,IAD),KSM)
          ENDIF
          CALL FV_UPDFR(N     ,EJ    ,J1    ,IDDL  ,IDDLM    ,
     1                 IKC    ,IAD_SS,JDI_SL,DIAG_SL,LT_SL   ,
     2                 UD     ,B     ,A     ,KSS(1,IS),KSM   ,
     3                 IDM(1) ,IFSS  ,IFSM)
          IF (INLOC(N).LE.NSL) THEN
           CALL GET_IAD(IAD_M ,IAD_SLNR,JDI_SLNR,N    ,IS  ,
     1                  J     ,IKC_SL(IS),NDOF ,IAD   )
          ENDIF
          CALL CP_REAL(9,KSM,KSI_FR(1,IAD))
         ENDDO
         ENDIF
        ENDDO
      ENDDO
C--------------------------------------------
C
      RETURN
      END
Chd|====================================================================
Chd|  ASSEM_KSL                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ASSEM_KII                     source/implicit/imp_glob_k.F  
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE ASSEM_KSL(IDDL    ,K_DIAG    ,K_LT      ,IADK     ,
     1                     JDIK    ,KSS       ,NSL       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .        IDDL(*)   ,IADK(*)  ,JDIK(*),NSL
C     REAL ou REAL*8
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,KSS(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, JLT , NFT ,ND ,J,N0,JLT_NEW,IS
      my_real
     .     K11(3,3,MVSIZ),OFF(MVSIZ)
C------------------------------------
       ND = 3
       DO NFT = 0 , NSL - 1 , NVSIZ
         JLT = MIN( NVSIZ, NSL - NFT )
         JLT_NEW = 0
        DO I = 1 , JLT
         IS = NFT+I
         IF (IKC_SI(IS).EQ.0) THEN
          JLT_NEW = JLT_NEW + 1
          DO J = 1 , 3
           K11(J,J,JLT_NEW) = KSS(J,IS)
          ENDDO
          K11(1,2,JLT_NEW) = KSS(4,IS)
          K11(1,3,JLT_NEW) = KSS(5,IS)
          K11(2,3,JLT_NEW) = KSS(6,IS)
          OFF(JLT_NEW) = ONE
         ENDIF
        ENDDO
        JLT = JLT_NEW
        CALL ASSEM_KII(ISL(NFT+1),JLT,IDDL,IADK,K_DIAG,K_LT,K11,ND,OFF)
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  ASSFR_KIJ                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        ASS_SPMD                      source/implicit/assem_int.F   
Chd|        ASS_SPMD11                    source/implicit/assem_int.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASSFR_KIJ( ID    ,JD   ,IADK  ,JDIK,K_LT  ,
     1                      KIJ   ,ND   )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND
      INTEGER ID,JD,IADK(*),JDIK(*)
C     REAL ou REAL*8
      my_real
     .   K_LT(*)   ,KIJ(ND,ND)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,JDL,L,JJ
C----6---------------------------------------------------------------7---------8
         DO K=1,ND
          JDL= -1
          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
C---------find l'adress dans LT-----
           IF (JDIK(JJ).EQ.(JD+1)) THEN
            JDL = JJ-1
            GOTO 300
           ENDIF
          ENDDO
 300      CONTINUE
         IF (JDL.GE.0) THEN
          DO L=1,ND
           K_LT(JDL+L) = K_LT(JDL+L) + KIJ(K,L)
          ENDDO
         ELSE
         ENDIF
         ENDDO
C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  GETFR_KIJ                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FVKM                      source/mpi/implicit/imp_fri.F 
Chd|        INI_KSI                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GETFR_KIJ( ID    ,JD   ,IADK  ,JDIK,K_LT  ,
     1                      KIJ   ,NK   ,NL    )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NK   ,NL
      INTEGER ID,JD,IADK(*),JDIK(*)
C     REAL ou REAL*8
      my_real
     .   K_LT(*)   ,KIJ(NK,NL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,JDL,L,JJ
C----6---------------------------------------------------------------7---------8
         DO K=1,NK
          JDL=-1
          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
C---------find l'adress dans LT-----
           IF (JDIK(JJ).EQ.(JD+1)) THEN
            JDL = JJ-1
            GOTO 300
           ENDIF
          ENDDO
 300      CONTINUE
         IF (JDL.GE.0) THEN
          DO L=1,NL
           KIJ(K,L) = K_LT(JDL+L)
          ENDDO
         ELSE
         ENDIF
         ENDDO
C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  PUTFR_KIJ                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PUTFR_KIJ( ID    ,JD   ,IADK  ,JDIK,K_LT  ,
     1                      KIJ   ,NK   ,NL    )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NK   ,NL
      INTEGER ID,JD,IADK(*),JDIK(*)
C     REAL ou REAL*8
      my_real
     .   K_LT(*)   ,KIJ(NK,NL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,JDL,L,JJ
C----6---------------------------------------------------------------7---------8
         DO K=1,NK
          JDL=0
          DO JJ = IADK(ID+K),IADK(ID+1+K)-1
C---------find l'adress dans LT-----
           IF (JDIK(JJ).EQ.(JD+1)) THEN
            JDL = JJ-1
            GOTO 300
           ENDIF
          ENDDO
 300      CONTINUE
          DO L=1,NL
           K_LT(JDL+L) = KIJ(K,L)
          ENDDO
         ENDDO
C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  KIN_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        DIM_KINFRK                    source/mpi/implicit/imp_fri.F 
Chd|        DOUB_NRS                      source/mpi/implicit/imp_fri.F 
Chd|        IND_KINFRK                    source/mpi/implicit/imp_fri.F 
Chd|        INI_SLNR                      source/mpi/implicit/imp_fri.F 
Chd|        TAG_INTS                      source/mpi/implicit/imp_fri.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE KIN_KSL(
     1    IPARI     ,INTBUF_TAB,NINT2     ,IINT2     ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    X         ,IBFV      ,LJ        ,SKEW      ,XFRAME    ,
     4    ISKEW     ,ICODT     ,NDOF      ,ILOCP     ,NSL       ,
     5    IAD_M     ,IRBE3     ,LRBE3     ,IRBE2     ,LRBE2     )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),NSL
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),ILOCP(*),NDOF(*) ,
     .        IBFV(*),LJ(*),ISKEW(*),ICODT(*),IAD_M(*),
     .        IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*)
C     REAL ou REAL*8
      my_real
     .       X(3,*),SKEW(*),XFRAME(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LNS,LNS2,NNMAX,NZ,LNS3,LNS4
      INTEGER I,J,K,L,N_KINE,IA2(NINT2),IA(NRBYAC),NRS(NSL)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ICOL
C------------------------------------------------------------
      DO I =1,NUMNOD
       ILOCP(I)=0
      ENDDO
C--------N_KINE: nm, sl (no double)----
      N_KINE=0
      CALL TAG_INTS(NSL    ,ILOCP     ,N_KINE)
C
      DO I=1,NSL
       NRS(I) = 0
      ENDDO
      CALL DIM_KINFRK(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,ILOCP     ,
     3    IA        ,IA2       ,NRS       ,LNS       ,LNS2      ,
     4    NSL       ,IRBE3     ,LRBE3     ,LNS3      ,IRBE2     ,
     5    LRBE2     ,LNS4      )
C
      NNMAX=0
      DO I=1,NSL
       NNMAX=MAX(NNMAX,NRS(I))
       NRS(I)=0
      ENDDO
      ALLOCATE(ICOL(NNMAX,NSL))
      CALL IND_KINFRK(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,ILOCP     ,
     3    IA        ,IA2       ,LNS       ,LNS2      ,NSL       ,
     4    IBFV      ,LJ        ,ISKEW     ,ICODT     ,NRS       ,
     5    ICOL      ,NNMAX     ,IRBE3     ,LRBE3     ,LNS3      ,
     6    IRBE2     ,LRBE2     ,LNS4      )
C
       CALL DOUB_NRS(NSL  ,NNMAX ,NRS  ,ICOL   ,ILOCP     )
       NZ = 0
       DO I=1,NSL
        DO J=1,NRS(I)
         NZ = NZ+1
        ENDDO
       ENDDO
      CALL INI_SLNR(NSL  ,NNMAX ,NRS  ,ICOL   ,NZ     ,
     .              NDOF ,IAD_M )
      DEALLOCATE(ICOL)
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  DOUB_NRS                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        KIN_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE DOUB_NRS(NSL  ,NNMAX ,NRS  ,ICOL   ,ILOCP     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,NNMAX ,NRS(*),ILOCP(*)
      INTEGER ICOL(NNMAX,*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .        I,J,K,N,NJ
C----------------------------
C--- partie double--traitement diff.  srem puisque [k]rem est dja construit
      DO I=1,NSL
       N = ISL(I)
       K = ILOCP(N)
       IF (NRS(I).LT.NRS(K)) THEN
        CALL CP_INT(NRS(K),ICOL(1,K),ICOL(1,I))
        NRS(I) = NRS(K)
       ENDIF
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  INI_SLNR                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        KIN_KML                       source/mpi/implicit/imp_fri.F 
Chd|        KIN_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE INI_SLNR(NSL  ,NNMAX ,NRS  ,ICOL   ,NZ     ,
     .                    NDOF ,IAD_M )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,NZ,NNMAX ,NRS(*),NDOF(*),IAD_M(*)
      INTEGER ICOL(NNMAX,*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .        I,J,K,N,NJ
C----------------------------
       IF(ALLOCATED(IAD_SLNR)) DEALLOCATE(IAD_SLNR)
       ALLOCATE(IAD_SLNR(NSL+1))
       IF(ALLOCATED(JDI_SLNR)) DEALLOCATE(JDI_SLNR)
       ALLOCATE(JDI_SLNR(NZ))
       NZ = 0
       IAD_SLNR(1) = NZ+1
       DO N=1,NSL
         DO J=1,NRS(N)
           NJ = ICOL(J,N)
           NZ = NZ+1
           JDI_SLNR(NZ) = NJ
         ENDDO
         IAD_SLNR(N+1) = NZ+1
       ENDDO
       NZ = 0
       IAD_M(1) = NZ+1
       DO N=1,NSL
        IF (IKC_SL(N).GT.0) THEN
C         I = ISL(N)
         DO J=1,MAX(1,NRS(N))
           NZ = NZ+IKC_SL(N)
          IF (NRS(N).GT.0) THEN
               NJ = ICOL(J,N)
C----------IKC_SL=NREMS when it's called by KIN_KML          
           IF (NDOF(NJ).EQ.6) NZ = NZ+IKC_SL(N)
          ENDIF
         ENDDO
        ENDIF
        IAD_M(N+1) = NZ+1
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  DIM_KINFRK                    source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        KIN_KML                       source/mpi/implicit/imp_fri.F 
Chd|        KIN_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_KINFRK(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,INLOC     ,
     3    IA        ,IA2       ,NRS       ,LNS       ,LNS2      ,
     4    N_KINE    ,IRBE3     ,LRBE3     ,LNS3      ,IRBE2     ,
     5    LRBE2     ,LNS4      )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C      INTEGER NNMAX,NKMAX
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IPARI(NPARI,*),
     .        IRBE3(NRBE3L,*),LRBE3(*)     ,LNS3   ,
     .        IRBE2(NRBE2L,*),LRBE2(*)     ,LNS4
      INTEGER
     .   INLOC(*),NRS(*),LNS  ,LNS2,N_KINE,IA(*),IA2(*)
C     REAL ou REAL*8
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .        I,J,K,N,L,NL,NJ,NI,J1,M,NSN,N1,N2,NK,ID,
     .        JI,NS,NNOD,IAD
C----------------------------
      LNS2=0
C--------int2---------
      DO J=1,NINT2
       N=IINT2(J)
       IA2(J)=0
       NSN = IPARI(5,N)
       JI=IPARI(1,N)
       DO I=1,NSN
         NI=INTBUF_TAB(N)%NSV(I)
        IF (INLOC(NI).GT.0) THEN
         L=INTBUF_TAB(N)%IRTLM(NI)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3).EQ.INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
         NK = INLOC(NI)
         IF (NK.GT.N_KINE ) NK = NK - N_KINE
         NRS(NK) = NRS(NK)+NNOD
         IA2(J) = N
         LNS2 = LNS2+1
C--------si Mi est aussi dependant------
          DO K =1,NNOD
           NJ=INTBUF_TAB(N)%IRECTM(NL+K)
           IF (INLOC(NJ).EQ.0) INLOC(NJ) = N_KINE + INLOC(NI)
          ENDDO
        ENDIF
       ENDDO
      ENDDO
C
      LNS3=0
C--------RBE3---------
      DO I=1,NRBE3
        IAD=IRBE3(1,I)
        NI=IRBE3(3,I)
        NNOD=IRBE3(5,I)
        IF (NI.EQ.0) CYCLE
        IF (INLOC(NI).GT.0) THEN
         NK = INLOC(NI)
         IF (NK.GT.N_KINE ) NK = NK - N_KINE
         NRS(NK) = NRS(NK)+NNOD
         LNS3 = LNS3+1
C--------si Mi est aussi dependant------
          DO K =1,NNOD
           NJ = LRBE3(IAD+K)
           IF (INLOC(NJ).EQ.0) INLOC(NJ) = N_KINE + INLOC(NI)
          ENDDO
        ENDIF
      ENDDO
C
      LNS=0
C-----active rigid body main nodes------
      DO J=1,NRBYAC
       IA(J)=0
       N=IRBYAC(J)
       K=IRBYAC(J+NRBYKIN)
       M  =NPBY(1,N)
       NSN  =NPBY(2,N)
        DO I=1,NSN
         ID = I+K
         NI=LPBY(ID)
         NK = INLOC(NI)
         IF (NK.GT.N_KINE) NK = INLOC(NI)-N_KINE
         IF (NK.GT.0) THEN
          NRS(NK) = NRS(NK)+1
          IA(J)=N
          LNS = LNS+1
          IF (INLOC(M).EQ.0) INLOC(M) = N_KINE + INLOC(NI)
         ENDIF
       ENDDO
      ENDDO
C-----RBE2------
      LNS4=0
      DO J=1,NRBE2
       IAD=IRBE2(1,J)
       M  =IRBE2(3,J)
       NSN =IRBE2(5,J)
        DO I=1,NSN
         ID = I+IAD
         NI=LRBE2(ID)
         NK = INLOC(NI)
         IF (NK.GT.N_KINE) NK = INLOC(NI)-N_KINE
         IF (NK.GT.0) THEN
          NRS(NK) = NRS(NK)+1
          LNS4 = LNS4+1
          IF (INLOC(M).EQ.0) INLOC(M) = N_KINE + INLOC(NI)
         ENDIF
       ENDDO
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IND_KINFRK                    source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        KIN_KML                       source/mpi/implicit/imp_fri.F 
Chd|        KIN_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IND_KINFRK(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,INLOC     ,
     3    IA        ,IA2       ,NSS       ,NSS2      ,N_KINE    ,
     4    IBFV      ,LJ        ,ISKEW     ,ICODT     ,NRS       ,
     5    ICOL      ,NNMAX     ,IRBE3     ,LRBE3     ,LNS3      ,
     6    IRBE2     ,LRBE2     ,LNS4      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE IMP_RWL
      USE IMP_ASPC
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNMAX
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IPARI(NPARI,*),ISKEW(*),ICODT(*)
      INTEGER
     .   INLOC(*),NSS,NSS2,N_KINE,IBFV(NIFV,*),LJ(*),ICOL(NNMAX,*),
     .   IA(*),IA2(*),NRS(*),IRBE3(NRBE3L,*),LRBE3(*),LNS3 ,
     .   IRBE2(NRBE2L,*),LRBE2(*),LNS4
C     REAL ou REAL*8
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .        I,J,K,N,L,NL,NJ,NI,J1,M,NSN,N1,N2,NK,ID,
     .        JI,IAD,NNOD,
     .        IERROR1,IERROR2,IERROR3,IERROR4,IERROR5,IERROR6,IERROR7
c----------------------
      NI2_FR=0
      NI2_FRS=0
      IF (NSS2.GT.0) THEN
       IF(ALLOCATED(IFRS2)) DEALLOCATE(IFRS2)
       ALLOCATE(IFRS2(2,NSS2),STAT=IERROR1)
       DO J=1,NINT2
        N=IA2(J)
        IF (N.GT.0) THEN
         NSN = IPARI(5,N)
         JI=IPARI(1,N)
         DO I=1,NSN
          NI=INTBUF_TAB(N)%NSV(I)
          NK = INLOC(NI)
          IF (NK.GT.N_KINE) NK =NK-N_KINE
          IF (NK.GT.0) THEN
           L=INTBUF_TAB(N)%IRTLM(NI)
           NL=4*(L-1)
           DO K =1,4
            NJ=INTBUF_TAB(N)%IRECTM(NL+K)
            CALL REORDER_A(NRS(NK),ICOL(1,NK),NJ)
           ENDDO
           NI2_FR=NI2_FR+1
           IFRS2(1,NI2_FR)=N
           IFRS2(2,NI2_FR)=I
          ENDIF
         ENDDO
        ENDIF
       ENDDO
      ENDIF
C-----RBE2------
      NRBE2_FR=0
      NRBE2_FRS=0
      IF (LNS4.GT.0) THEN
       IF(ALLOCATED(IFRS4)) DEALLOCATE(IFRSR)
       ALLOCATE(IFRS4(2,LNS4),STAT=IERROR2)
       DO N=1,NRBE2
        IAD=IRBE2(1,N)
        M  =IRBE2(3,N)
         IF (INLOC(M).GT.0) THEN
          NSN  =IRBE2(5,N)
          DO I=1,NSN
           ID = I+IAD
           NI=LRBE2(ID)
           NK = INLOC(NI)
           IF (NK.GT.N_KINE) NK = INLOC(NI)-N_KINE
           IF (NK.GT.0) THEN
            CALL REORDER_A(NRS(NK),ICOL(1,NK),M)
            NRBE2_FR=NRBE2_FR+1
            IFRS4(1,NRBE2_FR)=N
            IFRS4(2,NRBE2_FR)=NI
           ENDIF
          ENDDO
         ENDIF
       ENDDO
      ENDIF
c--------RBE3--------------
      NRBE3_FR=0
      NRBE3_FRS=0
      IF (LNS3.GT.0) THEN
       IF(ALLOCATED(IFRS3)) DEALLOCATE(IFRS3)
       ALLOCATE(IFRS3(LNS3),STAT=IERROR1)
       DO N=1,NRBE3
        IAD=IRBE3(1,N)
        NI=IRBE3(3,N)
        NNOD=IRBE3(5,N)
        IF (NI.EQ.0) CYCLE
        IF (INLOC(NI).GT.0) THEN
          NK = INLOC(NI)
          IF (NK.GT.N_KINE) NK =NK-N_KINE
           DO K =1,NNOD
            NJ = LRBE3(IAD+K)
            CALL REORDER_A(NRS(NK),ICOL(1,NK),NJ)
           ENDDO
           NRBE3_FR=NRBE3_FR+1
           IFRS3(NRBE3_FR)=N
        ENDIF
       ENDDO
      ENDIF
C-----active rigid body main nodes------
      NRB_FR=0
      NRB_FRS=0
      IF (NSS.GT.0) THEN
       IF(ALLOCATED(IFRSR)) DEALLOCATE(IFRSR)
       ALLOCATE(IFRSR(2,NSS),STAT=IERROR2)
       DO J=1,NRBYAC
        N=IA(J)
        IF (N.GT.0) THEN
         K=IRBYAC(J+NRBYKIN)
         M  =NPBY(1,N)
         IF (INLOC(M).GT.0) THEN
          NSN  =NPBY(2,N)
          DO I=1,NSN
           ID = I+K
           NI=LPBY(ID)
           NK = INLOC(NI)
           IF (NK.GT.N_KINE) NK = INLOC(NI)-N_KINE
           IF (NK.GT.0) THEN
            CALL REORDER_A(NRS(NK),ICOL(1,NK),M)
            NRB_FR=NRB_FR+1
            IFRSR(1,NRB_FR)=M
            IFRSR(2,NRB_FR)=NI
           ENDIF
          ENDDO
         ENDIF
        ENDIF
       ENDDO
      ENDIF
C+++ BC. LOCAL
      NBC_FR = 0
      DO N=1,NUMNOD
        IF (ISKEW(N).GT.1.AND.ICODT(N).NE.7) THEN
         IF (INLOC(N).GT.0)NBC_FR = NBC_FR + 1
        ENDIF
      ENDDO
      IF (NBC_FR.GT.0) THEN
       IF(ALLOCATED(IBC_FR)) DEALLOCATE(IBC_FR)
       ALLOCATE(IBC_FR(3,NBC_FR),STAT=IERROR5)
       NBC_FR = 0
       DO N=1,NUMNOD
        IF (ISKEW(N).GT.1.AND.ICODT(N).NE.7) THEN
         IF (INLOC(N).GT.0) THEN
           NBC_FR = NBC_FR + 1
           IBC_FR(1,NBC_FR) = N
           IBC_FR(2,NBC_FR) = ISKEW(N)
           IBC_FR(3,NBC_FR) = ICODT(N)
         ENDIF
        ENDIF
       ENDDO
      ENDIF
C+++ AUTOSPC
      NSPC_FR = 0
      DO N=1,NSPCL
       I = IN_SPC(N)
       IF (INLOC(N).GT.0.AND.IC_SPC(N).LE.3)NSPC_FR = NSPC_FR + 1
      ENDDO
      IF (NSPC_FR.GT.0) THEN
       IF(ALLOCATED(ISPC_FR)) DEALLOCATE(ISPC_FR)
       ALLOCATE(ISPC_FR(NSPC_FR),STAT=IERROR5)
       NSPC_FR = 0
       DO N=1,NSPCL
        I = IN_SPC(N)
        IF (INLOC(N).GT.0.AND.IC_SPC(N).LE.3) THEN
             NSPC_FR = NSPC_FR + 1
             ISPC_FR(NSPC_FR)= N
        ENDIF
       ENDDO
      ENDIF
C +++ FV---local
      NFX_FR = 0
      DO J=1,NFXVEL
        IF (LJ(J).GT.0.AND.LJ(J).LE.3) THEN
         N=IABS(IBFV(1,J))
         IF (INLOC(N).GT.0)NFX_FR = NFX_FR + 1
        ENDIF
      ENDDO
      IF (NFX_FR.GT.0) THEN
       IF(ALLOCATED(IFX_FR)) DEALLOCATE(IFX_FR)
       ALLOCATE(IFX_FR(2,NFX_FR),STAT=IERROR6)
       NFX_FR = 0
       DO J=1,NFXVEL
        IF (LJ(J).GT.0.AND.LJ(J).LE.3) THEN
         N=IABS(IBFV(1,J))
         IF (INLOC(N).GT.0) THEN
           NFX_FR = NFX_FR + 1
           IFX_FR(1,NFX_FR) = J
           IFX_FR(2,NFX_FR) = LJ(J)
         ENDIF
        ENDIF
       ENDDO
      ENDIF
C ---
C +++  sliding rigid wall---
      NRW_FR = 0
      DO J=1,N_RWL
       N=IN_RWL(J)
       IF (INLOC(N).GT.0) NRW_FR = NRW_FR + 1
      ENDDO
      IF (NRW_FR.GT.0) THEN
       IF(ALLOCATED(IRW_FR)) DEALLOCATE(IRW_FR)
       ALLOCATE(IRW_FR(NRW_FR),STAT=IERROR7)
       NRW_FR = 0
       DO J=1,N_RWL
         N=IN_RWL(J)
         IF (INLOC(N).GT.0) THEN
           NRW_FR = NRW_FR + 1
           IRW_FR(NRW_FR) = J
         ENDIF
       ENDDO
      ENDIF
C ---
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  GET_IAD                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FVKM                      source/mpi/implicit/imp_fri.F 
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        INTABFR                       source/mpi/implicit/imp_fri.F 
Chd|====================================================================
      SUBROUTINE GET_IAD(IAD_M ,IAD_S ,JDI_S ,NM   ,IS  ,
     1                   J     ,NRJ   ,NDOF  ,IAD   )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_M(*),IAD_S(*),JDI_S(*),NM ,NS,J,IAD,NRJ ,NDOF(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,NR,L,IS,NK,NZ,NJ,I0,ID
C----6---------------------------------------------------------------7---------8
        NR =IAD_S(IS+1)-IAD_S(IS)
        L = IAD_M(IS)
        NK =(IAD_M(IS+1)-IAD_M(IS))/NRJ
        IAD = L+(J-1)*NK
        IF (NM.GT.0.AND.NR.GT.0) THEN
         I0=IAD_S(IS)
         CALL INTABFR(NR,JDI_S(I0),NM,ID)
         IF (ID.GT.0) THEN
          NZ = 0
          DO I = 1,ID-1
           NJ = JDI_S(I0+I-1)
           IF (NDOF(NJ).EQ.6) THEN
            NZ = NZ + 2
           ELSE
            NZ = NZ +1
           ENDIF
          ENDDO
          IAD = IAD + NZ
         ELSE
          IAD = 0
         ENDIF
        ENDIF
C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IKINCF                        source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        DIM_FRKM1                     source/mpi/implicit/imp_fri.F 
Chd|        TRA_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      LOGICAL FUNCTION IKINCF(I)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----6---------------------------------------------------------------7---------8
       IF (I.EQ.0.OR.(I.GE.2.AND.I.LE.4).OR.I.EQ.9) THEN
         IKINCF =.TRUE.
       ELSE
         IKINCF =.FALSE.
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  DIM_FRKM1                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        SPMD_ISR                      source/mpi/implicit/imp_spmd.F
Chd|        IKINCF                        source/mpi/implicit/imp_fri.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE DIM_FRKM1(NSREM,NSL  ,IDDL ,IKC ,NDOF  ,
     .                     NF_SI,NF_SL,LSI  ,LSL ,MSI,MSL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSREM  ,NSL ,LSI,LSL ,NF_SL(*),NF_SI(*),
     .        IDDL(*) ,IKC(*) ,NDOF(*)  ,MSI,MSL
C     REAL ou REAL*8
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL IKINCF
      EXTERNAL IKINCF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,ID,NFAC,NJ
C----------NF_SL : new nm times of [K]3x3- of dependant---------------------------
        DO I = 1, NSL
        IF (IKC_SL(I).GT.0) THEN
         N = ISL(I)
         ID = IDDL(N)
         IF (NDOF(N).EQ.0.OR.(IKC(ID+1).NE.0.AND.IKC(ID+2).NE.0
     .       .AND.IKC(ID+3).NE.0)) THEN
          NFAC=0
          DO J=IAD_SLNR(I),IAD_SLNR(I+1)-1
           NJ = JDI_SLNR(J)
           ID = IDDL(NJ)
           IF (IKINCF(IKC(ID+1)).OR.IKINCF(IKC(ID+2)).OR.
     .         IKINCF(IKC(ID+3)))
     .      NFAC = NFAC +1
           IF (NDOF(NJ).EQ.6) THEN
           IF (IKINCF(IKC(ID+4)).OR.IKINCF(IKC(ID+5)).OR.
     .         IKINCF(IKC(ID+6)))
     .      NFAC = NFAC +1
           ENDIF
          ENDDO
Ctmp cas particulier d_imp dans tous les directions
          IF (NDOF(N).GT.0.AND.IAD_SLNR(I).EQ.IAD_SLNR(I+1))NFAC = 1
         ELSE
          NFAC = 1
         ENDIF
         NF_SL(I) = NFAC
        ELSE
         NF_SL(I) = 0
        ENDIF
        ENDDO
C
        CALL SPMD_ISR(IAD_SL,IAD_SREM,NF_SL,NF_SI,NSL ,NSREM )
C   -----new nb. [k] modifie SL,SI---for com---ISL for IKC(3,M)
        MSL = 0
        LSL = 0
        DO I = 1, NSL
         IF (IKC_SL(I).GT.0) THEN
           MSL = MSL + NF_SL(I)
           LSL = LSL + IKC_SL(I)*NF_SL(I)
         ENDIF
        ENDDO
c
        MSI = 0
        LSI = 0
        DO I = 1, NSREM
         IF (IKC_SI(I).GT.0) THEN
           MSI = MSI +  NF_SI(I)
           LSI = LSI + IKC_SI(I)*NF_SI(I)
         ENDIF
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  TRA_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        IKINCF                        source/mpi/implicit/imp_fri.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE TRA_FRKM(NSL  ,IDDL ,IKC  ,NDOF  ,IAD_M,
     .                    KSI  ,KSL  ,IKCSL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,IDDL(*),IKC(*),NDOF(*),IAD_M(*),IKCSL(3,*)
C     REAL ou REAL*8
      my_real
     .   KSI(9,*),KSL(9,*)
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL IKINCF
      EXTERNAL IKINCF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,ID,IADI,IADL,SIZE,NJ,NB,IDM,NKC
      LOGICAL NODOF
C-------------------------------------
        SIZE = 9
        IADL=1
        DO I = 1, NSL
        IF (IKC_SL(I).GT.0) THEN
         IADI=IAD_M(I)
         N = ISL(I)
         ID = IDDL(N)
         IF (NDOF(N).EQ.0.OR.(IKC(ID+1).NE.0.AND.IKC(ID+2).NE.0
     .       .AND.IKC(ID+3).NE.0)) THEN
          NODOF = .TRUE.
         ELSE
          NODOF = .FALSE.
         END IF
C
         DO J = 1, IKC_SL(I)
          DO K=IAD_SLNR(I),IAD_SLNR(I+1)-1
           NJ = JDI_SLNR(K)
           ID = IDDL(NJ)
           IF (NODOF.AND.(IKINCF(IKC(ID+1)).OR.IKINCF(IKC(ID+2)).OR.
     .         IKINCF(IKC(ID+3)))) THEN
            CALL CP_REAL(SIZE,KSI(1,IADI),KSL(1,IADL))
            IADL=IADL+1
           ENDIF
           IADI=IADI+1
           IF (NDOF(NJ).EQ.6) THEN
            IF (NODOF.AND.(IKINCF(IKC(ID+4)).OR.IKINCF(IKC(ID+5)).OR.
     .          IKINCF(IKC(ID+6)))) THEN
             CALL CP_REAL(SIZE,KSI(1,IADI),KSL(1,IADL))
             IADL=IADL+1
            ENDIF
            IADI=IADI+1
           ENDIF
          ENDDO
Ctmp cas particulier d_imp dans tous les directions
         IF (NODOF) THEN
           IF (NDOF(N).GT.0.AND.IAD_SLNR(I).EQ.IAD_SLNR(I+1))THEN
            NB = SIZE
            CALL CP_REAL(NB,KSI(1,IADI),KSL(1,IADL))
            IADL=IADL+1
           ENDIF
         ELSE
          NB = SIZE
          CALL CP_REAL(NB,KSI(1,IADI),KSL(1,IADL))
          IADL=IADL+1
C------case /BCS          
          IF (IAD_SLNR(I)==IAD_SLNR(I+1)) IADI = IADI + 1
          ENDIF
C         
         END DO !J = 1, IKC_SL(I)
C
        ENDIF
        ENDDO
C------init IKCSL-------
        IADI = 1
        DO I = 1, NSL
         N = ISL(I)
         IF (IKC_SL(I).GT.0) THEN
          DO J=IAD_SLNR(I),IAD_SLNR(I+1)-1
           NJ = JDI_SLNR(J)
           ID = IDDL(NJ)
           IF (IKINCF(IKC(ID+1)).OR.IKINCF(IKC(ID+2)).OR.
     .         IKINCF(IKC(ID+3))) THEN
            DO K =1,3
             IKCSL(K,IADI) = IKC(ID+K)
            ENDDO
            IADI = IADI + 1
           ENDIF
           IF (NDOF(NJ).EQ.6) THEN
            IF (IKINCF(IKC(ID+4)).OR.IKINCF(IKC(ID+5)).OR.
     .          IKINCF(IKC(ID+6))) THEN
             DO K =1,3
              IKCSL(K,IADI) = IKC(ID+K+3)
             ENDDO
             IADI = IADI + 1
            ENDIF
           ENDIF
          ENDDO
C----------other kin------------
          IF (IAD_SLNR(I).EQ.IAD_SLNR(I+1).AND.NDOF(N)>0) THEN
           ID = IDDL(N)
           IF (IKINCF(IKC(ID+1)).OR.IKINCF(IKC(ID+2)).OR.
     .         IKINCF(IKC(ID+3))) THEN
            DO K =1,3
             IKCSL(K,IADI) = IKC(ID+K)
            ENDDO
            IADI = IADI + 1
           ENDIF
          ENDIF
         ENDIF
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  KIN_KML                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        DIM_KINFRK                    source/mpi/implicit/imp_fri.F 
Chd|        IND_KINFRK                    source/mpi/implicit/imp_fri.F 
Chd|        INI_SLNR                      source/mpi/implicit/imp_fri.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE KIN_KML(
     1    IPARI     ,INTBUF_TAB,NINT2     ,IINT2     ,     
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    X         ,IBFV      ,LJ        ,SKEW      ,XFRAME    ,
     4    ISKEW     ,ICODT     ,NDOF      ,ILOCP     ,NSREM     ,
     5    IDDL      ,IKC       ,IAD_M     ,NML       ,IRBE3     ,
     6    LRBE3     ,IRBE2     ,LRBE2     )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),IPARI(NPARI,*),NSREM,NML
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),ILOCP(*),NDOF(*) ,IDDL(*) ,IKC(*),
     .        LJ(*),ISKEW(*),ICODT(*),IAD_M(*),IRBE3(*),LRBE3(*),
     .        IRBE2(*),LRBE2(*)
C     REAL ou REAL*8
      my_real
     .       X(3,*),SKEW(*),XFRAME(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LNS,LNS2,NNMAX,NZ,LNS3,LNS4
      INTEGER I,J,K,L,N_KINE,IA2(NINT2),IA(NRBYAC)
      INTEGER, DIMENSION(:), ALLOCATABLE :: ICOL,NRS
C------------------------------------------------------------
      ALLOCATE(NRS(NML))
C
      DO I=1,NML
       NRS(I) = 0
      ENDDO
      CALL DIM_KINFRK(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,ILOCP     ,
     3    IA        ,IA2       ,NRS       ,LNS       ,LNS2      ,
     4    NML       ,IRBE3     ,LRBE3     ,LNS3      ,IRBE2     ,
     5    LRBE2     ,LNS4      )
C
      NNMAX=0
      DO I=1,NML
       NNMAX=MAX(NNMAX,NRS(I))
       NRS(I)=0
      ENDDO
      ALLOCATE(ICOL(NNMAX*NML))
C--------ICOL(NNMAX,NML) for independent nodes----
      CALL IND_KINFRK(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,ILOCP     ,
     3    IA        ,IA2       ,LNS       ,LNS2      ,NML       ,
     4    IBFV      ,LJ        ,ISKEW     ,ICODT     ,NRS       ,
     5    ICOL      ,NNMAX     ,IRBE3     ,LRBE3     ,LNS3      ,
     6    IRBE2     ,LRBE2     ,LNS4      )
C
       NZ = 0
       DO I=1,NML
        DO J=1,NRS(I)
         NZ = NZ+1
        ENDDO
       ENDDO
      CALL INI_SLNR(NML  ,NNMAX ,NRS  ,ICOL   ,NZ     ,
     .              NDOF ,IAD_M )
      DEALLOCATE(NRS)
      DEALLOCATE(ICOL)
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  TAG_INTML                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE TAG_INTML(NSREM ,ILOC  ,N_IMPN ,IDDL ,IKC  ,
     .                     NDOF  ,LSI   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSREM,ILOC(*),N_IMPN,IDDL(*) ,IKC(*) ,NDOF(*) ,LSI
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,IG,NM,NDD,ID,IER1,IER2
C-----------------------------------------------
       DO I = 1, NSREM
        DO J =IAD_SINR(I),IAD_SINR(I+1)-1
         IG = JDI_SINR(J)
         IF (ILOC(IG).EQ.0) THEN
          N_IMPN = N_IMPN + 1
          ILOC(IG) = N_IMPN
         ENDIF
        ENDDO
       ENDDO
C---------------
       NML = N_IMPN
       IF(ALLOCATED(IML)) DEALLOCATE(IML)
       ALLOCATE(IML(NML),STAT=IER1)
       DO I = 1, NSREM
        DO J =IAD_SINR(I),IAD_SINR(I+1)-1
         IG = JDI_SINR(J)
         NM = ILOC(IG)
         IML(NM) = IG
        ENDDO
       ENDDO
C--------use also IKC_SL for ml IFLAG_KC ONLY----
        IF(ALLOCATED(IKC_SL)) DEALLOCATE(IKC_SL)
        ALLOCATE(IKC_SL(NML),STAT=IER2)
        LSI = 0
        DO I = 1, NML
         N = IML(I)
             ID = IDDL(N)
         NDD = 0
         DO J = 1 , MIN(3,NDOF(N))
           NDD = NDD + IKC(ID+J)
         ENDDO
         IF (NDOF(N).EQ.0.OR.NDD.GT.0) NDD = NSREM
         IKC_SL(I) = NDD
         LSI = LSI + NDD
        ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  SCOM_FRK                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        SPMD_EXCK                     source/mpi/implicit/imp_spmd.F
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE SCOM_FRK(KS11,KR11,SSIZE ,RSIZE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SSIZE  ,RSIZE
      my_real
     .     KS11(9,*),KR11(9,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,SIZE,IAD_S(NSPMD+1),IAD_R(NSPMD+1)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      SIZE = 9
      IAD_S(1) = 1
      IAD_R(1) = 1
       DO I=1,NSPMD
        IAD_S(I+1) = IAD_S(I)
        IAD_R(I+1) = IAD_R(I)
        DO J=IAD_SREM(I),IAD_SREM(I+1)-1
         IAD_S(I+1) = IAD_S(I+1) + IKC_SI(J)
        END DO
        DO J=IAD_SL(I),IAD_SL(I+1)-1
         IAD_R(I+1) = IAD_R(I+1) + IKC_SL(J)
        END DO
       END DO
C
      CALL SPMD_EXCK(KS11,KR11,IAD_S,IAD_R,SIZE ,SSIZE,RSIZE)
C
      RETURN
      END
Chd|====================================================================
Chd|  SCOM_FRK1                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        SPMD_EXCI                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_EXCK                     source/mpi/implicit/imp_spmd.F
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE SCOM_FRK1(KS11,KR11,NFACS,NFACR,IKCS ,IKCR )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFACS(*),NFACR(*),IKCS(3,*),IKCR(3,*)
      my_real
     .     KS11(9,*),KR11(9,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,SSIZE,RSIZE,SIZE,IAD_S(NSPMD+1),IAD_R(NSPMD+1)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IAD_S(1) = 1
      IAD_R(1) = 1
       DO I=1,NSPMD
        IAD_S(I+1) = IAD_S(I)
        IAD_R(I+1) = IAD_R(I)
        DO J=IAD_SL(I),IAD_SL(I+1)-1
         IAD_S(I+1) = IAD_S(I+1) + NFACS(J)
        END DO
        DO J=IAD_SREM(I),IAD_SREM(I+1)-1
         IAD_R(I+1) = IAD_R(I+1) + NFACR(J)
        END DO
       END DO
C
      SIZE = 3
      SSIZE = IAD_S(NSPMD+1) - 1
      RSIZE = IAD_R(NSPMD+1) - 1
      CALL SPMD_EXCI(IKCS,IKCR,IAD_S,IAD_R,SIZE ,SSIZE,RSIZE)
      SIZE = 9
      IAD_S(1) = 1
      IAD_R(1) = 1
       DO I=1,NSPMD
        IAD_S(I+1) = IAD_S(I)
        IAD_R(I+1) = IAD_R(I)
        DO J=IAD_SL(I),IAD_SL(I+1)-1
         IAD_S(I+1) = IAD_S(I+1) + NFACS(J)*IKC_SL(J)
        END DO
        DO J=IAD_SREM(I),IAD_SREM(I+1)-1
         IAD_R(I+1) = IAD_R(I+1) + NFACR(J)*IKC_SI(J)
        END DO
       END DO
      SSIZE = IAD_S(NSPMD+1) - 1
      RSIZE = IAD_R(NSPMD+1) - 1
      CALL SPMD_EXCK(KS11,KR11,IAD_S,IAD_R,SIZE ,SSIZE,RSIZE)
C
      RETURN
      END
Chd|====================================================================
Chd|  UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        BCL_FRK                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDFR                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDFR2                     source/constraints/general/bcs/bc_imp0.F
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        FVL_FRK                       source/constraints/general/impvel/fv_imp0.F
Chd|        FV_UPDFR                      source/constraints/general/impvel/fv_imp0.F
Chd|        GETFR_KIJ                     source/mpi/implicit/imp_fri.F 
Chd|        GET_IAD                       source/mpi/implicit/imp_fri.F 
Chd|        I2_FRK0                       source/interfaces/interf/i2_imp1.F
Chd|        I2_FRK1                       source/interfaces/interf/i2_imp1.F
Chd|        INTABFR                       source/mpi/implicit/imp_fri.F 
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|        PRERBE2FR                     source/constraints/general/rbe2/rbe2f.F
Chd|        PRERBE3FR                     source/constraints/general/rbe3/rbe3f.F
Chd|        RBE2_FRK                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_FR0                      source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_FRK                       source/constraints/general/rbody/rby_imp0.F
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPD_KML(
     1    IPARI     ,INTBUF_TAB,NINT2     ,IINT2     ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    X         ,IBFV      ,LJ        ,SKEW      ,XFRAME    ,
     4    ISKEW     ,ICODT     ,INLOC     ,NSL       ,IAD_M     ,
     5    IDDL      ,IKC       ,NDOF      ,IDDLM     ,UD        ,
     6    A         ,KSL       ,KSI       ,NSREM     ,NF_SI     ,
     7    IDDLI     ,IRBE3     ,LRBE3     ,FRBE3     ,IRBE2     ,
     8    LRBE2     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
      USE IMP_RWL
      USE IMP_ASPC
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),NSL,IAD_M(*),NSREM
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IDDL(*),IKC(*),NDOF(*),INLOC(*),
     .        IBFV(NIFV,*),LJ(*),ISKEW(*),ICODT(*),IDDLM(*),
     .        NF_SI(*) ,IDDLI(*),IRBE3(NRBE3L,*),LRBE3(*),
     .        IRBE2(NRBE2L,*),LRBE2(*)
C     REAL ou REAL*8
      my_real
     .  X(3,*),SKEW(LSKEW,*),XFRAME(*),
     .  UD(3,*),A(3,*),KSL(9,*) ,KSI(9,*),FRBE3(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,K,M,NS,NI,NSN,ILEV,IAD_M1(NSREM+1),
     .        JI,L,NNOD,
     .        NJ,ND,NL,ISK,IFM,J1,IFLAG,NSS,NM,ID,IAD0,IAD,
     .        I1,ICT,NF,NR,IS,JD,IFSS,IFSM,IBID,JT(3),JR(3),IADS,
     .        IROT,IADR,NN,IRAD,IC,EID
      my_real
     .  EJ(3),KSM(9),KNM(9,4),KRM(9,4),RBID
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: KNM3,KRM3
C-----only [ksm] to update : secnd dependant :FRK_SI, secnd inde. LT_SI,
C     ----       --added inde,:FRK_SL-----------
       IFSS = 0
       IFSM = 1
       ND = 3
C------partie kin, vis a vis de SL--------
       IAD_M1(1) = 1
       DO N=1,NSREM
        IAD_M1(N+1) = IAD_M1(N)+IKC_SI(N)*NF_SI(N)
       ENDDO
       DO I=1,IAD_M(NSL+1)-1
        DO J=1,9
         KSL(J,I) = ZERO
        ENDDO
       ENDDO
C------int2----------------------------------
       DO I=NI2_FR,1,-1
        N=IFRS2(1,I)
        NI=IFRS2(2,I)
         JI=IPARI(1,N)
         NSN=IPARI(5,N)
         NS=INTBUF_TAB(N)%NSV(NI)
         L=INTBUF_TAB(N)%IRTLM(NI)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3).EQ.INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
         ILEV  =IPARI(20,N)
C
         IF (INLOC(NS).GT.NSL) THEN
          NSS = INLOC(NS)-NSL
          NR=IAD_SLNR(NSS+1)- IAD_SLNR(NSS)
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NS,J)
          IF (J.GT.0) THEN
          DO IS = 1,NSREM
           IAD = IAD_M(NSS) + J + IS -2
           CALL CP_REAL(9,KSL(1,IAD),KSM)
          IF (ILEV.EQ.1) THEN
           CALL I2_FRK1(INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%DPARA,X    ,ITAB ,
     .                  INTBUF_TAB(N)%NSV,
     1                  INTBUF_TAB(N)%IRTLM,IKC  ,NDOF  ,IDDL ,IDDLM,
     2                  IBID ,IBID ,RBID,RBID ,RBID  ,A      ,
     3                  RBID ,KSM  ,KNM   ,KRM  ,NI  ,
     4                  IBID ,IFSS ,IFSM)
          ELSE
           CALL I2_FRK0(INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%CSTS,X    ,ITAB ,
     .                  INTBUF_TAB(N)%NSV,
     1                  INTBUF_TAB(N)%IRTLM,IKC  ,NDOF  ,IDDL ,IDDLM,
     2                  IBID ,IBID ,RBID,RBID ,RBID  ,A      ,
     3                  RBID ,KSM  ,KNM   ,KRM  ,NI  ,
     4                  IBID ,IFSS ,IFSM)
          ENDIF
          DO K =1,NNOD
           NJ=INTBUF_TAB(N)%IRECTM(NL+K)
           CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NJ,J)
           IAD = IAD_M(NSS) + J +IS -2
           CALL CP_REAL(9,KNM(1,K),KSL(1,IAD))
           IF (NDOF(NJ).GT.3)CALL CP_REAL(9,KRM(1,K),KSL(1,IAD+NSREM))
          ENDDO
          END DO ! IS = 1,NSREM
          END IF !(J.GT.0) THEN
         ELSE
          NSS = INLOC(NS)
          DO IS = 1,NSREM
           NR =IAD_SINR(IS+1)- IAD_SINR(IS)
           CALL INTABFR(NR,JDI_SINR(IAD_SINR(IS)),NS,J)
           IF (J.GT.0) THEN
           DO NF = 1,MAX(1,NF_SI(IS))
C-------------------in KFR_SI----
            IF (IKC_SI(IS).GT.0.AND.NF_SI(IS).GT.0) THEN
             N = 0
             CALL GET_IAD(IAD_M1,IAD_SINR,JDI_SINR,N    ,IS  ,
     1                   J    ,IKC_SI(IS),NDOF  ,IAD  )
             IAD = IAD + NF-1
             CALL CP_REAL(9,KSI(1,IAD),KSM)
            ELSE
             ID = IDDL_SI(IS)
             NM = JDI_SINR(IAD_SINR(IS)+J-1)
             JD = IDDLI(NM)
             CALL GETFR_KIJ( ID    ,JD   ,IAD_SI ,JDI_SI,LT_SI  ,
     1                     KSM   ,ND   ,ND    )
            ENDIF
            IF (ILEV.EQ.1) THEN
             CALL I2_FRK1(INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%DPARA,X    ,ITAB ,
     .                  INTBUF_TAB(N)%NSV,
     1                  INTBUF_TAB(N)%IRTLM,IKC  ,NDOF  ,IDDL ,IDDLM,
     2                  IBID ,IBID ,RBID,RBID ,RBID  ,A      ,
     3                  RBID ,KSM  ,KNM   ,KRM  ,NI  ,
     4                  IBID ,IFSS ,IFSM)
            ELSE
             CALL I2_FRK0(INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%CSTS,X    ,ITAB ,
     .                  INTBUF_TAB(N)%NSV,
     1                  INTBUF_TAB(N)%IRTLM,IKC  ,NDOF  ,IDDL ,IDDLM,
     2                  IBID ,IBID ,RBID,RBID ,RBID  ,A      ,
     3                  RBID ,KSM  ,KNM   ,KRM  ,NI  ,
     4                  IBID ,IFSS ,IFSM)
            ENDIF
            DO K =1,NNOD
             NJ=INTBUF_TAB(N)%IRECTM(NL+K)
             NR =IAD_SLNR(NSS+1)- IAD_SLNR(NSS)
             CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NJ,J)
             IAD = IAD_M(NSS) + J + IS -2
             CALL CP_REAL(9,KNM(1,K),KSL(1,IAD))
             IF (NDOF(NJ).GT.3)CALL CP_REAL(9,KRM(1,K),KSL(1,IAD+NSREM))
            ENDDO
           ENDDO
           ENDIF
          ENDDO
         ENDIF
C
       ENDDO
C------RBE2-------
       DO I=NRBE2_FR,1,-1
        N=IFRS4(1,I)
        M=IRBE2(3,N)
        NS=IFRS4(2,I)
        ISK = IRBE2(7,N)
            IRAD =IRBE2(11,N)
            IC = IRBE2(4,N)
C--------remove ICR---
        IC =(IC/512)*512
        IF (INLOC(NS).GT.NSL) THEN
          NSS = INLOC(NS)-NSL
          NR=IAD_SLNR(NSS+1)- IAD_SLNR(NSS)
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NS,J)
             IF (J.GT.0) THEN
         DO IS = 1,NSREM
          IAD = IAD_M(NSS) + J + IS -2
          CALL CP_REAL(9,KSL(1,IAD),KSM)
          CALL PRERBE2FR(IC    ,JT  ,JR   )
          CALL RBE2_FRK(NS    ,M    ,X     ,ISK  ,SKEW(1,ISK) ,
     1                  IRAD  ,NDOF  ,IDDL ,JT   ,JR     ,
     2                  IBID  ,IBID ,RBID  ,RBID ,RBID   ,
     3                  A     ,RBID ,KSM   ,KNM  ,KRM    ,
     4                  IBID  ,IFSS ,IFSM  )
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),M,J)
          IAD = IAD_M(NSS) + J + IS -2
          CALL CP_REAL(9,KNM,KSL(1,IAD))
          CALL CP_REAL(9,KRM,KSL(1,IAD+NSREM))
         END DO ! IS = 1,NSREM
             END IF !(J.GT.0) THEN
        ELSE
         NSS = INLOC(NS)
         DO IS = 1,NSREM
          NR =IAD_SINR(IS+1)- IAD_SINR(IS)
          CALL INTABFR(NR,JDI_SINR(IAD_SINR(IS)),NS,J)
          IF (J.GT.0) THEN
          DO NF = 1,MAX(1,NF_SI(IS))
C----------------in KFR_SI------
           IF (IKC_SI(IS).GT.0.AND.NF_SI(IS).GT.0) THEN
            N = 0
            CALL GET_IAD(IAD_M1,IAD_SINR,JDI_SINR,N    ,IS  ,
     1                   J    ,IKC_SI(IS),NDOF  ,IAD  )
            IAD = IAD + NF-1
            CALL CP_REAL(9,KSI(1,IAD),KSM)
           ELSE
            ID = IDDL_SI(IS)
            NM = JDI_SINR(J+IAD_SINR(IS)-1)
            JD = IDDLI(NM)
            CALL GETFR_KIJ(ID    ,JD   ,IAD_SI ,JDI_SI,LT_SI  ,
     1                     KSM   ,ND   ,ND    )
           ENDIF
          CALL PRERBE2FR(IC    ,JT  ,JR   )
          CALL RBE2_FRK(NS    ,M    ,X     ,ISK  ,SKEW(1,ISK) ,
     1                  IRAD  ,NDOF  ,IDDL ,JT   ,JR    ,
     2                  IBID  ,IBID  ,RBID  ,RBID ,RBID  ,
     3                  A     ,RBID  ,KSM   ,KNM  ,KRM   ,
     4                  IBID  ,IFSS  ,IFSM  )
           NR=IAD_SLNR(NSS+1)- IAD_SLNR(NSS)
           CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),M,J)
           IAD = IAD_M(NSS) + J + IS -2
           CALL CP_REAL(9,KNM,KSL(1,IAD))
           CALL CP_REAL(9,KRM,KSL(1,IAD+NSREM))
          ENDDO
          ENDIF
         ENDDO
        ENDIF
       ENDDO
C------RBE3----------------------------------
       DO I=1,NRBE3_FR
        N=IFRS3(I)
        NS=IRBE3(3,N)
        IADR=IRBE3(1,N)
        NNOD=IRBE3(5,N)
        IROT =IRBE3(6,N)
        EID =IRBE3(2,N)
        IADS = SLRBE3/2+IADR
        CALL PRERBE3FR(IRBE3 ,N    ,JT  ,JR   )
        ALLOCATE(KNM3(9*NNOD))
        IF (IROT>0) ALLOCATE(KRM3(9*NNOD))
        IF (INLOC(NS).GT.NSL) THEN
          NSS = INLOC(NS)-NSL
          NR=IAD_SLNR(NSS+1)- IAD_SLNR(NSS)
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NS,J)
           IF (J.GT.0) THEN
          DO IS = 1,NSREM
           IAD = IAD_M(NSS) + J + IS -2
           CALL CP_REAL(9,KSL(1,IAD),KSM)
          CALL RBE3_FR0(NS    ,NNOD  ,LRBE3(IADR+1)  ,X     ,IROT   ,
     2                  JT    ,JR    ,FRBE3(6*IADR+1)  ,SKEW  ,IKC   ,
     3                  NDOF  ,IBID  ,IBID   ,RBID  ,RBID   ,
     2                  RBID  ,KSM   ,KNM3  ,KRM3  ,IBID   ,
     3                  IFSS  ,IFSM  ,ITAB  ,LRBE3(IADS+1),EID)
          DO K =1,NNOD
           NJ = LRBE3(IADR+K)
           CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NJ,J)
           IAD = IAD_M(NSS) + J +IS -2
           ID = 9*(K-1) + 1 
           CALL CP_REAL(9,KNM3(ID),KSL(1,IAD))
           IF (IROT>0.AND.NDOF(NJ)==6)
     +           CALL CP_REAL(9,KRM3(ID),KSL(1,IAD+NSREM))
          ENDDO
          END DO ! IS = 1,NSREM
           END IF !(J.GT.0) THEN
         ELSE
          NSS = INLOC(NS)
          DO IS = 1,NSREM
           NR =IAD_SINR(IS+1)- IAD_SINR(IS)
           CALL INTABFR(NR,JDI_SINR(IAD_SINR(IS)),NS,J)
           IF (J.GT.0) THEN
           DO NF = 1,MAX(1,NF_SI(IS))
C-------------------in KFR_SI----
            IF (IKC_SI(IS).GT.0.AND.NF_SI(IS).GT.0) THEN
             N = 0
             CALL GET_IAD(IAD_M1,IAD_SINR,JDI_SINR,N    ,IS  ,
     1                   J    ,IKC_SI(IS),NDOF  ,IAD  )
             IAD = IAD + NF-1
             CALL CP_REAL(9,KSI(1,IAD),KSM)
            ELSE
             ID = IDDL_SI(IS)
             NM = JDI_SINR(IAD_SINR(IS)+J-1)
             JD = IDDLI(NM)
             CALL GETFR_KIJ( ID    ,JD   ,IAD_SI ,JDI_SI,LT_SI  ,
     1                     KSM   ,ND   ,ND    )
            ENDIF
            CALL RBE3_FR0(NS    ,NNOD  ,LRBE3(IADR+1)  ,X     ,IROT   ,
     2                    JT    ,JR    ,FRBE3(6*IADR+1)  ,SKEW  ,IKC   ,
     3                    NDOF  ,IBID  ,IBID   ,RBID  ,RBID   ,
     2                    RBID  ,KSM   ,KNM3  ,KRM3  ,IBID   ,
     3                    IFSS  ,IFSM  ,ITAB  ,LRBE3(IADS+1),EID)
            DO K =1,NNOD
             NJ = LRBE3(IADR+K)
             NR =IAD_SLNR(NSS+1)- IAD_SLNR(NSS)
             CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NJ,J)
             IAD = IAD_M(NSS) + J + IS -2
             ID = 9*(K-1) + 1 
             CALL CP_REAL(9,KNM3(ID),KSL(1,IAD))
             IF (IROT>0.AND.NDOF(NJ)==6)
     +           CALL CP_REAL(9,KRM3(ID),KSL(1,IAD+NSREM))
            ENDDO
           ENDDO
           ENDIF !(J.GT.0)
          ENDDO
         ENDIF
         DEALLOCATE(KNM3)
         IF (IROT>0) DEALLOCATE(KRM3)
C
       ENDDO
C------Rigid bodies-------
       DO I=NRB_FR,1,-1
        M=IFRSR(1,I)
        NS=IFRSR(2,I)
        IF (INLOC(NS).GT.NSL) THEN
          NSS = INLOC(NS)-NSL
          NR=IAD_SLNR(NSS+1)- IAD_SLNR(NSS)
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NS,J)
             IF (J.GT.0) THEN
         DO IS = 1,NSREM
          IAD = IAD_M(NSS) + J + IS -2
          CALL CP_REAL(9,KSL(1,IAD),KSM)
          CALL RBY_FRK(NS   ,M    ,X     ,ITAB ,IKC  ,
     1                NDOF  ,IDDL ,IDDLM,IBID ,IBID ,
     2                RBID  ,RBID ,RBID ,A    ,RBID ,
     3                KSM   ,KNM  ,KRM  ,IBID,IFSS,IFSM)
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),M,J)
          IAD = IAD_M(NSS) + J + IS -2
          CALL CP_REAL(9,KNM,KSL(1,IAD))
          CALL CP_REAL(9,KRM,KSL(1,IAD+NSREM))
         END DO ! IS = 1,NSREM
             END IF !(J.GT.0) THEN
        ELSE
         NSS = INLOC(NS)
         DO IS = 1,NSREM
          NR =IAD_SINR(IS+1)- IAD_SINR(IS)
          CALL INTABFR(NR,JDI_SINR(IAD_SINR(IS)),NS,J)
          IF (J.GT.0) THEN
          DO NF = 1,MAX(1,NF_SI(IS))
C----------------in KFR_SI------
           IF (IKC_SI(IS).GT.0.AND.NF_SI(IS).GT.0) THEN
            N = 0
            CALL GET_IAD(IAD_M1,IAD_SINR,JDI_SINR,N    ,IS  ,
     1                   J    ,IKC_SI(IS),NDOF  ,IAD  )
            IAD = IAD + NF-1
            CALL CP_REAL(9,KSI(1,IAD),KSM)
           ELSE
            ID = IDDL_SI(IS)
            NM = JDI_SINR(J+IAD_SINR(IS)-1)
            JD = IDDLI(NM)
            CALL GETFR_KIJ(ID    ,JD   ,IAD_SI ,JDI_SI,LT_SI  ,
     1                     KSM   ,ND   ,ND    )
           ENDIF
           CALL RBY_FRK(NS   ,M    ,X     ,ITAB ,IKC  ,
     1                NDOF  ,IDDL ,IDDLM,IBID ,IBID ,
     2                RBID  ,RBID ,RBID ,A    ,RBID ,
     3                KSM   ,KNM  ,KRM  ,IBID,IFSS,IFSM)
           NR=IAD_SLNR(NSS+1)- IAD_SLNR(NSS)
           CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),M,J)
           IAD = IAD_M(NSS) + J + IS - 2
           CALL CP_REAL(9,KNM,KSL(1,IAD))
           CALL CP_REAL(9,KRM,KSL(1,IAD+NSREM))
          ENDDO
          ENDIF
         ENDDO
        ENDIF
       ENDDO
C
      DO I1 = 1,NBC_FR
        NS = IBC_FR(1,I1)
        ISK= IBC_FR(2,I1)
        ICT= IBC_FR(3,I1)
        IF (INLOC(NS).GT.NSL) THEN
          NSS = INLOC(NS)-NSL
          NR=IAD_SLNR(NSS+1)- IAD_SLNR(NSS)
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NS,J)
             IF (J.GT.0) THEN
         DO IS = 1,NSREM
          IAD = IAD_M(NSS) + J + IS -2
          CALL CP_REAL(9,KSL(1,IAD),KSM)
          CALL BCL_FRK(NS     ,IDDL  ,IDDLM  ,ICT  ,ISK   ,
     1                 SKEW   ,IKC   ,IBID  ,IBID  ,RBID  ,
     2                 RBID   ,RBID  ,A     ,RBID  ,KSM   ,
     3                 IBID   ,IFSS  ,IFSM  )
          CALL CP_REAL(9,KNM,KSL(1,IAD))
         END DO ! IS = 1,NSREM
             END IF !(J.GT.0) THEN
        ELSE
         NSS = INLOC(NS)
         DO IS = 1,NSREM
          NR =IAD_SINR(IS+1)- IAD_SINR(IS)
          CALL INTABFR(NR,JDI_SINR(IAD_SINR(IS)),NS,J)
          IF (J.GT.0) THEN
          DO NF = 1,MAX(1,NF_SI(IS))
C----------------in KFR_SI------
           IF (IKC_SI(IS).GT.0.AND.NF_SI(IS).GT.0) THEN
            N = 0
            CALL GET_IAD(IAD_M1,IAD_SINR,JDI_SINR,N    ,IS  ,
     1                   J    ,IKC_SI(IS),NDOF  ,IAD  )
            IAD = IAD + NF-1
            CALL CP_REAL(9,KSI(1,IAD),KSM)
           ELSE
            ID = IDDL_SI(IS)
            NM = JDI_SINR(IAD_SINR(IS)+J-1)
            JD = IDDLI(NM)
            CALL GETFR_KIJ( ID    ,JD   ,IAD_SI ,JDI_SI,LT_SI  ,
     1                     KSM   ,ND   ,ND    )
           ENDIF
           CALL BCL_FRK(NS    ,IDDL  ,IDDLM ,ICT   ,ISK   ,
     1                 SKEW   ,IKC   ,IBID  ,IBID  ,RBID  ,
     2                 RBID   ,RBID  ,A     ,RBID  ,KSM   ,
     3                 IBID   ,IFSS  ,IFSM  )
C--------------!!!!!!!!! still--in KFR_SI--!!!!!!----
           IF (IKC_SI(IS).GT.0.AND.NF_SI(IS).GT.0) THEN
            CALL CP_REAL(9,KNM,KSI(1,IAD))
           ELSE
            IAD = IAD_M(NSS) + IS -1
            CALL CP_REAL(9,KNM,KSL(1,IAD))
           ENDIF
          ENDDO
          ENDIF
         ENDDO
        ENDIF
      ENDDO
C
      DO I1 = 1,NSPC_FR
        N = ISPC_FR(I1)
        NS = IN_SPC(N)
            IADR = 6*(N-1)+1
            NN = IC_SPC(N)
        IF (INLOC(NS).GT.NSL) THEN
          NSS = INLOC(NS)-NSL
          NR=IAD_SLNR(NSS+1)- IAD_SLNR(NSS)
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NS,J)
             IF (J.GT.0) THEN
         DO IS = 1,NSREM
          IAD = IAD_M(NSS) + J + IS -2
          CALL CP_REAL(9,KSL(1,IAD),KSM)
          CALL BC_UPDFR(NS    ,IDDL  ,SKEW_SPC(IADR),JI    ,IDDLM  ,
     1                  IKC   ,IBID  ,IBID  ,RBID  ,RBID   ,
     2                  RBID  ,A     ,RBID  ,KSM   ,IBID   ,
     3                  IFSS  ,IFSM  )
              IF (NN.EQ.1) THEN
               EJ(1)=SKEW_SPC(IADR)
               EJ(2)=SKEW_SPC(IADR+1)
               EJ(3)=SKEW_SPC(IADR+2)
           CALL L_DIR(EJ,JI)
           CALL BC_UPDFR(I     ,IDDL  ,EJ     ,JI    ,IDDLM  ,
     1                   IKC   ,IBID  ,IBID  ,RBID  ,RBID   ,
     2                   RBID  ,A     ,RBID  ,KSM   ,IBID   ,
     3                   IFSS  ,IFSM  )
          ELSE
           CALL BC_UPDFR2(I     ,IDDL  ,SKEW_SPC(IADR),SKEW_SPC(IADR+3),
     1                   IDDLM  ,IBID  ,IBID  ,IBID  ,RBID   ,RBID   ,
     2                   RBID  ,A     ,RBID  ,KSM   ,IBID   ,
     3                   IFSS  ,IFSM  )
              END IF
          CALL CP_REAL(9,KNM,KSL(1,IAD))
         END DO ! IS = 1,NSREM
             END IF !(J.GT.0) THEN
        ELSE
         NSS = INLOC(NS)
         DO IS = 1,NSREM
          NR =IAD_SINR(IS+1)- IAD_SINR(IS)
          CALL INTABFR(NR,JDI_SINR(IAD_SINR(IS)),NS,J)
          IF (J.GT.0) THEN
          DO NF = 1,MAX(1,NF_SI(IS))
C----------------in KFR_SI------
           IF (IKC_SI(IS).GT.0.AND.NF_SI(IS).GT.0) THEN
            N = 0
            CALL GET_IAD(IAD_M1,IAD_SINR,JDI_SINR,N    ,IS  ,
     1                   J    ,IKC_SI(IS),NDOF  ,IAD  )
            IAD = IAD + NF-1
            CALL CP_REAL(9,KSI(1,IAD),KSM)
           ELSE
            ID = IDDL_SI(IS)
            NM = JDI_SINR(IAD_SINR(IS)+J-1)
            JD = IDDLI(NM)
            CALL GETFR_KIJ( ID    ,JD   ,IAD_SI ,JDI_SI,LT_SI  ,
     1                     KSM   ,ND   ,ND    )
           ENDIF
               IF (NN.EQ.1) THEN
                EJ(1)=SKEW_SPC(IADR)
                EJ(2)=SKEW_SPC(IADR+1)
                EJ(3)=SKEW_SPC(IADR+2)
           CALL L_DIR(EJ,JI)
           CALL BC_UPDFR(I     ,IDDL  ,EJ     ,JI    ,IDDLM  ,
     1                   IKC   ,IBID  ,IBID  ,RBID  ,RBID   ,
     2                   RBID  ,A     ,RBID  ,KSM   ,IBID   ,
     3                   IFSS  ,IFSM  )
           ELSE
           CALL BC_UPDFR2(I     ,IDDL  ,SKEW_SPC(IADR),SKEW_SPC(IADR+3),
     1                   IDDLM  ,IBID  ,IBID  ,IBID  ,RBID   ,RBID   ,
     2                   RBID  ,A     ,RBID  ,KSM   ,IBID   ,
     3                   IFSS  ,IFSM  )
               END IF
C--------------!!!!!!!!! still--in KFR_SI--!!!!!!----
           IF (IKC_SI(IS).GT.0.AND.NF_SI(IS).GT.0) THEN
            CALL CP_REAL(9,KNM,KSI(1,IAD))
           ELSE
            IAD = IAD_M(NSS) + IS -1
            CALL CP_REAL(9,KNM,KSL(1,IAD))
           ENDIF
          ENDDO
          ENDIF
         ENDDO
        ENDIF
      ENDDO
C
      IF (NFX_FR.GT.0) THEN
       DO L=1,NFX_FR
        I = IFX_FR(1,L)
        J1 = IFX_FR(2,L)
        NS=IABS(IBFV(1,I))
        IF (INLOC(NS).GT.NSL) THEN
          NSS = INLOC(NS)-NSL
          NR=IAD_SLNR(NSS+1)- IAD_SLNR(NSS)
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NS,J)
             IF (J.GT.0) THEN
         DO IS = 1,NSREM
          IAD = IAD_M(NSS) + J + IS -2
          CALL CP_REAL(9,KSL(1,IAD),KSM)
          CALL FVL_FRK(J1      ,I      ,IBFV  ,SKEW    ,XFRAME,
     1                 IDDL    ,IDDLM  ,IKC   ,IBID    ,IBID  ,
     2                 RBID    ,RBID   ,UD    ,RBID    ,A     ,
     3                 RBID    ,KSM    ,IBID  ,IFSS    ,IFSM  )
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NS,J)
          IAD = IAD_M(NSS) + J + IS -2
          CALL CP_REAL(9,KNM,KSL(1,IAD))
          CALL CP_REAL(9,KRM,KSL(1,IAD+NSREM))
         END DO ! IS = 1,NSREM
             END IF !(J.GT.0) THEN
        ELSE
         NSS = INLOC(NS)
         DO IS = 1,NSREM
          NR =IAD_SINR(IS+1)- IAD_SINR(IS)
          CALL INTABFR(NR,JDI_SINR(IAD_SINR(IS)),NS,J)
          IF (J.GT.0) THEN
          DO NF = 1,MAX(1,NF_SI(IS))
C----------------in KFR_SI------
           IF (IKC_SI(IS).GT.0.AND.NF_SI(IS).GT.0) THEN
            N = 0
            CALL GET_IAD(IAD_M1,IAD_SINR,JDI_SINR,N    ,IS  ,
     1                   J    ,IKC_SI(IS),NDOF  ,IAD  )
            IAD = IAD + NF-1
            CALL CP_REAL(9,KSI(1,IAD),KSM)
           ELSE
            ID = IDDL_SI(IS)
            NM = JDI_SINR(IAD_SINR(IS)+J-1)
            JD = IDDLI(NM)
            CALL GETFR_KIJ( ID    ,JD   ,IAD_SI ,JDI_SI,LT_SI  ,
     1                     KSM   ,ND   ,ND    )
           ENDIF
           CALL FVL_FRK(J1     ,I      ,IBFV  ,SKEW    ,XFRAME,
     1                 IDDL    ,IDDLM  ,IKC   ,IBID    ,IBID  ,
     2                 RBID    ,RBID   ,UD    ,RBID    ,A     ,
     3                 RBID    ,KSM    ,IBID  ,IFSS    ,IFSM  )
C--------------!!!!!!!!! still--in KFR_SI--!!!!!!----
           IF (IKC_SI(IS).GT.0.AND.NF_SI(IS).GT.0) THEN
            CALL CP_REAL(9,KNM,KSI(1,IAD))
           ELSE
            IAD = IAD_M(NSS) + IS -1
            CALL CP_REAL(9,KNM,KSL(1,IAD))
           ENDIF
          ENDDO
          ENDIF
         ENDDO
        ENDIF
       ENDDO
      ENDIF
C
      DO L = 1,NRW_FR
        I = IRW_FR(L)
        NS=IN_RWL(I)
        EJ(1)=NOR_RWL(1,I)
        EJ(2)=NOR_RWL(2,I)
        EJ(3)=NOR_RWL(3,I)
        CALL L_DIR(EJ,J1)
        IF (INLOC(NS).GT.NSL) THEN
          NSS = INLOC(NS)-NSL
          NR=IAD_SLNR(NSS+1)- IAD_SLNR(NSS)
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NS,J)
             IF (J.GT.0) THEN
         DO IS = 1,NSREM
          IAD = IAD_M(NSS) + J + IS -2
          CALL CP_REAL(9,KSL(1,IAD),KSM)
          CALL FV_UPDFR(NS   ,EJ    ,J1    ,IDDL  ,IDDLM  ,
     1                 IKC   ,IBID  ,IBID  ,RBID  ,RBID    ,
     2                 UD    ,RBID  ,A     ,RBID  ,KSM     ,
     3                 IBID  ,IFSS  ,IFSM  )
          CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NS,J)
          IAD = IAD_M(NSS) + J + IS -2
          CALL CP_REAL(9,KNM,KSL(1,IAD))
          CALL CP_REAL(9,KRM,KSL(1,IAD+NSREM))
         END DO ! IS = 1,NSREM
             END IF !(J.GT.0) THEN
        ELSE
         NSS = INLOC(NS)
         DO IS = 1,NSREM
          NR =IAD_SINR(IS+1)- IAD_SINR(IS)
          CALL INTABFR(NR,JDI_SINR(IAD_SINR(IS)),NS,J)
          IF (J.GT.0) THEN
          DO NF = 1,MAX(1,NF_SI(IS))
C----------------in KFR_SI------
           IF (IKC_SI(IS).GT.0.AND.NF_SI(IS).GT.0) THEN
            N = 0
            CALL GET_IAD(IAD_M1,IAD_SINR,JDI_SINR,N    ,IS  ,
     1                   J    ,IKC_SI(IS),NDOF  ,IAD  )
            IAD = IAD + NF-1
            CALL CP_REAL(9,KSI(1,IAD),KSM)
           ELSE
            ID = IDDL_SI(IS)
            NM = JDI_SINR(IAD_SINR(IS)+J-1)
            JD = IDDLI(NM)
            CALL GETFR_KIJ( ID    ,JD   ,IAD_SI ,JDI_SI,LT_SI  ,
     1                     KSM   ,ND   ,ND    )
           ENDIF
           CALL FV_UPDFR(NS   ,EJ    ,J1    ,IDDL  ,IDDLM  ,
     1                 IKC   ,IBID  ,IBID  ,RBID  ,RBID    ,
     2                 UD    ,RBID  ,A     ,RBID  ,KSM     ,
     3                 IBID  ,IFSS  ,IFSM  )
C--------------!!!!!!!!! still--in KFR_SI--!!!!!!----
           IF (IKC_SI(IS).GT.0.AND.NF_SI(IS).GT.0) THEN
            CALL CP_REAL(9,KNM,KSI(1,IAD))
           ELSE
            IAD = IAD_M(NSS) +  IS -1
            CALL CP_REAL(9,KNM,KSL(1,IAD))
           ENDIF
          ENDDO
          ENDIF
         ENDDO
        ENDIF
      ENDDO
C--------------------------------------------
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        GETFR_KIJ                     source/mpi/implicit/imp_fri.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE IMP_FRKM(
     1    NSREM     ,IKINM     ,IKCSI   ,IKC     ,NDOF    ,
     2    IDDL      ,IDDLM     ,INLOC   ,IAD_M   ,FRK_SI  ,
     3    FRK_SL    ,NF_SI     ,IAD_MLD ,IDDLI   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .   NSREM,IDDL(*),IDDLM(*),INLOC(*),IKC(*),NDOF(*),IAD_M(*),
     .   IKCSI(3,*),IKINM ,NF_SI(*) ,IAD_MLD(*) ,IDDLI(*)
      my_real
     .   FRK_SI(3,3,*) ,FRK_SL(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,K,P,L,NL,NJ,NDOFI,NZ,NDS,NKC,IAD,J1,K1,ID,NS,NM
      INTEGER IDDL_CP(NDDL_SI),IAD_CP(NDDL_SI+1),JDI_CP(NZ_SI),
     .        IAD_M1(NSREM+1),NR,IDM,JD,IADI,
     .        IER1,IER2,IER3,IER4,IER5,IER6
      my_real
     .  KIJ(3,3),LT_CP(NZ_SI)
c----------IDDL_SL->condense--For the case of no Kine nodes should change also local id->globle --
C-------------(debug) 257564: 
       CALL CP_INT((NDDL_SI+1),IAD_SI,IAD_CP)
       CALL CP_INT(NZ_SI,JDI_SI,JDI_CP)
       CALL CP_REAL(NZ_SI,LT_SI,LT_CP)
c---------ini IAD_MLD----
       NDOFI = 3
       NL = 0
       NZ = 0
      IAD_MLD(1) = NL +1
      IF (IKINM.EQ.0 ) THEN
       DO P=1,NSPMD
        DO I=IAD_SREM(P),IAD_SREM(P+1)-1
         NL = NL + NDOFI
         DO K = 1,NDOFI
          DO J1 =IAD_SINR(I), IAD_SINR(I+1)-1
           NJ = JDI_SINR(J1)
           ID = IDDL(NJ)
           IDM = IDDLM(NJ)
C-----------INPEND. LT_CP-------
            IF (NDOF(NJ)>0) THEN
             ID = IDDL_SI(I)
             JD = IDDLI(NJ)
             CALL GETFR_KIJ( ID    ,JD   ,IAD_CP ,JDI_CP,LT_CP  ,
     1                       KIJ   ,NDOFI,NDOFI )
             DO K1 = 1,NDOFI
               NZ = NZ + 1
               JDI_SI(NZ) = IDM + K1
               LT_SI(NZ) = KIJ(K,K1)
             ENDDO
            END IF !(NDOF(NJ)>0) THEN
           END DO !J1 =IAD_SINR(I), IAD_SINR(I+1)-1
         END DO !DO K = 1,NDOFI
        END DO
        IAD_MLD(P+1) = NL +1
       END DO !DO P=1,NSPMD
      ELSE
C
       IAD_M1(1) = 0
       DO N=1,NSREM
        IAD_M1(N+1) = IAD_M1(N)+IKC_SI(N)*NF_SI(N)
       ENDDO
c----------DIMENSION----
       NL = 0
       NZ = 0
       IAD = 1
       DO I=1,NSREM
C-----------SECND DEPEND.: FRK_SI---------
        IF (IKC_SI(I).GT.0) THEN
         DO J =1, NF_SI(I)
          DO K = 1,NDOFI
           IF (IKCSI(K,IAD).EQ.0) THEN
            NL = NL + 1
            DO J1 =IAD_SINR(I), IAD_SINR(I+1)-1
             NJ = JDI_SINR(J1)
             ID = IDDL(NJ)
             DO K1 = 1,NDOFI
              IF (IKC(K1+ID).EQ.0) NZ = NZ + 1
             ENDDO
C-----------DEPEND. SUP-:FRK_SL--------
             NS = INLOC(NJ)
            IF (IKC_SL(NS).GT.0) THEN
             DO L =IAD_SLNR(NS), IAD_SLNR(NS+1)-1
              NM = JDI_SLNR(L)
              ID = IDDL(NM)
              DO K1 = 1,NDOFI
               IF (IKC(K1+ID).EQ.0) NZ = NZ + 1
              ENDDO
             ENDDO
            ENDIF
            ENDDO
           ENDIF
          ENDDO
          IAD = IAD + 1
         ENDDO
C-----------SECND INDEPEND.:LT,FRK_SL---------
        ELSE
         DO K = 1,NDOFI
          NL = NL + 1
          DO J1 =IAD_SINR(I), IAD_SINR(I+1)-1
           NJ = JDI_SINR(J1)
           ID = IDDL(NJ)
           NS = INLOC(NJ)
           IF (IKC_SL(NS).GT.0) THEN
C-----------DEPEND. SUP-:FRK_SL--------
C-------------debug 257564: forget /BCS
            NR = IAD_SLNR(NS+1)-IAD_SLNR(NS)
                IF (NR==0) THEN
              DO K1 = 1,NDOFI
               IF (IKC(K1+ID).EQ.0) NZ = NZ + 1
              ENDDO
                ELSE
            DO L =IAD_SLNR(NS), IAD_SLNR(NS+1)-1
             NM = JDI_SLNR(L)
             ID = IDDL(NM)
              DO K1 = 1,NDOFI
               IF (IKC(K1+ID).EQ.0) NZ = NZ + 1
              ENDDO
             IF (NDOF(NM)==6) THEN
              DO K1 = 1,NDOFI
               IF (IKC(K1+ID+3).EQ.0) NZ = NZ + 1
              ENDDO
             END IF
            ENDDO
                END IF !(NR==0) THEN
           ELSE
C-----------INPEND. LT_CP-------
            NZ = NZ + NDOFI
           ENDIF
          ENDDO
         ENDDO
        ENDIF
       ENDDO
C
       NDDL_SI = NL
C
       IF(ALLOCATED(IAD_SI)) DEALLOCATE(IAD_SI)
       ALLOCATE(IAD_SI(NL+1),STAT=IER1)
      IF (NZ.GT.NZ_SI) THEN
       IF(ALLOCATED(JDI_SI)) DEALLOCATE(JDI_SI)
       ALLOCATE(JDI_SI(NZ),STAT=IER2)
       IF(ALLOCATED(LT_SI)) DEALLOCATE(LT_SI)
       ALLOCATE(LT_SI(NZ),STAT=IER3)
      ENDIF
      NZ_SI =NZ
C
       NL = 0
       NZ = 0
       IAD = 1
       IAD_SI(NL+1) = NZ+1
       DO P=1,NSPMD
        DO I=IAD_SREM(P),IAD_SREM(P+1)-1
C-----------SECND DEPEND.: FRK_SI---------
        IF (IKC_SI(I).GT.0) THEN
         NR = IAD_SINR(I+1)-IAD_SINR(I)
         DO J =1, NF_SI(I)
          DO K = 1,NDOFI
           IF (IKCSI(K,IAD).EQ.0) THEN
            NL = NL + 1
            DO J1 =IAD_SINR(I), IAD_SINR(I+1)-1
             NJ = JDI_SINR(J1)
             ID = IDDL(NJ)
             IDM = IDDLM(NJ)
             IF (NDOF(NJ)>0) THEN
             NKC = 0
             IADI = IAD_M1(I) +NF_SI(I)*(J1-IAD_SINR(I))+ J
             DO K1 = 1,NDOFI
              IF (IKC(K1+ID).EQ.0) THEN
               NZ = NZ + 1
               JDI_SI(NZ) = IDM + K1-NKC
               LT_SI(NZ) = FRK_SI(K,K1,IADI)
              ELSE
               NKC = NKC + 1
              ENDIF
             ENDDO
             END IF !(NDOF(NJ)>0) THEN
C-----------DEPEND. SUP-:FRK_SL--------
             NS = INLOC(NJ)
            IF (IKC_SL(NS).GT.0) THEN
             DO L =IAD_SLNR(NS), IAD_SLNR(NS+1)-1
              NM = JDI_SLNR(L)
              ID = IDDL(NM)
              IDM = IDDLM(NM)
              IF (NDOF(NM)>0) THEN
              NKC = 0
              IADI = IAD_M(NS) + L-IAD_SLNR(NS) + I
              DO K1 = 1,NDOFI
               IF (IKC(K1+ID).EQ.0) THEN
                NZ = NZ + 1
                JDI_SI(NZ) = IDM + K1-NKC
                LT_SI(NZ) = FRK_SL(K,K1,IADI)
               ELSE
                NKC = NKC + 1
               ENDIF
              ENDDO
              END IF !(NDOF(NM)>0) THEN
             ENDDO
            ENDIF
            ENDDO
            IAD_SI(NL+1) = NZ+1
           ENDIF
          ENDDO
          IAD = IAD + 1
         ENDDO
C-----------SECND INDEPEND.:LT,FRK_SL---------
        ELSE
         DO K = 1,NDOFI
          NL = NL + 1
          DO J1 =IAD_SINR(I), IAD_SINR(I+1)-1
           NJ = JDI_SINR(J1)
           ID = IDDL(NJ)
           IDM = IDDLM(NJ)
c           IF (NDOF(NJ)>0) THEN
C
           NKC = 0
           NS = INLOC(NJ)
           IF (IKC_SL(NS).GT.0) THEN
C-----------DEPEND. SUP-:FRK_SL--------
            NR = IAD_SLNR(NS+1)-IAD_SLNR(NS)
C-----------/BCS--------
                IF (NR==0) THEN
             JD = IDDLI(NJ)
             CALL GETFR_KIJ(IDDL_SI(I),JD   ,IAD_CP ,JDI_CP,LT_CP  ,
     1                       KIJ   ,NDOFI,NDOFI )
             NKC = 0
             DO K1 = 1,NDOFI
               IF (IKC(K1+ID).EQ.0) THEN
                NZ = NZ + 1
                JDI_SI(NZ) = IDM + K1 -NKC
                LT_SI(NZ) = KIJ(K,K1)
               ELSE
                NKC = NKC + 1
               END IF !(IKC(K1+ID).EQ.0) THEN
             ENDDO
                ELSE
            DO L =IAD_SLNR(NS), IAD_SLNR(NS+1)-1
             NM = JDI_SLNR(L)
             ID = IDDL(NM)
             IDM = IDDLM(NM)
             NKC = 0
             IADI = IAD_M(NS) + L-IAD_SLNR(NS) +I-1
              DO K1 = 1,NDOFI
               IF (IKC(K1+ID).EQ.0) THEN
                NZ = NZ + 1
                JDI_SI(NZ) = IDM + K1-NKC
                LT_SI(NZ) = FRK_SL(K1,K,IADI)
               ELSE
                NKC = NKC + 1
               ENDIF
              ENDDO
              IF (NDOF(NM)==6) THEN
               DO K1 = 4,NDOFI+3
                IF (IKC(K1+ID).EQ.0) THEN
                 NZ = NZ + 1
                 JDI_SI(NZ) = IDM + K1-NKC
                 LT_SI(NZ) = FRK_SL(K1-3,K,IADI+NSREM)
                ELSE
                 NKC = NKC + 1
                ENDIF
               ENDDO
              END IF
             ENDDO
                 END IF !(NR==0) THEN
           ELSEIF (NDOF(NJ)>0) THEN
C-----------INPEND. LT_CP-------
             ID = IDDL_SI(I)
             JD = IDDLI(NJ)
             CALL GETFR_KIJ( ID    ,JD   ,IAD_CP ,JDI_CP,LT_CP  ,
     1                       KIJ   ,NDOFI,NDOFI )
              DO K1 = 1,NDOFI
               NZ = NZ + 1
               JDI_SI(NZ) = IDM + K1
               LT_SI(NZ) = KIJ(K,K1)
              ENDDO
           ENDIF !IF IKC_SL
C           END IF !(NDOF(NJ)>0) THEN
          ENDDO !DO J1
          IAD_SI(NL+1) = NZ+1
         ENDDO !DO K
        ENDIF !IF IKC_SI
        ENDDO !DO I=
        IAD_MLD(P+1) = NL+1
       ENDDO ! DO P=
      END IF !IF (IKINM.EQ.0 )
C
       IF(ALLOCATED(IDDL_SI)) DEALLOCATE(IDDL_SI)
c       IF(ALLOCATED(IKC_SI)) DEALLOCATE(IKC_SI)
       IF(ALLOCATED(IKC_SL)) DEALLOCATE(IKC_SL)
       IF(ALLOCATED(USI)) DEALLOCATE(USI)
       ALLOCATE(USI(NDDL_SI),STAT=IER4)
       IF(ALLOCATED(FSI)) DEALLOCATE(FSI)
       ALLOCATE(FSI(NDDL_SI),STAT=IER5)
C
      RETURN
      END
Chd|====================================================================
Chd|  DIM_FVN                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DIM_FVN(
     1    NDOF      ,IDDL      ,IKC       ,INLOC     ,NFV      )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFV, INLOC(*),NDOF(*),IKC(*),IDDL(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .        I,J,K,N,ND,NS
C----------------------------
      NFV=0
C--------imposed velocity---------------
       DO N = 1, NUMNOD
C--------local secnd node-----
        IF (INLOC(N).GT.0) THEN
         NS = 0
         DO J = 1, MIN(3,NDOF(N))
          ND = IDDL(N)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) NS=1
         ENDDO
         IF (NS.EQ.1) NFV = NFV +1
        ENDIF
       ENDDO
C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IND_FVN                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE IND_FVN(
     1    NDOF      ,IDDL      ,IKC       ,INLOC     ,NFV     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFV,INLOC(*),NDOF(*),IKC(*),IDDL(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .        I,J,K,N,ND,NS,NF
C-----------use islm-----------------
      IF(ALLOCATED(ISLM)) DEALLOCATE(ISLM)
      ALLOCATE(ISLM(NFV))
      NF=0
C--------imposed velocity---------------
       DO N = 1, NUMNOD
C--------local secnd node-----
        IF (INLOC(N).GT.0) THEN
         NS=0
         DO J = 1, MIN(3,NDOF(N))
          ND = IDDL(N)+J
          IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) NS = 1
         ENDDO
         IF (NS.EQ.1) THEN
          NF = NF + 1
          ISLM(NF) = N
         ENDIF
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FVKSS                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        GET_KII                       source/implicit/imp_glob_k.F  
Chd|        INTABFR                       source/mpi/implicit/imp_fri.F 
Chd|        PUT_KII                       source/implicit/imp_glob_k.F  
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE IMP_FVKSS(
     1    KSS      ,IDDL      ,IDDLM     ,IKC       ,NSL       ,
     2    D_IMP    ,LB        ,NFV       ,UDSL      ,INLOC     ,
     3    NDOF     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFV,NSL,IDDLM(*),IKC(*),IDDL(*),
     .        INLOC(*),NDOF(*)
C     REAL ou REAL*8
      my_real
     .     KSS(6,*),  D_IMP(3,*) ,LB(*) ,UDSL(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .       I,J,K,N,ID,ND,NS,NKC,NM,IS,NR,NJ,NN,NL,IM,
     .       IAD_M1(NSL+1),IADN(NSL),JJ
      my_real
     .       F_IMP(6) ,KII(6,6)
C----------------------------
       IAD_M1(1) = 1
         NN = 0
       DO I=1,NSL
        NL =0
        IF (IKC_SL(I).GT.0) THEN
         NR = IAD_SLNR(I+1)-IAD_SLNR(I)
         NL = MAX(1,NR)
         NN = NN + NL
         IADN(I) = NN
        ENDIF
        IAD_M1(I+1) = IAD_M1(I)+NL
       ENDDO
       DO I = 1, NFV
        N = ISLM(I)
        ID = IDDL(N)
        IM = IDDLM(N)
        DO K = 1, 3
         ND = ID + K
         IF ((IKC(ND).GE.2.AND.IKC(ND).LE.4).OR.IKC(ND).EQ.9) THEN
           UDSL(K,I) = D_IMP(K,N)
         ELSE
           UDSL(K,I) = ZERO
         ENDIF
        ENDDO
        DO IS = 1, NSL
        IF (INLOC(N).GT.NSL) THEN
         NR = IAD_SLNR(IS+1)-IAD_SLNR(IS)
         CALL INTABFR(NR,JDI_SLNR(IS),N,J)
         IF (J.GT.0) THEN
          NN = IAD_M1(IS) + J -1
          ND = NDOF(N)
          CALL GET_KII(NN ,IDDL_SL,IAD_SS,DIAG_SL,LT_SL,KII,ND)
          DO J = 1, ND
          DO K = J+1, ND
           KII(K,J) = KII(J,K)
          ENDDO
          ENDDO
          DO K = 1, ND
           F_IMP(K) = KII(K,1)*UDSL(1,I)+ KII(K,2)*UDSL(2,I)+
     .                KII(K,3)*UDSL(3,I)
          ENDDO
          NKC = 1
          DO K = 1, ND
           IF (IKC(ID+K).EQ.0) THEN
            NM = IM + NKC
            LB(NM) = LB(NM) -F_IMP(K)
            NKC = NKC + 1
           ENDIF
          ENDDO
         ENDIF
        ELSE
         IF (N.EQ.ISL(IS)) THEN
              J = IS
          F_IMP(1) = KSS(1,J)*UDSL(1,I)+ KSS(4,J)*UDSL(2,I)+
     .              KSS(5,J)*UDSL(3,I)
          F_IMP(2) = KSS(4,J)*UDSL(1,I)+ KSS(2,J)*UDSL(2,I)+
     .              KSS(6,J)*UDSL(3,I)
          F_IMP(3) = KSS(5,J)*UDSL(1,I)+ KSS(6,J)*UDSL(2,I)+
     .              KSS(3,J)*UDSL(3,I)
          ND = 3
          NKC = 1
          DO K = 1, ND
           IF (IKC(ID+K).EQ.0) THEN
            NM = IM + NKC
            LB(NM) = LB(NM) -F_IMP(K)
            NKC = NKC + 1
           ENDIF
          ENDDO
          DO K = 1, ND
           KII(K,K) = KSS(K,J)
          ENDDO
          KII(1,2)=KSS(4,J)
          KII(1,3)=KSS(5,J)
          KII(2,3)=KSS(6,J)
          KII(2,1)=KII(1,2)
          KII(3,1)=KII(1,3)
          KII(3,2)=KII(2,3)
          CALL PUT_KII(IADN(J),IDDL_SL,IAD_SS,DIAG_SL,LT_SL,KII,ND)
         ENDIF
        ENDIF
        ENDDO  ! IS = 1, NSL
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  INI_FRUD                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        INTABFR                       source/mpi/implicit/imp_fri.F 
Chd|        SPMD_EXCI                     source/mpi/implicit/imp_spmd.F
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE INI_FRUD(NSREM ,NSL   ,NFV   ,IFVSI ,IFVSL ,
     .                    NF_SI ,NF_SL ,LVSI  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSREM ,NSL,NFV ,IFVSI(*) ,IFVSL(*)  ,
     .        NF_SL(*),NF_SI(*),LVSI
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,K,IAD,NJ,L
      INTEGER SSIZE ,RSIZE,SIZE,IAD_S(NSPMD+1),IAD_R(NSPMD+1)
C--------------------------------------------
        IAD = 1
        DO I = 1, NSL
         IF (IKC_SL(I).GT.0) THEN
           DO K=1,NF_SL(I)
            IFVSL(IAD) = 0
            IAD = IAD + 1
           ENDDO
         ENDIF
        ENDDO
C
        IAD = 1
        DO I = 1, NSL
         IF (IKC_SL(I).GT.0.AND.NF_SL(I).GT.0) THEN
          N = ISL(I)
          CALL INTABFR(NFV,ISLM,N,J)
          IF (J.GT.0) THEN
           IFVSL(IAD) = J
          ELSE
           DO K=IAD_SLNR(I),IAD_SLNR(I+1)-1
            NJ = JDI_SLNR(K)
            CALL INTABFR(NFV,ISLM,NJ,L)
            IF (L.GT.0) IFVSL(IAD+K-IAD_SLNR(I)) = L
           ENDDO
          ENDIF
          IAD = IAD + NF_SL(I)
         ENDIF
        ENDDO
C
      IAD_S(1) = 1
      IAD_R(1) = 1
       DO I=1,NSPMD
        IAD_S(I+1) = IAD_S(I)
        IAD_R(I+1) = IAD_R(I)
        DO J=IAD_SL(I),IAD_SL(I+1)-1
         IAD_S(I+1) = IAD_S(I+1) + NF_SL(J)
        END DO
        DO J=IAD_SREM(I),IAD_SREM(I+1)-1
         IAD_R(I+1) = IAD_R(I+1) + NF_SI(J)
        END DO
       END DO
C
      SIZE = 1
      SSIZE = IAD_S(NSPMD+1) - 1
      RSIZE = IAD_R(NSPMD+1) - 1
      CALL SPMD_EXCI(IFVSL,IFVSI,IAD_S,IAD_R,SIZE ,SSIZE,RSIZE)
C
      LVSI = 0
        IAD = 1
        DO I = 1, NSREM
         IF (IKC_SI(I).GT.0) THEN
           DO K=1,NF_SI(I)
            IF (IFVSI(IAD) .GT. 0) LVSI = LVSI +1
            IAD = IAD + 1
           ENDDO
         ENDIF
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SCOM_FRUD                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        SPMD_EXCK                     source/mpi/implicit/imp_spmd.F
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE SCOM_FRUD(UDS,UDR,NF_S,NF_R,IKCS,IKCR )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKCS(*),IKCR(*),NF_S(*),NF_R(*)
      my_real
     .     UDS(3,*),UDR(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,K ,L,IADS,IADR,IER1,ID,ID0
      INTEGER SSIZE ,RSIZE,SIZE,IAD_S(NSPMD+1),IAD_R(NSPMD+1)
      my_real, DIMENSION(:,:),ALLOCATABLE :: UDS1
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      SIZE = 3
      IADS = 1
      IADR = 1
      IAD_S(1) = 1
      IAD_R(1) = 1
       DO I=1,NSPMD
        IAD_S(I+1) = IAD_S(I)
        IAD_R(I+1) = IAD_R(I)
        DO J=IAD_SL(I),IAD_SL(I+1)-1
         DO K = 1,NF_S(J)
          IAD_S(I+1) = IAD_S(I+1) + MIN(1,IKCS(IADS))
          IADS = IADS + 1
         END DO
        END DO
        DO J=IAD_SREM(I),IAD_SREM(I+1)-1
         DO K = 1,NF_R(J)
          IAD_R(I+1) = IAD_R(I+1) + MIN(1,IKCR(IADR))
          IADR = IADR + 1
         END DO
        END DO
       END DO
      SSIZE = IAD_S(NSPMD+1) - 1
      RSIZE = IAD_R(NSPMD+1) - 1
      IF (SSIZE.GT.0) THEN
       ALLOCATE(UDS1(3,SSIZE),STAT=IER1)
       IADS = 1
       DO I=1,NSPMD
        ID = IAD_S(I)
        DO J=IAD_SL(I),IAD_SL(I+1)-1
         DO K = 1,NF_S(J)
          ID0 = IKCS(IADS)
              IF (ID0.GT.0) THEN
           DO L = 1,3
            UDS1(L,ID) = UDS(L,ID0)
           END DO
           ID = ID + 1
          ENDIF
          IADS = IADS + 1
         END DO
        END DO
       END DO
      ENDIF
      CALL SPMD_EXCK(UDS1,UDR ,IAD_S,IAD_R,SIZE ,SSIZE,RSIZE)
      IF (SSIZE.GT.0) DEALLOCATE(UDS1)
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FVKSL                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_FVKSM                     source/mpi/implicit/imp_fri.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE IMP_FVKSL(
     1    IDDL      ,IDDLM     ,IKC       ,IFVSI     ,NF_SI     ,
     2    KSI       ,LB        ,NSREM     ,UDSI      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSREM,IDDLM(*),IKC(*),IDDL(*),IFVSI(*),NF_SI(*)
C     REAL ou REAL*8
      my_real
     .     KSI(9,*),LB(*) ,UDSI(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .      I,J,J1,NJ,IAD,IADU,IADI,ID,ND,JD,IAD_M1(NSREM+1)
C----------------------------
       IAD_M1(1) = 0
       DO I=1,NSREM
        IAD_M1(I+1) = IAD_M1(I)+IKC_SI(I)*NF_SI(I)
       ENDDO
       IAD = 0
       IADU = 0
       DO I=1,NSREM
        IF (IKC_SI(I).GT.0) THEN
          DO J =1, NF_SI(I)
           IAD = IAD + 1
           IF (IFVSI(IAD).GT.0) THEN
            IADU = IADU + 1
            DO J1 = IAD_SINR(I),IAD_SINR(I+1)-1
             NJ = JDI_SINR(J1)
             IADI = IAD_M1(I) +NF_SI(I)*(J1-IAD_SINR(I))+ J
             CALL IMP_FVKSM(
     1            NJ       ,IDDL      ,IDDLM     ,IKC       ,
     2            UDSI(1,IADU),KSI(1,IADI),LB        )
            ENDDO
           ENDIF
          ENDDO
        ENDIF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FVKSM                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FVKSL                     source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IMP_FVKSM(
     1    NJ       ,IDDL      ,IDDLM     ,IKC       ,
     2    UDS      ,KSM       ,LB        )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NJ,IDDLM(*),IKC(*),IDDL(*)
C     REAL ou REAL*8
      my_real
     .     KSM(3,3),  LB(*) ,UDS(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .        K,N,ND,NS,NKC,NM
      my_real
     .       LBD(3)
C----------------------------
         DO K=1,3
          LBD(K) = KSM(1,K)*UDS(1)+ KSM(2,K)*UDS(2)+
     .             KSM(3,K)*UDS(3)
         ENDDO
C
         NKC = 1
         DO K = 1, 3
          ND = IDDL(NJ)+K
          NM = IDDLM(NJ) + NKC
          IF (IKC(ND).EQ.0) THEN
           LB(NM) = LB(NM) -LBD(K)
           NKC = NKC + 1
          ENDIF
         ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FVKM                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        GETFR_KIJ                     source/mpi/implicit/imp_fri.F 
Chd|        GET_IAD                       source/mpi/implicit/imp_fri.F 
Chd|        INTABFR                       source/mpi/implicit/imp_fri.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE IMP_FVKM(
     1    KFR_SI   ,KFR_SL    ,IDDL      ,NDOF      ,IKC       ,
     2    INLOC    ,IAD_M     ,NSREM     ,NSL       ,UD0       ,
     3    FDSI     ,NF_SI     ,NFV       ,NFD       ,IDDLI     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSREM,IAD_M(*),NSL,IDDL(*),NDOF(*),INLOC(*),
     .        IKC(*),NF_SI(*),NFV,NFD,IDDLI(*)
C     REAL ou REAL*8
      my_real
     .     UD0(3,*),KFR_SI(3,3,*),KFR_SL(3,3,*),FDSI(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,K,M,NS,IAD_M1(NSREM+1),IS,JD,
     .        NJ,ND,NL,J1,NSS,NM,ID,IAD,NF,NR,IADI
      my_real
     .     KSM(3,3),UD(3,NFV)
C----------------------------
       ND = 3
       DO I = 1, NFD
        DO K =1,ND
         FDSI(K,I) = ZERO
        ENDDO
       ENDDO
       IAD_M1(1) = 1
       DO N=1,NSREM
        IAD_M1(N+1) = IAD_M1(N)+IKC_SI(N)*NF_SI(N)
       ENDDO
       DO I = 1, NFV
        NS = ISLM(I)
        DO K =1,ND
         ID = IDDL(NS)+K
         IF ((IKC(ID).GE.2.AND.IKC(ID).LE.4).OR.IKC(ID).EQ.9) THEN
          UD(K,I)=UD0(K,NS)
         ELSE
          UD(K,I)=ZERO
         ENDIF
        ENDDO
        IAD = 0
        DO IS = 1,NSREM
         NR =IAD_SINR(IS+1)- IAD_SINR(IS)
         CALL INTABFR(NR,JDI_SINR(IAD_SINR(IS)),NS,J)
          DO NF = 1,MAX(1,NF_SI(IS))
           IAD = IAD + 1
C----------------in KFR_SL------
           IF (INLOC(NS).GT.NSL) THEN
C
               DO J = IAD_SINR(IS), IAD_SINR(IS+1)-1
            NJ = JDI_SINR(J)
            NSS = INLOC(NJ)
            NR=IAD_SLNR(NSS+1)- IAD_SLNR(NSS)
            CALL INTABFR(NR,JDI_SLNR(IAD_SLNR(NSS)),NS,J1)
            IF (J1.GT.0) THEN
            IADI = IAD_M(NSS) + J1 + IS -2
            DO K =1,ND
             FDSI(K,IAD) = FDSI(K,IAD)+KFR_SL(1,K,IADI)*UD(1,I)+
     .              KFR_SL(2,K,IADI)*UD(2,I)+KFR_SL(3,K,IADI)*UD(3,I)
            ENDDO
            ENDIF !(J1.GT.0)
               ENDDO
C----------------in KFR_SI------
           ELSEIF (IKC_SI(IS).GT.0.AND.J.GT.0) THEN
            N = 0
            CALL GET_IAD(IAD_M1,IAD_SLNR,JDI_SLNR,N    ,IS  ,
     1                   J    ,IKC_SI(IS),NDOF  ,IADI  )
            IADI = IADI + NF-1
            DO K =1,ND
             FDSI(K,IAD) = FDSI(K,IAD)+KFR_SI(1,K,IADI)*UD(1,I)+
     .              KFR_SI(2,K,IADI)*UD(2,I)+KFR_SI(3,K,IADI)*UD(3,I)
            ENDDO
           ELSEIF (J.GT.0) THEN
            ID = IDDL_SI(IS)
            NM = JDI_SINR(IAD_SINR(IS)+J-1)
            JD = IDDLI(NM)
            CALL GETFR_KIJ( ID    ,JD   ,IAD_SI ,JDI_SI,LT_SI  ,
     1                      KSM   ,ND   ,ND    )
            DO K =1,ND
              FDSI(K,IAD) = FDSI(K,IAD)+KSM(1,K)*UD(1,I)+
     .                    KSM(2,K)*UD(2,I)+KSM(3,K)*UD(3,I)
            ENDDO
           ENDIF
          ENDDO
        ENDDO
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  INI_FRFD                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE INI_FRFD(NSREM   ,NFV   ,IKCSI   ,NF_SI     ,FDSI )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .   NSREM,IKCSI(3,*),NF_SI(*)  ,NFV
      my_real
     .     FDSI(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,K,NL,NDOFI,IAD,IADI
C
       DO I=1,NDDL_SI
        FSI(I) = ZERO
       ENDDO
C
       IF (NFV.EQ.0) RETURN
       NDOFI = 3
       NL = 0
       IAD = 1
       IADI = 1
       DO I=1,NSREM
C-----------SECND DEPEND.: FRK_SI---------
        IF (IKC_SI(I).GT.0) THEN
         DO J =1, NF_SI(I)
          DO K = 1,NDOFI
           IF (IKCSI(K,IAD).EQ.0) THEN
            NL = NL + 1
            FSI(NL)=-FDSI(K,IADI+J-1)
           ENDIF
          ENDDO
          IAD = IAD + 1
         ENDDO
C-----------SECND INDEPEND.:LT,FRK_SL---------
        ELSE
         DO K = 1,NDOFI
          NL = NL + 1
          FSI(NL)=-FDSI(K,IADI)
         ENDDO
        ENDIF
        IADI = IADI + MAX(1,NF_SI(I))
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CP_SLNR                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE CP_SLNR(IAD_CP  ,JDI_CP ,NSL  ,NZ)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,NZ ,IAD_CP(*),JDI_CP(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----------------------------
       CALL CP_INT((NSL+1),IAD_SLNR,IAD_CP)
       CALL CP_INT(NZ,JDI_SLNR,JDI_CP)
C
       IF(ALLOCATED(IML)) DEALLOCATE(IML)
       ALLOCATE(IML(NSL))
       CALL CP_INT(NSL,ISL,IML)
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IND_SLD                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        PUT_KII                       source/implicit/imp_glob_k.F  
Chd|        ZERO1                         source/system/zero.F          
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE IND_SLD(NSL  ,NDOF  ,KSS )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL ,NDOF(*)
      my_real
     .     KSS(6,*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,P,ID,NJ,NB,NL,NZ,J1,K1,NK,NN,IAD,
     .        IER1,IER2,IER3,IER4,IER5,IER6,ND
      my_real
     .     KII(6,6)
C-------------------------------------
C------IDDL en fonction de noeuds kin d'abords-----
       NN = 0
       DO I = 1,NSL
        IF (IKC_SL(I).GT.0) THEN
C----------int2,rb-,others-----------
         NB = IAD_SLNR(I+1)-IAD_SLNR(I)
         NN = NN + MAX(1,NB)
        ENDIF
       ENDDO
       IF(ALLOCATED(IDDL_SL)) DEALLOCATE(IDDL_SL)
       ALLOCATE(IDDL_SL(NN),STAT=IER1)
C------dim--[K_SL]-partie depedant only----LT---
       NN = 0
       NL = 0
       NZ = 0
       DO I = 1,NSL
         IF (IKC_SL(I).GT.0) THEN
C----------int2,rb------------
          DO J=IAD_SLNR(I),IAD_SLNR(I+1)-1
           NN = NN +1
           IDDL_SL(NN) = NL
           NJ = JDI_SLNR(J)
           DO K =1,NDOF(NJ)
             NL = NL + 1
             DO K1 = 1, K-1
              NZ = NZ + 1
             ENDDO
           ENDDO
C------   mimj  -------
           DO J1=IAD_SLNR(I),J-1
            NK = JDI_SLNR(J1)
            DO K =1,NDOF(NK)
              NZ = NZ + NDOF(NK)
            ENDDO
           ENDDO
          ENDDO
C----------other kin------------
          IF (IAD_SLNR(I).EQ.IAD_SLNR(I+1)) THEN
           NN = NN +1
           IDDL_SL(NN) = NL
           N = ISL(I)
           DO K =1,MIN(3,NDOF(N))
             NL = NL + 1
             DO K1 = 1, K-1
              NZ = NZ + 1
             ENDDO
           ENDDO
          ENDIF
         ENDIF
       ENDDO
C-----allocation------
       NDDL_SL = NL
       NZ_SL = NZ
       IF(ALLOCATED(IAD_SS)) DEALLOCATE(IAD_SS)
       ALLOCATE(IAD_SS(NL+1),STAT=IER2)
       IF(ALLOCATED(JDI_SL)) DEALLOCATE(JDI_SL)
       ALLOCATE(JDI_SL(NZ),STAT=IER3)
C
       NL = 0
       NZ = 0
       NN = 0
       IAD_SS(NL+1) = NZ + 1
        DO I = 1, NSL
         IF (IKC_SL(I).GT.0) THEN
          IAD = NN
          DO J=IAD_SLNR(I),IAD_SLNR(I+1)-1
           NN = NN +1
           NJ = JDI_SLNR(J)
           DO K =1,NDOF(NJ)
            NL = NL + 1
C------   mij  d'abord-------
            DO J1=IAD_SLNR(I),J-1
             NK = JDI_SLNR(J1)
             ID = IAD + J1-IAD_SLNR(I)+1
             DO K1 =1,NDOF(NK)
              NZ = NZ + 1
              JDI_SL(NZ) = IDDL_SL(ID)+ K1
             ENDDO
            ENDDO
C------   mii  ------
            DO K1 = 1, K-1
             NZ = NZ + 1
             JDI_SL(NZ) = IDDL_SL(NN) + K1
            ENDDO
            IAD_SS(NL+1) = NZ + 1
           ENDDO
          ENDDO
C----------other kin------------
          IF (IAD_SLNR(I).EQ.IAD_SLNR(I+1)) THEN
           NN = NN +1
           N = ISL(I)
           DO K =1,MIN(3,NDOF(N))
             NL = NL + 1
             DO K1 = 1, K-1
              NZ = NZ + 1
              JDI_SL(NZ) = IDDL_SL(NN) + K1
             ENDDO
             IAD_SS(NL+1) = NZ + 1
           ENDDO
          ENDIF
         ENDIF
        ENDDO
C
       IF(ALLOCATED(DIAG_SL)) DEALLOCATE(DIAG_SL)
       IF(ALLOCATED(LT_SL)) DEALLOCATE(LT_SL)
       ALLOCATE(DIAG_SL(NL),STAT=IER4)
       ALLOCATE(LT_SL(NZ),STAT=IER5)
       CALL ZERO1(DIAG_SL,NL)
       CALL ZERO1(LT_SL,NZ)
C----------initiation for the case /BCS       
       NN = 0
        DO I = 1, NSL
         IF (IKC_SL(I).GT.0) THEN
C----------other kin------------
          IF (IAD_SLNR(I).EQ.IAD_SLNR(I+1)) THEN
           NN = NN +1
           N = ISL(I)
           ND = MIN(3,NDOF(N))
           DO K = 1, ND
            KII(K,K) = KSS(K,I)
           ENDDO
           KII(1,2)=KSS(4,I)
           KII(1,3)=KSS(5,I)
           KII(2,3)=KSS(6,I)
           KII(2,1)=KII(1,2)
           KII(3,1)=KII(1,3)
           KII(3,2)=KII(2,3)
           CALL PUT_KII(NN,IDDL_SL,IAD_SS,DIAG_SL,LT_SL,KII,ND)
          ENDIF
         ENDIF
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_FRKS                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        GET_KII                       source/implicit/imp_glob_k.F  
Chd|        GET_KIJ                       source/implicit/imp_glob_k.F  
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE IMP_FRKS(NSL  ,IDDL ,IKC ,NDOF  ,IDDLM,
     .                    KSS  ,IAD_SLD )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL ,IDDL(*),IKC(*),NDOF(*),IDDLM(*),IAD_SLD(*)
C     REAL ou REAL*8
      my_real
     .  KSS(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDDL_CP(NDDL_SL),IAD_CP(NDDL_SL+1),JDI_CP(NZ_SL),
     .        NR,NK,NN,IDM,JD,IADI,IDJ,IDJM,IAD,NDOFI
      INTEGER I,J,K,N,P,ID,NJ,NB,NKC,NL,NZ,K1,NKC1,J1,NM,
     .        IER1,IER2,IER3,IER4,IER5,IER6
      my_real
     .  KII(6,6),KIJ(6,6),LT_CP(NZ_SL),DIAG_CP(NDDL_SL)
C-------------------------------------
C----------s'il y a kin modif copy----
      IF (NDDL_SL.GT.0) THEN
       NN = 0
       DO I = 1,NSL
        IF (IKC_SL(I).GT.0) THEN
C----------int2,rb------------
         NB = IAD_SLNR(I+1)-IAD_SLNR(I)
         NN = NN + MAX(1,NB)
        ENDIF
       ENDDO
       CALL CP_INT((NDDL_SL+1),IAD_SS,IAD_CP)
       CALL CP_INT(NN,IDDL_SL,IDDL_CP)
       CALL CP_INT(NZ_SL,JDI_SL,JDI_CP)
       CALL CP_REAL(NDDL_SL,DIAG_SL,DIAG_CP)
       CALL CP_REAL(NZ_SL,LT_SL,LT_CP)
      ENDIF
C------dim--[K_SL]-
       NL = 0
       NZ = 0
       DO I = 1,NSL
         IF (IKC_SL(I).GT.0) THEN
          DO J=IAD_SLNR(I),IAD_SLNR(I+1)-1
           NJ = JDI_SLNR(J)
           ID = IDDL(NJ)
           DO K =1,NDOF(NJ)
            IF (IKC(ID+K).EQ.0) THEN
             NL = NL + 1
C----------mij----------
             DO J1=IAD_SLNR(I),J-1
              NK = JDI_SLNR(J1)
              IDJ = IDDL(NK)
              DO K1 =1,NDOF(NK)
               IF (IKC(IDJ+K1).EQ.0) NZ = NZ + 1
              ENDDO
             ENDDO
             DO K1 = 1, K-1
              IF (IKC(ID+K1).EQ.0) NZ = NZ + 1
             ENDDO
            ENDIF
           ENDDO
          ENDDO
          IF (IAD_SLNR(I).EQ.IAD_SLNR(I+1)) THEN
           N = ISL(I)
           ID = IDDL(N)
           DO K =1,MIN(3,NDOF(N))
            IF (IKC(ID+K).EQ.0) THEN
             NL = NL + 1
             DO K1 = 1, K-1
              IF (IKC(ID+K1).EQ.0) NZ = NZ + 1
             ENDDO
            ENDIF
           ENDDO
          ENDIF
C------independent nodes---------
         ELSE
          N = ISL(I)
          DO K =1,MIN(3,NDOF(N))
            NL = NL + 1
            DO K1 = 1, K-1
             NZ = NZ + 1
            ENDDO
          ENDDO
         ENDIF
       ENDDO
C-----allocation------
       IF(ALLOCATED(IDDL_SL)) DEALLOCATE(IDDL_SL)
       ALLOCATE(IDDL_SL(NL),STAT=IER4)
       IF (NL.GT.NDDL_SL) THEN
        IF(ALLOCATED(IAD_SS)) DEALLOCATE(IAD_SS)
        ALLOCATE(IAD_SS(NL+1),STAT=IER1)
C---------iddl devient idtok---
        IF(ALLOCATED(DIAG_SL)) DEALLOCATE(DIAG_SL)
        ALLOCATE(DIAG_SL(NL),STAT=IER5)
       ENDIF
       IF (NZ.GT.NZ_SL) THEN
        IF(ALLOCATED(JDI_SL)) DEALLOCATE(JDI_SL)
        ALLOCATE(JDI_SL(NZ),STAT=IER2)
        IF(ALLOCATED(LT_SL)) DEALLOCATE(LT_SL)
        ALLOCATE(LT_SL(NZ),STAT=IER3)
       ENDIF
       NDDL_SL = NL
       NZ_SL = NZ
C
       NL = 0
       NZ = 0
       NN = 0
       IAD_SS(NL+1) = NZ + 1
       IAD_SLD(1) = NL + 1
       DO P = 1,NSPMD
        DO I = IAD_SL(P), IAD_SL(P+1)-1
         IF (IKC_SL(I).GT.0) THEN
          IAD = NN
          DO J=IAD_SLNR(I),IAD_SLNR(I+1)-1
           NN = NN + 1
           NJ = JDI_SLNR(J)
           ID = IDDL(NJ)
           IDM = IDDLM(NJ)
           CALL GET_KII(NN ,IDDL_CP,IAD_CP,DIAG_CP,LT_CP ,KII,NDOF(NJ))
           NKC =0
           DO K =1,NDOF(NJ)
            IF (IKC(ID+K).EQ.0) THEN
             NL = NL + 1
             IDDL_SL(NL) =IDM+K-NKC
             DIAG_SL(NL) = KII(K,K)
C----------mij----------
             DO J1=IAD_SLNR(I),J-1
              NK = JDI_SLNR(J1)
              IDJ = IDDL(NK)
              IDJM = IDDLM(NK)
              NM = IAD + J1 - IAD_SLNR(I) + 1
              CALL GET_KIJ(NM ,NN ,IDDL_CP,IAD_CP,JDI_CP,LT_CP ,KIJ ,
     .                     NDOF(NK),NDOF(NJ) ,IER1 )
              NKC1 = 0
              DO K1 =1,NDOF(NK)
               IF (IKC(IDJ+K1).EQ.0) THEN
                NZ = NZ + 1
                JDI_SL(NZ) = IDJM +  K1- NKC1
                LT_SL(NZ) = KIJ(K1,K)
C                LT_SL(NZ) = KIJ(K,K1)
               ELSE
                NKC1 = NKC1 + 1
               ENDIF
              ENDDO
             ENDDO
C----------mii----------
             NKC1 =0
             DO K1 = 1, K-1
              IF (IKC(ID+K1).EQ.0) THEN
               NZ = NZ + 1
               JDI_SL(NZ) = IDM +  K1- NKC1
               LT_SL(NZ) = KII(K1,K)
              ELSE
               NKC1 =NKC1+1
              ENDIF
             ENDDO
             IAD_SS(NL+1) = NZ+1
            ELSE
             NKC =NKC+1
            ENDIF
           ENDDO
          ENDDO
          IF (IAD_SLNR(I).EQ.IAD_SLNR(I+1)) THEN
           N = ISL(I)
           ID = IDDL(N)
           IDM = IDDLM(N)
           NN = NN + 1
C
           NDOFI = MIN(3,NDOF(N))
           CALL GET_KII(NN ,IDDL_CP,IAD_CP,DIAG_CP,LT_CP ,KII,NDOFI)
           NKC =0
           DO K =1,NDOFI
            IF (IKC(ID+K).EQ.0) THEN
             NL = NL + 1
             IDDL_SL(NL) =IDM+K-NKC
             DIAG_SL(NL) = KII(K,K)
             NKC1 =0
             DO K1 = 1, K-1
              IF (IKC(ID+K1).EQ.0) THEN
               NZ = NZ + 1
               JDI_SL(NZ) = IDM +  K1- NKC1
               LT_SL(NZ) = KII(K1,K)
              ELSE
               NKC1 =NKC1+1
              ENDIF
             ENDDO
             IAD_SS(NL+1) = NZ+1
            ELSE
             NKC =NKC+1
            ENDIF
           ENDDO
          ENDIF
C------independent nodes---------
         ELSE
          N = ISL(I)
          IDM = IDDLM(N)
          DO K =1,MIN(3,NDOF(N))
           NL = NL + 1
           IDDL_SL(NL) =IDM+K
           DIAG_SL(NL) =KSS(K,I)
           DO K1 = 1, K-1
            NZ = NZ + 1
            JDI_SL(NZ) = IDM +  K1
            ID = K1 + K + 1
            LT_SL(NZ) = KSS(ID,I)
           ENDDO
           IAD_SS(NL+1) = NZ+1
          ENDDO
         ENDIF
        ENDDO
        IAD_SLD(P+1) = NL + 1
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CP_IADD                       source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE CP_IADD(NSL ,NSREM,IAD_SLD  ,IAD_MLD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL ,NSREM,IAD_SLD(*),IAD_MLD(*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L
C----------------------------
       L = NSPMD + 1
       IF (NSL .GT.0) CALL CP_INT(L,IAD_SLD,IAD_SL)
       IF (NSREM .GT.0) CALL CP_INT(L,IAD_MLD,IAD_SREM)
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  NDDLI_FRB                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        GETNDDLI_G                    source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        SPMD_NROW                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE NDDLI_FRB(
     1    NDOF      ,IKC       ,IDDL      ,NDOFI     ,NDDLIFB   ,
     2    FR_ELEM   ,IAD_ELEM  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "impl1_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDOFI(*) ,IDDL(*),IKC(*),NDOF(*),NDDLIFB
      INTEGER
     .   FR_ELEM(*),IAD_ELEM(2,*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NK,IP,L,IFRE,II,IAD2,IJ,IND,N_FR,NB,
     .        K,ND,NZZK,NJ,NZZ,IAD,JAD,ID,JD,IDK,NC
      INTEGER, DIMENSION(:),ALLOCATABLE :: ITAG,ICONT
      my_real
     .        S1,STMP
C
      STMP = ZERO
      N_FR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
      IF (N_FR.GT.0) THEN
       ALLOCATE(ITAG(N_FR),ICONT(N_FR))
       ITAG = 0
       DO IP =1,NSPMD
        DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
         N=FR_ELEM(NK)
         IDK=IDDL(N)
         IFRE = 0
         DO J=1,MIN(NDOF(N),NDOFI(N))
          IF (IKC(IDK+J).EQ.0) IFRE=IFRE+1
         ENDDO
         ITAG(NK) = IFRE
        ENDDO
       ENDDO
       CALL SPMD_NROW(ITAG,ICONT,IAD_ELEM,N_FR)
       DO NK=1,N_FR
        ICONT(NK)=MIN(ITAG(NK),ICONT(NK))
        IF (ICONT(NK).GT.0) THEN
         ICONT(NK) = ITAG(NK)
             ITAG(NK) = 2
        ENDIF
       ENDDO
       DO NK=1,N_FR
        NB=-IFRLOC(NK)
        IF (NB.GT.0.AND.ICONT(NK).GT.0) ITAG(NB) = ITAG(NB) + 1
       ENDDO
       DO NK=1,N_FR
         IF (ICONT(NK).GT.0) THEN
          NB=-IFRLOC(NK)
          IF (NB.LT.0) NB = NK
              S1 = ONE/ITAG(NB)
          STMP = STMP + S1*ICONT(NK)
         ENDIF
       ENDDO
       DEALLOCATE(ITAG,ICONT)
      END IF !(N_FR.GT.0) THEN
      CALL SPMD_SUM_S(STMP)
      NDDLIFB = INT(STMP)
C
      RETURN
      END
Chd|====================================================================
Chd|  NDOFI_NSL                     source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        NDDLI_NS                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE NDOFI_NSL(NSL   ,NDDLI     ,NDOFI     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,NDOFI(*) ,NDDLI
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--------MIS  jour NDOFI pour NSL----
      INTEGER I,J,N,IDK,NC,NDOFII
       NDOFII = 3
      IF (NDDLI.EQ.0) THEN
       DO N =1,NUMNOD
         NDOFI(N)= 0
       ENDDO
      ENDIF
       DO I =1,NSL
         N=ISL(I)
         IF(NDOFI(N).EQ.0) NDOFI(N)= -NDOFII
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  GETNDDLI_G                    source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        NDDLI_FRB                     source/mpi/implicit/imp_fri.F 
Chd|        NDDLI_NS                      source/mpi/implicit/imp_fri.F 
Chd|        SPMD_NDDLIG                   source/mpi/implicit/imp_spmd.F
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE GETNDDLI_G(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,NDOFI     ,
     3    NDOF      ,IKC       ,IDDL      ,FR_ELEM   ,IAD_ELEM  ,
     4    NDDLI     ,NSL       ,NDDLIG    ,IRBE3     ,LRBE3     ,
     5    IRBE2     ,LRBE2     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "impl1_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDOFI(*) ,IDDL(*),IKC(*),NDOF(*),NDDLI,NDDLIG,NSL
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IPARI(NPARI,*),
     .        IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*)
      INTEGER
     .   FR_ELEM(*),IAD_ELEM(2,*)
C     REAL ou REAL*8
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  NDDLIFB,NDDLINS
C
       CALL NDDLI_FRB(
     1    NDOF      ,IKC       ,IDDL      ,NDOFI     ,NDDLIFB   ,
     2    FR_ELEM   ,IAD_ELEM  )
       CALL NDDLI_NS(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,NDOFI     ,
     3    NDOF      ,IKC       ,IDDL      ,NSL       ,NDDLI     ,
     4    NDDLINS   ,IRBE3     ,LRBE3     ,IRBE2     ,LRBE2     )
       CALL SPMD_NDDLIG(NDDLINS  ,NDDLIFB   ,NDDLIG    )
C
      RETURN
      END
Chd|====================================================================
Chd|  NDDLI_NS                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        GETNDDLI_G                    source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        NDOFI_NSL                     source/mpi/implicit/imp_fri.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE NDDLI_NS(
     1    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2    NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,NDOFI     ,
     3    NDOF      ,IKC       ,IDDL      ,NSL       ,NDDLI     ,
     4    NDDLINS   ,IRBE3     ,LRBE3     ,IRBE2     ,LRBE2     )
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C      INTEGER NNMAX,NKMAX
      INTEGER NDOFI(*) ,IDDL(*),IKC(*),NDOF(*),NDDLINS,NSL
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IPARI(NPARI,*),NDDLI,
     .        IRBE3(NRBE3L,*),LRBE3(*),IRBE2(NRBE2L,*),LRBE2(*)
C     REAL ou REAL*8
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .        I,J,K,N,L,NL,NJ,NI,J1,M,NSN,N1,N2,NK,ID,NM,
     .        JI,NS,NNOD,NDOFII,IAD
C------------
      CALL NDOFI_NSL(NSL   ,NDDLI     ,NDOFI     )
      DO J=1,NINT2
       N=IINT2(J)
       NSN = IPARI(5,N)
       JI=IPARI(1,N)
       DO I=1,NSN
        NI=INTBUF_TAB(N)%NSV(I)
        IF (NDOFI(NI).LT.0) THEN
           L=INTBUF_TAB(N)%IRTLM(I)
           NL=4*(L-1)
           IF (INTBUF_TAB(N)%IRECTM(NL+3).EQ.INTBUF_TAB(N)%IRECTM(NL+4)) THEN
            NNOD=3
           ELSE
            NNOD=4
           ENDIF
           DO M=1,NNOD
            NM=INTBUF_TAB(N)%IRECTM(NL+M)
            IF (NDOFI(NM).EQ.0) NDOFI(NM) = -6
           ENDDO
        ENDIF
       ENDDO
      ENDDO
C----RBE2-----
      DO N=1,NRBE2
       IAD=IRBE2(1,N)
       M  =IRBE2(3,N)
       NSN  =IRBE2(5,N)
        DO I=1,NSN
         ID = I+IAD
         NI=LRBE2(ID)
         IF (NDOFI(NI).LT.0) THEN
          IF (NDOFI(M).EQ.0) NDOFI(M) = -6
         ENDIF
       ENDDO
      ENDDO
C--------RBE3----
      DO N=1,NRBE3
       IAD=IRBE3(1,N)
       NI=IRBE3(3,N)
       IF (NI.EQ.0) CYCLE
       NNOD=IRBE3(5,N)
        IF (NDOFI(NI).LT.0) THEN
           DO M=1,NNOD
            NM=LRBE3(IAD+M)
            IF (NDOFI(NM).EQ.0) NDOFI(NM) = -6
           ENDDO
        ENDIF
      ENDDO
C-----active rigid body main nodes------
      DO J=1,NRBYAC
       N=IRBYAC(J)
       K=IRBYAC(J+NRBYKIN)
       M  =NPBY(1,N)
       NSN  =NPBY(2,N)
        DO I=1,NSN
         ID = I+K
         NI=LPBY(ID)
         IF (NDOFI(NI).LT.0) THEN
          IF (NDOFI(M).EQ.0) NDOFI(M) = -6
         ENDIF
       ENDDO
      ENDDO
C-----mis  jour NDOFI pour NSL+kin------
       NDDLINS = NDDLI
       DO N =1,NUMNOD
         IF(NDOFI(N).LT.0) THEN
          NDOFII = MIN(-NDOFI(N),NDOF(N))
              ID=IDDL(N)
          DO J=1,NDOFII
           IF (IKC(ID+J).EQ.0) NDDLINS = NDDLINS+1
          ENDDO
         END IF
       ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  SPC_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|-- called by -----------
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        INTAB0                        source/implicit/imp_fsa_inv.F 
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SPC_FR_K(
     1    IADK     ,JDIK     ,NDOF      ,IDDL     ,FR_ELEM  ,
     2    IAD_ELEM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "impl1_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*) ,JDIK(*),IDDL(*),NDOF(*)
      INTEGER
     .   FR_ELEM(*),IAD_ELEM(2,*)
C     REAL ou REAL*8
C-----------------------------------------------
C   External function
C-----------------------------------------------
      INTEGER INTAB0
      EXTERNAL INTAB0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NK,IP,L,IFIX,II,IAD2,IJ,IND,N_FR,NB,
     .        K,ND,NZZK,NJ,NZZ,IAD,JAD,ID,JD,IDK,NC
C
      IF (NDDLFR.LE.0) RETURN
C-------FR2K-------
      IAD2 = 0
      DO IP =1,NSPMD
       DO NK=IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
        N=FR_ELEM(NK)
        ID=IDDLFR(NK)+IAD2
        IDK=IDDL(N)
        DO J=1,NDOF(N)
          IFR2K(ID+J)=IDK+J
        ENDDO
       ENDDO
       IAD2 = IAD2 + ND_FR(IP)
      ENDDO
      LEN_V = IAD2
C
      LEN_K = 0
       IAD = 0
       IAD2 = 0
       JAD = 0
      IF (IKPAT.EQ.0) THEN
       DO IP =1,NSPMD
        DO I=1,ND_FR(IP)
         ID = I+IAD
         II = IFR2K(I+IAD2)
         DO J=IADFR(ID),IADFR(ID+1)-1
          JD = J + JAD
          K=JDIFR(JD)
          IJ = IFR2K(K+IAD2)
          IF (II.LT.IJ) THEN
           NC = IADK(II+1)-IADK(II)
           N=INTAB0(NC,JDIK(IADK(II)),IJ)
           IF (N.GT.0) THEN
            JFR2K(JD)=N+IADK(II)-1
           ELSE
            write(*,*)'index error in SPC_FR_K I<J',IJ,ip,nc
           ENDIF
          ELSE
           NC = IADK(IJ+1)-IADK(IJ)
           N=INTAB0(NC,JDIK(IADK(IJ)),II)
           IF (N.GT.0) THEN
            JFR2K(JD)=N+IADK(IJ)-1
           ELSE
            write(*,*)'index error in SPC_FR_K J<I',II,ip,nc
           ENDIF
          ENDIF
         ENDDO
        ENDDO
        NZZK=IADFR(IAD+ND_FR(IP)+1)-IADFR(IAD+1)
        IAD = IAD + ND_FR(IP) +1
        IAD2 = IAD2 + ND_FR(IP)
        JAD = JAD + NZZK
       ENDDO
      ELSE
       DO IP =1,NSPMD
        DO I=1,ND_FR(IP)
         ID = I+IAD
         II = IFR2K(I+IAD2)
         DO J=IADFR(ID),IADFR(ID+1)-1
          JD = J + JAD
          K=JDIFR(JD)
          IJ = IFR2K(K+IAD2)
          IF (II.GT.IJ) THEN
           NC = IADK(II+1)-IADK(II)
           N=INTAB0(NC,JDIK(IADK(II)),IJ)
           IF (N.GT.0) THEN
            JFR2K(JD)=N+IADK(II)-1
           ELSE
            write(*,*)'index error in SPC_FR_K I>J',IJ,ip,nc
           ENDIF
          ELSE
           NC = IADK(IJ+1)-IADK(IJ)
           N=INTAB0(NC,JDIK(IADK(IJ)),II)
           IF (N.GT.0) THEN
            JFR2K(JD)=N+IADK(IJ)-1
           ELSE
            write(*,*)'index error in SPC_FR_K J>I',II,ip,nc
           ENDIF
          ENDIF
         ENDDO
        ENDDO
        NZZK=IADFR(IAD+ND_FR(IP)+1)-IADFR(IAD+1)
        IAD = IAD + ND_FR(IP) +1
        IAD2 = IAD2 + ND_FR(IP)
        JAD = JAD + NZZK
       ENDDO
      END IF  !(IKPAT.GT.0)
       LEN_K = IAD2+JAD
C
      RETURN
      END

