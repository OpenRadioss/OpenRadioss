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
Chd|  IMP_K_EIG                     stub/imp_k_eig.F              
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PR_INFOK                      source/implicit/imp_solv.F    
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        UPD_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE IMP_K_EIG(NDDL0  , NNZK0   , DIAG_K , LT_K   , LSIZE ,
     .                     PM     , GEO     , IPM    , IGEO   , ELBUF ,
     .                     IXS    , IXQ     , IXC    , IXT    , IXP   ,
     .                     IXR    , IXTG    , IXTG1  , IPARG ,
     .                     TF     , NPC     , FR_WAVE, W16    , BUFMAT,
     .                     THKE   , BUFGEO  , RBY   ,
     .                     SKEW   , X       , WA     , IDDL   , NDOF  ,
     .                     IADK   , JDIK    , ICODT  , ICODR  , ISKEW ,
     .                     IBFV   , VEL     , LPBY   , NPBY   , ITAB  ,
     .                     WEIGHT , MS      , IN     , NRBYAC , IRBYAC,
     .                     NSC    , IKINW   , NMC    , IPARI  , INTBUF_TAB,
     .                     D_IMP   , LB     , NINT2  , IINT2 ,
     .                     IKC    , ITASK   , EIGIPM , EIGIBUF, NDDL  ,
     .                     INLOC  , IAD_ELEM, FR_ELEM,IRBE3   ,LRBE3  ,
     .                     FRBE3  ,IRBE2    , LRBE2  , ELBUF_TAB, STACK,
     .                     DRAPE_SH4N, DRAPE_SH3N   , DRAPEG  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD     
      USE INTBUFDEF_MOD   
      USE STACK_MOD  
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL0, NNZK0, LSIZE(*), IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        IXS(*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*),
     .        IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*),
     .        IXTG1(4,*), IPARG(NPARG,*),
     .        NPC(*), IDDL(*), NDOF(*), IADK(*),JDIK(*),
     .        ICODT(*), ICODR(*), ISKEW(*), IBFV(NIFV,*),
     .        LPBY(*), NPBY(NNPBY,*), ITAB(*),
     .        WEIGHT(*), NRBYAC, IRBYAC(*), NSC(*), IKINW(*), NMC,
     .        IPARI(NPARI,*), NINT2, IINT2(*), IKC(*),
     .        ITASK, EIGIPM(*), EIGIBUF(*), NDDL, INLOC(*),
     .        IAD_ELEM(2,*), FR_ELEM(*),IRBE3(*),LRBE3(*),IRBE2(*),
     .        LRBE2(*)
      my_real
     .        DIAG_K(*), LT_K(*), PM(NPROPM,*), GEO(NPROPG,*),
     .        ELBUF(*), TF(*), W16(*), BUFMAT(*),
     .        THKE(*), BUFGEO(*),RBY(*),
     .        SKEW(LSKEW,*), X(3,*), WA(*), VEL(LFXVELR,*), MS(*),
     .        IN(*), D_IMP(3,*), LB(*), FR_WAVE(*),FRBE3(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_) :: DRAPE_SH4N(NUMELC_DRAPE), DRAPE_SH3N(NUMELTG_DRAPE)
      TYPE (DRAPEG_) :: DRAPEG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NNZK, I, L1, L2, L3, LI1, LI2, LI3, LI4, LI5,
     .        LI6, LI7, LI8, ETAG(NUMNOD), NN, IBID1, IBID2, NT_RW,
     .        NMC2, LI9, LI10, LI11, LI12, NNMAX, NKMAX,IBID3,LI13,
     .        LI14,LI15
      my_real
     .        RBID
C=======================================================================
      DO I=1,NUMNOD
         ETAG(I)=0
      ENDDO
      NN=EIGIPM(10)
      DO I=1,NN
         ETAG(EIGIBUF(I))=1
      ENDDO
      DO I=1,NDDL0
         LB(I)=ZERO
      ENDDO
C Pas de vitesse imposee (ni de rigid wall) en analyse modale
      NFXVEL=0
      NT_RW=0
C
      NDDL = NDDL0
      NNZK = NNZK0
      DO I=1,NDDL
       DIAG_K(I)=ZERO
      ENDDO
      DO I=1,NNZK
       LT_K(I)=ZERO
      ENDDO
      NMC2=LSIZE(11)
      L1 = 1+NIXS*NUMELS
      L2 = L1+6*NUMELS10
      L3 = L2+12*NUMELS20
      LI1 =1
      LI2 = LI1+LSIZE(4)
      LI3 = LI2+LSIZE(5)
      LI4 = LI3+LSIZE(1)
      LI5 = LI4+LSIZE(3)
      LI6 = LI5+LSIZE(7)
      LI7 = LI6+LSIZE(2)
      LI8 = LI7+LSIZE(6)
      LI9 = LI8+NINT2
      LI10 = LI9+LSIZE(8)
      LI11 = LI10+(LSIZE(8)-NRBYAC)*LSIZE(9)
      LI12 = LI11+NRBYAC*LSIZE(10)
      LI13 = LI12+4*LSIZE(11)
      LI14 = LI13+LSIZE(14)
      LI15 = LI14+LSIZE(15)
C
      NGDONE = 1
      IF (IMON>0) CALL STARTIME(31,ITASK+1)
      CALL IMP_GLOB_K(
     1   PM        ,GEO       ,IPM       ,IGEO      ,ELBUF     ,
     2   IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     3   IXR       ,IXTG      ,IXTG1     ,IXS(L1)   ,
     4   IXS(L2)   ,IXS(L3)   ,IPARG     ,TF        ,NPC       ,
     5   FR_WAVE   ,W16       ,BUFMAT    ,THKE      ,BUFGEO    ,
     6   RBY       ,SKEW      ,X         ,
     7   WA        ,IDDL      ,NDOF      ,DIAG_K    ,LT_K      ,
     8   IADK      ,JDIK      ,IKG       ,ETAG      ,ELBUF_TAB ,
     9   STACK     ,DRAPE_SH4N, DRAPE_SH3N   ,DRAPEG     )
C      IF (IMON>0) CALL STOPTIME(25,ITASK+1)
C    /---------------/
      CALL MY_BARRIER
C    /---------------/
      CALL UPD_GLOB_K(
     1   ICODT     ,ICODR     ,ISKEW     ,IBFV      ,NPC       ,
     2   TF        ,VEL       ,RBID      ,
     3   RBY       ,X         ,SKEW      ,LPBY      ,NPBY      ,
     4   ITAB      ,WEIGHT    ,MS        ,IN        ,NRBYAC    ,
     5   IRBYAC    ,NSC       ,IKINW(LI1),NMC       ,IKINW(LI2),
     6   IKINW(LI3),IKINW(LI4),NINT2     ,IINT2     ,IKINW(LI8),
     7   IKINW(LI5),IKINW(LI6),IKINW(LI7),IPARI     ,INTBUF_TAB,
     8   NDDL      ,NNZK      ,IADK      ,JDIK      ,
     9   DIAG_K    ,LT_K      ,NDOF      ,IDDL      ,IKC       ,
     A   D_IMP     ,LB        ,IBID1     ,IBID2     ,RBID      ,
     B   NMC2      ,IKINW(LI12),NT_RW    ,RBID      ,IBID3     ,
     C   IRBE3     ,LRBE3     ,FRBE3     ,IKINW(LI13),IRBE2    ,
     D   LRBE2     ,IKINW(LI14),IKINW(LI15))
      IF (IMACH==3.AND.NSPMD>1) THEN
          CALL UPD_FR_K(
     1    IADK     ,JDIK     ,NDOF      ,IKC      ,IDDL     ,
     2    INLOC    ,FR_ELEM  ,IAD_ELEM  ,NDDL     )
      ENDIF
      IF (IMON>0) CALL STOPTIME(31,ITASK+1)
C
      NNMAX=LSIZE(9)
      NKMAX=LSIZE(10)
      CALL PR_INFOK(NDDL0, NNZK0, NDDL, NNZK, MAX(NNMAX,NKMAX))
C------------
      RETURN
      END

