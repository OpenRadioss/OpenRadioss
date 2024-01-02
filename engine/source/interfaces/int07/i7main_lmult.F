Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  I7MAIN_LMULT                  source/interfaces/int07/i7main_lmult.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I7CDCOR3                      source/interfaces/int07/i7cdcor3.F
Chd|        I7COR3                        source/interfaces/int07/i7cor3.F
Chd|        I7DST3                        source/interfaces/int07/i7dst3.F
Chd|        I7LAGM                        source/interfaces/int07/i7lagm.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I7MAIN_LMULT(
     1      NIN      ,IPARI   ,INTBUF_TAB,X        ,
     2      V        ,A       ,ITASK   ,MS         ,
     3      IADLL    ,LLL     ,JLL     ,SLL        ,XLL      ,
     4      N_MUL_MX ,NKMAX   ,ITAB    ,INDEX2     ,NB_JLT   ,
     5      NB_JLT_NEW,NB_STOK_N ,NEWFRONT,ICONTACT,ITAG     ,
     6      XTAG     ,COMNTAG ,KINET   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C-------------------------------------------------------------------------------
C     NOM    DIMENSION               DESCRIPTION                       E/S
C-------------------------------------------------------------------------------
C
C    NIN    1                        NUMERO INTERFACE                   E
C                  
C   IPARI   NPARI,NINTER             PARAMETRES D'INTERFACE             E
C                  
C     X     3,NUMNOD                 COORDONNEES                        E
C                  
C     V     3,NUMNOD                 VITESSES                           E
C                  
C    EMINX  6*NME<6*NUMELS           MIN MAX DE CHAQUE ELEMENT        TMP_GLOBAL
C                  
C                  
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,ITASK ,N_MUL_MX ,NKMAX ,
     .        NB_JLT,NB_JLT_NEW,NB_STOK_N,NEWFRONT   
      INTEGER IPARI(NPARI,NINTER), KINET(*),
     .        IADLL(*) ,LLL(*) ,JLL(*) ,SLL(*) ,ICONTACT(*),
     .        ITAB(*), INDEX2(*), ITAG(*), COMNTAG(*)
C     REAL
      my_real 
     .   X(3,*), V(3,*), A(3,*), MS(*),
     .   XLL(*), XTAG(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I_STOK_GLOB,NSN,NME,NAD,EAD,
     .        NME_T,ESH_T,IGN,IGE,MULTIMP,NOINT,I,MX_CAND,NTY,IVIS2,
     .        IGAP,INACTI,IBAG,I_STOK, I_STOK_LOC, JLT_NEW,
     .        JLT, NFT,DEBUT,NBID,NB_LOC,JTASK, IGSTI,ICURV,IADM
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), CN_LOC(MVSIZ),CE_LOC(MVSIZ),
     .        CAND_N_N(MVSIZ),CAND_E_N(MVSIZ),KINI(MVSIZ),IBID
C     REAL
      my_real
     .   STARTT, STOPT,GAP,GAPMIN,MAXBOX,MINBOX,BID,
     .   KMIN, KMAX, GAPMAX
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   GAPV(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ)
      my_real
     .    NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .    NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .    NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ),
     .    CMAJ(MVSIZ)
      my_real
     .     DRAD, DRAD2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(IPARI(33,NIN)==0)RETURN
C -------------------
      CALL MY_BARRIER
C -------------------
      DO I = ITASK+1,NUMNOD,NTHREAD
        ITAG(I) = 0
        XTAG(I) = ZERO
      ENDDO
C -------------------
      CALL MY_BARRIER
C -------------------
      JTASK=ITASK+1
C
      NBID=0
      BID=ZERO
      IBID = 0
C      
      NSN   =IPARI(5,NIN)
      NTY   =IPARI(7,NIN)
      IVIS2 =IPARI(14,NIN)
      NOINT =IPARI(15,NIN)
      IGAP  =IPARI(21,NIN)
      INACTI=IPARI(22,NIN)
      IBAG =IPARI(32,NIN) 
      IGSTI=IPARI(34,NIN) 
      ICURV =0
      IADM =IPARI(44,NIN) 
      STARTT=INTBUF_TAB(NIN)%VARIABLES(3)
      STOPT =INTBUF_TAB(NIN)%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
      GAP   =INTBUF_TAB(NIN)%VARIABLES(2)
      GAPMIN=INTBUF_TAB(NIN)%VARIABLES(13)
C
c      IF(NTY==7)THEN
C
      I_STOK = INTBUF_TAB(NIN)%I_STOK(1)
      MAXBOX = INTBUF_TAB(NIN)%VARIABLES(9)
      MINBOX = INTBUF_TAB(NIN)%VARIABLES(12)
      GAPMAX=INTBUF_TAB(NIN)%VARIABLES(16)
      KMIN  =INTBUF_TAB(NIN)%VARIABLES(17)
      KMAX  =INTBUF_TAB(NIN)%VARIABLES(18)
      DRAD    = ZERO
      DRAD2   = ZERO
      IF(NTY==7)THEN
        DRAD  = INTBUF_TAB(NIN)%VARIABLES(32)
        DRAD2 = DRAD*DRAD
      ENDIF
C     cette partie est effectuee en // apres le calcul des forces des elem.
C     decoupage statique
      NB_LOC = I_STOK / NTHREAD
      IF (JTASK==NTHREAD) THEN
          I_STOK_LOC = I_STOK-NB_LOC*(NTHREAD-1)
      ELSE
          I_STOK_LOC = NB_LOC
      ENDIF
      DEBUT = (JTASK-1)*NB_LOC
      I_STOK = 0
C recalcul du istok
      IF (INACTI==5.OR.INACTI==6)THEN
          DO I = DEBUT+1, DEBUT+I_STOK_LOC
            IF(INTBUF_TAB(NIN)%CAND_N(I)<0) THEN
Ctmp+++
              IF(I_STOK + 1>4*NUMNOD) THEN
                CALL ANCMSG(MSGID=97,ANMODE=ANINFO)
                CALL ARRET(2)
              ENDIF
Ctmp---
              I_STOK = I_STOK + 1
              INDEX2(I_STOK) = I
C inbuf == cand_n
              INTBUF_TAB(NIN)%CAND_N(I) = -INTBUF_TAB(NIN)%CAND_N(I)
            ELSE
C   remise a 0 de cand_p
              INTBUF_TAB(NIN)%CAND_P(I) = ZERO
            ENDIF
          ENDDO
      ELSE
          DO I = DEBUT+1, DEBUT+I_STOK_LOC
            IF(INTBUF_TAB(NIN)%CAND_N(I)<0) THEN
Ctmp+++
              IF(I_STOK + 1>4*NUMNOD) THEN
                CALL ANCMSG(MSGID=97,ANMODE=ANINFO)
                CALL ARRET(2)
              ENDIF
Ctmp---
              I_STOK = I_STOK + 1
              INDEX2(I_STOK) = I
C inbuf == cand_n
              INTBUF_TAB(NIN)%CAND_N(I) = -INTBUF_TAB(NIN)%CAND_N(I)
            ENDIF
          ENDDO
      ENDIF
C
      IF (DEBUG(3)>=1) THEN
          NB_JLT = NB_JLT + I_STOK_LOC
          NB_STOK_N = NB_STOK_N + I_STOK
      ENDIF
C
      DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
C preparation candidats retenus
          CALL I7CDCOR3(
     1         JLT,INDEX2(NFT+1),INTBUF_TAB(NIN)%CAND_E,INTBUF_TAB(NIN)%CAND_N,
     2         CAND_E_N,CAND_N_N)
C cand_n et cand_e remplace par cand_n_n et cand_e_n
          CALL I7COR3(
     1         JLT       ,X   ,INTBUF_TAB(NIN)%IRECTM,INTBUF_TAB(NIN)%NSV,
     .         CAND_E_N,
     2         CAND_N_N  ,INTBUF_TAB(NIN)%STFM,INTBUF_TAB(NIN)%STFNS,X1  ,X2 ,
     3         X3        ,X4        ,Y1        ,Y2        ,Y3      ,
     4         Y4        ,Z1        ,Z2        ,Z3        ,Z4      ,
     5         XI        ,YI        ,ZI        ,STIF      ,IX1     ,
     6         IX2       ,IX3       ,IX4       ,NSVG      ,IGAP    ,
     7         GAP       ,INTBUF_TAB(NIN)%GAP_S,INTBUF_TAB(NIN)%GAP_M,GAPV,
     9         MS        ,VXI       ,VYI     ,
     A         VZI       ,MSI       ,NSN       ,V         ,KINET   ,
     B         KINI      ,NTY       ,NIN       ,IGSTI     ,KMIN    ,
     C         KMAX      ,GAPMAX    ,GAPMIN    ,IADM      ,BID     ,
     D         BID       ,BID       ,BID       ,IBID      ,BID     ,
     E         BID       ,BID       ,BID       ,IBID      ,BID     ,
     F         IBID      ,IBID      ,IBID      ,INTBUF_TAB(NIN)%GAP_SL,
     .         INTBUF_TAB(NIN)%GAP_ML,
     G         IBID      ,IBID      ,IBID      ,IBID      ,IBID    ,
     H         IBID      ,IBID      ,BID       ,IBID      ,BID     )
          JLT_NEW = 0
          CALL I7DST3(
     1        JLT   ,CAND_N_N,CAND_E_N,CN_LOC,  CE_LOC,
     2        X1    ,X2    ,X3     ,X4     ,Y1        ,
     3        Y2    ,Y3    ,Y4     ,Z1     ,Z2        ,
     4        Z3    ,Z4    ,XI     ,YI     ,ZI        ,
     5        NX1   ,NX2   ,NX3    ,NX4    ,NY1       ,
     6        NY2   ,NY3   ,NY4    ,NZ1    ,NZ2       ,
     7        NZ3   ,NZ4   ,LB1    ,LB2    ,LB3       ,
     8        LB4   ,LC1   ,LC2    ,LC3    ,LC4       ,
     9        P1    ,P2    ,P3     ,P4     ,IX1       ,
     A        IX2   ,IX3   ,IX4    ,NSVG   ,STIF      ,
     B        JLT_NEW,GAPV ,INACTI ,INTBUF_TAB(NIN)%CAND_P,
     C        INDEX2(NFT+1),VXI    ,VYI       ,
     D        VZI   ,MSI   ,KINI   ,ICURV  ,INTBUF_TAB(NIN)%IRECTM,
     E        NNX1  ,NNX2  ,NNX3   ,NNX4   ,NNY1      ,
     F        NNY2  ,NNY3  ,NNY4   ,NNZ1   ,NNZ2      ,
     G        NNZ3  ,NNZ4  ,BID    ,IADM   ,BID       ,
     H        BID   ,IBID  ,BID    ,BID    ,BID      ,
     I        IBID  ,IBID  ,CMAJ   ,DRAD2  ,
     J        IBID   ,IBID     ,
     K        IBID  ,IBID  ,IBID   ,INTBUF_TAB(NIN)%CAND_F,
     L        IBID  ,IBID  ,BID    ,BID    )


          JLT = JLT_NEW
          IF(JLT_NEW/=0) THEN
            IPARI(29,NIN) = 1
            IF (DEBUG(3)>=1)
     .        NB_JLT_NEW = NB_JLT_NEW + JLT_NEW
            CALL I7LAGM(LLL,JLL,SLL      ,XLL     ,IADLL      ,
     2        N_MUL_MX,ITASK   ,NIN     ,NKMAX      , 
     3        JLT     ,A       ,V         ,ITAG    ,XTAG       , 
     4        GAP     ,NOINT   ,INTBUF_TAB(NIN)%STFNS,ITAB ,CN_LOC , 
     5        NX1     ,NX2     ,NX3       ,NX4     ,NY1        , 
     6        NY2     ,NY3     ,NY4       ,NZ1     ,NZ2        , 
     7        NZ3     ,NZ4     ,LB1       ,LB2     ,LB3        , 
     8        LB4     ,LC1     ,LC2       ,LC3     ,LC4        , 
     9        P1      ,P2      ,P3        ,P4      ,
     A        IX1     ,IX2     ,IX3       ,IX4     ,NSVG       , 
     B        GAPV    ,NEWFRONT,IBAG      ,ICONTACT,STIF       , 
     C        COMNTAG ,IADM    )
         ENDIF
      ENDDO
C
      RETURN
      END
