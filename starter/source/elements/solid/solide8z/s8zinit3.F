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
Chd|  S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ATHERI                        source/ale/atheri.F           
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|        FAILINI                       source/elements/solid/solide/failini.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        S8EDERI3                      source/elements/solid/solide8z/s8zderi3.F
Chd|        S8EJACIP3                     source/elements/solid/solide8z/s8zderi3.F
Chd|        S8EREFCOOR3                   source/elements/solid/solide8z/s8erefcoor3.F
Chd|        S8E_PIJ                       source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZDERI3                      source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZDERIC3                     source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZJAC_I3                     source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZJAC_IC                     source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZPIJ_IC                     source/elements/solid/solide8z/s8zderi3.F
Chd|        SBOLTINI                      source/loads/bolt/sboltini.F  
Chd|        SBULK3                        source/elements/solid/solide/sbulk3.F
Chd|        SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|        SCZERO3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SIGIN20B                      source/elements/solid/solide20/s20mass3.F
Chd|        SMASS3                        source/elements/solid/solide/smass3.F
Chd|        SMORTH3                       source/elements/solid/solide/smorth3.F
Chd|        SOLTOSPHV8                    source/elements/sph/soltosph.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|        SVALUE0                       source/elements/thickshell/solidec/scinit3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        BPRELOAD_MOD                  share/modules1/bpreload_mod.F 
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8ZINIT3(ELBUF_STR,MAS     ,IXS     ,PM      ,X     ,
     .                    DETONATORS,GEO     ,VEUL    ,ALE_CONNECTIVITY   ,IPARG ,
     .                    DTELEM   ,SIGI    ,NEL     ,SKEW    ,IGEO  ,
     .                    STIFN    ,PARTSAV ,V       ,IPARTS  ,MSS,
     .                    IPART    ,
     .                    SIGSP    ,NSIGI   ,MSNF    ,MSSF    ,IPM   ,
     .                    IUSER    ,NSIGS   ,VOLNOD  ,BVOLNOD ,VNS   ,
     .                      BNS      ,WMA     ,PTSOL   ,BUFMAT  ,MCP   ,
     .                    MCPS     ,TEMP    ,NPF     ,TF      ,XREFS ,
     .                    MSSA     ,STRSGLOB,STRAGLOB,FAIL_INI,SPBUF ,
     .                    KXSP     ,IPARTSP ,NOD2SP  ,SOL2SPH ,IRST  ,
     .                    ILOADP   ,FACLOAD ,PERTURB ,RNOISE  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE BPRELOAD_MOD
      USE DETONATORS_MOD      
      USE ALE_CONNECTIVITY_MOD
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
#include      "scr03_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "scry_c.inc"
#include      "vect01_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(*),IPARTS(*),
     .    NEL, IPART(LIPART1,*),IPM(NPROPMI,*), PTSOL(*), 
     .    NSIGI, IUSER, NSIGS, NPF(*),
     .    KXSP(NISP,*), IPARTSP(*), NOD2SP(*), SOL2SPH(2,*), IRST(3,*)
      INTEGER IGEO(NPROPGI,*),STRSGLOB(*),STRAGLOB(*),FAIL_INI(*),PERTURB(NPERTURB)
      my_real
     .   MAS(*),PM(NPROPM,*), X(*), GEO(NPROPG,*),
     .   VEUL(LVEUL,*), DTELEM(*),SIGI(NSIGS,*),SKEW(LSKEW,*),STIFN(*),
     .   PARTSAV(20,*), V(*), MSS(8,*),
     .  SIGSP(NSIGI,*),MSNF(*), MSSF(8,*), WMA(*),
     .  VOLNOD(*), BVOLNOD(*), VNS(8,*), BNS(8,*),BUFMAT(*),
     .  MCP(*), MCPS(8,*),TEMP(*), TF(*),XREFS(8,3,*), MSSA(*),
     .  SPBUF(NSPBUF,*),RNOISE(NPERTURB,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(DETONATOR_STRUCT_)::DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        IX5(MVSIZ), IX6(MVSIZ), IX7(MVSIZ), IX8(MVSIZ)
      INTEGER NF1, I, IL, IGTYP,IPID1,NCC,IDEF,NREFSTA,
     .     IP,IR, IS, IT,JHBE,IREP,MPT,NLAY,NPTR,NPTS,NPTT,NUVAR,
     .     L_PLA,L_SIGB,NSPHDIR, NCELF, NCELL,IBOLTP,L_JAC,NNPT
      CHARACTER*nchartitle,
     .   TITR1
      my_real
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),X5(MVSIZ),X6(MVSIZ),
     .   X7(MVSIZ),X8(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),
     .   Y5(MVSIZ),Y6(MVSIZ),Y7(MVSIZ),Y8(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),
     .   Z3(MVSIZ),Z4(MVSIZ),Z5(MVSIZ),Z6(MVSIZ),Z7(MVSIZ),Z8(MVSIZ),
     .   RX(MVSIZ) ,RY(MVSIZ) ,RZ(MVSIZ) ,SX(MVSIZ) ,
     .   SY(MVSIZ) ,SZ(MVSIZ) ,TX(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ) ,
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),
     .   E2Y(MVSIZ),E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   F1X(MVSIZ) ,F1Y(MVSIZ) ,F1Z(MVSIZ) ,
     .   F2X(MVSIZ) ,F2Y(MVSIZ) ,F2Z(MVSIZ) ,
     .   AJC1(MVSIZ) , AJC2(MVSIZ) , AJC3(MVSIZ) ,
     .   AJC4(MVSIZ) , AJC5(MVSIZ) , AJC6(MVSIZ) ,
     .   AJC7(MVSIZ) , AJC8(MVSIZ) , AJC9(MVSIZ) ,
     .   HX(4,MVSIZ) , HY(4,MVSIZ), HZ(4,MVSIZ),GAMA(6,MVSIZ),
     .   SMAX(MVSIZ) , VOLU(MVSIZ), DTX(MVSIZ), DELTAX(MVSIZ),
     .   PXC1(MVSIZ),PXC2(MVSIZ),PXC3(MVSIZ),PXC4(MVSIZ),
     .   PYC1(MVSIZ),PYC2(MVSIZ),PYC3(MVSIZ),PYC4(MVSIZ),
     .   PZC1(MVSIZ),PZC2(MVSIZ),PZC3(MVSIZ),PZC4(MVSIZ),
     .   RHOCP(MVSIZ),TEMP0(MVSIZ),AIRE(MVSIZ),NU(MVSIZ)
      my_real
     .   BID(MVSIZ), FV, STI, WI
      INTEGER NLYMAX, IPANG, IPTHK, IPPOS, IPMAT,IG,IM,MTN0
      INTEGER NPTR0,NPTS0,NPTT0 ,ICSTR,MAT0(MVSIZ),LLPIJ
      PARAMETER (NLYMAX = 200,IPMAT = 100,IPANG = 200)
      my_real
     .   AJP1(MVSIZ,8) , AJP2(MVSIZ,8) , AJP3(MVSIZ,8) ,
     .   AJP4(MVSIZ,8) , AJP5(MVSIZ,8) , AJP6(MVSIZ,8) ,
     .   AJP7(MVSIZ,8) , AJP8(MVSIZ,8) , AJP9(MVSIZ,8) ,
     .   ANGLE(MVSIZ),DTX0(MVSIZ),WT,ZR,ZS,ZT,ZZ
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)     
C-----------------------------------------------
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
C-----------------------------------------------
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9)
      DATA W_GAUSS / 
c---
     1 2.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     2 1.D0               ,1.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     3 0.555555555555556D0,0.888888888888889D0,0.555555555555556D0,
     3 0.D0               ,0.D0               ,0.D0               ,
     3 0.D0               ,0.D0               ,0.D0               ,
     4 0.347854845137454D0,0.652145154862546D0,0.652145154862546D0,
     4 0.347854845137454D0,0.D0               ,0.D0               ,
     4 0.D0               ,0.D0               ,0.D0               ,
     5 0.236926885056189D0,0.478628670499366D0,0.568888888888889D0,
     5 0.478628670499366D0,0.236926885056189D0,0.D0               ,
     5 0.D0               ,0.D0               ,0.D0               ,
     6 0.171324492379170D0,0.360761573048139D0,0.467913934572691D0,
     6 0.467913934572691D0,0.360761573048139D0,0.171324492379170D0,
     6 0.D0               ,0.D0               ,0.D0               ,
     7 0.129484966168870D0,0.279705391489277D0,0.381830050505119D0,
     7 0.417959183673469D0,0.381830050505119D0,0.279705391489277D0,
     7 0.129484966168870D0,0.D0               ,0.D0               ,
     8 0.101228536290376D0,0.222381034453374D0,0.313706645877887D0,
     8 0.362683783378362D0,0.362683783378362D0,0.313706645877887D0,
     8 0.222381034453374D0,0.101228536290376D0,0.D0               ,
     9 0.081274388361574D0,0.180648160694857D0,0.260610696402935D0,
     9 0.312347077040003D0,0.330239355001260D0,0.312347077040003D0,
     9 0.260610696402935D0,0.180648160694857D0,0.081274388361574D0/
c------------------------------------------------------------
      DATA A_GAUSS / 
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     2 -.577350269189625D0,0.577350269189625D0,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               , 
     3 -.774596669241483D0,0.D0               ,0.774596669241483D0,
     3 0.D0               ,0.D0               ,0.D0               ,
     3 0.D0               ,0.D0               ,0.D0               ,
     4 -.861136311594053D0,-.339981043584856D0,0.339981043584856D0,
     4 0.861136311594053D0,0.D0               ,0.D0               ,
     4 0.D0               ,0.D0               ,0.D0               ,
     5 -.906179845938664D0,-.538469310105683D0,0.D0               ,
     5 0.538469310105683D0,0.906179845938664D0,0.D0               ,
     5 0.D0               ,0.D0               ,0.D0               ,
     6 -.932469514203152D0,-.661209386466265D0,-.238619186083197D0,
     6 0.238619186083197D0,0.661209386466265D0,0.932469514203152D0,
     6 0.D0               ,0.D0               ,0.D0               ,
     7 -.949107912342759D0,-.741531185599394D0,-.405845151377397D0,
     7 0.D0               ,0.405845151377397D0,0.741531185599394D0,
     7 0.949107912342759D0,0.D0               ,0.D0               ,
     8 -.960289856497536D0,-.796666477413627D0,-.525532409916329D0,
     8 -.183434642495650D0,0.183434642495650D0,0.525532409916329D0,
     8 0.796666477413627D0,0.960289856497536D0,0.D0               ,
     9 -.968160239507626D0,-.836031107326636D0,-.613371432700590D0,
     9 -.324253423403809D0,0.D0               ,0.324253423403809D0,
     9 0.613371432700590D0,0.836031107326636D0,0.968160239507626D0/
C
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      DTX(1:MVSIZ) = ZERO
      DTX0(1:MVSIZ) = ZERO
      IL = 1
      GBUF => ELBUF_STR%GBUF
      MBUF => ELBUF_STR%BUFLY(IL)%MAT(1,1,1)
      LBUF => ELBUF_STR%BUFLY(IL)%LBUF(1,1,1)
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
      NPTT = ELBUF_STR%NPTT
c
      BID(:) = ZERO
      NREFSTA = NXREF
      NXREF = 0
      MPT =IABS(NPT)
      DO I=LFT,LLT
        DELTAX(I)=EP30
      ENDDO
      JHBE  = IPARG(23)
      IF (JHBE == 17) MPT = 222
      IREP  = IPARG(35)
      IGTYP = IPARG(38)
      IF (JHBE == 17) JCVT=IPARG(37)
C
      IF (JCVT==1.AND.ISORTH/=0) JCVT=2
C
      NF1=NFT+1
      IDEF =0
C
      IBOLTP = IPARG(72)  !Bolt preloading
C
      DO I=LFT,LLT
        RHOCP(I) =  PM(69,IXS(1,NFT+I))
        TEMP0(I) =  PM(79,IXS(1,NFT+I))
      ENDDO
      
C-----JAC_I [J]^-1 is calculated in global system
      IF (ISMSTR==10.OR.ISMSTR==12) THEN
C   cas GBUF%JAC_I  for all case
        CALL SCOOR3(X ,BID(1)  ,IXS(1,NF1) ,GEO  ,MAT  ,PID  ,NGL  ,
     .           IX1  ,IX2  ,IX3  ,IX4  ,IX5  ,IX6  ,IX7  ,IX8  ,
     .           X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .           Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .           Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .           RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .           E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .           F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,TEMP0, TEMP,
     .           XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .           YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .           ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
        IF (NSIGI > 0 ) THEN
           CALL S8EREFCOOR3(GBUF%SMSTR,8,NEL,
     .            XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .            YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .            ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
        END IF
        CALL S8ZJAC_IC(
     .           XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .           YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .           ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   ,
     .              AJC1 ,AJC2 ,AJC3 ,
     .              AJC4 ,AJC5 ,AJC6 ,
     .              AJC7 ,AJC8 ,AJC9 ,
     .              HX, HY, HZ,
     .              GBUF%JAC_I)
       LLPIJ = ELBUF_STR%BUFLY(IL)%L_PIJ
       IF (LLPIJ<=24) THEN
        DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
          LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
c
           ZR = A_GAUSS(IR,NPTR)
           ZS = A_GAUSS(IS,NPTS)
          ZT = A_GAUSS(IT,NPTT)
          WT = W_GAUSS(IT,NPTT)
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*WT
C
C   cas LBUF%L_PIJ=24 global system w/o assumed strain for Isolid=17,18 only
         CALL S8ZJAC_I3(
     .        ZR,ZS,ZT,WI,
     .        HX, HY, HZ,
     .        AJC1,AJC2,AJC3,
     .        AJC4,AJC5,AJC6,
     .        AJC7,AJC8,AJC9,LBUF%JAC_I,LLPIJ,LBUF%PIJ,LLT)
c
        ENDDO
        ENDDO
        ENDDO
C   cas LBUF%L_PIJ>24 local system w/ assumed strain and return to global only for Isolid=18
       ELSE
        CALL SRCOOR3(X,BID(1),IXS(1,NF1)   ,GEO  ,MAT  ,PID  ,NGL  ,JHBE ,
     .           IX1  ,IX2  ,IX3  ,IX4  ,IX5  ,IX6  ,IX7  ,IX8  ,
     .           X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .           Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .           Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .           RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .           E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .           F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,TEMP0,TEMP,
     .           XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .           YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .           ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
        CALL S8ZPIJ_IC(
     .              XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .              YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .              ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   ,
     .              AJC1 ,AJC2 ,AJC3 ,
     .              AJC4 ,AJC5 ,AJC6 ,
     .              AJC7 ,AJC8 ,AJC9 ,
     .              HX, HY, HZ,
     .              PXC1, PXC2, PXC3, PXC4, 
     .              PYC1, PYC2, PYC3, PYC4, 
     .              PZC1, PZC2, PZC3, PZC4)
C-----------Begin integrating points-----
        DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
          LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
c
           ZR = A_GAUSS(IR,NPTR)
           ZS = A_GAUSS(IS,NPTS)
          ZT = A_GAUSS(IT,NPTT)
          WT = W_GAUSS(IT,NPTT)
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*WT
C
         CALL S8ZJAC_I3(
     .        ZR,ZS,ZT,WI,
     .        HX, HY, HZ,
     .        AJC1,AJC2,AJC3,
     .        AJC4,AJC5,AJC6,
     .        AJC7,AJC8,AJC9,LBUF%JAC_I,LLPIJ,LBUF%PIJ,LLT)
c
        ENDDO
        ENDDO
        ENDDO
       
         NNPT = 8
         DO I=LFT,LLT
          NU(I)=MIN(HALF,PM(21,MAT(I)))
         ENDDO
         CALL S8E_PIJ(NPTR,NPTS,NPTT,NNPT,LLT,
     .               PXC1, PXC2, PXC3, PXC4, 
     .               PYC1, PYC2, PYC3, PYC4, 
     .               PZC1, PZC2, PZC3, PZC4, 
     .                E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .                NU   ,ELBUF_STR)
       END IF !(LLPIJ<=24) THEN
      END IF !(ISMSTR==10.OR.ISMSTR==12)
      IF (JCVT == 0) THEN
        CALL SCOOR3(X ,BID(1),IXS(1,NF1)   ,GEO  ,MAT  ,PID  ,NGL  ,
     .           IX1  ,IX2  ,IX3  ,IX4  ,IX5  ,IX6  ,IX7  ,IX8  ,
     .           X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .           Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .           Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .           RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .           E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .           F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,TEMP0, TEMP,
     .           XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .           YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .           ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
      ELSE
        CALL SRCOOR3(X,BID(1),IXS(1,NF1)   ,GEO  ,MAT  ,PID  ,NGL  ,JHBE ,
     .           IX1  ,IX2  ,IX3  ,IX4  ,IX5  ,IX6  ,IX7  ,IX8  ,
     .           X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .           Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .           Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .           RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .           E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .           F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,TEMP0,TEMP,
     .           XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .           YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .           ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
      ENDIF
      IF (IGTYP == 6) THEN
        CALL SMORTH3(PID  ,GEO  ,IGEO ,SKEW ,IREP ,GBUF%GAMA  ,
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,NSIGI,SIGSP,NSIGS,
     .         SIGI ,IXS  ,X    ,JHBE ,PTSOL,NEL  ,IPARG(28))
      ENDIF
      CALL S8ZDERIC3(GBUF%VOL,HX, HY, HZ,
     .     AJC1,AJC2,AJC3,
     .     AJC4,AJC5,AJC6,
     .     AJC7,AJC8,AJC9,SMAX, VOLU, NGL, 
     .     XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .     YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .     ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
C
      IP=8
      DO IR=1,NPTR
        DO IS=1,NPTS
          DO IT=1,NPTT
            LBUF  => ELBUF_STR%BUFLY(1)%LBUF(IR,IS,IT)
            MBUF  => ELBUF_STR%BUFLY(1)%MAT(IR,IS,IT)
            CALL MATINI(PM       ,IXS    ,NIXS       ,X          ,
     .            GEO      ,ALE_CONNECTIVITY  ,DETONATORS ,IPARG      ,
     .            SIGI     ,NEL    ,SKEW       ,IGEO       ,
     .            IPART    ,IPARTS ,
     .            MAT      ,IPM    ,NSIGS      ,NUMSOL     ,PTSOL  ,
     .            IP       ,NGL    ,NPF        ,TF         ,BUFMAT ,
     .            GBUF     ,LBUF   ,MBUF       ,ELBUF_STR  ,IlOADP ,
     .            FACLOAD, DELTAX)
          ENDDO
        ENDDO
      ENDDO
C 
      IF (IBOLTP /=0) THEN
        CALL SBOLTINI(E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     1                GBUF%BPRELD,NEL  ,IXS  ,NIXS ,VPRELOAD, IFLAG_BPRELOAD)
      ENDIF
C----------------------------------------
C     INITIALISATION DE LA THERMIQUE 
C----------------------------------------
      IF(JTHE /=0) CALL ATHERI(MAT,PM,GBUF%TEMP)
C
      CALL SCZERO3(GBUF%RHO,GBUF%SIG,GBUF%EINT,NEL)
      IF (JHBE == 17) THEN
C---------necessary for SP (to get the same LBUF%VOL)         
         CALL S8EJACIP3(
     .    HX,    HY,    HZ,   
     .    AJC1,AJC2,AJC3,
     .    AJC4,AJC5,AJC6,
     .    AJC7,AJC8,AJC9,
     .    AJP1,AJP2,AJP3,
     .    AJP4,AJP5,AJP6,
     .    AJP7,AJP8,AJP9)
      END IF
C------------------------
C     INTEGRATION POINTS
C------------------------
      NLAY = ELBUF_STR%NLAY
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
      NPTT = ELBUF_STR%NPTT
C-----------Begin integrating points-----

      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
         MBUF => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)
         L_PLA = ELBUF_STR%BUFLY(IL)%L_PLA
         L_SIGB= ELBUF_STR%BUFLY(IL)%L_SIGB
C
         ZR = A_GAUSS(IR,NPTR)
         ZS = A_GAUSS(IS,NPTS)
         ZT = A_GAUSS(IT,NPTT)
         WT = W_GAUSS(IT,NPTT)
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*WT
C
         IF (JHBE == 17) THEN
C---------necessary for SP (to get the same LBUF%VOL)         
          CALL S8EDERI3(LBUF%VOL,VEUL(1,NF1),GEO,WI,
     .                 AJP1(1,IP),AJP2(1,IP),AJP3(1,IP),
     .                 AJP4(1,IP),AJP5(1,IP),AJP6(1,IP),
     .                 AJP7(1,IP),AJP8(1,IP),AJP9(1,IP),
     .                 SMAX, DELTAX, NGL,LBUF%VOL0DP)
         ELSE
         CALL S8ZDERI3(LBUF%VOL,VEUL(1,NF1),GEO,
     .        ZR,ZS,ZT,WI,
     .        HX, HY, HZ,
     .        AJC1,AJC2,AJC3,
     .        AJC4,AJC5,AJC6,
     .        AJC7,AJC8,AJC9,SMAX, DELTAX, NGL,LBUF%VOL0DP)
         END IF !(JHBE == 17) THEN
c
         CALL MATINI(PM      ,IXS    ,NIXS       ,X        ,
     .               GEO     ,ALE_CONNECTIVITY  ,DETONATORS ,IPARG    ,
     .               SIGI    ,NEL    ,SKEW      ,IGEO      ,
     .               IPART   ,IPARTS ,
     .               MAT     ,IPM    ,NSIGS     ,NUMSOL    ,PTSOL  ,
     .               IP      ,NGL    ,NPF       ,TF        ,BUFMAT ,
     .               GBUF    ,LBUF   ,MBUF      ,ELBUF_STR ,ILOADP ,
     .               FACLOAD, DELTAX)
C
         IF(JTHE /=0) CALL ATHERI(MAT,PM,LBUF%TEMP)
C
         IF(MTN>=28)THEN
           NUVAR = IPM(8,IXS(1,NFT+1))
           IDEF =1
         ELSE
           NUVAR = 0
           IF(MTN == 14 .OR. MTN == 12)THEN              
              IDEF =1                                    
           ELSEIF(MTN == 24)THEN                         
             IDEF =1                                     
           ELSEIF(ISTRAIN == 1)THEN                      
             IF(MTN == 1)THEN                            
               IDEF =1                                   
             ELSEIF(MTN == 2)THEN                        
               IDEF =1                                   
             ELSEIF(MTN == 4)THEN                        
               IDEF =1                                   
             ELSEIF(MTN == 3.OR.MTN == 6.OR.MTN ==10.OR. 
     .              MTN == 21.OR.MTN == 22.OR.           
     .              MTN == 23.OR.MTN == 49)THEN          
               IDEF =1                                   
             ENDIF                                       
           ENDIF
         ENDIF
         CALL SIGIN20B(
     .     LBUF%SIG ,PM       ,LBUF%VOL ,SIGSP    ,
     .     SIGI     ,LBUF%EINT,LBUF%RHO ,MBUF%VAR ,LBUF%STRA,
     .     IXS      ,NIXS     ,NSIGI    ,IP       ,NUVAR    ,
     .     NEL      ,IUSER    ,IDEF     ,NSIGS    ,STRSGLOB ,
     .     STRAGLOB ,JHBE     ,IGTYP    ,X        ,GBUF%GAMA,
     .     MAT      ,LBUF%PLA ,L_PLA    ,PTSOL    ,LBUF%SIGB,
     .     L_SIGB   ,IPM      ,BUFMAT   ,LBUF%VOL0DP)
c
          CALL SVALUE0(
     .      LBUF%RHO,LBUF%VOL,LBUF%OFF,LBUF%SIG,LBUF%EINT,DTX,
     .      GBUF%RHO,GBUF%VOL,GBUF%OFF,GBUF%SIG,GBUF%EINT,DTX0,
     .      NEL     )
c
C----------------------------------------
c Initialization of stress tensor in case of Orthotropic properties
C----------------------------------------
          IF (ISIGI /= 0 .AND. ISORTH/=0) THEN 
            LBUF%SIGL = LBUF%SIG
          ENDIF          
c
        ENDDO
       ENDDO
      ENDDO
C----------------------------------------
C     INITIALISATION DES MASSES
C----------------------------------------
      CALL SMASS3(
     .     GBUF%RHO,MAS,PARTSAV,X,V,
     .     IPARTS(NF1),MSS(1,NF1),VOLU  ,
     .     MSNF    ,MSSF(1,NF1) ,BID(1)  ,
     .     BID(1)     ,BID(1)         ,WMA  ,RHOCP     ,MCP     ,
     .     MCPS(1,NF1) ,MSSA ,BID(1)   ,BID(1),GBUF%FILL, 
     .     IX1, IX2, IX3, IX4, IX5, IX6, IX7, IX8)
C----------------------------------------
c Failure model initialisation
C----------------------------------------
      CALL FAILINI(ELBUF_STR,NPTR,NPTS,NPTT,NLAY,
     .             IPM,SIGSP,NSIGI,FAIL_INI ,
     .             SIGI,NSIGS,IXS,NIXS,PTSOL,RNOISE,PERTURB,BUFMAT)
C------------------------------------------
C     assemblage des Volumes nodaux et Modules nodaux
C     (pour rigidites d'interface)
C------------------------------------------
C     attention : IX1, IX2 ... IX8 sont sous la forme NC(MVSIZ,8)
      IF(I7STIFS/=0)THEN
        NCC=8
        CALL SBULK3(VOLU  ,IX1    ,NCC,MAT,PM ,
     2              VOLNOD,BVOLNOD,VNS(1,NF1),BNS(1,NF1),BID(1),
     3              BID(1)   ,GBUF%FILL)
      ENDIF
C------------------------------------------
C       CALCUL DES DT ELEMENTAIRES
C------------------------------------------
        AIRE(:) = ZERO
        CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .       LBUF%EINT ,LBUF%TEMP ,LBUF%DELTAX ,LBUF%RK ,LBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .       VOLU, DTX,IGEO,IGTYP)
c
       DO 10 I=LFT,LLT
        IF(IXS(10,I+NFT)/=0.AND.INVERS>14) THEN
          IF (IGTYP/=0.AND.IGTYP/=6.AND.IGTYP/=14.AND.IGTYP/=15)
     .      THEN
             IPID1=IXS(NIXS-1,I+NFT)
             CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID1),LTITR)
             CALL ANCMSG(MSGID=226,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IGEO(1,IPID1),
     .                   C1=TITR1,
     .                   I2=IGTYP)
          ENDIF
        ENDIF
        DTELEM(NFT+I)=DTX(I)
C       STI = 0.25 * RHO * VOL / (DT*DT)
        STI = FOURTH * GBUF%FILL(I) * GBUF%RHO(I) * VOLU(I) /
     .        MAX(EM20,DTX(I)*DTX(I))
        STIFN(IXS(2,I+NFT))=STIFN(IXS(2,I+NFT))+STI
        STIFN(IXS(3,I+NFT))=STIFN(IXS(3,I+NFT))+STI
        STIFN(IXS(4,I+NFT))=STIFN(IXS(4,I+NFT))+STI
        STIFN(IXS(5,I+NFT))=STIFN(IXS(5,I+NFT))+STI
        STIFN(IXS(6,I+NFT))=STIFN(IXS(6,I+NFT))+STI
        STIFN(IXS(7,I+NFT))=STIFN(IXS(7,I+NFT))+STI
        STIFN(IXS(8,I+NFT))=STIFN(IXS(8,I+NFT))+STI
        STIFN(IXS(9,I+NFT))=STIFN(IXS(9,I+NFT))+STI
  10   CONTINUE
C------------------------------------------
C     SOLID TO SPH, COMPUTE INITIAL VOLUME & MASS OF PARTICLES
C------------------------------------------
      IF(NSPHSOL/=0)THEN
        DO I=LFT,LLT
          IF(SOL2SPH(1,NFT+I) < SOL2SPH(2,NFT+I))THEN
C          SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
           NSPHDIR=IGEO(37,IXS(10,NFT+I))
           NCELF  =SOL2SPH(1,NFT+I)+1
           NCELL  =SOL2SPH(2,NFT+I)-SOL2SPH(1,NFT+I)
           CALL SOLTOSPHV8(
     .       NSPHDIR ,GBUF%RHO(I) ,NCELL   ,X      ,SPBUF(1,NCELF),
     .       IXS(1,I+NFT),KXSP(1,NCELF),IPARTSP(NCELF),
     .                                  IRST(1,NCELF-FIRST_SPHSOL+1))
          END IF
        ENDDO
      END IF
      NXREF = NREFSTA
C-----------
      RETURN
      END
