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
Chd|  S20INIT3                      source/elements/solid/solide20/s20init3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ATHERI                        source/ale/atheri.F           
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|        FAILINI                       source/elements/solid/solide/failini.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        S20COOR3                      source/elements/solid/solide20/s20coor3.F
Chd|        S20DERI3                      source/elements/solid/solide20/s20deri3.F
Chd|        S20MASS3                      source/elements/solid/solide20/s20mass3.F
Chd|        S20MSI                        source/elements/solid/solide20/s20mass3.F
Chd|        S20RST                        source/elements/solid/solide20/s20deri3.F
Chd|        SBULK3                        source/elements/solid/solide/sbulk3.F
Chd|        SIGIN20B                      source/elements/solid/solide20/s20mass3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S20INIT3(
     1           ELBUF_STR,MAS      ,IXS     ,PM      ,X         ,
     2           DETONATORS,GEO      ,VEUL    ,ALE_CONNECTIVITY ,IPARG     ,
     3           DTELEM   ,SIGI     ,NEL     ,SKEW    ,IGEO      ,
     4           STIFN    ,PARTSAV  ,V       ,IPARTS  ,MSS       ,
     5           IXS20    ,IPART    ,MSSX    ,SIGSP   ,NSIGI     ,
     7           IPM      ,IUSER    ,NSIGS   ,VOLNOD  ,BVOLNOD   ,
     8           VNS      ,BNS      ,VNSX    ,BNSX    ,PTSOL     ,
     9           BUFMAT   ,MCP      ,MCPS    ,MCPSX   ,TEMP      ,
     A           NPF      ,TF       ,STRSGLOB,STRAGLOB,FAIL_INI  ,
     B           ILOADP   ,FACLOAD  ,RNOISE  ,PERTURB )
      USE MESSAGE_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD     
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
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "scry_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPARG(*),IPARTS(*),IGEO(NPROPGI,*),
     .   IXS20(12,*), IPART(LIPART1,*), IPM(NPROPMI,*), PTSOL(*),
     .   NPF(*),STRSGLOB(*),STRAGLOB(*),FAIL_INI(*),PERTURB(NPERTURB)
      INTEGER NEL,NSIGI,IUSER,NSIGS
      my_real
     .   MAS(*), PM(NPROPM,*), X(*), GEO(NPROPG,*),
     .   VEUL(LVEUL,*), DTELEM(*),SIGI(NSIGS,*),SKEW(LSKEW,*),STIFN(*),
     .   PARTSAV(20,*), V(*), MSS(8,*), MSSX(12,*), SIGSP(NSIGI,*),
     .   VOLNOD(*),BVOLNOD(*), VNS(8,*), BNS(8,*),RNOISE(NPERTURB,*),
     .   VNSX(12,*), BNSX(12,*),BUFMAT(*),MCP(*), MCPS(8,*),MCPSX(12,*),
     .   TEMP(*), TF(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*) 
      TYPE(DETONATOR_STRUCT_)::DETONATORS     
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NF1, IBID, I, IGTYP, IP, NF2,NPTR,NPTS,NPTT,IR,IS,IT,
     .        NB01,NB02,NB03,NB04,NB05,NB06, NUVAR,IDEF,
     .        JHBE, IPID1,NLAY,L_PLA,L_SIGB
      INTEGER NC(MVSIZ,20),MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ), NCC
      CHARACTER*nchartitle,
     .   TITR1
      my_real
     .   BID, FV(1),
     .   V8LOC(51,MVSIZ), MASS(MVSIZ),
     .  VOLP(MVSIZ,5), STI(MVSIZ),DELTAX(MVSIZ),DELTAX2(MVSIZ),
     .  XX(MVSIZ,20), YY(MVSIZ,20), ZZ(MVSIZ,20),
     .  VX(MVSIZ,20), VY(MVSIZ,20), VZ(MVSIZ,20),
     .  PX(MVSIZ,20), PY(MVSIZ,20), PZ(MVSIZ,20),
     .  RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),
     .  SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),VOLG(MVSIZ),
     .  TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ),UL(MVSIZ,20),
     .  NI(MVSIZ,20),DNIDR(MVSIZ,20),DNIDS(MVSIZ,20),DNIDT(MVSIZ,20),
     .  DTX(MVSIZ), WI,RHOCP(MVSIZ),TEMP0(MVSIZ), AIRE(MVSIZ)
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
      GBUF => ELBUF_STR%GBUF
      IGTYP = IPARG(38)
      JHBE  = IPARG(23)
      NF1   = NFT+1
      NF2   = NF1-(NUMELS8+NUMELS10)
c
      DO I=LFT,LLT
        RHOCP(I) =  PM(69,IXS(1,NFT+I))
        TEMP0(I) =  PM(79,IXS(1,NFT+I))
      ENDDO
C
      CALL S20COOR3(
     1   X          ,V     ,IXS(1,NF1),IXS20(1,NF2),XX      ,
     2   YY         ,ZZ    ,VX        ,VY          ,VZ      , 
     3   NC         ,NGL   ,MAT       ,PID         ,MASS    ,
     4   DTELEM(NF1),STI   ,GBUF%SIG  ,GBUF%EINT   ,GBUF%RHO,
     5   GBUF%QVIS  ,TEMP0 ,TEMP      ,NEL         )
C----------------------------------------
C     INITIALISATION DE LA THERMIQUE 
C----------------------------------------
      IF(JTHE /=0) CALL ATHERI(MAT,PM,GBUF%TEMP)
C-----------------------------
C     POINTS D' INTEGRATION
C-----------------------------
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
      NPTT = ELBUF_STR%NPTT
      NLAY = ELBUF_STR%NLAY
c
      DO IT=1,NPTT
       DO IS=1,NPTS
        DO IR=1,NPTR
c
          LBUF => ELBUF_STR%BUFLY(1)%LBUF(IR,IS,IT)
          MBUF => ELBUF_STR%BUFLY(1)%MAT(IR,IS,IT)
          L_PLA = ELBUF_STR%BUFLY(1)%L_PLA
          L_SIGB = ELBUF_STR%BUFLY(1)%L_SIGB
          IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
          WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IT,NPTT)
C
          CALL S20RST(
     1      A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),A_GAUSS(IT,NPTT),NI   ,
     2      DNIDR         ,DNIDS         ,DNIDT         )
c
          CALL S20DERI3(NGL,LBUF%OFF,
     1    A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),A_GAUSS(IT,NPTT),WI,
     2    DNIDR  ,DNIDS   ,DNIDT  ,RX     ,RY     ,RZ         ,
     3    SX     ,SY      ,SZ     ,TX     ,TY     ,TZ         ,
     4    XX     ,YY      ,ZZ     ,PX     ,PY     ,PZ         ,
     5    LBUF%VOL,DELTAX ,DELTAX2,IR*IS*IT,NPTR*NPTS*NPTT,UL ,
     6    GBUF%VOL,LBUF%VOL0DP)
C
          CALL MATINI(PM      ,IXS      ,NIXS      ,X         ,
     .               GEO      ,ALE_CONNECTIVITY    ,DETONATORS,IPARG     ,
     .               SIGI     ,NEL      ,SKEW      ,IGEO      ,
     .               IPART    ,IPARTS   ,
     .               MAT      ,IPM      ,NSIGS     ,NUMSOL    ,PTSOL  ,
     .               IP       ,NGL      ,NPF       ,TF        ,BUFMAT ,
     .               GBUF     ,LBUF     ,MBUF      ,ELBUF_STR ,ILOADP ,
     .               FACLOAD, DELTAX)
C----------------------------------------
          AIRE(:) = ZERO
          CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .         LBUF%EINT ,LBUF%TEMP ,LBUF%DELTAX ,LBUF%RK ,LBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .         GBUF%VOL, DTX       , IGEO)
C----------------------------------------
c         INITIALISATION DES MASSES
C
          CALL S20MSI(LBUF%RHO, MASS    , LBUF%VOL , DTELEM(NF1), STI    ,
     .                LBUF%OFF, LBUF%SIG, LBUF%EINT, DTX        , NEL    ,
     .                GBUF%OFF, GBUF%SIG, GBUF%EINT, GBUF%RHO   , WI/EIGHT)
C----------------------------------------
         IF (MTN>=28)THEN
           NUVAR  = IPM(8,IXS(1,NF1))
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
     .               MTN == 21.OR.MTN == 22.OR.
     .               MTN == 23.OR.MTN == 49)THEN
                IDEF =1
              ENDIF
            ENDIF
          ENDIF
         CALL SIGIN20B(LBUF%SIG,PM       ,LBUF%VOL,SIGSP    ,
     .                 SIGI    ,LBUF%EINT,LBUF%RHO,MBUF%VAR ,LBUF%STRA,
     .                 IXS     ,NIXS     ,NSIGI   ,IP       ,NUVAR    ,
     .                 NEL     ,IUSER    ,IDEF    ,NSIGS    ,STRSGLOB ,
     .                 STRAGLOB,JHBE     ,IGTYP   ,X        ,LBUF%GAMA,
     .                 MAT     ,LBUF%PLA ,L_PLA   ,PTSOL    ,LBUF%SIGB,
     .                 L_SIGB  ,IPM      ,BUFMAT  ,LBUF%VOL0DP)
        ENDDO
       ENDDO
      ENDDO     ! Points d'integration
C------------------------------------------
      LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
      AIRE(:) = ZERO
      CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .     GBUF%EINT ,GBUF%TEMP ,GBUF%DELTAX ,GBUF%RK ,LBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .     GBUF%VOL, DTX        , IGEO )
c
      CALL S20MASS3(
     1     MASS        ,MAS,PARTSAV,IPARTS(NF1),MSS(1,NF1),GBUF%VOL   ,
     2     XX          ,YY ,ZZ     ,VX         ,VY        ,VZ         ,
     3     NC          ,STI,STIFN  ,DELTAX2    ,GBUF%RHO  ,DTX        ,
     4     DTELEM(NF1) ,MSSX(1,NF1),RHOCP      ,MCP       ,MCPS(1,NF1),
     5     MCPSX(1,NF1),GBUF%FILL)
C----------------------------------------
c Failure model initialisation
C----------------------------------------
      CALL FAILINI(ELBUF_STR,NPTR,NPTS,NPTT,NLAY,
     .             IPM,SIGSP,NSIGI,FAIL_INI,
     .             SIGI,NSIGS,IXS,NIXS,PTSOL,
     .             RNOISE,PERTURB,BUFMAT)
C------------------------------------------
C     assemblage des Volumes nodaux et Modules nodaux
C     (pour rigidites d'interface)
C------------------------------------------
      IF(I7STIFS/=0)THEN
        NCC=20
        CALL SBULK3(GBUF%VOL ,NC     ,NCC      ,MAT        ,PM         ,
     2              VOLNOD   ,BVOLNOD,VNS(1,NF1),BNS(1,NF1),VNSX(1,NF1),
     3              BNSX(1,NF1),GBUF%FILL)
      ENDIF
C------------------------------------------
      DO I=LFT,LLT
        IF(IXS(10,I+NFT)/=0) THEN
          IF(     IGTYP/=0 .AND.IGTYP/=6
     .       .AND.IGTYP/=14.AND.IGTYP/=15)THEN
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
      ENDDO
C-----------
      RETURN
      END
