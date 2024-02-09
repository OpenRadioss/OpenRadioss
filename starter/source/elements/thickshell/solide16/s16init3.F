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
Chd|  S16INIT3                      source/elements/thickshell/solide16/s16init3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ATHERI                        source/ale/atheri.F           
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|        FAILINI                       source/elements/solid/solide/failini.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        S16COOR3                      source/elements/thickshell/solide16/s16coor3.F
Chd|        S16DERI3                      source/elements/thickshell/solide16/s16deri3.F
Chd|        S16MASS3                      source/elements/thickshell/solide16/s16mass3.F
Chd|        S16RST                        source/elements/thickshell/solide16/s16deri3.F
Chd|        S20MSI                        source/elements/solid/solide20/s20mass3.F
Chd|        SBULK3                        source/elements/solid/solide/sbulk3.F
Chd|        SIGIN20B                      source/elements/solid/solide20/s20mass3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S16INIT3(
     1      ELBUF_STR ,MAS     ,IXS     ,PM      ,X        ,
     2      DETONATORS,GEO     ,VEUL    ,ALE_CONNECTIVITY ,IPARG    ,
     3      DTELEM    ,SIGI    ,NEL     ,SKEW    ,IGEO     ,
     4      STIFN     ,PARTSAV ,V       ,IPARTS  ,MSS      ,
     5      IXS16     ,IPART   ,MSSX    ,SIGSP   ,NSIGI    ,
     6      IPM       ,IUSER   ,NSIGS   ,VOLNOD  ,BVOLNOD  ,
     7      VNS       ,BNS     ,VNSX    ,BNSX    ,PTSOL    ,
     8      BUFMAT    ,MCP     ,MCPS    ,MCPSX   ,TEMP     ,
     9      NPF       ,TF      ,STRSGLOB,STRAGLOB,FAIL_INI ,
     A      ILOADP    ,FACLOAD ,RNOISE  ,PERTURB ,MAT_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE DETONATORS_MOD      
      USE ALE_CONNECTIVITY_MOD
      USE MATPARAM_DEF_MOD
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
#include      "param_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "scry_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(*),IPARTS(*),IGEO(NPROPGI,*),
     .   IXS16(8,*), IPART(LIPART1,*),IPM(NPROPMI,*), PTSOL(*),
     .   NPF(*),STRSGLOB(*),STRAGLOB(*),FAIL_INI(*),PERTURB(NPERTURB)
      INTEGER NEL,NSIGI,IUSER,NSIGS
      my_real
     .   MAS(*), PM(NPROPM,*), X(*), GEO(NPROPG,*),
     .   VEUL(LVEUL,*), DTELEM(*),SIGI(NSIGS,*),SKEW(LSKEW,*),STIFN(*),
     .   PARTSAV(20,*), V(*), MSS(8,*), MSSX(12,*), SIGSP(NSIGI, *),
     .   VOLNOD(*), BVOLNOD(*), VNS(8,*), BNS(8,*),
     .   VNSX(12,*), BNSX(12,*),BUFMAT(*),RNOISE(NPERTURB,*),
     .   MCP(*), MCPS(8,*),MCPSX(12,*), TEMP(*), TF(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(DETONATORS_STRUCT_)::DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NF1,IBID,I,IGTYP,IP,NF2,NPTR,NPTS,NPTT,NLAY,IL,IR,IS,IT,
     .        N, NUVAR,IINT, NCC,IDEF,JHBE,IPID1,L_PLA,L_SIGB
      INTEGER NC(MVSIZ,16),MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ),RBID(1)
      CHARACTER*nchartitle,
     .   TITR1
      my_real
     .   BID, FV,WI,AA,BB
      my_real
     .   MASS(MVSIZ),
     .  VOLP(MVSIZ,5), STI(MVSIZ),DELTAX(MVSIZ),DELTAX2(MVSIZ),
     .  XX(MVSIZ,16), YY(MVSIZ,16), ZZ(MVSIZ,16),
     .  VX(MVSIZ,16), VY(MVSIZ,16), VZ(MVSIZ,16),
     .  PX(MVSIZ,16), PY(MVSIZ,16), PZ(MVSIZ,16),
     .  RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),
     .  SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),VOLG(MVSIZ),
     .  TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ),UL(MVSIZ,16),
     .  NI(MVSIZ,16),DNIDR(MVSIZ,16),DNIDS(MVSIZ,16),DNIDT(MVSIZ,16),
     .  DTX(MVSIZ),STIN(MVSIZ,16), RHOCP(MVSIZ),TEMP0(MVSIZ), AIRE(MVSIZ)
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
C-----------------------------------------------
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9),W_LOBATTO(9,9),A_LOBATTO(9,9),
     .  W_NEWTON(9,9),A_NEWTON(9,9)
C-----------------------------------------------
      DATA W_GAUSS /
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.555555555555556,0.888888888888889,0.555555555555556,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.347854845137454,0.652145154862546,0.652145154862546,
     4 0.347854845137454,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.236926885056189,0.478628670499366,0.568888888888889,
     5 0.478628670499366,0.236926885056189,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.171324492379170,0.360761573048139,0.467913934572691,
     6 0.467913934572691,0.360761573048139,0.171324492379170,
     6 0.               ,0.               ,0.               ,
     7 0.129484966168870,0.279705391489277,0.381830050505119,
     7 0.417959183673469,0.381830050505119,0.279705391489277,
     7 0.129484966168870,0.               ,0.               ,
     8 0.101228536290376,0.222381034453374,0.313706645877887,
     8 0.362683783378362,0.362683783378362,0.313706645877887,
     8 0.222381034453374,0.101228536290376,0.               ,
     9 0.081274388361574,0.180648160694857,0.260610696402935,
     9 0.312347077040003,0.330239355001260,0.312347077040003,
     9 0.260610696402935,0.180648160694857,0.081274388361574/
      DATA A_GAUSS /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/
C-----
      DATA W_LOBATTO /
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.333333333333333,1.333333333333333,0.333333333333333,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.166666666666667,0.833333333333333,0.833333333333333,
     4 0.166666666666667,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.1              ,0.544444444444444,0.711111111111111,
     5 0.544444444444444,0.1              ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.066666666666667,0.37847496       ,0.55485838       ,
     6 0.55485838       ,0.37847496       ,0.066666666666667,
     6 0.               ,0.               ,0.               ,
     7 0.04761904       ,0.27682604       ,0.43174538       ,
     7 0.48761904       ,0.43174538       ,0.27682604       ,
     7 0.04761904       ,0.               ,0.               ,
     8 0.03571428       ,0.21070422       ,0.34112270       ,
     8 0.41245880       ,0.41245880       ,0.34112270       ,
     8 0.21070422       ,0.03571428       ,0.               ,
     9 0.027777777777778,0.1654953616     ,0.2745387126     ,
     9 0.3464285110     ,0.3715192744     ,0.3464285110     ,
     9 0.2745387126     ,0.1654953616     ,0.027777777777778/
      DATA A_LOBATTO /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -1.              ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -1.              ,0.               ,1.               ,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -1.              ,-.44721360       ,0.44721360       ,
     4  1.              ,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -1.              ,-.65465367       ,0.               ,
     5 0.65465367       , 1.              ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -1.              ,-.76505532       ,-.28523152       ,
     6 0.28523152       ,0.76505532       , 1.              ,
     6 0.               ,0.               ,0.               ,
     7 -1.              ,-.83022390       ,-.46884879       ,
     7 0.               ,0.46884879       ,0.83022390       ,
     7  1.              ,0.               ,0.               ,
     8 -1.              ,-.87174015       ,-.59170018       ,
     8 -.20929922       ,0.20929922       ,0.59170018       ,
     8 0.87174015       , 1.              ,0.               ,
     9 -1.              ,-.8997579954     ,-.6771862795     ,
     9 -.3631174638     ,0.               ,0.3631174638     ,
     9 0.6771862795     ,0.8997579954     , 1.              /
C-----
C-----
      DATA W_NEWTON /
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.5              ,1.               ,0.5              ,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.166666666666667,0.833333333333333,0.833333333333333,
     4 0.166666666666667,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.25             ,0.5              ,0.5              ,
     5 0.5              ,0.25             ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.066666666666667,0.37847496       ,0.55485838       ,
     6 0.55485838       ,0.37847496       ,0.066666666666667,
     6 0.               ,0.               ,0.               ,
     7 0.04761904       ,0.27682604       ,0.43174538       ,
     7 0.48761904       ,0.43174538       ,0.27682604       ,
     7 0.04761904       ,0.               ,0.               ,
     8 0.03571428       ,0.21070422       ,0.34112270       ,
     8 0.41245880       ,0.41245880       ,0.34112270       ,
     8 0.21070422       ,0.03571428       ,0.               ,
     9 0.027777777777778,0.1654953616     ,0.2745387126     ,
     9 0.3464285110     ,0.3715192744     ,0.3464285110     ,
     9 0.2745387126     ,0.1654953616     ,0.027777777777778/
      DATA A_NEWTON /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -1.              ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -1.              ,0.               ,1.               ,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -1.              ,-.44721360       ,0.44721360       ,
     4  1.              ,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -1.              ,-.5              ,0.               ,
     5 0.5              , 1.              ,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -1.              ,-.76505532       ,-.28523152       ,
     6 0.28523152       ,0.76505532       , 1.              ,
     6 0.               ,0.               ,0.               ,
     7 -1.              ,-.83022390       ,-.46884879       ,
     7 0.               ,0.46884879       ,0.83022390       ,
     7  1.              ,0.               ,0.               ,
     8 -1.              ,-.87174015       ,-.59170018       ,
     8 -.20929922       ,0.20929922       ,0.59170018       ,
     8 0.87174015       , 1.              ,0.               ,
     9 -1.              ,-.8997579954     ,-.6771862795     ,
     9 -.3631174638     ,0.               ,0.3631174638     ,
     9 0.6771862795     ,0.8997579954     , 1.              /
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
      NPTT = ELBUF_STR%NPTT
      NLAY = ELBUF_STR%NLAY
c
      JHBE  = IPARG(23)
      IINT =  IPARG(36)
      IGTYP = IPARG(38)
      IDEF  = 0
      NF1=NFT+1
      NF2=NF1-(NUMELS8+NUMELS10+NUMELS20)
      IBID = 0
      RBID = ZERO
C
      DO I=LFT,LLT
        RHOCP(I) =  PM(69,IXS(1,NFT+I))
        TEMP0(I) =  PM(79,IXS(1,NFT+I))
      ENDDO
C
      CALL S16COOR3(
     1   X            ,V        ,IXS(1,NF1) ,IXS16(1,NF2),XX      ,
     2   YY           ,ZZ       ,VX         ,VY          ,VZ      ,
     3   NC           ,NGL      ,MAT        ,PID         ,MASS    ,
     4   DTELEM(NFT+1),STI      ,GBUF%SIG   ,GBUF%EINT   ,GBUF%RHO,
     5   GBUF%QVIS    ,TEMP0    ,TEMP       ,NEL         )
C
      DO N=1,16
        DO I=LFT,LLT
          UL(I,N) = ZERO
        ENDDO
      ENDDO
      DO I=LFT,LLT
        VOLG(I) = ZERO
      ENDDO
C
C----------------------------------------
C     INITIALISATION DE LA THERMIQUE 
C----------------------------------------
      IF(JTHE /=0) CALL ATHERI(MAT,PM,GBUF%TEMP)
C-----------------------------
C     POINTS D' INTEGRATION
C-----------------------------
      IS = 1 
      DO IT=1,NPTT
       DO IR=1,NPTR
        DO IL=1,NLAY
c
         LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
         MBUF => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)
         L_PLA = ELBUF_STR%BUFLY(IL)%L_PLA
         L_SIGB = ELBUF_STR%BUFLY(IL)%L_SIGB
         IP = IR + ( (IL-1) + (IT-1)*NLAY )*NPTR
C
         IF (IINT == 1) THEN
C---       GAUSS
           WI = W_GAUSS(IR,NPTR)*W_GAUSS(IL,NLAY)*W_GAUSS(IT,NPTT)
c
           CALL S16RST(
     1      A_GAUSS(IR,NPTR),A_GAUSS(IL,NLAY),A_GAUSS(IT,NPTT),NI   ,
     2      DNIDR         ,DNIDS         ,DNIDT         )

           CALL S16DERI3(NGL,LBUF%OFF,
     1      A_GAUSS(IR,NPTR),A_GAUSS(IL,NLAY),A_GAUSS(IT,NPTT),WI,
     2      DNIDR  ,DNIDS  ,DNIDT  ,RX     ,RY     ,RZ         ,
     3      SX     ,SY     ,SZ     ,TX     ,TY     ,TZ         ,
     4      XX     ,YY     ,ZZ     ,PX     ,PY     ,PZ         ,
     5      LBUF%VOL,DELTAX ,STIN   ,NI     ,VOLG   ,UL        ,LBUF%VOL0DP)
         ELSEIF (IINT == 2) THEN
C---       LOBATO
           WI = W_GAUSS(IR,NPTR)*W_LOBATTO(IL,NLAY)*W_GAUSS(IT,NPTT)
c
           CALL S16RST(
     1        A_GAUSS(IR,NPTR),A_LOBATTO(IL,NLAY),A_GAUSS(IT,NPTT),NI ,
     2        DNIDR         ,DNIDS         ,DNIDT         )

           CALL S16DERI3(NGL,LBUF%OFF,
     1      A_GAUSS(IR,NPTR),A_LOBATTO(IL,NLAY),A_GAUSS(IT,NPTT),WI,
     2      DNIDR  ,DNIDS  ,DNIDT  ,RX     ,RY     ,RZ         ,
     3      SX     ,SY     ,SZ     ,TX     ,TY     ,TZ         ,
     4      XX     ,YY     ,ZZ     ,PX     ,PY     ,PZ         ,
     5      LBUF%VOL,DELTAX ,STIN   ,NI     ,VOLG   ,UL        ,LBUF%VOL0DP )
         ENDIF
c
         CALL MATINI(PM       ,IXS    ,NIXS        ,X          ,
     .               GEO      ,ALE_CONNECTIVITY  ,DETONATORS  ,IPARG      ,
     .               SIGI     ,NEL    ,SKEW        ,IGEO(1,1)  ,
     .               IPART    ,IPARTS ,
     .               MAT      ,IPM    ,NSIGS       ,NUMSOL     ,PTSOL    ,
     .               IP       ,NGL    ,NPF         ,TF         ,BUFMAT   ,
     .               GBUF     ,LBUF   ,MBUF        ,ELBUF_STR  ,ILOADP   ,
     .               FACLOAD, DELTAX)
C----------------------------------------
C     INITIALISATION DES MASSES
C----------------------------------------
         CALL S20MSI(LBUF%RHO ,MASS     ,LBUF%VOL  ,DTELEM(NFT+1),STI    ,
     .               LBUF%OFF ,LBUF%SIG ,LBUF%EINT ,DTX          ,NEL    ,
     .               GBUF%OFF ,GBUF%SIG ,GBUF%EINT ,GBUF%RHO     ,WI/EIGHT)
C
C-------------------------------------------
C    INITIALISATION DES CONTRAINTES + ENERGIE+ DENSITE + PLA
C---------------------------------------------
         IF (MTN >= 28) THEN
                    NUVAR =  IPM(8,IXS(1,NFT+1))
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
     .         MTN == 23.OR.MTN == 49)THEN               
               IDEF =1                                   
             ENDIF                                       
           ENDIF                                        
         ENDIF
         CALL SIGIN20B(LBUF%SIG,PM       ,LBUF%VOL,SIGSP   ,           
     .                 SIGI    ,LBUF%EINT,LBUF%RHO,MBUF%VAR,LBUF%STRA, 
     .                 IXS     ,NIXS     ,NSIGI   ,IP      ,NUVAR    , 
     .                 NEL     ,IUSER    ,IDEF    ,NSIGS   ,STRSGLOB , 
     .                 STRAGLOB,JHBE     ,IGTYP   ,X       ,LBUF%GAMA, 
     .                 MAT     ,LBUF%PLA ,L_PLA   ,PTSOL   ,LBUF%SIGB,
     .                 L_SIGB  ,IPM      ,BUFMAT  ,LBUF%VOL0DP)                                       
        ENDDO
       ENDDO
      ENDDO
C----------
      DO I=LFT,LLT
        AA = MAX(UL(I,1),UL(I,2),UL(I,3),UL(I,4),
     .           UL(I,5),UL(I,6),UL(I,7),UL(I,8))
        BB = MAX(UL(I,9) ,UL(I,10),UL(I,11),UL(I,12),UL(I,13),UL(I,14),
     .           UL(I,15),UL(I,16))
        DELTAX2(I) = AA/MAX(AA,BB)
        AA = AA*THIRTY2
        BB = BB*THIRTY2*THIRD
        DELTAX(I) = SQRT(TWO*VOLG(I)/MAX(AA,BB))
        GBUF%VOL(I) = VOLG(I)
      ENDDO
C
      AIRE(:) = ZERO
      CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .     GBUF%EINT ,GBUF%TEMP ,GBUF%DELTAX ,GBUF%RK ,GBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .     GBUF%VOL, DTX, IGEO,IGTYP)
c
      CALL S16MASS3(
     1        MASS         ,MAS,PARTSAV,IPARTS(NF1),MSS(1,NF1),VOLG,
     2        XX           ,YY ,ZZ     ,VX         ,VY         ,VZ  ,
     3        NC           ,STI,STIFN  ,DELTAX2    ,GBUF%RHO   ,DTX ,
     4        DTELEM(NFT+1),MSSX(1,NF1),RHOCP, MCP, MCPS(1,NF1)     ,
     5        MCPSX(1,NF1) ,GBUF%FILL  )
C----------------------------------------
c Failure model initialisation
C----------------------------------------
      CALL FAILINI(ELBUF_STR,NPTR,NPTS,NPTT,NLAY,
     .             IPM,SIGSP,NSIGI,FAIL_INI ,
     .             SIGI,NSIGS,IXS,NIXS,PTSOL,
     .             RNOISE,PERTURB,MAT_PARAM)
C------------------------------------------
C     assemblage des Volumes nodaux et Modules nodaux
C     (pour rigidites d'interface)
C------------------------------------------
      IF (I7STIFS /= 0) THEN
        NCC=16
        CALL SBULK3(VOLG  ,NC     ,NCC,MAT,PM ,
     2              VOLNOD,BVOLNOD,VNS(1,NF1),BNS(1,NF1),
     3              VNSX(1,NF1),BNSX(1,NF1)  ,GBUF%FILL)
      ENDIF
C------------------------------------------
      DO I=LFT,LLT
        IF(IXS(10,I+NFT) /= 0) THEN
          IF (IGTYP/=0 .AND. IGTYP /= 14 .AND. IGTYP/=20 .AND.
     .        IGTYP/=21) THEN
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
