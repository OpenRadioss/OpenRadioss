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
Chd|  S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ATHERI                        source/ale/atheri.F           
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|        FAILINI                       source/elements/solid/solide/failini.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        S10COOR3                      source/elements/solid/solide10/s10coor3.F
Chd|        S10DERI3                      source/elements/solid/solide10/s10deri3.F
Chd|        S10LEN3                       source/elements/solid/solide10/s10len3.F
Chd|        S10MASS3                      source/elements/solid/solide10/s10mass3.F
Chd|        S10MSI                        source/elements/solid/solide10/s10mass3.F
Chd|        SBOLTINI                      source/loads/bolt/sboltini.F  
Chd|        SBULK3                        source/elements/solid/solide/sbulk3.F
Chd|        SIGIN20B                      source/elements/solid/solide20/s20mass3.F
Chd|        SMORTH3                       source/elements/solid/solide/smorth3.F
Chd|        SREPLOC3                      source/elements/solid/solide/sreploc3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        BPRELOAD_MOD                  share/modules1/bpreload_mod.F 
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S10INIT3(ELBUF_STR,
     1    MAS     ,IXS     ,PM      ,X      ,
     2    DETONATORS,GEO     ,VEUL    ,ALE_CONNECTIVITY ,IPARG    ,
     3    DTELEM  ,SIGI    ,NEL     ,SKEW   ,IGEO     ,
     4    STIFN   ,PARTSAV ,V       ,IPARTS ,MSS      ,
     5    IXS10   ,IPART   ,
     7    MSSX    ,SIGSP   ,NSIGI  ,IPM      ,
     8    IUSER   ,NSIGS   ,VOLNOD  ,BVOLNOD,VNS      ,
     9    BNS     ,VNSX    ,BNSX    ,PTSOL  ,BUFMAT   ,
     A    MCP     ,MCPS    ,MCPSX   ,TEMP   ,NPF      ,
     B    TF      ,IN      ,STIFR   ,INS    ,MSSA     ,
     C    STRSGLOB,STRAGLOB,FAIL_INI,ILOADP ,FACLOAD  ,
     D    RNOISE  ,PERTURB )
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
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr12_c.inc"
#include      "scry_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(*),IPARTS(*),IGEO(NPROPGI,*),
     .   IXS10(6,*), IPART(LIPART1,*),IPM(NPROPMI,*),
     .   NPF(*),STRSGLOB(*),STRAGLOB(*),PTSOL(*),FAIL_INI(*),PERTURB(NPERTURB)
      INTEGER NEL ,NSIGI,IUSER, NSIGS
      my_real
     .   MAS(*),PM(NPROPM,*), X(*), GEO(NPROPG,*),
     .   VEUL(LVEUL,*), DTELEM(*),SIGI(NSIGS,*),SKEW(LSKEW,*),STIFN(*),
     .   PARTSAV(20,*), V(*), MSS(8,*), MSSX(12,*) , SIGSP(NSIGI,*),
     .   VOLNOD(*),BVOLNOD(*), VNS(8,*), BNS(8,*),RNOISE(NPERTURB,*),
     .   VNSX(12,*), BNSX(12,*) ,BUFMAT(*),MCP(*),MCPS(8,*),MCPSX(12,*),
     .   TEMP(*), TF(*), IN(*),STIFR(*), INS(8,*), MSSA(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(DETONATOR_STRUCT_) :: DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IP,NF1,NF2,IBID,IGTYP,NUVAR,IREP,NCC,IDEF,JHBE,IPID
      INTEGER ID,NPTR,NPTS,NPTT,NLAY,L_PLA,L_SIGB,IBOLTP,IINT
      CHARACTER*nchartitle,
     .   TITR
      INTEGER NC(MVSIZ,10),MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ)
      DOUBLE PRECISION
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10) 
      my_real
     .   BID, FV,
     .   V8LOC(51,MVSIZ), VOLU(MVSIZ), MASS(MVSIZ),VOLG(MVSIZ),
     .   VOLP(MVSIZ,5), STI(MVSIZ),DELTAX(MVSIZ),DELTAX2(MVSIZ),
     .   VX(MVSIZ,10), VY(MVSIZ,10), VZ(MVSIZ,10),
     .   PX(MVSIZ,10,5),PY(MVSIZ,10,5),PZ(MVSIZ,10,5),
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),
     .   SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),
     .   TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ),
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),
     .   E2Y(MVSIZ),E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   NX(MVSIZ,10,5), WIP(5,5) ,ALPH(5,5),BETA(5,5),MASSCP(MVSIZ),
     .   RHOCP(MVSIZ),TEMP0(MVSIZ), AIRE(1), DTX(MVSIZ)
C
C-----------------------------------------------
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
C-----------------------------------------------
      DATA WIP / 1.  ,0.  ,0.  ,0.  ,0.  ,
     2           0.  ,0.  ,0.  ,0.  ,0.  ,
     3           0.  ,0.  ,0.  ,0.  ,0.  ,
     4           0.25,0.25,0.25,0.25,0.  ,
     5           0.45,0.45,0.45,0.45,-0.8/
      DATA ALPH /0.  ,0.  ,0.  ,0.  ,0.  ,
     2           0.  ,0.  ,0.  ,0.  ,0.  ,
     3           0.  ,0.  ,0.  ,0.  ,0.  ,
     4           0.58541020,0.58541020,0.58541020,0.58541020,0.  ,
     5           0.5       ,0.5       ,0.5       ,0.5       ,0.25/
      DATA BETA /0.  ,0.  ,0.  ,0.  ,0.  ,
     2           0.  ,0.  ,0.  ,0.  ,0.  ,
     3           0.  ,0.  ,0.  ,0.  ,0.  ,
     4           0.13819660,0.13819660,0.13819660,0.13819660,0.  ,
     5           0.16666666666667,0.16666666666667,0.16666666666667,
     5                                        0.16666666666667,0.25/
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF => ELBUF_STR%GBUF
c
      IREP  = IPARG(35)
      IGTYP = IPARG(38)
      JHBE  = IPARG(23)
      IINT  = IPARG(36)
      NF1  = NFT+1
      NF2  = NF1-NUMELS8
      IF (ISROT == 1) NF2=1
      IDEF = 0
      NPTR  =  ELBUF_STR%NPTR
      NPTS  =  ELBUF_STR%NPTS
      NPTT  =  ELBUF_STR%NPTT
      NLAY  =  ELBUF_STR%NLAY
C
      IBOLTP = IPARG(72)  !Bolt preloading
      JCVT  = IPARG(37)
C
      DO I=LFT,LLT
        RHOCP(I) =  PM(69,IXS(1,NFT+I))
        TEMP0(I) =  PM(79,IXS(1,NFT+I))
      ENDDO
C
      CALL S10COOR3(
     1              X          ,V     ,IXS(1,NF1) ,IXS10(1,NF2) ,XX      , 
     2              YY         ,ZZ    ,VX         ,VY           ,VZ      ,
     3              NC         ,NGL   ,MAT        ,PID          ,MASS    ,
     4              DTELEM(NF1),STI   ,GBUF%SIG   ,GBUF%EINT    ,GBUF%RHO,
     5              GBUF%QVIS  ,TEMP0 ,TEMP       ,GBUF%SMSTR   ,NEL     )
c
      CALL S10DERI3(VOLP,NGL,
     .   XX, YY, ZZ, PX,PY,PZ, NX,
     .   RX, RY, RZ, SX, SY, SZ, TX, TY, TZ,VOLU,GBUF%VOL,
     .   ELBUF_STR,VOLG)
      CALL S10LEN3(VOLP,NGL,DELTAX,DELTAX2,
     .   PX,PY,PZ, VOLU,GBUF%VOL,VOLG,
     .   RX, RY, RZ, SX, SY, SZ, TX, TY, TZ,
     .   NEL,MAT,PM,GBUF%DT_PITER,IINT)
      CALL SREPLOC3(
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  )
      IF (IGTYP == 6 .OR. IGTYP == 21)
     .  CALL SMORTH3(PID  ,GEO  ,IGEO ,SKEW ,IREP ,GBUF%GAMA  ,
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,NSIGI,SIGSP,NSIGS,
     .         SIGI ,IXS  ,X    ,JHBE ,PTSOL,NEL  ,IPARG(28))
C----------------------------------------
C     INITIALISATION DE LA THERMIQUE 
C----------------------------------------
      IF(JTHE < 0) THEN
       DO I=LFT,LLT
         MASSCP(I) = ZERO
       ENDDO
      ENDIF
      IF(JTHE /=0) CALL ATHERI(MAT,PM,GBUF%TEMP)
C-----------------------------
C     POINTS D'INTEGRATION
C-----------------------------
      DO IP=1,NPT
        LBUF => ELBUF_STR%BUFLY(1)%LBUF(IP,1,1)
        MBUF => ELBUF_STR%BUFLY(1)%MAT(IP,1,1)
        L_PLA = ELBUF_STR%BUFLY(1)%L_PLA
        L_SIGB =ELBUF_STR%BUFLY(1)%L_SIGB
C
        IF(ISROT /= 1)THEN
          DO I=LFT,LLT
            VOLU(I)=VOLP(I,IP)
            LBUF%VOL(I)=VOLU(I)
          ENDDO
        ELSE
          DO I=LFT,LLT
            LBUF%VOL(I)=VOLU(I)
          ENDDO
        ENDIF
        IF(JTHE /=0) CALL ATHERI(MAT,PM,LBUF%TEMP)

        CALL MATINI(PM       ,IXS      ,NIXS       ,X        ,
     .              GEO      ,ALE_CONNECTIVITY    ,DETONATORS,IPARG     ,
     .              SIGI     ,NEL      ,SKEW       ,IGEO   ,
     .              IPART    ,IPARTS ,
     .              MAT      ,IPM      ,NSIGS    ,NUMSOL     ,PTSOL  ,
     .              IP       ,NGL      ,NPF      ,TF         ,BUFMAT ,
     .              GBUF     ,LBUF     ,MBUF     ,ELBUF_STR  ,ILOADP ,
     .              FACLOAD, DELTAX)
C------------------------------------------
C       CALCUL DES DT ELEMENTAIRES
C------------------------------------------
      AIRE(:) = ZERO
      CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .     LBUF%EINT ,LBUF%TEMP ,LBUF%DELTAX ,LBUF%RK ,LBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .     VOLU, DTX , IGEO)
C----------------------------------------
C     INITIALISATION DES MASSES
C----------------------------------------
        CALL S10MSI(LBUF%RHO,MASS,VOLU,DTELEM(NFT+1),STI,
     .   LBUF%OFF,LBUF%SIG ,LBUF%EINT ,
     .   GBUF%OFF,GBUF%SIG,GBUF%EINT,GBUF%RHO,WIP(NPT,IP),
     .   MASSCP  ,RHOCP   ,GBUF%FILL,NEL, DTX)
C---------------------------------------------------------
C  INITIALATION DES CONTRAINTES
C---------------------------------------------------------

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
             ELSEIF(MTN == 3.OR.MTN == 6.OR.MTN == 10.OR.
     .            MTN == 21.OR.MTN == 22.OR.MTN == 23.
     .             OR.MTN == 49)THEN
                 IDEF =1
             ENDIF
           ENDIF
C
         ENDIF
C
        CALL SIGIN20B(
     .          LBUF%SIG,PM, LBUF%VOL,SIGSP,
     .          SIGI,LBUF%EINT,LBUF%RHO,MBUF%VAR ,LBUF%STRA,
     .          IXS  ,NIXS,NSIGI, IP, NUVAR,
     .          NEL,IUSER,IDEF,NSIGS ,STRSGLOB,
     .          STRAGLOB,JHBE,IGTYP,X,LBUF%GAMA,
     .          MAT   ,LBUF%PLA,L_PLA,PTSOL,LBUF%SIGB,
     .          L_SIGB,IPM     ,BUFMAT     ,LBUF%VOL0DP)
C
C----------------------------------------
c Initialization of stress tensor in case of Orthotropic properties
C----------------------------------------
        IF (ISIGI /= 0 .AND. ISORTH/=0) THEN 
          LBUF%SIGL = LBUF%SIG
        ENDIF
C
      ENDDO
C
        IF (IBOLTP /=0) THEN
         CALL SBOLTINI(E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     1                 GBUF%BPRELD,NEL  ,IXS  ,NIXS ,VPRELOAD, IFLAG_BPRELOAD)
        ENDIF
C----------------------------------------
C     INITIALISATION DES MASSES
C----------------------------------------
      CALL S10MASS3(MASS,MAS,PARTSAV,IPARTS(NF1),MSS(1,NF1),VOLU,
     .              XX  ,YY ,ZZ     ,VX         ,VY         ,VZ  ,
     .              NC  ,STI,STIFN  ,DELTAX2    ,MSSX(1,NF1),MASSCP,
     .              MCP ,MCPS(1,NF1),MCPSX(1,NF1),IN        ,STIFR,
     .              INS(1,NF1),MSSA(NF1),X       ,GBUF%FILL )
C----------------------------------------
c Failure model initialisation
C----------------------------------------
      CALL FAILINI(ELBUF_STR,NPTR,NPTS,NPTT,NLAY,
     .             IPM,SIGSP,NSIGI,FAIL_INI ,
     .             SIGI,NSIGS,IXS,NIXS,PTSOL,
     .             RNOISE,PERTURB,BUFMAT)
C------------------------------------------
C     assemblage des Volumes nodaux et Modules nodaux
C     (pour rigidites d'interface)
C------------------------------------------
      IF(I7STIFS/=0)THEN
        NCC=10
        CALL SBULK3(VOLU  ,NC    ,NCC,MAT,PM ,
     2              VOLNOD,BVOLNOD,VNS(1,NF1),BNS(1,NF1),
     3              VNSX(1,NF1),BNSX(1,NF1)  ,GBUF%FILL)
      ENDIF
C------------------------------------------
      DO I=LFT,LLT
        IF(IXS(10,I+NFT)/=0) THEN
          IF(     IGTYP/=0 .AND.IGTYP/=6
     .       .AND.IGTYP/=14.AND.IGTYP/=15)THEN
             IPID=IXS(NIXS-1,I+NFT)
             ID=IGEO(1,IPID)
             CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
             CALL ANCMSG(MSGID=496,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR)
          ENDIF
        ENDIF
      ENDDO
C---------------------------------------------------
      RETURN
      END
