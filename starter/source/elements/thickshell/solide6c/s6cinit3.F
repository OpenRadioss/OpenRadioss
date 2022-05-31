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
Chd|  S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ATHERI                        source/ale/atheri.F           
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|        FAILINI                       source/elements/solid/solide/failini.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        S6CCOOR3                      source/elements/thickshell/solide6c/s6ccoor3.F
Chd|        S6CDERI3                      source/elements/thickshell/solide6c/s6cderi3.F
Chd|        S6MASS3                       source/elements/thickshell/solide6c/s6mass3.F
Chd|        SBULK3                        source/elements/solid/solide/sbulk3.F
Chd|        SCMORTH3                      source/elements/thickshell/solidec/scmorth3.F
Chd|        SCZERO3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SIGIN20B                      source/elements/solid/solide20/s20mass3.F
Chd|        SVALUE0                       source/elements/thickshell/solidec/scinit3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S6CINIT3(
     .                    ELBUF_STR,MAS     ,IXS     ,PM       ,X     ,
     .                    DETONATORS,GEO     ,VEUL    ,ALE_CONNECTIVITY,IPARG ,
     .                    DTELEM   ,SIGI    ,NEL     ,SKEW     ,IGEO  ,
     .                    STIFN    ,PARTSAV ,V       ,IPARTS   ,MSS   ,
     .                    IPART    ,
     .                    SIGSP    ,NSIGI   ,IPM     ,IUSER    ,NSIGS ,
     .                    VOLNOD   ,BVOLNOD ,VNS     ,BNS      ,PTSOL ,
     .                    BUFMAT   ,MCP     ,MCPS    ,MCPSX    ,TEMP  ,
     .                    NPF      ,TF      ,STRSGLOB,STRAGLOB ,MSSA  ,
     .                    ORTHOGLOB,FAIL_INI,ILOADP  ,FACLOAD  ,RNOISE,
     .                    PERTURB  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
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
#include      "vect01_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(*),IPARTS(*), 
     .    NEL, IPART(LIPART1,*),PERTURB(NPERTURB), 
     .    IPM(NPROPMI,*), PTSOL(*), NSIGI, IUSER, NSIGS, NPF(*)
      INTEGER IGEO(NPROPGI,*),STRSGLOB(*),STRAGLOB(*),ORTHOGLOB(*),
     .    FAIL_INI(*)
      my_real
     .   MAS(*), PM(NPROPM,*), X(*), GEO(NPROPG,*),
     .   VEUL(LVEUL,*), DTELEM(*),SIGI(NSIGS,*),SKEW(LSKEW,*),STIFN(*),
     .   PARTSAV(20,*), V(*), MSS(8,*),SIGSP(NSIGI,*),
     .    VOLNOD(*), BVOLNOD(*), VNS(8,*), BNS(8,*),BUFMAT(*),MCP(*),
     .    MCPS(8,*), MCPSX(12,*),TEMP(*), TF(*), MSSA(*),RNOISE(NPERTURB,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(DETONATOR_STRUCT_) :: DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NF1,IBID,IGTYP,IREP,IP,ILAY,NLAY,NUVAR,NCC,JHBE,
     .   NUVARR,IDEF,IPANG,IPTHK,IPPOS,IPMAT,IG,IM,MTN0,NLYMAX,
     .   IPID1,NPTR,NPTS,NPTT,L_PLA,L_SIGB
      INTEGER MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ), MAT0(MVSIZ)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        IX5(MVSIZ), IX6(MVSIZ)
      my_real
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), X5(MVSIZ), X6(MVSIZ), 
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Y5(MVSIZ), Y6(MVSIZ), 
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), Z5(MVSIZ), Z6(MVSIZ)
      CHARACTER*nchartitle,
     .   TITR1
      my_real
     .   BID, FV, STI, ZI,WI
      my_real
     .   V8LOC(51,MVSIZ),VOLU(MVSIZ),DTX(MVSIZ),VZL(MVSIZ),VZQ(MVSIZ),
     .   RX(MVSIZ) ,RY(MVSIZ) ,RZ(MVSIZ) ,SX(MVSIZ) ,
     .   SY(MVSIZ) ,SZ(MVSIZ) ,TX(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ) ,
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),
     .   E2X(MVSIZ),E2Y(MVSIZ),E2Z(MVSIZ),
     .   E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   F1X(MVSIZ) ,F1Y(MVSIZ) ,F1Z(MVSIZ) ,LLSH(MVSIZ) ,
     .   F2X(MVSIZ) ,F2Y(MVSIZ) ,F2Z(MVSIZ) ,RHOCP(MVSIZ),TEMP0(MVSIZ), DELTAX(MVSIZ), AIRE(1)
C-----------------------------------------------
C-----------------------------------------------
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
C-----------------------------------------------
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9),ANGLE(MVSIZ),DTX0(MVSIZ)
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
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      LBUF  => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
      MBUF  => ELBUF_STR%BUFLY(1)%MAT(1,1,1)
      BUFLY => ELBUF_STR%BUFLY(1)
      NPTR  =  ELBUF_STR%NPTR
      NPTS  =  ELBUF_STR%NPTS
      NPTT  =  ELBUF_STR%NPTT
      NLAY  =  ELBUF_STR%NLAY
c
      JHBE  = IPARG(23)
      IREP  = IPARG(35)
      IGTYP = IPARG(38)
      NF1=NFT+1
      IDEF =0
      IBID = 0
      BID   = ZERO
      IF (IGTYP /= 22) THEN
       ISORTH = 0
      END IF
C
      DO I=LFT,LLT
        RHOCP(I) =  PM(69,IXS(1,NFT+I))
        TEMP0(I) =  PM(79,IXS(1,NFT+I))
      ENDDO
C
      CALL S6CCOOR3(X     ,IXS(1,NF1)   ,GEO  ,NGL  ,MAT  ,PID  ,
     .     RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .     E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .     F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,TEMP0, TEMP, 
     .     IX1, IX2, IX3, IX4, IX5, IX6, 
     .     X1, X2, X3, X4, X5, X6, 
     .     Y1, Y2, Y3, Y4, Y5, Y6,
     .     Z1, Z2, Z3, Z4, Z5, Z6)
      IF (IGTYP == 21 .OR. IGTYP == 22) THEN
        DO I=LFT,LLT
         ANGLE(I) =  GEO(1,PID(I))
        END DO
        CALL SCMORTH3(PID  ,GEO  ,IGEO ,SKEW ,IREP ,GBUF%GAMA   ,
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         NGL  ,ANGLE,NSIGI,SIGSP,NSIGS,SIGI ,IXS  ,1    ,
     .         ORTHOGLOB,PTSOL,NEL)
       IF (IGTYP == 22) THEN
        NLYMAX= 200
        IPANG = 200
        IPTHK = IPANG+NLYMAX
        IPPOS = IPTHK+NLYMAX
        IPMAT = 100
        IG=PID(LFT)
        MTN0=MTN
        DO I=LFT,LLT
         MAT0(I)=MAT(I)
         DTX0(I) = EP20
        ENDDO
       END IF
      END IF
      CALL S6CDERI3(GBUF%VOL,GEO   ,VZL   ,NGL, DELTAX, VOLU, 
     .     X1, X2, X3, X4, X5, X6, 
     .     Y1, Y2, Y3, Y4, Y5, Y6,
     .     Z1, Z2, Z3, Z4, Z5, Z6)
      IF (IDTTSH > 0) THEN
         CALL SDLENSH3N(LLT,GBUF%VOL,LLSH,
     .                  X1, X2, X3, X4, X5, X6,
     .                  Y1, Y2, Y3, Y4, Y5, Y6,
     .                  Z1, Z2, Z3, Z4, Z5, Z6)
        DO I=LFT,LLT
          IF (GBUF%IDT_TSH(I)>0) 
     .     DELTAX(I)=MAX(LLSH(I),DELTAX(I))
        ENDDO
      END IF        
C
      IP=0
      CALL MATINI(PM      ,IXS    ,NIXS       ,X      ,
     .            GEO     ,ALE_CONNECTIVITY  ,DETONATORS ,IPARG  ,
     .            SIGI    ,NEL    ,SKEW       ,IGEO   ,
     .            IPART   ,IPARTS ,
     .            MAT     ,IPM    ,NSIGS  ,NUMSOL     ,PTSOL  ,
     .            IP      ,NGL    ,NPF    ,TF         ,BUFMAT , 
     .            GBUF    ,LBUF   ,MBUF   ,ELBUF_STR  ,ILOADP ,
     .            FACLOAD, DELTAX)                         
C
      IF (IGTYP == 22) CALL SCZERO3(GBUF%RHO,GBUF%SIG,GBUF%EINT,NEL)
C----------------------------------------
C     INITIALISATION DE LA THERMIQUE 
C----------------------------------------
      IF(JTHE /=0) CALL ATHERI(MAT,PM,GBUF%TEMP)
C-----------------------------
C     POINTS D' INTEGRATION
C-----------------------------
      DO ILAY=1,NLAY
        LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
        MBUF => ELBUF_STR%BUFLY(ILAY)%MAT(1,1,1)
        L_PLA = ELBUF_STR%BUFLY(ILAY)%L_PLA
        L_SIGB= ELBUF_STR%BUFLY(ILAY)%L_SIGB
c
       IF (IGTYP == 22) THEN
          ZI = GEO(IPPOS+ILAY,IG)
          WI = GEO(IPTHK+ILAY,IG)
          IM=IGEO(IPMAT+ILAY,IG)
         MTN=NINT(PM(19,IM))
         DO I=LFT,LLT
          MAT(I)=IM
            ANGLE(I) = GEO(IPANG+ILAY,PID(I))
         ENDDO
       ELSE
          ZI = A_GAUSS(ILAY,NLAY)
          WI = W_GAUSS(ILAY,NLAY)
       ENDIF
c
        DO I=LFT,LLT
          LBUF%VOL0DP(I)= HALF*WI*(GBUF%VOL(I)+VZL(I)*ZI)
          LBUF%VOL(I)= LBUF%VOL0DP(I)
        ENDDO
        IF (IGTYP == 22)
     .  CALL SCMORTH3(PID  ,GEO  ,IGEO ,SKEW ,IREP ,LBUF%GAMA   ,
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         NGL  ,ANGLE,NSIGI,SIGSP,NSIGS,SIGI ,IXS,ILAY,
     .         ORTHOGLOB,PTSOL,NEL)
        CALL MATINI(PM      ,IXS    ,NIXS   ,X         ,
     .              GEO     ,ALE_CONNECTIVITY  ,DETONATORS,IPARG  ,
     .              SIGI    ,NEL    ,SKEW      ,IGEO   ,
     .              IPART   ,IPARTS ,
     .              MAT     ,IPM    ,NSIGS  ,NUMSOL    ,PTSOL  ,
     .              ILAY    ,NGL    ,NPF    ,TF        ,BUFMAT ,
     .              GBUF    ,LBUF   ,MBUF   ,ELBUF_STR ,ILOADP ,
     .              FACLOAD, DELTAX)
        IF (MTN >= 28) THEN
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
            ELSEIF(MTN == 3.OR.MTN == 6.OR.MTN == 10
     .       .OR.MTN == 21.OR.MTN == 22.OR.MTN == 23.OR.MTN == 49)THEN
               IDEF =1
             ENDIF
            ENDIF
        ENDIF
        CALL SIGIN20B(
     .     LBUF%SIG,PM      ,LBUF%VOL ,SIGSP    ,
     .     SIGI    ,LBUF%EINT,LBUF%RHO,MBUF%VAR ,LBUF%STRA,
     .     IXS     ,NIXS     ,NSIGI   ,ILAY     ,NUVAR    , 
     .     NEL     ,IUSER    ,IDEF    ,NSIGS    ,STRSGLOB ,
     .     STRAGLOB,JHBE     ,IGTYP   ,X        ,LBUF%GAMA,
     .     MAT     ,LBUF%PLA ,L_PLA   ,PTSOL    ,LBUF%SIGB,
     .     L_SIGB  ,IPM      ,BUFMAT  ,LBUF%VOL0DP)
c
        IF(IGTYP == 22) THEN
C         moyene density,sig,...---
          AIRE(:) = ZERO
          CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .         LBUF%EINT ,LBUF%TEMP ,LBUF%DELTAX ,LBUF%RK ,LBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .         VOLU, DTX , IGEO)
c
          CALL SVALUE0(
     .         LBUF%RHO,LBUF%VOL,LBUF%OFF,LBUF%SIG,LBUF%EINT,DTX,
     .         GBUF%RHO,GBUF%VOL,GBUF%OFF,GBUF%SIG,GBUF%EINT,DTX0,
     .         NEL     )
        ENDIF
      ENDDO  ! ILAY = 1,NLAY
C----------------------------------------
      IF(IGTYP == 22) THEN
       MTN=MTN0
       DO I=LFT,LLT
         MAT(I)=MAT0(I)
       ENDDO
      ENDIF
C----------------------------------------
C     INITIALISATION DES MASSES
C----------------------------------------
      CALL S6MASS3(GBUF%RHO,MAS,PARTSAV,X,V,IPARTS(NF1),MSS(1,NF1),
     .     RHOCP,MCP ,MCPS(1,NF1),MSSA(NF1),GBUF%FILL, VOLU, 
     .     IX1, IX2, IX3, IX4, IX6, IX6)
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
C     attention : IX1, IX2 ... IX6 sont sous la forme NC(MVSIZ,6)
      IF(I7STIFS.NE.0)THEN
        NCC=6
        CALL SBULK3(VOLU  ,IX1    ,NCC,MAT,PM ,
     2              VOLNOD,BVOLNOD,VNS(1,NF1),BNS(1,NF1),BID,
     3              BID ,GBUF%FILL)
      ENDIF
C------------------------------------------
C      CALCUL DES DT ELEMENTAIRES
C------------------------------------------
        AIRE(:) = ZERO
        CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .       LBUF%EINT ,LBUF%TEMP ,LBUF%DELTAX ,LBUF%RK ,LBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .       VOLU, DTX, IGEO)
C------------------------------------------
       IF(IGTYP == 22) THEN
        DO I=LFT,LLT
         DTX(I)=DTX0(I)
        ENDDO
       ENDIF
c
      DO I=LFT,LLT
        IF(IXS(10,I+NFT) /= 0) THEN
          IF (IGTYP < 20 .OR. IGTYP > 22) THEN
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
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SDLENSH3N                     source/elements/thickshell/solidec/scdtdc3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SDLENSH3N(NEL,VOLN,LLSH3N,
     .                     X1, X2, X3, X4, X5, X6,
     .                     Y1, Y2, Y3, Y4, Y5, Y6,
     .                     Z1, Z2, Z3, Z4, Z5, Z6)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real,DIMENSION(NEL),INTENT(IN) :: VOLN
      my_real,DIMENSION(MVSIZ),INTENT(OUT) :: LLSH3N
      my_real,DIMENSION(MVSIZ),INTENT(IN) :: 
     .   X1, X2, X3, X4, X5, X6,
     .   Y1, Y2, Y3, Y4, Y5, Y6,  
     .   Z1, Z2, Z3, Z4, Z5, Z6
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N
      my_real
     .        E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .        E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .        E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .        X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ),
     .        X32(MVSIZ), Y32(MVSIZ), Z32(MVSIZ),
     .        X21(MVSIZ), Y21(MVSIZ), Z21(MVSIZ), AREA(MVSIZ),
     .        X2L(MVSIZ), X3L(MVSIZ), Y3L(MVSIZ),
     .        XN(MVSIZ,3) , YN(MVSIZ,3) , ZN(MVSIZ,3) 
      my_real
     .   AL1,AL2,AL3,ALMAX,SUM
C=======================================================================
       DO I=LFT,LLT
          XN(I,1) = HALF*(X1(I)+X4(I))
          YN(I,1) = HALF*(Y1(I)+Y4(I))
          ZN(I,1) = HALF*(Z1(I)+Z4(I))
          XN(I,2) = HALF*(X2(I)+X5(I))
          YN(I,2) = HALF*(Y2(I)+Y5(I))
          ZN(I,2) = HALF*(Z2(I)+Z5(I))
          XN(I,3) = HALF*(X3(I)+X6(I))
          YN(I,3) = HALF*(Y3(I)+Y6(I))
          ZN(I,3) = HALF*(Z3(I)+Z6(I))
       ENDDO 
      DO I=LFT,LLT
        X21(I)=XN(I,2)-XN(I,1)
        Y21(I)=YN(I,2)-YN(I,1)
        Z21(I)=ZN(I,2)-ZN(I,1)
        X31(I)=XN(I,3)-XN(I,1)
        Y31(I)=YN(I,3)-YN(I,1)
        Z31(I)=ZN(I,3)-ZN(I,1)
        X32(I)=XN(I,3)-XN(I,2)
        Y32(I)=YN(I,3)-YN(I,2)
        Z32(I)=ZN(I,3)-ZN(I,2)
      ENDDO
C
      DO I=LFT,LLT
        E1X(I)= X21(I)
        E1Y(I)= Y21(I)
        E1Z(I)= Z21(I)
        X2L(I) = SQRT(E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I))
        E1X(I)=E1X(I)/X2L(I)
        E1Y(I)=E1Y(I)/X2L(I)
        E1Z(I)=E1Z(I)/X2L(I)
      ENDDO
C
      DO I=LFT,LLT
        E3X(I)=Y31(I)*Z32(I)-Z31(I)*Y32(I)
        E3Y(I)=Z31(I)*X32(I)-X31(I)*Z32(I)
        E3Z(I)=X31(I)*Y32(I)-Y31(I)*X32(I)
        SUM = SQRT(E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I))
        E3X(I)=E3X(I)/SUM
        E3Y(I)=E3Y(I)/SUM
        E3Z(I)=E3Z(I)/SUM
        AREA(I) = HALF * SUM
      ENDDO
C
      DO I=LFT,LLT
        E2X(I)=E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)
        E2Y(I)=E3Z(I)*E1X(I)-E3X(I)*E1Z(I)
        E2Z(I)=E3X(I)*E1Y(I)-E3Y(I)*E1X(I)
        SUM = SQRT(E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I))
        E2X(I)=E2X(I)/SUM
        E2Y(I)=E2Y(I)/SUM
        E2Z(I)=E2Z(I)/SUM
        Y3L(I)=E2X(I)*X31(I)+E2Y(I)*Y31(I)+E2Z(I)*Z31(I)
        X3L(I)=E1X(I)*X31(I)+E1Y(I)*Y31(I)+E1Z(I)*Z31(I)
      ENDDO
      DO I=LFT,LLT
        AL1 = X2L(I) * X2L(I)      
        AL2 = (X3L(I)-X2L(I)) * (X3L(I)-X2L(I)) + Y3L(I) * Y3L(I)
        AL3 = X3L(I) * X3L(I) + Y3L(I) * Y3L(I)
        ALMAX = MAX(AL1,AL2,AL3)
        LLSH3N(I)= TWO*AREA(I) / SQRT(ALMAX)
      ENDDO
C
      RETURN
      END SUBROUTINE SDLENSH3N
