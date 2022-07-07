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
Chd|  SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ATHERI                        source/ale/atheri.F           
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|        FAILINI                       source/elements/solid/solide/failini.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        SBULK3                        source/elements/solid/solide/sbulk3.F
Chd|        SCCOOR3                       source/elements/thickshell/solidec/sccoor3.F
Chd|        SCDERI3                       source/elements/thickshell/solidec/scderi3.F
Chd|        SCMORTH3                      source/elements/thickshell/solidec/scmorth3.F
Chd|        SCZERO3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|        SDLENSH                       source/elements/thickshell/solidec/scinit3.F
Chd|        SIGIN20B                      source/elements/solid/solide20/s20mass3.F
Chd|        SMASS3                        source/elements/solid/solide/smass3.F
Chd|        SVALUE0                       source/elements/thickshell/solidec/scinit3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SCINIT3(ELBUF_STR,
     .                  MAS       ,IXS       ,PM       ,X         ,MSS   ,
     .                  DETONATORS,GEO       ,VEUL     ,ALE_CONNECTIVITY,IPARG ,
     .                  DTELEM    ,SIGI      ,NEL      ,SKEW      ,IGEO  ,
     .                  STIFN     ,PARTSAV   ,V        ,IPARTS    ,IPART ,
     .                  SIGSP     ,NSIGI     ,MSNF     ,MSSF      ,IPM   ,
     .                  IUSER     ,NSIGS     ,VOLNOD   ,BVOLNOD   ,VNS   ,
     .                  BNS       ,WMA       ,PTSOL    ,BUFMAT    ,MCP   ,
     .                  MCPS      ,TEMP      ,NPF      ,TF        ,MSSA  ,
     .                  STRSGLOB  ,STRAGLOB ,ORTHOGLOB ,FAIL_INI  ,ILOADP,
     .                  FACLOAD   ,RNOISE   ,PERTURB   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
      INTEGER NEL,NSIGI, IUSER, NSIGS
      INTEGER IXS(NIXS,*),IPARG(*),IPARTS(*),IPART(LIPART1,*),
     .   IPM(NPROPMI,*),PTSOL(*), NPF(*),IGEO(NPROPGI,*),
     .   STRSGLOB(*),STRAGLOB(*),ORTHOGLOB(*),FAIL_INI(*),PERTURB(NPERTURB)
      my_real
     .   MAS(*), PM(NPROPM,*), X(*), GEO(NPROPG,*),
     .   VEUL(LVEUL,*), DTELEM(*),SIGI(NSIGS,*),SKEW(LSKEW,*),STIFN(*),
     .   PARTSAV(20,*), V(*), MSS(8,*), 
     .   SIGSP(NSIGI, *),MSNF(*), MSSF(8,*), WMA(*),
     .   VOLNOD(*), BVOLNOD(*), VNS(8,*), BNS(8,*), BUFMAT(*),
     .   MCP(*),MCPS(8,*),TEMP(*), TF(*), MSSA(*),RNOISE(NPERTURB,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(DETONATOR_STRUCT_)::DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NF1, IBID, I, NLAY,IGTYP,NLYMAX,IS,NUVAR,IREP,NCC,JHBE,
     .        IDEF, IP, IPANG, IPTHK, IPPOS, IPMAT,IG,IM,MTN0,IPID1,
     .        NPTR,NPTS,NPTT,L_PLA,L_SIGB
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        IX5(MVSIZ), IX6(MVSIZ), IX7(MVSIZ), IX8(MVSIZ)
      INTEGER  MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ) , MAT0(MVSIZ)
      CHARACTER*nchartitle,
     .   TITR1
      my_real
     .   BID, FV, STI, ZI,WI
      my_real
     .   V8LOC(51,MVSIZ),VOLU(MVSIZ),DTX(MVSIZ),VZL(MVSIZ),VZQ(MVSIZ),
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),X5(MVSIZ),X6(MVSIZ),
     .   X7(MVSIZ),X8(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),
     .   Y5(MVSIZ),Y6(MVSIZ),Y7(MVSIZ),Y8(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),
     .   Z3(MVSIZ),Z4(MVSIZ),Z5(MVSIZ),Z6(MVSIZ),Z7(MVSIZ),Z8(MVSIZ),
     .   RX(MVSIZ) ,RY(MVSIZ) ,RZ(MVSIZ) ,SX(MVSIZ) ,
     .   SY(MVSIZ) ,SZ(MVSIZ) ,TX(MVSIZ) ,TY(MVSIZ) ,
     .   TZ(MVSIZ) ,E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),
     .   E2Y(MVSIZ),E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   F1X(MVSIZ) ,F1Y(MVSIZ) ,F1Z(MVSIZ) ,
     .   F2X(MVSIZ) ,F2Y(MVSIZ) ,F2Z(MVSIZ) ,GAMA(6,MVSIZ),
     .   RHOCP(MVSIZ) ,TEMP0(MVSIZ),ANGLE(MVSIZ),DTX0(MVSIZ),
     .   DELTAX(MVSIZ), AIRE(1),LLSH(MVSIZ)
      my_real, DIMENSION(8,MVSIZ) :: BID8MVSIZ
      my_real, DIMENSION(MVSIZ) :: BIDMVSIZ
C--------------------------------
C-----------------------------------------------
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
C-----------------------------------------------
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9)
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
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF   => ELBUF_STR%GBUF
      LBUF   => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
      MBUF   => ELBUF_STR%BUFLY(1)%MAT(1,1,1)
      BUFLY  => ELBUF_STR%BUFLY(1)
      NPTR   =  ELBUF_STR%NPTR
      NPTS   =  ELBUF_STR%NPTS
      NPTT   =  ELBUF_STR%NPTT
      NLAY   =  ELBUF_STR%NLAY
c
      IREP   = IPARG(35)
      IGTYP  = IPARG(38)
      JHBE   = IPARG(23)
      NF1=NFT+1
      IF (JCVT==1.AND.ISORTH/=0) JCVT=2
      IF (IGTYP /= 22) ISORTH = 0
      IBID   = 0
      IDEF   = 0
C
      DO I=LFT,LLT
        RHOCP(I) =  PM(69,IXS(1,NFT+I))
        TEMP0(I) =  PM(79,IXS(1,NFT+I))
      ENDDO
      CALL SCCOOR3(X ,IXS(1,NF1),GEO  ,MAT  ,PID  ,NGL  ,
     .         IX1  ,IX2  ,IX3  ,IX4  ,IX5  ,IX6  ,IX7  ,IX8 ,
     .         X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .         Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .         Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,TEMP0, TEMP)
      IF (IGTYP == 21 .OR. IGTYP == 22) THEN
        DO I=LFT,LLT
         ANGLE(I) = GEO(1,PID(I))
        END DO
        CALL SCMORTH3(PID  ,GEO  ,IGEO ,SKEW ,IREP ,GBUF%GAMA ,
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         NGL  ,ANGLE,NSIGI,SIGSP,NSIGS,SIGI ,IXS(1,NF1) ,1    ,
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
           MAT0(I) = MAT(I)    
           DTX0(I) = EP20
        ENDDO
       END IF
      END IF
      CALL SCDERI3(GBUF%VOL   ,VEUL(1,NF1),GEO  ,
     .             VZL  ,VZQ  ,NGL  ,PID  ,
     .             X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .             Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .             Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   , VOLU)
C
      CALL SDLEN3(X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .            Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .            Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   , 
     .            DELTAX, VOLU)
      IF (IDTTSH > 0) THEN
         CALL SDLENSH(VOLU,LLSH,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8)
        DO I=LFT,LLT
          IF (GBUF%IDT_TSH(I)>0) 
     .     DELTAX(I)=MAX(LLSH(I),DELTAX(I))
        ENDDO
      END IF        
C
      IP=0
      CALL MATINI(PM      ,IXS    ,NIXS       ,X      ,
     .            GEO     ,ALE_CONNECTIVITY  ,DETONATORS ,IPARG     ,
     .            SIGI    ,NEL    ,SKEW       ,IGEO   ,
     .            IPART   ,IPARTS ,
     .            MAT     ,IPM    ,NSIGS      ,NUMSOL    ,PTSOL  ,
     .            IP      ,NGL    ,NPF        ,TF        ,BUFMAT ,
     .            GBUF    ,LBUF   ,MBUF       ,ELBUF_STR ,ILOADP ,
     .            FACLOAD, DELTAX)
c
      IF (IGTYP == 22) CALL SCZERO3(GBUF%RHO,GBUF%SIG,GBUF%EINT,NEL)
C----------------------------------------
C     INITIALISATION DE LA THERMIQUE 
C----------------------------------------
      IF(JTHE /=0) CALL ATHERI(MAT,PM,GBUF%TEMP)
C------------------------
C     INTEGRATION POINTS
C------------------------
      DO IS=1,NLAY
c
        LBUF => ELBUF_STR%BUFLY(IS)%LBUF(1,1,1)
        MBUF => ELBUF_STR%BUFLY(IS)%MAT(1,1,1)
        L_PLA = ELBUF_STR%BUFLY(IS)%L_PLA
        L_SIGB= ELBUF_STR%BUFLY(IS)%L_SIGB
c
       IF (IGTYP == 22) THEN
        ZI = GEO(IPPOS+IS,IG)
        WI = GEO(IPTHK+IS,IG)
        IM=IGEO(IPMAT+IS,IG)
         MTN=NINT(PM(19,IM))
         DO I=LFT,LLT
          MAT(I)=IM
          ANGLE(I) = GEO(IPANG+IS,PID(I))
         ENDDO
       ELSE
          ZI = A_GAUSS(IS,NLAY)
          WI = W_GAUSS(IS,NLAY)
       ENDIF
        DO I=LFT,LLT
          LBUF%VOL0DP(I) = HALF*WI*(GBUF%VOL(I)+VZL(I)*ZI)
          LBUF%VOL(I) = LBUF%VOL0DP(I)
        ENDDO
        IF (IGTYP == 22)
     .    CALL SCMORTH3(PID  ,GEO  ,IGEO ,SKEW ,IREP ,LBUF%GAMA  ,
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         NGL  ,ANGLE,NSIGI,SIGSP,NSIGS,SIGI ,IXS(1,NF1) ,IS   ,
     .         ORTHOGLOB,PTSOL,NEL)
c
        CALL MATINI(PM     ,IXS    ,NIXS        ,X        ,
     .             GEO     ,ALE_CONNECTIVITY  ,DETONATORS  ,IPARG    ,
     .             SIGI    ,NEL    ,SKEW        ,IGEO     ,
     .             IPART   ,IPARTS,   
     .             MAT     ,IPM    ,NSIGS       ,NUMSOL   ,PTSOL,
     .             IS      ,NGL    ,NPF         ,TF       ,BUFMAT,
     .             GBUF    ,LBUF   ,MBUF        ,ELBUF_STR,ILOADP,
     .             FACLOAD, DELTAX)
c
         NUVAR = ELBUF_STR%BUFLY(IS)%NVAR_MAT
         IF(MTN.GE.28)THEN
           IDEF =1
         ELSE
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
     .           MTN == 21.OR.MTN == 22.OR.MTN == 23.
     .           OR.MTN == 49)THEN
             IDEF =1
            ENDIF
           ENDIF
          ENDIF
c
         CALL SIGIN20B(LBUF%SIG,
     .     PM, LBUF%VOL,SIGSP,SIGI,LBUF%EINT,LBUF%RHO,MBUF%VAR ,
     .     LBUF%STRA,IXS  ,NIXS,NSIGI, IS, NUVAR,NEL,IUSER,IDEF,
     .     NSIGS,STRSGLOB,STRAGLOB,JHBE,IGTYP,X,GBUF%GAMA,
     .     MAT  ,LBUF%PLA,L_PLA,PTSOL,LBUF%SIGB,L_SIGB,IPM      ,
     .     BUFMAT,LBUF%VOL0DP)
        IF(IGTYP == 22) THEN
C         moyene density,sig,...---
          AIRE(:) = ZERO
          CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .         LBUF%EINT ,LBUF%TEMP ,LBUF%DELTAX ,LBUF%RK ,LBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .         VOLU, DTX , IGEO )
c
          CALL SVALUE0(
     .         LBUF%RHO,LBUF%VOL,LBUF%OFF,LBUF%SIG,LBUF%EINT,DTX,        
     .         GBUF%RHO,GBUF%VOL,GBUF%OFF,GBUF%SIG,GBUF%EINT,DTX0,
     .         NEL     )
        ENDIF
      ENDDO    ! IS=1,NLAY
C----------------
      IF(IGTYP == 22) THEN
       MTN=MTN0
       DO I=LFT,LLT
         MAT(I)=MAT0(I)
       ENDDO
      ENDIF
C----------------------------------------
C     INITIALISATION DES MASSES
C----------------------------------------
      BID8MVSIZ(1:8,1:MVSIZ) = ZERO
      BIDMVSIZ(1:MVSIZ) = ZERO
      CALL SMASS3(
     .     GBUF%RHO   ,MAS       ,PARTSAV  ,X         ,V      ,
     .     IPARTS(NF1),MSS(1,NF1),VOLU     ,
     .     MSNF       ,MSSF(1,NF1),BID     ,
     .     BID        ,BID8MVSIZ  ,WMA      ,RHOCP    ,MCP     ,
     .     MCPS(1,NF1),MSSA      ,BIDMVSIZ  ,BIDMVSIZ ,GBUF%FILL,
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
      IF(I7STIFS.NE.0)THEN
        NCC=8
        CALL SBULK3(VOLU  ,IX1    ,NCC,MAT,PM ,
     2              VOLNOD,BVOLNOD,VNS(1,NF1),BNS(1,NF1),BID,
     3              BID   ,GBUF%FILL)
      ENDIF
C------------------------------------------
C       CALCUL DES DT ELEMENTAIRES
C------------------------------------------
       AIRE(:) = ZERO
       CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .      LBUF%EINT ,LBUF%TEMP ,LBUF%DELTAX ,LBUF%RK ,LBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .      VOLU, DTX , IGEO )
c
       IF(IGTYP == 22) THEN
        DO I=LFT,LLT
         DTX(I)=DTX0(I)
        ENDDO
       ENDIF
       DO 10 I=LFT,LLT
        IF (IXS(10,I+NFT) /= 0) THEN
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
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SVALUE0                       source/elements/thickshell/solidec/scinit3.F
Chd|-- called by -----------
Chd|        IG3DINIT3                     source/elements/ige3d/ig3dinit3.F
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SVALUE0(RHO ,VOL ,OFF ,SIG ,EINT ,DTX ,
     .                   RHOG,VOLG,OFFG,SIGG,EINTG,DTXG,
     .                   NEL )
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   RHO(*), VOL(*),SIG(NEL,6),EINT(*),OFF(*),DTX(*),
     .   SIGG(NEL,6),EINTG(*),RHOG(*),OFFG(*),VOLG(*),DTXG(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   FAC
C
      DO I=LFT,LLT
        FAC  = OFF(I)*VOL(I)/VOLG(I)
        SIGG(I,1) = SIGG(I,1) + FAC * SIG(I,1)
        SIGG(I,2) = SIGG(I,2) + FAC * SIG(I,2)
        SIGG(I,3) = SIGG(I,3) + FAC * SIG(I,3)
        SIGG(I,4) = SIGG(I,4) + FAC * SIG(I,4)
        SIGG(I,5) = SIGG(I,5) + FAC * SIG(I,5)
        SIGG(I,6) = SIGG(I,6) + FAC * SIG(I,6)
        RHOG(I)   = RHOG(I)   + FAC * RHO(I)
        EINTG(I)  = EINTG(I)  + FAC * EINT(I)
c        OFFG(I)   = OFF(I)
	DTXG(I)  = MIN(DTXG(I),DTX(I))
      ENDDO
C
      RETURN
      END
C.....
Chd|====================================================================
Chd|  SCZERO3                       source/elements/thickshell/solidec/scinit3.F
Chd|-- called by -----------
Chd|        IG3DINIT3                     source/elements/ige3d/ig3dinit3.F
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCZERO3(RHOG,SIGG,EINTG,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   SIGG(NEL,6),EINTG(*),RHOG(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      DO I=LFT,LLT
        SIGG(I,1) = ZERO
        SIGG(I,2) = ZERO
        SIGG(I,3) = ZERO
        SIGG(I,4) = ZERO
        SIGG(I,5) = ZERO
        SIGG(I,6) = ZERO
        RHOG(I)   = ZERO
        EINTG(I)  = ZERO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SDLENSH                       source/elements/thickshell/solidec/scinit3.F
Chd|-- called by -----------
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|-- calls ---------------
Chd|        CLSYS3                        source/elements/thickshell/solidec/scinit3.F
Chd|====================================================================
      SUBROUTINE SDLENSH(VOLN,LLSH,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8)
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
      my_real,DIMENSION(MVSIZ),INTENT(IN) :: 
     .   VOLN,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,  
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8
      my_real,DIMENSION(MVSIZ),INTENT(OUT) :: LLSH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N
      my_real
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),
     .   VQ(MVSIZ,3,3), LXYZ0(3),DETA1(MVSIZ),XX,YY,ZZ,
     .   XL2(MVSIZ),XL3(MVSIZ),XL4(MVSIZ),YL2(MVSIZ),
     .   YL3(MVSIZ),YL4(MVSIZ),ZL1(MVSIZ),AREA(MVSIZ),
     .   XN(MVSIZ,4) , YN(MVSIZ,4) , ZN(MVSIZ,4) 
      my_real
     .   AL1,AL2,LL(MVSIZ),COREL(2,4)
      my_real
     .   X13,X24,Y13,Y24,L13,L24,C1,C2,THKLY,POSLY,
     .   FAC,VISCE,RX1,RY1,SX1,SY1,S1,FAC1,FAC2,FACI,FAC11,FACDT
C=======================================================================
        DO I=LFT,LLT
          XN(I,1) = HALF*(X1(I)+X5(I))
          YN(I,1) = HALF*(Y1(I)+Y5(I))
          ZN(I,1) = HALF*(Z1(I)+Z5(I))
          XN(I,2) = HALF*(X2(I)+X6(I))
          YN(I,2) = HALF*(Y2(I)+Y6(I))
          ZN(I,2) = HALF*(Z2(I)+Z6(I))
          XN(I,3) = HALF*(X3(I)+X7(I))
          YN(I,3) = HALF*(Y3(I)+Y7(I))
          ZN(I,3) = HALF*(Z3(I)+Z7(I))
          XN(I,4) = HALF*(X4(I)+X8(I))
          YN(I,4) = HALF*(Y4(I)+Y8(I))
          ZN(I,4) = HALF*(Z4(I)+Z8(I))
        ENDDO 
C------g1,g2 :
        DO I=LFT,LLT
          RX(I)=XN(I,2)+XN(I,3)-XN(I,1)-XN(I,4)
          RY(I)=YN(I,2)+YN(I,3)-YN(I,1)-YN(I,4)
          RZ(I)=ZN(I,2)+ZN(I,3)-ZN(I,1)-ZN(I,4)
          SX(I)=XN(I,3)+XN(I,4)-XN(I,1)-XN(I,2)
          SY(I)=YN(I,3)+YN(I,4)-YN(I,1)-YN(I,2)
          SZ(I)=ZN(I,3)+ZN(I,4)-ZN(I,1)-ZN(I,2)
        ENDDO 
C------Local elem. base:
        CALL CLSYS3(RX, RY, RZ, SX, SY, SZ, 
     .              VQ, DETA1,LLT)
C------ Global -> Local Coordinate  FOURTH=0.25 ;
        DO I=LFT,LLT
          LXYZ0(1)=FOURTH*(XN(I,1)+XN(I,2)+XN(I,3)+XN(I,4))
          LXYZ0(2)=FOURTH*(YN(I,1)+YN(I,2)+YN(I,3)+YN(I,4))
          LXYZ0(3)=FOURTH*(ZN(I,1)+ZN(I,2)+ZN(I,3)+ZN(I,4))
          XX=XN(I,2)-XN(I,1)
          YY=YN(I,2)-YN(I,1)
          ZZ=ZN(I,2)-ZN(I,1)
          XL2(I)=VQ(I,1,1)*XX+VQ(I,2,1)*YY+VQ(I,3,1)*ZZ
          YL2(I)=VQ(I,1,2)*XX+VQ(I,2,2)*YY+VQ(I,3,2)*ZZ
          XX=XN(I,2)-LXYZ0(1)
          YY=YN(I,2)-LXYZ0(2)
          ZZ=ZN(I,2)-LXYZ0(3)
          ZL1(I)=VQ(I,1,3)*XX+VQ(I,2,3)*YY+VQ(I,3,3)*ZZ
C          
          XX=XN(I,3)-XN(I,1)
          YY=YN(I,3)-YN(I,1)
          ZZ=ZN(I,3)-ZN(I,1)
          XL3(I)=VQ(I,1,1)*XX+VQ(I,2,1)*YY+VQ(I,3,1)*ZZ
          YL3(I)=VQ(I,1,2)*XX+VQ(I,2,2)*YY+VQ(I,3,2)*ZZ
C
          XX=XN(I,4)-XN(I,1)
          YY=YN(I,4)-YN(I,1)
          ZZ=ZN(I,4)-ZN(I,1)
          XL4(I)=VQ(I,1,1)*XX+VQ(I,2,1)*YY+VQ(I,3,1)*ZZ
          YL4(I)=VQ(I,1,2)*XX+VQ(I,2,2)*YY+VQ(I,3,2)*ZZ
          AREA(I)=FOURTH*DETA1(I)
        ENDDO 
      FAC = TWO
      FACDT = FIVE_OVER_4
C-------same than QBAT       
      IF (IDT1SOL.GT.0) FACDT =FOUR_OVER_3
C---- compute COREL(2,4) mean surface and area     
      DO I=LFT,LLT
        LXYZ0(1)=FOURTH*(XL2(I)+XL3(I)+XL4(I))
        LXYZ0(2)=FOURTH*(YL2(I)+YL3(I)+YL4(I))
        COREL(1,1)=-LXYZ0(1)
        COREL(1,2)=XL2(I)-LXYZ0(1)
        COREL(1,3)=XL3(I)-LXYZ0(1)
        COREL(1,4)=XL4(I)-LXYZ0(1)
        COREL(2,1)=-LXYZ0(2)
        COREL(2,2)=YL2(I)-LXYZ0(2)
        COREL(2,3)=YL3(I)-LXYZ0(2)
        COREL(2,4)=YL4(I)-LXYZ0(2)
        X13=(COREL(1,1)-COREL(1,3))*HALF
        X24=(COREL(1,2)-COREL(1,4))*HALF
        Y13=(COREL(2,1)-COREL(2,3))*HALF
        Y24=(COREL(2,2)-COREL(2,4))*HALF
C
        L13=X13*X13+Y13*Y13
        L24=X24*X24+Y24*Y24
        AL1=MAX(L13,L24)
        C1 =COREL(1,2)*COREL(2,4)-COREL(2,2)*COREL(1,4)
        C2 =COREL(1,1)*COREL(2,3)-COREL(2,1)*COREL(1,3)
        AL2 =MAX(ABS(C1),ABS(C2))/AREA(I)
        RX1=X24-X13
        RY1=Y24-Y13
        SX1=-X24-X13
        SY1=-Y24-Y13
        C1=SQRT(RX1*RX1+RY1*RY1)
        C2=SQRT(SX1*SX1+SY1*SY1)
        S1=FOURTH*(MAX(C1,C2)/MIN(C1,C2)-ONE)
        FAC1=MIN(HALF,S1)+ONE
        FAC2=AREA(I)/(C1*C2)
        FAC2=3.413*MAX(ZERO,FAC2-0.7071)
        FAC2=0.78+0.22*FAC2*FAC2*FAC2
        FACI=TWO*FAC1*FAC2
        S1 = SQRT(FACI*(FACDT+AL2)*AL1)
        S1 = MAX(S1,EM20)
        LLSH(I) = AREA(I)/S1
      ENDDO
C
      RETURN
      END SUBROUTINE SDLENSH
Chd|====================================================================
Chd|  CLSYS3                        source/elements/thickshell/solidec/scinit3.F
Chd|-- called by -----------
Chd|        SDLENSH                       source/elements/thickshell/solidec/scinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CLSYS3(RX, RY, RZ, SX, SY, SZ, VQ, DET,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real,DIMENSION(NEL),INTENT(IN) :: 
     .   RX , RY , RZ, 
     .   SX , SY , SZ
      my_real,DIMENSION(NEL),INTENT(OUT) :: DET
      my_real,DIMENSION(NEL,3,3),INTENT(OUT) :: VQ
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C RX      |    NEL  | R | R | X-of covariant vecter g1
C RY      |    NEL  | R | R | Y-of covariant vecter g1
C RZ      |    NEL  | R | R | Z-of covariant vecter g1
C SX      |    NEL  | R | R | X-of covariant vecter g2
C SY      |    NEL  | R | R | Y-of covariant vecter g2
C SZ      |    NEL  | R | R | Z-of covariant vecter g2
C VQ      |3*3*NEL  | R | W | Local elem sys bases
C DET     |    NEL  | R | W | det of g1 ^ g2
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   E1X(NEL), E1Y(NEL), E1Z(NEL),
     .   E2X(NEL), E2Y(NEL), E2Z(NEL),
     .   E3X(NEL), E3Y(NEL), E3Z(NEL), 
     .   C1,C2,CC,C1C1,C2C2,C1_1,C2_1
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO I=1,NEL
C---------E3------------
       E3X(I) = RY(I) * SZ(I) - RZ(I) * SY(I) 
       E3Y(I) = RZ(I) * SX(I) - RX(I) * SZ(I) 
       E3Z(I) = RX(I) * SY(I) - RY(I) * SX(I) 
       DET(I) = SQRT(E3X(I)*E3X(I) + E3Y(I)*E3Y(I) + E3Z(I)*E3Z(I))
C ----- EM20=1.0E-20      
       DET(I) = MAX(EM20,DET(I))
       E3X(I) = E3X(I) / DET(I) 
       E3Y(I) = E3Y(I) / DET(I) 
       E3Z(I) = E3Z(I) / DET(I) 
      ENDDO 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       DO I=1,NEL
        C1C1 = RX(I)*RX(I) + RY(I)*RY(I) + RZ(I)*RZ(I)
        C2C2 = SX(I)*SX(I) + SY(I)*SY(I) + SZ(I)*SZ(I)
C ----- ZERO=0., ONE=1.0    
        IF(C1C1 /= ZERO) THEN
          C2_1 = SQRT(C2C2/MAX(EM20,C1C1))
          C1_1 = ONE
        ELSEIF(C2C2 /= ZERO)THEN
          C2_1 = ONE
          C1_1 = SQRT(C1C1/MAX(EM20,C2C2))
        END IF 
        E1X(I) = RX(I)*C2_1+(SY(I)*E3Z(I)-SZ(I)*E3Y(I))*C1_1
        E1Y(I) = RY(I)*C2_1+(SZ(I)*E3X(I)-SX(I)*E3Z(I))*C1_1 
        E1Z(I) = RZ(I)*C2_1+(SX(I)*E3Y(I)-SY(I)*E3X(I))*C1_1
       ENDDO 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       DO I=1,NEL
        C1 = SQRT(E1X(I)*E1X(I) + E1Y(I)*E1Y(I) + E1Z(I)*E1Z(I))
        IF ( C1 /= ZERO) C1 = ONE / MAX(EM20,C1)
        E1X(I) = E1X(I)*C1
        E1Y(I) = E1Y(I)*C1
        E1Z(I) = E1Z(I)*C1
        E2X(I) = E3Y(I) * E1Z(I) - E3Z(I) * E1Y(I)
        E2Y(I) = E3Z(I) * E1X(I) - E3X(I) * E1Z(I)
        E2Z(I) = E3X(I) * E1Y(I) - E3Y(I) * E1X(I)
       ENDDO 
      DO I=1,NEL
        VQ(I,1,1)=E1X(I)
        VQ(I,2,1)=E1Y(I)
        VQ(I,3,1)=E1Z(I)
        VQ(I,1,2)=E2X(I)
        VQ(I,2,2)=E2Y(I)
        VQ(I,3,2)=E2Z(I)
        VQ(I,1,3)=E3X(I)
        VQ(I,2,3)=E3Y(I)
        VQ(I,3,3)=E3Z(I)
      ENDDO 
c-----------
      RETURN
      END
