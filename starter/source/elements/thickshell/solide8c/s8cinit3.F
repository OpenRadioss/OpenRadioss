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
Chd|  S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ATHERI                        source/ale/atheri.F           
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|        FAILINI                       source/elements/solid/solide/failini.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        S8ZDERI3                      source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZDERIC3                     source/elements/solid/solide8z/s8zderi3.F
Chd|        SBULK3                        source/elements/solid/solide/sbulk3.F
Chd|        SCMORTH3                      source/elements/thickshell/solidec/scmorth3.F
Chd|        SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|        SCZERO3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SDLENSH                       source/elements/thickshell/solidec/scinit3.F
Chd|        SIGIN20B                      source/elements/solid/solide20/s20mass3.F
Chd|        SMASS3                        source/elements/solid/solide/smass3.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|        SVALUE0                       source/elements/thickshell/solidec/scinit3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8CINIT3(ELBUF_STR,MAS     ,IXS     ,PM       ,X        ,
     .                    DETONATORS,GEO     ,VEUL    ,ALE_CONNECTIVITY    ,IPARG    ,
     .                    DTELEM   ,SIGI    ,NEL     ,SKEW     ,IGEO     ,
     .                    STIFN    ,PARTSAV ,V       ,IPARTS   ,MSS      ,
     .                    IPART    ,
     .                    SIGSP    ,NSIGI   ,MSNF    ,MSSF     ,IPM      ,
     .                    IUSER    ,NSIGS   ,VOLNOD  ,BVOLNOD  ,VNS      ,
     .		                  BNS      ,WMA     ,PTSOL   ,BUFMAT   ,MCP      ,
     .                    MCPS     ,TEMP    ,NPF     ,TF       ,XREFS    ,
     .                    MSSA     ,STRSGLOB,STRAGLOB,ORTHOGLOB,FAIL_INI ,
     .                    ILOADP   ,FACLOAD ,RNOISE  ,PERTURB  )
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
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(*),IPARTS(*),
     .    NEL, IPART(LIPART1,*),PERTURB(NPERTURB),
     .    IPM(NPROPMI,*), PTSOL(*), NSIGI, IUSER, NSIGS, NPF(*)
      INTEGER IGEO(NPROPGI,*),STRSGLOB(*),STRAGLOB(*),ORTHOGLOB(*),
     .        FAIL_INI(*)
      my_real
     .   MAS(*), PM(NPROPM,*), X(*), GEO(NPROPG,*),
     .   VEUL(LVEUL,*), DTELEM(*),SIGI(NSIGS,*),SKEW(LSKEW,*),STIFN(*),
     .   PARTSAV(20,*), V(*), MSS(8,*),
     .  SIGSP(NSIGI,*),MSNF(*), MSSF(8,*), WMA(*),RNOISE(NPERTURB,*),
     .  VOLNOD(*), BVOLNOD(*), VNS(8,*), BNS(8,*),BUFMAT(*),
     .  MCP(*), MCPS(8,*),TEMP(*), TF(*),XREFS(8,3,*), MSSA(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(DETONATOR_STRUCT_)::DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NF1,IBID,IGTYP,IP,IR,IS,IT,IL,NLAY,NPTR,NPTS,NPTT,NCC,
     .     JHBE,IREP,MPT,NUVAR,NUVARR,IDEF,NREFSTA,
     .     IPTHK, IPPOS,IG,IM,MTN0,ICSTR,IPID1,L_PLA,L_SIGB
      INTEGER PID(MVSIZ), NGL(MVSIZ),MAT(MVSIZ), MAT0(MVSIZ),
     .     IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .     IX5(MVSIZ), IX6(MVSIZ), IX7(MVSIZ), IX8(MVSIZ)
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
     .   RHOCP(MVSIZ),TEMP0(MVSIZ), AIRE(1),LLSH(MVSIZ)
      my_real
     .   BID, FV, STI, WI
      my_real
     .   ANGLE(MVSIZ),DTX0(MVSIZ),WT,ZR,ZS,ZT,ZZ
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)     
      INTEGER NLYMAX, IPMAT,IPANG
      CHARACTER TITR*nchartitle
      PARAMETER (NLYMAX = 200,IPMAT = 100,IPANG = 200)
C-----------------------------------------------
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
C-----------------------------------------------
      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9)
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
      MBUF => ELBUF_STR%BUFLY(1)%MAT(1,1,1)
      LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
c
      BID = ZERO
      NREFSTA = NXREF
      NXREF = 0
      DO I=LFT,LLT
        DELTAX(I)=EP30
      ENDDO
      JHBE  = IPARG(23)
      IREP  = IPARG(35)
      IGTYP = IPARG(38)
      IF (JCVT==1.AND.ISORTH/=0) JCVT=2
C
      NF1=NFT+1
      IF (IGTYP /= 22) ISORTH = 0
      ICSTR=IPARG(17)
C
      DO I=LFT,LLT
        RHOCP(I) =  PM(69,IXS(1,NFT+I))
        TEMP0(I) =  PM(79,IXS(1,NFT+I))
      ENDDO
      
      IF (JCVT == 0) THEN
        CALL SCOOR3(X ,BID,IXS(1,NF1)   ,GEO  ,MAT  ,PID  ,NGL  ,
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
        CALL SRCOOR3(X,BID,IXS(1,NF1)   ,GEO  ,MAT  ,PID  ,NGL  ,JHBE ,
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
c
      SELECT CASE (IGTYP)
c
       CASE(21)
        DO I=LFT,LLT
          ANGLE(I) = GEO(1,PID(I))
        END DO
        CALL SCMORTH3(PID  ,GEO  ,IGEO ,SKEW ,IREP ,GBUF%GAMA   ,
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         NGL  ,ANGLE,NSIGI,SIGSP,NSIGS,SIGI ,IXS(1,NF1) ,1    ,
     .         ORTHOGLOB ,PTSOL,NEL)
c
       CASE(22)
        DO I=LFT,LLT
          ANGLE(I) = GEO(1,PID(I))
        END DO
        CALL SCMORTH3(PID  ,GEO  ,IGEO ,SKEW ,IREP ,GBUF%GAMA   ,
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         NGL  ,ANGLE ,NSIGI,SIGSP,NSIGS,SIGI ,IXS(1,NF1),1    ,
     .         ORTHOGLOB ,PTSOL,NEL)
        IPTHK = IPANG+NLYMAX
        IPPOS = IPTHK+NLYMAX
        IG    = PID(LFT)
        MTN0  = MTN
        DO I=LFT,LLT
          MAT0(I) = MAT(I)
       	  DTX0(I) = EP20
        ENDDO
      END SELECT
c
      CALL S8ZDERIC3(GBUF%VOL,HX, HY, HZ,
     .     AJC1,AJC2,AJC3,
     .     AJC4,AJC5,AJC6,
     .     AJC7,AJC8,AJC9,SMAX, VOLU, NGL, 
     .     XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .     YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .     ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
      IF (IDTTSH > 0) THEN
         CALL SDLENSH(VOLU,LLSH,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8)
      END IF        
C
      IP=0
      CALL MATINI(PM       ,IXS    ,NIXS       ,X         ,
     .            GEO      ,ALE_CONNECTIVITY  ,DETONATORS ,IPARG     ,
     .            SIGI     ,NEL    ,SKEW       ,IGEO      ,
     .            IPART    ,IPARTS ,
     .            MAT      ,IPM    ,NSIGS      ,NUMSOL    ,PTSOL  ,
     .            IP       ,NGL    ,NPF        ,TF        ,BUFMAT ,
     .            GBUF     ,LBUF   ,MBUF       ,ELBUF_STR ,ILOADP ,
     .            FACLOAD, DELTAX)
c
      CALL SCZERO3(GBUF%RHO,GBUF%SIG,GBUF%EINT,NEL)
C----------------------------------------
C     INITIALISATION DE LA THERMIQUE 
C----------------------------------------
      IF(JTHE /=0) CALL ATHERI(MAT,PM,GBUF%TEMP)
C-------------------------------
C     INTEGRATION POINTS
C-------------------------------
      NLAY = ELBUF_STR%NLAY
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
      NPTT = ELBUF_STR%NPTT
      IT   = 1
C-----------Begin integrating points-----

      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IL=1,NLAY
C-----------
          LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
          MBUF => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)
          L_PLA = ELBUF_STR%BUFLY(IL)%L_PLA
          L_SIGB= ELBUF_STR%BUFLY(IL)%L_SIGB
c
         IF (IGTYP == 21.OR.IGTYP == 22) THEN
c          WT = GEO(IPTHK+IT,IG)
c          IM=IGEO(IPMAT+IS,IG)
c          MTN=NINT(PM(19,IM))
          IF (IGTYP == 22) THEN
            WT = GEO(IPTHK+IL,IG)
            ZZ = GEO(IPPOS+IL,IG)
            IM =IGEO(IPMAT+IL,IG)
            MTN=NINT(PM(19,IM))
            DO I=LFT,LLT
              MAT(I)=IM
              ANGLE(I) = GEO(IPANG+IL,PID(I))
            ENDDO
          ELSE
            ZZ = A_GAUSS(IL,NLAY)
            WT = W_GAUSS(IL,NLAY)
	         ENDIF
C----------------
          IF (ICSTR == 10) THEN
            ZR = A_GAUSS(IR,NPTR)
            ZS = A_GAUSS(IS,NPTS)
            ZT = ZZ
          ELSEIF (ICSTR == 100) THEN
            ZR = A_GAUSS(IR,NPTR)
            ZS = ZZ
            ZT = A_GAUSS(IS,NPTS)
          ELSEIF (ICSTR == 1) THEN
            ZR = ZZ
            ZS = A_GAUSS(IR,NPTR)
            ZT = A_GAUSS(IS,NPTS)
          ENDIF
         ELSE
           ZR = A_GAUSS(IR,NPTR)
           ZS = A_GAUSS(IS,NPTS)
           ZT = A_GAUSS(IL,NLAY)
           WT = W_GAUSS(IL,NLAY)
         ENDIF
         IP = IR + ( (IS-1) + (IL-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*WT
C
         CALL S8ZDERI3(LBUF%VOL,VEUL(1,NF1),GEO,
     .        ZR,ZS,ZT,WI,
     .        HX, HY, HZ,
     .        AJC1,AJC2,AJC3,
     .        AJC4,AJC5,AJC6,
     .        AJC7,AJC8,AJC9,SMAX, DELTAX, NGL,LBUF%VOL0DP)
         IF (IDTTSH > 0) THEN
           DO I=LFT,LLT
             IF (GBUF%IDT_TSH(I)>0) 
     .        DELTAX(I)=MAX(LLSH(I),DELTAX(I))
           ENDDO
         END IF
         IF (IGTYP == 22)
     .   CALL SCMORTH3(PID  ,GEO  ,IGEO ,SKEW ,IREP ,LBUF%GAMA,
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         NGL  ,ANGLE ,NSIGI,SIGSP,NSIGS,SIGI ,IXS(1,NF1),IL   ,
     .         ORTHOGLOB, PTSOL,NEL)
         CALL MATINI(PM     ,IXS    ,NIXS      ,X          ,
     .               GEO    ,ALE_CONNECTIVITY  ,DETONATORS,IPARG      ,
     .               SIGI   ,NEL    ,SKEW      ,IGEO       ,
     .               IPART  ,IPARTS ,
     .               MAT    ,IPM    ,NSIGS     ,NUMSOL     ,PTSOL  ,
     .               IP     ,NGL    ,NPF       ,TF         ,BUFMAT ,
     .               GBUF   ,LBUF   ,MBUF      ,ELBUF_STR  ,ILOADP ,
     .               FACLOAD, DELTAX)
c
          IDEF =0
          IF (MTN >= 28) THEN
            NUVAR  = IPM(8,IXS(1,NFT+1))
            IDEF =1
          ELSE
            NUVAR = 0
            IF (MTN == 14 .OR. MTN == 12 .OR. MTN == 24) THEN       
              IDEF =1                             
            ELSEIF (ISTRAIN == 1 .AND.
     .             (MTN == 1  .OR. MTN == 2  .OR. MTN == 3  .OR.
     .              MTN == 4  .OR. MTN == 6  .OR. MTN == 10 .OR.
     .              MTN == 21 .OR. MTN == 22 .OR. MTN == 23 .OR.
     .              MTN == 49)) THEN      
              IDEF =1                             
            ENDIF                                  
          ENDIF
c
          CALL SIGIN20B(
     .      LBUF%SIG ,PM       ,LBUF%VOL ,SIGSP    ,
     .      SIGI     ,LBUF%EINT,LBUF%RHO ,MBUF%VAR ,LBUF%STRA,
     .      IXS      ,NIXS     ,NSIGI    ,IP       ,NUVAR    ,
     .      NEL      ,IUSER    ,IDEF     ,NSIGS    ,STRSGLOB ,
     .      STRAGLOB ,JHBE     ,IGTYP    ,X        ,GBUF%GAMA,
     .      MAT      ,LBUF%PLA ,L_PLA    ,PTSOL    ,LBUF%SIGB,
     .      L_SIGB   ,IPM      ,BUFMAT   ,LBUF%VOL0DP)
c
          IF (IGTYP == 22) THEN
c           moyene density,sig,...---
            AIRE(:) = ZERO
            CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .           LBUF%EINT ,LBUF%TEMP ,LBUF%DELTAX ,LBUF%RK ,LBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .           VOLU, DTX,IGEO)
c
            CALL SVALUE0(
     .           LBUF%RHO,LBUF%VOL,LBUF%OFF,LBUF%SIG,LBUF%EINT,DTX,       
     .           GBUF%RHO,GBUF%VOL,GBUF%OFF,GBUF%SIG,GBUF%EINT,DTX0,
     .           NEL     )
          ELSE
            CALL SVALUE0(
     .           LBUF%RHO,LBUF%VOL,LBUF%OFF,LBUF%SIG,LBUF%EINT,DTX,       
     .           GBUF%RHO,GBUF%VOL,GBUF%OFF,GBUF%SIG,GBUF%EINT,DTX0,
     .           NEL     )
          ENDIF
C
        ENDDO
       ENDDO
      ENDDO
C-------------------------
      IF (IGTYP == 22) THEN
       MTN=MTN0
       DO I=LFT,LLT
         MAT(I)=MAT0(I)
       ENDDO
      ENDIF
C----------------------------------------
C     INITIALISATION DES MASSES
C----------------------------------------
      CALL SMASS3(
     .     GBUF%RHO,MAS,PARTSAV,X,V,
     .     IPARTS(NF1),MSS(1,NF1),VOLU  ,
     .     MSNF    ,MSSF(1,NF1) ,BID  ,
     .     BID     ,BID         ,WMA  ,RHOCP     ,MCP     ,
     .     MCPS(1,NF1) ,MSSA ,BID   ,BID,GBUF%FILL,
     .     IX1, IX2, IX3, IX4, IX5, IX6, IX7, IX8)
C------------------------------------------
C     assemblage des Volumes nodaux et Modules nodaux
C     (pour rigidites d'interface)
C------------------------------------------
C     attention : IX1, IX2 ... IX8 sont sous la forme NC(MVSIZ,8)
      IF (I7STIFS /= 0) THEN
        NCC=8
        CALL SBULK3(VOLU  ,IX1    ,NCC,MAT,PM ,
     .              VOLNOD,BVOLNOD,VNS(1,NF1),BNS(1,NF1),BID,
     .              BID   ,GBUF%FILL)
      ENDIF
C----------------------------------------
c Failure model initialisation
C----------------------------------------
      CALL FAILINI(ELBUF_STR,NPTR,NPTS,NPTT,NLAY,
     .             IPM,SIGSP,NSIGI,FAIL_INI ,
     .             SIGI,NSIGS,IXS,NIXS,PTSOL,RNOISE,PERTURB,BUFMAT)
C------------------------------------------
C       CALCUL DES DT ELEMENTAIRES
C------------------------------------------
        AIRE(:) = ZERO
        CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .       LBUF%EINT ,LBUF%TEMP ,LBUF%DELTAX ,LBUF%RK ,LBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .       VOLU, DTX,IGEO)
c
       IF (IGTYP == 22) THEN
         DO I=LFT,LLT
           DTX(I)=DTX0(I)
         ENDDO
       ENDIF
       DO I=LFT,LLT
        IF (IXS(10,I+NFT).NE.0.AND.INVERS.GT.14) THEN
          IF(IGTYP.NE.0.AND.IGTYP.NE.6.AND.IGTYP.NE.14.AND.IGTYP.NE.15
     .       .AND.IGTYP.NE.20.AND.IGTYP.NE.21.AND.IGTYP.NE.22)THEN
             IPID1=IXS(NIXS-1,I+NFT)
             CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPID1),LTITR)
             CALL ANCMSG(MSGID=226,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IGEO(1,IPID1),
     .                   C1=TITR,
     .                   I2=IGTYP)
          ENDIF
        ENDIF
        DTELEM(NFT+I)=DTX(I)
C       STI = 0.25 * RHO * VOL / (DT*DT)
        STI = FOURTH * GBUF%FILL(I) * GBUF%RHO(I) * VOLU(I) 
     .         / MAX(EM20,DTX(I)*DTX(I))
        STIFN(IXS(2,I+NFT))=STIFN(IXS(2,I+NFT))+STI
        STIFN(IXS(3,I+NFT))=STIFN(IXS(3,I+NFT))+STI
        STIFN(IXS(4,I+NFT))=STIFN(IXS(4,I+NFT))+STI
        STIFN(IXS(5,I+NFT))=STIFN(IXS(5,I+NFT))+STI
        STIFN(IXS(6,I+NFT))=STIFN(IXS(6,I+NFT))+STI
        STIFN(IXS(7,I+NFT))=STIFN(IXS(7,I+NFT))+STI
        STIFN(IXS(8,I+NFT))=STIFN(IXS(8,I+NFT))+STI
        STIFN(IXS(9,I+NFT))=STIFN(IXS(9,I+NFT))+STI
      ENDDO
c
      NXREF = NREFSTA
C-----------
      RETURN
      END
