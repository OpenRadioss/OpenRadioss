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
Chd|  SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|        FAILINI                       source/elements/solid/solide/failini.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        SBULK3                        source/elements/solid/solide/sbulk3.F
Chd|        SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|        SINI43                        source/elements/solid/sconnect/sini43.F
Chd|        SPCOOR3                       source/elements/solid/sconnect/spcoor3.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|        ST_USERLIB_SINIUSR            source/user_interface/dyn_userlib.c
Chd|        SUDERI3                       source/elements/elbuf_init/suderi3.F
Chd|        SUMASS3                       source/elements/elbuf_init/suinit3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SUINIT3(ELBUF_STR,MS      ,IXS    ,PM       ,X       ,
     .                   DETONATORS,GEO     ,VEUL   ,ALE_CONNECTIVITY ,IPARG   ,
     .                   DTELEM   ,SIGI    ,NEL    ,SKEW     ,IGEO    ,
     .                   STIFN    ,PARTSAV ,V      ,IPARTS   ,MSS     ,
     .                   IPART    ,SIGSP   ,
     .                   NSIGI    ,IN      ,VR     ,IPM      ,NSIGS   ,
     .                   VOLNOD   ,BVOLNOD ,VNS    ,BNS      ,PTSOL   ,
     .                   BUFMAT   ,NPF     ,TF     ,FAIL_INI ,INS     ,
     .                   ILOADP   ,FACLOAD ,RNOISE ,PERTURB  )
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
C   G l o b a l   P a r a m e t e r  s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "scry_c.inc"
#include      "vect01_c.inc"
#include      "warn_c.inc"
#include      "scr15_c.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(NPARG),IPARTS(*),
     .    NEL, IPART(LIPART1,*), 
     .    IGEO(NPROPGI,*), IPM(NPROPMI,*), PTSOL(*), NSIGI, NSIGS,
     .    NPF(*),FAIL_INI(*),PERTURB(NPERTURB)
      my_real
     .   MS(*), X(3,*), GEO(NPROPG,*),PM(NPROPM,*),
     .   VEUL(LVEUL,*), DTELEM(*),SIGI(NSIGS,*),SKEW(LSKEW,*),STIFN(*),
     .   PARTSAV(20,*), V(3,*), MSS(8,*),RNOISE(NPERTURB,*), 
     . SIGSP(NSIGI,*) , IN(*), VR(3,*),
     . VOLNOD(*), BVOLNOD(*), VNS(8,*), BNS(8,*),BUFMAT(*), TF(*),
     . INS(8,*)     
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(DETONATOR_STRUCT_)::DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NF1,IBID,JHBE,IGTYP,IREP,NCC,NUVAR,IP,NREFSTA,
     .        IPID1,NPTR,NPTS,NPTT,NLAY,IADB,MLW,II(6)
      INTEGER MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ),
     .        IPROP(MVSIZ) ,IMAT(MVSIZ) ,SID(MVSIZ),
     .        NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ),
     .        NC5(MVSIZ), NC6(MVSIZ), NC7(MVSIZ), NC8(MVSIZ)
      CHARACTER*nchartitle,
     .   TITR1
      CHARACTER*50 OPTION
      my_real
     .   BID, FV, V8LOC(51,MVSIZ), VOLU(MVSIZ), DTX(MVSIZ),
     .   MASS(MVSIZ),MAS(MVSIZ,8),INN(MVSIZ,8),XX(MVSIZ,8),YY(MVSIZ,8),
     .   ZZ(MVSIZ,8),VX(MVSIZ,8),VY(MVSIZ,8),VZ(MVSIZ,8),VRX(MVSIZ,8),
     .   VRY(MVSIZ,8),VRZ(MVSIZ,8),STI(MVSIZ),STIR(MVSIZ),VISCM(MVSIZ),
     .   VISCR(MVSIZ),AREA(MVSIZ),
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),X5(MVSIZ),X6(MVSIZ),
     .   X7(MVSIZ),X8(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),
     .   Y5(MVSIZ),Y6(MVSIZ),Y7(MVSIZ),Y8(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),
     .   Z3(MVSIZ),Z4(MVSIZ),Z5(MVSIZ),Z6(MVSIZ),Z7(MVSIZ),Z8(MVSIZ),
     .   RX(MVSIZ) ,RY(MVSIZ) ,RZ(MVSIZ) ,SX(MVSIZ) ,
     .   SY(MVSIZ) ,SZ(MVSIZ) ,TX(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ) ,
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),
     .   E2Y(MVSIZ),E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   F1X(MVSIZ) ,F1Y(MVSIZ) ,F1Z(MVSIZ) ,
     .   F2X(MVSIZ) ,F2Y(MVSIZ) ,F2Z(MVSIZ),
     .   PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .   PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .   PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .   SIG_LOC(6,NEL), DELTAX(MVSIZ), AIRE(1)
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
C   S o u r c e  L i n e s
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
      MBUF => ELBUF_STR%BUFLY(1)%MAT(1,1,1)
      NPTR =  ELBUF_STR%NPTR
      NPTS =  ELBUF_STR%NPTS
      NPTT =  ELBUF_STR%NPTT
      NLAY =  ELBUF_STR%NLAY
      MLW  =  ELBUF_STR%BUFLY(1)%ILAW
c
      NREFSTA = NXREF
      NXREF = 0
      BID   = ZERO
      JHBE  = IPARG(23)
      IREP  = IPARG(35)
      IGTYP = IPARG(38)
C
      NF1=NFT+1
!
      DO I=1,6
        II(I) = NEL*(I-1)
      ENDDO
!
c--------------------------
      IF (IGTYP == 43) THEN
        CALL SPCOOR3(
     .           X    ,IXS(1,NF1) ,GEO   ,MAT  ,PID  ,NGL  ,
     .           NC1  ,NC2  ,NC3  ,NC4  ,NC5  ,NC6  ,NC7  ,NC8  ,
     .           X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .           Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .           Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .           E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .           AREA )
      ELSEIF (JCVT == 0) THEN
        CALL SCOOR3(X ,BID  ,IXS(1,NF1) ,GEO  ,MAT  ,PID  ,NGL  ,
     .           NC1  ,NC2  ,NC3  ,NC4  ,NC5  ,NC6  ,NC7  ,NC8  ,
     .           X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .           Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .           Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .           RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .           E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .           F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,BID, BID,
     .           XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .           YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .           ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
      ELSE
        CALL SRCOOR3(X,BID  ,IXS(1,NF1) ,GEO  ,MAT  ,PID  ,NGL  ,JHBE ,
     .           NC1  ,NC2  ,NC3  ,NC4  ,NC5  ,NC6  ,NC7  ,NC8  ,
     .           X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .           Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .           Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .           RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .           E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .           F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,BID  , BID,
     .           XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .           YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .           ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
      ENDIF
c--------------------------
      CALL SUDERI3(NEL ,GBUF%VOL,
     .            X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .            Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .            Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   )
      CALL SDLEN3(
     .            X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .            Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .            Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8, 
     .            DELTAX, VOLU)
C
      IF (IGTYP /= 43) THEN
        IP = 0                                                 
        CALL MATINI(PM      ,IXS    ,NIXS      ,X         ,  
     .              GEO     ,ALE_CONNECTIVITY  ,DETONATORS,IPARG     ,  
     .              SIGI    ,NEL    ,SKEW      ,IGEO      ,  
     .              IPART   ,IPARTS ,  
     .              MAT     ,IPM    ,NSIGS     ,NUMSOL    ,PTSOL  ,  
     .              IP      ,NGL    ,NPF       ,TF        ,BUFMAT ,  
     .              GBUF    ,LBUF   ,MBUF      ,ELBUF_STR ,ILOADP ,
     .              FACLOAD, DELTAX)
      ENDIF      
C-----------------------------------------------
      DO J=1,8
        DO I=LFT,LLT
          XX(I,J)=X(1,IXS(J+1,I+NFT))
          YY(I,J)=X(2,IXS(J+1,I+NFT))
          ZZ(I,J)=X(3,IXS(J+1,I+NFT))
          VX(I,J)=V(1,IXS(J+1,I+NFT))
          VY(I,J)=V(2,IXS(J+1,I+NFT))
          VZ(I,J)=V(3,IXS(J+1,I+NFT))
        ENDDO
      ENDDO
      IF (IRODDL > 0) THEN
        DO J=1,8
          DO I=LFT,LLT
            VRX(I,J)=VR(1,IXS(J+1,I+NFT))
            VRY(I,J)=VR(2,IXS(J+1,I+NFT))
            VRZ(I,J)=VR(3,IXS(J+1,I+NFT))
          ENDDO
        ENDDO
      ELSE
        VRX=ZERO
        VRY=ZERO
        VRZ=ZERO
      ENDIF
C-----------------------------------------------
      DO I=LFT,LLT
        IPROP(I)=IXS(10,I+NFT)
        SID(I)  =IXS(11,I+NFT)
        IMAT(I) =IXS(1,I+NFT)
      ENDDO
      IADB  = IPM(7,IMAT(1))
      NUVAR = IPM(8,IMAT(1))
C----------------------------------------
C     INITIALISATION USER: VOLUME, RHO, MASSES et INERTIES
C----------------------------------------
      IF(IGTYP == 29)THEN
        DO I=LFT,LLT
          SIG_LOC(1,I) = GBUF%SIG(II(1)+I)
          SIG_LOC(2,I) = GBUF%SIG(II(2)+I)
          SIG_LOC(3,I) = GBUF%SIG(II(3)+I)
          SIG_LOC(4,I) = GBUF%SIG(II(4)+I)
          SIG_LOC(5,I) = GBUF%SIG(II(5)+I)
          SIG_LOC(6,I) = GBUF%SIG(II(6)+I)
        ENDDO
        IF (USERL_AVAIL==1)THEN
          CALL ST_USERLIB_SINIUSR(IGTYP,ROOTNAM,ROOTLEN,
     1       NEL    ,NUVAR    ,IPROP  ,IMAT   ,SID    ,
     2       GBUF%EINT,GBUF%VOL,MBUF%VAR,GBUF%OFF,GBUF%RHO,SIG_LOC,
     3       XX(1,1),XX(1,2),XX(1,3),XX(1,4),XX(1,5),XX(1,6),XX(1,7),XX(1,8),
     4       YY(1,1),YY(1,2),YY(1,3),YY(1,4),YY(1,5),YY(1,6),YY(1,7),YY(1,8),
     5       ZZ(1,1),ZZ(1,2),ZZ(1,3),ZZ(1,4),ZZ(1,5),ZZ(1,6),ZZ(1,7),ZZ(1,8),
     6       VX(1,1),VX(1,2),VX(1,3),VX(1,4),VX(1,5),VX(1,6),VX(1,7),VX(1,8),
     7       VY(1,1),VY(1,2),VY(1,3),VY(1,4),VY(1,5),VY(1,6),VY(1,7),VY(1,8),
     8       VZ(1,1),VZ(1,2),VZ(1,3),VZ(1,4),VZ(1,5),VZ(1,6),VZ(1,7),VZ(1,8),
     9       VRX(1,1),VRX(1,2),VRX(1,3),VRX(1,4),
     9                             VRX(1,5),VRX(1,6),VRX(1,7),VRX(1,8),
     A       VRY(1,1),VRY(1,2),VRY(1,3),VRY(1,4),
     A                             VRY(1,5),VRY(1,6),VRY(1,7),VRY(1,8),
     B       VRZ(1,1),VRZ(1,2),VRZ(1,3),VRZ(1,4),
     B                             VRZ(1,5),VRZ(1,6),VRZ(1,7),VRZ(1,8),
     C       MAS(1,1),MAS(1,2),MAS(1,3),MAS(1,4),
     C                             MAS(1,5),MAS(1,6),MAS(1,7),MAS(1,8),
     D       INN(1,1),INN(1,2),INN(1,3),INN(1,4),
     D                             INN(1,5),INN(1,6),INN(1,7),INN(1,8),
     C       STI    ,STIR   ,VISCM  ,VISCR)
        ELSE
         OPTION='/PROP/USER29'
         CALL ANCMSG(MSGID=1155,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=OPTION)
        ENDIF ! IF (USERL_AVAIL==1)THEN
        DO I=LFT,LLT
          GBUF%SIG(II(1)+I) = SIG_LOC(1,I)
          GBUF%SIG(II(2)+I) = SIG_LOC(2,I)
          GBUF%SIG(II(3)+I) = SIG_LOC(3,I)
          GBUF%SIG(II(4)+I) = SIG_LOC(4,I)
          GBUF%SIG(II(5)+I) = SIG_LOC(5,I)
          GBUF%SIG(II(6)+I) = SIG_LOC(6,I)
        ENDDO
      ELSEIF(IGTYP == 30)THEN
        CONTINUE
      ELSEIF(IGTYP == 31)THEN
        CONTINUE
      ELSEIF(IGTYP == 43)THEN
c       initialization of strain, stress, uvar  
        NUVAR = ELBUF_STR%BUFLY(1)%NVAR_MAT
c
        CALL SINI43(ELBUF_STR,
     1       MLW      ,NEL      ,AREA     ,GBUF%VOL ,GBUF%RHO ,
     2       STI      ,STIR     ,VISCM    ,VISCR    ,BUFMAT(IADB),
     3       MAS(1,1) ,MAS(1,2) ,MAS(1,3) ,MAS(1,4) ,MAS(1,5) ,
     4       MAS(1,6) ,MAS(1,7) ,MAS(1,8) ,INN(1,1) ,INN(1,2) ,
     5       INN(1,3) ,INN(1,4) ,INN(1,5) ,INN(1,6) ,INN(1,7) ,
     6       INN(1,8) ,PM       ,MAT      ,GBUF%OFF ,GBUF%EINT,
     7       PTSOL    ,SIGSP    ,NSIGI    ,NUVAR    )
      ENDIF
C
      DO J=1,8
        DO I=LFT,LLT
          V(1,IXS(J+1,I+NFT)) = VX(I,J)
          V(2,IXS(J+1,I+NFT)) = VY(I,J)
          V(3,IXS(J+1,I+NFT)) = VZ(I,J)
        ENDDO
      ENDDO
      IF (IRODDL > 0) THEN
        DO J=1,8
          DO I=LFT,LLT
            VR(1,IXS(J+1,I+NFT))= VRX(I,J)
            VR(2,IXS(J+1,I+NFT))= VRY(I,J)
            VR(3,IXS(J+1,I+NFT))= VRZ(I,J)
          ENDDO
        ENDDO
      ENDIF
C----------------------------------------
C     INITIALISATION DES MASSES et INERTIES
C----------------------------------------
      CALL SUMASS3(MS,PARTSAV,X,V,IPARTS(NF1),MSS(1,NF1),
     2             MAS,INN,GBUF%VOL,VOLU,MASS,IN,
     3             NC1, NC2, NC3, NC4, NC5, NC6, NC7, NC8,
     4             INS,GBUF%FILL)
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
C   attention : NC1, NC2 ... NC8 sont sous la forme NC(MVSIZ,8)
      IF(I7STIFS.NE.0)THEN
        NCC=8
        CALL SBULK3(VOLU  ,NC1    ,NCC,MAT,PM ,
     2              VOLNOD,BVOLNOD,VNS(1,NF1),BNS(1,NF1),BID,
     3              BID   ,GBUF%FILL)
      ENDIF
C------------------------------------------
C    CALCUL DES DT ELEMENTAIRES
C------------------------------------------
       AIRE(:) = ZERO
       CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .      LBUF%EINT ,LBUF%TEMP ,LBUF%DELTAX ,LBUF%RK ,LBUF%RE ,BUFMAT, DELTAX, AIRE,
     .      VOLU, DTX, IGEO)
C------------------------------------------
      DO I=LFT,LLT
        DTELEM(NFT+I)=DTX(I)
        STIFN(IXS(2,I+NFT))=STIFN(IXS(2,I+NFT))+STI(I)
        STIFN(IXS(3,I+NFT))=STIFN(IXS(3,I+NFT))+STI(I)
        STIFN(IXS(4,I+NFT))=STIFN(IXS(4,I+NFT))+STI(I)
        STIFN(IXS(5,I+NFT))=STIFN(IXS(5,I+NFT))+STI(I)
        STIFN(IXS(6,I+NFT))=STIFN(IXS(6,I+NFT))+STI(I)
        STIFN(IXS(7,I+NFT))=STIFN(IXS(7,I+NFT))+STI(I)
        STIFN(IXS(8,I+NFT))=STIFN(IXS(8,I+NFT))+STI(I)
        STIFN(IXS(9,I+NFT))=STIFN(IXS(9,I+NFT))+STI(I)
      ENDDO
      IF (IGTYP.NE.29 .AND. IGTYP.NE.30 .AND. IGTYP.NE.31  .AND.
     .    IGTYP.NE.43) THEN
        DO I=LFT,LLT
             IPID1=IXS(NIXS-1,I+NFT)
             CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID1),LTITR)
             CALL ANCMSG(MSGID=226,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=IGEO(1,IPID1),
     .                   C1=TITR1,
     .                   I2=IGTYP)
        ENDDO
      ENDIF
C
      NXREF = NREFSTA
C-----------
      RETURN
      END
C
Chd|====================================================================
Chd|  SUMASS3                       source/elements/elbuf_init/suinit3.F
Chd|-- called by -----------
Chd|        SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SUMASS3(MS,PARTSAV,X,V,IPART,MSS,
     2             MAS,INN,VOL,VOLU,MASS,IN,
     3             NC1, NC2, NC3, NC4, NC5, NC6, NC7, NC8,
     4             INS,FILL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(*)
C     REAL ou REAL*8
      my_real
     .   MS(*),IN(*),X(3,*),V(3,*),PARTSAV(20,*),VOL(*),VOLU(*),MASS(*),
     .   MSS(8,*),INS(8,*) ,FILL(*)   
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*), NC7(*),
     .   NC8(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IP,I1,I2,I3,I4,I5,I6,I7,I8, J
C     REAL ou REAL*8
      my_real
     .   XX,YY,ZZ,XY,YZ,ZX,
     .   MAS(MVSIZ,8),INN(MVSIZ,8)
C=======================================================================
       DO I=LFT,LLT      
C       
        VOLU(I) = VOL(I)
        MASS(I) = FILL(I)*(MAS(I,1)+MAS(I,2)+MAS(I,3)+MAS(I,4)
     +          + MAS(I,5)+MAS(I,6)+MAS(I,7)+MAS(I,8))*UNHUIT
        I1 = NC1(I)
        I2 = NC2(I)
        I3 = NC3(I)
        I4 = NC4(I)
        I5 = NC5(I)
        I6 = NC6(I)
        I7 = NC7(I)
        I8 = NC8(I)
C
        MSS(1,I) = MAS(I,1)
        MSS(2,I) = MAS(I,2)
        MSS(3,I) = MAS(I,3)
        MSS(4,I) = MAS(I,4)
        MSS(5,I) = MAS(I,5)
        MSS(6,I) = MAS(I,6)
        MSS(7,I) = MAS(I,7)
        MSS(8,I) = MAS(I,8)
C
        IF (IRODDL > 0) THEN
          INS(1,I)= INN(I,1)
          INS(2,I)= INN(I,2)
          INS(3,I)= INN(I,3)
          INS(4,I)= INN(I,4)
          INS(5,I)= INN(I,5)
          INS(6,I)= INN(I,6)
          INS(7,I)= INN(I,7)
          INS(8,I)= INN(I,8)
        ENDIF
C
        IP=IPART(I)
        PARTSAV(1,IP)=PARTSAV(1,IP) + HUIT*MASS(I)
        PARTSAV(2,IP)=PARTSAV(2,IP) + MASS(I)*
     .       (X(1,I1)+X(1,I2)+X(1,I3)+X(1,I4)
     .       +X(1,I5)+X(1,I6)+X(1,I7)+X(1,I8))
        PARTSAV(3,IP)=PARTSAV(3,IP) + MASS(I)*
     .       (X(2,I1)+X(2,I2)+X(2,I3)+X(2,I4)
     .       +X(2,I5)+X(2,I6)+X(2,I7)+X(2,I8))
        PARTSAV(4,IP)=PARTSAV(4,IP) + MASS(I)*
     .       (X(3,I1)+X(3,I2)+X(3,I3)+X(3,I4)
     .       +X(3,I5)+X(3,I6)+X(3,I7)+X(3,I8))
        XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2)
     .       +X(1,I3)*X(1,I3)+X(1,I4)*X(1,I4)
     .       +X(1,I5)*X(1,I5)+X(1,I6)*X(1,I6)
     .       +X(1,I7)*X(1,I7)+X(1,I8)*X(1,I8))
        XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2)
     .       +X(1,I3)*X(2,I3)+X(1,I4)*X(2,I4)
     .       +X(1,I5)*X(2,I5)+X(1,I6)*X(2,I6)
     .       +X(1,I7)*X(2,I7)+X(1,I8)*X(2,I8))
        YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2)
     .       +X(2,I3)*X(2,I3)+X(2,I4)*X(2,I4)
     .       +X(2,I5)*X(2,I5)+X(2,I6)*X(2,I6)
     .       +X(2,I7)*X(2,I7)+X(2,I8)*X(2,I8))
        YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2)
     .       +X(2,I3)*X(3,I3)+X(2,I4)*X(3,I4)
     .       +X(2,I5)*X(3,I5)+X(2,I6)*X(3,I6)
     .       +X(2,I7)*X(3,I7)+X(2,I8)*X(3,I8))
        ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2)
     .       +X(3,I3)*X(3,I3)+X(3,I4)*X(3,I4)
     .       +X(3,I5)*X(3,I5)+X(3,I6)*X(3,I6)
     .       +X(3,I7)*X(3,I7)+X(3,I8)*X(3,I8))
        ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2)
     .       +X(3,I3)*X(1,I3)+X(3,I4)*X(1,I4)
     .       +X(3,I5)*X(1,I5)+X(3,I6)*X(1,I6)
     .       +X(3,I7)*X(1,I7)+X(3,I8)*X(1,I8))
        PARTSAV(5,IP) =PARTSAV(5,IP)  + MASS(I) * (YY+ZZ)
        PARTSAV(6,IP) =PARTSAV(6,IP)  + MASS(I) * (ZZ+XX)
        PARTSAV(7,IP) =PARTSAV(7,IP)  + MASS(I) * (XX+YY)
        PARTSAV(8,IP) =PARTSAV(8,IP)  - MASS(I) * XY
        PARTSAV(9,IP) =PARTSAV(9,IP)  - MASS(I) * YZ
        PARTSAV(10,IP)=PARTSAV(10,IP) - MASS(I) * ZX
C
        PARTSAV(11,IP)=PARTSAV(11,IP) + MASS(I)*
     .       (V(1,I1)+V(1,I2)+V(1,I3)+V(1,I4)
     .       +V(1,I5)+V(1,I6)+V(1,I7)+V(1,I8))
        PARTSAV(12,IP)=PARTSAV(12,IP) + MASS(I)*
     .       (V(2,I1)+V(2,I2)+V(2,I3)+V(2,I4)
     .       +V(2,I5)+V(2,I6)+V(2,I7)+V(2,I8))
        PARTSAV(13,IP)=PARTSAV(13,IP) + MASS(I)*
     .       (V(3,I1)+V(3,I2)+V(3,I3)+V(3,I4)
     .       +V(3,I5)+V(3,I6)+V(3,I7)+V(3,I8))
        PARTSAV(14,IP)=PARTSAV(14,IP) + UNDEMI * MASS(I) *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .     +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2)
     .     +V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3)
     .     +V(1,I4)*V(1,I4)+V(2,I4)*V(2,I4)+V(3,I4)*V(3,I4)
     .     +V(1,I5)*V(1,I5)+V(2,I5)*V(2,I5)+V(3,I5)*V(3,I5)
     .     +V(1,I6)*V(1,I6)+V(2,I6)*V(2,I6)+V(3,I6)*V(3,I6)
     .     +V(1,I7)*V(1,I7)+V(2,I7)*V(2,I7)+V(3,I7)*V(3,I7)
     .     +V(1,I8)*V(1,I8)+V(2,I8)*V(2,I8)+V(3,I8)*V(3,I8))

       ENDDO
C-----------
      RETURN
      END

