Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    s4init3                ../starter/source/elements/solid/solide4/s4init3.F
!||--- called by ------------------------------------------------------
!||    initia                 ../starter/source/elements/initia/initia.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    atheri                 ../starter/source/ale/atheri.F
!||    aturi3                 ../starter/source/ale/ale3d/aturi3.F
!||    dtmain                 ../starter/source/materials/time_step/dtmain.F
!||    failini                ../starter/source/elements/solid/solide/failini.F
!||    fretitl2               ../starter/source/starter/freform.F
!||    m115_perturb           ../starter/source/materials/mat/mat115/m115_perturb.F
!||    matini                 ../starter/source/materials/mat_share/matini.F
!||    s4coor3                ../starter/source/elements/solid/solide4/s4coor3.F
!||    s4deri3                ../starter/source/elements/solid/solide4/s4deri3.F
!||    s4jaci3                ../starter/source/elements/solid/solide4/s4jaci3.F
!||    s4mass3                ../starter/source/elements/solid/solide4/s4mass3.F
!||    sboltini               ../starter/source/loads/bolt/sboltini.F
!||    sbulk3                 ../starter/source/elements/solid/solide/sbulk3.F
!||    smorth3                ../starter/source/elements/solid/solide/smorth3.F
!||    soltosphv4             ../starter/source/elements/sph/soltosph.F
!||    sreploc3               ../starter/source/elements/solid/solide/sreploc3.F
!||    userin3                ../starter/source/elements/solid/solide/userin3.F
!||    ustrsin3               ../starter/source/elements/solid/solide/userin3.F
!||--- uses       -----------------------------------------------------
!||    bpreload_mod           ../starter/share/modules1/bpreload_mod.F
!||    defaults_mod           ../starter/source/modules/defaults_mod.F90
!||    detonators_mod         ../starter/share/modules1/detonators_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE S4INIT3(
     1   ELBUF_STR,MAS     ,IXS     ,PM      ,X       ,
     2   DETONATORS,GEO     ,VEUL    ,ALE_CONNECTIVITY   ,IPARG_GR,
     3   DTELEM   ,SIGI    ,NEL     ,SKEW    ,IGEO    ,
     4   STIFN    ,PARTSAV ,V       ,IPARTS  ,MSS     ,
     5   IPART    ,MSNF    ,IPARG   ,
     6   MSSF     ,IPM     ,NSIGS   ,VOLNOD  ,BVOLNOD ,
     7   VNS      ,BNS     ,WMA     ,PTSOL   ,BUFMAT  ,
     8   MCP      ,MCPS    ,TEMP    ,NPF     ,TF      ,
     9   IUSER    ,SIGSP   ,NSIGI   ,MSSA    ,XREFS   ,
     A   STRSGLOB ,STRAGLOB,FAIL_INI,SPBUF   ,SOL2SPH ,
     B   ILOADP   ,FACLOAD ,RNOISE  ,PERTURB ,MAT_PARAM,
     C   DEFAULTS_SOLID    ,NINTEMP )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE BPRELOAD_MOD
      USE DETONATORS_MOD      
      USE ALE_CONNECTIVITY_MOD
      USE MATPARAM_DEF_MOD
      USE DEFAULTS_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
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
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "scry_c.inc"
#include      "vect01_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPARG_GR(NPARG),IPARG(NPARG,NGROUP),
     .   IPARTS(*),IPART(LIPART1,*),IGEO(NPROPGI,*),PTSOL(*),NPF(*),
     .   IPM(NPROPMI,*),STRSGLOB(*),STRAGLOB(*),FAIL_INI(*),SOL2SPH(2,*),
     .   PERTURB(NPERTURB)
      INTEGER NEL, NSIGS,  IUSER, NSIGI
      INTEGER ,INTENT(IN) :: NINTEMP
      my_real
     .   MAS(*), PM(NPROPM,*), X(*), GEO(NPROPG,*),
     .   VEUL(LVEUL,*), DTELEM(*),SIGI(NSIGS,*),SKEW(LSKEW,*),STIFN(*),
     .   PARTSAV(20,*), V(*), MSS(8,*)  , 
     .   MSNF(*), MSSF(8,*),WMA(*),XREFS(8,3,*),
     .   VOLNOD(*), BVOLNOD(*), VNS(8,*), BNS(8,*),BUFMAT(*),
     .   MCP(*), MCPS(8,*), TEMP(*), TF(*),SIGSP(NSIGI,*), MSSA(*),
     .   SPBUF(NSPBUF,*),RNOISE(NPERTURB,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(DETONATORS_STRUCT_)::DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
      TYPE(SOLID_DEFAULTS_), INTENT(IN) :: DEFAULTS_SOLID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NF1,I,IGTYP,IREP,NCC,IP,NUVAR,IDEF,JHBE,IPID1,NPTR,NPTS,NPTT,NLAY,L_SIGB,L_PLA,IMAS_DS
      INTEGER MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ), IXT4(MVSIZ,4)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
      INTEGER NSPHDIR,NCELF,NCELL,IBOLTP
      DOUBLE PRECISION
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),
     .   Y3(MVSIZ),Y4(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),Z3(MVSIZ),Z4(MVSIZ) 
      CHARACTER(LEN=NCHARTITLE)::TITR1
      my_real
     .   BID, FV, STI
      my_real
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),
     .   SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),
     .   TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ),
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),
     .   E2Y(MVSIZ),E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .   PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .   PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .   VOLU(MVSIZ), DTX(MVSIZ),RHOCP(MVSIZ),
     .   TEMP0(MVSIZ), DELTAX(MVSIZ), AIRE(MVSIZ)
      my_real :: TEMPEL(NEL)
C-----------------------------------------------
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================*
      GBUF => ELBUF_STR%GBUF
      LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
      MBUF => ELBUF_STR%BUFLY(1)%MAT(1,1,1)
c
      JHBE  = IPARG_GR(23)
      IREP  = IPARG_GR(35)
      IGTYP = IPARG_GR(38)
      NPTR  = ELBUF_STR%NPTR
      NPTS  = ELBUF_STR%NPTS
      NPTT  = ELBUF_STR%NPTT
      NLAY  = ELBUF_STR%NLAY
      L_SIGB= ELBUF_STR%BUFLY(1)%L_SIGB
      L_PLA = ELBUF_STR%BUFLY(1)%L_PLA
      NF1=NFT+1
      IF(MTN>=28)THEN
        NUVAR = IPM(8,IXS(1,NF1))
      ELSE
        NUVAR = 0
      ENDIF
C
      IMAS_DS = DEFAULTS_SOLID%IMAS
      IBOLTP = IPARG_GR(72)  !Bolt preloading
      JCVT  = IPARG_GR(37)
C
       DO I=LFT,LLT
        RHOCP(I) =  PM(69,IXS(1,NFT+I))
        TEMP0(I) =  PM(79,IXS(1,NFT+I))
      ENDDO
C
      CALL S4COOR3(X    ,XREFS(1,1,NF1),IXS(1,NF1),NGL  ,
     .             MAT  ,PID  ,IX1  ,IX2  ,IX3  ,IX4  ,
     .             X1   ,X2   ,X3   ,X4   ,Y1   ,Y2   ,
     .             Y3   ,Y4   ,Z1   ,Z2   ,Z3   ,Z4   )
      CALL S4DERI3(GBUF%VOL,VEUL(1,NF1),GEO ,IGEO   ,RX    ,
     .             RY     ,RZ     ,SX     ,SY     ,
     .             SZ     ,TX     ,TY     ,TZ     ,
     .             X1     ,X2     ,X3     ,X4     ,Y1     ,Y2     ,
     .             Y3     ,Y4     ,Z1     ,Z2     ,Z3     ,Z4     ,
     .             PX1    ,PX2    ,PX3    ,PX4    ,
     .             PY1    ,PY2    ,PY3    ,PY4    ,
     .             PZ1    ,PZ2    ,PZ3    ,PZ4    ,GBUF%JAC_I, 
     .             DELTAX ,VOLU   ,NGL    ,PID    ,MAT       ,
     .             PM     ,LBUF%VOL0DP)
      IREP = IPARG_GR(35)
      CALL SREPLOC3(
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  )
      IF (IGTYP == 6 .OR. IGTYP == 21)
     .  CALL SMORTH3(PID  ,GEO  ,IGEO ,SKEW ,IREP ,GBUF%GAMA  ,
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,NSIGI,SIGSP,NSIGS,
     .         SIGI ,IXS  ,X    ,JHBE ,PTSOL,NEL  ,IPARG_GR(28))
!
!     Initialize element temperature from /initemp
!      
      IF (JTHE == 0 .and. NINTEMP > 0) THEN
        DO I=1,NEL
          TEMPEL(I) = ONE_OVER_8 *(TEMP(IXS(2,I)) + TEMP(IXS(3,I))
     .                           + TEMP(IXS(4,I)) + TEMP(IXS(5,I))        
     .                           + TEMP(IXS(6,I)) + TEMP(IXS(7,I))        
     .                           + TEMP(IXS(8,I)) + TEMP(IXS(9,I))) 
        ENDDO
      ELSE
        TEMPEL(1:NEL) = TEMP0(1:NEL)
      END IF
!
      IP=1
      CALL MATINI(PM      ,IXS     ,NIXS       ,X         ,
     .            GEO     ,ALE_CONNECTIVITY   ,DETONATORS ,IPARG_GR  ,
     .            SIGI    ,NEL     ,SKEW       ,IGEO      ,
     .            IPART   ,IPARTS  ,
     .            MAT     ,IPM     ,NSIGS      ,NUMSOL    ,PTSOL  ,
     .            IP      ,NGL     ,NPF        ,TF        ,BUFMAT ,
     .            GBUF    ,LBUF    ,MBUF       ,ELBUF_STR ,ILOADP ,
     .            FACLOAD, DELTAX  ,TEMPEL     )
C
      ! Density perturbation for /MAT/LAW115
      IF (MTN == 115) THEN 
        CALL M115_PERTURB(PM       ,MAT      ,GBUF%RHO ,PERTURB  ,RNOISE   ) 
      ENDIF
C
      IF (IBOLTP /=0) THEN
        CALL SBOLTINI(E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     1                 GBUF%BPRELD,NEL  ,IXS  ,NIXS ,VPRELOAD, IFLAG_BPRELOAD)
      ENDIF
C----------------------------------------
C     INITIALISATION DE LA THERMIQUE ET TURBULENCE
C----------------------------------------
      IF(JTHE /=0) CALL ATHERI(MAT ,PM ,GBUF%TEMP)
      IF(JTUR /=0) CALL ATURI3(IPARG   ,GBUF%RHO,PM  ,IXS  ,X     ,
     .                         GBUF%RK ,GBUF%RE ,VOLU   )
C----------------------------------------
C     INITIALISATION DES MASSES
C----------------------------------------
      IF(JLAG+JALE+JEUL/=0) THEN
C-------- case /INIBRIS/STRS_FGLO missed         
         IF (ISIGI /= 0 .AND. (JCVT/=0.OR.ISORTH/=0)) 
     .   CALL USTRSIN3(
     .   SIGI    ,LBUF%SIG ,IXS     ,NIXS    ,NSIGS   ,
     .   NEL     ,STRSGLOB ,JHBE    ,IGTYP   ,X       ,
     .   GBUF%GAMA,PTSOL   ,LBUF%VOL0DP,RHOCP,GBUF%RHO)
C
          IDEF = 0
          IF(MTN >= 28.AND. MTN /= 49)THEN
            IDEF = 1
          ELSEIF(MTN == 14 .OR. MTN == 12) THEN
           IDEF = 1
          ELSEIF(ISTRAIN == 1)THEN
           IF(MTN == 1)THEN
             IDEF = 1
           ELSEIF(MTN == 2)THEN
              IDEF = 1
           ELSEIF(MTN == 4)THEN
            IDEF = 1
           ELSEIF(MTN == 3.OR.MTN == 6.OR.MTN == 10.OR.
     .        MTN == 21.OR.MTN == 22.OR.MTN == 23)THEN
            IDEF = 1
           ENDIF
          ENDIF
C
          IF (ISIGI /= 0 .AND. ((MTN >= 28 .AND. IUSER == 1).OR.
     .      (NVSOLID2 /= 0 .AND .IDEF /=0)))
     .    CALL USERIN3(
     .         SIGSP   ,SIGI    ,MBUF%VAR ,LBUF%STRA,
     .         IXS     ,NIXS    ,NSIGI    ,NUVAR    ,NEL     ,
     .         NSIGS   ,IUSER   ,IDEF     ,STRAGLOB ,JHBE    ,
     .         IGTYP   ,X       ,GBUF%GAMA,PTSOL    ,LBUF%SIGB,
     .         L_SIGB  ,MAT(1)  ,IPM      ,BUFMAT   ,LBUF%PLA,  
     .         L_PLA   )
c
          CALL S4MASS3(
     1      GBUF%RHO   ,MAS       ,PARTSAV,X   ,V,
     2      IPARTS(NF1),MSS(1,NF1),MSNF   ,MSSF(1,NF1),WMA,
     3      RHOCP      ,MCP       ,MCPS(1,NF1),TEMP0,TEMP ,
     4      MSSA       ,IX1       ,IX2     ,IX3     ,IX4    ,
     5      GBUF%FILL, VOLU       ,IMAS_DS ,NINTEMP )
C------------------------------------------
C       assemblage des Volumes nodaux et Modules nodaux
C       (pour rigidites d'interface)
C------------------------------------------
C       attention : IX1, IX2 ... IX4 sont sous la forme NC(MVSIZ,4)
        IF(I7STIFS/=0)THEN
          NCC=4
          IXT4(1:MVSIZ,1) = IX1(1:MVSIZ)
          IXT4(1:MVSIZ,2) = IX2(1:MVSIZ)
          IXT4(1:MVSIZ,3) = IX3(1:MVSIZ)
          IXT4(1:MVSIZ,4) = IX4(1:MVSIZ)
          CALL SBULK3(VOLU  ,IXT4    ,NCC,MAT,PM ,
     2              VOLNOD,BVOLNOD,VNS(1,NF1),BNS(1,NF1),BID,
     3              BID   ,GBUF%FILL)
        ENDIF
      ENDIF
C----------------------------------------
c Initialization of stress tensor in case of Orthotropic properties
C----------------------------------------
      IF (ISIGI /= 0 .AND. ISORTH/=0) THEN 
        LBUF%SIGL = LBUF%SIG
      ENDIF
C----------------------------------------
c Failure model initialisation
C----------------------------------------
      CALL FAILINI(ELBUF_STR,NPTR,NPTS,NPTT,NLAY,
     .             IPM,SIGSP,NSIGI,FAIL_INI ,
     .             SIGI,NSIGS,IXS,NIXS,PTSOL,RNOISE,PERTURB,MAT_PARAM)
C----------------------------------------
c initialisation inibri/eref
C----------------------------------------
      IF (NSIGI > 0.AND.(ISMSTR==10.OR.ISMSTR==12)) THEN
         CALL S4JACI3(GBUF%SMSTR,GBUF%JAC_I, GBUF%VOL,NEL  ) 
      END IF
C------------------------------------------
C     CALCUL DES DT ELEMENTAIRES
c
      AIRE(:) = ZERO
      CALL DTMAIN(GEO       ,PM        ,IPM         ,PID     ,MAT     ,FV    ,
     .     GBUF%EINT ,GBUF%TEMP ,GBUF%DELTAX ,GBUF%RK ,GBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .     VOLU, DTX ,IGEO,IGTYP)
C------------------------------------------
c
      DO 10 I=LFT,LLT
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
        DTELEM(NFT+I)=DTX(I)
C       STI = 2 * (MASS/4) /dt^2
        STI = HALF * GBUF%FILL(I)* GBUF%RHO(I) * VOLU(I) /
     .        MAX(EM20,DTX(I)*DTX(I))
        STIFN(IXS(2,I+NFT))=STIFN(IXS(2,I+NFT))+STI
        STIFN(IXS(4,I+NFT))=STIFN(IXS(4,I+NFT))+STI
        STIFN(IXS(6,I+NFT))=STIFN(IXS(6,I+NFT))+STI
        STIFN(IXS(7,I+NFT))=STIFN(IXS(7,I+NFT))+STI
  10  CONTINUE
C------------------------------------------
C     SOLID TO SPH, COMPUTE INITIAL VOLUME & MASS OF PARTICLES
C------------------------------------------
      IF(NSPHSOL/=0)THEN
        DO I=LFT,LLT
          IF(SOL2SPH(1,NFT+I) < SOL2SPH(2,NFT+I))THEN
C           SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
            NSPHDIR=IGEO(37,IXS(10,NFT+I))
            NCELF  =SOL2SPH(1,NFT+I)+1
            NCELL  =SOL2SPH(2,NFT+I)-SOL2SPH(1,NFT+I)
            CALL SOLTOSPHV4(
     .        NSPHDIR ,GBUF%RHO(I) ,NCELL   ,X      ,SPBUF(1,NCELF),
     .        IXS(1,I+NFT))
          END IF
        ENDDO
      END IF
C-----------
      RETURN
      END
