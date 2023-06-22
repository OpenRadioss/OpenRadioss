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
Chd|  FORINT_MOD                    source/elements/forint.F      
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      MODULE FORINT_MOD
      CONTAINS
Chd|====================================================================
Chd|  FORINT                        source/elements/forint.F      
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|        PRELECFLOW                    source/elements/solid/solide/prelecflow.F
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|        R23FORC3                      source/elements/spring/r23forc3.F
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SECTIOP                       source/tools/sect/sectio.F    
Chd|        SECTIOR                       source/tools/sect/sectio.F    
Chd|        SECTIOS                       source/tools/sect/sectio.F    
Chd|        SECTIOS4                      source/tools/sect/sectio.F    
Chd|        SECTIOS6                      source/tools/sect/sectio.F    
Chd|        SECTIOT                       source/tools/sect/sectio.F    
Chd|        SENSOR_ENERGY_PART            source/tools/sensor/sensor_energy_part.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SROTA6                        source/output/anim/generate/srota6.F
Chd|        STARTIMEG                     source/system/timer.F         
Chd|        STOPTIMEG                     source/system/timer.F         
Chd|        SUFORC3                       source/user_interface/suforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|        TFORC3                        source/elements/truss/tforc3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        DT_MOD                        share/modules/dt_mod.F        
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE FORINT(
     1    PM        ,GEO       ,X          ,A         ,AR        ,
     2    V         ,VR        ,MS         ,IN        ,W         ,
     3    ELBUF     ,WA        ,VAL2       ,VEUL      ,FV        ,
     4    STIFN     ,STIFR     ,FSKY       ,TF        ,BUFMAT    ,
     5    PARTSAV   ,D         ,DR         ,EANI      ,ELBUF_TAB ,
     6    TANI      ,FANI      ,FSAV       ,SENSORS   ,NLOC_DMG  ,
     7    SKEW      ,ANIN      ,DT2T       ,BUFGEO    ,ITAB      ,
     8    IADS      ,IADQ      ,IADT       ,IADP      ,MATPARAM_TAB,
     9    IADR      ,IPARG     ,ALE_CONNECT,NPC       ,
     A    IXS       ,IXQ       ,IXT        ,IXP       ,
     B    IXR       ,NELTST    ,IPARI      ,
     C    ITYPTST   ,NSTRF      ,IPART     ,
     D    IPARTS    ,IPARTQ    ,IPARTT     ,IPARTP    ,
     E    IPARTR    ,IPARTUR   ,FR_WAVE    ,RBY       ,
     F    IADUR     ,SECFCUM   ,AGRAV      ,IGRV      ,LGRAV     ,
     G    IXS10     ,
     H    IXS20     ,IADS10    ,IADS20     ,IXS16     ,IADS16    ,
     I    W16       ,FSKYM     ,MSNF       ,IGEO      ,IPM       ,
     J    XSEC      ,ITASK     ,TEMP       ,
     K    FTHE      ,FTHESKY               ,IGROUNC   ,NGROUNC   ,
     M    GRESAV    ,GRTH      ,IGRTH      ,XDP       ,MSSA      ,                                                                  
     N    DMELS     ,MSTR      ,DMELTR     ,MSP       ,DMELP     ,
     O    MSRT      ,DMELRT    ,TABLE      ,VF        ,AF        ,
     P    DF        ,WF        ,FFSKY      ,AFGLOB    ,NBSDVOIS  ,
     Q    NERCVOIS  ,NESDVOIS  ,LERCVOIS   ,LESDVOIS  ,PHI1      ,
     R    PHI2      ,MSF       ,NODFT      ,NODLT     ,
     S    FLG_KJ2   ,POR       ,ICONTACT   ,IFOAM     ,VARNOD    ,
     T    KXIG3D    ,IXIG3D    ,KNOT       ,WIGE      ,CONDN     ,
     U    CONDNSKY  ,
     V    TAGPRT_SMS,ITAGND    ,MS_2D      ,NALE      ,STRESSMEAN,
     W    KNOTLOCPC ,KNOTLOCEL ,SUBSET     ,FLAG_SLIPRING_UPDATE,FLAG_RETRACTOR_UPDATE,
     Y    H3D_DATA  ,IFTHE     ,ICONDN     ,DT        ,OUTPUT) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE TABLE_MOD
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE NLOCAL_REG_MOD
      USE MATPARAM_DEF_MOD
      USE GROUPDEF_MOD
      USE SENSOR_MOD
      USE ALE_CONNECTIVITY_MOD
      USE H3D_MOD
      USE ALE_MOD
      USE DT_MOD
      USE OUTPUT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "stati_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),ITAB(*),FLG_KJ2,
     .   IXQ(NIXQ,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IGEO(NPROPGI,NUMGEO),IPARI(NPARI,*),
     .   IPM(NPROPMI,NUMMAT),NPC(*), IPARG(NPARG,NGROUP), 
     .   NSTRF(*), IPART(LIPART1,*) ,
     .   IPARTS(*) ,IPARTQ(*) ,IPARTT(*) ,IPARTP(*) ,
     .   IPARTR(*) ,IPARTUR(*) ,
     .   IADS(8,*),IADQ(4,*),IADT(2,*),
     .   IADP(2,*),IADR(3,*),
     .   NELTST,ITYPTST,IADUR(4,*),
     .   IXS10(6,*),IXS20(12,*),IADS10(6,*),IADS20(12,*),
     .   IXS16(8,*),IADS16(8,*),ITASK,IGROUNC(*),
     .   NGROUNC,GRTH(*),IGRTH(*),NBSDVOIS(*),NERCVOIS(*),
     .   NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),NODFT, NODLT,
     .   ICONTACT(*),IFOAM(*),KXIG3D(NIXIG3D,*),IXIG3D(*),
     .   IGRV(*),LGRAV(*),
     .   TAGPRT_SMS(*),ITAGND(*),NALE(*),FLAG_SLIPRING_UPDATE,FLAG_RETRACTOR_UPDATE
      my_real
     .   X(3,*),V(3,*),VR(3,*),D(3,*),MS(*),IN(*),A(3,*),AR(3,*),
     .   DR(3,*),W(3,*),PM(NPROPM,NUMMAT),SKEW(LSKEW,*),GEO(NPROPG,NUMGEO),
     .   BUFMAT(*),VEUL(*),TF(*),FR_WAVE(*),ELBUF(*) ,
     .   FSAV(NTHVKI,*) ,WA(*),
     .   FV(*),VAL2(*),RBY(*),
     .   FANI(3,*) ,PARTSAV(*)    ,STIFN(*) ,STIFR(*),ANIN(*)    ,
     .   FSKY(*),TANI(*),EANI(*),BUFGEO(*),
     .   DT2T, SECFCUM(7,NUMNOD,NSECT),W16(*), FSKYM(*),
     .   MSNF(*),XSEC(4,3,NSECT),
     .   TEMP(*),FTHE(*), FTHESKY(*),
     .   GRESAV(*), MSSA(*), DMELS(*), MSTR(*), DMELTR(*),
     .   MSP(*), DMELP(*), MSRT(*), DMELRT(*),VF(3,*),AF(3,*),FFSKY(*),
     .   PHI1(*),PHI2(*),DF(3,*),WF(3,*),MSF(*),AFGLOB(3,*),
     .   POR(*) ,VARNOD(*),KNOT(*),WIGE(*),CONDN(*),CONDNSKY(*),AGRAV(*),
     .   MS_2D(*),STRESSMEAN(6,*),KNOTLOCPC(*),KNOTLOCEL(*)
      DOUBLE PRECISION XDP(3,*)
      TYPE(TTABLE) TABLE(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
      TYPE (MATPARAM_STRUCT_), DIMENSION(NUMMAT) :: MATPARAM_TAB 
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
      TYPE(H3D_DATABASE) :: H3D_DATA
      INTEGER, INTENT(IN) :: IFTHE, ICONDN
      TYPE (SENSORS_) ,INTENT(IN) ,TARGET :: SENSORS
      TYPE (DT_) ,  INTENT(INOUT) :: DT
      TYPE(OUTPUT_), INTENT(INOUT) :: OUTPUT !< output structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      DOUBLE PRECISION, POINTER :: pFBSAV6
      INTEGER INDXOF(MVSIZ), IERROR, NV46
      INTEGER I,II,J,N, NG, NVC, MLW, JFT, JLT,ISOLNOD,ITHK,IPLA,
     .   K1, K2, KAD,NF1,IPRI, NELEM, OFFSET, NSGRP, K,
     .   K0, K3, K5, K6, K7, K8, K9, NSG, NEL, KFTS,IOFC, ISTRA,
     .   JJ19,NPE,NIPMAX,ICNOD,NFT1,IBID,NN,
     .   L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16,
     .   L17,L18,L19,L20,L21,L22,L23,L24,L25,L26,L27,L28,L29,L30,
     .   ICP,ICS,IMATVIS,IEXPAN,NPTS,MPT,NPTT,NPTR,NPT0,IG,OFF_IGRTH,
     .   NCTRL,DIM6,IPARSENS,ISECT,GAMA(6),PX,PY,PZ,ISENS_ENERGY
      my_real BID,QMVBID
      my_real VOLN(MVSIZ)
      my_real, ALLOCATABLE, DIMENSION(:,:) :: FX,FY,FZ,MX,MY,MZ
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C======================================================================|
       ALLOCATE(FX(MVSIZ,64),FY(MVSIZ,64),FZ(MVSIZ,64),STAT=IERROR)
       ALLOCATE(MX(MVSIZ,4),MY(MVSIZ,4),MZ(MVSIZ,4),STAT=IERROR)
!
       IF(IERROR/=0)THEN
        CALL ANCMSG(MSGID=246,ANMODE=ANINFO)
        CALL ARRET(2)
       END IF
C  IOUTPRT for assembly synthesis, not need for spring which call *bilan each cycle   
         IPRI = 0
         IF(MOD(NCYCLE,IABS(NCPRI))==0.OR.TT>=OUTPUT%TH%THIS.OR.MDESS /= 0.
     .   OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S>=OUTPUT%TH%THIS).
     .   OR.TT>=OUTPUT%TH%THIS1(1).OR.TT>=OUTPUT%TH%THIS1(2).
     .   OR.TT>=OUTPUT%TH%THIS1(3).OR.TT>=OUTPUT%TH%THIS1(4).OR.TT>=OUTPUT%TH%THIS1(5).
     .   OR.TT>=OUTPUT%TH%THIS1(6).OR.TT>=OUTPUT%TH%THIS1(7).OR.TT>=OUTPUT%TH%THIS1(8).
     .   OR.TT>=OUTPUT%TH%THIS1(9).OR.NTH /= 0.OR.NANIM /= 0          .
     .   OR.TT>=TABFIS(1).OR.TT>=TABFIS(2).
     .   OR.TT>=TABFIS(3).OR.TT>=TABFIS(4).OR.TT>=TABFIS(5).
     .   OR.TT>=TABFIS(6).OR.TT>=TABFIS(7).OR.TT>=TABFIS(8).
     .   OR.TT>=TABFIS(9).OR.TT>=TABFIS(10).
     .   OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S>=OUTPUT%TH%THIS1(1)).
     .   OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S>=OUTPUT%TH%THIS1(2)).
     .   OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S>=OUTPUT%TH%THIS1(3)).
     .   OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S>=OUTPUT%TH%THIS1(4)).
     .   OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S>=OUTPUT%TH%THIS1(5)).
     .   OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S>=OUTPUT%TH%THIS1(6)).
     .   OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S>=OUTPUT%TH%THIS1(7)).
     .   OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S>=OUTPUT%TH%THIS1(8)).
     .   OR.(ALE%SUB%IALESUB /= 0.AND.T1S+DT2S>=OUTPUT%TH%THIS1(9)).OR.ISTAT==3) IPRI=1
!
      ISENS_ENERGY = 0
      DO I=1,SENSORS%NSENSOR
        IF (SENSORS%SENSOR_TAB(I)%TYPE == 14) ISENS_ENERGY = 1 ! save internal/kinetic energy (PARTSAV)
      ENDDO ! DO I=1,NSENSOR
C
       OFF_IGRTH = 0
       IBID = 0
C
        IF (IALELAG > 0) THEN   ! law 77
         DO I=NODFT,NODLT
           MSF(I) = MSF(I) - MSNF(I)
           MSNF(I) = ZERO
         ENDDO
c           /---------------/
            CALL MY_BARRIER
c           /---------------/
          CALL PRELECFLOW(ELBUF_TAB,NGROUNC,IGROUNC,IPARG ,NBSDVOIS,
     .                    NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,PHI1 ,
     .                    PHI2,POR )
        ENDIF
C        
C-------------------------------
C Boucle parallele dynamique SMP
C
!$OMP DO SCHEDULE(DYNAMIC,1)
c------------------------
      DO IG = 1, NGROUNC
        NG = IGROUNC(IG)         
        NGR_SENSOR = NG   
C       temporarily used to avoid pass KTBUF_STR everywhere
        NG_IMP = NG

        IF (IPARG(8,NG)==1) GOTO 250                                 
        ITY = IPARG(5,NG)
C       IF (ITY==3.OR.ITY==7) GOTO 250                              
        IF (IDDW>0) CALL STARTIMEG(NG)                              
        OFFSET  = 0                                                    
        MLW     = IPARG(1,NG)                                          
C       MLW= 0 ----> void                                              
C       MLW = 13 ----> rigid material                       
        IF (MLW == 0 .OR. MLW == 13) GOTO 250                          
C
C---
        CALL INITBUF(IPARG    ,NG      ,                              
     2     MLW     ,NEL     ,NFT     ,KAD     ,ITY     ,               
     3     NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,               
     4     JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,               
     5     NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,IPLA    ,               
     6     IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,               
     7     ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )               
C
        ICNOD   = IPARG(11,NG)                                        
        NSG     = IPARG(10,NG)                                        
        ICS     = IPARG(17,NG)                                        
        NVC     = IPARG(19,NG)                                        
        ITHK    = IPARG(28,NG)                                        
        ISOLNOD = IPARG(28,NG)                                        
        KFTS    = IPARG(30,NG)                                        
        ISTRA   = IPARG(44,NG)                                        
        IMATVIS = IPARG(45,NG)                                        
        IEXPAN  = IPARG(49,NG)                                        
        IGRE    = IPARG(51,NG)                                        
        ISPH2SOL = 0
        IPARTSPH = IPARG(69,NG)
        IFORMDT  = IPARG(73,NG)  
C
        IF (ITY==1) ICP = NSG                                       
        IF (ITY==1 .OR. ITY==2) JPLASOL=IPLA
C---------------------------------
        LFT   = 1                  
        LLT   = MIN(NVSIZ,NEL)     
        MTN   = MLW                
        JFT=LFT                    
        JLT=LLT                    
        NF1 = NFT+1                
        IAD = KAD                  
        JSPH=0                     
        IHET = 0                   
C----6---------------------------------------------------------------7---------8
        IF (ITY==1 .AND. JLAG==1)THEN
          IF(IXS(10,NF1)/=0)THEN         
            IGTYP = IGEO(11,IXS(10,NF1))   
          ELSE                             
            IGTYP=0                        
          ENDIF                            
!
          IF (ISENS_ENERGY == 1 .AND. IPRI == 0)
     .     CALL SENSOR_ENERGY_PART(IPARTS(NF1) ,SUBSET ,IPRI)
C---------
          IF(ISOLNOD==4.AND.(ISROT==0.OR.ISROT==3))THEN                            
C---------
            CALL S4FORC3(ELBUF_TAB,NG      ,
     1        PM      ,GEO       ,IXS      ,X          ,
     2        A       ,V         ,MS       ,W          ,WA      ,
     3        VAL2    ,VEUL      ,FV       ,ALE_CONNECT      ,IPARG   ,
     4        TF      ,NPC       ,BUFMAT   ,PARTSAV    ,NLOC_DMG,
     5        DT2T    ,NELTST    ,ITYPTST  ,STIFN      ,FSKY    ,
     6        IADS    ,OFFSET    ,EANI     ,IPARTS(NF1),
     7        FX(1,1) ,FY(1,1)   ,FZ(1,1)  ,FX(1,2)    ,FY(1,2) ,
     8        FZ(1,2) ,FX(1,3)   ,FY(1,3)  ,FZ(1,3)    ,FX(1,4) ,
     9        FY(1,4) ,FZ(1,4)   ,NEL      ,FSKYM      ,MSNF    ,
     A        IPM     ,IGEO      ,BID      ,ISTRA      ,ITASK   ,
     B        TEMP    ,FTHE      ,FTHESKY  ,IEXPAN     ,GRESAV  ,
     C        GRTH    ,IGRTH(NF1),MSSA(NF1),DMELS(NF1) ,TABLE   ,
     D        XDP     ,VARNOD    ,VOLN     ,CONDN      ,CONDNSKY,
     E        D       ,SENSORS   ,IPRI      ,MATPARAM_TAB,IBID  ,
     F        DT )
C
            IF (NSECT > 0) THEN 
              K0=NSTRF(25)                                             
              N=NINTER+NRWALL+NRBODY                            
              DO I=1,NSECT                                             
               N=N+1                                                   
               K2=K0+30+NSTRF(K0+14)                                   
               K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
               IPARSENS=0
               ISECT=0
               IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF (ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C               
               CALL SECTIOS4(JFT,JLT,NFT,NSTRF(K0+7),NSTRF(K0+3),      
     2          NSTRF(K0+4),NSTRF(K0+5),NSTRF(K3),X,V,FSAV(1,N),       
     3          IXS ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I),FX ,FY ,        
     4          FZ  ,NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),  
     5          NSTRF(K2),MS,                                          
     7          IXS10,ISOLNOD,XSEC(1,1,I),pFBSAV6,IPARSENS)                            
               K0=NSTRF(K0+24)                                         
              ENDDO                                                    
            ENDIF     
c---------
          ELSEIF (ISOLNOD==10 .OR. (ISOLNOD==4.AND.ISROT==1)) THEN
c---------
            CALL S10FORC3(ELBUF_TAB,NG    ,                            
     1       PM   ,GEO   ,IXS    ,X          ,                         
     2       A           ,V    ,MS    ,W      ,WA          ,           
     3       VAL2    ,VEUL   ,FV       ,ALE_CONNECT       ,IPARG       ,         
     4       TF      ,NPC    ,BUFMAT   ,PARTSAV     ,NLOC_DMG    ,    
     5       DT2T ,NELTST,ITYPTST,STIFN        ,FSKY        ,          
     6       IADS ,OFFSET,EANI     ,IPARTS(NF1) ,                      
     7       IXS10,IADS10 ,NEL    ,FX          ,                       
     8       FY   ,FZ    ,AR     ,VR           ,DR          ,          
     9       IPM     ,ISTRA  ,ISOLNOD  ,                                               
     A       ITASK   ,TEMP , FTHE , FTHESKY,IEXPAN ,                   
     B       STIFR  ,D   ,GRESAV  ,GRTH        ,IGRTH(OFF_IGRTH+NF1),  
     C       TABLE  ,MSSA(NF1),DMELS(NF1),IGEO ,XDP,
     D       VOLN    ,CONDN    ,CONDNSKY  , VARNOD, 
     E       ITAGND  ,SENSORS  ,IPRI     ,MATPARAM_TAB,IBID ,DT)                        
c
            IF(NSECT>0)THEN 
              K0=NSTRF(25)                                             
              N=NINTER+NRWALL+NRBODY                            
              DO I=1,NSECT                                             
               N=N+1                                                   
               K2=K0+30+NSTRF(K0+14)                                   
               K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
               IPARSENS=0
               ISECT=0
               IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C                
               CALL SECTIOS4(JFT,JLT,NFT,NSTRF(K0+7),NSTRF(K0+3),      
     2          NSTRF(K0+4),NSTRF(K0+5),NSTRF(K3),X,V,FSAV(1,N),       
     3          IXS ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I),FX ,FY ,        
     4          FZ  ,NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),  
     5          NSTRF(K2),MS,                                          
     7         IXS10,ISOLNOD,XSEC(1,1,I),pFBSAV6,IPARSENS)     
               K0=NSTRF(K0+24)                                         
              ENDDO                                                    
            ENDIF                                                      
c---------
          ELSEIF(ISOLNOD==20)THEN
C-------------------------
C             20 NODE SOLID
C-------------------------
              NPE = 20
              NIPMAX = 81
              L1 = 1
              L2 = L1 + MVSIZ * NIPMAX
              L3 = L2 + MVSIZ * NIPMAX
              L4 = L3 + MVSIZ * NIPMAX
              L5 = L4 + MVSIZ * NIPMAX
              L6 = L5 + MVSIZ * NIPMAX
              L7 = L6 + MVSIZ * NIPMAX
              L8 = L7 + MVSIZ * NIPMAX
              L9 = L8 + MVSIZ * NIPMAX
              L10 = L9 + MVSIZ * NIPMAX
              L11 = L10 + MVSIZ * NIPMAX
              L12 = L11
              L13 = L12 + MVSIZ * NPE
              L14 = L13 + MVSIZ * NPE
              L15 = L14 + MVSIZ * NPE
              L16 = L15 + MVSIZ * NPE
              L17 = L16 + MVSIZ * NPE
              L18 = L17 + MVSIZ * NPE
              L19 = L18 + MVSIZ * NPE
              L20 = L19 + MVSIZ * NPE
              L21 = L20 + MVSIZ * NPE
              L22 = L21 + MVSIZ * NPE
              L23 = L22 + MVSIZ * NPE
              L24 = L23 + MVSIZ * NPE
              L25 = L24 + MVSIZ * NPE
              L26 = L25 + MVSIZ * NPE
              L27 = L26 + MVSIZ * NPE
              L28 = L27 + MVSIZ * NPE * NIPMAX
              L29 = L28 + MVSIZ * NPE * NIPMAX
              L30 = L29 + MVSIZ * NPE * NIPMAX
              OFF_IGRTH = NUMELS8 + NUMELS10
c
              CALL S20FORC3(ELBUF_TAB,NG     ,
     1         PM       ,GEO      ,IXS       ,X          ,
     2         A            ,V        ,MS       ,W         ,WA         ,
     3         VAL2     ,VEUL     ,FV        ,ALE_CONNECT      ,IPARG      ,
     4         TF       ,NPC      ,BUFMAT    ,PARTSAV    ,NLOC_DMG   ,
     5         DT2T     ,NELTST   ,ITYPTST   ,STIFN      ,FSKY       ,
     6         IADS     ,OFFSET   ,EANI      ,IPARTS(NF1),
     7         IXS20    ,IADS20   ,NEL       ,FX         ,
     8         FY       ,FZ       ,W16(L1)   ,W16(L2)    ,W16(L3)    ,
     9         W16(L4)  ,W16(L5)  ,W16(L6)   ,W16(L7)    ,W16(L8)    ,
     A         W16(L9)  ,W16(L10) ,W16(L12)  ,W16(L13)   ,W16(L14)   ,
     B         W16(L15) ,W16(L16) ,W16(L17)  ,W16(L18)   ,W16(L19)   ,
     C         W16(L20) ,W16(L21) ,W16(L22)  ,W16(L23)   ,W16(L24)   ,
     D         W16(L25) ,W16(L26) ,W16(L27)  ,W16(L28)   ,W16(L29)   ,
     G         IPM      ,ISTRA    ,
     H         TEMP     ,FTHE     ,FTHESKY   ,IEXPAN     ,GRESAV     ,
     I         GRTH     ,IGRTH(OFF_IGRTH+NF1),TABLE      ,IGEO       ,
     J         VOLN     ,CONDN    ,CONDNSKY  ,
     K         ITASK    ,IPRI     ,MATPARAM_TAB,IBID     ,DT ) 
c
              IF(NSECT>0)THEN
               K0=NSTRF(25)
               N=NINTER+NRWALL+NRBODY
               DO I=1,NSECT
                N=N+1
                K2=K0+30+NSTRF(K0+14)
                K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
                IPARSENS=0
                ISECT=0
                IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
                CALL SECTIOS(JFT,JLT,NFT,NSTRF(K0+7),NSTRF(K0+3),
     2        NSTRF(K0+4),NSTRF(K0+5),NSTRF(K3),X,V,FSAV(1,N),
     3        IXS        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I),FX ,FY ,
     4        FZ   ,NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     5        NSTRF(K2),MS,
     7        IXS20,IXS16,ISOLNOD,XSEC(1,1,I),pFBSAV6,IPARSENS)
                K0=NSTRF(K0+24)
               ENDDO
              ENDIF
c---------
          ELSEIF(ISOLNOD==16)THEN
C-------------------------
C             16 NODE THICK SHELLS
C-------------------------
              NPE = 16
              NIPMAX = 81
              L1 = 1
              L2 = L1 + MVSIZ * NIPMAX
              L3 = L2 + MVSIZ * NIPMAX
              L4 = L3 + MVSIZ * NIPMAX
              L5 = L4 + MVSIZ * NIPMAX
              L6 = L5 + MVSIZ * NIPMAX
              L7 = L6 + MVSIZ * NIPMAX
              L8 = L7 + MVSIZ * NIPMAX
              L9 = L8 + MVSIZ * NIPMAX
              L10 = L9 + MVSIZ * NIPMAX
              L11 = L10 + MVSIZ * NIPMAX
              L12 = L11 + MVSIZ * NIPMAX
              L13 = L12 + MVSIZ * NPE
              L14 = L13 + MVSIZ * NPE
              L15 = L14 + MVSIZ * NPE
              L16 = L15 + MVSIZ * NPE
              L17 = L16 + MVSIZ * NPE
              L18 = L17 + MVSIZ * NPE
              L19 = L18 + MVSIZ * NPE
              L20 = L19 + MVSIZ * NPE
              L21 = L20 + MVSIZ * NPE
              L22 = L21 + MVSIZ * NPE
              L23 = L22 + MVSIZ * NPE
              L24 = L23 + MVSIZ * NPE
              L25 = L24 + MVSIZ * NPE
              L26 = L25 + MVSIZ * NPE
              L27 = L26 + MVSIZ * NPE
              L28 = L27 + MVSIZ * NPE * NIPMAX
              L29 = L28 + MVSIZ * NPE * NIPMAX
              L30 = L29 + MVSIZ * NPE * NIPMAX
              OFF_IGRTH = NUMELS8 + NUMELS10 + NUMELS20

              CALL S16FORC3(ELBUF_TAB,NG     ,
     1         PM       ,GEO      ,IXS      ,X          ,
     2         A        ,V        ,MS       ,W          ,WA         ,
     3         VAL2     ,VEUL     ,FV       ,ALE_CONNECT      ,IPARG      ,
     4         TF       ,NPC      ,BUFMAT   ,PARTSAV    ,NLOC_DMG   ,
     5         DT2T     ,NELTST   ,ITYPTST  ,STIFN      ,FSKY       ,
     6         IADS     ,OFFSET   ,EANI     ,IPARTS(NF1),
     7         IXS16    ,IADS16   ,NEL      ,FX         ,
     8         FY       ,FZ       ,W16(L1)  ,W16(L2)    ,W16(L3)    ,
     9         W16(L4)  ,W16(L5)  ,W16(L6)  ,W16(L7)    ,W16(L8)    ,
     A         W16(L9)  ,W16(L10) ,W16(L11) ,W16(L12)   ,W16(L13)   ,
     B         W16(L14) ,W16(L15) ,W16(L16) ,W16(L17)   ,W16(L18)   ,
     C         W16(L19) ,W16(L20) ,W16(L21) ,W16(L22)   ,W16(L23)   ,
     D         W16(L24) ,W16(L25) ,W16(L26) ,W16(L27)   ,W16(L28)   ,
     E         W16(L29) ,ICP      ,ICS      ,
     F         IPM      ,ISTRA    ,TEMP     ,FTHE       , FTHESKY,
     G         IEXPAN   ,GRESAV   ,GRTH ,IGRTH(OFF_IGRTH + NF1),TABLE,
     H         IGEO     ,VOLN     ,CONDN    ,CONDNSKY   ,ITASK,IPRI  ,
     I         MATPARAM_TAB,IBID  ,DT       )   
c
              IF(NSECT>0)THEN

               K0=NSTRF(25)
               N=NINTER+NRWALL+NRBODY
               DO I=1,NSECT
                N=N+1
                K2=K0+30+NSTRF(K0+14)
                K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
                IPARSENS=0
                ISECT=0
                IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
                CALL SECTIOS(JFT,JLT,NFT,NSTRF(K0+7),NSTRF(K0+3),
     2        NSTRF(K0+4),NSTRF(K0+5),NSTRF(K3),X,V,FSAV(1,N),
     3        IXS        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I),FX ,FY ,
     4        FZ   ,NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     5        NSTRF(K2),MS,
     7        IXS20,IXS16,ISOLNOD,XSEC(1,1,I),pFBSAV6,IPARSENS)
                K0=NSTRF(K0+24)
               ENDDO
              ENDIF
C-------------------------------------------------------------------7---------8
          ELSEIF(JHBE == 15)THEN
C-------------------------
C             THICK SHELLS HQEPH
C-------------------------
            IF(ISOLNOD==6)THEN
C              6 node penta
              CALL S6CFORC3(ELBUF_TAB,NG         ,
     1         PM   ,GEO   ,IXS    ,X            ,
     2         A           ,V    ,MS    ,W      ,WA          ,
     3         VAL2 ,VEUL  ,FV     ,ALE_CONNECT        ,IPARG       ,
     4         TF    ,NPC    ,BUFMAT       ,PARTSAV     ,
     5         DT2T ,NELTST,ITYPTST,STIFN        ,FSKY        ,
     6         IADS    ,OFFSET  ,EANI    ,IPARTS(NF1) ,
     7         FX(1,1) ,FY(1,1) ,FZ(1,1) ,FX(1,2) ,FY(1,2) ,
     8         FZ(1,2) ,FX(1,3) ,FY(1,3) ,FZ(1,3) ,FX(1,4) ,
     9         FY(1,4) ,FZ(1,4) ,FX(1,5) ,FY(1,5) ,FZ(1,5) ,
     A         FX(1,6) ,FY(1,6) ,FZ(1,6) ,NEL     ,
     B         ICP     ,ICS     ,NLOC_DMG,
     C         IPM     ,ISTRA   ,IGEO    ,GRESAV  ,GRTH       ,
     D         IGRTH(NF1),TABLE ,MSSA(NF1),DMELS(NF1)  ,VOLN  ,
     E         ITASK ,IPRI      ,MATPARAM_TAB,IBID,TEMP       ,
     F         FTHE  ,FTHESKY   ,CONDN    ,CONDNSKY ,IEXPAN,
     G         IFTHE ,ICONDN    ,DT      )
c
              IF(NSECT>0)THEN
               K0=NSTRF(25)
               N=NINTER+NRWALL+NRBODY
               DO I=1,NSECT
                N=N+1
                K2=K0+30+NSTRF(K0+14)
                K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
                IPARSENS=0
                ISECT=0
                IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
                CALL SECTIOS6(JFT, JLT, NFT, NSTRF(K0+7), NSTRF(K0+3),
     2           NSTRF(K0+4), NSTRF(K0+5), NSTRF(K3), X, V,
     3           FSAV(1,N), IXS, FANI(1,1+2*(I-1)), SECFCUM(1,1,I), FX,
     4           FY, FZ, NSTRF(K0), NSTRF(K0+14), NSTRF(K0+26),
     5           NSTRF(K0+6), NSTRF(K2), MS,XSEC(1,1,I),
     6           pFBSAV6,IPARSENS)
                K0=NSTRF(K0+24)
               ENDDO
              ENDIF
c---------
            ELSE
c---------
C              8 node hexa
              CALL SCFORC3(ELBUF_TAB,NG          ,
     1         PM       ,GEO      ,IXS     ,X           ,
     2         A        ,V        ,MS      ,W           ,WA      ,
     3         VAL2     ,VEUL     ,FV      ,ALE_CONNECT       ,IPARG     ,
     4         TF       ,NPC      ,BUFMAT  ,PARTSAV     ,NLOC_DMG  ,
     5         DT2T     ,NELTST   ,ITYPTST ,STIFN       ,FSKY    ,
     6         IADS     ,OFFSET   ,EANI    ,IPARTS(NF1) ,
     7         FX(1,1)  ,FY(1,1)  ,FZ(1,1) ,FX(1,2)     ,FY(1,2) ,
     8         FZ(1,2)  ,FX(1,3)  ,FY(1,3) ,FZ(1,3)     ,FX(1,4) ,
     9         FY(1,4)  ,FZ(1,4)  ,FX(1,5) ,FY(1,5)     ,FZ(1,5) ,
     A         FX(1,6)  ,FY(1,6)  ,FZ(1,6) ,FX(1,7)     ,FY(1,7) ,
     B         FZ(1,7)  ,FX(1,8)  ,FY(1,8) ,FZ(1,8)     ,NEL     ,
     C         ICP      ,ICS      ,NVC     ,
     D         IPM      ,ISTRA    ,TEMP    ,FTHE        ,FTHESKY   ,
     E         IEXPAN   ,IGEO     ,GRESAV  ,GRTH        ,IGRTH(NF1),
     F         MSSA(NF1),DMELS(NF1),TABLE  ,XDP         ,VOLN      ,
     G         CONDN    ,CONDNSKY ,ITASK   ,IPRI        ,MATPARAM_TAB,
     H         IBID     ,DT       )
C
            ENDIF
C
            IF (NSECT > 0)THEN
              K0=NSTRF(25)
              N=NINTER+NRWALL+NRBODY
              DO I=1,NSECT
                N=N+1
                K2=K0+30+NSTRF(K0+14)
                K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
                IPARSENS=0
                ISECT=0
                IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
                CALL SECTIOS(JFT,JLT,NFT,NSTRF(K0+7),NSTRF(K0+3),
     2        NSTRF(K0+4),NSTRF(K0+5),NSTRF(K3),X,V,FSAV(1,N),
     3        IXS        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I),FX ,FY ,
     4        FZ   ,NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     5        NSTRF(K2),MS,
     7        IXS20,IXS16,ISOLNOD,XSEC(1,1,I),pFBSAV6,IPARSENS)
                K0=NSTRF(K0+24)
              ENDDO
            ENDIF
          ELSEIF (JHBE == 17) THEN
C------------------------- Isolid=19 for Salim
            IF (IPARG(36,NG)==3) THEN
!            IF (IMPL_S /= 0) THEN
             CALL S8SFORC3(ELBUF_TAB,NG   ,
     1         PM         ,GEO      ,IXS      ,X          ,
     2         A          ,V        ,MS       ,W          ,WA        ,
     3         VAL2       ,VEUL     ,FV       ,ALE_CONNECT      ,IPARG     ,
     4         TF         ,NPC      ,BUFMAT   ,PARTSAV    ,NLOC_DMG  ,
     5         DT2T       ,NELTST   ,ITYPTST  ,STIFN      ,FSKY      ,
     6         IADS       ,OFFSET   ,EANI     ,IPARTS(NF1),ICP       ,
     7         FX(1,1)    ,FY(1,1)  ,FZ(1,1)  ,FX(1,2)    ,FY(1,2)   ,
     8         FZ(1,2)    ,FX(1,3)  ,FY(1,3)  ,FZ(1,3)    ,FX(1,4)   ,
     9         FY(1,4)    ,FZ(1,4)  ,FX(1,5)  ,FY(1,5)    ,FZ(1,5)   ,
     A         FX(1,6)    ,FY(1,6)  ,FZ(1,6)  ,FX(1,7)    ,FY(1,7)   ,
     B         FZ(1,7)    ,FX(1,8)  ,FY(1,8)  ,FZ(1,8)    ,NEL       ,
     C         NVC        ,IPM      ,ITASK    ,ISTRA      ,
     D         TEMP       ,FTHE     ,FTHESKY  ,IEXPAN     ,GRESAV    ,
     E         GRTH       ,IGRTH(NF1),MSSA(NF1),DMELS(NF1),TABLE     ,
     F         IGEO       ,XDP      ,VOLN      ,CONDN    ,CONDNSKY   ,
     G         D          ,IPRI     ,MATPARAM_TAB,IBID)
            ELSE
C-------------------------
C             SOLID HE8<->HE8
C-------------------------
            CALL S8EFORC3(ELBUF_TAB,NG   ,
     1         PM         ,GEO      ,IXS      ,X          ,
     2         A          ,V        ,MS       ,W          ,WA        ,
     3         VAL2       ,VEUL     ,FV       ,ALE_CONNECT      ,IPARG     ,
     4         TF         ,NPC      ,BUFMAT   ,PARTSAV    ,NLOC_DMG  ,
     5         DT2T       ,NELTST   ,ITYPTST  ,STIFN      ,FSKY      ,
     6         IADS       ,OFFSET   ,EANI     ,IPARTS(NF1),ICP       ,
     7         FX(1,1)    ,FY(1,1)  ,FZ(1,1)  ,FX(1,2)    ,FY(1,2)   ,
     8         FZ(1,2)    ,FX(1,3)  ,FY(1,3)  ,FZ(1,3)    ,FX(1,4)   ,
     9         FY(1,4)    ,FZ(1,4)  ,FX(1,5)  ,FY(1,5)    ,FZ(1,5)   ,
     A         FX(1,6)    ,FY(1,6)  ,FZ(1,6)  ,FX(1,7)    ,FY(1,7)   ,
     B         FZ(1,7)    ,FX(1,8)  ,FY(1,8)  ,FZ(1,8)    ,NEL       ,
     C         NVC        ,IPM      ,ITASK    ,ISTRA      ,
     D         TEMP       ,FTHE     ,FTHESKY  ,IEXPAN     ,GRESAV    ,
     E         GRTH       ,IGRTH(NF1),MSSA(NF1),DMELS(NF1),TABLE     ,
     F         IGEO       ,XDP      ,VOLN      ,CONDN    ,CONDNSKY   ,
     G         D          ,SENSORS  ,IPRI     ,MATPARAM_TAB,IBID,DT )
            ENDIF !(IPARG(36,NG)==3)   (IMPL_S /= 0)
c
            IF (NSECT > 0) THEN
              K0=NSTRF(25)
              N=NINTER+NRWALL+NRBODY
              DO I=1,NSECT
                N=N+1
                K2=K0+30+NSTRF(K0+14)
                K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
                IPARSENS=0
                ISECT=0
                IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
                CALL SECTIOS(JFT,JLT,NFT,NSTRF(K0+7),NSTRF(K0+3),
     2           NSTRF(K0+4),NSTRF(K0+5),NSTRF(K3),X,V,FSAV(1,N),
     3           IXS        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I),FX ,FY ,
     4           FZ   ,NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     5           NSTRF(K2),MS,
     7          IXS20,IXS16,ISOLNOD,XSEC(1,1,I),
     8          pFBSAV6,IPARSENS)
                K0=NSTRF(K0+24)
              ENDDO
            ENDIF
          ELSEIF (JHBE == 14 .AND. 
     .           (IGTYP == 20 .OR. IGTYP == 21 .OR. IGTYP == 22)) THEN
C-------------------------
C           THICK SHELL HA8
C-------------------------
            NIPMAX = 729                      
            L1  = 1                           
            L2  = L1  + MVSIZ * NIPMAX         
            L3  = L2  + MVSIZ * NIPMAX         
            L4  = L3  + MVSIZ * NIPMAX         
            L5  = L4  + MVSIZ * NIPMAX         
            L6  = L5  + MVSIZ * NIPMAX         
            L7  = L6  + MVSIZ * NIPMAX         
            L8  = L7  + MVSIZ * NIPMAX         
            L9  = L8  + MVSIZ * NIPMAX         
            L10 = L9  + MVSIZ * NIPMAX         
            L11 = L10 + MVSIZ * NIPMAX        
            L12 = L11 + MVSIZ * NIPMAX             
c
            CALL S8CFORC3(ELBUF_TAB,NG    ,
     1         PM    ,GEO   ,IXS    ,X            ,
     2         A            ,V     ,MS    ,W      ,WA          ,
     3         VAL2    ,VEUL    ,FV      ,ALE_CONNECT   ,IPARG       ,
     4         TF      ,NPC     ,BUFMAT  ,PARTSAV ,NLOC_DMG    ,
     5         DT2T ,NELTST ,ITYPTST,STIFN        ,FSKY        ,
     6         IADS ,OFFSET ,EANI     ,IPARTS(NF1) ,
     7         FX(1,1) ,FY(1,1) ,FZ(1,1) ,FX(1,2) ,FY(1,2) ,
     8         FZ(1,2) ,FX(1,3) ,FY(1,3) ,FZ(1,3) ,FX(1,4) ,
     9         FY(1,4) ,FZ(1,4) ,FX(1,5) ,FY(1,5) ,FZ(1,5) ,
     A         FX(1,6) ,FY(1,6) ,FZ(1,6) ,FX(1,7) ,FY(1,7) ,
     B         FZ(1,7) ,FX(1,8) ,FY(1,8) ,FZ(1,8) ,NEL     ,
     C         ICP ,
     F         ICS      ,W16(L1)   ,W16(L2) ,W16(L3) ,W16(L4) ,
     G         W16(L5)  ,W16(L6)   ,W16(L7) ,W16(L8) ,W16(L9) ,
     H         W16(L10) ,W16(L11)  ,W16(L12),NVC     ,IPM     ,
     I         ITASK     ,ISTRA   ,TEMP    ,FTHE,
     J         FTHESKY  ,IEXPAN    ,IGEO    ,NPT     ,GRESAV  ,
     K         GRTH     ,IGRTH(NF1),MSSA(NF1),DMELS(NF1),TABLE,
     L         XDP      ,VOLN      ,CONDN    ,CONDNSKY,SENSORS,
     M         IPRI      ,MATPARAM_TAB,IBID  ,DT )
C
            IF (NSECT > 0) THEN
              K0=NSTRF(25)
              N=NINTER+NRWALL+NRBODY
              DO I=1,NSECT
                N=N+1
                K2=K0+30+NSTRF(K0+14)
                K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
                IPARSENS=0
                ISECT=0
                IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
                CALL SECTIOS(JFT,JLT,NFT,NSTRF(K0+7),NSTRF(K0+3),
     2           NSTRF(K0+4),NSTRF(K0+5),NSTRF(K3),X,V,FSAV(1,N),
     3           IXS        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I),FX ,FY ,
     4           FZ   ,NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     5           NSTRF(K2),MS,
     7          IXS20,IXS16,ISOLNOD,XSEC(1,1,I),
     8          pFBSAV6,IPARSENS)
                K0=NSTRF(K0+24)
              ENDDO
            ENDIF
          ELSEIF (JHBE == 14 .OR. JHBE == 222) THEN
C-------------------------
C             SOLID HA8
C-------------------------
              NIPMAX = 729
              L1 = 1
              L2 = L1 + MVSIZ * NIPMAX
              L3 = L2 + MVSIZ * NIPMAX
              L4 = L3 + MVSIZ * NIPMAX
              L5 = L4 + MVSIZ * NIPMAX
              L6 = L5 + MVSIZ * NIPMAX
              L7 = L6 + MVSIZ * NIPMAX
              L8 = L7 + MVSIZ * NIPMAX
              L9 = L8 + MVSIZ * NIPMAX
              L10 = L9 + MVSIZ * NIPMAX
              L11 = L10 + MVSIZ * NIPMAX
              L12 = L11 + MVSIZ * NIPMAX
c
c            MPT =IABS(NPT)
c            NPTS=MPT/100
c            IF (NPTS==0) NPTS=IINT
c            NPTT=MOD(MPT/10,10)
c            IF (NPTT==0) NPTT=IINT
c            NPTR=MOD(MPT,10)
c            IF (NPTR==0) NPTR=IINT

            NPT0 = IPARG(6,NG)
            
            CALL S8ZFORC3(ELBUF_TAB,NG     ,
     1         PM         ,GEO       ,IXS      ,X          ,        
     2         A          ,V         ,MS       ,W          ,WA         , 
     3         VAL2       ,VEUL      ,FV       ,ALE_CONNECT      ,IPARG      , 
     4         TF         ,NPC       ,BUFMAT   ,PARTSAV    ,NLOC_DMG   , 
     5         DT2T       ,NELTST    ,ITYPTST  ,STIFN      ,FSKY       ,
     6         IADS       ,OFFSET    ,EANI     ,IPARTS(NF1),ICP        ,
     7         FX(1,1)    ,FY(1,1)   ,FZ(1,1)  ,FX(1,2)    ,FY(1,2)    ,
     8         FZ(1,2)    ,FX(1,3)   ,FY(1,3)  ,FZ(1,3)    ,FX(1,4)    ,
     9         FY(1,4)    ,FZ(1,4)   ,FX(1,5)  ,FY(1,5)    ,FZ(1,5)    ,
     A         FX(1,6)    ,FY(1,6)   ,FZ(1,6)  ,FX(1,7)    ,FY(1,7)    ,
     B         FZ(1,7)    ,FX(1,8)   ,FY(1,8)  ,FZ(1,8)    ,NEL        ,
     F         ICS        ,W16(L1)   ,W16(L2)  ,W16(L3)    ,W16(L4)    ,
     G         W16(L5)    ,W16(L6)   ,W16(L7)  ,W16(L8)    ,W16(L9)    ,
     H         W16(L10)   ,W16(L11)  ,W16(L12) ,NVC        ,IPM        ,
     I         ITASK     ,ISTRA    ,TEMP       ,FTHE       ,
     J         FTHESKY    ,IEXPAN    ,IGEO     ,NPT0       ,GRESAV     ,
     K         GRTH       ,IGRTH(NF1),MSSA(NF1),DMELS(NF1) ,TABLE      ,
     L         XDP        ,VOLN      ,CONDN    ,CONDNSKY   ,
     M         D          ,SENSORS   ,IPRI       ,MATPARAM_TAB,IBID,DT )
C
            IF (NSECT > 0) THEN
              K0=NSTRF(25)
              N=NINTER+NRWALL+NRBODY
              DO I=1,NSECT
                N=N+1
                K2=K0+30+NSTRF(K0+14)
                K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
                IPARSENS=0
                ISECT=0
                IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
                CALL SECTIOS(JFT,JLT,NFT,NSTRF(K0+7),NSTRF(K0+3),
     2           NSTRF(K0+4),NSTRF(K0+5),NSTRF(K3),X,V,FSAV(1,N),
     3           IXS        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I),FX ,FY ,
     4           FZ   ,NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     5           NSTRF(K2),MS,
     7          IXS20,IXS16,ISOLNOD,XSEC(1,1,I),
     8          pFBSAV6,IPARSENS)
                K0=NSTRF(K0+24)
              ENDDO
            ENDIF
C----6---------------------------------------------------------------7---------8
          ELSEIF(IGTYP>=29)THEN
!
            IF (ISENS_ENERGY == 1 .AND. IPRI == 0)
     .       CALL SENSOR_ENERGY_PART(IPARTS(NF1) ,SUBSET ,IPRI)
C-------------------------
C           USER SOLID PROPERTY
C-------------------------
            CALL SUFORC3(ELBUF_TAB(NG),
     1           JFT     ,JLT     ,NFT     ,NEL     ,IXS      ,
     2           PM      ,GEO     ,IPM     ,IGEO    ,X        ,
     3           A       ,AR      ,V       ,VR      ,W        ,
     4           D       ,MS      ,IN      ,TF      ,NPC      ,
     5           BUFMAT  ,IPARG   ,IPARTS(NF1),PARTSAV ,
     6           FSKY    ,FR_WAVE ,IADS    ,EANI    ,STIFN    ,
     7           STIFR   ,FX      ,FY      ,FZ      ,IFAILURE ,
     8           MTN     ,IGTYP   ,NPT     ,JSMS    ,MSSA(NF1),
     9           DMELS(NF1),ITASK ,IPRI    ,JTHE    ,TABLE )
C--------------------------
          ELSEIF (NPT == 1)THEN
C--------------------------
            IF (JHBE == 24) THEN
C-------------------------
C              SOLID HEPH FORMULATION
C-------------------------
               IHET = IINT
C
               CALL SZFORC3(ELBUF_TAB,NG    ,
     1         PM      ,GEO    ,IXS    ,X            ,
     2         A       ,V     ,MS     ,W      ,WA          ,
     3         VAL2      ,VEUL      ,FV         ,ALE_CONNECT      ,IPARG     ,  
     4         TF      ,NPC    ,BUFMAT       ,PARTSAV     ,
     5         DT2T    ,NELTST,ITYPTST,STIFN        ,FSKY        ,
     6         IADS    ,OFFSET,EANI     ,IPARTS(NF1) ,ICP,
     7         FX(1,1) ,FY(1,1) ,FZ(1,1) ,FX(1,2) ,FY(1,2) ,
     8         FZ(1,2) ,FX(1,3) ,FY(1,3) ,FZ(1,3) ,FX(1,4) ,
     9         FY(1,4) ,FZ(1,4) ,FX(1,5) ,FY(1,5) ,FZ(1,5) ,
     A         FX(1,6) ,FY(1,6) ,FZ(1,6) ,FX(1,7) ,FY(1,7) ,
     B         FZ(1,7) ,FX(1,8) ,FY(1,8) ,FZ(1,8) ,NEL     ,
     C         FSKYM   ,MSNF    ,NVC     ,IPM     ,ITASK  ,
     D         QMVBID  ,ISTRA   ,IMATVIS ,TEMP    ,FTHE   ,
     E         FTHESKY ,IEXPAN  ,GRESAV  ,GRTH    ,IGRTH(NF1),
     F         MSSA(NF1),DMELS(NF1),TABLE,IGEO    ,XDP    ,
     G         CONDN     ,CONDNSKY  ,
     H         D         ,TAGPRT_SMS,SENSORS  ,IPRI      ,
     I         NALE      ,NLOC_DMG  ,MATPARAM_TAB,IBID,
     J         DT, OUTPUT)
C
            ELSE
C-------------------------
C             STANDARD 8 NODE SOLID ELEMENT (JHBE = 1,2,101,102)
C-------------------------
              NV46   = 6
              IF(N2D /= 0) NV46 = 4
              !
              CALL SFORC3(
     1         ELBUF_TAB   ,NG         ,
     2        PM           ,GEO        ,IXS         ,X  	       ,NV46      ,
     3        A            ,V          ,MS        ,W           ,WA        ,
     4        VAL2         ,VEUL       ,FV          ,ALE_CONNECT	     ,IPARG     ,
     5        TF           ,NPC        ,BUFMAT    ,PARTSAV     ,ITAB      ,
     6        DT2T         ,NELTST     ,ITYPTST   ,STIFN       ,FSKY      ,
     7        IADS         ,OFFSET     ,EANI      ,IPARTS(NF1) ,
     8        FX(1,1)      ,FY(1,1)    ,FZ(1,1)   ,FX(1,2)     ,FY(1,2)   ,        
     9        FZ(1,2)      ,FX(1,3)    ,FY(1,3)   ,FZ(1,3)     ,FX(1,4)   ,        
     A        FY(1,4)      ,FZ(1,4)    ,FX(1,5)   ,FY(1,5)     ,FZ(1,5)   ,        
     B        FX(1,6)      ,FY(1,6)    ,FZ(1,6)   ,FX(1,7)     ,FY(1,7)   ,        
     C        FZ(1,7)      ,FX(1,8)    ,FY(1,8)   ,FZ(1,8)     ,NEL       ,        
     D        FSKYM        ,MSNF       ,IBID      ,BID         ,                   
     E        NVC          ,IPM        ,IGEO      ,AR          ,VR        ,        
     F        IN           ,FR_WAVE    ,DR        ,BID         ,        
     G        ITASK        ,QMVBID     ,ISTRA     ,TEMP        ,FTHE      ,        
     H        FTHESKY      ,IEXPAN     ,GRESAV    ,GRTH        ,IGRTH(NF1),        
     I        MSSA(NF1)    ,DMELS(NF1) ,TABLE     ,PHI1        ,PHI2      ,        
     J        VF           ,AF         ,DF        ,WF          ,FFSKY     ,        
     K        AFGLOB       ,MSF        ,IPARG(1,NG) ,XDP        ,
     M        POR          ,ICONTACT   ,IFOAM     ,VOLN        ,CONDN     ,
     N        CONDNSKY     ,AGRAV      ,IGRV      ,LGRAV       ,SENSORS   ,
     O        SKEW         ,NALE       ,D         ,IPRI        ,NLOC_DMG  ,
     Q        MATPARAM_TAB ,IBID       ,DT       )

            ENDIF
C
            IF (NSECT > 0)THEN
               K0=NSTRF(25)
               N=NINTER+NRWALL+NRBODY
               DO I=1,NSECT
                N=N+1
                K2=K0+30+NSTRF(K0+14)
                K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
                IPARSENS=0
                ISECT=0
                IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
                CALL SECTIOS(JFT,JLT,NFT,NSTRF(K0+7),NSTRF(K0+3),
     2           NSTRF(K0+4),NSTRF(K0+5),NSTRF(K3),X,V,FSAV(1,N),
     3           IXS        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I),FX ,FY ,
     4           FZ   ,NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     5           NSTRF(K2),MS,
     7           IXS20,IXS16,ISOLNOD,XSEC(1,1,I),
     8           pFBSAV6,IPARSENS)
                K0=NSTRF(K0+24)
               ENDDO
            ENDIF
          ELSEIF (NPT == 8 .AND. MTN /= 0) THEN
C-------------------------
C             8 NODE SOLID ELEMENT (JHBE = 12,112), 8 integration points (old)
C-------------------------
            CALL S8FORC3(
     1      ELBUF_TAB(NG),PM           ,GEO      ,IXS      ,X         ,           
     2      A          ,V            ,MS       ,                       
     3      VEUL         ,FV       ,ALE_CONNECT     ,IPARG      ,                     
     4      TF       ,NPC      ,BUFMAT    ,PARTSAV    ,             
     5      STIFN        ,FSKY     ,IADS     ,OFFSET    ,IPARTS(NF1),           
     6      NEL          ,DT2T     ,NELTST   ,ITYPTST   ,IPM        ,           
     7      ITASK        ,GRESAV   ,GRTH     ,IGRTH(NF1),MSSA(NF1)  ,           
     8      DMELS(NF1)   ,TABLE,IPRI     ,
     9      NG           )     
                                                 
            IF (NSECT > 0)THEN
               K0=NSTRF(25)
               N=NINTER+NRWALL+NRBODY
               DO I=1,NSECT
                N=N+1
                K2=K0+30+NSTRF(K0+14)
                K3=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
                IPARSENS=0
                ISECT=0
                IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
                CALL SECTIOS(JFT,JLT,NFT,NSTRF(K0+7),NSTRF(K0+3),
     2           NSTRF(K0+4),NSTRF(K0+5),NSTRF(K3),X,V,FSAV(1,N),
     3           IXS        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I),FX ,FY ,
     4           FZ   ,NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     5           NSTRF(K2),MS,
     7           IXS20,IXS16,ISOLNOD,XSEC(1,1,I),
     8           pFBSAV6,IPARSENS)
                K0=NSTRF(K0+24)
               ENDDO
            ENDIF
          ENDIF
C----6---------------------------------------------------------------7---------8
        ELSEIF(ITY==2.AND.JMULT==0.AND.JLAG==1.AND.
     .         ICRACK==0) THEN
             OFF_IGRTH = NUMELS
!
          IF (ISENS_ENERGY == 1 .AND. IPRI == 0)
     .     CALL SENSOR_ENERGY_PART(IPARTQ(NF1) ,SUBSET ,IPRI)
          IF(JHBE==17 .OR. (N2D==1.AND.JHBE==22)) THEN
          
            CALL Q4FORC2(
     1        PM   ,GEO   ,IXQ    ,X         ,A           ,
     2        V    ,MS    ,W      ,WA        ,VAL2 ,
     3        VEUL  ,FV     ,ALE_CONNECT     ,IPARG     ,NLOC_DMG,
     4        ELBUF_TAB,TF    ,NPC    ,BUFMAT    ,PARTSAV     ,
     5        DT2T ,NELTST,ITYPTST,STIFN     ,OFFSET      ,
     6        EANI ,IPARTQ(NF1),NEL,IADQ     ,FSKY        ,
     7        IPARG(10,NG),NG    ,
     8        IPM         ,BID   ,QMVBID  ,GRESAV   ,GRTH        ,
     9        IGRTH(OFF_IGRTH+NF1),TABLE     ,IGEO      ,ITASK   ,IEXPAN,
     A        MS_2D      ,FSKYM   ,IPRI      ,MATPARAM_TAB,IBID)
          ELSE

            CALL QFORC2(ELBUF_TAB,NG   ,
     1        PM      ,GEO    ,IXQ     ,X         ,A           ,
     2        V       ,MS     ,W       ,WA        ,VAL2        ,
     3        VEUL    ,FV     ,ALE_CONNECT   ,IPARG     ,NLOC_DMG    ,
     4        TF      ,NPC    ,BUFMAT  ,PARTSAV   ,
     5        DT2T    ,NELTST ,ITYPTST ,STIFN     ,OFFSET      ,
     6        EANI    ,IPARTQ(NF1),NEL ,IADQ      ,FSKY        ,
     9        IPM     ,BID    ,QMVBID       ,
     A        GRESAV  ,GRTH   ,IGRTH(OFF_IGRTH+NF1),TABLE,IGEO ,
     B        VOLN    ,ITASK  ,MS_2D   ,FSKYM     ,IPRI        ,
     C        MATPARAM_TAB,H3D_DATA%STRAIN)

          ENDIF
C----6---------------------------------------------------------------7---------8
        ELSEIF (ITY == 4) THEN
             K1=1 + 6*(NUMELC+NUMELTG)*IEPSDOT+15*NFT
             OFF_IGRTH = NUMELS+NUMELQ+NUMELC
C
!
          IF (ISENS_ENERGY == 1 .AND. IPRI == 0)
     .     CALL SENSOR_ENERGY_PART(IPARTT(NF1) ,SUBSET ,IPRI)
c
          CALL TFORC3(
     1   ELBUF_TAB(NG),       JFT,                 JLT,                 PM,
     2   GEO,                 IXT,                 X,                   A,
     3   V,                   PARTSAV,             BUFMAT,              DT2T,
     4   NELTST,              ITYPTST,             STIFN,               FSKY,
     5   IADT,                OFFSET,              IPARTT(NF1),         TANI(K1),
     6   FX(1,1),             FX(1,2),             FY(1,1),             FY(1,2),
     7   FZ(1,1),             FZ(1,2),             NEL,                 GRESAV,
     8   GRTH,                IGRTH(OFF_IGRTH+NF1),MSTR(NF1),           DMELTR(NF1),
     9   IPRI,                IPM,                 NPC,                 TF,
     A   ITASK,               H3D_DATA,            NFT,                 MTN,
     B   JSMS,                IGRE)
C
             IF (NSECT > 0) THEN
              K0=NSTRF(25)
            N=NINTER+NRWALL+NRBODY
              DO I=1,NSECT
               N=N+1
               K2=K0+30+NSTRF(K0+14)
               K6=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
     1           +2*NSTRF(K0+7)+2*NSTRF(K0+8)+2*NSTRF(K0+9)
               IPARSENS=0
               ISECT=0
               IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
               CALL SECTIOT(JFT,JLT,NFT,NSTRF(K0+10),NSTRF(K0+3),
     2          NSTRF(K0+4),NSTRF(K0+5),NSTRF(K6),X,V,VR,FSAV(1,N),
     3          IXT        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I) ,
     4          FX ,FY ,FZ ,NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),
     5          NSTRF(K0+6),NSTRF(K2) ,MS,
     6                 XSEC(1,1,I), pFBSAV6,IPARSENS)
C
               K0=NSTRF(K0+24)
            ENDDO ! DO I=1,NSECT
          ENDIF ! IF (NSECT > 0)
C----6---------------------------------------------------------------7---------8
        ELSEIF (ITY == 5) THEN
             OFF_IGRTH = NUMELS+NUMELQ+NUMELC+NUMELT
             K1=1 + 6*(NUMELC+NUMELTG)*IEPSDOT + 15*(NUMELT+NFT)
C
!
          IF (ISENS_ENERGY == 1 .AND. IPRI == 0)
     .     CALL SENSOR_ENERGY_PART(IPARTP(NF1) ,SUBSET ,IPRI)
          CALL PFORC3(
     1   ELBUF_TAB(NG),       JFT,                 JLT,                 NEL,
     2   MTN,                 ISMSTR,              PM,                  IXP(1,NF1),
     3   X,                   A,                   AR,                  V,
     4   VR,                  GEO,                 PARTSAV,             DT2T,
     5   NELTST,              ITYPTST,             STIFN,               STIFR,
     6   FSKY,                IADP,                OFFSET,              IPARTP(NF1),
     7   TANI(K1),            FX(1,1),             FX(1,2),             FY(1,1),
     8   FY(1,2),             FZ(1,1),             FZ(1,2),             MX(1,1),
     9   MX(1,2),             MY(1,1),             MY(1,2),             MZ(1,1),
     A   MZ(1,2),             IGEO,                IPM,                 BUFMAT,
     B   NPT,                 NPC,                 TF,                  GRESAV,
     C   GRTH,                IGRTH(OFF_IGRTH+NF1),MSP(NF1),            DMELP(NF1),
     D   IPRI,                ITASK,               JTHE,                TEMP,
     E   FTHE,                FTHESKY,             IEXPAN,              H3D_DATA,
     F   JSMS,                IGRE,                NFT)
C
             IF (NSECT > 0) THEN
              K0=NSTRF(25)
              N=NINTER+NRWALL+NRBODY
              DO I=1,NSECT
               N=N+1
               K2=K0+30+NSTRF(K0+14)
               K7=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
     1           +2*NSTRF(K0+7)+2*NSTRF(K0+8)+2*NSTRF(K0+9)
     2           +2*NSTRF(K0+10)
               IPARSENS=0
               ISECT=0
               IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
               CALL SECTIOP(JFT,JLT,NFT,NSTRF(K0+11),NSTRF(K0+3),
     2          NSTRF(K0+4),NSTRF(K0+5),NSTRF(K7),X,V,VR,FSAV(1,N),
     3          IXP        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I) ,
     4          FX ,FY ,FZ  ,MX  ,MY,  MZ,
     5          NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     6          NSTRF(K2),MS,
     8                  XSEC(1,1,I),pFBSAV6,IPARSENS)
               K0=NSTRF(K0+24)
            ENDDO ! DO I=1,NSECT
          ENDIF ! IF (NSECT > 0)
C----6---------------------------------------------------------------7---------8
        ELSEIF (ITY == 6) THEN
             K1=1 + 6*(NUMELC+NUMELTG)*IEPSDOT + 15*(NUMELT+NUMELP+NFT)
             OFF_IGRTH = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP
c
          IF (ISENS_ENERGY == 1 .AND. IPRI == 0)
     .     CALL SENSOR_ENERGY_PART(IPARTR(NF1) ,SUBSET ,IPRI)
         IF(IGTYP == 23) THEN
              CALL R23FORC3(
     1   ELBUF_TAB(NG),        JFT,                  JLT,                  NEL,
     2   MTN,                  IGEO,                 GEO,                  IPM,
     3   IXR(1,NF1),           X,                    TABLE,                XDP,
     4   A,                    NPC,                  TF,                   SKEW,
     5   FLG_KJ2,              VR,                   AR,                   V,
     6   DT2T,                 NELTST,               ITYPTST,              STIFN,
     7   STIFR,                MS,                   IN,                   FSKY,
     8   IADR,                 SENSORS   ,           OFFSET,               ANIN,
     9   PARTSAV,              IPARTR(NF1),          TANI(K1),             FR_WAVE,
     A   BUFMAT,               BUFGEO,               PM,                   RBY,
     B   FX(1,1),              FX(1,2),              FY(1,1),              FY(1,2),
     C   FZ(1,1),              FZ(1,2),              MX(1,1),              MX(1,2),
     D   MY(1,1),              MY(1,2),              MZ(1,1),              MZ(1,2),
     E   GRESAV,               GRTH,                 IGRTH(OFF_IGRTH+NF1), MSRT(NF1),
     F   DMELRT(NF1),          FLAG_SLIPRING_UPDATE, FLAG_RETRACTOR_UPDATE,H3D_DATA,
     G   JSMS,                 IGRE,                 NFT)
         ELSE
          CALL RFORC3(
     1   ELBUF_TAB(NG),       JFT,                 JLT,                 NEL,
     2   MTN,                 IGEO,                GEO,                 IXR(1,NF1),
     3   X,                   TABLE,               XDP,                 A,
     4   NPC,                 TF,                  SKEW,                FLG_KJ2,
     5   VR,                  AR,                  V,                   DT2T,
     6   NELTST,              ITYPTST,             STIFN,               STIFR,
     7   MS,                  IN,                  FSKY,                IADR,
     8   SENSORS   ,          OFFSET,              ANIN,                PARTSAV,
     9   IPARTR(NF1),         TANI(K1),            FR_WAVE,             BUFMAT,
     A   BUFGEO,              PM,                  RBY,                 FX(1,1),
     B   FX(1,2),             FY(1,1),             FY(1,2),             FZ(1,1),
     C   FZ(1,2),             MX(1,1),             MX(1,2),             MY(1,1),
     D   MY(1,2),             MZ(1,1),             MZ(1,2),             GRESAV,
     E   GRTH,                IGRTH(OFF_IGRTH+NF1),MSRT(NF1),           DMELRT(NF1),
     F   ITASK,               H3D_DATA,            JSMS,                NFT,
     G   IAD,                 IGRE)
             
         ENDIF
C
            IF (NSECT > 0) THEN
              K0=NSTRF(25)
              N=NINTER+NRWALL+NRBODY
              DO I=1,NSECT
               N=N+1
               K2=K0+30+NSTRF(K0+14)
               K8=K0+30+NSTRF(K0+14)+NSTRF(K0+6)
     1           +2*NSTRF(K0+7)+2*NSTRF(K0+8)+2*NSTRF(K0+9)
     2           +2*NSTRF(K0+10)+2*NSTRF(K0+11)
               IPARSENS=0
               ISECT=0
               IF (SENSORS%STABSEN > 0) ISECT=SENSORS%TABSENSOR(I+1)-SENSORS%TABSENSOR(I)
                NULLIFY(pFBSAV6)
                IF(ISECT/=0) THEN
                  IPARSENS=1
                  pFBSAV6 => SENSORS%FSAV(1,1,ISECT)
                ENDIF
C
               CALL SECTIOR(JFT,JLT,NFT,NSTRF(K0+12),NSTRF(K0+3),
     2          NSTRF(K0+4),NSTRF(K0+5),NSTRF(K8),X,V,VR,FSAV(1,N),
     3          IXR        ,FANI(1,1+2*(I-1)), SECFCUM(1,1,I) ,
     4          FX ,FY ,FZ  ,MX  ,MY,  MZ,
     5          NSTRF(K0),NSTRF(K0+14),NSTRF(K0+26),NSTRF(K0+6),
     6          NSTRF(K2),MS,
     8          XSEC(1,1,I), pFBSAV6,IPARSENS)
               K0=NSTRF(K0+24)
              ENDDO
            ENDIF
C----6---------------------------------------------------------------7---------8
        ELSEIF(ITY==101)THEN
              OFF_IGRTH = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .                   NUMELR
c              K = 1+2*(NSECT+NRBODY+NRWALL)
              
            NCTRL = IPARG(75,NG)
            PX = IGEO(41,IPARG(62,NG))
            PY = IGEO(42,IPARG(62,NG))
            PZ = IGEO(43,IPARG(62,NG))
!
          IF (ISENS_ENERGY == 1 .AND. IPRI == 0)
     .     CALL SENSOR_ENERGY_PART(IPARTS(NF1) ,SUBSET ,IPRI)

            CALL IG3DUFORC3(
     1   ELBUF_TAB,           NG,                  JFT,                 JLT,
     2   NFT,                 NEL,                 IXS,                 PM,
     3   GEO,                 IPM,                 IGEO,                X,
     4   A,                   AR,                  V,                   VR,
     5   W,                   D,                   MS,                  IN,
     6   TF,                  NPC,                 BUFMAT,              IPARG,
     7   IPARTS(NF1),         PARTSAV,             NLOC_DMG,            FSKY,
     8   FR_WAVE,             IADS,                EANI,                STIFN,
     9   STIFR,               FX,                  FY,                  FZ,
     A   IFAILURE,            MTN,                 IGTYP,               NPT,
     B   JSMS,                MSSA(NF1),           DMELS(NF1),          KXIG3D,
     C   IXIG3D,              KNOT,                NCTRL,               WIGE,
     D   WA,                  VAL2,                DT2T,                NELTST,
     E   ITYPTST,             OFFSET,              TABLE,               IEXPAN,
     F   ALE_CONNECT,         FV,                  ITASK,               IPRI,
     G   PX,                  PY,                  PZ,                  KNOTLOCPC,
     H   KNOTLOCEL,           GRESAV,              GRTH,                IGRTH(OFF_IGRTH+NF1),
     I   MATPARAM_TAB,        IBID,                ISMSTR,              JALE,
     J   JEUL,                JLAG,                JCVT,                JPLASOL,
     K   JSPH)

        ENDIF
            IF (IDDW>0) CALL STOPTIMEG(NG)

C---------Nitsche Method : element mean stress storing----

        IF(NITSCHE > 0 .AND. ITY == 1) THEN
          GBUF => ELBUF_TAB(NG)%GBUF 

          DO J=1,6
            DO I=LFT,LLT 
              IF(GBUF%OFF(I) > 0) THEN            
                 STRESSMEAN(J,NFT + I) = GBUF%SIG(NEL*(J-1)+I)
              ELSE
                 STRESSMEAN(J,NFT + I) = ZERO
              ENDIF
            ENDDO 
          ENDDO

C           STRESS TENSOR IN GLOBAL SYSTEM
          IF (JCVT /= 0 .AND. JHBE /= 16) THEN
C             ROTATION IF COROTA SYSTEM
            DO I=LFT,LLT   
              NN = NFT +I
             IF(JCVT==2.AND.JHBE/=14)THEN
                GAMA(1)=GBUF%GAMA(I + I)
                GAMA(2)=GBUF%GAMA(NEL + I)
                GAMA(3)=GBUF%GAMA(2*NEL + I)
                GAMA(4)=GBUF%GAMA(3*NEL + I)
                GAMA(5)=GBUF%GAMA(4*NEL + I)
                GAMA(6)=GBUF%GAMA(5*NEL + I)
              ELSE
                GAMA(1)=ONE
                GAMA(2)=ZERO
                GAMA(3)=ZERO
                GAMA(4)=ZERO
                GAMA(5)=ONE
                GAMA(6)=ZERO
              END IF
              CALL SROTA6(
     1   X,               IXS(1,NN),       JCVT,            STRESSMEAN(1,NN),
     2   GAMA,            JHBE,            IGTYP,           ISORTH)
            ENDDO           
          ENDIF
       ENDIF
  250   CONTINUE
      END DO
C
      DEALLOCATE(FX,FY,FZ,MX,MY,MZ)

C-----------
      RETURN
      END
C-----------
      END MODULE FORINT_MOD

