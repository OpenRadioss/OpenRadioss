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
Chd|  SUFORC3                       source/user_interface/suforc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ENG_USERLIB_SUSER             source/user_interface/dyn_userlib.c
Chd|        SFILLOPT                      source/elements/solid/solide/sfillopt.F
Chd|        SUCOOR3                       source/user_interface/suforc3.F
Chd|        SUCUMU3                       source/user_interface/suforc3.F
Chd|        SUCUMU3P                      source/user_interface/suforc3.F
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SUFORC3(ELBUF_STR,
     1           LFT      ,LLT      ,NFT     ,NEL     ,IXS     ,       
     2           PM       ,GEO      ,IPM     ,IGEO    ,X       ,       
     3           A        ,AR       ,V       ,VR      ,W       ,       
     4           D        ,MS       ,IN      ,TF      ,NPF     ,       
     5           BUFMAT   ,IPARG    ,IPARTS  ,PARTSAV ,         
     6           FSKY     ,FR_WAVE  ,IADS    ,EANI    ,STIFN   ,
     7           STIFR    ,FX       ,FY      ,FZ      ,IFAILURE,
     8           MTN      ,IGTYP    ,NPT     ,JSMS    ,MSSA    ,
     9           DMELS    ,ITASK    ,IOUTPRT ,JTHE    ,TABLE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
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
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr06_c.inc"
#include      "parit_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "userlib.inc"
#include      "chara_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT, LLT,NEL,NFT,MTN,IGTYP,IFAILURE,NPT, JSMS,IOUTPRT,JTHE
      INTEGER IXS(NIXS,*), IPARG(*), NPF(*),IADS(8,*),
     .        IPARTS(*), IGEO(NPROPGI,*), IPM(NPROPMI,*),ITASK
C     REAL
C     REAL
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), X(*), A(*), V(*), MS(*), W(*),
     .   AR(*), VR(*), IN(*),D(*),TF(*), BUFMAT(*),FR_WAVE(*),
     .   PARTSAV(*),STIFN(*), STIFR(*), FSKY(*),EANI(*),
     .   FX(MVSIZ,8),FY(MVSIZ,8),FZ(MVSIZ,8),
     .   MSSA(*), DMELS(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (TTABLE)  , DIMENSION(NTABLE) ::  TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NF1,IFLAG,NUPARAM,IG,IGT,
     .   NUVAR,NUVARP,II(6)
C-----
      INTEGER IMAT(MVSIZ),SID(MVSIZ),IPROP(MVSIZ),NC(MVSIZ,8)
      my_real
     .   MX(MVSIZ,8),MY(MVSIZ,8) , MZ(MVSIZ,8),
     .   STI(MVSIZ),STIR(MVSIZ), VISCM(MVSIZ) ,VISCR(MVSIZ)
      my_real
     .   OFF(MVSIZ) , RHOO(MVSIZ),FR_W_E(MVSIZ),
     .   XX(MVSIZ,8), YY(MVSIZ,8),  ZZ(MVSIZ,8), 
     .   UX(MVSIZ,8), UY(MVSIZ,8),  UZ(MVSIZ,8),
     .   VX(MVSIZ,8), VY(MVSIZ,8),  VZ(MVSIZ,8),
     .   VRX(MVSIZ,8),VRY(MVSIZ,8),VRZ(MVSIZ,8),SIG_LOC(6,NEL),
     .   EINT_LOC(MVSIZ),VOL_LOC(MVSIZ),OFF_LOC(MVSIZ),RHO_LOC(MVSIZ)
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      my_real,
     .   DIMENSION(:),POINTER :: UVAR
!
      CHARACTER OPTION*256
      INTEGER SIZE
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF  => ELBUF_STR%GBUF
      UVAR  => ELBUF_STR%BUFLY(1)%MAT(1,1,1)%VAR
      NF1=NFT+1
!
      DO I=1,6
        II(I) = NEL*(I-1)
      ENDDO
!
C-----------
C GATHER NODAL VARIABLES
      CALL SUCOOR3(IXS(1,NF1),X  ,V,VR,W,D,FR_WAVE ,FR_W_E  ,
     .             XX ,YY ,ZZ, UX  ,UY  ,UZ , 
     .             VX ,VY ,VZ, VRX ,VRY ,VRZ, 
     .             GBUF%OFF,OFF, NC,SID,IMAT,IPROP)
      NUVAR   = IPM(8,IMAT(1))
      NUPARAM = IPM(9,IMAT(1))
C-----------
      IG =IPROP(1)
      IGT=IGEO(11,IG)
      IF (IGT.GE.29)THEN
        NUVARP=NINT(GEO(25,IG))
      ELSE
        NUVARP=0
      ENDIF
C----------------------------
C     INTERNAL FORCES
C----------------------------
      IF(IGTYP.EQ.29)THEN
       DO I=LFT,LLT
         SIG_LOC(1,I) = GBUF%SIG(II(1)+I)
         SIG_LOC(2,I) = GBUF%SIG(II(2)+I)
         SIG_LOC(3,I) = GBUF%SIG(II(3)+I)
         SIG_LOC(4,I) = GBUF%SIG(II(4)+I)
         SIG_LOC(5,I) = GBUF%SIG(II(5)+I)
         SIG_LOC(6,I) = GBUF%SIG(II(6)+I)
         EINT_LOC(I)  = GBUF%EINT(I)
         VOL_LOC(I)   = GBUF%VOL(I)
         OFF_LOC(I)   = GBUF%OFF(I)
         RHO_LOC(I)   = GBUF%RHO(I)

       ENDDO
       IF (USERL_AVAIL>0)THEN
          CALL ENG_USERLIB_SUSER(IGTYP,
     1 NEL    ,NUVAR        ,IPROP(1),IMAT(1),SID  ,TT     ,DT1      ,
     2 EINT_LOC,VOL_LOC,UVAR,FR_W_E,OFF_LOC,RHO_LOC,SIG_LOC      ,
     3 XX(1,1),XX(1,2),XX(1,3),XX(1,4),XX(1,5),XX(1,6),XX(1,7),XX(1,8),
     4 YY(1,1),YY(1,2),YY(1,3),YY(1,4),YY(1,5),YY(1,6),YY(1,7),YY(1,8),
     5 ZZ(1,1),ZZ(1,2),ZZ(1,3),ZZ(1,4),ZZ(1,5),ZZ(1,6),ZZ(1,7),ZZ(1,8),
     6 UX(1,1),UX(1,2),UX(1,3),UX(1,4),UX(1,5),UX(1,6),UX(1,7),UX(1,8),
     7 UY(1,1),UY(1,2),UY(1,3),UY(1,4),UY(1,5),UY(1,6),UY(1,7),UY(1,8),
     8 UZ(1,1),UZ(1,2),UZ(1,3),UZ(1,4),UZ(1,5),UZ(1,6),UZ(1,7),UZ(1,8),
     9 VX(1,1),VX(1,2),VX(1,3),VX(1,4),VX(1,5),VX(1,6),VX(1,7),VX(1,8),
     A VY(1,1),VY(1,2),VY(1,3),VY(1,4),VY(1,5),VY(1,6),VY(1,7),VY(1,8),
     B VZ(1,1),VZ(1,2),VZ(1,3),VZ(1,4),VZ(1,5),VZ(1,6),VZ(1,7),VZ(1,8),
     C VRX(1,1),VRX(1,2),VRX(1,3),VRX(1,4),
     C                             VRX(1,5),VRX(1,6),VRX(1,7),VRX(1,8),
     D VRY(1,1),VRY(1,2),VRY(1,3),VRY(1,4),
     D                             VRY(1,5),VRY(1,6),VRY(1,7),VRY(1,8),
     E VRZ(1,1),VRZ(1,2),VRZ(1,3),VRZ(1,4),
     E                             VRZ(1,5),VRZ(1,6),VRZ(1,7),VRZ(1,8),
     F FX(1,1),FX(1,2),FX(1,3),FX(1,4),FX(1,5),FX(1,6),FX(1,7),FX(1,8),
     G FY(1,1),FY(1,2),FY(1,3),FY(1,4),FY(1,5),FY(1,6),FY(1,7),FY(1,8),
     H FZ(1,1),FZ(1,2),FZ(1,3),FZ(1,4),FZ(1,5),FZ(1,6),FZ(1,7),FZ(1,8),
     I MX(1,1),MX(1,2),MX(1,3),MX(1,4),MX(1,5),MX(1,6),MX(1,7),MX(1,8),
     J MY(1,1),MY(1,2),MY(1,3),MY(1,4),MY(1,5),MY(1,6),MY(1,7),MY(1,8),
     K MZ(1,1),MZ(1,2),MZ(1,3),MZ(1,4),MZ(1,5),MZ(1,6),MZ(1,7),MZ(1,8),
     L STI    ,STIR   ,VISCM  ,VISCR)
       IF(NFILSOL/=0) THEN
        CALL SFILLOPT(GBUF%FILL,STI,
     B    FX(1,1),FX(1,2),FX(1,3),FX(1,4),FX(1,5),FX(1,6),FX(1,7),FX(1,8),     
     F    FY(1,1),FY(1,2),FY(1,3),FY(1,4),FY(1,5),FY(1,6),FY(1,7),FY(1,8),     
     G    FZ(1,1),FZ(1,2),FZ(1,3),FZ(1,4),FZ(1,5),FZ(1,6),FZ(1,7),FZ(1,8))     
        CALL SFILLOPT(GBUF%FILL,STIR,
     B    MX(1,1),MX(1,2),MX(1,3),MX(1,4),MX(1,5),MX(1,6),MX(1,7),MX(1,8),     
     F    MY(1,1),MY(1,2),MY(1,3),MY(1,4),MY(1,5),MY(1,6),MY(1,7),MY(1,8),     
     G    MZ(1,1),MZ(1,2),MZ(1,3),MZ(1,4),MZ(1,5),MZ(1,6),MZ(1,7),MZ(1,8)) 
        END IF    
       ELSE
        ! ----------------
        ! ERROR to be printed & exit
        OPTION='/PROP/USER1 - SOLID'
        SIZE=LEN_TRIM(OPTION)
        CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
        CALL ARRET(2)
        ! ----------------
       ENDIF ! IF (USERL_AVAIL)
       DO I=LFT,LLT
         GBUF%SIG(II(1)+I) = SIG_LOC(1,I)
         GBUF%SIG(II(2)+I) = SIG_LOC(2,I)
         GBUF%SIG(II(3)+I) = SIG_LOC(3,I)
         GBUF%SIG(II(4)+I) = SIG_LOC(4,I)
         GBUF%SIG(II(5)+I) = SIG_LOC(5,I)
         GBUF%SIG(II(6)+I) = SIG_LOC(6,I)
         GBUF%EINT(I) = EINT_LOC(I)
         GBUF%VOL(I) = VOL_LOC(I)
         GBUF%OFF(I) = OFF_LOC(I)
         GBUF%RHO(I) = RHO_LOC(I)
       ENDDO

      ELSEIF(IGTYP.EQ.30)THEN
       DO I=LFT,LLT
         SIG_LOC(1,I) = GBUF%SIG(II(1)+I)
         SIG_LOC(2,I) = GBUF%SIG(II(2)+I)
         SIG_LOC(3,I) = GBUF%SIG(II(3)+I)
         SIG_LOC(4,I) = GBUF%SIG(II(4)+I)
         SIG_LOC(5,I) = GBUF%SIG(II(5)+I)
         SIG_LOC(6,I) = GBUF%SIG(II(6)+I)
         EINT_LOC(I)  = GBUF%EINT(I)
         VOL_LOC(I)   = GBUF%VOL(I)
         OFF_LOC(I)   = GBUF%OFF(I)
         RHO_LOC(I)   = GBUF%RHO(I)
       ENDDO
       IF (USERL_AVAIL>0)THEN
          CALL ENG_USERLIB_SUSER(IGTYP,
     1 NEL    ,NUVAR        ,IPROP(1),IMAT(1),SID  ,TT     ,DT1      ,
     2 EINT_LOC,VOL_LOC,UVAR,FR_W_E,OFF_LOC,RHO_LOC,SIG_LOC      ,
     3 XX(1,1),XX(1,2),XX(1,3),XX(1,4),XX(1,5),XX(1,6),XX(1,7),XX(1,8),
     4 YY(1,1),YY(1,2),YY(1,3),YY(1,4),YY(1,5),YY(1,6),YY(1,7),YY(1,8),
     5 ZZ(1,1),ZZ(1,2),ZZ(1,3),ZZ(1,4),ZZ(1,5),ZZ(1,6),ZZ(1,7),ZZ(1,8),
     6 UX(1,1),UX(1,2),UX(1,3),UX(1,4),UX(1,5),UX(1,6),UX(1,7),UX(1,8),
     7 UY(1,1),UY(1,2),UY(1,3),UY(1,4),UY(1,5),UY(1,6),UY(1,7),UY(1,8),
     8 UZ(1,1),UZ(1,2),UZ(1,3),UZ(1,4),UZ(1,5),UZ(1,6),UZ(1,7),UZ(1,8),
     9 VX(1,1),VX(1,2),VX(1,3),VX(1,4),VX(1,5),VX(1,6),VX(1,7),VX(1,8),
     A VY(1,1),VY(1,2),VY(1,3),VY(1,4),VY(1,5),VY(1,6),VY(1,7),VY(1,8),
     B VZ(1,1),VZ(1,2),VZ(1,3),VZ(1,4),VZ(1,5),VZ(1,6),VZ(1,7),VZ(1,8),
     C VRX(1,1),VRX(1,2),VRX(1,3),VRX(1,4),
     C                             VRX(1,5),VRX(1,6),VRX(1,7),VRX(1,8),
     D VRY(1,1),VRY(1,2),VRY(1,3),VRY(1,4),
     D                             VRY(1,5),VRY(1,6),VRY(1,7),VRY(1,8),
     E VRZ(1,1),VRZ(1,2),VRZ(1,3),VRZ(1,4),
     E                             VRZ(1,5),VRZ(1,6),VRZ(1,7),VRZ(1,8),
     F FX(1,1),FX(1,2),FX(1,3),FX(1,4),FX(1,5),FX(1,6),FX(1,7),FX(1,8),
     G FY(1,1),FY(1,2),FY(1,3),FY(1,4),FY(1,5),FY(1,6),FY(1,7),FY(1,8),
     H FZ(1,1),FZ(1,2),FZ(1,3),FZ(1,4),FZ(1,5),FZ(1,6),FZ(1,7),FZ(1,8),
     I MX(1,1),MX(1,2),MX(1,3),MX(1,4),MX(1,5),MX(1,6),MX(1,7),MX(1,8),
     J MY(1,1),MY(1,2),MY(1,3),MY(1,4),MY(1,5),MY(1,6),MY(1,7),MY(1,8),
     K MZ(1,1),MZ(1,2),MZ(1,3),MZ(1,4),MZ(1,5),MZ(1,6),MZ(1,7),MZ(1,8),
     L STI    ,STIR   ,VISCM  ,VISCR)
       IF(NFILSOL/=0) THEN
        CALL SFILLOPT(GBUF%FILL,STI,
     B    FX(1,1),FX(1,2),FX(1,3),FX(1,4),FX(1,5),FX(1,6),FX(1,7),FX(1,8),     
     F    FY(1,1),FY(1,2),FY(1,3),FY(1,4),FY(1,5),FY(1,6),FY(1,7),FY(1,8),     
     G    FZ(1,1),FZ(1,2),FZ(1,3),FZ(1,4),FZ(1,5),FZ(1,6),FZ(1,7),FZ(1,8))     
        CALL SFILLOPT(GBUF%FILL,STIR,
     B    MX(1,1),MX(1,2),MX(1,3),MX(1,4),MX(1,5),MX(1,6),MX(1,7),MX(1,8),     
     F    MY(1,1),MY(1,2),MY(1,3),MY(1,4),MY(1,5),MY(1,6),MY(1,7),MY(1,8),     
     G    MZ(1,1),MZ(1,2),MZ(1,3),MZ(1,4),MZ(1,5),MZ(1,6),MZ(1,7),MZ(1,8)) 
        END IF    
       ELSE
        ! ----------------
        ! ERROR to be printed & exit
        OPTION='/PROP/USER2 - SOLID'
        SIZE=LEN_TRIM(OPTION)
        CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
        CALL ARRET(2)
        ! ----------------
       ENDIF ! IF (USERL_AVAIL)
       DO I=LFT,LLT
         GBUF%SIG(II(1)+I) = SIG_LOC(1,I)
         GBUF%SIG(II(2)+I) = SIG_LOC(2,I)
         GBUF%SIG(II(3)+I) = SIG_LOC(3,I)
         GBUF%SIG(II(4)+I) = SIG_LOC(4,I)
         GBUF%SIG(II(5)+I) = SIG_LOC(5,I)
         GBUF%SIG(II(6)+I) = SIG_LOC(6,I)
         GBUF%EINT(I) = EINT_LOC(I)
         GBUF%VOL(I) = VOL_LOC(I)
         GBUF%OFF(I) = OFF_LOC(I)
         GBUF%RHO(I) = RHO_LOC(I)
       ENDDO

      ELSEIF(IGTYP.EQ.31)THEN
       DO I=LFT,LLT
         SIG_LOC(1,I) = GBUF%SIG(II(1)+I)
         SIG_LOC(2,I) = GBUF%SIG(II(2)+I)
         SIG_LOC(3,I) = GBUF%SIG(II(3)+I)
         SIG_LOC(4,I) = GBUF%SIG(II(4)+I)
         SIG_LOC(5,I) = GBUF%SIG(II(5)+I)
         SIG_LOC(6,I) = GBUF%SIG(II(6)+I)
         EINT_LOC(I)  = GBUF%EINT(I)
         VOL_LOC(I)   = GBUF%VOL(I)
         OFF_LOC(I)   = GBUF%OFF(I)
         RHO_LOC(I)   = GBUF%RHO(I)
       ENDDO
       IF (USERL_AVAIL>0)THEN
          CALL ENG_USERLIB_SUSER(IGTYP,
     1 NEL    ,NUVAR        ,IPROP(1),IMAT(1),SID  ,TT     ,DT1       ,
     2 EINT_LOC,VOL_LOC,UVAR,FR_W_E,OFF_LOC,RHO_LOC,SIG_LOC      ,
     3 XX(1,1),XX(1,2),XX(1,3),XX(1,4),XX(1,5),XX(1,6),XX(1,7),XX(1,8),
     4 YY(1,1),YY(1,2),YY(1,3),YY(1,4),YY(1,5),YY(1,6),YY(1,7),YY(1,8),
     5 ZZ(1,1),ZZ(1,2),ZZ(1,3),ZZ(1,4),ZZ(1,5),ZZ(1,6),ZZ(1,7),ZZ(1,8),
     6 UX(1,1),UX(1,2),UX(1,3),UX(1,4),UX(1,5),UX(1,6),UX(1,7),UX(1,8),
     7 UY(1,1),UY(1,2),UY(1,3),UY(1,4),UY(1,5),UY(1,6),UY(1,7),UY(1,8),
     8 UZ(1,1),UZ(1,2),UZ(1,3),UZ(1,4),UZ(1,5),UZ(1,6),UZ(1,7),UZ(1,8),
     9 VX(1,1),VX(1,2),VX(1,3),VX(1,4),VX(1,5),VX(1,6),VX(1,7),VX(1,8),
     A VY(1,1),VY(1,2),VY(1,3),VY(1,4),VY(1,5),VY(1,6),VY(1,7),VY(1,8),
     B VZ(1,1),VZ(1,2),VZ(1,3),VZ(1,4),VZ(1,5),VZ(1,6),VZ(1,7),VZ(1,8),
     C VRX(1,1),VRX(1,2),VRX(1,3),VRX(1,4),
     C                             VRX(1,5),VRX(1,6),VRX(1,7),VRX(1,8),
     D VRY(1,1),VRY(1,2),VRY(1,3),VRY(1,4),
     D                             VRY(1,5),VRY(1,6),VRY(1,7),VRY(1,8),
     E VRZ(1,1),VRZ(1,2),VRZ(1,3),VRZ(1,4),
     E                             VRZ(1,5),VRZ(1,6),VRZ(1,7),VRZ(1,8),
     F FX(1,1),FX(1,2),FX(1,3),FX(1,4),FX(1,5),FX(1,6),FX(1,7),FX(1,8),
     G FY(1,1),FY(1,2),FY(1,3),FY(1,4),FY(1,5),FY(1,6),FY(1,7),FY(1,8),
     H FZ(1,1),FZ(1,2),FZ(1,3),FZ(1,4),FZ(1,5),FZ(1,6),FZ(1,7),FZ(1,8),
     I MX(1,1),MX(1,2),MX(1,3),MX(1,4),MX(1,5),MX(1,6),MX(1,7),MX(1,8),
     J MY(1,1),MY(1,2),MY(1,3),MY(1,4),MY(1,5),MY(1,6),MY(1,7),MY(1,8),
     K MZ(1,1),MZ(1,2),MZ(1,3),MZ(1,4),MZ(1,5),MZ(1,6),MZ(1,7),MZ(1,8),
     L STI    ,STIR   ,VISCM  ,VISCR)
       IF(NFILSOL/=0) THEN
        CALL SFILLOPT(GBUF%FILL,STI,
     B    FX(1,1),FX(1,2),FX(1,3),FX(1,4),FX(1,5),FX(1,6),FX(1,7),FX(1,8),     
     F    FY(1,1),FY(1,2),FY(1,3),FY(1,4),FY(1,5),FY(1,6),FY(1,7),FY(1,8),     
     G    FZ(1,1),FZ(1,2),FZ(1,3),FZ(1,4),FZ(1,5),FZ(1,6),FZ(1,7),FZ(1,8))     
        CALL SFILLOPT(GBUF%FILL,STIR,
     B    MX(1,1),MX(1,2),MX(1,3),MX(1,4),MX(1,5),MX(1,6),MX(1,7),MX(1,8),     
     F    MY(1,1),MY(1,2),MY(1,3),MY(1,4),MY(1,5),MY(1,6),MY(1,7),MY(1,8),     
     G    MZ(1,1),MZ(1,2),MZ(1,3),MZ(1,4),MZ(1,5),MZ(1,6),MZ(1,7),MZ(1,8)) 
        END IF    
       ELSE
        ! ----------------
        ! ERROR to be printed & exit
        OPTION='/PROP/USER3 - SOLID'
        SIZE=LEN_TRIM(OPTION)
        CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
        CALL ARRET(2)
        ! ----------------
       ENDIF ! IF (USERL_AVAIL)
       DO I=LFT,LLT
         GBUF%SIG(II(1)+I) = SIG_LOC(1,I)
         GBUF%SIG(II(2)+I) = SIG_LOC(2,I)
         GBUF%SIG(II(3)+I) = SIG_LOC(3,I)
         GBUF%SIG(II(4)+I) = SIG_LOC(4,I)
         GBUF%SIG(II(5)+I) = SIG_LOC(5,I)
         GBUF%SIG(II(6)+I) = SIG_LOC(6,I)
         GBUF%EINT(I) = EINT_LOC(I)
         GBUF%VOL(I) = VOL_LOC(I)
         GBUF%OFF(I) = OFF_LOC(I)
         GBUF%RHO(I) = RHO_LOC(I)
       ENDDO

      ELSEIF (IGTYP == 43) THEN
C--------------------------
C-----------
        FX = ZERO
        FY = ZERO
        FZ = ZERO
        MX = ZERO
        MY = ZERO
        MZ = ZERO
        CALL SUSER43(
     1  ELBUF_STR ,IOUT   ,IPROP(1),IMAT(1),SID  ,TT     ,DT1  ,FR_W_E,       
     2  XX(1,1),XX(1,2),XX(1,3),XX(1,4),XX(1,5),XX(1,6),XX(1,7),XX(1,8), 
     3  YY(1,1),YY(1,2),YY(1,3),YY(1,4),YY(1,5),YY(1,6),YY(1,7),YY(1,8),     
     4  ZZ(1,1),ZZ(1,2),ZZ(1,3),ZZ(1,4),ZZ(1,5),ZZ(1,6),ZZ(1,7),ZZ(1,8),     
     5  UX(1,1),UX(1,2),UX(1,3),UX(1,4),UX(1,5),UX(1,6),UX(1,7),UX(1,8),     
     6  UY(1,1),UY(1,2),UY(1,3),UY(1,4),UY(1,5),UY(1,6),UY(1,7),UY(1,8),     
     7  UZ(1,1),UZ(1,2),UZ(1,3),UZ(1,4),UZ(1,5),UZ(1,6),UZ(1,7),UZ(1,8),     
     8  VX(1,1),VX(1,2),VX(1,3),VX(1,4),VX(1,5),VX(1,6),VX(1,7),VX(1,8),     
     9  VY(1,1),VY(1,2),VY(1,3),VY(1,4),VY(1,5),VY(1,6),VY(1,7),VY(1,8),     
     A  VZ(1,1),VZ(1,2),VZ(1,3),VZ(1,4),VZ(1,5),VZ(1,6),VZ(1,7),VZ(1,8),     
     B  FX(1,1),FX(1,2),FX(1,3),FX(1,4),FX(1,5),FX(1,6),FX(1,7),FX(1,8),     
     F  FY(1,1),FY(1,2),FY(1,3),FY(1,4),FY(1,5),FY(1,6),FY(1,7),FY(1,8),     
     G  FZ(1,1),FZ(1,2),FZ(1,3),FZ(1,4),FZ(1,5),FZ(1,6),FZ(1,7),FZ(1,8),     
     H  STI    ,STIR   ,VISCM  ,VISCR  ,PARTSAV,IPARTS ,BUFMAT ,IOUTPRT,     
     L  IFAILURE,NPF   ,TF     ,IPM    ,IGEO   ,NPT    ,NEL    ,JSMS   ,     
     M  DMELS  ,PM     ,GEO    ,ITASK  ,JTHE   ,TABLE )   
       IF(NFILSOL/=0) THEN
        CALL SFILLOPT(GBUF%FILL,STI,
     B    FX(1,1),FX(1,2),FX(1,3),FX(1,4),FX(1,5),FX(1,6),FX(1,7),FX(1,8),     
     F    FY(1,1),FY(1,2),FY(1,3),FY(1,4),FY(1,5),FY(1,6),FY(1,7),FY(1,8),     
     G    FZ(1,1),FZ(1,2),FZ(1,3),FZ(1,4),FZ(1,5),FZ(1,6),FZ(1,7),FZ(1,8))     
        END IF    
C
      ENDIF
C--------------------------------------------
C     Front wave
C--------------------------------------------
      IF(IFRWV.NE.0)THEN
#include "lockon.inc"
        DO J=1,8
         DO I=LFT,LLT
          IF(FR_WAVE(NC(I,J)).EQ.ZERO)FR_WAVE(NC(I,J))=-FR_W_E(I)
         ENDDO
        ENDDO
#include "lockoff.inc"
      ENDIF
C----------------------------
      IF (IPARIT == 0) THEN
          CALL SUCUMU3(
     .         A    ,AR   ,NC    ,STIFN ,STIFR ,STI   ,STIR  , 
     .         FX   ,FY   ,FZ    ,MX    ,MY    ,MZ    )
      ELSE
          CALL SUCUMU3P(FSKY,FSKY,IADS(1,NF1),STI,STIR,
     .                  FX  ,FY  ,FZ  ,MX  ,MY  ,MZ  )
      ENDIF
C-----------
      RETURN
      END


Chd|====================================================================
Chd|  SUCUMU3                       source/user_interface/suforc3.F
Chd|-- called by -----------
Chd|        SUFORC3                       source/user_interface/suforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SUCUMU3(A     ,AR    ,NC    ,STIFN ,STIFR ,
     .                   STI   ,STIR  ,FX    ,FY    ,FZ    ,
     .                   MX    ,MY    ,MZ    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC(MVSIZ,8)
C     REAL
      my_real
     .   A(3,*),AR(3,*),STIFN(*),STI(*),STIFR(*),STIR(*),
     .   FX(MVSIZ,8),FY(MVSIZ,8),FZ(MVSIZ,8),
     .   MX(MVSIZ,8),MY(MVSIZ,8),MZ(MVSIZ,8)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K
C=======================================================================
      DO J=1,8
        DO I=LFT,LLT
          K = NC(I,J)
          A(1,K)  = A(1,K)  + FX(I,J)
          A(2,K)  = A(2,K)  + FY(I,J)
          A(3,K)  = A(3,K)  + FZ(I,J)
          STIFN(K)= STIFN(K)+ STI(I)
        ENDDO
      ENDDO
c
      IF (IRODDL > 0) THEN
        DO J=1,8
          DO I=LFT,LLT
            K = NC(I,J)
            AR(1,K) = AR(1,K) + MX(I,J)
            AR(2,K) = AR(2,K) + MY(I,J)
            AR(3,K) = AR(3,K) + MZ(I,J)
            STIFR(K)= STIFR(K)+ STIR(I)
          ENDDO
        ENDDO
      ENDIF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SUCUMU3P                      source/user_interface/suforc3.F
Chd|-- called by -----------
Chd|        SUFORC3                       source/user_interface/suforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SUCUMU3P(FSKY,FSKYV,IADS,STI,STIR,
     .   FX,FY,FZ,MX,MY,MZ)
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   STI(*),STIR(*),
     .   FX(MVSIZ,8),FY(MVSIZ,8),FZ(MVSIZ,8),
     .   MX(MVSIZ,8),MY(MVSIZ,8),MZ(MVSIZ,8)
      my_real
     .   FSKYV(LSKY,8),FSKY(8,LSKY)
      INTEGER IADS(8,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, K, J
C=======================================================================
      IF (IVECTOR == 1) THEN
        DO J=1,8
#include "vectorize.inc"
          DO I=LFT,LLT
            K = IADS(J,I)
            FSKYV(K,1)=FX(I,J)
            FSKYV(K,2)=FY(I,J)
            FSKYV(K,3)=FZ(I,J)
            FSKYV(K,7)=STI(I)
          ENDDO
        ENDDO
      ELSE
        DO J=1,8
          DO I=LFT,LLT
            K = IADS(J,I)
            FSKY(1,K)=FX(I,J)
            FSKY(2,K)=FY(I,J)
            FSKY(3,K)=FZ(I,J)
            FSKY(7,K)=STI(I)
          ENDDO
        ENDDO
      ENDIF
c
      IF (IRODDL > 0) THEN
        IF (IVECTOR == 1) THEN
          DO J=1,8
#include   "vectorize.inc"
            DO I=LFT,LLT
              K = IADS(J,I)
              FSKYV(K,4)=MX(I,J)
              FSKYV(K,5)=MY(I,J)
              FSKYV(K,6)=MZ(I,J)
              FSKYV(K,8)=STIR(I)
            ENDDO
          ENDDO
        ELSE
          DO J=1,8
            DO I=LFT,LLT
              K = IADS(J,I)
              FSKY(4,K)=MX(I,J)
              FSKY(5,K)=MY(I,J)
              FSKY(6,K)=MZ(I,J)
              FSKY(8,K)=STIR(I)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  SUCOOR3                       source/user_interface/suforc3.F
Chd|-- called by -----------
Chd|        SUFORC3                       source/user_interface/suforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SUCOOR3(IXS,X  ,V,VR,W,D,FR_WAVE ,FR_W_E  ,
     .                   XX , YY, ZZ, UX, UY, UZ, 
     .                   VX , VY, VZ, VRX, VRY, VRZ,
     .                   OFFG,OFF,NC ,SID,IMAT,IPROP)
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
#include      "vect01_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*)
C     REAL
      my_real
     .   X(3,*),V(3,*),VR(3,*),W(3,*), D(3,*),FR_WAVE(*) ,FR_W_E(*),
     .   XX(MVSIZ,*), YY(MVSIZ,*), ZZ(MVSIZ,*), 
     .  UX(MVSIZ,*), UY(MVSIZ,*), UZ(MVSIZ,*), 
     .  VX(MVSIZ,*), VY(MVSIZ,*), VZ(MVSIZ,*), 
     .  VRX(MVSIZ,8),VRY(MVSIZ,8),VRZ(MVSIZ,8),
     .  OFFG(*),OFF(*)
      INTEGER NC(MVSIZ,8), IMAT(*), SID(*),IPROP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C=======================================================================
      DO I=LFT,LLT
        IPROP(I)=IXS(10,I)
        SID(I)  =IXS(11,I)
        IMAT(I) =IXS(1,I)
        OFF(I)  = MIN(ONE,OFFG(I))
      ENDDO
C----------------------------
      DO J=1,8
        DO I=LFT,LLT
          NC(I,J) = IXS(J+1,I)
          XX(I,J) = X(1,NC(I,J))
          YY(I,J) = X(2,NC(I,J))
          ZZ(I,J) = X(3,NC(I,J))
          UX(I,J) = D(1,NC(I,J))
          UY(I,J) = D(2,NC(I,J))
          UZ(I,J) = D(3,NC(I,J))
          VX(I,J) = V(1,NC(I,J))
          VY(I,J) = V(2,NC(I,J))
          VZ(I,J) = V(3,NC(I,J))
        ENDDO
      ENDDO
      IF (IRODDL > 0) THEN
        DO J=1,8
          DO I=LFT,LLT
            VRX(I,J)= VR(1,NC(I,J))
            VRY(I,J)= VR(2,NC(I,J))
            VRZ(I,J)= VR(3,NC(I,J))
          ENDDO
        ENDDO
      ELSE
        VRX = ZERO
        VRY = ZERO
        VRZ = ZERO
      ENDIF
C--------------------------------------------
C     Front wave
C--------------------------------------------
      IF(IFRWV.NE.0)THEN
        DO I=LFT,LLT
          FR_W_E(I)=ZERO
        ENDDO
        DO J=1,8
         DO I=LFT,LLT
          FR_W_E(I)=MAX(FR_W_E(I),FR_WAVE(NC(I,J)))
         ENDDO
        ENDDO
      ENDIF
C-----------
      RETURN
      END