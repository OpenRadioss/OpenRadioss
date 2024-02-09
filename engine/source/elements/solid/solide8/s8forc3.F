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
Chd|  S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|        S8BILAN                       source/elements/solid/solide8/s8bilan.F
Chd|        S8COOR3                       source/elements/solid/solide8/s8coor3.F
Chd|        S8CUMU3                       source/elements/solid/solide8/s8cumu3.F
Chd|        S8DEFO3                       source/elements/solid/solide8/s8defo3.F
Chd|        S8DERI3                       source/elements/solid/solide8/s8deri3.F
Chd|        S8FINT3                       source/elements/solid/solide8/s8fint3.F
Chd|        S8LAGR3                       source/elements/solid/solide8/s8lagr3.F
Chd|        S8ROTA3                       source/elements/solid/solide8/s8rota3.F
Chd|        SCUMU3P                       source/elements/solid/solide/scumu3p.F
Chd|        SMALLB3                       source/elements/solid/solide/smallb3.F
Chd|        SR8BILAN                      source/elements/solid/solide8/sr8bilan.F
Chd|        SR8COOR3                      source/elements/solid/solide8/sr8coor3.F
Chd|        SRROTA3                       source/elements/solid/solide/srrota3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE S8FORC3(
     1                   ELBUF_STR,PM       ,GEO    ,IXS      ,X      ,
     2                   A        ,V        ,MS     , 
     3                   VEUL     ,FV       ,ALE_CONNECT  ,IPARG    ,
     4                   TF       ,NPF      ,BUFMAT ,PARTSAV ,
     5                   STIFN ,FSKY ,IADS  ,OFFSET ,IPARTS  ,
     6                   NEL      ,DT2T     ,NELTST ,ITYPTST ,IPM      ,
     7                   ITASK    ,GRESAV   ,GRTH   ,IGRTH   ,MSSA     ,
     8                   DMELS    ,TABLE    ,IOUTPRT,MAT_ELEM,NG       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MAT_ELEM_MOD         
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
#include      "vect01_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "param_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(*),  IPARG(NPARG,*), NPF(*),IADS(8,*),
     .        IPARTS(*),IPM(*),OFFSET,NEL, NELTST, ITYPTST,ITASK,
     .        GRTH(*),IGRTH(*) ,IOUTPRT, NG
C     REAL
      my_real
     .   PM(*), GEO(*), X(*), A(*), V(*), MS(*),
     .   VEUL(*), FV(*),TF(*), BUFMAT(*),
     .   PARTSAV(*),STIFN(*),FSKY(*), DT2T,GRESAV(*), MSSA(*), DMELS(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (MAT_ELEM_) ,TARGET ,INTENT(INOUT) :: MAT_ELEM
      TYPE(TTABLE) TABLE(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,LCO,IFLAG,IPT,IBI
      INTEGER NC(8,MVSIZ),MXT(MVSIZ),NGL(MVSIZ),PID(MVSIZ)
      my_real
     .  FBI
      my_real
     .    XLOC(MVSIZ,8), YLOC(MVSIZ,8), ZLOC(MVSIZ,8),
     .   VXLOC(MVSIZ,8),VYLOC(MVSIZ,8),VZLOC(MVSIZ,8),
     .   PX1(MVSIZ,8),PX2(MVSIZ,8),PX3(MVSIZ,8),PX4(MVSIZ,8),      
     .   PX5(MVSIZ,8),PX6(MVSIZ,8),PX7(MVSIZ,8),PX8(MVSIZ,8),      
     .   PY1(MVSIZ,8),PY2(MVSIZ,8),PY3(MVSIZ,8),PY4(MVSIZ,8),      
     .   PY5(MVSIZ,8),PY6(MVSIZ,8),PY7(MVSIZ,8),PY8(MVSIZ,8),      
     .   PZ1(MVSIZ,8),PZ2(MVSIZ,8),PZ3(MVSIZ,8),PZ4(MVSIZ,8),      
     .   PZ5(MVSIZ,8),PZ6(MVSIZ,8),PZ7(MVSIZ,8),PZ8(MVSIZ,8),
     .    D1(MVSIZ,8), D2(MVSIZ,8), D3(MVSIZ,8), D4(MVSIZ,8),
     .    D5(MVSIZ,8), D6(MVSIZ,8),
     .   VOLGP(MVSIZ,8), VOLN(MVSIZ),DELTAX(MVSIZ), VD2(MVSIZ),
     .   WXX(MVSIZ,8),WYY(MVSIZ,8),WZZ(MVSIZ,8),DVOL(MVSIZ),
     .   RHO0(MVSIZ), STI(MVSIZ),GAMA(MVSIZ,6), OFF(MVSIZ),
     .   VGXA(MVSIZ),VGYA(MVSIZ),VGZA(MVSIZ), VGA2(MVSIZ),
     .   XGXA(MVSIZ),XGYA(MVSIZ),XGZA(MVSIZ),
     .   XGXYA(MVSIZ),XGYZA(MVSIZ),XGZXA(MVSIZ),
     .   XGXA2(MVSIZ),XGYA2(MVSIZ),XGZA2(MVSIZ)
C Variables utilisees en arguments dans les routines solides 
      my_real
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
      my_real :: SSP(MVSIZ)
      my_real :: F11(MVSIZ),F12(MVSIZ),F13(MVSIZ),F14(MVSIZ),
     .   F15(MVSIZ), F16(MVSIZ), F17(MVSIZ), F18(MVSIZ), F21(MVSIZ),
     .   F22(MVSIZ), F23(MVSIZ), F24(MVSIZ), F25(MVSIZ), F26(MVSIZ),
     .   F27(MVSIZ), F28(MVSIZ), F31(MVSIZ), F32(MVSIZ), F33(MVSIZ),
     .   F34(MVSIZ), F35(MVSIZ), F36(MVSIZ), F37(MVSIZ), F38(MVSIZ)

C

      my_real, 
     .  DIMENSION(:), POINTER :: EINT
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C=======================================================================
      GBUF => ELBUF_STR%GBUF
C-----
      LCO=1+11*NFT
C--------------------------
C-----------------------------------------------
C GATHERING VECTEURS LOCAUX
C-----------------------------------------------
      IF (JCVT==0) THEN
       CALL S8COOR3(
     1   GBUF%OFF,OFF,     X,       V,
     2   IXS(LCO),XLOC,    YLOC,    ZLOC,
     3   VXLOC,   VYLOC,   VZLOC,   MXT,
     4   NC,      NGL,     PID,     NEL)
      ELSE
       CALL SR8COOR3(GBUF%OFF,OFF,X,V,IXS(LCO),
     .             XLOC,YLOC,ZLOC,VXLOC,VYLOC,VZLOC,
     .             MXT,NC,NGL,PID,
     .             R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .             IOUTPRT,VGXA,VGYA,VGZA,VGA2,NEL,
     .             XGXA,XGYA,XGZA,XGXA2,XGYA2,XGZA2,
     .             XGXYA,XGYZA,XGZXA,IPARG(1,NG))
      ENDIF
C-----------------------------------------------
C FONCTIONS DE FORME, DEFORMATION, ET OBJECTIVITE 
C PXJ(K,I) FONCTION J DU PT INTEGRATION K ELEMENT I
C-----------------------------------------------
      CALL S8DERI3(
     1   XLOC,    YLOC,    ZLOC,    PX1,
     2   PX2,     PX3,     PX4,     PX5,
     3   PX6,     PX7,     PX8,     PY1,
     4   PY2,     PY3,     PY4,     PY5,
     5   PY6,     PY7,     PY8,     PZ1,
     6   PZ2,     PZ3,     PZ4,     PZ5,
     7   PZ6,     PZ7,     PZ8,     VOLGP,
     8   VOLN,    DELTAX,  NGL,     OFF,
     9   NEL)
      CALL S8LAGR3(
     1   PM,       GBUF%VOL, GBUF%RHO, GBUF%EINT,
     2   MXT,      VOLN,     RHO0,     DVOL,
     3   VD2,      NEL)
C
        DO IPT = 1,NPT
          LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)
        CALL S8DEFO3(
     1   VXLOC,     VYLOC,     VZLOC,     PX1,
     2   PX2,       PX3,       PX4,       PX5,
     3   PX6,       PX7,       PX8,       PY1,
     4   PY2,       PY3,       PY4,       PY5,
     5   PY6,       PY7,       PY8,       PZ1,
     6   PZ2,       PZ3,       PZ4,       PZ5,
     7   PZ6,       PZ7,       PZ8,       IPT,
     8   D1,        D2,        D3,        D4,
     9   D5,        D6,        WXX(1,IPT),WYY(1,IPT),
     A   WZZ(1,IPT),NEL,       JCVT)
        CALL S8ROTA3(
     1   LBUF%SIG,  WXX(1,IPT),WYY(1,IPT),WZZ(1,IPT),
     2   NEL,       JCVT)
      ENDDO
C------------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
C------------------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
      CALL MMAIN8(PM   ,GEO   ,ELBUF_STR,MAT_ELEM ,
     2            IXS  ,IPARG ,V        ,TF    ,    
     3            NPF  ,BUFMAT,STI    ,X     ,D1    ,
     4            D2   ,D3    ,D4     ,D5    ,D6    ,
     5            VOLGP,DELTAX,VOLN   ,DVOL  ,VD2   ,
     6            RHO0 ,MXT   ,NC       ,NGL   ,FV    ,           
     7            NEL  ,WXX   ,WYY    ,WZZ   ,PID   ,
     8            DT2T ,NELTST,ITYPTST,R11   ,R21   ,  
     9            R31  ,R12   ,R22    ,R32   ,R13   , 
     A            R23  ,R33   ,OFF    ,IPM   ,GAMA  ,
     B            MSSA ,DMELS ,TABLE  ,SSP   ,ITASK )
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
        IFLAG=MOD(NCYCLE,NCPRI)
        IF(IOUTPRT>0)THEN       
c           NB3S = NB3
c           IF (MTN==11) NB3S = NB14
          IF (MTN == 11) THEN                                    
            EINT => ELBUF_STR%GBUF%EINS(1:NEL)                   
          ELSE                                                   
            EINT => ELBUF_STR%GBUF%EINT(1:NEL)                   
          ENDIF                                                  
           IF (JCVT==0)THEN
            CALL S8BILAN(PARTSAV,EINT,GBUF%RHO,GBUF%RK,GBUF%VOL,
     .                  VOLN,VXLOC,VYLOC,VZLOC,IPARTS,
     .                  GRESAV,GRTH,IGRTH,XLOC,YLOC,ZLOC,ITASK,IPARG(1,NG))
           ELSE
            CALL SR8BILAN(PARTSAV,EINT,GBUF%RHO,GBUF%RK,GBUF%VOL,
     .                  VOLN,VGXA,VGYA,VGZA,VGA2,IPARTS,
     .                  GRESAV,GRTH,IGRTH,XGXA,XGYA,XGZA,
     .                  XGXA2,XGYA2,XGZA2,XGXYA,XGYZA,XGZXA,ITASK,IPARG(1,NG))
           ENDIF
        ENDIF
C----------------------------
C       OFF
C----------------------------
        CALL SMALLB3(
     1   GBUF%OFF,OFF,     NEL,     ISMSTR)
C----------------------------
C     INTERNAL FORCES
C----------------------------
        CALL S8FINT3(ELBUF_STR%BUFLY(1) ,VOLGP,GBUF%QVIS,
     .               PX1,PX2,PX3,PX4,PX5,PX6,PX7,PX8,
     .               PY1,PY2,PY3,PY4,PY5,PY6,PY7,PY8,
     .               PZ1,PZ2,PZ3,PZ4,PZ5,PZ6,PZ7,PZ8,NEL,
     .               F11, F12, F13, F14,
     .               F15, F16, F17, F18, F21,
     .               F22, F23, F24, F25, F26,
     .               F27, F28, F31, F32, F33,
     .               F34, F35, F36, F37, F38)

C----------------------------
C     CONVECTE --> GLOBAL.
C----------------------------
      IF (JCVT==1) THEN
       CALL SRROTA3(
     1   R11,     R21,     R31,     R12,
     2   R22,     R32,     R13,     R23,
     3   R33,     F11,     F12,     F13,
     4   F14,     F15,     F16,     F17,
     5   F18,     F21,     F22,     F23,
     6   F24,     F25,     F26,     F27,
     7   F28,     F31,     F32,     F33,
     8   F34,     F35,     F36,     F37,
     9   F38,     NEL)
      ENDIF
C-------------------------
C     ASSEMBLE
C-------------------------
      IF(IPARIT==0)THEN
          CALL S8CUMU3(
     1   GBUF%OFF,A,       NC,      STIFN,
     2   STI,     F11,     F21,     F31,
     3   F12,     F22,     F32,     F13,
     4   F23,     F33,     F14,     F24,
     5   F34,     F15,     F25,     F35,
     6   F16,     F26,     F36,     F17,
     7   F27,     F37,     F18,     F28,
     8   F38,     NEL)
      ELSE
          FBI = ZERO
          CALL SCUMU3P(
     1   GBUF%OFF,STI,     FSKY,    FSKY,
     2   IADS,    F11,     F21,     F31,
     3   F12,     F22,     F32,     F13,
     4   F23,     F33,     F14,     F24,
     5   F34,     F15,     F25,     F35,
     6   F16,     F26,     F36,     F17,
     7   F27,     F37,     F18,     F28,
     8   F38,     IBI,     IBI,     IBI,
     9   IBI,     IBI,     IBI,     IBI,
     A   IBI,     FBI,     FBI,     FBI,
     B   FBI,     FBI,     FBI,     FBI,
     C   FBI,     FBI,     FBI,     FBI,
     D   FBI,     FBI,     FBI,     FBI,
     E   FBI,     FBI,     FBI,     FBI,
     F   FBI,     FBI,     FBI,     FBI,
     G   FBI,     FBI,     FBI,     FBI,
     H   FBI,     FBI,     FBI,     FBI,
     I   NEL,     NFT,     JTHE,    ISROT,
     J   IPARTSPH)
      ENDIF
C
C-----------
      RETURN
      END
