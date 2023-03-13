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
Chd|  RGJOINT                       source/elements/joint/rgjoint.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        RANIM33                       source/elements/joint/ranim33.F
Chd|        RBILAN33                      source/elements/joint/rbilan33.F
Chd|        RCUM33                        source/elements/joint/rcum33.F
Chd|        RCUM33P                       source/elements/joint/rcum33.F
Chd|        RDTIME33                      source/elements/joint/rdtime33.F
Chd|        RSKEW33                       source/elements/joint/rskew33.F
Chd|        RUSER33                       source/elements/joint/ruser33.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE RGJOINT(
     1           JFT    ,JLT    ,IPARTR  ,NEL    ,NUVAR   ,
     2           IOUT   ,IPROP  ,IADR    ,FSKY   ,FSKYV   ,
     3           OFF    ,FORX   ,FORY    ,FORZ   ,E       ,
     4           DX     ,DY     ,DZ      ,RX     ,RY      ,
     5           RZ     ,XMOM   ,YMOM    ,ZMOM   ,UVAR    ,
     6           STIFN  ,STIFR  ,IXR     ,TANI   ,RBY     , 
     7           X      ,VX     ,VR      ,STI    ,STIR    ,
     8           MS     ,IN     ,PARTSAV ,DT1    ,DT2T    ,
     9           F      ,AR     ,FX1     ,FX2    ,FY1     ,
     A           FY2    ,FZ1    ,FZ2     ,MX1    ,MX2     ,
     B           MY1    ,MY2    ,MZ1     ,MZ2    ,GRESAV  ,
     C           GRTH   ,IGRTH  ,MSRT    ,DMELRT ,NELTST  ,
     D           ITYPTST,IGTYP  ,SENSOR_TAB,NC1     ,
     E           NC2    ,XDP    ,X0_ERR  ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE SENSOR_MOD
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "mvsiz_p.inc"
#include      "parit_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER JFT,JLT,IOUT,NEL,NUVAR,IPROP,
     .        IPARTR(*),IXR(NIXR,*),IADR(3,*),GRTH(*),IGRTH(*),
     .        NELTST,ITYPTST,IGTYP,NC1(*),NC2(*)
      my_real
     .   DT1, DT2T, UVAR(NUVAR,*),STIFN(*),STIFR(*), OFF(*), E(*),
     .   X(*), VX(*), VR(*),DX(*),DY(*),DZ(*), RX(*),RY(*),RZ(*), 
     .   F(*) , FORX(*), FORY(*), FORZ(*), 
     .   AR(*), XMOM(*), YMOM(*), ZMOM(*),
     .   FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .   FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .   MX1(MVSIZ),MY1(MVSIZ),MZ1(MVSIZ),
     .   MX2(MVSIZ),MY2(MVSIZ),MZ2(MVSIZ),RBY(*),
     .   PARTSAV(*),TANI(15,*),MS(*),IN(*),FSKY(*),FSKYV(*),GRESAV(*),
     .   MSRT(*), DMELRT(*),X0_ERR(3,*)
      DOUBLE PRECISION XDP(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISENS
      my_real
     .   UMAS(MVSIZ),UINER(MVSIZ),
     .   ROT1(3,MVSIZ),ROT2(3,MVSIZ),STI(3,MVSIZ),STIR(3,MVSIZ),
     .   XKM(MVSIZ),XKR(MVSIZ),XCM(MVSIZ),XCR(MVSIZ)
      DOUBLE PRECISION XL(MVSIZ,3)
C=======================================================================
C
      CALL RSKEW33(JFT    ,JLT     ,IXR    ,IOUT   ,IPROP,
     .             NUVAR  ,UVAR    ,RBY    ,X      ,XL   ,
     .             ROT1   ,ROT2    ,DX     ,DY     ,DZ   ,
     .             RX     ,RY      ,RZ     ,VR     ,IGTYP,
     .             SENSOR_TAB,ISENS  ,NC1    ,NC2  ,
     .             XDP)         
C
      CALL RUSER33(NEL   ,IOUT   ,IPROP  ,NUVAR   ,UVAR  ,
     .             FORX  ,FORY   ,FORZ   ,XMOM    ,YMOM  ,
     .             ZMOM  ,XKM    ,XKR    ,XCM     ,XCR   ,
     .             XL    ,UMAS   ,UINER  ,OFF     ,E     ,
     .             ROT1  ,ROT2   ,DX     ,DY      ,DZ    ,
     .             RX    ,RY     ,RZ     ,IGTYP   ,ISENS ,
     .             X0_ERR)
C
      CALL RDTIME33(JFT  ,JLT   ,DT2T   ,NELTST ,ITYPTST,
     .             IXR   ,E     ,STI    ,STIR   ,OFF    ,
     .             XKM   ,XKR   ,XCM    ,XCR    ,UMAS   ,
     .             UINER ,FORX  ,FORY   ,FORZ   ,
     .             XMOM  ,YMOM  ,ZMOM   ,ROT1   ,ROT2   ,
     .             MSRT  ,DMELRT,NUVAR  ,UVAR   ,IGTYP)
C
      CALL RBILAN33(JFT  ,JLT    ,E      ,PARTSAV ,UMAS ,
     .              VX   ,IPARTR ,GRESAV ,GRTH    ,IGRTH,
     .              NC1  ,NC2    )
      CALL RANIM33 (JFT,JLT,FORX,FORY,FORZ,XMOM,YMOM,ZMOM,XL,TANI,H3D_DATA)
C
      IF(IPARIT==0)THEN
        CALL RCUM33(JFT   ,JLT    ,XL    ,NC1   ,NC2   ,
     .              F     ,FORX   ,FORY  ,FORZ  ,AR    ,
     .              XMOM  ,YMOM   ,ZMOM  ,STI   ,STIR  ,
     .              STIFN ,STIFR  ,FX1   ,FX2   ,FY1   ,
     .              FY2   ,FZ1    ,FZ2   ,MX1   ,MX2   ,
     .              MY1   ,MY2    ,MZ1   ,MZ2   ,NUVAR ,
     .              UVAR)
      ELSE
        CALL RCUM33P(JFT   ,JLT   ,XL    ,
     .               FSKY  ,FSKYV ,FORX  ,FORY  ,FORZ  ,
     .               XMOM  ,YMOM  ,ZMOM  ,STI   ,STIR  ,
     .               FX1   ,FX2   ,FY1   ,FY2   ,FZ1   ,
     .               FZ2   ,MX1   ,MX2   ,MY1   ,MY2   ,
     .               MZ1   ,MZ2   ,IADR  ,NUVAR ,UVAR)
      ENDIF
C-------------------------------
      RETURN
      END
