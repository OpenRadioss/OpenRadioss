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
Chd|  R23FORC3                      source/elements/spring/r23forc3.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        R23LAW108                     source/elements/spring/r23law108.F
Chd|        R23LAW113                     source/elements/spring/r23law113.F
Chd|        R23LAW114                     source/elements/spring/r23law114.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE R23FORC3(ELBUF_STR,
     1                  JFT      ,JLT      ,NEL      ,MTN     ,IGEO    ,
     2                  GEO      ,IPM      ,IXR      ,X       ,TABLE   ,
     3                  XDP      ,F        ,NPF     ,TF      ,SKEW    ,
     4                  FLG_KJ2  ,VR       ,AR      ,V       ,DT2T    ,
     5                  NELTST   ,ITYPTST  ,STIFN   ,STIFR   ,MS      ,
     6                  IN       ,FSKY     ,IADR    ,SENSOR_TAB,
     7                  OFFSET   ,ANIM     ,PARTSAV ,IPARTR  ,TANI    ,
     8                  FR_WAVE  ,BUFMAT   ,BUFGEO  ,PM      ,RBY     ,
     9                  FX1      ,FX2      ,FY1     ,FY2     ,FZ1     ,
     A                  FZ2      ,MX1      ,MX2     ,MY1     ,MY2     ,
     B                  MZ1      ,MZ2      ,GRESAV  ,GRTH    ,IGRTH   ,
     C                  MSRT     ,DMELRT   ,
     D                  FLAG_SLIPRING_UPDATE,FLAG_RETRACTOR_UPDATE,
     E                  H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE TABLE_MOD
      USE H3D_MOD
      USE SENSOR_MOD
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
#include      "parit_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr03_c.inc"
#include      "chara_c.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*), NPF(*),IADR(3,*),IPARTR(*),
     .        IGEO(NPROPGI,*),JFT,JLT,NELTST ,ITYPTST,OFFSET,
     .        NEL,MTN,GRTH(*),IGRTH(*),FLG_KJ2,IPM(NPROPMI,*),FLAG_SLIPRING_UPDATE,
     .        FLAG_RETRACTOR_UPDATE
      my_real DT2T ,
     .   GEO(NPROPG,*),X(*),F(*),TF(*),SKEW(LSKEW,*),FSKY(*),
     .   VR(*), V(*), AR(*), STIFN(*),STIFR(*),MS(*), IN(*),
     .   ANIM(*),PARTSAV(*),TANI(15,*),
     .   FR_WAVE(*),BUFMAT(*),BUFGEO(*),PM(*),RBY(*),
     .   FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .   FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .   MX1(MVSIZ),MY1(MVSIZ),MZ1(MVSIZ),
     .   MX2(MVSIZ),MY2(MVSIZ),MZ2(MVSIZ),GRESAV(*),
     .   MSRT(*), DMELRT(*)
      DOUBLE PRECISION XDP(3,*)
      TYPE(TTABLE) TABLE(*)
C
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C=======================================================================
!
       IF(MTN  == 108) THEN
         ! Like type08 spring
         CALL R23LAW108(ELBUF_STR,
     1                  JFT      ,JLT      ,NEL      ,MTN     ,IGEO    ,
     2                  GEO      ,IPM      ,IXR      ,X       ,TABLE   ,
     3                  XDP      ,F        ,NPF     ,TF      ,SKEW    ,
     4                  FLG_KJ2  ,VR       ,AR      ,V       ,DT2T    ,
     5                  NELTST   ,ITYPTST  ,STIFN   ,STIFR   ,MS      ,
     6                  IN       ,FSKY     ,IADR    ,SENSOR_TAB,
     7                  OFFSET   ,ANIM     ,PARTSAV ,IPARTR  ,TANI    ,
     8                  FR_WAVE  ,BUFMAT   ,BUFGEO  ,PM      ,RBY     ,
     9                  FX1      ,FX2      ,FY1     ,FY2     ,FZ1     ,
     A                  FZ2      ,MX1      ,MX2     ,MY1     ,MY2     ,
     B                  MZ1      ,MZ2      ,GRESAV  ,GRTH    ,IGRTH   ,
     C                  MSRT     ,DMELRT   , H3D_DATA)     
       
       ELSEIF(MTN == 113) THEN
         ! Like type13 spring
         CALL R23LAW113(ELBUF_STR,
     1                  JFT      ,JLT      ,NEL      ,MTN     ,IGEO    ,
     2                  GEO      ,IPM      ,IXR      ,X       ,TABLE   ,
     3                  XDP      ,F        ,NPF     ,TF      ,SKEW    ,
     4                  FLG_KJ2  ,VR       ,AR      ,V       ,DT2T    ,
     5                  NELTST   ,ITYPTST  ,STIFN   ,STIFR   ,MS      ,
     6                  IN       ,FSKY     ,IADR    ,SENSOR_TAB,
     7                  OFFSET   ,ANIM     ,PARTSAV ,IPARTR  ,TANI    ,
     8                  FR_WAVE  ,BUFMAT   ,BUFGEO  ,PM      ,RBY     ,
     9                  FX1      ,FX2      ,FY1     ,FY2     ,FZ1     ,
     A                  FZ2      ,MX1      ,MX2     ,MY1     ,MY2     ,
     B                  MZ1      ,MZ2      ,GRESAV  ,GRTH    ,IGRTH   ,
     C                  MSRT     ,DMELRT   , H3D_DATA)

       ELSEIF(MTN == 114) THEN
         ! seatbelt_element
         CALL R23LAW114(ELBUF_STR,
     1                  JFT      ,JLT      ,NEL      ,MTN     ,IGEO    ,
     2                  GEO      ,IPM      ,IXR      ,X       ,TABLE   ,
     3                  XDP      ,F        ,NPF     ,TF      ,SKEW    ,
     4                  FLG_KJ2  ,VR       ,AR      ,V       ,DT2T    ,
     5                  NELTST   ,ITYPTST  ,STIFN   ,STIFR   ,MS      ,
     6                  IN       ,FSKY     ,IADR    ,SENSOR_TAB,
     7                  OFFSET   ,ANIM     ,PARTSAV ,IPARTR  ,TANI    ,
     8                  FR_WAVE  ,BUFMAT   ,BUFGEO  ,PM      ,RBY     ,
     9                  FX1      ,FX2      ,FY1     ,FY2     ,FZ1     ,
     A                  FZ2      ,MX1      ,MX2     ,MY1     ,MY2     ,
     B                  MZ1      ,MZ2      ,GRESAV  ,GRTH    ,IGRTH   ,
     C                  MSRT     ,DMELRT   ,
     D                  FLAG_SLIPRING_UPDATE,FLAG_RETRACTOR_UPDATE,
     E                  H3D_DATA ) 
 
       ENDIF
C-----------------------------------------------       
      RETURN
      END SUBROUTINE R23FORC3
