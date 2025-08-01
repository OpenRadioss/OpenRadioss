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
!||    r23forc3           ../engine/source/elements/spring/r23forc3.F
!||--- called by ------------------------------------------------------
!||    forint             ../engine/source/elements/forint.F
!||--- calls      -----------------------------------------------------
!||    r23law108          ../engine/source/elements/spring/r23law108.F
!||    r23law113          ../engine/source/elements/spring/r23law113.F
!||    r23law114          ../engine/source/elements/spring/r23law114.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod       ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    h3d_mod            ../engine/share/modules/h3d_mod.F
!||    python_funct_mod   ../common_source/modules/python_mod.F90
!||    sensor_mod         ../common_source/modules/sensor_mod.F90
!||    table_mod          ../engine/share/modules/table_mod.F
!||====================================================================
      SUBROUTINE R23FORC3(PYTHON,
     1   ELBUF_STR,            JFT,                  JLT,                  NEL,
     2   MTN,                  IGEO,                 GEO,                  IPM,
     3   IXR,                  X,                    TABLE,                XDP,
     4   F,                    NPF,                  TF,                   SKEW,
     5   FLG_KJ2,              VR,                   AR,                   V,
     6   DT2T,                 NELTST,               ITYPTST,              STIFN,
     7   STIFR,                MS,                   IN,                   FSKY,
     8   IADR,                 SENSORS   ,           OFFSET,               ANIM,
     9   PARTSAV,              IPARTR,               TANI,                 FR_WAVE,
     A   BUFMAT,               BUFGEO,               PM,                   RBY,
     B   FX1,                  FX2,                  FY1,                  FY2,
     C   FZ1,                  FZ2,                  MX1,                  MX2,
     D   MY1,                  MY2,                  MZ1,                  MZ2,
     E   GRESAV,               GRTH,                 IGRTH,                MSRT,
     F   DMELRT,               FLAG_SLIPRING_UPDATE, FLAG_RETRACTOR_UPDATE,H3D_DATA,
     G   JSMS,                 IGRE,                 NFT,                  PRELD1,
     H   STF_F,                STF,                  SANIN,                IRESP,
     I   SNPC )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE python_funct_mod
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      type(python_), intent(inout) :: PYTHON
      INTEGER, INTENT(IN) :: STF  !< Size of TF
      INTEGER, INTENT(IN) :: SANIN !< Size of ANIM
      INTEGER, INTENT(IN) :: IRESP  !< Single precision flag
      INTEGER, INTENT(IN) :: SNPC   !< Size of NPF
      INTEGER, INTENT(IN) :: IGRE
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: JSMS
      INTEGER IXR(NIXR,*), NPF(*),IADR(3,*),IPARTR(*),
     .        IGEO(NPROPGI,*),JFT,JLT,NELTST ,ITYPTST,OFFSET,
     .        NEL,MTN,GRTH(*),IGRTH(*),FLG_KJ2,IPM(NPROPMI,*),FLAG_SLIPRING_UPDATE,
     .        FLAG_RETRACTOR_UPDATE
      my_real DT2T ,
     .   GEO(NPROPG,*),X(*),F(*),TF(STF),SKEW(LSKEW,*),FSKY(*),
     .   VR(*), V(*), AR(*), STIFN(*),STIFR(*),MS(*), IN(*),
     .   ANIM(SANIN),PARTSAV(*),TANI(15,*),
     .   FR_WAVE(*),BUFMAT(*),BUFGEO(*),PM(*),RBY(*),
     .   FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .   FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .   MX1(MVSIZ),MY1(MVSIZ),MZ1(MVSIZ),
     .   MX2(MVSIZ),MY2(MVSIZ),MZ2(MVSIZ),GRESAV(*),
     .   MSRT(*), DMELRT(*)
      my_real, INTENT(IN)          :: PRELD1,STF_F
      DOUBLE PRECISION XDP(3,*)
      TYPE(TTABLE) TABLE(*)
C
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SENSORS_) , INTENT(IN) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NSENSOR
C=======================================================================
       NSENSOR = SENSORS%NSENSOR
!
       IF(MTN  == 108) THEN
         ! Like type08 spring
         CALL R23LAW108(PYTHON,
     1   ELBUF_STR, JFT,       JLT,       NEL,
     2   MTN,       IGEO,      GEO,       IPM,
     3   IXR,       X,         TABLE,     XDP,
     4   F,         NPF,       TF,        SKEW,
     5   FLG_KJ2,   VR,        AR,        V,
     6   DT2T,      NELTST,    ITYPTST,   STIFN,
     7   STIFR,     MS,        IN,        FSKY,
     8   IADR,      NSENSOR,SENSORS%SENSOR_TAB,OFFSET,    ANIM,
     9   PARTSAV,   IPARTR,    TANI,      
     A   BUFMAT,    BUFGEO,    PM,        RBY,
     B   FX1,       FX2,       FY1,       FY2,
     C   FZ1,       FZ2,       MX1,       MX2,
     D   MY1,       MY2,       MZ1,       MZ2,
     E   GRESAV,    GRTH,      IGRTH,     MSRT,
     F   DMELRT,    H3D_DATA,  JSMS,      IGRE,
     G   NFT,       STF,       SANIN,     IRESP,
     H   SNPC)
       
       ELSEIF(MTN == 113) THEN
         ! Like type13 spring
         CALL R23LAW113(PYTHON,
     1   ELBUF_STR, JFT,       JLT,       NEL,
     2   MTN,       IGEO,      GEO,       IPM,
     3   IXR,       X,         TABLE,     XDP,
     4   F,         NPF,       TF,        SKEW,
     5   FLG_KJ2,   VR,        AR,        V,
     6   DT2T,      NELTST,    ITYPTST,   STIFN,
     7   STIFR,     MS,        IN,        FSKY,
     8   IADR,      NSENSOR,SENSORS%SENSOR_TAB,OFFSET,    ANIM,
     9   PARTSAV,   IPARTR,    TANI,      FR_WAVE,
     A   BUFMAT,    BUFGEO,    PM,        RBY,
     B   FX1,       FX2,       FY1,       FY2,
     C   FZ1,       FZ2,       MX1,       MX2,
     D   MY1,       MY2,       MZ1,       MZ2,
     E   GRESAV,    GRTH,      IGRTH,     MSRT,
     F   DMELRT,    H3D_DATA,  JSMS,      IGRE,
     G   NFT,       PRELD1,    STF_F,     STF,
     H   SANIN,     IRESP,     SNPC)

       ELSEIF(MTN == 114) THEN
         ! seatbelt_element
         CALL R23LAW114(PYTHON,
     1   ELBUF_STR,            JFT,                  JLT,                  NEL,
     2   MTN,                  IGEO,                 GEO,                  IPM,
     3   IXR,                  X,                    TABLE,                XDP,
     4   F,                    NPF,                  TF,                   SKEW,
     5   FLG_KJ2,              VR,                   AR,                   V,
     6   DT2T,                 NELTST,               ITYPTST,              STIFN,
     7   STIFR,                MS,                   IN,                   FSKY,
     8   IADR,                 SENSORS%SENSOR_TAB,   OFFSET,               ANIM,
     9   PARTSAV,              IPARTR,               TANI,                 FR_WAVE,
     A   BUFMAT,               BUFGEO,               PM,                   RBY,
     B   FX1,                  FX2,                  FY1,                  FY2,
     C   FZ1,                  FZ2,                  MX1,                  MX2,
     D   MY1,                  MY2,                  MZ1,                  MZ2,
     E   GRESAV,               GRTH,                 IGRTH,                MSRT,
     F   DMELRT,               FLAG_SLIPRING_UPDATE, FLAG_RETRACTOR_UPDATE,H3D_DATA,
     G   JSMS,                 IGRE,                 NFT,                  NSENSOR,
     H   STF,                  SANIN,                IRESP,                SNPC )
 
       ENDIF
C-----------------------------------------------       
      RETURN
      END SUBROUTINE R23FORC3
