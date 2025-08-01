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
!||    hm_read_prethgrou             ../starter/source/output/th/hm_read_prethgrou.F
!||--- called by ------------------------------------------------------
!||    lectur                        ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    hm_get_intv                   ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_count               ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_read_key            ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start               ../starter/source/devtools/hm_reader/hm_option_start.F
!||--- uses       -----------------------------------------------------
!||    checksum_starter_option_mod   ../starter/source/output/checksum/checksum_option.F90
!||    hm_option_read_mod            ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod                  ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_PRETHGROU(IFI,NVARTOT,LSUBMODEL,IFLAG,OUTPUT)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This Subroutine is defining
C  allocation sizes (NVARTOT & IFI) related to /TH entities
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
       USE HM_OPTION_READ_MOD
       USE SUBMODEL_MOD
       USE GROUPDEF_MOD
       USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARLINE
       USE output_mod
       USE checksum_starter_option_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFI,NVARTOT,IFLAG
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE(output_) OUTPUT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

      INTEGER I,IG,IGS,ID
      INTEGER IFIX_TMP
      CHARACTER(LEN=NCHARLINE) :: KEY
      CHARACTER(LEN=NCHARTITLE) :: TITR
C-----------------------------------------------
C   P a r a m e t e r s
C-----------------------------------------------
      INTEGER NVARN,NVARS,NVARC,NVART,NVARP,NVARR,NVARUR
      INTEGER NVARNS,NVARSPH
      INTEGER NVARIN,NVARRW,NVARRB,NVARAC,NVARSE,NVARJO,NVARFX,NVARFXM,NVARGAU
      INTEGER NVARAB,NVARMV4,NVARMV,NVARPA
      INTEGER NVARF1,NVARFR
      INTEGER NVARRIV,NVARRIVG,NSELRT
      INTEGER NVARNG,NVARSG,NVARCG,NVARTG,NVARPG,NVARRG,NVARURG
      INTEGER NVARNSG,NVARSPG,NVARSENS,NVARCHECKSUM
      INTEGER NVARING,NVARRWG, NVARRBG,NVARACG,NVARSEG,NVARJOG
      INTEGER NVARABG,NVARMG4,NVARMVG,NVARPAG,NVARFXG,NVARFXMG
      INTEGER NVARF1G,NVARFRG,NVARGAUG,NVARCLUS,NVARCLUSG,NVARFLOW
      INTEGER NVARSURF,NVARSLIP,NVARSLIPG,NVARRET,NVARRETG
      INTEGER HM_NTHGRP,NTHACCEL,NTHINTER,NTHRWALL,NTHSECTIO,NTHCLUS,IDSMAX
      INTEGER NTHBEAM,NTHTRUS,NTHBRIC,NTHNODE,NTHSHEL,NTHSH3N,NTHSPRING,NTHRBODY
      INTEGER NTHMONVOL,HM_NTHPART,HM_NTHSUBS,HM_NTHSPHCEL, HM_NTHQUAD, HM_NTHSPHFLOW
      INTEGER HM_NTHGAUGE, HM_NTHFXBODY, HM_NTHFRAME, HM_NTHCYLJO, HM_NTHNSTRAND,HM_NTHSURF
      INTEGER HM_NTHTRIA,HM_NTHSLIPRING,HM_NTHRETRACTOR,HM_NTHSENS,HM_NTHCHECKSUM
      LOGICAL IS_AVAILABLE
C
      PARAMETER (NVARRIV = 10,NVARRIVG = 1)
      PARAMETER (NVARN = 628,NVARS =239554 ,NVARC = 37856,NVART = 6)
      PARAMETER (NVARP = 337,NVARR = 66,NVARUR = 12)
      PARAMETER (NVARNS = 4,NVARSPH = 41)
      PARAMETER (NVARIN = 29,NVARRW = 6,NVARRB =15,NVARFX =4)
      PARAMETER (NVARFXM = 3)
      PARAMETER (NVARAC = 3,NVARSE =39,NVARJO = 6,NVARGAU = 8)
      PARAMETER (NVARAB = 7,NVARMV4= 9,NVARMV = 150)
      PARAMETER (NVARPA = 32)
      PARAMETER (NVARF1 = 18,NVARFR = 24,NVARCLUS=11)
      PARAMETER (NVARNG = 7,NVARSG = 39766,NVARCG =574 ,NVARTG = 1)
      PARAMETER (NVARPG = 1,NVARRG = 1,NVARURG = 1)
      PARAMETER (NVARNSG = 1,NVARSPG = 2)
      PARAMETER (NVARING = 6,NVARRWG = 3,NVARRBG = 6,NVARFXG = 1)
      PARAMETER (NVARFXMG =1)
      PARAMETER (NVARACG = 2,NVARSEG = 7,NVARJOG = 3)
      PARAMETER (NVARABG = 1,NVARMG4 = 1,NVARMVG = 62)
      PARAMETER (NVARPAG = 1)
      PARAMETER (NVARF1G = 3,NVARFRG = 4,NVARGAUG = 1,NVARCLUSG = 2)
      PARAMETER (NVARFLOW = 1)
      PARAMETER (NVARSURF = 6)
      PARAMETER (NVARSLIP = 6,NVARSLIPG = 6,NVARRET = 3,NVARRETG = 3)
      PARAMETER (NVARSENS = 1)
      PARAMETER (NVARCHECKSUM = 1)
C-----------------------------------------------
C   E x t e r n a l
C-----------------------------------------------
      INTEGER  PRETHGRNE,PRETHGRKI,PRETHGRPA,PRETHGRNS,PRETHGRVAR
      CHARACTER THFILE*4
C=======================================================================
      IGS=0
      IS_AVAILABLE = .FALSE.
      HM_NTHGRP = 0
c
      ! Choose TH file type
      IF (IFLAG == 0) THEN
        THFILE = '/TH'
      ELSEIF (IFLAG == 1) THEN
        THFILE = '/ATH'
      ELSEIF (IFLAG == 2) THEN
        THFILE = '/BTH'
      ELSEIF (IFLAG == 3) THEN
        THFILE = '/CTH'
      ELSEIF (IFLAG == 4) THEN
        THFILE = '/DTH'
      ELSEIF (IFLAG == 5) THEN
        THFILE = '/ETH'
      ELSEIF (IFLAG == 6) THEN
        THFILE = '/FTH'
      ELSEIF (IFLAG == 7) THEN
        THFILE = '/GTH'
      ELSEIF (IFLAG == 8) THEN
        THFILE = '/HTH'
      ELSEIF (IFLAG == 9) THEN
        THFILE = '/ITH'
      ENDIF
C
      !-------------------------------------------
      ! /TH/ACCEL
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/ACCEL',NTHACCEL)
      IF (NTHACCEL > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/ACCEL')
        HM_NTHGRP = HM_NTHGRP + NTHACCEL
        DO I = 1, NTHACCEL
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARAC + 3*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/INTER
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/INTER' ,NTHINTER  )
      IF (NTHINTER > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/INTER')
        HM_NTHGRP = HM_NTHGRP + NTHINTER
        DO I = 1, NTHINTER
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARIN + 3*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/RWALL
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/RWALL' ,NTHRWALL  )
      IF (NTHRWALL > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/RWALL')
        HM_NTHGRP = HM_NTHGRP + NTHRWALL
        DO I = 1, NTHRWALL
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARRW + 3*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/SECTIO
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SECTIO' ,NTHSECTIO)
      IF (NTHSECTIO > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/SECTIO')
        HM_NTHGRP = HM_NTHGRP + NTHSECTIO
        DO I = 1, NTHSECTIO
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARSE + 3*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/CLUSTER
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/CLUSTER' ,NTHCLUS)
      IF (NTHCLUS > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/CLUSTER')
        HM_NTHGRP = HM_NTHGRP + NTHCLUS
        DO I = 1, NTHCLUS
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARCLUS + 3*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/BEAM
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/BEAM' ,NTHBEAM)
      IF (NTHBEAM > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/BEAM')
        HM_NTHGRP = HM_NTHGRP + NTHBEAM
        DO I = 1, NTHBEAM
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARP + LVARITHB*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/TRUS
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/TRUSS' ,NTHTRUS)
      IF (NTHTRUS > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/TRUSS')
        HM_NTHGRP = HM_NTHGRP + NTHTRUS
        DO I = 1, NTHTRUS
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVART + LVARITHB*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/SPRING
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SPRING' ,NTHSPRING)
      IF (NTHSPRING > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/SPRING')
        HM_NTHGRP = HM_NTHGRP + NTHSPRING
        DO I = 1, NTHSPRING
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARR + LVARITHB*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/BRIC
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/BRIC' ,NTHBRIC)
      IF (NTHBRIC > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/BRIC')
        HM_NTHGRP = HM_NTHGRP + NTHBRIC
        DO I = 1, NTHBRIC
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARS + LVARITHB*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/NODE
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/NODE' ,NTHNODE)
      IF (NTHNODE > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/NODE')
        HM_NTHGRP = HM_NTHGRP + NTHNODE
        DO I = 1, NTHNODE
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARN + LVARITHB*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/SHEL
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SHEL' ,NTHSHEL)
      IF (NTHSHEL > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/SHEL')
        HM_NTHGRP = HM_NTHGRP + NTHSHEL
        DO I = 1, NTHSHEL
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARC + LVARITHB*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/SH3N
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SH3N' ,NTHSH3N)
      IF (NTHSH3N > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/SH3N')
        HM_NTHGRP = HM_NTHGRP + NTHSH3N
        DO I = 1, NTHSH3N
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARC + LVARITHB*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/RBODY
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/RBODY' ,NTHRBODY)
      IF (NTHRBODY > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/RBODY')
        HM_NTHGRP = HM_NTHGRP + NTHRBODY
        DO I = 1, NTHRBODY
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARRB + 3*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/MONV
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/MONV' ,NTHMONVOL)
      IF (NTHMONVOL > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/MONV')
        HM_NTHGRP = HM_NTHGRP + NTHMONVOL
        DO I = 1, NTHMONVOL
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + (NVARMV + 3*IDSMAX + 40*IDSMAX)*10*NVOLU
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/PART
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/PART' ,HM_NTHPART)
      IF (HM_NTHPART > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/PART')
        HM_NTHGRP = HM_NTHGRP + HM_NTHPART
        DO I = 1, HM_NTHPART
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARPA
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/SUBSET
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SUBS' ,HM_NTHSUBS)
      IF (HM_NTHSUBS > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/SUBS')
        HM_NTHGRP = HM_NTHGRP + HM_NTHSUBS
        DO I = 1, HM_NTHSUBS
          NVARTOT   = NVARTOT   + 180
          IFI       = IFI       + NVARPA
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/FXBODY
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/FXBODY' ,HM_NTHFXBODY)
      IF (HM_NTHFXBODY > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/FXBODY')
        HM_NTHGRP = HM_NTHGRP + HM_NTHFXBODY
        DO I = 1, HM_NTHFXBODY
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT   = NVARTOT + 180
          IFI       = IFI     + NVARFX + 3*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/SPHCEL
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SPHCEL' ,HM_NTHSPHCEL  )
      IF (HM_NTHSPHCEL > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/SPHCEL')
        HM_NTHGRP = HM_NTHGRP + HM_NTHSPHCEL
        DO I = 1, HM_NTHSPHCEL
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARSPH + LVARITHB*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/CYL_JO
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/CYL_JO' ,HM_NTHCYLJO)
      IF (HM_NTHCYLJO > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/CYL_JO')
        HM_NTHGRP = HM_NTHGRP + HM_NTHCYLJO
        DO I = 1, HM_NTHCYLJO
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT   = NVARTOT   + 180
          IFI       = IFI       + NVARJO + 3*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/FRAME
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/FRAME' ,HM_NTHFRAME)
      IF (HM_NTHFRAME > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/FRAME')
        HM_NTHGRP = HM_NTHGRP + HM_NTHFRAME
        DO I = 1, HM_NTHFRAME
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT   = NVARTOT + 180
          IFI       = IFI     + NVARFR + 3*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/GAUGE
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/GAUGE' ,HM_NTHGAUGE)
      IF (HM_NTHGAUGE > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/GAUGE')
        HM_NTHGRP = HM_NTHGRP + HM_NTHGAUGE
        DO I = 1, HM_NTHGAUGE
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT   = NVARTOT   + 180
          IFI       = IFI       + NVARGAU + 3*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/SPH_FLOW
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SPH_FLOW' ,HM_NTHSPHFLOW)
      IF (HM_NTHSPHFLOW > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/SPH_FLOW')
        HM_NTHGRP = HM_NTHGRP + HM_NTHSPHFLOW
        DO I = 1, HM_NTHSPHFLOW
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT   = NVARTOT   + 180
          IFI       = IFI       + NVARFLOW + 3*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/QUAD
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/QUAD' ,HM_NTHQUAD  )
      IF (HM_NTHQUAD > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/QUAD')
        HM_NTHGRP = HM_NTHGRP + HM_NTHQUAD
        DO I = 1, HM_NTHQUAD
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180
          IFI     = IFI     + NVARS + LVARITHB*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/NSTRAND
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/NSTRAND' ,HM_NTHNSTRAND  )
      IF (HM_NTHNSTRAND > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/NSTRAND')
        HM_NTHGRP = HM_NTHGRP + HM_NTHNSTRAND
        DO I = 1, HM_NTHNSTRAND
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('Num_Cards',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180 !NVARTOT = NVARTOT + PRETHGRVAR()
          IFI     = IFI     + NVARNS + LVARITHB*IDSMAX + 40*IDSMAX ! IFI = IFI +  PRETHGRNS(NVARNS)
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/SURF
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SURF' ,HM_NTHSURF  )
      IF (HM_NTHSURF > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/SURF')
        HM_NTHGRP = HM_NTHGRP + HM_NTHSURF
        DO I = 1, HM_NTHSURF
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180 !NVARTOT = NVARTOT + PRETHGRVAR()
          IFI     = IFI     + NVARSURF + 43*IDSMAX ! IFI = IFI +  PRETHGRKI(NVARSURF)
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/TRIA
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/TRIA' ,HM_NTHTRIA  )
      IF (HM_NTHTRIA> 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/TRIA')
        HM_NTHGRP = HM_NTHGRP + HM_NTHTRIA
        DO I = 1, HM_NTHTRIA
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180 !NVARTOT = NVARTOT + PRETHGRVAR()
          IFI     = IFI     + NVARS + LVARITHB*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/SLIPRING
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SLIPRING' ,HM_NTHSLIPRING  )
      IF (HM_NTHSLIPRING > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/SLIPRING')
        HM_NTHGRP = HM_NTHGRP + HM_NTHSLIPRING
        DO I = 1, HM_NTHSLIPRING
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180 !NVARTOT = NVARTOT + PRETHGRVAR()
          IFI     = IFI     + NVARSLIP + LVARITHB*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/RETRACTOR
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/RETRACTOR' ,HM_NTHRETRACTOR  )
      IF (HM_NTHRETRACTOR > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/RETRACTOR')
        HM_NTHGRP = HM_NTHGRP + HM_NTHRETRACTOR
        DO I = 1, HM_NTHRETRACTOR
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT = NVARTOT + 180 !NVARTOT = NVARTOT + PRETHGRVAR()
          IFI     = IFI     + NVARRET + LVARITHB*IDSMAX + 40*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/SENSOR
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SENSOR' ,HM_NTHSENS  )
      IF (HM_NTHSENS > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/SENSOR')
        HM_NTHGRP = HM_NTHGRP + HM_NTHSENS
        DO I = 1, HM_NTHSENS
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL)
          NVARTOT   = NVARTOT + 180
          IFI       = IFI     + NVARSENS + 43*IDSMAX
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/CHECKSUM ( activated automaticaly if /CHECKSUM is used )
      !-------------------------------------------
      
      HM_NTHCHECKSUM = OUTPUT%CHECKSUM%checksum_count
      IF (HM_NTHCHECKSUM > 0) THEN
        HM_NTHGRP = HM_NTHGRP + 1
        NVARTOT   = NVARTOT + 180
        IFI       = IFI     + NVARCHECKSUM + 40 * HM_NTHCHECKSUM
      ENDIF
c-----------
      RETURN
      END
