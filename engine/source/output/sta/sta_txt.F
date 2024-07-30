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
      !||====================================================================
      !||    strs_txt    ../engine/source/output/sta/sta_txt.F
      !||--- calls      -----------------------------------------------------
      !||    write_c_c   ../common_source/tools/input_output/write_routtines.c
      !||====================================================================
      SUBROUTINE STRS_TXT(TEXT,LEN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*80 TEXT
      INTEGER LEN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER CTEXT(81),I
C
      DO 100 I=1,LEN
 100  CTEXT(I)=ICHAR(TEXT(I:I))
      CTEXT(LEN+1)=0
C
      CALL WRITE_C_C(CTEXT,81)
C
      RETURN
      END
C
      !||====================================================================
      !||    strs_txt50         ../engine/source/output/sta/sta_txt.F
      !||--- called by ------------------------------------------------------
      !||    dynain_c_strag     ../engine/source/output/dynain/dynain_c_strag.F
      !||    dynain_c_strsg     ../engine/source/output/dynain/dynain_c_strsg.F
      !||    dynain_node        ../engine/source/output/dynain/dynain_node.F
      !||    dynain_shel_mp     ../engine/source/output/dynain/dynain_shel_mp.F
      !||    dynain_shel_spmd   ../engine/source/output/dynain/dynain_shel_spmd.F
      !||    gendynain          ../engine/source/output/dynain/gendynain.F
      !||    genstat            ../engine/source/output/sta/genstat.F
      !||    stat_c_auxf        ../engine/source/output/sta/stat_c_auxf.F
      !||    stat_c_epspf       ../engine/source/output/sta/stat_c_epspf.F
      !||    stat_c_fail        ../engine/source/output/sta/stat_c_fail.F
      !||    stat_c_off         ../engine/source/output/sta/stat_c_off.F
      !||    stat_c_orth_loc    ../engine/source/output/sta/stat_c_orth_loc.F
      !||    stat_c_straf       ../engine/source/output/sta/stat_c_straf.F
      !||    stat_c_strafg      ../engine/source/output/sta/stat_c_strafg.F
      !||    stat_c_strsf       ../engine/source/output/sta/stat_c_strsf.F
      !||    stat_c_strsfg      ../engine/source/output/sta/stat_c_strsfg.F
      !||    stat_c_thk         ../engine/source/output/sta/stat_c_thk.F
      !||    stat_n_bcs         ../engine/source/output/sta/stat_n_bcs.F
      !||    stat_n_temp        ../engine/source/output/sta/stat_n_temp.F
      !||    stat_n_vel         ../engine/source/output/sta/state_n_vel.F
      !||    stat_p_aux         ../engine/source/output/sta/stat_p_aux.F
      !||    stat_s_auxf        ../engine/source/output/sta/stat_s_auxf.F
      !||    stat_s_eref        ../engine/source/output/sta/stat_s_eref.F
      !||    stat_s_fail        ../engine/source/output/sta/stat_s_fail.F
      !||    stat_s_ortho       ../engine/source/output/sta/stat_s_ortho.F
      !||    stat_s_straf       ../engine/source/output/sta/stat_s_straf.F
      !||    stat_s_strsf       ../engine/source/output/sta/stat_s_strsf.F
      !||    tab_strs_txt50     ../engine/source/output/sta/sta_txt.F
      !||--- calls      -----------------------------------------------------
      !||    write_c_c_txt      ../common_source/tools/input_output/write_routtines.c
      !||====================================================================
      SUBROUTINE STRS_TXT50(TEXT,LENGTH)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENGTH
      CHARACTER(LEN = LENGTH) :: TEXT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TEXT_LEN
      TEXT_LEN=LEN_TRIM(TEXT)

      CALL WRITE_C_C_TXT(TEXT,TEXT_LEN)
C
      RETURN
      END
      !||====================================================================
      !||    tab_strs_txt50   ../engine/source/output/sta/sta_txt.F
      !||--- called by ------------------------------------------------------
      !||    stat_c_auxf      ../engine/source/output/sta/stat_c_auxf.F
      !||    stat_c_epspf     ../engine/source/output/sta/stat_c_epspf.F
      !||    stat_c_fail      ../engine/source/output/sta/stat_c_fail.F
      !||    stat_c_straf     ../engine/source/output/sta/stat_c_straf.F
      !||    stat_c_strafg    ../engine/source/output/sta/stat_c_strafg.F
      !||    stat_c_strsf     ../engine/source/output/sta/stat_c_strsf.F
      !||    stat_c_strsfg    ../engine/source/output/sta/stat_c_strsfg.F
      !||    stat_p_aux       ../engine/source/output/sta/stat_p_aux.F
      !||    stat_s_auxf      ../engine/source/output/sta/stat_s_auxf.F
      !||    stat_s_eref      ../engine/source/output/sta/stat_s_eref.F
      !||    stat_s_fail      ../engine/source/output/sta/stat_s_fail.F
      !||    stat_s_ortho     ../engine/source/output/sta/stat_s_ortho.F
      !||    stat_s_straf     ../engine/source/output/sta/stat_s_straf.F
      !||    stat_s_strsf     ../engine/source/output/sta/stat_s_strsf.F
      !||--- calls      -----------------------------------------------------
      !||    strs_txt50       ../engine/source/output/sta/sta_txt.F
      !||====================================================================
      SUBROUTINE TAB_STRS_TXT50(WAP0,CPT,J,SIZP0,NBPLINE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CPT,J,SIZP0,NBPLINE
      double precision   
     .   WAP0(SIZP0)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*100 LINE
      INTEGER I,K,K1
      INTEGER J1
C
      J1 = J
      IF (NBPLINE > 5)NBPLINE = 5
      DO K=1,CPT
        IF (MOD(K,NBPLINE) == 0) THEN
          SELECT CASE (NBPLINE)
          CASE (1)
            WRITE(LINE,'(1P1E20.13)')(WAP0(J1 + K1),K1=1,NBPLINE)
            CALL STRS_TXT50(LINE,20)
          CASE (2)
            WRITE(LINE,'(1P2E20.13)')(WAP0(J1 + K1),K1=1,NBPLINE)
            CALL STRS_TXT50(LINE,40)
          CASE (3)
            WRITE(LINE,'(1P3E20.13)')(WAP0(J1 + K1),K1=1,NBPLINE)
            CALL STRS_TXT50(LINE,60)
          CASE (4)
            WRITE(LINE,'(1P4E20.13)')(WAP0(J1 + K1),K1=1,NBPLINE)
            CALL STRS_TXT50(LINE,80)
          CASE (5)
            WRITE(LINE,'(1P5E20.13)')(WAP0(J1 + K1),K1=1,NBPLINE)
            CALL STRS_TXT50(LINE,100)
          END SELECT
          J1 = J1 + NBPLINE
        ELSEIF (K == CPT) THEN
          SELECT CASE (MOD(K,NBPLINE))
          CASE (1)
            WRITE(LINE,'(1P1E20.13)')(WAP0(J1 + K1),K1=1,MOD(K,NBPLINE))
            CALL STRS_TXT50(LINE,20)
          CASE (2)
            WRITE(LINE,'(1P2E20.13)')(WAP0(J1 + K1),K1=1,MOD(K,NBPLINE))
            CALL STRS_TXT50(LINE,40)
          CASE (3)
            WRITE(LINE,'(1P3E20.13)')(WAP0(J1 + K1),K1=1,MOD(K,NBPLINE))
            CALL STRS_TXT50(LINE,60)
          CASE (4)
            WRITE(LINE,'(1P4E20.13)')(WAP0(J1 + K1),K1=1,MOD(K,NBPLINE))
            CALL STRS_TXT50(LINE,80)
          CASE (5)
            WRITE(LINE,'(1P5E20.13)')(WAP0(J1 + K1),K1=1,MOD(K,NBPLINE))
            CALL STRS_TXT50(LINE,100)
          END SELECT
        ENDIF
      ENDDO
C
      RETURN
      END
C
