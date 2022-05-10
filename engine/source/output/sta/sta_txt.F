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
Chd|  STRS_TXT                      source/output/sta/sta_txt.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITE_C_C                     source/output/tools/sortie_c.c
Chd|====================================================================
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
Chd|====================================================================
Chd|  STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|        STAT_C_AUXF                   source/output/sta/stat_c_auxf.F
Chd|        STAT_C_EPSPF                  source/output/sta/stat_c_epspf.F
Chd|        STAT_C_FAIL                   source/output/sta/stat_c_fail.F
Chd|        STAT_C_OFF                    source/output/sta/stat_c_off.F
Chd|        STAT_C_ORTH_LOC               source/output/sta/stat_c_orth_loc.F
Chd|        STAT_C_STRAF                  source/output/sta/stat_c_straf.F
Chd|        STAT_C_STRAFG                 source/output/sta/stat_c_strafg.F
Chd|        STAT_C_STRSF                  source/output/sta/stat_c_strsf.F
Chd|        STAT_C_STRSFG                 source/output/sta/stat_c_strsfg.F
Chd|        STAT_C_THK                    source/output/sta/stat_c_thk.F
Chd|        STAT_N_BCS                    source/output/sta/stat_n_bcs.F
Chd|        STAT_N_TEMP                   source/output/sta/stat_n_temp.F
Chd|        STAT_N_VEL                    source/output/sta/state_n_vel.F
Chd|        STAT_P_AUX                    source/output/sta/stat_p_aux.F
Chd|        STAT_S_AUXF                   source/output/sta/stat_s_auxf.F
Chd|        STAT_S_EREF                   source/output/sta/stat_s_eref.F
Chd|        STAT_S_FAIL                   source/output/sta/stat_s_fail.F
Chd|        STAT_S_ORTHO                  source/output/sta/stat_s_ortho.F
Chd|        STAT_S_STRAF                  source/output/sta/stat_s_straf.F
Chd|        STAT_S_STRSF                  source/output/sta/stat_s_strsf.F
Chd|        TAB_STRS_TXT50                source/output/sta/sta_txt.F   
Chd|-- calls ---------------
Chd|        WRITE_C_C_TXT                 source/output/tools/sortie_c.c
Chd|====================================================================
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
      INTEGER CTEXT(LENGTH+1),I
C
      DO I=1,LENGTH
        CTEXT(I)=ICHAR(TEXT(I:I))
        CALL WRITE_C_C_TXT(CTEXT(I),I,LENGTH)
      ENDDO
        CTEXT(LENGTH+1)=0
C
      RETURN
      END
Chd|====================================================================
Chd|  TAB_STRS_TXT50                source/output/sta/sta_txt.F   
Chd|-- called by -----------
Chd|        STAT_C_AUXF                   source/output/sta/stat_c_auxf.F
Chd|        STAT_C_EPSPF                  source/output/sta/stat_c_epspf.F
Chd|        STAT_C_FAIL                   source/output/sta/stat_c_fail.F
Chd|        STAT_C_STRAF                  source/output/sta/stat_c_straf.F
Chd|        STAT_C_STRAFG                 source/output/sta/stat_c_strafg.F
Chd|        STAT_C_STRSF                  source/output/sta/stat_c_strsf.F
Chd|        STAT_C_STRSFG                 source/output/sta/stat_c_strsfg.F
Chd|        STAT_P_AUX                    source/output/sta/stat_p_aux.F
Chd|        STAT_S_AUXF                   source/output/sta/stat_s_auxf.F
Chd|        STAT_S_EREF                   source/output/sta/stat_s_eref.F
Chd|        STAT_S_FAIL                   source/output/sta/stat_s_fail.F
Chd|        STAT_S_ORTHO                  source/output/sta/stat_s_ortho.F
Chd|        STAT_S_STRAF                  source/output/sta/stat_s_straf.F
Chd|        STAT_S_STRSF                  source/output/sta/stat_s_strsf.F
Chd|-- calls ---------------
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|====================================================================
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
      IF (NBPLINE .GT. 5)NBPLINE = 5
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
