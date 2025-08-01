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
!||    st_qaprint_composite_options   ../starter/source/output/qaprint/st_qaprint_composite_options.F
!||--- called by ------------------------------------------------------
!||    st_qaprint_driver              ../starter/source/output/qaprint/st_qaprint_driver.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    drape_mod                      ../starter/share/modules1/drape_mod.F
!||====================================================================
      SUBROUTINE ST_QAPRINT_COMPOSITE_OPTIONS(DRAPE, DRAPEG)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE DRAPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (DRAPE_), DIMENSION(NUMELC_DRAPE + NUMELTG_DRAPE), TARGET      :: DRAPE
      TYPE (DRAPEG_)                         :: DRAPEG
      TYPE (DRAPE_PLY_)          ,POINTER    :: DRAPE_PLY
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,MY_ID,NPT,J,TEMP_INT,IE_DRAPE,KK,NSLICE,IP
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C-----------------------------------------------
      IF (MYQAKEY('/DRAPE')) THEN
        DO I = 1,NUMELC+ NUMELTG
           IE_DRAPE = DRAPEG%INDX(I)
           IF(IE_DRAPE == 0) CYCLE
            NPT =  DRAPE(IE_DRAPE)%NPLY 
            IF (NPT > 0) THEN
              IF (I <= NUMELC) THEN
                WRITE(VARNAME,'(A)') 'SHELL INTERNAL NUMBER'
                TEMP_INT = I
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
              ELSE
                WRITE(VARNAME,'(A)') 'SH3N INTERNAL NUMBER'
                TEMP_INT = I
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
              ENDIF
              DO II = 1, NPT
                IP = DRAPE(IE_DRAPE)%INDX_PLY(II)
                IF(IP == 0) CYCLE
                DRAPE_PLY =>  DRAPE(IE_DRAPE)%DRAPE_PLY(IP)
                NSLICE = DRAPE_PLY%NSLICE
                WRITE(VARNAME,'(A,I10)') 'IDDRAPE',IP
                TEMP_INT = DRAPE_PLY%IPID
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8) 
                DO KK =1,NSLICE
                      DO J = 1,2
                       WRITE(VARNAME,'(A,I10,I10,I10)') 'IDRAPE',II,KK,J
                       TEMP_INT = DRAPE_PLY%IDRAPE(KK,J)
                       CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
                     ENDDO
                     DO J = 1,2
                       WRITE(VARNAME,'(A,I10,I10,I10)') 'RDRAPE',II,KK,J
                       TEMP_DOUBLE = DRAPE_PLY%RDRAPE(KK,J)
                       CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                     ENDDO
                ENDDO     
              ENDDO
            ENDIF
        ENDDO
      END IF
C-----------------------------------------------------------------------
      RETURN
      END
