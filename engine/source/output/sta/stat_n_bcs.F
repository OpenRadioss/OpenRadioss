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
Chd|  STAT_N_BCS                    source/output/sta/stat_n_bcs.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        SPMD_ISTAT_GATH               source/mpi/output/spmd_stat.F 
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|====================================================================
      SUBROUTINE STAT_N_BCS(ICODE,NUMNOD,ITAB,ITABG,LENG,
     .   NODGLOB,ISKEW,NODTAG)

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODE(*),ITAB(*), NUMNOD,ITABG(*),LENG,
     .        NODGLOB(*),ISKEW(*),NODTAG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N, IC, IC1, IC2, IROT, ITRA, ISK,
     .   ICODEG(LENG),ISKEWG(LENG),NODTAGLOB(LENG)
      CHARACTER TRA*3,ROT*3
      CHARACTER*100 LINE
C-----------------------------------------------
C

      IF (NSPMD > 1) THEN
        CALL SPMD_ISTAT_GATH(ICODE,NODGLOB,ICODEG)
        CALL SPMD_ISTAT_GATH(ISKEW,NODGLOB,ISKEWG)
        CALL SPMD_ISTAT_GATH(NODTAG,NODGLOB,NODTAGLOB)
        IF (ISPMD/=0) RETURN
      ENDIF


      IF (IZIPSTRS == 0) THEN
        WRITE(IUGEO,'(A)')'/NBCS/1   '
        WRITE(IUGEO,'(A)')' INITIAL BOUNDARY CONDITIONS'
        WRITE(IUGEO,'(2A)')'#   TRAROT      SKEW      NODE'
      ELSE
        WRITE(LINE,'(A)')'/NBCS/1'
               CALL STRS_TXT50(LINE,100)
        WRITE(LINE,'(A)')' INITIAL BOUNDARY CONDITIONS'
               CALL STRS_TXT50(LINE,100)
        WRITE(LINE,'(A)')
     .      '#   TRAROT      SKEW      NODE'
               CALL STRS_TXT50(LINE,100)
      ENDIF


      IF (NSPMD == 1) THEN
        DO N=1,NUMNOD
           IF(NODTAG(N) /= 0) THEN
             IC=ICODE(N)       
             IC1=IC/512
             ITRA = 1
             IROT = 1
             TRA = '000'
             ROT = '000'
             IF(IC1 == 7) THEN
                TRA = '111'
             ELSEIF(IC1 == 6) THEN
                TRA = '110'
             ELSEIF(IC1 == 5) THEN
                TRA = '101'
             ELSEIF(IC1 == 4) THEN
                TRA = '100'
             ELSEIF(IC1 == 3) THEN
                TRA = '011'
             ELSEIF(IC1 == 2) THEN
                TRA = '010'
             ELSEIF(IC1 == 1) THEN
                TRA = '001'
             ELSE
                ITRA = 0    
             ENDIF
             IF(IRODDL>0)THEN 
                IC2=(IC-512*IC1)/64
                IF(IC2 == 7) THEN
                   ROT = '111'
                ELSEIF(IC2 == 6) THEN
                   ROT = '110'
                ELSEIF(IC2 == 5) THEN
                   ROT = '101'
                ELSEIF(IC2 == 4) THEN
                   ROT = '100'
                ELSEIF(IC2 == 3) THEN
                   ROT = '011'
                ELSEIF(IC2 == 2) THEN
                   ROT = '010'
                ELSEIF(IC2 == 1) THEN
                   ROT = '001'
                ELSE
                   IROT = 0        
                ENDIF
             ELSE
                IROT = 0        
             ENDIF
C
             IF(ITRA/=0.AND.IROT/=0)THEN
                ISK = ISKEW(N) -1
                IF (IZIPSTRS == 0) THEN
                    WRITE(IUGEO,'(3X,A3,1X,A3,I10,I10)')
     .                TRA,ROT,ISK,ITAB(N)
                ELSE
                    WRITE(LINE,'(3X,A3,1X,A3,I10,I10)')
     .                TRA,ROT,ISK,ITAB(N)
                     CALL STRS_TXT50(LINE,100)
                ENDIF
             ENDIF
           ENDIF
        END DO
      ELSE
        DO N=1,NUMNODG
           IF(NODTAGLOB(N) /= 0) THEN
             IC=ICODEG(N)       
             IC1=IC/512
             ITRA = 1
             IROT = 1
             TRA = '000'
             ROT = '000'
             IF(IC1 == 7) THEN
                TRA = '111'
             ELSEIF(IC1 == 6) THEN
                TRA = '110'
             ELSEIF(IC1 == 5) THEN
                TRA = '101'
             ELSEIF(IC1 == 4) THEN
                TRA = '100'
             ELSEIF(IC1 == 3) THEN
                TRA = '011'
             ELSEIF(IC1 == 2) THEN
                TRA = '010'
             ELSEIF(IC1 == 1) THEN
                TRA = '001'
             ELSE
                ITRA = 0    
             ENDIF
             IF(IRODDL>0)THEN 
                IC2=(IC-512*IC1)/64
                IF(IC2 == 7) THEN
                   ROT = '111'
                ELSEIF(IC2 == 6) THEN
                   ROT = '110'
                ELSEIF(IC2 == 5) THEN
                   ROT = '101'
                ELSEIF(IC2 == 4) THEN
                   ROT = '100'
                ELSEIF(IC2 == 3) THEN
                   ROT = '011'
                ELSEIF(IC2 == 2) THEN
                   ROT = '010'
                ELSEIF(IC2 == 1) THEN
                   ROT = '001'
                ELSE
                   IROT = 0        
                ENDIF
             ELSE
                IROT = 0        
             ENDIF
C
             IF(ITRA/=0.AND.IROT/=0)THEN
               ISK = ISKEWG(N) -1
               IF (IZIPSTRS == 0) THEN
                 WRITE(IUGEO,'(3X,A3,1X,A3,I10,I10)')
     .           TRA,ROT,ISK,ITABG(N)
               ELSE
                 WRITE(LINE,'(3X,A3,1X,A3,I10,I10)')
     .           TRA,ROT,ISK,ITABG(N)
                 CALL STRS_TXT50(LINE,100)
               END IF
           END IF
          ENDIF
        END DO
      ENDIF

C
      RETURN
      END

