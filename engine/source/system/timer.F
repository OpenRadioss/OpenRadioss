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
Chd|  PRINT_SUMMARY                 source/system/timer.F         
Chd|-- called by -----------
Chd|        PRINTIME                      source/system/timer.F         
Chd|-- calls ---------------
Chd|        PRINT_BLOCK                   source/system/timer.F         
Chd|====================================================================
      SUBROUTINE PRINT_SUMMARY(FLAG,NTIMAX,TABTIME,TITLES,OUT_ID) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "macro.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "couple_c.inc"
#include      "impl1_c.inc"
#include      "remesh_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr18_c.inc"
#include      "alefvm.inc"
#include      "sms_c.inc"
#include      "sphcom.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
#include      "filescount_c.inc"
#include        "timeri_c.inc"
C#include        "timerr_c.inc"

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: FLAG ! 1 : No MIN/MAX block 
                                    ! 2 : Min/Max block
        INTEGER, INTENT(IN) :: OUT_ID ! file descriptor
        INTEGER, INTENT(IN) :: NTIMAX ! max number of timer
        CHARACTER(LEN=9), INTENT(INOUT) :: TITLES(NTIMAX) ! number of columns
        REAL, INTENT(INOUT) :: TABTIME(NTIMAX,PARASIZ) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: NF,NFIELDS,FIELDS(NTIMAX) 
        INTEGER I,J
        REAL :: TOTAL
        REAL ::  ZEROS = 0.0 

        IF (IMPL_S.EQ.1) THEN
         NFIELDS = 4
         FIELDS(1) = 31 ; FIELDS(2) = 32 ; FIELDS(3) = 33 ; FIELDS(4) = 34
         CALL PRINT_BLOCK("** IMPLICIT  **",TABTIME,NTIMAX,
     .                      FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)
        ENDIF
C
c       IF(IMONM .GE. 1)THEN
         NF = 1
         IF(NINTER > 0) THEN
           FIELDS(NF) = MACRO_TIMER_CONTSORT
           NF = NF + 1
           FIELDS(NF) = MACRO_TIMER_CONTFOR
           NF = NF + 1
         ENDIF
         FIELDS(NF) = MACRO_TIMER_ELEMENT
         NF = NF + 1
         FIELDS(NF) = MACRO_TIMER_KIN
         NF = NF + 1
         FIELDS(NF) = MACRO_TIMER_INTEG
         NF = NF + 1
         FIELDS(NF) = MACRO_TIMER_IO
         NF = NF + 1
         FIELDS(NF) = MACRO_TIMER_P0
         NF = NF + 1
         FIELDS(NF) = MACRO_TIMER_ASM
         IF(.NOT.( (IDTMINS==0).AND.(IDTMINS_INT==0))) THEN
           NF = NF + 1
           FIELDS(NF) = MACRO_TIMER_AMS
         ENDIF
         NF = NF + 1
         FIELDS(NF) = MACRO_TIMER_RESOL
         NFIELDS = NF
         CALL PRINT_BLOCK("** SUMMARY **",TABTIME,NTIMAX,
     .                      FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)


        IF(NADMESH.NE.0)THEN
         NFIELDS = 3
         FIELDS(1) = 36 ; FIELDS(2) = 37 ; FIELDS(3) = 38 
         CALL PRINT_BLOCK("** ADAPTIVE MESH TIME  **",TABTIME,NTIMAX,
     .                      FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)

        END IF !adaptive mesh

        IF (NSPMD > 1) THEN
         DO I = 1, NSPMD
           TOTAL = TABTIME(10,I)+TABTIME(11,I)+
     .             TABTIME(12,I)+TABTIME(13,I)
           TABTIME(MACRO_TIMER_TMP1,I) = TOTAL
           IF(TABTIME(1,I) > 0 ) THEN
             TABTIME(MACRO_TIMER_TMP2,I) = 100.D0*TOTAL/TABTIME(1,I)
           ELSE
             TABTIME(MACRO_TIMER_TMP2,I) = 0.0D0
           ENDIF
         ENDDO
         FIELDS(1) = 10 ; TITLES(10) = 'FORCES' 
         FIELDS(2) = 11 ; TITLES(11) = 'RBYFOR' 
         FIELDS(3) = 12 ; TITLES(12) = 'RBYVEL ' 
         FIELDS(4) = 13 ; TITLES(13) = 'VEL ' 
         FIELDS(5) = MACRO_TIMER_TMP1 ; TITLES(MACRO_TIMER_TMP1) = 'TOT'
         FIELDS(6) = MACRO_TIMER_TMP2 ; TITLES(MACRO_TIMER_TMP2) = '%'
         NFIELDS = 6
         CALL PRINT_BLOCK("** SPMD COM. TIME **",TABTIME,NTIMAX,
     .                     FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)
        ENDIF ! SPMD
C
        IF (IMONM.GE.1) THEN
          IF(IDTMINS/=0.OR.IDTMINS_INT/=0)THEN
            FIELDS(1)  =61 ; TITLES(61) = 'PCG' 
            FIELDS(2)  =62 ; TITLES(62) = 'PARITF' 
            FIELDS(3)  =63 ; TITLES(63) = 'COMMR ' 
            FIELDS(4)  =64 ; TITLES(64) = 'COMP_MV' 
            FIELDS(5)  =65 ; TITLES(65) = 'VFI' 
            FIELDS(6)  =80 ; TITLES(80) = 'COMM_MV' 
            FIELDS(7)  =70 ; TITLES(70) = 'TOT_PROJ ' 
            FIELDS(8)  =71 ; TITLES(71) = 'MAT_LT2' 
            FIELDS(9)  =72 ; TITLES(72) = 'MAM_NM' 
            FIELDS(10) =73 ; TITLES(73) = 'MAN_NNM' 
            FIELDS(11) =74 ; TITLES(74) = 'COMP_MV_E ' 
            FIELDS(12) =75 ; TITLES(75) = 'BUILD_M' 
            NFIELDS = 12
            CALL PRINT_BLOCK("** AMS TIME **",TABTIME,NTIMAX,
     .                        FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)

          END IF
C
          IF(IMPL_S == 1) THEN
            FIELDS(1) = 67 ; TITLES(FIELDS(1)) = 'COMMDOT' 
            FIELDS(2) = 66 ; TITLES(FIELDS(2)) = 'COMMV' 
            FIELDS(3) = 68 ; TITLES(FIELDS(3)) = 'GPU_CPU ' 
            FIELDS(4) = 69 ; TITLES(FIELDS(4)) = 'CPU_GPU' 
            NFIELDS = 4
            CALL PRINT_BLOCK("** IMPLICIT PCG **",TABTIME,NTIMAX,
     .                        FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)
          END IF
C
          DO I = 1, NSPMD
           TABTIME(MACRO_TIMER_TMP1,I) = MAX(ZERO,TABTIME(8,I)-TABTIME(20,I)
     .                                            -TABTIME(29,I)
     .                                            -TABTIME(76,I),
     .                                            +TABTIME(77,I))
           TABTIME(MACRO_TIMER_TMP2,I) = MAX(ZERO,TABTIME(8,I)+
     .                               TABTIME(19,I)+TABTIME(3,I)
     .                              -TABTIME(29,I)-TABTIME(76,I))

          ENDDO

          IF( NINTER > 0 ) THEN
            FIELDS(1) = 15 ; TITLES(FIELDS(1)) = 'BUKCRIT' 
            FIELDS(2) = 16 ; TITLES(FIELDS(2)) = 'COMMCRIT' 
            FIELDS(3) = 17 ; TITLES(FIELDS(3)) = 'BUCKET ' 
            FIELDS(4) = 18 ; TITLES(FIELDS(4)) = 'GFRONT' 
            FIELDS(5) = 19 ; TITLES(FIELDS(5)) = 'CONTOPT' 
            FIELDS(6) = 20 ; TITLES(FIELDS(6)) = 'CONTF' 
            FIELDS(7) = MACRO_TIMER_TMP1 ; 
            TITLES(FIELDS(7)) = 'DST' 
            FIELDS(8) = MACRO_TIMER_TMP2 ;
            TITLES(FIELDS(8)) = 'CE_FORCE' 
            NFIELDS = 8
            IF(NINTER25 > 0) THEN
              FIELDS(9) = 106 ; TITLES(FIELDS(9)) = 'NORML' 
              NFIELDS = NFIELDS + 1
            ENDIF

            IF(NINTERFRIC > 0) THEN
              NFIELDS = NFIELDS + 1
              FIELDS(NFIELDS) = MACRO_TIMER_FRIC ; TITLES(FIELDS(NFIELDS)) = 'FRIC' 
            ENDIF
            
            CALL PRINT_BLOCK("** INTERFACE TIME **",TABTIME,NTIMAX,
     .                        FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)
            
            FIELDS(1 )=21    ; TITLES(FIELDS(1 )) = 'FICOM1'
            FIELDS(2 )=22    ; TITLES(FIELDS(2 )) = 'FICOM2'
            FIELDS(3 )=23    ; TITLES(FIELDS(3 )) = 'ISDXV1'
            FIELDS(4 )=24    ; TITLES(FIELDS(4 )) = 'ISDXV2'
            FIELDS(5 )=27    ; TITLES(FIELDS(5 )) = 'TRIMMX'
            FIELDS(6 )=25    ; TITLES(FIELDS(6 )) = 'TRIBOX'
            FIELDS(7 )=30    ; TITLES(FIELDS(7 )) = 'TRIBUC'
            FIELDS(8 )=26    ; TITLES(FIELDS(8 )) = 'TRIGAT'
            FIELDS(9 )=76    ; TITLES(FIELDS(9 )) = 'SYNCCOM'
            FIELDS(10)=29    ; TITLES(FIELDS(10)) = 'SHOOTING'
            FIELDS(11)=28    ; TITLES(FIELDS(11)) = 'INT2_FV'
            NFIELDS = 11
            CALL PRINT_BLOCK(" ",TABTIME,NTIMAX,
     .                        FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)
          ENDIF

          FIELDS(1 )=40    ; TITLES(FIELDS(1 )) = 'RB_SENS'
          FIELDS(2 )=41    ; TITLES(FIELDS(2 )) = 'CLOADS'
          FIELDS(3 )=42    ; TITLES(FIELDS(3 )) = 'BCS'
          FIELDS(4 )=43    ; TITLES(FIELDS(4 )) = 'RL_RW'
          FIELDS(5 )=44    ; TITLES(FIELDS(5 )) = 'FIXVEL'
          FIELDS(6 )=45    ; TITLES(FIELDS(6 )) = 'RBE3'
          FIELDS(7 )=46    ; TITLES(FIELDS(7 )) = 'GRAV'

          NFIELDS = 7
          CALL PRINT_BLOCK("** KIN COND & TASK0 TIME **",TABTIME,NTIMAX,
     .                      FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)

          FIELDS(1 )=47    ; TITLES(FIELDS(1 )) = 'MANCTR'         
          FIELDS(2 )=50    ; TITLES(FIELDS(2 )) = 'MONVOL'        
          FIELDS(3 )=52    ; TITLES(FIELDS(3 )) = 'DAMP'     
          FIELDS(4 )=28    ; TITLES(FIELDS(4 )) = 'INT2'      
          FIELDS(5 )=44                                   
          FIELDS(6 )=41                                 
          FIELDS(7 )=53    ; TITLES(FIELDS(7 )) = 'DT2'      
          FIELDS(8 )=49    ; TITLES(FIELDS(8 )) = 'AUX'       
          FIELDS(9 )=51    ; TITLES(FIELDS(9 )) = 'FORINTS'       
          NFIELDS = 9
          IF(TABTIME(54,1) > 0 ) THEN
            NFIELDS = NFIELDS + 1
            FIELDS(10)=54    ; TITLES(FIELDS(10)) = 'R2R'      
          ENDIF
          IF(TABTIME(55,1) > 0 ) THEN
            NFIELDS = NFIELDS + 1
            FIELDS(11)=55    ; TITLES(FIELDS(11)) = 'R2M'      
          ENDIF
          CALL PRINT_BLOCK(" ",TABTIME,NTIMAX,
     .                      FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)

          IF(IALE+IEULER+ITHERM.NE.0) THEN
            FIELDS(1) = MACRO_TIMER_ALEMAIN                           
            FIELDS(2) = MACRO_TIMER_IFSUB0                           
            FIELDS(3) = MACRO_TIMER_MULTIFVM                           
            FIELDS(4) = MACRO_TIMER_SPMDCFD                           
            FIELDS(5) = MACRO_TIMER_MUSCL
            NFIELDS = 5
            CALL PRINT_BLOCK("** ALE TIME **",TABTIME,NTIMAX,
     .                      FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)

          ENDIF

          IF(.FALSE.) THEN
          NFIELDS = 0 
          IF(TABTIME(MACRO_TIMER_MONVOL,1) > 0) THEN
            NFIELDS = NFIELDS + 1 
            FIELDS(NFIELDS) = MACRO_TIMER_MONVOL  
            TITLES(MACRO_TIMER_MONVOL) = 'UP' 
          ENDIF
          IF(TABTIME(MACRO_TIMER_FVMBAG,1) > 0) THEN
            NFIELDS = NFIELDS + 1 
            FIELDS(NFIELDS) = MACRO_TIMER_FVMBAG  
            TITLES(MACRO_TIMER_FVMBAG) = 'FVMESH' 
          ENDIF
          IF(TABTIME(MACRO_TIMER_FVMBAG,1) > 0) THEN
            NFIELDS = NFIELDS + 1 
            FIELDS(NFIELDS) = MACRO_TIMER_FVMBAG1  
            TITLES(MACRO_TIMER_FVMBAG1) = 'FVBAG' 
          ENDIF
          IF(NFIELDS > 0) CALL PRINT_BLOCK("** AIRBAGS **",TABTIME,NTIMAX,
     .                      FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)
          ENDIF


C /INTER/TYPE25 25
          NFIELDS = 0
          J = MACRO_TIMER_T25SLIDING
          IF(TABTIME(J,1) > 0) THEN
            NFIELDS = NFIELDS + 1 
            FIELDS(NFIELDS) = J 
            TITLES(J) = 'SLIDING' 
          ENDIF
          J = MACRO_TIMER_T25NORM
          IF(TABTIME(J,1) > 0) THEN
            NFIELDS = NFIELDS + 1 
            FIELDS(NFIELDS) = J 
            TITLES(J) = 'NORM' 
          ENDIF
          J = MACRO_TIMER_T25STFE
          IF(TABTIME(J,1) > 0) THEN
            NFIELDS = NFIELDS + 1 
            FIELDS(NFIELDS) = J 
            TITLES(J) = 'GETSTIF' 
          ENDIF
          J = MACRO_TIMER_T25VOX0
          IF(TABTIME(J,1) > 0) THEN
            NFIELDS = NFIELDS + 1 
            FIELDS(NFIELDS) = J 
            TITLES(J) = 'VOX0' 
          ENDIF
          J = MACRO_TIMER_T25VOX0E2E
          IF(TABTIME(J,1) > 0) THEN
            NFIELDS = NFIELDS + 1 
            FIELDS(NFIELDS) = J 
            TITLES(J) = 'VOX0E2E' 
          ENDIF
          J = MACRO_TIMER_T25RNUM
          IF(TABTIME(J,1) > 0) THEN
            NFIELDS = NFIELDS + 1 
            FIELDS(NFIELDS) = J 
            TITLES(J) = 'RNUM' 
          ENDIF
          J = MACRO_TIMER_T25RNUME
          IF(TABTIME(J,1) > 0) THEN
            NFIELDS = NFIELDS + 1 
            FIELDS(NFIELDS) = J 
            TITLES(J) = 'RNUME2E' 
          ENDIF
          J = MACRO_TIMER_T25BUC
          IF(TABTIME(J,1) > 0) THEN
            NFIELDS = NFIELDS + 1 
            FIELDS(NFIELDS) = J 
            TITLES(J) = 'BUC' 
          ENDIF
          J = MACRO_TIMER_T25BUCE2E
          IF(TABTIME(J,1) > 0) THEN
            NFIELDS = NFIELDS + 1 
            FIELDS(NFIELDS) = J 
            TITLES(J) = 'BUCE2E' 
          ENDIF
          J = MACRO_TIMER_T25TRCE
          IF(TABTIME(J,1) > 0) THEN
            NFIELDS = NFIELDS + 1 
            FIELDS(NFIELDS) = J 
            TITLES(J) = 'TRCE2E' 
          ENDIF

          IF(NFIELDS > 0) CALL PRINT_BLOCK("**/INTER/TYPE25 **",TABTIME,NTIMAX,
     .                      FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)


          NFIELDS = 0
          IF (NUMSPH > 0)THEN
            FIELDS(1 )=48    ; TITLES(FIELDS(1 )) = 'SPH_TOT'         
            FIELDS(2 )=87    ; TITLES(FIELDS(2 )) = 'SPH_SORT'        
            FIELDS(3 )=88    ; TITLES(FIELDS(3 )) = 'SPH_FORCES'     
            FIELDS(4 )=89    ; TITLES(FIELDS(4 )) = 'SPH_OTHER'      
            FIELDS(5 )=90    ; TITLES(FIELDS(5 )) = 'SPH_SORT1'       
            FIELDS(6 )=91    ; TITLES(FIELDS(6 )) = 'COMSORT1'   
            FIELDS(7 )=94    ; TITLES(FIELDS(7 )) = 'SPH_SORT0'      
            FIELDS(8 )=92    ; TITLES(FIELDS(8 )) = 'COMM_SORT0'       
            FIELDS(9 )=93    ; TITLES(FIELDS(9 )) = 'COMM_FORCES'       
            NFIELDS = 9
            CALL PRINT_BLOCK("** SPH TIME **",TABTIME,NTIMAX,
     .                      FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)
          ENDIF
          IF(TABTIME(MACRO_TIMER_GENH3D,1) > 0 ) THEN
          
            FIELDS(1 )=MACRO_TIMER_GENH3D ; TITLES(FIELDS(1 ))= 'TOTAL'         
            FIELDS(2 )=MACRO_TIMER_SPMDH3D; TITLES(FIELDS(2 ))= 'SPMD'      
            FIELDS(3 )=MACRO_TIMER_LIBH3D ; TITLES(FIELDS(3 ))= 'H3DLIB'       
c           FIELDS(4 )=MACRO_TIMER_GENH3D1; TITLES(FIELDS(4 ))= 'SOLID'      
c           FIELDS(5 )=MACRO_TIMER_GENH3D2; TITLES(FIELDS(5 ))= 'WRITE'       

            NFIELDS = 3

            CALL PRINT_BLOCK("** H3D ANIMATIONS **",TABTIME,NTIMAX,
     .                      FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)
          ENDIF
C

          
          IF (IMONM == 2) THEN
            FIELDS(1 )=56    ; TITLES(FIELDS(1 )) = 'BEG_CRIT'         
            FIELDS(2 )=57    ; TITLES(FIELDS(2 )) = 'EOF_CRIT'        
            FIELDS(3 )=58    ; TITLES(FIELDS(3 )) = 'EOF_SORT'     
            FIELDS(4 )=59    ; TITLES(FIELDS(4 )) = 'EOF_FORCES'      
            FIELDS(5 )=60    ; TITLES(FIELDS(5 )) = 'EOF_CYCLE'       
            FIELDS(6 )=95    ; TITLES(FIELDS(6 )) = 'EOF_SPH'   
            NFIELDS = 6

            CALL PRINT_BLOCK("** FORCED SYNCH",TABTIME,NTIMAX,
     .                      FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)

          ENDIF
        ENDIF !IMONM > 0


      END SUBROUTINE


Chd|====================================================================
Chd|  PRINT_BLOCK                   source/system/timer.F         
Chd|-- called by -----------
Chd|        PRINT_SUMMARY                 source/system/timer.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRINT_BLOCK(TITLE_PART,TABTIME,NTIMAX,
     . FLAG,OUT_ID,NFIELDS,FIELDS,TITLES)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "macro.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
C#include      "timerr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NTIMAX
        CHARACTER(LEN=*), INTENT(IN) :: TITLE_PART
        REAL, INTENT(INOUT) :: TABTIME(NTIMAX,PARASIZ) 
        INTEGER, INTENT(IN) :: FLAG ! 1 : No Min/Max 
                                    ! 2 :    Min/Max    
        INTEGER, INTENT(IN) :: OUT_ID ! file descriptor
        INTEGER, INTENT(IN) :: NFIELDS ! number of columns (must be <= 15 )
        INTEGER, INTENT(IN) :: FIELDS(NFIELDS) ! number of columns

        CHARACTER(LEN=9), INTENT(IN) :: TITLES(NTIMAX) ! number of columns
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        CHARACTER(LEN=22) :: FORMAT1="(1x,A4,1x,15(1x,E9.4))"
        CHARACTER(LEN=22) :: FORMAT2="(1x,A5,1x,15(1x,A9))"
        CHARACTER(LEN=22) :: FORMAT3="(17x,A20)"
        CHARACTER(LEN=22) :: FORMAT4="(I4,2x,15(1x,E9.4))"
        CHARACTER(LEN=22) :: FORMAT5="(A4,2x,15(1x,E9.4))"

        DOUBLE PRECISION  :: DMIN(NFIELDS) 
        DOUBLE PRECISION  :: DMAX(NFIELDS)
        DOUBLE PRECISION  :: AVG(NFIELDS)

        CHARACTER(LEN=9)  :: TITLES_LOC(NFIELDS) ! number of columns
        DOUBLE PRECISION  :: ARRAY(15)
        INTEGER I,J

        DO J = 1,NFIELDS
          TITLES_LOC(j) = TITLES(FIELDS(J))
        ENDDO

        WRITE(OUT_ID,*)' '
        WRITE(OUT_ID,FORMAT3) TITLE_PART
        WRITE(OUT_ID,*)' '
        WRITE(OUT_ID,FORMAT2) "#PROC",TITLES_LOC(1:NFIELDS)
        DO I = 1,NSPMD
          DO J = 1,NFIELDS
            ARRAY(J) = TABTIME(FIELDS(J),I)
          ENDDO
            WRITE(OUT_ID,FORMAT4) I,ARRAY(1:NFIELDS)
        ENDDO
    
        IF(FLAG==2 .AND. NSPMD > 2) THEN 
          DO I = 1, NFIELDS
            AVG(I)=ZERO
            DMAX(I)=TABTIME(FIELDS(I),1)
            DMIN(I)=TABTIME(FIELDS(I),1)
          END DO
          DO I = 1, NSPMD
            DO J = 1, NFIELDS
              AVG(J)=AVG(J)+TABTIME(FIELDS(J),I)
              DMIN(J)=MIN(DMIN(J)  ,TABTIME(FIELDS(J),I))
              DMAX(J)=MAX(DMAX(J)  ,TABTIME(FIELDS(J),I))
            END DO
          END DO
          DO I=1,NFIELDS
            AVG(I)=AVG(I)/NSPMD
          END DO

          WRITE(OUT_ID,*)' '
          WRITE(OUT_ID,FORMAT2) "     ",TITLES_LOC(1:NFIELDS)
          WRITE(OUT_ID,FORMAT5) "MIN",DMIN(1:NFIELDS)
          WRITE(OUT_ID,FORMAT5) "MAX",DMAX(1:NFIELDS)
          WRITE(OUT_ID,FORMAT5) "AVG",AVG(1:NFIELDS)
        ENDIF
      END SUBROUTINE

Chd|====================================================================
Chd|  INITIME                       source/system/timer.F         
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INITIME()
C     initialisation des timers
C     timer                       signification
C
C     1                           resol
C     2                           interfaces (total ou tri fop) [tri type 7, ALE]
C     3                           elements
C     4                           cond. cin.
C     5                           integration
C     6                           non parallel sur p0
C     7                           IO
C     8                           interfaces (forces fop) [i7opcd,i7dst3,i7for3,shooting nodes]
C     9                           assemblage forces
C    10                           echange forces spmd
C    11                           echange rigid bodies forces
C    12                           echange rigid bodies vitesses
C    13                           echange vitesse spmd
C    14                           -----
C********* TIMERS 15 A 25 ACTIVE sur /DEBUG/CAND + /MON/ON ***************                        
c    15                           interface i7buce_crit 
C    16                           interface icomcrit (comm critere de tris)
C    17                           interface i7main_tri (tri)
C    18                           interface spmd_ifront     (maj frontieres)
C    19                           interface i7optcd 
C    20                           interface force i7for3
C    21                           interface envoi force
C    22                           interface reception force
C    23                           interface envoi vitesse
C    24                           interface reception vitesse
C    25                           interface envoi XV tri (TRI7BOX)
C    26                           interface maj cand tri (no comm, included in i7maindb)
C    27                           interface tri mmx (comm only, included in spmd_i7crit)
C    28                           interface tied type 2
C    29                           interface shooting nodes
C    30                           interface i7buce pure (sans comm)
c    31                           [K] setup 
C    32                           [M] setup
C    33                           implicit solver
C    34                           total implicit     
C    35                           time spent in Material routines
C    36                           adaptive meshing : criteria
C    37                           adaptive meshing : kinematic conditions forces
C    38                           adaptive meshing : kinematic conditions velocities
C    39                           selective mass scaling
C********* TIMERS 40 A 60 ACTIVE sur /DEBUG/CAND + /MON/ON ***************
C    40                           cond cine rbodies + sensors + accelerometres 
C    41                           cond cine concentrated load forces
C    42                           cond cine boundary conditions
C    43                           cond cine rlinks + rivets + cjoints + rwalls
C    44                           cond cine fixvel + fixtemp
C    45                           cond cine RBE3
C    46                           cond cine gravity
C    47                           task0 manctr
C    48                           sph (total)
C    49                           task0 geo+uwin+tfum+desacti
C    50                           task0 airbag
C    51                           task0 forints 
C    52                           task0 damping
C    53                           task0 DT2
C    54                           task0 R2R
C    55                           -----
C    56                           synchro avant critere tri 
C    57                           synchro apres critere tri 
C    58                           synchro fin de tri 
C    59                           synchro apres element+interf forces 
C    60                           synchro fin cycle si cc a envoyer 
C********* TIMERS 61 A 70 ACTIVE sur /MON/FULL ***************
C    61                           AMS PCG  PCG       :: PCG sauf produits matrice-vecteur W=MV 
C    62                           AMS PCG  PARIT F   :: calcul des normes et produits scalaires P/ON (sum_6_float)
C    63                           AMS PCG  COMM R    :: comm. pour calcul des normes et produiys scalaires
C    64                           AMS PCG  COMP M.V  :: calcul W=MV hors communications
C    65                           AMS PCG  COMM VFI  :: COMM. ECHANGE VFI cf contacts
C    66                           IMP PCG
C    67                           IMP PCG
C    68                           IMP PCG
C    69                           IMP PCG
C    70                           AMS EIGENVECTORS
C******** 71 A 74 ACTIVE sur /MON/FULL ***********************
C    71                           AMS EIGENVECTORS
C    72                           AMS EIGENVECTORS
C    73                           AMS EIGENVECTORS
C    74                           AMS PCG  COMP MV/E :: calcul W=MV wrt matrice elementaire uniquement - inclus dans time(64)
C    75                           AMS BUILD MATRIX
C    76                           synchro fin de cycle comm shooting
C
C    75 - 86                      utilise, mais non decrit ..                          
C
C    80                           AMS PCG COMM M.V   :: COMM. pour ASSEMBLAGE de W=MV
C
C    87                           sph preparation (sorting, ...)
C    88                           sph interactions (forintp)
C    89                           sph others
C******** 90 A 95 SPH************************************
C    90                           SPH/SORT1 including 91
C    91                           SPH/COMM.SORT1
C    94                           SPH/SORT0 including 92
C    92                           SPH/COMM.SORT0
C    93                           SPH/COMM.FORCES
C    95                           SPH/LOAD INBALANCE SORTING
C******** 96 A 100 LIBRES ************************************
C******** 106 /INTER/TYPE25 Specific  ************************
C   106                           /INTER/TYPE25 Normals computation
C   107                           Libre
C   108                           Libre
C   109                           Libre
C********* 110-119  ALE ***************************
C********* 120-122  H3D ANIM***********************
C********  149-150  temporary *********************

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "macro.inc"
#include      "task_c.inc"
#include      "timerr_c.inc"
      COMMON /ICLOCK/CLOCK0
      INTEGER CLOCK0
      COMMON /RCLOCK/ELAPSED
      DOUBLE PRECISION ELAPSED
#if _OPENMP
      REAL(kind=8) OMP_get_wtime
      external OMP_get_wtime
#endif
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C-----------------------------------------------
      DO I =1, PARASIZ 
        DO J = 1, NTIMAX 
          CPUTIME(J,I) = ZERO
          SYSTIME(J,I) = ZERO
          REALTIME(J,I) = ZERO
        ENDDO 
      ENDDO
      CALL SYSTEM_CLOCK(CLOCK0)   ! recuperation du temps de debut dans clock0
      IF(CLOCK0 < 0 )  CALL SYSTEM_CLOCK(CLOCK0)   !In case of failure
      ELAPSED = ZERO  ! initialisation a zero de l'elapsed time

#if _OPENMP
        OMP_STARTING_TIME = OMP_GET_WTIME( )
#endif
C
      RETURN
      END

Chd|====================================================================
Chd|  STARTIME                      source/system/timer.F         
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        ALEWDX                        source/ale/alewdx.F           
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|        DYNA_INA                      source/implicit/imp_dyna.F    
Chd|        DYNA_WEX                      source/implicit/imp_dyna.F    
Chd|        FORINTP                       source/elements/forintp.F     
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|        I11MAINF                      source/interfaces/int11/i11mainf.F
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|        I17MAIN_TRI                   source/interfaces/int17/i17main_pena.F
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|        I21MAINF                      source/interfaces/int21/i21mainf.F
Chd|        I21MAIN_TRI                   source/interfaces/intsort/i21main_tri.F
Chd|        I21OPTCD                      source/interfaces/int21/i21optcd.F
Chd|        I22MAINF                      source/interfaces/int22/i22mainf.F
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|        I23MAINF                      source/interfaces/int23/i23mainf.F
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|        I25MAIN_FREE                  source/interfaces/intsort/i25main_free.F
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_K_EIG                     stub/imp_k_eig.F              
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        INTCRIT                       source/interfaces/intsort/intcrit.F
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVHM                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|        MAV_LTH                       source/implicit/produt_v.F    
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        MULTI_MUSCL_GRADIENTS         source/multifluid/multi_muscl_gradients.F
Chd|        PREC_SOLVH                    source/implicit/prec_solv.F   
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|        PRODUT_HP                     source/implicit/produt_v.F    
Chd|        RBYFOR                        source/constraints/general/rbody/rbyfor.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RMATFORP                      source/materials/mat/mat013/rmatforp.F
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SMS_CHECK                     source/ams/sms_fsa_inv.F      
Chd|        SMS_INIST                     source/ams/sms_proj.F         
Chd|        SMS_INIX                      source/ams/sms_proj.F         
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|        SMS_MAV_LT                    source/ams/sms_pcg.F          
Chd|        SMS_MAV_LT1                   source/ams/sms_pcg.F          
Chd|        SMS_MAV_LT2                   source/ams/sms_pcg.F          
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|        SMS_PRO_P                     source/ams/sms_proj.F         
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|        SPHTRI0                       source/elements/sph/sphtri0.F 
Chd|        SPMD_CELL_EXCHANGE            source/mpi/generic/spmd_cell_exchange.F
Chd|        SPMD_E1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_E4VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_E6VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_ENVOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_EVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_EXALEW                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_EXALEW_PON               source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_EXTAG                    source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_GLOB_DMIN9               source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_I4VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_I8VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_INIT_EBCS                source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_L11VOIS                  source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_L51VOIS                  source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_LAGBOUNDS_EXCH_I22       source/mpi/interfaces/spmd_lagbounds_exch_i22.F
Chd|        SPMD_SEGCOM                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_TRI11VOX                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI18_151VOX             source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI20BOX                 source/mpi/interfaces/spmd_tri20box.F
Chd|        SPMD_TRI22VOX                 source/mpi/interfaces/spmd_tri22vox.F
Chd|        SPMD_TRI24VOX                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI25VOX                 source/mpi/interfaces/spmd_tri25vox.F
Chd|        SPMD_TRI7VOX                  source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI7VOX_OPTIMIZED        source/mpi/interfaces/spmd_tri7vox_optimized.F
Chd|        SPMD_WVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_XVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        MY_ETIME                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE STARTIME(EVENT,ITASK)
C     routine d'incrementation du timer event
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "timerr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EVENT,ITASK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#if _OPENMP
       REAL(kind=8) OMP_GET_WTIME
       external OMP_GET_WTIME
#endif
C
C pour eviter erreur passage hybrid
      CALL MY_ETIME(TIMER(1,EVENT,ITASK))
#if _OPENMP
      OMP_INITIME(EVENT,ITASK) = OMP_GET_WTIME( )
#else
      CALL SYSTEM_CLOCK(CLOCKINI(EVENT,ITASK))
      IF(CLOCKINI(EVENT,ITASK)< 0 ) CALL SYSTEM_CLOCK(CLOCKINI(EVENT,ITASK))
#endif


      RETURN
      END

Chd|====================================================================
Chd|  STOPTIME                      source/system/timer.F         
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        ALEWDX                        source/ale/alewdx.F           
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|        DYNA_INA                      source/implicit/imp_dyna.F    
Chd|        DYNA_WEX                      source/implicit/imp_dyna.F    
Chd|        FORINTP                       source/elements/forintp.F     
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|        I11MAINF                      source/interfaces/int11/i11mainf.F
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|        I17MAIN_TRI                   source/interfaces/int17/i17main_pena.F
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|        I21MAINF                      source/interfaces/int21/i21mainf.F
Chd|        I21MAIN_TRI                   source/interfaces/intsort/i21main_tri.F
Chd|        I22MAINF                      source/interfaces/int22/i22mainf.F
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|        I23MAINF                      source/interfaces/int23/i23mainf.F
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|        I25MAIN_FREE                  source/interfaces/intsort/i25main_free.F
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_K_EIG                     stub/imp_k_eig.F              
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        INTCRIT                       source/interfaces/intsort/intcrit.F
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVHM                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|        MAV_LTH                       source/implicit/produt_v.F    
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        MULTI_MUSCL_GRADIENTS         source/multifluid/multi_muscl_gradients.F
Chd|        PREC_SOLVH                    source/implicit/prec_solv.F   
Chd|        RBYFOR                        source/constraints/general/rbody/rbyfor.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RMATFORP                      source/materials/mat/mat013/rmatforp.F
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SMS_CHECK                     source/ams/sms_fsa_inv.F      
Chd|        SMS_INIST                     source/ams/sms_proj.F         
Chd|        SMS_INIX                      source/ams/sms_proj.F         
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|        SMS_MAV_LT                    source/ams/sms_pcg.F          
Chd|        SMS_MAV_LT1                   source/ams/sms_pcg.F          
Chd|        SMS_MAV_LT2                   source/ams/sms_pcg.F          
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|        SMS_PRO_P                     source/ams/sms_proj.F         
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|        SPHTRI0                       source/elements/sph/sphtri0.F 
Chd|        SPMD_CELL_EXCHANGE            source/mpi/generic/spmd_cell_exchange.F
Chd|        SPMD_E1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_E4VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_E6VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_ENVOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_EVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_EXALEW                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_EXALEW_PON               source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_EXTAG                    source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_GLOB_DMIN9               source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_I4VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_I8VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_INIT_EBCS                source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_L11VOIS                  source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_L51VOIS                  source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_LAGBOUNDS_EXCH_I22       source/mpi/interfaces/spmd_lagbounds_exch_i22.F
Chd|        SPMD_SEGCOM                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_TRI18_151VOX             source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI20BOX                 source/mpi/interfaces/spmd_tri20box.F
Chd|        SPMD_TRI22VOX                 source/mpi/interfaces/spmd_tri22vox.F
Chd|        SPMD_TRI24VOX                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI25VOX                 source/mpi/interfaces/spmd_tri25vox.F
Chd|        SPMD_TRI7VOX                  source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI7VOX_OPTIMIZED        source/mpi/interfaces/spmd_tri7vox_optimized.F
Chd|        SPMD_WVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_XVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        MY_ETIME                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE STOPTIME(EVENT,ITASK)
C     routine incrementation du timer event
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "timerr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EVENT,ITASK
C
      INTEGER CLOCK1, CLOCKRATE, NBMAX
      DOUBLE PRECISION  SECS
      REAL(KIND=8) :: OMP_ENDING_TIME
#if _OPENMP
       REAL(kind=8) OMP_GET_WTIME
       external OMP_GET_WTIME
#endif
C pour eviter erreur passage hybrid
C
      CALL MY_ETIME(TIMER(3,EVENT,ITASK))
      CPUTIME(EVENT,ITASK) = CPUTIME(EVENT,ITASK) +
     .                       TIMER(3,EVENT,ITASK)-TIMER(1,EVENT,ITASK)
      SYSTIME(EVENT,ITASK) = SYSTIME(EVENT,ITASK) +
     .                       TIMER(4,EVENT,ITASK)-TIMER(2,EVENT,ITASK)
#if _OPENMP
      OMP_ENDING_TIME = OMP_GET_WTIME( )
      SECS = OMP_ENDING_TIME - OMP_INITIME(EVENT,ITASK)
#else
      CALL SYSTEM_CLOCK(COUNT=CLOCK1, COUNT_RATE=CLOCKRATE,
     +                  COUNT_MAX=NBMAX                   )
      IF(CLOCK1 < 0 ) THEN 
      ! Retry in case of failure
        CALL SYSTEM_CLOCK(COUNT=CLOCK1, COUNT_RATE=CLOCKRATE,
     +                  COUNT_MAX=NBMAX                   )
      ENDIF

      SECS = CLOCK1-CLOCKINI(EVENT,ITASK)    ! diff temps courant - temps initial
      IF(SECS.LT.ZERO) SECS = SECS + NBMAX   ! cas depassement nb de periode maximum
      SECS = SECS/CLOCKRATE
#endif

      REALTIME(EVENT,ITASK)=REALTIME(EVENT,ITASK)+SECS
C
      RETURN
      END

Chd|====================================================================
Chd|  PRINTIME                      source/system/timer.F         
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CLOSE_C                       source/output/tools/sortie_c.c
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        ELAPSTIME                     source/system/timer.F         
Chd|        FSEEK_END_C                   source/output/tools/sortie_c.c
Chd|        GET_MEM_MAP_VAR               source/system/timer.F         
Chd|        MAP_MEMORY                    ../common_source/comm/memory_use_c.c
Chd|        OPEN_C                        source/output/tools/sortie_c.c
Chd|        PRINT_SUMMARY                 source/system/timer.F         
Chd|        RAD_SPMD_RECV                 source/mpi/generic/rad_spmd_recv.F
Chd|        RAD_SPMD_SEND                 source/mpi/generic/rad_spmd_send.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        WRITE_DPDB                    source/output/tools/write_db.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        TIME_MOD                      share/modules/time_mod.F      
Chd|====================================================================
      SUBROUTINE PRINTIME(ITASK)
C     routine d'affichage des timers
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIME_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include        "com01_c.inc"
#include        "com04_c.inc"
#include        "couple_c.inc"
#include        "impl1_c.inc"
#include        "remesh_c.inc"
#include        "scr05_c.inc"
#include        "scr07_c.inc"
#include        "scr18_c.inc"
#include        "alefvm.inc"
#include        "sms_c.inc"
#include        "sphcom.inc"
#include        "task_c.inc"
#include        "timerc_c.inc"
#include        "timeri_c.inc"
#include        "timerr_c.inc"
#include        "units_c.inc"
#include        "warn_c.inc"
#include        "filescount_c.inc"
      COMMON /ICLOCK/CLOCK0
      INTEGER CLOCK0
      COMMON /RCLOCK/ELAPSED
      DOUBLE PRECISION ELAPSED
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NPROC, MSGTAG, IRSIZE, J, IH, IM, IS
      DOUBLE PRECISION  SECS, TOTAL, CUMUL(NTIMAX),
     .        AVERAGE(NTIMAX), DEVIATION(NTIMAX),
     .        DMIN(NTIMAX), DMAX(NTIMAX),CUMUL_MAT,CUMUL_ELEM,
     .        CUMUL_FAIL,ARRAY(NSPMD),X_MINVALUE,X_MAXVALUE,Y_MAXVALUE
      INTEGER MEMUSE(NSPMD),MEM_L,MMIN,MMAX,MTOT,MAVR,
     .        OUTSIZE,RFILESIZE,NB_INTERV
      INTEGER VMPEAK, VMSIZE,VMRSS,VMHWM, VMSTK,MEMSTAT(5,NSPMD),
     .        GOT_MEM_MAP
      CHARACTER(LEN=9) TITLES(NTIMAX)
      INTEGER FIELDS(11),NFIELDS
      REAL ZEROS
      INTEGER :: NF,IJK,IJKL,LEN_RST_NAME
      CHARACTER*8  DATE
      CHARACTER*10  TIME
      LOGICAL :: BOOL_FILE
      INTEGER :: CURRENT_RUN,OFFSET
      INTEGER, DIMENSION(2148) :: RST_NAME
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME
      INTEGER :: INDICE

      TITLES(MACRO_TIMER_RESOL      ) = "RESOL"
      TITLES(MACRO_TIMER_CONTSORT   ) = "CONTSORT"
      TITLES(MACRO_TIMER_CONTFOR    ) = "CONTFOR"
      TITLES(MACRO_TIMER_ELEMENT    ) = "ELEMENT"
      TITLES(MACRO_TIMER_KIN        ) = "KINCOND"
      TITLES(MACRO_TIMER_INTEG      ) = "INTEG"
      TITLES(MACRO_TIMER_P0         ) = "T0"
      TITLES(MACRO_TIMER_IO         ) = "IO"
      TITLES(MACRO_TIMER_ASM        ) = "ASM"
      TITLES(MACRO_TIMER_AMS        ) = "SMS"


      TITLES(MACRO_TIMER_ALEMAIN    ) = "ALEMAIN"
      TITLES(MACRO_TIMER_IFSUB0     ) = "IFSUB0"
      TITLES(MACRO_TIMER_MULTIFVM   ) = "MULTIFVM"
      TITLES(MACRO_TIMER_SPMDCFD    ) = "SPMDCFD"
      TITLES(MACRO_TIMER_MUSCL    ) = "MUSCL"

      
      TITLES(31) = '[K]SETUP'
      TITLES(32) = '[M]SETUP'
      TITLES(33) = 'IMP_SOLV'
      TITLES(34) = 'IMP_TOT'
      TITLES(36) = 'CRITER'
      TITLES(37) = 'FOR'
      TITLES(38) = 'VEL'
C
      ZEROS=ZERO
      IF (ICRAY.EQ.1) THEN
        IRSIZE = 8
      ELSE
        IRSIZE = 4
      ENDIF
C
      VMPEAK=-1
      VMSIZE=-1
      VMRSS=-1
      VMHWM=-1
      VMSTK=-1
      CALL MAP_MEMORY(VMPEAK, VMSIZE,VMRSS,VMHWM, VMSTK)
      NPROC = NSPMD
C nproc = nombre de domaines, toutes les taches SMP comptent pour un seul proc
CC      NPROC = MAX(NTHREAD,NSPMD)
CC smp nproc = ntask, spmd nproc = NSPMD
      IF (NSPMD > 1) THEN
C gather des cpu et sys time pour spmd
        MSGTAG = 1000
        IF (ISPMD.NE.0) THEN
          CALL RAD_SPMD_SEND(CPUTIME,NTIMAX*IRSIZE,0,IT_SPMD,
     .                       MSGTAG+ISPMD+1,INTSIZE)
          CALL RAD_SPMD_SEND(SYSTIME,NTIMAX*IRSIZE,0,IT_SPMD,
     .                       MSGTAG+ISPMD+1,INTSIZE)
          CALL RAD_SPMD_SEND(REALTIME,NTIMAX*IRSIZE,0,IT_SPMD,
     .                       MSGTAG+ISPMD+1,INTSIZE)
        ELSE
          DO I = 2, NSPMD
            CALL RAD_SPMD_RECV(CPUTIME(1,I),NTIMAX*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            CALL RAD_SPMD_RECV(SYSTIME(1,I),NTIMAX*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            CALL RAD_SPMD_RECV(REALTIME(1,I),NTIMAX*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
          ENDDO
        ENDIF

C Gather de la m  moire utilis  e
        MSGTAG = 1100
        MEMUSE=0
        MEM_L=0

        MEMUSE(ISPMD+1)=VMPEAK
        MEMSTAT(1,ISPMD+1) = VMPEAK
        MEMSTAT(2,ISPMD+1) = VMSIZE
        MEMSTAT(3,ISPMD+1) = VMRSS
        MEMSTAT(4,ISPMD+1) = VMHWM
        MEMSTAT(5,ISPMD+1) = VMSTK

        IF (ISPMD.NE.0) THEN
          CALL RAD_SPMD_SEND( MEMUSE(ISPMD+1),1*IRSIZE,0,IT_SPMD,
     .                        MSGTAG+ISPMD+1,INTSIZE)
          CALL RAD_SPMD_SEND( VMSIZE,1*IRSIZE,0,IT_SPMD,
     .                        MSGTAG+ISPMD+1,INTSIZE)
          CALL RAD_SPMD_SEND( VMRSS,1*IRSIZE,0,IT_SPMD,
     .                        MSGTAG+ISPMD+1,INTSIZE)
          CALL RAD_SPMD_SEND( VMHWM,1*IRSIZE,0,IT_SPMD,
     .                        MSGTAG+ISPMD+1,INTSIZE)
          CALL RAD_SPMD_SEND( VMSTK,1*IRSIZE,0,IT_SPMD,
     .                        MSGTAG+ISPMD+1,INTSIZE)
        ELSE
          DO I = 2, NSPMD
            CALL RAD_SPMD_RECV(MEMUSE(I),1*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            MEMSTAT(1,I) = MEMUSE(I)
            CALL RAD_SPMD_RECV(MEMSTAT(2,I),1*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            CALL RAD_SPMD_RECV(MEMSTAT(3,I),1*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            CALL RAD_SPMD_RECV(MEMSTAT(4,I),1*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            CALL RAD_SPMD_RECV(MEMSTAT(5,I),1*IRSIZE,I-1,IT_SPMD,
     .                         MSGTAG+I,INTSIZE)
            
          ENDDO

          INDICE = 1
          IF(MEMSTAT(4,1)/=-1) THEN
             INDICE = 4    
          ELSEIF(MEMSTAT(1,1)/=-1) THEN
             INDICE = 1
          ELSEIF(MEMSTAT(2,1)/=-1) THEN
             INDICE = 2
          ELSEIF(MEMSTAT(3,1)/=-1) THEN
             INDICE = 3
          ENDIF
          MMIN = MEMSTAT(INDICE,1)
          MMAX = MEMSTAT(INDICE,1)
          MTOT = MEMSTAT(INDICE,1)
          DO I=2,NSPMD
            IF (MMIN > MEMSTAT(INDICE,I) ) THEN
              MMIN = MEMSTAT(INDICE,I)
            ENDIF
            IF (MMAX < MEMSTAT(INDICE,I) ) THEN
              MMAX = MEMUSE(I)
            ENDIF
            MTOT = MTOT+ MEMSTAT(INDICE,I)
          ENDDO
          MAVR=MTOT/NSPMD
        ENDIF
      ELSE
C Monoproc run
C Gestion de la m  moire utilisee
        IF(VMHWM/=-1) THEN
            MEM_L = VMHWM
        ELSEIF(VMPEAK/=-1) THEN
            MEM_L = VMPEAK
        ELSEIF(VMSIZE/=-1) THEN
            MEM_L = VMSIZE
        ELSE
            MEM_L = -1
        ENDIF
        MMIN = MEM_L
        MMAX = MEM_L
        MTOT = MEM_L
        MAVR = MEM_L
      ENDIF
C
C Gather de la taille des fichiers restart
      IF(NSPMD > 1)CALL SPMD_GLOB_ISUM9(RESTARTFILESIZE,1)
C Gather taille des fichiers multiple restart
      IF(NSPMD > 1)CALL SPMD_GLOB_ISUM9(MULTIRESTS,26)
C


C
      IF (ISPMD.NE.0) RETURN

      WRITE(ISTDO,*)' '
      WRITE(ISTDO,*)'                          ** CPU USER TIME **'
      WRITE(ISTDO,*)' '

        CALL PRINT_SUMMARY(1,NTIMAX,CPUTIME,TITLES,ISTDO) 

      WRITE(IOUT,*)' '
      WRITE(IOUT,*)'                          ** CPU USER TIME **'
      WRITE(IOUT,*)' '

        
        CALL PRINT_SUMMARY(1,NTIMAX,CPUTIME,TITLES,IOUT) 




C
C
      IF(IMON_MAT==1)THEN
          CUMUL_MAT=ZERO
          CUMUL_ELEM=ZERO
          CUMUL_FAIL=ZERO
          DO I = 1, NPROC
            CUMUL_MAT = CUMUL_MAT +CPUTIME(35,I)
            CUMUL_ELEM= CUMUL_ELEM+CPUTIME(3,I)
            CUMUL_FAIL= CUMUL_FAIL+CPUTIME(121,I)
          ENDDO

          WRITE(IOUT,*)' '
          WRITE(IOUT,*)'                  ** MATERIAL LAWS COST **'
          WRITE(IOUT,*)' '
          WRITE(ISTDO,*)' '
          WRITE(ISTDO,*)'                  ** MATERIAL LAWS COST **'
          WRITE(ISTDO,*)' '
          WRITE(IOUT, '(A)')' #PROC  ELEM COST  MAT COST    FAIL COST'
          WRITE(ISTDO,'(A)')' #PROC  ELEM COST  MAT COST    FAIL COST'
          DO I = 1, NPROC
             WRITE(IOUT, '(I4,3x,E9.4,3x,E9.4,3x,E9.4)') I,CPUTIME(3,I),CPUTIME(35,I),CPUTIME(121,I)
             WRITE(ISTDO,'(I4,3x,E9.4,3x,E9.4,3x,E9.4)') I,CPUTIME(3,I),CPUTIME(35,I),CPUTIME(121,I)
          ENDDO
          WRITE(IOUT,*)' '
          WRITE(ISTDO,*)' '
          WRITE(IOUT,'(A,E9.4)')  ' TOTAL ELEMENT COST..............: ',CUMUL_ELEM
          WRITE(IOUT,'(A,E9.4)')  ' TOTAL MATERIAL COST.............: ',CUMUL_MAT
          WRITE(IOUT,'(A,E9.4)')  ' TOTAL FAIL COST.................: ',CUMUL_FAIL

          WRITE(ISTDO,'(A,E9.4)') ' TOTAL ELEMENT COST..............: ',CUMUL_ELEM
          WRITE(ISTDO,'(A,E9.4)') ' TOTAL MATERIAL COST.............: ',CUMUL_MAT
          WRITE(ISTDO,'(A,E9.4)') ' TOTAL FAIL COST.................: ',CUMUL_FAIL
          WRITE(IOUT,*)' '
          WRITE(ISTDO,*)' '
      ENDIF
C
      WRITE(IOUT,*)' '
C---------------------------
C Global timings
C---------------------------
      DO I = 1, NTIMAX
        CUMUL(I) = ZERO
      END DO
      DO I = 1,NPROC
        DO J = 1, NTIMAX
         CUMUL(J) = CUMUL(J) + CPUTIME(J,I)
        ENDDO
      ENDDO
      IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
        TOTAL = CUMUL(1)-CUMUL(2)-CUMUL(8)-CUMUL(3)-CUMUL(4)-CUMUL(5)
     -                  -CUMUL(9)-CUMUL(MACRO_TIMER_ALEMAIN)
      ELSE
        TOTAL = CUMUL(1)-CUMUL(2)-CUMUL(8)-CUMUL(3)-CUMUL(4)-CUMUL(5)
     -                  -CUMUL(9)-CUMUL(39)-CUMUL(MACRO_TIMER_ALEMAIN) 
      END IF
      IF(CUMUL(1)<=ZERO)CUMUL(1)=EM10


        WRITE(ISTDO,*) ''

      WRITE(ISTDO,*)
     .  '                  ** CUMULATIVE CPU TIME SUMMARY **'
      WRITE(ISTDO,*)' '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' CONTACT SORTING.............: ',
     +  CUMUL(2),100*CUMUL(2)/CUMUL(1),' % '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' CONTACT FORCES..............: ',
     +  CUMUL(8),100*CUMUL(8)/CUMUL(1),' % '
      IF(NINTER25/=0)                  
     * WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' ..INCLUDING CONTACT NORMALS: ',
     *  CUMUL(106),100*CUMUL(106)/CUMUL(1),' % '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' ELEMENT FORCES..............: ',
     +  CUMUL(3),100*CUMUL(3)/CUMUL(1),' % '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' KINEMATIC COND..............: ',
     +  CUMUL(4),100*CUMUL(4)/CUMUL(1),' % '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' INTEGRATION.................: ',
     +  CUMUL(5),100*CUMUL(5)/CUMUL(1),' % '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' ASSEMBLING..................: ',
     +  CUMUL(9),100*CUMUL(9)/CUMUL(1),' % '
      IF(IDTMINS/=0.OR.IDTMINS_INT/=0)THEN
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' AMS.........................: ',
     +  CUMUL(39),100*CUMUL(39)/CUMUL(1),' % '
      END IF
      IF(IALE+IEULER+ITHERM.NE.0) THEN
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' ALE.........................: ',
     + CUMUL(MACRO_TIMER_ALEMAIN),
     + 100*CUMUL(MACRO_TIMER_ALEMAIN)/CUMUL(1),' % '
      ENDIF

      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' OTHERS (including I/O)......: ',
     +  TOTAL,100*TOTAL/CUMUL(1),' % '
      WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' TOTAL.......................: ',
     +  CUMUL(1),100*CUMUL(1)/CUMUL(1),' % '
c     IF(NUMSPH .NE.0) THEN
c     WRITE(ISTDO,'(A,E9.4,3x,F6.2,A)')' SPH TOTAL...................: ',
c    + CUMUL(48),
c    + 100*CUMUL(48)/CUMUL(1),' % '
c     ENDIF



      WRITE(ISTDO,*)' '

        WRITE(IOUT,*) ''
      WRITE(IOUT,*)
     .  '                  ** CUMULATIVE CPU TIME SUMMARY **'
      WRITE(IOUT,*)' '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' CONTACT SORTING.............: ',
     +  CUMUL(2),100*CUMUL(2)/CUMUL(1),' % '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' CONTACT FORCES..............: ',
     +  CUMUL(8),100*CUMUL(8)/CUMUL(1),' % '
      IF(NINTER25/=0)
     *  WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' .. INCLUDING CONTACT NORMALS: ',
     *  CUMUL(106),100*CUMUL(106)/CUMUL(1),' % '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' ELEMENT FORCES..............: ',
     +  CUMUL(3),100*CUMUL(3)/CUMUL(1),' % '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' KINEMATIC COND..............: ',
     +  CUMUL(4),100*CUMUL(4)/CUMUL(1),' % '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' INTEGRATION.................: ',
     +  CUMUL(5),100*CUMUL(5)/CUMUL(1),' % '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' ASSEMBLING..................: ',
     +  CUMUL(9),100*CUMUL(9)/CUMUL(1),' % '
      IF(IDTMINS/=0.OR.IDTMINS_INT/=0)THEN
        WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' AMS.........................: ',
     +  CUMUL(39),100*CUMUL(39)/CUMUL(1),' % '
      END IF
      IF(IALE+IEULER+ITHERM.NE.0) THEN
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' ALE.........................: ',
     +  CUMUL(MACRO_TIMER_ALEMAIN)
     +  ,100*CUMUL(MACRO_TIMER_ALEMAIN)/CUMUL(1),' % '
        ENDIF
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' OTHERS (including I/O)......: ',
     +  TOTAL,100*TOTAL/CUMUL(1),' % '
      WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' TOTAL.......................: ',
     +  CUMUL(1),100*CUMUL(1)/CUMUL(1),' % '
c     IF(NUMSPH .NE.0) THEN
c     WRITE(IOUT,'(A,E9.4,3x,F6.2,A)')' SPH TOTAL...................: ',
c    + CUMUL(48),
c    + 100*CUMUL(48)/CUMUL(1),' % '
c     ENDIF

      WRITE(IOUT,*)' '

      
C
      
       WRITE(IOUT,*)' '                                               
       WRITE(IOUT,*)'                          ** ELAPSED TIME **'    
       WRITE(IOUT,*)' '                                               
        
        CALL PRINT_SUMMARY(2,NTIMAX,REALTIME,TITLES,IOUT) 

       IF(.FALSE.)THEN
        WRITE(IOUT,*)' '
        WRITE(IOUT,*)'               ** ADDITIONAL DEBUG TIMERS (2) **'
        WRITE(IOUT,*)' '
        DO I = 1, NPROC
           WRITE(IOUT,5550)I,
     .      REALTIME(96,I),REALTIME(97,I),REALTIME(98,I),REALTIME(99,I),
     .      REALTIME(100,I),REALTIME(101,I),REALTIME(102,I)
        ENDDO
       END IF
C
      WRITE(IOUT,*)' '

      WRITE(ISTDO,*)
     .  '                  ** MEMORY USAGE STATISTICS **'
      WRITE(ISTDO,*)' '
      WRITE(ISTDO,'(A,A,I8,A)')' TOTAL MEMORY USED ',
     +                   '.........................: ',MTOT,' MB'
      WRITE(ISTDO,'(A,A,I8,A)')' MAXIMUM MEMORY PER PROCESSOR'  ,
     *          '...............: ',  MMAX,' MB'
      WRITE(ISTDO,'(A,A,I8,A)')' MINIMUM MEMORY PER PROCESSOR',
     *          '...............: ',MMIN,' MB'
      WRITE(ISTDO,'(A,A,I8,A)')' AVERAGE MEMORY PER PROCESSOR',
     *          '...............: ', MAVR,' MB'
      WRITE(ISTDO,*)' '
     
      IF (IMONM > 0)THEN
        WRITE(ISTDO,'(A)') ' #PROC     MEMORY USED'
        DO I = 1, NPROC
           WRITE(ISTDO,'(I4,A,I8,A)') I,'       ',MEMUSE(I),' MB'
        ENDDO
        WRITE(ISTDO,*)' '
      ENDIF


C Output file
      WRITE(IOUT,*)
     .  '                  ** MEMORY USAGE STATISTICS **'
      WRITE(IOUT,*)' '
      WRITE(IOUT,'(A,A,I8,A)')' TOTAL MEMORY USED ',
     +                   '.........................: ',MTOT,' MB'
      WRITE(IOUT,'(A,A,I8,A)')' MAXIMUM MEMORY PER PROCESSOR'  ,
     *          '...............: ',  MMAX,' MB'
      WRITE(IOUT,'(A,A,I8,A)')' MINIMUM MEMORY PER PROCESSOR',
     *          '...............: ',MMIN,' MB'
      WRITE(IOUT,'(A,A,I8,A)')' AVERAGE MEMORY PER PROCESSOR',
     *          '...............: ', MAVR,' MB'
      WRITE(IOUT,*)' '

c      IF (IMONM > 0)THEN
c        WRITE(IOUT,'(A)') ' #PROC   MEMORY USE'
c        DO I = 1, NPROC
c           WRITE(IOUT,'(I4,A,I8,A)') I,'       ',MEMUSE(I),' MB'
c        ENDDO
c        WRITE(IOUT,*)' '
c      ENDIF

C workaround / one variable in commandline.inc conflicts with scr07.inc
      CALL GET_MEM_MAP_VAR(GOT_MEM_MAP)
      IF(GOT_MEM_MAP==1 .OR. IMONM > 0)THEN
        WRITE(IOUT,'(A)') '                  ** PROCESS MEMORY MAPPING'
        WRITE(IOUT,*)' '
        WRITE(IOUT,'(A)') ' #PROC    VMPEAK     VMSIZE      VMRSS      VMHWM      VMSTK'
        DO I = 1, NPROC
           WRITE(IOUT,'(I4,A,I10,A,I10,A,I10,A,I10,A,I10)') 
     *       I,'  ',MEMSTAT(1,I),' ',MEMSTAT(2,I),' ',MEMSTAT(3,I),' ',
     *              MEMSTAT(4,I),' ',MEMSTAT(5,I)
        ENDDO
        WRITE(ISTDO,*)' '
         
      ENDIF

C Engine Disk usage Statistics
!     ANIMTOTALSIZE : kb
!     THFILESIZE : kb
!     OUTPFILESIZE : kb
!     H3DTOTALSIZE : mb --> kb
      H3DTOTALSIZE = H3DTOTALSIZE*1024
      OUTSIZE=ANIMTOTALSIZE+THFILESIZE+OUTPFILESIZE+H3DTOTALSIZE
      DO I=1,9
        OUTSIZE = OUTSIZE+MULTITHFILESIZE(I)
      ENDDO
        RFILESIZE=RESTARTFILESIZE
      DO I=1,26
        RFILESIZE = RFILESIZE+MULTIRESTS(I)
      ENDDO
      TOTALFILECOUNT = (OUTSIZE)+RFILESIZE+MUMPSFILESIZE+
     *                 BCSFILESIZE
      TOTALFILECOUNT = TOTALFILECOUNT/1024
      OUTSIZE = OUTSIZE / 1024
      RFILESIZE = RFILESIZE / 1024
      MUMPSFILESIZE = MUMPSFILESIZE / 1024
      BCSFILESIZE = BCSFILESIZE / 1024
      WRITE(ISTDO,*)
     .  '                  ** DISK USAGE STATISTICS **'
      WRITE(ISTDO,*)' '
      WRITE(ISTDO,'(A,A,I10,A)')' TOTAL DISK SPACE USED ',
     *                    '.....................: ',TOTALFILECOUNT,
     *                   ' MB'
      WRITE(ISTDO,'(A,A,I10,A)')' ANIMATION/H3D/TH/OUTP SIZE ',
     *                 '................: ',OUTSIZE,
     *                   ' MB'
      WRITE(ISTDO,'(A,A,I10,A)')' RESTART FILE SIZE ',
     *                   '.........................: ',
     *                  RFILESIZE,' MB'
      IF (MUMPSFILESIZE > 0)THEN
        WRITE(ISTDO,'(A,A,I10,A)')' MUMPS INTERNAL FILE USAGE ',
     *                   '.................: ',
     *                  MUMPSFILESIZE,' MB'
      ENDIF
      IF (BCSFILESIZE > 0) THEN
        WRITE(ISTDO,'(A,A,I10,A)')' BCS INTERNAL FILE USAGE ',
     *                   '...................: ',
     *                  BCSFILESIZE,' MB'
      ENDIF


      WRITE(ISTDO,*)' '


      WRITE(IOUT,*)
     .  '                  ** DISK USAGE STATISTICS **'
      WRITE(IOUT,*)' '
      WRITE(IOUT,'(A,A,I10,A)')' TOTAL DISK SPACE USED ',
     *                   '.....................: ',TOTALFILECOUNT,
     *                   ' MB'
      WRITE(IOUT,'(A,A,I10,A)')' ANIMATION/H3D/TH/OUTP SIZE ',
     *                   '................: ',OUTSIZE,
     *                   ' MB'
      WRITE(IOUT,'(A,A,I10,A)')' RESTART FILE SIZE ',
     *                   '.........................: ',
     *                  RFILESIZE,' MB'
      IF (MUMPSFILESIZE > 0)THEN
        WRITE(IOUT,'(A,A,I10,A)')' MUMPS INTERNAL FILE USAGE ',
     *                   '.................: ',
     *                  MUMPSFILESIZE/1024,' MB'
      ENDIF
      IF (BCSFILESIZE > 0) THEN
        WRITE(IOUT,'(A,A,I10,A)')' BCS INTERNAL FILE USAGE ',
     *                   '...................: ',
     *                  BCSFILESIZE/1024,' MB'
      ENDIF

      WRITE(IOUT,*)' '

C---------------------------
C Elapsed time & Estimated Speedup
C---------------------------
      CALL ELAPSTIME(SECS)
      CALL DATE_AND_TIME(DATE,TIME) 
!     ---------------------------
      IF(ISPMD==0) THEN
        CURRENT_RUN = GLOBAL_COMP_TIME%RUN_NBR
        GLOBAL_COMP_TIME%ENGINE_TIME(CURRENT_RUN) = SECS
        LEN_RST_NAME = LEN_TRIM(GLOBAL_COMP_TIME%RST_NAME)
        LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_RST_NAME
        TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//GLOBAL_COMP_TIME%RST_NAME(1:LEN_RST_NAME)

        DO I = 1, LEN_TMP_NAME
                RST_NAME(I) = ICHAR(TMP_NAME(I:I))
        END DO
        BOOL_FILE=.FALSE.
        INQUIRE(FILE=TMP_NAME(1:LEN_TMP_NAME), EXIST=BOOL_FILE)
!       check if *_0000.rst exists
        IF(BOOL_FILE) THEN
            CALL CUR_FIL_C(20)
            CALL OPEN_C(RST_NAME,LEN_TMP_NAME,2)
!           write the starter + engine elapsed time
            OFFSET= - STORAGE_SIZE(GLOBAL_COMP_TIME%ENGINE_TIME(CURRENT_RUN))/8        
            CALL FSEEK_END_C(OFFSET)
            CALL WRITE_DPDB(GLOBAL_COMP_TIME%ENGINE_TIME(CURRENT_RUN),1)
            CALL CLOSE_C
        ENDIF
      ENDIF
!      ---------------------------        


C      CALL SYSTEM_CLOCK(COUNT=CLOCK1, COUNT_RATE=CLOCKRATE,
C     +                  COUNT_MAX=NBMAX                   )
C      SECS = CLOCK1-CLOCK0         ! diff temps final - temps initial (qui peut ne pas etre 0 !)
C      IF(SECS.LT.ZERO) SECS = SECS + NBMAX   ! cas depassement nb de periode maximum
C      SECS = SECS/CLOCKRATE

      IH=SECS/3600
      IM=(SECS-IH*3600)/60
      IS=SECS-IH*3600-IM*60

      WRITE(IOUT,*)
     .  '                  ** COMPUTE TIME INFORMATION **'
      WRITE(IOUT,*)
     .  '                       ** CURRENT ENGINE **'
      WRITE(IOUT,*)' '

      WRITE(IOUT,6200)STARTDATE(1:4),STARTDATE(5:6),STARTDATE(7:8),
     .                STARTTIME(1:2),STARTTIME(3:4),STARTTIME(5:6)
      WRITE(IOUT,6300)DATE(1:4),DATE(5:6),DATE(7:8),
     .                TIME(1:2),TIME(3:4),TIME(5:6)
      WRITE(IOUT,*)' '

      WRITE(ISTDO,6000)SECS
      WRITE(ISTDO,6100)IH,IM,IS
      IF(NPROC>1.AND.SECS>ZERO)WRITE(ISTDO,8000)CUMUL(1)/SECS
      WRITE(ISTDO,*)' '
      WRITE(IOUT,6000)SECS
      WRITE(IOUT,6100)IH,IM,IS
      IF(NPROC>1.AND.SECS>ZERO)WRITE(IOUT,8000)CUMUL(1)/SECS

 1000 FORMAT(' ** PROCESSOR NUMBER **',16(I8,5x,"%",2x))
 2000 FORMAT(' #PROC  ','CONT.SORT  ','CONT. F   ','ELEMENT  ',
     .       'KIN.COND.  ','INTEGR.','    I/O     ','TASK0     ',
     .       'ASSEMB.   ','RESOL  ')
 2001 FORMAT(' #PROC  ','CONT.SORT  ','CONT. F   ','ELEMENT  ',
     .       'KIN.COND.  ','INTEGR.','    I/O     ','TASK0     ',
     .       'ASSEMB.   ','AMS      ','RESOL  ')
 2050 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     .       1x,E9.4,1x,E9.4,1x,E9.4)
 2051 FORMAT(I4,3x,F6.2,A,1x,F6.2,A,1x,F6.2,A,1x,F6.2,A,1x,F6.2,A,
     .       1x,F6.2,A,1x,F6.2,A,1x,F6.2,A,1x,F6.2,A)
 2055 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     .       1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 2057 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     .       1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 2100 FORMAT(' #PROC  ','CONT.SORT  ','CONT. F   ','ELEMENT  ',
     .       'KIN.COND.  ','INTEGR.','    I/O     ','TASK0     ',
     .       'ASSEMB.   ','RESOL  ')
 2101 FORMAT(' #PROC  ','CONT.SORT  ','CONT. F   ','ELEMENT  ',
     .       'KIN.COND.  ','INTEGR.','    I/O     ','TASK0     ',
     .       'ASSEMB.   ' ,'AMS      ','RESOL  ')
 2102 FORMAT('        ','CONT.SORT  ','CONT. F   ','ELEMENT  ',
     .       'KIN.COND.  ','INTEGR.','    I/O     ','TASK0     ',
     .       'ASSEMB.   ','RESOL  ')
 2103 FORMAT('        ','CONT.SORT  ','CONT. F   ','ELEMENT  ',
     .       'KIN.COND.  ','INTEGR.','    I/O     ','TASK0     ',
     .       'ASSEMB.   ' ,'AMS      ','RESOL  ')
 2110 FORMAT(' #PROC  ','[K]SETUP ','[M]SETUP ','IMPL.SOLV ',
     .       'IMPL.TOTAL')
 2120 FORMAT(' #PROC  ',' COMM DOT',' COMM V ','COMM GPU>CPU ',
     .       'COMM CPU>GPU ')
 2121 FORMAT('        ',' COMM DOT',' COMM V ','COMM GPU>CPU ',
     .       'COMM CPU>GPU ')
 2125 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 2126 FORMAT(1x,A4,2x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 2150 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,
     .       E9.4,1x,E9.4,1x,E9.4)
 2151 FORMAT(1x,A4,2x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,
     .       E9.4,1x,E9.4,1x,E9.4)
 2155 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     .       1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 2156 FORMAT(1x,A4,2x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     .       1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 2157 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     .       1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 2158 FORMAT(1x,A4,2x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     .       1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 2160 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 2200 FORMAT(' #PROC  ','CRITER  ','ADAPT FOR ','ADAPT VEL ')
 2201 FORMAT('        ','CRITER  ','ADAPT FOR ','ADAPT VEL ')
 2250 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4)
 2251 FORMAT(1x,A4,2x,E9.4,1x,E9.4,1x,E9.4)
 3000 FORMAT(' #PROC  ','FORCES    ','RBY.FOR.  ','RBY.VEL.  ',
     .       'VELOCITIES','   TOTAL   ','% CPU')
 3001 FORMAT('        ','FORCES    ','RBY.FOR.  ','RBY.VEL.  ',
     .       'VELOCITIES','   TOTAL   ')
 3050 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,2x,F6.2)
 3051 FORMAT(1x,A4,2x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 3330 FORMAT(' #PROC  ',' PCG     ',' PARIT F ','  COMM R  ',
     .       '  COMP M.V ',' COMM VFI ',' COMM M.V ',' TOT PROJ ',
     .       ' MAV_LT2  ',' MAM_NM    ',' MAV_N,MN ',' COMP MV/E',
     .       ' BUILD M  ')
 3331 FORMAT('        ',' PCG     ',' PARIT F ','  COMM R  ',
     .       '  COMP M.V ',' COMM VFI ',' COMM M.V ',' TOT PROJ ',' MAV_LT2  ',
     .       ' MAM_NM    ',' MAV_N,MN ',' COMP MV/E',' BUILD M  ')
 3335 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     .       1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 3336 FORMAT(1x,A4,2x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     .       1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 4000 FORMAT(' #PROC  ','BUCKCRIT ',' COMMCRIT','  BUCKET  ',
     .       '  GFRONT   ',' CONT.OPT ',' CONT.F** ',
     .       ' CONT.DST ',' C+E FORCE',' CONT.NORML')
 4001 FORMAT('        ','BUCKCRIT ',' COMMCRIT','  BUCKET  ',
     .       '  GFRONT   ',' CONT.OPT ',' CONT.F** ',
     .       ' CONT.DST ',' C+E FORCE')
 4050 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,
     .       E9.4,1x,E9.4,1x,E9.4)
 4051 FORMAT(1x,A4,2x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,
     .       E9.4,1x,E9.4)
 4500 FORMAT(' #PROC  ','RB+SENS ','  CLOADS  ','  BCS    ',
     .       '   RL+RW   ','  FIXVEL  ','   RBE3   ','   GRAV')
 4501 FORMAT('        ','RB+SENS ','  CLOADS  ','  BCS    ',
     .       '   RL+RW   ','  FIXVEL  ','   RBE3   ','   GRAV')
 4550 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,2x,E9.4,1x,
     .       E9.4,1x,E9.4,1x,E9.4)
 4551 FORMAT(1x,A4,2x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,2x,E9.4,1x,
     .       E9.4,1x,E9.4,1x,E9.4)
 5000 FORMAT(' #PROC  ','FICOM1   ',' FICOM2  ','  ISDXV1   ',
     .                  ' ISDXV2  ',' TRI MMX**','  TRI BOX  ',
     .                  ' TRI BUC**',' TRI GAT**',' SYNC COM ',
     .                  ' SHOOTING ',' INT2 F+V')
 5001 FORMAT('        ','FICOM1   ',' FICOM2  ','  ISDXV1   ',
     .                  ' ISDXV2  ',' TRI MMX**','  TRI BOX  ',
     .                  ' TRI BUC**',' TRI GAT**',' SYNC COM ',
     .                  ' SHOOTING ',' INT2 F+V')
 5050 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     +       1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 5051 FORMAT(1x,A4,2x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     +       1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 5500 FORMAT(' #PROC  ','MANCTR  ','  MONVOL  ','  DAMP   ',
     .                  '   INT2*   ','  FIXVEL* ','  CLOADS* ',
     .                  '  DT2   ','    AUX   ','    FORINTS ',
     .                  '    R2R   ','    R2M')
 5501 FORMAT('        ','MANCTR  ','  MONVOL  ','  DAMP   ',
     .                  '   INT2*   ','  FIXVEL* ','  CLOADS* ',
     .                  '  DT2   ','    AUX   ','    FORINTS ',
     .                  '    R2R   ','    R2M')
 5550 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     +       1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     +       1x,E9.4,1x,E9.4)
 5551 FORMAT(1x,A4,2x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     +       1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,
     +       1x,E9.4,1x,E9.4)
 5600 FORMAT(' #PROC ',' SPH/TOTAL',' SPH/SORT ','SPH/FORCES',
     .       ' SPH/OTHER',' SPH/SORT1',' COMM.SORT1',' SPH/SORT0',
     .       ' COMM.SORT0',' COMM.FORCES')
 5601 FORMAT('       ',' SPH/TOTAL',' SPH/SORT ','SPH/FORCES',
     .       ' SPH/OTHER',' SPH/SORT1',' COMM.SORT1',' SPH/SORT0',
     .       ' COMM.SORT0',' COMM.FORCES')
 6000 FORMAT(' ELAPSED TIME     =',F14.2,' s')
 6100 FORMAT('                   ',I8,':',I2.2,':',I2.2)
 6200 FORMAT(' EXECUTION STARTED .........................:      ',
     .        A4,'/',A2,'/',A2,'  ',A2,':',A2,':',A2)
 6300 FORMAT(' EXECUTION COMPLETED .......................:      ',
     .        A4,'/',A2,'/',A2,'  ',A2,':',A2,':',A2)
 6500 FORMAT(' #PROC  ','BEG CRIT  ','EOF CRIT  ','EOF SORT ',
     .                  ' EOF FORCE',' EOF CYCLE','   EOF SPH')
 6501 FORMAT('        ','BEG CRIT  ','EOF CRIT  ','EOF SORT ',
     .                  ' EOF FORCE',' EOF CYCLE','   EOF SPH')
 6550 FORMAT(I4,3x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 6551 FORMAT(1x,A4,2x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4,1x,E9.4)
 8000 FORMAT(' ESTIMATED SPEEDUP=',F14.2)
C
      RETURN
      END
Chd|====================================================================
Chd|  PRINT_RUNTIME                 source/system/timer.F         
Chd|-- called by -----------
Chd|        ARRET                         source/system/arret.F         
Chd|-- calls ---------------
Chd|        TIME_MOD                      share/modules/time_mod.F      
Chd|====================================================================
        SUBROUTINE PRINT_RUNTIME( )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIME_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include	"com01_c.inc"
#include        "scr05_c.inc"
#include        "scr07_c.inc"
#include	"task_c.inc"
#include        "units_c.inc"
#include        "warn_c.inc"
#include        "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------        
        INTEGER :: I,IJK,IJKL,CURRENT_RUN
        REAL(kind=8) :: TOTAL_TIME,RESULT
        INTEGER, DIMENSION(GLOBAL_COMP_TIME%RUN_NBR+2) :: IH_TAB,IM_TAB,IS_TAB
!       ----------------------------------------

        TOTAL_TIME = GLOBAL_COMP_TIME%STARTER_TIME
        DO I=1,GLOBAL_COMP_TIME%RUN_NBR
                TOTAL_TIME = TOTAL_TIME + GLOBAL_COMP_TIME%ENGINE_TIME(I)
                IH_TAB(I)=GLOBAL_COMP_TIME%ENGINE_TIME(I)/3600
                IM_TAB(I)=(GLOBAL_COMP_TIME%ENGINE_TIME(I)-IH_TAB(I)*3600)/60
                IS_TAB(I)=GLOBAL_COMP_TIME%ENGINE_TIME(I)-IH_TAB(I)*3600-IM_TAB(I)*60
        ENDDO
        IJK = GLOBAL_COMP_TIME%RUN_NBR + 1
        IH_TAB(IJK)=GLOBAL_COMP_TIME%STARTER_TIME/3600
        IM_TAB(IJK)=(GLOBAL_COMP_TIME%STARTER_TIME-IH_TAB(IJK)*3600)/60
        IS_TAB(IJK)=GLOBAL_COMP_TIME%STARTER_TIME-IH_TAB(IJK)*3600-IM_TAB(IJK)*60

        IJKL = GLOBAL_COMP_TIME%RUN_NBR + 2
        IH_TAB(IJKL)=TOTAL_TIME/3600
        IM_TAB(IJKL)=(TOTAL_TIME-IH_TAB(IJKL)*3600)/60
        IS_TAB(IJKL)=TOTAL_TIME-IH_TAB(IJKL)*3600-IM_TAB(IJKL)*60
      
        WRITE(IOUT,*) ' '
        WRITE(IOUT,*)
     .  '                  ** COMPUTE RUNTIME INFORMATION SUMMARY **'
        WRITE(IOUT,*) ' '
        WRITE(IOUT,100) GLOBAL_COMP_TIME%STARTER_TIME,IH_TAB(IJK),IM_TAB(IJK),IS_TAB(IJK)
        DO I=1,GLOBAL_COMP_TIME%RUN_NBR
                WRITE(IOUT,101) I,GLOBAL_COMP_TIME%ENGINE_TIME(I),IH_TAB(I),IM_TAB(I),IS_TAB(I)
        ENDDO
        WRITE(IOUT,103)
        WRITE(IOUT,102)TOTAL_TIME,IH_TAB(IJKL),IM_TAB(IJKL),IS_TAB(IJKL)      
        WRITE(IOUT,*)' '
C

C
 100  FORMAT('        STARTER RUNTIME =',  (F14.2),'s',' (',I2.2,':',I2.2,':',I2.2,')')
 101  FORMAT(' #',(I4.3), '   ENGINE RUNTIME =',(F14.2),'s',' (',I2.2,':',I2.2,':',I2.2,')')
 102  FORMAT(' STARTER+ENGINE RUNTIME =',(F14.2),'s',' (',I2.2,':',I2.2,':',I2.2,')')
 103  FORMAT(' ------------------------------ ')

        RETURN
        END SUBROUTINE 

Chd|====================================================================
Chd|  PRINTCPU                      source/system/timer.F         
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        CPUINFO                       source/system/mon_c.c         
Chd|        SPMD_PRINTCPUINFO             source/mpi/output/spmd_printcpuinfo.F
Chd|====================================================================
      SUBROUTINE PRINTCPU(IFLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include	"com01_c.inc"
#include        "scr05_c.inc"
#include        "scr07_c.inc"
#include	"task_c.inc"
#include        "units_c.inc"
#include        "warn_c.inc"
#include        "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLAG

      CHARACTER*256 HOSTNAME, CPUNAM
      INTEGER FREQUENCE,LENH,LENC,I,MEM,SWAP
      SAVE HOSTNAME,CPUNAM,FREQUENCE,LENH,LENC,MEM,SWAP

      IF(IFLAG.EQ.1) THEN

        CALL CPUINFO(HOSTNAME,LENH,CPUNAM, LENC, FREQUENCE,MEM,SWAP)

      ELSEIF(IFLAG.EQ.2) THEN
C 2eme appel apres init parallele (echange et ecriture)
        IF(ISPMD.EQ.0) THEN
          WRITE(IOUT,*)' ' 
          WRITE(IOUT,7000) 
          WRITE(IOUT,*)' ' 
          IF(NSPMD.EQ.1) THEN
            IF(MEM.GT.-1)THEN
              WRITE(IOUT,7400) 
              WRITE(IOUT,'(A,A,A,A,A,I4,A,I6,A,I6,A)') ' ',
     .        HOSTNAME(1:LENH),'   ',CPUNAM(1:LENC),', ',FREQUENCE,
     .        ' MHz, ',MEM,' MB RAM, ',SWAP,' MB swap'
            ELSE
              WRITE(IOUT,7450) 
              WRITE(IOUT,'(A,A,A,A,A,I4,A)') ' ',
     .        HOSTNAME(1:LENH),'   ',CPUNAM(1:LENC),', ',FREQUENCE,
     .        ' MHz '
            END IF
          ELSE
            I = 1
            IF(MEM.GT.-1)THEN
              WRITE(IOUT,7500) 
              WRITE(IOUT,'(I4,A,A,A,A,A,I4,A,I6,A,I6,A)') I,'   ',
     .        HOSTNAME(1:LENH),'   ',CPUNAM(1:LENC),', ',FREQUENCE,
     .        ' MHz, ',MEM,' MB RAM, ',SWAP,' MB swap'
            ELSE
              WRITE(IOUT,7550) 
              WRITE(IOUT,'(I4,A,A,A,A,A,I4,A)') I,'   ',
     .        HOSTNAME(1:LENH),'   ',CPUNAM(1:LENC),', ',FREQUENCE,
     .        ' MHz '
            END IF
          END IF
        END IF
C ecriture du complement en multiprocs
        IF(NSPMD.GT.1)
     .    CALL SPMD_PRINTCPUINFO(HOSTNAME,LENH,CPUNAM, LENC, FREQUENCE,
     .                           MEM,SWAP)
        IF(ISPMD.EQ.0) WRITE(IOUT,*)' ' 

      ENDIF
     
      LMEMV = MEM

 7000 FORMAT (' COMPUTATION HARDWARE DESCRIPTION')
 7400 FORMAT (' HOSTNAME     CPU TYPE, FREQUENCY AND MEMORY')
 7450 FORMAT (' HOSTNAME     CPU TYPE AND FREQUENCY')
 7500 FORMAT (' CPU#  HOSTNAME     CPU TYPE, FREQUENCY AND MEMORY')
 7550 FORMAT (' CPU#  HOSTNAME     CPU TYPE AND FREQUENCY ')

      END
Chd|====================================================================
Chd|  INITIMEG                      source/system/timer.F         
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INITIMEG(NG)
C     initialisation des timers groupes elem.
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
      COMMON /TIMERG/TIMERG(2,500000),CPUTIMEG(500000)
      REAL TIMERG,CPUTIMEG
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J
C-----------------------------------------------
      DO J=1,NG
        CPUTIMEG(J) = ZERO
      ENDDO 
C
      RETURN
      END
Chd|====================================================================
Chd|  STARTIMEG                     source/system/timer.F         
Chd|-- called by -----------
Chd|        ERR_THK                       source/elements/shell/err_thk.F
Chd|        FORINT                        source/elements/forint.F      
Chd|        FORINTC                       source/elements/forintc.F     
Chd|        FORINTP                       source/elements/forintp.F     
Chd|        FORINTS                       source/elements/forints.F     
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|        INIXFEM                       source/elements/xfem/inixfem.F
Chd|        PRELECFLOW                    source/elements/solid/solide/prelecflow.F
Chd|        SMS_INI_KDI                   source/ams/sms_init.F         
Chd|        SOLTOSPHA                     source/elements/sph/soltospha.F
Chd|        SOLTOSPHF                     source/elements/sph/soltosph.F
Chd|        SOLTOSPHP                     source/elements/sph/soltosph.F
Chd|        SOLTOSPH_ON1                  source/elements/sph/soltosph_on1.F
Chd|        SOLTOSPH_ON12                 source/elements/sph/soltosph_on1.F
Chd|        SOLTOSPH_ON2                  source/elements/sph/soltosph_on2.F
Chd|        SPLISSV                       source/elements/sph/splissv.F 
Chd|        SPSTABS                       source/elements/sph/spstab.F  
Chd|        THICKVAR                      source/elements/shell/coque/thickvar.F
Chd|        UPXFEM1                       source/elements/xfem/upxfem1.F
Chd|        UPXFEM2                       source/elements/xfem/upxfem2.F
Chd|        UPXFEM_TAGXP                  source/elements/xfem/upxfem_tagxp.F
Chd|        XFEOFF                        source/elements/xfem/xfeoff.F 
Chd|-- calls ---------------
Chd|        MY_ETIME                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE STARTIMEG(NG)
C     routine d'incrementation des timers elem.
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
      COMMON /TIMERG/TIMERG(2,500000),CPUTIMEG(500000)
      REAL TIMERG,CPUTIMEG
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NG
C
      CALL MY_ETIME(TIMERG(1,NG))
     
      RETURN
      END

Chd|====================================================================
Chd|  STOPTIMEG                     source/system/timer.F         
Chd|-- called by -----------
Chd|        ERR_THK                       source/elements/shell/err_thk.F
Chd|        FORINT                        source/elements/forint.F      
Chd|        FORINTC                       source/elements/forintc.F     
Chd|        FORINTP                       source/elements/forintp.F     
Chd|        FORINTS                       source/elements/forints.F     
Chd|        INIXFEM                       source/elements/xfem/inixfem.F
Chd|        PRELECFLOW                    source/elements/solid/solide/prelecflow.F
Chd|        SMS_INI_KDI                   source/ams/sms_init.F         
Chd|        SOLTOSPHA                     source/elements/sph/soltospha.F
Chd|        SOLTOSPHF                     source/elements/sph/soltosph.F
Chd|        SOLTOSPHP                     source/elements/sph/soltosph.F
Chd|        SOLTOSPH_ON1                  source/elements/sph/soltosph_on1.F
Chd|        SOLTOSPH_ON12                 source/elements/sph/soltosph_on1.F
Chd|        SOLTOSPH_ON2                  source/elements/sph/soltosph_on2.F
Chd|        SPLISSV                       source/elements/sph/splissv.F 
Chd|        SPSTABS                       source/elements/sph/spstab.F  
Chd|        THICKVAR                      source/elements/shell/coque/thickvar.F
Chd|        UPXFEM1                       source/elements/xfem/upxfem1.F
Chd|        UPXFEM2                       source/elements/xfem/upxfem2.F
Chd|        UPXFEM_TAGXP                  source/elements/xfem/upxfem_tagxp.F
Chd|        XFEOFF                        source/elements/xfem/xfeoff.F 
Chd|-- calls ---------------
Chd|        MY_ETIME                      source/system/machine.F       
Chd|====================================================================
      SUBROUTINE STOPTIMEG(NG)
C     routine incrementation du timer event
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
      COMMON /TIMERG/TIMERG(2,500000),CPUTIMEG(500000)
      REAL TIMERG,CPUTIMEG
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NG
C
      CALL MY_ETIME(TIMERG(2,NG))
      CPUTIMEG(NG) = CPUTIMEG(NG) +
     .                       TIMERG(2,NG)-TIMERG(1,NG)
      RETURN
      END
Chd|====================================================================
Chd|  PRINTIMEG                     source/system/timer.F         
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTERLAGRAN                   source/system/timer.F         
Chd|        PRINTCOQUES                   source/system/timer.F         
Chd|        PRINTSOLIDES                  source/system/timer.F         
Chd|        PRINTTET10                    source/system/timer.F         
Chd|        PRINTTET4                     source/system/timer.F         
Chd|        PRINTTRIANGLES                source/system/timer.F         
Chd|====================================================================
      SUBROUTINE PRINTIMEG(IPARG,PM,IPM,IXC,IXTG,IXS)
C     routine d'affichage des timers
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr07_c.inc"
      COMMON /TIMERG/TIMERG(2,500000),CPUTIMEG(500000)
      REAL TIMERG,CPUTIMEG
C-----------------------------------------------
#include	"com01_c.inc"
#include	"com04_c.inc"
#include        "scr05_c.inc"
#include	"task_c.inc"
#include        "units_c.inc"
#include        "param_c.inc"
#include        "spmd_c.inc"
#include        "rnur_c.inc"
#include        "chara_c.inc"
#include        "scr19_c.inc"
#include        "ddspmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .     IPM(NPROPMI,*),IXS(NIXS,*)
      my_real
     .     PM(NPROPM,*),CC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NPROC, MSGTAG, MSGTAG2, ISTART, NGROUP_R,
     .        NEL,PROC,NGF, NGL,
     .        J, NELT,
     .        NG,IRSIZE, MSGTAG3 ,NES,NEQ,NEC,NETG,NET,NEP,NER,NEUR,
     .        MPT,NPTS,NPTT,NPTOT,ISOLNOD

       CHARACTER FILNAM*100,PROCNAM*4
       my_real
     .     TS, TQ, TC, TTG, TT, TP, TR, TUR, DTU,MINS,MAXS,MINQ,
     .     MAXQ,MINC,MAXC,MINTG,MAXTG,MINP,MAXP,MINT,MAXT,MINR,
     .     MAXR,MINUR,MAXUR, CPUG, CPUELEM, T(8,2)
C
      INTEGER NUMSOLG,NUMSHG,NUMTRIG,NUMOTHER,ITY,MTN,NELEM,NPT,NSG,
     .        NFT,IHBE,MMA,NFUNC,FILEN,NUMSOL4G,NUMSOL10G

      real
     .   THEOGP(NGROUP),TABX(3),TABMAT(3),NPTR,TIMMAT,TELT,TPSTHTOT,
     .   TPSREEL,BATOZMULT,TPPOND

      TPSREEL = 0.
      TPSTHTOT = 0.
      BATOZMULT = 4
      TIMMAT = 0.
Cow ouverture du fichier DDW
      WRITE(PROCNAM,'(I4.4)')ISPMD+1
      FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//PROCNAM//'.ddw2'
      OPEN(UNIT=66,FILE=FILNAM,ACCESS='SEQUENTIAL',
     .                  FORM='FORMATTED',STATUS='UNKNOWN')
      FILEN=ROOTLEN+14 
Cow
Cow preparation de la ponderation
Cow
      IF (TPSENG.EQ.ZERO) THEN
         TPPOND = ONE
      ELSE
         TPPOND = TPSENG/TPSREF
      ENDIF

      WRITE(66,'(A)') 'DOMAIN DECOMPOSITION ANALYSIS FILE '
      WRITE(66,'(A)') '---------------------------------- '
      WRITE(66,'(A)') ' '
      WRITE(66,'(A)') ' '
      
      WRITE(66,1000) ISPMD,NCYCLE
c      WRITE(66,1100) NCYCLE


Cow Comptage des groupes
      NUMSOLG = 0
      NUMSHG = 0
      NUMTRIG = 0
      NUMSOL4G = 0
      NUMSOL10G = 0

      DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        IF (ITY.EQ.1) THEN
          ISOLNOD = IPARG(28,NG) 
          IF (ISOLNOD.EQ.4) THEN
            NUMSOL4G = NUMSOL4G+1
          ELSEIF (ISOLNOD.EQ.10) THEN
            NUMSOL10G = NUMSOL10G+1
          ELSE
            NUMSOLG = NUMSOLG + 1
          ENDIF
        ELSEIF (ITY.EQ.3) THEN
           NUMSHG = NUMSHG + 1
        ELSEIF (ITY.EQ.7) THEN
           NUMTRIG = NUMTRIG + 1
        ENDIF
      ENDDO

Cow Calcul du temps theorique de chaque groupe
      DO NG=1,NGROUP
         ITY = IPARG(5,NG)
         MTN = IPARG(1,NG)
         NELEM = IPARG(2,NG)
         NPT = IPARG(6,NG)
         NSG = IPARG(10,NG)
         NFT = IPARG(3,NG)+1
         IHBE = IPARG(23,NG)

         THEOGP(NG)=0.
Cow SOLIDES
         IF (ITY.EQ.1) THEN
          ISOLNOD=IPARG(28,NG)
          IF (ISOLNOD.EQ.4) THEN
           TELT = TET4TNL(MTN,1) + TET4TELT(1)
Cow TETRA10
          ELSEIF (ISOLNOD.EQ.10) THEN
	   MMA = IXS(1,NFT)
	   NFUNC = MAX(IPM(10,MMA) - 3,1)
	   IF (NFUNC.LE.2) THEN
              TELT = TET10TNL(MTN,1) + TET10TELT(1)
	   ELSEIF (NFUNC.GT.2.AND.NFUNC.LE.7) THEN
	      TELT = TET10TNL(MTN,2) + TET10TELT(1)
	   ELSE
	      TELT = TET10TNL(MTN,3) + TET10TELT(1)
	   ENDIF
          ELSE
           IF (IHBE.LE.1.OR.IHBE.EQ.101) THEN
Cow Solides ISOLD1
            TELT = SOL1TNL(MTN,1) + SOLTELT(1)
           ELSEIF (IHBE.EQ.2.OR.IHBE.EQ.102) THEN
Cow Solides ISOLD2
            TELT = SOL1TNL(MTN,1) + SOLTELT(2)
C
           ELSEIF (IHBE.EQ.24.OR.IHBE.EQ.104) THEN
Cow Solides ISOLD24  - HEPH
            TELT = SOL1TNL(MTN,1) + SOLTELT(3)
C
           ELSEIF (IHBE.EQ.12.OR.IHBE.EQ.112) THEN
Cow Solides ISOLD12 - std 8 node integ point
            TELT = SOL8TNL(MTN,1) + SOLTELT(4)
C
           ELSEIF (IHBE.EQ.14.OR.IHBE.GE.222) THEN
Cow Solides HA8
            MPT = ABS (NPT)
            NPTS = MPT/100
            NPTT = MOD(MPT/10,10)
            NPTR = MOD(MPT,10)
            NPTOT = NPTS*NPTT*NPTR
C
            TELT = NPTOT*SOL8TNL(MTN,1)+SOLTELT(5)+NPTOT*SOLTELT(6)
	   
	   ELSEIF(IHBE.EQ.17)THEN
	     TELT = SOL1TNL(MTN,1)*8+SOLTELT(7)
	   ELSE
	     TELT = SOL1TNL(MTN,1) + SOLTELT(1)
           ENDIF
          ENDIF
C
           THEOGP(NG)= TELT*NELEM*NCYCLE*TPPOND
Cow COQUES
         ELSEIF (ITY.EQ.3) THEN

          DO J=NFT,NFT+NELEM-1
           MMA = IXC(1,J)
           IF (NPT.GT.0) THEN
Cow traitement a part pour zero points d integration
             IF (MTN.EQ.2.OR.MTN.EQ.3) THEN
               CC = PM(43,MMA)
               IF (CC.NE.0.0 )THEN
                 TABMAT(1) = SHTNL(MTN,1,2)
                 TABMAT(2) = SHTNL(MTN,2,2)
                 TABMAT(3) = SHTNL(MTN,3,2)
               ELSE
                 TABMAT(1) = SHTNL(MTN,1,1)
                 TABMAT(2) = SHTNL(MTN,2,1)
                 TABMAT(3) = SHTNL(MTN,3,1)
               ENDIF

             ELSEIF (MTN.EQ.36) THEN
                NFUNC = IPM(10,MMA)

               IF (NFUNC.LE.2) THEN
                 TABMAT(1) = SHTNL(MTN,1,1)
                 TABMAT(2) = SHTNL(MTN,2,1)
                 TABMAT(3) = SHTNL(MTN,3,1)
               ELSEIF (NFUNC.GT.2.AND.NFUNC.LE.7) THEN
                 TABMAT(1) = SHTNL(MTN,1,2)
                 TABMAT(2) = SHTNL(MTN,2,2)
                 TABMAT(3) = SHTNL(MTN,3,2)
               ELSEIF (NFUNC.GT.7) THEN
                 TABMAT(1) = SHTNL(MTN,1,3)
                 TABMAT(2) = SHTNL(MTN,2,3)
                 TABMAT(3) = SHTNL(MTN,3,3)
               ENDIF
             ELSE
               TABMAT(1) = SHTNL(MTN,1,1)
               TABMAT(2) = SHTNL(MTN,2,1)
               TABMAT(3) = SHTNL(MTN,3,1)
             ENDIF

             TABX(1) = 1.
             TABX(2) = 3.
             TABX(3) = 5.

             NPTR = NPT

             CALL INTERLAGRAN(TABMAT,TABX,3,NPTR,TIMMAT)

            ELSE
Cow Cas Zero points d integration
             IF (MTN.EQ.2.OR.MTN.EQ.3) THEN
               CC = PM(43,MMA)
               IF (CC.NE.0.0 )THEN
                 TIMMAT = SHTNL(MTN,0,2)
               ELSE
                 TIMMAT = SHTNL(MTN,0,1)
               ENDIF

             ELSEIF (MTN.EQ.36) THEN
               NFUNC = NINT(PM(40,MMA))
               IF (NFUNC.LE.2) THEN
                 TIMMAT = SHTNL(MTN,0,1)
               ELSEIF (NFUNC.GT.2.AND.NFUNC.LE.7) THEN
                 TIMMAT = SHTNL(MTN,0,2)
               ELSEIF (NFUNC.GT.7) THEN
                 TIMMAT = SHTNL(MTN,0,3)
               ENDIF
             ELSE
                 TIMMAT = SHTNL(MTN,0,1)
             ENDIF
               
Cow fin cas zero point d integrations
            ENDIF 

Cow temps elementaire
            IF (IHBE.GE.11.AND.IHBE.LE.19) THEN
              TELT = SHTELT(3)+BATOZMULT*TIMMAT
            ELSEIF (IHBE.GE.21.AND.IHBE.LE.29) THEN
              TELT = SHTELT(2) + TIMMAT
            ELSE
              TELT = SHTELT(1)  + TIMMAT
            ENDIF
Cow fin temps elementaire
              THEOGP(NG)= THEOGP(NG)+TELT*NCYCLE*TPPOND
          ENDDO
Cow TRIANGLES
         ELSEIF (ITY.EQ.7) THEN

          DO J=NFT,NFT+NELEM-1
           MMA = IXTG(1,J)
           IF (NPT.GT.0) THEN
Cow traitement a part pour zero points d integration
             IF (MTN.EQ.2.OR.MTN.EQ.3) THEN
               CC = PM(43,MMA)
               IF (CC.NE.0.0 )THEN
                 TABMAT(1) = TRITNL(MTN,1,2)
                 TABMAT(2) = TRITNL(MTN,2,2)
                 TABMAT(3) = TRITNL(MTN,3,2)
               ELSE
                 TABMAT(1) = TRITNL(MTN,1,1)
                 TABMAT(2) = TRITNL(MTN,2,1)
                 TABMAT(3) = TRITNL(MTN,3,1)
               ENDIF

             ELSEIF (MTN.EQ.36) THEN
               NFUNC = IPM(10,MMA)
               IF (NFUNC.LE.2) THEN
                 TABMAT(1) = TRITNL(MTN,1,1)
                 TABMAT(2) = TRITNL(MTN,2,1)
                 TABMAT(3) = TRITNL(MTN,3,1)
               ELSEIF (NFUNC.GT.2.AND.NFUNC.LE.7) THEN
                 TABMAT(1) = TRITNL(MTN,1,2)
                 TABMAT(2) = TRITNL(MTN,2,2)
                 TABMAT(3) = TRITNL(MTN,3,2)
               ELSEIF (NFUNC.GT.7) THEN
                 TABMAT(1) = TRITNL(MTN,1,3)
                 TABMAT(2) = TRITNL(MTN,2,3)
                 TABMAT(3) = TRITNL(MTN,3,3)
               ENDIF
             ELSE
               TABMAT(1) = TRITNL(MTN,1,1)
               TABMAT(2) = TRITNL(MTN,2,1)
               TABMAT(3) = TRITNL(MTN,3,1)
             ENDIF

             TABX(1) = 1.
             TABX(2) = 3.
             TABX(3) = 5.

             NPTR = NPT

             CALL INTERLAGRAN(TABMAT,TABX,3,NPTR,TIMMAT)

            ELSE
Cow Cas Zero points d integration
             IF (MTN.EQ.2.OR.MTN.EQ.3) THEN
               CC = PM(43,MMA)
               IF (CC.NE.0.0 )THEN
                 TIMMAT = TRITNL(MTN,0,2)
               ELSE
                 TIMMAT = TRITNL(MTN,0,1)
               ENDIF

             ELSEIF (MTN.EQ.36) THEN
               NFUNC = NINT(PM(40,MMA))
               IF (NFUNC.LE.2) THEN
                 TIMMAT = TRITNL(MTN,0,1)
               ELSEIF (NFUNC.GT.2.AND.NFUNC.LE.7) THEN
                 TIMMAT = TRITNL(MTN,0,2)
               ELSEIF (NFUNC.GT.7) THEN
                 TIMMAT = TRITNL(MTN,0,3)
               ENDIF
             ELSE
                 TIMMAT = TRITNL(MTN,0,1)
             ENDIF
               
Cow fin cas zero point d integrations
            ENDIF 

Cow temps elementaire
            IF (IHBE.GE.11.AND.IHBE.LE.19) THEN
              TELT = TRITELT(3)+BATOZMULT*TIMMAT
            ELSEIF (IHBE.GE.21.AND.IHBE.LE.29) THEN
              TELT = TRITELT(2) + TIMMAT
            ELSE
              TELT = TRITELT(1)  + TIMMAT
            ENDIF
Cow fin temps elementaire
              THEOGP(NG)= THEOGP(NG)+TELT*NCYCLE*TPPOND
           ENDDO

         ENDIF
              TPSTHTOT = TPSTHTOT + THEOGP(NG)
              TPSREEL = TPSREEL + CPUTIMEG(NG)
      ENDDO


Cow Maintenant qu on a tous les temps theoriques des groupes on
Cow peux passer au traitement et a l affichage.

       NUMOTHER = NGROUP - NUMSHG - NUMTRIG - NUMSOLG
       WRITE(66,'(A)') ' ' 
       WRITE(66,1100) NUMSOLG,NUMSHG,NUMTRIG,NUMOTHER
       WRITE(66,'(A)') ' ' 
       WRITE(66,1200) TPSREEL,TPSTHTOT
      IF (NUMSOLG.GT.0)
     .  CALL PRINTSOLIDES(IPARG,THEOGP,NUMSOLG,IPM,IXS)

      IF (NUMSOL4G.GT.0)
     .  CALL PRINTTET4(IPARG,THEOGP,NUMSOL4G,IPM,IXS)

      IF (NUMSOL10G.GT.0)
     .  CALL PRINTTET10(IPARG,THEOGP,NUMSOL10G)
      IF (NUMSHG.GT.0)
     .  CALL PRINTCOQUES(IPARG,IXC,IPM,THEOGP,NUMSHG)

      IF (NUMTRIG.GT.0)
     .  CALL PRINTTRIANGLES(IPARG,THEOGP,NUMTRIG)

      CLOSE(UNIT=66)

      WRITE (IOUT,1300)  FILNAM(1:FILEN)
      WRITE (ISTDO,1300) FILNAM(1:FILEN)


 1000 FORMAT(
     & 4X,'SPMD PROCESSOR NUMBER . . . . . . . . . . . . . .',I10/
     & 4X,'TOTAL NUMBER OF CYCLES. . . . . . . . . . . . . .',I10)

 1100 FORMAT(
     & 4X,'NUMBER OF SOLID ELEMENTS GROUPS . . . . . . . . .',I10/
     & 4X,'NUMBER OF SHELL ELEMENTS GROUPS . . . . . . . . .',I10/
     & 4X,'NUMBER OF TRIANGULAR ELEMENTS GROUPS. . . . . . .',I10/
     & 4X,'NUMBER OF OTHER ELEMENT GROUPS. . . . . . . . . .',I10)

 1200 FORMAT(
     & 4X,'SOLID, SHELL, TRIANGULAR GROUP TIME'/,
     & 4X,'-----------------------------------'/,
     & 4X,'TOTAL GROUP TIME . . . . . . . . . . . . .  . . .',1PG20.13/
     & 4X,'STARTER ESTIMATED GROUP TIME . . . . . . .  . . .',1PG20.13)

 1300   FORMAT (4X,' DD ANALYSIS FILE:',1X,A,' WRITTEN')
C
      RETURN
      END

Chd|====================================================================
Chd|  PRINTCOQUES                   source/system/timer.F         
Chd|-- called by -----------
Chd|        PRINTIMEG                     source/system/timer.F         
Chd|-- calls ---------------
Chd|        MY_FSORT                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE PRINTCOQUES(IPARG,IXC,IPM,THEOGP,NUMSHG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include    "com01_c.inc"
#include    "com04_c.inc"
#include    "scr05_c.inc"
#include    "task_c.inc"
#include    "units_c.inc"
#include    "param_c.inc"
#include    "spmd_c.inc"
#include    "rnur_c.inc"
#include    "chara_c.inc"
#include    "scr07_c.inc"
#include    "scr19_c.inc"
#include    "ddspmd_c.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
      COMMON /TIMERG/TIMERG(2,500000),CPUTIMEG(500000)
      REAL TIMERG,CPUTIMEG
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMSHG
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),IPM(NPROPMI,*)
C
      REAL THEOGP(NGROUP)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL,NG,NUMGROUP(NUMSHG),INDEXSH(NUMSHG),
     .       ITY,MTN,NELEM,NPT,NSG,MID,MUID,
     .        NFT,IHBE,MMA,NFUNC,I,J
      REAL DIFFTT,DIFFTTSH(NUMSHG),TEMP(NUMSHG)
      REAL THEOTP(NUMSHG),REELTP(NUMSHG)
      REAL TABX(3),TABMAT(3),NPTR,TIMMAT,TELT
      REAL TPSTHTOT,TPSREEL,PCT
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      NGL = 0

      DO NG=1,NGROUP
        ITY   = IPARG(5,NG)
        MTN   = IPARG(1,NG)
        NELEM = IPARG(2,NG)
        NPT   = IPARG(6,NG)
        NSG   = IPARG(10,NG)
        NFT   = IPARG(3,NG)+1
        IHBE  = IPARG(23,NG)


        IF (ITY.EQ.3) THEN

          NGL = NGL + 1

          NUMGROUP(NGL) = NG
          DIFFTTSH(NGL) = CPUTIMEG(NG) - THEOGP(NG)

        ENDIF
      ENDDO

      DO I=1,NGL
       INDEXSH(I) = I
      ENDDO

      CALL MY_FSORT(DIFFTTSH,INDEXSH,NUMSHG,TEMP)

      WRITE(66,2000)
c      WRITE(66,'(A)') ' '
c      WRITE(66,'(A)') 'SHELL ELEMENT GROUPS'
c      WRITE(66,'(A)') ' '
c      WRITE(66,'(A,A,A)')
c     .                '  GRP_NB  ELEM_TYP  NUM_ELEM  MAT_NUMB  ',
c     .                '     NPT       NSG         CPUTIMEG',
c     .                '         TIMETHEO             DIFF      PCT'

      DO I=1,NUMSHG
        NGL   = INDEXSH(I)
        NG    = NUMGROUP(NGL)

        ITY   = IPARG(5,NG)
        MTN   = IPARG(1,NG)
        NELEM = IPARG(2,NG)
        NFT   = IPARG(3,NG)+1
        NSG   = IPARG(10,NG)
        NPT   = IPARG(6,NG)
        IHBE  = IPARG(23,NG)
        
        MID = IXC(1,NFT)
        MUID = IPM(1,MID)

        DIFFTT = CPUTIMEG(NG) -  THEOGP(NG)

        PCT = ((CPUTIMEG(NG) - THEOGP(NG))/CPUTIMEG(NG) )*100

        WRITE(66,'(I10,A,I10,A,I10,A,I10,A,I10,A,I10,A,I10,A,I10,
     .             A,E15.3,A,E15.3,A,E15.3,A,F8.3,A)')
     .     NG,'  ',ITY,'  ',IHBE,'   ',NELEM,'  ',MTN,'  ',MUID,
     .     '  ',NPT,'  ',NSG,'  ',
     .     CPUTIMEG(NG),'  ',THEOGP(NG),'  ',DIFFTT,'  ',PCT,'%'

      ENDDO


 2000 FORMAT(
     &       /' '/' '/10X,' SHELL ELEMENT GROUPS'/
     &         10X,' --------------------'/
     &        ' '/
     &        ' '/
     &        '    GRP_NB    ELEM_TYP        IHBE     ',
     &        'NUM_ELEM    MAT_NUMB ',
     &        '  MAT_USERN         NPT         NSG         CPUTIMEG',
     &        '         TIMETHEO             DIFF        PCT')

      END

Chd|====================================================================
Chd|  PRINTSOLIDES                  source/system/timer.F         
Chd|-- called by -----------
Chd|        PRINTIMEG                     source/system/timer.F         
Chd|-- calls ---------------
Chd|        MY_FSORT                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE PRINTSOLIDES(IPARG,THEOGP,NUMSOLG,IPM,IXS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include    "com01_c.inc"
#include    "com04_c.inc"
#include    "scr05_c.inc"
#include    "task_c.inc"
#include    "units_c.inc"
#include    "param_c.inc"
#include    "spmd_c.inc"
#include    "rnur_c.inc"
#include    "chara_c.inc"
#include    "scr07_c.inc"
#include    "scr19_c.inc"
#include    "ddspmd_c.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
      COMMON /TIMERG/TIMERG(2,500000),CPUTIMEG(500000)
      REAL TIMERG,CPUTIMEG
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMSOLG
      INTEGER IPARG(NPARG,*),IXS(NIXS,*),IPM(NPROPMI,*)
      REAL THEOGP(NGROUP)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL,NG,NUMGROUP(NUMSOLG),INDEXSH(NUMSOLG),
     .       ITY,MTN,NELEM,NPT,NSG,
     .       NFT,IHBE,MMA,NFUNC,I,J,ISOLNOD,
     .       MID,MUID
      REAL DIFFTT,DIFFTTSH(NUMSOLG),TEMP(NUMSOLG)
      REAL THEOTP(NUMSOLG),REELTP(NUMSOLG)
      REAL TABX(3),TABMAT(3),NPTR,TIMMAT,TELT
      REAL TPSTHTOT,TPSREEL,PCT
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      NGL = 0

      DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        MTN = IPARG(1,NG)
        NELEM = IPARG(2,NG)
        NPT = IPARG(6,NG)
        NSG = IPARG(10,NG)
        NFT = IPARG(3,NG)+1
        IHBE    = IPARG(23,NG)

        IF (ITY.EQ.1) THEN
          ISOLNOD = IPARG(28,NG)
          IF (ISOLNOD.NE.4.AND.ISOLNOD.NE.10) THEN
            NGL = NGL + 1
            NUMGROUP(NGL) = NG
            DIFFTTSH(NGL) = CPUTIMEG(NG) - THEOGP(NG)
          ENDIF 
        ENDIF
      ENDDO

      DO I=1,NGL
       INDEXSH(I) = I
      ENDDO

      CALL MY_FSORT(DIFFTTSH,INDEXSH,NUMSOLG,TEMP)

      WRITE(66,3000)

      DO I=1,NUMSOLG
        NGL = INDEXSH(I)
        NG = NUMGROUP(NGL)

        ITY = IPARG(5,NG)
        MTN = IPARG(1,NG)
        NELEM = IPARG(2,NG)
	NFT   = IPARG(3,NG)+1
        NSG = IPARG(10,NG)
        NPT = IPARG(6,NG)
	
        MID = IXS(1,NFT)
        MUID = IPM(1,MID)	

        DIFFTT = CPUTIMEG(NG) - THEOGP(NG) 

        PCT = ((CPUTIMEG(NG) - THEOGP(NG))/CPUTIMEG(NG) )*100

        WRITE(66,'(I8,A,I8,A,I8,A,I8,A,I8,A,I8,A,I8,
     .           A,E15.3,A,E15.3,A,E15.3,A,F8.3,A)')
     .     NG,'  ',ITY,'  ',NELEM,'  ',MTN,'  ',MUID,'  ',NPT,
     .     '  ',NSG,'  ',
     .     CPUTIMEG(NG),'  ',THEOGP(NG),'  ',DIFFTT,'  ',PCT,'%'

      ENDDO

 3000 FORMAT(
     &       /' '/' '/10X,' SOLID ELEMENT GROUPS'/
     &         10X,' --------------------'/
     &        ' '/
     &        ' '/
     &        '  GRP_NB  ELEM_TYP  NUM_ELEM  MAT_NUMB   MAT_USERN',
     &        '     NPT       NSG         CPUTIMEG',
     &        '         TIMETHEO             DIFF        PCT')

      END
Chd|====================================================================
Chd|  PRINTTET4                     source/system/timer.F         
Chd|-- called by -----------
Chd|        PRINTIMEG                     source/system/timer.F         
Chd|-- calls ---------------
Chd|        MY_FSORT                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE PRINTTET4(IPARG,THEOGP,NUMSOLG,IPM,IXS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include    "com01_c.inc"
#include    "com04_c.inc"
#include    "scr05_c.inc"
#include    "task_c.inc"
#include    "units_c.inc"
#include    "param_c.inc"
#include    "spmd_c.inc"
#include    "rnur_c.inc"
#include    "chara_c.inc"
#include    "scr07_c.inc"
#include    "scr19_c.inc"
#include    "ddspmd_c.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
      COMMON /TIMERG/TIMERG(2,500000),CPUTIMEG(500000)
      REAL TIMERG,CPUTIMEG
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMSOLG
      INTEGER IPARG(NPARG,*),IXS(NIXS,*),IPM(NPROPMI,*)
      REAL THEOGP(NGROUP)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL,NG,NUMGROUP(NUMSOLG),INDEXSH(NUMSOLG),
     .       ITY,MTN,NELEM,NPT,NSG,ISOLNOD,
     .        NFT,IHBE,MMA,NFUNC,I,J,MID,MUID
      REAL DIFFTT,DIFFTTSH(NUMSOLG),TEMP(NUMSOLG)
      REAL THEOTP(NUMSOLG),REELTP(NUMSOLG)
      REAL TABX(3),TABMAT(3),NPTR,TIMMAT,TELT
      REAL TPSTHTOT,TPSREEL,PCT
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      NGL = 0

      DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        MTN = IPARG(1,NG)
        NELEM = IPARG(2,NG)
        NPT = IPARG(6,NG)
        NSG = IPARG(10,NG)
        NFT = IPARG(3,NG)+1
        IHBE    = IPARG(23,NG)

        IF (ITY.EQ.1) THEN
          ISOLNOD =  IPARG(28,NG)
          IF (ISOLNOD.EQ.4) THEN
            NGL = NGL + 1
            NUMGROUP(NGL) = NG
            DIFFTTSH(NGL) = CPUTIMEG(NG) - THEOGP(NG)
          ENDIF
        ENDIF
      ENDDO

      DO I=1,NGL
       INDEXSH(I) = I
      ENDDO

      CALL MY_FSORT(DIFFTTSH,INDEXSH,NUMSOLG,TEMP)

      WRITE(66,3000)

      DO I=1,NUMSOLG
        NGL = INDEXSH(I)
        NG = NUMGROUP(NGL)
	NFT   = IPARG(3,NG)+1

        ITY = IPARG(5,NG)
        MTN = IPARG(1,NG)
        NELEM = IPARG(2,NG)
        NSG = IPARG(10,NG)
        NPT = IPARG(6,NG)

        MID = IXS(1,NFT)
        MUID = IPM(1,MID)

        DIFFTT = CPUTIMEG(NG) - THEOGP(NG) 

        PCT = ((CPUTIMEG(NG) - THEOGP(NG))/CPUTIMEG(NG) )*100

        WRITE(66,'(I8,A,I8,A,I8,A,I8,A,I8,A,I8,A,I8,
     .             A,E15.3,A,E15.3,A,E15.3,A,F8.3,A)')
     .     NG,'  ',ITY,'  ',NELEM,'  ',MTN,'  ',MUID,'  ',NPT,'  ',
     .     NSG,'  ',
     .     CPUTIMEG(NG),'  ',THEOGP(NG),'  ',DIFFTT,'  ',PCT,'%'

      ENDDO

 3000 FORMAT(
     &       /' '/' '/10X,' TETRA 4 ELEMENT GROUPS'/
     &         10X,' --------------------'/
     &        ' '/
     &        ' '/
     &        '  GRP_NB  ELEM_TYP  NUM_ELEM  MAT_NUMB   MAT_USERN',
     &        '     NPT       NSG         CPUTIMEG',
     &        '         TIMETHEO             DIFF        PCT')

      END
Chd|====================================================================
Chd|  PRINTTET10                    source/system/timer.F         
Chd|-- called by -----------
Chd|        PRINTIMEG                     source/system/timer.F         
Chd|-- calls ---------------
Chd|        MY_FSORT                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE PRINTTET10(IPARG,THEOGP,NUMSOLG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include    "com01_c.inc"
#include    "com04_c.inc"
#include    "scr05_c.inc"
#include    "task_c.inc"
#include    "units_c.inc"
#include    "param_c.inc"
#include    "spmd_c.inc"
#include    "rnur_c.inc"
#include    "chara_c.inc"
#include    "scr07_c.inc"
#include    "scr19_c.inc"
#include    "ddspmd_c.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
      COMMON /TIMERG/TIMERG(2,500000),CPUTIMEG(500000)
      REAL TIMERG,CPUTIMEG
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMSOLG
      INTEGER IPARG(NPARG,*)
      REAL THEOGP(NGROUP)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL,NG,NUMGROUP(NUMSOLG),INDEXSH(NUMSOLG),
     .       ITY,MTN,NELEM,NPT,NSG,ISOLNOD,
     .        NFT,IHBE,MMA,NFUNC,I,J
      REAL DIFFTT,DIFFTTSH(NUMSOLG),TEMP(NUMSOLG)
      REAL THEOTP(NUMSOLG),REELTP(NUMSOLG)
      REAL TABX(3),TABMAT(3),NPTR,TIMMAT,TELT
      REAL TPSTHTOT,TPSREEL,PCT
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      NGL = 0

      DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        MTN = IPARG(1,NG)
        NELEM = IPARG(2,NG)
        NPT = IPARG(6,NG)
        NSG = IPARG(10,NG)
        NFT = IPARG(3,NG)+1
        IHBE    = IPARG(23,NG)

        IF (ITY.EQ.1) THEN
          ISOLNOD =  IPARG(28,NG)
          IF (ISOLNOD.EQ.10) THEN
            NGL = NGL + 1
            NUMGROUP(NGL) = NG
            DIFFTTSH(NGL) = CPUTIMEG(NG) - THEOGP(NG)
          ENDIF
        ENDIF
      ENDDO

      DO I=1,NGL
       INDEXSH(I) = I
      ENDDO

      CALL MY_FSORT(DIFFTTSH,INDEXSH,NUMSOLG,TEMP)

      WRITE(66,3000)

      DO I=1,NUMSOLG
        NGL = INDEXSH(I)
        NG = NUMGROUP(NGL)

        ITY = IPARG(5,NG)
        MTN = IPARG(1,NG)
        NELEM = IPARG(2,NG)
        NSG = IPARG(10,NG)
        NPT = IPARG(6,NG)

        DIFFTT = CPUTIMEG(NG) - THEOGP(NG) 

        PCT = ((CPUTIMEG(NG) - THEOGP(NG))/CPUTIMEG(NG) )*100

        WRITE(66,'(I8,A,I8,A,I8,A,I8,A,I8,A,I8,A,E15.3,A,E15.3,A,
     .           E15.3,A,F8.3,A)')
     .     NG,'  ',ITY,'  ',NELEM,'  ',MTN,'  ',NPT,'  ',NSG,'  ',
     .     CPUTIMEG(NG),'  ',THEOGP(NG),'  ',DIFFTT,'  ',PCT,'%'

      ENDDO

 3000 FORMAT(
     &       /' '/' '/10X,' TETRA 10 ELEMENT GROUPS'/
     &         10X,' ---------------------'/
     &        ' '/
     &        ' '/
     &        '  GRP_NB  ELEM_TYP  NUM_ELEM  MAT_NUMB  ',
     &        '     NPT       NSG         CPUTIMEG',
     &        '         TIMETHEO             DIFF      PCT')

      END

Chd|====================================================================
Chd|  PRINTTRIANGLES                source/system/timer.F         
Chd|-- called by -----------
Chd|        PRINTIMEG                     source/system/timer.F         
Chd|-- calls ---------------
Chd|        MY_FSORT                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE PRINTTRIANGLES(IPARG,THEOGP,NUMTRIG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include    "com01_c.inc"
#include    "com04_c.inc"
#include    "scr05_c.inc"
#include    "task_c.inc"
#include    "units_c.inc"
#include    "param_c.inc"
#include    "spmd_c.inc"
#include    "rnur_c.inc"
#include    "chara_c.inc"
#include      "scr07_c.inc"
#include     "scr19_c.inc"
#include     "ddspmd_c.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
      COMMON /TIMERG/TIMERG(2,500000),CPUTIMEG(500000)
      REAL TIMERG,CPUTIMEG
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMTRIG
      INTEGER IPARG(NPARG,*)
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL,NG,NUMGROUP(NUMTRIG),INDEXSH(NUMTRIG),
     .       ITY,MTN,NELEM,NPT,NSG,
     .        NFT,IHBE,MMA,NFUNC,I,J
      REAL DIFFTT,DIFFTTSH(NUMTRIG),TEMP(NUMTRIG)
      REAL THEOTP(NUMTRIG),REELTP(NUMTRIG)
      REAL THEOGP(NGROUP),TABX(3),TABMAT(3),NPTR,TIMMAT,TELT
      REAL TPSTHTOT,TPSREEL,PCT
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      NGL = 0


      DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        MTN = IPARG(1,NG)
        NELEM = IPARG(2,NG)
        NPT = IPARG(6,NG)
        NSG = IPARG(10,NG)
        NFT = IPARG(3,NG)+1
        IHBE    = IPARG(23,NG)

        IF (ITY.EQ.7) THEN

          NGL = NGL + 1

          NUMGROUP(NGL) = NG
          DIFFTTSH(NGL) = CPUTIMEG(NG) - THEOGP(NG) 

        ENDIF
      ENDDO

      DO I=1,NGL
       INDEXSH(I) = I
      ENDDO

      CALL MY_FSORT(DIFFTTSH,INDEXSH,NUMTRIG,TEMP)

      WRITE(66,3200)

      DO I=1,NUMTRIG
        NGL = INDEXSH(I)
        NG = NUMGROUP(NGL)

        ITY = IPARG(5,NG)
        MTN = IPARG(1,NG)
        NELEM = IPARG(2,NG)
        NSG = IPARG(10,NG)
        NPT = IPARG(6,NG)

        DIFFTT = CPUTIMEG(NG) - THEOGP(NG)

        PCT = ((CPUTIMEG(NG) - THEOGP(NG))/CPUTIMEG(NG) )*100

        WRITE(66,'(I8,A,I8,A,I8,A,I8,A,I8,A,I8,A,E15.3,A,E15.3,A,
     .           E15.3,A,F8.3,A)')
     .     NG,'  ',ITY,'  ',NELEM,'  ',MTN,'  ',NPT,'  ',NSG,'  ',
     .     CPUTIMEG(NG),'  ',THEOGP(NGL),'  ',DIFFTT,'  ',PCT,'%'

      ENDDO

 3200 FORMAT(
     &       /' '/' '/10X,' TRIANGULAR ELEMENT GROUPS'/
     &         10X,' -------------------------'/
     &        ' '/
     &        ' '/
     &        '  GRP_NB  ELEM_TYP  NUM_ELEM  MAT_NUMB  ',
     &        '     NPT       NSG         CPUTIMEG',
     &        '         TIMETHEO             DIFF        PCT')

      END



Chd|====================================================================
Chd|  INTERLAGRAN                   source/system/timer.F         
Chd|-- called by -----------
Chd|        PRINTIMEG                     source/system/timer.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTERLAGRAN(TAB,LX,LTAB,X,Y)

      REAL TAB(LTAB),LX(LTAB),X,Y,MUL
      INTEGER I,J,LTAB

      Y = 0.

      DO I=1,LTAB

      MUL = 1.
      DO J=1,LTAB
        IF (I.NE.J) THEN
          MUL= MUL * (X-LX(J))/(LX(I)-LX(J))
        ENDIF
      ENDDO
      
      Y = Y + TAB(I)*MUL      

      ENDDO
      END

Cow mon tri flottant
Chd|====================================================================
Chd|  MY_FSORT                      source/system/timer.F         
Chd|-- called by -----------
Chd|        PRINTCOQUES                   source/system/timer.F         
Chd|        PRINTSOLIDES                  source/system/timer.F         
Chd|        PRINTTET10                    source/system/timer.F         
Chd|        PRINTTET4                     source/system/timer.F         
Chd|        PRINTTRIANGLES                source/system/timer.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MY_FSORT(ARRAY,INDEX,SIZE,WORK)
         INTEGER SIZE
         real
     .      ARRAY(SIZE),WORK(SIZE),TMP

         INTEGER INDEX(SIZE),I,J,ITMP

         DO I=1,SIZE
           WORK(I) = ARRAY(I)
         ENDDO

         DO I=1,SIZE
           DO J=1,SIZE-I
              IF (WORK(J).LT.WORK(J+1)) THEN
                 TMP = WORK(J)
                 WORK(J) = WORK(J+1)
                 WORK(J+1) = TMP

                 ITMP = INDEX(J)
                 INDEX(J) = INDEX(J+1)
                 INDEX(J+1) = ITMP
              ENDIF
           ENDDO
         ENDDO
      END

Chd|====================================================================
Chd|  ELAPSTIME                     source/system/timer.F         
Chd|-- called by -----------
Chd|        ADD_ELAPSED_TIME_MON_OFF      source/system/timer.F         
Chd|        ECRIT                         source/output/ecrit.F         
Chd|        PRINTIME                      source/system/timer.F         
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ELAPSTIME(ETIME)
C     reset timer elapsed time et calcul tu temps elapsed courant
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "timerr_c.inc"
      COMMON /ICLOCK/CLOCK0
      INTEGER CLOCK0
      COMMON /RCLOCK/ELAPSED
      DOUBLE PRECISION ELAPSED
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      DOUBLE PRECISION ETIME
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER CLOCK1, CLOCKRATE, NBMAX
      DOUBLE PRECISION  SECS
#if _OPENMP
       REAL(kind=8) :: OMP_ENDING_TIME
       REAL(kind=8) OMP_GET_WTIME
       external OMP_GET_WTIME
#endif
C-----------------------------------------------
      CALL SYSTEM_CLOCK(COUNT=CLOCK1, COUNT_RATE=CLOCKRATE,
     +                  COUNT_MAX=NBMAX                   )

      IF(CLOCK1 < 0 ) THEN 
        CALL SYSTEM_CLOCK(COUNT=CLOCK1, COUNT_RATE=CLOCKRATE,
     +                  COUNT_MAX=NBMAX                   )
      ENDIF
#if _OPENMP
      OMP_ENDING_TIME = OMP_GET_WTIME( )
      CLOCK0 = CLOCK1
      ELAPSED = OMP_ENDING_TIME - OMP_STARTING_TIME
      ETIME = ELAPSED
#else
      IF(CLOCK1 > 0 ) THEN
        SECS = CLOCK1-CLOCK0         ! diff temps courant - temps initial (qui peut ne pas etre 0 !)
        IF(SECS.LT.ZERO) SECS = SECS + NBMAX   ! cas depassement nb de periode maximum
        SECS = SECS/CLOCKRATE
        CLOCK0 = CLOCK1              ! recuperation du temps courant dans clock0
        ELAPSED = ELAPSED + SECS
        ETIME = ELAPSED
      ENDIF
#endif
C
      RETURN
      END
Chd|====================================================================
Chd|  STARTIME_MP                   source/system/timer.F         
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE STARTIME_MP(EVENT,NBR_GR,TIMER_MP)
C     routine d'incrementation du timer event
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "timerr_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EVENT,NBR_GR
      my_real, DIMENSION(2,NBR_GR) :: TIMER_MP      
C-----------------------------------------------
C   L o c a l   A r g u m e n t s
C-----------------------------------------------
      INTEGER I
      
      CALL CPU_TIME(TIMER_MP(1,EVENT))
     
      RETURN
      END

Chd|====================================================================
Chd|  STOPTIME_MP                   source/system/timer.F         
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE STOPTIME_MP(EVENT,NBR_GR,TIMER_MP,CPUTIME_MP)
C     routine incrementation du timer event
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "timerr_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EVENT,NBR_GR
      my_real, DIMENSION(NBR_GR) :: CPUTIME_MP
      my_real, DIMENSION(2,NBR_GR) :: TIMER_MP
C
      INTEGER CLOCK1, CLOCKRATE, NBMAX
      DOUBLE PRECISION  SECS
      INTEGER NBR_ELEM,I
C      **********************************
C
          CALL CPU_TIME(TIMER_MP(2,EVENT))
         CPUTIME_MP(EVENT) = CPUTIME_MP(EVENT) +
     .       (TIMER_MP(2,EVENT)-TIMER_MP(1,EVENT))
C
      RETURN
      END
       
Chd|====================================================================
Chd|  CUMULTIME_MP                  source/system/timer.F         
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PRINTIME_MP                   source/system/timer.F         
Chd|        SPMD_EX_CPUTIME               source/mpi/output/spmd_ex_cputime.F
Chd|====================================================================
      SUBROUTINE CUMULTIME_MP(
     1          TAILLE,IPARG,
     2          IXC,IXQ,IXT,IXP,IXTG,
     3          IXR,IXUR,IXS,KXIG3D,IPM,
     4          IGEO,GEO,POIN_UMP,CPUTIME_MP,
     5          NBR_GPMP,CPUTIME_MP_GLOB,TAB_UMP,PM,
     6          BUFMAT,TABMP_L,TAB_MAT )
C     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr07_c.inc"
      COMMON /TIMERG/TIMERG(2,500000),CPUTIMEG(500000)
      REAL TIMERG,CPUTIMEG
C-----------------------------------------------
#include    "com01_c.inc"
#include    "com04_c.inc"
#include    "scr05_c.inc"
#include    "task_c.inc"
#include    "units_c.inc"
#include    "param_c.inc"
#include    "spmd_c.inc"
#include    "rnur_c.inc"
#include    "chara_c.inc"
#include    "scr19_c.inc"
#include    "ddspmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),IXS(NIXS,*),
     .        IXQ(NIXQ,*),IXT(NIXT,*), IXP(NIXP,*),
     .        IXR(NIXR,*), IXUR(*),KXIG3D(*),IXTG(NIXTG,*),
     .        IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        TAILLE,NBR_GPMP,TABMP_L

      my_real PM(NPROPM,*),GEO(NPROPG,*),BUFMAT(*),TAB_MAT(NGROUP)
c      my_real, DIMENSION(NUMMAT,NUMGEO,TABMP_L), INTENT(IN) :: TAB_MAT_PROP

C     Mat + Prop timers     
      INTEGER, DIMENSION(NUMMAT) :: POIN_UMP
      INTEGER, DIMENSION(7,TAILLE) :: TAB_UMP
      my_real, DIMENSION(TAILLE) :: CPUTIME_MP
      my_real, DIMENSION(NBR_GPMP,NSPMD+1):: CPUTIME_MP_GLOB
      
      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG,ITY,MID,MUID,NFT,I,J,K,
     .        PID,PUID,
     .        ISOL,ISOL2,TEST,INDI3
      my_real, DIMENSION(NSPMD+1,TAILLE) :: CPUTIME_MP2
      INTEGER, DIMENSION(TAILLE) :: ELEM_MP
      my_real, DIMENSION(TAILLE) :: POIDS_REF
      my_real T1, T2
      
      
      
      INTEGER  NPROC, MSGTAG, MSGTAG2, ISTART, NGROUP_R,
     .        NEL,PROC,NGF, NGL,
     .         NELT,
     .        IRSIZE, MSGTAG3 ,NES,NEQ,NEC,NETG,NET,NEP,NER,NEUR,
     .        MPT,NPTS,NPTT,NPTOT,ISOLNOD,JSROT

       CHARACTER FILNAM*100,PROCNAM*4
       my_real
     .     TS, TQ, TC, TTG, TT, TP, TR, TUR, DTU,MINS,MAXS,MINQ,
     .     MAXQ,MINC,MAXC,MINTG,MAXTG,MINP,MAXP,MINT,MAXT,MINR,
     .     MAXR,MINUR,MAXUR, CPUG, CPUELEM, T(8,2),CC
C
      INTEGER NUMSOLG,NUMSHG,NUMTRIG,NUMOTHER,MTN,NELEM,NPT,NSG,
     .        IHBE,NFUNC,FILEN,NUMSOL4G,NUMSOL10G,JHBE

      REAL
     .   THEOGP(NGROUP),TABX(3),TABMAT(3),NPTR,TIMMAT,TELT,TPSTHTOT,
     .   TPSREEL,BATOZMULT,TPPOND
      INTEGER RECHERCHE,OFF,NUMEL_RE
      my_real INVTREF
      my_real, DIMENSION(NUMMAT,NUMGEO) :: SOLTELT1_OLD,SOLTELT6_OLD,
     .             SOLTELT8_OLD,SOLTELT16_OLD,SOLTELT20_OLD,TETRA4_OLD,
     .             TETRA10_OLD,SHTNL_OLD,TRITNL_OLD
      REAL WD
C-----------------------------------------------

!#include "weights_p4linux964_spmd.inc" 
      T1 = 0
      ELEM_MP = ZERO
      POIDS_REF = ZERO
      DO NG=1,NGROUP
        ITY   = IPARG(5,NG)
        NFT   = IPARG(3,NG)+1
        K = 0

        MTN = IPARG(1,NG)
        NELEM = IPARG(2,NG)
        NPT = IPARG(6,NG)
        NSG = IPARG(10,NG)
        IHBE = IPARG(23,NG)   
        
        
        IF(ITY==1) THEN
                !   BRIC
                MID = IXS(1,NFT)
                PID = IXS(10,NFT)
                MUID = IPM(1,MID)
                PUID = IGEO(1,PID)
                I = POIN_UMP(MID)
                K = I
        ELSEIF(ITY==2) THEN
                !   QUAD
                MID = IXQ(1,NFT)
                PID = IXQ(6,NFT)
                MUID = IPM(1,MID)
                PUID = IGEO(1,PID)
                I = POIN_UMP(MID)
                K = I
        ELSEIF(ITY==3) THEN
                !   SHELL
                MID = IXC(1,NFT)
                PID = IXC(6,NFT)
                MUID = IPM(1,MID)
                PUID = IGEO(1,PID)
                I = POIN_UMP(MID)
                K = I
        ELSEIF(ITY==4) THEN
                !   TRUSS
                MID = IXT(1,NFT)
                PID = IXT(4,NFT)
                MUID = IPM(1,MID)
                PUID = IGEO(1,PID)
                I = POIN_UMP(MID)
                K = I
        ELSEIF(ITY==5) THEN
                !   BEAM
                MID = IXP(1,NFT)
                PID = IXP(5,NFT)
                MUID = IPM(1,MID)
                PUID = IGEO(1,PID)
                I = POIN_UMP(MID)
                K = I
        ELSEIF(ITY==6) THEN
                !   RESSORT MUID=0 --> 1er case de TAB_UMP
                MID = TAB_UMP(3,1)
                PID = IXR(1,NFT)
                MUID = 0
                PUID = IGEO(1,PID)
                I = 1
                K = 1
        ELSEIF(ITY==7) THEN
                !   SHELL_3N
                MID = IXTG(1,NFT)
                PID = IXTG(5,NFT)
                MUID = IPM(1,MID)
                PUID = IGEO(1,PID)
                I = POIN_UMP(MID)
                K = I
        ENDIF

        IF(K /= 0) THEN
          TEST = 0
          DO WHILE((K<=TAILLE).AND.(TEST==0))
            ! +-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+
            IF((TAB_UMP(1,K)==MUID)) THEN
              ! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
              IF(TAB_UMP(2,K)==PUID) THEN
               ! -------------------------------
               IF(ITY/=1) THEN
                 IF (TAB_UMP(7,K)==ITY)  THEN
                  CPUTIME_MP(K) = CPUTIME_MP(K) + CPUTIMEG(NG)
                  ELEM_MP(K) = ELEM_MP(K) + IPARG(2,NG)
                  TEST=1
                  ! ---------------------
                  IF(ITY==3) THEN 
c                    POIDS_REF(K)=TAB_MAT_PROP(MID,PID,1)
                    POIDS_REF(K)=TAB_MAT(NG)

                   ELSEIF(ITY==7) THEN
c                     POIDS_REF(K)=TAB_MAT_PROP(MID,PID,2)              
                    POIDS_REF(K)=TAB_MAT(NG)              
                   ENDIF   !   <--- fin ITY==3 or 7
                   ! ---------------------          
                 ELSE      !   <--- fin TAB_UMP(7,K)==ITY
                  K=K+1
                 ENDIF
               ! -------------------------------
               ELSE        !   <--- fin ITY/=1
                 ISOL = IPARG(28,NG)
                 IF((ISOL==4).OR.(ISOL==6).OR.
     .              (ISOL==8).OR.(ISOL==10).OR.
     .              (ISOL==16).OR.(ISOL==20)) THEN
                   ISOL=ISOL+1000
                 ELSE
                  ISOL = 1
                 ENDIF
                 IF(TAB_UMP(7,K)==ISOL)  THEN
                   CPUTIME_MP(K) = CPUTIME_MP(K) + CPUTIMEG(NG)
                   ELEM_MP(K) = ELEM_MP(K) + IPARG(2,NG)
                   TEST=1
                   ! ----------------- 
                   IF(ISOL==1008) THEN
                    INDI3 = 3
                   ELSEIF(ISOL==1010) THEN
                    INDI3 = 4
                   ELSEIF(ISOL==1016) THEN 
                    INDI3 = 5
                   ELSEIF(ISOL==1020) THEN
                    INDI3 = 6
                   ELSEIF(ISOL==1006) THEN
                    INDI3 = 7
                   ELSEIF(ISOL==1004) THEN
                    INDI3 = 8
                   ELSE
                   INDI3 = 9
                   ENDIF
                   ! -----------------
c                   POIDS_REF(K)= TAB_MAT_PROP(MID,PID,INDI3)  
                   POIDS_REF(K)= TAB_MAT(NG)  
                 ELSE      !   <--- fin TAB_UMP(7,K)==ISOL
                   K=K+1
                 ENDIF
               ENDIF
              ELSE         !   <--- fin PUID
               K=K+1
              ENDIF
              ! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
            ELSE           !   <--- fin MUID 
                TEST = 1
!                PRINT*,ISPMD,NG,' ERRORRRRRRRRR ',ITY,MUID,PUID,MID,PID,MTN,CPUTIMEG(NG)
!                T1=T1+CPUTIMEG(NG)
            ENDIF
            ! +-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+-*+               
          ENDDO
        ENDIF
      ENDDO
      
      DO J=1,TAILLE
       DO I=1,NSPMD+1
        CPUTIME_MP2(I,J) = ZERO
       ENDDO
      ENDDO

      IF(NSPMD > 1) THEN
        !   MPI Comm
        CALL SPMD_EX_CPUTIME(TAILLE,CPUTIME_MP,CPUTIME_MP2)
      IF(ISPMD==0) THEN
       T1 = ZERO
       T2 = ZERO 
       DO I=1,TAILLE
        DO J=1,NSPMD
         T1 = T1 + CPUTIME_MP2(J,I)
         CPUTIME_MP2(NSPMD+1,I) = CPUTIME_MP2(NSPMD+1,I) + CPUTIME_MP2(J,I)/(NCYCLE)
        ENDDO
        IF((CPUTIME_MP2(NSPMD+1,I)>= ZERO).AND.(TAB_UMP(5,I)>0)) THEN
         CPUTIME_MP2(NSPMD+1,I) = CPUTIME_MP2(NSPMD+1,I)/(TAB_UMP(5,I))
        ELSE
         CPUTIME_MP2(NSPMD+1,I) = ZERO             
        ENDIF
       ENDDO    
      ENDIF   ! <-- (ISPMD==0)
      ELSE
       DO I=1,TAILLE
        CPUTIME_MP2(1,I) = CPUTIME_MP(I)
        CPUTIME_MP2(2,I) = CPUTIME_MP(I)/NCYCLE
        IF((CPUTIME_MP2(2,I)>= ZERO).AND.(TAB_UMP(5,I)>0)) THEN
         CPUTIME_MP2(2,I) = CPUTIME_MP2(2,I)/(TAB_UMP(5,I))
        ELSE
         CPUTIME_MP2(2,I) = ZERO             
        ENDIF
       ENDDO
      ENDIF

      DO I=1,TAILLE
       IF((CPUTIME_MP(I)>ZERO).AND.(ELEM_MP(I)>0)) THEN
        CPUTIME_MP(I) = CPUTIME_MP(I)/(ELEM_MP(I)*NCYCLE)
       ENDIF
      ENDDO
C
      CALL PRINTIME_MP(TAILLE,TAB_UMP,CPUTIME_MP2,POIN_UMP,T1,ELEM_MP,
     .                 CPUTIME_MP,POIDS_REF)
 
      RETURN
      END
      
      
Chd|====================================================================
Chd|  PRINTIME_MP                   source/system/timer.F         
Chd|-- called by -----------
Chd|        CUMULTIME_MP                  source/system/timer.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRINTIME_MP(TAILLE,TAB_UMP,CPUTIME_MP2,POIN_UMP,T1,
     .                       ELEM_MP,CPUTIME_MP,POIDS_REF)
C     routine d'ecriture des timers
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr07_c.inc"
      COMMON /TIMERG/TIMERG(2,500000),CPUTIMEG(500000)
      REAL TIMERG,CPUTIMEG
C-----------------------------------------------
#include    "com01_c.inc"
#include    "com04_c.inc"
#include    "scr05_c.inc"
#include    "task_c.inc"
#include    "units_c.inc"
#include    "param_c.inc"
#include    "spmd_c.inc"
#include    "rnur_c.inc"
#include    "chara_c.inc"
#include    "scr19_c.inc"
#include    "ddspmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAILLE
      INTEGER, DIMENSION(7,TAILLE) :: TAB_UMP
      my_real T1
      my_real, DIMENSION(NSPMD+1,TAILLE) :: CPUTIME_MP2
      INTEGER, DIMENSION(NUMMAT) :: POIN_UMP
      INTEGER, DIMENSION(TAILLE) :: ELEM_MP
      my_real, DIMENSION(TAILLE) :: POIDS_REF
      my_real, DIMENSION(TAILLE) :: CPUTIME_MP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

      CHARACTER FILNAM*100,PROCNAM*4
C
      INTEGER ITY,MTN,NEL,MID,PID,MUID,PUID,FILEN,
     .        J,I,PROC
      my_real TIME_MATPROP,POIN,TIME_REF!,TIME_ELE
C-----------------------------------------------

      IF(ISPMD==0) THEN
C ro ouverture du fichier DDW proc = 0 
       FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.ddw'
       OPEN(UNIT=66,FILE=FILNAM,ACCESS='SEQUENTIAL',
     .                  FORM='FORMATTED',STATUS='UNKNOWN')
       FILEN=ROOTLEN+14 

       WRITE(66,'(A)') 'DOMAIN DECOMPOSITION ANALYSIS FILE '
       WRITE(66,'(A)') '---------------------------------- '
       WRITE(66,'(A)') ' '
       WRITE(66,'(A)') ' '
       WRITE(66,1000) NCYCLE, T1
       WRITE(66,3000)

       DO I=1,TAILLE
        TIME_MATPROP = CPUTIME_MP2(NSPMD+1,I)
        NEL          = TAB_UMP(5,I)
        MUID         = TAB_UMP(1,I)
        PUID         = TAB_UMP(2,I)
        MID          = TAB_UMP(3,I)
        PID          = TAB_UMP(4,I)
        MTN          = TAB_UMP(6,I)
        ITY          = TAB_UMP(7,I)      

        WRITE(66,'(I12,A,I12,A,I12,A,I12,A,I12,A,I12,A,
     .           I12,A,E15.5)')
     .     MTN,'  ',ITY,'  ',NEL,'  ',MID,'  ',MUID,'  ',PID,
     .     '  ',PUID,'  ', TIME_MATPROP

       ENDDO
!
       WRITE(66,1100)
       WRITE(66,1400) NUMMAT,NUMGEO
       DO I=1,(NUMMAT)
        POIN = POIN_UMP(I)
        WRITE(66,'(I8,A)') POIN_UMP(I)
       ENDDO
       WRITE(66,'(A)') '/END'
       CLOSE(UNIT=66)
       WRITE (IOUT,1300)  FILNAM(1:FILEN)
       WRITE (ISTDO,1300) FILNAM(1:FILEN)
      ENDIF

C ro ouverture du fichier DDW par proc
      IF(IDDWSTAT/=0) THEN
       WRITE(PROCNAM,'(I4.4)')ISPMD+1
       FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//PROCNAM//'.ddw'
       OPEN(UNIT=67,FILE=FILNAM,ACCESS='SEQUENTIAL',
     .                  FORM='FORMATTED',STATUS='UNKNOWN')
       FILEN=ROOTLEN+14 

       WRITE(67,'(A)') 'DOMAIN DECOMPOSITION ANALYSIS FILE '
       WRITE(67,'(A)') '---------------------------------- '
       WRITE(67,'(A)') ' '
       WRITE(67,'(A)') ' '
       WRITE(67,2000) NCYCLE
       PROC = ISPMD
       WRITE(67,1500) PROC
       WRITE(67,3101)
!
       DO I=1,TAILLE
        TIME_MATPROP = CPUTIME_MP(I)
        TIME_REF = POIDS_REF(I)
        IF(TIME_MATPROP>ZERO) THEN
         NEL          = ELEM_MP(I)
         MUID         = TAB_UMP(1,I)
         PUID         = TAB_UMP(2,I)
         MID          = TAB_UMP(3,I)
         PID          = TAB_UMP(4,I)
         MTN          = TAB_UMP(6,I)
         ITY          = TAB_UMP(7,I)
                  

         WRITE(67,'(I12,A,I12,A,I12,A,I12,A,I12,A,I12,A,
     .            I12,A,E15.5,A,E15.5)')
     .      MTN,'  ',ITY,'  ',NEL,'  ',MID,'  ',MUID,'  ',PID,
     .      '  ',PUID,' ', TIME_MATPROP,' ',TIME_REF
        ENDIF

        

       ENDDO
       WRITE(67,'(A)') '/END'
       CLOSE(UNIT=67)
      ENDIF             ! end IDDWSTAT/=0


 1000 FORMAT(
     & 4X,'TOTAL NUMBER OF CYCLES. . . . . . . . . . . . . .',I10/
     & 4X,'TOTAL ELEMENT FORCE TIME. . . . . . . . . . . . .',1PG20.13)
 2000 FORMAT(
     & 4X,'TOTAL NUMBER OF CYCLES. . . . . . . . . . . . . .',I10)
 1400 FORMAT(
     & 4X,'MATERIAL NUMBER . . . . . . . . . . . . . .',I10/
     & 4X,'PROPERTY NUMBER . . . . . . . . . . . . . .',I10)

 1100 FORMAT(
     &       /' '/' '/4X,' POINTER  ')
     
 1500 FORMAT(
     & /' '/' '/4X,'ISPMD. . . . . . . . . . . . . .',I10)

 1200 FORMAT(
     & 4X,'SOLID, SHELL, TRIANGULAR GROUP TIME'/,
     & 4X,'-----------------------------------'/,
     & 4X,'TOTAL GROUP TIME . . . . . . . . . . . . .  . . .',1PG20.13/
     & 4X,'STARTER ESTIMATED GROUP TIME . . . . . . .  . . .',1PG20.13)

 1300   FORMAT (4X,' DD ANALYSIS FILE:',1X,A,' WRITTEN')
 
 3000 FORMAT(
     &       /' '/' '/10X,' MATERIAL/PROPERTY TIMER'/
     &         10X,' --------------------'/
     &        ' '/
     &        ' '/
     &        '    LAW_NUMB      ELEM_TYP      NUM_ELEM      MAT_NUMB      MAT_USER',
     &        '     PROP_NUMB     PROP_USER          CPUTIME')
     
 3100 FORMAT(
     &        '    LAW_NUMB      ELEM_TYP      NUM_ELEM      MAT_NUMB      MAT_USER',
     &        '     PROP_NUMB     PROP_USER          CPUTIME')
     
 3101 FORMAT(
     &        '    LAW_NUMB      ELEM_TYP      NUM_ELEM      MAT_NUMB      MAT_USER',
     &        '     PROP_NUMB     PROP_USER         CPUTIME')
C
      RETURN
      END

C workaround / one variable in commandline.inc conflicts with scr07.inc
Chd|====================================================================
Chd|  GET_MEM_MAP_VAR               source/system/timer.F         
Chd|-- called by -----------
Chd|        PRINTIME                      source/system/timer.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GET_MEM_MAP_VAR(GOT_M_MAP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include       "commandline.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER GOT_M_MAP
C-----------------------------------------------
      GOT_M_MAP = GOT_MEM_MAP
        
      END

!       ----------------------------------------
Chd|====================================================================
Chd|  ADD_ELAPSED_TIME_MON_OFF      source/system/timer.F         
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CLOSE_C                       source/output/tools/sortie_c.c
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        ELAPSTIME                     source/system/timer.F         
Chd|        FSEEK_END_C                   source/output/tools/sortie_c.c
Chd|        OPEN_C                        source/output/tools/sortie_c.c
Chd|        WRITE_DPDB                    source/output/tools/write_db.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        TIME_MOD                      share/modules/time_mod.F      
Chd|====================================================================
        SUBROUTINE ADD_ELAPSED_TIME_MON_OFF( )
!       When /MON/OFF is used, main processor 
!       must write the elapsed time
C-----------------------------------------------
C       M o d u l e s
C-----------------------------------------------
        USE TIME_MOD
        USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C       G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C       C o m m o n   B l o c k s
C-----------------------------------------------
#include        "com01_c.inc"
#include        "com04_c.inc"
#include        "couple_c.inc"
#include        "impl1_c.inc"
#include        "remesh_c.inc"
#include        "scr05_c.inc"
#include        "scr07_c.inc"
#include        "scr18_c.inc"
#include        "alefvm.inc"
#include        "sms_c.inc"
#include        "sphcom.inc"
#include        "task_c.inc"
#include        "timerc_c.inc"
#include        "timeri_c.inc"
#include        "timerr_c.inc"
#include        "units_c.inc"
#include        "warn_c.inc"
#include        "filescount_c.inc"
        COMMON /ICLOCK/CLOCK0
        INTEGER CLOCK0
        COMMON /RCLOCK/ELAPSED
        DOUBLE PRECISION ELAPSED
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, NPROC
        DOUBLE PRECISION  SECS
        INTEGER :: LEN_RST_NAME
        LOGICAL :: BOOL_FILE
        INTEGER :: CURRENT_RUN,OFFSET
        INTEGER, DIMENSION(2148) :: RST_NAME
        INTEGER :: LEN_TMP_NAME
        CHARACTER(len=4096) :: TMP_NAME

C---------------------------
C       Elapsed time & Estimated Speedup
C---------------------------
        CALL ELAPSTIME(SECS)
!       ---------------------------
        IF(ISPMD==0) THEN
            CURRENT_RUN = GLOBAL_COMP_TIME%RUN_NBR
            GLOBAL_COMP_TIME%ENGINE_TIME(CURRENT_RUN) = SECS
            LEN_RST_NAME = LEN_TRIM(GLOBAL_COMP_TIME%RST_NAME)
            LEN_TMP_NAME = OUTFILE_NAME_LEN +LEN_RST_NAME
            TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//GLOBAL_COMP_TIME%RST_NAME(1:LEN_RST_NAME)

            DO I = 1, LEN_TMP_NAME
                RST_NAME(I) = ICHAR(TMP_NAME(I:I))
            END DO
            BOOL_FILE=.FALSE.
            INQUIRE(FILE=TMP_NAME(1:LEN_TMP_NAME), EXIST=BOOL_FILE)
!           check if *_0000.rst exists
            IF(BOOL_FILE) THEN
                CALL CUR_FIL_C(20)
                CALL OPEN_C(RST_NAME,LEN_TMP_NAME,2)
!               write the starter + engine elapsed time
                OFFSET= - STORAGE_SIZE(GLOBAL_COMP_TIME%ENGINE_TIME(CURRENT_RUN))/8        
                CALL FSEEK_END_C(OFFSET)
                CALL WRITE_DPDB(GLOBAL_COMP_TIME%ENGINE_TIME(CURRENT_RUN),1)
                CALL CLOSE_C
            ENDIF
        ENDIF
!       ---------------------------        

        RETURN
        END SUBROUTINE ADD_ELAPSED_TIME_MON_OFF
!       ----------------------------------------
