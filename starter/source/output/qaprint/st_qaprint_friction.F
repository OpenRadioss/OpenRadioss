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
Chd|  ST_QAPRINT_FRICTION           source/output/qaprint/st_qaprint_friction.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_FRICTION(NOM_OPT   ,INOM_OPT  ,INTBUF_FRIC_TAB,NPFRICORTH,
     2                                PFRICORTH , IREPFORTH, PHIFORTH,VFORTH)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE INTBUF_FRIC_MOD                     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "param_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NOM_OPT(LNOPT1,SNOM_OPT1), INOM_OPT(SINOM_OPT)
      INTEGER, INTENT(IN) :: NPFRICORTH , PFRICORTH(*) , IREPFORTH(*)
      my_real, INTENT(IN) :: PHIFORTH(*), VFORTH(*)
C
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MY_ID, MY_FRIC, LENI, LENC,IORTH ,NSET,J
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C-----------------------------------------------
C     /FRICTION /FRIC_ORIENT
C-----------------------------------------------
      IF (MYQAKEY('FRICTION')) THEN
        DO MY_FRIC=1,NINTERFRIC
C
          TITR(1:nchartitle)=''

          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,MY_FRIC),LTITR)

          MY_ID = NOM_OPT(1,INOM_OPT(29)+ MY_FRIC) 


          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT('FRICTION',MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_FRIC_FAKE_NAME', MY_ID,0.0_8)
          END IF
C
          NSET = INTBUF_FRIC_TAB(MY_FRIC)%NSETPRTS

          WRITE(VARNAME,'(A)') 'NSETPRTS'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INTBUF_FRIC_TAB(MY_FRIC)%NSETPRTS,0.0_8) 

          WRITE(VARNAME,'(A)') 'FRICMOD'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INTBUF_FRIC_TAB(MY_FRIC)%FRICMOD,0.0_8) 

          WRITE(VARNAME,'(A)') 'FRICFORM'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INTBUF_FRIC_TAB(MY_FRIC)%FRICFORM,0.0_8) 

          WRITE(VARNAME,'(A)') 'IFFILTER'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INTBUF_FRIC_TAB(MY_FRIC)%IFFILTER,0.0_8) 

          WRITE(VARNAME,'(A)') 'S_TABPARTS_FRIC'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INTBUF_FRIC_TAB(MY_FRIC)%S_TABPARTS_FRIC,0.0_8) 

          IORTH = INTBUF_FRIC_TAB(MY_FRIC)%IORTHFRIC

          WRITE(VARNAME,'(A)') 'IORTHFRIC'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INTBUF_FRIC_TAB(MY_FRIC)%IORTHFRIC,0.0_8) 

          WRITE(VARNAME,'(A)') 'XFILTR_FRIC'
          TEMP_DOUBLE = INTBUF_FRIC_TAB(MY_FRIC)%XFILTR_FRIC
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

          IF(NSET > 0) THEN
            DO I=1,NSET
               WRITE(VARNAME,'(A,I0)') 'PART_COUPLE',I
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INTBUF_FRIC_TAB(MY_FRIC)%TABCOUPLEPARTS_FRIC(I),0.0_8) 
            ENDDO

            DO I=1,INTBUF_FRIC_TAB(MY_FRIC)%S_TABPARTS_FRIC
               WRITE(VARNAME,'(A,I0)') 'PART_FRIC',I
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INTBUF_FRIC_TAB(MY_FRIC)%TABPARTS_FRIC(I),0.0_8) 
            ENDDO

            DO I=1,INTBUF_FRIC_TAB(MY_FRIC)%S_TABPARTS_FRIC
               WRITE(VARNAME,'(A,I0)') 'AD_PART_FRIC',I
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INTBUF_FRIC_TAB(MY_FRIC)%ADPARTS_FRIC(I),0.0_8) 
            ENDDO

            DO I=1,NSET
               WRITE(VARNAME,'(A,I0)') 'IFRICORTH',I
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INTBUF_FRIC_TAB(MY_FRIC)%IFRICORTH(I),0.0_8) 
            ENDDO
    
          ENDIF

          IF(INTBUF_FRIC_TAB(MY_FRIC)%FRICMOD ==0 ) THEN   
             LENC =2  
          ELSE
             LENC = 8
          ENDIF
          
          IF (IORTH == 0 ) THEN
             LENI    = 1
          ELSE
             LENI    = 2
          ENDIF

          IF(NSET > 0) THEN
C default values 
            WRITE(VARNAME,'(A,I0)') 'mu_def'
            TEMP_DOUBLE = INTBUF_FRIC_TAB(MY_FRIC)%TABCOEF_FRIC(1)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)   

            WRITE(VARNAME,'(A,I0)') 'Viscf_def'
            TEMP_DOUBLE = INTBUF_FRIC_TAB(MY_FRIC)%TABCOEF_FRIC(2)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE) 
    
            IF(INTBUF_FRIC_TAB(MY_FRIC)%FRICMOD > 0 ) THEN 
               DO J=1,6
                  WRITE(VARNAME,'(A,I0,I0)') 'FRIC_COEF_def_',J,I
                  TEMP_DOUBLE = INTBUF_FRIC_TAB(MY_FRIC)%TABCOEF_FRIC(2+J)
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
               ENDDO
             ENDIF  
      
            DO I=1,NSET
C             VARNAME sert de nom de la varable dans le ref.extract (sans blank character)
              IORTH = INTBUF_FRIC_TAB(MY_FRIC)%IFRICORTH(I)
              IF(IORTH == 0 ) THEN
                 WRITE(VARNAME,'(A,I0)') 'mu_',I
                 TEMP_DOUBLE = INTBUF_FRIC_TAB(MY_FRIC)%TABCOEF_FRIC(LENI*LENC*(I-1)+LENC+1)
                 CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

                 WRITE(VARNAME,'(A,I0)') 'Viscf_',I
                 TEMP_DOUBLE = INTBUF_FRIC_TAB(MY_FRIC)%TABCOEF_FRIC(LENI*LENC*(I-1)+LENC+2)
                 CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

                 IF(INTBUF_FRIC_TAB(MY_FRIC)%FRICMOD > 0 ) THEN 
                   DO J=1,6
                      WRITE(VARNAME,'(A,I0,I0)') 'FRIC_COEF_',J,I
                      TEMP_DOUBLE = INTBUF_FRIC_TAB(MY_FRIC)%TABCOEF_FRIC(LENI*LENC*(I-1)+LENC+2+J)
                      CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                   ENDDO
                 ENDIF  
               ELSE
                 WRITE(VARNAME,'(A,I0)') 'mu_orth_1_',I
                 TEMP_DOUBLE = INTBUF_FRIC_TAB(MY_FRIC)%TABCOEF_FRIC(LENI*LENC*(I-1)+LENC+1)
                 CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

                 WRITE(VARNAME,'(A,I0)') 'Viscf_orth_1_',I
                 TEMP_DOUBLE = INTBUF_FRIC_TAB(MY_FRIC)%TABCOEF_FRIC(LENI*LENC*(I-1)+LENC+2)
                 CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

                 WRITE(VARNAME,'(A,I0)') 'mu_orth_2_',I
                 TEMP_DOUBLE = INTBUF_FRIC_TAB(MY_FRIC)%TABCOEF_FRIC(LENI*LENC*(I-1)+2*LENC+1)
                 CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

                 WRITE(VARNAME,'(A,I0)') 'Viscf_orth_2_',I
                 TEMP_DOUBLE = INTBUF_FRIC_TAB(MY_FRIC)%TABCOEF_FRIC(LENI*LENC*(I-1)+2*LENC+2)
                 CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

                 IF(INTBUF_FRIC_TAB(MY_FRIC)%FRICMOD > 0 ) THEN 
                   DO J=1,6
                      WRITE(VARNAME,'(A,I0,I0)') 'FRIC_COEF_orth_1_',J,I
                      TEMP_DOUBLE = INTBUF_FRIC_TAB(MY_FRIC)%TABCOEF_FRIC(LENI*LENC*(I-1)+LENC+2+J)
                      CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)

                      WRITE(VARNAME,'(A,I0,I0)') 'FRIC_COEF_orth_2_',J,I
                      TEMP_DOUBLE = INTBUF_FRIC_TAB(MY_FRIC)%TABCOEF_FRIC(LENI*LENC*(I-1)+2*LENC+2+J)
                      CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
                   ENDDO
                 ENDIF  
               ENDIF

            END DO
          ENDIF
C
        END DO ! MY_FRIC=1,NINTERFRIC

        IF(NPFRICORTH /=0) THEN

           WRITE(VARNAME,'(A)') 'NPFRICORTH'
           CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NPFRICORTH,0.0_8) 

           DO I=1,NPART
             WRITE(VARNAME,'(A,I0)') 'PFRICORTH',I
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),PFRICORTH(I),0.0_8) 
           ENDDO

           DO I=1,NPFRICORTH
             WRITE(VARNAME,'(A,I0)') 'IREPFORTH',I
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IREPFORTH(I),0.0_8) 
           ENDDO
           DO I=1,3*NPFRICORTH
             WRITE(VARNAME,'(A,I0)') 'VFORTH',I
              TEMP_DOUBLE = VFORTH(I)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
           ENDDO
           DO I=1,NPFRICORTH
             WRITE(VARNAME,'(A,I0)') 'PHIFORTH',I
              TEMP_DOUBLE = PHIFORTH(I)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
           ENDDO
        ENDIF

      ENDIF
C-----------------------------------------------
      RETURN
      END
