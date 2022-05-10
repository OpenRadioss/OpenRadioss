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
Chd|  ST_QAPRINT_EBCS               source/output/qaprint/st_qaprint_ebcs.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        ALE_EBCS_MOD                  ../common_source/modules/ale_ebcs_mod.F
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_EBCS(EBCS_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE QA_OUT_MOD
        USE ALE_EBCS_MOD
        USE RESTMOD
        USE EBCS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "scr10_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
#include      "nchar_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(t_ebcs_tab), TARGET, INTENT(IN) :: EBCS_TAB
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: OK_QA
      CHARACTER (LEN=255) :: VARNAME
      INTEGER :: II, JJ
      DOUBLE PRECISION :: TMPVAL
      CLASS(t_ebcs), POINTER :: EBCS
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(NEBCS == 0)RETURN

      OK_QA = MYQAKEY('/EBCS')  
                                                         
      IF (OK_QA) THEN   
         DO II = 1, NEBCS
            EBCS => EBCS_TAB%tab(ii)%poly
            CALL QAPRINT('EBCS ID ',EBCS%ebcs_id,0.0_8)
            CALL QAPRINT('EBCS TYPE ',EBCS%type,0.0_8)
            CALL QAPRINT('EBCS SURF_ID ',EBCS%surf_id,0.0_8)
            CALL QAPRINT('EBCS NB_NODE ',EBCS%nb_node,0.0_8)
            CALL QAPRINT('EBCS NB_ELEM ',EBCS%nb_elem,0.0_8)
            SELECT TYPE (EBCS) 
            TYPE IS (t_ebcs_pres)
              CALL QAPRINT('IPRES ',EBCS%ipres,0.0_8)
              CALL QAPRINT('IRHO ',EBCS%irho,0.0_8)
              CALL QAPRINT('IENER ',EBCS%iener,0.0_8)
              CALL QAPRINT('IVX ',EBCS%ivx,0.0_8)
              CALL QAPRINT('IVY ',EBCS%ivy,0.0_8)
              CALL QAPRINT('IVZ ',EBCS%ivz,0.0_8)
              TMPVAL = EBCS%c
              CALL QAPRINT('C ', 0, TMPVAL)
              TMPVAL = EBCS%pres
              CALL QAPRINT('PRES ', 0, TMPVAL)
              TMPVAL = EBCS%rho
              CALL QAPRINT('RHO ', 0, TMPVAL)
              TMPVAL = EBCS%ener
              CALL QAPRINT('ENER ', 0, TMPVAL)
              TMPVAL = EBCS%vx
              CALL QAPRINT('VX ', 0, TMPVAL)
              TMPVAL = EBCS%vy
              CALL QAPRINT('VY ', 0, TMPVAL)
              TMPVAL = EBCS%vz
              CALL QAPRINT('VZ ', 0, TMPVAL)
              TMPVAL = EBCS%lcar
              CALL QAPRINT('LCAR ',0, TMPVAL)
              TMPVAL = EBCS%r1
              CALL QAPRINT('R1 ', 0, TMPVAL)
              TMPVAL = EBCS%r2
              CALL QAPRINT('R2 ', 0, TMPVAL)
            TYPE IS (t_ebcs_valvin)
              CALL QAPRINT('IPRES ',EBCS%ipres,0.0_8)
              CALL QAPRINT('IRHO ',EBCS%irho,0.0_8)
              CALL QAPRINT('IENER ',EBCS%iener,0.0_8)
              TMPVAL = EBCS%c
              CALL QAPRINT('C ', 0, TMPVAL)
              TMPVAL = EBCS%pres
              CALL QAPRINT('PRES ', 0, TMPVAL)
              TMPVAL = EBCS%rho
              CALL QAPRINT('RHO ', 0, TMPVAL)
              TMPVAL = EBCS%ener
              CALL QAPRINT('ENER ', 0, TMPVAL)
               TMPVAL = EBCS%lcar
              CALL QAPRINT('LCAR ', 0, TMPVAL)
              TMPVAL = EBCS%r1
              CALL QAPRINT('R1 ', 0, TMPVAL)
              TMPVAL = EBCS%r2
              CALL QAPRINT('R2 ', 0, TMPVAL)
            TYPE IS (t_ebcs_valvout)
              CALL QAPRINT('IPRES ',EBCS%ipres,0.0_8)
              CALL QAPRINT('IRHO ',EBCS%irho,0.0_8)
              CALL QAPRINT('IENER ',EBCS%iener,0.0_8)
              TMPVAL = EBCS%c
              CALL QAPRINT('C ', 0, TMPVAL)
              TMPVAL = EBCS%pres
              CALL QAPRINT('PRES ', 0, TMPVAL)
              TMPVAL = EBCS%rho
              CALL QAPRINT('RHO ', 0, TMPVAL)
              TMPVAL = EBCS%ener
              CALL QAPRINT('ENER ', 0, TMPVAL)
               TMPVAL = EBCS%lcar
              CALL QAPRINT('LCAR ', 0, TMPVAL)
              TMPVAL = EBCS%r1
              CALL QAPRINT('R1 ', 0, TMPVAL)
              TMPVAL = EBCS%r2
              CALL QAPRINT('R2 ', 0, TMPVAL)
            TYPE IS (t_ebcs_gradp0)
              CALL QAPRINT('IPRES ',EBCS%ipres,0.0_8)
              CALL QAPRINT('IRHO ',EBCS%irho,0.0_8)
              CALL QAPRINT('IENER ',EBCS%iener,0.0_8)
              TMPVAL = EBCS%c
              CALL QAPRINT('C ', 0, TMPVAL)
              TMPVAL = EBCS%pres
              CALL QAPRINT('PRES ', 0, TMPVAL)
              TMPVAL = EBCS%rho
              CALL QAPRINT('RHO ', 0, TMPVAL)
              TMPVAL = EBCS%ener
              CALL QAPRINT('ENER ', 0, TMPVAL)
              TMPVAL = EBCS%lcar
              CALL QAPRINT('LCAR ', 0, TMPVAL)
              TMPVAL = EBCS%r1
              CALL QAPRINT('R1 ', 0, TMPVAL)
              TMPVAL = EBCS%r2
              CALL QAPRINT('R2 ', 0, TMPVAL)
            TYPE IS (t_ebcs_vel)
              CALL QAPRINT('IRHO ',EBCS%irho,0.0_8)
              CALL QAPRINT('IENER ',EBCS%iener,0.0_8)
              CALL QAPRINT('IVX ',EBCS%ivx,0.0_8)
              CALL QAPRINT('IVY ',EBCS%ivy,0.0_8)
              CALL QAPRINT('IVZ ',EBCS%ivz,0.0_8)
              TMPVAL = EBCS%c
              CALL QAPRINT('C ', 0, TMPVAL)
              TMPVAL = EBCS%rho
              CALL QAPRINT('RHO ', 0, TMPVAL)
              TMPVAL = EBCS%ener
              CALL QAPRINT('ENER ', 0, TMPVAL)
              TMPVAL = EBCS%vx
              CALL QAPRINT('VX ', 0, TMPVAL)
              TMPVAL = EBCS%vy
              CALL QAPRINT('VY ', 0, TMPVAL)
              TMPVAL = EBCS%vz
              CALL QAPRINT('VZ ', 0, TMPVAL)
              TMPVAL = EBCS%lcar
              CALL QAPRINT('LCAR ', 0, TMPVAL)
              TMPVAL = EBCS%r1
              CALL QAPRINT('R1 ', 0, TMPVAL)
              TMPVAL = EBCS%r2
              CALL QAPRINT('R2 ', 0, TMPVAL)
            TYPE IS (t_ebcs_normv)
              CALL QAPRINT('IRHO ',EBCS%irho,0.0_8)
              CALL QAPRINT('IENER ',EBCS%iener,0.0_8)
              CALL QAPRINT('IVIMP ',EBCS%ivimp,0.0_8)
              TMPVAL = EBCS%c
              CALL QAPRINT('C ', 0, TMPVAL)
              TMPVAL = EBCS%rho
              CALL QAPRINT('RHO ', 0, TMPVAL)
              TMPVAL = EBCS%ener
              CALL QAPRINT('ENER ', 0, TMPVAL)
              TMPVAL = EBCS%vimp
              CALL QAPRINT('VIMP ',0, TMPVAL)
              TMPVAL = EBCS%lcar
              CALL QAPRINT('LCAR ', 0, TMPVAL)
              TMPVAL = EBCS%r1
              CALL QAPRINT('R1 ', 0, TMPVAL)
              TMPVAL = EBCS%r2
              CALL QAPRINT('R2 ', 0, TMPVAL)
            TYPE IS (t_ebcs_inip)
              TMPVAL = EBCS%c
              CALL QAPRINT('C ', 0, TMPVAL)
              TMPVAL = EBCS%rho
              CALL QAPRINT('RHO ', 0, TMPVAL)
              TMPVAL = EBCS%lcar
              CALL QAPRINT('LCAR ',0, TMPVAL)
            TYPE IS (t_ebcs_iniv)
              TMPVAL = EBCS%c
              CALL QAPRINT('C ', 0, TMPVAL)
              TMPVAL = EBCS%rho
              CALL QAPRINT('RHO ', 0, TMPVAL)
              TMPVAL = EBCS%lcar
              CALL QAPRINT('LCAR ', 0, TMPVAL)
            TYPE IS (t_ebcs_monvol)
              CALL QAPRINT('MONVOLID ',EBCS%monvol_id,0.0_8)
              CALL QAPRINT('SENSORID ',EBCS%sensor_id,0.0_8)
            TYPE IS (t_ebcs_inlet)
              CALL QAPRINT('FORMULATION ',EBCS%fvm_inlet_data%formulation,0.0_8)
              CALL QAPRINT('VECTOR_VELOCITY ',EBCS%fvm_inlet_data%vector_velocity,0.0_8)
              DO JJ = 1, 3
                 IF (EBCS%fvm_inlet_data%func_vel(JJ) /= 0) THEN
                    WRITE(VARNAME,'(A,I0)') 'IVEL', JJ
                    CALL QAPRINT(TRIM(VARNAME),EBCS%fvm_inlet_data%func_vel(JJ),0.0_8)
                 ENDIF
              ENDDO
              DO JJ = 1, 21
                 IF (EBCS%fvm_inlet_data%func_alpha(JJ) /= 0) THEN
                    WRITE(VARNAME,'(A,I0)') 'IALPHA', JJ
                    CALL QAPRINT(TRIM(VARNAME),EBCS%fvm_inlet_data%func_alpha(JJ),0.0_8)
                 ENDIF
              ENDDO
              DO JJ = 1, 21
                 IF (EBCS%fvm_inlet_data%func_rho(JJ) /= 0) THEN
                    WRITE(VARNAME,'(A,I0)') 'IRHO', JJ
                    CALL QAPRINT(TRIM(VARNAME),EBCS%fvm_inlet_data%func_rho(JJ),0.0_8)
                 ENDIF
              ENDDO
              DO JJ = 1, 21
                 IF (EBCS%fvm_inlet_data%func_pres(JJ) /= 0) THEN
                    WRITE(VARNAME,'(A,I0)') 'Ipres', JJ
                    CALL QAPRINT(TRIM(VARNAME),EBCS%fvm_inlet_data%func_pres(JJ),0.0_8)
                 ENDIF
              ENDDO
              DO JJ = 1, 3
                 TMPVAL = EBCS%fvm_inlet_data%val_vel(JJ)
                 IF (EBCS%fvm_inlet_data%val_vel(JJ) /= ZERO) THEN
                    WRITE(VARNAME,'(A,I0)') 'FVEL', JJ
                    CALL QAPRINT(TRIM(VARNAME),0,TMPVAL)
                 ENDIF
              ENDDO
              DO JJ = 1, 21
                 TMPVAL = EBCS%fvm_inlet_data%val_alpha(JJ)
                 IF (EBCS%fvm_inlet_data%val_alpha(JJ) /= ZERO) THEN
                    WRITE(VARNAME,'(A,I0)') 'FALPHA', JJ
                    CALL QAPRINT(TRIM(VARNAME),0,TMPVAL)
                 ENDIF
              ENDDO
              DO JJ = 1, 21
                 TMPVAL = EBCS%fvm_inlet_data%val_rho(JJ)
                 IF (EBCS%fvm_inlet_data%val_rho(JJ) /= ZERO) THEN
                    WRITE(VARNAME,'(A,I0)') 'FRHO', JJ
                    CALL QAPRINT(TRIM(VARNAME),0,TMPVAL)
                 ENDIF
              ENDDO
              DO JJ = 1, 21
                 TMPVAL = EBCS%fvm_inlet_data%val_pres(JJ)
                 IF (EBCS%fvm_inlet_data%val_pres(JJ) /= ZERO) THEN
                    WRITE(VARNAME,'(A,I0)') 'Fpres', JJ
                    CALL QAPRINT(TRIM(VARNAME),0,TMPVAL)
                 ENDIF
              ENDDO
            TYPE IS (t_ebcs_fluxout)
            
            END SELECT
         ENDDO
      ENDIF                                                                             
C-----------------------------------------------
      RETURN
      END

