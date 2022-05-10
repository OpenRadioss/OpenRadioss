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
Chd|  ST_QAPRINT_ELEMENT            source/output/qaprint/st_qaprint_element.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_ELEMENT(IXS , IXS10, IPM   , IGEO, ITAB, 
     1                              ISOLNOD,IXR, R_SKEW,ISKWN, IXP ,
     2                              IXT    ,X  , IXC   ,IXTG ,THKE ,
     3                              SH4ANG ,THKEC,SH3ANG,KXSP ,IPARTSP,
     4                              IPART  ,IXR_KJ,KXX ,IXX  , IPARTX ,
     5                              IXRI   ,IXS16 ,IXQ )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE MESSAGE_MOD
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
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr21_c.inc"
#include      "scr23_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
#include      "my_allocate.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IXS(NIXS,*), IXS10(6,*), IPM(NPROPMI,*), IGEO(NPROPGI,*)
      INTEGER, INTENT(IN) :: ITAB(*), ISOLNOD(*),IXR_KJ(5,*)
      INTEGER, INTENT(IN) :: IXR(NIXR,*), R_SKEW(*), ISKWN(LISKN,*), IXP(NIXP,*), IXT(NIXT,*),
     .                       IXC(NIXC,*),IXTG(NIXTG,*),KXSP(NISP,*),IPARTSP(*),IPART(LIPART1,*),
     .                       KXX(NIXX,*),IXX(*),IPARTX(*),IXRI(4,*),IXS16(8,*),IXQ(NIXQ,*)
      my_real, INTENT(IN) ::
     .                       X(3,NUMNOD),
     .                        THKE(*),THKEC(*),SH4ANG(*),SH3ANG(*)
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, MY_ID, TEMP_INT, NS, MY_MID, MY_PID, MY_NOD, MY_SKEW,NC
      INTEGER :: WORK(70000),NUMEL_KJ
      INTEGER, ALLOCATABLE, DIMENSION(:) :: INDEX,ITR1
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
      INTEGER ITETRA4(4),IPENTA6(6)
      DATA ITETRA4/2,4,7,6/,IPENTA6/2,3,4,6,7,8/
C-----------------------------------------------
C     Elements
C-----------------------------------------------

      IF (MYQAKEY('ELEMENTS')) THEN
C
C-----------------------------------------------
C       Solid elements
C-----------------------------------------------
C
        MY_ALLOCATE(INDEX,2*NUMELS)
        MY_ALLOCATE(ITR1,NUMELS)
C
        DO I=1,NUMELS8
          ITR1(I)=IXS(NIXS,I)
        ENDDO 
        CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMELS8,1)
        DO I=1,NUMELS8
          NS=INDEX(I)
C
          MY_ID = IXS(NIXS,NS)
          CALL QAPRINT('A_SOLID_ELEMENT_ID', MY_ID,0.0_8)
C
          MY_MID = IPM(1,IXS(1,NS))
          CALL QAPRINT('A_SOLID_ELEMENT_MID', MY_MID,0.0_8)
C
          MY_PID = IGEO(1,IXS(10,NS))
          CALL QAPRINT('A_SOLID_ELEMENT_PID', MY_PID,0.0_8)
C
          IF(ISOLNOD(NS)==4)THEN
            DO J=1,4
              IF(IXS(ITETRA4(J),NS)/=0)THEN
C
C               VARNAME sert de nom de la varable dans le ref.extract (sans blank character)
C
                MY_NOD = ITAB(IXS(ITETRA4(J),NS))
                WRITE(VARNAME,'(A,I0)') 'NODE_',J
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
              END IF
            END DO
          ELSEIF(ISOLNOD(NS)==6)THEN
            DO J=1,6
              IF(IXS(IPENTA6(J),NS)/=0)THEN
C
C               VARNAME sert de nom de la varable dans le ref.extract (sans blank character)
C
                MY_NOD = ITAB(IXS(IPENTA6(J),NS))
                WRITE(VARNAME,'(A,I0)') 'NODE_',J
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
              END IF
            END DO
          ELSEIF(ISOLNOD(NS)==8)THEN
            DO J=2,9
              IF(IXS(J,NS)/=0)THEN
C
C               VARNAME sert de nom de la varable dans le ref.extract (sans blank character)
C
                MY_NOD = ITAB(IXS(J,NS))
                WRITE(VARNAME,'(A,I0)') 'NODE_',J-1
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
              END IF
            END DO
          END IF
C
        END DO ! DO I=1,NUMELS8
C
C-----------------------------------------------
C       Tetra10 Elements
C-----------------------------------------------
        DO I=1,NUMELS10
          ITR1(I)=IXS(NIXS,NUMELS8+I)
        ENDDO 
        CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMELS10,1)
C
        DO I=1,NUMELS10
          NS=INDEX(I)
C
          MY_ID = IXS(NIXS,NUMELS8+NS)
          CALL QAPRINT('A_TETRA10_ELEMENT_ID', MY_ID,0.0_8)
C
          MY_MID = IPM(1,IXS(1,NUMELS8+NS))
          CALL QAPRINT('A_TETRA10_ELEMENT_MID', MY_MID,0.0_8)
C
          MY_PID = IGEO(1,IXS(10,NUMELS8+NS))
          CALL QAPRINT('A_TETRA10_ELEMENT_PID', MY_PID,0.0_8)
C
          DO J=1,4
            IF(IXS(ITETRA4(J),NUMELS8+NS)/=0)THEN
C
C             VARNAME sert de nom de la varable dans le ref.extract (sans blank character)
C
              MY_NOD = ITAB(IXS(ITETRA4(J),NUMELS8+NS))
              WRITE(VARNAME,'(A,I0)') 'NODE_',J
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
            END IF
          END DO
C
          DO J=1,6
            IF(IXS10(J,NS)/=0)THEN
C
C             VARNAME sert de nom de la varable dans le ref.extract (sans blank character)
C
              MY_NOD = ITAB(IXS10(J,NS))
              WRITE(VARNAME,'(A,I0)') 'NODE_',J+4
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
            END IF
          END DO
C
        END DO ! DO I=1,NUMELS10
C-----------------------------------------------
C       Shell16 elements
C-----------------------------------------------
        DO I=1,NUMELS16
          ITR1(I)=IXS(NIXS,NUMELS8+NUMELS10+NUMELS20+I)
        ENDDO 
        CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMELS16,1)
C
        DO I=1,NUMELS16
          NS=INDEX(I)
C
          MY_ID = IXS(NIXS,NUMELS8+NUMELS10+NUMELS20+NS)
          CALL QAPRINT('A_SHEL16_ELEMENT_ID', MY_ID,0.0_8)
C
          MY_MID = IPM(1,IXS(1,NUMELS8+NUMELS10+NUMELS20+NS))
          CALL QAPRINT('A_SHEL16_ELEMENT_MID', MY_MID,0.0_8)
C
          MY_PID = IGEO(1,IXS(10,NUMELS8+NUMELS10+NUMELS20+NS))
          CALL QAPRINT('A_SHEL16_ELEMENT_PID', MY_PID,0.0_8)
C
          DO J=2,9
            IF(IXS(J,NUMELS8+NUMELS10+NUMELS20+NS)/=0)THEN
C
C             VARNAME sert de nom de la varable dans le ref.extract (sans blank character)
C
              MY_NOD = ITAB(IXS(J,NUMELS8+NUMELS10+NUMELS20+NS))
              WRITE(VARNAME,'(A,I0)') 'NODE_',J
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
            END IF
          END DO
C
          DO J=1,8
            IF(IXS16(J,NS)/=0)THEN
C
C             VARNAME sert de nom de la varable dans le ref.extract (sans blank character)
C
              MY_NOD = ITAB(IXS16(J,NS))
              WRITE(VARNAME,'(A,I0)') 'NODE_',J+8
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
            END IF
          END DO
C
        END DO ! DO I=1,NUMELS16

        DEALLOCATE(INDEX,ITR1)
C
C-----------------------------------------------
C       Spring elements
C-----------------------------------------------
C
        MY_ALLOCATE(INDEX,2*NUMELR)
        MY_ALLOCATE(ITR1,NUMELR)
C
        DO I=1,NUMELR
          ITR1(I)=IXR(NIXR,I)
        ENDDO 
        CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMELR,1)
C
        DO I=1,NUMELR
          NS=INDEX(I)
C
          MY_ID = IXR(NIXR,NS)
          CALL QAPRINT('A_SPRING_ELEMENT_ID', MY_ID,0.0_8)
C
          MY_MID = 0
          IF (IXR(5,NS) > 0) MY_MID = IPM(1,IXR(5,NS))
          CALL QAPRINT('A_SPRING_ELEMENT_MID', MY_MID,0.0_8)
C
          MY_PID = IGEO(1,IXR(1,NS))
          CALL QAPRINT('A_SPRING_ELEMENT_PID', MY_PID,0.0_8)
C
          DO J=2,3
            IF(IXR(J,NS)/=0)THEN
              MY_NOD = ITAB(IXR(J,NS))
              WRITE(VARNAME,'(A,I0)') 'NODE_',J-1
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
            END IF
          END DO
C
          IF(IXR(4,NS)/=0)THEN
            MY_NOD = ITAB(IXR(4,NS))
            WRITE(VARNAME,'(A,I0)') 'NODE_',3
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
C
            TEMP_DOUBLE = X(1,IXR(4,NS))-X(1,IXR(2,NS))
            CALL QAPRINT('--> VEC_N1N3_X', 0,TEMP_DOUBLE)
            TEMP_DOUBLE = X(2,IXR(4,NS))-X(2,IXR(2,NS))
            CALL QAPRINT('--> VEC_N1N3_Y', 0,TEMP_DOUBLE)
            TEMP_DOUBLE = X(3,IXR(4,NS))-X(3,IXR(2,NS))
            CALL QAPRINT('--> VEC_N1N3_Z', 0,TEMP_DOUBLE)
          END IF
C
          IF(R_SKEW(NS)/=0)THEN
            MY_SKEW = ISKWN(4,R_SKEW(NS))
            WRITE(VARNAME,'(A,I0)') 'SKEW_',J-1
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_SKEW,0.0_8)
          END IF
C
        END DO ! DO I=1,NUMELR
C
        DEALLOCATE(INDEX,ITR1)
C
C-----  Additional output of IXR_KJ for kjoints
C
        IF (NUMELR > 0) THEN
C
          NUMEL_KJ = IXR_KJ(1,NUMELR+1)
          MY_ALLOCATE(INDEX,2*NUMEL_KJ)
          MY_ALLOCATE(ITR1,NUMEL_KJ)
C
          DO I=1,NUMEL_KJ
            ITR1(I)=IXR_KJ(4,I)
          ENDDO 
          CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMEL_KJ,1)
C
          DO I=1,NUMEL_KJ
            NS=INDEX(I)
C
            DO J=1,3
              IF(IXR_KJ(J,NS)/=0)THEN
                MY_NOD = ITAB(IXR_KJ(J,NS))
                WRITE(VARNAME,'(A,X,I0,X,A,I0)') 'KJOINT_ID',IXR_KJ(4,NS),'ADDITIONAL_NODE_',3+J
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
              END IF
            END DO
C
          ENDDO
C
          DEALLOCATE(INDEX,ITR1)
C
        ENDIF
C
C-----------------------------------------------
C       Beam elements
C-----------------------------------------------
C
        MY_ALLOCATE(INDEX,2*NUMELP)
        MY_ALLOCATE(ITR1,NUMELP)
C
        DO I=1,NUMELP
          ITR1(I)=IXP(NIXP,I)
        ENDDO 
        CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMELP,1)
C
        DO I=1,NUMELP
          NS=INDEX(I)
C
          MY_ID = IXP(NIXP,NS)
          CALL QAPRINT('A_BEAM_ELEMENT_ID', MY_ID,0.0_8)
C
          MY_MID = IPM(1,IXP(1,NS))
          CALL QAPRINT('A_BEAM_ELEMENT_MID', MY_MID,0.0_8)
C
          MY_PID = IGEO(1,IXP(5,NS))
          CALL QAPRINT('A_BEAM_ELEMENT_PID', MY_PID,0.0_8)
C
          DO J=2,3
            IF(IXP(J,NS)/=0)THEN
              MY_NOD = ITAB(IXP(J,NS))
              WRITE(VARNAME,'(A,I0)') 'NODE_',J-1
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
            END IF
          END DO
C
          IF(IXP(4,NS)/=0)THEN
            MY_NOD = ITAB(IXP(4,NS))
            WRITE(VARNAME,'(A,I0)') 'NODE_',J-1
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
C
            TEMP_DOUBLE = X(1,IXP(4,NS))-X(1,IXP(2,NS))
            CALL QAPRINT('--> VEC_N1N3_X', 0,TEMP_DOUBLE)
            TEMP_DOUBLE = X(2,IXP(4,NS))-X(2,IXP(2,NS))
            CALL QAPRINT('--> VEC_N1N3_Y', 0,TEMP_DOUBLE)
            TEMP_DOUBLE = X(3,IXP(4,NS))-X(3,IXP(2,NS))
            CALL QAPRINT('--> VEC_N1N3_Z', 0,TEMP_DOUBLE)
          END IF
C
        END DO ! DO I=1,NUMELP
C
        DEALLOCATE(INDEX,ITR1)
C
C-----------------------------------------------
C       Truss elements
C-----------------------------------------------
C
        MY_ALLOCATE(INDEX,2*NUMELT)
        MY_ALLOCATE(ITR1,NUMELT)
C
        DO I=1,NUMELT
          ITR1(I)=IXT(NIXT,I)
        ENDDO 
        CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMELT,1)
C
        DO I=1,NUMELT
          NS=INDEX(I)
C
          MY_ID = IXT(NIXT,NS)
          CALL QAPRINT('A_TRUSS_ELEMENT_ID', MY_ID,0.0_8)
C
          MY_MID = IPM(1,IXT(1,NS))
          CALL QAPRINT('A_TRUSS_ELEMENT_MID', MY_MID,0.0_8)
C
          MY_PID = IGEO(1,IXT(4,NS))
          CALL QAPRINT('A_TRUSS_ELEMENT_PID', MY_PID,0.0_8)
C
          DO J=2,3
            IF(IXT(J,NS)/=0)THEN
              MY_NOD = ITAB(IXT(J,NS))
              WRITE(VARNAME,'(A,I0)') 'NODE_',J-1
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
            END IF
          END DO
C
        END DO ! DO I=1,NUMELT
C
        DEALLOCATE(INDEX,ITR1)
C
C-----------------------------------------------
C       SHELL elements
C-----------------------------------------------
        MY_ALLOCATE(INDEX,2*NUMELC)
        MY_ALLOCATE(ITR1,NUMELC)
C
        DO I=1,NUMELC
          ITR1(I)=IXC(NIXC,I)
        ENDDO 

        CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMELC,1)
        DO I=1,NUMELC
          NC=INDEX(I)
C
          MY_ID = IXC(NIXC,NC)
          CALL QAPRINT('A_SHELL_ELEMENT_ID', MY_ID,0.0_8)
C
          MY_MID = IPM(1,IXC(1,NC))
          CALL QAPRINT('A_SHELL_ELEMENT_MID', MY_MID,0.0_8)
C
          MY_PID = IGEO(1,IXC(6,NC))
          CALL QAPRINT('A_SHELL_ELEMENT_PID', MY_PID,0.0_8)
C
          DO J=1,4
              MY_NOD = ITAB(IXC(J+1,NC))
              WRITE(VARNAME,'(A,I0)') 'NODE_',J
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
          END DO
C
          WRITE(VARNAME,'(A,I0)') 'THK_'
          TEMP_DOUBLE = THKE(NC)
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)  
C
          WRITE(VARNAME,'(A,I0)') 'ANGLE_'
          TEMP_DOUBLE = SH4ANG(NC)
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE) 

        ENDDO 
        DEALLOCATE(INDEX,ITR1)
C-----------------------------------------------
C       QUAD elements
C-----------------------------------------------
        MY_ALLOCATE(INDEX,2*NUMELQ)
        MY_ALLOCATE(ITR1,NUMELQ)
C
        DO I=1,NUMELQ
          ITR1(I)=IXQ(NIXQ,I)
        ENDDO 

        CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMELQ,1)
        DO I=1,NUMELQ
          NC=INDEX(I)
C
          MY_ID = IXQ(NIXQ,NC)
          CALL QAPRINT('A_QUAD_ELEMENT_ID', MY_ID,0.0_8)
C
          MY_MID = IPM(1,IXQ(1,NC))
          CALL QAPRINT('A_QUAD_ELEMENT_MID', MY_MID,0.0_8)
C
          MY_PID = IGEO(1,IXQ(6,NC))
          CALL QAPRINT('A_QUAD_ELEMENT_PID', MY_PID,0.0_8)
C
          DO J=1,4
              MY_NOD = ITAB(IXQ(J+1,NC))
              WRITE(VARNAME,'(A,I0)') 'NODE_',J
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
          END DO
C
        ENDDO 
        DEALLOCATE(INDEX,ITR1)
C-----------------------------------------------
C       SH3N elements
C-----------------------------------------------
        MY_ALLOCATE(INDEX,2*NUMELTG)
        MY_ALLOCATE(ITR1,NUMELTG)
C
        DO I=1,NUMELTG
          ITR1(I)=IXTG(NIXTG,I)
        ENDDO 

        CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMELTG,1)
        DO I=1,NUMELTG
          NC=INDEX(I)
C
          MY_ID = IXTG(NIXTG,NC)
          CALL QAPRINT('A_SH3N_ELEMENT_ID', MY_ID,0.0_8)
C
          MY_MID = IPM(1,IXTG(1,NC))
          CALL QAPRINT('A_SH3N_ELEMENT_MID', MY_MID,0.0_8)
C
          MY_PID = IGEO(1,IXTG(5,NC))
          CALL QAPRINT('A_SH3N_ELEMENT_PID', MY_PID,0.0_8)
C
          DO J=1,3
              MY_NOD = ITAB(IXTG(J+1,NC))
              WRITE(VARNAME,'(A,I0)') 'NODE_',J
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_NOD,0.0_8)
          END DO
C
          WRITE(VARNAME,'(A,I0)') 'THK_'
          TEMP_DOUBLE = THKEC(NC)
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)  
C
          WRITE(VARNAME,'(A,I0)') 'ANGLE_'
          TEMP_DOUBLE = SH3ANG(NC)
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE) 

        ENDDO 
        DEALLOCATE(INDEX,ITR1)
C
C-----------------------------------------------
C       Sph elements
C-----------------------------------------------
        MY_ALLOCATE(INDEX,2*NUMSPH)
        MY_ALLOCATE(ITR1,NUMSPH)
C
        DO I=1,NUMSPH
          ITR1(I)=KXSP(NISP,I)
        ENDDO 
        CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMSPH,1)
C
        DO I=1,NUMSPH
C
          NS=INDEX(I)
C
          MY_ID = KXSP(NISP,NS)
          CALL QAPRINT('A_SPH_CELL_ELEMENT_ID', MY_ID,0.0_8)
C
          MY_ID = IPARTSP(NS)
          CALL QAPRINT('A_SPH_CELL_PART_ID', MY_ID,0.0_8)

          MY_MID = IPART(1,IPARTSP(NS))
          CALL QAPRINT('A_SPH_CELL_MID', MY_MID,0.0_8)
C
          MY_PID = IGEO(1,IPART(2,IPARTSP(NS)))
          CALL QAPRINT('A_SPH_CELL_PID', MY_PID,0.0_8)
C
          DO J=2,7
            MY_ID = KXSP(J,NS)
            IF (MY_ID /= 0) THEN
              WRITE(VARNAME,'(A,I0)') 'KXSP_',J
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_ID,0.0_8)
            ENDIF
          ENDDO
C
        ENDDO
        DEALLOCATE(INDEX,ITR1)
C-----------------------------------------------
C       Xelem elements
C-----------------------------------------------
        MY_ALLOCATE(INDEX,2*NUMELX)
        MY_ALLOCATE(ITR1,NUMELX)
C
        DO I=1,NUMELX
          ITR1(I)=KXX(NIXX,I)
        ENDDO 
        CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMELX,1)
C
        DO I=1,NUMELX
C
          NS=INDEX(I)
C
          MY_ID = KXX(NIXX,NS)
          CALL QAPRINT('A_XELEM_ELEMENT_ID', MY_ID,0.0_8)
C
          MY_ID = IPARTX(NS)
          CALL QAPRINT('A_XELEM_PART_ID', MY_ID,0.0_8)
C
          DO J=1,KXX(3,NS)
            MY_ID = ITAB(IXX(KXX(4,NS)+J-1))
            IF (MY_ID /= 0) THEN
              WRITE(VARNAME,'(A,I0)') 'IXX_',KXX(4,NS)+J
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_ID,0.0_8)
            ENDIF
          ENDDO
C
        ENDDO
        DEALLOCATE(INDEX,ITR1)
C-----------------------------------------------
C       Rivet elements
C-----------------------------------------------
        MY_ALLOCATE(INDEX,2*NRIVET)
        MY_ALLOCATE(ITR1,NRIVET)
C
        DO I=1,NRIVET
          ITR1(I)=IXRI(4,I)
        ENDDO 
        CALL MY_ORDERS(0,WORK,ITR1,INDEX,NRIVET,1)
C
        DO I=1,NRIVET
C
          NS=INDEX(I)
C
          MY_ID = IXRI(4,NS)
          CALL QAPRINT('A_RIVET_ELEMENT_ID', MY_ID,0.0_8)
C
          MY_ID = IGEO(1,IXRI(1,NS))
          CALL QAPRINT('A_RIVET_PID', MY_ID,0.0_8)
C
          DO J=1,2
            MY_ID = ITAB(IXRI(J+1,NS))
            IF (MY_ID /= 0) THEN
              WRITE(VARNAME,'(A,I0)') 'IXRI_',J
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_ID,0.0_8)
            ENDIF
          ENDDO
C
        ENDDO
        DEALLOCATE(INDEX,ITR1)
C-----------------------------------------------
      END IF
C-----------------------------------------------
      RETURN
      END
