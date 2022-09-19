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
Chd|  ST_QAPRINT_INITIAL_CONDITIONS source/output/qaprint/st_qaprint_initial_conditions.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        INIGRAV                       share/modules1/inigrav_mod.F  
Chd|        INIMAP1D_MOD                  share/modules1/inimap1d_mod.F 
Chd|        INIMAP2D_MOD                  share/modules1/inimap2d_mod.F 
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_INITIAL_CONDITIONS(
     1     NOM_OPT   ,INOM_OPT  ,ITAB      ,V         ,VR        ,
     2     W         ,TEMP      ,INICRACK  ,FVM_INIVEL,
     3     INIMAP1D, INIMAP2D)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE INIGRAV
      USE OPTIONDEF_MOD
      USE MULTI_FVM_MOD
      USE INIMAP1D_MOD
      USE INIMAP2D_MOD
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
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NOM_OPT(LNOPT1,SNOM_OPT1), INOM_OPT(SINOM_OPT)
      INTEGER, INTENT(IN) :: ITAB(NUMNOD)
      TYPE (FVM_INIVEL_STRUCT), DIMENSION(NINVEL), INTENT(IN) :: FVM_INIVEL
      TYPE(INIMAP1D_STRUCT), DIMENSION(NINIMAP1D), INTENT(IN) :: INIMAP1D
      TYPE(INIMAP2D_STRUCT), DIMENSION(NINIMAP2D), INTENT(IN) :: INIMAP2D
C-----------------------------------------------
C      NOM_OPT(LNOPT1,SNOM_OPT1) 
C         * Possibly, NOM_OPT(1) = ID
C        NOM_OPT(LNOPT1-LTITL+1:LTITL) <=> TITLES of the OPTIONS
C--------------------------------------------------
C      SNOM_OPT1= NRBODY+NACCELM+NVOLU+NINTER+NINTSUB+
C     +           NRWALL+NJOINT+NSECT+NLINK+
C     +           NUMSKW+1+NUMFRAM+1+NFXBODY+NFLOW+NRBE2+
C     +           NRBE3+NSUBMOD+NFXVEL+NUMBCS+NUMMPC+
C     +           NGJOINT+NUNIT0+NFUNCT+NADMESH+
C     +           NSPHIO+NSPCOND+NRBYKIN+NEBCS+
C     +           NINICRACK+NODMAS+NBGAUGE+NCLUSTER+NINTERFRIC+
C     +           NRBMERGE
C-----------------------------------------------
C      INOM_OPT(SINOM_OPT)
C--------------------------------------------------
C      INOM_OPT(1) = NRBODY
C      INOM_OPT(2) = INOM_OPT(1) + NACCELM
C      INOM_OPT(3) = INOM_OPT(2) + NVOLU
C      INOM_OPT(4) = INOM_OPT(3) + NINTER
C      INOM_OPT(5) = INOM_OPT(4) + NINTSUB
C      INOM_OPT(6) = INOM_OPT(5) + NRWALL
C      INOM_OPT(7) = INOM_OPT(6) 
C      INOM_OPT(8) = INOM_OPT(7) + NJOINT
C      INOM_OPT(9) = INOM_OPT(8) + NSECT
C      INOM_OPT(10)= INOM_OPT(9) + NLINK
C      INOM_OPT(11)= INOM_OPT(10)+ NUMSKW+1+NUMFRAM+1+NSUBMOD
C      INOM_OPT(12)= INOM_OPT(11)+ NFXBODY
C      INOM_OPT(13)= INOM_OPT(12)+ NFLOW
C      INOM_OPT(14)= INOM_OPT(13)+ NRBE2
C      INOM_OPT(15)= INOM_OPT(14)+ NRBE3
C      INOM_OPT(16)= INOM_OPT(15)+ NFXVEL
C      INOM_OPT(17)= INOM_OPT(16)+ NUMBCS
C      INOM_OPT(18)= INOM_OPT(17)+ NUMMPC
C      INOM_OPT(19)= INOM_OPT(18)+ NGJOINT
C      INOM_OPT(20)= INOM_OPT(19)+ NUNIT0
C      INOM_OPT(21)= INOM_OPT(20)+ NFUNCT
C      INOM_OPT(22)= INOM_OPT(21)+ NADMESH
C      INOM_OPT(23)= INOM_OPT(22)+ NSPHIO
C      INOM_OPT(24)= INOM_OPT(23)+ NSPCOND
C      INOM_OPT(25)= INOM_OPT(24)+ NEBCS
C      INOM_OPT(26)= INOM_OPT(25)+ NINICRACK
C      INOM_OPT(27)= INOM_OPT(26)+ NODMAS
C      INOM_OPT(28)= INOM_OPT(27)+ NBGAUGE
C      INOM_OPT(29)= INOM_OPT(28)+ NCLUSTER
C      INOM_OPT(30)= INOM_OPT(29)+ NINTERFRIC
C      INOM_OPT(31)= INOM_OPT(30)+ NRBMERGE
C      .. TO BE MAINTAINED (cf doc/inom_opt.txt) ..
C-----------------------------------------------
      my_real, INTENT(IN) ::
     .                       V(3,NUMNOD), VR(SVR), ! SVR=3*NUMNOD*IRODDL
     .                       W(SW)                 ! SW=3*NUMNOD*IALE
      my_real, INTENT(IN) ::
     .                       TEMP(NUMNOD)
      TYPE (INICRACK_) , DIMENSION(NINICRACK) :: INICRACK
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,JJ,MY_ID,MY_NODE,POSI(NINIGRAV+1),IDS(NINIGRAV),IDX(NINIGRAV),
     .        IDS2(NINICRACK),IDX2(NINICRACK)
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
      LOGICAL :: DO_QA
C-----------------------------------------------
C     INIVEL 
C-----------------------------------------------
      DO_QA = MYQAKEY('VELOCITIES')
      IF (DO_QA) THEN
        DO MY_NODE=1,NUMNOD
C
          MY_ID = ITAB(MY_NODE)
C
          DO I=1,3
            IF(V(I,MY_NODE)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0,A,I0)') 'V_',MY_ID,'_',I ! Specific format for THIS option !
              TEMP_DOUBLE = V(I,MY_NODE)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
          IF(SVR/=0)THEN
            DO I=1,3
              IF(VR(3*(MY_NODE-1)+I)/=ZERO)THEN
C
C               VARNAME: variable name in ref.extract (without blanks)
                WRITE(VARNAME,'(A,I0,A,I0)') 'VR_',MY_ID,'_',I ! Specific format for THIS option !
                TEMP_DOUBLE = VR(3*(MY_NODE-1)+I)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              END IF
            END DO
          END IF
C
        END DO ! MY_NODE=1,NUMNOD
        DO II = 1, NINVEL
           IF (FVM_INIVEL(II)%FLAG) THEN
              WRITE(VARNAME, '(A, I0)') "FVM_INIVEL_", II
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,0.0_8)
              WRITE(VARNAME,'(A)') 'VX_'
              TEMP_DOUBLE = FVM_INIVEL(II)%VX
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              WRITE(VARNAME,'(A)') 'VY_'
              TEMP_DOUBLE = FVM_INIVEL(II)%VY
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              WRITE(VARNAME,'(A)') 'VZ_'
              TEMP_DOUBLE = FVM_INIVEL(II)%VZ
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              WRITE(VARNAME,'(A)') 'GRBRIC_'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),FVM_INIVEL(II)%GRBRICID,0.0_8)
              WRITE(VARNAME,'(A)') 'GRQUAD_'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),FVM_INIVEL(II)%GRQUADID,0.0_8)
              WRITE(VARNAME,'(A)') 'GRTRIA_'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),FVM_INIVEL(II)%GRSH3NID,0.0_8)
           ENDIF
        ENDDO
      END IF
C-----------------------------------------------
      IF (SW /= 0 .AND. MYQAKEY('GRID_VELOCITIES')) THEN
        DO MY_NODE=1,NUMNOD
C
          MY_ID = ITAB(MY_NODE)
C
          DO I=1,3
            IF(W(3*(MY_NODE-1)+I)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0,A,I0)') 'W_',MY_ID,'_',I ! Specific format for THIS option !
              TEMP_DOUBLE = W(3*(MY_NODE-1)+I)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_NODE=1,NUMNOD
      END IF
C-----------------------------------------------
C     INITEMP 
C-----------------------------------------------
      IF (MYQAKEY('/INITEMP')) THEN
        DO MY_NODE=1,NUMNOD
C
          MY_ID = ITAB(MY_NODE)
C
          IF(TEMP(MY_NODE)/=ZERO)THEN
C
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'TEMP_',MY_ID ! Specific format for THIS option !
            TEMP_DOUBLE = TEMP(MY_NODE)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          END IF
C
        END DO ! MY_NODE=1,NUMNOD
      END IF
C-----------------------------------------------
C     INITEMP 
C-----------------------------------------------
      IF (MYQAKEY('/INIGRAV')) THEN
C      
        IF (NINIGRAV > 0) THEN
C          
!         Sort by ID to ensure internal order independant output
          DO I = 1, NINIGRAV
            IDS(I)    = INIGRV(4,I)
            IDX(I)    = I
          ENDDO
          CALL QUICKSORT_I2(IDS, IDX, 1, NINIGRAV)
C         
!         Loop over INIGRAVs
          DO II = 1, NINIGRAV
C
            MY_ID = IDX(II)
            CALL QAPRINT('A_INIGRAV_FAKE_NAME',II,0.0_8)
C
            ! INIGRV table
            DO I = 1,4
              WRITE(VARNAME,'(A,I0)') 'INIGRV_',I
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INIGRV(I,MY_ID),0.0_8)
            ENDDO
C
            ! LINIGRAV table
            DO I = 1,11
              WRITE(VARNAME,'(A,I0)') 'LINIGRAV_',I
              TEMP_DOUBLE = LINIGRAV(I,MY_ID)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            ENDDO
          ENDDO
        ENDIF
      END IF
C-----------------------------------------------
C     INISTA
C-----------------------------------------------
      IF (MYQAKEY('/INISTA')) THEN
C      
        ! Inista file name
        CALL QAPRINT('INISTA_FILE_NAME',0,0.0_8)
        CALL QAPRINT(S0FILE(1:LEN_TRIM(S0FILE)),0,0.0_8)
C
        ! Inista initial balance
        WRITE(VARNAME,'(A)') 'ISIGI_'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISIGI,0.0_8)
C        
        ! Inista output format
        WRITE(VARNAME,'(A)') 'IOUTP_FMT_'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IOUTP_FMT,0.0_8)
C        
        ! Inista file format reading
        WRITE(VARNAME,'(A)') 'IROOTYY_R_'
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IROOTYY_R,0.0_8)
C         
      END IF
C-----------------------------------------------
C     INICRACK
C-----------------------------------------------
      IF (MYQAKEY('/INICRACK')) THEN
C      
        IF (NINICRACK > 0) THEN
C        
!         Sort by ID to ensure internal order independant output
          DO I = 1, NINICRACK
            IDS2(I) = INICRACK(I)%ID
            IDX2(I) = I
          ENDDO
          CALL QUICKSORT_I2(IDS2, IDX2, 1, NINICRACK)
C
!         Loop over INICRACKs
          DO II = 1, NINICRACK
C          
            MY_ID = IDX2(II)
            TITR  = INICRACK(MY_ID)%TITLE
            IF (LEN_TRIM(TITR) /= 0) THEN
              CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),II,0.0_8)
            ELSE
              CALL QAPRINT('A_INICRACK_FAKE_NAME',II,0.0_8)
            END IF
C
            WRITE(VARNAME,'(A,I0,A)') 'INICRACK_',II,'_ID_'
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INICRACK(MY_ID)%ID,0.0_8)
C
            DO I = 1,INICRACK(MY_ID)%NSEG

              WRITE(VARNAME,'(A,I0,A,I0,A)') 'INICRACK_',II,'_SEG_',I,'_NODE1_'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INICRACK(MY_ID)%SEG(I)%NODES(1),0.0_8)
C       
              WRITE(VARNAME,'(A,I0,A,I0,A)') 'INICRACK_',II,'_SEG_',I,'_NODE2_'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INICRACK(MY_ID)%SEG(I)%NODES(2),0.0_8)
C             
              WRITE(VARNAME,'(A,I0,A,I0,A)') 'INICRACK_',II,'_SEG_',I,'_RATIO_'
              TEMP_DOUBLE = INICRACK(MY_ID)%SEG(I)%RATIO
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
C   
            ENDDO
          ENDDO
C        
        ENDIF
C         
      END IF

!     /INIMAP1D and /INIMAP2D options
      DO_QA = MYQAKEY('INIMAP')
      IF (DO_QA) THEN
         IF (NINIMAP1D > 0) THEN
            DO II = 1, NINIMAP1D
               WRITE(VARNAME, '(A)') INIMAP1D(II)%TITLE(1:255)
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP1D(II)%ID, 0.0_8)
               WRITE(VARNAME, '(A)') 'FORMULATION '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP1D(II)%FORMULATION, 0.0_8)
               WRITE(VARNAME, '(A)') 'PROJECTION TYPE '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP1D(II)%PROJ, 0.0_8)
               WRITE(VARNAME, '(A)') 'GRBRIC '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP1D(II)%GRBRICID, 0.0_8)
               WRITE(VARNAME, '(A)') 'GRQUAD '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP1D(II)%GRQUADID, 0.0_8)
               WRITE(VARNAME, '(A)') 'GRTRIA '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP1D(II)%GRSH3NID, 0.0_8)
               WRITE(VARNAME, '(A)') 'NDOE1 '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP1D(II)%NODEID1, 0.0_8)
               WRITE(VARNAME, '(A)') 'NDOE2 '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP1D(II)%NODEID2, 0.0_8)
               WRITE(VARNAME, '(A)') 'FUNC_VEL '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP1D(II)%FUNC_VEL, 0.0_8)
               TEMP_DOUBLE =  INIMAP1D(II)%FAC_VEL
               WRITE(VARNAME, '(A)') 'FAC_VEL '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, TEMP_DOUBLE)
               DO JJ = 1, INIMAP1D(II)%NBMAT
                  WRITE(VARNAME, '(A)') 'FUNC_ALPHA '
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP1D(II)%FUNC_ALPHA(JJ), 0.0_8)
                  WRITE(VARNAME, '(A)') 'FUNC_RHO '
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP1D(II)%FUNC_RHO(JJ), 0.0_8)
                  WRITE(VARNAME, '(A)') 'FUNC_PRES '
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP1D(II)%FUNC_PRES(JJ), 0.0_8)
                  WRITE(VARNAME, '(A)') 'FUNC_ENER '
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP1D(II)%FUNC_ENER(JJ), 0.0_8)
                  TEMP_DOUBLE = INIMAP1D(II)%FAC_RHO(JJ)
                  WRITE(VARNAME, '(A)') 'FAC_RHO '
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, TEMP_DOUBLE)
                  TEMP_DOUBLE = INIMAP1D(II)%FAC_PRES_ENER(JJ)
                  WRITE(VARNAME, '(A)') 'FAC_PRES_ENER '
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, TEMP_DOUBLE)
               ENDDO
            ENDDO
         ENDIF
         IF (NINIMAP2D > 0) THEN
            DO II = 1, NINIMAP2D
               WRITE(VARNAME, '(A)') INIMAP2D(II)%TITLE(1:255)
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP2D(II)%ID, 0.0_8)
               WRITE(VARNAME, '(A)') 'FORMULATION '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP2D(II)%FORMULATION, 0.0_8)
               WRITE(VARNAME, '(A)') 'GRBRIC '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP2D(II)%GRBRICID, 0.0_8)
               WRITE(VARNAME, '(A)') 'GRQUAD '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP2D(II)%GRQUADID, 0.0_8)
               WRITE(VARNAME, '(A)') 'GRTRIA '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP2D(II)%GRSH3NID, 0.0_8)
               WRITE(VARNAME, '(A)') 'NDOE1 '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP2D(II)%NODEID1, 0.0_8)
               WRITE(VARNAME, '(A)') 'NDOE2 '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP2D(II)%NODEID2, 0.0_8)
               WRITE(VARNAME, '(A)') 'NDOE3 '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP2D(II)%NODEID3, 0.0_8)
               WRITE(VARNAME, '(A)') 'FUNC_VEL '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP2D(II)%FUNC_VEL, 0.0_8)
               TEMP_DOUBLE =  INIMAP2D(II)%FAC_VEL
               WRITE(VARNAME, '(A)') 'FAC_VEL '
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, TEMP_DOUBLE)
               DO JJ = 1, INIMAP2D(II)%NBMAT
                  WRITE(VARNAME, '(A)') 'FUNC_ALPHA '
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP2D(II)%FUNC_ALPHA(JJ), 0.0_8)
                  WRITE(VARNAME, '(A)') 'FUNC_RHO '
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP2D(II)%FUNC_RHO(JJ), 0.0_8)
                  WRITE(VARNAME, '(A)') 'FUNC_PRES '
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP2D(II)%FUNC_PRES(JJ), 0.0_8)
                  WRITE(VARNAME, '(A)') 'FUNC_ENER '
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), INIMAP2D(II)%FUNC_ENER(JJ), 0.0_8)
                  TEMP_DOUBLE = INIMAP2D(II)%FAC_RHO(JJ)
                  WRITE(VARNAME, '(A)') 'FAC_RHO '
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, TEMP_DOUBLE)
                  TEMP_DOUBLE = INIMAP2D(II)%FAC_PRES_ENER(JJ)
                  WRITE(VARNAME, '(A)') 'FAC_PRES_ENER '
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, TEMP_DOUBLE)
               ENDDO
            ENDDO
         ENDIF
      ENDIF
C-----------------------------------------------

C-----------------------------------------------
      RETURN
      END
