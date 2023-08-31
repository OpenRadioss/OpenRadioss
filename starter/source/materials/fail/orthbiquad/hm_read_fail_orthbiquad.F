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
Chd|  HM_READ_FAIL_ORTHBIQUAD       source/materials/fail/orthbiquad/hm_read_fail_orthbiquad.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
       SUBROUTINE HM_READ_FAIL_ORTHBIQUAD(
     .            UPARAM  ,MAXUPARAM ,NUPARAM ,NUVAR   ,IFUNC    ,
     .            MAXFUNC ,NFUNC     ,UNITAB  ,FAIL_ID ,MAT_ID   ,
     .            TITR    ,LSUBMODEL )
C-----------------------------------------------
c    Orthotropic strain  failure model
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE ORTHBIQUAD
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     UNITAB          UNITS ARRAY
C     FAIL_ID         FAILURE ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE   
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C UPARAM   | NUPARAM | F | W | USER FAILURE MODEL PARAMETER ARRAY
C MAXUPARAM|  1      | I | R | MAXIMUM SIZE OF UPARAM 
C NUPARAM  |  1      | I | W | SIZE OF UPARAM =< MAXUPARAM
C NUVAR    |  1      | I | W | NUMBER OF USER  VARIABLES
C----------+---------+---+---+--------------------------------------------
C IFUNC    | NFUNC   | I | W | FUNCTION NUMBER ARRAY
C MAXFUNC  |  1      | I | R | MAXIMUM SIZE OF IFUNC
C NFUNC    |  1      | I | W | SIZE OF IFUNC =< MAXFUNC
C----------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN)   :: UNITAB 
      INTEGER,INTENT(IN)             :: MAXUPARAM,MAXFUNC,FAIL_ID,MAT_ID
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)          :: IFUNC(MAXFUNC),NFUNC
      my_real,INTENT(INOUT)          :: UPARAM(MAXUPARAM)
      INTEGER,INTENT(INOUT)            :: NUPARAM,NUVAR
      CHARACTER TITR*nchartitle
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NANGLE,I,J,K,INFO,REG_FUNC,MFLAG,SFLAG,RATE_FUNC,IPIV2(2),IPIV3(3)
      my_real 
     .    PTHK,REF_SIZ,REF_SIZ_UNIT,EPSD0,CJC,RATE_SCALE,REF_RATE_UNIT,
     .    R1,R2,R4,R5,C5,C5_MIN,THETA_MYREAL
      my_real, DIMENSION(:), ALLOCATABLE :: C1,C2,C3,C4,INST
      DOUBLE PRECISION A_1(2,2),B_1(2),A_2(3,3),B_2(3),
     .                 TRIAX_1_LIN,TRIAX_2_LIN,TRIAX_3_LIN,
     .                 TRIAX_4_LIN,TRIAX_5_LIN,TRIAX_1_QUAD,
     .                 TRIAX_2_QUAD,TRIAX_3_QUAD,TRIAX_4_QUAD,
     .                 TRIAX_5_QUAD,COS2(10,10),XMIN,YMIN
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: THETA,THETA_RAD,Q_X11,Q_X12,Q_X13,
     .                                               Q_X21,Q_X22,Q_X23,Q_INST
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: X_1,X_2,AMAT,BVEC
      INTEGER, DIMENSION(:), ALLOCATABLE :: IPIV
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
      DATA TRIAX_1_LIN, TRIAX_2_LIN, TRIAX_3_LIN, TRIAX_4_LIN,
     .     TRIAX_5_LIN 
     .      / -0.33333333, 0.0, 0.33333333, 0.577350269, 0.66666667 /
      DATA TRIAX_1_QUAD, TRIAX_2_QUAD, TRIAX_3_QUAD, 
     .     TRIAX_4_QUAD, TRIAX_5_QUAD 
     .      / 0.11111111, 0.0, 0.11111111, 0.33333333, 0.44444444 /
C
      DATA  COS2/
     1 1.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,   
     2 0.   ,1.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,     
     3 -1.  ,0.   ,2.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,     
     4 0.   ,-3.  ,0.   ,4.   ,0.   ,0.   ,0.   ,0.   ,0.   ,0.   ,     
     5 1.   ,0.   ,-8.  ,0.   ,8.   ,0.   ,0.   ,0.   ,0.   ,0.   ,     
     6 0.   ,5.   ,0.   ,-20. ,0.   ,16.  ,0.   ,0.   ,0.   ,0.   ,     
     7 -1.  ,0.   ,18.  ,0.   ,-48. ,0.   ,32.  ,0.   ,0.   ,0.   ,     
     8 0.   ,-7.  ,0.   ,56.  ,0.   ,-112.,0.   ,64.  ,0.   ,0.   ,     
     9 1.   ,0.   ,-32. ,0.   ,160. ,0.   ,-256.,0.   ,128. ,0.   ,     
     A 0.   ,9.   ,0.   ,-120.,0.   ,432. ,0.   ,-576 ,0.   ,256. /
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C======================================================================================
C EXTRACT DATAS (REAL VALUES)
C======================================================================================
!---------------
! -> Card1
!---------------
! Percentage of integration failure, flags and size regularization
      CALL HM_GET_FLOATV ('Pthk'           ,PTHK   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('MAT_MFLAG'      ,MFLAG  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('MAT_SFLAG'      ,SFLAG  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('MAT_refanglemax',NANGLE ,IS_AVAILABLE,LSUBMODEL)
      ! Error message
      IF (NANGLE > 10) THEN 
        CALL ANCMSG(MSGID=2015,MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)      
      ENDIF 
      CALL HM_GET_INTV   ('fct_IDel' ,REG_FUNC    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('EI_ref'   ,REF_SIZ     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
! Default values      
      IF (PTHK == ZERO) PTHK = ONE - EM06
      PTHK = MIN(PTHK, ONE)
      PTHK = MAX(PTHK,-ONE)
      IF (SFLAG == 0)    SFLAG = 2
! Units
      IF ((REF_SIZ == ZERO).AND.(REG_FUNC > 0)) THEN
        CALL HM_GET_FLOATV_DIM('EI_ref' ,REF_SIZ_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
        REF_SIZ = ONE*REF_SIZ_UNIT
      ENDIF
!---------------
! -> Card2
!---------------
! Size regularization and percentage of integration failure
      CALL HM_GET_FLOATV ('MAT_C5'    ,C5          ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('MAT_EPSD0' ,EPSD0       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('MAT_CJC'   ,CJC         ,IS_AVAILABLE,LSUBMODEL,UNITAB) 
      CALL HM_GET_INTV   ('fct_IDrate',RATE_FUNC   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('RATE_scale',RATE_SCALE  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
! Units and default values
      IF ((RATE_SCALE == ZERO).AND.(RATE_FUNC > 0)) THEN 
        CALL HM_GET_FLOATV_DIM('RATE_scale' ,REF_RATE_UNIT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        RATE_SCALE = REF_RATE_UNIT*ONE
      ENDIF
      IF (RATE_FUNC > 0) THEN 
        CJC   = ZERO
        EPSD0 = ZERO
      ELSE
        RATE_SCALE = ZERO
      ENDIF
      IF (CJC == ZERO .OR. EPSD0 == ZERO) THEN 
        CJC   = ZERO
        EPSD0 = ZERO
      ENDIF
!---------------
! -> Card3
!---------------
! Biquad parameter by angle
      IF (.NOT.ALLOCATED(C1))   ALLOCATE(C1(NANGLE))
      IF (.NOT.ALLOCATED(C2))   ALLOCATE(C2(NANGLE))
      IF (.NOT.ALLOCATED(C3))   ALLOCATE(C3(NANGLE)) 
      IF (.NOT.ALLOCATED(C4))   ALLOCATE(C4(NANGLE))
      IF (SFLAG == 3) THEN 
        IF (.NOT.ALLOCATED(INST)) ALLOCATE(INST(NANGLE))
        INST = ZERO
      ENDIF
      C5_MIN = INFINITY
      ! Material selector
      IF (MFLAG == 0) THEN 
        ! Loop over angles (must be equally distributed between 0 and pi/2)
        DO J = 1, NANGLE
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C1',C1(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C2',C2(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C3',C3(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C4',C4(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
          ! Default values
          IF (C3(J) == ZERO) C3(J) = 0.6D0
          IF (C1(J) == ZERO .AND. C2(J) == ZERO .AND. C4(J) == ZERO .AND. C5 == ZERO) THEN
            C1(J)  = 3.5D0*C3(J)
            C2(J)  = 1.6D0*C3(J)
            C4(J)  = 0.6D0*C3(J)
            C5_MIN = MIN(C5_MIN,1.5D0*C3(J))
          ENDIF
          ! If necking instability is activated
          IF (SFLAG == 3) THEN
            CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_INST',INST(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
            IF (INST(J) <= ZERO)   THEN
              CALL ANCMSG(MSGID=2016,MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)    
              SFLAG = 2
            ELSEIF (INST(J) >= C4(J)) THEN
              CALL ANCMSG(MSGID=2017,MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)
              SFLAG = 2
            ENDIF           
          ENDIF
        ENDDO
      ELSE
        ! RX parameters
        IF (MFLAG == 1) THEN ! Mild Seel (c3 = 0.6)
          R1 = 3.5D0 
          R2 = 1.6D0 
          R4 = 0.6D0 
          R5 = 1.5D0
        ELSEIF (MFLAG == 2) THEN ! DP600 (c3 = 0.5)
          R1 = 4.3D0 
          R2 = 1.4D0
          R4 = 0.6D0
          R5 = 1.6D0
        ELSEIF (MFLAG == 3) THEN ! Boron (c3 = 0.12)
          R1 = 5.2D0 
          R2 = 3.1D0 
          R4 = 0.8D0 
          R5 = 3.5D0 
        ELSEIF (MFLAG == 4) THEN ! Aluminium AA5182 (c3 = 0.3)
          R1 = 5.0D0
          R2 = 1.0D0 
          R4 = 0.4D0
          R5 = 0.8D0
        ELSEIF (MFLAG == 5) THEN ! Aluminium AA6082-T6 (c3 = 0.17)
          R1 = 7.8D0
          R2 = 3.5D0   
          R4 = 0.6D0
          R5 = 2.8D0
        ELSEIF (MFLAG == 6) THEN ! Plastic light_eBody PA6GF30 (c3 = 0.1)
          R1 = 3.6D0
          R2 = 0.6D0
          R4 = 0.5D0
          R5 = 0.6D0
        ELSEIF (MFLAG == 7) THEN ! Plastic light_eBody PP T40 ( c3=0.11 )
          R1 = 10.0D0
          R2 = 2.7D0
          R4 = 0.6D0
          R5 = 0.7D0
        ELSEIF (MFLAG == 99) THEN ! user scalling factors
          CALL HM_GET_FLOATV ('MAT_R1' ,R1    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV ('MAT_R2' ,R2    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV ('MAT_R4' ,R4    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV ('MAT_R5' ,R5    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ELSE ! ELSE --> Mild Seel
          R1 = 3.5D0
          R2 = 1.6D0
          R4 = 0.6D0
          R5 = 1.5D0
        ENDIF   
        ! Loop over angles (must be equally distributed between 0 and pi/2)
        DO J = 1, NANGLE
          CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_C3'  ,C3(J)  ,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
          ! Default values 
          IF (C3(J) == ZERO) THEN 
            IF (MFLAG == 1) THEN 
              C3(J) = 0.6D0
            ELSEIF (MFLAG == 2) THEN 
              C3(J) = 0.5D0
            ELSEIF (MFLAG == 3) THEN
              C3(J) = 0.12D0
            ELSEIF (MFLAG == 4) THEN
              C3(J) = 0.3D0
            ELSEIF (MFLAG == 5) THEN
              C3(J) = 0.17D0
            ELSEIF (MFLAG == 6) THEN
              C3(J) = 0.1D0
            ELSEIF (MFLAG == 7) THEN
              C3(J) = 0.11D0
            ENDIF
          ENDIF
          ! Computation of C1,C2,C4,C5
          C1(J)  = R1*C3(J)
          C2(J)  = R2*C3(J)
          C4(J)  = R4*C3(J)
          C5_MIN = MIN(C5_MIN,R5*C3(J))
          ! If necking instability is activated
          IF (SFLAG == 3) THEN
            CALL HM_GET_FLOAT_ARRAY_INDEX('MAT_INST',INST(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
            IF (INST(J) <= ZERO)   THEN
              CALL ANCMSG(MSGID=2016,MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)    
              SFLAG = 2
            ELSEIF (INST(J) >= C4(J)) THEN
              CALL ANCMSG(MSGID=2017,MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)
              SFLAG = 2
            ENDIF           
          ENDIF
        ENDDO
      ENDIF
      ! Default value
      IF (C5 == ZERO) C5 = C5_MIN
C======================================================================================
C    COMPUTING FITTING PARAMETERS
C======================================================================================
      IF (.NOT.ALLOCATED(X_1))  ALLOCATE(X_1(NANGLE,3))
      IF (.NOT.ALLOCATED(X_2))  ALLOCATE(X_2(NANGLE,3))
      !================================================
      ! Loop over the angle
      !================================================
      DO J = 1,NANGLE
C
        ! Coefficient for the first parable
        ! ---------------------------------------
        A_1(1,1) = TRIAX_1_LIN
        A_1(1,2) = TRIAX_1_QUAD
        A_1(2,1) = TRIAX_3_LIN
        A_1(2,2) = TRIAX_3_QUAD
        B_1(1)   = C1(J) - C2(J)
        B_1(2)   = C3(J) - C2(J)
C
        ! Fitting the first quadratic function
#ifndef WITHOUT_LINALG
        CALL DGESV(2, 1, A_1, 2, IPIV2, B_1, 2, INFO)  
#else
        WRITE(6,*) "Error: Blas/Lapack required"
#endif
        X_1(J,1)   = C2(J)
        X_1(J,2:3) = B_1(1:2)
C      
        ! Coefficient for the second parable
        !----------------------------------------
        A_2(1,1) = ONE
        A_2(1,2) = TRIAX_3_LIN
        A_2(1,3) = TRIAX_3_QUAD
        A_2(2,1) = ONE
        A_2(2,2) = TRIAX_4_LIN
        A_2(2,3) = TRIAX_4_QUAD
        A_2(3,1) = ONE
        A_2(3,2) = TRIAX_5_LIN
        A_2(3,3) = TRIAX_5_QUAD
        B_2(1)   = C3(J)
        B_2(2)   = C4(J)
        B_2(3)   = C5
C
        ! Fitting the second quadratic function
#ifndef WITHOUT_LINALG
        CALL DGESV(3, 1, A_2, 3, IPIV3, B_2, 3, INFO)
#endif
        X_2(J,1:3) = B_2(1:3)
C
      ENDDO
C
C======================================================================================
C    COMPUTING COSINE INTERPOLATION
C======================================================================================
c      
      IF (.NOT.ALLOCATED(THETA))     ALLOCATE(THETA(NANGLE))
      IF (.NOT.ALLOCATED(THETA_RAD)) ALLOCATE(THETA_RAD(NANGLE))
c
      ! Computation of angles and check curves
      DO J = 1, NANGLE
        IF (NANGLE > 1) THEN 
          THETA(J)     = (J-1)*(90.0D0/(NANGLE-1))
          THETA_RAD(J) = THETA(J)*(PI/180.0D0)
        ELSE
          THETA(J)     = ZERO
          THETA_RAD(J) = ZERO
        ENDIF
c
        ! Check if minimum of first parabola is negative 
        XMIN = -X_1(J,2)/(TWO*X_1(J,3))
        YMIN = X_1(J,3)*(XMIN**2) + X_1(J,2)*XMIN + X_1(J,1)
        IF (YMIN < ZERO) THEN 
          THETA_MYREAL = THETA(J)
          CALL ANCMSG(MSGID=3002,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=J,
     .                R1=THETA_MYREAL)          
        ENDIF
c
        ! Check if minimum of second parabola is negative 
        IF (SFLAG == 1) THEN 
          XMIN = -X_2(J,2)/(TWO*X_2(J,3))
          YMIN = X_2(J,3)*(XMIN**2) + X_2(J,2)*XMIN + X_2(J,1)
          IF (YMIN < ZERO) THEN 
            THETA_MYREAL = THETA(J)
            CALL ANCMSG(MSGID=3003,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  I2=J,
     .                  R1=THETA_MYREAL)              
          ENDIF
        ENDIF
c
      ENDDO
c
      ! Allocation of the A-MATRIX and the Pivot vector
      ALLOCATE (AMAT(NANGLE,NANGLE),IPIV(NANGLE))
c
      ! Filling the A-MATRIX
      DO J = 1,NANGLE
        DO I = 1,NANGLE
          AMAT(J,I) = ZERO
          DO K = 1,I
            AMAT(J,I) = AMAT(J,I) + COS2(K,I)*(COS(TWO*THETA_RAD(J)))**(K-1)
          ENDDO
        ENDDO
      ENDDO
c
      ! Allocation of factors
      ALLOCATE(Q_X11(NANGLE),Q_X12(NANGLE),Q_X13(NANGLE),
     .         Q_X21(NANGLE),Q_X22(NANGLE),Q_X23(NANGLE))
c
      ! Initialization of tables
      Q_X11(1:NANGLE) = ZERO
      Q_X12(1:NANGLE) = ZERO
      Q_X13(1:NANGLE) = ZERO
      Q_X21(1:NANGLE) = ZERO
      Q_X22(1:NANGLE) = ZERO
      Q_X23(1:NANGLE) = ZERO
      IF (SFLAG == 3) THEN 
        ALLOCATE(Q_INST(NANGLE))
        Q_INST(1:NANGLE) = ZERO
      ENDIF
c
      ! Filling the B vector with experimental points
      IF (SFLAG == 3) THEN 
        ALLOCATE (BVEC(NANGLE,7))
      ELSE
        ALLOCATE (BVEC(NANGLE,6))
      ENDIF
      BVEC(1:NANGLE,1) = X_1(1:NANGLE,1)
      BVEC(1:NANGLE,2) = X_1(1:NANGLE,2)
      BVEC(1:NANGLE,3) = X_1(1:NANGLE,3)
      BVEC(1:NANGLE,4) = X_2(1:NANGLE,1)
      BVEC(1:NANGLE,5) = X_2(1:NANGLE,2)
      BVEC(1:NANGLE,6) = X_2(1:NANGLE,3)
      IF (SFLAG == 3) BVEC(1:NANGLE,7) = INST(1:NANGLE)
c        
      ! Initialization of the Pivot vector
      IPIV(1:NANGLE) = 0
c
      ! Solving the A-MATRIX * x = B vector system
#ifndef WITHOUT_LINALG
      IF (SFLAG == 3) THEN 
        CALL DGESV(NANGLE, 7, AMAT, NANGLE, IPIV, BVEC, NANGLE, INFO)
      ELSE
        CALL DGESV(NANGLE, 6, AMAT, NANGLE, IPIV, BVEC, NANGLE, INFO)
      ENDIF
#else
      WRITE(6,*) "Error: Blas/Lapack required"
#endif
c        
      ! Recovering the solution
      Q_X11(1:NANGLE)  = BVEC(1:NANGLE,1)
      Q_X12(1:NANGLE)  = BVEC(1:NANGLE,2)
      Q_X13(1:NANGLE)  = BVEC(1:NANGLE,3)
      Q_X21(1:NANGLE)  = BVEC(1:NANGLE,4)
      Q_X22(1:NANGLE)  = BVEC(1:NANGLE,5)
      Q_X23(1:NANGLE)  = BVEC(1:NANGLE,6)
      IF (SFLAG == 3) Q_INST(1:NANGLE) = BVEC(1:NANGLE,7)
c
C======================================================================================
c     Filling buffer tables
C======================================================================================
      ! -> Number of parameters
      NUPARAM = 7
      IF (SFLAG == 3) THEN 
        NUPARAM = NUPARAM + 7*NANGLE
      ELSE
        NUPARAM = NUPARAM + 6*NANGLE
      ENDIF
      ! -> Number of functions
      NFUNC   = 0
      IF (RATE_FUNC /= 0) THEN 
        NFUNC = NFUNC + 1
        IFUNC(NFUNC) = RATE_FUNC
      ENDIF
      IF (REG_FUNC /= 0) THEN 
        NFUNC = NFUNC + 1 
        IFUNC(NFUNC) = REG_FUNC
      ENDIF
      ! -> Number of user variables
      NUVAR = 3   
c-----------------------------------------------------    
      UPARAM(1) = PTHK
      UPARAM(2) = SFLAG
      UPARAM(3) = REF_SIZ
      UPARAM(4) = EPSD0
      UPARAM(5) = CJC
      UPARAM(6) = RATE_SCALE
      UPARAM(7) = NANGLE
      IF (SFLAG == 3) THEN 
        DO J = 1,NANGLE
          UPARAM(8  + 7*(J-1)) = Q_X11(J)
          UPARAM(9  + 7*(J-1)) = Q_X12(J)
          UPARAM(10 + 7*(J-1)) = Q_X13(J)
          UPARAM(11 + 7*(J-1)) = Q_X21(J)
          UPARAM(12 + 7*(J-1)) = Q_X22(J)
          UPARAM(13 + 7*(J-1)) = Q_X23(J)
          UPARAM(14 + 7*(J-1)) = Q_INST(J)
        ENDDO    
      ELSE
        DO J = 1,NANGLE
          UPARAM(8  + 6*(J-1)) = Q_X11(J)
          UPARAM(9  + 6*(J-1)) = Q_X12(J)
          UPARAM(10 + 6*(J-1)) = Q_X13(J)
          UPARAM(11 + 6*(J-1)) = Q_X21(J)
          UPARAM(12 + 6*(J-1)) = Q_X22(J)
          UPARAM(13 + 6*(J-1)) = Q_X23(J)
        ENDDO          
      ENDIF
c-----------------------------------------------------
c--------------------------
c     Printout data
c-------------------------- 
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1000)
        IF (MFLAG /= 0) WRITE(IOUT, 1100) MFLAG
        DO J = 1,NANGLE
          WRITE(IOUT,1200) THETA(J),C1(J),C2(J),C3(J),C4(J),C5,
     &    X_1(J,3),X_1(J,2),X_1(J,1),X_2(J,3),X_2(J,2),X_2(J,1)           
          IF (SFLAG == 3) WRITE(IOUT, 1900) INST(J)
        ENDDO
        WRITE(IOUT,1300) PTHK,SFLAG
        IF (REG_FUNC > 0) WRITE(IOUT,1400) REG_FUNC,REF_SIZ
        IF (EPSD0 > ZERO) THEN 
          WRITE(IOUT,1500) EPSD0,CJC
        ELSEIF (RATE_FUNC > 0) THEN 
          WRITE(IOUT,1600) RATE_FUNC,RATE_SCALE
        ENDIF
        WRITE(IOUT,2000)
      ENDIF
c--------------------------
c     Deallocation
c--------------------------  
      IF (ALLOCATED(C1))        DEALLOCATE(C1)
      IF (ALLOCATED(C2))        DEALLOCATE(C2)
      IF (ALLOCATED(C3))        DEALLOCATE(C3)
      IF (ALLOCATED(C4))        DEALLOCATE(C4)
      IF (ALLOCATED(INST))      DEALLOCATE(INST)
      IF (ALLOCATED(X_1))       DEALLOCATE(X_1)
      IF (ALLOCATED(X_2))       DEALLOCATE(X_2)
      IF (ALLOCATED(THETA))     DEALLOCATE(THETA)
      IF (ALLOCATED(THETA_RAD)) DEALLOCATE(THETA_RAD)
      IF (ALLOCATED(Q_X11))     DEALLOCATE(Q_X11)
      IF (ALLOCATED(Q_X12))     DEALLOCATE(Q_X12)
      IF (ALLOCATED(Q_X13))     DEALLOCATE(Q_X13)
      IF (ALLOCATED(Q_X21))     DEALLOCATE(Q_X21)
      IF (ALLOCATED(Q_X22))     DEALLOCATE(Q_X22)
      IF (ALLOCATED(Q_X23))     DEALLOCATE(Q_X23)
      IF (ALLOCATED(Q_INST))    DEALLOCATE(Q_INST)
      IF (ALLOCATED(AMAT))      DEALLOCATE(AMAT)
      IF (ALLOCATED(IPIV))      DEALLOCATE(IPIV)
c-----------------------------------------------------
 1000 FORMAT(
     & 5X,'  ------------------------------------------   ',/
     & 5X,'    FAILURE CRITERION : ORTHOTROPIC BIQUAD     ',/,
     & 5X,'  ------------------------------------------   ',/)
 1100 FORMAT(
     & 5X,'MATERIAL PARAMETER SELECTOR  . . . . . . . . . . .=',I10)
 1200 FORMAT(     
     & 5X,'|| FAILURE STRAINS FOR ANGLE',F5.1,' DEG',/,
     & 5X,'  -------------------------------------------------',/,
     & 5X,'  SIMPLE COMPRESSION C1  . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'  SHEAR C2 . . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'  SIMPLE TENSION C3  . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'  PLANE STRAIN C4  . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'  BIAXIAL TENSION C5 . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'                                                   ',/
     & 5X,'  LOW STRESS TRIAXIALITY PARABOLA PARAMETER A. . .=',1PG20.13/ 
     & 5X,'  LOW STRESS TRIAXIALITY PARABOLA PARAMETER B. . .=',1PG20.13/ 
     & 5X,'  LOW STRESS TRIAXIALITY PARABOLA PARAMETER C. . .=',1PG20.13/ 
     & 5X,'                                                   ',/
     & 5X,'  HIGH STRESS TRIAXIALITY PARABOLA PARAMETER D . .=',1PG20.13/ 
     & 5X,'  HIGH STRESS TRIAXIALITY PARABOLA PARAMETER E . .=',1PG20.13/ 
     & 5X,'  HIGH STRESS TRIAXIALITY PARABOLA PARAMETER F . .=',1PG20.13/)
 1300 FORMAT(
     & 5X,'ELEMENT DELETION :',/,
     & 5X,'SHELL ELEMENT DELETION PARAMETER PTHICKFAIL. . . .=',1PG20.13,/,
     & 5X,'  > 0.0 : FRACTION OF FAILED THICKNESS             ',/,
     & 5X,'  < 0.0 : FRACTION OF FAILED INTG. POINTS OR LAYERS',/,
     & 5X,'S-FLAG . . . . . . . . . . . . . . . . . . . . . .=',I10,/)
 1400 FORMAT(
     & 5X,'ELEMENT LENGTH REGULARIZATION USED:',/,
     & 5X,'REGULARIZATION FUNCTION ID . . . . . . . . . . . .=',I10,/,
     & 5X,'REFERENCE ELEMENT LENGTH . . . . . . . . . . . . .=',1PG20.13,/)
 1500 FORMAT(
     & 5X,'JOHNSON-COOK STRAIN-RATE DEPENDENCY:',/,
     & 5X,'REFERENCE STRAIN-RATE  . . . . . . . . . . . . . .=',1PG20.13,/,
     & 5X,'JOHNSON-COOK PARAMETER . . . . . . . . . . . . . .=',1PG20.13,/)
 1600 FORMAT(
     & 5X,'TABULATED STRAIN-RATE DEPENDENCY:',/,
     & 5X,'STRAIN-RATE DEPENDENCY FUNCTION ID . . . . . . . .=',I10,/,
     & 5X,'STRAIN-RATE SCALE FACTOR . . . . . . . . . . . . .=',1PG20.13,/)
 1900 FORMAT(
     & 5X,'  INSTABILITY STRAIN . . . . . . . . . . . . . . .=',1PG20.13,//)
 2000 FORMAT(
     & 5X,' --------------------------------------------------',//)
c----------- 
      END
