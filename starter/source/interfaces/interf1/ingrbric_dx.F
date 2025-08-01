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
!||    ingrbric_dx            ../starter/source/interfaces/interf1/ingrbric_dx.F
!||--- called by ------------------------------------------------------
!||    lecint                 ../starter/source/interfaces/interf1/lecint.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE INGRBRIC_DX(NBRIC    , IBUFSSG, GLOBAL_GAP     , IXS        , X      ,  
     .                       NOINT    , TITR   , IS_GAP_COMPUTED, PM         , IPM    ,
     .                       IDDLEVEL , ISTIFF , AUTO_RHO       , AUTO_LENGTH,
     .                       MULTI_FVM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is computing mesh size from the brick group which is
C related to interface type 18. A gap value is then set consequently.
C
C A check is also introduced about aspect ratio 
C because 
C  - in this case it is not obvious to determine if computed gap is the expected one
C  - it is recommended to use uniform mesh size with colocated scheme : ie law151
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NBRIC, NOINT, IDDLEVEL,ISTIFF
      INTEGER,INTENT(IN) :: IBUFSSG(*), IXS(NIXS,NUMELS), IPM(NPROPMI,NUMMAT)
      my_real,INTENT(INOUT) :: GLOBAL_GAP
      my_real,INTENT(IN) :: X(3,NUMNOD)
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN) :: TITR
      LOGICAL, INTENT(INOUT) :: IS_GAP_COMPUTED
      my_real , INTENT(IN) :: PM(NPROPM,NUMMAT)
      my_real, INTENT(INOUT) :: AUTO_RHO, AUTO_LENGTH
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM      
C-----------------------------------------------
C   L o c a l   V a r a i b l e s
C-----------------------------------------------
      INTEGER :: I, J, J2, IEDG, CONNECT1(12), CONNECT2(12),IE,IMAT, ENUM, ILAW
      my_real :: MIN_X,MIN_Y,MIN_Z
      my_real :: MAX_X,MAX_Y,MAX_Z  
      my_real :: XX,YY,ZZ
      my_real :: XX2,YY2,ZZ2
      my_real :: DX,DY,DZ
      my_real :: DIAG, DIAG_MAX , MAX_RATIO, LEN_EDGE(12), LMAX, LMIN, RATIO2
      my_real :: RHO_MAX, RHO0
      LOGICAL :: CHECK_ASPECT 
      CHARACTER(LEN=NCHARTITLE) :: MSGTITL
      CHARACTER*10 :: CHAR_ID
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
     
      IS_GAP_COMPUTED = .FALSE.
     
      !-----------------------------------------
      ! COMPUTE GLOBAL GAP
      !-----------------------------------------
      IF(GLOBAL_GAP == ZERO)THEN
        !global gap required to estimate a stiffness value (automatic stiffness value when ISTIFF==2)
        !global gap also required when a constant gap is not input IGAP=0
        DIAG_MAX = EM20
        DO I=1,NBRIC
          IE = IBUFSSG(I)
          IF(IE==0)EXIT
          MAX_X = -EP20
          MAX_Y = -EP20
          MAX_Z = -EP20
          MIN_X = EP20
          MIN_Y = EP20
          MIN_Z = EP20 
          DO J=2,9
            IF(IXS(J,IBUFSSG(I))==0)EXIT
            XX = X(1,IXS(J,IE))
            YY = X(2,IXS(J,IE)) 
            ZZ = X(3,IXS(J,IE)) 
            IF(XX < MIN_X)MIN_X=XX
            IF(YY < MIN_Y)MIN_Y=YY
            IF(ZZ < MIN_Z)MIN_Z=ZZ    
            IF(XX > MAX_X)MAX_X=XX
            IF(YY > MAX_Y)MAX_Y=YY
            IF(ZZ > MAX_Z)MAX_Z=ZZ 
          ENDDO
          DX = MIN_X-MAX_X
          DY = MIN_Y-MAX_Y
          DZ = MIN_Z-MAX_Z                    
          DIAG = SQRT(DX*DX+DY*DY+DZ*DZ)  
          DIAG = SQRT(THREE)*DIAG
          DIAG = HALF*DIAG            !   sqrt(3)/2 * DIAG = ~ 1.5*MESH_SIZE
          IF(DIAG > DIAG_MAX)DIAG_MAX=DIAG
        END DO
        GLOBAL_GAP = DIAG_MAX
        AUTO_LENGTH = SQRT(THREE)*THIRD*DIAG_MAX
        IS_GAP_COMPUTED = .TRUE.
       ENDIF
       
      !-----------------------------------------
      ! DETERMINE GLOBAL DENSITY
      !-----------------------------------------
      RHO_MAX = ZERO
      IF(ISTIFF == 2)THEN
        RHO_MAX=ZERO
        DO I=1,NBRIC
          IE=IBUFSSG(I)      
          IF(IE == 0)EXIT
          IMAT=IXS(1,IE)
          RHO0=PM(89,IMAT)
          ILAW=IPM(2,IMAT)
          IF(ILAW == 51 .OR. ILAW == 151)THEN
            RHO_MAX = MAX(RHO_MAX,PM(91,IMAT)) ! use rho_max(1:nsubmat) in case of multi material laws
          ELSE
            RHO_MAX = MAX(RHO_MAX, RHO0) ! monomaterial case
          ENDIF
        END DO
      ENDIF  
      AUTO_RHO = RHO_MAX     
      
      !-----------------------------------------
      ! CHECK ASPECT RATIO
      !-----------------------------------------
      CHECK_ASPECT=.FALSE.
      IF(IDDLEVEL==1 .AND. MULTI_FVM%IS_USED)CHECK_ASPECT=.TRUE.
      IF (CHECK_ASPECT)THEN
        !edge connectivity
        CONNECT1(1:12)=(/1,1,1,2,2,3,3,4,5,5,6,7/)
        CONNECT2(1:12)=(/2,4,5,3,6,4,7,8,6,8,7,8/) 
        ENUM=0
        DO I=1,NBRIC
          DO IEDG=1,12
            J=1+CONNECT1(IEDG)
            J2=1+CONNECT2(IEDG)
            XX  = X(1,IXS(J,IBUFSSG(I)))
            YY  = X(2,IXS(J,IBUFSSG(I))) 
            ZZ  = X(3,IXS(J,IBUFSSG(I))) 
            XX2 = X(1,IXS(J2,IBUFSSG(I)))
            YY2 = X(2,IXS(J2,IBUFSSG(I))) 
            ZZ2 = X(3,IXS(J2,IBUFSSG(I)))
            DX = XX-XX2
            DY = YY-YY2
            DZ = ZZ-ZZ2                    
            LEN_EDGE(IEDG) = DX*DX + DY*DY + DZ*DZ 
          ENDDO
          LMIN=MINVAL(LEN_EDGE)
          LMAX=MAXVAL(LEN_EDGE)
          RATIO2 = LMAX/LMIN ! ratio of squared values
          IF(RATIO2 > 6.25 .AND. ENUM < 10)THEN
            CHAR_ID=' '
            WRITE(CHAR_ID,FMT='(I0)')IXS(11,IBUFSSG(I))
            MSGTITL='CHECK ASPECT RATIO CELL ID ='//CHAR_ID 
            CALL ANCMSG(MSGID=1826, MSGTYPE=MSGWARNING, ANMODE=ANINFO, I1=NOINT, C1=TITR, C2=MSGTITL)
            ENUM=ENUM+1
          ENDIF
          IF(ENUM == 10)EXIT
        ENDDO      
      ENDIF
C------------------------------------------------------------
      RETURN
      END
