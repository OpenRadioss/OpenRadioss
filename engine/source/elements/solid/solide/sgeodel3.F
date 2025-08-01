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
C-----------------------------------------------
!||====================================================================
!||    sgeodel3      ../engine/source/elements/solid/solide/sgeodel3.F
!||--- called by ------------------------------------------------------
!||    s10forc3      ../engine/source/elements/solid/solide10/s10forc3.F
!||    s20forc3      ../engine/source/elements/solid/solide20/s20forc3.F
!||    s4forc3       ../engine/source/elements/solid/solide4/s4forc3.F
!||    s8eforc3      ../engine/source/elements/solid/solide8e/s8eforc3.F
!||    s8zforc3      ../engine/source/elements/solid/solide8z/s8zforc3.F
!||    sforc3        ../engine/source/elements/solid/solide/sforc3.F
!||    szforc3       ../engine/source/elements/solid/solidez/szforc3.F
!||--- uses       -----------------------------------------------------
!||    dt_mod        ../engine/source/modules/dt_mod.F
!||    message_mod   ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE SGEODEL3(
     1   NGL,     OFFG,    VOLG,    DELTAX,
     2   VOLG0,   GEO,     L_MAX,   DT,
     3   NEL,     IDEL7NOK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE DT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: IDEL7NOK
      INTEGER ,INTENT(IN)  :: NEL
      INTEGER ,DIMENSION(MVSIZ) ,INTENT(IN)  :: NGL
      my_real, DIMENSION(NPROPG) , INTENT(IN)  :: GEO 
      my_real, DIMENSION(NEL) , INTENT(IN)  :: VOLG0 
      my_real, DIMENSION(MVSIZ) , INTENT(IN)  :: VOLG,DELTAX,L_MAX 
      my_real, DIMENSION(NEL) , INTENT(INOUT)  :: OFFG 
      TYPE(DT_), INTENT(INOUT) :: DT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ICOUNT,LIST(MVSIZ),ITET
      my_real
     .   TET_COLLA(MVSIZ),DEF_V(MVSIZ),APS_R(MVSIZ),
     .   C_MIN,AL,VDEFMIN,VDEFMAX,ASPMAX,COLMIN,C_MAX,GDEL
C---------------------------------------------
      VDEFMIN = MAX(GEO(190),DT%BRICK_DEL_DEFV_MIN)
      VDEFMAX = GEO(191)
      ASPMAX  = GEO(192)
      COLMIN  = MAX(GEO(193),DT%BRICK_DEL_COL_MIN)
C-----
      GDEL=VDEFMIN+VDEFMAX+ASPMAX+COLMIN
      IF(GDEL==ZERO.AND.DT%IDEL_BRICK==0) RETURN
C-----
      IF(DT%BRICK_DEL_DEFV_MAX>ZERO .AND. (DT%BRICK_DEL_DEFV_MAX<VDEFMAX 
     .   .OR.VDEFMAX==ZERO) ) VDEFMAX = DT%BRICK_DEL_DEFV_MAX 
      IF(DT%BRICK_DEL_ASP_MAX>ZERO .AND. ( DT%BRICK_DEL_ASP_MAX<ASPMAX 
     .   .OR.ASPMAX==ZERO) ) ASPMAX = DT%BRICK_DEL_ASP_MAX 
      C_MIN = ONE
      C_MAX = ONE
      IF (L_MAX(1) >ZERO) THEN
        ITET = 0
      ELSE
        ITET = 1
      END IF
C--- tet      
      IF (ITET >0) THEN
       C_MAX = 1.24*SQR3
       C_MIN = ONE/C_MAX
      END IF
C
      IF ((VDEFMIN+VDEFMAX) >ZERO) THEN
        DO I=1,NEL
          IF (OFFG(I)==ZERO) CYCLE
          DEF_V(I) = VOLG(I)/VOLG0(I)
        END DO
      END IF
C      
      IF (VDEFMIN >ZERO) THEN
       ICOUNT = 0
       DO I=1,NEL
        IF (OFFG(I)==ZERO) CYCLE
        IF (DEF_V(I) < VDEFMIN) THEN
         OFFG(I) = ZERO
         ICOUNT=ICOUNT+1
         LIST(ICOUNT)=I
        END IF
       END DO
       IF (ICOUNT > 0) THEN
#include "lockon.inc"
          WRITE(IOUT,1001)
          WRITE(ISTDO,1001)
        DO J=1,ICOUNT
          I=LIST(J)
          WRITE(IOUT,'(6X,I10,1PG20.13)')NGL(I),DEF_V(I)
          WRITE(ISTDO,'(6X,I10,1PG20.13)')NGL(I),DEF_V(I)
        ENDDO
         WRITE(IOUT,*)
         WRITE(ISTDO,*)
#include "lockoff.inc"
         IDEL7NOK = 1
       END IF !(ICOUNT > 0) THEN
      END IF 
C       
      IF (VDEFMAX >ZERO) THEN
       ICOUNT = 0
       DO I=1,NEL
        IF (OFFG(I)==ZERO) CYCLE
        IF (DEF_V(I) > VDEFMAX) THEN
         OFFG(I) = ZERO
         ICOUNT=ICOUNT+1
         LIST(ICOUNT)=I
        END IF
       END DO
        IF (ICOUNT > 0) THEN
#include "lockon.inc"
          WRITE(IOUT,1002)
          WRITE(ISTDO,1002)
         DO J=1,ICOUNT
          I=LIST(J)
          WRITE(IOUT,'(6X,I10,1PG20.13)')NGL(I),DEF_V(I)
          WRITE(ISTDO,'(6X,I10,1PG20.13)')NGL(I),DEF_V(I)
         ENDDO
         WRITE(IOUT,*)
         WRITE(ISTDO,*)
#include "lockoff.inc"
         IDEL7NOK = 1
        END IF !(ICOUNT > 0) THEN
      END IF 
C--- Hexa : APS_r=lmax/lmin lmax~sqrt(Amax); lmin~lc & V~ lc*Amax -> 
C---        APS_r~sqrt(Amax/lc^2)=sqrt(V/lc^3) ; now we compute L_max for Hexa   
        IF ((ASPMAX+COLMIN) >ZERO) THEN
          IF (ITET ==0) THEN
            DO I=1,NEL
              IF (OFFG(I)==ZERO) CYCLE
              APS_R(I) = L_MAX(I)/DELTAX(I)
              TET_COLLA(I) = DELTAX(I) / L_MAX(I)
            END DO
          ELSE
            DO I=1,NEL
              IF (OFFG(I)==ZERO) CYCLE
              AL = SQRT(VOLG(I)/DELTAX(I)**3)
              APS_R(I) = C_MAX*AL
              TET_COLLA(I) = C_MIN / AL
            END DO
          END IF
        END IF
        IF (ASPMAX>ZERO) THEN
          ICOUNT = 0
          DO I=1,NEL
           IF (OFFG(I)==ZERO) CYCLE
           IF (APS_R(I) > ASPMAX) THEN
            OFFG(I) = ZERO
            ICOUNT=ICOUNT+1
            LIST(ICOUNT)=I
           END IF
          END DO
          IF (ICOUNT > 0) THEN
#include "lockon.inc"
              WRITE(IOUT,1003)
              WRITE(ISTDO,1003)
            DO J=1,ICOUNT
              I=LIST(J)
              WRITE(IOUT,'(6X,I10,1PG20.13)')NGL(I),APS_R(I)
              WRITE(ISTDO,'(6X,I10,1PG20.13)')NGL(I),APS_R(I)
            ENDDO
             WRITE(IOUT,*)
             WRITE(ISTDO,*)
#include "lockoff.inc"
            IDEL7NOK = 1
          END IF !(ICOUNT > 0) THEN
        END IF
C
C--- Tet4 : TET_c= h/sqrt(A)/1.24 h~lc; V~ lc*A*2/6 -> 
C---        TET_c~sqrt(h^2/A)/1.24=sqrt(h^3/V)/1.24/sqrt(3)
C---        TET_c =1/ASP_r
        IF (COLMIN>ZERO) THEN
          ICOUNT = 0
          DO I=1,NEL
           IF (OFFG(I)==ZERO) CYCLE
           IF (TET_COLLA(I) < COLMIN) THEN
            OFFG(I) = ZERO
            ICOUNT=ICOUNT+1
            LIST(ICOUNT)=I
           END IF
          END DO
          IF (ICOUNT > 0) THEN
#include "lockon.inc"
             WRITE(IOUT,1004)
             WRITE(ISTDO,1004)
           DO J=1,ICOUNT
             I=LIST(J)
             WRITE(IOUT,'(6X,I10,1PG20.13)')NGL(I),TET_COLLA(I)
             WRITE(ISTDO,'(6X,I10,1PG20.13)')NGL(I),TET_COLLA(I)
           ENDDO
            WRITE(IOUT,*)
            WRITE(ISTDO,*)
#include "lockoff.inc"
            IDEL7NOK = 1
          END IF !(ICOUNT > 0) THEN
        END IF
C
      RETURN
 1001 FORMAT(/
     .    'THE FOLLOWING SOLID ELEMENTS WILL BE DELETED BY MINIMUM VOLUME RATIO' /)
 1002 FORMAT(/
     .    'THE FOLLOWING SOLID ELEMENTS WILL BE DELETED BY MAXIMUM VOLUME RATIO' /)
 1003 FORMAT(/
     .    'THE FOLLOWING SOLID ELEMENTS WILL BE DELETED BY MAXIMUM ASPECT RATIO' /)
 1004 FORMAT(/
     .    'THE FOLLOWING SOLID ELEMENTS WILL BE DELETED BY MINIMUM COLLAPSE RATIO' /)
      END
