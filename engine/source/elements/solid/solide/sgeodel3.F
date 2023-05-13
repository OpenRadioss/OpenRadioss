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
C-----------------------------------------------
Chd|====================================================================
Chd|  SGEODEL3                      source/elements/solid/solide/sgeodel3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        DT_MOD                        share/modules/dt_mod.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SGEODEL3(NGL, OFFG ,VOLG ,DELTAX, VOLG0 ,GEO ,ITET,DT,NEL)
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
      INTEGER ,INTENT(IN)  :: ITET,NEL
      INTEGER ,DIMENSION(MVSIZ) ,INTENT(IN)  :: NGL
      my_real, DIMENSION(NPROPG) , INTENT(IN)  :: GEO 
      my_real, DIMENSION(NEL) , INTENT(IN)  :: VOLG0 
      my_real, DIMENSION(MVSIZ) , INTENT(IN)  :: VOLG,DELTAX 
      my_real, DIMENSION(NEL) , INTENT(INOUT)  :: OFFG 
      TYPE(DT_), INTENT(INOUT) :: DT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ICOUNT,LIST(MVSIZ)
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
      IF(VDEFMAX==ZERO .OR. DT%BRICK_DEL_DEFV_MAX<VDEFMAX) 
     .                   VDEFMAX = DT%BRICK_DEL_DEFV_MAX 
      IF(ASPMAX==ZERO .OR. DT%BRICK_DEL_ASP_MAX<ASPMAX)
     .                   ASPMAX = DT%BRICK_DEL_ASP_MAX 
      C_MIN = ONE
      C_MAX = ONE
C--- tet      
      IF (ITET >0) THEN
       C_MAX = 1.24*SQR3
       C_MIN = ONE/C_MAX
      END IF
C
      IF (VDEFMIN >ZERO) THEN
       ICOUNT = 0
       DO I=1,NEL
        IF (OFFG(I)==ZERO) CYCLE
        DEF_V(I) = VOLG(I)/VOLG0(I)
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
        END IF !(ICOUNT > 0) THEN
      END IF 
C--- Hexa : APS_r=lmax/lmin lmax~sqrt(Amax); lmin~lc & V~ lc*Amax -> 
C---        APS_r~sqrt(Amax/lc^2)=sqrt(V/lc^3)     
        IF (ASPMAX>ZERO) THEN
          ICOUNT = 0
          DO I=1,NEL
           IF (OFFG(I)==ZERO) CYCLE
           APS_R(I) = C_MAX*SQRT(VOLG(I)/DELTAX(I)**3)
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
           TET_COLLA(I) = C_MIN * SQRT(DELTAX(I)**3 / VOLG(I))
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
