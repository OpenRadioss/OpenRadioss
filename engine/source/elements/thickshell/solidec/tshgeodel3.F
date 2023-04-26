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
Chd|  TSHGEODEL3                    source/elements/thickshell/solidec/tshgeodel3.F
Chd|-- called by -----------
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE TSHGEODEL3(NGL, OFFG ,VOLG ,AREA, VOLG0 ,L_SH,GEO ,NNOD,DT,NEL)
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
      INTEGER ,INTENT(IN)  :: NNOD,NEL
      INTEGER ,DIMENSION(MVSIZ) ,INTENT(IN)  :: NGL
      my_real, DIMENSION(NPROPG) , INTENT(IN)  :: GEO 
      my_real, DIMENSION(NEL) , INTENT(IN)  :: VOLG0 
      my_real, DIMENSION(MVSIZ) , INTENT(IN)  :: VOLG,AREA,L_SH ! L_SH=L_MAX**2
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
      VDEFMIN = GEO(190)
      VDEFMAX = GEO(191)
      ASPMAX  = GEO(192)
      COLMIN  = GEO(193)
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
C--- penta6      
      IF (NNOD ==6) THEN
       C_MAX = FOURTH*SQR3
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
C--- Hexa : APS_r=lmax/lmin lmax~sqrt(L_SH); lmin~A/lmax  -> 
C---        APS_r~lmax^2/A    
        IF (ASPMAX>ZERO) THEN
          ICOUNT = 0
          DO I=1,NEL
           IF (OFFG(I)==ZERO) CYCLE
           APS_R(I) = C_MAX*L_SH(I)/AREA(I)
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
C---        TET_c =1/ASP_r
        IF (COLMIN>ZERO) THEN
          ICOUNT = 0
          DO I=1,NEL
           IF (OFFG(I)==ZERO) CYCLE
           TET_COLLA(I) = C_MIN *AREA(I)/L_SH(I)
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
     .    'THE FOLLOWING THICK-SHELL ELEMENTS WILL BE DELETED BY MINIMUM VOLUME RATIO' /)
 1002 FORMAT(/
     .    'THE FOLLOWING THICK-SHELL ELEMENTS WILL BE DELETED BY MAXIMUM VOLUME RATIO' /)
 1003 FORMAT(/
     .    'THE FOLLOWING THICK-SHELL ELEMENTS WILL BE DELETED BY MAXIMUM ASPECT RATIO' /)
 1004 FORMAT(/
     .    'THE FOLLOWING THICK-SHELL ELEMENTS WILL BE DELETED BY MINIMUM COLLAPSE RATIO' /)
      END
