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
Chd|  USENSOR_DUM                   source/user_interface/usensor.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE USENSOR_DUM(IERR)
C---------+---------+---+---+--------------------------------------------
#include "implicit_f.inc"
       INTEGER IERR
       IERR=0
      END
Chd|====================================================================
Chd|  USENS_SHIFT_AB                source/user_interface/usensor.F
Chd|-- called by -----------
Chd|        SENSOR1                       source/tools/sensor/sensor1.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE USENS_SHIFT_AB(SENSOR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "my_real.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr_usens_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real SENSOR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C
       SENSOR(ISENBUF) = SENSOR(ISENBUF+LSENBUF)
       DO I = 1, LSENBUF-1
         SENSOR(ISENBUF+LSENBUF+I) = SENSOR(ISENBUF+I)
         SENSOR(ISENBUF+I) = ZERO
       ENDDO
C---
       RETURN
       END
Chd|====================================================================
Chd|  USENS_SHIFT_BA                source/user_interface/usensor.F
Chd|-- called by -----------
Chd|        SENSOR2                       source/tools/sensor/sensor2.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE USENS_SHIFT_BA(SENSOR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "my_real.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr_usens_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real SENSOR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
       DO I = 1, LSENBUF-1
         SENSOR(ISENBUF+LSENBUF+I) = SENSOR(ISENBUF+I)
       ENDDO
C---
       RETURN
       END
Chd|====================================================================
Chd|  GET_U_NUMSENS                 source/user_interface/usensor.F
Chd|-- called by -----------
Chd|        CSENS3                        source/elements/shell/coque/csens3.F
Chd|-- calls ---------------
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION GET_U_NUMSENS(IDSENS)
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
      INTEGER IDSENS
      INTEGER J,K,ID,ITYP
C===============================================================
C
      GET_U_NUMSENS = 0
      IF(IDSENS.LE.0)THEN
        RETURN
      ENDIF
C---
      DO J=1,NSENSOR
        K = (J-1)*NSENSI
        CALL GET_VAR_USER_I(7,K+1,ID)
        CALL GET_VAR_USER_I(7,K+2,ITYP)
cow41k4---
C       LSENSOR(I,J) == ILOCA(K + I)
        IF(IDSENS.EQ.ID)THEN
          IF (ITYP.GE.29 .AND. ITYP.LE.31)THEN
            GET_U_NUMSENS = J
            RETURN
          ENDIF
        ENDIF
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  GET_U_SENS_ID                 source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION GET_U_SENS_ID(ISENS)
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
      INTEGER ISENS
      INTEGER J,K,ID,ITYP
C===============================================================
C
      GET_U_SENS_ID = 0
      IF(ISENS.LE.0)THEN
        RETURN
      ENDIF
      K=(ISENS-1)*NSENSI+1
      CALL GET_VAR_USER_I(7,K,GET_U_SENS_ID)
C---
      RETURN
      END
#include "my_real.inc"
Chd|====================================================================
Chd|  SET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|-- called by -----------
Chd|        FORCE                         source/loads/general/force.F  
Chd|        FORCEPINCH                    source/loads/general/forcepinch.F
Chd|        H3D_PRE_SKIN_SCALAR           source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_VECTOR               source/output/h3d/h3d_results/h3d_skin_vector.F
Chd|        IMP_KPRES                     source/implicit/imp_glob_k.F  
Chd|        PFLUID                        source/loads/general/pfluid/pfluid.F
Chd|-- calls ---------------
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|        SET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        SET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION SET_U_SENS_VALUE(NSENS,IVAR,VAR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr_usens_c.inc"
#include      "r4r8_p.inc"
#include      "scr05_c.inc"
      INTEGER K,L, SHIFT
      INTEGER NSENS,ITYP,IVAR 
      my_real 
     .      VAR

Cow remonte les common blocs avant la declaration
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C============================================================
C
C
      SET_U_SENS_VALUE = -1
      IF(NSENS.EQ.0)THEN
        RETURN
      ENDIF
      IF(IVAR.GT.LSENBUF-1)THEN
        WRITE(*,*)"ERROR WRITING USER SENSOR / TOO MANY VARIABLES"
        RETURN
      ENDIF
      SHIFT = 0
      IF (ISECTOR.EQ.1) SHIFT = LSENBUF
      K = (NSENS-1)*NSENSI+2
      L = (NSENS-1)*NSENSR+ISENBUF+SHIFT+IVAR
C       LSENSOR(I,NSENS) == ILOCA(K + I)
C       SENSOR (I,NSENS) ==  LOCA(L + I)
C
      CALL GET_VAR_USER_I(7,K,ITYP)
C
      IF (ITYP.GE.29 .AND. ITYP.LE.31)THEN
        IF (IRESP .EQ.1) THEN
          CALL SET_VAR_USER_F_SP(8,L,VAR)
        ELSE
          CALL SET_VAR_USER_F(8,L,VAR)
        ENDIF
        SET_U_SENS_VALUE = 0
      ENDIF
C---
      RETURN
      END
#include "my_real.inc"
Chd|====================================================================
Chd|  GET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|-- called by -----------
Chd|        FORCE                         source/loads/general/force.F  
Chd|        FORCEPINCH                    source/loads/general/forcepinch.F
Chd|        H3D_PRE_SKIN_SCALAR           source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_VECTOR               source/output/h3d/h3d_results/h3d_skin_vector.F
Chd|        IMP_KPRES                     source/implicit/imp_glob_k.F  
Chd|        PFLUID                        source/loads/general/pfluid/pfluid.F
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION GET_U_SENS_VALUE(NSENS, IVAR, VAR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr_usens_c.inc"
#include      "scr05_c.inc"
      INTEGER K,L, SHIFT,NSENS, ITYP, IVAR
      my_real 
     .      VAR
Cow remonte les common block avant les declarations
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C============================================================
C

      GET_U_SENS_VALUE = -1
      IF(NSENS.EQ.0)THEN
        VAR = 0.0
        RETURN
      ENDIF
      IF(IVAR.GT.LSENBUF-1)THEN
        WRITE(*,*)"ERROR READING USER SENSOR / TOO MANY VARIABLES"
        RETURN
      ENDIF
      SHIFT = LSENBUF
      IF (ISECTOR.EQ.1) SHIFT = 0
      K = (NSENS-1)*NSENSI+2
      L = (NSENS-1)*NSENSR+ISENBUF+SHIFT+IVAR
C       LSENSOR(I,NSENS) == ILOCA(K + I)
C       SENSOR (I,NSENS) ==  LOCA(L + I)
      CALL GET_VAR_USER_I(7,K,ITYP)
      IF (ITYP.GE.29 .AND. ITYP.LE.31)THEN
        IF (IRESP .EQ. 1) THEN
          CALL GET_VAR_USER_F_SP(8,L,VAR)
        ELSE
          CALL GET_VAR_USER_F (8,L,VAR)
        ENDIF
        GET_U_SENS_VALUE = 0
      ENDIF
C---
      RETURN
      END
#include "my_real.inc"
Chd|====================================================================
Chd|  SET_U_SENS_MAXVALUE           source/user_interface/usensor.F
Chd|-- called by -----------
Chd|        CSENS3                        source/elements/shell/coque/csens3.F
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        SET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION SET_U_SENS_MAXVALUE(NSENS,IVAR,VAR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr_usens_c.inc"
#include      "r4r8_p.inc"
#include      "scr05_c.inc"
      INTEGER K,L, SHIFT
      INTEGER NSENS,ITYP,IVAR 
      my_real 
     .      VAR,MAXV
C=======================================================================
      SET_U_SENS_MAXVALUE = -1
      IF(NSENS.EQ.0)THEN
        RETURN
      ENDIF
      IF(IVAR.GT.LSENBUF-1)THEN
        WRITE(*,*)"ERROR WRITING USER SENSOR / TOO MANY VARIABLES"
        RETURN
      ENDIF
      SHIFT = 0
      IF (ISECTOR.EQ.1) SHIFT = LSENBUF
      K = (NSENS-1)*NSENSI+2
      L = (NSENS-1)*NSENSR+ISENBUF+SHIFT+IVAR
      CALL GET_VAR_USER_I(7,K,ITYP)
      IF (ITYP.GE.29 .AND. ITYP.LE.31)THEN
        CALL MY_BARRIER()
        IF (IRESP .EQ.1) THEN
          CALL GET_VAR_USER_F_SP(8,L,MAXV)
          MAXV = MAX(MAXV,VAR)
          CALL SET_VAR_USER_F_SP(8,L,MAXV)
        ELSE
          CALL GET_VAR_USER_F (8,L,MAXV)
          MAXV = MAX(MAXV,VAR)
          CALL SET_VAR_USER_F(8,L,MAXV)
        ENDIF
        CALL MY_BARRIER()
        SET_U_SENS_MAXVALUE = 0
      ENDIF
C---
      RETURN
      END
Chd|====================================================================
Chd|  GET_U_SENS_FPAR               source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION GET_U_SENS_FPAR(NSENS,IVAR,VAR)
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr_usens_c.inc"
#include      "scr05_c.inc"
      INTEGER NSENS,K,L, IVAR,ITYP
      my_real
     .      VAR
C===============================================================
C

      GET_U_SENS_FPAR = -1
      IF(IVAR.GT.NSENPARR)THEN
        WRITE(IOUT,*)
     .      ' **ERROR NUMBER OF USER PARAMETER(',IVAR,
     .      ') EXCEED MAXNUPARAM(',NSENPARR,')'
        WRITE(ISTDO,*)
     .      ' **ERROR NUMBER OF USER PARAMETER(',IVAR,
     .      ') EXCEED MAXNUPARAM(',NSENPARR,')'
        GET_U_SENS_FPAR = NSENPARR
        RETURN
      ENDIF
C
      K=(NSENS-1)*NSENSI+2
      L=(NSENS-1)*NSENSR+ISENPARR+IVAR
C       LSENSOR(I,NSENS) == ILOCA(K + I)
C       SENSOR (I,NSENS) ==  LOCA(L + I)
      CALL GET_VAR_USER_I(7,K,ITYP)
      IF (ITYP.GE.29 .AND. ITYP.LE.31)THEN
        IF (IRESP.EQ.1) THEN 
           CALL GET_VAR_USER_F_SP(8,L,VAR)
        ELSE
           CALL GET_VAR_USER_F(8,L,VAR)
        ENDIF
        GET_U_SENS_FPAR = 0
      ENDIF
C---
      RETURN
      END
Chd|====================================================================
Chd|  GET_U_SENS_IPAR               source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION GET_U_SENS_IPAR(NSENS,IVAR,VAR)
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr_usens_c.inc"
#include      "scr05_c.inc"
      INTEGER NSENS,K,IVAR,ITYP,VAR,IL,KL
C===============================================================

      GET_U_SENS_IPAR = -1
      IF(IVAR.GT.NSENPARI)THEN
        WRITE(IOUT,*)
     .      ' **ERROR NUMBER OF USER PARAMETER(',IVAR,
     .      ') EXCEED MAXNUPARAM(',NSENPARI,')'
        WRITE(ISTDO,*)
     .      ' **ERROR NUMBER OF USER PARAMETER(',IVAR,
     .      ') EXCEED MAXNUPARAM(',NSENPARI,')'
        GET_U_SENS_IPAR = NSENPARI
        RETURN
      ENDIF
C
C       LSENSOR(I,NSENS) == ILOCA(K + I)
      K =(NSENS-1)*NSENSI
      KL=K+2
      CALL GET_VAR_USER_I(7,KL,ITYP)
      IF (ITYP.GE.29 .AND. ITYP.LE.31)THEN
        KL=K+ISENPARI+IVAR
        CALL GET_VAR_USER_I(7,KL,VAR)
        GET_U_SENS_IPAR = 0
      ENDIF
C---
      RETURN
      END
c
#include "my_real.inc"
Chd|====================================================================
Chd|  SET_U_SENS_ACTI               source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|        SET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        SET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION SET_U_SENS_ACTI(NSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
      INTEGER NSENS,K,L,ITYP,LL
      my_real TSTART,SETVAR
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "scr_usens_c.inc"
#include      "scr05_c.inc"
c========================================================================
      SET_U_SENS_ACTI = -1
      IF (NSENS /= 0) THEN
        K = (NSENS-1)*NSENSI+2
        L = (NSENS-1)*NSENSR+2
        CALL GET_VAR_USER_I (7,K,ITYP)

        IF (ITYP >= 29 .AND. ITYP <= 31)THEN
          IF (IRESP == 1) THEN
            CALL GET_VAR_USER_F_SP(8,L,TSTART)
            SETVAR = MIN(TSTART, TT)
            LL = L+LSENBUF
            CALL SET_VAR_USER_F_SP(8,LL,SETVAR)
          ELSE
            CALL GET_VAR_USER_F(8,L,TSTART)
            SETVAR = MIN(TSTART, TT)
            LL = L+LSENBUF
            CALL SET_VAR_USER_F(8,LL,SETVAR)
          ENDIF
          SET_U_SENS_ACTI = 0
        ENDIF
      ENDIF
C---
      RETURN
      END
c
#include "my_real.inc"
Chd|====================================================================
Chd|  SET_U_SENS_DEACTI             source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|        SET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        SET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|====================================================================
      INTEGER FUNCTION SET_U_SENS_DEACTI(NSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
      INTEGER NSENS,K,L,ITYP,LL
      my_real TSTART,SETVAR
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "scr_usens_c.inc"
#include      "scr05_c.inc"
c========================================================================
      SET_U_SENS_DEACTI = -1
      IF (NSENS /= 0) THEN
        K = (NSENS-1)*NSENSI+2
        L = (NSENS-1)*NSENSR+2
        CALL GET_VAR_USER_I (7,K,ITYP)

        IF (ITYP >= 29 .AND. ITYP <= 31)THEN
          IF (IRESP == 1) THEN
            CALL GET_VAR_USER_F_SP(8,L,TSTART)
            SETVAR = EP30
            LL = L+LSENBUF
            CALL SET_VAR_USER_F_SP(8,LL,SETVAR)
          ELSE
            CALL GET_VAR_USER_F(8,L,TSTART)
            SETVAR = EP30
            LL = L+LSENBUF
            CALL SET_VAR_USER_F(8,LL,SETVAR)
          ENDIF
          SET_U_SENS_DEACTI = 0
        ENDIF
      ENDIF
C---
      RETURN
      END
c
#include "my_real.inc"
Chd|====================================================================
Chd|  GET_U_SENS_ACTI               source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|====================================================================
      my_real FUNCTION GET_U_SENS_ACTI(NSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
      INTEGER NSENS,L,ITYP
      my_real TSTART
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "scr_usens_c.inc"
#include      "scr05_c.inc"
c========================================================================
      GET_U_SENS_ACTI = ZERO
      IF (NSENS == 0) THEN
        GET_U_SENS_ACTI = TT
      ELSE
        L = (NSENS-1)*NSENSR+LSENBUF+2
        IF (IRESP == 1) THEN
          CALL GET_VAR_USER_F_SP(8,L,TSTART)
        ELSE
          CALL GET_VAR_USER_F(8,L,TSTART)
        ENDIF
        IF (TT > TSTART) THEN
          GET_U_SENS_ACTI = TT-TSTART
        ENDIF
      ENDIF
C---
      RETURN
      END
c
#include "my_real.inc"
Chd|====================================================================
Chd|  GET_U_SENS                    source/user_interface/usensor.F
Chd|-- called by -----------
Chd|        RUSER32                       source/elements/spring/ruser32.F
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_I                source/user_interface/eng_callback_c.c
Chd|====================================================================
      my_real FUNCTION GET_U_SENS(USENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
      INTEGER USENS,I,J,DECALAGE,LI
      my_real 
     .       LT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
C
C     LSENSOR(I,J) == ILOCA(ISH5+I+(J-1)*NSENSI)
C     SENSOR(I,J)  == LOCA(ISH6+I+(J-1)*NSENSR)

      GET_U_SENS = ZERO
      IF(USENS.EQ.0)THEN
        GET_U_SENS = TT
        RETURN
      ENDIF
      DO J=1,NSENSOR
C       IF(USENS.EQ.LSENSOR(1,J))THEN
          DECALAGE = 1+(J-1)*NSENSI
          CALL GET_VAR_USER_I (7,DECALAGE,LI)
          IF (USENS .EQ. LI) THEN 
C         IF(TT.GT.SENSOR(2,J))THEN
C
            DECALAGE=2+(J-1)*NSENSR
            IF (IRESP.EQ.1) THEN
             CALL GET_VAR_USER_F_SP(8,DECALAGE,LT)
            ELSE
             CALL GET_VAR_USER_F(8,DECALAGE,LT)
            ENDIF
            IF (TT .GT. LT) THEN
            GET_U_SENS = TT - LT
          ENDIF
          RETURN
        ENDIF
      ENDDO
C----
      RETURN
      END
#include "my_real.inc"
Chd|====================================================================
Chd|  GET_U_SENS_DELAY              source/user_interface/usensor.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GET_VAR_USER_F                source/user_interface/eng_callback_c.c
Chd|        GET_VAR_USER_F_SP             source/user_interface/eng_callback_c.c
Chd|====================================================================
      my_real FUNCTION GET_U_SENS_DELAY(NSENS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
      INTEGER NSENS,K,L,ITYP
      my_real TDELAY
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "scr_usens_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C
      GET_U_SENS_DELAY = ZERO
      IF(NSENS.EQ.0)THEN
        GET_U_SENS_DELAY = TT
        RETURN
      ENDIF
      L=(NSENS-1)*NSENSR+1
      IF (IRESP.EQ.1) THEN
        CALL GET_VAR_USER_F_SP(8,L,TDELAY)
      ELSE
        CALL GET_VAR_USER_F(8,L,TDELAY)
      ENDIF
      GET_U_SENS_DELAY = TDELAY
C---
      RETURN
      END
