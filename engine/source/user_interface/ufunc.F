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
      !||    ufunc_dum   ../engine/source/user_interface/ufunc.F
      !||--- called by ------------------------------------------------------
      !||    radioss2    ../engine/source/engine/radioss2.F
      !||====================================================================
      SUBROUTINE UFUNC_DUM(IERR)
C---------+---------+---+---+--------------------------------------------
#include      "implicit_f.inc"
       INTEGER IERR
       IERR=0
      END
      !||====================================================================
      !||    get_u_func_n     ../engine/source/user_interface/ufunc.F
      !||--- calls      -----------------------------------------------------
      !||    get_var_user_i   ../engine/source/user_interface/eng_callback_c.c
      !||====================================================================
      INTEGER FUNCTION GET_U_FUNC_N(IFUNC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER IFUNC,I,NPFI,NPFIP
      CALL GET_VAR_USER_I(5,IFUNC,NPFI)
      CALL GET_VAR_USER_I(5,IFUNC+1,NPFIP)
      GET_U_FUNC_N=(NPFIP-NPFI)/2
      RETURN
      END
      !||====================================================================
      !||    get_u_func_x        ../engine/source/user_interface/ufunc.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg              ../engine/source/output/message/message.F
      !||    arret               ../engine/source/system/arret.F
      !||    get_u_numfun        ../engine/source/user_interface/ufunc.F
      !||    get_var_user_f      ../engine/source/user_interface/eng_callback_c.c
      !||    get_var_user_f_sp   ../engine/source/user_interface/eng_callback_c.c
      !||    get_var_user_i      ../engine/source/user_interface/eng_callback_c.c
      !||--- uses       -----------------------------------------------------
      !||    message_mod         ../engine/share/message_module/message_mod.F
      !||====================================================================
      my_real FUNCTION GET_U_FUNC_X(IFUNC,N)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr05_c.inc"
#include      "comlock.inc"
      INTEGER IFUNC,I,NPFI,NPFIP,N,NMAX
      my_real
     .    XX
      INTEGER GET_U_NUMFUN
      EXTERNAL GET_U_NUMFUN
      CALL GET_VAR_USER_I(5,IFUNC,NPFI)
      CALL GET_VAR_USER_I(5,IFUNC+1,NPFIP)
      NMAX=(NPFIP-NPFI)/2
      I=NPFI+(N-1)*2
      IF (N>NMAX.OR.N<=0) THEN
        CALL ANCMSG(MSGID=178,ANMODE=ANINFO,
     .              I1=N,I2=GET_U_NUMFUN(IFUNC),I3=NMAX)
        CALL ARRET(2)
      END IF
      IF (IRESP==1) THEN
          CALL GET_VAR_USER_F_SP(6,I,XX)
      ELSE
          CALL GET_VAR_USER_F(6,I,XX)
      ENDIF
      GET_U_FUNC_X=XX
      RETURN
      END
      !||====================================================================
      !||    get_u_func_y        ../engine/source/user_interface/ufunc.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg              ../engine/source/output/message/message.F
      !||    arret               ../engine/source/system/arret.F
      !||    get_u_numfun        ../engine/source/user_interface/ufunc.F
      !||    get_var_user_f      ../engine/source/user_interface/eng_callback_c.c
      !||    get_var_user_f_sp   ../engine/source/user_interface/eng_callback_c.c
      !||    get_var_user_i      ../engine/source/user_interface/eng_callback_c.c
      !||--- uses       -----------------------------------------------------
      !||    message_mod         ../engine/share/message_module/message_mod.F
      !||====================================================================
      my_real FUNCTION GET_U_FUNC_Y(IFUNC,N)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr05_c.inc"
#include      "comlock.inc"
      INTEGER IFUNC,I,NPFI,NPFIP,N,NMAX
      my_real
     .    YY
      INTEGER GET_U_NUMFUN
      EXTERNAL GET_U_NUMFUN
      CALL GET_VAR_USER_I(5,IFUNC,NPFI)
      CALL GET_VAR_USER_I(5,IFUNC+1,NPFIP)
      NMAX=(NPFIP-NPFI)/2
      I=NPFI+(N-1)*2
      IF (N>NMAX.OR.N<=0) THEN
          CALL ANCMSG(MSGID=178,ANMODE=ANINFO,
     .              I1=N,I2=GET_U_NUMFUN(IFUNC),I3=NMAX)
          CALL ARRET(2)
      END IF
      IF (IRESP==1) THEN
          CALL GET_VAR_USER_F_SP(6,I+1,YY)
      ELSE
          CALL GET_VAR_USER_F(6,I+1,YY)
      ENDIF
      GET_U_FUNC_Y=YY
      RETURN
      END
      !||====================================================================
      !||    get_u_func          ../engine/source/user_interface/ufunc.F
      !||--- called by ------------------------------------------------------
      !||    airbagb             ../engine/source/airbag/airbag2.F
      !||    airbagb1            ../engine/source/airbag/airbagb1.F
      !||    damping51           ../engine/source/assembly/damping.F
      !||    damping_vref_rby    ../engine/source/assembly/damping_vref_rby.F90
      !||    fv_up_switch        ../engine/source/airbag/fv_up_switch.F
      !||    fvbag1              ../engine/source/airbag/fvbag1.F
      !||    fvvent0             ../engine/source/airbag/fvvent0.F
      !||    material_flow       ../engine/source/tools/seatbelts/material_flow.F
      !||    porfor4             ../engine/source/airbag/porfor4.F
      !||    porform4            ../engine/source/airbag/porfor4.F
      !||    ruser32             ../engine/source/elements/spring/ruser32.F
      !||    ruser33             ../engine/source/elements/joint/ruser33.F
      !||    ruser35             ../engine/source/elements/spring/ruser35.F
      !||    ruser36             ../engine/source/elements/spring/ruser36.F
      !||    ruser44             ../engine/source/elements/spring/ruser44.F
      !||    ruser46             ../engine/source/elements/spring/ruser46.F
      !||    stdpl               ../engine/source/elements/joint/ruser33.F
      !||    volpvgb             ../engine/source/airbag/volpvg.F
      !||    xddl33              ../engine/source/elements/joint/ruser33.F
      !||    xddl33i             ../engine/source/elements/joint/ruser33.F
      !||    xforc28             ../engine/source/elements/xelem/xforc28.F
      !||    xforc29             ../engine/source/elements/xelem/xforc29.F
      !||    xforc30             ../engine/source/elements/xelem/xforc30.F
      !||    xforc31             ../engine/source/elements/xelem/xforc31.F
      !||--- calls      -----------------------------------------------------
      !||    get_var_user_f      ../engine/source/user_interface/eng_callback_c.c
      !||    get_var_user_f_sp   ../engine/source/user_interface/eng_callback_c.c
      !||    get_var_user_i      ../engine/source/user_interface/eng_callback_c.c
      !||====================================================================
      my_real FUNCTION GET_U_FUNC(IFUNC,XX,DERI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr05_c.inc"
C
      INTEGER IFUNC,I,NPFI,NPFIP,DEPLA
      my_real 
     .    ABC,DERI,XX,DX1,DX2,TFI,TFI1,TFI2,TFIP,VAL
C-----------------------------------------------
        GET_U_FUNC = ZERO
        CALL GET_VAR_USER_I(5,IFUNC,NPFI)
        CALL GET_VAR_USER_I(5,IFUNC+1,NPFIP)
        IF (IRESP==1) THEN
          CALL GET_VAR_USER_F_SP(6,NPFI,VAL)
        ELSE
          CALL GET_VAR_USER_F(6,NPFI,VAL)
        ENDIF
        DX2=VAL-XX
C
      DO 100 I=NPFI+2,NPFIP-2,2
         IF (IRESP==1) THEN
           CALL GET_VAR_USER_F_SP(6,I,TFI)
           CALL GET_VAR_USER_F_SP(6,I+1,TFIP)
           CALL GET_VAR_USER_F_SP(6,I-1,TFI1)
           CALL GET_VAR_USER_F_SP(6,I-2,TFI2)
         ELSE
           CALL GET_VAR_USER_F(6,I,TFI)
           CALL GET_VAR_USER_F(6,I+1,TFIP)
           CALL GET_VAR_USER_F(6,I-1,TFI1)
           CALL GET_VAR_USER_F(6,I-2,TFI2)
         ENDIF
       DX1 = -DX2
       DX2 = TFI - XX
       IF(DX2>=ZERO.OR.I==NPFIP-2)THEN
         DERI = (TFIP - TFI1) / (TFI - TFI2)
         IF(DX1<=DX2)THEN
           GET_U_FUNC = TFI1 + DX1 * DERI
         ELSE
           GET_U_FUNC = TFIP - DX2 * DERI
         ENDIF
         RETURN
       ENDIF
 100  CONTINUE
C
      RETURN
      END
      !||====================================================================
      !||    get_v_func           ../engine/source/user_interface/ufunc.F
      !||--- called by ------------------------------------------------------
      !||    ruser44              ../engine/source/elements/spring/ruser44.F
      !||--- calls      -----------------------------------------------------
      !||    get_var_user_vfunc   ../engine/source/user_interface/eng_callback_c.c
      !||    u_vinter2            ../engine/source/user_interface/eng_callback_c.c
      !||    u_vinter2dp          ../engine/source/user_interface/eng_callback_c.c
      !||====================================================================
      SUBROUTINE GET_V_FUNC(IFUNC,LLT,XX,DYDX,YY,JPOS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "scr05_c.inc"
C
      INTEGER IFUNC(*),JPOS(*),I,LLT,JAD(MVSIZ),JLEN(MVSIZ)
      INTEGER DEPLA
      my_real
     .    DYDX(*),XX(*),YY(*),VAR,TF

      CALL GET_VAR_USER_VFUNC(5,LLT,IFUNC,JPOS,JAD,JLEN)
      IF (IRESP==1) THEN
        CALL u_vinter2dp(JAD ,JPOS ,JLEN ,LLT,XX ,DYDX,YY )
      ELSE
      CALL u_vinter2(JAD ,JPOS ,JLEN ,LLT,XX ,DYDX,YY )
      ENDIF
C
      RETURN
      END
      !||====================================================================
      !||    get_u_numfun     ../engine/source/user_interface/ufunc.F
      !||--- called by ------------------------------------------------------
      !||    get_u_func_x     ../engine/source/user_interface/ufunc.F
      !||    get_u_func_y     ../engine/source/user_interface/ufunc.F
      !||--- calls      -----------------------------------------------------
      !||    get_var_user_i   ../engine/source/user_interface/eng_callback_c.c
      !||====================================================================
      INTEGER FUNCTION GET_U_NUMFUN(IDFUN)
#include      "implicit_f.inc"
#include      "com04_c.inc"
      INTEGER IDFUN
      INTEGER J,ID
C=======================================================================
      GET_U_NUMFUN = 0
      IF(IDFUN<=0)THEN
        RETURN
      ENDIF
C---
      DO J=1,NFUNCT
        CALL GET_VAR_USER_I(5,NFUNCT+1+J,ID)
        IF(IDFUN==ID)THEN
          GET_U_NUMFUN = J
          RETURN
        ENDIF
      ENDDO
C---
      RETURN
      END
      !||====================================================================
      !||    get_u_fid        ../engine/source/user_interface/ufunc.F
      !||--- calls      -----------------------------------------------------
      !||    get_var_user_i   ../engine/source/user_interface/eng_callback_c.c
      !||====================================================================
      INTEGER FUNCTION GET_U_FID(IFUN)
#include      "implicit_f.inc"
#include      "com04_c.inc"
      INTEGER IFUN,DEPLA
C=======================================================================
      GET_U_FID = 0
      IF(IFUN<=0)THEN
        RETURN
      ENDIF
      DEPLA = NFUNCT+1+IFUN
      CALL GET_VAR_USER_I(5,DEPLA,GET_U_FID)
C---
      RETURN
      END
      !||====================================================================
      !||    get_u_func_inv      ../engine/source/user_interface/ufunc.F
      !||--- calls      -----------------------------------------------------
      !||    get_var_user_f      ../engine/source/user_interface/eng_callback_c.c
      !||    get_var_user_f_sp   ../engine/source/user_interface/eng_callback_c.c
      !||    get_var_user_i      ../engine/source/user_interface/eng_callback_c.c
      !||====================================================================
      my_real FUNCTION GET_U_FUNC_INV(IFUNC,YY)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr05_c.inc"
C
      INTEGER IFUNC,I,NPFI,NPFIP,DEPLA
      my_real 
     .    ABC,YY,DERI,DY1,DY2,TFI,TFI1,TFI2,TFIP,VAL
C-----------------------------------------------
      GET_U_FUNC_INV = ZERO
      CALL GET_VAR_USER_I(5,IFUNC,NPFI)
      CALL GET_VAR_USER_I(5,IFUNC+1,NPFIP)
      IF (IRESP==1) THEN
        CALL GET_VAR_USER_F_SP(6,NPFI+1,VAL)
      ELSE
        CALL GET_VAR_USER_F(6,NPFI+1,VAL)
      ENDIF
C
      DY2=VAL-YY
C
      DO 100 I=NPFI+2,NPFIP-2,2
        IF (IRESP==1) THEN
          CALL GET_VAR_USER_F_SP(6,I,TFI)
          CALL GET_VAR_USER_F_SP(6,I+1,TFIP)
          CALL GET_VAR_USER_F_SP(6,I-1,TFI1)
          CALL GET_VAR_USER_F_SP(6,I-2,TFI2)
        ELSE
          CALL GET_VAR_USER_F(6,I,TFI)
          CALL GET_VAR_USER_F(6,I+1,TFIP)
          CALL GET_VAR_USER_F(6,I-1,TFI1)
          CALL GET_VAR_USER_F(6,I-2,TFI2)
        ENDIF
        DY1 = -DY2
        DY2 = TFIP - YY
        IF(DY2>=ZERO.OR.I==NPFIP-2)THEN
          IF (TFIP-TFI1 == ZERO) THEN
            DERI = ZERO
          ELSE
            DERI = (TFI - TFI2) / (TFIP-TFI1)
          ENDIF  
          IF(DY1<=DY2)THEN
            GET_U_FUNC_INV = TFI2 + DY1 * DERI
          ELSE
            GET_U_FUNC_INV = TFI  - DY2 * DERI
          ENDIF
          RETURN
        ENDIF
C
 100  CONTINUE
C
      RETURN
      END
