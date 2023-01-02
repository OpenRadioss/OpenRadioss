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
Chd|  RUSER44                       source/elements/spring/ruser44.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        GET_V_FUNC                    source/user_interface/ufunc.F 
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        GET_U_PNU                     source/user_interface/upidmid.F
Chd|====================================================================
      SUBROUTINE RUSER44(NEL,IOUT   ,IPROP  ,UVAR   ,NUVAR  ,
     2             FX      ,FY      ,FZ     ,XMOM   ,YMOM   ,
     3             ZMOM    ,E       ,OFF    ,STIFM  ,STIFR  ,
     4             VISCM   ,VISCR   ,MASS   ,XINER  ,DT     ,
     5             XL      ,VX      ,RY1    ,RZ1    ,RX     ,
     6             RY2     ,RZ2     ,FR_WAVE_E)
C-------------------------------------------------------------------------
C     Crushable frame spring property
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C IPROP    |  1      | I | R | PROPERTY NUMBER
C----------+---------+---+---+--------------------------------------------
C XL       |   NEL   | F | R | ELEMENT LENGTH
C----------+---------+---+---+--------------------------------------------
C UVAR     |NUVAR*NEL| F |R/W| USER ELEMENT VARIABLES
C NUVAR    |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C-------------------------------------------------------------------------
C FUNCTION 
C-------------------------------------------------------------------------
C INTEGER II = GET_U_PNU(I,IP,KK)
C         IFUNCI = GET_U_PNU(I,IP,KFUNC)
C         IPROPI = GET_U_PNU(I,IP,KFUNC)
C         IMATI  = GET_U_PNU(I,IP,KMAT)
C         I     :     VARIABLE INDEX(1 for first variable,...)
C         IP    :     PROPERTY NUMBER
C         KK    :     PARAMETER KFUNC,KMAT,KPROP
C         THIS FUNCTION RETURN THE USER STORED FUNCTION(IF KK=KFUNC), 
C         MATERIAL(IF KK=KMAT) OR PROPERTY(IF KK=KPROP) NUMBER. 
C         SEE LECG29 FOR CORRESPONDING ID STORAGE.
C-------------------------------------------------------------------------
C INTEGER IFUNCI = GET_U_MNU(I,IM,KFUNC)
C         I     :     VARIABLE INDEX(1 for first function)
C         IM    :     MATERIAL NUMBER
C         KFUNC :     ONLY FUNCTION ARE YET AVAILABLE.
C         THIS FUNCTION RETURN THE USER STORED FUNCTION NUMBER(function 
C         referred by users materials).
C         SEE LECM29 FOR CORRESPONDING ID STORAGE.
C-------------------------------------------------------------------------
C my_real PARAMI = GET_U_GEO(I,IP)
C         I     :     PARAMETER INDEX(1 for first parameter,...)
C         IP    :     PROPERTY NUMBER
C         THIS FUNCTION RETURN THE USER GEOMETRY PARAMETERS 
C-------------------------------------------------------------------------
C my_real PARAMI = GET_U_MAT(I,IM)
C         I     :     PARAMETER INDEX(1 for first parameter,...)
C         IM    :     MATERIAL NUMBER
C         THIS FUNCTION RETURN THE USER MATERIAL PARAMETERS 
C         NOTE: GET_U_MAT(0,IMAT) RETURN THE DENSITY
C-------------------------------------------------------------------------
C INTEGER PID = GET_U_PID(IP)
C         IP    :     PROPERTY NUMBER
C         THIS FUNCTION RETURN THE USER PROPERTY ID CORRESPONDING TO
C         USER PROPERTY NUMBER IP. 
C-------------------------------------------------------------------------
C INTEGER MID = GET_U_MID(IM)
C         IM   :     MATERIAL NUMBER
C         THIS FUNCTION RETURN THE USER MATERIAL ID CORRESPONDING TO
C         USER MATERIAL NUMBER IM. 
C-------------------------------------------------------------------------
C my_real Y = GET_U_FUNC(IFUNC,X,DYDX)
C         IFUNC :     function number obtained by 
C                     IFUNC = GET_U_MNU(I,IM,KFUNC) or IFUNC = GET_U_PNU(I,IP,KFUNC)
C         X     :     X value
C         DYDX  :     slope dY/dX
C         THIS FUNCTION RETURN Y(X)
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER IOUT,NEL,NUVAR,IPROP,ICO,
     .        GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU,
     .        KFUNC,KMAT,KPROP
      my_real
     .   UVAR(NUVAR,*),DT ,
     .   FX(*), FY(*), FZ(*), E(*), VX(*),MASS(*) ,XINER(*),
     .   RY1(*), RZ1(*), OFF(*), XMOM(*), YMOM(*),
     .   ZMOM(*), RX(*), RY2(*), RZ2(*),XL(*),
     .   STIFM(*) ,STIFR(*) , VISCM(*) ,VISCR(*) ,FR_WAVE_E(*) ,
     .   GET_U_MAT, GET_U_GEO, GET_U_FUNC
      EXTERNAL GET_U_MNU,GET_U_PNU,GET_U_MID,GET_U_PID,
     .         GET_U_MAT,GET_U_GEO, GET_U_FUNC
      PARAMETER (KFUNC=29)
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=47)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID1,ID2,ID10,ID20,
     .        IFUN_XP,IFUN_XMI,IFUN_XXPI,IFUN_XXMI,IFUN_YY1PI,
     .        IFUN_YY1MI,IFUN_YY2PI,IFUN_YY2MI,IFUN_ZZ1PI,
     .        IFUN_ZZ1MI,IFUN_ZZ2PI,IFUN_ZZ2MI,
     .        IFUN_XMR,IFUN_XXPR,IFUN_XXMR,IFUN_YY1PR,
     .        IFUN_YY1MR,IFUN_YY2PR,IFUN_YY2MR,IFUN_ZZ1PR,
     .        IFUN_ZZ1MR,IFUN_ZZ2PR,IFUN_ZZ2MR,
     .        IFUN_DAMP_XI,IFUN_DAMP_YI,IFUN_DAMP_ZI,
     .        IFUN_DAMP_XXI,IFUN_DAMP_YYI,IFUN_DAMP_ZZI
      INTEGER IFUNXM(NEL),IFUNXXM(NEL),IFUNYY1M(NEL),
     .        IFUNZZ1M(NEL),IFUNYY2M(NEL),IFUNZZ2M(NEL),
     .        IFUNXP(NEL),IFUNXXP(NEL),IFUNYY1P(NEL),
     .        IFUNZZ1P(NEL),IFUNYY2P(NEL),IFUNZZ2P(NEL),
     .        JPOSXM(NEL),JPOSXXM(NEL),JPOSYY1M(NEL),
     .        JPOSZZ1M(NEL),JPOSYY2M(NEL),JPOSZZ2M(NEL),
     .        JPOSXP(NEL),JPOSXXP(NEL),JPOSYY1P(NEL),
     .        JPOSZZ1P(NEL),JPOSYY2P(NEL),JPOSZZ2P(NEL),
     .        JPOS_DAMP_X(NEL),JPOS_DAMP_Y(NEL),JPOS_DAMP_Z(NEL),
     .        JPOS_DAMP_XX(NEL),JPOS_DAMP_YY(NEL),JPOS_DAMP_ZZ(NEL),
     .        IFUN_DAMP_X(NEL),IFUN_DAMP_Y(NEL),IFUN_DAMP_Z(NEL),
     .        IFUN_DAMP_XX(NEL),IFUN_DAMP_YY(NEL),IFUN_DAMP_ZZ(NEL)
      my_real :: 
     .        XLIMG,XLIM,XXLIM,YY1LIM,YY2LIM,ZZ1LIM,ZZ2LIM,
     .        K11,K44,K55,K66,K5B,K6C,LENG2,LENG,ULENG0,R,AX,AY,AZ,
     .        FSCAL_X,FSCAL_RX,FSCAL_RY1,FSCAL_RY2,FSCAL_RZ1,FSCAL_RZ2,
     .        A,B,D,FF1,FF2,R1,MM,FF,FSCAL_DAMP_X,FSCAL_DAMP_Y,
     .        FSCAL_DAMP_Z,FSCAL_DAMP_XX,FSCAL_DAMP_YY,
     .        FSCAL_DAMP_ZZ,F_X,F_Y,F_Z,F_XX,F_YY,F_ZZ,ALPHA,NCF,
     .        IDAMPING
      my_real , DIMENSION(NEL) ::
     .        DYDX,X,FXM,FXP,XX,FXXM,FXXP,YY1,FYY1M,FYY1P,
     .        ZZ1,FZZ1M,FZZ1P,YY2,FYY2M,FYY2P,ZZ2,FZZ2M,FZZ2P,
     .        ULENG,MY1,MY2,MZ1,MZ2,FX_DAMP,FY_DAMP,FZ_DAMP,
     .        FXX_DAMP,FYY_DAMP,FZZ_DAMP,XDOT,XXDOT,YY1DOT,ZZ1DOT,YY2DOT,ZZ2DOT,
     .        XDOT_OLD,XXDOT_OLD,YY1DOT_OLD,ZZ1DOT_OLD,YY2DOT_OLD,ZZ2DOT_OLD,
     .        ELODOTX,ELODOTY,ELODOTZ,ELODOTXX,ELODOTYY,ELODOTZZ
C=======================================================================
      XLIMG  = GET_U_GEO(1,IPROP)
      XLIM   = GET_U_GEO(2,IPROP)
      XXLIM  = GET_U_GEO(3,IPROP)
      YY1LIM = GET_U_GEO(4,IPROP)
      ZZ1LIM = GET_U_GEO(5,IPROP)
      YY2LIM = GET_U_GEO(6,IPROP)
      ZZ2LIM = GET_U_GEO(7,IPROP)
      ICO    = NINT(GET_U_GEO(16,IPROP))
      FSCAL_X   = GET_U_GEO(17,IPROP)
      FSCAL_RX  = GET_U_GEO(18,IPROP)
      FSCAL_RY1 = GET_U_GEO(19,IPROP)
      FSCAL_RY2 = GET_U_GEO(20,IPROP)
      FSCAL_RZ1 = GET_U_GEO(21,IPROP)
      FSCAL_RZ2 = GET_U_GEO(22,IPROP)
!------------------------------------------
      IFUN_XMI  = GET_U_PNU(1,IPROP,KFUNC) 
      IFUN_XXMI = GET_U_PNU(2,IPROP,KFUNC) 
      IFUN_YY1MI= GET_U_PNU(3,IPROP,KFUNC) 
      IFUN_ZZ1MI= GET_U_PNU(4,IPROP,KFUNC) 
      IFUN_YY2MI= GET_U_PNU(5,IPROP,KFUNC) 
      IFUN_ZZ2MI= GET_U_PNU(6,IPROP,KFUNC) 
      IFUN_XP   = GET_U_PNU(7,IPROP,KFUNC) 
      IFUN_XXPI = GET_U_PNU(8,IPROP,KFUNC) 
      IFUN_YY1PI= GET_U_PNU(9,IPROP,KFUNC) 
      IFUN_ZZ1PI= GET_U_PNU(10,IPROP,KFUNC) 
      IFUN_YY2PI= GET_U_PNU(11,IPROP,KFUNC) 
      IFUN_ZZ2PI= GET_U_PNU(12,IPROP,KFUNC) 
      IFUN_XMR  = GET_U_PNU(13,IPROP,KFUNC) 
      IFUN_XXMR = GET_U_PNU(14,IPROP,KFUNC) 
      IFUN_YY1MR= GET_U_PNU(15,IPROP,KFUNC) 
      IFUN_ZZ1MR= GET_U_PNU(16,IPROP,KFUNC) 
      IFUN_YY2MR= GET_U_PNU(17,IPROP,KFUNC) 
      IFUN_ZZ2MR= GET_U_PNU(18,IPROP,KFUNC) 
      IFUN_XXPR = GET_U_PNU(19,IPROP,KFUNC) 
      IFUN_YY1PR= GET_U_PNU(20,IPROP,KFUNC) 
      IFUN_ZZ1PR= GET_U_PNU(21,IPROP,KFUNC) 
      IFUN_YY2PR= GET_U_PNU(22,IPROP,KFUNC) 
      IFUN_ZZ2PR= GET_U_PNU(23,IPROP,KFUNC) 
!------------------------------------------
!  strain rate filtering number of cycles:
      NCF = GET_U_GEO(35,IPROP)
!  flag for damping activation
      IDAMPING = GET_U_GEO(36,IPROP)
!
!  add damping:
!
      IF (IDAMPING > ZERO) THEN
        IFUN_DAMP_XI  = GET_U_PNU(24,IPROP,KFUNC)
        IFUN_DAMP_YI  = GET_U_PNU(25,IPROP,KFUNC)
        IFUN_DAMP_ZI  = GET_U_PNU(26,IPROP,KFUNC)
        IFUN_DAMP_XXI = GET_U_PNU(27,IPROP,KFUNC)
        IFUN_DAMP_YYI = GET_U_PNU(28,IPROP,KFUNC)
        IFUN_DAMP_ZZI = GET_U_PNU(29,IPROP,KFUNC)
!
        FSCAL_DAMP_X   = GET_U_GEO(23,IPROP)
        FSCAL_DAMP_Y   = GET_U_GEO(24,IPROP)
        FSCAL_DAMP_Z   = GET_U_GEO(25,IPROP)
        FSCAL_DAMP_XX  = GET_U_GEO(26,IPROP)
        FSCAL_DAMP_YY  = GET_U_GEO(27,IPROP)
        FSCAL_DAMP_ZZ  = GET_U_GEO(28,IPROP)
! coefficients for function damping:
        F_X  = GET_U_GEO(29,IPROP)
        F_Y  = GET_U_GEO(30,IPROP)
        F_Z  = GET_U_GEO(31,IPROP)
        F_XX = GET_U_GEO(32,IPROP)
        F_YY = GET_U_GEO(33,IPROP)
        F_ZZ = GET_U_GEO(34,IPROP)
      ENDIF ! IF (IDAMPING > ZERO)
!
!  add filtering:
!
      IF (NCF > ZERO) THEN
        ALPHA = TWO/(NCF+ONE)
!
        DO I=1,NEL
          XDOT_OLD(I)   = UVAR(37,I) ! old strain rate
          XXDOT_OLD(I)  = UVAR(38,I)
          YY1DOT_OLD(I) = UVAR(39,I)
          ZZ1DOT_OLD(I) = UVAR(40,I)
          YY2DOT_OLD(I) = UVAR(41,I)
          ZZ2DOT_OLD(I) = UVAR(42,I)
        ENDDO
!
        DO I=1,NEL
          ULENG0    = UVAR(30,I)
          XDOT(I)   = VX(I)  * ULENG0 ! current strain rate
          XXDOT(I)  = RX(I)
          YY1DOT(I) = RY1(I)
          ZZ1DOT(I) = RZ1(I)
          YY2DOT(I) = RY2(I)
          ZZ2DOT(I) = RZ2(I)
        ENDDO
!  filter current strain rate
        DO I=1,NEL
          XDOT(I)   =  ALPHA * XDOT(I)   + (ONE-ALPHA) * XDOT_OLD(I)
          XXDOT(I)  =  ALPHA * XXDOT(I)  + (ONE-ALPHA) * XXDOT_OLD(I)
          YY1DOT(I) =  ALPHA * YY1DOT(I) + (ONE-ALPHA) * YY1DOT_OLD(I)
          ZZ1DOT(I) =  ALPHA * ZZ1DOT(I) + (ONE-ALPHA) * ZZ1DOT_OLD(I)
          YY2DOT(I) =  ALPHA * YY2DOT(I) + (ONE-ALPHA) * YY2DOT_OLD(I)
          ZZ2DOT(I) =  ALPHA * ZZ2DOT(I) + (ONE-ALPHA) * ZZ2DOT_OLD(I)
        ENDDO
! save new filtered strain rate:
        DO I=1,NEL
          UVAR(37,I) = XDOT(I)  ! new filtered strain rate
          UVAR(38,I) = XXDOT(I) 
          UVAR(39,I) = YY1DOT(I)
          UVAR(40,I) = ZZ1DOT(I)
          UVAR(41,I) = YY2DOT(I)
          UVAR(42,I) = ZZ2DOT(I)
        ENDDO
! total filtered strain :
        DO I=1,NEL
!!          ULENG0 = UVAR(30,I)
!!          X(I)   = UVAR(1,I) + DT * XDOT(I)  * ULENG0  ! filtered strain
          X(I)   = UVAR(1,I) + DT * XDOT(I)  ! filtered strain
          XX(I)  = UVAR(2,I) + DT * XXDOT(I) 
          YY1(I) = UVAR(3,I) + DT * YY1DOT(I)
          ZZ1(I) = UVAR(4,I) + DT * ZZ1DOT(I)
          YY2(I) = UVAR(5,I) + DT * YY2DOT(I)
          ZZ2(I) = UVAR(6,I) + DT * ZZ2DOT(I)
        ENDDO
      ENDIF ! IF (NCF > ZERO)
C=======================================================================
C     ELASTIQUE
C=======================================================================
      DO I=1,NEL
        MY1(I) = -YMOM(I)+HALF*XL(I)*FZ(I)
        MY2(I) =  YMOM(I)+HALF*XL(I)*FZ(I)
        MZ1(I) = -ZMOM(I)-HALF*XL(I)*FY(I)
        MZ2(I) =  ZMOM(I)-HALF*XL(I)*FY(I)	  
C
        LENG = MAX(XL(I),EM20)
        LENG2 = LENG*LENG
        ULENG(I) = ONE/LENG
C
        K11 = UVAR(19,I)
        K44 = UVAR(20,I)
        K55 = UVAR(21,I)*LENG2
        K66 = UVAR(22,I)*LENG2
        K5B = UVAR(23,I)*LENG2
        K6C = UVAR(24,I)*LENG2
C
        FX(I)  = FX(I)  + DT *  K11 * VX(I)
        XMOM(I)= XMOM(I)+ DT *  K44 * RX(I)
        MY1(I) = MY1(I) + DT * (K55 * RY1(I) + K5B * RY2(I))
        MY2(I) = MY2(I) + DT * (K5B * RY1(I) + K55 * RY2(I))
        MZ1(I) = MZ1(I) + DT * (K66 * RZ1(I) + K6C * RZ2(I))
        MZ2(I) = MZ2(I) + DT * (K6C * RZ1(I) + K66 * RZ2(I))
C
        ULENG0 = UVAR(30,I)
C
        IF (NCF == ZERO) THEN  ! no filtering
          X(I)   = UVAR(1,I) + DT * VX(I) * ULENG0  ! strain
          XX(I)  = UVAR(2,I) + DT * RX(I)
          YY1(I) = UVAR(3,I) + DT * RY1(I)
          ZZ1(I) = UVAR(4,I) + DT * RZ1(I)
          YY2(I) = UVAR(5,I) + DT * RY2(I)
          ZZ2(I) = UVAR(6,I) + DT * RZ2(I)
        ENDIF ! IF (NCF == 0)
C
        JPOSXM(I)   = NINT(UVAR(7,I))
        JPOSXXM(I)  = NINT(UVAR(8,I))
        JPOSYY1M(I) = NINT(UVAR(9,I))
        JPOSZZ1M(I) = NINT(UVAR(10,I))
        JPOSYY2M(I) = NINT(UVAR(11,I))
        JPOSZZ2M(I) = NINT(UVAR(12,I))
        JPOSXP(I)   = NINT(UVAR(13,I))
        JPOSXXP(I)  = NINT(UVAR(14,I))
        JPOSYY1P(I) = NINT(UVAR(15,I))
        JPOSZZ1P(I) = NINT(UVAR(16,I))
        JPOSYY2P(I) = NINT(UVAR(17,I))
        JPOSZZ2P(I) = NINT(UVAR(18,I))
C
        IFUNXP(I)   = IFUN_XP
        IFUNXM(I)   = IFUN_XMI
        IFUNXXM(I)  = IFUN_XXMI
        IFUNXXP(I)  = IFUN_XXPI
        IFUNYY1M(I) = IFUN_YY1MI
        IFUNYY1P(I) = IFUN_YY1PI
        IFUNYY2M(I) = IFUN_YY2MI
        IFUNYY2P(I) = IFUN_YY2PI
        IFUNZZ1M(I) = IFUN_ZZ1MI
        IFUNZZ1P(I) = IFUN_ZZ1PI
        IFUNZZ2M(I) = IFUN_ZZ2MI
        IFUNZZ2P(I) = IFUN_ZZ2PI
!
!  add function damping:
!
        IF (IDAMPING > ZERO) THEN
          IFUN_DAMP_X(I)  = IFUN_DAMP_XI
          IFUN_DAMP_Y(I)  = IFUN_DAMP_YI
          IFUN_DAMP_Z(I)  = IFUN_DAMP_ZI
          IFUN_DAMP_XX(I) = IFUN_DAMP_XXI
          IFUN_DAMP_YY(I) = IFUN_DAMP_YYI
          IFUN_DAMP_ZZ(I) = IFUN_DAMP_ZZI
!
          JPOS_DAMP_X(I)  = NINT(UVAR(31,I))
          JPOS_DAMP_Y(I)  = NINT(UVAR(32,I))
          JPOS_DAMP_Z(I)  = NINT(UVAR(33,I))
          JPOS_DAMP_XX(I) = NINT(UVAR(34,I))
          JPOS_DAMP_YY(I) = NINT(UVAR(35,I))
          JPOS_DAMP_ZZ(I) = NINT(UVAR(36,I))
        ENDIF ! IF (IDAMPING > ZERO)
      ENDDO
!
      IF (IDAMPING > ZERO) THEN
!  elongation rate for damping (linear + function):
        DO I=1,NEL
           ELODOTX(I)  = VX(I)                                 ! FX
           ELODOTY(I)  = - HALF * (RZ2(I) + RZ1(I)) * XL(I)  ! FY
           ELODOTZ(I)  =   HALF * (RY2(I) + RY1(I)) * XL(I)  ! FZ
           ELODOTXX(I) = RX(I)                                 ! XMOM
           ELODOTYY(I) = RY2(I) - RY1(I)                       ! YMOM
           ELODOTZZ(I) = RZ2(I) - RZ1(I)                       ! ZMOM
        ENDDO
      ENDIF ! IF (IDAMPING > ZERO)
C--------------------------------
      IF (ICO == 0) THEN  ! => ELASTO PLASTIQUE - Classique
C--------------------------------
        DO I=1,NEL
          ID1 = NINT(UVAR(29,I))/2
          ID2 = NINT(UVAR(29,I)) - 2*ID1
          ID10=ID1
          ID20=ID2
          IF (FR_WAVE_E(I) == ONE) THEN
            JPOSXM(I) = 0
            IFUNXM(I) = IFUN_XMR
          ENDIF
          IF (X(I) < XLIMG) THEN  !!! collapsed element
            FR_WAVE_E(I)=ONE
          ELSE
            FR_WAVE_E(I)=ZERO
          ENDIF
C
          IF (X(I) < XLIM .OR. ABS(XX(I)) > XXLIM) THEN
            ID1 = 1
            ID2 = 1
          ENDIF
          IF (ABS(YY1(I)) > YY1LIM .OR. ABS(ZZ1(I)) > ZZ1LIM) ID1 = 1
          IF (ABS(YY2(I)) > YY2LIM .OR. ABS(ZZ2(I)) > ZZ2LIM) ID2 = 1
C
          IF (ID1 == 1) THEN
            IFUNXM(I)   = IFUN_XMR
            IFUNXXM(I)  = IFUN_XXMR
            IFUNXXP(I)  = IFUN_XXPR
            IFUNYY1M(I) = IFUN_YY1MR
            IFUNYY1P(I) = IFUN_YY1PR
            IFUNZZ1M(I) = IFUN_ZZ1MR
            IFUNZZ1P(I) = IFUN_ZZ1PR
            IF (ID10 == 0) THEN
              JPOSXM(I)   = 0
              JPOSXXM(I)  = 0
              JPOSXXP(I)  = 0
              JPOSYY1M(I) = 0
              JPOSZZ1M(I) = 0
              JPOSYY1P(I) = 0
              JPOSZZ1P(I) = 0
            ENDIF
          ENDIF
          IF (ID2 == 1) THEN
            IFUNXM(I)   = IFUN_XMR
            IFUNXXM(I)  = IFUN_XXMR
            IFUNXXP(I)  = IFUN_XXPR
            IFUNYY2M(I) = IFUN_YY2MR
            IFUNYY2P(I) = IFUN_YY2PR
            IFUNZZ2M(I) = IFUN_ZZ2MR
            IFUNZZ2P(I) = IFUN_ZZ2PR
            IF (ID20 == 0) THEN
              JPOSXM(I)   = 0
              JPOSXXM(I)  = 0
              JPOSXXP(I)  = 0
              JPOSYY2M(I) = 0
              JPOSZZ2M(I) = 0
              JPOSYY2P(I) = 0
              JPOSZZ2P(I) = 0
            ENDIF
          ENDIF
          UVAR(29,I) = 2*ID1 + ID2
        ENDDO
!
        CALL GET_V_FUNC(IFUNXM,NEL,X,DYDX,FXM,JPOSXM)
        CALL GET_V_FUNC(IFUNXP,NEL,X,DYDX,FXP,JPOSXP)
        CALL GET_V_FUNC(IFUNXXM,NEL,XX,DYDX,FXXM,JPOSXXM)
        CALL GET_V_FUNC(IFUNXXP,NEL,XX,DYDX,FXXP,JPOSXXP)
        CALL GET_V_FUNC(IFUNYY1M,NEL,YY1,DYDX,FYY1M,JPOSYY1M)
        CALL GET_V_FUNC(IFUNYY1P,NEL,YY1,DYDX,FYY1P,JPOSYY1P)
        CALL GET_V_FUNC(IFUNZZ1M,NEL,ZZ1,DYDX,FZZ1M,JPOSZZ1M)
        CALL GET_V_FUNC(IFUNZZ1P,NEL,ZZ1,DYDX,FZZ1P,JPOSZZ1P)
        CALL GET_V_FUNC(IFUNYY2M,NEL,YY2,DYDX,FYY2M,JPOSYY2M)
        CALL GET_V_FUNC(IFUNYY2P,NEL,YY2,DYDX,FYY2P,JPOSYY2P)
        CALL GET_V_FUNC(IFUNZZ2M,NEL,ZZ2,DYDX,FZZ2M,JPOSZZ2M)
        CALL GET_V_FUNC(IFUNZZ2P,NEL,ZZ2,DYDX,FZZ2P,JPOSZZ2P)
!-------------------------------------------------------------
!
!  add damping function interpolation:
!
        IF (IDAMPING > ZERO) THEN
          IF (IFUN_DAMP_XI > 0) THEN
            DO I=1,NEL
              ELODOTX(I) = ELODOTX(I) / F_X
            ENDDO
            CALL GET_V_FUNC(IFUN_DAMP_X,NEL,ELODOTX,DYDX,FX_DAMP,JPOS_DAMP_X)
          ENDIF
!
          IF (IFUN_DAMP_YI > 0) THEN
            DO I=1,NEL
              ELODOTY(I) = ELODOTY(I) / F_Y
            ENDDO
            CALL GET_V_FUNC(IFUN_DAMP_Y,NEL,ELODOTY,DYDX,FY_DAMP,JPOS_DAMP_Y)
          ENDIF
!
          IF (IFUN_DAMP_ZI > 0) THEN 
            DO I=1,NEL
              ELODOTZ(I) = ELODOTZ(I) / F_Z
            ENDDO
            CALL GET_V_FUNC(IFUN_DAMP_Z,NEL,ELODOTZ,DYDX,FZ_DAMP,JPOS_DAMP_Z)
          ENDIF
!
          IF (IFUN_DAMP_XXI > 0) THEN
            DO I=1,NEL
              ELODOTXX(I) = ELODOTXX(I) / F_XX
            ENDDO
            CALL GET_V_FUNC(IFUN_DAMP_XX,NEL,ELODOTXX,DYDX,FXX_DAMP,JPOS_DAMP_XX)
          ENDIF
!
          IF (IFUN_DAMP_YYI > 0) THEN
            DO I=1,NEL
              ELODOTYY(I) = ELODOTYY(I) / F_YY
            ENDDO
            CALL GET_V_FUNC(IFUN_DAMP_YY,NEL,ELODOTYY,DYDX,FYY_DAMP,JPOS_DAMP_YY)
          ENDIF
!
          IF (IFUN_DAMP_ZZI > 0) THEN
            DO I=1,NEL
              ELODOTZZ(I) = ELODOTZZ(I) / F_ZZ
            ENDDO
            CALL GET_V_FUNC(IFUN_DAMP_ZZ,NEL,ELODOTZZ,DYDX,FZZ_DAMP,JPOS_DAMP_ZZ)
          ENDIF
        ENDIF ! IF (IDAMPING > ZERO)
!
        DO I=1,NEL
          FXP(I)   = FXP(I)  * FSCAL_X
          FXM(I)   = FXM(I)  * FSCAL_X
          FXXP(I)  = FXXP(I) * FSCAL_RX
          FXXM(I)  = FXXM(I) * FSCAL_RX
          FYY1M(I) = FYY1M(I)* FSCAL_RY1
          FYY1P(I) = FYY1P(I)* FSCAL_RY1 
          FZZ1M(I) = FZZ1M(I)* FSCAL_RZ1 
          FZZ1P(I) = FZZ1P(I)* FSCAL_RZ1 
          FYY2M(I) = FYY2M(I)* FSCAL_RY2 
          FYY2P(I) = FYY2P(I)* FSCAL_RY2  
          FZZ2M(I) = FZZ2M(I)* FSCAL_RZ2  
          FZZ2P(I) = FZZ2P(I)* FSCAL_RZ2             
        ENDDO
!
        DO I=1,NEL
          UVAR(1,I) = X(I) 
          UVAR(2,I) = XX(I) 
          UVAR(3,I) = YY1(I) 
          UVAR(4,I) = ZZ1(I) 
          UVAR(5,I) = YY2(I) 
          UVAR(6,I) = ZZ2(I) 
          UVAR(7,I)  = JPOSXM(I)
          UVAR(8,I)  = JPOSXXM(I)
          UVAR(9,I)  = JPOSYY1M(I)
          UVAR(10,I) = JPOSZZ1M(I)
          UVAR(11,I) = JPOSYY2M(I)
          UVAR(12,I) = JPOSZZ2M(I)
          UVAR(13,I) = JPOSXP(I)
          UVAR(14,I) = JPOSXXP(I)
          UVAR(15,I) = JPOSYY1P(I)
          UVAR(16,I) = JPOSZZ1P(I)
          UVAR(17,I) = JPOSYY2P(I)
          UVAR(18,I) = JPOSZZ2P(I)
!---
!  add damping:
!---
          IF (IDAMPING > ZERO) THEN
            UVAR(31,I) = JPOS_DAMP_X(I)
            UVAR(32,I) = JPOS_DAMP_Y(I)
            UVAR(33,I) = JPOS_DAMP_Z(I)
            UVAR(34,I) = JPOS_DAMP_XX(I)
            UVAR(35,I) = JPOS_DAMP_YY(I)
            UVAR(36,I) = JPOS_DAMP_ZZ(I)
          ENDIF ! IF IDAMPING > ZERO)
!---
          FX(I)   = MAX(MIN(FX(I)   ,FXP(I)),FXM(I) )
          XMOM(I) = MAX(MIN(XMOM(I),FXXP(I)),FXXM(I))
          MY1(I)  = MAX(MIN(MY1(I),FYY1P(I)),FYY1M(I))
          MZ1(I)  = MAX(MIN(MZ1(I),FZZ1P(I)),FZZ1M(I))
          MY2(I)  = MAX(MIN(MY2(I),FYY2P(I)),FYY2M(I))
          MZ2(I)  = MAX(MIN(MZ2(I),FZZ2P(I)),FZZ2M(I))
          FZ(I)   =  (MY1(I)+MY2(I)) * ULENG(I)
          FY(I)   = -(MZ1(I)+MZ2(I)) * ULENG(I)
          YMOM(I) = -HALF*(MY1(I)-MY2(I))
          ZMOM(I) = -HALF*(MZ1(I)-MZ2(I))
C
          VISCM(I) = ZERO
          VISCR(I) = ZERO
          STIFM(I) = UVAR(25,I)
          STIFR(I) = UVAR(26,I)
          MASS(I)  = UVAR(27,I)
          XINER(I) = UVAR(28,I)
!
!  add damping term (linear or function):
!
          IF (IDAMPING > ZERO) THEN
            IF (IFUN_DAMP_X(I) == 0) THEN ! !  linear damping
              FX(I) = FX(I) + FSCAL_DAMP_X * ELODOTX(I)
            ELSE
              FX(I) = FX(I) + FSCAL_DAMP_X * FX_DAMP(I)
            ENDIF
!
            IF (IFUN_DAMP_Y(I) == 0) THEN ! !  linear damping
              FY(I) = FY(I) + FSCAL_DAMP_Y * ELODOTY(I)
            ELSE
              FY(I) = FY(I) + FSCAL_DAMP_Y * FY_DAMP(I)
            ENDIF
!
            IF (IFUN_DAMP_Z(I) == 0) THEN ! !  linear damping
              FZ(I) = FZ(I) + FSCAL_DAMP_Z * ELODOTZ(I)
            ELSE
              FZ(I) = FZ(I) + FSCAL_DAMP_Z * FZ_DAMP(I)
            ENDIF
!
            IF (IFUN_DAMP_XX(I) == 0) THEN ! !  linear damping
              XMOM(I) = XMOM(I) + FSCAL_DAMP_XX * ELODOTXX(I)
            ELSE
              XMOM(I) = XMOM(I) + FSCAL_DAMP_XX * FXX_DAMP(I)
            ENDIF
!
            IF (IFUN_DAMP_YY(I) == 0) THEN ! !  linear damping
              YMOM(I) = YMOM(I) + FSCAL_DAMP_YY * ELODOTYY(I)
            ELSE
              YMOM(I) = YMOM(I) + FSCAL_DAMP_YY * FYY_DAMP(I)
            ENDIF
!
            IF (IFUN_DAMP_ZZ(I) == 0) THEN ! !  linear damping
              ZMOM(I) = ZMOM(I) + FSCAL_DAMP_ZZ * ELODOTZZ(I)
            ELSE
              ZMOM(I) = ZMOM(I) + FSCAL_DAMP_ZZ * FZZ_DAMP(I)
            ENDIF
          ENDIF ! IF (IDAMPING > ZERO)
!
        ENDDO
C--------------------------------
      ELSE    ! => ELASTO PLASTIQUE - Directions couplees
C--------------------------------
        DO I=1,NEL
          ID1 = NINT(UVAR(29,I))/2
          ID2 = NINT(UVAR(29,I)) - 2*ID1
          ID10=ID1
          ID20=ID2
          IF (FR_WAVE_E(I) == ONE) THEN  !!! collapsed element
            JPOSXM(I) = 0
            IFUNXM(I) = IFUN_XMR
          ENDIF
          IF (X(I) < XLIMG) THEN
            FR_WAVE_E(I)=ONE
          ELSE
            FR_WAVE_E(I)=ZERO
          ENDIF
C
          IF (X(I) < XLIM .OR. ABS(XX(I)) > XXLIM) THEN
            ID1 = 1
            ID2 = 1
          ENDIF
          IF (ABS(YY1(I)) > YY1LIM .OR. ABS(ZZ1(I)) > ZZ1LIM) ID1 = 1
          IF (ABS(YY2(I)) > YY2LIM .OR. ABS(ZZ2(I)) > ZZ2LIM) ID2 = 1
C
          IF (ID1 == 1) THEN
            IFUNXM(I)   = IFUN_XMR
            IFUNXXM(I)  = IFUN_XXMR
            IFUNXXP(I)  = IFUN_XXPR
            IFUNYY1M(I) = IFUN_YY1MR
            IFUNYY1P(I) = IFUN_YY1PR
            IFUNZZ1M(I) = IFUN_ZZ1MR
            IFUNZZ1P(I) = IFUN_ZZ1PR
            IF (ID10 == 0) THEN
              JPOSXM(I)   = 0
              JPOSXXM(I)  = 0
              JPOSXXP(I)  = 0
              JPOSYY1M(I) = 0
              JPOSZZ1M(I) = 0
              JPOSYY1P(I) = 0
              JPOSZZ1P(I) = 0
            ENDIF
          ENDIF
          IF (ID2 == 1) THEN
            IFUNXM(I)   = IFUN_XMR
            IFUNXXM(I)  = IFUN_XXMR
            IFUNXXP(I)  = IFUN_XXPR
            IFUNYY2M(I) = IFUN_YY2MR
            IFUNYY2P(I) = IFUN_YY2PR
            IFUNZZ2M(I) = IFUN_ZZ2MR
            IFUNZZ2P(I) = IFUN_ZZ2PR
            IF (ID20 == 0) THEN
              JPOSXM(I)   = 0
              JPOSXXM(I)  = 0
              JPOSXXP(I)  = 0
              JPOSYY2M(I) = 0
              JPOSZZ2M(I) = 0
              JPOSYY2P(I) = 0
              JPOSZZ2P(I) = 0
            ENDIF
          ENDIF
          UVAR(29,I) = 2*ID1 + ID2
        ENDDO
C
        CALL GET_V_FUNC(IFUNXM,NEL,X,DYDX,FXM,JPOSXM)
        CALL GET_V_FUNC(IFUNXP,NEL,X,DYDX,FXP,JPOSXP)
        CALL GET_V_FUNC(IFUNXXM,NEL,XX,DYDX,FXXM,JPOSXXM)
        CALL GET_V_FUNC(IFUNXXP,NEL,XX,DYDX,FXXP,JPOSXXP)
        CALL GET_V_FUNC(IFUNYY1M,NEL,YY1,DYDX,FYY1M,JPOSYY1M)
        CALL GET_V_FUNC(IFUNYY1P,NEL,YY1,DYDX,FYY1P,JPOSYY1P)
        CALL GET_V_FUNC(IFUNZZ1M,NEL,ZZ1,DYDX,FZZ1M,JPOSZZ1M)
        CALL GET_V_FUNC(IFUNZZ1P,NEL,ZZ1,DYDX,FZZ1P,JPOSZZ1P)
        CALL GET_V_FUNC(IFUNYY2M,NEL,YY2,DYDX,FYY2M,JPOSYY2M)
        CALL GET_V_FUNC(IFUNYY2P,NEL,YY2,DYDX,FYY2P,JPOSYY2P)
        CALL GET_V_FUNC(IFUNZZ2M,NEL,ZZ2,DYDX,FZZ2M,JPOSZZ2M)
        CALL GET_V_FUNC(IFUNZZ2P,NEL,ZZ2,DYDX,FZZ2P,JPOSZZ2P)
!
!  add damping function interpolation:
!
        IF (IDAMPING > ZERO) THEN
          IF (IFUN_DAMP_XI > 0) THEN
            DO I=1,NEL
              ELODOTX(I) = ELODOTX(I) / F_X
            ENDDO
            CALL GET_V_FUNC(IFUN_DAMP_X,NEL,ELODOTX,DYDX,FX_DAMP,JPOS_DAMP_X)
          ENDIF
!
          IF (IFUN_DAMP_YI > 0) THEN
            DO I=1,NEL
              ELODOTY(I) = ELODOTY(I) / F_Y
            ENDDO
            CALL GET_V_FUNC(IFUN_DAMP_Y,NEL,ELODOTY,DYDX,FY_DAMP,JPOS_DAMP_Y)
          ENDIF
!
          IF (IFUN_DAMP_ZI > 0) THEN 
            DO I=1,NEL
              ELODOTZ(I) = ELODOTZ(I) / F_Z
            ENDDO
            CALL GET_V_FUNC(IFUN_DAMP_Z,NEL,ELODOTZ,DYDX,FZ_DAMP,JPOS_DAMP_Z)
          ENDIF
!
          IF (IFUN_DAMP_XXI > 0) THEN
            DO I=1,NEL
              ELODOTXX(I) = ELODOTXX(I) / F_XX
            ENDDO
            CALL GET_V_FUNC(IFUN_DAMP_XX,NEL,ELODOTXX,DYDX,FXX_DAMP,JPOS_DAMP_XX)
          ENDIF
!
          IF (IFUN_DAMP_YYI > 0) THEN
            DO I=1,NEL
              ELODOTYY(I) = ELODOTYY(I) / F_YY
            ENDDO
            CALL GET_V_FUNC(IFUN_DAMP_YY,NEL,ELODOTYY,DYDX,FYY_DAMP,JPOS_DAMP_YY)
          ENDIF
!
          IF (IFUN_DAMP_ZZI > 0) THEN
            DO I=1,NEL
              ELODOTZZ(I) = ELODOTZZ(I) / F_ZZ
            ENDDO
            CALL GET_V_FUNC(IFUN_DAMP_ZZ,NEL,ELODOTZZ,DYDX,FZZ_DAMP,JPOS_DAMP_ZZ)
          ENDIF
        ENDIF ! IF (IDAMPING > ZERO)
!
        DO I=1,NEL
          FXP(I)   = FXP(I)  * FSCAL_X
          FXM(I)   = FXM(I)  * FSCAL_X
          FXXP(I)  = FXXP(I) * FSCAL_RX
          FXXM(I)  = FXXM(I) * FSCAL_RX
          FYY1M(I) = FYY1M(I)* FSCAL_RY1
          FYY1P(I) = FYY1P(I)* FSCAL_RY1 
          FZZ1M(I) = FZZ1M(I)* FSCAL_RZ1 
          FZZ1P(I) = FZZ1P(I)* FSCAL_RZ1 
          FYY2M(I) = FYY2M(I)* FSCAL_RY2 
          FYY2P(I) = FYY2P(I)* FSCAL_RY2  
          FZZ2M(I) = FZZ2M(I)* FSCAL_RZ2  
          FZZ2P(I) = FZZ2P(I)* FSCAL_RZ2             
        ENDDO
C
        DO I=1,NEL
          UVAR(1,I) = X(I)
          UVAR(2,I) = XX(I)
          UVAR(3,I) = YY1(I)
          UVAR(4,I) = ZZ1(I)
          UVAR(5,I) = YY2(I)
          UVAR(6,I) = ZZ2(I)
          UVAR(7,I)  = JPOSXM(I)
          UVAR(8,I)  = JPOSXXM(I)
          UVAR(9,I)  = JPOSYY1M(I)
          UVAR(10,I) = JPOSZZ1M(I)
          UVAR(11,I) = JPOSYY2M(I)
          UVAR(12,I) = JPOSZZ2M(I)
          UVAR(13,I) = JPOSXP(I)
          UVAR(14,I) = JPOSXXP(I)
          UVAR(15,I) = JPOSYY1P(I)
          UVAR(16,I) = JPOSZZ1P(I)
          UVAR(17,I) = JPOSYY2P(I)
          UVAR(18,I) = JPOSZZ2P(I)
!---
!  add damping:
!---
          IF (IDAMPING > ZERO) THEN
            UVAR(31,I) = JPOS_DAMP_X(I)
            UVAR(32,I) = JPOS_DAMP_Y(I)
            UVAR(33,I) = JPOS_DAMP_Z(I)
            UVAR(34,I) = JPOS_DAMP_XX(I)
            UVAR(35,I) = JPOS_DAMP_YY(I)
            UVAR(36,I) = JPOS_DAMP_ZZ(I)
          ENDIF ! IF IDAMPING > ZERO)
!
          MY1(I)  = MAX(MIN(MY1(I),FYY1P(I)),FYY1M(I))
          MZ1(I)  = MAX(MIN(MZ1(I),FZZ1P(I)),FZZ1M(I))
          MY2(I)  = MAX(MIN(MY2(I),FYY2P(I)),FYY2M(I))
          MZ2(I)  = MAX(MIN(MZ2(I),FZZ2P(I)),FZZ2M(I))
!
          XMOM(I) = MAX(MIN(XMOM(I),FXXP(I)),FXXM(I))
!
          FYY1P(I)  =  MAX(EM20, FYY1P(I))
          FYY1M(I)  = -MAX(EM20,-FYY1M(I))
          FYY2P(I)  =  MAX(EM20, FYY2P(I))
          FYY2M(I)  = -MAX(EM20,-FYY2M(I))
!
          FZZ1P(I)  =  MAX(EM20, FZZ1P(I))
          FZZ1M(I)  = -MAX(EM20,-FZZ1M(I))
          FZZ2P(I)  =  MAX(EM20, FZZ2P(I))
          FZZ2M(I)  = -MAX(EM20,-FZZ2M(I))
!
          FXXP(I)  =  MAX(EM20, FXXP(I))
          FXXM(I)  = -MAX(EM20,-FXXM(I))
!
C---------------------------------------------------
C         Mo = R M
C         Mn = Mo [l^2 Fo^2 - l^2Fo F + Mo M ]/[l^2Fo^2+Mo^2]
C         Fn = Fo [ Mo^2 - Mo M + l^2Fo F ]/[l^2Fo^2+Mo^2]
C         Fn = Fo [ 1 - Mn/Mo ] 
C         Fn = Fo [ 1 - [ l^2Fo^2 - l^2Fo F + Mo M ]/[l^2Fo^2+Mo^2]]
C         Fn = Fo [ 1 - [ l^2Fo^2 - l^2Fo F + R M^2 ]/[l^2Fo^2+R^2M^2]]
C         Fn = Fo [ 1 - [ l^2Fo(Fo - F) + R M^2 ]/[l^2Fo^2+R^2M^2]]
C         Mn = Mo [ 1 - Fn/Fo ]
C         Rn = Mn/M = Mo/M [ 1 - Fn/Fo ]
C         Rn = R [ 1 - Fn/Fo ]
C         si l = Mo/Fo
C         Fn = Fo [ 1/2 + F/2Fo - 1/2R  ]
C         Rn = R [ 1 - Fn/Fo ]  ! twice written
C---------------------------------------------------
!
!    - NODE 1 -
!
          AX = MAX(XMOM(I)/FXXP(I),XMOM(I)/FXXM(I))
          AY = MAX(MY1(I)/FYY1P(I),MY1(I)/FYY1M(I))
          AZ = MAX(MZ1(I)/FZZ1P(I),MZ1(I)/FZZ1M(I))
!
          MM = SQRT(AX*AX+AY*AY+AZ*AZ)
!
          FF = FX(I)/MIN(-EM20,FXM(I))
          R = (ONE+MM-FF)/MAX(EM20,TWO*MM)
          IF (R > ZERO .AND. R < ONE) THEN
            FF1 = HALF*(ONE-MM+FF)*FXM(I)
          ELSE
            FF1 = FXM(I)
            R = MAX(ZERO,MIN(ONE,R))
          ENDIF
          R1 = R
!
!    - NODE 2 -
!
          AY = MAX(MY2(I)/FYY2P(I),MY2(I)/FYY2M(I))
          AZ = MAX(MZ2(I)/FZZ2P(I),MZ2(I)/FZZ2M(I))
          MM = SQRT(AX*AX+AY*AY+AZ*AZ)
          R = (ONE+MM-FF)/MAX(EM20,TWO*MM)
          IF (R > ZERO .AND. R < ONE) THEN
            FF2 = HALF*(ONE-MM+FF)*FXM(I)
          ELSE
            FF2 = FXM(I)
            R = MAX(ZERO,MIN(ONE,R))
          ENDIF
          MY1(I) = R1*MY1(I)
          MZ1(I) = R1*MZ1(I)
          MY2(I) = R*MY2(I)
          MZ2(I) = R*MZ2(I)
C
          XMOM(I) = MIN(R,R1)*XMOM(I)
C
          FX(I)   = MAX(MIN(FX(I),FXP(I)), FF1, FF2)
C
          FZ(I)   =  (MY1(I)+MY2(I)) * ULENG(I)
          FY(I)   = -(MZ1(I)+MZ2(I)) * ULENG(I)
          YMOM(I) = -HALF*(MY1(I)-MY2(I))
          ZMOM(I) = -HALF*(MZ1(I)-MZ2(I))
C
          VISCM(I) = ZERO
          VISCR(I) = ZERO
          STIFM(I) = UVAR(25,I)
          STIFR(I) = UVAR(26,I)
          MASS(I)  = UVAR(27,I)
          XINER(I) = UVAR(28,I)
!
!  add damping term (linear or function):
!
          IF (IDAMPING > ZERO) THEN
            IF (IFUN_DAMP_X(I) == 0) THEN ! !  linear damping
              FX(I) = FX(I) + FSCAL_DAMP_X * ELODOTX(I)
            ELSE
              FX(I) = FX(I) + FSCAL_DAMP_X * FX_DAMP(I)
            ENDIF
!
            IF (IFUN_DAMP_Y(I) == 0) THEN ! !  linear damping
              FY(I) = FY(I) + FSCAL_DAMP_Y * ELODOTY(I)
            ELSE
              FY(I) = FY(I) + FSCAL_DAMP_Y * FY_DAMP(I)
            ENDIF
!
            IF (IFUN_DAMP_Z(I) == 0) THEN ! !  linear damping
              FZ(I) = FZ(I) + FSCAL_DAMP_Z * ELODOTZ(I)
            ELSE
              FZ(I) = FZ(I) + FSCAL_DAMP_Z * FZ_DAMP(I)
            ENDIF
!
            IF (IFUN_DAMP_XX(I) == 0) THEN ! !  linear damping
              XMOM(I) = XMOM(I) + FSCAL_DAMP_XX * ELODOTXX(I)
            ELSE
              XMOM(I) = XMOM(I) + FSCAL_DAMP_XX * FXX_DAMP(I)
            ENDIF
!
            IF (IFUN_DAMP_YY(I) == 0) THEN ! !  linear damping
              YMOM(I) = YMOM(I) + FSCAL_DAMP_YY * ELODOTYY(I)
            ELSE
              YMOM(I) = YMOM(I) + FSCAL_DAMP_YY * FYY_DAMP(I)
            ENDIF
!
            IF (IFUN_DAMP_ZZ(I) == 0) THEN ! !  linear damping
              ZMOM(I) = ZMOM(I) + FSCAL_DAMP_ZZ * ELODOTZZ(I)
            ELSE
              ZMOM(I) = ZMOM(I) + FSCAL_DAMP_ZZ * FZZ_DAMP(I)
            ENDIF
          ENDIF ! IF (IDAMPING > ZERO)
!
        ENDDO
C-------------------------------
      ENDIF
C-------------------------------
      RETURN
      END
