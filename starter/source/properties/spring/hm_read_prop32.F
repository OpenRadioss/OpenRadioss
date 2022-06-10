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
Chd|  HM_READ_PROP32                source/properties/spring/hm_read_prop32.F
Chd|-- called by -----------
Chd|        HM_READ_PROP_GENERIC          source/properties/hm_read_prop_generic.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        SET_U_GEO                     source/user_interface/uaccess.F
Chd|        SET_U_PNU                     source/user_interface/uaccess.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP32(IOUT  ,NUVAR ,PARGEO,UNITAB,IG,
     .                          IGTYP,PROP_TAG,TITR,LSUBMODEL)
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD
      USE SUBMODEL_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "warn_c.inc"
#include      "scr17_c.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IIN      |  1      | I | R | INPUT FILE UNIT (D00 file) 
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C NUVAR    |  1      | I | W | NUMBER OF USER ELEMENT VARIABLES
C----------+---------+---+---+--------------------------------------------
C PARGEO   |  *      | F | W | 1)SKEW NUMBER
C          |         |   |   | 2)STIFNESS FOR INTERFACE
C          |         |   |   | 3)FRONT WAVE OPTION
C          |         |   |   | 4)... not yet used
C----------+---------+---+---+--------------------------------------------
C
C     This subroutine read the user geometry parameters.
C
C     The geometry datas has to bee stored in radioss storage 
C     with the function SET_U_GEO(value_index,value). 
C
C     If some standard radioss functions (time function or 
C     x,y function) are used, this function IDs has to 
C     bee stored with the function SET_U_PNU(func_index,func_id,KFUNC).
C
C     If this property refers to a user material, this 
C     material IDs has to bee stored with the function 
C     SET_U_PNU(mat_index,mat_id,KMAT).
C
C     If this property refers to a user property, this 
C     sub-property IDs has to bee stored with the function 
C     SET_U_PNU(sub_prop_index,sub_prop_id,KMAT).
C
C     SET_U_GEO and SET_U_PNU return 0 if no error 
C     SET_U_GEO and SET_U_PNU return the maximum allowed index 
C     if index is larger than this maximum
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IOUT,NUVAR,IGTYP
      my_real PARGEO(*)
      INTEGER SET_U_PNU,SET_U_GEO,KFUNC
      EXTERNAL SET_U_PNU,SET_U_GEO
      PARAMETER (KFUNC=29)
      INTEGER IG
      CHARACTER*nchartitle,
     .   TITR
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C=======================================================================
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFUNC1,IFUNC2,ISENS,IERROR,ITYP,ILOCK
      my_real 
     .  AMAS,AA,STIF00,STIF0,STIF1,E1,F1,D1,TSCAL,DSCAL,FSCAL,
     .  T_UNIT,L_UNIT,F_UNIT
      LOGICAL IS_AVAILABLE, IS_CRYPTED
C=======================================================================
C
      IS_CRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('ISENSOR',ISENS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ILock',ILOCK,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_A1',IFUNC1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B1',IFUNC2,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('MASS',AMAS,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STIFF0',STIF0,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STIFF1',STIF1,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('SPR_PRE_F1',F1,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('SPR_PRE_D1',D1,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('SPR_PRE_E1',E1,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Scale_t',TSCAL,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Scale_d',DSCAL,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Scale_f',FSCAL,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV_DIM('Scale_t',T_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Scale_d',L_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Scale_f',F_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
C----------------------
C
      IF(.NOT. IS_CRYPTED)THEN
        WRITE(IOUT,1400) IG
      ELSE
        WRITE(IOUT,1500) IG
      ENDIF
C
      NUVAR = 4
C
      IF (TSCAL == ZERO) TSCAL = ONE * T_UNIT
      IF (DSCAL == ZERO) DSCAL = ONE * L_UNIT
      IF (FSCAL == ZERO) FSCAL = ONE * F_UNIT
C
      D1 = -ABS(D1)
      STIF00=EM20
      IF(IFUNC1.NE.0.AND.IFUNC2.NE.0)THEN
        ITYP=4
      ELSEIF(IFUNC2.NE.0)THEN
        ITYP=3
      ELSEIF(IFUNC1.NE.0)THEN
        ITYP=2
      ELSE
        ITYP=1
        IF(F1.NE.0..AND.D1.NE.0.)THEN
         IF(E1.NE.0..OR.STIF1.NE.0.)THEN
            CALL ANCMSG(MSGID=408,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                   I1=IG,
     .                   C1=TITR)
         ENDIF
        ELSEIF(F1.NE.0..AND.E1.NE.0.)THEN
         IF(STIF1.NE.0.)THEN
            CALL ANCMSG(MSGID=408,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                   I1=IG,
     .                   C1=TITR)
         ENDIF
        ELSEIF(D1.NE.0..AND.E1.NE.0.)THEN
         IF(STIF1.NE.0.)THEN
            CALL ANCMSG(MSGID=408,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                   I1=IG,
     .                   C1=TITR)
         ENDIF
        ENDIF
        IF(F1.NE.ZERO)THEN
          IF(D1.NE.ZERO)THEN
            STIF1=-F1/D1
          ELSEIF(E1.NE.ZERO)THEN
            STIF1=HALF*F1*F1/E1
          ELSEIF(STIF1.EQ.ZERO)THEN  
            STIF1=STIF00
          ENDIF
          D1=-F1/STIF1
          E1=-HALF*F1*D1
          ELSEIF(D1.NE.ZERO)THEN
          IF(E1.NE.ZERO)THEN
            STIF1=TWO*E1/D1/D1
          ELSEIF(STIF1.EQ.ZERO)THEN
            STIF1=STIF00
          ENDIF
          F1=-STIF1*D1
          E1=-HALF*F1*D1
        ELSEIF(E1.NE.ZERO)THEN
          IF(STIF1.EQ.ZERO)THEN
            STIF1=STIF00
          ENDIF
          F1=SQRT(TWO*E1*STIF1)
          D1=-F1/STIF1
        ELSE
          IF(STIF1.EQ.ZERO)THEN
            STIF1=STIF00
          ENDIF
          F1=ZERO
          E1=ZERO
          D1=ZERO 
        ENDIF
      ENDIF
      IF(STIF1.EQ.ZERO)STIF1=STIF0
      AA = ISENS
      IERROR = SET_U_GEO(5,AA)
      AA = ITYP
      IERROR = SET_U_GEO(6,AA)
      AA = ILOCK
      IERROR = SET_U_GEO(10,AA)
C
      PARGEO(1) = 0
      PARGEO(2) = STIF0+STIF1
C
      IF(.NOT. IS_CRYPTED)THEN
       IF(ITYP.EQ.1)THEN
        WRITE(IOUT,1001)AMAS,STIF0,ISENS,ILOCK,F1,D1,E1,STIF1
       ELSEIF(ITYP.EQ.2)THEN
        WRITE(IOUT,1002)AMAS,STIF0,ISENS,ILOCK,IFUNC1,DSCAL
       ELSEIF(ITYP.EQ.3)THEN
        WRITE(IOUT,1002)AMAS,STIF0,ISENS,ILOCK,IFUNC2,TSCAL
       ELSEIF(ITYP.EQ.4)THEN
        WRITE(IOUT,1004)AMAS,STIF0,ISENS,ILOCK,IFUNC1,DSCAL,IFUNC2,TSCAL
       ENDIF
      ENDIF
C
      IERROR = SET_U_GEO(1,AMAS)
      IERROR = SET_U_GEO(2,STIF0)
      IERROR = SET_U_GEO(3,STIF1)
      IERROR = SET_U_GEO(4,F1)
      IERROR = SET_U_PNU(1,IFUNC1,KFUNC)
      IERROR = SET_U_PNU(2,IFUNC2,KFUNC)
      IERROR = SET_U_GEO(7,ONE/TSCAL)
      IERROR = SET_U_GEO(8,ONE/DSCAL)
      IERROR = SET_U_GEO(9,FSCAL)
      IERROR = SET_U_GEO(11,D1)
C
C-----------------------------
C       PROPERTY BUFFER 
C-----------------------------
C
      PROP_TAG(IGTYP)%G_FOR  = 3
      PROP_TAG(IGTYP)%G_MOM = 3
      PROP_TAG(IGTYP)%G_SKEW = 3
      PROP_TAG(IGTYP)%G_SKEW_ERR = 3
      PROP_TAG(IGTYP)%G_MASS = 1
      PROP_TAG(IGTYP)%G_V_REPCVT  = 3 ! -- VITESSES REPERE CONVECTEE (V_REPCVT)
      PROP_TAG(IGTYP)%G_VR_REPCVT = 3  ! -- VITESSES REPERE CONVECTEE (VR_REPCVT)
      PROP_TAG(IGTYP)%G_NUVAR = NUVAR
C
      RETURN
 999  CONTINUE
      CALL ANCMSG(MSGID=401,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=IG,
     .            C2=TITR,
     .            C1='USER 32')
      RETURN
 1001 FORMAT(
     & 5X,'LINEAR PRETENSION SPRING',/,
     & 5X,'MASS. . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'STIFFNESS BEFORE SENSOR ACTIVATION. . .=',1PG20.13/,
     & 5X,'ACTIVATION SENSOR ID. . . . . . . . . .=',I10/,
     & 5X,'FLAG FOR LOCK FEATURE ACTIVATION. . . .=',I10/,
     & 5X,'FORCE AFTER SENSOR ACTIVATION . . . . .=',1PG20.13/,
     & 5X,'MAX RETRACTION AFTER SENSOR ACTIVATION.=',1PG20.13/,
     & 5X,'INITIAL ENERGY AFTER SENSOR ACTIVATION.=',1PG20.13/,
     & 5X,'STIFFNESS AFTER SENSOR ACTIVATION . . .=',1PG20.13//)
 1002 FORMAT(
     & 5X,'NON LINEAR PRETENSION SPRING',/,
     & 5X,'----------------------------',/,
     & 5X,' DISPLACEMENT DEPENDING F=f(x-x0)',/,
     & 5X,'MASS. . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'STIFFNESS BEFORE SENSOR ACTIVATION. . .=',1PG20.13/,
     & 5X,'ACTIVATION SENSOR ID. . . . . . . . . .=',I10/,
     & 5X,'FLAG FOR LOCK FEATURE ACTIVATION. . . .=',I10/,
     & 5X,'FORCE(DISP-ACTIV_DISP) FUNCTION ID. . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13//)
 1003 FORMAT(
     & 5X,'NON LINEAR PRETENSION SPRING',/,
     & 5X,'----------------------------',/,
     & 5X,' TIME DEPENDING F=f(t-t0)',/,
     & 5X,'MASS. . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'STIFFNESS BEFORE SENSOR ACTIVATION. . .=',1PG20.13/,
     & 5X,'ACTIVATION SENSOR ID. . . . . . . . . .=',I10/,
     & 5X,'LOCK FEATURE. . . . . . . . . . . . . .=',I10/,
     & 5X,'FORCE(TIME-ACTIV_TIME) FUNCTION ID. . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13//)
 1004 FORMAT(
     & 5X,'NON LINEAR PRETENSION SPRING',/,
     & 5X,'----------------------------',/,
     & 5X,' DISPLACEMENT AND TIME DEPENDING F=g(t-t0)*f(x-x0)',/,
     & 5X,'MASS. . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'STIFFNESS BEFORE SENSOR ACTIVATION. . .=',1PG20.13/,
     & 5X,'ACTIVATION SENSOR ID. . . . . . . . . .=',I10/,
     & 5X,'FLAG FOR LOCK FEATURE ACTIVATION. . . .=',I10/,
     & 5X,'FORCE(DISP-ACTIV_DISP) FUNCTION ID. . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'F. SCALE FAC(TIME-ACTIV_T) FUNCTION ID.=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13//)
C
 1400 FORMAT(
     & 5X,'USER PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10)
C
 1500 FORMAT(
     & 5X,'USER PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10,
     & 5X,'CONFIDENTIAL DATA'//)
C
      END
Chd|====================================================================
Chd|  RINI32                        source/properties/spring/hm_read_prop32.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        AREA                          source/properties/spring/hm_read_prop32.F
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|        GET_U_PNU                     source/user_interface/uaccess.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RINI32(NEL   ,IOUT   ,IPROP ,
     3                  IX    ,XL     ,MASS  ,XINER  ,STIFM ,
     4                  STIFR ,VISCM  ,VISCR ,UVAR   ,NUVAR ,
     5                  ID,TITR,EINT,NPF,TF)
      USE MESSAGE_MOD
C-------------------------------------------------------------------------
C     This subroutine initialize springs using user properties.
C-------------------------------------------------------------------------
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C IPROP    |  1      | I | R | PROPERTY NUMBER
C----------+---------+---+---+--------------------------------------------
C IX       | 3*NEL   | I | R | SPRING CONNECTIVITY
C                            | IX(1,I) NODE 1 ID
C                            | IX(2,I) NODE 2 ID
C                            | IX(3,I) OPTIONNAL NODE 3 ID
C                            | IX(4,I) SPRING ID
C XL       |   NEL   | F | R | ELEMENT LENGTH
C----------+---------+---+---+--------------------------------------------
C MASS     |   NEL   | F | W | ELEMENT MASS
C XINER    |   NEL   | F | W | ELEMENT INERTIA (SPHERICAL)
C STIFM    |   NEL   | F | W | ELEMENT STIFNESS (TIME STEP)
C STIFR    |   NEL   | F | W | ELEMENT ROTATION STIFNESS (TIME STEP)
C VISCM    |   NEL   | F | W | ELEMENT VISCOSITY (TIME STEP)
C VISCR    |   NEL   | F | W | ELEMENT ROTATION VISCOSITY (TIME STEP)
C----------+---------+---+---+--------------------------------------------
C UVAR     |NUVAR*NEL| F | W | USER ELEMENT VARIABLES
C NUVAR    |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C----------+---------+---+---+--------------------------------------------
C-------------------------------------------------------------------------
C FUNCTION 
C-------------------------------------------------------------------------
C INTEGER II = GET_U_PNU(I,IP,KK)
C         IFUNCI = GET_U_PNU(I,IP,KFUNC)
C         IPROPI = GET_U_PNU(I,IP,KPROP)
C         IMATI = GET_U_PNU(I,IP,KMAT)
C         I     :     VARIABLE INDEX(1 for first variable,...)
C         IP    :     PROPERTY NUMBER
C         KK    :     PARAMETER KFUNC,KMAT,KPROP
C         THIS FUNCTION RETURN THE USER STORED FUNCTION(IF KK=KFUNC), 
C         MATERIAL(IF KK=KMAT) OR PROPERTY(IF KK=KPROP) NUMBERS. 
C         SEE LECG29 FOR CORRESPONDING ID STORAGE.
C-------------------------------------------------------------------------
C INTEGER IFUNCI = GET_U_MNU(I,IM,KFUNC)
C         I     :     VARIABLE INDEX(1 for first function)
C         IM    :     MATERIAL NUMBER
C         KFUNC :     ONLY FUNCTION ARE YET AVAILABLE.
C         THIS FUNCTION RETURN THE USER STORED FUNCTION NUMBERS(function 
C         refered by users materials).
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
C INTEGER MID = GET_U_PID(IP)
C         IP    :     PROPERTY NUMBER
C         THIS FUNCTION RETURN THE USER PROPERTY ID CORRESPONDING TO
C         USER PROPERTY NUMBER IP. 
C-------------------------------------------------------------------------
C INTEGER PID = GET_U_MID(IM)
C         IM   :     MATERIAL NUMBER
C         THIS FUNCTION RETURN THE USER MATERIAL ID CORRESPONDING TO
C         USER MATERIAL NUMBER IM. 
C-------------------------------------------------------------------------
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
#include      "scr17_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER IOUT,NUVAR,NEL,IPROP,   
     .        IX(4,NEL),NPF(*),KFUNC,
     .        GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU
      my_real 
     .        XL(NEL) ,MASS(NEL) ,XINER(NEL) ,STIFM(NEL) ,
     .        STIFR(NEL),VISCM(NEL) ,VISCR(NEL),UVAR(NUVAR,*),
     .        GET_U_MAT,GET_U_GEO,EINT(*),TF(*)
C-----------------------------------------------
      EXTERNAL GET_U_PNU,GET_U_MNU,GET_U_MAT,GET_U_GEO,GET_U_PID,
     .         GET_U_MID
      PARAMETER (KFUNC=29)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     .        AMAS,STIF0,STIF1,F1,TSCAL,DSCAL,FSCAL,
     .        D1,X,Y,X2,Y2,X3,Y3,F0
      INTEGER I , J, ITYP, IFUNC1, IFUNC2, NPOINT
C=======================================================================
        AMAS   = GET_U_GEO(1,IPROP)
        STIF0  = GET_U_GEO(2,IPROP)
        STIF1  = GET_U_GEO(3,IPROP)
        F1     = GET_U_GEO(4,IPROP)
        ITYP   = GET_U_GEO(6,IPROP)
        TSCAL  = GET_U_GEO(7,IPROP)    
        DSCAL  = GET_U_GEO(8,IPROP)    
        FSCAL  = GET_U_GEO(9,IPROP)    
        D1     = GET_U_GEO(11,IPROP)
C
C       MEAN VALUES
C
C--------------------------------------
C       ELEMENT CHECK
C--------------------------------------
        DO I=1,NEL
          IF(XL(I).EQ.0.0)THEN
             CALL ANCMSG(MSGID=406,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IX(4,I))
          ENDIF
        ENDDO
C--------------------------------------
C       ELEMENT INITIALIZATION
C--------------------------------------
        DO I=1,NEL
C         Compute initial internal energy
          EINT(I) = 0
          IF (ITYP == 1) THEN
C           Linear case
            IF (D1==0) THEN
              EINT(I) = EINT(I) + (F1/STIF0)*F1/2
            ELSE
              EINT(I) = EINT(I) + ABS(D1*F1)/2
            ENDIF
          ELSEIF (ITYP == 2) THEN
C           Non linear case with only space IFUNC1 defined
            IFUNC1   = GET_U_PNU(1,IPROP,KFUNC)
            NPOINT=(NPF(IFUNC1+1)-NPF(IFUNC1))/2
            X2=DSCAL*TF(NPF(IFUNC1)+2*0+0)
            Y2=FSCAL*TF(NPF(IFUNC1)+2*0+1)
            X3=DSCAL*TF(NPF(IFUNC1)+2*0+2)
            Y3=FSCAL*TF(NPF(IFUNC1)+2*0+3)
            IF (D1==0.AND.Y2<=0) D1=-1e30
            IF (D1>0.AND.Y2>0.AND.X2>D1) THEN
              Y=Y2+(Y3-Y2)/(X3-X2)*(D1-X2)
              X=D1
              CALL AREA(D1,X,X2,Y,Y2,EINT(I),STIF0)
            ENDIF
            DO J=0,NPOINT-2
              X=DSCAL*TF(NPF(IFUNC1)+2*J)
              Y=FSCAL*TF(NPF(IFUNC1)+2*J+1)
              X2=DSCAL*TF(NPF(IFUNC1)+2*J+2)
              Y2=FSCAL*TF(NPF(IFUNC1)+2*J+3)
              IF (X<0) CALL AREA(D1,X,X2,Y,Y2,EINT(I),STIF0)
            ENDDO
          ELSEIF (ITYP == 3) THEN
C           Non linear case with only time IFUNC2 defined          
C           Warning: initial internal energy computed with STIF0 when no stiffness is defined 
            IFUNC2   = GET_U_PNU(2,IPROP,KFUNC)
            F0=FSCAL*TF(NPF(IFUNC2)+1)
            EINT(I) = EINT(I) + (F0/STIF0)*F0/2
          ELSEIF (ITYP == 4) THEN 
C           Non linear case with both space IFUNC1 and time IFUNC2 defined          
            IFUNC1   = GET_U_PNU(1,IPROP,KFUNC)
            IFUNC2   = GET_U_PNU(2,IPROP,KFUNC)
            F0=FSCAL*TF(NPF(IFUNC2)+1)
            NPOINT=(NPF(IFUNC1+1)-NPF(IFUNC1))/2
            IF (D1==0) D1=-1e30
            IF (D1>0.AND.Y2>0.AND.X2>D1) THEN
              Y=Y2+(Y3-Y2)/(X3-X2)*(D1-X2)
              X=D1
              CALL AREA(D1,X,X2,Y,Y2,EINT(I),STIF0)
            ENDIF
            DO J=0,NPOINT-2
              X=DSCAL*TF(NPF(IFUNC1)+2*J)
              Y=F0*TF(NPF(IFUNC1)+2*J+1)
              X2=DSCAL*TF(NPF(IFUNC1)+2*J+2)
              Y2=F0*TF(NPF(IFUNC1)+2*J+3)
              IF (X<0) CALL AREA(D1,X,X2,Y,Y2,EINT(I),STIF0)
            ENDDO           
          ENDIF
          MASS(I)   = AMAS
          XINER(I)  = 0.
          UVAR(1,I) = 0.
          UVAR(2,I) = 0.
          UVAR(3,I) = 0.
          UVAR(4,I) = 0.
C FOR NODAL AND ELEMENT TIME STEP COMPUTATION
          STIFM(I) = STIF0 + STIF1
          STIFR(I) = 0.
          VISCM(I) = 0.
          VISCR(I) = 0.
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  AREA                          source/properties/spring/hm_read_prop32.F
Chd|-- called by -----------
Chd|        RINI32                        source/properties/spring/hm_read_prop32.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE AREA(D1,X,X2,Y,Y2,EINT,STIF0)
      USE MESSAGE_MOD
C-------------------------------------------------------------------------
C     This subroutine compute the area under the curve (X,Y=F(X));(X2,Y2=F(X2)).
C-------------------------------------------------------------------------
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      my_real 
     .        D1,X,X2,Y,Y2,EINT,STIF0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     .        X0,FD1
C=======================================================================
C     Only positive area computed and if X <= D1 (spring length)
      IF (D1>=X2)THEN
      ELSEIF (D1>=X) THEN
        FD1 = Y+(Y2-Y)/(X2-X)*(D1-X)
        IF (Y<0.AND.Y2>0) THEN
          X0 = X-Y*(X2-X)/(Y2-Y)
          IF (D1<=X0) THEN
            EINT = EINT + (X2-X0)*Y2/2
          ELSE
            EINT = EINT + (X2-D1)*FD1+(X2-D1)*(Y2-FD1)/2
            EINT = EINT + (FD1/STIF0)*FD1/2
          ENDIF
        ELSEIF (Y>=0.AND.Y2>0) THEN
          EINT = EINT + (X2-D1)*FD1+(X2-D1)*(Y2-FD1)/2
          EINT = EINT + (FD1/STIF0)*FD1/2
        ENDIF
      ELSEIF (Y>=0.AND.Y2>0) THEN
        EINT = EINT + (X2-X)*Y+(X2-X)*(Y2-Y)/2
      ENDIF
      RETURN
      END
