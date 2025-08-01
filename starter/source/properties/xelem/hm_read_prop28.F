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
C     This subroutine is organized as a user routine.
!||====================================================================
!||    hm_read_prop28             ../starter/source/properties/xelem/hm_read_prop28.F
!||--- called by ------------------------------------------------------
!||    hm_read_prop_generic       ../starter/source/properties/hm_read_prop_generic.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                     ../starter/source/output/message/message.F
!||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_floatv              ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_get_string_index        ../starter/source/devtools/hm_reader/hm_get_string_index.F
!||    hm_option_is_encrypted     ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    set_u_geo                  ../starter/source/user_interface/uaccess.F
!||    set_u_pnu                  ../starter/source/user_interface/uaccess.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod               ../starter/share/modules1/elbuftag_mod.F
!||    message_mod                ../starter/share/message_module/message_mod.F
!||    submodel_mod               ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_PROP28(IOUT  ,NUVAR ,PARGEO, UNITAB,ID,
     .                          TITR  ,IGTYP ,PROP_TAG,LSUBMODEL,IUNIT)
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD
      USE SUBMODEL_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARLINE, NCHARFIELD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tablen_c.inc"
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IIN      |  1      | I | R | INPUT FILE UNIT (D00 file) 
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C NUVAR    |  2      | I | R | NUMBER OF USER ELEMENT VARIABLES
C                            | NUMBER OF USER ELEMENT VARIABLES PER NODE
C----------+---------+---+---+--------------------------------------------
C PARGEO   |  *      | F | W | 1)SKEW NUMBER
C          |         |   |   | 2)STIFNESS FOR INTERFACE
C          |         |   |   | 3)FRONT WAVE OPTION
C          |         |   |   | 4)... not yet used
C----------+---------+---+---+--------------------------------------------
C
C     This subroutine reads the user geometry parameters.
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
C     SET_U_PNU(sub_prop_index,sub_prop_id,KPROP).
C
C     SET_U_GEO and SET_U_PNU return 0 if no error 
C     SET_U_GEO and SET_U_PNU return the maximum allowed index 
C     if index is larger than this maximum
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IOUT,NUVAR(*),IGTYP,IUNIT
      my_real PARGEO(*)
      INTEGER SET_U_PNU,SET_U_GEO,
     .        KFUNC
      EXTERNAL SET_U_PNU,SET_U_GEO
      PARAMETER (KFUNC=29)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IERROR,I
      INTEGER IFUNC, IFV, IVTYP, NIP
      my_real 
     .  XK,XVTYP,RHO, XC, DMN, DMX, MU1, MU2, FRIC,Y_SCAL,X_SCAL
      my_real
     .   FAC_M, FAC_L, FAC_T
      CHARACTER(LEN=NCHARFIELD) :: KEYWORD
      CHARACTER(LEN=NCHARLINE) :: CART
      INTEGER IP
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED

C=======================================================================
C
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('FUN_A1',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B1',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('NIP',NIP,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('MASS',RHO,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STIFF2',XK,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DAMP2',XC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STRAIN1',DMN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STRAIN2',DMX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FScale11',Y_SCAL,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FScale22',X_SCAL,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE1',MU1,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_MUE2',MU2,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
C----------------------
C
      WRITE(IOUT,1400) ID
C
      CART=' '
      PARGEO(1) = 0
C   secnd only in interfaces.
        PARGEO(2) = 0.0
C------------------------------------------------------- 
      NUVAR(1) = 3
      NUVAR(2) = 5   
C   
      FAC_M = UNITAB%FAC_M(IUNIT)
      FAC_L = UNITAB%FAC_L(IUNIT)
      FAC_T = UNITAB%FAC_T(IUNIT)
C
      IF (DMN == ZERO) DMN=-EP30
      IF (DMX == ZERO) DMX= EP30
      IF (Y_SCAL == ZERO) Y_SCAL = ONE * FAC_M * FAC_L / ( FAC_T * FAC_T )
      IF (X_SCAL == ZERO) X_SCAL = ONE / FAC_T 
C      
        IERROR = SET_U_GEO(10,MU1)
        IF (IERROR>0) THEN
           CALL ANCMSG(MSGID=378,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(10,MU1)')
        ENDIF
        IERROR = SET_U_GEO(11,MU2)
        IF (IERROR>0) THEN
           CALL ANCMSG(MSGID=378,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(11,MU2)')
        ENDIF
        IERROR = SET_U_GEO(3,RHO)
        IF (IERROR>0) THEN
           CALL ANCMSG(MSGID=378,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(3,RHO)')
        ENDIF
        IERROR = SET_U_GEO(4,XK)
        IF (IERROR>0) THEN
           CALL ANCMSG(MSGID=378,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(4,XK)')
        ENDIF
        IERROR = SET_U_GEO(5,XC)
        IF (IERROR>0) THEN
           CALL ANCMSG(MSGID=378,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(5,XC)')
        ENDIF
        IERROR = SET_U_PNU(1,IFUNC,KFUNC)
        IF (IERROR>0) THEN
           CALL ANCMSG(MSGID=378,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_PNU(1,IFUNC,KFUNC)')
        ENDIF
        IERROR = SET_U_PNU(2,IFV,KFUNC)
        IF (IERROR>0) THEN
           CALL ANCMSG(MSGID=378,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_PNU(2,IFV,KFUNC)')
        ENDIF
        IVTYP=100
        XVTYP=IVTYP
        IERROR = SET_U_GEO(7,XVTYP)
        IF (IERROR>0) THEN
           CALL ANCMSG(MSGID=378,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(7,XVTYP)')
        ENDIF
        IERROR = SET_U_GEO(8,DMN)
        IF (IERROR>0) THEN
           CALL ANCMSG(MSGID=378,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(8,DMN)')
        ENDIF
        IERROR = SET_U_GEO(9,DMX)
        IF (IERROR>0) THEN
           CALL ANCMSG(MSGID=378,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(9,DMX)')
        ENDIF
        IF(RHO==0.)THEN
           CALL ANCMSG(MSGID=423,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                   I1=ID,
     .                   C1=TITR)
        ENDIF
C 
        IERROR = SET_U_GEO(12,Y_SCAL)
        IF (IERROR > 0) THEN
           CALL ANCMSG(MSGID=378,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(12,Y_SCAL)')
        ENDIF
        IERROR = SET_U_GEO(13,ONE/X_SCAL)
        IF (IERROR > 0) THEN
           CALL ANCMSG(MSGID=378,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(13,X_SCAL)')
        ENDIF
C
        IF(.NOT. IS_ENCRYPTED)THEN 
          WRITE(IOUT,3000)  RHO,XK,XC,IFUNC,IFV,Y_SCAL,X_SCAL,
     .                    DMN,DMX,MU1,MU2
        ELSE
          WRITE(IOUT,'(5X,A)')'  NSTRAND PROPERTY SET'
          WRITE(IOUT,'(5X,A,/)')'  --------------------'
          WRITE(IOUT,'(5X,A,//)')'  CONFIDENTIAL DATA'
        ENDIF
C
        DO I=1,475
C      non user specified coefficient is set to negative value.
         FRIC=-1.0
         IERROR=SET_U_GEO(50+I,FRIC)
         IF (IERROR>0) THEN
            CALL ANCMSG(MSGID=378,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(50+I,FRIC)')
         ENDIF
         IERROR=SET_U_GEO(525+I,FRIC)
         IF (IERROR>0) THEN
            CALL ANCMSG(MSGID=378,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(525+I,FRIC)')
         ENDIF
        ENDDO
C
        WRITE(IOUT,4000)
C
        DO IP=1,NIP
C
          CALL HM_GET_STRING_INDEX('NAME_ARRAY',KEYWORD,IP,6,IS_AVAILABLE)
          CALL HM_GET_INT_ARRAY_INDEX('Nb1_arr',I,IP,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('Mu_arr',FRIC,IP,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
          IF (KEYWORD(1:6)=='PULLEY') THEN
            IF (I<2.OR.I>475) THEN
              CALL ANCMSG(MSGID=379,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   C2='PULLEY',
     .                   I2=I)
            ENDIF
            IERROR=SET_U_GEO(50+I,FRIC)
            IF (IERROR>0) THEN
              CALL ANCMSG(MSGID=378,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(50+I,FRIC)')
            ENDIF
            IF(.NOT. IS_ENCRYPTED) WRITE(IOUT,'(A,I10,A,1PG20.13)')
     .                           'PULLEY FRICTION COEFFICIENT : PULLEY NUMBER =',I,
     .                           'VALUE =',FRIC
          ELSEIF (KEYWORD(1:6)=='STRAND') THEN
             IF (I<1.OR.I>475) THEN
               CALL ANCMSG(MSGID=379,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   C2='STRAND',
     .                   I2=I)
             ENDIF
             IERROR=SET_U_GEO(525+I,FRIC)
             IF (IERROR>0) THEN
               CALL ANCMSG(MSGID=378,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 C2='SET_U_GEO(525+I,FRIC)')
             ENDIF
c
             IF(.NOT. IS_ENCRYPTED) WRITE(IOUT,'(A,I10,A,1PG20.13)')
     .                            'STRAND FRICTION COEFFICIENT : STRAND NUMBER =',I,
     .                            'VALUE =',FRIC
          ELSE
            CALL ANCMSG(MSGID=380,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                 C1=TITR,
     .                 C2=KEYWORD)
          ENDIF
C
        ENDDO
C
C-----------------------------
C       PROPERTY BUFFER 
C-----------------------------
        PROP_TAG(IGTYP)%G_EINT = 1
        PROP_TAG(IGTYP)%G_MASS = 1
        PROP_TAG(IGTYP)%G_NUVAR  = NUVAR(1)
C        PROP_TAG(IGTYP)%G_NUVARN = NINT(GEO(35,I)) ! init. in elbuf_ini

C---------------------------------------------------------------
      RETURN
 999  CALL ANCMSG(MSGID=606,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            C1=CART)
 3000 FORMAT(
     & 5X,'  NSTRAND PROPERTY SET   ',/,
     & 5X,'  --------------------   ',/,
     & 5X,'MASS PER LENGTH UNIT. . . . . . . . . .=',1PG20.13/,
     & 5X,'UNITARY STIFFNESS . . . . . . . . . . .=',1PG20.13/,
     & 5X,'UNITARY DAMPING . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FORCE/STRAIN CURVE NUMBER . . . . . . .=',I10/,
     & 5X,'DYNAMIC AMPLIFICATION CURVE NUMBER. . .=',I10/,
     & 5X,'FORCE SCALE FACTOR. . . . . . . . . . .=',1PG20.13/,
     & 5X,'STRAIN RATE SCALE FACTOR. . . . . . . .=',1PG20.13/,
     & 5X,'NEGATIVE FAILURE STRAIN . . . . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE STRAIN . . . . . . . .=',1PG20.13/,
     & 5X,'PULLEY FRICTION DEFAULT COEFFICIENT . .=',1PG20.13/,
     & 5X,'STRAND FRICTION DEFAULT COEFFICIENT . .=',1PG20.13/)
 4000 FORMAT(
     & 5X,'  SPECIFIED FRICTION COEFFICIENTS : ',/,
     & 5X,'  +++++++++++++++++++++++++++++++   ',/)
C
 1400 FORMAT(
     & 5X,'USER PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10)
c
      END
