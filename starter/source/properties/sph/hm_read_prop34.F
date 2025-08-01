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
!||    hm_read_prop34           ../starter/source/properties/sph/hm_read_prop34.F
!||--- called by ------------------------------------------------------
!||    hm_read_prop_generic     ../starter/source/properties/hm_read_prop_generic.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    set_u_geo                ../starter/source/user_interface/uaccess.F
!||    set_u_pnu                ../starter/source/user_interface/uaccess.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_PROP34(GEO, IGEO ,IOUT  ,NUVAR ,PARGEO,
     .                          QA, QB    ,ISKN  ,IG    ,TITR  ,UNITAB,
     .                          PROP_TAG,IGTYP,LSUBMODEL)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD  
      USE SUBMODEL_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
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
      INTEGER IOUT,NUVAR,ISKN(LISKN,*),IG,IGTYP,IGEO(*)
      CHARACTER(LEN=NCHARTITLE)::TITR
      my_real
     .        PARGEO(*), GEO(*)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      INTEGER SET_U_PNU,SET_U_GEO
      EXTERNAL SET_U_PNU,SET_U_GEO
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C=======================================================================
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IERROR,IORDER,ISK,K
      my_real
     .        XK,MP,QA,QB,ALPCS,XORDER,DIST,PUN,ZSTAB,
     .        HMIN,HMAX,H_SCAL
      INTEGER IFLG_H
      my_real
     .   H_DILAT_COEFF,RFLG_H
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA PUN/0.1/
C-----------------------------------------------

      NUVAR=0
      PARGEO(1) = 0
      XK=0.0
      PARGEO(2)=XK
C     front wave=1
      PARGEO(3) =1
C-------
      ISK=0
      IFLG_H=0
      IORDER=-1
      DIST  =ZERO
      ZSTAB =ZERO
C
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('SKEW_CSID',ISK,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('h_1D',IFLG_H,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ORDER',IORDER,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('MASS',MP,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('qa',QA,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('qb',QB,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ALPHA1',ALPCS,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('h',DIST,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Xi_Stab',ZSTAB,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('hmin',HMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('hmax',HMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('h_scal',H_SCAL,IS_AVAILABLE,LSUBMODEL,UNITAB)
C----------------------
C
      WRITE(IOUT,1100) IG

      IF(QA==ZERO)QA=TWO
      IF(QB==ZERO)QB=ONE
      IF (ZSTAB>0) NSPBUF=15
C      
      IF (IFLG_H == 3) THEN
        IF(HMIN==ZERO) HMIN = ZEP2
        IF(HMAX==ZERO) HMAX = TWO
        IF(H_SCAL==ZERO) H_SCAL = ONEP2
      ENDIF  
C
      IF(ISK /= 0)THEN
        DO K=1,NUMSKW
          IF(ISK == ISKN(4,K+1)) THEN
            PARGEO(1)=(K+1)+PUN
            GO TO 100
          ENDIF
        ENDDO
        CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              C1='PROPERTY',
     .              C2='PROPERTY',
     .              I1=IG,I2=ISK,C3=TITR)
100     CONTINUE
      ELSE
        PARGEO(1)=ZERO
        K = 1
      ENDIF
C------
C      IF(NSPMD > 1 .AND. IORDER==1)THEN
C        CALL ANCMSG(MSGID=755,
C     .              MSGTYPE=MSGERROR,
C     .              ANMODE=ANINFO,
C     .              C1='SPH CORRECTION ORDER 1')
C      END IF
C
      RFLG_H = IFLG_H
C
      IF (IFLG_H==1) THEN
        H_DILAT_COEFF = ONE
      ELSEIF (IFLG_H==2) THEN
        H_DILAT_COEFF = ZERO
      ELSE
        H_DILAT_COEFF = THIRD
      ENDIF      
C
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'  CONFIDENTIAL DATA'
      ELSE
        IF(DIST==ZERO)THEN
          WRITE(IOUT,1000)MP,QA,QB,ALPCS,ZSTAB,ISK,IORDER
          IF (IFLG_H==3) THEN
            WRITE(IOUT,1005)
          ELSE   
            WRITE(IOUT,1004)
          ENDIF   
        ELSE
         WRITE(IOUT,1001)MP,QA,QB,ALPCS,ZSTAB,ISK,IORDER,DIST
        ENDIF
        IF (IFLG_H==1) THEN
          WRITE(IOUT,1002)
        ELSEIF (IFLG_H==2) THEN
          WRITE(IOUT,1003)
        ELSEIF (IFLG_H==3) THEN
          WRITE(IOUT,1006) HMIN,HMAX,H_SCAL
        END IF
      ENDIF
        
      IF(MP<=ZERO)THEN
        CALL ANCMSG(MSGID=138,ANMODE=ANINFO,MSGTYPE=MSGWARNING,
     .              C1=TITR,I1=IG)
        MP=ONE
      ENDIF                      
C        
      IERROR = SET_U_GEO(1,MP)
      IERROR = SET_U_GEO(2,QA)
      IERROR = SET_U_GEO(3,QB)
      IERROR = SET_U_GEO(4,ALPCS)
      XORDER = IORDER+EM01
      IERROR = SET_U_GEO(5,XORDER)
      IERROR = SET_U_GEO(6,DIST)
      IERROR = SET_U_GEO(7,ZSTAB)
      IERROR = SET_U_GEO(8,H_DILAT_COEFF)
      IERROR = SET_U_GEO(9,RFLG_H)     
      IERROR = SET_U_GEO(10,HMIN)
      IERROR = SET_U_GEO(11,HMAX)
      IERROR = SET_U_GEO(12,H_SCAL)
C
      GEO(14)=QA
      GEO(15)=QB
      IF (GEO(16) /= ZERO .OR. GEO(17) /= ZERO) THEN
        IGEO(33) = 1   ! ISVIS flag
      ENDIF         
C
      PROP_TAG(IGTYP)%G_SIG  = 6
      PROP_TAG(IGTYP)%G_VOL  = 1
      PROP_TAG(IGTYP)%G_EINT = 1
      PROP_TAG(IGTYP)%G_QVIS = 1
      PROP_TAG(IGTYP)%L_SIG  = 6
      PROP_TAG(IGTYP)%L_EINT = 1
      PROP_TAG(IGTYP)%L_VOL  = 1
      PROP_TAG(IGTYP)%L_QVIS = 1
C
      RETURN
C------
 999  CONTINUE
C      WRITE(IOUT,*)' **ERROR IN SPH PROPERTY INPUT.'
C      IERR=IERR+1
C      CALL ARRET(2)
      CALL ANCMSG(MSGID=401,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=IG,
     .            C2=TITR,
     .            C1='SPH')
      RETURN
C------
 1000 FORMAT(
     & 5X,'PARTICLES MASS. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'QA. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'QB. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'ALPCS . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'COEFFICIENT WRT TENSILE INSTABILITY . .=',1PG20.13/,
     & 5X,'ORTHOTROPIC INITIAL SKEW SYSTEM . . . .=',I10/,
     & 5X,'FORMULATION CORRECTION ORDER. . . . . .=',I10/,
     & 5X,'SMOOTHING LENGTH AUTOMATICALLY COMPUTED')
 1001 FORMAT(
     & 5X,'PARTICLES MASS. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'QA. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'QB. . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'ALPCS . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'COEFFICIENT WRT TENSILE INSTABILITY . .=',1PG20.13/,
     & 5X,'ORTHOTROPIC INITIAL SKEW SYSTEM . . . .=',I10/,
     & 5X,'FORMULATION CORRECTION ORDER. . . . . .=',I10/,
     & 5X,'SMOOTHING LENGTH. . . . . . . . . . . .=',1PG20.13)
 1002 FORMAT(
     & 5X,'UNIAXIAL DILATATION OF SMOOTHING LENGTH')
 1003 FORMAT(
     & 5X,'CONSTANT SMOOTHING LENGTH')
 1004 FORMAT(
     & 5X,'SMOOTHING LENGTH COMPUTED FROM PARTICLE MASS')
 1005 FORMAT(
     & 5X,'SMOOTHING LENGTH COMPUTED FROM INTERPARTICLE DISTANCE')    
 1006 FORMAT(
     & 5X,'BOUNDED DILATATION OF SMOOTHING LENGTH'/,
     & 5X,'MINIMUM DILATATION RATIO . . . . . . . =',1PG20.13/,
     & 5X,'MAXIMUM DILATATION RATIO . . . . . . . =',1PG20.13/,  
     & 5X,'SMOOTHING LENGTH SCALING FACTOR. . . . =',1PG20.13)        
 1100 FORMAT(
     & 5X,'SPH PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10)
      END
