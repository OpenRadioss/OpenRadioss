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
Chd|  HM_READ_PROP36                source/properties/spring/hm_read_prop36.F
Chd|-- called by -----------
Chd|        HM_READ_PROP_GENERIC          source/properties/hm_read_prop_generic.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        SET_U_GEO                     source/user_interface/uaccess.F
Chd|        SET_U_PNU                     source/user_interface/uaccess.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP36(IOUT,NUVAR ,PARGEO, UNITAB,
     .                          ISKN,IG,TITR,IGTYP,PROP_TAG,GEO,LSUBMODEL,SUB_ID)
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
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "tablen_c.inc"
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IOUT,NUVAR,ISKN(LISKN,*),IG,IGTYP,SUB_ID
      my_real GEO(*),PARGEO(*)
      CHARACTER TITR*nchartitle
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      INTEGER SET_U_PNU,SET_U_GEO,
     .        KFUNC,KUMAT,KUPROP
      EXTERNAL SET_U_PNU,SET_U_GEO
      PARAMETER (KFUNC=29)
      PARAMETER (KUMAT=31)
      PARAMETER (KUPROP=33)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C=======================================================================
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISK,IUTYP,PID1,PID2,MID1,IERROR,K
      my_real
     .  XK,AREA,IXX,IYY,IZZ,AA,RAY,ry,rz
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
      CALL HM_GET_INTV('P36_lutype',IUTYP,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
      IF(IUTYP==1)THEN
C-------------------------------------------------------
C       USER SUB TYPE 1 PROPERTY REFERENCED BY A SPRING PART
C       THIS PROPERTY REFERS TO 2 USER PROPERTIES
C-------------------------------------------------------
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('SKEW_CSID',ISK,IS_AVAILABLE,LSUBMODEL)
        IF(ISK == 0 .AND. SUB_ID /= 0 ) ISK = LSUBMODEL(SUB_ID)%SKEW
        CALL HM_GET_INTV('PROP_ID1',PID1,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('PROP_ID2',PID2,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('Xk',XK,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
        NUVAR     = 15
C
C PID1 and PID2 are USER property IDs
C
        IERROR = SET_U_PNU(1,PID1,KUPROP)
        IERROR = SET_U_PNU(2,PID2,KUPROP)
C
        DO K=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
          IF(ISK == ISKN(4,K+1)) THEN
            ISK=K+1
            GO TO 100
          ENDIF
        ENDDO
        CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              C1='PROPERTY',
     .              C2='PROPERTY',
     .              I1=IG,I2=ISK,C3=TITR)
100     CONTINUE
C
        PARGEO(1) = ISK
        PARGEO(2) = XK
C
        IF(IS_ENCRYPTED)THEN
          WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
        ELSE
          WRITE(IOUT,1000)ISKN(4,ISK),PID1,PID2,XK
        ENDIF
C
      ELSEIF(IUTYP==2)THEN
C-------------------------------------------------------
C       USER SUB TYPE 2 PROPERTY REFERENCED BY A USER SUB TYPE 1 PROPERTY
C       THIS PROPERTY REFERS TO 1 USER MATERIAL
C-------------------------------------------------------
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('MAT_ID',MID1,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('AREA',AREA,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('IXX',IXX,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('IYY',IYY,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('IZZ',IZZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('RAY',RAY,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
C MID1 is a USER material ID
        IERROR = SET_U_PNU(1,MID1,KUMAT)
C
         IF(RAY==0.AND.AREA/=0) THEN
           IF(IXX==0.OR.IYY==0.OR.IZZ==0) THEN
              CALL ANCMSG(MSGID=640,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=IG,
     .                    C1=TITR)
           ENDIF
         ENDIF
C
        IF ((AREA<=ZERO).AND.(RAY<=ZERO)) THEN
          WRITE(IOUT,*)' ** ERROR : PROPERTY INPUT '
          IF(.NOT. IS_ENCRYPTED)THEN
           WRITE(IOUT,*)' AREA =',AREA,' R =',RAY
          ENDIF
        ENDIF
C
        IF ((AREA<=ZERO).AND.(RAY/=ZERO)) THEN
         AREA=RAY*RAY*PI
C
         IXX=AREA*RAY*RAY*HALF
         IYY=HALF*IXX

         IZZ=IYY
         RY=RAY
         RZ=RAY
        ELSE
C
         RY=SQRT(FOUR*IYY/AREA)
         RZ=SQRT(FOUR*IZZ/AREA)
        ENDIF
C
        AA = IUTYP
        IERROR = SET_U_GEO(1,AA)
        IERROR = SET_U_GEO(2,AREA)
        IERROR = SET_U_GEO(3,IXX)
        IERROR = SET_U_GEO(4,IYY)
        IERROR = SET_U_GEO(5,IZZ)
        IERROR = SET_U_GEO(6,RY)
        IERROR = SET_U_GEO(7,RZ)
C
        IF(IS_ENCRYPTED)THEN
          WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
        ELSE
          WRITE(IOUT,2000)MID1,AREA,IXX,IYY,IZZ,RY,RZ
        ENDIF

      ENDIF
C
      GEO(25) = NUVAR
      PROP_TAG(IGTYP)%G_EINT = 1
      PROP_TAG(IGTYP)%G_FOR  = 3
      PROP_TAG(IGTYP)%G_MOM = 5
      PROP_TAG(IGTYP)%G_SKEW = 6
      PROP_TAG(IGTYP)%G_MASS = 1
      PROP_TAG(IGTYP)%G_V_REPCVT  = 3 ! -- VITESSES REPERE CONVECTEE (V_REPCVT)
      PROP_TAG(IGTYP)%G_VR_REPCVT = 3  ! -- VITESSES REPERE CONVECTEE (VR_REPCVT)
C   if (IUTYP = 1 .or. NINT(GEO(25,I)) > 0 )  --> see lecg36.F
      IF(NINT(GEO(25)) > 0) PROP_TAG(IGTYP)%G_NUVAR = NINT(GEO(25))
C
      RETURN
 999  CONTINUE
      WRITE(IOUT,*)' **ERROR IN PROPERTY 36 INPUT'
      RETURN
 1000 FORMAT(
     & 5X,'  USER PROPERTY TYPE 1 (used by spring elements) ',/,
     & 5X,'  --------------------  ',//,
     & 5X,'SKEW ID . . . . . . . . . . . . . . . .=',I10/
     & 5X,'FIRST END TYPE 2 USER PROPERTY ID . . .=',I10/
     & 5X,'SECOND END TYPE 2 USER PROPERTY ID. . .=',I10/
     & 5X,'STIFFNESS FOR INTERFACE . . . . . . . .=',1PG20.13//)
 2000 FORMAT(
     & 5X,'  USER PROPERTY TYPE 2 (used by property type 1)  ',/,
     & 5X,'  --------------------  ',//,
     & 5X,'USER MATERIAL ID. . . . . . . . . . . .=',I10/,
     & 5X,'AREA. . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'TORSION SECTION INERTIA . . . . . . . .=',1PG20.13/,
     & 5X,'BENDING SECTION INERTIA IYY. . . . . . .=',1PG20.13/,
     & 5X,'BENDING SECTION INERTIA IZZ. . . . . . .=',1PG20.13/,
     & 5X,'BENDING SECTION  RAYON  RY . . . . . . .=',1PG20.13/,
     & 5X,'BENDING SECTION  RAYON  RZ . . . . . . .=',1PG20.13//)
 3000 FORMAT(
     & 5X,'USER PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10)
      END
Chd|====================================================================
Chd|  RINI36                        source/properties/spring/hm_read_prop36.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|        GET_U_MAT                     source/user_interface/uaccess.F
Chd|        GET_U_PID                     source/user_interface/uaccess.F
Chd|        GET_U_PNU                     source/user_interface/uaccess.F
Chd|====================================================================
      SUBROUTINE RINI36(NEL   ,IOUT   ,IPROP ,
     3                  IX    ,XL     ,MASS  ,XINER  ,STIFM ,
     4                  STIFR ,VISCM  ,VISCR ,UVAR   ,NUVAR )
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
C                            | IX(3,I) OPTIONAL NODE 3 ID
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
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER IOUT,NUVAR,NEL,IPROP,
     .        IX(4,NEL) ,
     .        GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU,
     .        KFUNC,KMAT,KPROP
      my_real
     .        XL(NEL) ,MASS(NEL) ,XINER(NEL) ,STIFM(NEL) ,
     .        STIFR(NEL),VISCM(NEL) ,VISCR(NEL),UVAR(NUVAR,*),
     .        GET_U_MAT,GET_U_GEO
      EXTERNAL GET_U_PNU,GET_U_MNU,GET_U_MAT,GET_U_GEO,GET_U_PID,GET_U_MID
      PARAMETER (KFUNC=29)
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=33)
C=======================================================================
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .        FAC,RHO,AREA,IXX,IYY,IZZ,IMYZ,YOUNG,G,
     .        AREA1,IXX1,IYY1,IZZ1,RHO1,YOUNG1,G1,
     .        AREA2,IXX2,IYY2,IZZ2,RHO2,YOUNG2,G2,
     .        RY1,RZ1,RY2,RZ2,RY,RZ,xl3,ktran,krot,
     .        K11,K22,K26,K33,K35,K44,K55,K5B,K66,K6C,
     .        dt1,dt2,XL2,ATMP,ARY,ARZ
      INTEGER I,IUTYP,n0,
     .        IMAT1,IPROP1,IUTYP1,
     .        IMAT2,IPROP2,IUTYP2
C-----------------------------------------------
C
        n0=11
        IPROP1 = GET_U_PNU(1,IPROP,KPROP)
        IPROP2 = GET_U_PNU(2,IPROP,KPROP)
C
C       first end
C
        IUTYP1 = NINT(GET_U_GEO(1,IPROP1))
        WRITE(IOUT,*)' **VALUE OF IUTYP1',IUTYP1
        IF(IUTYP1/=2)THEN
          WRITE(IOUT,*)' **ERROR SPRING USER PROPERTY',
     .       GET_U_PID(IPROP),' REFERS TO WRONG USER PROPERTY',
     .       GET_U_PID(IPROP1)
        ENDIF
        AREA1  = GET_U_GEO(2,IPROP1)
        IXX1   = GET_U_GEO(3,IPROP1)
        IYY1   = GET_U_GEO(4,IPROP1)
        IZZ1   = GET_U_GEO(5,IPROP1)
        RY1    = GET_U_GEO(6,IPROP1)
        RZ1    = GET_U_GEO(7,IPROP1)
        IMAT1  = GET_U_PNU(1,IPROP1,KMAT)
        YOUNG1 = GET_U_MAT(7,IMAT1)
        G1     = GET_U_MAT(6,IMAT1)
        RHO1   = GET_U_MAT(0,IMAT1)
C
C       second end
C
        IUTYP2 = NINT(GET_U_GEO(1,IPROP2))
        IF(IUTYP2/=2)THEN
          WRITE(IOUT,*)' **ERROR SPRING USER PROPERTY',
     .       GET_U_PID(IPROP),' REFERS TO WRONG USER PROPERTY',
     .       GET_U_PID(IPROP2)
        ENDIF
        AREA2  = GET_U_GEO(2,IPROP2)
        IXX2   = GET_U_GEO(3,IPROP2)
        IYY2   = GET_U_GEO(4,IPROP2)
        IZZ2   = GET_U_GEO(5,IPROP2)
        RY2    = GET_U_GEO(6,IPROP2)
        RZ2    = GET_U_GEO(7,IPROP2)
C
C SEE LECM29 FOR USER MATERIAL PARAMETER STORAGE (RHO IS ALWAYS AT 0)
C
        IMAT2  = GET_U_PNU(1,IPROP2,KMAT)
        YOUNG2 = GET_U_MAT(7,IMAT2)
        G2     = GET_U_MAT(6,IMAT2)
        RHO2   = GET_U_MAT(0,IMAT2)
C
C       MEAN VALUES
C
        AREA   = HALF*(AREA1+AREA2)
        RHO    = HALF*(RHO1+RHO2)
        FAC    = AREA*RHO
        IXX    = HALF*(IXX1+IXX2)
        IYY    = HALF*(IYY1+IYY2)
        IZZ    = HALF*(IZZ1+IZZ2)
        RY     = HALF*(RY1+RY2)
        RZ     = HALF*(RZ1+RZ2)
        IMYZ   = MAX(IYY,IZZ)
        YOUNG  = HALF*(YOUNG1+YOUNG2)
        G      = HALF*(G1+G2)
          ATMP   = YOUNG/MAX(EM20,G*AREA)
C--------------------------------------
C       ELEMENT CHECK
C--------------------------------------
        DO I=1,NEL
          IF(XL(I)==ZERO)THEN
            WRITE(IOUT,*)' **ERROR ZERO LENGTH SPRING :'
          ENDIF
        ENDDO
C--------------------------------------
C       ELEMENT INITIALIZATION
C--------------------------------------
        DO I=1,NEL
          MASS(I)   = XL(I)*FAC
          XINER(I)  = XL(I)*RHO*MAX(IXX,IMYZ+AREA*XL(I)*XL(I)/12)
           UVAR(n0,I) = ZERO
           UVAR(n0+1,I) = EP30
           UVAR(n0+2,I) = ZERO
           UVAR(n0+3,I) = ZERO
           UVAR(n0+4,I) = ZERO
c---------------------------------------------
C FOR NODAL AND ELEMENT TIME STEP COMPUTATION
c---------------------------------------------
          XL2      = XL(I)*XL(I)/12.
          ARY      = ONE/(ATMP+XL2/MAX(EM20,IYY))
          ARZ      = ONE/(ATMP+XL2/MAX(EM20,IZZ))
          KTRAN    = MAX(AREA,ARY,ARZ)/XL(I)
          KROT     = 4. *MAX(IYY/XL(I),IZZ/XL(I))
          STIFM(I) = YOUNG*KTRAN
          STIFR(I) = MAX( G*IXX/XL(I),YOUNG*KROT)
          VISCM(I) = 0.
          VISCR(I) = 0.
        ENDDO
C
      RETURN
      END
