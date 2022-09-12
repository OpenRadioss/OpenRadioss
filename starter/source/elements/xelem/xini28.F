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
Chd|  XINI28                        source/elements/xelem/xini28.F
Chd|-- called by -----------
Chd|        XINIT3                        source/elements/xelem/xinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GET_U_FUNC                    source/user_interface/uaccess.F
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|        GET_U_PNU                     source/user_interface/uaccess.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE XINI28(NX     ,NAX1D ,NAX2D ,NAX3D  ,XEL   ,
     2                  VEL    ,VREL  ,IOUT  ,IPROP  ,IMAT  ,
     3                  IX    ,IDS    ,MASS  ,XINER  ,STIFM ,
     4                  STIFR ,VISCM  ,VISCR ,UVAR   ,NUVAR ,
     5                  UVARN ,NUVARN ,DTE )
      USE MESSAGE_MOD
C-------------------------------------------------------------------------
C     This subroutine initialize a multipurpose element
C     when element uses property TYPE28==NSTRAND.
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C IPROP    |  1      | I | R | PROPERTY NUMBER
C IMAT     |  1      | I | R | MATERIAL NUMBER
C----------+---------+---+---+--------------------------------------------
C NX       |    1    | I | R | NUMBER OF NODES
C----------+---------+---+---+--------------------------------------------
C XEL      | 3*NX    | F | R | NODES COORDINATES
C VEL      | 3*NX    | F | R | NODES VELOCITIES
C VREL     | 3*NX    | F | R | NODES ROTATIONAL VELOCITIES
C----------+---------+---+---+--------------------------------------------
C NAX1D    |    1    | I | W | NUMBER OF EDGES TO BE DRAWN INTO ANIM
C NAX2D    |    1    | I | W | NUMBER OF FACETS TO BE DRAWN INTO ANIM
C NAX3D    |    1    | I | W | NUMBER OF SOLIDS TO BE DRAWN INTO ANIM
C----------+---------+---+---+--------------------------------------------
C IX       |   NX    | I | R | ELEMENT CONNECTIVITY
C                            | IX(J) (1<=J<=NX) : NODE J USER ID
C IDS      |    1    | I | R | ELEMENT USER IDENTIFIER
C----------+---------+---+---+--------------------------------------------
C MASS     |   NX    | F | W | NODAL MASS
C XINER    |   NX    | F | W | NODAL INERTIA (SPHERICAL)
C STIFM    |   NX    | F | W | NODAL STIFNESS (TIME STEP)
C STIFR    |   NX    | F | W | NODAL ROTATION STIFNESS (TIME STEP)
C VISCM    |   NX    | F | W | NODAL VISCOSITY (TIME STEP)
C VISCR    |   NX    | F | W | NODAL ROTATION VISCOSITY (TIME STEP)
C----------+---------+---+---+--------------------------------------------
C UVAR     |NUVAR    | F | W | USER ELEMENT VARIABLES
C NUVAR    |    1    | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVARN    |NUVARN*NX| F | W | USER ELEMENT VARIABLES
C NUVARN   |    1    | I | R | NUMBER OF USER ELEMENT VARIABLES PER NODE.
C----------+---------+---+---+--------------------------------------------
C DTE      |    1    | F | W | MAXIMUM ELEMENT TIME STEP TO BE USED BY RADIOSS.
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
C         SEE LECG28 FOR CORRESPONDING ID STORAGE.
C-------------------------------------------------------------------------
C INTEGER IFUNCI = GET_U_MNU(I,IM,KFUNC)
C         I     :     VARIABLE INDEX(1 for first function)
C         IM    :     MATERIAL NUMBER
C         KFUNC :     ONLY FUNCTION ARE YET AVAILABLE.
C         THIS FUNCTION RETURN THE USER STORED FUNCTION NUMBERS(function 
C         refered by users materials).
C         SEE LECM28 FOR CORRESPONDING ID STORAGE.
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
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER IOUT,NUVAR,NUVARN,IPROP,IMAT,
     .        NX ,NAX1D ,NAX2D ,NAX3D , IX(NX), IDS,
     .        GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU,
     .        KFUNC,KMAT,KPROP
      my_real
     .        XEL(3,NX),VEL(3,NX),VREL(3,NX),
     .        MASS(NX) ,XINER(NX) ,STIFM(NX) ,
     .        STIFR(NX),VISCM(NX) ,VISCR(NX) ,UVAR(NUVAR) ,
     .        UVARN(NUVARN*NX), DTE,
     .        GET_U_MAT,GET_U_GEO,GET_U_FUNC,FFAC
      EXTERNAL GET_U_PNU,GET_U_MNU,GET_U_MAT,GET_U_GEO,GET_U_PID,
     .         GET_U_MID,GET_U_FUNC
      PARAMETER (KFUNC=29)
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=33)
C=======================================================================
C
C     EXAMPLE : NSTRAND.
C
C=======================================================================
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     .       MS,XK,XC,EPSTOT,F,DFDX,RHO,STIF,DEPS,G,DGDX,L0,
     .       LPREV, LNEXT,
     .       XM, XKM, XCM, FACT, XN, DTC, DTK
      INTEGER I,K,NB1,NB2,NB3,MB1,MB2,MB3,MB4,MB5,IFUNCT,IFV
C-----------------------------------------------
C       POLYLINE TO DRAW INTO ANIM.
        NAX1D=NX-1
        NAX2D=0
        NAX3D=0
C-----------------------------------------------
C       initial total length UVAR(NB1:NB1)
        NB1=1
C       previous elongation  UVAR(NB2:NB2)
        NB2=NB1+1
C       previous force       UVAR(NB3:NB3)
        NB3=NB2+1
C       initial nodes masses UVARN(MB1:MB1+NX-1)
        MB1=1
C       forces into strands UVARN(MB2:MB2+NX-1)
C       using UVARN(MB2:MB2+NX-2) only.
        MB2=MB1+NX
C       strands initial length UVARN(MB3:MB3+NX-1)
C      using UVARN(MB3:MB3+NX-2) only.
        MB3=MB2+NX
C       strands elongations UVARN(MB4:MB4+NX-1)
C       using UVARN(MB4:MB4+NX-2) only.
        MB4=MB3+NX
C       strands internal energy UVARN(MB5:MB5+NX-1)
C       using UVARN(MB5:MB5+NX-2) only.
        MB5=MB4+NX
C-----------------------------------------------
C       ELEMENT CHECK
        IF(NX.LT.3)THEN
C         WRITE(IOUT,*)
C     . ' ** ERROR NSTRAND : LESS THAN 3 NODES, ELEMENT=',           
C     .    IDS
           CALL ANCMSG(MSGID=381,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=IDS)
C         IERR=IERR+1
C         GOTO 999
        ENDIF
C-------
        RHO   =GET_U_GEO(3,IPROP)
C-------
C       POLYLINE LENGTH & NODES MASS.
C-------
        LPREV=
     .       SQRT((XEL(1,2)-XEL(1,1))*(XEL(1,2)-XEL(1,1))
     .           +(XEL(2,2)-XEL(2,1))*(XEL(2,2)-XEL(2,1))
     .           +(XEL(3,2)-XEL(3,1))*(XEL(3,2)-XEL(3,1)))
        UVAR(NB1) =LPREV
        UVARN(MB3)=LPREV
C
        MASS(1)  =HALF*RHO*LPREV
        IF (LPREV.LE.EM15) THEN
C         WRITE(IOUT,*)
C     .' ** ERROR NSTRAND : NULL STRAND LENGH, ELEMENT=',
C     .      IDS
C        IERR=IERR+1
C        GOTO 999
            CALL ANCMSG(MSGID=382,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=IDS)
        ENDIF
        IF(MASS(1).LE.EM15)THEN
C         WRITE(IOUT,*)
C     .' ** ERROR NSTRAND : NULL NODAL MASS, ELEMENT=',
C     .      IDS
C        IERR=IERR+1
C        GOTO 999
            CALL ANCMSG(MSGID=383,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=IDS)
        ENDIF
        DO K=2,NX-1
          LNEXT=
     .       SQRT((XEL(1,K+1)-XEL(1,K))*(XEL(1,K+1)-XEL(1,K))
     .           +(XEL(2,K+1)-XEL(2,K))*(XEL(2,K+1)-XEL(2,K))
     .           +(XEL(3,K+1)-XEL(3,K))*(XEL(3,K+1)-XEL(3,K)))
          IF (LNEXT.LE.EM15) THEN
C           WRITE(IOUT,*)
C     .' ** ERROR NSTRAND : NULL STRAND LENGTH, ELEMENT=',
C     .      IDS
C          IERR=IERR+1
C           GOTO 999
            CALL ANCMSG(MSGID=382,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=IDS)
          ENDIF
          MASS(K)       = HALF*RHO*(LPREV+LNEXT)
          UVARN(MB3+K-1)=LNEXT
          UVAR(NB1)     =UVAR(NB1)+LNEXT
          IF(MASS(K).LE.EM15)THEN
C           WRITE(IOUT,*)
C     .' ** ERROR NSTRAND : NULL NODAL MASS, ELEMENT=',
C     .      IDS
C          IERR=IERR+1
C           GOTO 999
            CALL ANCMSG(MSGID=383,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=IDS)
          ENDIF
          LPREV=LNEXT
        ENDDO
        MASS(NX)  = HALF*RHO*LPREV
        IF(MASS(NX).LE.EM15)THEN
C         WRITE(IOUT,*)
C     .' ** ERROR NSTRAND : NULL NODAL MASS, ELEMENT=',
C     .      IDS
C        IERR=IERR+1
C         GOTO 999
            CALL ANCMSG(MSGID=383,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=IDS)
        ENDIF
C------------------------------------------
        XK  =GET_U_GEO(4,IPROP)
        DFDX=ZERO
        IFUNCT=GET_U_PNU(1,IPROP,KFUNC)
C
        XC  =GET_U_GEO(5,IPROP)
        IFV=GET_U_PNU(2,IPROP,KFUNC)
        FFAC=GET_U_GEO(12,IPROP)
C
        IF (IFUNCT.EQ.0.AND.IFV.EQ.0) THEN
C------------------------------------------
C        LINEAR ELASTIC
         L0  =UVAR(NB1)
         STIF=XK
         F   =ZERO
        ELSEIF (IFUNCT.EQ.0.AND.IFV.NE.0) THEN
C------------------------------------------
C        G(Deps) only.
         L0  =UVAR(NB1)
         STIF=ZERO
         F   =ONE
        ELSE
C------------------------------------------
C        NON LINEAR ELASTIC
         L0    =UVAR(NB1)
         EPSTOT=ZERO
         F     =GET_U_FUNC(IFUNCT,EPSTOT,DFDX)
         STIF  =FFAC*DFDX
        ENDIF
C------------------------------------------
        DEPS=ZERO
        DGDX=ZERO
        G   =ONE
        IF (IFV.NE.0) THEN
         G=GET_U_FUNC(IFV,DEPS,DGDX)
         STIF=STIF*G
        ENDIF
C-------
C        CHECK NSTRAND STIFNESS AND DAMPING.
         IF(      STIF/UVAR(NB1).LE.EM15
     .      .AND.(F*DGDX+XC)/UVAR(NB1).LE.EM15)THEN
C          WRITE(IOUT,*)
C     .' ** ERROR NSTRAND : NULL NODAL STIFFNESS & DAMPING, ELEMENT=',
C     .     IDS
C         IERR=IERR+1
C          GOTO 999
           CALL ANCMSG(MSGID=384,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=IDS)
         ENDIF
C-------
C       RETURN ELEMENT TIME STEP.
        XN=NX
        DTE  = EP20
        DO K=1,NX-1
          XM   = RHO*UVARN(MB3+K-1)
C         rigidite tangente
C tous les brins ont la meme longueur correspond a :
C         XKM  = STIF*(XN-1.)/L0
          XKM  = STIF/UVARN(MB3+K-1)
C tous les brins ont la meme longueur correspond a :
C         XCM  = (F*DGDX+XC)*(XN-1.)/L0
          XCM  = (F*DGDX+XC)/UVARN(MB3+K-1)
          IF(XCM+XKM.LT.EM15)XM  =ONE
          XKM= MAX(EM15,XKM)
C           DTK=(SQRT(0.25*XCM*XCM+XM*XKM)-0.5*XCM)/MAX(EM15,XKM)
C           DTC=0.5 * 2.*XM/MAX(EM15,XCM)
          DTK=(SQRT(XCM*XCM+XM*XKM)-XCM)/MAX(EM15,XKM)
          DTC=XM/MAX(EM15,XCM)
          IF (DTK.EQ.ZERO) THEN
C          XKM==0.
           DTK=DTC
          ELSE
           DTK=MIN(DTK,DTC)
          ENDIF
          DTE=MIN(DTE,DTK)
        ENDDO
C-------
C        FOR NODAL TIME STEP COMPUTATION.
         VISCM(1) =(F*DGDX+XC)/UVARN(MB3)
         STIFM(1) =STIF/UVARN(MB3)
         DO K=2,NX-1
          FACT     =ONE/UVARN(MB3+K-2) + ONE/UVARN(MB3+K-1)
          STIFM(K) =STIF*FACT
          VISCM(K) =(F*DGDX+XC)*FACT
         ENDDO
         VISCM(NX) =(F*DGDX+XC)/UVARN(MB3+NX-2)
         STIFM(NX) =STIF/UVARN(MB3+NX-2)
C-------
        DO K=1,NX
         XINER(K) = ZERO
         STIFR(K) = ZERO
         VISCR(K) = ZERO

        ENDDO
C-------
         DO K=1,NX
          UVARN(MB1+K-1)=MASS(K)
         ENDDO
C-------
      WRITE(IOUT,1000) IDS,L0,RHO*L0,STIF/L0
 1000 FORMAT(' NSTRAND ELEMENT CHECKING :',/,
     .       ' ------------------------  ',/,
     .       ' ELEMENT IDENTIFIER . . . .',I8/,
     .       ' TOTAL LENGTH . . . . . . .',E12.4/,
     .       ' MASS . . . . . . . . . . .',E12.4/,
     .       ' INITIAL GLOBAL STIFFNESS .',E12.4//)
C-------
      RETURN
      END
