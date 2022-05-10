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
Chd|  XFORC28                       source/elements/xelem/xforc28.F
Chd|-- called by -----------
Chd|        XFORC3                        source/elements/xelem/xforc3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        GET_U_PNU                     source/user_interface/upidmid.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE XFORC28(NX     ,
     2             XEL     ,VEL     ,VREL   ,UIX    ,UID    ,
     3             IOUT    ,IPROP   ,IMAT   ,OFF    ,KEINT  ,
     4             EINT   ,MASS    ,XINER   ,STIFM  ,STIFR  ,
     5             VISCM  ,VISCR   ,FORC    ,TORQ   ,
     6             NUVAR   ,UVAR    ,NUVARN ,UVARN  ,
     7             DT      ,DTE     )
C-------------------------------------------------------------------------
C     This subroutine computes multi-purpose element forces and moments
C     when element uses property TYPE28==NSTRAND.
C-------------------------------------------------------------------------
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C NX       |    1    | I | R | NUMBER OF NODES (CONSTANT IN THE GROUP)
C----------+---------+---+---+--------------------------------------------
C XEL      | 3*NX    | F | R | NODES COORDINATES
C VEL      | 3*NX    | F | R | NODES VELOCITIES
C VREL     | 3*NX    | F | R | NODES ROTATIONAL VELOCITIES
C----------+---------+---+---+--------------------------------------------
C UIX      |   NX    | I | R | ELEMENT CONNECTIVITY
C                            | IX(J) (1<=J<=NX) : NODE J USER ID
C UID      |    1    | I | R | ELEMENT USER IDENTIFIER
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C IPROP    |  1      | I | R | PROPERTY NUMBER
C IMAT     |  1      | I | R | MATERIAL NUMBER
C----------+---------+---+---+--------------------------------------------
C OFF      |  1      | F | W | ELEMENT STATE (ON=1./OFF=0.)
C KEINT    |  1      | I | W | ELEMENT INTERNAL ENERGY FLAG
C                        | 0 | HAS TO BE COMPUTED BY RADIOSS OUT OF USER ROUTINE.
C                        | 1 | IS RETURNED BY THIS USER ROUTINE.
C EINT     |  1      | F | W | ELEMENT INTERNAL ENERGY IF KEINT=1
C----------+---------+---+---+--------------------------------------------
C MASS     |   NX    | F | W | NODAL MASS
C XINER    |   NX    | F | W | NODAL INERTIA (SPHERICAL)
C STIFM    |   NX    | F | W | NODAL STIFNESS (TIME STEP)
C STIFR    |   NX    | F | W | NODAL ROTATION STIFNESS (TIME STEP)
C VISCM    |   NX    | F | W | NODAL VISCOSITY (TIME STEP)
C VISCR    |   NX    | F | W | NODAL ROTATION VISCOSITY (TIME STEP)
C----------+---------+---+---+--------------------------------------------
C FORC     | 3*NX    | F | W | NODAL FORCES
C TORQ     | 3*NX    | F | W | NODAL TORQUS
C----------+---------+---+---+--------------------------------------------
C NUVAR    |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR     |NUVAR    | F |R/W| USER ELEMENT VARIABLES 
C                            |               (FIX SIZE ZONE)
C NUVARN   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES PER NODE
C UVARN    |NUVARN*NX| F |R/W| USER ELEMENT VARIABLES PER NODE 
C                            |               (NX DEPENDENT SIZE ZONE)
C----------+---------+---+---+--------------------------------------------
C DT       |  1      | F | R | PREVIOUS TIME INCREMENT :
C                            |    CURRENT TIME POINT IS T = TOLD+DT
C----------+---------+---+---+--------------------------------------------
C DTE      |  1      | F | W | ELEMENT TIME STEP
C----------+---------+---+---+--------------------------------------------
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
C         SEE LECG28 FOR CORRESPONDING ID STORAGE.
C-------------------------------------------------------------------------
C INTEGER IFUNCI = GET_U_MNU(I,IM,KFUNC)
C         I     :     VARIABLE INDEX(1 for first function)
C         IM    :     MATERIAL NUMBER
C         KFUNC :     ONLY FUNCTION ARE YET AVAILABLE.
C         THIS FUNCTION RETURN THE USER STORED FUNCTION NUMBER(function 
C         refered by users materials).
C         SEE LECM28 FOR CORRESPONDING ID STORAGE.
C-------------------------------------------------------------------------
C my_real PARAMI = GET_U_GEO(I,IP)
C         I     :     PARAMETER INDEX(1 for first parameter,...)
C         IP    :     PROPERTY NUMBER
C         THIS FUNCTION RETURN THE USER GEOMETRY PARAMETERS 
C         NOTE: IF(IP==IPROP) UPARAG(I) == GET_U_GEO(I,IPROP)
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
C my_real Y = GET_U_FUNC(IFUNC,X,DXDY)
C         IFUNC :     function number obtained by 
C                     IFUNC = GET_U_MNU(I,IM,KFUNC) or IFUNC = GET_U_PNU(I,IP,KFUNC)
C         X     :     X value
C         DXDY  :     slope dX/dY
C         THIS FUNCTION RETURN Y(X)
C-------------------------------------------------------------------------
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER IOUT,NUVAR,NUVARN,IPROP,IMAT,
     .        NX ,UIX(NX) ,UID, KEINT,
     .        GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU,
     .        KFUNC,KMAT,KPROP
      my_real
     .   OFF, EINT, XEL(3,NX),  VEL(3,NX) ,VREL(3,NX),
     .   MASS(NX) ,  XINER(NX) ,STIFM(NX) ,
     .   STIFR(NX),  VISCM(NX) ,VISCR(NX) ,
     .   FORC(3,NX), TORQ(3,NX),
     .   UVAR(NUVAR),UVARN(NUVARN*NX),DT ,DTE ,ALPHA,
     .   GET_U_MAT, GET_U_GEO, GET_U_FUNC
      EXTERNAL GET_U_MNU,GET_U_PNU,GET_U_MID,GET_U_PID,
     .         GET_U_MAT,GET_U_GEO, GET_U_FUNC
      PARAMETER (KFUNC=29)
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=33)
C=================================================================
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB1, NB2, NB3, MB1, MB2, MB3, MB4, MB5,
     .        K, IFUNCT, IFV, NIT, ECHANGE
      my_real
     .  LPREV, LNEXT, VPREV(3), VNEXT(3), VV,XK, XC,
     .  L0, L, DX, DXOLD, DDX, DT11, DVX, FX, G, Y, DFDX, DGDX, F,
     .  STIF, MU1, MU2, FRIC, MU, BETA, FMAX, DFS, DF,   FF,
     .  EPSMOY, DF1, DF2, XN, FXOLD, EPSTOT, EPSMIN, EPSMAX, DEPS,
     .  RHO, XM, XKM, XKS, XCM, DTC, DTK, FACT, FFAC,XFAC  
C-----------------------------------------------
C       initial total lenght UVAR(NB1:NB1)
        NB1=1
C       previous elongation  UVAR(NB2:NB2)
        NB2=NB1+1
C       previous force average UVAR(NB3:NB3)
        NB3=NB2+1
C       initial nodes masses UVARN(MB1:MB1+NX-1)
        MB1=1
C       forces into strands UVARN(MB2:MB2+NX-1)
Clm     using UVARN(MB2:MB2+NX-2) only.
        MB2=MB1+NX
C       strands initial lenght UVARN(MB3:MB3+NX-1)
Clm     using UVARN(MB3:MB3+NX-2) only.
        MB3=MB2+NX
C       strands elongations UVARN(MB4:MB4+NX-1)
Clm     using UVARN(MB4:MB4+NX-2) only.
        MB4=MB3+NX
C       strands internal energy UVARN(MB5:MB5+NX-1)
Clm     using UVARN(MB5:MB5+NX-2) only.
        MB5=MB4+NX
C-----------------------------------------------
C       already broken only returns mass & null stiffness.
        IF (OFF.EQ.ZERO) GOTO 200
C-----------------------------------------------
C       FORCE & PLASTIC DISPLACEMENT,
C-----------------------------------------------
C       ACTUAL LENGHT.
        L=0.0
        DO K=1,NX-1
          LNEXT=
     .       SQRT((XEL(1,K+1)-XEL(1,K))*(XEL(1,K+1)-XEL(1,K))
     .           +(XEL(2,K+1)-XEL(2,K))*(XEL(2,K+1)-XEL(2,K))
     .           +(XEL(3,K+1)-XEL(3,K))*(XEL(3,K+1)-XEL(3,K)))
          L=L+LNEXT
          MASS(K) =LNEXT
        ENDDO
C-------
        XK  =GET_U_GEO(4,IPROP)
        IFUNCT =GET_U_PNU(1,IPROP,KFUNC)
        IFV    =GET_U_PNU(2,IPROP,KFUNC)
        FFAC=GET_U_GEO(12,IPROP)
        XFAC=GET_U_GEO(13,IPROP)
C-------
Clm     STIF (tangent stiffness) is considered for friction ..
        IF (IFUNCT.EQ.0.AND.IFV.EQ.0) THEN
C------------------------------------------
C        LINEAR ELASTIC
         L0  =UVAR(NB1)
         DX  =L-L0
C        raideur tgte par unite de longueur.
         STIF=XK
         F   =XK*DX/L0
         EPSTOT=DX/L0
        ELSEIF (IFUNCT.EQ.0.AND.IFV.NE.0) THEN
C------------------------------------------
C        G(Deps) only.
         L0  =UVAR(NB1)
         DX  =L-L0
C        no friction when only viscous ..
         STIF=0.
         F = FFAC
         EPSTOT=DX/L0
        ELSE
C------------------------------------------
C        NON LINEAR ELASTIC
         L0    =UVAR(NB1)
         DX    =L-L0
         EPSTOT=DX/L0
         F     =GET_U_FUNC(IFUNCT,EPSTOT,DFDX)
C        raideur tgte par unite de longueur.
         F = FFAC*F
         STIF=FFAC*DFDX
        ENDIF
C-------------------------------------
C       LINEAR DAMPING.
        DT11 = DT
        IF(DT11.EQ.ZERO)DT11 = EP30
C
        DXOLD=UVAR(NB2)
        DDX=DX-DXOLD
        DVX=DDX/DT11
C
        DEPS =(EPSTOT-DXOLD/L0)/DT11
C
        UVAR(NB2)=DX
C
        XC  =GET_U_GEO(5,IPROP)
        DGDX=ZERO
        IF (IFV.GT.0) THEN
         G  = GET_U_FUNC(IFV,DEPS*XFAC,DGDX)
         DGDX = DGDX*XFAC
         FX = F*G+XC*DEPS
         STIF=STIF*G
        ELSE
         FX= F+XC*DEPS
         G = UN
        ENDIF
C-------
        STIF=STIF*L/L0
C-----------------------------------------------
C       FRICTION PULLEYS.
C       STRAND STIFFNESS == K*/l0 is assumed to be K*/l * L/L0
C-----------------------------------------------
Clm     TORQ is used for temporary storage (computation optimization).
        DO K=1,NX-1
         VNEXT(1)=XEL(1,K+1)-XEL(1,K)
         VNEXT(2)=XEL(2,K+1)-XEL(2,K)
         VNEXT(3)=XEL(3,K+1)-XEL(3,K)
         VV=1./MAX(EM15,MASS(K))
         VNEXT(1)=VV*VNEXT(1)
         VNEXT(2)=VV*VNEXT(2)
         VNEXT(3)=VV*VNEXT(3)
         TORQ(1,K)=VNEXT(1)
         TORQ(2,K)=VNEXT(2)
         TORQ(3,K)=VNEXT(3)
        ENDDO
C-------
Clm     temporary storage for internal energy computation.
        DO K=1,NX-1
         VISCR(K)=UVARN(MB2+K-1)
         XINER(K)=UVARN(MB4+K-1)
        ENDDO
C-------------------------------------
Clm     rupture criteria is based upon average of total deformation
C       into nstrand element.
        EPSMIN=GET_U_GEO(8,IPROP)
        EPSMAX=GET_U_GEO(9,IPROP)
        IF (EPSTOT.LT.EPSMIN.OR.EPSTOT.GT.EPSMAX) THEN
         OFF=ZERO
         FX =ZERO
#include "lockon.inc"
          WRITE(IOUT, 1000) UID
#include "lockoff.inc"
         GOTO 100
        ENDIF                 
C-------
        MU1    = GET_U_GEO(10,IPROP)
        MU2    = GET_U_GEO(11,IPROP)
Clm     also available at time 0.
        EPSMOY=(DX-DXOLD)/MAX(EM15,L)
C-------
        XN=NX
        DO K=1,NX-1
         VPREV(1)=TORQ(1,K)
         VPREV(2)=TORQ(2,K)
         VPREV(3)=TORQ(3,K)
         DDX = VPREV(1) * (VEL(1,K+1) - VEL(1,K))
     .       + VPREV(2) * (VEL(2,K+1) - VEL(2,K))
     .       + VPREV(3) * (VEL(3,K+1) - VEL(3,K))
         UVARN(MB2+K-1)=UVARN(MB2+K-1)
C tous les brins ont la meme longueur correspond a :
     .       +STIF*(DT*DDX*(XN-UN)/MAX(EM15,L)-EPSMOY)
        ENDDO
C-------
C
        ECHANGE=1
        NIT=0
        DO WHILE(ECHANGE.EQ.1.AND.NIT.LT.10)
          ECHANGE=0
          NIT=NIT+1
          DO K=1,NX-1
Clm         stifr utilise de maniere temporaire.
            STIFR(K)=UVARN(MB2+K-1)
          END DO
          DO K=2,NX-1
C          default pulley friction.
           FRIC=MU1
           MU  = GET_U_GEO(50+K,IPROP)
           IF (MU.GE.ZERO) FRIC=MU
           MU  = GET_U_GEO(525+K-1,IPROP)
           IF (MU.GE.ZERO) THEN 
             FRIC=FRIC + UNDEMI*MU
           ELSE
             FRIC=FRIC + UNDEMI*MU2
           ENDIF
           MU  = GET_U_GEO(525+K,IPROP)
           IF (MU.GE.ZERO) THEN 
             FRIC=FRIC+UNDEMI*MU
           ELSE
             FRIC=FRIC + UNDEMI*MU2
           ENDIF
C
           VPREV(1)=TORQ(1,K-1)
           VPREV(2)=TORQ(2,K-1)
           VPREV(3)=TORQ(3,K-1)
           VNEXT(1)=-TORQ(1,K)
           VNEXT(2)=-TORQ(2,K)
           VNEXT(3)=-TORQ(3,K)
           ALPHA=VPREV(1)*VNEXT(1)+VPREV(2)*VNEXT(2)+VPREV(3)*VNEXT(3)
           ALPHA = MIN(ALPHA,UN)
           ALPHA = MAX(ALPHA,-UN)
           BETA  = PI-ACOS(ALPHA)
C
           FF   = DEUX*FX+STIFR(K-1)+STIFR(K)
           FMAX = MAX(ZERO,FF*TANH(UNDEMI*FRIC*BETA))
           DFS  = STIFR(K-1)-STIFR(K)
           IF(ABS(DFS).GT.FMAX)THEN
             DF =SIGN(ABS(DFS)-FMAX,DFS)
C            tous les brins ont la meme longueur correspond a :
             UVARN(MB2+K-2)=UVARN(MB2+K-2)-UNDEMI*DF
             UVARN(MB2+K-1)=UVARN(MB2+K-1)+UNDEMI*DF
             ECHANGE=1
           END IF
       END DO
       END DO
C-----------------------------------------------
C       RETURN FORCES.
C-----------------------------------------------
        VPREV(1)=TORQ(1,1)
        VPREV(2)=TORQ(2,1)
        VPREV(3)=TORQ(3,1)
        FORC(1,1)=FX*VPREV(1)
        FORC(2,1)=FX*VPREV(2)
        FORC(3,1)=FX*VPREV(3)
        DO K=2,NX-1
         VNEXT(1)=TORQ(1,K)
         VNEXT(2)=TORQ(2,K)
         VNEXT(3)=TORQ(3,K)
         FORC(1,K)=FX*(VNEXT(1)-VPREV(1))
         FORC(2,K)=FX*(VNEXT(2)-VPREV(2))
         FORC(3,K)=FX*(VNEXT(3)-VPREV(3))
         VPREV(1)=VNEXT(1)
         VPREV(2)=VNEXT(2)
         VPREV(3)=VNEXT(3)
        ENDDO
        FORC(1,NX)=-FX*VPREV(1)
        FORC(2,NX)=-FX*VPREV(2)
        FORC(3,NX)=-FX*VPREV(3)
C-------
        DO K=1,NX-1
         DFS=UVARN(MB2+K-1)
         VPREV(1)=DFS*TORQ(1,K)
         VPREV(2)=DFS*TORQ(2,K)
         VPREV(3)=DFS*TORQ(3,K)
         FORC(1,K)  =FORC(1,K)  +VPREV(1)
         FORC(2,K)  =FORC(2,K)  +VPREV(2)
         FORC(3,K)  =FORC(3,K)  +VPREV(3)
         FORC(1,K+1)=FORC(1,K+1)-VPREV(1)
         FORC(2,K+1)=FORC(2,K+1)-VPREV(2)
         FORC(3,K+1)=FORC(3,K+1)-VPREV(3)
        ENDDO
C-------
        DO K=1,NX
         TORQ(1,K)=ZERO
         TORQ(2,K)=ZERO
         TORQ(3,K)=ZERO 
        ENDDO
C-----------------------------------------------
C       SAVE ELONGATION AND INTERNAL ENERGY OF EACH STRAND (for TH).
C-----------------------------------------------
100     CONTINUE
        FXOLD=UVAR(NB3)
        DO K=1,NX-1
         UVARN(MB4+K-1)=MASS(K)-UVARN(MB3+K-1)
         UVARN(MB5+K-1)=UVARN(MB5+K-1)
     .    + UNDEMI*(UVARN(MB4+K-1)-XINER(K))
     .        *(FX+FXOLD+UVARN(MB2+K-1)+VISCR(K))
        ENDDO
C       return internal energy of element.
        KEINT=1
        EINT =ZERO
        DO K=1,NX-1
         EINT=EINT+UVARN(MB5+K-1)
        ENDDO
        UVAR(NB3)=FX
C-------
        IF (OFF.EQ.ZERO) THEN
C        vanish forces into strands.
         DO K=1,NX-1
          UVARN(MB2+K-1)=ZERO
         ENDDO
         GOTO 200
        ENDIF
C-----------------------------------------------
C       ELEMENT TIME STEP == MINIMUM TIME STEP OF ALL STRANDS.
C-----------------------------------------------
        RHO  = GET_U_GEO(3,IPROP)
        DTE  = EP20
        DO K=1,NX-1
C-------
C         CHECK NSTRAND STIFNESS AND DAMPING..
          IF(MASS(K).LE.EM15)THEN
#include "lockon.inc"
             WRITE(IOUT,*)
     .' ** ERROR NSTRAND : NULL STRAND LENGHT, ELEMENT=',
     .        UID
#include "lockoff.inc"
            GOTO 999
          ENDIF
C-------
          XM   = RHO*UVARN(MB3+K-1)
C         rigidite tangente
          XKM  =STIF*(NX-1)/MAX(EM15,L)
          XCM  = (F*DGDX+XC)*L/(MAX(EM15,MASS(K))*L0)
C-------
C         CHECK NSTRAND STIFNESS AND DAMPING..
          IF(XKM.LE.EM15.AND.XCM.LE.EM15)THEN
#include "lockon.inc"
             WRITE(IOUT,*)
     .' ** ERROR NSTRAND : NULL STRAND STIFFNESS & DAMPING, ELEMENT=',
     .        UID
#include "lockoff.inc"
            GOTO 999
          ENDIF
C-------
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
C-----------------------------------------------
C       FOR NODAL TIME STEP COMPUTATION :
C-----------------------------------------------
200     CONTINUE
        DO K=1,NX
         XINER(K) =ZERO
         STIFR(K) =ZERO
         VISCR(K) =ZERO 
        ENDDO
C
        IF (OFF.EQ.ZERO) THEN
         DO K=1,NX
          VISCM(K) =ZERO
          STIFM(K) =ZERO  
         ENDDO
        ELSE
         VISCM(1) =(F*DGDX+XC)*L/(MAX(EM15,MASS(1))*L0)
         STIFM(1) =STIF*(NX-1)/MAX(EM15,L)
         DO K=2,NX-1
          FACT     =UN/MAX(EM15,MASS(K-1))+UN/MAX(EM15,MASS(K))
          VISCM(K) =FACT*(F*DGDX+XC)*L/L0
          STIFM(K) =2.*STIF*(NX-1)/MAX(EM15,L)
         ENDDO
         VISCM(NX) =(F*DGDX+XC)*L/(MAX(EM15,MASS(NX-1))*L0)
         STIFM(NX) =STIF*(NX-1)/MAX(EM15,L)
        ENDIF
C
        DO K=1,NX
         MASS(K)=UVARN(MB1+K-1)
        ENDDO
C-----------------------------------------------
 1000 FORMAT(1X,'-- RUPTURE OF NSTRAND ELEMENT NUMBER ',I10)
      RETURN
C-----
 999  CONTINUE
      CALL ANCMSG(MSGID=217,ANMODE=ANINFO)
      CALL ARRET(2)
      END
      
