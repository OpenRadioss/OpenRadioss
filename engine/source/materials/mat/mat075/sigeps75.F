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
Chd|  SIGEPS75                      source/materials/mat/mat075/sigeps75.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        EOSMAIN                       ../common_source/eos/eosmain.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
       SUBROUTINE SIGEPS75(
     1      NEL    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2      TF     , TIME   , TIMESTEP, UPARAM, RHO0  , RHO   ,
     3      VNEW   , EINT   , MUOLD   ,
     4      EPSPXX , EPSPYY , EPSPZZ  , EPSPXY, EPSPYZ, EPSPZX,
     5      DEPSXX , DEPSYY , DEPSZZ  , DEPSXY, DEPSYZ, DEPSZX,
     6      EPSXX  , EPSYY  , EPSZZ   , EPSXY , EPSYZ , EPSZX ,
     7      SIGOXX , SIGOYY , SIGOZZ  , SIGOXY, SIGOYZ, SIGOZX,
     8      SIGNXX , SIGNYY , SIGNZZ  , SIGNXY, SIGNYZ, SIGNZX,
     9      SIGVXX , SIGVYY , SIGVZZ  , SIGVXY, SIGVYZ, SIGVZX,
     A      SOUNDSP, VISCMAX, UVAR    , OFF   , DVOL  , VOL0  ,
     B      PM     , IPM    , MAT     , PSH   , BUFMAT)
C
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C VNEW    | NEL     | F | R | VOLUME
C VOL0    | NEL     | F | R | VOLUME INITIAL (LAGRANGE)
C         |         | F | R | VOLUME (ALE)
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "com08_c.inc"
#include "param_c.inc"
#include "scr17_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR, IPM(NPROPMI,*), MAT(NEL)
C
      my_real
     .      TIME       , TIMESTEP   , UPARAM(NUPARAM),
     .      RHO   (NEL), RHO0  (NEL), VNEW(NEL), EINT(NEL),
     .      EPSPXX(NEL), EPSPYY(NEL), EPSPZZ(NEL),
     .      EPSPXY(NEL), EPSPYZ(NEL), EPSPZX(NEL),
     .      DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL),
     .      DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .      EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL),
     .      EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .      SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL),
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),
     .      DVOL(NEL)  , VOL0(NEL)  , PM(NPROPM,*),
     .      PSH(NEL)   , BUFMAT(*)  , MUOLD(NEL)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      SIGNXX (NEL), SIGNYY (NEL), SIGNZZ(NEL),
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SIGVXX (NEL), SIGVYY (NEL), SIGVZZ(NEL),
     .      SIGVXY (NEL), SIGVYZ (NEL), SIGVZX(NEL),
     .      SOUNDSP(NEL), VISCMAX(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     .      UVAR(NEL,NUVAR), OFF(NEL)
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER,FINTTE,TF(*),FINT2V
      EXTERNAL FINTER,FINTTE
C----------------------------------------------------------------
C  L O C A L  V A R I A B L E S
C----------------------------------------------------------------
      INTEGER I, EOSTYP, ITER, MATS(NEL),IPLA(NEL), MXS, IFLAG1,
     .        IFLAG2, ITEMAX, IMAX, IPLAS
      my_real
     .   DFS(NEL), MUS(NEL), MU2S(NEL), DPDM(NEL), ECOLD(NEL),
     .   THETA(NEL), DPDE(NEL)
      my_real
     .   MU(NEL)
      my_real
     .   PS,PE,ALPHAE,ALPHAP,EN,BETA,ETA,XI,UNSURN,
     .   BULK0,C0,DALPDPE,AA,HH,HH2,G,G2,DAV,
     .   BULK1, CRIT, TOL, PNEW1, DFDA, DGDA,
     .   GS, DGDALPHA, BULK
      my_real
     .   RHOS(NEL),RHOS0(NEL),ESPES(NEL),
     .   VNEWS(NEL),DVOLS(NEL),PNEW(NEL),
     .   ALPHAYLD(NEL),PYLD(NEL),ALPHA0(NEL),
     .   ALPHAOLD(NEL),DPDAEL(NEL),EINTN(NEL),POLD(NEL)
      my_real
     .   SIGOLD(NEL,6), PNEWG(NEL)
      DOUBLE PRECISION ALPHA1(NEL),ALPHA(NEL),DALPHA
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      BULK=UPARAM(1)
      PE = UPARAM(2)
      PS = UPARAM(3)
      EN = UPARAM(4)
      TOL= UPARAM(5)
      MXS   =NINT(UPARAM(6))
      IFLAG1=NINT(UPARAM(7))
      IFLAG2=NINT(UPARAM(8))
      ITEMAX=NINT(UPARAM(17))
      EOSTYP=IPM(4,MXS)
      DO I=1,NEL
         MATS(I) = MXS
         RHOS0(I)= PM(89,MXS)
      ENDDO
      BULK0= PM(32,MXS)
      G     =UPARAM(10)
      ALPHAE=UPARAM(11)
      GS   = PM(22,MXS)
      DGDALPHA= (G-GS)/(ALPHAE-ONE)
      ALPHAP=UPARAM(12)
      AA    =UPARAM(13)
      UNSURN=UPARAM(14)
      XI    =UPARAM(15)
      ETA   =UPARAM(16)
C
      DO I=1,NEL
         ALPHAOLD(I)= UVAR(I,3)
         ALPHAYLD(I)= UVAR(I,4)
         POLD(I)=-(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD
         PYLD(I)=PS-XI*(ALPHAYLD(I)-ONE)**UNSURN
      ENDDO
      DO I=1,NEL
         MU(I) =RHO(I)/RHO0(I)-ONE
      ENDDO
      DO I=1,NEL
        SIGOLD(I,1:3)=-POLD(I)
        SIGOLD(I,4:6)=ZERO
      ENDDO
C----------------------------
C FIRST GUESS LINEAR SOLUTION
C----------------------------
      DO I=1,NEL
        IF(ALPHAOLD(I) == ONE) THEN
C COMPACT MATERIAL
          ALPHA0(I)=ONE
          IPLA(I)=2
        ELSE
C POROUS MATERIAL
          HH =AA*(ALPHAOLD(I)-ONE)+ONE
          HH2=HH*HH
          DPDAEL(I)=BULK0/(ALPHAOLD(I)*(ONE-ALPHAOLD(I)/HH2))
          BULK1=BULK0*(ONE+MU(I))/ALPHAE
          IF(IFLAG1 == 1) THEN
             ALPHA0(I)=(POLD(I)+BULK0-ALPHAOLD(I)*DPDAEL(I))/(BULK1-DPDAEL(I))
          ELSEIF(IFLAG1 == 2) THEN
             ALPHA0(I)= BULK0/(BULK1-POLD(I))
          ENDIF
          ALPHA0(I)=MAX(ALPHA0(I),ONE)
        ENDIF
      ENDDO
C
      DO I=1,NEL
          ALPHA(I)=ALPHA0(I)
      ENDDO
C---------------------
C SOLUTION ELASTIQUE
C---------------------
      ITER=0
  100 ITER=ITER+1
C----------------
C SOLID VARIABLES
C----------------
      DO I=1,NEL
         RHOS(I) =RHO(I)*ALPHA(I)
         DVOLS(I)=DVOL(I)-VNEW(I)*(ONE-ALPHAOLD(I)/ALPHA(I))
         DVOLS(I)=DVOLS(I)/ALPHAOLD(I)
         VNEWS(I)=VNEW(I)/ALPHA(I)
      ENDDO
C------------------
C ENERGY ESTIMATION
C------------------
      DO I=1,NEL
         MUS(I) = (ONE + MU(I))*ALPHA(I)/ALPHAE - ONE
         DFS(I) = ONE/(ONE+MUS(I))
         EINTN(I)=EINT(I)-HALF*POLD(I)*DVOLS(I)
      ENDDO
C--------------
C EOS FOR SOLID
C--------------
      PNEW(1:NEL) = ZERO
      CALL EOSMAIN(0      ,NEL    ,EOSTYP  ,PM     ,OFF    ,EINTN ,
     .             RHOS   ,RHOS0  ,MUS     ,MU2S   ,ESPES  ,
     .             DVOLS  ,DFS    ,VNEWS   ,MATS   ,PSH    ,
     .             PNEW   ,DPDM   ,DPDE    ,THETA  ,ECOLD  ,
     .             BUFMAT ,SIGOLD ,MUOLD   ,75     ,POLD   ,
     .             NPF    ,TF)
      CALL EOSMAIN(1      ,NEL    ,EOSTYP  ,PM     ,OFF    ,EINTN ,
     .             RHOS   ,RHOS0  ,MUS     ,MU2S   ,ESPES  ,
     .             DVOLS  ,DFS    ,VNEWS   ,MATS   ,PSH    ,
     .             PNEW   ,DPDM   ,DPDE    ,THETA  ,ECOLD  ,
     .             BUFMAT ,SIGOLD ,MUOLD   ,75     ,POLD   ,
     .             NPF    ,TF)
      IF(IFLAG1 == 2) THEN
        DO I=1,NEL
           PNEW(I)=PNEW(I)/ALPHA(I)
        ENDDO
      ENDIF
C------------
C CONVERGENCE
C------------
      IF (ITER <= ITEMAX) THEN
C-----------------
C NEWTON ITERATION
C-----------------
        DO I=1,NEL
          IF(IPLA(I)==2)CYCLE
          IPLA(I)=0
          DFDA=DPDM(I)*(ONE+MU(I))/ALPHAE + DPDE(I)*EINT(I)/VOL0(I)
          DGDA=DPDAEL(I)
          PNEWG(I)=POLD(I)+DPDAEL(I)*(ALPHA(I)-ALPHAOLD(I))
C
          IF(IFLAG1 == 1) THEN
            DALPHA=(PNEW(I)-PNEWG(I))/(DFDA-DGDA)
          ELSEIF(IFLAG1 == 2) THEN
            DALPHA=(PNEW(I)-PNEWG(I))/((DFDA-PNEW(I))/ALPHA(I)-DGDA)
          ENDIF
          ALPHA1(I)=ALPHA(I)-DALPHA
          ALPHA1(I)=MAX(ALPHA1(I),ONE)
        ENDDO

        CRIT=-ONE
        DO I=1,NEL
          IF(IPLA(I)==2)CYCLE
          BETA=ABS(ALPHA(I)-ALPHA1(I))
          IF(BETA > CRIT) THEN
           CRIT=BETA
           IMAX=I
          ENDIF
        ENDDO
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----A----+----B----+----C----+
C        WRITE(6,'(5X,A,I4,8(A,E18.10))')'ELAS ITER=',ITER,'MUS=',MUS(IMAX),' ALPHA1=',ALPHA1(IMAX),' ALPHA=',ALPHA(IMAX),
C     .                                 ' CRIT=',CRIT,' PNEW=',PNEW(IMAX),' PNEWG=',PNEWG(IMAX),' ALPHAOLD=',ALPHAOLD(IMAX)
C		  		  
        IF (CRIT > TOL) THEN
          DO I=1,NEL
            ALPHA(I)=ALPHA1(I)
          ENDDO
          GO TO 100
        ENDIF
      ELSE
          CALL ANCMSG(MSGID=137,ANMODE=ANINFO,I1=ITEMAX)
          CALL ARRET(2)
      ENDIF
C-----------------------
C CHECK ELASTIC SOLUTION
C-----------------------
      IPLAS=0
      DO I=1,NEL
        IF(IPLA(I)==2)CYCLE
        IF(PNEW(I) > PYLD(I)) THEN
         IPLA(I)=1
         IPLAS=1
        ENDIF
      ENDDO
      IF(IPLAS==0) GO TO 300
C---------------------
C SOLUTION PLASTIQUE
C---------------------
      DO I=1,NEL
        IF(IPLA(I)==1) ALPHA(I)=ALPHA0(I)
      ENDDO
      ITER=0
  200 ITER=ITER+1
C----------------
C SOLID VARIABLES
C----------------
      DO I=1,NEL
         RHOS(I) =RHO(I)*ALPHA(I)
         DVOLS(I)=DVOL(I)-VNEW(I)*(ONE-ALPHAOLD(I)/ALPHA(I))
         DVOLS(I)=DVOLS(I)/ALPHAOLD(I)
         VNEWS(I)=VNEW(I)/ALPHA(I)
      ENDDO
C------------------
C ENERGY ESTIMATION
C------------------
      DO I=1,NEL
         MUS(I) = (ONE + MU(I))*ALPHA(I)/ALPHAE - ONE
         DFS(I) = ONE/(ONE+MUS(I))
         EINTN(I)=EINT(I)-HALF*POLD(I)*DVOLS(I)
      ENDDO
C--------------
C EOS FOR SOLID
C--------------
      PNEW(1:NEL) = ZERO
      CALL EOSMAIN(0     ,NEL   ,EOSTYP ,PM    ,OFF   ,EINTN ,
     .             RHOS  ,RHOS0 ,MUS    ,MU2S  ,ESPES ,
     .             DVOLS ,DFS   ,VNEWS  ,MATS  ,PSH   ,
     .             PNEW  ,DPDM  ,DPDE   ,THETA ,ECOLD ,
     .             BUFMAT,SIGOLD,MUOLD  ,75    ,POLD  ,
     .             NPF   ,TF)
      CALL EOSMAIN(1     ,NEL   ,EOSTYP ,PM    ,OFF   ,EINTN ,
     .             RHOS  ,RHOS0 ,MUS    ,MU2S  ,ESPES ,
     .             DVOLS ,DFS   ,VNEWS  ,MATS  ,PSH   ,
     .             PNEW  ,DPDM  ,DPDE   ,THETA ,ECOLD ,
     .             BUFMAT,SIGOLD,MUOLD  ,75    ,POLD  ,
     .             NPF   ,TF)
      IF(IFLAG1 == 2) THEN
        DO I=1,NEL
           PNEW(I)=PNEW(I)/ALPHA(I)
        ENDDO
      ENDIF
C------------
C CONVERGENCE
C------------
      IF (ITER <= ITEMAX) THEN
C-----------------
C NEWTON ITERATION
C-----------------
        DO I=1,NEL
            IF(IPLA(I)/=1)CYCLE
            DFDA=DPDM(I)*(ONE+MU(I))/ALPHAE + DPDE(I)*EINT(I)/VOL0(I)
            DGDA=-XI*UNSURN*(ALPHA(I)-ONE)**(UNSURN-ONE)
            PNEWG(I)=PS-XI*(ALPHA(I)-ONE)**UNSURN
C
            IF(IFLAG1 == 1) THEN
              DALPHA=(PNEW(I)-PNEWG(I))/(DFDA-DGDA)
            ELSEIF(IFLAG1 == 2) THEN
              DALPHA=(PNEW(I)-PNEWG(I))/((DFDA-PNEW(I))/ALPHA(I)-DGDA)
            ENDIF
            ALPHA1(I)=ALPHA(I)-DALPHA
            ALPHA1(I)=MAX(ALPHA1(I),ONE)
        ENDDO

        CRIT=-ONE
        DO I=1,NEL
          IF(IPLA(I)/=1)CYCLE
          BETA=ABS(ALPHA(I)-ALPHA1(I))
          IF(BETA > CRIT) THEN
           CRIT=BETA
           IMAX=I
          ENDIF
        ENDDO
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----A----+----B----+----C----+
C        WRITE(6,'(5X,A,I4,8(A,E18.10))')'PLAS ITER=',ITER,'MUS=',MUS(IMAX),' ALPHA1=',ALPHA1(IMAX),' ALPHA=',ALPHA(IMAX),
C     .                                 ' CRIT=',CRIT,' PNEW=',PNEW(IMAX),' PNEWG=',PNEWG(IMAX),' ALPHAOLD=',ALPHAOLD(IMAX)
C
        IF (CRIT > TOL) THEN
          DO I=1,NEL
            ALPHA(I)=ALPHA1(I)
          ENDDO
          GO TO 200
        ENDIF
      ELSE
          CALL ANCMSG(MSGID=137,ANMODE=ANINFO,I1=ITEMAX)
          CALL ARRET(2)
      ENDIF
C--------------------
C UPDATE YIELD
C--------------------
  300 CONTINUE
      DO I=1,NEL
      IF(ALPHA(I) > ONE) THEN
         IF(IPLA(I) == 1) THEN
           IF(PNEW(I) < PS) THEN
              ALPHAYLD(I)= ONE+ ETA*(PS-PNEW(I))**EN
           ELSE
              ALPHAYLD(I)=ONE
           ENDIF
         ENDIF
      ENDIF
      ENDDO
C--------------------
C DEVIATORIC STRESSES
C--------------------
      IF(IFLAG2==2)THEN
        DO I=1,NEL
          DAV = (DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))*THIRD
          G   = MAX(ZERO,GS+DGDALPHA*(ALPHA(I)-ONE))
          G2  = TWO*G
          SIGNXX(I)=SIGOXX(I)+POLD(I)+G2*(DEPSXX(I)-DAV)
          SIGNYY(I)=SIGOYY(I)+POLD(I)+G2*(DEPSYY(I)-DAV)
          SIGNZZ(I)=SIGOZZ(I)+POLD(I)+G2*(DEPSZZ(I)-DAV)
          SIGNXY(I)=SIGOXY(I)+G*DEPSXY(I)
          SIGNYZ(I)=SIGOYZ(I)+G*DEPSYZ(I)
          SIGNZX(I)=SIGOZX(I)+G*DEPSZX(I)
        ENDDO
      ENDIF
C-------
C UPDATE
C-------
      DO I=1,NEL
        UVAR(I,2)=THETA(I)
        UVAR(I,3)=ALPHA(I)
        UVAR(I,4)=ALPHAYLD(I)
        SIGNXX(I)=SIGNXX(I)-PNEW(I)
        SIGNYY(I)=SIGNYY(I)-PNEW(I)
        SIGNZZ(I)=SIGNZZ(I)-PNEW(I)
C SET MAXIMUM VISCOSITY
        VISCMAX(I)=ZERO
      ENDDO
C SET SOUND SPEED
      DO I=1,NEL
        IF(ALPHA(I) > ONE) THEN
          SOUNDSP(I)=SQRT(BULK0/RHOS0(1))*(AA*(ALPHA(I)-ONE)+ONE)
        ELSE
          SOUNDSP(I)=SQRT(BULK0/RHOS0(1))
        ENDIF
      ENDDO
C
      RETURN
      END

