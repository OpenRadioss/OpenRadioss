Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
C----s---1----+----2----+----3----+----4----+----5----+----6----+----7--
      !||====================================================================
      !||    redef3_mod       ../engine/source/elements/spring/redef3.F
      !||--- called by ------------------------------------------------------
      !||    r1def3           ../engine/source/elements/spring/r1def3.F
      !||    r23l108def3      ../engine/source/elements/spring/r23l108def3.F
      !||    r23l114def3      ../engine/source/elements/spring/r23l114def3.F
      !||    r2def3           ../engine/source/elements/spring/r2def3.F
      !||    r3def3           ../engine/source/elements/spring/r3def3.F
      !||    r4def3           ../engine/source/elements/spring/r4def3.F
      !||    r6def3           ../engine/source/elements/spring/r6def3.F
      !||    redef_seatbelt   ../engine/source/tools/seatbelts/redef_seatbelt.F
      !||====================================================================
       MODULE REDEF3_MOD
        CONTAINS
!! for performance reasons, this function must inlined, because it is called in a loop
!!      \brief return .true. if the function id corresponds to a python function
           PURE INTEGER FUNCTION GET_PYTHON_FUNCT_ID(NFUNCT, FUNCT_ID, NPC) RESULT(ID)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     MODULE
! ----------------------------------------------------------------------------------------------------------------------
          IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
!                                                     ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER, INTENT(IN) :: NFUNCT
          INTEGER, INTENT(IN) :: FUNCT_ID !< THE ID OF THE FUNCTION
          INTEGER, INTENT(IN) :: NPC(3*NFUNCT+1) 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: I 
! ----------------------------------------------------------------------------------------------------------------------
!                                                      BODY
! ----------------------------------------------------------------------------------------------------------------------
          I = 0
          ID = 0
          IF (FUNCT_ID> 0) I = NPC(2*NFUNCT+FUNCT_ID+1)
          IF(I < 0) ID = -I
        END FUNCTION GET_PYTHON_FUNCT_ID 

      !||====================================================================
      !||    redef3                 ../engine/source/elements/spring/redef3.F
      !||--- called by ------------------------------------------------------
      !||    r1def3                 ../engine/source/elements/spring/r1def3.F
      !||    r23l108def3            ../engine/source/elements/spring/r23l108def3.F
      !||    r23l114def3            ../engine/source/elements/spring/r23l114def3.F
      !||    r2def3                 ../engine/source/elements/spring/r2def3.F
      !||    r3def3                 ../engine/source/elements/spring/r3def3.F
      !||    r4def3                 ../engine/source/elements/spring/r4def3.F
      !||    r6def3                 ../engine/source/elements/spring/r6def3.F
      !||--- calls      -----------------------------------------------------
      !||    vinter2                ../engine/source/tools/curve/vinter.F
      !||    vinter2dp              ../engine/source/tools/curve/vinter.F
      !||    vinter_mixed           ../engine/source/tools/curve/vinter_mixed.F90
      !||--- uses       -----------------------------------------------------
      !||    python_funct_mod       ../common_source/modules/python_mod.F90
      !||    vinter_mixed_mod       ../engine/source/tools/curve/vinter_mixed.F90
      !||====================================================================
      SUBROUTINE REDEF3(PYTHON,
     1   FX,      XK,      DX,      FXEP,
     2   DXOLD,   DPX,     TF,      NPF,
     3   XC,      OFF,     E,       DPX2,
     4   ANIM,    IANI,    POS,     
     5   XL0,     DMN,     DMX,     DVX,
     6   FF,      LSCALE,  EE,      GF3,
     7   IFUNC3,  YIELD,   ALDP,    AK,
     8   B,       D,       IECROU,  IFUNC,
     9   IFV,     IFUNC2,  EPLA,    XX_OLD,
     A   NEL, NFT, MAX_SLOPE, FX_MAX,  XKC)
C-----------------------------------------------
        USE PYTHON_FUNCT_MOD
        USE VINTER_MIXED_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      type(python_), intent(inout) :: PYTHON
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
      INTEGER NPF(*)
      INTEGER IANI
      INTEGER IECROU(MVSIZ) !< Spring Hardening flag
C                0: Elastic spring.
C                1: Nonlinear elastic plastic spring with isotropic hardening.
C                2: Nonlinear elastic plastic spring with uncoupled hardening.
C                4: Nonlinear elastic plastic spring with kinematic hardening.
C                5: Nonlinear elastic plastic spring with nonlinear unloading.
C                6: Nonlinear elastic plastic spring with isotropic hardening and nonlinear unloading.
C                7: Nonlinear elastic spring with elastic hystersis.
C                8: Nonlinear elastic spring with total length function
      INTEGER IFUNC(MVSIZ)
      INTEGER IFV(MVSIZ)
      INTEGER IFUNC2(MVSIZ)
      my_real FX(*) !< External force? GBUF%FOR
      my_real XK(MVSIZ)                      
      my_real DX(*)                
      my_real FXEP(*) 
      my_real DXOLD(*) 
      my_real DPX(*) 
      my_real TF(*) !< functions
      my_real XC(*) !<                     
      my_real OFF(*) !< Offset
      my_real E(*) !< Young's modulus
      my_real DPX2(*) !<                       
      my_real ANIM(*) !< Animation 
      my_real POS(5,*) !< Position
      my_real XL0(*) !< Length
      my_real DMN(*) !<                     
      my_real DMX(*) !<                     
      my_real DVX(*) !<                      
      my_real FF(*) !<               
      my_real LSCALE(*) !< Length scale
      my_real EE(*) !< Energy
      my_real YIELD(*) !< Yield
      my_real AK(MVSIZ) !<                   
      my_real B(MVSIZ) !<                   
      my_real D(MVSIZ) !<                   
      my_real EPLA(MVSIZ) !<                   
      my_real XX_OLD(*) !<                   
      my_real, OPTIONAL :: MAX_SLOPE(MVSIZ) !<
      my_real, OPTIONAL :: FX_MAX(*) !<                
      my_real, OPTIONAL :: XKC(*) !<                   
      DOUBLE PRECISION ALDP(*) !                      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JPOS(MVSIZ)
      INTEGER JLEN(MVSIZ)
      INTEGER JAD(MVSIZ)
      INTEGER JPOS2(MVSIZ)
      INTEGER JLEN2(MVSIZ)
      INTEGER JPOS3(MVSIZ)
      INTEGER JLEN3(MVSIZ)
      INTEGER JAD2(MVSIZ)
      INTEGER JFUNC
      INTEGER JFUNC2
      INTEGER JDMP
      INTEGER JECROU(-1:11)
      INTEGER J2DMP
      INTEGER K1
      INTEGER NP2
      INTEGER I
      INTEGER II
      INTEGER INTERP
      INTEGER K
!     INTEGER IC1(MVSIZ)
      INTEGER IC2(MVSIZ)
      INTEGER FUND
      INTEGER IFUNC3(MVSIZ)
      INTEGER J2POS(MVSIZ)
      INTEGER J2LEN(MVSIZ)
      INTEGER J2AD(MVSIZ)
      INTEGER J2FUNC
c     REAL ou REAL*8
      my_real B1
      my_real DDX(MVSIZ)
      my_real FOLD(MVSIZ)
      my_real GX(MVSIZ)
      my_real DXELA(MVSIZ)
      my_real DYDX(MVSIZ)
      my_real XX(MVSIZ)
      my_real XX2(MVSIZ)
      my_real XX3(MVSIZ)
      my_real YY(MVSIZ)
      my_real YY2(MVSIZ)
      my_real YY3(MVSIZ)
      my_real DYDX2(MVSIZ)
      my_real DYDX3(MVSIZ)
      my_real DYDXV(MVSIZ)
      my_real DPERM(MVSIZ)
      my_real DVV
      my_real DFAC
      my_real DT11
      my_real DAMP
      my_real DAMM
      my_real FMAX(MVSIZ)
      my_real DVXS(MVSIZ)
      my_real GF3(MVSIZ)
      my_real DYDXV2(MVSIZ)
      my_real FMIN(MVSIZ)
      my_real GX2(MVSIZ)
      my_real XI1
      my_real XI2
      my_real YI1
      my_real YI2
      my_real X1
      my_real X2
      my_real Y1
      my_real Y2
      my_real AN3Y0(MVSIZ)
      my_real DDXT
      my_real DDXC

      INTEGER :: NFUNCT !< total number of functions
      INTEGER :: PYID1, PYID2 !< Python function ID
      LOGICAL :: ANY_PYTHON_FUNC !< Any python function?
C=======================================================================
      NFUNCT = PYTHON%FUNCT_OFFSET + PYTHON%nb_functs! OFFSET = nb of non-python functions
      ANY_PYTHON_FUNC = .FALSE.
      DT11 = DT1
      IF(DT11==ZERO)DT11 = EP30
      DO I=1,NEL
        DX(I)=DX(I)/XL0(I)
        DXOLD(I)=DXOLD(I)/XL0(I)
        DPX(I)=DPX(I)/XL0(I)
        DPX2(I)=DPX2(I)/XL0(I)
        E(I)=E(I)/XL0(I)
      ENDDO
C
      DO I=1,NEL
        FOLD(I)=FX(I)
        DDX(I)= (DX(I)-DXOLD(I))
        DVX(I)= DDX(I)/ DT11
        DVXS(I)= DVX(I)*FF(I)
      ENDDO
C
C
      IF(IANI/=0)THEN
        DO I=1,NEL
          II=I+NFT
          DAMP=DX(I)/MAX(DMX(I),EM15) !
          DAMM=DX(I)/MIN(DMN(I),-EM15)
          ANIM(II)=MAX(ANIM(II),DAMP,DAMM)
          ANIM(II)=MIN(ANIM(II),ONE)
        ENDDO
      ENDIF
C-------------------------------------
C        VECTOR INTERPOLATION (ADRESS)
C-------------------------------------
      JECROU(-1)  = 0
      JECROU(0)  = 0
      JECROU(1)  = 0
      JECROU(2)  = 0
      JECROU(3)  = 0
      JECROU(4)  = 0
      JECROU(5)  = 0
      JECROU(6)  = 0
      JECROU(7)  = 0
      JECROU(8)  = 0
      JECROU(9)  = 0
      JECROU(10) = 0
      JECROU(11) = 0
      INTERP = 0
      JDMP = 0
      J2DMP = 0
C
      DO I=1,NEL
       IF(IECROU(I) == 9)THEN
         JECROU(9) = JECROU(9) + 1
       ELSEIF(IECROU(I) == 11)THEN
         JECROU(11) = JECROU(11) + 1
       ELSEIF(IFUNC(I)==0)THEN  ! ifunc =IGEO(101)-FCT_id1
         JECROU(-1) = JECROU(-1) + 1
c modif pour vectorisation
       ELSEIF(IECROU(I)==0)THEN
         JECROU(0) = JECROU(0) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==1)THEN
         JECROU(1) = JECROU(1) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==2)THEN
         JECROU(2) = JECROU(2) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==3)THEN
         JECROU(3) = JECROU(3) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==4)THEN
         JECROU(4) = JECROU(4) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==5)THEN
         JECROU(5) = JECROU(5) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==6)THEN
         JECROU(6) = JECROU(6) + 1
         INTERP = 1
       ELSEIF(IECROU(I)==7)THEN
         JECROU(7) = JECROU(7) + 1
         INTERP = 1
       ELSEIF(IECROU(I) == 8)THEN
         JECROU(8) = JECROU(8) + 1
         INTERP = 1
       ELSEIF(IECROU(I) == 10)THEN
         JECROU(10) = JECROU(10) + 1
         INTERP = 1
       ENDIF
       IF(IFV(I)/=0) JDMP = JDMP + 1
       IF(IFUNC3(I)/=0) J2DMP = J2DMP + 1
      ENDDO
C
      IF(INTERP>0)THEN
        DO I=1,NEL
          JPOS(I)  = NINT(POS(1,I))
          JPOS2(I) = NINT(POS(2,I))
          JPOS3(I) = NINT(POS(3,I))
          PYID1 = GET_PYTHON_FUNCT_ID(NFUNCT, IFUNC(I),NPF)
          PYID2 = GET_PYTHON_FUNCT_ID(NFUNCT, IFUNC2(I),NPF)
          IF(PYID1 > 0) THEN 
            JAD(I) = -PYID1
            JLEN(I) = -PYID1
            JLEN3(I) = -PYID1
            IFUNC(I) = PYID1
            ANY_PYTHON_FUNC = .TRUE.
          ELSE 
            JFUNC =MAX(1,IFUNC(I))
            JAD(I)   = NPF(JFUNC) / 2  + 1
            JLEN(I)  = NPF(JFUNC+1) / 2  - JAD(I)  - JPOS(I)
            JLEN3(I) = NPF(JFUNC+1) / 2  - JAD(I)  - JPOS3(I)
          ENDIF

          IF(PYID2 > 0) THEN
            JAD2(I) = -PYID2
            JLEN2(I) = -PYID2  
            IFUNC2(I) =  PYID2
            ANY_PYTHON_FUNC = .TRUE.
          ELSE
            JFUNC2=MAX(1,IFUNC2(I))
            JAD2(I)  = NPF(JFUNC2) / 2 + 1
            JLEN2(I) = NPF(JFUNC2+1) / 2 - JAD2(I) - JPOS2(I)
          ENDIF
          XX(I) =ZERO
          XX2(I)=ZERO
          XX3(I)=ZERO  
        ENDDO
      ENDIF            
C-------------------------------------
C        NON LINEAIRE ELASTIQUE, F=f(total length)
C-------------------------------------
      IF (JECROU(8) > 0) THEN
        DO I=1,NEL
          IF (IECROU(I) == 8) THEN
            XX(I) = ALDP(I)
          ELSE
            XX(I) = DX(I)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        NON LINEAR ELASTIC
C        NL ELASTO PLASTIC (TENSION COMPRESSION DECOUPLED)
C-------------------------------------
      IF(JECROU(0)+JECROU(2)>0)THEN
        DO I=1,NEL
          IF(IFUNC(I) /= 0.AND.(IECROU(I)==0.OR.IECROU(I)==2))THEN
            XX(I)=DX(I)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIC (ISOTROPIC)
C        ELASTO PLASTIC (TENSION COMPRESSION COUPLED 6 DOF COUPLES)
C-------------------------------------
      IF(JECROU(1)+JECROU(3)>0)THEN
        DO I=1,NEL
         IF(IFUNC(I) /= 0.AND.(IECROU(I)==1.OR.IECROU(I)==3))THEN
           FX(I)=FXEP(I)+XK(I)*DDX(I)
           IF(FX(I)>=ZERO)THEN
             XX(I)=DPX(I)+FX(I)/XK(I)
           ELSE
             XX(I)=-DPX(I)+FX(I)/XK(I)
           ENDIF
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIC (KINEMATIC HARDENING)
C-------------------------------------
      IF(JECROU(4)>0)THEN
        DO I=1,NEL
         IF(IFUNC(I) /= 0.AND.IECROU(I)==4)THEN
           INTERP = MAX(2,INTERP)
           XX(I) =DX(I)
           XX2(I)=DX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIC (TENSION COMPRESSION DECOUPLED)
C         (D/R)NONLINEAR RELOADING
C         DPX = MAXIMUM DISPLACEMENT (AND NOT PLASTIC)
C-------------------------------------
      IF(JECROU(5)>0)THEN
        DO I=1,NEL
         IF(IFUNC(I) /= 0.AND.IECROU(I)==5)THEN
           XX(I)=DX(I)
           IF(DX(I)>ZERO)THEN
             INTERP = MAX(3,INTERP)
             XX2(I)=DPX(I)
             XX3(I)=DPX(I)
           ELSEIF(DX(I)<ZERO)THEN
             INTERP = MAX(3,INTERP)
             XX2(I)=DPX2(I)
             XX3(I)=DPX2(I)
           ELSE
             INTERP = MAX(1,INTERP)
           ENDIF
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIC (ISOTROPIC HARDENING)
C-------------------------------------

      IF(JECROU(6)>0)THEN
        IF(ANY_PYTHON_FUNC) THEN
          DO I=1,NEL                                          
            IF(IFUNC(I) /= 0.AND.IECROU(I)== 6 .AND. JAD2(I) < 0 )THEN
            FUND = -JAD2(I)           ! Curve ID
            ! Obtain the derivative directly at FXEP(I)
            CALL PYTHON_DERIV_FUNCT1D(PYTHON, FUND, FXEP(I), AN3Y0(I))
            ! Calculate FX(I) and XX(I) based on the derivative
            FX(I) = FXEP(I) + AN3Y0(I) * DDX(I)
            XX(I) = SIGN(ABS(XX_OLD(I)), FX(I))
            XX(I) = XX(I) + DDX(I)
            ENDIF
          ENDDO
        ENDIF
        DO I=1,NEL
         !if not a python function
         IF(IFUNC(I) /= 0.AND.IECROU(I)== 6 .AND. JAD2(I) >= 0 )THEN
          FUND = IFUNC2(I)                                                                  
          NP2  = (NPF(FUND+1)-NPF(FUND))/2
          AN3Y0(I)= ZERO 
          DO  K=2,NP2                                                       
             K1=2*(K-2)
             X1=TF(NPF(FUND)+K1)                                       
             X2=TF(NPF(FUND)+K1+2)                                     
             Y1=TF(NPF(FUND)+K1+1)                                     
             Y2=TF(NPF(FUND)+K1+3) 
             IF((FXEP(I)< Y2.AND.FXEP(I)>=Y1))THEN              
                AN3Y0(I)=(Y2-Y1)/ (X2-X1)                     
                EXIT     
             ENDIF 
           ENDDO 
           IF (AN3Y0(I)== ZERO)THEN
             X1=TF(NPF(FUND)+(NP2-2)*2)
             X2=TF(NPF(FUND)+(NP2-2)*2+2)
             Y1=TF(NPF(FUND)+(NP2-2)*2+1)
             Y2=TF(NPF(FUND)+(NP2-2)*2+3) 
C
             XI1=TF(NPF(FUND))                                       
             XI2=TF(NPF(FUND)+2)                                     
             YI1=TF(NPF(FUND)+1)                                     
             YI2=TF(NPF(FUND)+3) 
             IF(FXEP(I)>Y2)AN3Y0(I)=(Y2-Y1)/ (X2-X1) 
             IF(FXEP(I)<YI1)AN3Y0(I)=(YI2-YI1)/ (XI2-XI1)
           ENDIF    
           FX(I)=FXEP(I)+AN3Y0(I)*DDX(I)
           XX(I)=SIGN(ABS(XX_OLD(I)),FX(I))
           XX(I)=XX(I)+DDX(I)
          ENDIF
        ENDDO
      ENDIF
c-------------------------------------
c      ELASTO PLASTIC TWO CURVES FOR LOAD AND UNLOAD
c-------------------------------------
      IF(JECROU(7)>0)THEN
        INTERP = MAX(2,INTERP)
        DO I=1,NEL
         IF(IFUNC(I) /= 0.AND.IECROU(I)==7)THEN
           XX(I) =DX(I)
           XX2(I)=DX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PERFECLTY PLASTIC (ISOTROPIC HARDENING) - input by values
C-------------------------------------
      IF(JECROU(9)>0)THEN
        DO I=1,NEL
          IF(IECROU(I)== 9) THEN                      
            AN3Y0(I)= ZERO 
            IF (ABS(FXEP(I)) > FX_MAX(I)) THEN
              AN3Y0(I)= ZERO
            ELSE
              AN3Y0(I)= XK(I)
            ENDIF
C    
            FX(I)=FXEP(I)+AN3Y0(I)*DDX(I)
            XX(I)=SIGN(ABS(XX_OLD(I)),FX(I))
            XX(I)=XX(I)+DDX(I)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (ISOTROPIC HARDENING) - PERFECLTY PLASTIC IN COMPRESSION
C-------------------------------------


      IF(JECROU(10)>0)THEN

         IF(ANY_PYTHON_FUNC) THEN
           DO I=1,NEL                                          
             IF(IFUNC(I) /=0 .AND.IECROU(I)== 10 .AND. JAD2(I) < 0)THEN
              FUND = -JAD2(I)           ! Curve ID
              AN3Y0(I) = ZERO
              DXELA(I) = DX(I) - DPX(I)
              IF (((DXELA(I) >= ZERO) .OR. (FXEP(I) >= ZERO)) .AND. (FUND > 0)) THEN
                  CALL PYTHON_CALL_FUNCT1D(PYTHON, FUND, FXEP(I), Y1)
                  CALL PYTHON_DERIV_FUNCT1D(PYTHON, FUND, FXEP(I), AN3Y0(I))
                  IF (AN3Y0(I) == ZERO) THEN
                      CALL PYTHON_CALL_FUNCT1D(PYTHON, FUND, FXEP(I), Y1)
                      IF (FXEP(I) > Y1) THEN
                          CALL PYTHON_DERIV_FUNCT1D(PYTHON, FUND, FXEP(I) + DDX(I), AN3Y0(I))
                      ELSE
                          CALL PYTHON_DERIV_FUNCT1D(PYTHON, FUND, FXEP(I) - DDX(I), AN3Y0(I))
                      END IF
                  END IF
                  IF ((DXELA(I) < ZERO) .AND. (ABS(DDX(I)) > ZERO)) THEN
                      DDXT = -FXEP(I) / AN3Y0(I)
                      DDXC = DDX(I) - DDXT
                      AN3Y0(I) = (DDXT / DDX(I)) * AN3Y0(I) + (DDXC / DDX(I)) * XKC(I)
                  END IF
                  IF (DXELA(I) >= ZERO) XX(I) = XX_OLD(I) + DDX(I)
              ELSE
                  AN3Y0(I) = XKC(I)
              END IF
              FX(I) = FXEP(I) + AN3Y0(I) * DDX(I)
            ENDIF
          ENDDO
        ENDIF
        DO I=1,NEL
         IF(IFUNC(I) /=0 .AND.IECROU(I)== 10 .AND. JAD2(I) > 0)THEN
          FUND = IFUNC2(I)     ! courbe N3 de unload                                                             
          NP2  = (NPF(FUND+1)-NPF(FUND))/2
          AN3Y0(I)= ZERO
          DXELA(I)=DX(I)-DPX(I)
          IF (((DXELA(I) >= ZERO).OR.(FXEP(I) >= ZERO)).AND.(FUND > 0)) THEN
C--- Tension - load curve is used
            DO  K=2,NP2                                                       
              K1=2*(K-2)                                                
              X1=TF(NPF(FUND)+K1)                                       
              X2=TF(NPF(FUND)+K1+2)                                     
              Y1=TF(NPF(FUND)+K1+1)                                     
              Y2=TF(NPF(FUND)+K1+3) 
              IF((FXEP(I)< Y2.AND.FXEP(I)>=Y1))THEN  
                AN3Y0(I)=(Y2-Y1)/ (X2-X1)                     
                EXIT     
              ENDIF 
            ENDDO
            IF (AN3Y0(I)== ZERO)THEN ! extrapolation 
              X1=TF(NPF(FUND)+(NP2-2)*2)                                       
              X2=TF(NPF(FUND)+(NP2-2)*2+2)                                     
              Y1=TF(NPF(FUND)+(NP2-2)*2+1)                                     
              Y2=TF(NPF(FUND)+(NP2-2)*2+3) 
C
              XI1=TF(NPF(FUND))                                       
              XI2=TF(NPF(FUND)+2)                                     
              YI1=TF(NPF(FUND)+1)                                     
              YI2=TF(NPF(FUND)+3) 
              IF(FXEP(I)>Y2)AN3Y0(I)=(Y2-Y1)/ (X2-X1) 
              IF(FXEP(I)<YI1)AN3Y0(I)=(YI2-YI1)/ (XI2-XI1)
            ENDIF
C----       Crossing of compression/tension line - mix stiffness computed
            IF ((DXELA(I) < ZERO).AND.(ABS(DDX(I)) > ZERO)) THEN
              DDXT = -FXEP(I)/AN3Y0(I)
              DDXC = DDX(I) - DDXT
              AN3Y0(I) = (DDXT/DDX(I))*AN3Y0(I) + (DDXC/DDX(I))*XKC(I)
            ENDIF
C
            IF (DXELA(I) >= ZERO) XX(I)=XX_OLD(I)+DDX(I)
          ELSE
C--- Compression - perfectly plastic behavior
            AN3Y0(I)= XKC(I)
          ENDIF
          FX(I)=FXEP(I)+AN3Y0(I)*DDX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        LINEAR ELASTIC in tension - perfleclty palstic in compression (same as 10 without curve)
C-------------------------------------
      IF(JECROU(11)>0)THEN
        DO I=1,NEL
          IF(IECROU(I)== 11) THEN                      
            AN3Y0(I)= ZERO
            DXELA(I)=DX(I)-DPX(I)
            IF ((DXELA(I) >= ZERO).OR.(FXEP(I) >= ZERO)) THEN
              AN3Y0(I)= XK(I)
C----       Crossing of compression/tension line - mix stiffness computed
              IF ((DXELA(I) < ZERO).AND.(ABS(DDX(I)) > ZERO)) THEN
                DDXT = -FXEP(I)/AN3Y0(I)
                DDXC = DDX(I) - DDXT
                AN3Y0(I) = (DDXT/DDX(I))*AN3Y0(I) + (DDXC/DDX(I))*XKC(I)
              ENDIF
            ELSE
              AN3Y0(I)= XKC(I)
            ENDIF                                                          
            FX(I)=FXEP(I)+AN3Y0(I)*DDX(I)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
c     VECTOR INTERPOLATION
C-------------------------------------
      DO I=1,NEL
        XX(I)  = XX(I) *LSCALE(I)                   
        XX2(I) = XX2(I)*LSCALE(I)                   
        XX3(I) = XX3(I)*LSCALE(I)                   
      ENDDO                          
C----s---1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF(ANY_PYTHON_FUNC) THEN
          IF(INTERP>=1) CALL VINTER_MIXED(PYTHON, TF,JAD,JPOS,JLEN,NEL,XX,DYDX,YY)
          IF(INTERP>=2) CALL VINTER_MIXED(PYTHON, TF,JAD2,JPOS2,JLEN2,NEL,XX2,DYDX2,YY2)
          IF(INTERP>=3) CALL VINTER_MIXED(PYTHON, TF,JAD ,JPOS3,JLEN3,NEL,XX3,DYDX3,YY3)
      ELSE
        IF (IRESP==1) THEN

          IF(INTERP>=1) CALL VINTER2DP(TF,JAD ,JPOS ,JLEN ,NEL,XX ,DYDX ,YY)
          IF(INTERP>=2) CALL VINTER2DP(TF,JAD2,JPOS2,JLEN2,NEL,XX2,DYDX2,YY2)
          IF(INTERP>=3) CALL VINTER2DP(TF,JAD ,JPOS3,JLEN3,NEL,XX3,DYDX3,YY3)
        ELSE
          IF(INTERP>=1) CALL VINTER2(TF,JAD ,JPOS ,JLEN ,NEL,XX ,DYDX ,YY )
          IF(INTERP>=2) CALL VINTER2(TF,JAD2,JPOS2,JLEN2,NEL,XX2,DYDX2,YY2)
          IF(INTERP>=3) CALL VINTER2(TF,JAD ,JPOS3,JLEN3,NEL,XX3,DYDX3,YY3)
        ENDIF
      ENDIF

      IF(INTERP>0)THEN
        DO I=1,NEL
          POS(1,I) = JPOS(I)
          POS(2,I) = JPOS2(I)
          POS(3,I) = JPOS3(I)
        ENDDO
      ENDIF
C-------------------------------------
C        LINEAIRE ELASTIQUE
C-------------------------------------
      IF(JECROU(-1)>0)THEN
        DO I=1,NEL
          IF(IFUNC(I)==0)THEN
            FX(I)=XK(I)*DX(I)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        NON LINEAIRE ELASTIQUE, F = f(total length)
C-------------------------------------
      IF (JECROU(8) > 0)THEN
        DO I=1,NEL
          IF (IECROU(I) == 8) FX(I)=YY(I)
        ENDDO
      ENDIF
C-------------------------------------
C        NON LINEAIRE ELASTIQUE
C-------------------------------------
      IF(JECROU(0)>0)THEN
        DO I=1,NEL
           IF(IFUNC(I) > 0 .AND.IECROU(I)==0) FX(I)=YY(I)
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (ISOTROPE)
C-------------------------------------
      IF(JECROU(1)>0)THEN
        DO I=1,NEL
         IF(IFUNC(I) /= 0.AND.IECROU(I)==1)THEN
           IF(FX(I)>=ZERO.AND.FX(I)>YY(I))THEN
             DPX(I)=DPX(I)+(FX(I)-YY(I))/XK(I)
             FX(I)=YY(I)
           ELSEIF(FX(I)<ZERO.AND.FX(I)<YY(I))THEN
             DPX(I)=DPX(I)+(YY(I)-FX(I))/XK(I)
             FX(I)=YY(I)
           ENDIF
           FXEP(I)=FX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (TENSION COMPRESSION DECOUPLEE)
C-------------------------------------
      IF(JECROU(2)>0)THEN
        DO I=1,NEL
         IF(IFUNC(I) /= 0.AND.IECROU(I)==2)THEN
           IF(DX(I)>DPX(I))THEN
             FX(I)  = XK(I) * (DX(I)-DPX(I))
             FXEP(I)= YY(I)
             FX(I)  = MIN(FX(I),FXEP(I))
             DPX(I) = DX(I) - FX(I) / XK(I) 
           ELSEIF(DX(I)<DPX2(I))THEN
             FX(I)   = XK(I) * (DX(I)-DPX2(I))
             FXEP(I) = YY(I)
             FX(I)   = MAX(FX(I),FXEP(I))
             DPX2(I) = DX(I) - FX(I) / XK(I) 
           ELSE
             FX(I)   = ZERO
           ENDIF
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (TENSION COMPRESSION COUPLEE 6 DOF COUPLES)
C-------------------------------------
      IF(JECROU(3)>0)THEN
        DO I=1,NEL
         IF(IFUNC(I) /= 0.AND.IECROU(I)==3)THEN
           IF(FX(I)>=ZERO.AND.FX(I)>YY(I))THEN
               EPLA(I)=EPLA(I)+ABS(YY(I)*(FX(I)-YY(I))/XK(I))
               FX(I)=YY(I)
           ELSEIF(FX(I)<ZERO.AND.FX(I)<YY(I))THEN
               EPLA(I)=EPLA(I)+ABS(YY(I)*(YY(I)-FX(I))/XK(I))
               FX(I)=YY(I)
           ENDIF
           FXEP(I)=FX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIQUE (ECOUISSAGE CINEMATIQUE)
C-------------------------------------
      IF(JECROU(4)>0)THEN
        DO I=1,NEL
          IF(IFUNC(I) /= 0.AND.IECROU(I)==4)THEN
            FX(I) = FXEP(I) + XK(I)*DDX(I)
            IC2(I)= 0
            IF(FX(I)>YY(I))THEN
              IC2(I)=1
              FX(I) = YY(I)
            ENDIF
            IF(FX(I)<YY2(I))THEN
              IC2(I)=2
              FX(I) = YY2(I)
            ENDIF
          ENDIF
        ENDDO
        DO I=1,NEL
          IF(IFUNC(I) /= 0.AND.IECROU(I)==4)THEN
            FXEP(I)=FX(I)
            DPX(I) = DX(I) - FX(I) / XK(I) 
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIC (TENSION COMPRESSION DECOUPLED)
C         (D/R)NONLINEAR RELOADING
C         DPX = MAXIMUM DISPLACEMENT (AND NOT PLASTIC)
C-------------------------------------
      IF(JECROU(5)>0)THEN
        DO I=1,NEL
         IF(IFUNC(I) /= 0.AND.IECROU(I)==5)THEN
           IF(DX(I)>DPX(I))THEN
             FX(I)=YY(I)
             DPX(I) = DX(I)
           ELSEIF(DX(I)>ZERO)THEN
             DPERM(I)=MAX(YY2(I),ZERO)
             IF(DX(I)>DPERM(I).AND.YY3(I)/=ZERO)THEN
               FMAX(I)=YY3(I)/LSCALE(I)
               DPERM(I)=MIN(DPERM(I),DPX(I)- FMAX(I) / XK(I))
C              y = a (x-x1)^b
               B1 = (DPX(I)-DPERM(I))*XK(I)/FMAX(I)
               FMIN(I) = FMAX(I) *
     .            ( (DX(I)-DPERM(I))/(DPX(I)-DPERM(I)) )**B1
               FMAX(I) = FMAX(I)*(DX(I)-DPERM(I))/(DPX(I)-DPERM(I))
               FX(I)=FXEP(I)+XK(I)*DDX(I)
               FX(I)=MAX(FX(I),FMIN(I),ZERO)
               FX(I)=MIN(FX(I),FMAX(I),YY(I))
             ELSE
               FX(I) = ZERO
             ENDIF
           ELSEIF(DX(I)<DPX2(I))THEN
             FX(I)=YY(I)
             DPX2(I) = DX(I)
           ELSEIF(DX(I)<ZERO)THEN
             DPERM(I)=YY2(I)
             DPERM(I)=MIN(DPERM(I),ZERO)
             IF(DX(I)<DPERM(I).AND.YY3(I)/=ZERO)THEN
               FMAX(I)=YY3(I)/LSCALE(I)
               DPERM(I)=MAX(DPERM(I),DPX2(I)- FMAX(I) / XK(I))
C              y = a (x-x1)^b
               B1 = (DPX2(I)-DPERM(I))*XK(I)/FMAX(I)
               FMIN(I) = FMAX(I)*
     .          ( (-DX(I)+DPERM(I))/(-DPX2(I)+DPERM(I)) )**B1
               FMAX(I) = FMAX(I)*(DX(I)-DPERM(I))/(DPX2(I)-DPERM(I))
               FX(I)=FXEP(I)+XK(I)*DDX(I)
               FX(I)=MIN(FX(I),FMIN(I),ZERO)
               FX(I)=MAX(FX(I),FMAX(I),YY(I))
             ELSE
               FX(I) = ZERO
             ENDIF
           ENDIF
           FXEP(I)=FX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C        ELASTO PLASTIC (ISOTROPIC HARDENING)
C-------------------------------------
      IF(JECROU(6)>0)THEN
        IF(ANY_PYTHON_FUNC) THEN 
          CALL VINTER_MIXED(PYTHON, TF,JAD,JPOS,JLEN,NEL,XX,DYDX,YY)
        ELSE
          CALL VINTER2(TF,JAD ,JPOS ,JLEN ,NEL,XX ,DYDX ,YY )
        ENDIF
        DO I=1,NEL
         IF(IFUNC(I) /=  0.AND.IECROU(I)== 6)THEN
           IF(FX(I)>= ZERO.AND.FX(I)>YIELD(I))THEN
               POS(1,I) = JPOS(I)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)    
               DPX(I)=DPX(I)+(FX(I)-YY(I))/AN3Y0(I)
               DXELA(I)=DX(I)-DPX(I)
               FX(I)=YY(I) !H1
               YIELD(I)=FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
C---FX< O 
           ELSEIF(FX(I)< ZERO.AND.FX(I)< -YIELD(I))THEN
               POS(1,I) = JPOS(I)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)  
               DPX(I)=DPX(I)+(YY(I)-FX(I))/AN3Y0(I)  
               DXELA(I)=DX(I)-DPX(I)
               FX(I)=YY(I)
               YIELD(I)=-FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
           ENDIF
           FXEP(I)=FX(I)
         ENDIF
        ENDDO
      ENDIF
C
C-------------------------------------
C        ELASTO PLASTIC (!!)
C-------------------------------------
      IF (JECROU(7) > 0) THEN
        DO I=1,NEL
          IF (IFUNC(I) /= 0 .AND. IECROU(I)==7) THEN
            FX(I) = FXEP(I) + XK(I)*DDX(I)
            IF (DX(I)>= DXOLD(I) .AND. DX(I)>=0) THEN
              IF (FX(I)>YY(I)) FX(I) = YY(I)                     
            ELSEIF (DX(I)< DXOLD(I) .AND. DX(I)>= 0) THEN
              IF  (FX(I) < YY2(I)) FX(I) = YY2(I)                    
            ELSEIF (DX(I)>= DXOLD(I) .AND. DX(I)<0) THEN
              IF (FX(I)> YY2(I)) FX(I) = YY2(I)                  
            ELSEIF (DX(I)< DXOLD(I) .AND. DX(I)<0) THEN
              IF (FX(I)< YY(I))  FX(I) = YY(I)
            ENDIF
            FXEP(I) = FX(I)                      
            DPX(I)  = DX(I) - FX(I) / XK(I)      
          ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C     SEATBELT - ELASTO PERFECTLY PLASTIC (ISOTROPIC HARDENING) - input by values
C-------------------------------------
      IF(JECROU(9)>0)THEN
        DO I=1,NEL
         IF(IECROU(I)== 9)THEN
           IF (ABS(XK(I)*XX(I)) > FX_MAX(I)) THEN
             YY(I) =SIGN(FX_MAX(I),XX(I))
             DYDX = 0
           ELSE
             YY(I) = XK(I)*XX(I)
             DYDX = XK(I) 
           ENDIF
           IF(FX(I)>= ZERO.AND.FX(I)>YIELD(I))THEN
c              POS(1,I) = JPOS(I)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)    
               DPX(I)=DPX(I)+(FX(I)-YY(I))/AN3Y0(I)
               DXELA(I)=DX(I)-DPX(I)
               FX(I)=YY(I) !H1
               YIELD(I)=FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
C---FX< O 
           ELSEIF(FX(I)< ZERO.AND.FX(I)< -YIELD(I))THEN
c              POS(1,I) = JPOS(I)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)  
               DPX(I)=DPX(I)+(YY(I)-FX(I))/AN3Y0(I)  
               DXELA(I)=DX(I)-DPX(I)
               FX(I)=YY(I)
               YIELD(I)=-FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
           ENDIF
           FXEP(I)=FX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C     SEATBELT - ELASTO PLASTIQUE (ECOUISSAGE ISOTROPE) in tension - perfleclty plastic in compression
C-------------------------------------
      IF(JECROU(10)>0)THEN
        IF(ANY_PYTHON_FUNC) THEN 
          CALL VINTER_MIXED(PYTHON, TF,JAD,JPOS,JLEN,NEL,XX,DYDX,YY)
        ELSE
          CALL VINTER2(TF,JAD ,JPOS ,JLEN ,NEL,XX ,DYDX ,YY )
        ENDIF
        DO I=1,NEL
         IF(IFUNC(I) /=  0.AND.IECROU(I)== 10)THEN
           IF(FX(I)> ZERO.AND.FX(I)>YIELD(I))THEN
               POS(1,I) = JPOS(I)
C-- COMPUTE PLASTIC AND ELASTIC DEFORMATION (TOTAL)    
               DPX(I)=DPX(I)+(FX(I)-YY(I))/AN3Y0(I)
               FX(I)=YY(I)
               YIELD(I)=FX(I)
C-- ECR variable for hardening/softening - always incremented with positive value
               XX_OLD(I) = XX_OLD(I) + ABS(DDX(I))
          ELSEIF(FX(I)<= -FX_MAX(I))THEN
               YY(I) = -FX_MAX(I)
C-- COMPUTE PLASTIC DEFORMATION (TOTAL)  
               DPX(I)=DPX(I)+(-YY(I)+FX(I))/AN3Y0(I)
               FX(I)=YY(I)
          ENDIF
          FXEP(I)=FX(I)
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------
C     SEATBELT - LINEAR ELASTIC in tension - perfleclty plastic in compression
C-------------------------------------
      IF(JECROU(11)>0)THEN
        DO I=1,NEL
          IF(IECROU(I)== 11)THEN
            IF(FX(I)<= -FX_MAX(I))THEN
              YY(I) = -FX_MAX(I)
C-- COMPUTE PLASTIC DEFORMATION (TOTAL)  
              DPX(I)=DPX(I)+(-YY(I)+FX(I))/AN3Y0(I)
              FX(I)=YY(I)
            ENDIF
            FXEP(I)=FX(I)
          ENDIF
        ENDDO
      ENDIF 
C--------------------------------------------------------------------
C     NON LINEAR DAMPING
C--------------------------------------------------------------------
      IF(IMPL_S==0.OR.IDYNA>0) THEN
!  rentrer un func4 (ou 3)
        IF(JDMP>0)THEN
          DO I=1,NEL
           JPOS(I) = NINT(POS(4,I))
           JFUNC=MAX(IFV(I),1)
           PYID1 = GET_PYTHON_FUNCT_ID(NFUNCT,JFUNC,NPF)
           IF(PYID1>0)THEN
            !python function
            JAD(I) = -PYID1
            JLEN(I)= -PYID1
            ANY_PYTHON_FUNC = .TRUE.
           ELSE
             JAD(I)  = NPF(JFUNC) / 2 + 1
             JLEN(I) = NPF(JFUNC+1) / 2 - JAD(I) - JPOS(I)
           ENDIF
          ENDDO

          IF(ANY_PYTHON_FUNC) THEN
            CALL VINTER_MIXED(PYTHON, TF,JAD,JPOS,JLEN,NEL,XX,DYDXV,GX)
          ELSE
            CALL VINTER2(TF,JAD,JPOS,JLEN,NEL,DVXS,DYDXV,GX)
          ENDIF
C
          DO I=1,NEL
            POS(4,I) = JPOS(I)
          ENDDO
        ENDIF
c---------------------------G * funct_id_4
        IF(J2DMP>0)THEN
          DO I=1,NEL
            J2POS(I) = NINT(POS(5,I))
            J2FUNC=MAX(IFUNC3(I),1)
            PYID2 = GET_PYTHON_FUNCT_ID(NFUNCT,J2FUNC,NPF)
            IF(PYID2>0)THEN
             !python function
             J2AD(I) = -PYID2
             J2LEN(I)= -PYID2
             ANY_PYTHON_FUNC = .TRUE.
            ELSE
              J2AD(I)  = NPF(J2FUNC) / 2 + 1
              J2LEN(I) = NPF(J2FUNC+1) / 2 - J2AD(I) - J2POS(I)
            ENDIF
          ENDDO
          IF(ANY_PYTHON_FUNC) THEN
            CALL VINTER_MIXED(PYTHON, TF,J2AD,J2POS,J2LEN,NEL,XX,DYDXV2,GX2)
            IF(PRESENT(MAX_SLOPE)) THEN
              DO I=1,NEL
                J2POS(I) = NINT(POS(5,I))
                J2FUNC=MAX(IFUNC3(I),1)
                PYID2 = GET_PYTHON_FUNCT_ID(NFUNCT,J2FUNC,NPF)
                IF(PYID2>0) MAX_SLOPE(I) = MAX(MAX_SLOPE(I), TWO*ABS(DYDXV2(I)))
              ENDDO
            ENDIF
 
          ELSE
            CALL VINTER2(TF,J2AD,J2POS,J2LEN,NEL,DVXS,DYDXV2,GX2)
          ENDIF
          DO I=1,NEL
            POS(5,I) = J2POS(I)
          ENDDO
        ENDIF
c-------------------------        
        IF(JDMP/=NEL)THEN
          DO I=1,NEL
           IF(IFV(I)==0) GX(I)=ZERO
          ENDDO
        ENDIF
        IF(J2DMP/=NEL)THEN 
          DO I=1,NEL            
           IF(IFUNC3(I)==0) GX2(I)=ZERO
          ENDDO
        ENDIF

        DO I=1,NEL
          DVV  = MAX(ONE,ABS(DVX(I)/D(I)))
          DFAC = AK(I) + B(I) * LOG(DVV) + EE(I)*GX(I)
          FX(I)= ( DFAC*FX(I) + XC(I)*DVX(I) + GF3(I)*GX2(I) ) *OFF(I)
          E(I) = E(I) + (DX(I)-DXOLD(I)) * (FX(I)+FOLD(I)) * HALF
        ENDDO
      ELSE
        DO I=1,NEL
         FX(I)= FX(I)  *AK(I)* OFF(I)
         E(I) = E(I) + (DX(I)-DXOLD(I)) * (FX(I)+FOLD(I)) * HALF
        ENDDO
      ENDIF
      DO I=1,NEL
        DX(I)=DX(I)*XL0(I)
        DXOLD(I)=DXOLD(I)*XL0(I)
        DPX(I)=DPX(I)*XL0(I)
        DPX2(I)=DPX2(I)*XL0(I)
        E(I)=E(I)*XL0(I)
      ENDDO
C
C----
      RETURN
      END
      END MODULE 
