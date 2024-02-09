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
Chd|====================================================================
Chd|  SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|-- called by -----------
Chd|        SUFORC3                       source/user_interface/suforc3.F
Chd|-- calls ---------------
Chd|        ENG_USERLIB_GET_LAW_VAR       source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_SET_LAW_VAR       source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_SIGEPS99          source/user_interface/dyn_userlib.c
Chd|        FAIL_CONNECT                  source/materials/fail/connect/fail_connect.F
Chd|        FAIL_SNCONNECT                source/materials/fail/snconnect/fail_snconnect.F
Chd|        SBILAN43                      source/elements/solid/sconnect/sbilan43.F
Chd|        SCONNECT_OFF                  source/elements/solid/sconnect/sconnect_off.F
Chd|        SCOOR43                       source/elements/solid/sconnect/scoor43.F
Chd|        SDEF43                        source/elements/solid/sconnect/sdef43.F
Chd|        SFINT43                       source/elements/solid/sconnect/sfint43.F
Chd|        SIGEPS116                     source/materials/mat/mat116/sigeps116.F
Chd|        SIGEPS117                     source/materials/mat/mat117/sigeps117.F
Chd|        SIGEPS120_CONNECT_MAIN        source/materials/mat/mat120/sigeps120_connect_main.F
Chd|        SIGEPS59                      source/materials/mat/mat059/sigeps59.F
Chd|        SIGEPS83                      source/materials/mat/mat083/sigeps83.F
Chd|        SMOM43                        source/elements/solid/sconnect/smom43.F
Chd|        SRROTA3                       source/elements/solid/solide/srrota3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        LAW_USERSO                    source/user_interface/law_userso.F
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SUSER43(
     1 ELBUF_STR,IOUT  ,IPROP  ,IMAT   ,NGL    ,TIME  ,TIMESTEP,FR_WAVE,
     2 XX1      ,XX2   ,XX3    ,XX4    ,XX5    ,XX6    ,XX7    ,XX8    ,   
     3 YY1      ,YY2   ,YY3    ,YY4    ,YY5    ,YY6    ,YY7    ,YY8    , 
     4 ZZ1      ,ZZ2   ,ZZ3    ,ZZ4    ,ZZ5    ,ZZ6    ,ZZ7    ,ZZ8    ,
     5 UX1      ,UX2   ,UX3    ,UX4    ,UX5    ,UX6    ,UX7    ,UX8    ,
     6 UY1      ,UY2   ,UY3    ,UY4    ,UY5    ,UY6    ,UY7    ,UY8    ,
     7 UZ1      ,UZ2   ,UZ3    ,UZ4    ,UZ5    ,UZ6    ,UZ7    ,UZ8    ,
     8 VX1      ,VX2   ,VX3    ,VX4    ,VX5    ,VX6    ,VX7    ,VX8    ,
     9 VY1      ,VY2   ,VY3    ,VY4    ,VY5    ,VY6    ,VY7    ,VY8    ,
     A VZ1      ,VZ2   ,VZ3    ,VZ4    ,VZ5    ,VZ6    ,VZ7    ,VZ8    ,
     B FX1      ,FX2   ,FX3    ,FX4    ,FX5    ,FX6    ,FX7    ,FX8    ,
     F FY1      ,FY2   ,FY3    ,FY4    ,FY5    ,FY6    ,FY7    ,FY8    ,
     G FZ1      ,FZ2   ,FZ3    ,FZ4    ,FZ5    ,FZ6    ,FZ7    ,FZ8    ,
     H STIFM    ,STIFR ,VISCM  ,VISCR  ,PARTSAV,IPARTS ,BUFMAT ,IOUTPRT,
     L IFAILURE ,NPF   ,TF     ,IPM    ,IGEO   ,NPG    ,NEL    ,JSMS   ,
     M DMELS    ,PM    ,GEO    ,ITASK  ,JTHE   ,TABLE  ,MAT_PARAM,
     N IDTMINS,DTFACS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MAT_ELEM_MOD            
      USE LAW_USERSO
      USE SIGEPS169_CONNECT_MOD
C-------------------------------------------------------------------------
C     This subroutine compute user 8 nodes solids forces and moments.
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C NEL      |  1      | I | R | NUMBER OF ELEMENTS IN CURRENT GROUP
C NUVAR    |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L01 file)
C IPROP    |  1      | I | R | PROPERTY NUMBER
C IMAT     |  1      | I | R | MATERIAL NUMBER
C NGL |  NEL    | I | R | SOLID ELEMENT ID
C----------+---------+---+---+--------------------------------------------
C TIME     |  1      | F | R | CURRENT TIME 
C TIMESTEP |  1      | F | R | CURRENT TIME STEP
C----------+---------+---+---+--------------------------------------------
C EINT     |  NEL    | F | R | TOTAL INTERNAL ENERGY at t=TIME-TIMESTEP
C          |         |   |   | Internal energy is automatically recomputed
C          |         |   |   | at each cycle.
C VOL      |  NEL    | F | R | INITIAL VOLUME
C----------+---------+---+---+--------------------------------------------
C UVAR     |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLES
C          |*NPG     |   |   | NUVAR IS DEFINED IN LECG29 (not in LECMAT29)
C FR_WAVE  |  NEL    | F |R/W| COMMUNICATION ARRAY TO ELEMENTS CONNECTED
C          |         |   |   | TO COMMON NODES
C----------+---------+---+---+--------------------------------------------
C OFF      |  NEL    | F |R/W| DELETE FLAG (=1. ON =0. OFF)
C RHO      |  NEL    | F |R/W| DENSITY
C SIG      | 6*NEL   | F |R/W| STRESS TENSOR SX,SY,SZ,SXY,SYZ,SZX
C          |         |   |   | RHO and SIG cann be used for post processing 
C          |         |   |   | in TH++ or ModAnim
C          |         |   |   | The modification of these variables has only 
C          |         |   |   | effect on output. If these value are not
C          |         |   |   | modified RHO and SIG are initial values
C----------+---------+---+---+--------------------------------------------
C XX1      |   NEL   | F | R | X COORDINATE NODE 1 in global frame at time TIME
C YY1      |   NEL   | F | R | Y COORDINATE NODE 1 in global frame at time TIME
C ZZ1      |   NEL   | F | R | Z COORDINATE NODE 1 in global frame at time TIME
C XX2..ZZ8 |   NEL   | F | R | SAME FOR NODE 2 TO 8
C UX1      |   NEL   | F | R | X DISPLACEMENT NODE 1,global frame, time TIME
C UY1      |   NEL   | F | R | Y DISPLACEMENT NODE 1,global frame, time TIME
C UZ1      |   NEL   | F | R | Z DISPLACEMENT NODE 1,global frame, time TIME
C VX1      |   NEL   | F | R | X VELOCITY NODE 1,glob f, time TIME-TIMESTEP/2
C VY1      |   NEL   | F | R | Y VELOCITY NODE 1,glob f, time TIME-TIMESTEP/2
C VZ1      |   NEL   | F | R | Z VELOCITY NODE 1,glob f, time TIME-TIMESTEP/2
C          |         |   |   | displacement increment from t=TIME-TIMESTEP
C          |         |   |   | to t=TIME is given by DUX1 = VX1(I)*TIMESTEP
C VRX1     |   NEL   | F | R | X ROTATIONAL VELOCITY NODE 1 ...
C VRY1     |   NEL   | F | R | Y ROTATIONAL VELOCITY NODE 1 ...
C VRZ1     |   NEL   | F | R | Z ROTATIONAL VELOCITY NODE 1 ...
C-------------------------------------------------------------------------
C FX1      |   NEL   | F | W | X FORCE NODE 1
C FY1      |   NEL   | F | W | Y FORCE NODE 1
C FZ1      |   NEL   | F | W | Z FORCE NODE 1
C ....
C MX1      |   NEL   | F | W | X MOMENT NODE 1
C MY1      |   NEL   | F | W | Y MOMENT NODE 1
C MZ1      |   NEL   | F | W | Z MOMENT NODE 1
C STIFM    |   NEL   | F | W | TRANSLATIONAL STIFNESS OVERESTIMATION
C STIFR    |   NEL   | F | W | ROTATIONAL STIFNESS OVERESTIMATION
C VISCM    |   NEL   | F | W | TRANSLATIONAL VISCOSITY OVERESTIMATION
C VISCR    |   NEL   | F | W | ROTATIONAL VISCOSITY OVERESTIMATION
C          |         |   |   | STIFM,STIFR,VISCM,VISCR are needed to compute
C          |         |   |   | element or nodal time step. 
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr19_c.inc"
#include      "task_c.inc"
#include      "userlib.inc"
#include      "timeri_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s  
C----------------------------------------------------------
      INTEGER NEL,IOUT,IPROP,IMAT,IOUTPRT,IFAILURE,NPG, JSMS,JTHE
      INTEGER NGL(NEL),IPARTS(*),NPF(*),IPM(NPROPMI,*),
     .  IGEO(NPROPGI,*),ITASK
      TARGET  :: IPM
      INTEGER  :: IDTMINS
      my_real  :: DTFACS
      my_real
     .  TIME,TIMESTEP,PARTSAV(NPSAV,*),DMELS(*),PM(NPROPM,*),
     .  STIFM(*) ,STIFR(*) , VISCM(*) ,VISCR(*) ,FR_WAVE(*),TF(*),             
     .  XX1(*),XX2(*),XX3(*),XX4(*),XX5(*),XX6(*),XX7(*),XX8(*),         
     .  YY1(*),YY2(*),YY3(*),YY4(*),YY5(*),YY6(*),YY7(*),YY8(*),         
     .  ZZ1(*),ZZ2(*),ZZ3(*),ZZ4(*),ZZ5(*),ZZ6(*),ZZ7(*),ZZ8(*),         
     .  UX1(*),UX2(*),UX3(*),UX4(*),UX5(*),UX6(*),UX7(*),UX8(*),         
     .  UY1(*),UY2(*),UY3(*),UY4(*),UY5(*),UY6(*),UY7(*),UY8(*),         
     .  UZ1(*),UZ2(*),UZ3(*),UZ4(*),UZ5(*),UZ6(*),UZ7(*),UZ8(*),         
     .  VX1(*),VX2(*),VX3(*),VX4(*),VX5(*),VX6(*),VX7(*),VX8(*),         
     .  VY1(*),VY2(*),VY3(*),VY4(*),VY5(*),VY6(*),VY7(*),VY8(*),         
     .  VZ1(*),VZ2(*),VZ3(*),VZ4(*),VZ5(*),VZ6(*),VZ7(*),VZ8(*),         
     .  FX1(*),FX2(*),FX3(*),FX4(*),FX5(*),FX6(*),FX7(*),FX8(*),         
     .  FY1(*),FY2(*),FY3(*),FY4(*),FY5(*),FY6(*),FY7(*),FY8(*),         
     .  FZ1(*),FZ2(*),FZ3(*),FZ4(*),FZ5(*),FZ6(*),FZ7(*),FZ8(*),
     .  GEO(NPROPG,*)
      TYPE (ELBUF_STRUCT_)  ,TARGET :: ELBUF_STR
      my_real ,DIMENSION(*) ,TARGET :: BUFMAT
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,I1,I2,II,JJ(6),J,J1,J2,IC,IR,IPG,JPT,IUV,IEL,ISRATE,
     .  NPAR,NPARF,NVARF,NFUNC,NFUNCR,NFAILS,ISMSTR,ILAW_USER,IPTR,IPTS,IPTT,
     .  IADBUF,IFAIL,NUVAR,MTN,NDAMF,ISOLID,ISOLIDF,NUMTABL,NVARTMP
      INTEGER IFUNC(MAXFUNC),IFUNCR(MAXFUNC)
      my_real :: A,B,C,ALPHA,EPSP,ASRATE,TTHICK,OFF_EL
      my_real 
     .   HH(NPG,NPG),AREAP(MVSIZ,NPG),AREAT(MVSIZ),
     .   UXLOC(MVSIZ,8),UYLOC(MVSIZ,8),UZLOC(MVSIZ,8),
     .   VXLOC(MVSIZ,8),VYLOC(MVSIZ,8),VZLOC(MVSIZ,8),
     .   VXZ(MVSIZ,NPG),VYZ(MVSIZ,NPG),VZZ(MVSIZ,NPG),
     .   VGXA(MVSIZ),VGYA(MVSIZ),VGZA(MVSIZ), VGA2(MVSIZ),
     .   R1X(MVSIZ),R2X(MVSIZ),R3X(MVSIZ),R4X(MVSIZ),
     .   R5X(MVSIZ),R6X(MVSIZ),R7X(MVSIZ),R8X(MVSIZ),
     .   R1Y(MVSIZ),R2Y(MVSIZ),R3Y(MVSIZ),R4Y(MVSIZ),
     .   R5Y(MVSIZ),R6Y(MVSIZ),R7Y(MVSIZ),R8Y(MVSIZ),
     .   R1Z(MVSIZ),R2Z(MVSIZ),R3Z(MVSIZ),R4Z(MVSIZ),
     .   R5Z(MVSIZ),R6Z(MVSIZ),R7Z(MVSIZ),R8Z(MVSIZ),
     .   RXX(MVSIZ),RYY(MVSIZ),RZZ(MVSIZ),EP1(MVSIZ),EP2(MVSIZ),
     .   EP3(MVSIZ),SIG0ZZ(MVSIZ),SIG0YZ(MVSIZ),SIG0ZX(MVSIZ),
     .   DEIN(MVSIZ),DEIT(MVSIZ),SYM(MVSIZ),SSP(MVSIZ),RHO0(MVSIZ),
     .   E1X(MVSIZ),E2X(MVSIZ),E3X(MVSIZ),E1Y(MVSIZ),E2Y(MVSIZ),
     .   E3Y(MVSIZ),E1Z(MVSIZ),E2Z(MVSIZ),E3Z(MVSIZ),VISCMAX(MVSIZ),
     .   BID(MVSIZ),DPLA(MVSIZ),SIGY(MVSIZ),EPSZZ(MVSIZ),EPSYZ(MVSIZ),
     .   EPSZX(MVSIZ),DEPSZZ(MVSIZ),DEPSYZ(MVSIZ),DEPSZX(MVSIZ),
     .   SIGNZZ(MVSIZ),SIGNYZ(MVSIZ),SIGNZX(MVSIZ),SOFT(NEL),DELTAE(NEL),
     .   USER_PLA(MVSIZ),USER_OFF(MVSIZ),USER_EINT(MVSIZ),USER_RHO(MVSIZ),USER_VOL(MVSIZ)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_MAT_)  ,POINTER :: MATBUF
      TYPE(FAIL_LOC_) ,POINTER :: FLOC
      TARGET :: AREAP   
      TYPE(BUF_FAIL_), POINTER  :: FBUF
      TYPE(ULAWINTBUF) :: USERBUF
      TYPE (TTABLE)  , DIMENSION(NTABLE) ::  TABLE
      INTEGER, DIMENSION(:) ,POINTER   :: ITABLE,VARTMP
      my_real, DIMENSION(:),POINTER :: EPLASN,EPLAST,EPSD,UPARAM,UVAR,AREA,AREAN,OFFI
     
C=======================================================================
      GBUF  => ELBUF_STR%GBUF
      
      I7KGLO = 1
      ISOLID = 4  ! All Gauss points must fail before deleting the element
      IADBUF = IPM(7,IMAT) 
      NUVAR  = IPM(8,IMAT)
      NPAR   = IPM(9,IMAT)
      NFUNC  = IPM(10,IMAT) 
      NUMTABL= IPM(226,IMAT)                 

      MTN = IPM(2,IMAT)   
      DO I=1,NFUNC               
        IFUNC(I)=IPM(10+I,IMAT)  
      ENDDO 

      ITABLE => IPM(226+1:226+NUMTABL,IMAT)
                     
      ISRATE = IPM(3,IMAT)
      ALPHA  = PM(9,IMAT)
      
      ISMSTR = IGEO(5,IPROP)
      TTHICK = GEO(41,IPROP)
      ISOLIDF = 4
c
      UPARAM => BUFMAT(IADBUF:IADBUF+NPAR-1) 
!
      DO I=1,6
        JJ(I) = NEL*(I-1)
      ENDDO
!
C
      CALL SCOOR43(
     .         GBUF%OFF   ,NEL  ,IOUTPRT    ,GBUF%GAMA ,
     .         XX1  ,XX2  ,XX3  ,XX4  ,XX5  ,XX6  ,XX7  ,XX8  ,   
     .         YY1  ,YY2  ,YY3  ,YY4  ,YY5  ,YY6  ,YY7  ,YY8  ,   
     .         ZZ1  ,ZZ2  ,ZZ3  ,ZZ4  ,ZZ5  ,ZZ6  ,ZZ7  ,ZZ8  ,   
     .         VX1  ,VX2  ,VX3  ,VX4  ,VX5  ,VX6  ,VX7  ,VX8  ,   
     .         VY1  ,VY2  ,VY3  ,VY4  ,VY5  ,VY6  ,VY7  ,VY8  ,   
     .         VZ1  ,VZ2  ,VZ3  ,VZ4  ,VZ5  ,VZ6  ,VZ7  ,VZ8  ,   
     .         R1X  ,R2X  ,R3X  ,R4X  ,R5X  ,R6X  ,R7X  ,R8X  ,
     .         R1Y  ,R2Y  ,R3Y  ,R4Y  ,R5Y  ,R6Y  ,R7Y  ,R8Y  ,
     .         R1Z  ,R2Z  ,R3Z  ,R4Z  ,R5Z  ,R6Z  ,R7Z  ,R8Z  ,
     .         RXX  ,RYY  ,RZZ  ,VXLOC,VYLOC,VZLOC,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         AREAP,TIME ,TIMESTEP,NGL,
     .         VGXA ,VGYA ,VGZA ,VGA2, SYM  , IPM, IMAT)
c
      CALL SDEF43(NEL    ,NPG    ,HH     ,
     .            VXZ    ,VYZ    ,VZZ    ,VXLOC,VYLOC,VZLOC)
c-----------------------------------------------------------------------  
      DO I=1,NEL               
        AREAT(I) = ZERO           
        VISCM(I) = ZERO            
        VISCR(I) = ZERO            
        STIFM(I) = ZERO           
        STIFR(I) = ZERO            
        GBUF%SIG(JJ(3)+I) = ZERO
        GBUF%SIG(JJ(5)+I) = ZERO
        GBUF%SIG(JJ(6)+I) = ZERO
        EP1(I)   = ZERO 
        EP2(I)   = ZERO 
        EP3(I)   = ZERO 
        FX1(I)   = ZERO
        FX2(I)   = ZERO
        FX3(I)   = ZERO
        FX4(I)   = ZERO
        FY1(I)   = ZERO
        FY2(I)   = ZERO
        FY3(I)   = ZERO
        FY4(I)   = ZERO
        FZ1(I)   = ZERO
        FZ2(I)   = ZERO
        FZ3(I)   = ZERO
        FZ4(I)   = ZERO
        FX5(I)   = ZERO
        FX6(I)   = ZERO
        FX7(I)   = ZERO
        FX8(I)   = ZERO
        FY5(I)   = ZERO
        FY6(I)   = ZERO
        FY7(I)   = ZERO
        FY8(I)   = ZERO
        FZ5(I)   = ZERO
        FZ6(I)   = ZERO
        FZ7(I)   = ZERO
        FZ8(I)   = ZERO
      ENDDO       
      IF (GBUF%G_PLA > 0) GBUF%PLA(:NEL) = ZERO
      IF (ISMSTR == 1 .AND. TIME == ZERO) THEN
        DO IPG = 1,NPG         
          ELBUF_STR%BUFLY(1)%LBUF(IPG,1,1)%VOL(1:NEL)=AREAP(1:NEL,IPG)
        ENDDO
      ENDIF
C-------------------
C     MEAN STRAIN RATE
C-------------------
      IF (MTN == 116) THEN
        CONTINUE
      ELSE
        DO IPG=1,NPG                                          
          DO I=1,NEL                                          
            EP1(I) = EP1(I) + VXZ(I,IPG)
            EP2(I) = EP2(I) + VYZ(I,IPG)                       
            EP3(I) = EP3(I) + VZZ(I,IPG)                       
          ENDDO                                               
        ENDDO                                                 
        DO I=1,NEL                                            
          EP1(I) = EP1(I)*FOURTH                               
          EP2(I) = EP2(I)*FOURTH                               
          EP3(I) = EP3(I)*FOURTH                               
          EPSP   = SQRT(EP1(I)**2 + EP2(I)**2 + EP3(I)**2)    
                                                              
          IF (ISRATE > 0) THEN                                
            ASRATE = MIN(ONE,ALPHA*TIMESTEP)                    
            EPSP = ASRATE*EPSP + (ONE - ASRATE)*GBUF%EPSD(I)    
          ENDIF                                               
          GBUF%EPSD(I)=EPSP                                   
        ENDDO
      END IF                                            
C--------------------------------------------------
      DELTAE(1:NEL) = ZERO
c--------------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
c--------------------------------------------------
c     BOUCLE SUR LES POINTS DE GAUSS
c--------------------------------------------------
      DO IPG = 1,NPG
        LBUF => ELBUF_STR%BUFLY(1)%LBUF(IPG,1,1)
        UVAR => ELBUF_STR%BUFLY(1)%MAT(IPG,1,1)%VAR

        NVARTMP = ELBUF_STR%BUFLY(1)%NVARTMP
        VARTMP => ELBUF_STR%BUFLY(1)%MAT(IPG,1,1)%VARTMP

        EPSD(1:NEL) => LBUF%EPSD(1:NEL)
c
        AREAN(1:NEL) => AREAP(1:NEL,IPG)

        IF (ISMSTR == 1) THEN ! read in property
          AREA(1:NEL)  => ELBUF_STR%BUFLY(1)%LBUF(IPG,1,1)%VOL(1:NEL)
        ELSE
          AREA(1:NEL) => AREAP(1:NEL,IPG)
        ENDIF
C
        DO IEL=1,NEL
          OFF_EL = LBUF%OFF(IEL)
          DEPSZZ(IEL) = VZZ(IEL,IPG)*TIMESTEP * OFF_EL
          DEPSYZ(IEL) = VYZ(IEL,IPG)*TIMESTEP * OFF_EL
          DEPSZX(IEL) = VXZ(IEL,IPG)*TIMESTEP * OFF_EL
          SIG0ZZ(IEL) = LBUF%SIG(JJ(3)+IEL)
          SIG0YZ(IEL) = LBUF%SIG(JJ(5)+IEL)
          SIG0ZX(IEL) = LBUF%SIG(JJ(6)+IEL)
          SIGNZZ(IEL) = ZERO
          SIGNYZ(IEL) = ZERO
          SIGNZX(IEL) = ZERO
          DEIN(IEL)   = ZERO           
          DEIT(IEL)   = ZERO           
        ENDDO
        IF (ELBUF_STR%BUFLY(1)%L_EPE > 0) THEN
          DO IEL=1,NEL
            EPSZZ(IEL)  = LBUF%EPE(JJ(1)+IEL) + DEPSZZ(IEL) 
            EPSYZ(IEL)  = LBUF%EPE(JJ(2)+IEL) + DEPSYZ(IEL) 
            EPSZX(IEL)  = LBUF%EPE(JJ(3)+IEL) + DEPSZX(IEL) 
          ENDDO
        END IF
c--------------------------------------------------
c       material laws           
c--------------------------------------------------
        IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
c--------------------------------------------------
        SELECT CASE(MTN)
c---
          CASE (59)
c
            EPLASN => LBUF%PLA(1:NEL)
            EPLAST => LBUF%PLA(NEL+1:NEL*2)
c
            CALL SIGEPS59(
     1        NEL      ,TIME     ,TIMESTEP,UPARAM   ,GBUF%OFF     ,
     2        GBUF%EPSD,STIFM    ,NPAR    ,
     3        IFUNC    ,MAXFUNC  ,NPF     ,TF       ,AREA     ,
     4        EPSZZ    ,EPSYZ    ,EPSZX   ,DEPSZZ   ,DEPSYZ   ,DEPSZX  ,
     5        SIG0ZZ   ,SIG0YZ   ,SIG0ZX  ,SIGNZZ   ,SIGNYZ   ,SIGNZX  ,
     6        EPLASN   ,EPLAST   ,JSMS    ,DMELS    )
C
c-------
          CASE (83)
c
            CALL SIGEPS83(
     1         NEL      ,TIME     ,TIMESTEP ,UPARAM   ,GBUF%OFF     ,  
     2         LBUF%EPSD,STIFM    ,IFUNC    ,MAXFUNC  ,NPF      ,TF      ,                                     
     3         AREA     ,DEPSZZ   ,DEPSYZ   ,DEPSZX   ,NPAR     ,EPSZZ   ,  
     4         SIG0ZZ   ,SIG0YZ   ,SIG0ZX   ,SIGNZZ   ,SIGNYZ   ,SIGNZX  ,  
     5         LBUF%PLA ,JSMS     ,DMELS    ,SYM      ,UVAR     ,NUVAR   ,  
     6         LBUF%DMG ,ASRATE   ,ISRATE   )                                                      
c           mean Gauss point values for element output                                                     
            DO IEL=1,NEL
              GBUF%PLA(IEL)  = GBUF%PLA(IEL)  + FOURTH*LBUF%PLA(IEL)
              GBUF%EPSD(IEL) = GBUF%EPSD(IEL) + FOURTH*LBUF%EPSD(IEL)
            ENDDO
c-------
          CASE (116)
c
            EPLASN => LBUF%PLA(1:NEL)
            EPLAST => LBUF%PLA(NEL+1:NEL*2)
c
            CALL SIGEPS116(
     1        NEL      ,NPAR     ,NUVAR    ,JSMS     ,TIME     ,TIMESTEP ,
     2        UPARAM   ,UVAR     ,AREA     ,EPSD     ,GBUF%OFF ,LBUF%OFF ,   
     3        EPSZZ    ,EPSYZ    ,EPSZX    ,DEPSZZ   ,DEPSYZ   ,DEPSZX   ,
     4        SIGNZZ   ,SIGNYZ   ,SIGNZX   ,STIFM    ,DMELS    ,LBUF%DMG ,
     5        EPLASN   ,EPLAST   ,IPG      ,ISOLIDF  ,NGL      )

            GBUF%EPSD(1:NEL) = GBUF%EPSD(1:NEL) + FOURTH*LBUF%EPSD(1:NEL)
c
          CASE (117)
c
c
            CALL SIGEPS117(
     1        NEL      ,NPAR     ,NUVAR    ,JSMS     ,TIME     ,TIMESTEP ,
     2        UPARAM   ,UVAR     ,AREA     ,GBUF%OFF ,LBUF%OFF ,   
     3        EPSZZ    ,EPSYZ    ,EPSZX    ,DEPSZZ   ,DEPSYZ   ,DEPSZX   ,
     4        SIGNZZ   ,SIGNYZ   ,SIGNZX   ,STIFM    ,DMELS    ,LBUF%DMG ,
     5        IPG      ,ISOLIDF  ,NGL      ,NFUNC    ,IFUNC    ,NPF    ,TF)

          CASE (120) ! TAPO model
            CALL SIGEPS120_CONNECT_MAIN(
     1         NEL      ,NGL      ,TIME     ,TIMESTEP ,UPARAM   ,GBUF%OFF     ,  
     2         LBUF%EPSD,STIFM    ,JTHE     ,      
     3         AREA     ,DEPSZZ   ,DEPSYZ   ,DEPSZX   ,EPSZZ    ,NPAR     ,
     4         SIG0ZZ   ,SIG0YZ   ,SIG0ZX   ,SIGNZZ   ,SIGNYZ   ,SIGNZX  ,  
     5         LBUF%PLA ,JSMS     ,DMELS    ,UVAR     ,NUVAR    ,
     6         NUMTABL  ,ITABLE   ,TABLE    ,NVARTMP  ,VARTMP   ,LBUF%TEMP,
     7         LBUF%DMG)                                           
c           mean Gauss point values for element output                                                     
            DO IEL=1,NEL
              GBUF%PLA(IEL)  = GBUF%PLA(IEL)  + FOURTH*LBUF%PLA(IEL)
              GBUF%EPSD(IEL) = GBUF%EPSD(IEL) + FOURTH*LBUF%EPSD(IEL)
            ENDDO 
C-----------------------------------------------
          CASE (169) ! MAT_ARUP_ADHESIVE

           CALL SIGEPS169_CONNECT(
     1        NEL      ,TIME     ,MAT_PARAM(IMAT)%UPARAM, MAT_PARAM(IMAT)%IPARAM ,
     2        MAT_PARAM(IMAT)%NIPARAM     ,MAT_PARAM(IMAT)%NUPARAM    ,STIFM    ,
     3        AREA     ,GBUF%OFF ,NUVAR   ,UVAR     ,IPG     ,
     4        DEPSZZ   ,DEPSYZ   ,DEPSZX  ,EPSZZ    ,EPSYZ    ,EPSZX   ,
     5        SIG0ZZ   ,SIG0YZ   ,SIG0ZX  ,SIGNZZ   ,SIGNYZ   ,SIGNZX  ,
     6        LBUF%PLA ,IOUT     ,JSMS       ,LBUF%DMG,NGL    ,DMELS   ,IDTMINS,DTFACS)
C-----------------------------------------------
         CASE (99)
c
          IF (USERL_AVAIL>0) THEN
            IPTR = IPG
            IPTS = 1
            IPTT = 1
            DO IEL=1,NEL
              BID(IEL)  = ZERO
              RHO0(IEL) = PM(1,IMAT)
              USER_PLA(IEL)  = LBUF%PLA(IEL)
              USER_OFF(IEL)  = GBUF%OFF(IEL)
              USER_EINT(IEL) = GBUF%EINT(IEL)
              USER_RHO(IEL)  = GBUF%RHO(IEL)
              USER_VOL(IEL)  = GBUF%VOL(IEL)
            ENDDO
            ILAW_USER  = IPM(217, IMAT)
            NUVAR = ELBUF_STR%BUFLY(1)%NVAR_MAT
c
C           Fill Structure in dynamical library
            CALL ENG_USERLIB_GET_LAW_VAR(
     *           NCYCLE, IMAT,IPTR, IPTS,IPTT,
     *           E1X  ,E1Y  ,E1Z   ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .           BID  ,BID, SIG0ZZ, BID,  SIG0YZ,
     *           SIG0ZX,    BID, BID, EP1, BID, EP2,  EP3,  
     *           BID,    BID, BID, BID, BID, BID,  BID,  
     *           BID,    DEPSZZ, BID, DEPSYZ, DEPSZX, RHO0, BID,   
     *           BID,    SIGNZZ,  BID,  SIGNYZ,  SIGNZX,  BID,  BID,  
     *            BID,    BID, BID, BID )                 
c
c           Call user law in dynamical user library
            CALL  ENG_USERLIB_SIGEPS99(
     *            NEL     ,NPAR  ,NUVAR  ,ILAW_USER,NFUNC,
     *            IFUNC   ,NPF   ,TF     ,TIME     ,TIMESTEP,
     *            BUFMAT(IADBUF) ,USER_RHO,USER_VOL,USER_EINT,NGL,
     *            SSP            ,VISCMAX ,UVAR ,USER_OFF  ,SIGY ,
     *            USER_PLA     )
c
C           Get back results from user library structure
            CALL ENG_USERLIB_SET_LAW_VAR(
     *           BID   ,BID   ,SIGNZZ    ,BID    ,SIGNYZ    ,SIGNZX      ,
     *           BID   ,BID   ,BID   ,BID    ,BID   ,BID     ,
     *           DPLA  )
c
            DO IEL=1,NEL              
              LBUF%PLA(IEL)  = USER_PLA(IEL)
              GBUF%OFF(IEL)  = USER_OFF(IEL)
              GBUF%EINT(IEL) = USER_EINT(IEL)
              GBUF%RHO(IEL)  = USER_RHO(IEL)
              GBUF%VOL(IEL)  =  USER_VOL(IEL)
              STIFM(IEL) = SSP(IEL)*SSP(IEL)*AREA(IEL)*GBUF%RHO(IEL)
            ENDDO
          ENDIF  
c-------  
        END SELECT     !  MTN
c--------------------------------------------------
        DO IEL=1,NEL
          DEIN(IEL) = LBUF%OFF(IEL)*HALF*                     
     .                DEPSZZ(IEL)*(SIG0ZZ(IEL) + SIGNZZ(IEL))   
          DEIT(IEL) = LBUF%OFF(IEL)*HALF*(                    
     .                DEPSYZ(IEL)*(SIG0YZ(IEL) + SIGNYZ(IEL))+  
     .                DEPSZX(IEL)*(SIG0ZX(IEL) + SIGNZX(IEL)) ) 
        ENDDO
c--------------------------------------------------
        IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)
c--------------------------------------------------
c       Failure Models
c--------------------------------------------------
        IF ((ITASK==0).AND.(IMON_MAT==1))CALL STARTIME(121,1)
c--------------------------------------------------
        SOFT(1:NEL) = ONE
c
        IF (IFAILURE == 1) THEN                                                
          NFAILS = MAT_PARAM(IMAT)%NFAIL     
                           
          DO IR = 1,NFAILS                                                     
            IFAIL  = MAT_PARAM(IMAT)%FAIL(IR)%IRUPT                                   
            NPARF  = MAT_PARAM(IMAT)%FAIL(IR)%NUPARAM                                       
            NVARF  = MAT_PARAM(IMAT)%FAIL(IR)%NUVAR    
            NFUNCR = MAT_PARAM(IMAT)%FAIL(IR)%NFUNC
            DO I=1,NFUNCR                                                      
              IFUNCR(I) = MAT_PARAM(IMAT)%FAIL(IR)%IFUNC(I) 
            ENDDO                                                              
C
            FLOC  => ELBUF_STR%BUFLY(1)%FAIL(IPG,1,1)%FLOC(IR)                      
c
            IF (IFAIL == 20)THEN                                                   
C
              CALL FAIL_CONNECT(                                                          
     1        NEL       ,NPARF    ,NVARF   ,NFUNCR   ,IFUNCR    ,                    
     2        NPF       ,TF       ,TIME    ,TIMESTEP ,MAT_PARAM(IMAT)%FAIL(IR)%UPARAM,                
     3        FLOC%VAR  ,NGL      ,EPSZZ   ,EPSZX    ,EPSYZ     ,                           
     4        GBUF%EPSD ,GBUF%OFF ,LBUF%OFF,IPG      ,ISOLIDF   ,                       
     5        SIGNZZ    ,SIGNYZ   ,SIGNZX  ,DEIN     ,DEIT      ,              
     6        FLOC%DAMMX,FLOC%TDEL,AREAN   ,SOFT     )                                                   
c
            ELSEIF (IFAIL == 26)THEN                                               
C
              NDAMF =  FLOC%LF_DAM                                                  
              CALL FAIL_SNCONNECT(                                                          
     1        NEL       ,NPARF     ,NVARF     ,NFUNCR    ,IFUNCR    ,             
     2        NPF       ,TF        ,TIME      ,TIMESTEP  ,MAT_PARAM(IMAT)%FAIL(IR)%UPARAM,     
     3        FLOC%VAR  ,NGL       ,IPG       ,NPG       ,NDAMF     ,              
     4        LBUF%EPSD ,LBUF%PLA  ,GBUF%OFF  ,LBUF%OFF  ,ISOLIDF   ,         
     5        SIGNZZ    ,SIGNYZ    ,SIGNZX    ,SYM       ,AREAN     ,               
     6        LBUF%DMG  ,FLOC%DAM  ,FLOC%DAMMX,FLOC%TDEL )                    

            ENDIF
c
          ENDDO     ! IR = 1,NFAILS                                                               
        ENDIF       ! Failure 
c
c--------------------------------------------------
        ISOLID = MIN(ISOLID, ISOLIDF)
c--------------------------------------------------
        IF (ITASK==0 .and. IMON_MAT==1) CALL STOPTIME(121,1)                                                           
c--------------------------------------------------
c       global element constraints in local frame (for output)
c
        DO IEL=1,NEL
          SOFT(IEL) = SOFT(IEL)*GBUF%OFF(IEL)
          GBUF%SIG(JJ(3)+IEL) = GBUF%SIG(JJ(3)+IEL) + SIGNZZ(IEL)*FOURTH*SOFT(IEL)
          GBUF%SIG(JJ(5)+IEL) = GBUF%SIG(JJ(5)+IEL) + SIGNYZ(IEL)*FOURTH*SOFT(IEL)
          GBUF%SIG(JJ(6)+IEL) = GBUF%SIG(JJ(6)+IEL) + SIGNZX(IEL)*FOURTH*SOFT(IEL)
        ENDDO
c
c       internal forces
c
        CALL SFINT43(IPG  ,NPG   ,NEL   ,HH    ,AREA  ,SOFT  ,
     .       FX1   ,FX2   ,FX3   ,FX4   ,FX5   ,FX6   ,FX7   ,FX8   ,
     .       FY1   ,FY2   ,FY3   ,FY4   ,FY5   ,FY6   ,FY7   ,FY8   ,
     .       FZ1   ,FZ2   ,FZ3   ,FZ4   ,FZ5   ,FZ6   ,FZ7   ,FZ8   ,  
     .       SIGNZZ,SIGNYZ,SIGNZX)
c
c       energy
c
        DO IEL=1,NEL
          AREAT(IEL)  = AREAT(IEL)  + AREA(IEL)
          DELTAE(IEL) = DELTAE(IEL) + (DEIN(IEL) + DEIT(IEL))*AREA(IEL)*SOFT(IEL)
        ENDDO
c--------------------------------------------------
c       save current strain and stress              
        DO IEL=1,NEL                        
          LBUF%SIG(JJ(3)+IEL) = SIGNZZ(IEL)*LBUF%OFF(IEL)      
          LBUF%SIG(JJ(5)+IEL) = SIGNYZ(IEL)*LBUF%OFF(IEL)      
          LBUF%SIG(JJ(6)+IEL) = SIGNZX(IEL)*LBUF%OFF(IEL)      
        ENDDO                               
        IF (ELBUF_STR%BUFLY(1)%L_EPE > 0) THEN
          DO IEL=1,NEL
            LBUF%EPE(JJ(1)+IEL) = EPSZZ(IEL)   
            LBUF%EPE(JJ(2)+IEL) = EPSYZ(IEL)   
            LBUF%EPE(JJ(3)+IEL) = EPSZX(IEL)   
          ENDDO
        END IF
c
C----
      ENDDO   ! IPG=1,NPG                                                               
c--------------------------------------------------
c     FIN BOUCLE SUR LES POINTS DE GAUSS
c--------------------------------------------------
c     element suppression
c--------------------------------------------------
      CALL SCONNECT_OFF(ELBUF_STR ,GBUF%OFF  ,NEL    ,NPG    ,NGL   ,
     .                  ISOLID    ,TIME      )
c--------------------------------------------------
c
      DO IEL=1,NEL
        GBUF%EINT(IEL) = GBUF%EINT(IEL) + DELTAE(IEL) ! / AREAT(IEL)
      ENDDO
c
      IF (IOUTPRT/=0)
     .  CALL SBILAN43(NEL    ,IPARTS ,PARTSAV,GBUF%EINT,GBUF%RHO,
     .                AREAT  ,VGXA   ,VGYA   ,VGZA   ,VGA2   ,
     .                GBUF%FILL)
C     Add forces for moment equilibrium
      CALL SMOM43(NEL   ,
     .     FX1   ,FX2   ,FX3   ,FX4   ,FX5   ,FX6   ,FX7   ,FX8   ,
     .     FY1   ,FY2   ,FY3   ,FY4   ,FY5   ,FY6   ,FY7   ,FY8   ,
     .     FZ1   ,FZ2   ,FZ3   ,FZ4   ,FZ5   ,FZ6   ,FZ7   ,FZ8   ,
     .     R1X   ,R2X   ,R3X   ,R4X   ,R5X   ,R6X   ,R7X   ,R8X   ,
     .     R1Y   ,R2Y   ,R3Y   ,R4Y   ,R5Y   ,R6Y   ,R7Y   ,R8Y   ,
     .     R1Z   ,R2Z   ,R3Z   ,R4Z   ,R5Z   ,R6Z   ,R7Z   ,R8Z   ,
     .     RXX   ,RYY   ,RZZ   ,TTHICK ) 
C     Nodal forces : corotationnal --> global
      CALL SRROTA3(
     1   E1X,     E1Y,     E1Z,     E2X,
     2   E2Y,     E2Z,     E3X,     E3Y,
     3   E3Z,     FX1,     FX2,     FX3,
     4   FX4,     FX5,     FX6,     FX7,
     5   FX8,     FY1,     FY2,     FY3,
     6   FY4,     FY5,     FY6,     FY7,
     7   FY8,     FZ1,     FZ2,     FZ3,
     8   FZ4,     FZ5,     FZ6,     FZ7,
     9   FZ8,     NEL)
c-----------
      RETURN
      END
