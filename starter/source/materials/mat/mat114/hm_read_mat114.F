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
Chd|  HM_READ_MAT114                source/materials/mat/mat114/hm_read_mat114.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT114(UPARAM   ,MAXUPARAM,NUPARAM  ,NFUNC    ,PARMAT   ,
     .                          UNITAB   ,PM       ,LSUBMODEL,ISRATE   ,MAT_ID   ,
     .                          TITR     ,IFUNC    ,MAXFUNC  ,MTAG     ,MATPARAM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD 
      USE ELBUFTAG_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e sXM
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN) :: MAT_ID,MAXUPARAM,MAXFUNC
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT) :: PM     
      CHARACTER*nchartitle ,INTENT(IN) :: TITR
      INTEGER, INTENT(INOUT) :: ISRATE,IFUNC(MAXFUNC)
      INTEGER, INTENT(INOUT)   :: NUPARAM,NFUNC
      my_real, DIMENSION(MAXUPARAM) ,INTENT(INOUT) :: UPARAM
      my_real, DIMENSION(100),INTENT(INOUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MLAW_TAG_), INTENT(INOUT) :: MTAG
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER J, IFUNC1, IFUNC2,IFUNC3, IECROU, IFUNC4, IG,
     .         IFAIL,ILENG,IFAIL2,FLGCHK,ILAW,
     .         I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14,
     .         IF1,IF2,IF3,IF4
C     REAL
      my_real
     .   A, B, D, E, F, XK, XC, DN, DX, FWV, LSCALE,
     .   PUN,VT0, VR0, CC(6), CN(6), XA(6), XB(6),ASRATE,GF3,
     .   CHECK(13,6),RHO0,A_UNIT,E_UNIT,D_UNIT,
     .   L_UNIT,GF_UNIT,F_UNIT,LMIN,YOUNG,SAREA,F_MAX,M_MAX,RFAC,IBEND,ITORS,
     .   K1,K2
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 114
      PUN = EM01
      FWV = ZERO
      ISRATE = 0
      ASRATE = ZERO
      CC(1:6) = ZERO
      FLGCHK = 0
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c------------------------------------------
c      
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,1000)MAT_ID
      ELSE
        WRITE(IOUT,2000)
      ENDIF
c
!-------------------------------------------------------
!                      Density
!-------------------------------------------------------
c-------------------------------------------------------------------------------
      WRITE(IOUT,1100) TRIM(TITR),MAT_ID,ILAW
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      WRITE(IOUT,1300) RHO0  
      PM(1)  = RHO0
      PM(89) = RHO0
c-------------------------------------------------------------------------------
!-------------------------------------------------------
!                       Flags
!-------------------------------------------------------
      IFAIL2 = 0
      IFAIL = 0
      ILENG = 1

      UPARAM(1)=IFAIL
      UPARAM(2)=ILENG
      UPARAM(3)=IFAIL2
      UPARAM(4) = 6
      NUPARAM = 4
C    
!-------------------------------------------------------
!                      Tension parameters
!------------------------------------------------------- 
      IFUNC2 = 0
      IFUNC4 = 0
C
      CALL HM_GET_FLOATV('LMIN'      ,LMIN     ,IS_AVAILABLE, LSUBMODEL, UNITAB)   
      CALL HM_GET_FLOATV('STIFF1'    ,XK       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('DAMP1'     ,XC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C
      CALL HM_GET_INTV  ('FUN_L'     ,IFUNC1   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_UL'    ,IFUNC3   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Fcoeft1'   ,A        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Xcoeft1'   ,LSCALE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C
!-------------------------------------------------------
!                      Compression - bending parameeters only
!-------------------------------------------------------
C
      CALL HM_GET_FLOATV('YOUNG'     ,YOUNG    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('SHEAR_AREA',SAREA    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FMAX'      ,F_MAX    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      CALL HM_GET_FLOATV('MMAX'      ,M_MAX    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Rfac'      ,RFAC     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Ibend'     ,IBEND    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Itors'     ,ITORS    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C
C----
      IF (IS_ENCRYPTED) THEN 
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,2001)'TENSION',XK,XC,IFUNC1,IFUNC3,LSCALE,A,LMIN
        WRITE(IOUT,2002)'BEAM PARAMETERS',YOUNG,F_MAX,M_MAX,IBEND,ITORS,RFAC,SAREA
      ENDIF
C
!-------------------------------------------------------
!                      Common parameters
!------------------------------------------------------
C
      UPARAM(119) = LMIN

C      IF (F_MAX == ZERO) F_MAX = INFINITY
C      IF (M_MAX == ZERO) M_MAX = INFINITY
      UPARAM(120) = F_MAX
      UPARAM(121) = M_MAX
C
C --- Computation of inertia / length only if E > 0
      IF (RFAC == ZERO) RFAC = ONE
      UPARAM(122) = IBEND 
      UPARAM(123) = ITORS
      UPARAM(124) = RFAC           
C
!-------------------------------------------------------
!                      Tension 
!------------------------------------------------------
C
      IF (IFUNC1 /= 0) THEN
        IECROU = 10
        IF (IFUNC3 == 0) IFUNC3 = IFUNC1
      ELSE
        IECROU = 11
      ENDIF
C
      IF (IFUNC1 == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=TITR)
      ENDIF
C
C--------
      DN =-INFINITY
      DX = INFINITY
      IF (LSCALE == ZERO) LSCALE = ONE
      IF (A == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Fcoeft1' ,A_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        A  = ONE * A_UNIT
      ENDIF
C
      IF (IFUNC1 == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C----
      I1 = NUPARAM
      I2 = I1 + 6 
      I3 = I2 + 6 
      I4 = I3 + 6 
      I5 = I4 + 6 
      I6 = I5 + 6 
      I7 = I6 + 6 
      I8 = I7 + 6 
      I9 = I8 + 6 
      I10 = I9 + 6 
      I11 = I10 + 6 
      I12 = I11 + 6 
      I13 = I12 + 6
C       
      UPARAM(I1 + 1)   = A
      UPARAM(I3 + 1)   = ONE
      UPARAM(I4 + 1)   = ONE
      UPARAM(I5 + 1)   = ONE                       
      UPARAM(I6 + 1)   = ONE
      UPARAM(I7 + 1)   = ONE / LSCALE
      UPARAM(I8 + 1)   = DN
      UPARAM(I9 + 1)   = DX
      UPARAM(I11 + 1)  = XK
      UPARAM(I12 + 1)  = XC
      UPARAM(I13 + 1)  = IECROU+PUN
C
      UPARAM(117) = YOUNG
      UPARAM(118) = ZERO
C
C     for interface stifness
      PM(191) = XK       
      !!
      IF1 = 0
      IF2 = 6
      IF3 = 12
      IF4 = 18
C      
      IFUNC(1) = IFUNC1
      IFUNC(IF2 + 1) = 0
      IFUNC(IF3 + 1) = IFUNC3
      IFUNC(IF4 + 1) = 0      
      NFUNC = 4
C
c-------------------------------------------------------------------------------      
!-------------------------------------------------------
!                      Compression bending shear torsion
!------------------------------------------------------- 

c-------------------------------------------------------------------------------  
!-----------------
      ! Shear XY
!-----------------
      IFUNC1 = 0
      IFUNC2 = 0
      IFUNC3 = 0
      IFUNC4 = 0
      DN =-INFINITY
      DX = INFINITY
      IECROU = 0
C
      XK = ZERO
      XC = ZERO
C
      IF (YOUNG > ZERO) THEN
        IECROU = 9
        XK = HALF*FIVE_OVER_6*YOUNG*SAREA
      ENDIF
C----
      UPARAM(I1 + 2)   = ONE
      UPARAM(I3 + 2)   = ONE
      UPARAM(I4 + 2)   = ONE           
      UPARAM(I5 + 2)   = ONE
      UPARAM(I6 + 2)   = ONE
      UPARAM(I7 + 2)   = ONE
      UPARAM(I8 + 2)   = DN
      UPARAM(I9 + 2)   = DX
      UPARAM(I11 + 2)  = XK
      UPARAM(I12 + 2)  = XC
      UPARAM(I13 + 2)  = IECROU+PUN
CC     for interface stifness
      PM(192) = XK       
      !!
      IFUNC(2) = IFUNC1
      IFUNC(IF2 + 2) = IFUNC2
      IFUNC(IF3 + 2) = IFUNC3
      IFUNC(IF4 + 2) = IFUNC4 
      NFUNC = NFUNC  + 4
C----
c-------------------------------------------------------------------------------  
!-----------------
      ! Shear XZ
!-----------------
      IFUNC1 = 0
      IFUNC2 = 0
      IFUNC3 = 0
      IFUNC4 = 0
      DN =-INFINITY
      DX = INFINITY
      IECROU = 0
C
      XK = ZERO
      XC = ZERO
C
      IF (YOUNG > ZERO) THEN
        IECROU = 9
        XK = HALF*FIVE_OVER_6*YOUNG*SAREA
      ENDIF
C----
      UPARAM(I1 + 3)   = ONE
      UPARAM(I3 + 3)   = ONE
      UPARAM(I4 + 3)   = ONE           
      UPARAM(I5 + 3)   = ONE
      UPARAM(I6 + 3)   = ONE
      UPARAM(I7 + 3)   = ONE
      UPARAM(I8 + 3)   = DN
      UPARAM(I9 + 3)   = DX
      UPARAM(I11 + 3)  = XK
      UPARAM(I12 + 3)  = XC
      UPARAM(I13 + 3)  = IECROU+PUN
CC     for interface stifness
      PM(193) = XK       
      !!
      IFUNC(3) = IFUNC1
      IFUNC(IF2 + 3) = IFUNC2
      IFUNC(IF3 + 3) = IFUNC3
      IFUNC(IF4 + 3) = IFUNC4 
      NFUNC = NFUNC  + 4
c
!-------------------------------------------------------
!                      Rotations
!-------------------------------------------------------
!-----------------
      ! Torsion X
!-----------------
      IFUNC1 = 0
      IFUNC2 = 0
      IFUNC3 = 0
      IFUNC4 = 0
      DN =-INFINITY
      DX = INFINITY
      IECROU = 0
C
      XK = ZERO
      XC = ZERO
C
      IF (YOUNG > ZERO) THEN
        IECROU = 9
        XK = HALF*YOUNG*ITORS
      ENDIF
C----
      UPARAM(I1 + 4)   = ONE
      UPARAM(I3 + 4)   = ONE
      UPARAM(I4 + 4)   = ONE           
      UPARAM(I5 + 4)   = ONE
      UPARAM(I6 + 4)   = ONE
      UPARAM(I7 + 4)   = ONE
      UPARAM(I8 + 4)   = DN
      UPARAM(I9 + 4)   = DX
      UPARAM(I11 + 4)  = XK
      UPARAM(I12 + 4)  = XC
      UPARAM(I13 + 4)  = IECROU+PUN
C
      IFUNC(3) = IFUNC1
      IFUNC(IF2 + 3) = IFUNC2
      IFUNC(IF3 + 3) = IFUNC3
      IFUNC(IF4 + 3) = IFUNC4 
      NFUNC = NFUNC  + 4
!-----------------
      ! Rotation Y
!----------------
      IFUNC1 = 0
      IFUNC2 = 0
      IFUNC3 = 0
      IFUNC4 = 0
      DN =-INFINITY
      DX = INFINITY
      IECROU = 0
C
      XK = ZERO
      XC = ZERO
C
      IF (YOUNG > ZERO) THEN
        IECROU = 9
        XK = YOUNG*IBEND
      ENDIF
C----
      UPARAM(I1 + 5)   = ONE
      UPARAM(I3 + 5)   = ONE
      UPARAM(I4 + 5)   = ONE           
      UPARAM(I5 + 5)   = ONE
      UPARAM(I6 + 5)   = ONE
      UPARAM(I7 + 5)   = ONE
      UPARAM(I8 + 5)   = DN
      UPARAM(I9 + 5)   = DX
      UPARAM(I11 + 5)  = XK
      UPARAM(I12 + 5)  = XC
      UPARAM(I13 + 5)  = IECROU+PUN
C
      IFUNC(5) = IFUNC1
      IFUNC(IF2 + 5) = IFUNC2
      IFUNC(IF3 + 5) = IFUNC3
      IFUNC(IF4 + 5) = IFUNC4 
      NFUNC = NFUNC  + 4
!-----------------
      ! Rotation Z
!-----------------
      IFUNC1 = 0
      IFUNC2 = 0
      IFUNC3 = 0
      IFUNC4 = 0
      DN =-INFINITY
      DX = INFINITY
      IECROU = 0
C
      XK = ZERO
      XC = ZERO
C
      IF (YOUNG > ZERO) THEN
        IECROU = 9
        XK = YOUNG*IBEND
      ENDIF
C----
      UPARAM(I1 + 6)   = ONE
      UPARAM(I3 + 6)   = ONE
      UPARAM(I4 + 6)   = ONE           
      UPARAM(I5 + 6)   = ONE
      UPARAM(I6 + 6)   = ONE
      UPARAM(I7 + 6)   = ONE
      UPARAM(I8 + 6)   = DN
      UPARAM(I9 + 6)   = DX
      UPARAM(I11 + 6)  = XK
      UPARAM(I12 + 6)  = XC
      UPARAM(I13 + 6)  = IECROU+PUN
C
      IFUNC(6) = IFUNC1
      IFUNC(IF2 + 6) = IFUNC2
      IFUNC(IF3 + 6) = IFUNC3
      IFUNC(IF4 + 6) = IFUNC4 
      NFUNC = NFUNC  + 4
C-----------------------------
C      
      NUPARAM = 128
C   
C------------------------
C------------------------
      MTAG%G_TOTDEPL = 3  ! DX (DY,DZ) - total deformation (translation)
      MTAG%G_TOTROT = 3   ! RX (RY,RZ) - total deformation (rotation)
      MTAG%G_DEP_IN_TENS = 3   ! DPX  (DPY,DPZ) - max displacement in tension
      MTAG%G_DEP_IN_COMP = 3   ! DPX2 (DPY2,DPZ2) - max displacement in compression
      MTAG%G_ROT_IN_TENS = 3   ! RPX (RPY,RPZ) - max rotation in tension
      MTAG%G_ROT_IN_COMP = 3   ! RPX2 (RPY2,RPY2) - max rotation in compression
      MTAG%G_POSX = 5
      MTAG%G_POSY = 5
      MTAG%G_POSZ = 5
      MTAG%G_POSXX = 5
      MTAG%G_POSYY = 5
      MTAG%G_POSZZ = 5
      MTAG%G_YIELD = 6
      MTAG%G_RUPTCRIT = 1
      MTAG%G_NUVAR = MAX(MTAG%G_NUVAR,NINT(UPARAM(4)))
      MTAG%G_MASS = 1
      PARMAT(4) = ZERO
      PARMAT(5) = ZERO
C
      MTAG%G_SLIPRING_ID = 1
      MTAG%G_SLIPRING_FRAM_ID = 1
      MTAG%G_SLIPRING_STRAND = 1  
      MTAG%G_RETRACTOR_ID = 1
      MTAG%G_RINGSLIP = 1
      MTAG%G_ADD_NODE = 2
      MTAG%G_UPDATE = 1
      MTAG%G_DFS = 1
      MTAG%G_FRAM_FACTOR = 1
C-- INTVAR used to comput force in 2nd strand when element is in slipring
      MTAG%G_INTVAR = 10
C------------------------
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPRING_MATERIAL")
C------------------------
C------------------------
      RETURN
c-----------
 1000   FORMAT(
     & 5X,'SPRING MATERIAL SET (SEATBELT TYPE)'/,
     & 5X,'-------------------------------'/,
     & 5X,'MATERIAL SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'CONFIDENTIAL DATA'//)
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL SET NUMBER. . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1300 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)      
 2000 FORMAT(
     & 5X,'SPRING MATERIAL SET (SEATBELT TYPE)'/,
     & 5X,'-------------------------------'/)
 2001 FORMAT(
     & 5X,A,/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'FORCE-ENGINEERING STRAIN CURVE. . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR UNLOADING ',/,
     & 5X,'FORCE-ENGINEERING STRAIN CURVE CURVE  .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'ORDINATE SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'MINIUM LENGTH FOR MASS COMPUTATION . . =',1PG20.13/)
 2002 FORMAT(
     & 5X,A,/,
     & 5X,'YOUNG MODULUS . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM FORCE FOR SHEAR/COMPRESSION . .=',1PG20.13/,
     & 5X,'MAXIMUM TORQUE FOR BENDING/TORSION  . .=',1PG20.13/,
     & 5X,'AREA MOMENT OF INERTIA FOR BENDING  . .=',1PG20.13/,
     & 5X,'AREA MOMENT OF INERTIA FOR TORSION  . .=',1PG20.13/,
     & 5X,'SCALING FACTOR FOR INERTIA. . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA  . . . . . . . . . . . . . .=',1PG20.13/)
c-----------
      RETURN
      END
