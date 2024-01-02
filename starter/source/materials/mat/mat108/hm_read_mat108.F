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
Chd|  HM_READ_MAT108                source/materials/mat/mat108/hm_read_mat108.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        NL                            source/output/message/message.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT108(UPARAM   ,MAXUPARAM,NUPARAM  ,NFUNC    ,PARMAT   ,
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
     .         IFAIL,IFAIL2,FLGCHK,ILAW,IEQUIL,
     .         I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14,
     .         IF1,IF2,IF3,IF4
C     REAL
      my_real
     .   A, B, D, E, F, XK, XC, DN, DX, FWV, LSCALE,
     .   PUN, ASRATE,GF3 ,RHO0,A_UNIT,E_UNIT,D_UNIT,
     .   L_UNIT,GF_UNIT,F_UNIT
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW = 108
      PUN = EM01
      FWV = ZERO
      ISRATE = 0
      ASRATE = ZERO
c------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c------------------------------------------
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
      CALL HM_GET_INTV  ('Ifail'     ,IFAIL    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('Iequil'    ,IEQUIL   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('Ifail2'    ,IFAIL2   ,IS_AVAILABLE, LSUBMODEL)
c      
C----
C----
C
      IF (IFAIL2 /= 1 .AND. IFAIL2 /= 2 .AND. IFAIL2 /= 3) IFAIL2 = 0
      
      UPARAM(1)=IFAIL
      UPARAM(2)=IEQUIL
      UPARAM(3)=IFAIL2
C     UPARAM(4) used for NUVAR    
C
      NUPARAM = 4
c      
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,1000)MAT_ID
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1700)IFAIL,IFAIL2,IEQUIL
      ENDIF
c-------------------------------------------------------------------------------      
!-------------------------------------------------------
!                      Translations
!-------------------------------------------------------
!-----------------
      ! Traction X
!-----------------   
      CALL HM_GET_FLOATV('STIFF1'    ,XK       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('DAMP1'     ,XC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Acoeft1'   ,A        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Bcoeft1'   ,B        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Dcoeft1'   ,D        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('FUN_A1'    ,IFUNC1   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('HFLAG1'    ,IECROU   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_B1'    ,IFUNC2   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_C1'    ,IFUNC3   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_D1'    ,IFUNC4   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MIN_RUP1'  ,DN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP1'  ,DX       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Prop_X_F'  ,F        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Prop_X_E'  ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale1'    ,LSCALE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('ffac'      ,GF3      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      IF (IECROU == 4 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF

      IF (IECROU == 4 ) THEN
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 5. AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 6 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=1057,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 7 .AND. IFUNC1 == 0) THEN
         CALL ANCMSG(MSGID=1058,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ELSEIF (IECROU == 7 .AND. IFUNC3 == 0) THEN
         CALL ANCMSG(MSGID=1059,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR,
     .               I2=IECROU)
        IECROU = 2
      ENDIF
      IF (IFUNC1 == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=TITR)
      ENDIF
C--------
C----
      IF (DN == ZERO) DN=-INFINITY
      IF (DX == ZERO) DX= INFINITY
      IF (A == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Acoeft1' ,A_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        A  = ONE * A_UNIT
      ENDIF  
      IF (D == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Dcoeft1' ,D_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        D  = ONE * D_UNIT
      ENDIF
      IF (E == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Prop_X_E',E_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        E = ONE * E_UNIT
      ENDIF
      IF (F == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Prop_X_F',F_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        F = ONE * F_UNIT
      ENDIF
      IF (LSCALE == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('scale1',L_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        LSCALE = ONE*L_UNIT
      ENDIF
      IF (GF3 == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('ffac',GF_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        GF3 = ONE*GF_UNIT
      ENDIF
      IF (IFUNC1 == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
!
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
      I11 = I10 + 6 ! not used
      I12 = I11 + 6 
      I13 = I12 + 6
      ! I14 not used 
C       
      UPARAM(I1 + 1)   = A   ! 
      UPARAM(I2 + 1)   = B
      UPARAM(I3 + 1)   = D
      UPARAM(I4 + 1)   = E
      UPARAM(I5 + 1)   = GF3                       
      UPARAM(I6 + 1)   = ONE / F
      UPARAM(I7 + 1)   = ONE / LSCALE
      UPARAM(I8 + 1)   = DN
      UPARAM(I9 + 1)   = DX
      UPARAM(I10 + 1)  = ZERO ! not used
      UPARAM(I11 + 1)  = XK
      UPARAM(I12 + 1)  = XC
      UPARAM(I13 + 1)  = IECROU+PUN      
C  
C     for interface stifness
      PM(191) = XK       
      !!    
C--   If H=6 - 
      IF (IECROU == 6) THEN
        UPARAM(4) = 6
      ENDIF
C
      IF1 = 0
      IF2 = 6
      IF3 = 12
      IF4 = 18
!!      
      IFUNC(1) = IFUNC1
      IFUNC(IF2 + 1) = IFUNC2
      IFUNC(IF3 + 1) = IFUNC3
      IFUNC(IF4 + 1) = IFUNC4      
      NFUNC = 4
C----
      IF (IS_ENCRYPTED) THEN
        ! WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        IF (IECROU /= 5) THEN
            WRITE(IOUT,1810)'X',XK,XC,IFUNC1,LSCALE,IFUNC3,F,IECROU,
     .                       A,B,D,E,GF3,IFUNC2,IFUNC4,DN,DX
        ELSE
            WRITE(IOUT,1820)'X',XK,XC,IFUNC1,LSCALE,IFUNC3,F,IECROU,
     .                       A,B,D,E,GF3,IFUNC2,IFUNC4,DN,DX       
        ENDIF ! IF (IECROU /= 5)
      ENDIF ! IF (IS_ENCRYPTED)
c-------------------------------------------------------------------------------  
!-----------------
      ! Shear XY
!-----------------
C
      CALL HM_GET_FLOATV('STIFF2'    ,XK       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('DAMP2'     ,XC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Acoeft2'   ,A        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Bcoeft2'   ,B        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Dcoeft2'   ,D        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('FUN_A2'    ,IFUNC1   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('HFLAG2'    ,IECROU   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_B2'    ,IFUNC2   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_C2'    ,IFUNC3   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_D2'    ,IFUNC4   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MIN_RUP2'  ,DN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP2'  ,DX       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Prop_Y_F'  ,F        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Prop_Y_E'  ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale2'    ,LSCALE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('df'        ,GF3      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C
      IF (IECROU == 4 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
!!            IF (IECROU == 4 .AND. GEO(2) == ZERO)THEN 
      IF (IECROU == 4 )THEN
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 5 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 6 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=1057,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 7 .AND. IFUNC1 == 0) THEN
         CALL ANCMSG(MSGID=1058,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR) 
      ELSEIF (IECROU == 7 .AND. IFUNC3 == 0) THEN
         CALL ANCMSG(MSGID=1059,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR,
     .               I2=IECROU)
        IECROU = 2
      ENDIF
      IF (IFUNC1 == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=TITR)
      ENDIF
C----
      IF (DN == ZERO) DN =-INFINITY
      IF (DX == ZERO) DX = INFINITY
      IF (A == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Acoeft2' ,A_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        A  = ONE * A_UNIT
      ENDIF
      IF (D == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Dcoeft2' ,D_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        D  = ONE * D_UNIT
      ENDIF
      IF (E == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Prop_Y_E',E_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        E = ONE * E_UNIT
      ENDIF
      IF (F == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Prop_Y_F',F_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        F = ONE * F_UNIT
      ENDIF
      IF (LSCALE == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('scale2',L_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        LSCALE = ONE*L_UNIT
      ENDIF
      IF (GF3 == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('df',GF_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        GF3 = ONE*GF_UNIT
      ENDIF
      IF (IFUNC1 == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C----
      UPARAM(I1 + 2)   = A   ! 
      UPARAM(I2 + 2)   = B
      UPARAM(I3 + 2)   = D
      UPARAM(I4 + 2)   = E
      UPARAM(I5 + 2)   = GF3           
      UPARAM(I6 + 2)   = ONE / F
      UPARAM(I7 + 2)   = ONE / LSCALE
      UPARAM(I8 + 2)   = DN
      UPARAM(I9 + 2)   = DX
      UPARAM(I10 + 2)  = ZERO ! not used
      UPARAM(I11 + 2)  = XK
      UPARAM(I12 + 2)  = XC
      UPARAM(I13 + 2)  = IECROU+PUN
C     for interface stifness
      PM(192) = XK       
      !!
C--   If H=6 - addditional internal variables must be stored in UVAR - 
      IF (IECROU == 6) THEN
        UPARAM(4) = 6
      ENDIF
C
      IFUNC(2) = IFUNC1
      IFUNC(IF2 + 2) = IFUNC2
      IFUNC(IF3 + 2) = IFUNC3
      IFUNC(IF4 + 2) = IFUNC4 
      NFUNC = NFUNC  + 4
C----
      IF (IS_ENCRYPTED) THEN
        ! WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        IF (IECROU /= 5) THEN
            WRITE(IOUT,1810)'Y ',XK,XC,IFUNC1,LSCALE,IFUNC3,F,IECROU,
     .                       A,B,D,E,GF3,IFUNC2,IFUNC4,DN,DX
        ELSE
            WRITE(IOUT,1820)'Y SHEAR',XK,XC,IFUNC1,LSCALE,IFUNC3,F,IECROU,
     .                       A,B,D,E,GF3,IFUNC2,IFUNC4,DN,DX
        ENDIF ! IF (IECROU /= 5)
      ENDIF ! IF (IS_ENCRYPTED)
c-------------------------------------------------------------------------------  
!-----------------
      ! Traction Z
!-----------------
c
      CALL HM_GET_FLOATV('STIFF3'    ,XK       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('DAMP3'     ,XC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Acoeft3'   ,A        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Bcoeft3'   ,B        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Dcoeft3'   ,D        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('FUN_A3'    ,IFUNC1   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('HFLAG3'    ,IECROU   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_B3'    ,IFUNC2   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_C3'    ,IFUNC3   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_D3'    ,IFUNC4   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MIN_RUP3'  ,DN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP3'  ,DX       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Prop_Z_F'  ,F        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Prop_Z_E'  ,E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale3'    ,LSCALE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('D2'        ,GF3      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C----
      IF (IECROU == 4 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
!!      IF (IECROU == 4 .AND. GEO(2) == ZERO) THEN 
      IF (IECROU == 4 ) THEN
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 5 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 6 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=1057,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 7 .AND. IFUNC1 == 0) THEN
         CALL ANCMSG(MSGID=1058,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ELSEIF (IECROU == 7 .AND. IFUNC3 == 0) THEN
         CALL ANCMSG(MSGID=1059,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR,
     .               I2=IECROU)
        IECROU = 2
      ENDIF
      IF (IFUNC1 == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=TITR)
      ENDIF
C----
      IF (DN == ZERO) DN =-INFINITY
      IF (DX == ZERO) DX = INFINITY
      IF (A == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Acoeft3' ,A_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        A  = ONE * A_UNIT
      ENDIF
      IF (D == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Dcoeft3' ,D_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        D  = ONE * D_UNIT
      ENDIF
      IF (E == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Prop_Z_E',E_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        E = ONE * E_UNIT
      ENDIF
      IF (F == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Prop_Z_F',F_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        F = ONE * F_UNIT
      ENDIF
      IF (LSCALE == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('scale3',L_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        LSCALE = ONE*L_UNIT
      ENDIF
      IF (GF3 == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('D2'  ,GF_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        GF3 = ONE*GF_UNIT
      ENDIF
      IF (IFUNC1 == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C----
      UPARAM(I1 + 3)   = A   ! 
      UPARAM(I2 + 3)   = B
      UPARAM(I3 + 3)   = D
      UPARAM(I4 + 3)   = E
      UPARAM(I5 + 3)   = GF3                       
      UPARAM(I6 + 3)   = ONE / F
      UPARAM(I7 + 3)   = ONE / LSCALE
      UPARAM(I8 + 3)   = DN
      UPARAM(I9 + 3)   = DX
      UPARAM(I10 + 3)  = ZERO ! not used
      UPARAM(I11 + 3)  = XK
      UPARAM(I12 + 3)  = XC
      UPARAM(I13 + 3)  = IECROU+PUN   
C     for interface stifness
      PM(193) = XK       
      !!
C--   If H=6 - addditional internal variables must be stored in UVAR 
      IF (IECROU == 6) THEN
       UPARAM(4) = 6
      ENDIF
C
      IFUNC(3) = IFUNC1
      IFUNC(IF2 + 3) = IFUNC2
      IFUNC(IF3 + 3) = IFUNC3
      IFUNC(IF4 + 3) = IFUNC4 
      NFUNC = NFUNC + 4
C----
      IF (IS_ENCRYPTED) THEN
!        ! WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        IF (IECROU /= 5) THEN
            WRITE(IOUT,1810)'Z ',XK,XC,IFUNC1,LSCALE,IFUNC3,F,IECROU,
     .                       A,B,D,E,GF3,IFUNC2,IFUNC4,DN,DX
        ELSE
            WRITE(IOUT,1820)'Z ',XK,XC,IFUNC1,LSCALE,IFUNC3,F,IECROU,
     .                       A,B,D,E,GF3,IFUNC2,IFUNC4,DN,DX
        ENDIF ! IF (IECROU /= 5)
      ENDIF ! IF (IS_ENCRYPTED)
c
!-------------------------------------------------------
!                      Rotations
!-------------------------------------------------------
!-----------------
      ! Torsion X
!-----------------
      CALL HM_GET_FLOATV('STIFF4'    ,XK       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('DAMP4'     ,XC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Acoeft4'   ,A        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Bcoeft4'   ,B        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Dcoeft4'   ,D        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('FUN_A4'    ,IFUNC1   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('HFLAG4'    ,IECROU   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_B4'    ,IFUNC2   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_C4'    ,IFUNC3   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_D4'    ,IFUNC4   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MIN_RUP4'  ,DN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP4'  ,DX       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Prop_Tor_F',F        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Prop_Tor_E',E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale4'    ,LSCALE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Y0'        ,GF3      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C----
      IF (IECROU == 4 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
!!      IF (IECROU == 4. AND. GEO(2) == ZERO) THEN
      IF (IECROU == 4) THEN
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 5 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 6 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=1057,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 7 .AND. IFUNC1 == 0) THEN
         CALL ANCMSG(MSGID=1058,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR) 
      ELSEIF (IECROU == 7 .AND. IFUNC3 == 0) THEN
         CALL ANCMSG(MSGID=1059,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR,
     .               I2=IECROU)
        IECROU = 2
      ENDIF
      IF (IFUNC1 == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=TITR)
      ENDIF
C----
      IF (DN == ZERO) DN =-INFINITY
      IF (DX == ZERO) DX = INFINITY
      IF (A == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Acoeft4' ,A_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        A  = ONE * A_UNIT
      ENDIF
      IF (D == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Dcoeft4' ,D_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        D  = ONE * D_UNIT
      ENDIF
      IF (E == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Prop_Tor_E',E_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        E = ONE * E_UNIT
      ENDIF
      IF (F == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Prop_Tor_F',F_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        F = ONE * F_UNIT
      ENDIF
      IF (LSCALE == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('scale4',L_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        LSCALE = ONE*L_UNIT
      ENDIF
      IF (GF3 == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Y0',GF_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        GF3 = ONE*GF_UNIT
      ENDIF
      IF (IFUNC1 == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C----
C      
      UPARAM(I1 + 4)   = A   ! 
      UPARAM(I2 + 4)   = B
      UPARAM(I3 + 4)   = D
      UPARAM(I4 + 4)   = E
      UPARAM(I5 + 4)   = GF3           
      UPARAM(I6 + 4)   = ONE / F
      UPARAM(I7 + 4)   = ONE / LSCALE
      UPARAM(I8 + 4)   = DN
      UPARAM(I9 + 4)   = DX
      UPARAM(I10 + 4)  = ZERO ! not used
      UPARAM(I11 + 4)  = XK
      UPARAM(I12 + 4)  = XC
      UPARAM(I13 + 4)  = IECROU+PUN
C--   If H=6 - addditional internal variables must be stored in UVAR - 
      IF (IECROU == 6) THEN
        UPARAM(4) = 6
      ENDIF
C
      IFUNC(4) = IFUNC1
      IFUNC(IF2 + 4) = IFUNC2
      IFUNC(IF3 + 4) = IFUNC3
      IFUNC(IF4 + 4) = IFUNC4 
      NFUNC = NFUNC + 4
C----
      IF (IS_ENCRYPTED) THEN
        ! WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        IF (IECROU /= 5) THEN
            WRITE(IOUT,1830)'X ROTATION',XK,XC,IFUNC1,LSCALE,IFUNC3,F,IECROU,
     .                       A,B,D,E,GF3,IFUNC2,IFUNC4,DN,DX
        ELSE
            WRITE(IOUT,1840)'X ROTATION',XK,XC,IFUNC1,LSCALE,IFUNC3,F,IECROU,
     .                       A,B,D,E,GF3,IFUNC2,IFUNC4,DN,DX
        ENDIF ! IF (IECROU /= 5)
      ENDIF ! IF (IS_ENCRYPTED)
C
!-----------------
      ! Rotation Y
!----------------
      CALL HM_GET_FLOATV('STIFF5'     ,XK       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('DAMP5'      ,XC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Acoeft5'    ,A        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Bcoeft5'    ,B        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Dcoeft5'    ,D        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('FUN_A5'     ,IFUNC1   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('HFLAG5'     ,IECROU   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_B5'     ,IFUNC2   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_C5'     ,IFUNC3   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_D5'     ,IFUNC4   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MIN_RUP5'   ,DN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP5'   ,DX       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Prop_FlxY_F',F        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Prop_FlxY_E',E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale5'     ,LSCALE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Z0'         ,GF3      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C----
      IF (IECROU == 4 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
!!      IF (IECROU == 4 .AND. GEO(2) == ZERO) THEN
      IF (IECROU == 4 ) THEN      
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 5 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 6 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=1057,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 7 .AND. IFUNC1 == 0) THEN
         CALL ANCMSG(MSGID=1058,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR) 
      ELSEIF (IECROU == 7 .AND. IFUNC3 == 0) THEN
         CALL ANCMSG(MSGID=1059,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR,
     .               I2=IECROU)
        IECROU = 2
      ENDIF
      IF (IFUNC1 == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=TITR)
      ENDIF
C----
      IF (DN == ZERO) DN =-INFINITY
      IF (DX == ZERO) DX = INFINITY
      IF (A == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Acoeft5' ,A_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        A  = ONE * A_UNIT
      ENDIF
      IF (D == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Dcoeft5' ,D_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        D  = ONE * D_UNIT
      ENDIF
      IF (E == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Prop_FlxY_E',E_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        E = ONE * E_UNIT
      ENDIF
      IF (F == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Prop_FlxY_F',F_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        F = ONE * F_UNIT
      ENDIF
      IF (LSCALE == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('scale5',L_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        LSCALE = ONE*L_UNIT
      ENDIF
      IF (GF3 == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Z0',GF_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        GF3 = ONE*GF_UNIT
      ENDIF
      IF (IFUNC1 == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C----
      UPARAM(I1 + 5)   = A   ! 
      UPARAM(I2 + 5)   = B
      UPARAM(I3 + 5)   = D
      UPARAM(I4 + 5)   = E
      UPARAM(I5 + 5)   = GF3           
      UPARAM(I6 + 5)   = ONE / F
      UPARAM(I7 + 5)   = ONE / LSCALE
      UPARAM(I8 + 5)   = DN
      UPARAM(I9 + 5)   = DX
      UPARAM(I10 + 5)  = ZERO ! not used
      UPARAM(I11 + 5)  = XK
      UPARAM(I12 + 5)  = XC
      UPARAM(I13 + 5)  = IECROU+PUN  
C--   If H=6 - addditional internal variables must be stored in UVAR - 
      IF (IECROU == 6) THEN
         UPARAM(4) = 6
      ENDIF
C
      IFUNC(5) = IFUNC1
      IFUNC(IF2 + 5) = IFUNC2
      IFUNC(IF3 + 5) = IFUNC3
      IFUNC(IF4 + 5) = IFUNC4 
      NFUNC = NFUNC + 4
C----
      IF (IS_ENCRYPTED) THEN
        ! WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        IF (IECROU /= 5) THEN
            WRITE(IOUT,1830)'Y ROTATION ',XK,XC,IFUNC1,LSCALE,IFUNC3,F,
     .                       IECROU,A,B,D,E,GF3,IFUNC2,IFUNC4,DN,DX
        ELSE
            WRITE(IOUT,1840)'Y ROTATION',XK,XC,IFUNC1,LSCALE,IFUNC3,F,
     .                       IECROU,A,B,D,E,GF3,IFUNC2,IFUNC4,DN,DX
        ENDIF ! IF (IECROU /= 5)
      ENDIF ! IF (IS_ENCRYPTED)
C
!-----------------
      ! Rotation Z
!-----------------
      CALL HM_GET_FLOATV('STIFF6'     ,XK       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('DAMP6'      ,XC       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Acoeft6'    ,A        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Bcoeft6'    ,B        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Dcoeft6'    ,D        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('FUN_A6'     ,IFUNC1   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('HFLAG6'     ,IECROU   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_B6'     ,IFUNC2   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_C6'     ,IFUNC3   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_D6'     ,IFUNC4   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MIN_RUP6'   ,DN       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP6'   ,DX       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Prop_FlxZ_F',F        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Prop_FlxZ_E',E        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('scale6'     ,LSCALE   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Hscale6'    ,GF3      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C----
      IF (IECROU == 4 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
!!      IF (IECROU == 4 .AND. GEO(2) == ZERO) THEN
      IF (IECROU == 4 ) THEN
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 5 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 6 .AND. (IFUNC1 == 0 .OR. IFUNC3 == 0)) THEN
         CALL ANCMSG(MSGID=1057,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF
      IF (IECROU == 7 .AND. IFUNC1 == 0) THEN
         CALL ANCMSG(MSGID=1058,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR) 
      ELSEIF (IECROU == 7 .AND. IFUNC3 == 0) THEN
         CALL ANCMSG(MSGID=1059,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR,
     .               I2=IECROU)
        IECROU = 2
      ENDIF
      IF (IFUNC1 == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=TITR)
      ENDIF
C----
      IF (DN == ZERO) DN =-INFINITY
      IF (DX == ZERO) DX = INFINITY
      IF (A == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Acoeft6' ,A_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        A  = ONE * A_UNIT
      ENDIF
      IF (D == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Dcoeft6' ,D_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
        D  = ONE * D_UNIT
      ENDIF
      IF (E == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Prop_FlxZ_E',E_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        E = ONE * E_UNIT
      ENDIF
      IF (F == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Prop_FlxZ_F',F_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        F = ONE * F_UNIT
      ENDIF
      IF (LSCALE == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('scale6',L_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        LSCALE = ONE*L_UNIT
      ENDIF
      IF (GF3 == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('Hscale6',GF_UNIT    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        GF3 = ONE*GF_UNIT
      ENDIF
      IF (IFUNC1 == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C---     
      UPARAM(I1 + 6)   = A   ! 
      UPARAM(I2 + 6)   = B
      UPARAM(I3 + 6)   = D
      UPARAM(I4 + 6)   = E
      UPARAM(I5 + 6)   = GF3           
      UPARAM(I6 + 6)   = ONE / F
      UPARAM(I7 + 6)   = ONE / LSCALE
      UPARAM(I8 + 6)   = DN
      UPARAM(I9 + 6)   = DX
      UPARAM(I10 + 6)  = ZERO ! not used
      UPARAM(I11 + 6)  = XK
      UPARAM(I12 + 6)  = XC
      UPARAM(I13 + 6)  = IECROU+PUN  
!!      
      NUPARAM = NUPARAM + 6*14
C--   If H=6 - addditional internal variables must be stored in UVAR - 
      IF (IECROU == 6) THEN
        UPARAM(4) = 6
      ENDIF
C
      IFUNC(6) = IFUNC1
      IFUNC(IF2 + 6) = IFUNC2
      IFUNC(IF3 + 6) = IFUNC3
      IFUNC(IF4 + 6) = IFUNC4 
      NFUNC = NFUNC + 4
C----
      IF (IS_ENCRYPTED) THEN
        ! WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        IF (IECROU /= 5) THEN
            WRITE(IOUT,1830)'Z ROTATION',XK,XC,IFUNC1,LSCALE,IFUNC3,F,
     .                       IECROU,A,B,D,E,GF3,IFUNC2,IFUNC4,DN,DX
        ELSE
            WRITE(IOUT,1840)'Z ROTATION',XK,XC,IFUNC1,LSCALE,IFUNC3,F,
     .                       IECROU,A,B,D,E,GF3,IFUNC2,IFUNC4,DN,DX
        ENDIF ! IF (IECROU /= 5)
      ENDIF ! IF (IS_ENCRYPTED)
C----
C-----------------------------
!-------------------------------------------------------
c---    filtering
!-------------------------------------------------------
      CALL HM_GET_FLOATV('Asrate'      ,ASRATE    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('ISRATE'      ,ISRATE    ,IS_AVAILABLE, LSUBMODEL)
c           
C----
      IF (ASRATE == ZERO) ASRATE = INFINITY
C----
!
      IF (IS_ENCRYPTED) THEN
        ! WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1900) ISRATE,ASRATE
      ENDIF
!
C----! NUPARAM = 88 = 4 + 14*6
!      
      UPARAM(NUPARAM + 1) = ISRATE
      UPARAM(NUPARAM + 2) = ASRATE
C      
      NUPARAM = NUPARAM  + 2
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
      MTAG%G_SKEW_ID = 1  
C 
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPRING_MATERIAL")
C
      PARMAT(4) = ISRATE
      PARMAT(5) = ASRATE
C------------------------
      RETURN
c-----------
 1000   FORMAT(
     &    5X,'SPRING MATERIAL SET'/,
     &    5X,'-------------------'/,
     &    5X,'MATERIAL SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
 1300 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)  
 1700 FORMAT(
     & 5X,'FAILURE FLAG (0:UNCOUPLED 1:COUPLED). .=',I10/,
     & 5X,'FAILURE2 (0:DISPL,1:FORCE,2:ENERGY) . .=',I10/,
     & 5X,'EQUILIBRIUM FLAG. . . . . . . . . . . .=',I10/,
     & 5X,'     0: no equilibrium  1:force and moment equilibrium '  ,/)
 1810 FORMAT(
     & 5X,A,' TRANSLATION'/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'FORCE-DISPLACEMENT CURVE. . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR UNLOADING ',/,
     & 5X,'FORCE-DISPLACEMENT CURVE (H=4,5,7). . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/)
 1820 FORMAT(
     & 5X,A,/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'FORCE-DISPLACEMENT CURVE. . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'PERMANENT DISPL./MAX. DISPL. CURVE(H=5)=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/)
 1830 FORMAT(
     & 5X,A,/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'MOMENT-ROTATION CURVE . . . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR UNLOADING ',/,
     & 5X,'MOMENT-ROTATION CURVE (H=4,5,7). . . . =',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE ROTATION . . . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE ROTATION . . . . . . .=',1PG20.13/)
 1840 FORMAT(
     & 5X,A,/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'MOMENT/ROTATION CURVE . . . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'PERMANENT ROT./MAX. ROT. CURVE (H=5). .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE ROTATION . . . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE ROTATION . . . . . . .=',1PG20.13/)
 1900 FORMAT(
     & 5X,'SMOOTH STRAIN RATE OPTION . . .. . . . =',I10/,
     & 5X,'STRAIN RATE CUTTING FREQUENCY .. . . . =',1PG20.13/)
c-----------
c-----------
      RETURN
      END
