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
Chd|  HM_READ_MAT62                 source/materials/mat/mat062/hm_read_mat62.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_MAT62(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           PARMAT   ,UNITAB   ,PM       ,MAT_ID   ,TITR     ,
     .           IMATVIS  ,LSUBMODEL,MATPARAM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MAT LAW62 (VISC_HYP)
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C     UNITAB          UNITS ARRAY
C     ID              MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER, INTENT(IN)    :: MAT_ID,MAXUPARAM
      my_real, DIMENSION(NPROPM) ,INTENT(INOUT) :: PM     
      CHARACTER*nchartitle ,INTENT(IN) :: TITR
      INTEGER, INTENT(OUT)   :: NUPARAM,NUVAR,NFUNC,IMATVIS
      my_real, DIMENSION(MAXUPARAM) ,INTENT(OUT)   :: UPARAM
      my_real, DIMENSION(100),INTENT(OUT) :: PARMAT
      TYPE(SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NORDER,NVISC,FLAG_VISC,IVISC,ILAW
      my_real :: RHO0,RHOR,GAMMAINF,NU,SUM,GS,P,VISCMAX
      my_real, DIMENSION(100) ::  MU,AL,GAMA,TAUX
      LOGICAL :: IS_AVAILABLE,IS_CRYPTED
C=======================================================================
      IS_CRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW    = 62
c
C--------------------------------------------------
C     check crypting
C--------------------------------------------------
c
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
c
C--------------------------------------------------
C     Read DATA
C--------------------------------------------------
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_NU'    ,NU        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('ORDER'     ,NORDER    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('Order2'    ,NVISC     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MU'        ,VISCMAX   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('Vflag'     ,FLAG_VISC ,IS_AVAILABLE,LSUBMODEL)
c
      IF (NORDER > 0) THEN
        DO J=1,NORDER    
          CALL HM_GET_FLOAT_ARRAY_INDEX('Mu_arr'   ,MU(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ENDDO
        DO J=1,NORDER     
          CALL HM_GET_FLOAT_ARRAY_INDEX('Alpha_arr',AL(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ENDDO
      ELSE
        CALL ANCMSG(MSGID=559,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF
c
      IF (NVISC > 0) THEN
        DO J=1,NVISC    
          CALL HM_GET_FLOAT_ARRAY_INDEX('Gamma_arr',GAMA(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ENDDO
        DO J=1,NVISC     
          CALL HM_GET_FLOAT_ARRAY_INDEX('Tau_arr'  ,TAUX(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ENDDO
      ENDIF
c--------------------------------------------------
c     CHECK VALUES
c--------------------------------------------------
      IF (RHOR == ZERO)  RHOR  = RHO0
c
      DO I=1,NORDER
        IF (AL(I) == ZERO) AL(I) = UN
      ENDDO  
c
      GAMMAINF = UN
      SUM      = ZERO
      IF (NVISC > 0) THEN 
        DO I=1,NVISC
          IF (TAUX(I) <= ZERO) TAUX(I) = EP20
          IF (GAMA(I) > UN .OR. GAMA(I) < ZERO) THEN
            CALL ANCMSG(MSGID=560,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  R1=GAMA(I))
          ENDIF  
          SUM = SUM + GAMA(I)
        ENDDO
        GAMMAINF = UN - SUM
      ENDIF
      IF (NU >= UNDEMI) NU=0.49999999
      IF (VISCMAX == ZERO) VISCMAX=EP20
c
      GS = ZERO
      DO I=1,NORDER 
        GS = GS + MU(I)
      ENDDO
      GS = GS*DEUX
      IF (GS < ZERO) THEN
        CALL ANCMSG(MSGID=846,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF

      P = GS*(UN+NU)/MAX(EM20,TROIS*(UN-DEUX*NU)) 
C          
       IF (NVISC > 0) THEN
         IVISC = 1
         IF (FLAG_VISC == 1) IVISC = 2
       ELSE
         IVISC = 0
       ENDIF  
       FLAG_VISC = MIN(FLAG_VISC, 2)
c---------------------
       UPARAM(1) = NU
       UPARAM(2) = NORDER
       UPARAM(3) = NVISC
       UPARAM(4) = GAMMAINF
       UPARAM(5) = P
       UPARAM(6) = VISCMAX
       DO I=1,NORDER
         UPARAM(6 + I  ) = MU(I)
         UPARAM(6 + NORDER +   I) = AL(I)
       ENDDO
       IF (NVISC > 0) THEN
         DO I= 1,NVISC 
           UPARAM(6 + NORDER*2 + I)         = GAMA(I)
           UPARAM(6 + NORDER*2 + NVISC + I) = TAUX(I)
        ENDDO 
       ENDIF 
       NUPARAM = 6 + 2*NORDER + 2*NVISC + 1
c      
       UPARAM(NUPARAM) = IVISC
c---------------------
      PARMAT(1) = P
      PARMAT(2) = GS*(UN + NU)
      PARMAT(3) = NU
      PARMAT(16) = 2  ! Formulation for solid elements time step computation.
      PARMAT(17) = GS/(P + DTHIRD*GS)
c-----------------
      PM(1)  = RHOR
      PM(89) = RHO0
c---------------------
      NFUNC = 0
      NUVAR = 6 + 6*NVISC  
c
c     NUVAR   : pour savegarder H et S (integration convolutive)        
c              + pressure viscosity NUVAR  = 9 + 3*NVISC*2 (a developpe en cas de besoin), 
c---------------------
      IF (NVISC > 0) THEN
        IMATVIS = 3
      ELSE
        IMATVIS = 1         
      ENDIF
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      IF (NU >= 0.49) THEN
        CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      ELSE
        CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      END IF
c-----------------
      WRITE(IOUT,1100) TRIM(TITR),MAT_ID,62
      WRITE(IOUT,1000)
      IF (IS_CRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0

        WRITE(IOUT,1300) NU,GS*UNDEMI,VISCMAX,NORDER
        WRITE(IOUT,1400) (MU(I),AL(I),I=1,NORDER)
        IF (IVISC > 0) THEN
          WRITE(IOUT,1500) NVISC,FLAG_VISC
          IF (NVISC /= ZERO) WRITE(IOUT,1600)(GAMA(I),TAUX(I),I=1,NVISC)
        ENDIF  
      ENDIF     
C-----------------
      RETURN
 999  CALL FREERR(3)
C-----------------
 1000 FORMAT
     & (5X,'MATERIAL MODEL : VISCO HYPERELASTIC',/,
     &  5X,'-----------------------------------',/)
 1100 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1200 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)  
 1300 FORMAT
     &(5X,'POISSON RATIO. . . . . . . . . .  . . .=',E12.4/
     &,5X,'INITIAL SHEAR MODULUS . . . . . . . . .=',E12.4/
     & 5X,'MAXIMUM VISCOSITY. . . . .. . . . . . .=',E12.4// 
     &,5X,'ORDER OF STRAIN ENERGY. . . . . . . . .=',I8)
 1400 FORMAT(
     & 5X,'MATERIAL PARAMETER (MU). . . . . . . . =',E12.4/
     & 5X,'MATERIAL PARAMETER (ALPHA) . . . . . . =',E12.4)    
 1500 FORMAT(//
     & 5X,'ORDER OF MAXWELL MODEL . . . . . . . . =',I8 / 
     & 5X,'VISCOUS STRESS FORMULATION . . . . . . =',I8 /
     &10X,' 0 : VISCOUS STRESS IS DEVIATORIC ' , /
     &10X,' 1 : VISCOUS STRESS IS SPHERICAL AND DEVIATORIC' )
 1600 FORMAT(
     & 5X,'STIFFNESS RATIO. . . . . . . . . . .. .=',E12.4/
     & 5X,'RELAXATION TIME . . . . . . . . . . .  =',E12.4)     
C-----------------
      RETURN
      END
