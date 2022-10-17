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
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
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
      INTEGER :: J,NORDER,NVISC,FLAG_VISC,IVISC,ILAW,ITAG
      my_real :: RHO0,RHOR,GAMMAINF,NUG,SUM,GS,P,VISCMAX,BULK
      my_real, DIMENSION(100) ::  MU,AL,GAMA,TAUX,NU,BETA
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
      IS_AVAILABLE = .FALSE.
      ILAW    = 62
c
C--------------------------------------------------
C     check crypting
C--------------------------------------------------
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
C--------------------------------------------------
C     Read DATA
C--------------------------------------------------
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c
      CALL HM_GET_FLOATV('MAT_NU'    ,NUG       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
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
      ITAG = 0
      IF (NORDER > 0) THEN
           DO J=1,NORDER                                                                            
               CALL HM_GET_FLOAT_ARRAY_INDEX('Nu_arr'   ,NU(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)  
               IF(NU(J) /= ZERO) ITAG = 1   
               IF (NU(J) >= HALF) NU(J) = ZEP499
           ENDDO                                                                                    
      ENDIF                                                                                               
c--------------------------------------------------
c     CHECK VALUES
c--------------------------------------------------
      IF (RHOR == ZERO)  RHOR  = RHO0
c
      DO J=1,NORDER
        IF (AL(J) == ZERO) AL(J) = ONE
      ENDDO  
c
      GAMMAINF = ONE
      SUM      = ZERO
      IF (NVISC > 0) THEN 
        DO J=1,NVISC
          IF (TAUX(J) <= ZERO) TAUX(J) = EP20
          IF (GAMA(J) > ONE .OR. GAMA(J) < ZERO) THEN
            CALL ANCMSG(MSGID=560,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  R1=GAMA(J))
          ENDIF  
          SUM = SUM + GAMA(J)
        ENDDO
        GAMMAINF = ONE - SUM
      ENDIF
      IF (NUG >= HALF) NUG = ZEP499
      IF (VISCMAX == ZERO) VISCMAX=EP20
c
      GS = ZERO
      DO J=1,NORDER 
        GS = GS + MU(J)
      ENDDO
      IF (GS < ZERO) THEN
        CALL ANCMSG(MSGID=846,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=MAT_ID,
     .              C1=TITR)
      ENDIF
      IF(ITAG == 1) THEN
         BULK = ZERO
         DO J=1,NORDER
           BETA(J) = NU(J)/(ONE - TWO*NU(J))
           BULK = BULK  + TWO*MU(J)*(THIRD + BETA(J))
         ENDDO
         NUG = HALF*(THREE*BULK - TWO*GS)/(THREE*BULK+ GS)
      ELSE
          BETA(1:NORDER) = NUG/(ONE - TWO*NUG)  
          BULK = TWO_THIRD*GS*(ONE + NUG)/MAX(EM20,(ONE - TWO*NUG))  
      ENDIF    
C          
       IF (NVISC > 0) THEN
         IVISC = 1
         IF (FLAG_VISC == 1) IVISC = 2
       ELSE
         IVISC = 0
       ENDIF  
       FLAG_VISC = MIN(FLAG_VISC, 2)
c---------------------
       UPARAM(1) = NUG
       UPARAM(2) = NORDER
       UPARAM(3) = NVISC
       UPARAM(4) = GAMMAINF
       UPARAM(5) = BULK
       UPARAM(6) = VISCMAX
       DO J=1,NORDER
         UPARAM(6 + J  ) = MU(J)
         UPARAM(6 + NORDER +   J) = AL(J)
       ENDDO
       IF (NVISC > 0) THEN
         DO J= 1,NVISC 
           UPARAM(6 + NORDER*2 + J)         = GAMA(J)
           UPARAM(6 + NORDER*2 + NVISC + J) = TAUX(J)
        ENDDO 
       ENDIF 
       NUPARAM = 6 + 2*NORDER + 2*NVISC + 1
       UPARAM(NUPARAM) = IVISC
       !! adding beta_i
        DO J=1,NORDER
         UPARAM(NUPARAM + J  ) = BETA(J)
       ENDDO
       NUPARAM = NUPARAM + NORDER
c---------------------
      GS = GS*TWO
      PARMAT(1) = BULK
      PARMAT(2) = GS*(ONE + NUG)
      PARMAT(3) = NUG
      PARMAT(16) = 2  ! Formulation for solid elements time step computation.
      PARMAT(17) = GS/(BULK + TWO_THIRD*GS)
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
      IF (NUG >= 0.49) THEN
        CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      ELSE
        CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      END IF
c-----------------
      WRITE(IOUT,1100) TRIM(TITR),MAT_ID,62
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1200) RHO0

        WRITE(IOUT,1300) NUG,GS*HALF,VISCMAX,NORDER
        WRITE(IOUT,1400) (MU(J),AL(J),NU(J),J=1,NORDER)
        IF (IVISC > 0) THEN
          WRITE(IOUT,1500) NVISC
          IF (NVISC /= ZERO) WRITE(IOUT,1600)(GAMA(J),TAUX(J),J=1,NVISC)
          WRITE(IOUT,1700) FLAG_VISC
        ENDIF  
      ENDIF     
C-----------------
      RETURN
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
     &(5X,'EQUIVALENT POISSON RATIO . . . .  . . .=',E12.4/
     &,5X,'INITIAL SHEAR MODULUS . . . . . . . . .=',E12.4/
     & 5X,'MAXIMUM VISCOSITY. . . . .. . . . . . .=',E12.4// 
     &,5X,'ORDER OF STRAIN ENERGY. . . . . . . . .=',I8)
 1400 FORMAT(
     & 7X,'MATERIAL PARAMETER (MU). . . . . . . . =',E12.4/
     & 7X,'MATERIAL PARAMETER (ALPHA) . . . . . . =',E12.4/
     & 7X,'MATERIAL PARAMETER (NU) . . . . . . .  =',E12.4/)    
 1500 FORMAT(//
     & 5X,'ORDER OF MAXWELL MODEL . . . . . . . . =',I8 )
 1600 FORMAT(
     & 7X,'STIFFNESS RATIO. . . . . . . . . . .. .=',E12.4/
     & 7X,'RELAXATION TIME . . . . . . . . . . .  =',E12.4)     
 1700 FORMAT(/
     & 5X,'VISCOUS STRESS FORMULATION . . . . . . =',I8 /
     &10X,' 0 : VISCOUS STRESS IS DEVIATORIC ' , /
     &10X,' 1 : VISCOUS STRESS IS SPHERICAL AND DEVIATORIC' )
C-----------------
      RETURN
      END
