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
Chd|  HM_READ_FAIL_GURSON           source/materials/fail/gurson/hm_read_fail_gurson.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GET_LEMAX                     ../common_source/modules/nlocal_reg_mod.F
Chd|        GET_LENGTH                    ../common_source/modules/nlocal_reg_mod.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_GURSON(
     .           FAIL    ,MAT_ID   ,FAIL_ID  ,IRUPT    ,NLOC_MOD   ,
     .           TITR    ,LSUBMODEL,UNITAB   )
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Read Gurson damage model parameters (irup = 35)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER            ,INTENT(IN) :: FAIL_ID         ! failure model ID
      INTEGER            ,INTENT(IN) :: MAT_ID          ! material law ID
      INTEGER            ,INTENT(IN) :: IRUPT           ! failure model type number
      CHARACTER          ,INTENT(IN) :: TITR*nchartitle ! material model title
      TYPE(UNIT_TYPE_)   ,INTENT(IN) :: UNITAB          ! table of input units
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)    ! submodel table
      INTEGER            ,INTENT(INOUT) :: NLOC_MOD     ! non-local variable regularization flag
      TYPE(FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL         ! failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ILOC
      my_real :: Q1,Q2,Q3,EPN,AS,KW,FC,FR,F0,RLEN,HKHI,LE_MAX
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C-----------------------------------------------
C   S o u r c e   L i n e s
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      ! Card 1 
      CALL HM_GET_FLOATV ('FAIL_q1'        ,Q1      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('FAIL_q2'        ,Q2      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('FAIL_Iloc'      ,ILOC    ,IS_AVAILABLE,LSUBMODEL) 
      ! Card 2
      CALL HM_GET_FLOATV ('FAIL_eps_strain',EPN     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('FAIL_As'        ,AS      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('FAIL_Kw'        ,KW      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ! Card 3
      CALL HM_GET_FLOATV ('FAIL_Fc'        ,FC      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('FAIL_Fr'        ,FR      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('FAIL_F0'        ,F0      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ! Card 4
      CALL HM_GET_FLOATV ('FAIL_RLen'      ,RLEN    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('FAIL_Hchi'      ,HKHI    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('FAIL_Le_max'    ,LE_MAX  ,IS_AVAILABLE,LSUBMODEL,UNITAB)      
c-----------------------------------------------------------------------
      ! Checking Gurson yield criterion parameters
      IF (Q1 == ZERO) Q1 = THREE_HALF
      IF (Q2 == ZERO) Q2 = ONE
      Q3 = Q1**2
      ! Checking value of nucleation function
      IF (AS == ZERO) THEN
        ! No nucleation trigger
        EPN = INFINITY
      ENDIF
      ! Checking value of coalescence function
      IF (FC == ZERO) FC = INFINITY
      IF (FR == ZERO) FR = TWO*INFINITY
      IF (FC >= FR)  THEN
        ! Error message
        CALL ANCMSG(MSGID=1747,MSGTYPE=MSGERROR,
     .    ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)
      ENDIF
      IF (F0 >= FC) THEN
        ! Warning message
        CALL ANCMSG(MSGID=1748,MSGTYPE=MSGWARNING,
     .      ANMODE=ANINFO_BLIND_1,I1=MAT_ID,C1=TITR)
      ENDIF
c
      ! Choice of the non-local regularization method
c     ILOC = 1 => local damage : no plastic strain regularization (default)
c     ILOC = 2 => Forest (micromorphic) method of nodal variable regularization
c     ILOC = 3 => Peerlings method of nodal variable regularization
      IF (ILOC == 0) ILOC = 1
      NLOC_MOD = ILOC-1
c
      ! Automatic Rlen computation
      IF (LE_MAX > ZERO) THEN 
        CALL GET_LENGTH(RLEN,LE_MAX)
      ! Or printout the maximum element length target for convergence
      ELSE
        CALL GET_LEMAX(LE_MAX,RLEN)
      ENDIF
c
      ! Micromorphic penalty parameter (homogeneous to a stress [MPa])
      IF (ILOC /= 2) THEN 
        HKHI = ZERO    ! only used for micromorphic
      ENDIF
c-----------------------------------------------------------------------
      FAIL%KEYWORD = 'GURSON' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 12
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 0
      FAIL%NFUNC   = 0
      FAIL%NTABLE  = 0
      FAIL%NMOD    = 0
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
c
      FAIL%UPARAM(1) = NLOC_MOD  ! non-local variable regularization flag
      FAIL%UPARAM(2) = Q1        ! Gurson yield criterion 1st parameter
      FAIL%UPARAM(3) = Q2        ! Gurson yield criterion 2nd parameter
      FAIL%UPARAM(4) = Q3        ! Gurson yield criterion 3rd parameter
      FAIL%UPARAM(5) = EPN       ! Trigger plastic strain for nucleation
      FAIL%UPARAM(6) = AS        ! Nucleation rate
      FAIL%UPARAM(7) = KW        ! Nahshon-Hutchinson shear parameter
      FAIL%UPARAM(8) = FR        ! Failure void volume fraction
      FAIL%UPARAM(9) = FC        ! Critical void volume fraction
      FAIL%UPARAM(10)= F0        ! Initial void volume fraction
      FAIL%UPARAM(11)= RLEN      ! Non-local internal length
      FAIL%UPARAM(12)= HKHI      ! Micromorphic penalty parameter
c-----------------------------------------------------------------------
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT, 1100) Q1,Q2,EPN,AS,KW,F0,FC,FR,ILOC
        IF (ILOC > 1)  WRITE(IOUT, 1200) RLEN,LE_MAX
        IF (ILOC == 2) WRITE(IOUT, 1300) HKHI
      ENDIF
C-----------
      RETURN
C-----------
 1100 FORMAT(
     & 5X,40H    GURSON DUCTILE DAMAGE MODEL          /,
     & 5X,40H    ------------------------------------ /,
     & 5X,'Q1 GURSON COEFFICIENT. . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'Q2 GURSON COEFFICIENT. . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'NUCLEATION PLASTIC STRAIN. . . . . . . . . . . . . =',1PG20.13/
     & 5X,'AS NUCLEATION SLOPE. . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'SHEAR DAMAGE COEFFICIENT . . . . . . . . . . . . . =',1PG20.13/
C
     & 5X,'INITIAL VOID VOLUME FRACTION . . . . . . . . . . . =',1PG20.13/
     & 5X,'CRITICAL VOID VOLUME FRACTION. . . . . . . . . . . =',1PG20.13/
     & 5X,'RUPTURE VOID VOLUME FRACTION . . . . . . . . . . . =',1PG20.13/
     & 5X,'NON LOCAL PLASTIC STRAIN REGULARIZATION FLAG . . . =',I3/
     & 5X,'     ILOC=1  LOCAL DAMAGE MODEL '/
     & 5X,'     ILOC=2  NON LOCAL MICROMORPHIC MODEL '/
     & 5X,'     ILOC=3  NON LOCAL PEERLING MODEL ')
 1200 FORMAT(
     & 5X,'NON-LOCAL REGULARIZATION LENGTH. . . . . . . . . . =',1PG20.13/
     & 5X,'CONVERGENCE ELEMENT LENGTH TARGET. . . . . . . . . =',1PG20.13/)
 1300 FORMAT(
     & 5X,'PENALTY PARAMETER FOR NONLOCAL MICROMORPHIC METHOD =',1PG20.13/)
C-----------       
      RETURN
      END
