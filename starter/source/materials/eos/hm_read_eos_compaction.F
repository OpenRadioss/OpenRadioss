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
Chd|  HM_READ_EOS_COMPACTION        source/materials/eos/hm_read_eos_compaction.F
Chd|-- called by -----------
Chd|        HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_EOS_COMPACTION(IOUT,PM,UNITAB,IUNIT,LSUBMODEL,IMIDEOS,EOS_TAG,IEOS,MAXEOS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD       
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C reading parameters for
C   COMPACTION EQUATION OF STATE
C-----------------------------------------------
C   C o m m e n t s
C-----------------------------------------------
C  RHOI = PM(89)   -> provided by /MAT
C  RHOR = PM(01)   -> provided by /MAT (can be erased by EOS if present : obsolete)
C  => MU0 = RHO/RHOR-1.
C  PM(31) = P(MU0,E0) -> will be used to initialize diagonal of stress tensor SIG(1:3,*)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IIN,IOUT,IUNIT
      my_real PM(NPROPM)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      INTEGER,INTENT(IN) :: IMIDEOS
      TYPE(EOS_TAG_),DIMENSION(0:MAXEOS) ,INTENT(INOUT) :: EOS_TAG  
      INTEGER,INTENT(IN) :: IEOS, MAXEOS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "submod_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real  GAMMA, P0,T0, E0, PSH, RHO0,FAC_L,FAC_T,FAC_M,FAC_C,PSTAR, FAC_R,RHOI,RHOR
      my_real  C0,C1,C2,C3,MUMAX,BID,BUNL,MU,MUMIN
      my_real  MU0,SSP0,DF, G0, BULK,BULK2,P_, BB, POLD, MU2, MUOLD, ALPHA,DPDMU
      INTEGER IFORM, IOUTP
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IFORM=0
      IOUTP=1
      
      EOS_TAG(IEOS)%G_MU = 1                            
      EOS_TAG(IEOS)%L_MU = 1 
           
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('EOS_COM_C0', C0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_COM_C1', C1, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_COM_C2', C2, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_COM_C3', C3, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV('IFORM', IFORM, IS_AVAILABLE,LSUBMODEL)

      CALL HM_GET_FLOATV('EOS_COM_Mue_min', MUMIN, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_COM_Mue_max', MUMAX, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_COM_B', BUNL, IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('LAW5_PSH', PSH, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho', RHO0, IS_AVAILABLE,LSUBMODEL,UNITAB)

      RHOR = PM(1)
      RHOI = PM(89)

      IF(RHO0 > ZERO) THEN
        RHOR = RHO0
        PM(1)= RHO0 
      ELSE
        RHO0=RHOR                   
      ENDIF
            
      IF(C1 <= ZERO)THEN
         CALL ANCMSG(MSGID=67,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=IMIDEOS,
     .               C1='/EOS/COMPACTION',
     .               C2='C1 MUST BE POSITIVE')
      ENDIF

      !IFORM=1 : CONSTANT unload modulus BUNL (DEFAULT)
      !IFORM=2 : LINEAR uload modulus from C1 to BUNL
      !IFORM=10 : legacy formulation law10 (slope change when mu<0) 
      IF(IFORM /= 1 .AND. IFORM /= 2 .AND. IFORM /= 10)THEN 
        IFORM=1 !default     
        IOUTP=0
      ENDIF
      
      MU = RHO0/RHOR-ONE
      P0 = C0+MIN(C1*MU,C1*MU+C2*MU*MU+C3*MU*MU*MU)
      E0 = ZERO

      PM(49) = C0
      PM(32) = C1
      PM(33) = C2
      PM(34) = C3
      PM(88) = PSH
      PM(45) = BUNL
      PM(46) = MUMAX
      PM(47) = MUMIN
      PM(48) = IFORM
      IF(PM(79)==ZERO)PM(79)=THREE100     
            
      PM(23) = E0
      PM(31) = P0-PSH
      PM(104)= P0-PSH
      
      IF(RHOI == ZERO)THEN
        MU0 = ZERO ! error 683 already displayed
      ELSE
        IF(RHOR /= ZERO)THEN
          MU0 = RHOI/RHOR-ONE
        ELSE
          MU0 = ZERO ! error 683 already displayed
        ENDIF
      ENDIF
      
      IF(RHOI /= ZERO)THEN
        DF = RHOR/RHOI
      ELSE
        DF = ZERO
      ENDIF
      
      MU2=MU0*MU0
      MUOLD=MU0
      POLD=P0
      BULK = BUNL
      BULK2 = BUNL          

      !SSP0
      SSP0 = ZERO 
      G0 = PM(22)
      RHOI = PM(89) 
        IF(IFORM == 10)THEN    
            BB = BULK
        ELSEIF(IFORM == 1)THEN
            BB=BUNL 
        ELSEIF(IFORM == 2)THEN
          ALPHA = ONE
          IF(MUMAX > ZERO)THEN
            ALPHA=MUOLD/MUMAX
          ENDIF        
          BB = ALPHA*BUNL+(ONE-ALPHA)*C1
        ENDIF           
        DPDMU = C1 + TWO*C2*MU0+THREE*C3*MU2   !can be discussed in expansion...
        DPDMU = MAX(BB,DPDMU)

      DPDMU=MAX(ZERO,DPDMU)
      IF(RHOR > ZERO) SSP0 = SQRT((DPDMU + TWO_THIRD*G0)/RHOR) 
      PM(27)=SSP0      

      WRITE(IOUT,1000)

      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1500)C0,C1,C2,C3,PSH,BUNL,MUMIN,MUMAX
        IF(IOUTP == 1)THEN
          IF(IFORM==10)THEN
             WRITE(IOUT,1510)
          ELSEIF(IFORM==1)THEN
             WRITE(IOUT,1501)
          ELSEIF(IFORM==2)THEN
             WRITE(IOUT,1502)
          ENDIF
        ENDIF
      ENDIF

      RETURN
 1000 FORMAT(
     & 5X,'  COMPACTION EOS    ',/,
     & 5X,'  --------------    ',/)
 1500 FORMAT(
     & 5X,'C0. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C1. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C2. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C3. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'PRESSURE SHIFT. . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BUNL : UNLOADING MODULUS. . . . . . . . . .=',1PG20.13/,
     & 5X,'MU_MIN : ELASTIC LIMIT. . . . . . . . . . .=',1PG20.13/,
     & 5X,'MU_MAX : MAXIMUM COMPACTION . . . . . . . .=',1PG20.13/)
 1510 FORMAT(
     & 5X,'LEGACY FORMULATION'/)
 1501 FORMAT(
     & 5X,'CONSTANT UNLOAD MODULUS'/)
 1502 FORMAT(
     & 5X,'CONTINUOUS UNLOAD MODULUS FROM C1 TO BUNL IN RANGE [MUMIN,MUMAX]'/)

      RETURN
  
      END
