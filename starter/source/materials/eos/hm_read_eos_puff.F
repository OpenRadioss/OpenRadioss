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
Chd|  HM_READ_EOS_PUFF              source/materials/eos/hm_read_eos_puff.F
Chd|-- called by -----------
Chd|        HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_EOS_PUFF(IOUT,PM,UNITAB,IUNIT,LSUBMODEL,IMIDEOS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD      
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C reading parameters for
C   PUFF EQUATION OF STATE
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
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IIN,IOUT,IUNIT
      my_real PM(NPROPM)
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      INTEGER,INTENT(IN) :: IMIDEOS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "submod_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   C1, C2, C3, G0, T1, T2, ES, HH, E0, RHO0,
     .   FAC_L,FAC_T,FAC_M,FAC_C,RHOR,RHOI,MU2,
     .   XX,AA,BB,GX,PRES,EXPA,ETA,EE,CC, MU0, DF, PP ,SSP0, DPDMU, PC
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_IS_CRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV('EOS_C1', C1, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_C2', C2, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('EOS_C3', C3, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_GAMA0', G0, IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('MAT_SIGT1',T1, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_SIGT2', T2, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('E_S', ES, IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('MAT_EOH',HH, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_EA', E0, IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho', RHO0, IS_AVAILABLE,LSUBMODEL,UNITAB)

      IF(T1 == ZERO) T1=C1

      RHOR = PM(1)
      RHOI = PM(89)

      IF(RHO0 > ZERO) THEN
        RHOR = RHO0
        PM(1)= RHO0 
      ELSE
        RHO0=RHOR                   
      ENDIF

      PM( 23) = E0
      PM( 32) = C1
      PM( 33) = C2
      PM( 34) = C3
      PM( 35) = T1
      PM( 36) = T2
      PM(160) = ES
      PM(161) = G0
      PM(162) = HH
      IF(PM(79)==ZERO)PM(79)=THREE100

      !COMPUTING INITIAL PRESSURE
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

      MU2 = MU0*MU0
            
      XX =MU0/(ONE+MU0)  
      IF(MU0 >= ZERO) THEN                                              
        AA=(C1+C3*MU2)*MU0+C2*MU2                      
        GX=ONE-G0*HALF*XX                                           
        BB=G0                                                        
        PRES=MAX(AA*GX+BB*E0,PM(37))                                                                              
      ELSEIF(E0<ES)THEN                                      
        AA=(T1+T2*MU0)*MU0                                    
        GX=ONE-G0*HALF*XX                                           
        BB=G0                                                        
        PRES=MAX(AA*GX+BB*E0,PM(37))                                                                          
      ELSE                                                              
        ETA=ONE+MU0                                                    
        EE=SQRT(ETA)                                                    
        BB=(HH+(G0-HH)*EE)*ETA                                 
        CC= C1/(G0*ES)                                      
        EXPA=EXP(CC*XX)                                                 
        AA= BB*ES*(EXPA-ONE)                                       
        PRES=MAX(AA+BB*E0,PM(37))                                                                           
      ENDIF
      PM(31) = PRES 
      PC = PM(37)   
      
      !SSP0
      PP = PM(31)
      SSP0 = ZERO 
      G0 = PM(22)
      RHOI = PM(89)

      XX =MU0/(ONE+MU0)                                              
      IF(MU0>=ZERO) THEN                                                              
        DPDMU=(C1+TWO*C2*MU0+THREE*C3*MU2)*GX + G0*DF*DF*(PRES-HALF*AA)                     
      ELSEIF(E0<ES)THEN                                                         
        DPDMU=(T1+TWO*T2*MU0)*GX + G0*DF*DF*(PRES-HALF*AA)                                                                          
      ELSE                                                                                   
        DPDMU=BB*DF*DF*(PRES+ES*EXPA*CC) + (E0+ES*(EXPA-ONE))*(HH+THREE_HALF*EE*(G0-HH))                                                     
      ENDIF                                                             
        
      DPDMU=MAX(ZERO,DPDMU)
      IF(RHOR > ZERO) SSP0 = SQRT((DPDMU + TWO_THIRD*G0)/RHOR) 
      PM(27)=SSP0      
      

      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1500)C1,C2,C3,G0,T1,T2,ES,HH,E0,PM(31),PM(1)
      ENDIF

      RETURN
      
 1000 FORMAT(
     & 5X,'  PUFF EOS     ',/,
     & 5X,'  --------     ',/)
 1500 FORMAT(
     & 5X,'C1. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C2. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C3. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'GRUNEISEN GAMMA . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'T1. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'T2. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SUBLIMATION ENERGY (PER UNIT VOLUME). . . .=',1PG20.13/,
     & 5X,'H . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'INITIAL INTERNAL ENERGY (PER UNIT VOLUME) .=',1PG20.13/,
     & 5X,'INITIAL PRESSURE. . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'REFERENCE DENSITY . . . . . . . . . . . . .=',1PG20.13//)

      RETURN
      END
