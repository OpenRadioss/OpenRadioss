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
Chd|  INIVOL_SET                    source/initial_conditions/inivol/inivol_set.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTIFLUID_GLOBAL_TDET(IPARG,ELBUF_TAB,MULTI_FVM,IPM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C CASE OF SUBMATERIAL BASED ON JWL MATERIAL LAW (HIGH EXPLISIVE WITH TIME CONTROL DETONATION)
C
C This subroutine is computing detonation times in global buffer from detonation times of layers (submaterials
C Global detonation time is relevant when there are several explosive submaterial laws.
C
C GBUF%TB : global burning time
C LBUF%TB : local burning time of current layer
C If iniital volume fraction is 0.0 then detonation time of current layer is not taken into account to calculate global detonation time.
C
C This subroutine must be called after INIVIOL treatment (INITIAL VOLUME FRACTION)
C Precondition : MULTI_FVM%IS_USED=.TRUE.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
      USE MULTI_FVM_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)                 :: IPARG(NPARG,NGROUP),IPM(NPROPMI,NUMMAT)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP),INTENT(INOUT) :: ELBUF_TAB
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NEL,IG,OFFSET,NG,SUBMATLAW,ILAY,NLAY,II
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF  
      TYPE(BUF_MAT_) ,POINTER :: MBUF
      my_real :: VFRAC
      LOGICAL IS_JWL
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
     
      IF(MULTI_FVM%IS_USED)THEN

        DO NG=1,NGROUP    
              MTN     = IPARG(1,NG)
              NEL     = IPARG(2,NG)
              NFT     = IPARG(3,NG)
              ITY     = IPARG(5,NG)
              
              IF (MTN == 151) THEN

                GBUF => ELBUF_TAB(NG)%GBUF
                NLAY = ELBUF_TAB(NG)%NLAY

                DO IG=1,NEL,NVSIZ                                                                               
                  OFFSET   = IG - 1                                                                                
                  LFT      = 1                                             
                  LLT      = MIN(NVSIZ,NEL-OFFSET)                                                                 
                  NFT      = IPARG(3,NG) + OFFSET  
                  
                  !Number of layers ( = number of material in law 151)
                  IF (NLAY > 1) THEN
                     IF (GBUF%G_TB > ZERO) THEN
                        GBUF%TB(LFT:LLT) = -EP20
                     ENDIF     

                     DO ILAY = 1, NLAY
                        !SUBMATLAW = IPM(2, IPM(20 + ILAY, MTN))
                        SUBMATLAW = ELBUF_TAB(NG)%BUFLY(ILAY)%ILAW  
                         
                        IF (SUBMATLAW == 5) THEN
                           LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)
                           DO II = LFT, LLT
                              VFRAC = LBUF%VOL(II)/GBUF%VOL(II) 
                              IF(VFRAC > ZERO)THEN
                                GBUF%TB(II) = MAX(GBUF%TB(II), LBUF%TB(II))  !must be done only in case of initial volume fraction
                              ENDIF
                           ENDDO
                           IS_JWL = .TRUE.
                        ENDIF
                     ENDDO
                     
                     IF(.NOT.IS_JWL)THEN
                       DO II = LFT, LLT
                         GBUF%TB(II) = ZERO
                       ENDDO   
                     ELSE
                       DO II = LFT, LLT
                         IF(GBUF%TB(II) == -EP20)THEN
                           GBUF%TB(II) = ZERO
                         ENDIF
                       ENDDO                                        
                     ENDIF
                     
                   ENDIF                                        

                ENDDO ! next IG    
                
              ENDIF
                                                                                                

         ENDDO ! next NG                                 

      ENDIF    
          
      END SUBROUTINE  
