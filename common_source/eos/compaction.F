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
Chd|  COMPACTION                    eos/compaction.F              
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COMPACTION     (
     1                            IFLAG,NEL  ,PM   ,OFF  ,EINT ,MU   ,MU2 , 
     2                            ESPE ,DVOL ,DF   ,VNEW ,MAT  ,PSH  ,POLD,
     3                            PNEW ,DPDM ,DPDE ,THETA,ECOLD,SIG  ,MU_OLD)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of COMPACTION EOS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*), IFLAG, NEL
      my_real
     .   PM(NPROPM,*), 
     .   OFF(*)  ,EINT(*) ,MU(*)   , 
     .   MU2(*)  ,ESPE(*) ,DVOL(*) ,DF(*)  , 
     .   VNEW(*) ,PNEW(*) ,DPDM(*),
     .   DPDE(*) ,THETA(*),ECOLD(*),
     .   SIG(NEL,6),POLD(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, IFORM
      my_real :: P0,PSH(MVSIZ),GAMMA,T0,E0,SPH,AA, BB,TFEXTT, DVV, PP, AR0B, B(MVSIZ),PNE1,RHO0(MVSIZ),PFRAC
      my_real :: C0,C1,C2,C3,BUNL,MU_OLD(MVSIZ),MU_MAX,PEN1,P(MVSIZ),P_
      my_real :: BULK2(MVSIZ),BULK(MVSIZ),ALPHA,MUMIN
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

       MX           = MAT(LFT)
       E0           = PM(23,MX)         
       C0           = PM(49,MX)
       C1           = PM(32,MX)
       C2           = PM(33,MX)
       C3           = PM(34,MX)
       PSH(LFT:LLT) = PM(88,MX)
       BUNL         = PM(45,MX)
       MU_MAX       = PM(46,MX)
       SPH          = PM(69,MX)
       P0           = PM(31,MX)
       PFRAC        = PM(37,MX)
       MUMIN        = PM(47,MX)
       IFORM        = NINT(PM(48,MX))
       TFEXTT       = ZERO

       BULK(LFT:LLT)  = BUNL
       BULK2(LFT:LLT) = BUNL

      !----------------------------------------------------------------!
      !  COMPACTION EOS                                                !
      !----------------------------------------------------------------!  
      IF(IFORM.EQ.1)THEN    
        DO I=LFT,LLT
          IF(MU_OLD(I).LT.ZERO)BULK(I)  = C1 
          IF(MU(I).LT.ZERO)    BULK2(I) = C1
          PNE1 = POLD(I)+PSH(I)+MU(I)*BULK2(I)-MU_OLD(I)*BULK(I)
          P(I)    = C0+C1*MU(I)+C2*MU2(I)+C3*MU2(I)*MU(I)
          IF(MU(I).LT.MU_MAX) P(I) = MIN(P(I),PNE1) 
          P(I) = MAX(P(I),PFRAC)  *OFF(I) 
          !B(I) = MAX(BULK(I),BULK2(I))
          B(I) = BULK(I)
        ENDDO !next I   
      ELSEIF(IFORM.EQ.2)THEN
        DO I=LFT,LLT
          P(I) = C0+C1*MU(I)+(C2+C3*MU(I))*MU2(I)
          P_   = C0+C1*MU_OLD(I)+(C2+C3*MU_OLD(I))*MU_OLD(I)*MU_OLD(I)
          B(I)=BUNL 
          PNE1 = P_-(MU_OLD(I)-MU(I))*B(I)
          IF(MU_OLD(I).GT.MUMIN) P(I) = MIN(PNE1, P(I))
          P(I) = MAX(P(I),PFRAC)  *OFF(I)
        ENDDO !next I   
      ELSEIF(IFORM.EQ.3)THEN
        DO I=LFT,LLT
          P(I) = C0+C1*MU(I)+(C2+C3*MU(I))*MU2(I)
          P_   = C0+C1*MU_OLD(I)+(C2+C3*MU_OLD(I))*MU_OLD(I)*MU_OLD(I)
          !linear unload modulus
          ALPHA = UN
          IF(MU_MAX.GT.ZERO)THEN
            ALPHA=MU_OLD(I)/MU_MAX
          ENDIF
          B(I) = ALPHA*BUNL+(UN-ALPHA)*C1
          PNE1 = P_-(MU_OLD(I)-MU(I))*B(I)
          IF(MU_OLD(I).GT.MUMIN) P(I) = MIN(PNE1, P(I))
          P(I) = MAX(P(I),PFRAC)  *OFF(I)
        ENDDO !next I   
      ENDIF
      !----------------------------------------------------------------!
      !  SOUND SPEED                                                   !
      !----------------------------------------------------------------!      
      DO I=LFT,LLT
        DPDM(I) = C1 + DEUX*C2*MU(I)+TROIS*C3*MU2(I)   !can be discussed in expansion...
        DPDM(I) = MAX(B(I),DPDM(I))
        DPDE(I) = ZERO
      ENDDO !next I   
      !----------------------------------------------------------------!
      !  OUTPUT                                                        !
      !----------------------------------------------------------------!      
      DO I=LFT,LLT
        P(I)=MAX(PFRAC,P(I))*OFF(I)
        PNEW(I) = P(I)-PSH(I)
      ENDDO !next I     
      DO I=LFT,LLT
        ECOLD(I)=-TROIS100*SPH
        !IF(MU(I).GT.ZERO) ECOLD(I)=
      ENDDO

      

      IF(IFLAG == 1) THEN
        !----------------------------------------------------------------!
        !  FRACTURE  - MU_OLD                                            !
        !----------------------------------------------------------------!      
        DO I=LFT,LLT
          EINT(I) = EINT(I) - UNDEMI*DVOL(I)*(PNEW(I)+PSH(I) )
        ENDDO !next I 
        !----------------------------------------------------------------!
        !  FRACTURE  - MU_OLD                                            !
        !----------------------------------------------------------------!      
        IF(IFORM.EQ.1)THEN
          DO I=LFT,LLT
            IF(P(I).LE.PFRAC)THEN
              MU_OLD(I) =MU_OLD(I)+(PFRAC-POLD(I)-PSH(I))/BULK2(I)  
            ELSE
              MU_OLD(I)= MU(I)
            ENDIF
          ENDDO !next I
        ELSEIF(IFORM.GE.2)THEN
          DO I=LFT,LLT
            IF(MU(I).GT.MU_OLD(I)) MU_OLD(I) = MIN(MU_MAX,MU(I))
          ENDDO !next I 
        ENDIF
        !----------------------------------------------------------------!
        !  OUTPUT                                                        !
        !----------------------------------------------------------------!      
        DO I=LFT,LLT
          P(I)=MAX(PFRAC,P(I))*OFF(I)
          PNEW(I) = P(I)-PSH(I)
        ENDDO !next I     
         DO I=LFT,LLT
           ECOLD(I)=-TROIS100*SPH
           !IF(MU(I).GT.ZERO) ECOLD(I)=
         ENDDO
        !----------------------------------------------------------------!
        !  PRESSURE WORK                                                 !
        !----------------------------------------------------------------!      
         DO I=LFT,LLT
           TFEXTT     = TFEXTT-DVOL(I)*PSH(I)
         ENDDO
#include "atomic.inc"
         TFEXT = TFEXT + TFEXTT
#include "atomend.inc"

      ELSEIF(IFLAG == 2) THEN
        !----------------------------------------------------------------!
        !  FRACTURE  - MU_OLD                                            !
        !----------------------------------------------------------------!      
        IF(IFORM.EQ.1)THEN
          DO I=1,NEL
            IF(P(I).LE.PFRAC)THEN
              MU_OLD(I) =MU_OLD(I)+(PFRAC-POLD(I)-PSH(I))/BULK2(I)  
            ELSE
              MU_OLD(I)= MU(I)
            ENDIF
          ENDDO !next I
        ELSEIF(IFORM.GE.2)THEN
          DO I=1,NEL
            IF(MU(I).GT.MU_OLD(I)) MU_OLD(I) = MIN(MU_MAX,MU(I))
          ENDDO !next I 
        ENDIF
        !----------------------------------------------------------------!
        !  OUTPUT                                                        !
        !----------------------------------------------------------------!      
        DO I=1,NEL
          P(I)=MAX(PFRAC,P(I))*OFF(I)
          PNEW(I) = P(I)-PSH(I)
        ENDDO !next I     
      ENDIF


C------------------------      
      RETURN
      END
