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
Chd|  COMPACTION                    eos/compaction.F              
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COMPACTION(
     1                      IFLAG, NEL  , PM   , OFF  , EINT , MU   , MU2 , 
     2                      ESPE , DVOL , DF   , VNEW , MAT  , PSH  , POLD,
     3                      PNEW , DPDM , DPDE , THETA, ECOLD, SIG  , MU_OLD)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving of COMPACTION EOS
C
C Iform : formulation flag for unload behavior
C          1 : constant unload modulus (DEFAULT)
C          2 : unload modulus increases with compaction from C1 to BUNL in [MU_MIN,MU_MAX]
C         10 : legacy behvior (fixed with iform=1) (not documented)
C
C  MU_MIN : elastic behavior up to this limit
C  MU_MAX : elastic behavior above this limit
C  C0,C1,C2,C3 : EoS parameter
C  BUNL : unload modulus
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: MAT(NEL), IFLAG, NEL
      my_real,INTENT(INOUT) :: PM(NPROPM,NUMMAT), 
     .        OFF(NEL)  ,EINT(NEL) ,MU(NEL), 
     .        MU2(NEL)  ,ESPE(NEL) ,DVOL(NEL),
     .        VNEW(NEL) ,PNEW(NEL) ,DPDM(NEL),
     .        DPDE(NEL) ,THETA(NEL),ECOLD(NEL),
     .        SIG(NEL,6),POLD(NEL) ,DF(NEL)   
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

       MX         = MAT(1)
       E0         = PM(23,MX)         
       C0         = PM(49,MX)
       C1         = PM(32,MX)
       C2         = PM(33,MX)
       C3         = PM(34,MX)
       PSH(1:NEL) = PM(88,MX)
       BUNL       = PM(45,MX)
       MU_MAX     = PM(46,MX)
       SPH        = PM(69,MX)
       P0         = PM(31,MX)
       PFRAC      = PM(37,MX)
       MUMIN      = PM(47,MX)
       IFORM      = NINT(PM(48,MX))
       
       TFEXTT       = ZERO
       BULK(1:NEL)  = BUNL
       BULK2(1:NEL) = BUNL
       
      !----------------------------------------------------------------!
      !  COMPACTION EOS                                                !
      !----------------------------------------------------------------!  
      !--- constant unload slope ---!       
      IF(IFORM == 1)THEN
        DO I=1,NEL
          P(I) = C0+C1*MU(I)+(C2+C3*MU(I))*MU2(I)
          P_   = C0+C1*MU_OLD(I)+(C2+C3*MU_OLD(I))*MU_OLD(I)*MU_OLD(I)
          B(I) = BUNL 
          PNE1 = P_-(MU_OLD(I)-MU(I))*B(I)
          IF(MU_OLD(I) > MUMIN) P(I) = MIN(PNE1, P(I))
          P(I) = MAX(P(I),PFRAC)*OFF(I)
        ENDDO !next I   
      !--- continuous unload slope (increases with compaction) ---!
      ELSEIF(IFORM == 2)THEN
        DO I=1,NEL
          P(I) = C0+C1*MU(I)+(C2+C3*MU(I))*MU2(I)
          P_   = C0+C1*MU_OLD(I)+(C2+C3*MU_OLD(I))*MU_OLD(I)*MU_OLD(I)
          !linear unload modulus
          ALPHA = ONE
          IF(MU_MAX > ZERO)THEN
            ALPHA=MU_OLD(I)/MU_MAX
          ENDIF
          B(I) = ALPHA*BUNL+(ONE-ALPHA)*C1
          PNE1 = P_-(MU_OLD(I)-MU(I))*B(I)
          IF(MU_OLD(I) > MUMIN) P(I) = MIN(PNE1, P(I))
          P(I) = MAX(P(I),PFRAC)  *OFF(I)
        ENDDO !next I  
      !--- legacy behavior ---! 
      ELSEIF(IFORM == 10)THEN    
        DO I=1,NEL
          IF(MU_OLD(I) < ZERO)BULK(I)  = C1 
          IF(MU(I) < ZERO)    BULK2(I) = C1
          PNE1 = POLD(I)+PSH(I)+MU(I)*BULK2(I)-MU_OLD(I)*BULK(I)
          P(I)    = C0+C1*MU(I)+C2*MU2(I)+C3*MU2(I)*MU(I)
          IF(MU(I) < MU_MAX) P(I) = MIN(P(I),PNE1) 
          P(I) = MAX(P(I),PFRAC)  *OFF(I) 
          !B(I) = MAX(BULK(I),BULK2(I))
          B(I) = BULK(I)
        ENDDO !next I            
      ENDIF
      !----------------------------------------------------------------!
      !  SOUND SPEED                                                   !
      !----------------------------------------------------------------!      
      DO I=1,NEL
        DPDM(I) = C1 + MAX(ZERO,MU(I)) *( TWO*C2+THREE*C3*MU(I) ) 
        DPDM(I) = MAX(B(I),DPDM(I))
        DPDE(I) = ZERO
      ENDDO !next I   
      !----------------------------------------------------------------!
      !  OUTPUT                                                        !
      !----------------------------------------------------------------!      
      DO I=1,NEL
        P(I)=MAX(PFRAC,P(I))*OFF(I)
        PNEW(I) = P(I)-PSH(I)
      ENDDO !next I     
      DO I=1,NEL
        ECOLD(I)=-THREE100*SPH
        !IF(MU(I) > ZERO) ECOLD(I)=
      ENDDO

      

      IF(IFLAG == 1) THEN
        !----------------------------------------------------------------!
        !  FRACTURE  - MU_OLD                                            !
        !----------------------------------------------------------------!      
        DO I=1,NEL
          EINT(I) = EINT(I) - HALF*DVOL(I)*(PNEW(I)+PSH(I) )
        ENDDO !next I 
        !----------------------------------------------------------------!
        !  FRACTURE  - MU_OLD                                            !
        !----------------------------------------------------------------!      
        IF(IFORM == 10)THEN
          DO I=1,NEL
            IF(P(I) <= PFRAC)THEN
              MU_OLD(I) =MU_OLD(I)+(PFRAC-POLD(I)-PSH(I))/BULK2(I)  
            ELSE
              MU_OLD(I)= MU(I)
            ENDIF
          ENDDO !next I
        ELSEIF(IFORM <= 2)THEN   !iform=1,2
          DO I=1,NEL
            IF(MU(I) > MU_OLD(I)) MU_OLD(I) = MIN(MU_MAX,MU(I))
          ENDDO !next I 
        ENDIF
        !----------------------------------------------------------------!
        !  OUTPUT                                                        !
        !----------------------------------------------------------------!      
        DO I=1,NEL
          P(I)=MAX(PFRAC,P(I))*OFF(I)
          PNEW(I) = P(I)-PSH(I)
        ENDDO !next I     
         DO I=1,NEL
           ECOLD(I)=-THREE100*SPH
           !IF(MU(I) > ZERO) ECOLD(I)=
         ENDDO
        !----------------------------------------------------------------!
        !  PRESSURE WORK                                                 !
        !----------------------------------------------------------------!      
         DO I=1,NEL
           TFEXTT     = TFEXTT-DVOL(I)*PSH(I)
         ENDDO
#include "atomic.inc"
         TFEXT = TFEXT + TFEXTT
#include "atomend.inc"


      ELSEIF(IFLAG == 2) THEN
        !----------------------------------------------------------------!
        !  FRACTURE  - MU_OLD                                            !
        !----------------------------------------------------------------!      
        IF(IFORM == 10)THEN
          DO I=1,NEL
            IF(P(I) <= PFRAC)THEN
              MU_OLD(I) =MU_OLD(I)+(PFRAC-POLD(I)-PSH(I))/BULK2(I)  
            ELSE
              MU_OLD(I)= MU(I)
            ENDIF
          ENDDO !next I
        ELSEIF(IFORM <= 2)THEN  !iform=1,2
          DO I=1,NEL
            IF(MU(I) > MU_OLD(I)) MU_OLD(I) = MIN(MU_MAX,MU(I))
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
