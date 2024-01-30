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
Chd|  DAMPING_VREF                  source/assembly/damping_vref.F
Chd|-- called by -----------
Chd|        DAMPING51                     source/assembly/damping.F     
Chd|-- calls ---------------
Chd|        DAMPVREF_SUM6                 source/assembly/dampvref_sum6.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|        SPMD_FR_POFF                  source/mpi/kinematic_conditions/spmd_fr_poff.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE DAMPING_VREF(IGRNOD,V,A,MS,DAMPR,
     .                        WEIGHT,V_REF,A_REF,NDAMP_VREL,ID_DAMP_VREL,
     .                        FR_DAMP_VREL,NGRNOD,NUMNOD,NDAMP,NRDAMP,
     .                        NSPMD,IPARIT,ISPMD)                          
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: WEIGHT(NUMNOD),NDAMP_VREL,ID_DAMP_VREL(NDAMP_VREL)
      INTEGER, INTENT(IN) :: FR_DAMP_VREL(NSPMD+2,NDAMP_VREL)
      INTEGER, INTENT(IN) :: NGRNOD,NUMNOD,NDAMP,NRDAMP,NSPMD,IPARIT,ISPMD
      my_real, INTENT(IN) :: A(3,NUMNOD),V(3,NUMNOD),MS(NUMNOD)
      my_real, INTENT(IN) :: DAMPR(NRDAMP,NDAMP)
      my_real, INTENT(OUT) :: A_REF(3,NDAMP),V_REF(3,NDAMP)
C-----------------------------------------------
      TYPE (GROUP_),DIMENSION(NGRNOD), INTENT(IN) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,ND,IGR,NNOD,ID
      my_real :: INV_MAS 
      DOUBLE PRECISION SUML6(7,6,NDAMP_VREL),SUML(7,NDAMP_VREL)  
C-----------------------------------------------
C
C======================================================================|
C     Computation of reference velocity for /DAMP/VREF with no rbody
C======================================================================|

      IF (IPARIT == 1) THEN
C        
C--     Parith/ON assembling
C   
        DO ND = 1, NDAMP_VREL
          ID = ID_DAMP_VREL(ND)
          IGR   = NINT(DAMPR(2,ID))
          SUML6(1:7,1:6,ND) = ZERO
          NNOD = IGRNOD(IGR)%NENTITY
          CALL DAMPVREF_SUM6(IGRNOD,V,A,MS,WEIGHT,
     .                       ND,NNOD,IGR,SUML6,NGRNOD,
     .                       NUMNOD,NDAMP)
        ENDDO
C
        CALL MY_BARRIER
C
        DO ND = 1, NDAMP_VREL
          ID = ID_DAMP_VREL(ND)
          IF ((NSPMD>1).AND.(FR_DAMP_VREL(ISPMD+1,ND)==1)) THEN
            CALL SPMD_EXCH_FR6(FR_DAMP_VREL(1,ND),SUML6(1,1,ND),7*6)
          ENDIF   
C
          INV_MAS=ONE/MAX(EM20,(SUML6(7,1,ND)+SUML6(7,2,ND)+SUML6(7,3,ND)+
     .                          SUML6(7,4,ND)+SUML6(7,5,ND)+SUML6(7,6,ND)))         
C          
          V_REF(1,ID)=INV_MAS*(SUML6(1,1,ND)+SUML6(1,2,ND)+SUML6(1,3,ND)+
     .                         SUML6(1,4,ND)+SUML6(1,5,ND)+SUML6(1,6,ND))
          V_REF(2,ID)=INV_MAS*(SUML6(2,1,ND)+SUML6(2,2,ND)+SUML6(2,3,ND)+
     .                         SUML6(2,4,ND)+SUML6(2,5,ND)+SUML6(2,6,ND))
          V_REF(3,ID)=INV_MAS*(SUML6(3,1,ND)+SUML6(3,2,ND)+SUML6(3,3,ND)+
     .                         SUML6(3,4,ND)+SUML6(3,5,ND)+SUML6(3,6,ND))
          A_REF(1,ID)=INV_MAS*(SUML6(4,1,ND)+SUML6(4,2,ND)+SUML6(4,3,ND)+
     .                         SUML6(4,4,ND)+SUML6(4,5,ND)+SUML6(4,6,ND))
          A_REF(2,ID)=INV_MAS*(SUML6(5,1,ND)+SUML6(5,2,ND)+SUML6(5,3,ND)+
     .                         SUML6(5,4,ND)+SUML6(5,5,ND)+SUML6(5,6,ND))
          A_REF(3,ID)=INV_MAS*(SUML6(6,1,ND)+SUML6(6,2,ND)+SUML6(6,3,ND)+
     .                         SUML6(6,4,ND)+SUML6(6,5,ND)+SUML6(6,6,ND))
        ENDDO          
C        
      ELSE
C        
C--     Parith/OFF assembling        
C
        DO ND = 1, NDAMP_VREL
          ID = ID_DAMP_VREL(ND)
          SUML(1:7,ND) = ZERO
          IGR   = NINT(DAMPR(2,ID))
          DO N=1,IGRNOD(IGR)%NENTITY
            I=IGRNOD(IGR)%ENTITY(N)
            SUML(1,ND) = SUML(1,ND) + MS(I)*V(1,I)*WEIGHT(I)
            SUML(2,ND) = SUML(2,ND) + MS(I)*V(2,I)*WEIGHT(I)
            SUML(3,ND) = SUML(3,ND) + MS(I)*V(3,I)*WEIGHT(I)
            SUML(4,ND) = SUML(4,ND) + MS(I)*A(1,I)*WEIGHT(I)
            SUML(5,ND) = SUML(5,ND) + MS(I)*A(2,I)*WEIGHT(I)
            SUML(6,ND) = SUML(6,ND) + MS(I)*A(3,I)*WEIGHT(I)
            SUML(7,ND) = SUML(7,ND) + MS(I)*WEIGHT(I)
          ENDDO 
        ENDDO
C
        CALL MY_BARRIER
C
        DO ND = 1, NDAMP_VREL
          ID = ID_DAMP_VREL(ND)
          IF ((NSPMD>1).AND.(FR_DAMP_VREL(ISPMD+1,ND)==1)) THEN           
            CALL SPMD_FR_POFF(FR_DAMP_VREL(1,ND),SUML(1,ND),7)
          ENDIF  
          INV_MAS = ONE/MAX(EM20,SUML(7,ND))
          V_REF(1,ID) = SUML(1,ND)*INV_MAS
          V_REF(2,ID) = SUML(2,ND)*INV_MAS
          V_REF(3,ID) = SUML(3,ND)*INV_MAS
          A_REF(1,ID) = SUML(4,ND)*INV_MAS
          A_REF(2,ID) = SUML(5,ND)*INV_MAS
          A_REF(3,ID) = SUML(6,ND)*INV_MAS 
        ENDDO         
C       
      ENDIF  
C
      RETURN
C      
      END