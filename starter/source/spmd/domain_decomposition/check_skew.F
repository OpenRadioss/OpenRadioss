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
Chd|  CHECK_SKEW                    source/spmd/domain_decomposition/check_skew.F
Chd|-- called by -----------
Chd|        DOMDEC2                       source/spmd/domdec2.F         
Chd|-- calls ---------------
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        SKEW_MOD                      share/modules1/skew_mod.F     
Chd|====================================================================
        SUBROUTINE CHECK_SKEW(IXR,IGEO,ISKN,CEP,ISKWP,NSKWP,TAG_SKN,MULTIPLE_SKEW,
     .                        R_SKEW,IPM)
C----------------------------------------------------------
C----------------------------------------------------------
        USE SKEW_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, DIMENSION(*), INTENT(INOUT) :: NSKWP,R_SKEW
        INTEGER, DIMENSION(NUMSKW+NSUBMOD+1), INTENT(INOUT) :: TAG_SKN
        INTEGER, DIMENSION(*), INTENT(IN) :: CEP
        INTEGER, DIMENSION(NUMSKW+1), INTENT(INOUT) :: ISKWP
        INTEGER, DIMENSION(NIXR,*), INTENT(IN) :: IXR
        INTEGER, DIMENSION(NPROPGI,*), INTENT(IN) :: IGEO
        INTEGER, DIMENSION(LISKN,*), INTENT(IN) :: ISKN
        INTEGER, DIMENSION(NPROPMI,*), INTENT(IN) :: IPM
        TYPE(SKEW_TYPE), DIMENSION(NUMSKW+1), INTENT(INOUT) :: MULTIPLE_SKEW
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       NSKWP : integer ; dimension = NSPMD
!               number of skew per processor
!       TAG_SKN : integer ; dimension=NUMSKW+NSUBMOD+1
!                 tag array --> tag the i SKEW if a SPRING uses it
!                 tag array=0 --> the SKEW is not used by a SPRING
!                 tag array=1 --> the SKEW is used by one SPRING      
!                 tag array>1 --> the SKEW is used by several SPRING
!       CEP : integer ; dimension=NUMNOD
!             gives the ID processor of the current i node
!       ISKWP : integer ; dimension=NUMSKW+1
!               gives the ID processir of the current i SKEW
!               ISKWP < 0 --> the SKEW is local on a processor
!               and we don't need to communicate the data
!               ISKWP > 0 --> the SKEW is global and the data must be
!               communicated
!       IXR : integer ; dimension=NIXR*number of SPRING
!             SPRING array
!       IGEO : integer ; dimension=NPROPGI*number of /PROP
!              PROP array
!       ISKN : integer ; dimension=LISKN*number of SKEW
!              SKEW array
!       MULTIPLE_SKEW : SKEW_TYPE ; dimension=NUMSKW+1
!                       MULTIPLE_SKEW(I)%PLIST(:) is a list of processor
!                       where the SKEW is stuck
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,II,NN
        INTEGER :: NUMBER_SKEW_SP,SPRING_TYPE
        INTEGER :: N1,N2,N3,IMASTER,SUM_NI,SIZE_SKEW,MAT_TYPE
        INTEGER, DIMENSION(:), ALLOCATABLE :: SKEW_PER_SP,LOCAL_P,P_SKEW
        INTEGER, DIMENSION(NSPMD) :: PROC_SKEW
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
!$COMMENT
!       CHECK_SKEW description
!       CHECK_SKEW sticks a SKEW on a given processor in order to reduce
!       the SKEW communication in the engine.
!       A SKEW can be linked to a SPRING --> in this case, the SKEW is 
!       stuck on the local SPRING processor
!       If a SKEW is used by several SPRING, the SKEW is stuck on each 
!       processors that need it
!       
!       CHECK_SKEW organization :
!       - link every SPRING with theirs SKEW
!       - if several SPRING use a same SKEW, allocate MULTIPLE_SKEW(I)%PLIST
!       - stick a SKEW on a given processor, count the number of SKEW per
!         processor, initialize the MULTIPLE_SKEW(I)%PLIST array
!       - count the number of SKEW per processor in the case of multiple 
!         SPRING per SKEW
!$ENDCOMMENT
C
!       compute adhoc size for local arrays
        NUMBER_SKEW_SP = 0
        DO I=1,NUMELR
          SPRING_TYPE = IGEO( 11,IXR(1,I) )
          MAT_TYPE   = 0
          IF (IXR(5,I) > 0) MAT_TYPE = IPM(2,IXR(5,I))
C
          IF (R_SKEW(I) > 1) THEN
            IF(SPRING_TYPE==8 .OR. (SPRING_TYPE== 23 .AND. MAT_TYPE == 108)) NUMBER_SKEW_SP = NUMBER_SKEW_SP + 1
          ENDIF
C
          IF (IGEO( 2,IXR(1,I) )>1) THEN
            IF(SPRING_TYPE==8 .OR. SPRING_TYPE==13 .OR. SPRING_TYPE== 23) NUMBER_SKEW_SP = NUMBER_SKEW_SP + 1     
          ENDIF
        ENDDO

!       allocation and initialization of local array
        ALLOCATE(SKEW_PER_SP(NUMBER_SKEW_SP))
        ALLOCATE(LOCAL_P(NUMBER_SKEW_SP))

        SKEW_PER_SP(1:NUMBER_SKEW_SP) = 0
        LOCAL_P(1:NUMBER_SKEW_SP) = 0
        TAG_SKN(1:NUMSKW+NSUBMOD+1) = 0
        NUMBER_SKEW_SP = 0

!       link between SPRING and SKEW (only for SPRING type =8,13 or 23)
        DO I=1,NUMELR
                SPRING_TYPE = IGEO( 11,IXR(1,I) )
                MAT_TYPE   = 0
                IF (IXR(5,I) > 0) MAT_TYPE = IPM(2,IXR(5,I))
C
                IF (R_SKEW(I) > 1) THEN
                        IF(SPRING_TYPE==8 .OR. (SPRING_TYPE== 23 .AND. MAT_TYPE == 108)) THEN
                                NUMBER_SKEW_SP = NUMBER_SKEW_SP + 1
                                SKEW_PER_SP( NUMBER_SKEW_SP ) = R_SKEW(I)   
                                LOCAL_P(NUMBER_SKEW_SP) = CEP(I)+1
                                TAG_SKN( R_SKEW(I) ) = TAG_SKN( R_SKEW(I) ) + 1
                        ENDIF
                ENDIF
C
                IF (IGEO( 2,IXR(1,I) )>1) THEN
                        IF(SPRING_TYPE==8 .OR. SPRING_TYPE==13 .OR. SPRING_TYPE== 23) THEN
                                NUMBER_SKEW_SP = NUMBER_SKEW_SP + 1
                                SKEW_PER_SP( NUMBER_SKEW_SP ) = IGEO( 2,IXR(1,I) )    
                                LOCAL_P(NUMBER_SKEW_SP) = CEP(I)+1
                                TAG_SKN( IGEO(2,IXR(1,I)) ) = TAG_SKN( IGEO(2,IXR(1,I)) ) + 1
                        ENDIF     
                ENDIF
        ENDDO
!       allocation of MULTIPLE_SKEW
        DO I=1,NUMSKW+1
                SIZE_SKEW = TAG_SKN(I)
                IF(SIZE_SKEW>1) THEN
                        IF(.NOT.ALLOCATED(MULTIPLE_SKEW(I)%PLIST)) ALLOCATE( MULTIPLE_SKEW(I)%PLIST(SIZE_SKEW) )
                        MULTIPLE_SKEW(I)%PLIST(1:SIZE_SKEW) = 0
                ENDIF
        ENDDO    

        ALLOCATE(P_SKEW(NUMSKW+1))
        P_SKEW(1:NUMSKW+1) = 0
                
!       stick a SKEW on a given processor
        DO J=1,NUMBER_SKEW_SP
                I = SKEW_PER_SP(J)
                IMASTER = LOCAL_P(J)
                N1=ISKN(1,I)
                N2=ISKN(2,I)
                N3=ISKN(3,I)
                SUM_NI = N1+N2+N3
                IF(SUM_NI/=0) THEN

                        CALL IFRONTPLUS(N1,IMASTER)
                        CALL IFRONTPLUS(N2,IMASTER)
                        CALL IFRONTPLUS(N3,IMASTER)
                        IF(ISKWP(I)==0) ISKWP(I) = -IMASTER
                        IF(TAG_SKN(I)>1) THEN
                                P_SKEW(I) = P_SKEW(I) + 1
                                MULTIPLE_SKEW(I)%PLIST(P_SKEW(I)) =  IMASTER
                        ELSE
                                NSKWP(IMASTER) = NSKWP(IMASTER) + 1
                        ENDIF                                
                ENDIF
        ENDDO

!       count and add the number of SKEW per processor in the case of multiple SPRING per SKEW
        DO I=1,NUMSKW+1
                SIZE_SKEW = TAG_SKN(I)
                IF(SIZE_SKEW>1) THEN
                        PROC_SKEW(1:NSPMD) = 0
                        DO II=1,SIZE_SKEW
                                IMASTER = MULTIPLE_SKEW(I)%PLIST(II)
                                IF(IMASTER>0) PROC_SKEW(IMASTER) = PROC_SKEW(IMASTER) + 1
                        ENDDO
                        DO II=1,NSPMD
                                IF(PROC_SKEW(II)>0) NSKWP(II) = NSKWP(II) + 1
                        ENDDO
                ENDIF
        ENDDO

!       deallocation of local arrais
        DEALLOCATE(SKEW_PER_SP,LOCAL_P)
        DEALLOCATE(P_SKEW)

        RETURN
        END SUBROUTINE CHECK_SKEW
