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
Chd|  SPLIT_SKEW                    source/restart/ddsplit/split_skew.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        SKEW_MOD                      share/modules1/skew_mod.F     
Chd|====================================================================
        SUBROUTINE SPLIT_SKEW(CURR_PROC,ISKWP_L,ISKWP,TAG_SKN,MULTIPLE_SKEW)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: CURR_PROC
        INTEGER, DIMENSION(NUMSKW+1), INTENT(IN) :: TAG_SKN
        INTEGER, DIMENSION(NUMSKW+1), INTENT(IN) :: ISKWP
        INTEGER, DIMENSION(NUMSKW+1), INTENT(INOUT) :: ISKWP_L
        TYPE(SKEW_TYPE), DIMENSION(NUMSKW+1), INTENT(IN) :: MULTIPLE_SKEW
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       CURR_PROC : integer
!                   current ID processor
!       TAG_SKN : integer ; dimension=NUMSKW+1
!                 tag array --> tag the i SKEW if a SPRING uses it
!                 tag array=0 --> the SKEW is not used by a SPRING
!                 tag array=1 --> the SKEW is used by one SPRING
!                 tag array>1 --> the SKEW is used by several SPRING
!       ISKWP : integer ; dimension=NUMSKW+1
!               gives the ID processir of the current i SKEW
!               ISKWP < 0 --> the SKEW is local on a processor
!               and we don't need to communicate the data
!               ISKWP > 0 --> the SKEW is global and the data must be
!       ISKWP_L : integer ; dimension=NUMSKW+1
!                 index of SKEW on the current processor, used in the engine
!       MULTIPLE_SKEW : SKEW_TYPE ; dimension=NUMSKW+1
!                       MULTIPLE_SKEW(I)%PLIST(:) is a list of processor
!                       where the SKEW is stuck
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,NN
        INTEGER :: SIZE_SKEW,ISKEW
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
!$COMMENT
!       SPLIT_SKEW description
!       SPLIT_SKEW initializes the SKEW index ISKWP_L used in the engine
!
!       SPLIT_SKEW organization :
!       - loop over the NUMSKW and check if the SKEW is on the curent
!         processor
!$ENDCOMMENT

        NN = 0
        DO I=1,NUMSKW
                SIZE_SKEW = TAG_SKN(I+1)
                IF(SIZE_SKEW>1) THEN
                        ISKEW = 0
                        DO J=1,SIZE_SKEW
                                IF(MULTIPLE_SKEW(I+1)%PLIST(J)==CURR_PROC) ISKEW = ISKEW + 1
                        ENDDO
                        IF(ISKEW>0) THEN
                                NN = NN + 1
                                ISKWP_L( NN ) = I
                        ENDIF
                ELSE
                        IF(ABS(ISKWP(I+1))==CURR_PROC) THEN
                                NN = NN + 1
                                ISKWP_L( NN ) = I
                        ENDIF
                ENDIF
        ENDDO

        RETURN
        END SUBROUTINE SPLIT_SKEW












Chd|====================================================================
Chd|  SPLIT_SKEW_save               source/restart/ddsplit/split_skew.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SKEW_MOD                      share/modules1/skew_mod.F     
Chd|====================================================================
        SUBROUTINE SPLIT_SKEW_save(CURR_PROC,ISKWP_L,ISKWP,TAG_SKN,MULTIPLE_SKEW,NSKWP)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: CURR_PROC
        INTEGER, DIMENSION(NUMSKW+1), INTENT(IN) :: TAG_SKN
        INTEGER, DIMENSION(NUMSKW+1), INTENT(IN) :: ISKWP
        INTEGER, DIMENSION(NUMSKW+1), INTENT(INOUT) :: ISKWP_L
        TYPE(SKEW_TYPE), DIMENSION(NUMSKW+1), INTENT(IN) :: MULTIPLE_SKEW


        integer, dimension(*) :: NSKWP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,NN
        INTEGER :: SIZE_SKEW,ISKEW
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
        NN = 0
        DO I=1,NUMSKW
                SIZE_SKEW = TAG_SKN(I+1)
                IF(SIZE_SKEW>1) THEN
                        ISKEW = 0
                        DO J=1,SIZE_SKEW
                                IF(MULTIPLE_SKEW(I+1)%PLIST(J)==CURR_PROC) ISKEW = ISKEW + 1
                        ENDDO
                        IF(ISKEW>0) THEN
                                NN = NN + 1
                                ISKWP_L( NN ) = I
                        ENDIF
                ELSE
                        IF(ABS(ISKWP(I+1))==CURR_PROC) THEN
                                NN = NN + 1
                                ISKWP_L( NN ) = I
                        ENDIF
                ENDIF
        ENDDO

        j = 0
        do i =1,NUMSKW+1
                if(ABS(ISKWP(I))==CURR_PROC) j = j + 1
        enddo

        if(j/=nn) then
        print*,' '
        print*,' *********************** '
        print*,' CURR PROC=',CURR_PROC
        DO i =1,nn
                print*,ISKWP_L( i)
        enddo
        print*,' '
        do i =1,NUMSKW
                if(ABS(ISKWP(I+1))==CURR_PROC) print*,i+1
        enddo
        print*,' *********************** '
        endif

        IF(NN/=NSKWP(CURR_PROC)) then
                print*,CURR_PROC,'pb sur taille nskwp !',nn,NSKWP(CURR_PROC)
        endif


        RETURN
        END SUBROUTINE SPLIT_SKEW_save
