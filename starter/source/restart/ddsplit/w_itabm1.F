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
C
Chd|====================================================================
Chd|  W_ITABM1                      source/restart/ddsplit/w_itabm1.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|====================================================================
      SUBROUTINE W_ITABM1(ITABM1,ITABM2,NODGLOB,NODLOCAL,NUMNOD_L,
     2                   ITAB, LEN_IA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITABM1(*), ITABM2(*), NODGLOB(*),  NODLOCAL(*),
     .        NUMNOD_L, LEN_IA,ITAB(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NI,WORK(70000),ND
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITMPM1,ITMPM2,INDX
      CHARACTER MESS*40
      DATA MESS/'TOTO '/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C-----------------------------------------------
!     allocate 1d array
      ALLOCATE( ITMPM1(NUMNOD_L),ITMPM2(NUMNOD_L) )
      ALLOCATE( INDX(2*NUMNOD_L) )
! ---------------------------------
      DO I = 1, NUMNOD_L
        ITMPM1(I) = ITAB(NODGLOB(I))
        INDX(I) = I
      ENDDO
C
C Retris ordre croissant itabm1 (num user)
C
      CALL MY_ORDERS(0,WORK,ITMPM1,INDX,NUMNOD_L,1)
      DO I = 1, NUMNOD_L
        ITMPM2(I)= ITMPM1(INDX(I))
      ENDDO
C Ecriture ITABM1 num user tries ordre croissant
      CALL WRITE_I_C(ITMPM2,NUMNOD_L)
      LEN_IA = LEN_IA + NUMNOD_L
C
      DO I = 1, NUMNOD_L
        NI = ITMPM2(I)
        ND = USR2SYS(NI,ITABM1,MESS,0)
        ITMPM1(I) = NODLOCAL(ND)
      END DO

C Ecriture ITABM2 (num systemes)
      CALL WRITE_I_C(ITMPM1,NUMNOD_L)
      LEN_IA = LEN_IA + NUMNOD_L
C
! ---------------------------------
!     deallocate 1d array
      DEALLOCATE( ITMPM1,ITMPM2 )
      DEALLOCATE( INDX )
! ---------------------------------
      RETURN
      END
