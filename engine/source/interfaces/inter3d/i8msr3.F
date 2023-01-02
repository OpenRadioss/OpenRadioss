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
Chd|  I8MSR3                        source/interfaces/inter3d/i8msr3.F
Chd|-- called by -----------
Chd|        INTVO8                        source/interfaces/inter3d/intvo8.F
Chd|-- calls ---------------
Chd|        I8_NEAREST_SEG                source/interfaces/inter3d/i8_nearest_seg.F
Chd|        INT8_MOD                      ../common_source/modules/int8_mod.F
Chd|====================================================================
      SUBROUTINE I8MSR3(X,IRECT,LMSR,MSR,NSV,ILOC,IRTL,NSEG,XFACE, 
     .           NBSECNDS,ITAB,HAS_MOVED,TAB_RMAX,TAB_RMAX_UID)
C-----------------------------------------------
C   I n f o r m a t i o n s            
C-----------------------------------------------
C   This routine computes the local value of IRTL 
C   on each SPMD dommain
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INT8_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)  :: IRECT(4,*), LMSR(*), MSR(*), NSV(*), ILOC(*), NSEG(*)
      INTEGER, INTENT(INOUT) :: IRTL(*)
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  XFACE
      my_real, INTENT(IN)  ::  X(3,*)
	  
      INTEGER TAB_RMAX_UID(4,*),HAS_MOVED(*)
      INTEGER NBSECNDS,ITAB(*)
      my_real TAB_RMAX(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, JL, L, JJJ, JJ, J1, J2, LL1, LL2, LL, LG, IG, JG, M, N
      INTEGER KM1(4), KN1(4), LSEG, LSEG_OLD, LSEG_NEW, IFLAG
      INTEGER FACE_GLOB_ID(4)
      my_real BMIN, BMAX
      DATA KM1/2,3,4,1/
      DATA KN1/4,1,2,3/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER IS_SUP_FACE_ID
      EXTERNAL IS_SUP_FACE_ID
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----A----+----B----+----C--

      ! At the end of this routine 
      !HAS_MOVED(i) == 1 : the main node of the ith secnd may have
      !changed (i.e. it has changed locally, or was not on this processor)
      !HAS_MOVED(i) == 0 : the ith secnd node keep the same main

      HAS_MOVED(1:NBSECNDS) = 1
      TAB_RMAX(1:NBSECNDS)  = ZERO
      TAB_RMAX_UID(1:4,1:NBSECNDS) = 0 

      DO I=LFT,LLT
        IL=I+NFT
        IG=NSV(IL)
        JL=ILOC(IL)
        IF(JL <= 0) CYCLE
        JG=MSR(JL)
        L=IRTL(IL)

        IF(XFACE(I)==ZERO)THEN
         IRTL(IL)=MAX(L,1)
        ELSE
         BMAX=-EP30
         LSEG_OLD=L
         LSEG_NEW=0
C
         IF(L<=0) GOTO 100
       ! IF this secnd has a main face from the previous cycle, 
       ! this one is checked first                                      

         FACE_GLOB_ID(1)  = ITAB(MSR(IRECT(1,L)))
         FACE_GLOB_ID(2)  = ITAB(MSR(IRECT(2,L)))
         FACE_GLOB_ID(3)  = ITAB(MSR(IRECT(3,L)))
         FACE_GLOB_ID(4)  = ITAB(MSR(IRECT(4,L)))

         DO JJJ=1,4
           JJ=JJJ
           IF(IRECT(JJ,L)==JL) EXIT
         ENDDO
         J1=KM1(JJ)
         J2=KN1(JJ)
         IF(JJ==3.AND.IRECT(3,L)==IRECT(4,L)) J1=1
         M=MSR(IRECT(J1,L))
         N=MSR(IRECT(J2,L))
         CALL I8_NEAREST_SEG(X, IG, JG, M, N, BMIN)
         IFLAG=IS_SUP_FACE_ID(TAB_RMAX_UID(1,IL),FACE_GLOB_ID)

         IF(BMIN > BMAX .OR. (BMIN == BMAX .AND. IFLAG == 1)) THEN
            LSEG_NEW=LSEG_OLD
            BMAX=BMIN
            TAB_RMAX(IL) = BMAX
            TAB_RMAX_UID(1,IL) = FACE_GLOB_ID(1)
            TAB_RMAX_UID(2,IL) = FACE_GLOB_ID(2)
            TAB_RMAX_UID(3,IL) = FACE_GLOB_ID(3)
            TAB_RMAX_UID(4,IL) = FACE_GLOB_ID(4)
         ENDIF

         IF(BMIN >= ZERO) THEN
           HAS_MOVED(IL) = 0
           GO TO 200
         ENDIF
C
 100     CONTINUE
         LL1=NSEG(JL)
         LL2=NSEG(JL+1)-1
         DO LL=LL1,LL2
           LG=LMSR(LL)
           LSEG=LG
           IF(L==LG) CYCLE
           DO JJJ=1,4
             JJ=JJJ
             IF(IRECT(JJ,LG)==JL) EXIT
           ENDDO
           J1=KM1(JJ)
           J2=KN1(JJ)
           IF(JJ==3.AND.IRECT(3,LG)==IRECT(4,LG)) J1=1
           FACE_GLOB_ID(1)  =ITAB(MSR(IRECT(1,LG)))
           FACE_GLOB_ID(2)  =ITAB(MSR(IRECT(2,LG)))
           FACE_GLOB_ID(3)  =ITAB(MSR(IRECT(3,LG)))
           FACE_GLOB_ID(4)  =ITAB(MSR(IRECT(4,LG)))
           M=MSR(IRECT(J1,LG))
           N=MSR(IRECT(J2,LG))
           CALL I8_NEAREST_SEG(X, IG, JG, M, N, BMIN)
           IFLAG=IS_SUP_FACE_ID(TAB_RMAX_UID(1,IL),FACE_GLOB_ID)
           IF(BMIN > BMAX .OR. (BMIN == BMAX .AND. IFLAG == 1)) THEN
              LSEG_NEW=LSEG
              BMAX=BMIN
              TAB_RMAX(IL) = BMAX
              TAB_RMAX_UID(1,IL) = FACE_GLOB_ID(1)
              TAB_RMAX_UID(2,IL) = FACE_GLOB_ID(2)
              TAB_RMAX_UID(3,IL) = FACE_GLOB_ID(3)
              TAB_RMAX_UID(4,IL) = FACE_GLOB_ID(4)
           ENDIF

           IF(BMIN < ZERO) CYCLE
           IRTL(IL)=LSEG_NEW
           HAS_MOVED(IL) = 1
           GO TO 200
         ENDDO  !LL=LL1,LL2
C
         IRTL(IL)=LSEG_NEW
		 HAS_MOVED(IL)=1
 200     CONTINUE

         TAB_RMAX(IL) = BMAX 
         TAB_RMAX_UID(1,IL) = ITAB(MSR(IRECT(1,LSEG_NEW)))
         TAB_RMAX_UID(2,IL) = ITAB(MSR(IRECT(2,LSEG_NEW)))
         TAB_RMAX_UID(3,IL) = ITAB(MSR(IRECT(3,LSEG_NEW)))
         TAB_RMAX_UID(4,IL) = ITAB(MSR(IRECT(4,LSEG_NEW)))
		 
        ENDIF !XFACE
      ENDDO !I=LFT,LLT

      RETURN
      END

      FUNCTION IS_SUP_FACE_ID(A,B)
C-----------------------------------------------
C   I n f o r m a t i o n s            
C-----------------------------------------------
C Comparison of the User ID of two faces.
C This UID is defined by four User ID of the nodes
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER :: A(4),B(4)
        INTEGER :: IS_SUP_FACE_ID
        IS_SUP_FACE_ID = 0       
        IF( A(1) > B(1) .OR.
     .  A(1) == B(1) .AND. ( A(2) > B(2) .OR.
     .  A(2) == B(2) .AND. ( A(3) > B(3) .OR.
     .  A(3) == B(3) .AND. A(4) > B(4)))) THEN
          IS_SUP_FACE_ID = 1
        ENDIF
        RETURN
      END FUNCTION
