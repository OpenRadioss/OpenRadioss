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
Chd|  THQUAD_COUNT                  source/output/th/thquad_count.F
Chd|-- called by -----------
Chd|        INIT_TH                       source/output/th/init_th.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE THQUAD_COUNT(NTHGRP2 ,ITHGRP,WA_SIZE,INDEX_WA_QUAD,
     1                        IPARG,ITHBUF  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP),ITHBUF(SITHBUF)
      INTEGER, INTENT(inout) :: WA_SIZE,NTHGRP2
      INTEGER, DIMENSION(2*NTHGRP2+1), INTENT(inout) :: INDEX_WA_QUAD
      INTEGER, DIMENSION(NITHGR,*), INTENT(in) :: ITHGRP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, KRK, LL, I, J, K, L ,N, IH, IP, NG, MTE, NUVAR,
     .   NC1, NC2, NC3, NC4,  NEL, MTN1,KK(6),IJ,NPTR,NPTS,
     .   IR,IS
      LOGICAL :: BOOL
      INTEGER :: J_FIRST,NITER,IADB,NN,IADV,NVAR,ITYP,IJK
      INTEGER, DIMENSION(NTHGRP2) :: INDEX_QUAD

C--------------------------------------------   

        IJK = 0
        WA_SIZE = 0
        INDEX_QUAD(1:NTHGRP2) = 0
        IJK = 0
        DO NITER=1,NTHGRP2
            ITYP=ITHGRP(2,NITER)
            NN  =ITHGRP(4,NITER)
            IADB =ITHGRP(5,NITER)
            NVAR=ITHGRP(6,NITER)
            IADV=ITHGRP(7,NITER)
            II=0
            IF(ITYP==2.OR.ITYP==117)THEN
!   -----------------------------
      NUVAR = 0
      II=0
      IH=IADB
      IF(ITYP == 117) ITYP = 7

C decalage IH
      DO WHILE((ITHBUF(IH+NN)/=ISPMD).AND.(IH<IADB+NN))
          IH = IH + 1
        ENDDO
C----
        IF (IH>=IADB+NN) GOTO 666
C----
c
      DO NG=1,NGROUP
       ITY=IPARG(5,NG)
       IF (ITY == ITYP) THEN
         NFT     = IPARG(3,NG)
         MTE = IPARG(1,NG)
         NEL = IPARG(2,NG)
         IF(MTE /= 13) THEN
C
          DO I=1,NEL
            N=I+NFT
            K=ITHBUF(IH)
            IP=ITHBUF(IH+NN)
C
            IF (K==N)THEN
                IH=IH+1
                IF (IMACH==3) THEN
C recherche du ii correct
                    II = ((IH-1) - IADB)*NVAR
                    DO WHILE((ITHBUF(IH+NN)/=ISPMD).AND.(IH<IADB+NN))
                        IH = IH + 1
                    ENDDO
                ENDIF
                IF (IH > IADB+NN) GOTO 666
                WA_SIZE = WA_SIZE + NVAR + 1
            ENDIF
          ENDDO
c --------------
         ENDIF  ! mte /= 13
        ENDIF
      ENDDO ! groupe
!   -----------------------------
            ENDIF
 666    continue
            INDEX_QUAD(NITER) = WA_SIZE
        ENDDO


        J_FIRST = 0
        BOOL = .TRUE.
        DO I=1,NTHGRP2
            IF(BOOL.EQV..TRUE.) THEN
                IF( INDEX_QUAD(I)/=0 ) THEN
                    BOOL = .FALSE.
                    J_FIRST = I
                ENDIF
            ENDIF
        ENDDO

        J = 0
        IF(J_FIRST>0) THEN
            J=J+1
            INDEX_WA_QUAD(J) = INDEX_QUAD(J_FIRST)
            J=J+1
            INDEX_WA_QUAD(J) = J_FIRST
            DO I=J_FIRST+1,NTHGRP2
                IF( INDEX_QUAD(I)-INDEX_QUAD(I-1)>0 ) THEN
                    J=J+1
                    INDEX_WA_QUAD(J) = INDEX_QUAD(I)
                    J=J+1
                    INDEX_WA_QUAD(J) = I
                ENDIF
            ENDDO
        ENDIF
        INDEX_WA_QUAD(2*NTHGRP2+1) = J  !   number of non-zero index
C-----------
      RETURN
      END SUBROUTINE THQUAD_COUNT
