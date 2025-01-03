Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    imp_setb      ../engine/source/implicit/imp_setb.F
      !||--- called by ------------------------------------------------------
      !||    d_to_u        ../engine/source/implicit/produt_v.F
      !||    imp_chkm      ../engine/source/implicit/imp_solv.F
      !||    imp_dykv      ../engine/source/implicit/imp_dyna.F
      !||    imp_dykv0     ../engine/source/implicit/imp_dyna.F
      !||    imp_solv      ../engine/source/implicit/imp_solv.F
      !||    produt_u      ../engine/source/implicit/produt_v.F
      !||    produt_u2     ../engine/source/implicit/produt_v.F
      !||    produt_uh     ../engine/source/implicit/produt_v.F
      !||    produt_uh2    ../engine/source/implicit/produt_v.F
      !||    produt_uhp    ../engine/source/implicit/produt_v.F
      !||    produt_uhp2   ../engine/source/implicit/produt_v.F
      !||    upd_rhs       ../engine/source/implicit/upd_glob_k.F
      !||====================================================================
      SUBROUTINE IMP_SETB(F     ,M      ,IDDL   ,NDOF  ,B  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IDDL(*),NDOF(*)
C     REAL
      my_real
     . F(3,*),M(3,*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ND,ID,JR
C------------------------------------------
       DO I = 1,NUMNOD
        ND = IDDL(I)
        DO J =1,NDOF(I)
         ID = ND + J
         IF (J>3) THEN
          JR=J-3
          B(ID) = M(JR,I)
         ELSE
          B(ID) = F(J,I)
         ENDIF
        ENDDO
       ENDDO
C--------------------------------------------
      RETURN
      END
      !||====================================================================
      !||    imp_setbp   ../engine/source/implicit/imp_setb.F
      !||--- called by ------------------------------------------------------
      !||    ext_rhs     ../engine/source/implicit/upd_glob_k.F
      !||    get_fext    ../engine/source/implicit/imp_solv.F
      !||    rer02       ../engine/source/implicit/upd_glob_k.F
      !||    upd_rhs     ../engine/source/implicit/upd_glob_k.F
      !||====================================================================
      SUBROUTINE IMP_SETBP(F     ,M      ,IDDL   ,NDOF   ,IKC   ,
     .                    INLOC ,B      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IDDL(*),NDOF(*),IKC(*),INLOC(*)
C     REAL
      my_real
     . F(3,*),M(3,*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,K,ND,ID,JR,NKC
C------------------------------------------
      NKC=0
      DO N = 1,NUMNOD
       I=INLOC(N)
       DO J=1,NDOF(I)
        ND = IDDL(I)+J
        ID = ND-NKC
        IF (IKC(ND)<1) THEN
          IF (J<=3) THEN
           B(ID)=B(ID)+F(J,I)
          ELSE 
           B(ID)=B(ID)+M(J-3,I)
          ENDIF
        ELSE 
          NKC=NKC+1
        ENDIF
       ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
      !||====================================================================
      !||    imp_setba   ../engine/source/implicit/imp_setb.F
      !||--- called by ------------------------------------------------------
      !||    ext_rhs     ../engine/source/implicit/upd_glob_k.F
      !||    get_fext    ../engine/source/implicit/imp_solv.F
      !||    imp_intfr   ../engine/source/implicit/imp_solv.F
      !||    upd_rhs     ../engine/source/implicit/upd_glob_k.F
      !||====================================================================
      SUBROUTINE IMP_SETBA(F     ,M      ,IDDL   ,NDOF  ,B     ,
     1                     IFLAG )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IDDL(*),NDOF(*),IFLAG
C     REAL
      my_real
     . F(3,*),M(3,*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ND,ID,JR
C------------------------------------------
      IF (IFLAG==1) THEN
       DO I = 1,NUMNOD
        ND = IDDL(I)
        DO J =1,NDOF(I)
         ID = ND + J
         IF (J>3) THEN
          JR=J-3
          B(ID) = B(ID)+M(JR,I)
         ELSE
          B(ID) = B(ID)+F(J,I)
         ENDIF
        ENDDO
       ENDDO
      ELSE
       DO I = 1,NUMNOD
        ND = IDDL(I)
        DO J =1,NDOF(I)
         ID = ND + J
         IF (J>3) THEN
          JR=J-3
          B(ID) = B(ID)-M(JR,I)
         ELSE
          B(ID) = B(ID)-F(J,I)
         ENDIF
        ENDDO
       ENDDO
      ENDIF 
C--------------------------------------------
      RETURN
      END
