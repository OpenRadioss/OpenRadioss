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
Chd|  INITNOISE                     source/general_controls/computation/initnoise.F
Chd|-- called by -----------
Chd|        NOISE                         source/general_controls/computation/noise.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INITNOISE(IN,NOIADD,IXS,IWA,LENGTH,IXQ)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scrnoi_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IN(*),IXS(NIXS,*),IWA(0:*),IXQ(NIXQ,*)
      INTEGER NOIADD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,LENGTH,K
C-----------------------------------------------
C
C ADRESSE DES NOEUDS SAUVES DS NOISE
C
      DO I=0,NUMNOD
       IWA(I)=0
      ENDDO
      DO I=1,NNOISE
       IWA(IN(I))=I                 ! IN(I)=0 si noeud non present en SPMD
       NOIADD(I)=0
      ENDDO
      NOIADD(NNOISE+1)=0
C
C STOCKE DANS NOIADD(K) LE NOMBRE d'ELEMENTS CONNECTES AU NOEUD K-1
C
      IF(N2D==0)THEN
        DO I=1,NUMELS
         DO J=2,9
          K=IWA(IXS(J,I))
          IF(K/=0)
     .       NOIADD(K+1)=NOIADD(K+1)+1
         ENDDO
        ENDDO
      ELSE
        DO I=1,NUMELQ
         DO J=2,5
          K=IWA(IXQ(J,I))
          IF(K/=0)
     .       NOIADD(K+1)=NOIADD(K+1)+1
         ENDDO
        ENDDO
      ENDIF      
C
C ADRESSE PAR NOEUD DANS LE TABLEAU DES ELEMENTS A SAUVER DANS NOEUDS
C
      NOIADD(1)=1
      DO I=2,NNOISE+1
       NOIADD(I)=NOIADD(I)+NOIADD(I-1)
      ENDDO
      LENGTH=NOIADD(NNOISE+1)-1
c-----------
      RETURN
      END

Chd|====================================================================
Chd|  INITNOISE2                    source/general_controls/computation/initnoise.F
Chd|-- called by -----------
Chd|        NOISE                         source/general_controls/computation/noise.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INITNOISE2(IN,ELNOI,ELG,NOIADD,IXS,IWA,IPARG,IXQ)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scrnoi_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IN(*),IXS(NIXS,*),IXQ(NIXQ,*),IWA(*),IPARG(NPARG,*),
     .   ELNOI(*),ELG(*),NOIADD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER , DIMENSION (:), ALLOCATABLE :: KN
      INTEGER I,J,LENGTH,K,II
      INTEGER NG,NEL,ITY,NB2,NFT
C-----------------------------------------------
      IF(NNOISE>0)THEN
        ALLOCATE(KN(NNOISE))
      ENDIF
      DO I=1,NNOISE
        KN(I)=0
      ENDDO
C
C REMPLISSAGE DE ELNOI PAR LES ADRESSES DANS ELBUF
C
      DO NG=1,NGROUP
        ITY   =IPARG(5,NG)
        IF(ITY==1)THEN
         NEL   =IPARG(2,NG)
         NB2   =IPARG(4,NG) + NEL
         NFT   =IPARG(3,NG)
         DO I=1,NEL
          II=I+NFT
          DO J=2,9
            K=IWA(IXS(J,II))
            IF(K /=0)THEN
              ELNOI(NOIADD(K)+KN(K))=I
              ELG(NOIADD(K)+KN(K))=NG
              KN(K)=KN(K)+1
            ENDIF
          ENDDO
         ENDDO
        ELSEIF(ITY==2)THEN
         NEL   =IPARG(2,NG)
         NB2   =IPARG(4,NG) + NEL
         NFT   =IPARG(3,NG)
         DO I=1,NEL
          II=I+NFT
          DO J=2,5
            K=IWA(IXQ(J,II))
            IF(K /=0)THEN
              ELNOI(NOIADD(K)+KN(K))=(I-1)*6
              ELG(NOIADD(K)+KN(K))=NG
              KN(K)=KN(K)+1
            ENDIF
          ENDDO
         ENDDO
        ENDIF
        
      ENDDO
C
      IF(NNOISE>0)THEN
        DEALLOCATE(KN)
      ENDIF
C-----------
      RETURN
      END






         
