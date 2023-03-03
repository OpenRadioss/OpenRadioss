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
Chd|  THPINIT                       source/output/th/thpinit.F    
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE THPINIT(
     1      ITHGRP,ITHBUF,IPARG  ,DD_IAD,IXRI,
     2      IFLAG ,NTHGRP2)
C----------------------------------------------
C     INITIALISATION DU BUFFER TH (PROC SPMD)
C----------------------------------------------
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
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITHGRP(NITHGR,*),ITHBUF(*),IPARG(NPARG,*),
     &        DD_IAD(NSPMD+1,*), IXRI(4,*),
     &        IFLAG, NTHGRP2
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     &        P,NT,NG,I,K,NNE,IAD,ITYP,IH,NFT,ITY,NEL,N1,N2
C-----------------------------------------------
      IF(IFLAG==0) THEN
C
C Initialisation de nft0 = nft
C
        DO NG = 1, NGROUP
          IPARG(31,NG) = IPARG(3,NG)
        ENDDO
      ENDIF
C
C Mise a jour de PROC dans ITHBUF en fonction de la domdec
C
      IF (IMACH==3.AND.NSPMD>1) THEN
C traitement de p1 a pmaxporc-1 (p0 par defaut)
        DO NT = 1, NTHGRP2
          ITYP=ITHGRP(2,NT)
          NNE =ITHGRP(4,NT)
          IAD =ITHGRP(5,NT)            
          IF((ITYP>=1.AND.ITYP<=7).OR.
     .        ITYP==50.OR.ITYP==51.OR.ITYP==100)THEN
            DO IH = 1, NNE
              K = ITHBUF(IAD-1+IH)
              DO NG = 1, NGROUP
                ITY = IPARG(5,NG)
                IF(ITY==ITYP) THEN
                  NEL = IPARG(2,NG)
                  NFT = IPARG(3,NG)
                  P   = IPARG(32,NG)
                  IF (K>NFT.AND.K<=NFT+NEL) THEN
                    ITHBUF(IAD+NNE-1+IH) = P
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
          ELSEIF (ITYP==0) THEN
c            DO IH = 1, NNE
c              K = ITHBUF(IAD-1+IH)
c              DO P = 1, NSPMD
c                IF(MOD(FRONT(K,P),10)==1) THEN
c                  ITHBUF(IAD+NNE-1+IH) = P-1
c                  GOTO 209
c                ENDIF
c              ENDDO
c 209          CONTINUE
c            ENDDO
          ELSEIF (ITYP==109) THEN
            DO IH = 1, NNE
              K = ITHBUF(IAD-1+IH)
              N1 = IXRI(2,K)
              N2 = IXRI(3,K)
              DO P = 1, NSPMD
                IF(NLOCAL(N1,P)==1.AND.
     .             NLOCAL(N2,P)==1) THEN
                  ITHBUF(IAD+NNE-1+IH) = P
                  GOTO 109
                ENDIF
              ENDDO
 109          CONTINUE
            ENDDO               
          ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END
