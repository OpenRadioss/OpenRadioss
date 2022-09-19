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
Chd|  ASS2SORT                      source/assembly/ass2sort.F    
Chd|-- called by -----------
Chd|        ASSPAR2                       source/assembly/asspar2.F     
Chd|        ASSPAR4                       source/assembly/asspar4.F     
Chd|        I17LLL_ASS                    source/interfaces/int17/i17for3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ASS2SORT(FSKYI,JJ1,JJ2,FSKYT,NFSK)
C  FSKYI : IN/OUT : TABLEAU DE FORCES A TRIER EN ENTREE,
C                   TABLEAU DE FORCES TRIE EN SORTIE
C  JJ1   : IN     : INDICE DE DEBUT DANS FSKYI
C  JJ2   : IN     : INDICE DE FIN DANS FSKYI
C  FSKYT : IN/OUT : TABLEAU TEMPORAIRE
C  NFSK : IN      : Dimension de FSKYI
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JJ1, JJ2, NFSK
C     REAL
      my_real
     .   FSKYI(LSKYI,NFSK), FSKYT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, L, ERROR, PERM(JJ2-JJ1+1)
C-----------------------------------------------   
      DO L = 1, NFSK
        DO K = JJ1, JJ2
          FSKYT(K-JJ1+1) = FSKYI(K,L)
        ENDDO
        CALL STLSORT(JJ2-JJ1+1,FSKYT)
        DO K = JJ1, JJ2
          FSKYI(K,L) = FSKYT(JJ2-K+1)
        ENDDO
      ENDDO

C
      RETURN
      END
Chd|====================================================================
Chd|  ASS2SORT_PXFEM                source/assembly/ass2sort.F    
Chd|-- called by -----------
Chd|        ASSPAR4                       source/assembly/asspar4.F     
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ASS2SORT_PXFEM(FSKYI,JJ1,JJ2,FSKYT,NFSK)
C  FSKYI : IN/OUT : TABLEAU DE FORCES A TRIER EN ENTREE,
C                   TABLEAU DE FORCES TRIE EN SORTIE
C  JJ1   : IN     : INDICE DE DEBUT DANS FSKYI
C  JJ2   : IN     : INDICE DE FIN DANS FSKYI
C  FSKYT : IN/OUT : TABLEAU TEMPORAIRE
C  NFSK : IN      : Dimension de FSKYI
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JJ1, JJ2, NFSK
C     REAL
      my_real
     .   FSKYI(LSKYI,NFSK), FSKYT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, L, ERROR, PERM(JJ2-JJ1+1),
     .  IDX1(JJ2-JJ1+1),IDX2(JJ2-JJ1+1),N1,N2,IPLY,
     .  IPLY1,KK
C-----------------------------------------------   
      N1 = 1
      N2 = 0
      KK = JJ1
      DO WHILE(FSKYI(KK,5)== ZERO .AND. KK <= JJ2)
       KK = KK + 1
      ENDDO
      IPLY = INT(FSKYI(KK,5))
      IDX1(1) = KK
      IF(IPLY == 0) N1 = 0
C      
      IF(IPLY /=  0) THEN
          DO K = JJ1+1, JJ2
            IPLY1 = INT(FSKYI(K,5))
            IF(IPLY1 == IPLY) THEN
              N1 = N1 + 1
              IDX1(N1) = K
            ELSEIF(IPLY1 /= 0) THEN
              N2 = N2 + 1
              IDX2(N2) = K
            ENDIF
          ENDDO
      ENDIF
C          
      IF(N1 > 0) THEN
        DO L = 1, NFSK-1
          DO K = 1, N1
             KK = IDX1(K)
             FSKYT(K) = FSKYI(KK,L)
          ENDDO
          CALL STLSORT(N1, FSKYT)
          DO K = 1, N1
             KK = IDX1(K)
             FSKYI(KK,L) = FSKYT(N1-K+1)
          ENDDO
        ENDDO
       ENDIF
C         
       IF(N2 > 0) THEN
          DO L = 1, NFSK-1
           DO K = 1, N2
             KK = IDX2(K)
             FSKYT(K) = FSKYI(KK,L)
           ENDDO
           CALL STLSORT(N2, FSKYT)
           DO K = 1, N2
             KK = IDX2(K)
             FSKYI(KK,L) = FSKYT(N2-K+1)
           ENDDO
         ENDDO
        ENDIF
C
      RETURN
      END
