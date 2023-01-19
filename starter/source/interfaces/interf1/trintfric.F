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
Chd|  TRIINTFRIC                    source/interfaces/interf1/trintfric.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE TRIINTFRIC(
     .          TABCOUPLEPARTS_FRIC_TMP  ,TABCOEF_FRIC_TMP  ,INTBUF_FRIC_TAB   ,
     .          TABPARTS_FRIC_TMP,NSETFRICTOT,NSETINIT,IORTHFRICMAX,IFRICORTH_TMP,
     .          NSETMAX          )

C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUF_FRIC_MOD                     
C-----------------------------------------------      
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------

#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSETFRICTOT,IORTHFRICMAX,NSETMAX
      INTEGER TABCOUPLEPARTS_FRIC_TMP(NINTERFRIC,*),NSETINIT(NINTERFRIC),
     .        TABPARTS_FRIC_TMP(NINTERFRIC,*),IFRICORTH_TMP(NINTERFRIC,*)

      my_real
     .   TABCOEF_FRIC_TMP(NINTERFRIC,*)

      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIF , NSET ,I ,J ,K ,STAT ,NSETT ,IORTH ,
     . WORK(70000)
      INTEGER, DIMENSION(:),   ALLOCATABLE :: INDEX ,ITRI2 ,INDEX2
      INTEGER, DIMENSION(:,:),   ALLOCATABLE :: ITRI
      INTEGER, DIMENSION(:),   ALLOCATABLE :: TRIFRIORTH
      my_real, DIMENSION(:,:),   ALLOCATABLE :: TRICOEF
C
C--------------------------------------------
C=======================================================================


      ALLOCATE(INDEX(2*NSETMAX), STAT=Stat)
      ALLOCATE(ITRI(2,NSETMAX), STAT=Stat)
      IF(IORTHFRICMAX == 0 ) THEN
        ALLOCATE(TRICOEF(NSETMAX,8), STAT=Stat)
      ELSE
        ALLOCATE(TRICOEF(NSETMAX,16), STAT=Stat)
      ENDIF
      ALLOCATE(ITRI2(2*NSETMAX), STAT=Stat)
      ALLOCATE(INDEX2(4*NSETMAX), STAT=Stat)
      ALLOCATE(TRIFRIORTH(NSETMAX), STAT=Stat)    

C
       DO NIF=1,NINTERFRIC
          NSET  = INTBUF_FRIC_TAB(NIF)%NSETPRTS
          IORTH = INTBUF_FRIC_TAB(NIF)%IORTHFRIC
          J = 1
C------Copy-----------------------------
          DO I=1,NSET
             ITRI(1,I) = TABCOUPLEPARTS_FRIC_TMP(NIF,J)
             ITRI(2,I) = TABCOUPLEPARTS_FRIC_TMP(NIF,J+1)
             INDEX(I)=I
             J = J+2
          ENDDO
          DO I=1,NSET
             TRIFRIORTH(I) = IFRICORTH_TMP(NIF,I)
          ENDDO
          IF(IORTH == 0 ) THEN
             DO I=1,NSET
                DO J=1,8
                   TRICOEF(I,J) = TABCOEF_FRIC_TMP(NIF,I*8+J)
                ENDDO
             ENDDO
          ELSEIF(IORTH == 1 ) THEN
             DO I=1,NSET
                DO J=1,16
                   TRICOEF(I,J) = TABCOEF_FRIC_TMP(NIF,8+16*(I-1)+J)
                ENDDO
             ENDDO     
         ENDIF
C----------------------
          CALL MY_ORDERS( 0, WORK, ITRI, INDEX, NSET , 2)
C
           J = 1 
          DO I=1,NSET
             TABCOUPLEPARTS_FRIC_TMP(NIF,J)= ITRI(1,INDEX(I))
             TABCOUPLEPARTS_FRIC_TMP(NIF,J+1)= ITRI(2,INDEX(I))
             J = J+2
          ENDDO 

C------Delete duplicated parts pairs-----------------------------
          NSETINIT(NIF) = NSET
          J = 1
          K = NSET
          DO I=1,NSET-1
             IF(TABCOUPLEPARTS_FRIC_TMP(NIF,J)==TABCOUPLEPARTS_FRIC_TMP(NIF,J+2).AND. 
     .          TABCOUPLEPARTS_FRIC_TMP(NIF,J+1)==TABCOUPLEPARTS_FRIC_TMP(NIF,J+3)  ) THEN
                TABCOUPLEPARTS_FRIC_TMP(NIF,J) = 0
                TABCOUPLEPARTS_FRIC_TMP(NIF,J+1) = 0
                K = K - 1
             ENDIF  
            J = J + 2
          ENDDO 
          INTBUF_FRIC_TAB(NIF)%NSETPRTS = K

C---------Tabs of tagged parts--------------------------------- 
          K = 0 
          J = 1
          DO I =1,NSET
             IF(TABCOUPLEPARTS_FRIC_TMP(NIF,J) /= 0 ) THEN
                K = K +1
                TABPARTS_FRIC_TMP(NIF,K) = TABCOUPLEPARTS_FRIC_TMP(NIF,J)
             ENDIF
c
             J = J +1
             IF(TABCOUPLEPARTS_FRIC_TMP(NIF,J) /= 0 ) THEN
                K = K +1
                TABPARTS_FRIC_TMP(NIF,K) = TABCOUPLEPARTS_FRIC_TMP(NIF,J)
             ENDIF
             J = J +1
          ENDDO

          NSETT = K

          DO I =1,NSETT
             ITRI2(I) = TABPARTS_FRIC_TMP(NIF,I)
             INDEX2(I)=I
          ENDDO
          CALL MY_ORDERS( 0, WORK, ITRI2, INDEX2, NSETT , 1)

          DO I =1,NSETT
             TABPARTS_FRIC_TMP(NIF,I) = ITRI2(INDEX2(I)) 
          ENDDO

          K = 1
          DO I =1,NSETT
             IF(TABPARTS_FRIC_TMP(NIF,K) /= TABPARTS_FRIC_TMP(NIF,I)) THEN
                K = K +1
                TABPARTS_FRIC_TMP(NIF,K) = TABPARTS_FRIC_TMP(NIF,I)  
              ENDIF
          ENDDO
          IF(NSETT > 0) THEN
             INTBUF_FRIC_TAB(NIF)%S_TABPARTS_FRIC = K
          ELSE
             INTBUF_FRIC_TAB(NIF)%S_TABPARTS_FRIC = 0
          ENDIF
C------Coefs-----------------------------
          DO I=1,NSET
            IFRICORTH_TMP(NIF,I) = TRIFRIORTH(INDEX(I))
          ENDDO
  
          IF(IORTH == 0 ) THEN
            DO I=1,NSET
               DO J=1,8
                  TABCOEF_FRIC_TMP(NIF,I*8+J) = TRICOEF(INDEX(I),J)
               ENDDO
             ENDDO
          ELSEIF(IORTH == 1) THEN
            DO I=1,NSET
               DO J=1,16
                  TABCOEF_FRIC_TMP(NIF,8+(I-1)*16+J) = TRICOEF(INDEX(I),J)
               ENDDO
             ENDDO
          ENDIF
       ENDDO
C
       DEALLOCATE(INDEX)
       DEALLOCATE(ITRI)
       DEALLOCATE(TRICOEF)
       DEALLOCATE(ITRI2,INDEX2)
       DEALLOCATE(TRIFRIORTH)    
C
       RETURN
      END SUBROUTINE TRIINTFRIC


