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
Chd|  INTBUF_FRIC_COPY              source/interfaces/interf1/intbuf_fric_copy.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INTBUF_FRIC_COPY(
     .          TABCOUPLEPARTS_FRIC_TMP  ,TABCOEF_FRIC_TMP  ,
     .          TABPARTS_FRIC_TMP,NSETINIT,IFRICORTH_TMP,INTBUF_FRIC_TAB)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TABCOUPLEPARTS_FRIC_TMP(NINTERFRIC,*),TABPARTS_FRIC_TMP(NINTERFRIC,*),
     .        NSETINIT(NINTERFRIC)  ,IFRICORTH_TMP(NINTERFRIC,*) 
      my_real
     .   TABCOEF_FRIC_TMP(NINTERFRIC,*)

      TYPE(INTBUF_FRIC_STRUCT_), TARGET, DIMENSION(NINTERFRIC) ::  INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIF , NSET ,I ,J ,K ,N ,IP ,NSETT ,NPARTF ,IORTH ,MFROT ,LENC
      INTEGER, DIMENSION(:)   ,POINTER  :: TABCOUPLEPARTS_FRIC
      INTEGER, DIMENSION(:)   ,POINTER  :: TABPARTS_FRIC
      INTEGER, DIMENSION(:)   ,POINTER  :: ADPART_FRIC
      INTEGER, DIMENSION(:)   ,POINTER  :: IFRICORTH
      my_real, DIMENSION(:)   ,POINTER  :: TABCOEF_FRIC
C
C--------------------------------------------
C=======================================================================

       DO NIF = 1, NINTERFRIC
           TABCOUPLEPARTS_FRIC  => INTBUF_FRIC_TAB(NIF)%TABCOUPLEPARTS_FRIC
           TABCOEF_FRIC  => INTBUF_FRIC_TAB(NIF)%TABCOEF_FRIC 
           TABPARTS_FRIC  => INTBUF_FRIC_TAB(NIF)%TABPARTS_FRIC
           ADPART_FRIC   => INTBUF_FRIC_TAB(NIF)%ADPARTS_FRIC  
           NSET  = INTBUF_FRIC_TAB(NIF)%NSETPRTS
           NSETT  = NSETINIT(NIF)
           NPARTF = INTBUF_FRIC_TAB(NIF)%S_TABPARTS_FRIC 
           IORTH = INTBUF_FRIC_TAB(NIF)%IORTHFRIC
           IFRICORTH => INTBUF_FRIC_TAB(NIF)%IFRICORTH
           MFROT = INTBUF_FRIC_TAB(NIF)%FRICMOD

           IF(MFROT ==0 ) THEN   
              LENC =2  
           ELSE
              LENC = 8
           ENDIF 

C----------Copying default values of friction in the top of the Tab TABCOEF_FRIC_TMP
           DO J=1,LENC
              TABCOEF_FRIC(J) =TABCOEF_FRIC_TMP(NIF,J)
           ENDDO
C----------Copying parts numbers and coefficient in the new structure and ommiting duplicated couple of parts
           J = 1
           K = 0
           IF(IORTH == 0) THEN
              DO I=1,NSETT
                 IF( TABCOUPLEPARTS_FRIC_TMP(NIF,J).NE. 0) THEN
                    K = K +1
C
                    TABCOUPLEPARTS_FRIC(K) = TABCOUPLEPARTS_FRIC_TMP(NIF,J+1)
C
                    DO N=1,LENC
                       TABCOEF_FRIC(LENC*K+N) =TABCOEF_FRIC_TMP(NIF,I*8+N)
                    ENDDO
C
                    IFRICORTH(K) = IFRICORTH_TMP (NIF,I)
                  ENDIF
                  J = J+2
                ENDDO
            ELSEIF(IORTH==1) THEN
              DO I=1,NSETT
                 IF( TABCOUPLEPARTS_FRIC_TMP(NIF,J).NE. 0) THEN
                    K = K +1
C
                    TABCOUPLEPARTS_FRIC(K) = TABCOUPLEPARTS_FRIC_TMP(NIF,J+1)
C
                    DO N=1,LENC
                       TABCOEF_FRIC(LENC+2*LENC*(K-1)+N) =TABCOEF_FRIC_TMP(NIF,8+16*(I-1)+N)
                       TABCOEF_FRIC(2*K*LENC+N) =TABCOEF_FRIC_TMP(NIF,16*I+N)
                    ENDDO
                    IFRICORTH(K) = IFRICORTH_TMP (NIF,I)
                  ENDIF
C

                  J = J+2
                ENDDO

            ENDIF     

C----------Copying parts tab tagged by friction model
           DO I=1,NPARTF
                TABPARTS_FRIC(I) = TABPARTS_FRIC_TMP(NIF,I)
           ENDDO    

C----------Computation of the adress of each part in the tabs TABPARTSFRIC TABCOEF_FRIC_TMP
           ADPART_FRIC(1) = 1
           ADPART_FRIC(2:NPARTF+1) = 0
           DO I=1,NPARTF 
              K = 0
              J = 1 
              DO N=1,NSETT
                IF( TABCOUPLEPARTS_FRIC_TMP(NIF,J).NE. 0) THEN
                 K = K + 1                  
                 IF(TABCOUPLEPARTS_FRIC_TMP(NIF,J) == TABPARTS_FRIC(I)) THEN  
                    ADPART_FRIC(I+1) = ADPART_FRIC(I+1) + 1
                 ENDIF  
                ENDIF
                J = J +2
              ENDDO
           ENDDO
           DO I=1,NPARTF 
               K = I +1
              ADPART_FRIC(K) =ADPART_FRIC(K) +ADPART_FRIC(I)
           ENDDO

        ENDDO
C
       RETURN

      END SUBROUTINE INTBUF_FRIC_COPY


