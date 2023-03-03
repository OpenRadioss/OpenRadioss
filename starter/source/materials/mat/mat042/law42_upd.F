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
Chd|  LAW42_UPD                     source/materials/mat/mat042/law42_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW42_UPD(IOUT,TITR    ,MAT_ID,UPARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle  :: TITR
      INTEGER MAT_ID,IOUT
      my_real 
     .         UPARAM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

c
      INTEGER NDATA,I,K,INDX
      my_real
     .   MU(5),AL(5),LAM_MIN,LAM_MAX,STRAIN_MIN,STRAIN_MAX,
     .   D11,D22,D12,LAM1,LAM2,LAM3,LAM12,INVD1,INVD2
      my_real , DIMENSION(:), ALLOCATABLE :: STRESS,STRETCH
      INTEGER , DIMENSION(:), ALLOCATABLE :: ITAB_ON_A,INDEX

C=======================================================================
!       CHECK the material stability (Drucker proguer conditions)
!====================================================================
       NDATA = 10000  ! La=0.1,10, EM3!!
       LAM_MIN = EM3                                         
       LAM_MAX = TEN 
C       
       DO I=1,5
         MU(I) = UPARAM(I)
         AL(I) = UPARAM(5+I)
       ENDDO  
C
       ALLOCATE (STRETCH(NDATA))                              
       ALLOCATE (STRESS(NDATA))                                  
       ALLOCATE (ITAB_ON_A(NDATA))                                    
       ALLOCATE (INDEX(NDATA))                                                                           
       STRETCH(1)=LAM_MIN
       ITAB_ON_A = 0
       DO K= 2,NDATA
         STRETCH(K)=STRETCH(K-1) + EM3
       ENDDO                                         
       STRESS=ZERO  
C                                                            
       WRITE(IOUT,1000)MAT_ID  
C  Tension/compression      
       INDX =0                
       DO I = 1, NDATA                                        
          D11= ZERO                                         
          D22= ZERO                                   
          D12= ZERO 
          LAM1 =STRETCH(I)
          LAM2 = ONE/SQRT(LAM1)
          LAM3 = LAM2
C            
          DO K=1,5 
             LAM12 = (lAM1*LAM2)**(-AL(K))
             D11=D11+ AL(K)*MU(K) * (LAM1**AL(K) + LAM12 )
             D22=D22+ AL(K)*MU(K) * (LAM2**AL(K) + LAM12 )
             D12=D12+ AL(K)*MU(K) * LAM12
          ENDDO
          
           INVD1 = D11 + D22
           INVD2 = D11*D22 - D12**2 
           IF (INVD1 > 0 .AND. INVD2 > 0) THEN
             INDX = INDX +1 
             ITAB_ON_A(INDX) = 1
             INDEX(INDX) = I
           ENDIF  
       ENDDO 
       IF(INDX > 0 .AND. INDX < NDATA) THEN 
         WRITE(IOUT,1010)
         I = INDEX(1) - 1
         IF(I > 1) THEN
          STRAIN_MIN = STRETCH(I) - ONE 
          WRITE(IOUT,1100)STRAIN_MIN 
         ENDIF
         I = INDEX(INDX) + 1
         IF(I <= NDATA)THEN
           STRAIN_MAX = STRETCH(I) - ONE
           WRITE(IOUT,1200)STRAIN_MAX
         ENDIF  
       ENDIF
C  Biaxial      
       INDX =0                                 
       DO I = 1, NDATA                                        
          D11= ZERO                                         
          D22= ZERO                                   
          D12= ZERO 
          LAM1 =STRETCH(I)
          LAM2 = LAM1
          LAM3 = ONE/LAM1/LAM1
C            
          DO K=1,5 
             LAM12 = (lAM1*LAM2)**(-AL(K))
             D11=D11+ AL(K)*MU(K) * (LAM1**AL(K) + LAM12 )
             D22=D22+ AL(K)*MU(K) * (LAM2**AL(K) + LAM12 )
             D12=D12+ AL(K)*MU(K) * LAM12
          ENDDO
          
           INVD1 = D11 + D22
           INVD2 = D11*D22 - D12**2 
           IF (INVD1 > 0 .AND. INVD2 > 0) THEN
             INDX = INDX + 1 
             ITAB_ON_A(INDX) = 1
             INDEX(INDX) = I 
           ENDIF  
       ENDDO
       IF(INDX > 0 .AND. INDX < NDATA) THEN
         WRITE(IOUT,1020)
         I = INDEX(1) - 1
         IF(I > 1) THEN
          STRAIN_MIN = STRETCH(I) - ONE 
          WRITE(IOUT,1100)STRAIN_MIN 
         ENDIF 
         I = INDEX(INDX) + 1
         IF(I <= NDATA)THEN
           STRAIN_MAX = STRETCH(I) - ONE
           WRITE(IOUT,1200)STRAIN_MAX
         ENDIF 
       ENDIF
C shear test
       INDX =0                                 
       DO I = 1, NDATA                                        
          D11= ZERO                                         
          D22= ZERO                                   
          D12= ZERO 
          LAM1 =STRETCH(I)
          LAM2 = ONE
          LAM3 = ONE/LAM1
C            
          DO K=1,5 
             LAM12 = (lAM1*LAM2)**(-AL(K))
             D11=D11+ AL(K)*MU(K) * (LAM1**AL(K) + LAM12 )
             D22=D22+ AL(K)*MU(K) * (LAM2**AL(K) + LAM12 )
             D12=D12+ AL(K)*MU(K) * LAM12
          ENDDO
          
           INVD1 = D11 + D22
           INVD2 = D11*D22 - D12**2 
           IF (INVD1 > 0 .AND. INVD2 > 0) THEN
             INDX = INDX +1 
             ITAB_ON_A(INDX) = 1
             INDEX(INDX) = I
           ENDIF  
       ENDDO
       IF(INDX > 0 .AND. INDX < NDATA) THEN 
         WRITE(IOUT,1030)
         I = INDEX(1) - 1
         IF(I > 1) THEN
          STRAIN_MIN = STRETCH(I) - ONE 
          WRITE(IOUT,1100)STRAIN_MIN 
         ENDIF
         I = INDEX(INDX) + 1
         IF(I <= NDATA)THEN
           STRAIN_MAX = STRETCH(I) - ONE
           WRITE(IOUT,1200)STRAIN_MAX
         ENDIF
       ENDIF 
C             
        DEALLOCATE (STRETCH)                                                 
        DEALLOCATE (STRESS)                                 
        DEALLOCATE (ITAB_ON_A)                                    
        DEALLOCATE (INDEX)  
      RETURN
 1000 FORMAT
     & (//5X, 'CHECK THE DRUCKER PRAGER STABILITY CONDITIONS   ' ,/,
     &    5X, ' -----------------------------------------------', /,
     &    5X, 'MATERIAL LAW = OGDEN (LAW42) ',/,
     &    5X, 'MATERIAL NUMBER =',I10,//)
 1010 FORMAT(
     & 7X,'TEST TYPE = UNIXIAL  ')
 1020 FORMAT(//,
     & 7X,'TEST TYPE = BIAXIAL  ')
 1030 FORMAT(//,
     & 7X,'TEST TYPE = PLANAR (SHEAR)')
 1100 FORMAT(
     & 8X,'COMPRESSION:    UNSTABLE AT A NOMINAL STRAIN LESS THAN ',1PG20.13)
 1200 FORMAT(
     & 8X,'TENSION:        UNSTABLE AT A NOMINAL STRAIN LARGER THAN ',1PG20.13)
c-----------
      END
