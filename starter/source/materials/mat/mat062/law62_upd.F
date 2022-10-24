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
Chd|  LAW42_UPD                     source/materials/mat/mat042/law42_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW62_UPD(IOUT,TITR    ,MAT_ID,NUPARAM,UPARAM)
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
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle  :: TITR
      INTEGER MAT_ID,IOUT,NUPARAM
      my_real, DIMENSION(NUPARAM), INTENT(INOUT) :: UPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

c
      INTEGER NDATA,I,K,INDX,ITER,NORDRE,J,NPRONY
      my_real
     .   LAM_MIN,LAM_MAX,STRAIN_MIN,STRAIN_MAX,
     .   D11,D22,D12,LAM1,LAM2,LAM3,LAM12,INVD1,INVD2,A1,A2,A3,D33,D13,D23,
     .   AA,B,BB,DT2,DT3,T2,T3,INVD3,RV
      my_real , DIMENSION(:), ALLOCATABLE :: STRESS,STRETCH,MU,AL,BETA,
     .                                       MUOVERAL,ALBETA,MUBETA
      INTEGER , DIMENSION(:), ALLOCATABLE :: ITAB_ON_A,INDEX

C=======================================================================
!       CHECK the material stability (Drucker proguer conditions)
!====================================================================
      !! NDATA = 5000  ! La=0.01,10, EM3!!
       LAM_MIN =EM03                                          
       LAM_MAX =FOUR 
       NDATA = NINT((LAM_MAX-LAM_MIN)/EM03)
C       
       NORDRE  = NINT(UPARAM(2))
       NPRONY  = NINT(UPARAM(3))
       ALLOCATE ( MU(NORDRE),AL(NORDRE),BETA(NORDRE), 
     .            MUOVERAL(NORDRE),ALBETA(NORDRE),MUBETA(NORDRE) )
       DO J= 1,NORDRE
         MU(J) = UPARAM(6 + J)
         AL(J) = UPARAM(6 + NORDRE + J)
         BETA(J) = UPARAM(2*NORDRE + 2*NPRONY + 7 + J)
         MUOVERAL(J) = TWO*MU(J)/AL(J)
         ALBETA(J) = AL(J)*BETA(J)
         MUBETA(J) = MU(J)*BETA(J)
       ENDDO
C
       ALLOCATE (STRETCH(NDATA))                              
       ALLOCATE (STRESS(NDATA))                                  
       ALLOCATE (ITAB_ON_A(NDATA))                                    
       ALLOCATE (INDEX(NDATA))                                                                       
       STRETCH(1)=LAM_MIN
       ITAB_ON_A = 0
       DO K= 2,NDATA
         STRETCH(K)=STRETCH(K-1) + EM03
       ENDDO                                         
       STRESS=ZERO  
C                                                            
       WRITE(IOUT,1000)MAT_ID  
       
       ! uniaxial  Lam1= lam, lam2=lam3 (find lam2 (T2(lam2) = 0)
       INDX =0                
       DO I = 1, NDATA                                        
          D11= ZERO                                         
          D22= ZERO
          D33 = ZERO                                   
          D12 = ZERO 
          D13 = ZERO
          D23 = ZERO
          LAM1 =STRETCH(I)
          LAM2 = LAM1 ! starting point 
          LAM3 = LAM2   
          DO ITER = 1,20
            RV = LAM1*LAM2*LAM3 ! lam3=lam2
            T2= ZERO
            DT2 = ZERO
            DO J=1,NORDRE
               AA = EXP(AL(J)*LOG(LAM2))
               BB = EXP(-ALBETA(J)*LOG(RV))
               T2 = T2   + MUOVERAL(J)*(AA - BB)
               DT2 = DT2 + TWO*MU(J)*AA + TWO*MUBETA(J)*BB
               DT2 = DT2 /LAM2
            ENDDO ! NORDRE
            IF(DT2 /= ZERO) LAM2 = LAM2 - T2/DT2
            LAM2 = MAX(EM03, LAM2)
            LAM3 = LAM2
          ENDDO ! iter
          RV= LAM1*LAM2*LAM3
          A1 = ZERO
          A2 = ZERO
          A3 = ZERO
          B  = ZERO
          DO J=1,NORDRE
           A1 = A1 + TWO*MU(J)*EXP(AL(J)*LOG(LAM1))
           A2 = A2 + TWO*MU(J)*EXP(AL(J)*LOG(LAM2))
           B  = B + TWO*MUBETA(J)*EXP(-ALBETA(J)*LOG(RV))
          ENDDO
          D11 = A1 + B
          D22 = A2 + B
          D33 = D22
           !D12 = D13= D21 = D23 = D31 = D32 =  B
          AA  = B**2
          INVD1 = D11 + D22 + D33
          INVD2 = D11*D22 + D22*D33 + D11*D33 - THREE*AA 
          INVD3 = D11*D22*D33 + TWO*AA*B - TWO*D22*AA - D11*AA
          IF (INVD1 > ZERO .AND. INVD2 > ZERO .AND. INVD3 > ZERO) THEN
             INDX = INDX +1 
             ITAB_ON_A(INDX) = 1
             INDEX(INDX) = I 
           ENDIF
       ENDDO ! nDATA 
       IF(INDX > 0 .AND. INDX < NDATA) THEN 
         WRITE(IOUT,1010)
         I = INDEX(1) - 1
         IF(I > 1 .AND. I < NDATA) THEN
          STRAIN_MIN = STRETCH(I) - ONE 
          WRITE(IOUT,1100)STRAIN_MIN
         ENDIF
         I = INDEX(INDX) + 1
         IF(I < NDATA)THEN
           STRAIN_MAX = STRETCH(I) - ONE
           WRITE(IOUT,1200)STRAIN_MAX
         ENDIF  
       ENDIF
C  Biaxial      
       INDX =0                
       DO I = 1, NDATA                                        
          D11 = ZERO                                         
          D22 = ZERO
          D33 = ZERO                                   
          D12 = ZERO 
          D13 = ZERO
          D23 = ZERO
          LAM1 = STRETCH(I)
          LAM2 = LAM1
          LAM3 = LAM1 ! staring point 
C         ! bi-axial  Lam1=lam2= lam, lam3 (find lam3 corresponding to (T3(lam3) = 0)   
          DO ITER = 1,20
            RV = LAM1*LAM2*LAM3 !
            T3 = ZERO
            DT3 = ZERO
            DO J=1,NORDRE
               AA = EXP(AL(J)*LOG(LAM3))
               BB = EXP(-ALBETA(J)*LOG(RV))
               T3 =  T3 + MUOVERAL(J)*(AA - BB)
               DT3 = DT3 + TWO*MU(J)* AA + TWO*MUBETA(J)*BB
               DT3 = DT3 /LAM3
            ENDDO  ! NORDRE
              IF(DT3 /= ZERO) LAM3 = LAM3 - T3/DT3
              LAM3 = MAX(EM03, LAM3)
          ENDDO ! Iter 
          RV= LAM1*LAM2*LAM3
          A1 = ZERO
          A2 = ZERO
          A3 = ZERO
          B  = ZERO
          DO J=1,NORDRE
           A1 = A1 + TWO*MU(J)*EXP(AL(J)*LOG(LAM1))
           A3 = A3 + TWO*MU(J)*EXP(AL(J)*LOG(LAM3))
           B  = B + TWO*MUBETA(J)*EXP(-ALBETA(J)*LOG(RV))
          ENDDO
          D11 = A1 + B
          D22 = D11
          D33 = A3 + B
           !D12 = D13= D21 = D23 = D31 = D32 =  B
          AA  = B**2
          INVD1 = D11 + D22 + D33
          INVD2 = D11*D22 + D22*D33 + D11*D33 - THREE*AA 
          INVD3 = D11*D22*D33 + TWO*AA*B - TWO*D22*AA - D11*AA
          IF (INVD1 > ZERO .AND. INVD2 > ZERO .AND. INVD3 > ZERO) THEN
             INDX = INDX +1 
             ITAB_ON_A(INDX) = 1
             INDEX(INDX) = I
          ENDIF  
       ENDDO ! NDATA
       IF(INDX > 0 .AND. INDX < NDATA) THEN
           WRITE(IOUT,1020)
           I = INDEX(1) - 1
           IF(I > 1 .AND. I < NDATA) THEN
              STRAIN_MIN = STRETCH(I) - ONE 
              WRITE(IOUT,1100)STRAIN_MIN 
           ENDIF 
           I = INDEX(INDX) + 1
           IF(I < NDATA)THEN
              STRAIN_MAX = STRETCH(I) - ONE
              WRITE(IOUT,1200)STRAIN_MAX
           ENDIF 
       ENDIF
C plane test
       INDX =0                
       DO I = 1, NDATA                                        
          D11 = ZERO                                         
          D22 = ZERO
          D33 = ZERO                                   
          D12 = ZERO 
          D13 = ZERO
          D23 = ZERO
          LAM1 = STRETCH(I)
          LAM2 = ONE
          LAM3 = LAM1 ! starting point 
C         ! bi-axial  Lam1=LAM, lam2= ONE, lam3 (find lam3 corresponding to (T3(lam3) = 0)   
          DO ITER = 1,20
            RV = LAM1*LAM2*LAM3 !
            T3 = ZERO
            DT3 = ZERO
            DO J=1,NORDRE
               AA = EXP(AL(J)*LOG(LAM3))
               BB = EXP(-ALBETA(J)*LOG(RV))
               T3 = T3 + MUOVERAL(J)*(AA - BB )
               DT3 = DT3 + TWO*MU(J)* AA + TWO*MUBETA(J)*BB
               DT3 = DT3 /LAM3
            ENDDO   ! NORDRE
            IF(DT3 /= ZERO) LAM3 = LAM3 - T3/DT3
            LAM3 = MAX(EM03, LAM3)
          ENDDO ! iter
          RV= LAM1*LAM2*LAM3
          A1 = ZERO
          A2 = ZERO
          A3 = ZERO
          B  = ZERO
          DO J=1,NORDRE
           A1 = A1 + TWO*MU(J)*EXP(AL(J)*LOG(LAM1))
           A2 = A2 + TWO*MU(J)*EXP(AL(J)*LOG(LAM2))
           A3 = A3 + TWO*MU(J)*EXP(AL(J)*LOG(LAM3))
           B  = B + TWO*MUBETA(J)*EXP(-ALBETA(J)*LOG(RV))
          ENDDO
          D11 = A1 + B
          D22 = A2 + B
          D33 = A3 + B
           !D12 = D13= D21 = D23 = D31 = D32 =  B
          AA  = B**2
          INVD1 = D11 + D22 + D33
          INVD2 = D11*D22 + D22*D33 + D11*D33 - THREE*AA 
          INVD3 = D11*D22*D33 + TWO*AA*B - TWO*D22*AA - D11*AA
          IF (INVD1 > ZERO .AND. INVD2 > ZERO .AND. INVD3 > ZERO) THEN
             INDX = INDX +1 
             ITAB_ON_A(INDX) = 1
             INDEX(INDX) = I
          ENDIF  
       ENDDO ! NDATA
       IF(INDX > 0 .AND. INDX < NDATA) THEN 
         WRITE(IOUT,1030)
         I = INDEX(1) - 1
         IF(I > 1 .AND. I < NDATA) THEN
          STRAIN_MIN = STRETCH(I) - ONE 
          WRITE(IOUT,1100)STRAIN_MIN 
         ENDIF
         I = INDEX(INDX) + 1
         IF(I < NDATA)THEN
           STRAIN_MAX = STRETCH(I) - ONE
           WRITE(IOUT,1200)STRAIN_MAX
         ENDIF
       ENDIF 
C             
        DEALLOCATE (STRETCH)                                                 
        DEALLOCATE (STRESS)                                 
        DEALLOCATE (ITAB_ON_A)                                    
        DEALLOCATE (INDEX) 
        DEALLOCATE ( MU,AL,BETA,  MUOVERAL,ALBETA,MUBETA)
      RETURN
 1000 FORMAT
     & (//5X, 'CHECK THE DRUCKER PRAGER STABILITY CONDITIONS   ' ,/,
     &    5X, ' -----------------------------------------------', /,
     &    5X, 'MATERIAL LAW =  LAW62 ',/,
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
