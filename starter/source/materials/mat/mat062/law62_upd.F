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
!||    law62_upd              ../starter/source/materials/mat/mat062/law62_upd.F
!||--- called by ------------------------------------------------------
!||    updmat                 ../starter/source/materials/updmat.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    table_mod              ../starter/share/modules1/table_mod.F
!||====================================================================
      SUBROUTINE LAW62_UPD(IOUT,TITR    ,MAT_ID,NUPARAM,UPARAM, PM ,
     .                    GAMA_INF )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER(LEN=NCHARTITLE)  :: TITR
      INTEGER MAT_ID,IOUT,NUPARAM
      my_real, DIMENSION(NUPARAM), INTENT(INOUT) :: UPARAM
      my_real,  INTENT(IN)                       :: GAMA_INF
      my_real, DIMENSION(:),  INTENT(INOUT) ::  PM(NPROPM)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NDATA,I,K,INDX,ITER,NORDRE,J,NPRONY
      my_real
     .   LAM_MIN,LAM_MAX,STRAIN_MIN,STRAIN_MAX,
     .   D11,D22,D12,LAM1,LAM2,LAM3,LAM12,INVD1,INVD2,A1,A2,A3,D33,D13,D23,
     .   AA,B,BB,DT2,DT3,T2,T3,INVD3,RV,BULK,GS,NUG,YOUNG
      my_real , DIMENSION(:), ALLOCATABLE :: STRESS,STRETCH,MU,AL,BETA,
     .                                       MUOVERAL,ALBETA,MUBETA,NU
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
     .            MUOVERAL(NORDRE),ALBETA(NORDRE),MUBETA(NORDRE),NU(NORDRE) )
       DO J= 1,NORDRE
         MU(J) = UPARAM(6 + J)/GAMA_INF
         AL(J) = UPARAM(6 + NORDRE + J)
         BETA(J) = UPARAM(2*NORDRE + 2*NPRONY + 7 + J)
         MUOVERAL(J) = TWO*MU(J)/AL(J)
         ALBETA(J) = AL(J)*BETA(J)
         MUBETA(J) = MU(J)*BETA(J)
         NU(J) = ONE*BETA(J)/(ONE + TWO*BETA(J))
       ENDDO
       GS = ZERO
       BULK = ZERO
       IF(GAMA_INF /= ONE) THEN
          DO J= 1,NORDRE
             UPARAM(6 + J) =  MU(J) 
             GS = GS + MU(J)
             BULK = BULK +  TWO*MU(J)*(THIRD + BETA(J))
          ENDDO
          NUG = HALF*(THREE*BULK - TWO*GS)/(THREE*BULK+ GS)
          UPARAM(1) = NUG
          UPARAM(5) = BULK                                                 
C parameters 
          GS = GS*TWO  
          YOUNG  = GS*(ONE + NUG)                                              
          PM(20) = YOUNG                                                
          PM(21) = NUG                                                   
          PM(22) = GS !! TWO*G                                                  
          PM(24) = YOUNG/(ONE - NUG**2)                                   
          PM(32) = BULK                                                
          PM(100) = BULK 
C     Formulation for solid elements time step computation.
          PM(105) = GS/(BULK + TWO_THIRD*GS) 
       ENDIF
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
       IF(GAMA_INF /= ONE ) THEN
               WRITE(IOUT,1000) MAT_ID
               WRITE(IOUT,1100) NUG,GS*HALF,BULK,NORDRE
               WRITE(IOUT,1200) (MU(J),AL(J),NU(J),J=1,NORDRE)
       ENDIF                                                            
       WRITE(IOUT,2000)MAT_ID  
       
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
         WRITE(IOUT,2010)
         I = INDEX(1) - 1
         IF(I > 1 .AND. I < NDATA) THEN
          STRAIN_MIN = STRETCH(I) - ONE 
          WRITE(IOUT,2100)STRAIN_MIN
         ENDIF
         I = INDEX(INDX) + 1
         IF(I < NDATA)THEN
           STRAIN_MAX = STRETCH(I) - ONE
           WRITE(IOUT,2200)STRAIN_MAX
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
           WRITE(IOUT,2020)
           I = INDEX(1) - 1
           IF(I > 1 .AND. I < NDATA) THEN
              STRAIN_MIN = STRETCH(I) - ONE 
              WRITE(IOUT,2100)STRAIN_MIN 
           ENDIF 
           I = INDEX(INDX) + 1
           IF(I < NDATA)THEN
              STRAIN_MAX = STRETCH(I) - ONE
              WRITE(IOUT,2200)STRAIN_MAX
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
!!    
       IF(INDX > 0 .AND. INDX < NDATA) THEN 
         WRITE(IOUT,2030)
         I = INDEX(1) - 1
         IF(I > 1 .AND. I < NDATA) THEN
          STRAIN_MIN = STRETCH(I) - ONE 
          WRITE(IOUT,2100)STRAIN_MIN 
         ENDIF
         I = INDEX(INDX) + 1
         IF(I < NDATA)THEN
           STRAIN_MAX = STRETCH(I) - ONE
           WRITE(IOUT,2200)STRAIN_MAX
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
     & (//5X, 'MODIFIED  MATERIAL RIGIDITY    ' ,/,
     &    5X, ' ---------------------------', /,
     &    5X, 'MATERIAL LAW =  LAW62 ',/,
     &    5X, 'MATERIAL NUMBER =',I10,//)
 1100 FORMAT
     &(5X,'EQUIVALENT POISSON RATIO . . . .  . . .=',E12.4/
     &,5X,'INITIAL SHEAR MODULUS . . . . . . . . .=',E12.4/
     & 5X,'INITIAL BULK MODULUS. . .. . . . . . .=',E12.4// 
     &,5X,'ORDER OF STRAIN ENERGY. . . . . . . . .=',I8)
 1200 FORMAT(
     & 7X,'MATERIAL PARAMETER (MU). . . . . . . . =',E12.4/
     & 7X,'MATERIAL PARAMETER (ALPHA) . . . . . . =',E12.4/
     & 7X,'MATERIAL PARAMETER (NU) . . . . . . .  =',E12.4/) 
 2000 FORMAT
     & (//5X, 'CHECK THE DRUCKER PRAGER STABILITY CONDITIONS   ' ,/,
     &    5X, ' -----------------------------------------------', /,
     &    5X, 'MATERIAL LAW =  LAW62 ',/,
     &    5X, 'MATERIAL NUMBER =',I10,//)
 2010 FORMAT(
     & 7X,'TEST TYPE = UNIXIAL  ')
 2020 FORMAT(//,
     & 7X,'TEST TYPE = BIAXIAL  ')
 2030 FORMAT(//,
     & 7X,'TEST TYPE = PLANAR (SHEAR)')
 2100 FORMAT(
     & 8X,'COMPRESSION:    UNSTABLE AT A NOMINAL STRAIN LESS THAN ',1PG20.13)
 2200 FORMAT(
     & 8X,'TENSION:        UNSTABLE AT A NOMINAL STRAIN LARGER THAN ',1PG20.13)
c-----------
      END
