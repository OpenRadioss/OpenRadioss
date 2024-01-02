Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  LAW100_UPD_NHT                source/materials/mat/mat100/law100_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        FUNC_MAXY                     source/tools/curve/func_maxy.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW100_UPD_NHT(IOUT, TITR,MAT_ID,UPARAM,NFUNC, 
     .                        IFUNC, FUNC_ID , NPC   , PLD , PM)
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
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle  :: TITR
      INTEGER MAT_ID,IOUT, NFUNC
      INTEGER NPC(*), FUNC_ID(*) 
      my_real 
     .         UPARAM(*),PLD(*),PM(NPROPM)
      INTEGER, DIMENSION(NFUNC):: IFUNC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDFC,IDFD 
 
      my_real 
     .      CMIN,CMAX,CINI,CAVG,DMIN,DMAX,DINI,DAVG,FAC1,FAC2,SB,RBULK,
     .      NU,G
C=======================================================================
      !DIRECTION CHAINE
      IDFC = IFUNC(1)
      IDFD = IFUNC(2)
      SB   = UPARAM(4)
      FAC1 = UPARAM(8+1)
      FAC2 = UPARAM(8+2)
                    
      CALL FUNC_MAXY(IDFC,FAC1,NPC,PLD,CMAX)
      CALL FUNC_MAXY(IDFD,FAC2,NPC,PLD,DMAX)

      G    = CMAX *(SB + ONE)
      RBULK= DMAX *(ONE + SB) 

      NU = (THREE*RBULK -TWO*G)/(THREE*RBULK + G)/TWO
      PM(20)= CMAX / TWO
      PM(21)= NU
      PM(22)= G
      PM(24)= CMAX/(ONE - NU**2) / TWO
      PM(32)= RBULK  
      UPARAM(8+4)  = G 
      UPARAM(8+5)  = RBULK 
      RETURN
      END
cc
c
Chd|====================================================================
Chd|  LAW100_UPD_AB                 source/materials/mat/mat100/law100_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        LAW92_NLSQF                   source/materials/mat/mat092/law92_nlsqf.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW100_UPD_AB(IOUT,TITR    ,MAT_ID,UPARAM,NFUNC,
     .                      IFUNC, FUNC_ID,NPC   ,PLD   ,PM)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle  :: TITR
      INTEGER MAT_ID,IOUT, NFUNC
      INTEGER NPC(*), FUNC_ID(*) 
      my_real 
     .         UPARAM(*),PLD(*),PM(NPROPM)
      INTEGER, DIMENSION(NFUNC):: IFUNC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N_NETWORK,N,K,ITEST,ICHECK,II,JJ,NSTART,IC1,IC2,NOGD,NDATA,NMULA,
     .        TAB,NMUL,NTEMP,NPLAS,NVISC(10)
      my_real
     .   E,NU,GS,RBULK, D,YOUNG,SCALEFAC,
     .   ERRTOL,AVE_SLOPE,MU,MU_MAX,MU_MIN,DX,LAM,BETA,
     .   LAM_MAX,LAM_MIN,AMULA(2)
      my_real , DIMENSION(:), ALLOCATABLE :: STRESS,STRETCH     
      LOGICAL  IS_ENCRYPTED         
C==================================================================== 
!       IDENTIFICATION
!====================================================================
       IS_ENCRYPTED = .FALSE.
       CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
       TAB = 8 
       NSTART = 2
       ERRTOL = FIVEEM3   
       IC1 = NPC(IFUNC(1))
       IC2 = NPC(IFUNC(1)+1)

       SCALEFAC = UPARAM(TAB +11) 

       NOGD=(IC2-IC1)/2
       NDATA=NOGD

       ALLOCATE (STRETCH(NOGD))                              
       ALLOCATE (STRESS(NOGD))                               

       AVE_SLOPE = ZERO
       JJ=0                                                  
       STRETCH=ZERO                                          
       STRESS=ZERO                                           
       MU=ZERO                                               
       RBULK=ZERO                                            
       GS=ZERO                                               
       LAM_MAX= ZERO                                         
       LAM_MIN= ZERO                                         
       DO II = IC1,IC2-2,2                                   
            JJ=JJ+1                                          
            STRETCH(JJ) = PLD(II)  + ONE                       
            STRESS(JJ)  = SCALEFAC * PLD(II+1)                          
            LAM_MAX = MAX(LAM_MAX, ABS(STRETCH(JJ)))         
       ENDDO                                                 
       NOGD = JJ                                             
       MU_MAX = ZERO                                          
       MU_MIN = 1E20                                          
       DO K = 1, NDATA                                        
        DX = STRETCH(K) - ONE                                  
c       avolid dx to be too small                             
        IF (DX >= ZERO) THEN                                  
          DX = MAX(DX, EM6)                                   
        ELSE                                                  
!!          DX = MIN(DX,-EM6)                                 
            DX = ABS(DX)                                      
        ENDIF                                                 
        MU_MAX = MAX (MU_MAX, STRESS(K)  / DX)                
        AVE_SLOPE = AVE_SLOPE + ABS(STRESS(K))  / DX          
       ENDDO                                                   
       AVE_SLOPE = AVE_SLOPE / (ONE * NDATA)                  
       MU= AVE_SLOPE                                           
! initial value  
       LAM = MAX(SEVEN,THREE*LAM_MAX)         
C                                            
       NMULA  = 2                            
       AMULA(1) = MAX(MU,MU_MAX)             
       AMULA(2) = LAM                        
       ITEST  =  UPARAM(TAB +9)
      !----------------       
        IF(IS_ENCRYPTED)THEN
          WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
       ELSE 
         WRITE(IOUT,1000)    
         WRITE(IOUT,1001)TRIM(TITR),MAT_ID
       ENDIF  
      !----         
        CALL LAW92_NLSQF(STRETCH,STRESS,NMULA,NOGD,AMULA,
     .                   NSTART, ERRTOL,MAT_ID,TITR,ITEST)
       
        DEALLOCATE (STRETCH)                                                 
        DEALLOCATE (STRESS)                                                  
        NU  =  UPARAM( TAB+10)                                             
        MU  = AMULA(1)                                                       
        LAM = AMULA(2)                                                       
        BETA = ONE/LAM/LAM
        GS= MU*(ONE +  THREE*BETA /FIVE + EIGHTY19*BETA*BETA/175. 
     .         + 513.*BETA**3/875. + 42039.*BETA**4/67375.) 
        RBULK=TWO*GS*(ONE+NU)                                               
     .        /MAX(EM30,THREE*(ONE-TWO*NU))                                  
        D= TWO/RBULK                                                        
        UPARAM(TAB + 6)=MU                                                  
        UPARAM(TAB + 7)=ONE/D                                                   
        UPARAM(TAB + 8)=BETA !LAM 
        N_NETWORK = UPARAM(1)                                       
        NMUL    = UPARAM( 6)  
        NTEMP   = UPARAM( 7)  
        NPLAS   = UPARAM( 8) 
        TAB = TAB + 10 + NMUL + NTEMP +NPLAS 
        DO N = 1, N_NETWORK
          NVISC(N) = UPARAM(TAB + 3)  
          TAB = TAB + 3 + NVISC(N)
        ENDDO
         
        UPARAM( TAB + 1 )=GS                                                  
        UPARAM( TAB + 2 )=RBULK       
C parameter    
        YOUNG  = TWO*GS*(ONE + NU)               
        PM(20) = YOUNG                      
        PM(21) = NU                         
        PM(22) = GS                         
        PM(24) = YOUNG/(ONE - NU**2)         
        PM(32) = RBULK                      
        PM(100) = RBULK  !PARMAT(1)                   
!!        
        IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,1100)MU,D,LAM,GS,RBULK
c----------------
c     end of optimization loop
c----------------
      RETURN
 1000 FORMAT
     & (//5X, 'FITTED PARAMETERS FOR HYPERELASTIC_MATERIAL LAW100 ' ,/,
     &    5X, ' --------------------------------------------------') 
 1001 FORMAT(
     & 5X,A,/,
     &    5X, 'MATERIAL NUMBER =',I10,//)
 1100 FORMAT(
C
     & 5X,'TYPE = ARRUDA-BOYCE ',/,
     & 5X,'MU . . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'D. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'LAM. . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'GROUND-STATE SHEAR MODULUS . . . . . . . .=',1PG20.13/ 
     & 5X,'BULK MODULUS . . . . . . . . . . . . . . .=',1PG20.13//)
c-----------
      RETURN
      END
c=================================================================================
Chd|====================================================================
Chd|  YMAX                          source/materials/mat/mat100/law100_upd.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE YMAX(IDN,FAC,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDN,NPC(*) 
      my_real PLD(*),FAC,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG
C-----------------------------------------------
      INTENT(IN)    :: NPC,PLD,IDN
      INTENT(INOUT)   :: STIFFMAX,STIFFINI,STIFFAVG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ID,NP1,NP2,K1,PN1,PN2
      my_real DYDX,DX,DY
C=======================================================================
          ! COMPUTE MAXIMUM SLOPE AND INITIAL SLOPE OF FUNCTION
C=======================================================================
          PN1 = NPC(IDN)
          PN2 = NPC(IDN+1)
          STIFFINI = (PLD(PN1+3) - PLD(PN1+1))*FAC / (PLD(PN1+2) - PLD(PN1))
          STIFFAVG = ZERO
          STIFFMAX = ZERO
          STIFFMIN = EP20
          DO J = PN1,PN2-4,2
            DX = PLD(J+2) - PLD(J)
            DY = PLD(J+3) - PLD(J+1)               
            DYDX = FAC*DY/DX
            STIFFMAX = MAX(STIFFMAX,DYDX)
            STIFFMIN = MIN(STIFFMIN,DYDX)
            STIFFAVG = STIFFAVG + DYDX
          ENDDO
          STIFFAVG = STIFFAVG*TWO /(PN2-PN1)
c-----------
      RETURN
      END

