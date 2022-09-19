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
Chd|  LAW92_UPD                     source/materials/mat/mat092/law92_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        LAW92_NLSQF                   source/materials/mat/mat092/law92_nlsqf.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW92_UPD(IOUT,TITR    ,MAT_ID,UPARAM,NFUNC,
     .                      IFUNC, FUNC_ID,NPC   ,PLD   ,PM,IPM)
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
      INTEGER MAT_ID,IOUT, NFUNC
      INTEGER NPC(*), FUNC_ID(*), IPM(NPROPMI)
      my_real UPARAM(*),PLD(*),PM(NPROPM)
      INTEGER, DIMENSION(NFUNC):: IFUNC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,K,ITEST,II,JJ,NSTART,IC1,IC2,NOGD,NDATA,NMULA,IFC
      my_real E,NU,GS,RBULK,D,YOUNG,ERRTOL,AVE_SLOPE,MU,MU_MAX,MU_MIN,DX,
     .   LAM,BETA,LAM_MAX,LAM_MIN,SCALEFAC,AMULA(2)
      my_real , DIMENSION(:), ALLOCATABLE :: STRESS,STRETCH!
      LOGICAL  IS_ENCRYPTED
C==================================================================== 
!       IDENTIFICATION
!====================================================================
       IS_ENCRYPTED = .FALSE.
       CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
  !!
       NSTART = 2
       ERRTOL = FIVEEM3
       IFC = IFUNC(1)
       IC1 = NPC(IFC)
       IC2 = NPC(IFC+1)
       SCALEFAC =  UPARAM(14)
       NOGD=(IC2-IC1)/2
       NDATA=NOGD
c
       ALLOCATE (STRETCH(NOGD))
       ALLOCATE (STRESS(NOGD))
c
       AVE_SLOPE = ZERO
       JJ=0
       STRETCH=ZERO
       STRESS=ZERO
       MU=ZERO
       RBULK=ZERO
       GS=ZERO
       LAM_MAX= ZERO
       LAM_MIN= ZERO
c
       DO II = IC1,IC2-2,2
            JJ=JJ+1
            STRETCH(JJ) = PLD(II) + ONE
            STRESS(JJ)  = SCALEFAC *  PLD(II+1)
            LAM_MAX = MAX(LAM_MAX, ABS(STRETCH(JJ)))
       ENDDO
c
       NOGD = JJ
       MU_MAX = ZERO
       MU_MIN = 1E20
       DO K = 1, NDATA
        DX = STRETCH(K) - ONE
c       avolid dx to be too small
        IF (DX >= ZERO) THEN
          DX = MAX(DX, EM6)
        ELSE
            DX = ABS(DX)
        ENDIF
        MU_MAX = MAX (MU_MAX, STRESS(K)  / DX)
        AVE_SLOPE = AVE_SLOPE + ABS(STRESS(K))  / DX
       ENDDO
c
       AVE_SLOPE = AVE_SLOPE / (ONE * NDATA)
       MU= AVE_SLOPE
c      initial value
       LAM = MAX(SEVEN,THREE*LAM_MAX)
C                                            
       NMULA  = 2
       AMULA(1) = MAX(MU,MU_MAX)
       AMULA(2) = LAM
       ITEST  =  UPARAM(12)
      !----------------
        IF(IS_ENCRYPTED)THEN
          WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
       ELSE
         WRITE(IOUT,1000)
         WRITE(IOUT,1001)TRIM(TITR),MAT_ID
       ENDIF
      !----
c------------------------------------------------------
       CALL LAW92_NLSQF(STRETCH,STRESS,NMULA,NOGD,AMULA,
     .                   NSTART, ERRTOL,MAT_ID,TITR,ITEST)
c------------------------------------------------------
        DEALLOCATE (STRETCH)
        DEALLOCATE (STRESS)
        NU  = UPARAM(11)
        MU  = AMULA(1)
        LAM = AMULA(2)
        BETA = ONE/LAM/LAM
        GS = MU*(ONE +  THREE*BETA /FIVE + EIGHTY19*BETA*BETA/175.
     .         + 513.*BETA**3/875. + 42039.*BETA**4/67375.)
        RBULK=TWO*GS*(ONE+NU)
     .        /MAX(EM30,THREE*(ONE-TWO*NU))
        D= TWO/RBULK
        UPARAM(1)=MU
        UPARAM(2)=D
        UPARAM(3)=LAM
        UPARAM(4)=GS
        UPARAM(5)=RBULK
c       parameters
        YOUNG  = TWO*GS*(ONE + NU)
        PM(20) = YOUNG
        PM(21) = NU
        PM(22) = GS
        PM(24) = YOUNG/(ONE - NU**2)
        PM(32) = RBULK
        PM(100) = RBULK  !PARMAT(1)
C-----------
C     Formulation for solid elements time step computation.
        IPM(252)= 2
        PM(105) = TWO*GS/(RBULK + FOUR_OVER_3*GS)
C                          
      IF(.NOT.IS_ENCRYPTED) WRITE(IOUT,1100)MU,D,LAM,GS,RBULK
c----------------
c     end of optimization loop
c----------------
      RETURN
c----------------
 1000 FORMAT
     & (//5X, 'FITTED PARAMETERS FOR HYPERELASTIC_MATERIAL LAW92 ' ,/,
     &    5X, ' --------------------------------------------------')
 1001 FORMAT(
     & 5X,A,/,
     &    5X, 'MATERIAL NUMBER =',I10,//)
 1100 FORMAT(
C
     & 5X,'ARRUDA-BOYCE LAW',/,
     & 5X,'MU . . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'D. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'LAM. . . . . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'INITIAL SHEAR MODULUS. . . . . . . . . . .=',1PG20.13/
     & 5X,'BULK MODULUS . . . . . . . . . . . . . . .=',1PG20.13//)
c-----------
      RETURN
      END
