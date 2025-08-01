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
C  
!||====================================================================
!||    law111_upd             ../starter/source/materials/mat/mat111/law111_upd.F
!||--- called by ------------------------------------------------------
!||    updmat                 ../starter/source/materials/updmat.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    arret                  ../starter/source/system/arret.F
!||    func_slope             ../starter/source/tools/curve/func_slope.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    table_mod              ../starter/share/modules1/table_mod.F
!||====================================================================
      SUBROUTINE LAW111_UPD(IOUT,TITR    ,MAT_ID,UPARAM,NFUNC,
     .                      IFUNC, FUNC_ID,NPC   ,PLD   ,PM,IPM)
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
      INTEGER MAT_ID,IOUT, NFUNC
      INTEGER NPC(*), FUNC_ID(*), IPM(NPROPMI)
      my_real UPARAM(*),PLD(*),PM(NPROPM)
      INTEGER, DIMENSION(NFUNC):: IFUNC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,K,ITEST,II,JJ,NSTART,IC1,IC2,NOGD,NDATA,NMULA,IFC,ICRYPT,
     .        ICHECK,NCOMP
      my_real E,NU,GS,RBULK,D,YOUNG,ERRTOL,AVE_SLOPE,MU,MU_MAX,MU_MIN,DX,
     .   SCALEFAC,STIFFMIN,STIFFMAX,STIFFINI,STFAVG
      my_real , DIMENSION(:), ALLOCATABLE :: STRESS,STRETCH
C==================================================================== 
!       IDENTIFICATION
!====================================================================
       ICRYPT = 0    !
       NSTART = 2
       ERRTOL = FIVEEM3   
       IFC = IFUNC(1)
       IC1 = NPC(IFC)
       IC2 = NPC(IFC + 1)
       SCALEFAC =  UPARAM(3)
       NOGD=(IC2-IC1)/2
       NDATA=NOGD   
C
C  !! check if the curve don't have (0,0) point.
C        
         ICHECK = 0
         NCOMP  = 0
         DO JJ = IC1,IC2 - 4,2
              IF (PLD(JJ) == ZERO .AND. PLD(JJ + 1) == ZERO )ICHECK = 1
              IF (PLD(JJ) <  ZERO ) NCOMP = NCOMP + 1
         ENDDO 
         IF (ICHECK == 0 ) THEN
                              ! Error message
                 CALL ANCMSG(MSGID=1896,
     .                           MSGTYPE=MSGERROR,
     .                           ANMODE=ANINFO,
     .                           I1=MAT_ID,
     .                           C1=TITR,
     .                           I2=FUNC_ID(IFC)) ! Id_function
                   CALL ARRET(2)
         ENDIF               
!!         IF (NCOMP == 0 ) THEN   ! No curve definition in compression => warning
!!                 CALL ANCMSG(MSGID=1917,
!!     .                       MSGTYPE=MSGWARNING,
!!     .                       ANMODE=ANINFO,
!!     .                       I1=MAT_ID,
!!     .                       C1=TITR,
!!     .                       I2=FUNC_ID(IFC)) ! Id_function
!!         ENDIF               
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
c    
       CALL FUNC_SLOPE(IFUNC(1),SCALEFAC,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STFAVG) 
C                                                                                        
        NU  = UPARAM(1)                                             
        !!GS  = STIFFMAX  
        GS  = STIFFINI 
C        
        RBULK=TWO*GS*(ONE+NU)                                               
     .        /MAX(EM30,THREE*(ONE-TWO*NU)) 
        UPARAM(4) = GS                                                  
        UPARAM(5) = RBULK
        UPARAM(6) = UPARAM(4)
        IF(NCOMP == 0) UPARAM(7) = 1  
!!        UPARAM(6)=TWO*STIFFMIN*(ONE+NU)
!!     .        /MAX(EM30,THREE*(ONE-TWO*NU))
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
        IF (ICRYPT == 0) THEN
          WRITE(IOUT,1000)
          WRITE(IOUT,1100)GS,RBULK
        ENDIF
c----------------
c     end of optimization loop
c----------------
      RETURN
c----------------
 1000 FORMAT
     & (//5X, ' PARAMETERS FOR HYPERELASTIC_MATERIAL LAW111 ' ,/,
     &    5X, ' --------------------------------------------------')
 1100 FORMAT(
C
     & 5X,'MARLOW LAW',/,
     & 5X,'INITIAL SHEAR MODULUS. . . . . . . . . . .=',1PG20.13/
     & 5X,'BULK MODULUS . . . . . . . . . . . . . . .=',1PG20.13//)
c-----------
      RETURN
      END
