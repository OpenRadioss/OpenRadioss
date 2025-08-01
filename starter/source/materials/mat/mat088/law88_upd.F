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
!||    law88_upd              ../starter/source/materials/mat/mat088/law88_upd.F
!||--- called by ------------------------------------------------------
!||    updmat                 ../starter/source/materials/updmat.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    arret                  ../starter/source/system/arret.F
!||    func_inters            ../starter/source/tools/curve/func_inters.F
!||    func_inters_c          ../starter/source/tools/curve/func_inters.F
!||    func_slope             ../starter/source/tools/curve/func_slope.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    table_mod              ../starter/share/modules1/table_mod.F
!||====================================================================
      SUBROUTINE LAW88_UPD(IOUT   ,TITR   ,UPARAM ,NPC    ,PLD    ,  
     .                     NFUNC  ,IFUNC  ,MAT_ID ,FUNC_ID,PM     ,
     .                     NFUNCT )
      USE MESSAGE_MOD
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
      INTEGER MAT_ID,IOUT
      INTEGER ,INTENT(IN) :: NFUNC
      INTEGER ,INTENT(IN) :: NFUNCT
      INTEGER NPC(*), FUNC_ID(NFUNCT),IFUNC(NFUNC)
      my_real UPARAM(*),PLD(*)
      my_real , DIMENSION(NPROPM), INTENT(INOUT) :: PM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,FUNC,FUND,PN,IOK,NL,IUNL_FOR,ICASE,NV,
     .  IC1,IC2,NOGD,II,JJ,ICHECK,ITENS
      my_real KC,KT,KFC,KFT,GMAX,DERI,STIFF,STIFFMIN,STIFFINI, 
     .  STIFFMAX,STIFFAVG,XINT1,YINT1,XINT2,YINT2,FAC,FAC1,FAC2,
     .  DX,DY, XINC,YINC,XINT,YINT
      my_real 
     .     EMAX, EMIN,EINI,SCALEFAC,E0,EC_MAX,NU,GS
      my_real , DIMENSION(:), ALLOCATABLE :: STRESS,STRETCH
C=======================================================================
C----------------------------
       DO J = 1, NFUNC
         K = IFUNC(J) 
         IC1  =  NPC(K)
         IC2  =  NPC(K+1)
         NOGD =  (IC2-IC1)/2
         ALLOCATE (STRETCH(NOGD), STRESS(NOGD))   
         JJ = 0       
         ICHECK = 0
         DO II = IC1,IC2-2,2                                 
              JJ=JJ+1                                        
              STRETCH(JJ) = PLD(II) + ONE                     
              STRESS(JJ)  = PLD(II + 1)   
                IF(PLD(II) <=  - ONE) THEN
                  CALL ANCMSG(MSGID=3085,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO,
     .                         I1=MAT_ID,
     .                         C1=TITR,
     .                         I2=FUNC_ID(K)) ! Id_function
              ENDIF
              !! check if the curve don't have (0,0) point.
              IF( PLD(II) == ZERO .AND. PLD(II + 1) == ZERO )ICHECK = 1
         ENDDO 
         IF(ICHECK == 0 ) THEN
                              ! Error message
                 CALL ANCMSG(MSGID=1896,
     .                           MSGTYPE=MSGERROR,
     .                           ANMODE=ANINFO,
     .                           I1=MAT_ID,
     .                           C1=TITR,
     .                           I2=FUNC_ID(K)) ! Id_function
                   CALL ARRET(2)
         ENDIF            
C  check if the curve is monotonic
         DO JJ =1,NOGD - 1                                
           DX = STRETCH(JJ + 1) - STRETCH(JJ)               
           DY = STRESS(JJ  + 1) - STRESS(JJ) 
           IF(DX * DY < ZERO) THEN            
             CALL ANCMSG(MSGID=1176,                     
     .              MSGTYPE=MSGERROR,                  
     .              ANMODE=ANINFO,                     
     .              I1=MAT_ID,                         
     .              C1=TITR,                           
     .              I2=FUNC_ID(K))  
           ENDIF                                    
         ENDDO                              
          DEALLOCATE( STRETCH,STRESS)   
        ENDDO ! NFUNC   
c
c       Intersection - quasistatic curve and unloading curve but is not used 
c
        NL = INT(UPARAM(4))
        IUNL_FOR = NINT(UPARAM(5))
        ITENS    = NINT(UPARAM(8))
        ICASE    = NINT(UPARAM(9))
        NV = 9 + 2*NFUNC
c
        XINC = ZERO
        YINC = ZERO
        XINT = ZERO
        YINT = ZERO
        FUNC = IFUNC(1)
        IF(NFUNC > NL ) THEN
           FUND = IFUNC(NL + 1)
           FAC1 = UPARAM(11  )
           FAC2 = UPARAM(11 + 2*NL  )
C intersection pt of tension if existing
           UPARAM(NV + 1) = 0  ! not existing
           IF(FUNC /= 0 .AND. FUND /= 0) THEN
               CALL FUNC_INTERS(TITR   ,MAT_ID ,FUNC   ,FUND   ,FAC1   ,
     .                            FAC2   ,NPC    ,PLD    ,XINT  ,YINT  )
               UPARAM(NV + 2) = XINT   
               UPARAM(NV + 3) = YINT
               IF(XINT*YINT /= ZERO)UPARAM(NV + 1) = 1 ! only tension 
C intersection pt of compression if existing 
               CALL FUNC_INTERS_C(TITR   ,MAT_ID ,FUNC   ,FUND   ,FAC1   ,
     .                            FAC2   ,NPC    ,PLD    ,XINC  ,YINC  )
               UPARAM(NV + 4) = XINC   
               UPARAM(NV + 5) = YINC
               IF(XINC*YINC /= ZERO ) THEN
                  IF(INT(UPARAM(NV + 1)) == 0 )THEN
                     UPARAM(NV + 1) = -1  ! only compression 
                   ELSE  
                      UPARAM(NV + 1) = 2  ! tension & compression
                   ENDIF
               ENDIF
           ENDIF  
        ENDIF 
        WRITE(IOUT,1000)
        IF(XINT*YINT > ZERO) WRITE(IOUT,1600) XINT,YINT
        IF(XINC*YINC > ZERO) WRITE(IOUT,1700) XINC,YINC
C-----check E_MAX        
           EMAX = ZERO
           EMIN = EP20
           EINI = ZERO
          DO J = 1, NFUNC 
              SCALEFAC= UPARAM(9 + 2*J )  
              CALL FUNC_SLOPE(IFUNC(J),SCALEFAC,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)  
              EMAX = MAX(EMAX,  STIFFMAX )
              EMIN = MIN(EMIN,  STIFFMIN)
              EINI = MAX(EINI, STIFFINI)
          ENDDO ! NFUNC 
          NU =UPARAM(2)
          GS =UPARAM(3)
          E0 = TWO*GS*(ONE + NU)
          EC_MAX = MAX(E0,EMAX)
          PM(24) = EC_MAX
          WRITE(IOUT,1003) EC_MAX    
c----- ------
 1000 FORMAT
     & (5X,'TABULATED OGDEN LAW',/,
     &  5X,'-------------',//)
 1600 FORMAT
     & (5X,  'STRAIN TENSION OF INTERSECTION PT BETWEEN QUASISTAIC AND UNLOADING CURVES. . . . .  . . .=',1PG20.13/
     & ,5X,  'STRESS TENSION OF INTERSECTION PT BETWEEN QUASISTAIC AND UNLOADING CURVES. . . . . . . . =',1PG20.13//)
     
 1700 FORMAT
     & (5X,  'STRAIN COMPRESSION OF INTERSECTION PT BETWEEN QUASISTAIC AND UNLOADING CURVES. . . . .  . . .=',1PG20.13/
     & ,5X,  'STRESS COMPRESSION OF INTERSECTION PT BETWEEN QUASISTAIC AND UNLOADING CURVES. . . . . . . . =',1PG20.13//)
 1003   FORMAT(
     & 5X,'YOUNG''S MODULUS FOR HG COMPUTE . . . .=',1PG20.13/)
      RETURN
      END
