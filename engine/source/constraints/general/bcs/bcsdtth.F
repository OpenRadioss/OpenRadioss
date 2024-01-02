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
Ch       
Chd|====================================================================
Chd|  BCSDTTH                       source/constraints/general/bcs/bcsdtth.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE BCSDTTH(ICODT,ICODR,KINET,ITAB,LPBY,NPBY)
      USE MESSAGE_MOD
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "kincod_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODT(*),ICODR(*),ITAB(*),KINET(*),NPBY(NNPBY,*),LPBY(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, N, NSLRB,JWARN1,JWARN2
      INTEGER IK(9), NK(9,NUMNOD),NKK
C
C=======================================================================

       JWARN1 = 0
       JWARN2 = 0
       IK(1:9) = 0
       NK(1:9,1:NUMNOD) = 0
       DO N=1,NUMNOD 
         IF(ICODT(N)/=7.OR.ICODR(N)/=7) THEN
          
          IF(IRB(KINET(N))==0) THEN 
              JWARN1 = JWARN1 + 1
              IK(1) = IK(1) + 1    
              NK(1,JWARN1) = ITAB(N)    
          ELSE
           DO I=1,NRBODY
             NSLRB = NPBY(2,I)
             DO J=1,NSLRB
               IF(LPBY(J)==N)THEN
                 IF(ICODT(NPBY(1,I))/=7.OR.ICODR(NPBY(1,I))/=7) THEN   
                   JWARN1 = JWARN1 + 1  
                   IK(1) = IK(1) + 1    
                   NK(1,JWARN1) = ITAB(NPBY(1,I))                     
                  ENDIF
               ENDIF
             ENDDO
            ENDDO

          ENDIF
         ELSE
        NKK=ITF(KINET(N))+IRB(KINET(N))+IRB2(KINET(N))+IRBM(KINET(N))+
     .     IWL(KINET(I))+IVF(KINET(I))+IRV(KINET(I))+IJO(KINET(I))+
     .     IRLK(KINET(I))

           IF(NKK>=1) THEN
               JWARN2 = JWARN2 + 1
             IF(ITF(KINET(N))/= 0) THEN
                IK(2) = IK(2) + 1  
                J = IK(2)  
                NK(2,J) = ITAB(N)              
             ELSEIF(IRB(KINET(N))/= 0) THEN
                IK(3) = IK(3) + 1  
                J = IK(3)  
                NK(3,J) = ITAB(N)  
             ELSEIF(IRB2(KINET(N))/= 0) THEN
                IK(3) = IK(3) + 1  
                J = IK(3)  
                NK(3,J) = ITAB(N)  
             ELSEIF(IRBM(KINET(N))/= 0)THEN
                IK(4) = IK(4) + 1  
                J = IK(4)  
                NK(4,J) = ITAB(N)  
             ELSEIF (IWL(KINET(I))/= 0)THEN
                IK(5) = IK(5) + 1  
                J = IK(5)  
                NK(5,J) = ITAB(N)  
             ELSEIF (IVF(KINET(I))/= 0)THEN
                IK(6) = IK(6) + 1  
                J = IK(6)  
                NK(6,J) = ITAB(N) 
             ELSEIF (IRV(KINET(I))/= 0)THEN
                IK(7) = IK(7) + 1  
                J = IK(7)  
               NK(7,J) = ITAB(N) 
             ELSEIF (IJO(KINET(I))/= 0)THEN
               IK(8) = IK(8) + 1  
               J = IK(8)  
               NK(8,J) = ITAB(N) 
            ELSEIF (IRLK(KINET(I))/= 0)THEN
               IK(9) = IK(9) + 1  
               J = IK(9)  
               NK(9,J) = ITAB(N) 
           ENDIF

         ENDIF

        ENDIF

      ENDDO
C
      IF(JWARN1/=0)THEN
          WRITE(IOUT,'(A,A)')
     . ' ** WARNING : THERMAL TIME STEP CALCULATION',
     . ' THESE NODES MUST BE BLOCKED'
          WRITE(IOUT,*) NK(1,1:IK(1))

            WRITE(ISTDO,'(A,A,I10,A)')
     . ' ** WARNING : THERMAL TIME STEP CALCULATION',
     . ' NODE(S) MUST BE BLOCKED',
     .  JWARN1,'WARNING(S)'
      ENDIF

      IF(JWARN2/=0)THEN
            WRITE(IOUT,'(A,A)')
     .'** WARNING : THERMAL TIME STEP CALCULATION
     .  POSSIBLE INCOMPATIBLE CONDITION(S)'
!            WRITE(ISTDO,'(A,A,I10,A)')
!     .'** WARNING : THERMAL TIME STEP CALCULATION'
!     .' POSSIBLE INCOMPATIBLE CONDITION(S)',
!     .  JWARN2,'WARNING(S)'
             WRITE(ISTDO, 1000) JWARN2
          IF(IK(2)/= 0) THEN
            WRITE(IOUT,'(A)')
     .     '             - INTERFACE TYPE 1 2 OR 9 FOR NODES : '
            WRITE(IOUT,*) NK(2,1:IK(2))  
          
          ELSEIF(IK(3)/= 0) THEN
            WRITE(IOUT,'(A)')
     .     '             - RIGID BODY FOR NODES'
            WRITE(IOUT,*) NK(3,1:IK(3))  
  
          ELSEIF(IK(4)/= 0)THEN
            WRITE(IOUT,*)
     .     '             - IMPOSED BODY VELOCITY FOR NODES : '
            WRITE(IOUT,*) NK(4,1:IK(4)) 

          ELSEIF (IK(5)/= 0)THEN
            WRITE(IOUT,*)
     .     '             - RIGID WALL FOR NODES'
            WRITE(IOUT,*) NK(5,1:IK(5))  

          ELSEIF (IK(6)/= 0)THEN
            WRITE(IOUT,*)
     .     '             - IMPOSED ACCELERATION, IMPOSED DISPLACEMENT
     . , IMPOSED VELOCITY FOR NODES : '
            WRITE(IOUT,*) NK(6,1:IK(6)) 

            WRITE(ISTDO,*)
     .     '             - IMPOSED ACCELERATION, IMPOSED DISPLACEMENT
     . , IMPOSED VELOCITY FOR NODES'
            WRITE(ISTDO,*) NK(6,1:IK(6)) 

          ELSEIF (IK(7)/= 0)THEN
            WRITE(IOUT,*)
     .     '             - RIVET FOR NODES'
            WRITE(IOUT,*) NK(7,1:IK(7)) 

          ELSEIF (IK(8)/= 0)THEN
            WRITE(IOUT,*)
     .     '             - CYLINDRICAL JOINT FOR NODES : '
            WRITE(IOUT,*) NK(8,1:IK(8)) 

          ELSEIF (IRLK(KINET(I))/= 0)THEN
            WRITE(IOUT,*)
     .     '             - RIGID LINK FOR NODES : '
            WRITE(IOUT,*) NK(9,1:IK(9)) 

          ENDIF
      ENDIF


 1000 FORMAT(1X,'** WARNING : THERMAL TIME STEP CALCULATION POSSIBLE INCOMPATIBLE CONDITION(S)',I10,1X,'WARNING(S)')

      RETURN
      END

C-------------------------------------------------------------------------------------
Ch       
Chd|====================================================================
Chd|  BCSDTTH_COPY                  source/constraints/general/bcs/bcsdtth.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCSDTTH_COPY(ICODT, ICODR, ICODT0, ICODR0,IFLAG)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLAG,
     .        ICODT(*),ICODR(*),ICODT0(*),ICODR0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NG, IG, ITY
C
C=======================================================================
      IF(IFLAG==1) THEN   ! copy initial Boundary conditions into temporal tab and constraint DDLS
       IF(IRODDL>0)THEN
         DO N=1,NUMNOD 
           ICODT0(N) =ICODT(N)   
           ICODR0(N) =ICODR(N) 
           ICODT(N)  =7   
           ICODR(N)  =7  
         ENDDO
       ELSE
         DO N=1,NUMNOD 
            ICODT0(N) =ICODT(N)   
            ICODT(N)  =7   
         ENDDO
       ENDIF

      ELSE  ! Recopy initial Boundary conditions before end 
       IF(IRODDL>0)THEN
         DO N=1,NUMNOD 
           ICODT(N) =ICODT0(N)   
           ICODR(N) =ICODR0(N)   
         ENDDO
       ELSE
         DO N=1,NUMNOD 
           ICODT(N) =ICODT0(N)   
         ENDDO
       ENDIF
      ENDIF
      RETURN
      END

