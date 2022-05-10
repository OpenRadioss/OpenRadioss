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
Chd|  FVMESH0                       source/airbag/fvmesh0.F       
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FVBRIC                        source/airbag/fvbric.F        
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE FVMESH0(MONVOL, X, VOLMON, IXS)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MONVOL(*), IXS(NIXS,*)
      my_real
     .        X(3,*), VOLMON(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K1, KK1, KIBALE, IFV,
     .        N, ITYP, NNS, NTG, NBRIC, KI1, KI2, NBX, NBY, NNB, IMESH,
     .        NBA, NTGA, NNA, KIA1, KIA2, KIA3, KIA4, KIA5, KIA6,
     .        NNI, NTGI
C
      IFV=0
      K1=1
      KIBALE=1+NIMV*NVOLU+LICBAG+LIBAGJET+LIBAGHOL
      KK1=1
      DO N=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP.EQ.6.OR.ITYP.EQ.8) THEN
           IFV=MONVOL(K1-1+45)
           IMESH=MONVOL(K1-1+56)
           IF (IMESH.NE.0) THEN
            NNS=MONVOL(K1-1+32)
            NTG=MONVOL(K1-1+33)
            NNI =MONVOL(K1-1+68)
            NTGI=MONVOL(K1-1+69)
            NBA =MONVOL(K1-1+62)
            NTGA=MONVOL(K1-1+63)
            NNA =MONVOL(K1-1+64)
C
            KI1=KIBALE+MONVOL(K1-1+31)
            KI2=KI1+NNS+NNI
            KIA1=KI2+6*(NTG+NTGI)
            KIA2=KIA1+2*NBA
            KIA3=KIA2+12*NBA
            KIA4=KIA3+2*(NTG+NTGI)
            KIA5=KIA4+NNA
            KIA6=KIA5+3*NTGA
C
            NBX=MONVOL(K1-1+54)          
            NBY=MONVOL(K1-1+55)          
            NBRIC=NBX*NBY          
            NNB=(NBX+1)*(NBY+1)*2
            IF (ISPMD.NE.FVSPMD(IFV)%PMASTER-1) THEN
               ALLOCATE(FVDATA(IFV)%BRIC(8,0),
     .                  FVDATA(IFV)%TBRIC(13,0),
     .                  FVDATA(IFV)%XB(3,0),
     .                  FVDATA(IFV)%SFAC(6,4,0))
            ELSE
               ALLOCATE(FVDATA(IFV)%BRIC(8,NBRIC),
     .                  FVDATA(IFV)%TBRIC(13,NBRIC),
     .                  FVDATA(IFV)%XB(3,NNB),
     .                  FVDATA(IFV)%SFAC(6,4,NBRIC))
            ENDIF
C
            CALL FVBRIC(MONVOL(K1), VOLMON(KK1), MONVOL(KI1), X, NNS)
C
            MONVOL(K1-1+50)=MONVOL(K1-1+46)
            MONVOL(K1-1+51)=MONVOL(K1-1+47)
            MONVOL(K1-1+52)=MONVOL(K1-1+48)
            MONVOL(K1-1+53)=MONVOL(K1-1+49)
C
            CALL FVMESH1(
     . MONVOL(KI1), MONVOL(KI2), X,      MONVOL(K1), FVDATA(IFV)%BRIC,
     . FVDATA(IFV)%XB, VOLMON(KK1), NTG, NBRIC,      FVDATA(IFV)%TBRIC,
     . FVDATA(IFV)%SFAC, FVDATA(IFV)%DLH, NBA, NTGA, NNA,
     . MONVOL(KIA1), MONVOL(KIA2), MONVOL(KIA3), MONVOL(KIA4),
     . MONVOL(KIA5), MONVOL(KIA6), IXS,          NNS         )
C
            DEALLOCATE(FVDATA(IFV)%BRIC, FVDATA(IFV)%TBRIC,
     .                 FVDATA(IFV)%XB,   FVDATA(IFV)%SFAC )
          ENDIF
         ENDIF
         K1=K1+NIMV
         KK1=KK1+NRVOLU
      ENDDO
C
      RETURN
      END

