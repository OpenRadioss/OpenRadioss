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
Chd|  WEIGHT0                       source/elements/sph/weight.F  
Chd|-- called by -----------
Chd|        SPCOMPL                       source/elements/sph/spcompl.F 
Chd|        SPGAUGE                       source/elements/sph/spgauge.F 
Chd|        SPLISSV                       source/elements/sph/splissv.F 
Chd|        SPMD_SPHGETD                  source/mpi/elements/spmd_sph.F
Chd|        SPONFV                        source/elements/sph/sponfv.F  
Chd|        SPSTABW                       source/elements/sph/spstab.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WEIGHT0(XI,YI,ZI,XJ,YJ,ZJ,H,W)
C-------------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      my_real
     .   XI,YI,ZI,XJ,YJ,ZJ,H,W
C----------------------------------------------------------
      my_real
     .   R,RH,RH2,RH3,RHM,RHM3
C=======================================================================
      IF (H==ZERO) STOP
C-----
      R =SQRT((XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ))
C-----
      IF (R<=H) THEN
        RH  =R/H
        RH2 =RH*RH
        RH3 =RH2*RH
        W   =(ONE - THREE_HALF*RH2+THREE_OVER_4*RH3)*INVPI/(H*H*H)
      ELSEIF (R<=TWO*H) THEN
        RHM =TWO-R/H
        RHM =RHM/H
        RHM3=RHM*RHM*RHM
        W   =FOURTH*RHM3*INVPI
      ELSE
        W   =ZERO
      ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  WEIGHT1                       source/elements/sph/weight.F  
Chd|-- called by -----------
Chd|        SPCOMPL                       source/elements/sph/spcompl.F 
Chd|        SPDENS                        source/elements/sph/spdens.F  
Chd|        SPFORCP                       source/elements/sph/spforcp.F 
Chd|        SPGRADT                       source/elements/sph/sptemp.F  
Chd|        SPLAPLT                       source/elements/sph/sptemp.F  
Chd|        SPMD_SPHGETD                  source/mpi/elements/spmd_sph.F
Chd|        SPONFV                        source/elements/sph/sponfv.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WEIGHT1(XI,YI,ZI,XJ,YJ,ZJ,H,W,WGRAD)
C-------------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      my_real
     .   XI,YI,ZI,XJ,YJ,ZJ,H,W,WGRAD(3)
C----------------------------------------------------------
      my_real
     .   R,RH,RH2,RH3,IH3,RHM,RHM2,RHM3,WPRIMR
C=======================================================================
      IF (H==ZERO) STOP
C-----
      R =SQRT((XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ))
C-----
      IF (R<=H) THEN
        RH  =R/H
        RH2 =RH*RH
        RH3 =RH2*RH
        IH3 =INVPI/(H*H*H)
        W      =(ONE - THREE_HALF*RH2+THREE_OVER_4*RH3)*IH3
        WPRIMR =(-THREE+TWOP25*RH)*IH3/(H*H)
      ELSEIF (R<=TWO*H) THEN
        RHM =TWO - R/H
        RHM =RHM/H
        RHM2=RHM*RHM*INVPI
        RHM3=RHM2*RHM
        W      = FOURTH*RHM3
        WPRIMR =-THREE_OVER_4*RHM2/(H*H*R)
      ELSE
        W      =ZERO
        WPRIMR =ZERO	
      ENDIF
      WGRAD(1)=WPRIMR*(XI-XJ)
      WGRAD(2)=WPRIMR*(YI-YJ)
      WGRAD(3)=WPRIMR*(ZI-ZJ)
      RETURN
      END

