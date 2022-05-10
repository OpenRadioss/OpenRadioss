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
C.../INTER22/

      COMMON /INTER22_i/  
     . NSUB22         ! Number of layer or number of effective Sides (2),one of them can be a disjoint union od several topologies
     .,IOUTP22        ! flag for debug output : -1 no output, 0: output from i22datainit.F
     .,INT22          ! Number of defined interfaces /INTER/TYPE22
     .,I22LEN         ! Buffer Size for intersected bricks
     .,I22LEN_L      
     .,NADJ_F         ! Number of adjacent master per face for additional neighbors
     .,I22_DEGENERATED! Tell if slave has a 0 volume.This need to activate specific pre-treatment to remove slave/slave adjacency.
     .,IDT_INT22      ! Tell if cinematic time step occurs during current cycle
     .,I22LAW37
     .,I22LAW51
     .,I22_ALEUL      ! Tell if used with ALE or EULER


      COMMON /INTER22_r/       
     . dbVOL
     .,dbMASS 
     .,CritMerge22                           ! Merge criteria (50%)
     .,CritDeMerge22                         ! Merge criteria (50%)
     .,CritDVol22                            ! For Continuity Criteria (25%)-Change In volume(Ratio)in case Master becomes slave 
     .,dt22_min ,dx22_min, v22_max, nCross22 ! Cinematic Time Step 
     .,DTFAC22       
     .,RATIO22                               ! RATIO used to activate adaptative merging :1.0 systematically, 
     .,JMULT22                               !                                            0.5:Vsub<50% Vglobal: 
                                             !                                            0.0 never
      INTEGER  
     . NSUB22
     .,IOUTP22 
     .,INT22  
     .,I22LEN
     .,I22LEN_L
     .,NADJ_F
     .,I22_DEGENERATED
     .,IDT_INT22  
     .,I22LAW37 
     .,I22LAW51 
     .,I22_ALEUL
  
       DOUBLE PRECISION dbVOL    , dbMASS   , CritMerge22, CritDeMerge22 , CritDVol22 , 
     .                  dt22_min , dx22_min , v22_max    , nCross22      , DTFAC22    ,
     .                  RATIO22  , JMULT22

