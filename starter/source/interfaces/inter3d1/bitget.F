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
Chd|  BITGET                        source/interfaces/inter3d1/bitget.F
Chd|-- called by -----------
Chd|        I20DST3                       source/interfaces/inter3d1/i20dst3.F
Chd|        I20GAP1                       source/interfaces/inter3d1/i20dst3.F
Chd|        I24S1S2                       source/interfaces/inter3d1/i24buc1.F
Chd|        I25NEIGH                      source/interfaces/inter3d1/i25neigh.F
Chd|        I25S1S2                       source/interfaces/inter3d1/i25sto.F
Chd|        I25TRIVOX_EDG                 source/interfaces/inter3d1/i25trivox_edg.F
Chd|        I7REMNODE_INIT                source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION BITGET(I,N)
      INTEGER I,N
      INTEGER B,S,I2P(0:12)
      DATA I2P/1,2,4,8,16,32,64,128,256,512,1024,2048,4096/

      S = I/I2P(N)
      BITGET = S - (S/2)*2
      RETURN
      END

Chd|====================================================================
Chd|  BITSET                        source/interfaces/inter3d1/bitget.F
Chd|-- called by -----------
Chd|        I20BORD                       source/interfaces/inter3d1/i20surfi.F
Chd|        I20EDGE1                      source/interfaces/inter3d1/i20surfi.F
Chd|        I20STI3                       source/interfaces/inter3d1/i20sti3.F
Chd|        I20SURFI                      source/interfaces/inter3d1/i20surfi.F
Chd|        I24EDGE1                      source/interfaces/inter3d1/i24surfi.F
Chd|        I24SURFI                      source/interfaces/inter3d1/i24surfi.F
Chd|        I25NEIGH                      source/interfaces/inter3d1/i25neigh.F
Chd|        I25SURFI                      source/interfaces/inter3d1/i25surfi.F
Chd|        ININTSUB_11                   source/output/subinterface/inintsub_11.F
Chd|        ININTSUB_25                   source/output/subinterface/inintsub_25.F
Chd|        ININTSUB_7                    source/output/subinterface/inintsub_7.F
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION BITSET(I,N)
      INTEGER I,N
      INTEGER B,S,I2P(0:12)
      DATA I2P/1,2,4,8,16,32,64,128,256,512,1024,2048,4096/

      S = I/I2P(N)
      IF(S - (S/2)*2 == 0) I = I + I2P(N)
      BITSET = I
      RETURN
      END

Chd|====================================================================
Chd|  BITUNSET                      source/interfaces/inter3d1/bitget.F
Chd|-- called by -----------
Chd|        I20STI3                       source/interfaces/inter3d1/i20sti3.F
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION BITUNSET(I,N)
      INTEGER I,N
      INTEGER B,S,I2P(0:12)
      DATA I2P/1,2,4,8,16,32,64,128,256,512,1024,2048,4096/

      S = I/I2P(N)
      IF(S - (S/2)*2 == 1) I = I - I2P(N)
      BITUNSET = I
      RETURN
      END

