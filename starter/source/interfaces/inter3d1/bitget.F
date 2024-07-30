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
      !||====================================================================
      !||    bitget           ../starter/source/interfaces/inter3d1/bitget.F
      !||--- called by ------------------------------------------------------
      !||    i20dst3          ../starter/source/interfaces/inter3d1/i20dst3.F
      !||    i20gap1          ../starter/source/interfaces/inter3d1/i20dst3.F
      !||    i20sti3          ../starter/source/interfaces/inter3d1/i20sti3.F
      !||    i24s1s2          ../starter/source/interfaces/inter3d1/i24buc1.F
      !||    i25neigh         ../starter/source/interfaces/inter3d1/i25neigh.F
      !||    i25s1s2          ../starter/source/interfaces/inter3d1/i25sto.F
      !||    i25trivox_edg    ../starter/source/interfaces/inter3d1/i25trivox_edg.F
      !||    i7remnode_init   ../starter/source/interfaces/inter3d1/i7remnode.F
      !||====================================================================
      INTEGER FUNCTION BITGET(I,N)
      INTEGER I,N
      INTEGER B,S,I2P(0:12)
      DATA I2P/1,2,4,8,16,32,64,128,256,512,1024,2048,4096/

      S = I/I2P(N)
      BITGET = S - (S/2)*2
      RETURN 
      END
      
      !||====================================================================
      !||    bitset        ../starter/source/interfaces/inter3d1/bitget.F
      !||--- called by ------------------------------------------------------
      !||    i20bord       ../starter/source/interfaces/inter3d1/i20surfi.F
      !||    i20dst3       ../starter/source/interfaces/inter3d1/i20dst3.F
      !||    i20edge1      ../starter/source/interfaces/inter3d1/i20surfi.F
      !||    i20gap1       ../starter/source/interfaces/inter3d1/i20dst3.F
      !||    i20sti3       ../starter/source/interfaces/inter3d1/i20sti3.F
      !||    i20surfi      ../starter/source/interfaces/inter3d1/i20surfi.F
      !||    i24edge1      ../starter/source/interfaces/inter3d1/i24surfi.F
      !||    i24edge2      ../starter/source/interfaces/inter3d1/i24surfi.F
      !||    i24surfi      ../starter/source/interfaces/inter3d1/i24surfi.F
      !||    i25neigh      ../starter/source/interfaces/inter3d1/i25neigh.F
      !||    i25surfi      ../starter/source/interfaces/inter3d1/i25surfi.F
      !||    inintsub      ../starter/source/interfaces/interf1/inintsub.F
      !||    inintsub_11   ../starter/source/output/subinterface/inintsub_11.F
      !||    inintsub_25   ../starter/source/output/subinterface/inintsub_25.F
      !||    inintsub_7    ../starter/source/output/subinterface/inintsub_7.F
      !||====================================================================
      INTEGER FUNCTION BITSET(I,N)
      INTEGER I,N
      INTEGER B,S,I2P(0:12)
      DATA I2P/1,2,4,8,16,32,64,128,256,512,1024,2048,4096/

      S = I/I2P(N)
      IF(S - (S/2)*2 == 0) I = I + I2P(N)
      BITSET = I
      RETURN 
      END

      !||====================================================================
      !||    bitunset   ../starter/source/interfaces/inter3d1/bitget.F
      !||--- called by ------------------------------------------------------
      !||    i20dst3    ../starter/source/interfaces/inter3d1/i20dst3.F
      !||    i20gap1    ../starter/source/interfaces/inter3d1/i20dst3.F
      !||    i20sti3    ../starter/source/interfaces/inter3d1/i20sti3.F
      !||====================================================================
      INTEGER FUNCTION BITUNSET(I,N)
      INTEGER I,N
      INTEGER B,S,I2P(0:12)
      DATA I2P/1,2,4,8,16,32,64,128,256,512,1024,2048,4096/

      S = I/I2P(N)
      IF(S - (S/2)*2 == 1) I = I - I2P(N)
      BITUNSET = I
      RETURN 
      END

