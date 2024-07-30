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
      !||    random_mod       ../starter/share/modules1/random_mod.F
      !||--- called by ------------------------------------------------------
      !||    execargcheck     ../starter/source/starter/execargcheck.F
      !||    hm_preread_eig   ../starter/source/general_controls/computation/hm_read_eig.F
      !||    hm_read_eig      ../starter/source/general_controls/computation/hm_read_eig.F
      !||    hm_read_rand     ../starter/source/general_controls/computation/hm_read_rand.F
      !||    init_random      ../starter/source/general_controls/computation/init_random.F
      !||    lectur           ../starter/source/starter/lectur.F
      !||====================================================================
        MODULE RANDOM_MOD
!$COMMENT
!       RANDOM_MOD description
!       module for -rxalea / -rseed command line option
!       allowing to run a starter with /RANDOM option
!       without input deck modification 
!       
!       RANDOM_MOD organization :
!       - CMD : logical, true if -rxalea or -rseed are used
!       - ALEA_NBR : double, value for rxalea
!       - SEED_NBR : double, value for rseed
!       - ALEA : logical, true if -rxalea is used
!       - SEED : logical, true if -rseed is used
!$ENDCOMMENT
            TYPE RANDOM_STRUCT
                LOGICAL :: CMD
                REAL(kind=8) :: ALEA_NBR,SEED_NBR
                LOGICAL :: ALEA,SEED
            END TYPE RANDOM_STRUCT
            type(RANDOM_STRUCT) :: RAND_STRUCT
        END MODULE RANDOM_MOD
