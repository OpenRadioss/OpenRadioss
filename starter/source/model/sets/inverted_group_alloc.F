Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    inverted_group_alloc      ../starter/source/model/sets/inverted_group_alloc.F
!||--- called by ------------------------------------------------------
!||    compute_connect_partelm   ../starter/source/model/sets/compute_connect_partelm.F
!||--- uses       -----------------------------------------------------
!||    inverted_group_mod        ../starter/share/modules1/inverted_group_mod.F
!||====================================================================
        SUBROUTINE INVERTED_GROUP_ALLOC(INV_GROUP)
!$COMMENT
!       INVERTED_GROUP_ALLOC description
!       allocation of the different arrays of INV_GROUP structure
!       
!       INVERTED_GROUP_ALLOC organization :
!           none
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE INVERTED_GROUP_MOD
      use my_alloc_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(INVERTGROUP_STRUCT_) :: INV_GROUP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: MY_SIZE
!   ----------------------------------------
        INV_GROUP%IS_USED = .TRUE.
        INV_GROUP%ALREADY_ALLOC = .TRUE.

        MY_SIZE = INV_GROUP%SIZE_INDPART
        call my_alloc(INV_GROUP%INDPARTS, MY_SIZE, "INV_GROUP%INDPARTS")
        call my_alloc(INV_GROUP%INDPARTQ, MY_SIZE, "INV_GROUP%INDPARTQ")
        call my_alloc(INV_GROUP%INDPARTC, MY_SIZE, "INV_GROUP%INDPARTC")
        call my_alloc(INV_GROUP%INDPARTT, MY_SIZE, "INV_GROUP%INDPARTT")
        call my_alloc(INV_GROUP%INDPARTP, MY_SIZE, "INV_GROUP%INDPARTP")
        call my_alloc(INV_GROUP%INDPARTTG, MY_SIZE, "INV_GROUP%INDPARTTG")
        call my_alloc(INV_GROUP%INDPARTTRIA, MY_SIZE, "INV_GROUP%INDPARTTRIA")
        call my_alloc(INV_GROUP%INDPARTR, MY_SIZE, "INV_GROUP%INDPARTR")
        call my_alloc(INV_GROUP%INDPARTSPH, MY_SIZE, "INV_GROUP%INDPARTSPH")

        INV_GROUP%INDPARTS( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTQ( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTC( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTT( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTP( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTTG( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTTRIA( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTR( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTSPH( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_S
        call my_alloc(INV_GROUP%PARTS, MY_SIZE, "INV_GROUP%PARTS")
        INV_GROUP%PARTS( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_Q
        call my_alloc(INV_GROUP%PARTQ, MY_SIZE, "INV_GROUP%PARTQ")
        INV_GROUP%PARTQ( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_C
        call my_alloc(INV_GROUP%PARTC, MY_SIZE, "INV_GROUP%PARTC")
        INV_GROUP%PARTC( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_T
        call my_alloc(INV_GROUP%PARTT, MY_SIZE, "INV_GROUP%PARTT")
        INV_GROUP%PARTT( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_P
        call my_alloc(INV_GROUP%PARTP, MY_SIZE, "INV_GROUP%PARTP")
        INV_GROUP%PARTP( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_TG
        call my_alloc(INV_GROUP%PARTTG, MY_SIZE, "INV_GROUP%PARTTG")
        INV_GROUP%PARTTG( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_TRIA
        call my_alloc(INV_GROUP%PARTTRIA, MY_SIZE, "INV_GROUP%PARTTRIA")
        INV_GROUP%PARTTRIA( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_R
        call my_alloc(INV_GROUP%PARTR, MY_SIZE, "INV_GROUP%PARTR")
        INV_GROUP%PARTR( 1:MY_SIZE ) = 0      

        MY_SIZE = INV_GROUP%SIZE_SPH
        call my_alloc(INV_GROUP%PARTSPH, MY_SIZE, "INV_GROUP%PARTSPH")
        INV_GROUP%PARTSPH( 1:MY_SIZE ) = 0
!   ----------------------------------------
        RETURN
        END SUBROUTINE INVERTED_GROUP_ALLOC
