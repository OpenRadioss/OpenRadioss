/*Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/
#ifndef MV_COMPARE_H
#define MV_COMPARE_H



#include <UTILS/mv_string.h>





/** @name Comparing entity to entity */
//@{
/// Comparing integers
bool MV_compare(int lvalue,const string &comparator,int rvalue);
/// Comparing floats
bool MV_compare(double lvalue,const string &comparator,double rvalue);
/// Comparing strings
bool MV_compare(const string &lvalue,const string &comparator,const string &rvalue);
//@}

/** @name Comparing entity to string */
//@{
/// Comparing integers
bool MV_compare(int lvalue,const string &comparator,const string &rvalue);
/// Comparing floats
bool MV_compare(double lvalue,const string &comparator,const string &rvalue);
/*
/// Comparing strings
bool MV_compare(const string &lvalue,const string &comparator,const string &rvalue);
*/
//@}




#endif //MV_COMPARE_H




