//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2022 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.    
#include "IntVector.hpp"

#define _FCALL

// Interface

extern "C"
{
  void intvector_create_(std::vector<int>**);
  void intvector_delete_(std::vector<int>**);
  void intvector_clear_(std::vector<int>**);
  void intvector_push_back_(std::vector<int>**, const int*);
  void intvector_get_size_(std::vector<int>**, int*);
  void intvector_get_redundant_(std::vector<int>**, int*, int*, int*);

  void _FCALL INTVECTOR_CREATE(std::vector<int>** vec_ptr)
  {
    intvector_create_(vec_ptr);
  }
  void _FCALL INTVECTOR_DELETE(std::vector<int>** vec_ptr)
  {
    intvector_delete_(vec_ptr);
  }
  void _FCALL INTVECTOR_CLEAR(std::vector<int>** vec_ptr)
  {
    intvector_clear_(vec_ptr);
  }
  void _FCALL INTVECTOR_PUSH_BACK(std::vector<int>** vec_ptr, const int* i)
  {
    intvector_push_back_(vec_ptr, i);
  }
  void _FCALL INTVECTOR_GET_SIZE(std::vector<int>** vec_ptr, int* i)
  {
    intvector_get_size_(vec_ptr, i);
  }
  void _FCALL INTVECTOR_GET_REDUNDANT(std::vector<int>** vec_ptr, int* res, int* err, int* card)
  {
    intvector_get_redundant_(vec_ptr, res, err, card);
  }
}

// Create
void intvector_create_(std::vector<int>** vec_ptr)
{
  *vec_ptr = new std::vector<int> ();
}
// Delete
void intvector_delete_(std::vector<int>** vec_ptr)
{
  delete(*vec_ptr);
  *vec_ptr = nullptr;
}
// Clear
void intvector_clear_(std::vector<int>** vec_ptr)
{
  (*vec_ptr)->clear();
}
// Push_back
void intvector_push_back_(std::vector<int>** vec_ptr, const int* i)
{
  (*vec_ptr)->push_back(*i);
}
// Get size
void intvector_get_size_(std::vector<int>** vec_ptr, int* i)
{
  *i = (*vec_ptr)->size();
}
// Get the redundant element
void intvector_get_redundant_(std::vector<int>** vec_ptr, int* res, int* err, int* card)
{
  auto correct_count = [&vec_ptr,&card](int ii) {return (*card == count((*vec_ptr)->begin(), (*vec_ptr)->end(), ii));};
  auto iter = find_if((*vec_ptr)->begin(), (*vec_ptr)->end(), correct_count);
  if (iter != (*vec_ptr)->end()) {
    *res = *iter;
    *err = 0;
  } else {
    *res = 0;
    *err = 1;
  }
}
