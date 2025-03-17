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
#ifndef MV_ARRAY_UTILS_H
#define MV_ARRAY_UTILS_H


#include <UTILS/mv_algorithm.h>
#include <UTILS/memory_utils.h>

/* Class allowing to sort index-accessed data in an object.
* The concept is based on an ArrayDataType which can be instantiated giving an object of ObjectType
* (which contains the data to be sorted) and the index of the data in the object.
* Example for a use case where data in multiple parallel arrays is sorted: MODEL/mv_unresref.cpp.
*/
template <class ObjectType, class ArrayDataType> class MvArraySorter
{
public:
    // constructor.
    MvArraySorter(ObjectType object) : my_object(object) {}

    // Sort by first sorting an array of indexes, then updating the object.
    void sortItemsUsingIndexArray();

    // resort the array based on a (previously computed) array of new indeces.
    void resortItems(int index_nb, const int *index_old_from_index_new_a);

    // Comparison method, used by sortItemsUsingIndexArray().
    bool operator() (int i1, int i2)
    {
        ArrayDataType attdata1(&my_object, i1);
        ArrayDataType attdata2(&my_object, i2);
        return attdata1 < attdata2;
    }

private:
    ObjectType my_object;
};


typedef ptrdiff_t MvArrayDiff_t;

/* Class allowing to (random access) iterate through index-accessed data of an object.
* The concept is based on an ArrayDataType which can be instantiated giving an object of ObjectType
* (which contains the data to be sorted) and the index of the data in the object.
* Example for a use case for a binary search of data in multiple parallel arrays:
*   MODEL/mv_attribute_data_object.cpp.
*/

template <class ObjectType, class ArrayDataType> class MvArrayIterator_t : 
    public iterator<std::random_access_iterator_tag, ArrayDataType, MvArrayDiff_t>
{
public:
    MvArrayIterator_t() : my_data() {}
    MvArrayIterator_t(ObjectType *object_p, int index) : my_data(object_p, index) {}
    // The copy constructor of the iterator must not call the copy-constructor of the array data,
    // but the one "by pointing".
    MvArrayIterator_t(const MvArrayIterator_t &rhs) : my_data((ObjectType*)(rhs.my_data.my_object_p), rhs.getIndex()) {}
    // The operator= of the iterator must not call the operator= of the array data,
    // but just set object and index (i.e. update the data used "by pointing").
    MvArrayIterator_t & operator=(const MvArrayIterator_t &rhs) {
        my_data.setByPointing(rhs.my_data);
        return *this;}

    // Accessors
    ObjectType *       getObject()       { return my_data.getObject(); }
    const ObjectType * getObject() const { return my_data.getObject(); }
    int       &getIndex()       { return my_data.getIndex(); }
    const int &getIndex() const { return my_data.getIndex(); }
    void setIndex(int i)        { my_data.setIndex(i); }

    // Comparisons (we presuppose that both iterators work on the same attdataobj)
    bool operator<(const MvArrayIterator_t &rhs)  const  {return getIndex() < rhs.getIndex();}
    bool operator>(const MvArrayIterator_t &rhs)  const  {return getIndex() > rhs.getIndex();}
    bool operator<=(const MvArrayIterator_t &rhs) const  {return getIndex() <= rhs.getIndex();}
    bool operator>=(const MvArrayIterator_t &rhs) const  {return getIndex() >= rhs.getIndex();}
    bool operator==(const MvArrayIterator_t &rhs) const  {return getIndex() == rhs.getIndex();}
    bool operator!=(const MvArrayIterator_t &rhs) const  {return getIndex() != rhs.getIndex();}

    // Dereferencing
    ArrayDataType & operator*() {return my_data;}
    ArrayDataType * operator->() {return &my_data;}
    // The doc says that a random access iterator has to define the following, but we don't seem
    // to need them for what we are doing. In order to get a compiler warning if they are used,
    // we just declare but not define them, in order to prevent the compiler from creating them.
    ArrayDataType & operator[](MvArrayDiff_t incr);

    // Integer arithmetics, incrementing, decrementing
    // prefix ++: no parameter, returns a reference
    MvArrayIterator_t<ObjectType, ArrayDataType> & operator++()  {
        getIndex()++; my_data.InitValues(); return *this;}
    // postfix ++: dummy parameter, returns a value
    MvArrayIterator_t<ObjectType, ArrayDataType> operator++(int) {
        MvArrayIterator_t<ObjectType, ArrayDataType> temp=*this; getIndex()++; my_data.InitValues(); return temp;}
    MvArrayIterator_t<ObjectType, ArrayDataType> & operator--()  {
        getIndex()--; my_data.InitValues(); return *this;}
    MvArrayIterator_t<ObjectType, ArrayDataType> operator--(int) {
        MvArrayIterator_t<ObjectType, ArrayDataType> temp=*this; getIndex()--; my_data.InitValues(); return temp;}
    MvArrayIterator_t<ObjectType, ArrayDataType> & operator+=(MvArrayDiff_t incr) {
        getIndex()+=(int)incr; my_data.InitValues(); return *this;}
    MvArrayIterator_t<ObjectType, ArrayDataType> operator+(MvArrayDiff_t incr)    {
        return MvArrayIterator_t<ObjectType, ArrayDataType>(*this) += incr;}
    MvArrayIterator_t<ObjectType, ArrayDataType> & operator-=(MvArrayDiff_t incr) {
        getIndex()-=(int)incr; my_data.InitValues(); return *this;}
    MvArrayIterator_t<ObjectType, ArrayDataType> operator-(MvArrayDiff_t incr)    {
        return MvArrayIterator_t<ObjectType, ArrayDataType>(*this) -= incr;}
    MvArrayDiff_t operator-(const MvArrayIterator_t &rhs) {
        return (MvArrayDiff_t)(getIndex() - rhs.getIndex());}

    ArrayDataType my_data;
};


// Definition of methods. Could maybe be moved into .cpp file?!

template <class ObjectType, class ArrayDataType>
void MvArraySorter<ObjectType, ArrayDataType>::resortItems(int nb_index, const int *index_old_from_index_new_a)
{
    /* index_old_from_index_new contains for each index in the resorted array the original index in the
    * unsorted array. We could write
    * int index_old = index_old_from_index_new[index_new]. */
    // We first create the reverse array:
    int *index_new_from_index_old_a = (int*)mymalloc(nb_index * sizeof(int));
    for(int i = 0; i < nb_index; ++i) index_new_from_index_old_a[index_old_from_index_new_a[i]] = i;

    // Now we copy one by one
    int index_new = -1, index_old_start = 0, nb_copied = 0, index_old = -1;
    ArrayDataType source_copy, target_copy;
    while(nb_copied < nb_index)
    {
        // find a starting point, if necessary
        if(-1 == index_new)
        {
            // this means that the data at index_new is already new, because there was an
            // "unconnected group". We have to search a new start:
            // (... or it is just the first cycle)
            while((index_old_start < nb_index) && (-1 == index_new_from_index_old_a[index_old_start]))
            {
                index_old_start++;
            }
            if(index_old_start >= nb_index) break; // Security, but shouldn't get here
            index_old = index_old_start;
            index_old_start++; // for next time
            index_new = index_new_from_index_old_a[index_old];
            if(index_new != index_old) // make copy, not necessary if same
            {
                MvArrayIterator_t<ObjectType, ArrayDataType> source_iter(&my_object, index_old);
                source_copy = *source_iter;
            }
        }
        if(index_new != index_old) // actual copying, not necessary if same
        {
            MvArrayIterator_t<ObjectType, ArrayDataType> target_iter(&my_object, index_new);
            target_copy = *target_iter; // copy from index_new into a copy
            *target_iter = source_copy; // copy from source to index_new
            source_copy = target_copy; // update source_copy with target_copy for next cycle
        }
        nb_copied++;
        // ... we "mark" position index_new as "already new".
        index_new_from_index_old_a[index_old] = -1;
        // for next cycle increment indeces
        index_old = index_new;
        index_new = index_new_from_index_old_a[index_old];
    }

    myfree(index_new_from_index_old_a);
}

template <class ObjectType, class ArrayDataType>
void MvArraySorter<ObjectType, ArrayDataType>::sortItemsUsingIndexArray()
{
    int nb_items = my_object.getNbItems();
    // Fill array with indexes to resort
    int *index_a = (int*)mymalloc(nb_items * sizeof(int));
    for(int i=0; i<nb_items; i++) index_a[i]=i;
    // call std:sort, using comparison method of this
    sort(index_a, index_a+nb_items, *this);
    // resort / update the array data
    resortItems(nb_items, index_a);
    myfree(index_a);
}

#endif /* MV_ARRAY_UTILS_H */
