Doubly Linked List for Fortran 2003
====

Overview
This is a module to introduce a Doubly Linked List feature into Fortran 2003.

## Description
The List is based on the Object-Oriented Programming, 
so the List can treat all types Fortran supports including integer, real, character and derived-type by using the Unlimited Polymorphism feature.
The List also provide iterator feature for user's convenience.
This iterator allows you to write multiple loop.

## Demo
You can try the List by compiling linkedlist.f90 and execute it.

## Feature
*written in Fortran 2003 - you may not be staring in envy Java anymore
*based on the Object-Oriented Programming
*treat all types Fortran supports by the Unlimited Polymorphism feature
*provide iterator feature such as Java's List. the procedures next() and hasNext() are also available

## Requirement
*Recommended (perhaps necessary) compiler is Intel Fortran Composer XE 2013 update 5 or later.
  I am trying to compile PGI Fortran 14.9.

## Bugs
Calling subroutines related to remove an item from a list causes illegal memory access.
if you remove all items form a list, you can use the clear() subroutine.


## Author
[Tom](https://github.com/degawa)