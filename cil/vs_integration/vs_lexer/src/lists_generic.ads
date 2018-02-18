GENERIC

  TYPE ElementType IS PRIVATE;

PACKAGE Lists_Generic IS
------------------------------------------------------------------------
--| Generic ADT for one-way linked lists
--| Author: Michael B. Feldman, The George Washington University 
--| Last Modified: January 1996                                     
------------------------------------------------------------------------

 -- exported types

  TYPE Position IS PRIVATE;
  TYPE List IS LIMITED PRIVATE;

  -- exported exceptions

  OutOfSpace: EXCEPTION;  -- raised if no space left for a new node
  PastEnd   : EXCEPTION;  -- raised if a Position is past the end
  PastBegin : EXCEPTION;  -- raised if a Position is before the begin
  EmptyList : EXCEPTION;

  -- basic constructors

  PROCEDURE Initialize(L: IN OUT List);
  -- Pre:  none
  -- Post: L is initialized. If L contained nodes, these are deleted.

  PROCEDURE AddToFront(L: IN OUT List; X: ElementType);
  PROCEDURE AddToRear (L: IN OUT List; X: ElementType);
  -- Pre:  L and X are defined
  -- Post: a node containing X is inserted 
  --   at the front or rear of L, respectively

  -- basic selectors

  FUNCTION First (L: List) RETURN Position;
  FUNCTION Last  (L: List) RETURN Position;
  -- Pre:  L is defined
  -- Post: returns the position of the first or last node 
  --   of L, respectively; return NULL if L is empty

  FUNCTION Retrieve (L: IN List; P: IN Position) RETURN ElementType;
  -- Pre:    L and P are defined; P designates a node in L
  -- Post:   returns the value of the element at position P
  -- Raises: EmptyList if L is empty
  --         PastBegin if P points before the beginning of L
  --         PastEnd   if P points beyond the end of L

  -- other constructors

  PROCEDURE Insert (L: IN OUT List; X: ElementType; P: Position);
  -- Pre:  L, X, and P are defined; P designates a node in L
  -- Post: X is inserted into L at position P; equivalent to
  --   AddToRear if P is NULL

  PROCEDURE Replace (L: IN OUT List; X: ElementType; P: Position);
  -- Pre:    L, X, and P are defined; P designates a node in L
  -- Post:   X replace the element in L at position P
  -- Raises: PastEnd if P is NULL

  PROCEDURE Delete (L: IN OUT List; P: Position);
  -- Pre:    L and P are defined; P designates a node in L
  -- Post:   the node at position P of L is deleted
  -- Raises: EmptyList if L is empty
  --         PastBegin if P is NULL

  PROCEDURE Copy (To: IN OUT List; From: IN List);
  -- Pre:  From is defined
  -- Post: To is a list whose elements are the same as those
  --   of From, in the same order.

  -- iterator operations

  PROCEDURE GoAhead (L: List; P: IN OUT Position);
  -- Pre:    L and P are defined; P designates a node in L
  -- Post:   P is advanced to designate the next node of L
  -- Raises: EmptyList if L is empty
  --         PastEnd   if P points beyond the end of L

  PROCEDURE GoBack    (L: List; P: IN OUT Position);
  -- Pre:    L and P are defined; P designates a node in L
  -- Post:   P is moved to designate the previous node of L
  -- Raises: EmptyList if L is empty
  --         PastBegin if P points beyond the end of L

  -- inquiry operators

  FUNCTION  IsEmpty   (L: List) RETURN Boolean;
  FUNCTION  IsFirst   (L: List; P: Position) RETURN Boolean;
  FUNCTION  IsLast    (L: List; P: Position) RETURN Boolean;
  FUNCTION  IsPastEnd (L: List; P: Position) RETURN Boolean;
  FUNCTION  IsPastBegin (L: List; P: Position) RETURN Boolean;
  -- Pre:    L and P are defined
  -- Post:   return True iff the condition is met; False otherwise

PRIVATE

  TYPE Node;
  TYPE Position IS ACCESS Node;

  TYPE Node IS RECORD
    Info: ElementType;
    Link: Position;
  END RECORD;

  TYPE List IS RECORD
    Head: Position;
    Tail: Position;
  END RECORD;

END Lists_Generic;
