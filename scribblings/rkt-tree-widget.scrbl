#lang scribble/manual
@require[@for-label[rkt-tree-widget
                    racket/base racket/class racket/gui/base racket/contract/base]]

@title{rkt-tree-widget}
@author{yjqww6}

@defmodule[rkt-tree-widget]

Yet another tree widget for Racket. It uses functional cursors to represent the nodes of the tree.

@section{Tree Widget}
@defclass[tree-widget% canvas% ()]{
 A @racket[canvas%]-based tree widget.
   
 @defconstructor/auto-super[([wheel-step exact-positive-integer? 3])]{
  The @racket[wheel-step] argument controls the speed of scrolling.
 }

 @defmethod[(get-root) root-cursor?]{
  Returns a cursor representing the root of the tree.
 }

 @defmethod[(append-item [c generic-cursor?] [v any/c] [expand? boolean? #f]) void?]{
  Appends an item @racket[v] to the children of @racket[c]. All the cursors acquired previously will be invalidated.
 }

 @defmethod[(prepend-item [c generic-cursor?] [v any/c] [expand? boolean? #f]) void?]{
  Prepends an item @racket[v] to the children of @racket[c]. All the cursors acquired previously will be invalidated.
 }

 @defmethod[(insert-item [c generic-cursor?] [i exact-nonnegative-integer?] [v any/c] [expand? boolean? #f]) void?]{
  Inserts an item @racket[v] as the @racket[i]th child of @racket[c]. All the cursors acquired previously will be invalidated.
 }

 @defmethod[(update-item [c generic-cursor?] [i exact-nonnegative-integer?] [v any/c]) void?]{
  Updates the @racket[i]th child of @racket[c] to @racket[v]. All the cursors acquired previously will be invalidated.
 }

 @defmethod[(delete-item [c generic-cursor?] [i exact-nonnegative-integer?]) void?]{
  Deletes the @racket[i]th child of @racket[c]. All the cursors acquired previously will be invalidated.
 }

 @defmethod[(expand-item [c generic-cursor?] [expand? boolean?]) void?]{
  Changes the expanding status of @racket[c] to @racket[expand?]. All the cursors acquired previously will be invalidated.
 }

 @defmethod[(reset-items) void]{
  Resets the tree to empty. All the cursors acquired previously will be invalidated.
 }

 @defmethod[(on-positions-changed) void?]{
  Called after the tree is modified.

  
  @italic{Default Implementation:} Calls @method[window<%> refresh].
 }

 @defmethod[(paint-item [c node-cursor?] [v any/c]
                        [x exact-nonnegative-integer?] [y exact-nonnegative-integer?])
            void?]{
  Paints the item @racket[v] represented by cursor @racket[c] at specific dc location.
 }

 @defmethod[(compute-item-size [v any/c])
            (values exact-positive-integer? exact-positive-integer? exact-nonnegative-integer?)]{
  Computes the width, height and children indentation of item @racket[v].

                                                              
  @italic{Default Implementation:} Returns @racket[(values 1 1 0)].                                                       
 }
                                                                                                
 @defmethod[(locate-item [x (or/c #f exact-nonnegative-integer?)] [y exact-nonnegative-integer?])
            (or/c #f node-cursor?)]{
  Finds out the item at specific location (dc coordinates).
  If @racket[x] is @racket[#f], only @racket[y] is considered.
 }

 @defmethod[(make-indices-cursor [indices (non-empty-listof exact-nonnegative-integer?)])
            indices-cursor?]{
  Constructs an cursor from @racket[indices], which should be only used to modify the tree immediately.
  Equivalent to
  @racketblock[(for/fold ([t (send this get-root)])
                         ([i (in-list indices)])
                 (cursor-get-child t i))]
  when used to modify the tree.
 }
 
}

@section{Functional Updating}
@defclass[tree-updater% object% ()]{
 @racket[tree-updater%] is used to building and updating @racket[tree-widget%] functionally.
 @defconstructor[([tree (is-a?/c tree-widget%)])]{
  Constructs a @racket[tree-updater%] associated with @racket[tree].
 }

 @defmethod[(append-item [t generic-cursor?] [v any/c]
                         [expand? boolean? #f] [children (or/c #f cursor?) #f])
            root-cursor?]{
 }
 @defmethod[(prepend-item [t generic-cursor?] [v any/c]
                          [expand? boolean? #f] [children (or/c #f cursor?) #f])
            root-cursor?]{
 }
 @defmethod[(insert-item [t generic-cursor?] [i exact-nonnegative-integer?] [v any/c]
                         [expand? boolean? #f] [children (or/c #f cursor?) #f])
            root-cursor?]{
 }
 @defmethod[(update-item [t generic-cursor?] [i exact-nonnegative-integer?] [v any/c])
            root-cursor?]{
 }
 @defmethod[(delete-item [t generic-cursor?] [i exact-nonnegative-integer?])
            root-cursor?]{
 }
 @defmethod[(expand-item [t generic-cursor?] [expand? boolean?]) root-cursor?]{
 }
 @defmethod[(update-children [t generic-cursor?] [f (-> root-cursor? root-cursor?)])
            root-cursor?]{
 }
 @defmethod[(set-tree [tree root-cursor?]) void?]{
  Set the tree of associated @racket[tree-widget%] to @racket[tree].
 }
 @defmethod[(empty-tree) root-cursor?]{
 }
}

@section{Cursor Operations}
@defproc[(root-cursor? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a root cursor, otherwise @racket[#f]. A root cursor represents the root of a tree.
}
@defproc[(node-cursor? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a node cursor, otherwise @racket[#f]. A node cursor represents an internal node of a tree.
}
@defproc[(cursor? [v any/c]) boolean?]{
 Returns @racket[#t] if either @racket[(root-cursor? v)] or @racket[(node-cursor? v)] return @racket[#t], otherwise @racket[#f].
}
@defproc[(indices-cursor? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a indices cursor, otherwise @racket[#f]. See also @method[tree-widget% make-indices-cursor].
}
@defproc[(generic-cursor? [v any/c]) boolean?]{
 Returns @racket[#t] if either @racket[(cursor? v)] or @racket[(indices-cursor? v)] return @racket[#t], otherwise @racket[#f].
}

@defproc[(cursor-up [c cursor?]) cursor?]{
 Returns a cursor representing the parent node of @racket[c]. If @racket[c] is a root cursor, returns itself.
}
@defproc[(cursor-equal? [a cursor?] [b cursor?]) boolean?]{
 Returns @racket[#t] if @racket[a] and @racket[b] represent same node (or same root), otherwise @racket[#f].
}
@defproc[(cursor-valid? [t root-cursor?] [c cursor?]) boolean?]{
 Returns @racket[#t] if @racket[t] is the root of @racket[c], otherwise @racket[#f].
}
@defproc[(cursor-children [c cursor?]) (listof node-cursor?)]{
 Returns the children of @racket[c].
}
@defproc[(cursor-children-cursor [c cursor?]) root-cursor?]{
 Similar to @racket[cursor-children], but returns a root cursor instead.
}
@defproc[(cursor-children-count [c cursor?]) exact-nonnegative-integer?]{
 Returns the the number of children of @racket[c].
}
@defproc[(cursor-get-child [c cursor?] [i exact-nonnegative-integer?]) node-cursor?]{
 Returns the @racket[i]th child of @racket[c].
}
@defproc[(node-cursor-item-size [c node-cursor?]) (values exact-positive-integer? exact-positive-integer?)]{
 Returns the width and height of the item of @racket[c].
}
@defproc[(node-cursor-value [c node-cursor?]) any/c]{
 Returns the item value of @racket[c].
}
@defproc[(node-cursor-expand? [c node-cursor?]) boolean?]{
 Returns @racket[#t] if @racket[c] is expanded, otherwise @racket[#f].
}
@defproc[(node-cursor-pos [c node-cursor?]) exact-nonnegative-integer?]{
 Returns the the position of @racket[c] in its parent.
}
@defproc[(node-cursor-children-indent [c node-cursor?]) exact-nonnegative-integer?]{
 Returns the chilren indentation of @racket[c].
}
@defproc[(root-cursor-total-size [c root-cursor?])
         (values exact-nonnegative-integer? exact-nonnegative-integer?)]{
 Returns the total size required by @racket[c].
}
@defproc[(root-cursor-locate-item [c root-cursor?] [x (or/c #f exact-nonnegative-integer?)] [y exact-nonnegative-integer?])
         (or/c #f node-cursor?)]{
 Similar to @method[tree-widget% locate-item], but @racket[x] and @racket[y] are relative to @racket[c].
}
@defproc[(root-cursor-get-visible-items [c root-cursor?] [y-start exact-nonnegative-integer?] [y-end exact-nonnegative-integer?])
         (listof (vector/c node-cursor? exact-nonnegative-integer? any/c))]{
 Returns a list of visible items (flattened) in [@racket[y-start], @racket[y-end]) (relative to @racket[c]).
 Vectors in that list contains the cursor, the y location, and the value of correspoding node.
}

