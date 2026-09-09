struct tree_node {
  int value;
  struct forest_cell *children;
};

struct forest_cell {
  struct tree_node *head;
  struct forest_cell *tail;
};

/*@
datatype tree {
  TNode {i32 value, datatype forest children}
}

datatype forest {
  FNil {},
  FCons {datatype tree head, datatype forest tail}
}

predicate [rec] (datatype tree) IsTree(pointer p) {
  take T = RW<struct tree_node>(p);
  take F = IsForest(T.children);
  return TNode {value: T.value, children: F};
}

predicate [rec] (datatype forest) IsForest(pointer p) {
  if (is_null(p)) {
    return FNil {};
  } else {
    take F = RW<struct forest_cell>(p);
    take T = IsTree(F.head);
    take Fs = IsForest(F.tail);
    return FCons {head: T, tail: Fs};
  }
}
@*/
