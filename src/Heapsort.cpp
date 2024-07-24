#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix heapify_c(NumericMatrix A, int i, int heap_size) {
  A = clone(A);
  // Left child index
  int l = 2 * i + 1;
  // Right child index
  int r = 2 * i + 2;
  int largest;
  NumericVector key;

  // Make sure this is a complete binary tree
  // Compare between root and left child
  if (l < heap_size && A(l, 0) > A(i, 0)) {
    largest = l;
  } else {
    largest = i;
  }
  // Compare between largest and left child
  if (r < heap_size && A(r, 0) > A(largest, 0)) {
    largest = r;
  }
  // If largest is not root
  if (largest != i) {
    // Swap the larger child with parent
    key = A(i, _);
    A(i, _) = A(largest, _);
    A(largest, _) = key;
    // Recursively heapify for sub-tree
    A = heapify_c(A, largest, heap_size);
  }

  return(A);
}

// [[Rcpp::export]]
NumericMatrix build_heap_c(NumericMatrix A) {
  A = clone(A);
  int heap_size = A.nrow();
  // Check all the parental nodes
  for (int i = floor(heap_size / 2); i >= 0; --i) {
    A = heapify_c(A, i, heap_size);
  }
  return(A);
}

// [[Rcpp::export]]
NumericMatrix heapsort_c(NumericMatrix A) {
  A = clone(A);
  int n = A.nrow();
  A = build_heap_c(A);
  NumericVector key;
  for (int i = n - 1; i >= 1; --i) {
    // Exchange the first element with the ith element
    key = A(0, _);
    A(0, _) = A(i, _);
    A(i, _) = key;
    // Decrease the heap size by setting the heap size to i
    A = heapify_c(A, 0, i);
  }
  return(A);
}
