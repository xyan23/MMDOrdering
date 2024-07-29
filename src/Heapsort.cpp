#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix heapify_c(NumericMatrix points, int index, int heap_size) {

  // Left child index
  int l = 2 * index + 1; 
  // Right child index
  int r = 2 * index + 2; 
  int largest; 
  NumericVector key;

  // Make sure this is a complete binary tree
  // Compare between root and left child
  if (l < heap_size && points(l, 0) > points(index, 0)) {
    largest = l;
  } else {
    largest = index;
  }
  // Compare between largest and left child
  if (r < heap_size && points(r, 0) > points(largest, 0)) {
    largest = r;
  }
  // If largest is not root
  if (largest != index) {
    // Swap the larger child with parent
    key = points(index, _);
    points(index, _) = points(largest, _);
    points(largest, _) = key;
    // Recursively heapify for sub-tree
    points = heapify_c(points, largest, heap_size);
  }
  
  return(points);
}

// [[Rcpp::export]]
NumericMatrix build_heap_c(NumericMatrix points) {
  
  int heap_size = points.nrow();
  // Check all the parental nodes
  for (int i = floor(heap_size / 2); i >= 0; --i) {
    points = heapify_c(points, i, heap_size);
  }
  return(points);
}

// [[Rcpp::export]]
NumericMatrix heapsort_c(NumericMatrix points) {
  
  int n = points.nrow();
  points = build_heap_c(points);
  NumericVector key;
  for (int i = n - 1; i >= 1; --i) {
    // Exchange the first element with the ith element
    key = points(0, _);
    points(0, _) = points(i, _);
    points(i, _) = key;
    // Decrease the heap size by setting the heap size to i
    points = heapify_c(points, 0, i);
  }
  return(points);
}