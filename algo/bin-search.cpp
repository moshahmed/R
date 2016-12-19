// what: search k in array x of n numbers
// returns index m : k==x[m] or -1 if not found.
// Changes GPL(C) moshahmed@gmail.com

bool bin_search(const int x[], const int n, const int k) {
  int left = 0, right = n-1;
  while (left <= right) {
    int m = (left+right)/2;
    if (x[m] == k) return m; // found.
    if (x[m] < k)
      left = m+1;  // search in  upper half.
    else
      right = m-1; // search in lower half.
  }
  return -1; // not found
} 

