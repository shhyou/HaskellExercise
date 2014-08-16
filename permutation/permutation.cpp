#include <cstdio>
#include <algorithm>

const int N = 4;

int ans[N];

bool permute(int n, int arr[]) {
  int i = 0;
  for (i = n-1; i > 0; --i)
    if (arr[i-1] < arr[i])
      break;
  if (i == 0)
    return false;
  int j = i;
  for (int k = i; k < n; ++k)
    if (arr[i-1]<arr[k] && arr[k]<arr[j])
      j = k;
  std::swap(arr[i-1], arr[j]);
  for (int k=i,l=n-1; k < l; ++k,--l)
    std::swap(arr[k], arr[l]);
  return true;
}

int main() {
  for (int i = 0; i < N; ++i)
    ans[i] = i+1;
  do {
    for (int i = 0; i < N; ++i)
      std::printf("%d ", ans[i]);
    std::putchar('\n');
  } while (permute(N, ans));
  return 0;
}