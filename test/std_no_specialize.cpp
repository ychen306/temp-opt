// RUN: %temp_opt %s -o - -- -std=c++17 $(../macos-clang-flags.sh) | %FileCheck %s

#include <algorithm>
#include <vector>

template<typename T>
T square(T v) {
  return v * v;
}

int use_std() {
  std::vector<int> v = {1, 2};
  int m = std::max(square(1), square(2));
  v.push_back(m);
  return v.back();
}

// CHECK: template<typename T>
// CHECK-NEXT: T square(T v) {
// CHECK: template int square<int>(int);
// CHECK-NOT: {{^}}template<>
