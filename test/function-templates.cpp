// RUN: %temp_opt %s -o - -- -std=c++17 | %FileCheck %s

// CHECK: {{^}}template<typename T>
// CHECK-NEXT: T my_max(T a, T b) {
// CHECK-NEXT:   return a >= b ? a : b;
// CHECK-NEXT: }
// CHECK-NOT: my_min
// CHECK: int my_abs(int a) {
// CHECK-NEXT:   return my_max(a, 0);
// CHECK-NEXT: }

template<typename T>
T my_max(T a, T b) {
  return a >= b ? a : b;
}

template<typename T>
T my_min(T a, T b) {
  return a >= b ? a : b;
}

int my_abs(int a) {
  return my_max(a, 0);
}
