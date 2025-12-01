// RUN: %temp_opt -o - %s -- -std=c++17 | %FileCheck %s

// CHECK: {{^}}template<typename T>
// CHECK-NEXT: struct Used {
// CHECK-NOT: struct Unused

template<typename T>
struct Used {
  T value;
};

template<typename T>
struct Unused {
  T value;
};

Used<int> u;
