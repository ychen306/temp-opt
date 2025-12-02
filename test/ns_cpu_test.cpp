// RUN: %temp_opt %s -o - -- -std=c++17 $(../macos-clang-flags.sh) | %FileCheck %s

// CHECK: #include <algorithm>
// CHECK: #include <vector>
// CHECK: #include <string>
// CHECK: #include "user_headers.h"

// CHECK: int __tempopt_fn_outer__inner__add_int_(int a, int b) {

// CHECK: namespace outer {
// CHECK-NEXT: namespace inner {
// CHECK-NEXT: template<> struct Holder<int> {
// CHECK: } // namespace inner
// CHECK: } // namespace outer

// CHECK: template <typename T>
// CHECK-NEXT: T square(T x) {
// CHECK: template int square<int>(int);

// CHECK: template <typename T>
// CHECK-NEXT: struct Box {
// CHECK: template struct Box<int>;

// CHECK: int use_templates_in_ns() {
// CHECK:   int sum = __tempopt_fn_outer__inner__add_int_(sa, sb);
// CHECK:   std::vector<int> v = {sa, sb, sum};
// CHECK:   std::sort(v.begin(), v.end());
// CHECK:   outer::inner::Holder<int> h{v.back()};
// CHECK:   Box<int> bx{h.value};
