// RUN: %temp_opt %s -o - -- -std=c++17 | %FileCheck %s

namespace A {
namespace B {

template<typename T>
T inc(T v) {
  return v + 1;
}

} // namespace B
} // namespace A

using namespace A;
using namespace B;

int use() {
  return inc(41);
}

// CHECK: namespace A {
// CHECK-NEXT: namespace B {
// CHECK: template<typename T>
// CHECK-NEXT: T inc(T v) {
// CHECK: template int A::B::inc<int>(int);
// CHECK: using namespace A;
// CHECK-NEXT: using namespace B;
// CHECK: int use() {
// CHECK-NEXT:   return inc(41);

