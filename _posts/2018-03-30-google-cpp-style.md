---
layout: post
title: C++ Style
excerpt: My C++ coding style based on Google C++ Style Guide
category: ["C++ language"]
modified: 2018-03-30
---
# Google C++ Style Digest

## 0. Introduction

This post essentially sorts out the most easy to understand items in [Google C++ Style Guide](https://google.github.io/styleguide/cppguide.html). It may contain some minor adjustment to google-style based on my personal taste, but no breaking and fundamental change involving C++ code quality is made.

## 1. Naming

> Names should be descriptive; avoid abbreviation.

### 1.1 Naming Terms

#### 1.1.1 Lower Underscore Naming

Names contain only lower-case letter and number, and use underscore `_` to seperate words.

e.g. `lower_underscore_naming`

#### 1.1.2 Upper Underscore Naming

Names contain only upper-case letter and number, and use underscore `_` to seperate words.

e.g. `UPPER_UNDERSCORE_NAMING`

#### 1.1.3 Capital Camel Case

Every starting letter of a word is capitalized, and no underscore is used. Abbreviation of multiple words is considered to be one word.

e.g. `CapitalCamelCase`, `TcpStream`

#### 1.1.4 Camel Case

Similar to capital camel case, but first character of a name is lower cased.

e.g. `camelCaseNaming`

### 1.2 File Name

- Use hyphen `-` to seperate words
- Extension of header file is `.h`
- Extension of c++ source file is `.cpp`

e.g. `my-important-class.h`, `my-important-class.cpp`

### 1.3 Type Name

Capital camel case. This rule applies to the following:

- class / struct
- type alias
- type template parameter

### 1.4 Variable Name

#### 1.4.1 Local Variable

Lower underscore naming. This rule applies to function parameter as well.

#### 1.4.2 Class/Struct Data Members

Lower underscore naming, but **private member** is additionally followed by one trailing underscore.

e.g.

``` cpp
class SomeStruct {
  public:
    int public_member;
  private:
    int private_member_;
};
```

#### 1.5 Constant

Capital camel case, but with a extra leading lower-case `k`. This rule applies to only compile time constant value like `constexpr`  value (so const reference doesn't count). Additionally, this rule applies to `enum` value.

e.g.

``` cpp
constexpr int kLengthOfArray = 40;
```

### 1.6 Function

Capital camel case. This rule applies to all function and class methods of all visibilities.

### 1.7 Namespace

Lower underscore naming.

### 1.8 Macro

Upper underscore naming.

### 1.9 Define Guard

Upper underscore naming with an extra trailing underscore `_`.

And naming should follow: `<PROJECT>_<PATH_TO_SOURCE>_H_`

e.g. `MYPROJECT_IO_LOGGER_H_`

## 2. Formatting

Use [clang format](https://clang.llvm.org/docs/ClangFormat.html). Personally I'm not a great fan of google style formatting and I customed my own clang-format style file which based on google style. But anyway, just adhere to a fix style in the code by using clang-format.

## 3. Classes & Structs

### Class or Struct

- `struct` emphasizes the type is merely a **aggregation of data** which should be very simple in most of the cases.

- Use `class` for general use of data abstraction.

### Access Control

Any non-compile-time-constant data member in a `class` should be kept `private`, or `protected` if you have strong reason to do so.

### Visibility Declaration

Every class declarations should starts with `public`, and then `protected` and `private`.

```
todo
```

## 4. Function

### Size of Function Body

Google style requires the body of function never exceed 40 lines. Practically, I believe length of most functions can be kept below 30 lines or less.

### Reference and Pointer

Google style recommend all reference should be `const` qualified, and type of output parameter should be non-const pointer.

e.g.

``` cpp
ErrorCode SomeFunc(const int& input_param, int* output_param) {
    // ...
}
```

### `auto` Function Return Type

`auto` return type should only be used when it's inconceivable not to do so (like returning lambda expression).

### `const` method

Mark class method as `const` whenever possible. And it's highly encouraged to declare `const` method.

```
todo
```

## 5. Misc

### C++ Standard

C++14 or higher, and avoid uses of deprecated c++ feature like `auto_ptr`.

### Integer Type

- Use `int` in normal use of integer.
- If certain byte size of integer is required, use types like `int32_t`, `int64_t` etc. in `<cstdint>` instead of `short` or `long`.

### Zero Values

Use `0` for integers, `0.0` for floating points, `nullptr` for pointers, and `'\0'` for characters.

### Global Variable

No global variable is allowed.

### Variable Scope

In source file, use anonymous namespace to enclose functions and variables that are never referenced elsewhere.

e.g.

``` cpp
// in something.cpp
#include "something.h"
namespace {
    void LocalFunction() {
        // ...
    }
}
// declared in something.h
void Something() {
    LocalFunction();
}
```

### Exception

No runtime exception throw is allowed.

### Pre-increment/decrement

Use pre-increment(decrement) operator whenever possible.

### Macro

Macro is not allowed to:

1. declaring compile-time const value
2. inlining computation ("macro function")

Both use cases can be replaced by using `constexpr` value and function.

e.g.

``` cpp
#define BAD 40

constexpr int kGood = 40;

#define BAD_FUNC(a, b) (a * b)

template <typename T>
constexpr T GoodFunc(T a, T b) {
    return a * b;
}
```

### Type Alias

Always use `using` keyword to declare type alias.

e.g.

``` cpp
typedef void (*BadCallback)(); // NO

using GoodCallBack = void(*)(); // YES
```

### Vector and Array

Use `std::vector` to represent sequence of value in normal cases, if static memory allocation is necessary, use `std::array` instead of built-in array.

### Memory Management

No explicit `new` and `delete` statement is allowed. Use `unique_ptr`, `shared_ptr` to declare ownership to object with auto memory management.

e.g.

``` cpp
void Bad() {
    int* p = new int(4);
    // use of p
    delete p;
}
void Good() {
    auto p = std::make_unique<int>(4);
}
```

### Value Initialization

Always initialize value in declaration. Including always initializing local value and initializing class member in initialization list and using default initialization syntax.

### Lambda Expression Capture

Always explicitly capture all variable. Avoid using implicit capture syntax of lambda.

e.g.

``` cpp
int some_var = 0;

auto bad_lambda = [&]() -> int {
    return some_var;
}
auto good_lambda = [&some_var]() -> int {
    return some_var;
}
```

```
todo
```