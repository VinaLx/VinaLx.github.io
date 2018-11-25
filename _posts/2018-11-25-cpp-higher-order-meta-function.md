---
layout: post
title: C++ Higher-order Meta-function
excerpt: Some simple template tricks
category: ["C++ language", "template"]
modified: 2018-11-26
---

Today [@yb](https://github.com/YanB25) and I were talking about about his compile-time quicksort implementation, and then we were talking about simple currying of his "compile-time less-than" operator. Although in modern C++, we can almost always use `constexpr` function (essentially since C++14) and even `constexpr` lamdba(since C++17) to meet similar requirements easier. But it should be more fun to mess around with old-school templates (:EvilSmile:).

## Prelude

### What is function?

The function we're mentioning is a mapping of _parameter(s)_ to the _result_. and it's (supposed to be) side-effect-free (pure), that is, you always get the same result when you provide same parameters to the same function.

### What is higher-order function?

If either types of parameters or return type of a certain function is another function type. The function is considered a higher-order function. I believe we all wrote codes like this:

```cpp
bool GreaterThan(int i, int j) {
    return i > j;
}
void SortDescend(std::vector<int>& v) {
    std::sort(begin(v), end(v), &GreaterThan);
}
```

Here `std::sort` can be considered a higher-order function (not really, since it isn't pure) since the third parameter is a function.

### What is meta-function?

When we refer to "meta-function" in C++, we're usually talking about templates.

Parameters of meta-function is type (or non-type) parametere of templates. And the return value of meta-function is the dependent name inside template. For example:

```cpp
// normal function
int Plus(int lhs, int rhs) {
    return lhs + rhs;
}
// meta function
// parameters are both compile-time constants
template <int lhs, int rhs>
struct Plus {
    static constexpr int value = lhs + rhs;
};

assert(Plus(1, 1) == 2);

static_assert(Plus<1, 1>::value == 2);
```

You may think it very stupid to write so much more codes to achieve the same effect with meta-function. Well, yes and no.

#### "Yes"

- Currently non-type template parameter only support integral types, so the parameter template can process is very limited. (After C++20, string constants can also be template parameter).
- [`constexpr` function after C++14](https://en.cppreference.com/w/cpp/language/constexpr) is MUCH more convenient to process compile time values.

#### "No"

- ~~Messing around template itself is a lot of fun.~~
- Usually the purpose of using template is not to process "value"s, but "types", for example:

```cpp
template <typename A, typename B>
struct IsSameAs {
    static constexpr bool value = false;
};

// specialization of `IsSame<>`
template <typename A>
struct IsSameAs<A, A> {
    static constexpr bool value = true;
};

// invalid C++ below, only for illustration purpose
bool TypesAreTheSame(Type a, Type b) {
    return a == b;
}
```

Well, you may ask whether type function is useful at all. That would be a very big topic itself so I won't explain here, but the answer is yes.

Most examples in this post, for the sake of clarity, are integer operations. But the idea is the same for type operations.

### [`std::integral_constant`](https://en.cppreference.com/w/cpp/types/integral_constant)

This struct defined in `<type_traits>` may become very handy when defining meta-function, and we will be using that a lot in a moment. Here's an equivalent implementation:

```cpp
template <typename Int, Int Value>
struct integral_constant {
    using value_type = Int;
    static constexpr Int value = Value;
};

template <bool B>
using bool_constant = integral_constant<bool, B>;

using true_type = bool_constant<true>;
using false_type = bool_constant<false>;
```

So that we can defined our previous `Plus` meta-function as:

```cpp
// with public inheritance
template <int Lhs, int Rhs>
struct Plus: integral_constant<int, Lhs + Rhs> {};

// or with template using type alias
template <int Lhs, int Rhs>
using Plus = integral_constant<int, Lhs + Rhs>;
```

Saved a lot of boilerplate huh?

## Introduction to Higher-order Meta-function

Now it's very straightforward to imagine what higher-order meta-function looks like.

### Function as Parameter

```cpp
int ApplyTwice(function<int(int)> f, int i) {
    return f(f(i));
}

// template template parameter syntax
template <template <int> typename F, int I>
using ApplyTwice = integral_constant<int, F<F<I>::value>::value>;
```

Actually, we don't have to provide the integer parameter here, we can just return a function that applies the parameter twice when called.

### Function as Result

```cpp
function<int(int)> Twice(funciton<int(int)> f) {
    return [f](int i) { return f(f(i)); };
}

template <template <int> typename F>
struct Twice {
    template <int I>
    using value = F<F<I>::value>;
};

// or in a more general way

template <template <auto> typename G, template <auto> typename F>
struct Compose {
    template <auto Param>
    using value = G<F<Param>::value>
};

template <template <int> typename F>
using Twice = Compose<F, F>;
```

Now things are looking more interesting, how about we test our `Twice` meta-function.

```cpp
template <int X>
using Increment = integral_constant<int, X + 1>;

// 5 + 1 + 1 == 7
static_assert(Twice<Increment>::value<5>::value == 7);
```

## Example: List and Higher-order Functions

One of the most prevalent uses of higher-order function is list (or array) operations. e.g.

```cpp
// invalid C++, illustration purpose
vector<Out> Map(function<Out(In)> f, vector<In> v);

assert(Map(to_string, {1, 2, 3}) == {"1", "2", "3"});

vector<A> Filter(function<bool(A)> f, vector<A> v);

assert(Filter(is_upper, {'a', 'B', 'c', 'D'}) == {'B', 'D'});

// etc..
```

Of course in template system, what we are manipulating is list of integers or list of types, but before we are able to manipulate them, we should first define them.

### Compile-time Integer List

C++11 introduced a life-saver feature, [template parameter pack](https://en.cppreference.com/w/cpp/language/parameter_pack). With that, our lives become much easier. And STL provide [`std::integer_sequence`](https://en.cppreference.com/w/cpp/utility/integer_sequence). The definition is very simple.

```cpp
template <typename T, T... Ints>
struct integer_sequence {};
```

All the information we need is stored in template parameters. Here let's simplify it a little bit.

```cpp
template <int... Ints>
using IntList = integer_sequence<int, Ints...>;

using OneTwoThreeFourFive = IntList<1, 2, 3, 4, 5>;
```

So we can define some simple operations of `IntList`, the implementations of are very straighforward, with the help of template specialization and pattern matching. Just note that the list itself is actually a 'type' instead of a 'value'.

```cpp
template <typename IntList>
struct Length;

template <int... Ints>
struct Length<IntList<Ints...>>: integral_constant<size_t, sizeof...(Ints)> {};

template <int V, typename IntList>
struct Push;

template <int V, int... Ints>
struct Push<V, IntList<Ints...>> {
    using type = IntList<V, Ints...>; // prepend V to Ints
};

// etc..
```

### Recursion

So let's implement `Map` and `Filter`.

```cpp
template <template <int> typename F, typename IntList>
struct Map;

template <template <int> typename F>
struct Map<F, IntList<>> {
    using type = IntList<>;
};

template <template <int> typename F, int Head, int... Tail>
struct Map<F, IntList<Head, Tail...>>:
    Push<F<Head>::value, Map<F, IntList<Tail...>>::type> {};
```

To traverse the `IntList`, we pattern match the integer list and perform a basic recursion:

- If the `IntList` contains no value, an empty list is returned
- If the `IntList` contains at least one value, we apply the meta-function to the first one (the `Head`), and recurse into the rest (the `Tail`), then concatenate two parts of results.

We can try it with `static_assert`:

```cpp
static_assert(
    std::is_same<
        Map<Increment, IntList<1, 2, 3>>::type,
        IntList<2, 3, 4>
    >::value
);
```

The `Filter` is similar, but we need to define our "compile-time branching"

```cpp
template <bool B, typename IfTrue, typename IfFalse>
struct If;

template <typename IfTrue, typename IfFalse>
struct If<true, IfTrue, IfFalse> {
    using type = IfTrue;
};

template <typename IfTrue, typename IfFalse>
struct If<false, IfTrue, IfFalse> {
    using type = IfFalse;
};

template <template <int> typename F, typename IntList>
struct Filter;

template <template <int> typename F>
struct Filter<F, IntList<>> {
    using type = IntList<>;
};

template <template <int> typename F, int Head, int... Tail>
struct Filter<F, IntList<Head, Tail...>>:
    If<F<Head>::value,
        Push<Head, Filter<F, Tail...>>,
        Filter<F, Tail...>>::type {};
```

The template `If` is a meta-function that returns different type with different bool input. And `Filter` makes use of that, it says if the `F<Head>` returns true, we want to add `Head` into the result, otherwise we just skip it.

If you're familiar with the functional representation of `List`, the logic of such operations are essentially the same.

And now you can define a lot more operations like `FindIf`, `RemoveIf` and all the list operations in the world.

How about `QuickSort`?

## Example: Currying of Meta-function

The premise of being able to sort a list is knowing how to compare the elements in the list, so the interface should be:

```cpp
template <typename IntList, template <int, int> typename LessThan>
struct QuickSort;
```

The trickiest part to implement quicksort is "partition". Ideally we filter the list with the condition being "less than the chosen _pivot_", but also keep track of the elements which are greater than or equal to pivot, rather than discarding them. (Such functions are usually called `Split`)

Since the value of "pivot" is a "variable", so to express "less than pivot", we can write:

```cpp
template <template <int, int> typename LessThan, int Pivot>
struct LTPivot {
    template <int N>
    using value = LessThan<N, Pivot>;
};

// and then we can do something like...

SplitWith<LTPivot<LessThan, Pivot>::value, integers>
```

This is absolutely correct, but we can find better abstraction of it.

### Currying

[Currying](https://en.wikipedia.org/wiki/Currying) came from an observation that every multi-parameter function can be transformed to a higher-order single-parameter function. And such transformations are called "Currying". For instance function like `(int, int) -> int` can be transformed to `int -> (int -> int)`

And utilitarians always ask: what's the use of that?

Think about "adding some value to every number in the list".

"I know higher-order function, that's easy"

```cpp
function<int(int)> AddSomeNumber(int lhs) {
    return [lhs](int rhs) { return lhs + rhs; };
}
vector<int> AllAddSomeNumber(int n, const vector<int>& v) {
    return Map(AddSomeNumber(n), v);
}
```

How about "multiply some value". "No problem!"

```cpp
function<int(int)> MultiplySomeNumber(int lhs) {
    return [lhs](int rhs) { return lhs * rhs; };
}
```

And you notice the pattern here, we are transforming some binary integer operation to a function that returns a unary function, And that's exactly currying. If we have a function `Curry` that performs the currying to a function, and a function `Plus`. We would be able to write:

```cpp
vector<int> AllAddSomeNumber(int n, const vector<int>& v) {
    return Map(Curry(Plus)(n), v);
}
```

### Curried Meta-function

And now we recall our `LessThanPivot` function. It should be defined as:

```cpp
template <template <int, int> typename F>
struct Curry {
    template <int Lhs>
    struct value {
        template <int Rhs>
        using value = F<Lhs, Rhs>::value;
    };
};
```

Then our `Split` call becomes:

```cpp
// due to the application order of `LessThan`, we have to "flip" it
template <template <int, int> typename F>
struct Flip {
    template <int A, int B>
    using value = F<B, A>;
};

Split<Curry<Flip<LessThan>::value>::value<Pivot>::value, integers>
```

Well, to be honest that's not as clean as I expected it to be.

_Actually, it would be even messier in the context where `LessThan` and `Pivot` are both template parameters, since you have to add all the [`template` and `typename` disambiguators](https://stackoverflow.com/questions/610245/where-and-why-do-i-have-to-put-the-template-and-typename-keywords) in the world to convince C++ compiler that the `value`s you're referring to are template names, which can be VERY annoying._

### Currying of arbitrary amount of parameters

Ok, we can do currying of 2 parameters, that's good. And we probably can do it for 3 or 4, just take some time, nest templates with templates. Not so complicated after you get there.

```cpp
template <template <int, int, int> typename F>
struct Curry3;

template <template <int, int, int, int> typename F>
struct Curry4;
```

Well, can we do **infinite**?

```cpp
template <template <int...> typename F>
struct Curry;

// so that

template <template <int, int, int> typename F>
using Curry3 = Curry<F>;

template <template <int, int, int, int> typename F>
using Curry4 = Curry<F>;

template <template <int,int,int,int,int,int,int,int,int,int,int,int> typename F>
using Curry12 = Curry<F>;
```

The first and easiest problem is how to apply arguments one at a time, since the number of application is uncertain, we have to do it recursively at very least. Like:

```cpp
template <template <int...> typename F>
struct Curry {
    template <int Arg>
    using value = Curry<F>;
};
```

And next we should store all the previously applied arguments. In the world of template, Of course, with template parameters.

```cpp
template <template <int...> typename F, int... PreviousArgs>
struct CurryImpl {
    template <int NextArg>
    using value = CurryImpl<F, PreviousArgs..., NextArg>;
};

template <template <int...> typename F>
using Curry = CurryImpl<F>;
```

Here comes another problem, `CurryImpl::value` introduces an infinite [(co)recursion](https://en.wikipedia.org/wiki/Corecursion) without a stop. `Curry<F>::value<1>::value<2>::value<3>:: ...`. That's no good.

We should stop the (co)recursion when `F` is fully applied and returning the result of `F`.

```cpp
template <template <int...> typename F, int... Args>
struct IsFullyApplied;

template <template <int...> typename F, int... PreviousArgs>
struct CurryImpl {
    template <int Arg>
    using value =
        If<IsFullyApplied<F, PreviousArgs..., Arg>::value>,
            F<PreviousArgs..., Arg>,
            CurryImpl<F, PreviousArgs..., Arg>
        >::type;
};
```

The last question is how do we implement `IsFullyApplied`. We need some extra tools called [SFINAE](https://en.cppreference.com/w/cpp/language/sfinae) which I won't cover here. But the idea is "if `F<Args...>::value` is a valid call, then we consider it `F` is fully applied with arguments `Args`, otherwise it isn't.

```cpp
template <template <int...> typename F, typename SFINAE, int... Args>
struct IsFullyApplied: false_type {};

template <template <int...> typename F, int... Args>
struct IsFullyApplied<F, std::void_t<decltype(F<Args...>::value)>, Args...>: true_type {};

// and the call site become:

If<IsFullyApplied<F, void, PreviousArgs..., Arg>::value, ...>
//  an extra void here ^
```

And that concludes our `Curry`. We can now do something like:

```cpp
template <int Lhs, int Rhs>
struct Plus : integral_constant<int, Lhs + Rhs> {};

template <int X, int Y, int Z>
struct Plus3: integral_constant<int, X + Y + Z> {};

template <int X>
using Add3 = Curry<Plus>::value<3>::value<X>;

template <int X>
using AnotherAdd3 = Curry<Plus3>::value<1>::value<2>::value<X>;

// 1 + 3 + 3 == 7
static_assert(Twice<Add3>::value<1>::value == 7);
static_assert(Twice<AnotherAdd3>::value<1>::value == 7);
```

## Epilogue

Thanks [@yb](https://github.com/YanB25) again for motivating me to write this post, which is (surprisingly) my first post about template. I was thinking that the content would be too little to form a complete post, anyhow here it is.

It's always very interesting (for me) to observe the similarities and connections between template system and lambda calculus (functional programming). If you are familiar with the latter, you should find a lot of concepts in this post rather natural. Although, the angle brackets and clunky syntax sometimes certainly ruin my mood of playing it.

And I skipped one (probably the most) important topic (intentionally): "what's the use of all these higher-something sh**". To be honest, this would be my last question to ask. But if you insist, the short answer is "they are useful, especially when you replace all those integers with actual types". Of course, they are certainly not in any sense "necessary" for the program to run.

Thanks for reading and Happy Cplusplusing!