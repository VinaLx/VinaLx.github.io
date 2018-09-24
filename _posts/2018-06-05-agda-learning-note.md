---
layout: post
title: Agda Learning Note
excerpt: Notes on agda core language and standard library.
category: ["agda"]
modified: 2018-04-15
---

## Standard Library

### `Data.List`

| Operator | fixity | precedence | note |
| -------- | ------ | ---------- | ---- |
| `_∷_`    | R      | 5          |      |
| `_++_`   | R      | 5          |      |

### `Data.Product`

| Operator | fixity | precedence | note                     |
| -------- | ------ | ---------- | ------------------------ |
| `_,_`    | R      | 4          | constructor of product   |
| `_,'_`   | R      | 4          | operands are independent |
| `_×_`    | R      | 2          |                          |

### `Data.Sum`

| Operator | fixity | precedence | note |
| -------- | ------ | ---------- | ---- |
| `_⊎_`    | R      | 1          |      |

### `Relation.Binary`

| Operator | fixity | precedence | note                |
| -------- | ------ | ---------- | ------------------- |
| `_≈_`    | N/A    | 4          |                     |
| `_≟_`    | N/A    | 4          | decidable of `_≟_`  |
| `_≤_`    | N/A    | 4          |                     |
| `_≤?_`   | N/A    | 4          | decidable of `_≤_`  |
| `_<_`    | N/A    | 4          |                     |
| `_<?_`   | N/A    | 4          | decidable of `_<?_` |

### `Relation.Binary.PropositionalEquivalence`

| Operator | fixity | precedence | note |
| -------- | ------ | ---------- | ---- |
| ≡        | N/A    | 4          |      |
