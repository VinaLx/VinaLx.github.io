---
layout: post
title: Agda Operator Precedence Cheatsheet
excerpt: Quick reference to agda operator precedence
category: ["agda"]
modified: 2018-04-15
---

## `Data`

###  `List`

| Operator | fixity | precedence | note |
| -------- | ------ | ---------- | ---- |
| `_∷_`    | R      | 5          |      |
| `_++_`   | R      | 5          |      |

### `Product`

| Operator | fixity | precedence | note                     |
| -------- | ------ | ---------- | ------------------------ |
| `_,_`    | R      | 4          | constructor of product   |
| `_,'_`   | R      | 4          | operands are independent |
| `_×_`    | R      | 2          |                          |

### `Sum`

| Operator | fixity | precedence | note |
| -------- | ------ | ---------- | ---- |
| `_⊎_`    | R      | 1          |      |

## `Relation`

### `Binary`

| Operator | fixity | precedence | note                |
| -------- | ------ | ---------- | ------------------- |
| `_≈_`    | N/A    | 4          |                     |
| `_≟_`    | N/A    | 4          | decidable of `_≟_`  |
| `_≤_`    | N/A    | 4          |                     |
| `_≤?_`   | N/A    | 4          | decidable of `_≤_`  |
| `_<_`    | N/A    | 4          |                     |
| `_<?_`   | N/A    | 4          | decidable of `_<?_` |

#### `PropositionalEquivalence`

| Operator | fixity | precedence | note |
| -------- | ------ | ---------- | ---- |
| ≡        | N/A    | 4          |      |
