---
layout: post
title: Algorithm Homework Weekly - 18
excerpt: "Algorithm Ex. 8.12: K-Spanning-Tree NP-completeness"
category: [algorithm homwork]
modified: 2017-12-28
---

## 8.20

> The k-SPANNING TREE problem is the following.
>
> Input: An undirected graph $$ G = (V, E) $$
>
> Output: A spanning tree of $$ G $$ in which each node has $$ degree \le k $$ if such a tree exists.
>
> Show that for any $$ k \ge 2 $$, k-spanning-tree is NP-complete

## Proof

It's well know that the [Hamiltonian Path Problem](https://en.wikipedia.org/wiki/Hamiltonian_path) is a NP-complete problem to solve.

And we can reduce the hanmilton path problem to a k-spanning-tree problem as follow:

Given a graph \\(G\\), **for each node** \\(N\_n\\) in \\(G\\), create `k - 2` new empty nodes (nodes with no edge attached) \\(N\_{n1}, N\_{n2}, \\dots, N\_{n(k-2)}\\), and connect them to `N`, adding edges \\(E\_{n1}, E\_{n2}, \dots E\_{n(k-2)}\\), and this form a new graph \\(G'\\). Finding a k-spanning-tree in \\(G'\\), say \\(T\\).

It's certain that all newly added edges \\(E\_{n1}, E\_{n2}, \dots E\_{n(k-2)}\\) must be in \\(T\\), since we have no other ways to connect those newly added nodes except add those new edges to the tree.

And at last we disconnect all new edges from \\(T\\), so nodes in the rest of the tree have at most degree 2, which forms a hanmilton path for the original graph \\(G\\).

Since we proved the hamilton path problem is NP-complete, and we can obtain a hamilton path by k-spanning-tree if exists, so the k-spanning-tree is NP-complete.