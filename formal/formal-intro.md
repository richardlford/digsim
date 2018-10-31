# Formal Methods for Digital Simulation

By Richard L Ford

## Introduction
"Formal Methods" is the name used to describe tools and techniques for
rigorously specifying and verifying mathematics, software and hardware.
In this paper I will describe ways that formal methods can be applied
to Cyber-Physical systems and in particular how they can be used to
verify the validity of the simulation of such systems.

It has been said that "a program without a specification can never
be incorrect, but it may be surprising". Thus a key part of formal
methods is providing a mathematically precise way of describing
the desired behavior of the system being specifified.

This can be done at various levels of abstraction.

The behavior of a physical system can be described using
differential equations. There are foundational physical laws,
such as Newton's second law and Newton's law of universal gravitation.
These can be used as the starting point and from them we can
derive formulas, e.g. solutions to the two-body problem.
We can prove the correctness of these derived formulas.

Such solutions would in general be in terms of infinite precision
real numbers. If we want to compute the numerical results of
such solutions we will need to do the computations using some
finite precision on a computer. In such a case we would like to
specify and prove error bounds on the computation due to
round-off errors.

Often it is the case that we may not be able to find a closed-form
solution to the differential equations of a system. In such a case we
may need to resort to methods of numerical integration. This introduces
another potential source of error. We would like to be able to
specify and prove bounds in the errors introduced by the numerical
methods.

The above differential equations and numerical integration may have
been expressed at an abstract level. But ultimately these need to
be programmed in a programming language. We would like to be able to
prove that the program correctly implements the algorithm or
numerical integration. This requires formal systems that understand
the formal semantics of the programming language used to write the
program, and that provides automation facilities to facilitate
the correctness proofs.

In summary, formal methods may be used to
- Model the physics and control systems of a Cyber-Physical System
- Model methods of solving the equations for such systems
- Express and prove properties of the above models
- Specify the required behavior of software that implements such
  a system
- Prove the correctness of software relative to its formal specification.

## Software Systems

In this section we will mention some of the software systems currently
available that support formal methods.

### Coq Proof Assistant

Coq has a specification language, Gallina, that can be used to define
data types, define functions that operate on those types, express and
prove theorems about those types and functions.

See https://coq.inria.fr.

### KeYmaera X

From the KeYmaera X readme:

    "Self-driving cars, autonomous robots, modern airplanes, or robotic
    surgery: we increasingly entrust our lives to computers and therefore
    should strive for nothing but the highest safety standards -
    mathematical correctness proof. Proofs for such cyber-physical systems
    can be constructed with the KeYmaera X prover. As a hybrid systems
    theorem prover, KeYmaera X analyzes the control program and the
    physical behavior of the controlled system together in differential
    dynamic logic."

See http://www.ls.cs.cmu.edu/KeYmaeraX. Also, there is a book,
"Logical Foundations of Cyber-Physical Systems", by Andre Platzer,
which describes differential dynamic logic in detail (dL for short).
dL would be an appropriate language to use for the most abstract
description of a system.

### CompCert
CompCert is a C compiler written in Coq (Gallina) that has been
proven to be correct.

See http://compcert.inria.fr.

### Verified Software Toolchain
This Coq-based toolchain can be used to specify the meaning of
C programs and then verify that the implementation correctly
implements the specification. It is closely tied to the
CompCert infrastructure as it uses the CompCert Clight
language as the level at which it reasons about the program being
proved. An advantage of this approach is that the specifications
and proofs are done at the input end of CompCert,and since
CompCert has been proven correct, you be then be sure that the
program you have proven correct will still be correct when compiled
to machine code.

The VST includes a powerful program logic that uses separation logic
to specify the contents of memory expected as input to functions and
the resulting memory contents after execution of the function.
Thus it is able to deal with complex data structures such as
linked lists, trees, hash tables, etc.

See http://vst.cs.princeton.edu.

### Frama-C
From its web site:

    "Frama-C is a suite of tools dedicated to the analysis of the source
    code of software written in C.

    Frama-C gathers several static analysis techniques in a single
    collaborative framework. The collaborative approach of Frama-C allows
    static analyzers to build upon the results already computed by other
    analyzers in the framework. Thanks to this approach, Frama-C provides
    sophisticated tools, such as a slicer and dependency analysis."

When using Frama-C, the semantics of the C program being verified are
expressed using ACSL: ANSI/ISO C Specification Language. The ACSL is
written as specialized comments in the C code that give the preconditions
and postconditions of the C functions. The frama-c tools then
parses the program, extracting the ACSL annotations. Frama-C plug-ins then
analyze the intermediate representation of the program. Two of the Frama-C
plugins, Jessie and Wp, produce verification condition theorems which, if
proven, imply the correctness of the C program with respect to the
ACSL annotations. There if a facility to pass these theorems to the
why3 systems (see below).

In addition, the frama-clang plugin (see
https://frama-c.com/frama-clang.html) is actually two plug-ins. One is a
clang plug-in that compiles C++ programs and translates the clang
abstract syntax tree (AST) into a format acceptable to Frama-C, and
a Frama-C plugin that reads the output of the clang plug-in and
produces the actual Frama-C intermediage form. The net effect is that
C++ programs can be specified and proven. This plug-in is still in the
early stages of its development.

See https://frama-c.com.

### Why3
From the why3 web page:

"Why3 is a platform for deductive program verification. It provides a
rich language for specification and programming, called WhyML, and
relies on external theorem provers, both automated and interactive, to
discharge verification conditions."

The Jessie Frama-C plug-in can send the verification conditions to
why3. It then will pass these to theorem provers that are configured.
Some of these, such as alt-ergo, are SMT solvers that in many cases will
be able to automatically prove the theorems. When the automated solvers
fail to prove a theorem, it can be passed to an interactive prover
such as Coq.

### Real and Floating-Point formalizations

Since many of the simulation computations involve real numbers and
the computer approximations of these, i.e. floating point numbers,
it is important to have formalizations of the theory of real numbers,
limits, derivatives and integrals (in other words--calculus), as well
as formalizations of floating point numbers.

The state of the art in this area is described in the book "Computer
Arithmetic and Formal Proofs: Verifying Floating-point Algorithms with
the Coq System", by Sylvie Boldo and Guillaume Melquiond. They are the
authors of the Flocq Coq library (see http://flocg.gforge.inria.fr)
that formalized floating-point arithmetics. They have also written a
Coq package called Gappa (http://gappa.gforge.inria.fr/) that aids in
automating proved of error bounds on computations.

They also use the Coquelicot library (http://coquelicot.saclay.inria.fr)
as a more user friendly formalization of calculus. It is compatible
with the standard Coq formalization of reals, but defines total functions
for limits, derivatives and integrals that are more easily manipulated
than the standard Coq definitions (see page 263 of the book).

### Enabling C++ using VST

Currently the VST can only handle C. The clightgen tool parses C code
and translates it to the CompCert clight language (expressed as a
Coq input file). But it should be possible to combine the
functionality of the frama-clang clang plug and the clightgen tool
so that C++ code can be verified by VST. We currently are working
to produce such an adaptation.

## Proposed Strategy

A comprehensive verification of a cyber-physical system should be specified
and verified at multiple levels.

### Differential Dynamic Logic Level
The most abstract level is expressed as a combination of
differential equations and discrete cyber interactions.
These can be expressed using dL and the Keymaera X system
can be used to express properties of such a system and
to prove that they are satisfied.

For example, in the case where a missile is trying to hit a target,
the dL specification would model the physics of the missile between
commands and the effects of commands on the missile physics (e.g.
commands to steer the missile in a new direction). With such a
specification we would like to prove that the system will hit
the target (within some error).

### Simulation Algorithm Level
For systems beyond a certain complexity, analytic solutions of the
differential equations may not be possible. It then is necessary to
use numerical simulations. Even if there is an analytic solution,
we would want to compare the analytic solution to the numerical
solution obtained by simulation.

There are theorems (e.g. Theorems 6.3 and 6.4 in
"An Introduction to Numerical Methods and Analysis", by James F. Epperson,
Revised Edition) that bound the errors on the numerical methods.
We should formally prove these theorems, or similar theorems for whatever
numerical methods we are using, so that we know the error bounds on the
simulations. The theorems depend on properties of the differential
equations being solved, e.g. Lipschitz continuity. We need to verify
that the differential equations being verified satisfy the required
conditions for the theorems that give the error bounds.

### Simulation Implementation Level
Assuming we have a simulation algorithm that would give acceptable
error bounds using infinite-precision real arithmetic, we would then
like to show that the actual C (or other language) implementation using
floating point arithmetic has related error bounds that combine the
effect of the algorithm errors and the floating point roundoff errors.

At this level we also want to prove that the code is actually implementing
the algorithm from the algorithm level.
