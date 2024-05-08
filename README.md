<div align=center>
  <h1>
    Designing Programming Language
  </h1>
  <p>
    <b>KAIST CS320: Programming Language (Spring 2024)</b><br>
  </p>
</div>

<div align=center>
  <p>
    Instructor: <a href=https://plrg.kaist.ac.kr/ryu target="_blank"><b>Sukyoung Ryu</b></a> (sryu.cs [at] kaist.ac.kr)<br>
  </p>
</div>


# Project #1: FIBER

**Read the [common instructions](https://github.com/kaist-plrg-cs320/assignment-docs) first if you have not read them.**
**Don't forget to read the
[REPL](https://github.com/kaist-plrg-cs320/assignment-docs#repl),
[Fuzzing](https://github.com/kaist-plrg-cs320/assignment-docs#fuzzing), and
[Grading](https://github.com/kaist-plrg-cs320/assignment-docs#grading)
sections.**

## Download 

```bash
sbt new kaist-plrg-cs320/fiber.g8
```

## Descriptions

Read the FIBER language specification (`fiber-spec.pdf`) in the project directory,
and then implement an interpreter for FIBER.
Expressions are defined in `core/src/main/scala/cs320/Expr.scala`,
and values are defined in `core/src/main/scala/cs320/Value.scala`.

### Reference Interpreter

The reference interpreter of FIBER is available at <https://plrg.kaist.ac.kr/fiber>.
Use the reference interpreter to find the correct result of a certain expression.

## Submission

Submit your `Implementation.scala` at
<https://kaist-cs320.appspot.com/assignment/Project%201>.



# Project #2: X-FIBER

**Read the [common instructions](https://github.com/kaist-plrg-cs320/assignment-docs) first if you have not read them.**
**Don't forget to read the
[REPL](https://github.com/kaist-plrg-cs320/assignment-docs#repl),
[Fuzzing](https://github.com/kaist-plrg-cs320/assignment-docs#fuzzing), and
[Grading](https://github.com/kaist-plrg-cs320/assignment-docs#grading)
sections.**

## Download 

```bash
sbt new kaist-plrg-cs320/x-fiber.g8
```

## Descriptions

Read the X-FIBER language specification (`x-fiber-spec.pdf`) in the project directory,
and then implement an interpreter for X-FIBER.
Expressions are defined in `core/src/main/scala/cs320/Expr.scala`,
and values are defined in `core/src/main/scala/cs320/Value.scala`.

### Reference Interpreter

The reference interpreter of X-FIBER is available at <https://plrg.kaist.ac.kr/x-fiber>.
Use the reference interpreter to find the correct result of a certain expression.

## Submission

Submit your `Implementation.scala` at
<https://kaist-cs320.appspot.com/assignment/Project%202>.
