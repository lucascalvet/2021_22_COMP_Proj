# Compilers Project
## GROUP: comp2022-7a

| NAME | NR | GRADE | CONTRIBUTION |
| :---: | :---: | :--------------: | :----------: |
| Ana Matilde Barra | 201904795 | | 25% |
| Eunice Amorim | 201904920 | | 25% |
| José Ferreira | 201904515 | | 25% |
| Lucas Calvet | 201904517 | | 25% |

**GLOBAL Grade of the project:** <0 to 20>

[comment]: <> (IN THE END TO A REVISION OF ALL TEXT AND FACT CHECK ALL OF THIS!)


## Summary:
[comment]: <> (&#40;Describe what your tool does and its main features.&#41;)

This project consists on a simple compiler for the Java-- (Jmm) Language. <br>
It parses Jmm files into Java byte code, which is later executed using the Jasmin Library. 
This tool offers options for code optimization, which can be turned on by using the flags `-r x` and `-o`.

**Main Features:**

- Using a Context Free Grammar (defined in [JmmGrammar.jj](/javacc/JmmGrammar.jj)) to identify any Syntatic Errors and build and AST (Abstract Syntax Tree);
- Building a Symbol Table with the use of all the annotations created in the AST;
- Using this Symbol Table to build a String representing the initial file to compile contents in Ollir code;
- Interpreting the Ollir code and translating it to Jasmin Code, which can later be executed.
- Available Optimizations:
  - ...
    

**Command Line Arguments:**

`.\comp2022-7a.bat <file_path> [-r x] [-o] [-d]`

- Mandatory:
  - <file_path> - 
- Optionals (in any order):
  - **`-r x` (Register Allocation)** - ***Falta Fazer*** When this option is activated, the compiler will try to minimize the number of registers used, indicating if the value given is enough or if more are needed;
  - **`-o` (Additional Optimizations)** - ***Vamos Fazer??***
  - **`-d` (Debug)** - ***Parece simples de fazer*** When this option is activated by adding it to the execution command, the outputs will not only be the ones of the compiled file or the errors, but also the additional information of the compiler (AST, Ollir Code, Jasmin Code, etc);
 
## Semantic Analysis:
[comment]: <> (&#40;Refer the semantic rules implemented by your tool.&#41;)

Our tool implements the following semantic rules:

- [x] Rule 1
- [ ] Rule 2

## Code Generation:
[comment]: <> (&#40;describe how the code generation of your tool works and identify the possible problems your tool has regarding code generation.&#41;)

In order to generate code that can be executed, our tool follows the steps explained below:
  - Parsing the input file
  - Translating to Ollir
  - Translating to Jasmin
  - ...

(Possible problems our compiler may have regarding code generation)

## Pros:
[comment]: <> (&#40;Identify the most positive aspects of your tool&#41;)

- We have a 'by default' optimization, which involves the use of optimized Jasmin instructions when dealing with the stack. 
- ...

## Cons:
[comment]: <> (&#40;Identify the most negative aspects of your tool&#41;)

- ... 

## Task Distribution: 
- Ana Matilde Barra - Parser, Jasmin, Report
- Eunice Amorim - Parser, Jasmin+, 
- José Ferreira - Parser, Symbol Table, Semantic Analysis
- Lucas Calvet -  Parser, Symbol Table, Ollir

---

# Initial Instructions 

For this project, you need to install [Java](https://jdk.java.net/), [Gradle](https://gradle.org/install/), and [Git](https://git-scm.com/downloads/) (and optionally, a [Git GUI client](https://git-scm.com/downloads/guis), such as TortoiseGit or GitHub Desktop). Please check the [compatibility matrix](https://docs.gradle.org/current/userguide/compatibility.html) for Java and Gradle versions.

## Project setup

There are three important subfolders inside the main folder. First, inside the subfolder named ``javacc`` you will find the initial grammar definition. Then, inside the subfolder named ``src`` you will find the entry point of the application. Finally, the subfolder named ``tutorial`` contains code solutions for each step of the tutorial. JavaCC21 will generate code inside the subfolder ``generated``.

## Compile and Running

To compile and install the program, run ``gradle installDist``. This will compile your classes and create a launcher script in the folder ``./build/install/comp2022-00/bin``. For convenience, there are two script files, one for Windows (``comp2022-00.bat``) and another for Linux (``comp2022-00``), in the root folder, that call tihs launcher script.

After compilation, a series of tests will be automatically executed. The build will stop if any test fails. Whenever you want to ignore the tests and build the program anyway, you can call Gradle with the flag ``-x test``.

## Test

To test the program, run ``gradle test``. This will execute the build, and run the JUnit tests in the ``test`` folder. If you want to see output printed during the tests, use the flag ``-i`` (i.e., ``gradle test -i``).
You can also see a test report by opening ``./build/reports/tests/test/index.html``.

## Checkpoint 1
For the first checkpoint the following is required:

1. Convert the provided e-BNF grammar into JavaCC grammar format in a .jj file
2. Resolve grammar conflicts, preferably with lookaheads no greater than 2
3. Include missing information in nodes (i.e. tree annotation). E.g. include the operation type in the operation node.
4. Generate a JSON from the AST

### JavaCC to JSON
To help converting the JavaCC nodes into a JSON format, we included in this project the JmmNode interface, which can be seen in ``src-lib/pt/up/fe/comp/jmm/ast/JmmNode.java``. The idea is for you to use this interface along with the Node class that is automatically generated by JavaCC (which can be seen in ``generated``). Then, one can easily convert the JmmNode into a JSON string by invoking the method JmmNode.toJson().

Please check the JavaCC tutorial to see an example of how the interface can be implemented.

### Reports
We also included in this project the class ``src-lib/pt/up/fe/comp/jmm/report/Report.java``. This class is used to generate important reports, including error and warning messages, but also can be used to include debugging and logging information. E.g. When you want to generate an error, create a new Report with the ``Error`` type and provide the stage in which the error occurred.


### Parser Interface

We have included the interface ``src-lib/pt/up/fe/comp/jmm/parser/JmmParser.java``, which you should implement in a class that has a constructor with no parameters (please check ``src/pt/up/fe/comp/CalculatorParser.java`` for an example). This class will be used to test your parser. The interface has a single method, ``parse``, which receives a String with the code to parse, and returns a JmmParserResult instance. This instance contains the root node of your AST, as well as a List of Report instances that you collected during parsing.

To configure the name of the class that implements the JmmParser interface, use the file ``config.properties``.

### Compilation Stages 

The project is divided in four compilation stages, that you will be developing during the semester. The stages are Parser, Analysis, Optimization and Backend, and for each of these stages there is a corresponding Java interface that you will have to implement (e.g. for the Parser stage, you have to implement the interface JmmParser).


### config.properties

The testing framework, which uses the class TestUtils located in ``src-lib/pt/up/fe/comp``, has methods to test each of the four compilation stages (e.g., ``TestUtils.parse()`` for testing the Parser stage). 

In order for the test class to find your implementations for the stages, it uses the file ``config.properties`` that is in root of your repository. It has four fields, one for each stage (i.e. ``ParserClass``, ``AnalysisClass``, ``OptimizationClass``, ``BackendClass``), and initially it only has one value, ``pt.up.fe.comp.SimpleParser``, associated with the first stage.

During the development of your compiler you will update this file in order to setup the classes that implement each of the compilation stages.
