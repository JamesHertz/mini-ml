# Jasmin notes

I place here some notes to help with the development of a jvm compiler.

# Types

Primitive types:
- I -> for integer
- V -> for void
- Z -> for booleans

Arrays:
- `[<type>` -> stands for an array of `<type>`

Class types:
- `L<path-to-class>;`


# Classes

In order to define a class you use:
```jasmine
.class public <class-name>
.super <path-to-super-class> ; most of the times java/lang/Object
.implements <path-to-interface> ; should always come after `.super`

.field <name> <type>

; init method it is a special method you should define for every class
; you also want to call it after create a class with the `new` keyword
.method public <init>()V
    invokenonvirtual java/lang/Object/<init>()V ; calling constructor of class object
    return
.end method

.method public <methodname>(<args-type>*)<return-type>
    <lots-of-instructions>
    return ; or ireturn or areturn I suppose
.end method
```

## Interfaces
To define inteface you use something very similar to classes:
```jasmine
.interface public <interface-name>
.super <path-to-super-class> ; most of the times java/lang/Object

.method public abstract <method-name>(<args-type>*)<return-type>
.end method
```

## Method declaration
About method declaration you may want to do add do commands as follows:
```
.method <my-happy-method>(<arg-type>*)<return-tpe>
    .limit locals <nr-of-local-variables> ; the ones you access with aload, iload etc...
    .limit stack  <stack-size-in-bytes>
.end method
```
I am talking about the `.limit` for both `locals` and `stack`. The jvm might complain once or twice so you should always specify them. According with the professor 256 bytes for stack size is a good default.

## Methods calls and fields

To call a non-static method you use: `invokevirtual <path-to-method>`. Don't forget you need to have on the stack the reference to the object followed by the list of arguments.

To call a method from an interface you use: `invokeinterface <path-to-method-in-interface> <num-args>`.

To set or get the value of a field use respetively:  
```jasmine
setfield <path-to-field> <type>
getfield <path-to-field> <type>
```
In both you will need the reference of the object on the stack and in the case of set followed by the value we want to set to the field. On get, as expected the value will be left on the top of the stack.

## Typecast
Yes you will something to do cast you can use the following: `checkcast <interface>`

# Useful links
- (List of JVM instruction - Wikipedia)[https://en.wikipedia.org/wiki/List_of_Java_bytecode_instructions]
- (Jasmine Github Repo)[https://github.com/davidar/jasmin]
- (Jasmine Official Website)[https://jasmin.sourceforge.net/]
- (JVM specification - Oracle)[https://docs.oracle.com/javase/specs/jvms/se12/html/index.html]
