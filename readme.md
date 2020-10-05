# The Tau Programming Language

Tau is a statically compiled, general-purpose programming language
in early stages of development.

It's goal is to satisfy similar needs to C++ while providing:
* More expressive compile time metaprogramming
* Better code organisation (no header files)
* Faster compile times
* Better error handling and more safety in debug builds
* Cleaner syntax and better type inference

Tau currently uses LLVM for code generation, though having a additonal, custom
backend for faster debug builds is planned.

## Features

### Generics
```go
struct List[T: Type] { 
    begin: *T;
    //...
    func append(value: T){}
}
func main(){
    l: List[T];
    l.append(2);
}
```
Generic types are monomorphized at compile time, like C++ templates
Unlike with C++ templates though, there is type checking for the generic
type and no 'copy paste' SFINAE etc.

### Trait Oriented Programming
```rust
struct Car{
    //...
}
trait Vehicle{
    func drive();
    func crash();
}
impl Vehicle for Car{ //make the Vehicle "Interface" available for Cars
    func drive(){
        puts("wroom!");
    }
    func crash(){
        puts("boom!");
    }
}
impl int{ //implement methods directly on types (structs, primitives, etc.)
    func increment() -> int{
        self += 1;
        return self;
    }
}
func main() -> int{
    c: Car;
    c.drive();
    return -1.increment();
}
```

### Full Compile time evaluation

```go
require "libc.tau";
import libc::(malloc, free, printf);

public func main() -> int {
    // all code prefixed with '#' runs at compile time
    #result: int;
    #{
        x := malloc(64) as *[8]int;
        //...
        result = (*x)[0];  
        free(x as *void);
    }
    #printf("Hello from the compiler! It calculated: %i\n", result);
    return #result; //compile time variables become runtime constants 
}
```
It's possible to run arbitrary code can run at compile time,
including code using external libraries.
Compile time code uses the same code generation as runtime code.
Together with pastes this enables nearly unlimited metaprogramming.

### Compile time mixins
```go
public func main() -> int{  
    #x := 7;
    #y = 0;
    // multiline string 
    #s := \\" 
        #{
            y += 6;
            if(x == 0) paste("#y")
            else paste(s);
        }
    \\";
    // 'paste' inserts a string as code to be executed
    // in this case, this even happens recursively 
    // in the end, "#y" remains and is evaluated (42)
    return #paste(s);
}
```
Pastes can be used in almost any context. They can even introduce top level
declarations. The compiler keeps a full dependency graph to avoid ambiguitys
in resolution for this case.

## Work in progress Features
## Optional Types
```go
x: ?int; //either an int or None
x.then {
    x++;
}
```

## Enum Types
```rust
enum Fruit{
    Apple(sour: bool);
    Banana;
    Blueberry;
}
```
## Error types
```go
error MemoryError{ //similar to enums
    AllocationFail;
    NullPointerDeref;
}
// explicit error type: MemoryError or *void
func malloc() -> MemoryError!*void{
    //..
}
// inferred error type: *void or any of the returned errors
func foo() -> !int{ 
    x:= try malloc(10); // if malloc fails the function returns malloc's error
    //...
    return 3;
}
```

## Operator Overloading
```go
struct vec2{
    int x;
    int y;
    op construct(x: int, y: int){
        self.x = x;
        self.y = y;
    }
    op add(other: vec2){
        self.x += other.x;
        self.y += other.y;
    }
}
#vec2(1,2) + vec2(3,4);
```

## Dynamic, Generic Traits
```go
trait Addable{
    op add(other: Self);
}
trait Iterator[T: Type]{
    func next() -> ?T;
    //...
}
func sum[T: Addable](it: &Iterator[T]) -> T{
    sum := 0;
    loop{
        // dynamic dispatch, trait pointer is fat pointer containing the impl 
        // vtable
        n := it.next(); 
        n.then{
            sum += x;
        }
        else{
            break;
        }
    }
    return sum;
}
```

## Building
Currently Tau is tested and developed on Debian GNU/Linux, though
support for all major plattforms is planned.
To build it, clone the git repository using
```
git clone --recursive git@github.com:cmrschwarz/tau.git
```
Afterwards enter the root project directory.
```
cd tau
```

Then, either place a precompiled version of LLVM in 
```deps/llvm-project-prebuild```, or run 
```
./precompile-llvm.sh
``` 
(Precompiling yourself is quite slow since LLVM is a huge project)


To build the compiler itself:
```
mkdir build 
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j $(nproc)
