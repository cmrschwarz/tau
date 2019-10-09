require "print.t";
struct foo{
    x: int;
    y: int;
}
public func bar() -> int{
   puts("foo");
  return 5;
}
public func main() -> int{
    f := #bar();
    g := #bar();
    printfln("pp: %i%c", f + g);
    return 0;
}
