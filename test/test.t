require "print.t";
struct foo{
    x: int;
    y: int;
}
public func main() -> int{
    f := # @res{
        f: foo;
        f.x = 0;
        f.y = 1;
        loop{
            if(f.x == 10)break @res f;
            f.y = f.y * 2;
            f.x++;
        }
    };
    printfln("pp: %i", f.y);
    return 0;
}
