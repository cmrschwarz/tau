require "print.t";
struct foo{
    x: int;
    y: int;
}
public func main() -> int{
    f := @res{
        g: foo;
        g.x = 0;
        g.y = 1;
        loop{
            if(g.x == 10)break @res g;
            g.y = g.y * 2;
            g.x++;
        }
    };
    printfln("pp: %i", f.y);
    return 0;
}
