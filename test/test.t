require "print.t";
struct foo{
    x: int;
    y: int;
}
public func main() -> int{
    loop{
        break 3;
        break 7;
    }
    f := @res{
        g: foo;
        f.x = 0;
        f.y = 1;
        loop{
            if(f.x == 10)break @res g;
            f.y = f.y * 2;
            f.x++;
        }
    };
    printfln("pp: %i", f.y);
    return 0;
}
