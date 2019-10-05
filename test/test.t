require "print.t";

public func main() -> int{
    x := # @res{
        x:=0;
        y:=1;
        loop{
            if(x == 10)break @res y;
            y = y * 2;
            x++;
        }
    };
    printfln("pp: %i", x);
    return 0;
}
