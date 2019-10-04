require "print.t";
 #1+2;
 #@res{
        x:=0;
        y:=1;
        loop{
            if(x == 10)break @res y;
            y = y * 2;
            x++;
        }
    };
public func main() -> int{
    return 0;
}
