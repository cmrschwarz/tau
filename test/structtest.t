
public func printf(fmt: string, y: int, c: int);
struct foo{
    lorem : int;
    ipsum: int;
}


public func main(){
    f: foo;
    f.ipsum = 17;
    printf("ipsum: %i%c", f.ipsum,10);
}
