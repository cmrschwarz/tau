require "print.t";
struct foo{
    lorem : int;
    ipsum: int;
    struct bar{
        x: int;
        y: string;
    }
}

public func main() -> int{
    f: foo;
    fp := &f;
    (*fp).ipsum = 17;
    printfln("ipsum: %i", (*fp).ipsum);
    return 0;
}
