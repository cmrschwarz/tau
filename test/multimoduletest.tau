module a{public func foo()->int{
    return 1;
};}
module b{public func foo()->int{
    return 1;
};}
module c{public func foo()->int{
    return 1;
};}
module d{public func foo()->int{
    return 1;
};}
module e{public func foo()->int{
    return 1;
};}
module f{public func foo()->int{
    return 1;
};}
module g{public func foo()->int{
    return 1;
};}

import {a, b, c, d, e, f, g};

public func printf(s: string, i: int, j: int);
func collect()->int{
    x: int = 0;
    x = x + a::foo();
    x = x + b::foo();
    x = x + c::foo();
    x = x + d::foo();
    x = x + e::foo();
    x = x + f::foo();
    x = x + g::foo();
    return x;
}
public func main() -> int{
    printf("foo: %i%c", collect(), 10);
    return 0;
}