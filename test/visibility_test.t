module m{
    module impl{
    }
    require "visibility_test.t";
    public struct Foo {
        protected struct Bar {
            private struct Baz {
            }
            struct Qux{
            }
        }
    }
    public func MTest() -> int{
    return 3;
    }
}
extend m{
    //this should not conflict with m::impl in the module decl,
    // since both are default visibility
    module impl{} 
    struct Lorem{
        
    }
    private struct Ipsum{
    }
    protected struct Dolor{
    }
}

func main(){
    //should work
    import m;
    import m::(Foo);
    foo: Foo;
    using m;  //TODO: allow m::*;
    dolor: Dolor; //should work since we are using
    //should fail
    import m::(Lorem);
    lorem: m::Lorem;
    bar: Foo::Bar;
    ipsum: m::Ipsum;
}