import bar;

module bar;
//#x := 3;
func main(args: String[]){    
    func bar(){}
    func test_compound_assignment(){
        if(3) @label{
            bar();
        };
        if (x) 3 else 5;
        x := 3;
        using ((), a, ((),),) = 17;
        (x, y, (a,b,c), d) := foo();
        (x, y:, (a,b: int,c), d) = foo();
        b: int;
        b += 3;
        x * #match(foo) @blk{
            if (1) {*x = 3;} => bar;
            x || Foo(x) => {
                return x;
            }
        };
        

        loop @foo{
            continue @foo;
            continue;
            return 12;
        }
        (x: int) := foo;
        
        y:= if (1) {
                foo();
            }
            else {
                x++;
            } 
            * 3;
        using z = if (x > 3) @foo{
            return 3 * {
            break @foo 12;
            break;
            return 12;
            };
        }
        else{
            break 27;
        };
    }
    return 0;
}