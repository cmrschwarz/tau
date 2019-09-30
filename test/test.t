//require "test2.tau";
public func printf(x: string, y: int, c: int);

func foo(i: int)-> int {
    if(i < 5){
        return if(i < 3){
           break if(i == 1) 1 else 2;
        }
        else{
            break if(i == 3) 3 else 4;
        };
    }
    else {
        return if(i < 7){
            break if(i == 5) 5 else 6;
        }
        else{
            break  if(i == 7) 7 else 8;
        };
    };
}

public func main() -> int{
    mymac(){
    }
    i := 1;
    ip := &i;
    loop @foo{
        printf("foo: %i%c", foo(*ip), 10);
        (*ip)++;
        if(*ip == 9)break @foo;
    }
    return 0;
}
