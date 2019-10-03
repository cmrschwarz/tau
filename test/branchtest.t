require "print.t";

func foo(i: int)-> int {
    return if(i < 5){
        return if(i < 3){
           break if(i == 1) 1 else 2;
        }
        else{
            break if(i == 3) 3 else 4;
        };
    }
    else {
        break if(i < 7){
            break if(i == 5) 5 else 6;
        }
        else{
            break  if(i == 7) 7 else 8;
        };
    };
}


public func main(){
    i := foo(1);
    ip := &i;
    loop @foo{
        printf("foo: %i%c", foo(*ip), 10);
        (*ip)++;
        if(*ip == 9)break @foo;
    }
}
