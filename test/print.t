public func puts(s: string);
public func printf(fmt: string, y: int, c: int);
public func printf(fmt: string, y: int){
    printf(fmt, y, 0);
}
public func printfln(fmt: string, i: int){
    printf(fmt, i);
    puts("");
}
