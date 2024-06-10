package stdlib;
public class Unit {
    public static final Unit SINGLE = new Unit();

    public String toString(){
        Func.partialApplication(null, null);
        return "()";
    }
}
