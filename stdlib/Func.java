public interface Func {

  Object apply(Object[] args);

  // TODO: think about print of these things c:
  static Func partialApplication(Func func, Object[] extra){
    return new PartialApplication(func, extra);
  }

  static class PartialApplication implements Func {
    final Object[] extra;
    final Func func;

    private PartialApplication(Func func, Object[] extra){
      this.extra = extra;
      this.func  = func;
    }

     @Override
     public Object apply(Object[] args) {
        Object[] values = new Object[extra.length + args.length];

        for(int i = 0; i < extra.length; i++)
            values[i] = extra[i];

        for(int i = 0; i < args.length; i++)
            values[extra.length + i] = args[i];

        return func.apply(values);
     }
  }
}
