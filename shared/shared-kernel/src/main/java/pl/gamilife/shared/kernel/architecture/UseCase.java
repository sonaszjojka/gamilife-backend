package pl.gamilife.shared.kernel.architecture;

public interface UseCase<C extends Command, R> {
    R execute(C cmd);
}
