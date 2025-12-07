package pl.gamilife.infrastructure.core.architecture;

public interface UseCase<C extends Command, R> {
    R execute(C cmd);
}
